// Licensed to the Apache Software Foundation (ASF) under one
// or more contributor license agreements.  See the NOTICE file
// distributed with this work for additional information
// regarding copyright ownership.  The ASF licenses this file
// to you under the Apache License, Version 2.0 (the
// "License"); you may not use this file except in compliance
// with the License.  You may obtain a copy of the License at
//
//   http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing,
// software distributed under the License is distributed on an
// "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
// KIND, either express or implied.  See the License for the
// specific language governing permissions and limitations
// under the License.

#include "arrow/dataset/expression.h"

#include <unordered_map>
#include <unordered_set>
#include <vector>

#include "arrow/compute/api_vector.h"
#include "arrow/record_batch.h"
#include "arrow/table.h"
#include "arrow/util/logging.h"

namespace arrow {

using internal::checked_cast;

namespace dataset {

bool Identical(const Expression2& l, const Expression2& r) { return l.impl_ == r.impl_; }

const Expression2::Call* CallNotNull(const Expression2& expr) {
  auto call = expr.call();
  DCHECK_NE(call, nullptr);
  return call;
}

inline void GetAllFieldRefs(const Expression2& expr,
                            std::unordered_set<FieldRef, FieldRef::Hash>* refs) {
  if (auto lit = expr.literal()) return;

  if (auto ref = expr.field_ref()) {
    refs->emplace(*ref);
    return;
  }

  for (const Expression2& arg : CallNotNull(expr)->arguments) {
    GetAllFieldRefs(arg, refs);
  }
}

inline std::vector<ValueDescr> GetDescriptors(const std::vector<Expression2>& exprs) {
  std::vector<ValueDescr> descrs(exprs.size());
  for (size_t i = 0; i < exprs.size(); ++i) {
    DCHECK(exprs[i].IsBound());
    descrs[i] = exprs[i].descr();
  }
  return descrs;
}

inline std::vector<ValueDescr> GetDescriptors(const std::vector<Datum>& values) {
  std::vector<ValueDescr> descrs(values.size());
  for (size_t i = 0; i < values.size(); ++i) {
    descrs[i] = values[i].descr();
  }
  return descrs;
}

struct ARROW_DS_EXPORT ExpressionState {
  std::unordered_map<Expression2, std::shared_ptr<compute::KernelState>,
                     Expression2::Hash>
      kernel_states;

  compute::KernelState* Get(const Expression2& expr) const {
    auto it = kernel_states.find(expr);
    if (it == kernel_states.end()) return nullptr;
    return it->second.get();
  }

  void Replace(const Expression2& expr, const Expression2& replacement) {
    auto it = kernel_states.find(expr);
    if (it == kernel_states.end()) return;

    auto kernel_state = std::move(it->second);
    kernel_states.erase(it);
    kernel_states.emplace(replacement, std::move(kernel_state));
  }

  void Drop(const Expression2& expr) {
    auto it = kernel_states.find(expr);
    if (it == kernel_states.end()) return;
    kernel_states.erase(it);
  }

  void MoveFrom(ExpressionState* other) {
    std::move(other->kernel_states.begin(), other->kernel_states.end(),
              std::inserter(kernel_states, kernel_states.end()));
  }
};

struct FieldPathGetDatumImpl {
  template <typename T, typename = decltype(FieldPath{}.Get(std::declval<const T&>()))>
  Result<Datum> operator()(const std::shared_ptr<T>& ptr) {
    return path_.Get(*ptr).template As<Datum>();
  }

  template <typename T>
  Result<Datum> operator()(const T&) {
    return Status::NotImplemented("FieldPath::Get() into Datum ", datum_.ToString());
  }

  const Datum& datum_;
  const FieldPath& path_;
};

inline Result<Datum> GetDatumField(const FieldRef& ref, const Datum& input) {
  Datum field;

  FieldPath path;
  if (auto type = input.type()) {
    ARROW_ASSIGN_OR_RAISE(path, ref.FindOneOrNone(*input.type()));
  } else if (input.kind() == Datum::RECORD_BATCH) {
    ARROW_ASSIGN_OR_RAISE(path, ref.FindOneOrNone(*input.record_batch()->schema()));
  } else if (input.kind() == Datum::TABLE) {
    ARROW_ASSIGN_OR_RAISE(path, ref.FindOneOrNone(*input.table()->schema()));
  }

  if (path) {
    ARROW_ASSIGN_OR_RAISE(field,
                          util::visit(FieldPathGetDatumImpl{input, path}, input.value));
  }

  if (field == Datum{}) {
    field = Datum(std::make_shared<NullScalar>());
  }

  return field;
}

struct Comparison {
  enum type {
    NA = 0,
    EQUAL = 1,
    LESS = 2,
    GREATER = 4,
    NOT_EQUAL = LESS | GREATER,
    LESS_EQUAL = LESS | EQUAL,
    GREATER_EQUAL = GREATER | EQUAL,
  };

  static const type* Get(const std::string& function) {
    static std::unordered_map<std::string, type> flipped_comparisons{
        {"equal", EQUAL},     {"not_equal", NOT_EQUAL},
        {"less", LESS},       {"less_equal", LESS_EQUAL},
        {"greater", GREATER}, {"greater_equal", GREATER_EQUAL},
    };

    auto it = flipped_comparisons.find(function);
    return it != flipped_comparisons.end() ? &it->second : nullptr;
  }

  static const type* Get(const Expression2& expr) {
    if (auto call = expr.call()) {
      return Comparison::Get(call->function);
    }
    return nullptr;
  }

  static Result<type> Execute(Datum l, Datum r) {
    if (!l.is_scalar() || !r.is_scalar()) {
      return Status::Invalid("Cannot Execute Comparison on non-scalars");
    }
    std::vector<Datum> arguments{std::move(l), std::move(r)};

    ARROW_ASSIGN_OR_RAISE(auto equal, compute::CallFunction("equal", arguments));

    if (!equal.scalar()->is_valid) return NA;
    if (equal.scalar_as<BooleanScalar>().value) return EQUAL;

    ARROW_ASSIGN_OR_RAISE(auto less, compute::CallFunction("less", arguments));

    if (!less.scalar()->is_valid) return NA;
    return less.scalar_as<BooleanScalar>().value ? LESS : GREATER;
  }

  static type GetFlipped(type op) {
    switch (op) {
      case NA:
        return NA;
      case EQUAL:
        return EQUAL;
      case LESS:
        return GREATER;
      case GREATER:
        return LESS;
      case NOT_EQUAL:
        return NOT_EQUAL;
      case LESS_EQUAL:
        return GREATER_EQUAL;
      case GREATER_EQUAL:
        return LESS_EQUAL;
    };
    DCHECK(false);
    return NA;
  }

  static std::string GetName(type op) {
    switch (op) {
      case NA:
        DCHECK(false) << "unreachable";
        break;
      case EQUAL:
        return "equal";
      case LESS:
        return "less";
      case GREATER:
        return "greater";
      case NOT_EQUAL:
        return "not_equal";
      case LESS_EQUAL:
        return "less_equal";
      case GREATER_EQUAL:
        return "greater_equal";
    };
    DCHECK(false);
    return "na";
  }
};

inline bool IsSetLookup(const std::string& function) {
  return function == "is_in" || function == "index_in";
}

inline const compute::SetLookupOptions* GetSetLookupOptions(
    const Expression2::Call& call) {
  if (!IsSetLookup(call.function)) return nullptr;
  return checked_cast<const compute::SetLookupOptions*>(call.options.get());
}

inline bool RequriesDictionaryTransparency(const Expression2::Call& call) {
  // TODO move this functionality into compute::

  // Functions which don't provide kernels for dictionary types. Dictionaries will be
  // decoded for these functions.
  if (Comparison::Get(call.function)) return true;

  if (IsSetLookup(call.function)) return true;

  return false;
}

inline Status EnsureNotDictionary(ValueDescr* descr) {
  const auto& type = descr->type;
  if (type && type->id() == Type::DICTIONARY) {
    descr->type = checked_cast<const DictionaryType&>(*type).value_type();
  }
  return Status::OK();
}

inline Status EnsureNotDictionary(Datum* datum) {
  if (datum->type()->id() != Type::DICTIONARY) {
    return Status::OK();
  }

  if (datum->is_scalar()) {
    ARROW_ASSIGN_OR_RAISE(
        *datum,
        checked_cast<const DictionaryScalar&>(*datum->scalar()).GetEncodedValue());
    return Status::OK();
  }

  DCHECK_EQ(datum->kind(), Datum::ARRAY);
  ArrayData indices = *datum->array();
  indices.type = checked_cast<const DictionaryType&>(*datum->type()).index_type();
  auto values = std::move(indices.dictionary);

  ARROW_ASSIGN_OR_RAISE(
      *datum, compute::Take(values, indices, compute::TakeOptions::NoBoundsCheck()));
  return Status::OK();
}

inline Status EnsureNotDictionary(Expression2::Call* call) {
  if (auto options = GetSetLookupOptions(*call)) {
    auto new_options = *options;
    RETURN_NOT_OK(EnsureNotDictionary(&new_options.value_set));
    call->options.reset(new compute::SetLookupOptions(std::move(new_options)));
  }
  return Status::OK();
}

inline Result<std::shared_ptr<StructScalar>> FunctionOptionsToStructScalar(
    const Expression2::Call& call) {
  if (call.options == nullptr) {
    return nullptr;
  }

  auto Finish = [](ScalarVector values, std::vector<std::string> names) {
    FieldVector fields(names.size());
    for (size_t i = 0; i < fields.size(); ++i) {
      fields[i] = field(std::move(names[i]), values[i]->type);
    }
    return std::make_shared<StructScalar>(std::move(values), struct_(std::move(fields)));
  };

  if (auto options = GetSetLookupOptions(call)) {
    if (!options->value_set.is_array()) {
      return Status::NotImplemented("chunked value_set");
    }
    return Finish(
        {
            std::make_shared<ListScalar>(options->value_set.make_array()),
            MakeScalar(options->skip_nulls),
        },
        {"value_set", "skip_nulls"});
  }

  if (call.function == "cast") {
    auto options = checked_cast<const compute::CastOptions*>(call.options.get());
    return Finish(
        {
            MakeNullScalar(options->to_type),
            MakeScalar(options->allow_int_overflow),
            MakeScalar(options->allow_time_truncate),
            MakeScalar(options->allow_time_overflow),
            MakeScalar(options->allow_decimal_truncate),
            MakeScalar(options->allow_float_truncate),
            MakeScalar(options->allow_invalid_utf8),
        },
        {
            "to_type_holder",
            "allow_int_overflow",
            "allow_time_truncate",
            "allow_time_overflow",
            "allow_decimal_truncate",
            "allow_float_truncate",
            "allow_invalid_utf8",
        });
  }

  return Status::NotImplemented("conversion of options for ", call.function);
}

inline Status FunctionOptionsFromStructScalar(const StructScalar* repr,
                                              Expression2::Call* call) {
  if (repr == nullptr) {
    call->options = nullptr;
    return Status::OK();
  }

  if (IsSetLookup(call->function)) {
    ARROW_ASSIGN_OR_RAISE(auto value_set, repr->field("value_set"));
    ARROW_ASSIGN_OR_RAISE(auto skip_nulls, repr->field("skip_nulls"));
    call->options = std::make_shared<compute::SetLookupOptions>(
        checked_cast<const ListScalar&>(*value_set).value,
        checked_cast<const BooleanScalar&>(*skip_nulls).value);
    return Status::OK();
  }

  if (call->function == "cast") {
    auto options = std::make_shared<compute::CastOptions>();
    ARROW_ASSIGN_OR_RAISE(auto to_type_holder, repr->field("to_type_holder"));
    options->to_type = to_type_holder->type;

    int i = 1;
    for (bool* opt : {
             &options->allow_int_overflow,
             &options->allow_time_truncate,
             &options->allow_time_overflow,
             &options->allow_decimal_truncate,
             &options->allow_float_truncate,
             &options->allow_invalid_utf8,
         }) {
      *opt = checked_cast<const BooleanScalar&>(*repr->value[i++]).value;
    }

    call->options = std::move(options);
    return Status::OK();
  }

  return Status::NotImplemented("conversion of options for ", call->function);
}

struct FlattenedAssociativeChain {
  bool was_left_folded = true;
  std::vector<Expression2> exprs, fringe;

  explicit FlattenedAssociativeChain(Expression2 expr) : exprs{std::move(expr)} {
    auto call = CallNotNull(exprs.back());
    fringe = call->arguments;

    auto it = fringe.begin();

    while (it != fringe.end()) {
      auto sub_call = it->call();
      if (!sub_call || sub_call->function != call->function) {
        ++it;
        continue;
      }

      if (it != fringe.begin()) {
        was_left_folded = false;
      }

      exprs.push_back(std::move(*it));
      it = fringe.erase(it);
      it = fringe.insert(it, sub_call->arguments.begin(), sub_call->arguments.end());
      // NB: no increment so we hit sub_call's first argument next iteration
    }

    DCHECK(std::all_of(exprs.begin(), exprs.end(), [](const Expression2& expr) {
      return CallNotNull(expr)->options == nullptr;
    }));
  }
};

}  // namespace dataset
}  // namespace arrow
