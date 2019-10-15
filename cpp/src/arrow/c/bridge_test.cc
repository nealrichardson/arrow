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

#include <deque>
#include <functional>
#include <string>
#include <utility>
#include <vector>

#include <gtest/gtest.h>

#include "arrow/c/bridge.h"
#include "arrow/c/helpers.h"
#include "arrow/ipc/json_simple.h"
#include "arrow/memory_pool.h"
#include "arrow/testing/gtest_util.h"
#include "arrow/testing/util.h"
#include "arrow/util/macros.h"
#include "arrow/util/string_view.h"

namespace arrow {

class ExportGuard {
 public:
  explicit ExportGuard(struct ArrowArray* c_export) : c_export_(c_export) {}

  ~ExportGuard() { Release(); }

  void Release() {
    if (c_export_ && c_export_->format && c_export_->release) {
      c_export_->release(c_export_);
      c_export_ = nullptr;
    }
  }

 private:
  ARROW_DISALLOW_COPY_AND_ASSIGN(ExportGuard);

  struct ArrowArray* c_export_;
};

class ReleaseCallback {
 public:
  explicit ReleaseCallback(struct ArrowArray* c_struct) : called_(false) {
    orig_release_ = c_struct->release;
    orig_private_data_ = c_struct->private_data;
    c_struct->release = ReleaseUnbound;
    c_struct->private_data = this;
  }

  static void ReleaseUnbound(struct ArrowArray* c_struct) {
    reinterpret_cast<ReleaseCallback*>(c_struct->private_data)->Release(c_struct);
  }

  void Release(struct ArrowArray* c_struct) {
    ASSERT_FALSE(called_) << "ReleaseCallback called twice";
    called_ = true;
    ASSERT_NE(c_struct->format, nullptr)
        << "ReleaseCallback called with released ArrowArray";
    // Call original release callback
    c_struct->release = orig_release_;
    c_struct->private_data = orig_private_data_;
    if (c_struct->release != nullptr) {
      c_struct->release(c_struct);
    }
  }

  void AssertCalled() { ASSERT_TRUE(called_) << "ReleaseCallback was not called"; }

  void AssertNotCalled() { ASSERT_FALSE(called_) << "ReleaseCallback was called"; }

 private:
  ARROW_DISALLOW_COPY_AND_ASSIGN(ReleaseCallback);

  bool called_;
  void (*orig_release_)(struct ArrowArray*);
  void* orig_private_data_;
};

////////////////////////////////////////////////////////////////////////////
// Export tests

static constexpr int64_t kDefaultFlags = ARROW_FLAG_NULLABLE;

struct ExportChecker {
  ExportChecker(std::vector<std::string> flattened_formats,
                std::vector<std::string> flattened_names,
                std::vector<int64_t> flattened_flags = {})
      : flattened_formats_(std::move(flattened_formats)),
        flattened_names_(std::move(flattened_names)),
        flattened_flags_(
            flattened_flags.empty()
                ? std::vector<int64_t>(flattened_formats_.size(), kDefaultFlags)
                : std::move(flattened_flags)),
        flattened_index_(0) {}

  void operator()(struct ArrowArray* c_export, const ArrayData& expected_data,
                  bool inner = false) {
    ASSERT_LT(flattened_index_, flattened_formats_.size());
    ASSERT_LT(flattened_index_, flattened_names_.size());
    ASSERT_LT(flattened_index_, flattened_flags_.size());
    ASSERT_EQ(std::string(c_export->format), flattened_formats_[flattened_index_]);
    ASSERT_EQ(std::string(c_export->name), flattened_names_[flattened_index_]);
    ASSERT_EQ(c_export->metadata, nullptr);
    ASSERT_EQ(c_export->flags, flattened_flags_[flattened_index_]);
    ++flattened_index_;

    ASSERT_EQ(c_export->length, expected_data.length);
    ASSERT_EQ(c_export->null_count, expected_data.null_count);
    ASSERT_EQ(c_export->offset, expected_data.offset);

    ASSERT_EQ(c_export->n_buffers, static_cast<int64_t>(expected_data.buffers.size()));
    ASSERT_EQ(c_export->n_children,
              static_cast<int64_t>(expected_data.child_data.size()));
    ASSERT_NE(c_export->buffers, nullptr);
    for (int64_t i = 0; i < c_export->n_buffers; ++i) {
      auto expected_ptr =
          expected_data.buffers[i] ? expected_data.buffers[i]->data() : nullptr;
      ASSERT_EQ(c_export->buffers[i], expected_ptr);
    }

    if (expected_data.dictionary != nullptr) {
      // Recurse into dictionary
      ASSERT_NE(c_export->dictionary, nullptr);
      operator()(c_export->dictionary, *expected_data.dictionary->data(), true);
    } else {
      ASSERT_EQ(c_export->dictionary, nullptr);
    }

    if (c_export->n_children > 0) {
      ASSERT_NE(c_export->children, nullptr);
      // Recurse into children
      for (int64_t i = 0; i < c_export->n_children; ++i) {
        ASSERT_NE(c_export->children[i], nullptr);
        operator()(c_export->children[i], *expected_data.child_data[i], true);
      }
    } else {
      ASSERT_EQ(c_export->children, nullptr);
    }

    if (!inner) {
      // Caller gave the right number of names and format strings
      ASSERT_EQ(flattened_index_, flattened_formats_.size());
      ASSERT_EQ(flattened_index_, flattened_names_.size());
      ASSERT_EQ(flattened_index_, flattened_flags_.size());
    }
  }

  const std::vector<std::string> flattened_formats_;
  const std::vector<std::string> flattened_names_;
  std::vector<int64_t> flattened_flags_;
  size_t flattened_index_;
};

class TestExport : public ::testing::Test {
 public:
  void SetUp() override { pool_ = default_memory_pool(); }

  static std::function<Status(std::shared_ptr<Array>*)> JSONArrayFactory(
      std::shared_ptr<DataType> type, const char* json) {
    return [=](std::shared_ptr<Array>* out) -> Status {
      return ::arrow::ipc::internal::json::ArrayFromJSON(type, json, out);
    };
  }

  template <typename ArrayFactory, typename ExportCheckFunc>
  void TestWithArrayFactory(ArrayFactory&& factory, ExportCheckFunc&& func) {
    auto orig_bytes = pool_->bytes_allocated();

    std::shared_ptr<Array> arr;
    ASSERT_OK(factory(&arr));
    const ArrayData& data = *arr->data();  // non-owning reference
    struct ArrowArray c_export;
    ASSERT_OK(ExportArray(*arr, &c_export));

    ExportGuard guard(&c_export);
    auto new_bytes = pool_->bytes_allocated();
    ASSERT_GT(new_bytes, orig_bytes);

    // Release the shared_ptr<Array>, underlying data should be held alive
    arr.reset();
    ASSERT_EQ(pool_->bytes_allocated(), new_bytes);
    func(&c_export, data);

    // Release the ArrowArray, underlying data should be destroyed
    guard.Release();
    ASSERT_EQ(pool_->bytes_allocated(), orig_bytes);
  }

  template <typename ArrayFactory>
  void TestNested(ArrayFactory&& factory, std::vector<std::string> flattened_formats,
                  std::vector<std::string> flattened_names,
                  std::vector<int64_t> flattened_flags = {}) {
    ExportChecker checker(std::move(flattened_formats), std::move(flattened_names),
                          std::move(flattened_flags));

    TestWithArrayFactory(std::move(factory), checker);
  }

  void TestNested(const std::shared_ptr<DataType>& type, const char* json,
                  std::vector<std::string> flattened_formats,
                  std::vector<std::string> flattened_names,
                  std::vector<int64_t> flattened_flags = {}) {
    TestNested(JSONArrayFactory(type, json), std::move(flattened_formats),
               std::move(flattened_names), std::move(flattened_flags));
  }

  template <typename ArrayFactory>
  void TestPrimitive(ArrayFactory&& factory, const char* format) {
    TestNested(std::forward<ArrayFactory>(factory), {format}, {""});
  }

  void TestPrimitive(const std::shared_ptr<DataType>& type, const char* json,
                     const char* format) {
    TestNested(type, json, {format}, {""});
  }

  template <typename ArrayFactory, typename ExportCheckFunc>
  void TestMoveWithArrayFactory(ArrayFactory&& factory, ExportCheckFunc&& func) {
    auto orig_bytes = pool_->bytes_allocated();

    std::shared_ptr<Array> arr;
    ASSERT_OK(factory(&arr));
    const ArrayData& data = *arr->data();  // non-owning reference
    struct ArrowArray c_export_temp, c_export_final;
    ASSERT_OK(ExportArray(*arr, &c_export_temp));

    // Move the ArrowArray to its final location
    ArrowMoveArray(&c_export_temp, &c_export_final);
    ASSERT_EQ(c_export_temp.format, nullptr);  // released

    ExportGuard guard(&c_export_final);
    auto new_bytes = pool_->bytes_allocated();
    ASSERT_GT(new_bytes, orig_bytes);

    // Release the shared_ptr<Array>, underlying data should be held alive
    arr.reset();
    ASSERT_EQ(pool_->bytes_allocated(), new_bytes);
    func(&c_export_final, data);

    // Release the ArrowArray, underlying data should be destroyed
    guard.Release();
    ASSERT_EQ(pool_->bytes_allocated(), orig_bytes);
  }

  template <typename ArrayFactory>
  void TestMoveNested(ArrayFactory&& factory, std::vector<std::string> flattened_formats,
                      std::vector<std::string> flattened_names) {
    ExportChecker checker(std::move(flattened_formats), std::move(flattened_names));

    TestMoveWithArrayFactory(std::move(factory), checker);
  }

  void TestMoveNested(const std::shared_ptr<DataType>& type, const char* json,
                      std::vector<std::string> flattened_formats,
                      std::vector<std::string> flattened_names) {
    TestMoveNested(JSONArrayFactory(type, json), std::move(flattened_formats),
                   std::move(flattened_names));
  }

  void TestMovePrimitive(const std::shared_ptr<DataType>& type, const char* json,
                         const char* format) {
    TestMoveNested(type, json, {format}, {""});
  }

  template <typename ArrayFactory, typename ExportCheckFunc>
  void TestMoveChildWithArrayFactory(ArrayFactory&& factory, int64_t child_id,
                                     ExportCheckFunc&& func) {
    auto orig_bytes = pool_->bytes_allocated();

    std::shared_ptr<Array> arr;
    ASSERT_OK(factory(&arr));
    struct ArrowArray c_export_parent, c_export_child;
    ASSERT_OK(ExportArray(*arr, &c_export_parent));

    auto bytes_with_parent = pool_->bytes_allocated();
    ASSERT_GT(bytes_with_parent, orig_bytes);

    // Move the child ArrowArray to its final location
    {
      ExportGuard parent_guard(&c_export_parent);
      ASSERT_LT(child_id, c_export_parent.n_children);
      ArrowMoveArray(c_export_parent.children[child_id], &c_export_child);
    }
    ExportGuard child_guard(&c_export_child);

    // Now parent is released
    ASSERT_EQ(c_export_parent.format, nullptr);
    auto bytes_with_child = pool_->bytes_allocated();
    ASSERT_LT(bytes_with_child, bytes_with_parent);
    ASSERT_GT(bytes_with_child, orig_bytes);

    // Release the shared_ptr<Array>, some underlying data should be held alive
    const ArrayData& data = *arr->data()->child_data[child_id];  // non-owning reference
    arr.reset();
    ASSERT_LT(pool_->bytes_allocated(), bytes_with_child);
    ASSERT_GT(pool_->bytes_allocated(), orig_bytes);
    func(&c_export_child, data);

    // Release the ArrowArray, underlying data should be destroyed
    child_guard.Release();
    ASSERT_EQ(pool_->bytes_allocated(), orig_bytes);
  }

  template <typename ArrayFactory>
  void TestMoveChild(ArrayFactory&& factory, int64_t child_id,
                     std::vector<std::string> flattened_formats,
                     std::vector<std::string> flattened_names) {
    ExportChecker checker(std::move(flattened_formats), std::move(flattened_names));

    TestMoveChildWithArrayFactory(std::move(factory), child_id, checker);
  }

  void TestMoveChild(const std::shared_ptr<DataType>& type, const char* json,
                     int64_t child_id, std::vector<std::string> flattened_formats,
                     std::vector<std::string> flattened_names) {
    TestMoveChild(JSONArrayFactory(type, json), child_id, std::move(flattened_formats),
                  std::move(flattened_names));
  }

 protected:
  MemoryPool* pool_;
};

TEST_F(TestExport, Primitive) {
  TestPrimitive(int8(), "[1, 2, null, -3]", "c");
  TestPrimitive(int16(), "[1, 2, -3]", "s");
  TestPrimitive(int32(), "[1, 2, null, -3]", "i");
  TestPrimitive(int64(), "[1, 2, -3]", "l");
  TestPrimitive(uint8(), "[1, 2, 3]", "C");
  TestPrimitive(uint16(), "[1, 2, null, 3]", "S");
  TestPrimitive(uint32(), "[1, 2, 3]", "I");
  TestPrimitive(uint64(), "[1, 2, null, 3]", "L");

  TestPrimitive(boolean(), "[true, false, null]", "b");
  TestPrimitive(null(), "[null, null]", "n");

  TestPrimitive(float32(), "[1.5, null]", "f");
  TestPrimitive(float64(), "[1.5, null]", "g");

  TestPrimitive(fixed_size_binary(3), R"(["foo", "bar", null])", "w:3");
  TestPrimitive(binary(), R"(["foo", "bar", null])", "z");
  TestPrimitive(large_binary(), R"(["foo", "bar", null])", "Z");
  TestPrimitive(utf8(), R"(["foo", "bar", null])", "u");
  TestPrimitive(large_utf8(), R"(["foo", "bar", null])", "U");

  TestPrimitive(decimal(16, 4), R"(["1234.5670", null])", "d:16,4");
}

TEST_F(TestExport, PrimitiveSliced) {
  auto factory = [](std::shared_ptr<Array>* out) -> Status {
    *out = ArrayFromJSON(int16(), "[1, 2, null, -3]")->Slice(1, 2);
    return Status::OK();
  };

  TestPrimitive(factory, "s");
}

TEST_F(TestExport, Null) {
  TestPrimitive(null(), "[null, null, null]", "n");
  TestPrimitive(null(), "[]", "n");
}

TEST_F(TestExport, List) {
  TestNested(list(int8()), "[[1, 2], [3, null], null]", {"+l", "c"}, {"", "item"});
  TestNested(large_list(uint16()), "[[1, 2], [3, null], null]", {"+L", "S"},
             {"", "item"});
  TestNested(fixed_size_list(int64(), 2), "[[1, 2], [3, null], null]", {"+w:2", "l"},
             {"", "item"});

  TestNested(list(large_list(int32())), "[[[1, 2], [3], null], null]", {"+l", "+L", "i"},
             {"", "item", "item"});
}

TEST_F(TestExport, ListSliced) {
  {
    auto factory = [](std::shared_ptr<Array>* out) -> Status {
      *out = ArrayFromJSON(list(int8()), "[[1, 2], [3, null], [4, 5, 6], null]")
                 ->Slice(1, 2);
      return Status::OK();
    };
    TestNested(factory, {"+l", "c"}, {"", "item"});
  }
  {
    auto factory = [](std::shared_ptr<Array>* out) -> Status {
      auto values = ArrayFromJSON(int16(), "[1, 2, 3, 4, null, 5, 6, 7, 8]")->Slice(1, 6);
      auto offsets = ArrayFromJSON(int32(), "[0, 2, 3, 5, 6]")->Slice(2, 4);
      return ListArray::FromArrays(*offsets, *values, default_memory_pool(), out);
    };
    TestNested(factory, {"+l", "s"}, {"", "item"});
  }
}

TEST_F(TestExport, Struct) {
  const char* data = R"([[1, "foo"], [2, null]])";
  auto type = struct_({field("a", int8()), field("b", utf8())});
  TestNested(type, data, {"+s", "c", "u"}, {"", "a", "b"},
             {ARROW_FLAG_NULLABLE, ARROW_FLAG_NULLABLE, ARROW_FLAG_NULLABLE});

  type = struct_({field("a", int8(), /*nullable=*/false), field("b", utf8())});
  TestNested(type, data, {"+s", "c", "u"}, {"", "a", "b"},
             {ARROW_FLAG_NULLABLE, 0, ARROW_FLAG_NULLABLE});
}

TEST_F(TestExport, Map) {
  TestNested(map(int8(), utf8()), R"([[[1, "foo"], [2, null]], [[3, "bar"]]])",
             {"+m", "+s", "c", "u"}, {"", "entries", "key", "value"},
             {ARROW_FLAG_NULLABLE, 0, 0, ARROW_FLAG_NULLABLE});
}

TEST_F(TestExport, Union) {
  const char* data = "[null, [42, 1], [43, true], [42, null], [42, 2]]";
  // Dense
  auto field_a = field("a", int8());
  auto field_b = field("b", boolean(), /*nullable=*/false);
  auto type = union_({field_a, field_b}, {42, 43}, UnionMode::DENSE);
  TestNested(type, data, {"+ud:42,43", "c", "b"}, {"", "a", "b"},
             {ARROW_FLAG_NULLABLE, ARROW_FLAG_NULLABLE, 0});
  // Sparse
  field_a = field("a", int8(), /*nullable=*/false);
  field_b = field("b", boolean());
  type = union_({field_a, field_b}, {42, 43}, UnionMode::SPARSE);
  TestNested(type, data, {"+us:42,43", "c", "b"}, {"", "a", "b"},
             {ARROW_FLAG_NULLABLE, 0, ARROW_FLAG_NULLABLE});
}

TEST_F(TestExport, Dictionary) {
  {
    auto factory = [](std::shared_ptr<Array>* out) -> Status {
      auto values = ArrayFromJSON(utf8(), R"(["foo", "bar", "quux"])");
      auto indices = ArrayFromJSON(int32(), "[0, 2, 1, null, 1]");
      return DictionaryArray::FromArrays(dictionary(indices->type(), values->type()),
                                         indices, values, out);
    };
    TestNested(factory, {"i", "u"}, {"", ""});
  }
  {
    auto factory = [](std::shared_ptr<Array>* out) -> Status {
      auto values = ArrayFromJSON(list(utf8()), R"([["abc", "def"], ["efg"], []])");
      auto indices = ArrayFromJSON(int32(), "[0, 2, 1, null, 1]");
      return DictionaryArray::FromArrays(
          dictionary(indices->type(), values->type(), /*ordered=*/true), indices, values,
          out);
    };
    TestNested(factory, {"i", "+l", "u"}, {"", "", "item"},
               {ARROW_FLAG_NULLABLE | ARROW_FLAG_ORDERED, ARROW_FLAG_NULLABLE,
                ARROW_FLAG_NULLABLE});
  }
  {
    auto factory = [](std::shared_ptr<Array>* out) -> Status {
      auto values = ArrayFromJSON(list(utf8()), R"([["abc", "def"], ["efg"], []])");
      auto indices = ArrayFromJSON(int32(), "[0, 2, 1, null, 1]");
      std::shared_ptr<Array> dict_array;
      RETURN_NOT_OK(DictionaryArray::FromArrays(
          dictionary(indices->type(), values->type()), indices, values, &dict_array));
      auto offsets = ArrayFromJSON(int64(), "[0, 2, 5]");
      RETURN_NOT_OK(
          LargeListArray::FromArrays(*offsets, *dict_array, default_memory_pool(), out));
      return (*out)->Validate();
    };
    TestNested(factory, {"+L", "i", "+l", "u"}, {"", "item", "", "item"});
  }
}

TEST_F(TestExport, MovePrimitive) {
  TestMovePrimitive(int8(), "[1, 2, null, -3]", "c");
  TestMovePrimitive(fixed_size_binary(3), R"(["foo", "bar", null])", "w:3");
  TestMovePrimitive(binary(), R"(["foo", "bar", null])", "z");
}

TEST_F(TestExport, MoveNested) {
  TestMoveNested(list(int8()), "[[1, 2], [3, null], null]", {"+l", "c"}, {"", "item"});
  TestMoveNested(list(large_list(int32())), "[[[1, 2], [3], null], null]",
                 {"+l", "+L", "i"}, {"", "item", "item"});
  TestMoveNested(struct_({field("a", int8()), field("b", utf8())}),
                 R"([[1, "foo"], [2, null]])", {"+s", "c", "u"}, {"", "a", "b"});
}

TEST_F(TestExport, MoveDictionary) {
  {
    auto factory = [](std::shared_ptr<Array>* out) -> Status {
      auto values = ArrayFromJSON(utf8(), R"(["foo", "bar", "quux"])");
      auto indices = ArrayFromJSON(int32(), "[0, 2, 1, null, 1]");
      return DictionaryArray::FromArrays(dictionary(indices->type(), values->type()),
                                         indices, values, out);
    };
    TestMoveNested(factory, {"i", "u"}, {"", ""});
  }
  {
    auto factory = [](std::shared_ptr<Array>* out) -> Status {
      auto values = ArrayFromJSON(list(utf8()), R"([["abc", "def"], ["efg"], []])");
      auto indices = ArrayFromJSON(int32(), "[0, 2, 1, null, 1]");
      std::shared_ptr<Array> dict_array;
      RETURN_NOT_OK(DictionaryArray::FromArrays(
          dictionary(indices->type(), values->type()), indices, values, &dict_array));
      auto offsets = ArrayFromJSON(int64(), "[0, 2, 5]");
      RETURN_NOT_OK(
          LargeListArray::FromArrays(*offsets, *dict_array, default_memory_pool(), out));
      return (*out)->Validate();
    };
    TestMoveNested(factory, {"+L", "i", "+l", "u"}, {"", "item", "", "item"});
  }
}

TEST_F(TestExport, MoveChild) {
  TestMoveChild(list(int8()), "[[1, 2], [3, null], null]", /*child_id=*/0, {"c"},
                {"item"});
  TestMoveChild(list(large_list(int32())), "[[[1, 2], [3], null], null]",
                /*child_id=*/0, {"+L", "i"}, {"item", "item"});
  TestMoveChild(struct_({field("ints", int8()), field("strs", utf8())}),
                R"([[1, "foo"], [2, null]])",
                /*child_id=*/0, {"c"}, {"ints"});
  TestMoveChild(struct_({field("ints", int8()), field("strs", utf8())}),
                R"([[1, "foo"], [2, null]])",
                /*child_id=*/1, {"u"}, {"strs"});
  {
    auto factory = [](std::shared_ptr<Array>* out) -> Status {
      auto values = ArrayFromJSON(list(utf8()), R"([["abc", "def"], ["efg"], []])");
      auto indices = ArrayFromJSON(int32(), "[0, 2, 1, null, 1]");
      std::shared_ptr<Array> dict_array;
      RETURN_NOT_OK(DictionaryArray::FromArrays(
          dictionary(indices->type(), values->type()), indices, values, &dict_array));
      auto offsets = ArrayFromJSON(int64(), "[0, 2, 5]");
      RETURN_NOT_OK(
          LargeListArray::FromArrays(*offsets, *dict_array, default_memory_pool(), out));
      return (*out)->Validate();
    };
    TestMoveChild(factory, /*child_id=*/0, {"i", "+l", "u"}, {"item", "", "item"});
  }
}

////////////////////////////////////////////////////////////////////////////
// Import tests

// [true, false, true, true, false, true, true, true] * 2
static const uint8_t bits_buffer1[] = {0xed, 0xed};

static const void* buffers_no_nulls_no_data[1] = {nullptr};
static const void* buffers_nulls_no_data1[1] = {bits_buffer1};

static const uint8_t data_buffer1[] = {1, 2,  3,  4,  5,  6,  7,  8,
                                       9, 10, 11, 12, 13, 14, 15, 16};
static const uint8_t data_buffer2[] = "abcdefghijklmnopqrstuvwxyz";
static const uint64_t data_buffer3[] = {123456789, 0, 987654321, 0};
static const uint8_t data_buffer4[] = {1, 2, 0, 1, 3, 0};
static const float data_buffer5[] = {0.0f, 1.5f, -2.0f, 3.0f, 4.0f, 5.0f};
static const double data_buffer6[] = {0.0, 1.5, -2.0, 3.0, 4.0, 5.0};
static const void* primitive_buffers_no_nulls1[2] = {nullptr, data_buffer1};
static const void* primitive_buffers_nulls1[2] = {bits_buffer1, data_buffer1};
static const void* primitive_buffers_no_nulls2[2] = {nullptr, data_buffer2};
static const void* primitive_buffers_no_nulls3[2] = {nullptr, data_buffer3};
static const void* primitive_buffers_no_nulls4[2] = {nullptr, data_buffer4};
static const void* primitive_buffers_no_nulls5[2] = {nullptr, data_buffer5};
static const void* primitive_buffers_no_nulls6[2] = {nullptr, data_buffer6};

static const uint8_t string_data_buffer1[] = "foobarquux";

static const int32_t string_offsets_buffer1[] = {0, 3, 3, 6, 10};
static const void* string_buffers_no_nulls1[3] = {nullptr, string_offsets_buffer1,
                                                  string_data_buffer1};

static const int64_t large_string_offsets_buffer1[] = {0, 3, 3, 6, 10};
static const void* large_string_buffers_no_nulls1[3] = {
    nullptr, large_string_offsets_buffer1, string_data_buffer1};

static const int32_t list_offsets_buffer1[] = {0, 2, 2, 5, 6, 8};
static const void* list_buffers_no_nulls1[2] = {nullptr, list_offsets_buffer1};

static const int64_t large_list_offsets_buffer1[] = {0, 2, 2, 5, 6, 8};
static const void* large_list_buffers_no_nulls1[2] = {nullptr,
                                                      large_list_offsets_buffer1};

static const uint8_t type_codes_buffer1[] = {42, 42, 43, 43, 42};
static const int32_t union_offsets_buffer1[] = {0, 1, 0, 1, 2};
static const void* sparse_union_buffers_no_nulls1[3] = {nullptr, type_codes_buffer1,
                                                        nullptr};
static const void* dense_union_buffers_no_nulls1[3] = {nullptr, type_codes_buffer1,
                                                       union_offsets_buffer1};

class TestImport : public ::testing::Test {
 public:
  void SetUp() override {
    memset(&c_struct_, 0, sizeof(c_struct_));
    c_struct_.name = "";
  }

  // Create a new ArrowArray struct with a stable C pointer
  struct ArrowArray* AddChild() {
    nested_structs_.emplace_back();
    struct ArrowArray* result = &nested_structs_.back();
    memset(result, 0, sizeof(*result));
    return result;
  }

  // Create a stable C pointer to the N last structs in nested_structs_
  struct ArrowArray** NLastChildren(int64_t n_children, struct ArrowArray* parent) {
    children_arrays_.emplace_back(n_children);
    struct ArrowArray** children = children_arrays_.back().data();
    int64_t nested_offset;
    // If parent is itself at the end of nested_structs_, skip it
    if (parent != nullptr && &nested_structs_.back() == parent) {
      nested_offset = static_cast<int64_t>(nested_structs_.size()) - n_children - 1;
    } else {
      nested_offset = static_cast<int64_t>(nested_structs_.size()) - n_children;
    }
    for (int64_t i = 0; i < n_children; ++i) {
      children[i] = &nested_structs_[nested_offset + i];
    }
    return children;
  }

  struct ArrowArray* LastChild(struct ArrowArray* parent) {
    return *NLastChildren(1, parent);
  }

  void FillPrimitive(struct ArrowArray* c, const char* format, int64_t length,
                     int64_t null_count, int64_t offset, const void** buffers,
                     int64_t flags = kDefaultFlags) {
    c->flags = flags;
    c->format = format;
    c->length = length;
    c->null_count = null_count;
    c->offset = offset;
    c->n_buffers = 2;
    c->buffers = buffers;
  }

  void FillDictionary(struct ArrowArray* c) { c->dictionary = LastChild(c); }

  void FillStringLike(struct ArrowArray* c, const char* format, int64_t length,
                      int64_t null_count, int64_t offset, const void** buffers,
                      int64_t flags = kDefaultFlags) {
    c->flags = flags;
    c->format = format;
    c->length = length;
    c->null_count = null_count;
    c->offset = offset;
    c->n_buffers = 3;
    c->buffers = buffers;
  }

  void FillListLike(struct ArrowArray* c, const char* format, int64_t length,
                    int64_t null_count, int64_t offset, const void** buffers,
                    int64_t flags = kDefaultFlags) {
    c->flags = flags;
    c->format = format;
    c->length = length;
    c->null_count = null_count;
    c->offset = offset;
    c->n_buffers = 2;
    c->buffers = buffers;
    c->n_children = 1;
    c->children = NLastChildren(1, c);
    c->children[0]->name = "item";
  }

  void FillFixedSizeListLike(struct ArrowArray* c, const char* format, int64_t length,
                             int64_t null_count, int64_t offset, const void** buffers,
                             int64_t flags = kDefaultFlags) {
    c->flags = flags;
    c->format = format;
    c->length = length;
    c->null_count = null_count;
    c->offset = offset;
    c->n_buffers = 1;
    c->buffers = buffers;
    c->n_children = 1;
    c->children = NLastChildren(1, c);
    c->children[0]->name = "item";
  }

  void FillStructLike(struct ArrowArray* c, const char* format, int64_t length,
                      int64_t null_count, int64_t offset,
                      std::vector<std::string> child_names, const void** buffers,
                      int64_t flags = kDefaultFlags) {
    c->flags = flags;
    c->format = format;
    c->length = length;
    c->null_count = null_count;
    c->offset = offset;
    c->n_buffers = 1;
    c->buffers = buffers;
    c->n_children = static_cast<int64_t>(child_names.size());
    c->children = NLastChildren(c->n_children, c);
    for (int64_t i = 0; i < c->n_children; ++i) {
      children_names_.push_back(std::move(child_names[i]));
      c->children[i]->name = children_names_.back().c_str();
    }
  }

  void FillUnionLike(struct ArrowArray* c, const char* format, int64_t length,
                     int64_t null_count, int64_t offset,
                     std::vector<std::string> child_names, const void** buffers,
                     int64_t flags = kDefaultFlags) {
    c->flags = flags;
    c->format = format;
    c->length = length;
    c->null_count = null_count;
    c->offset = offset;
    c->n_buffers = 3;
    c->buffers = buffers;
    c->n_children = static_cast<int64_t>(child_names.size());
    c->children = NLastChildren(c->n_children, c);
    for (int64_t i = 0; i < c->n_children; ++i) {
      children_names_.push_back(std::move(child_names[i]));
      c->children[i]->name = children_names_.back().c_str();
    }
  }

  void FillPrimitive(const char* format, int64_t length, int64_t null_count,
                     int64_t offset, const void** buffers,
                     int64_t flags = kDefaultFlags) {
    FillPrimitive(&c_struct_, format, length, null_count, offset, buffers, flags);
  }

  void FillDictionary() { FillDictionary(&c_struct_); }

  void FillStringLike(const char* format, int64_t length, int64_t null_count,
                      int64_t offset, const void** buffers,
                      int64_t flags = kDefaultFlags) {
    FillStringLike(&c_struct_, format, length, null_count, offset, buffers, flags);
  }

  void FillListLike(const char* format, int64_t length, int64_t null_count,
                    int64_t offset, const void** buffers, int64_t flags = kDefaultFlags) {
    FillListLike(&c_struct_, format, length, null_count, offset, buffers, flags);
  }

  void FillFixedSizeListLike(const char* format, int64_t length, int64_t null_count,
                             int64_t offset, const void** buffers,
                             int64_t flags = kDefaultFlags) {
    FillFixedSizeListLike(&c_struct_, format, length, null_count, offset, buffers, flags);
  }

  void FillStructLike(const char* format, int64_t length, int64_t null_count,
                      int64_t offset, std::vector<std::string> child_names,
                      const void** buffers, int64_t flags = kDefaultFlags) {
    FillStructLike(&c_struct_, format, length, null_count, offset, std::move(child_names),
                   buffers, flags);
  }

  void FillUnionLike(const char* format, int64_t length, int64_t null_count,
                     int64_t offset, std::vector<std::string> child_names,
                     const void** buffers, int64_t flags = kDefaultFlags) {
    FillUnionLike(&c_struct_, format, length, null_count, offset, std::move(child_names),
                  buffers, flags);
  }

  void CheckImport(const std::shared_ptr<Array>& expected) {
    ReleaseCallback cb(&c_struct_);

    std::shared_ptr<Array> array;
    ASSERT_OK(ImportArray(&c_struct_, &array));
    ASSERT_TRUE(ArrowIsReleased(&c_struct_));  // was moved
    ASSERT_OK(array->Validate());
    // Special case: Null array doesn't have any data, so it needn't
    // keep the ArrowArray struct alive.
    if (expected->type_id() != Type::NA) {
      cb.AssertNotCalled();
    }
    AssertArraysEqual(*expected, *array, true);
    array.reset();
    cb.AssertCalled();
  }

  void CheckImportError() {
    ReleaseCallback cb(&c_struct_);

    std::shared_ptr<Array> array;
    ASSERT_RAISES(Invalid, ImportArray(&c_struct_, &array));
    ASSERT_TRUE(ArrowIsReleased(&c_struct_));  // was moved
    // The ArrowArray should have been released.
    cb.AssertCalled();
  }

 protected:
  struct ArrowArray c_struct_;
  // Deque elements don't move when the deque is appended to, which allows taking
  // stable C pointers to them.
  std::deque<struct ArrowArray> nested_structs_;
  std::deque<std::vector<struct ArrowArray*>> children_arrays_;
  std::deque<std::string> children_names_;
};

TEST_F(TestImport, Primitive) {
  FillPrimitive("c", 3, 0, 0, primitive_buffers_no_nulls1);
  CheckImport(ArrayFromJSON(int8(), "[1, 2, 3]"));
  FillPrimitive("C", 5, 0, 0, primitive_buffers_no_nulls1);
  CheckImport(ArrayFromJSON(uint8(), "[1, 2, 3, 4, 5]"));
  FillPrimitive("s", 3, 0, 0, primitive_buffers_no_nulls1);
  CheckImport(ArrayFromJSON(int16(), "[513, 1027, 1541]"));
  FillPrimitive("S", 3, 0, 0, primitive_buffers_no_nulls1);
  CheckImport(ArrayFromJSON(uint16(), "[513, 1027, 1541]"));
  FillPrimitive("i", 2, 0, 0, primitive_buffers_no_nulls1);
  CheckImport(ArrayFromJSON(int32(), "[67305985, 134678021]"));
  FillPrimitive("I", 2, 0, 0, primitive_buffers_no_nulls1);
  CheckImport(ArrayFromJSON(uint32(), "[67305985, 134678021]"));
  FillPrimitive("l", 2, 0, 0, primitive_buffers_no_nulls1);
  CheckImport(ArrayFromJSON(int64(), "[578437695752307201, 1157159078456920585]"));
  FillPrimitive("L", 2, 0, 0, primitive_buffers_no_nulls1);
  CheckImport(ArrayFromJSON(uint64(), "[578437695752307201, 1157159078456920585]"));

  FillPrimitive("b", 3, 0, 0, primitive_buffers_no_nulls1);
  CheckImport(ArrayFromJSON(boolean(), "[true, false, false]"));
  FillPrimitive("f", 6, 0, 0, primitive_buffers_no_nulls5);
  CheckImport(ArrayFromJSON(float32(), "[0.0, 1.5, -2.0, 3.0, 4.0, 5.0]"));
  FillPrimitive("g", 6, 0, 0, primitive_buffers_no_nulls6);
  CheckImport(ArrayFromJSON(float64(), "[0.0, 1.5, -2.0, 3.0, 4.0, 5.0]"));

  // With nulls
  FillPrimitive("c", 9, -1, 0, primitive_buffers_nulls1);
  CheckImport(ArrayFromJSON(int8(), "[1, null, 3, 4, null, 6, 7, 8, 9]"));
  FillPrimitive("c", 9, 2, 0, primitive_buffers_nulls1);
  CheckImport(ArrayFromJSON(int8(), "[1, null, 3, 4, null, 6, 7, 8, 9]"));
  FillPrimitive("b", 3, -1, 0, primitive_buffers_nulls1);
  CheckImport(ArrayFromJSON(boolean(), "[true, null, false]"));
  FillPrimitive("b", 3, 1, 0, primitive_buffers_nulls1);
  CheckImport(ArrayFromJSON(boolean(), "[true, null, false]"));
}

TEST_F(TestImport, Null) {
  const void* buffers[] = {nullptr};
  c_struct_.format = "n";
  c_struct_.length = 3;
  c_struct_.null_count = 3;
  c_struct_.offset = 0;
  c_struct_.n_buffers = 1;
  c_struct_.buffers = buffers;
  CheckImport(ArrayFromJSON(null(), "[null, null, null]"));
}

TEST_F(TestImport, PrimitiveWithOffset) {
  FillPrimitive("c", 3, 0, 2, primitive_buffers_no_nulls1);
  CheckImport(ArrayFromJSON(int8(), "[3, 4, 5]"));
  FillPrimitive("S", 3, 0, 1, primitive_buffers_no_nulls1);
  CheckImport(ArrayFromJSON(uint16(), "[1027, 1541, 2055]"));

  FillPrimitive("b", 4, 0, 7, primitive_buffers_no_nulls1);
  CheckImport(ArrayFromJSON(boolean(), "[false, false, true, false]"));
}

TEST_F(TestImport, NullWithOffset) {
  const void* buffers[] = {nullptr};
  c_struct_.format = "n";
  c_struct_.length = 3;
  c_struct_.null_count = 3;
  c_struct_.offset = 5;
  c_struct_.n_buffers = 1;
  c_struct_.buffers = buffers;
  CheckImport(ArrayFromJSON(null(), "[null, null, null]"));
}

TEST_F(TestImport, String) {
  FillStringLike("u", 4, 0, 0, string_buffers_no_nulls1);
  CheckImport(ArrayFromJSON(utf8(), R"(["foo", "", "bar", "quux"])"));
  FillStringLike("z", 4, 0, 0, string_buffers_no_nulls1);
  CheckImport(ArrayFromJSON(binary(), R"(["foo", "", "bar", "quux"])"));
  FillStringLike("U", 4, 0, 0, large_string_buffers_no_nulls1);
  CheckImport(ArrayFromJSON(large_utf8(), R"(["foo", "", "bar", "quux"])"));
  FillStringLike("Z", 4, 0, 0, large_string_buffers_no_nulls1);
  CheckImport(ArrayFromJSON(large_binary(), R"(["foo", "", "bar", "quux"])"));

  FillPrimitive("w:3", 2, 0, 0, primitive_buffers_no_nulls2);
  CheckImport(ArrayFromJSON(fixed_size_binary(3), R"(["abc", "def"])"));
  FillPrimitive("d:15,4", 2, 0, 0, primitive_buffers_no_nulls3);
  CheckImport(ArrayFromJSON(decimal(15, 4), R"(["12345.6789", "98765.4321"])"));
}

TEST_F(TestImport, List) {
  FillPrimitive(AddChild(), "c", 8, 0, 0, primitive_buffers_no_nulls1);
  FillListLike("+l", 5, 0, 0, list_buffers_no_nulls1);
  CheckImport(ArrayFromJSON(list(int8()), "[[1, 2], [], [3, 4, 5], [6], [7, 8]]"));
  FillPrimitive(AddChild(), "s", 5, 0, 0, primitive_buffers_no_nulls1);
  FillListLike("+l", 3, 0, 0, list_buffers_no_nulls1);
  CheckImport(ArrayFromJSON(list(int16()), "[[513, 1027], [], [1541, 2055, 2569]]"));

  // Large list
  FillPrimitive(AddChild(), "s", 5, 0, 0, primitive_buffers_no_nulls1);
  FillListLike("+L", 3, 0, 0, large_list_buffers_no_nulls1);
  CheckImport(
      ArrayFromJSON(large_list(int16()), "[[513, 1027], [], [1541, 2055, 2569]]"));

  // Fixed-size list
  FillPrimitive(AddChild(), "c", 9, 0, 0, primitive_buffers_no_nulls1);
  FillFixedSizeListLike("+w:3", 3, 0, 0, buffers_no_nulls_no_data);
  CheckImport(
      ArrayFromJSON(fixed_size_list(int8(), 3), "[[1, 2, 3], [4, 5, 6], [7, 8, 9]]"));
}

TEST_F(TestImport, NestedList) {
  FillPrimitive(AddChild(), "c", 8, 0, 0, primitive_buffers_no_nulls1);
  FillListLike(AddChild(), "+l", 5, 0, 0, list_buffers_no_nulls1);
  FillListLike("+L", 3, 0, 0, large_list_buffers_no_nulls1);
  CheckImport(ArrayFromJSON(large_list(list(int8())),
                            "[[[1, 2], []], [], [[3, 4, 5], [6], [7, 8]]]"));

  FillPrimitive(AddChild(), "c", 6, 0, 0, primitive_buffers_no_nulls1);
  FillFixedSizeListLike(AddChild(), "+w:3", 2, 0, 0, buffers_no_nulls_no_data);
  FillListLike("+l", 2, 0, 0, list_buffers_no_nulls1);
  CheckImport(
      ArrayFromJSON(list(fixed_size_list(int8(), 3)), "[[[1, 2, 3], [4, 5, 6]], []]"));
}

TEST_F(TestImport, ListWithOffset) {
  // Offset in child
  FillPrimitive(AddChild(), "c", 8, 0, 1, primitive_buffers_no_nulls1);
  FillListLike("+l", 5, 0, 0, list_buffers_no_nulls1);
  CheckImport(ArrayFromJSON(list(int8()), "[[2, 3], [], [4, 5, 6], [7], [8, 9]]"));

  FillPrimitive(AddChild(), "c", 9, 0, 1, primitive_buffers_no_nulls1);
  FillFixedSizeListLike("+w:3", 3, 0, 0, buffers_no_nulls_no_data);
  CheckImport(
      ArrayFromJSON(fixed_size_list(int8(), 3), "[[2, 3, 4], [5, 6, 7], [8, 9, 10]]"));

  // Offset in parent
  FillPrimitive(AddChild(), "c", 8, 0, 0, primitive_buffers_no_nulls1);
  FillListLike("+l", 4, 0, 1, list_buffers_no_nulls1);
  CheckImport(ArrayFromJSON(list(int8()), "[[], [3, 4, 5], [6], [7, 8]]"));

  FillPrimitive(AddChild(), "c", 9, 0, 0, primitive_buffers_no_nulls1);
  FillFixedSizeListLike("+w:3", 3, 0, 1, buffers_no_nulls_no_data);
  CheckImport(
      ArrayFromJSON(fixed_size_list(int8(), 3), "[[4, 5, 6], [7, 8, 9], [10, 11, 12]]"));

  // Both
  FillPrimitive(AddChild(), "c", 8, 0, 2, primitive_buffers_no_nulls1);
  FillListLike("+l", 4, 0, 1, list_buffers_no_nulls1);
  CheckImport(ArrayFromJSON(list(int8()), "[[], [5, 6, 7], [8], [9, 10]]"));

  FillPrimitive(AddChild(), "c", 9, 0, 2, primitive_buffers_no_nulls1);
  FillFixedSizeListLike("+w:3", 3, 0, 1, buffers_no_nulls_no_data);
  CheckImport(ArrayFromJSON(fixed_size_list(int8(), 3),
                            "[[6, 7, 8], [9, 10, 11], [12, 13, 14]]"));
}

TEST_F(TestImport, Struct) {
  FillStringLike(AddChild(), "u", 3, 0, 0, string_buffers_no_nulls1);
  FillPrimitive(AddChild(), "S", 3, -1, 0, primitive_buffers_nulls1);
  FillStructLike("+s", 3, 0, 0, {"strs", "ints"}, buffers_no_nulls_no_data);
  auto expected = ArrayFromJSON(struct_({field("strs", utf8()), field("ints", uint16())}),
                                R"([["foo", 513], ["", null], ["bar", 1541]])");
  CheckImport(expected);

  FillStringLike(AddChild(), "u", 3, 0, 0, string_buffers_no_nulls1);
  FillPrimitive(AddChild(), "S", 3, 0, 0, primitive_buffers_no_nulls1);
  FillStructLike("+s", 3, -1, 0, {"strs", "ints"}, buffers_nulls_no_data1);
  expected = ArrayFromJSON(struct_({field("strs", utf8()), field("ints", uint16())}),
                           R"([["foo", 513], null, ["bar", 1541]])");
  CheckImport(expected);

  FillStringLike(AddChild(), "u", 3, 0, 0, string_buffers_no_nulls1, /*flags=*/0);
  FillPrimitive(AddChild(), "S", 3, 0, 0, primitive_buffers_no_nulls1);
  FillStructLike("+s", 3, -1, 0, {"strs", "ints"}, buffers_nulls_no_data1);
  expected = ArrayFromJSON(
      struct_({field("strs", utf8(), /*nullable=*/false), field("ints", uint16())}),
      R"([["foo", 513], null, ["bar", 1541]])");
  CheckImport(expected);
}

TEST_F(TestImport, Union) {
  // Sparse
  FillStringLike(AddChild(), "u", 3, 0, 0, string_buffers_no_nulls1);
  FillPrimitive(AddChild(), "c", 3, -1, 0, primitive_buffers_nulls1);
  FillUnionLike("+us:43,42", 4, 0, 0, {"strs", "ints"}, sparse_union_buffers_no_nulls1);
  auto type =
      union_({field("strs", utf8()), field("ints", int8())}, {43, 42}, UnionMode::SPARSE);
  auto expected =
      ArrayFromJSON(type, R"([[42, 1], [42, null], [43, "bar"], [43, "quux"]])");
  CheckImport(expected);

  // Dense
  FillStringLike(AddChild(), "u", 3, 0, 0, string_buffers_no_nulls1);
  FillPrimitive(AddChild(), "c", 3, -1, 0, primitive_buffers_nulls1);
  FillUnionLike("+ud:43,42", 5, 0, 0, {"strs", "ints"}, dense_union_buffers_no_nulls1);
  type =
      union_({field("strs", utf8()), field("ints", int8())}, {43, 42}, UnionMode::DENSE);
  expected =
      ArrayFromJSON(type, R"([[42, 1], [42, null], [43, "foo"], [43, ""], [42, 3]])");
  CheckImport(expected);
}

TEST_F(TestImport, StructWithOffset) {
  // Child
  FillStringLike(AddChild(), "u", 3, 0, 1, string_buffers_no_nulls1);
  FillPrimitive(AddChild(), "c", 3, 0, 2, primitive_buffers_no_nulls1);
  FillStructLike("+s", 3, 0, 0, {"strs", "ints"}, buffers_no_nulls_no_data);
  auto expected = ArrayFromJSON(struct_({field("strs", utf8()), field("ints", int8())}),
                                R"([["", 3], ["bar", 4], ["quux", 5]])");
  CheckImport(expected);

  // Parent and child
  FillStringLike(AddChild(), "u", 4, 0, 0, string_buffers_no_nulls1);
  FillPrimitive(AddChild(), "c", 4, 0, 2, primitive_buffers_no_nulls1);
  FillStructLike("+s", 3, 0, 1, {"strs", "ints"}, buffers_no_nulls_no_data);
  expected = ArrayFromJSON(struct_({field("strs", utf8()), field("ints", int8())}),
                           R"([["", 4], ["bar", 5], ["quux", 6]])");
  CheckImport(expected);
}

TEST_F(TestImport, Dictionary) {
  FillStringLike(AddChild(), "u", 4, 0, 0, string_buffers_no_nulls1);
  FillPrimitive("c", 6, 0, 0, primitive_buffers_no_nulls4);
  FillDictionary();

  auto dict_values = ArrayFromJSON(utf8(), R"(["foo", "", "bar", "quux"])");
  auto indices = ArrayFromJSON(int8(), "[1, 2, 0, 1, 3, 0]");
  std::shared_ptr<Array> expected;
  ASSERT_OK(DictionaryArray::FromArrays(dictionary(int8(), utf8()), indices, dict_values,
                                        &expected));
  CheckImport(expected);

  FillStringLike(AddChild(), "u", 4, 0, 0, string_buffers_no_nulls1);
  FillPrimitive("c", 6, 0, 0, primitive_buffers_no_nulls4,
                ARROW_FLAG_NULLABLE | ARROW_FLAG_ORDERED);
  FillDictionary();

  ASSERT_OK(DictionaryArray::FromArrays(dictionary(int8(), utf8(), /*ordered=*/true),
                                        indices, dict_values, &expected));
  CheckImport(expected);
}

TEST_F(TestImport, DictionaryWithOffset) {
  FillStringLike(AddChild(), "u", 3, 0, 1, string_buffers_no_nulls1);
  FillPrimitive("c", 3, 0, 0, primitive_buffers_no_nulls4);
  FillDictionary();

  auto dict_values = ArrayFromJSON(utf8(), R"(["", "bar", "quux"])");
  auto indices = ArrayFromJSON(int8(), "[1, 2, 0]");
  std::shared_ptr<Array> expected;
  ASSERT_OK(DictionaryArray::FromArrays(dictionary(int8(), utf8()), indices, dict_values,
                                        &expected));
  CheckImport(expected);

  FillStringLike(AddChild(), "u", 4, 0, 0, string_buffers_no_nulls1);
  FillPrimitive("c", 4, 0, 2, primitive_buffers_no_nulls4);
  FillDictionary();

  dict_values = ArrayFromJSON(utf8(), R"(["foo", "", "bar", "quux"])");
  indices = ArrayFromJSON(int8(), "[0, 1, 3, 0]");
  ASSERT_OK(DictionaryArray::FromArrays(dictionary(int8(), utf8()), indices, dict_values,
                                        &expected));
  CheckImport(expected);
}

TEST_F(TestImport, ErrorFormatString) {
  FillPrimitive("cc", 3, 0, 0, primitive_buffers_no_nulls1);
  CheckImportError();
  FillPrimitive("w3", 2, 0, 0, primitive_buffers_no_nulls2);
  CheckImportError();
  FillPrimitive("w:three", 2, 0, 0, primitive_buffers_no_nulls2);
  CheckImportError();
  FillPrimitive("w:3,5", 2, 0, 0, primitive_buffers_no_nulls2);
  CheckImportError();
  FillPrimitive("d:15", 2, 0, 0, primitive_buffers_no_nulls3);
  CheckImportError();
  FillPrimitive("d:15.4", 2, 0, 0, primitive_buffers_no_nulls3);
  CheckImportError();
}

TEST_F(TestImport, ErrorPrimitive) {
  // Bad number of buffers
  FillPrimitive("c", 3, 0, 0, primitive_buffers_no_nulls1);
  c_struct_.n_buffers = 1;
  CheckImportError();
  // Zero null bitmap but non-zero null_count
  FillPrimitive("c", 3, 1, 0, primitive_buffers_no_nulls1);
  CheckImportError();
}

TEST_F(TestImport, ErrorDictionary) {
  // Bad index type
  FillPrimitive(AddChild(), "c", 3, 0, 0, primitive_buffers_no_nulls4);
  FillStringLike("u", 3, 0, 1, string_buffers_no_nulls1);
  FillDictionary();
  CheckImportError();
}

// TODO roundtripping tests (C++ -> C -> C++ and C -> C++ -> C)

}  // namespace arrow
