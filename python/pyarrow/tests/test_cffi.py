# -*- coding: utf-8 -*-
# Licensed to the Apache Software Foundation (ASF) under one
# or more contributor license agreements.  See the NOTICE file
# distributed with this work for additional information
# regarding copyright ownership.  The ASF licenses this file
# to you under the Apache License, Version 2.0 (the
# "License"); you may not use this file except in compliance
# with the License.  You may obtain a copy of the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing,
# software distributed under the License is distributed on an
# "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
# KIND, either express or implied.  See the License for the
# specific language governing permissions and limitations
# under the License.

import pyarrow as pa

# TODO move this into pyarrow.something
import cffi

c_source = """
    struct ArrowArray {
      // Type description
      const char* format;
      const char* name;
      const char* metadata;
      int64_t flags;

      // Data description
      int64_t length;
      int64_t null_count;
      int64_t offset;
      int64_t n_buffers;
      int64_t n_children;
      const void** buffers;
      struct ArrowArray** children;
      struct ArrowArray* dictionary;

      // Release callback
      void (*release)(struct ArrowArray*);
      // Opaque producer-specific data
      void* private_data;
    };
    """

ffi = cffi.FFI()
ffi.cdef(c_source)


def test_export_import():
    c = ffi.new("struct ArrowArray*")
    ptr = int(ffi.cast("uintptr_t", c))

    old_allocated = pa.total_allocated_bytes()

    arr = pa.array([1, 2, 42])
    assert pa.total_allocated_bytes() > old_allocated
    arr.export_to_c(ptr)
    del arr
    arr_new = pa.Array.import_from_c(ptr)
    assert arr_new.to_pylist() == [1, 2, 42]
    del arr_new
    assert pa.total_allocated_bytes() == old_allocated
