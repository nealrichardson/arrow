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

from __future__ import absolute_import

import sys

if sys.version_info < (3,):
    raise ImportError("Python Dataset bindings require Python 3")

from pyarrow._dataset import (  # noqa
    Dataset,
    DataSource,
    DataFragment,
    ParquetFileFormat,
    FileSource,
    FileFormat,
    Expression,
    SimpleDataFragment,
    ScanOptions,
    ScanTask,
    Scanner,
    ScannerBuilder,
    ScanOptions,
    ScanContext,
    SimpleDataSource,
    TreeDataSource,
    FileSystemDataSource,
    HivePartitionScheme,
    FileSystemDiscoveryOptions,
    FileSystemDataSourceDiscovery,
    SchemaPartitionScheme,
    PartitionScheme,
    ScalarExpression,
    FieldExpression,
    AndExpression,
    OrExpression,
    NotExpression,
    CompareOperator,
    ComparisonExpression,
)
