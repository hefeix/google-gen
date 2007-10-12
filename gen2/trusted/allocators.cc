// Copyright (C) 2007 Google Inc. and Georges Harik
// 
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// 
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//
// Author: Georges Harik and Noam Shazeer

#include "allocators.h"

LinearAllocator::LinearAllocator() {
  AddBlock();
}

void LinearAllocator::AddBlock() {
  blocks_.push_back(last_block_end_ = new int[LINEAR_ALLOCATOR_BLOCK_SIZE]);
}

void LinearAllocator::RemoveLastBlock() {
  CHECK(blocks_.size() > 1);
  delete [] blocks_[blocks_.size()-1];
  blocks_.pop_back();
}

void LinearAllocator::Clear() {
  while (blocks_.size() > 1) RemoveLastBlock();
  last_block_end_ = 0;
}
