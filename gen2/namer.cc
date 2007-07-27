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

#include "namer.h"
#include "changelist.h"
Namer::Namer() {
  index_.resize(Named::NUM_NAMED_TYPES);
}
Named * Namer::Lookup(Named::Type type, Object name) const {
  Named *const * look = Index(type) % name;
  if (look) return *look;
  return NULL;
}

void Named::L1_SetName(Object new_name) {
  Named ** clobbered = N.index_[GetType()] % new_name;
  if (clobbered) {
    // we should only be clobbering automatic names, which are all integers.
    CHECK(new_name.Type() == INTEGER);
    CL.RemoveFromMap(&N.index_[GetType()], new_name);
  }
  CL.ChangeValue(&name_, new_name);
  CL.InsertIntoMap(&N.index_[GetType()], new_name, this);
  if (clobbered) (*clobbered)->L1_AutomaticallyName();
}

void Named::L1_AutomaticallyName() {
  // do we care if the increment of next_name_ reverts??
  while (N.Index(GetType()) 
	 % (Object)Integer::Make(N.next_name_)) N.next_name_++;
  L1_SetName(Integer::Make(N.next_name_));
}

void Named::L1_Erase() {
  CL.RemoveFromMap(&N.index_[GetType()], name_);
  CL.Destroying(this);
}

void Named::Init() {
  CL.Creating(this);
  L1_AutomaticallyName();
}

