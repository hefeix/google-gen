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

#include "model.h"

Model::Model(){
  global_flake_chooser_ = new Chooser(NULL);
  global_uint_chooser_ = new UintChooser();
  ln_likelihood_ = 0;
  next_name_ = 0;
}

void Named::L1_SetName(Object new_name) {
  Named ** clobbered = M.name_index_ % new_name;
  if (clobbered) CL.RemoveFromMap(&M.name_index_, new_name);
  CL.ChangeValue(&name_, new_name);
  CL.InsertIntoMap(&m.name_index_, new_name, this);
  if (clobbered) (*clobbered)->L1_AutomaticallyName();
}

void Named::L1_AutomaticallyName() {
  while (name_index_ % Integer::Make(next_name_)) next_name_++;
  L1_SetName(Integer::Make(next_name_));
}

void Named::L1_Erase() {
  CL.RemoveFromMap(&M.name_index_, name_);
  CL.Destroying(this);
}

Named::Named() {
  CL.Creating(this);
  L1_AutomaticallyName();
}
