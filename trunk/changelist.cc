// Copyright (C) 2006 Google Inc. and Georges Harik
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
// Author: Noam Shazeer and Georges Harik

#include "changelist.h"

void Changelist::MakeChange(Change * c){
  history_.push_back(c);
}
Checkpoint Changelist::GetCheckpoint(){
  return history_.size();
}
void Changelist::Rollback(Checkpoint cp){
  CHECK(history_.size() >= cp);
  while (history_.size() > cp){
    delete history_.back();
    history_.pop_back();
  }
}