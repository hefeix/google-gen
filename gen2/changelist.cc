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

Changelist CL;

DestructibleCheckpoint::DestructibleCheckpoint(Changelist *cl){
  cl_ = cl;
  cp_ = cl_->GetCheckpoint();
}

DestructibleCheckpoint::~DestructibleCheckpoint(){
  cl_->Rollback(cp_);
}

string Changelist::ToString() const {
  ostringstream out;
  out << "Changelist" << endl;
  for (uint i=0; i<history_.size(); i++) 
    out << " " << i << " " << history_[i]->ToString() << endl;
  return out.str();
}
void Changelist::Make(Change * c){
  history_.push_back(c);
}
Checkpoint Changelist::GetCheckpoint(){
  return history_.size();
}
void Changelist::Rollback(Checkpoint cp){
  CHECK(history_.size() >= (uint)cp);
  while (history_.size() > (uint)cp){
    history_.back()->Undo();
    delete history_.back();
    history_.pop_back();
  }
}
void Changelist::MakeChangesPermanent(){
  for (uint i=0; i<history_.size(); i++) {
    history_[i]->MakePermanent();
    delete history_[i];
  }
  history_.clear();
}
