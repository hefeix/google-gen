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

#include "execution.h"
#include "parser.h"
#include "element.h"

void OnSubscription::Init(Thread t, OTuple variable_tuple) {
  Blackboard::Subscription::Init
    (OTuple::Make(VariablesToWildcards(variable_tuple.Data())),
     t.execution_->blackboard_);
}

void OnSubscription::Update(OTuple tuple, OTime time) {
  Thread t = thread_;
  t.time_ = OTime::Make(max(t.time_.Data(), time.Data()) + BitSeq::Min());
  Map new_binding;
  if (!ComputeSubstitution(variable_tuple_.Data(), 
			   tuple.Data(), &new_binding)) return;
  t.binding_ = OMap::Make(Union(t.binding_.Data(), new_binding));
  t.execution_->Enqueue(t);
}
void Execution::ParseAndExecute(OTuple program_tuple) {
  vector<Element *> v = ParseElements(program_tuple.Data());
  forall(run, v) AddCodeTreeToRun(*run);
  ExecuteForever();
}

void Execution::ExecuteRunnableThreads() {
  CHECK(run_queue_.size());
  current_time_ = OTime::Make(run_queue_.begin()->first);
  const vector<Thread> & v = run_queue_.begin()->second;
  forall(run, v) {
    Thread t = *run;
    t.element_->Execute(t);
  }
  run_queue_.erase(run_queue_.begin());
}
