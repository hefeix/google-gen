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

string Thread::ToString() {
  string ret;
  ret += OMap::Make(binding_).ToString() + " ";
  ret += element_->ProgramTree() + " ";
  return ret;
}

void OnSubscription::Init(Thread t, const Tuple &  variable_tuple) {
  CHECK(t.element_);
  thread_ = t;
  variable_tuple_ = variable_tuple;
  Blackboard::Subscription::Init
    (VariablesToWildcards(variable_tuple),
     t.execution_->blackboard_);
}

void OnSubscription::Update(const Tuple & tuple) {
  Thread new_thread = thread_;
  VLOG(1)  << "Update " << tuple
	   << " Threadinfo " << new_thread.ToString() << endl;
  if (!ExtendSubstitution(variable_tuple_, 
			  tuple, &new_thread.binding_)) return;
  new_thread.execution_->Enqueue(new_thread, BitSeq::Min());
}
void Execution::ParseAndExecute(OTuple program_tuple) {
  vector<Element *> v = ParseElements(program_tuple.Data());
  forall(run, v) AddCodeTreeToRun(*run);
  ExecuteForever();
}

void Execution::ExecuteRunnableThreads() {
  CHECK(run_queue_.size());
  current_time_.Increment(run_queue_.begin()->first, 1);
  // we copy off and delete first to make delay statements work properly.
  const vector<Thread> v = run_queue_.begin()->second; 
  run_queue_.erase(run_queue_.begin());
  forall(run, v) {
    Thread t = *run;
    t.element_->Execute(t);
  }
}

void Execution::AddCodeTreeToRun(Element *top_element) {
  CHECK(top_element->VerifyTree());
  Thread t;
  t.element_ = top_element;
  t.execution_ = this;
  Enqueue(t, BitSeq::Min());
  top_elements_.push_back(top_element);
}

// huh - maybe we should move this to element.cc MatchBaseElement::whatever
Tuple Execution::MatchAndRun(Thread thread, const Tuple & variable_tuple) {
  // Immediately execute everything that currently matches
  Blackboard * bb = thread.execution_->blackboard_;
  vector<Map> results;
  Tuple output;
  bb->GetVariableMatches(variable_tuple, thread.binding_, &results);
  VLOG(1) << "MatchAndRun " << thread.ToString() << " " << variable_tuple
	  << " #results=" << results.size() << endl;
  forall(run, results){
    Thread new_thread = thread;
    new_thread.binding_ = *run;
    output.push_back(new_thread.element_->Execute(new_thread));    
  }
  return output;
}

Record Execution::GetRecordForDisplay() const { 
  Record ret = Base::GetRecordForDisplay();
  forall(run, top_elements_) 
    ret["Program"] += (*run)->ProgramTree() + "\n";
  ret["Program"] = "<pre>" + ret["Program"] + "</pre>";
  ret["output"] = "<pre>" + output_ + "</pre>";
  return ret;
}
