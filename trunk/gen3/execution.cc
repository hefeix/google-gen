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
  ret += OTuple::Make(stack_).ToString() + " ";
  ret += element_->PrettyProgramTree() + " ";
  return ret;
}

void OnSubscription::Init(const Thread & t, Blackboard::Row *row) {
  CHECK(t.element_);
  thread_ = t;
  Blackboard::Subscription::Init(row);
}

void OnSubscription::Update(Blackboard::Row *row, int tuple_num) {
  Thread new_thread = thread_;
  row->CopyBinding(tuple_num, &new_thread.stack_, 
		   new_thread.element_->parent_->incoming_stack_depth_);
  // VLOG(1)  << "Update " << tuple
  //	   << " Threadinfo " << new_thread.ToString() << endl;
  new_thread.execution_->Enqueue(new_thread, BitSeq::Min());
}
void Execution::ParseAndExecute(OTuple program_tuple, 
				bool pretty, 
				bool execute) {
  vector<Element *> v;
  if (pretty) v = PrettyParseElements(program_tuple.Data());  
  else v = SimpleParseElements(program_tuple.Data());
  forall(run, v) AddCodeTreeToRun(*run);
  if (!execute) return;
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

Record Execution::GetRecordForDisplay() const { 
  Record ret = Base::GetRecordForDisplay();
  forall(run, top_elements_) 
    ret["Program"] += (*run)->PrettyProgramTree() + "\n";
  ret["Program"] = "<pre>" + ret["Program"] + "</pre>";
  ret["output"] = "<pre>" + output_ + "</pre>";
  return ret;
}
