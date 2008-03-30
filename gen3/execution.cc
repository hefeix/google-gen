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
#include <fstream>

string Thread::ToString() {
  string ret;
  ret += "depth " + itoa(stack_.size()) + " ";
  ret += OTuple::Make(stack_).ToString() + " ";
  // ret += element_->PrettyProgramTree() + " ";
  return ret;
}

void OnSubscription::Init(const Thread & t, Blackboard::Row *row) {
  CHECK(t.element_);
  thread_ = t;
  Blackboard::Subscription::Init(row);
}

void OnSubscription::Update(Blackboard::Row *row, int tuple_num) {
  VLOG(2) << "updating wildcard_tuple " << row->wildcard_tuple_ 
	  << " num_wildcards_ " << row->num_wildcards_
	  << " tuple_num " << tuple_num 
	  << " parent incoming stack depth " <<
	  thread_.element_->parent_->incoming_stack_depth_ << endl;  
  VLOG(2) << "Update Thread before binding " << thread_.ToString() << endl;
  row->CopyBinding(tuple_num, 
		   &thread_.stack_,
		   thread_.element_->parent_->incoming_stack_depth_);
  VLOG(2) << "Update Thread after binding " << thread_.ToString() << endl;
  thread_.execution_->Enqueue(thread_, MININT64);
}

void Execution::ParseAndExecute(const Tuple & program_tuple, 
				bool pretty, 
				bool execute) {
  vector<Element *> v;
  if (pretty) v = PrettyParseElements(program_tuple);  
  else v = SimpleParseElements(program_tuple);
  forall(run, v) AddCodeTreeToRun(*run);
  if (!execute) return;
  ExecuteForever();
}

void Execution::ExecuteRunnableThreads() {
  if (run_queue_.size() == 0) return;
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
  Enqueue(t, MININT64);
  top_elements_.push_back(top_element);
}

Record Execution::GetRecordForDisplay() const { 
  Record ret = Base::GetRecordForDisplay();
  forall(run, top_elements_) 
    ret["Program"] += (*run)->PrettyProgramTree() + "\n";
  ret["Program"] = "<pre>" + ret["Program"] + "</pre>";
  ret["output"] = "<pre>" + output_ + "</pre>";
  ret["total_bias"] = dtoa(total_bias_);
  if (guide_) ret["guide"] = guide_->GetLink("guide");
  ret["blackboard"] = blackboard_->GetLink("blackboard");
  return ret;
}

Execution * Execute(const Tuple & main_program, const Tuple & guide_program,
		    bool pretty_parsing) {
  Execution *main = New<Execution>();
  main->AddGuide();
  main->ParseAndExecute(main_program, pretty_parsing, false);
  main->guide_->ParseAndExecute(guide_program, pretty_parsing, false);
  main->ExecuteForever();
  return main;  
}

void ReadCodeFile(string filename, Tuple *result) {
  ifstream input(filename.c_str());
  Object o;
  while (input >> o) result->push_back(o);
}
