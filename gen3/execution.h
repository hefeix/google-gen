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

#ifndef _EXECUTION_H_
#define _EXECUTION_H_

#include "blackboard.h"

struct Element;
struct Execution;

	      

struct Thread {
  Thread() {
    element_ = NULL;
    execution_ = NULL;
  }
  Tuple stack_;
  Element * element_;
  Execution * execution_;
  string ToString();
};

struct OnSubscription : public Blackboard::Subscription {
  // t should point to the child of the On.
  void Init(const Thread & t, Blackboard::Row *row);
  void Update(Blackboard::Row *rw, int tuple_num);
  // data
  Thread thread_;
};

struct Execution : public Base {
  void Init() { 
    Base::Init(); 
    current_time_ = Time();
    blackboard_ = New<Blackboard>();
  }
  Base::Type GetBaseType() const { return Base::EXECUTION;}

  void ParseAndExecute(OTuple program_tuple);

  void ExecuteForever() {
    while (run_queue_.size()) ExecuteOneEpoch();
  }
  void ExecuteOneEpoch() {
    ExecuteRunnableThreads();
    CommitPostings();
  }
  void CommitPostings() {
    forall(run, post_queue_) {
      // cout << "Committing " << *run << " at " << current_time_ << endl;
      blackboard_->Post(*run);
    }
    post_queue_.clear();
  }
  void ExecuteRunnableThreads();

  void AddCodeTreeToRun(Element *top_element);

  // The thread points at the immediately executable code
  static Tuple MatchAndRun(Thread & thread, const Tuple & variable_tuple);

  void Enqueue(Thread t, BitSeq time_delay) {
    run_queue_[time_delay].push_back(t);
  }
  
  void AddPost(const Tuple & post) {
    post_queue_.push_back(post);
  }
  
  Record GetRecordForDisplay() const;

  // the static program.
  vector<Element *> top_elements_;

  // these are all the subscriptions generated by executing on statements.
  set<OnSubscription *> subscriptions_;

  // This contains threads ordered by when they can be run
  map<BitSeq, vector<Thread> > run_queue_;

  // This contains the list of tuples to be posted at the end
  // of this epoch
  vector<Tuple> post_queue_;

  // This is the blackboard for the execution
  Blackboard *blackboard_;

  // The current time (Tracked during execution for no purpose)
  Time current_time_;

  // the output of the program
  string output_; 
};

#endif
