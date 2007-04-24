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

#include "objects.h"

class Blackboard;

class Posting {
  Posting(Tuple tuple, Time time, Blackboard *blackboard);
  void L1_Erase();
  void L1_ChangeTime(Time new_time);
  Tuple tuple_;
  Time time_;
  Blackboard * blackboard_;
};
/*bool operator <=(const Posting & p1, const Posting & p2) {
  if (p1.time_ < p2.time_) return true;
  if (p1.time_ > p2.time_) return false;
  return (p1.tuple_ < p2.tuple_);
  }*/

class TupleInfo {
  TupleInfo(Blackboard *blackboard);
  void L1_Erase();

  set<pair<Time, Posting *> > postings_; // all postings that make it true.
  Time first_; // time it first comes true.
  Blackboard * blackboard_;
};

class IndexRow {
  IndexRow(Tuple wildcard_tuple, Blackboard *blackboard);
  void L1_Erase();
  Tuple wildcard_tuple_;
  set<pair<Time, TupleInfo *> > generalizations_;
  Blackboard * blackboard_;
};

class Blackboard {
  
  map<Tuple, IndexRow *> index_;
  map<Tuple, TupleInfo *> tuple_info_;
  Changelist *changelist_;
};
