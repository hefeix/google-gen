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

#include "blackboard.h"

Posting::Posting(Blackboard *blackboard,OTuple tuple, Time time)
  :blackboard_(blackboard), tuple_(tuple), time_(time){
  blackboard_->changelist_->Creating(this);
  blackboard_->AddPosting(this);
}
Posting::L1_Erase(){
  blackboard_->RemovePosting(this);
  blackboard_->changelist_->Destroying(this);
}
TupleInfo::TupleInfo(Blackboard *blackboard, Posting *first_posting)
  :blackboard_(blackboard_), tuple_(tuple) {
  blackboard_->changelist_->Creating(this);
  blackboard_->changelist_->Make
    (new MapInsertChange<OTuple, TupleInfo *>
     (blackboard_->tuple_info_, tuple_, this));
  blackboard_->changelist_->Make
    (new SetInsertChange<pair<Time, Posting *> >
     (&postings_, make_pair(first_posting->time_, first_posting)));
  for (GeneralizationIterator g_iter(tuple_.Data()); !g_iter.Done(); ++g_iter){
    const Tuple & generalized = g_iter.Current();
    IndexRow * ir = blackboard_->GetAddIndexRow(generalized);
    ir->AddTuple(this);
  }
}
TupleInfo::L1_Erase() {
  // remove me from index rows and blackboard.
  for (GeneralizationIterator g_iter(tuple_.Data()); !g_iter.Done(); ++g_iter){
    const Tuple & generalized = g_iter.Current();
    IndexRow * ir = blackboard_->GetIndexRow(Generalized);
    CHECK(ir);
    ir->RemoveTuple(this);
  }
  blackboard_->changelist_->Make
    (new MapRemoveChange<OTuple, TupleInfo *>
     (blackboard_->tuple_info_, tuple_, this));
  blackboard_->changelist_->Destroying(this);
}
Time TupleInfo::FirstTime() const {
  CHECK(postings_.size());
  return postings_.begin()->first;
}
TupleInfo::ChangeTimesInIndexRows(Time old_first_time, Time new_first_time) {
  if (new_first_time != old_first_time) {
    for (GeneralizationIterator g_iter(tuple_.Data()); !g_iter.Done(); 
	 ++g_iter){
      const Tuple & generalized = g_iter.Current();
      IndexRow * ir = blackboard_->GetIndexRow();
      CHECK(ir);
      ir->ChangeTupleTime(this, old_first_time, new_first_time);
    }    
  }
}
TupleInfo::L1_AddPosting(Posting *p) {
  Time old_first_time = FirstTime();
  blackboard_->changelist_->Make
    (new SetInsertChange<pair<Time, Posting *> >
     (&postings_, make_pair(p->time_, p) ) );
  Time new_first_time = FirstTime();
  ChangeTimesInIndexRows(old_first_time, new_first_time);
}
TupleInfo::L1_RemovePosting(Posting *p){
  if (postings_.size()==1) {
    L1_Erase();
    return;
  }
  Time old_first_time = FirstTime();
  blackboard_->changelist_->Make
    (new SetRemoveChange<pair<Time, Posting *> >
     (&postings_, make_pair(p->time_, p) ) );
  Time new_first_time = FirstTime();
  ChangeTimesInIndexRows(old_first_time, new_first_time);
}

Blackboard::L1_AddPosting(Posting *p){
  TupleInfo *ti = GetTupleInfo(p->tuple_);
  if (ti) {
    ti->AddPosting(p);
    return;
  }
  new TupleInfo(this, p);
}

Blackboard::L1_RemovePosting(Posting * p){
  TupleInfo * ti = GetTupleInfo(p);
  ti->RemovePosting(p);
}
TupleInfo * Blackboard::GetTupleInfo(OTuple t){
  TupleInfo ** find = tuples_ % p->tuple_;
  if (find) return *find;
  return NULL;
}

