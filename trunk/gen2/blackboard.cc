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
#include "tuple.h"

Posting::Posting(OTuple tuple, Time time, Blackboard *blackboard)
  :tuple_(tuple), time_(time), blackboard_(blackboard){
  blackboard_->changelist_->Creating(this);
  blackboard_->L1_AddPosting(this);
}
void Posting::L1_Erase(){
  blackboard_->L1_RemovePosting(this);
  blackboard_->changelist_->Destroying(this);
}
TupleInfo::TupleInfo(Posting *first_posting, Blackboard *blackboard)
  :tuple_(first_posting->tuple_), blackboard_(blackboard) {
  blackboard_->changelist_->Creating(this);
  blackboard_->changelist_->Make
    (new MapInsertChange<OTuple, TupleInfo *>
     (&blackboard_->tuple_info_, tuple_, this));
  blackboard_->changelist_->Make
    (new SetInsertChange<pair<Time, Posting *> >
     (&postings_, make_pair(first_posting->time_, first_posting)));
  for (GeneralizationIterator g_iter(tuple_.Data()); !g_iter.done(); ++g_iter){
    const Tuple & generalized = g_iter.Current();
    IndexRow * ir = blackboard_->GetAddIndexRow(OTuple::Make(generalized));
    ir->AddTuple(this);
  }
}
void TupleInfo::L1_Erase() {
  // remove me from index rows and blackboard.
  for (GeneralizationIterator g_iter(tuple_.Data()); !g_iter.done(); ++g_iter){
    const Tuple & generalized = g_iter.Current();
    IndexRow * ir = blackboard_->GetIndexRow(OTuple::Make(generalized));
    CHECK(ir);
    ir->RemoveTuple(this);
  }
  blackboard_->changelist_->Make
    (new MapRemoveChange<OTuple, TupleInfo *>
     (&blackboard_->tuple_info_, tuple_));
  blackboard_->changelist_->Destroying(this);
}
Time TupleInfo::FirstTime() const {
  CHECK(postings_.size());
  return postings_.begin()->first;
}
void TupleInfo::ChangeTimesInIndexRows(Time old_first_time, 
				       Time new_first_time) {
  if (new_first_time != old_first_time) {
    for (GeneralizationIterator g_iter(tuple_.Data()); !g_iter.done(); 
	 ++g_iter){
      const Tuple & generalized = g_iter.Current();
      IndexRow * ir = blackboard_->GetIndexRow(OTuple::Make(generalized));
      CHECK(ir);
      ir->ChangeTupleTime(this, old_first_time, new_first_time);
    }    
  }
}
void TupleInfo::L1_AddPosting(Posting *p) {
  Time old_first_time = FirstTime();
  blackboard_->changelist_->Make
    (new SetInsertChange<pair<Time, Posting *> >
     (&postings_, make_pair(p->time_, p) ) );
  Time new_first_time = FirstTime();
  ChangeTimesInIndexRows(old_first_time, new_first_time);
}
void TupleInfo::L1_RemovePosting(Posting *p){
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

IndexRow::IndexRow(OTuple wildcard_tuple, Blackboard *blackboard)
  :wildcard_tuple_(wildcard_tuple), blackboard_(blackboard){  
  blackboard_->changelist_->Creating(this);
  blackboard_->changelist_->Make
    (new MapInsertChange<OTuple, IndexRow *>
     (&blackboard_->index_, wildcard_tuple_, this) );
}
void IndexRow::L1_Erase() {
  blackboard_->changelist_->Make
    (new MapRemoveChange<OTuple, IndexRow *>
     (&blackboard_->index_, wildcard_tuple_) );
  blackboard_->changelist_->Destroying(this);
}
void IndexRow::ChangeTupleTime(TupleInfo *tuple_info, 
			       Time old_time, Time new_time){
  blackboard_->changelist_->Make
    (new SetRemoveChange<pair<Time, TupleInfo *> >
     (&tuples_, make_pair(old_time, tuple_info)));
  blackboard_->changelist_->Make
    (new SetInsertChange<pair<Time, TupleInfo *> >
     (&tuples_, make_pair(new_time, tuple_info)));
  forall(run, time_matters_subscriptions_) {
    (*run)->TimeChange(tuple_info->tuple_, old_time, new_time);
  }
}
void IndexRow::AddTuple(TupleInfo *tuple_info) {
  Time time = tuple_info->FirstTime();
  blackboard_->changelist_->Make
    (new SetInsertChange<pair<Time, TupleInfo *> >
     (&tuples_, make_pair(time, tuple_info)));
  forall(run, existence_subscriptions_) {
    (*run)->AddTuple(tuple_info->tuple_, time);
  }
  forall(run, time_matters_subscriptions_) {
    (*run)->AddTuple(tuple_info->tuple_, time);
  }
}
void IndexRow::EraseIfEmpty(){
  if (tuples_.size() ||
      time_matters_subscriptions_.size() ||
      existence_subscriptions_.size()) return;
  L1_Erase();
}

void IndexRow::RemoveTuple(TupleInfo *tuple_info) {
  Time time = tuple_info->FirstTime();
  blackboard_->changelist_->Make
    (new SetRemoveChange<pair<Time, TupleInfo *> >
     (&tuples_, make_pair(time, tuple_info)));
  forall(run, existence_subscriptions_) {
    (*run)->RemoveTuple(tuple_info->tuple_, time);
  }
  forall(run, time_matters_subscriptions_) {
    (*run)->RemoveTuple(tuple_info->tuple_, time);
  }
  EraseIfEmpty();
}
void IndexRow::AddWTSubscription(WTSubscription *sub) {
  blackboard_->changelist_->Make
    (new SetInsertChange<WTSubscription *>
     (sub->TimeMatters()?(&time_matters_subscriptions_):
      (&existence_subscriptions_), sub));
}
void IndexRow::RemoveWTSubscription(WTSubscription *sub) {
  blackboard_->changelist_->Make
    (new SetRemoveChange<WTSubscription *>
     (sub->TimeMatters()?(&time_matters_subscriptions_):
      (&existence_subscriptions_), sub));
  EraseIfEmpty();
}

Blackboard::Blackboard(Changelist *cl) 
  :changelist_(cl) {
}


void Blackboard::L1_AddPosting(Posting *p){
  TupleInfo *ti = GetTupleInfo(p->tuple_);
  if (ti) {
    ti->L1_AddPosting(p);
    return;
  }
  new TupleInfo(p, this);
}

void Blackboard::L1_RemovePosting(Posting * p){
  TupleInfo * ti = GetTupleInfo(p->tuple_);
  CHECK(ti);
  ti->L1_RemovePosting(p);
}
TupleInfo * Blackboard::GetTupleInfo(OTuple t){
  TupleInfo ** find = tuple_info_ % t;
  if (find) return *find;
  return NULL;
}

IndexRow * Blackboard::GetIndexRow(OTuple wildcard_tuple){
  IndexRow ** find = index_ % wildcard_tuple;
  if (find) return *find;
  return NULL;
}
IndexRow * Blackboard::GetAddIndexRow(OTuple wildcard_tuple){
  IndexRow ** find = index_ % wildcard_tuple;
  if (find) return *find;
  return new IndexRow(wildcard_tuple, this);
}

void Blackboard::L1_AddWTSubscription(WTSubscription *sub) {
  IndexRow *ir = GetAddIndexRow(sub->WildcardTuple());
  ir->AddWTSubscription(sub);
}
void Blackboard::L1_RemoveWTSubscription(WTSubscription *sub) {
  IndexRow *ir = GetIndexRow(sub->WildcardTuple());
  CHECK(ir);
  ir->RemoveWTSubscription(sub);
}

void Blackboard::Shell() {
  Changelist cl;
  Blackboard b(&cl);
  string command;
  OTuple tuple;
  Time time;
  vector<Posting *> postings;
  vector<WTSubscription *> subscriptions;
  for (;(cin >> command) && command != "q"; cout << endl) {
    if (command == "post") {
      cin >> tuple >> time;
      cout << "Posting " << (postings.size()) << endl;      
      postings.push_back(new Posting(tuple, time, &b));
      continue;
    }
    if (command == "dp") {
      int i;
      cin >> i;
      postings[i]->L1_Erase();
    }
    if (command == "subscribe") {
      int time_matters;
      cin >> tuple >> time_matters;
      cout << "Subscription " << (subscriptions.size()) << endl;
      WTSubscription *sub = new LoggingSubscription(tuple, time_matters);
      subscriptions.push_back(sub);
      b.L1_AddWTSubscription(sub);
      continue;      
    }
    if (command == "ds") {
      int i;
      cin >> i;
      b.L1_RemoveWTSubscription(subscriptions[i]);
    }
  };
  
}

