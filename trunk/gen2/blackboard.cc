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


SamplingInfo::SamplingInfo() {
  sampled_ = false;
}
SamplingInfo::SamplingInfo(int position, uint32 start_hash, uint32 end_hash){
  sampled_ = true;
  position_ = position;
  start_hash_ = start_hash;
  end_hash_ = end_hash;
}

SamplingInfo SamplingInfo::RandomRange(int position, int denominator, int part){
  if (part < 0) part = rand() % denominator;
  uint32 start = (0xFFFFFFFF / denominator) * part;
  uint32 end = (0xFFFFFFFF / denominator) * (part+1);
  return SamplingInfo(position, start, end);
}

SamplingInfo SamplingInfo::LimitToPosition(uint32 position) const{
  if (!sampled_ || (position != position_)) return SamplingInfo();
  return SamplingInfo(0, start_hash_, end_hash_);
}

bool SamplingInfo::RemovePosition(uint32 position) {
  if (!sampled_) return false;
  if (position < position_) {
    position_--;
    return true;
  }
  if (position == position_) {
    sampled_ = false;
    return false;
  }
  return true;
}

bool SamplingInfo::Matches(const Tuple& t) const {
  if (!sampled_) return true;
  uint32 fp = t.Fingerprint32();
  return (start_hash_ <= fp) && (fp <= end_hash_);
}

SamplingInfo SamplingInfo::StringToSamplingInfo(const string& s) {
  istringstream istr(s);
  int position, denominator, part;
  istr >> position >> denominator >> part;
  return RandomRange(position, denominator, part);
}

string SamplingInfo::ToString() const {
  if (!sampled_) return "Unsampled";
  return "{pos=" + itoa(position_) + " 1/" 
    + dtoa(1.0/GetFraction()) + "}";
}
double SamplingInfo::GetFraction() const {
  if (!sampled_) return 1.0;
  return (end_hash_-start_hash_+1.0)/pow(2,32);
}

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

OTuple WTSubscription::GetWildcardTuple() const {
  return index_row_->GetWildcardTuple();
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
  WTUpdate update;
  update.changes_.push_back
    (make_pair(tuple_info->tuple_, 
	       make_pair(&old_time, &new_time)));
  forall(run, time_matters_subscriptions_) {
    (*run)->Update(update);
  }
}
void IndexRow::AddTuple(TupleInfo *tuple_info) {
  Time time = tuple_info->FirstTime();
  blackboard_->changelist_->Make
    (new SetInsertChange<pair<Time, TupleInfo *> >
     (&tuples_, make_pair(time, tuple_info)));
  WTUpdate update;
  update.count_delta_ = 1;
  update.changes_.push_back
    (make_pair(tuple_info->tuple_, 
	       make_pair((const Time *)NULL, &time)));
  forall(run, time_matters_subscriptions_) (*run)->Update(update);
  forall(run, existence_subscriptions_) (*run)->Update(update);
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
  WTUpdate update;
  update.count_delta_ = -1;
  update.changes_.push_back
    (make_pair(tuple_info->tuple_, 
	       make_pair(&time, (const Time *)NULL)));
  forall(run, time_matters_subscriptions_) (*run)->Update(update);
  forall(run, existence_subscriptions_) (*run)->Update(update);
  EraseIfEmpty();
}
void IndexRow::L1_AddWTSubscription(WTSubscription *sub) {
  blackboard_->changelist_->Make
    (new SetInsertChange<WTSubscription *>
     ((sub->Needs() & UPDATE_TIME)?(&time_matters_subscriptions_):
      (&existence_subscriptions_), sub));
}
void IndexRow::L1_RemoveWTSubscription(WTSubscription *sub) {
  blackboard_->changelist_->Make
    (new SetRemoveChange<WTSubscription *>
     ((sub->Needs() & UPDATE_TIME)?(&time_matters_subscriptions_):
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

WTSubscription::WTSubscription(Blackboard *blackboard, OTuple wildcard_tuple,
			       UpdateNeeds needs) {
  index_row_ = blackboard->GetAddIndexRow(wildcard_tuple);
  needs_ = needs;
  index_row_->L1_AddWTSubscription(this);
} 
void WTSubscription::L1_Erase() { 
  index_row_->L1_RemoveWTSubscription(this);
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
      continue;
    }
    if (command == "subscribe") {
      UpdateNeeds needs;
      cin >> tuple >> needs;
      cout << "Subscription " << (subscriptions.size()) << endl;
      WTSubscription *sub 
	= new LoggingWTSubscription(&b, tuple, needs);
      subscriptions.push_back(sub);
      continue;      
    }
    if (command == "ds") {
      int i;
      cin >> i;
      subscriptions[i]->L1_Erase();
      continue;
    }
  };
  
}
