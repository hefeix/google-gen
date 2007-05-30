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
#include "searchtree.h"
#include <fstream>

SamplingInfo::SamplingInfo() {
  sampled_ = false;
  position_ = -1;
  fraction_ = 0;
}

SamplingInfo::SamplingInfo(int position, double fraction) {
  sampled_ = true;
  position_ = position;
  fraction_ = fraction;
}

SamplingInfo SamplingInfo::LimitToPosition(int position) const{
  if (!sampled_ || (position != position_)) return SamplingInfo();
  return SamplingInfo(0, fraction_);
}
SamplingInfo SamplingInfo::LimitToPart(const vector<int> &partition, int part) 
  const {
  if (!sampled_ || partition[position_] != part) return SamplingInfo();
  int new_position = 0;
  for (int i=0; i<position_; i++) if (partition[i]==part) new_position++;
  return SamplingInfo(new_position, fraction_);
}

SamplingInfo SamplingInfo::RemovePosition(int position) const {
  if (sampled_ && (position < position_)) {
    return SamplingInfo(position_-1, fraction_);
  }
  return SamplingInfo();
}

SamplingInfo SamplingInfo::StringToSamplingInfo(const string& s) {
  istringstream istr(s);
  int position;
  double fraction;
  istr >> position >> fraction;
  return SamplingInfo(position, fraction);
}

string SamplingInfo::ToString() const {
  if (!sampled_) return "Unsampled";
  return "{pos=" + itoa(position_) + " fraction=1/" + dtoa(1/fraction_) + "}"; 
}

Posting::Posting(OTuple tuple, Time time, Blackboard *blackboard)
  :tuple_(tuple), time_(time), blackboard_(blackboard){
  CL.Creating(this);
  blackboard_->L1_AddPosting(this);
}
void Posting::L1_Erase(){
  blackboard_->L1_RemovePosting(this);
  CL.Destroying(this);
}
TupleInfo::TupleInfo(Posting *first_posting, Blackboard *blackboard)
  :tuple_(first_posting->tuple_), blackboard_(blackboard) {
  CL.Creating(this);
  CL.Make
    (new MapInsertChange<OTuple, TupleInfo *>
     (&blackboard_->tuple_info_, tuple_, this));
  CL.Make
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
  CL.Make
    (new MapRemoveChange<OTuple, TupleInfo *>
     (&blackboard_->tuple_info_, tuple_));
  CL.Destroying(this);
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
  CL.Make
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
  CL.Make
    (new SetRemoveChange<pair<Time, Posting *> >
     (&postings_, make_pair(p->time_, p) ) );
  Time new_first_time = FirstTime();
  ChangeTimesInIndexRows(old_first_time, new_first_time);
}

OTuple WTSubscription::GetWildcardTuple() const {
  return subscribee_->GetWildcardTuple();
}

IndexRow::IndexRow(OTuple wildcard_tuple, Blackboard *blackboard)
  :wildcard_tuple_(wildcard_tuple), blackboard_(blackboard){  
  CL.Creating(this);
  CL.Make
    (new MapInsertChange<OTuple, IndexRow *>
     (&blackboard_->index_, wildcard_tuple_, this) );
}
void IndexRow::L1_Erase() {
  CL.Make
    (new MapRemoveChange<OTuple, IndexRow *>
     (&blackboard_->index_, wildcard_tuple_) );
  CL.Destroying(this);
}
void IndexRow::ChangeTupleTime(TupleInfo *tuple_info, 
			       Time old_time, Time new_time){  

  CL.RemoveFromSet(&tuples_, make_pair(old_time, tuple_info));
  CL.InsertIntoSet(&tuples_, make_pair(new_time, tuple_info));
  WTUpdate update;
  update.changes_.push_back
    (make_pair(tuple_info->tuple_, 
	       make_pair(&old_time, &new_time)));
  forall(run_type, subscriptions_) {
    if (!(run_type->first & UPDATE_TIME)) continue; 
    forall(run, run_type->second) {
      (*run)->Update(update);
    }
  }
}
void IndexRow::AddTuple(TupleInfo *tuple_info) {
  Time time = tuple_info->FirstTime();
  CL.InsertIntoSet(&tuples_, make_pair(time, tuple_info));
  WTUpdate update;
  update.count_delta_ = 1;
  update.changes_.push_back
    (make_pair(tuple_info->tuple_, 
	       make_pair((const Time *)NULL, &time)));
  forall(run_type, subscriptions_) {
    forall(run, run_type->second) {
      (*run)->Update(update);
    }
  }
}
void IndexRow::EraseIfEmpty(){
  if (tuples_.size() ||
      subscriptions_.size()) return;
  L1_Erase();
}

void IndexRow::RemoveTuple(TupleInfo *tuple_info) {
  Time time = tuple_info->FirstTime();
  CL.RemoveFromSet(&tuples_, make_pair(time, tuple_info));
  WTUpdate update;
  update.count_delta_ = -1;
  update.changes_.push_back
    (make_pair(tuple_info->tuple_, 
	       make_pair(&time, (const Time *)NULL)));
  forall(run_type, subscriptions_) {
    forall(run, run_type->second) {
      (*run)->Update(update);
    }
  }
  EraseIfEmpty();
}

void Blackboard::L1_AddPosting(Posting *p){
  if (num_nonupdated_queries_ > 0) 
    cerr << "Adding a posting while nonupdated queries exist";
  TupleInfo *ti = GetTupleInfo(p->tuple_);
  if (ti) {
    ti->L1_AddPosting(p);
    return;
  }
  new TupleInfo(p, this);
}

void Blackboard::L1_RemovePosting(Posting * p){
  if (num_nonupdated_queries_ > 0) 
    cerr << "Removing a posting while nonupdated queries exist";
  TupleInfo * ti = GetTupleInfo(p->tuple_);
  CHECK(ti);
  ti->L1_RemovePosting(p);
}
TupleInfo * Blackboard::GetTupleInfo(OTuple t){
  TupleInfo ** find = tuple_info_ % t;
  if (find) return *find;
  return NULL;
}

uint64 Blackboard::GetNumWildcardMatches(OTuple wildcard_tuple) {
  IndexRow * ir = GetIndexRow(wildcard_tuple);
  if (!ir) return 0;
  return ir->size();
}

void Blackboard::L1_ChangeNumNonupdatedQueries(int delta) {
  if (delta==0) return;
  CL.ChangeValue(&num_nonupdated_queries_, num_nonupdated_queries_+delta);
  CHECK(num_nonupdated_queries_>=0);
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
  subscribee_ = blackboard->GetAddIndexRow(wildcard_tuple);
  needs_ = needs;
  CL.Make(new MapOfSetsInsertChange<UpdateNeeds, WTSubscription *>
	  (&(subscribee_->subscriptions_), needs_, this));
  
} 
void WTSubscription::L1_Erase() { 
  CL.Make(new MapOfSetsRemoveChange<UpdateNeeds, WTSubscription*>
	  (&(subscribee_->subscriptions_), needs_, this));
  subscribee_->EraseIfEmpty();
}
void WTSubscription::L1_ChangeNeeds(UpdateNeeds new_needs){
  CL.Make(new MapOfSetsRemoveChange<UpdateNeeds, WTSubscription*>
	  (&(subscribee_->subscriptions_), needs_, this));
  CL.ChangeValue(&needs_, new_needs);
  CL.Make(new MapOfSetsInsertChange<UpdateNeeds, WTSubscription*>
	  (&(subscribee_->subscriptions_), needs_, this));
  subscribee_->EraseIfEmpty();
}

void Blackboard::Shell() {
  Blackboard b;
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
    if (command == "postfile") {
      string fn;
      cin >> fn;
      ifstream input(fn.c_str());
      int count = 0;
      while (input >> tuple) {
	new Posting(tuple, Time(), &b);
	count++;
      }
      cout << "Posted " << count << " tuples" << endl;      
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
    if (command == "query") {
      OPattern p;
      cin >> p;
      Query *q = new Query(&b, p.Data(), SamplingInfo());
      q->L1_Search(NULL);
      cout << "#matches:" << q->GetCount() << endl;
      vector<Map> subs;
      q->GetSubstitutions(&subs);
      for (uint c=0; c<10 && c<subs.size(); c++) {
	OPattern ps = Substitute(subs[c], p);
	cout << ps << endl;
      }
    }
  };
  
}

