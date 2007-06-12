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
#include "query.h"
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
  blackboard_->L1_FlushUpdates();
}

void Posting::L1_Erase(){
  blackboard_->L1_RemovePosting(this);
  CL.Destroying(this);
  blackboard_->L1_FlushUpdates();
}

void Posting::L1_ChangeTime(Time new_time) {
  if (time_ == new_time) return;
  TupleInfo *ti = blackboard_->GetTupleInfo(tuple_);
  CHECK(ti);
  ti->L1_ChangePostingTime(this, new_time);
  blackboard_->L1_FlushUpdates();
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
  SingleWTUpdate update = SingleWTUpdate::Create(tuple_, first_posting->time_);
  // First we add the tuple to all of the index rows, then we send updates.
  for (int pass=0; pass<2; pass++) {
    for (GeneralizationIterator g_iter(tuple_.Data()); 
	 !g_iter.done(); ++g_iter){
      const Tuple & generalized = g_iter.Current();
      IndexRow * ir = blackboard_->GetAddIndexRow(OTuple::Make(generalized));
      if (pass==0) ir->L1_AddTuple(this);
      else ir->L1_SendUpdates(update);
    }
  }
  
}

void TupleInfo::L1_Erase() {
  // first we send updates, then we remove the tuple from all index rows.
  SingleWTUpdate update = SingleWTUpdate::Destroy(tuple_, FirstTime());
  for (int pass=0; pass<2; pass++) {
    for (GeneralizationIterator g_iter(tuple_.Data()); 
	 !g_iter.done(); ++g_iter){
      const Tuple & generalized = g_iter.Current();
      IndexRow * ir = blackboard_->GetIndexRow(OTuple::Make(generalized));
      CHECK(ir);
      if (pass==0) ir->L1_SendUpdates(update);
      else ir->L1_RemoveTuple(this);
    }
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

void TupleInfo::L1_ChangeTimesInIndexRows(Time old_first_time, 
					  Time new_first_time) {

  if (new_first_time == old_first_time) return;
  SingleWTUpdate update 
    = SingleWTUpdate::ChangeTime(tuple_, old_first_time, new_first_time);
  blackboard_->current_wt_update_ = new SingleWTUpdate(update);
  // first we send the updates, then we change the times in the index rows.
  // this is an arbitrary choice. 
  for (int pass=0; pass<2; pass++) {
    for (GeneralizationIterator g_iter(tuple_.Data()); !g_iter.done(); 
	 ++g_iter){
      const Tuple & generalized = g_iter.Current();
      IndexRow * ir = blackboard_->GetIndexRow(OTuple::Make(generalized));
      CHECK(ir);
      if (pass==0) ir->L1_SendUpdates(update);
      else ir->L1_ChangeTupleTime(this, old_first_time, new_first_time);
    }    
  }
  delete blackboard_->current_wt_update_;
  blackboard_->current_wt_update_ = NULL;
}

void TupleInfo::L1_ChangePostingTime(Posting *p, Time new_time) {
  Time old_first_time = FirstTime();
  CL.Make
    (new SetRemoveChange<pair<Time, Posting*> >
     (&postings_, make_pair(p->time_, p)));
  CL.ChangeValue(&p->time_, new_time);
  CL.Make
    (new SetInsertChange<pair<Time, Posting*> >
     (&postings_, make_pair(p->time_, p)));
  Time new_first_time = FirstTime();
  if (old_first_time != new_first_time) {
    L1_ChangeTimesInIndexRows(old_first_time, new_first_time);
  }
}

void TupleInfo::L1_AddPosting(Posting *p) {
  Time old_first_time = FirstTime();
  CL.Make
    (new SetInsertChange<pair<Time, Posting *> >
     (&postings_, make_pair(p->time_, p) ) );
  Time new_first_time = FirstTime();
  L1_ChangeTimesInIndexRows(old_first_time, new_first_time);
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
  L1_ChangeTimesInIndexRows(old_first_time, new_first_time);
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

void IndexRow::L1_SendUpdates(const SingleWTUpdate & update){
  forall(run_type, subscriptions_) {
    if (update.action_ == UPDATE_CHANGE_TIME &&
	!(run_type->first & UPDATE_TIME)) continue; 
    forall(run, run_type->second) {
      (*run)->Update(update);
    }
  }
}

void IndexRow::L1_ChangeTupleTime(TupleInfo *tuple_info, 
				  Time old_time, Time new_time){  

  CL.RemoveFromSet(&tuples_, make_pair(old_time, tuple_info));
  CL.InsertIntoSet(&tuples_, make_pair(new_time, tuple_info));
}
void IndexRow::L1_AddTuple(TupleInfo *tuple_info) {
  Time time = tuple_info->FirstTime();
  CL.InsertIntoSet(&tuples_, make_pair(time, tuple_info));
}
void IndexRow::L1_EraseIfUnnecessary(){
  if (tuples_.size() ||
      subscriptions_.size()) return;
  L1_Erase();
}

void IndexRow::L1_RemoveTuple(TupleInfo *tuple_info) {
  Time time = tuple_info->FirstTime();
  CL.RemoveFromSet(&tuples_, make_pair(time, tuple_info));
  L1_EraseIfUnnecessary();
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

void Blackboard::L1_FlushUpdates() {
  while (searches_to_flush_.size()) {
    pair<int, Search*> start = *(searches_to_flush_.begin());
    CL.RemoveFromSet(&searches_to_flush_, start);
    start.second->L1_FlushUpdates();
  }
}

void Blackboard::L1_AddSearchToFlush(Search * s) {
  int num_terms = s->query_->pattern_.size();
  if (searches_to_flush_ % make_pair(num_terms, s)) return;
  CL.InsertIntoSet(&searches_to_flush_, make_pair(num_terms, s));
}

Time RandomTime() {
  vector<pair<BitSeq, int> > coordinates;
  coordinates.push_back(make_pair(BitSeq(), rand()%3 + 1));
  return Time(coordinates);
}
Object RandomConstant() {
  return Integer::Make(rand() % 3 + 1);
}
Object RandomVariable() {
  return Variable::Make(rand() % 3);
}

OTuple RandomConstantTuple() {
  Tuple t;
  for (int i=0; i<2; i++) {
    t.push_back(RandomConstant());
  }
  return OTuple::Make(t);
}
OTuple RandomVariableTuple() {
  Tuple t;
  for (int i=0; i<2; i++) {
    if (rand() % 2) t.push_back(RandomConstant());
    else t.push_back(RandomVariable());
  }
  return OTuple::Make(t);
}
OPattern RandomPattern() {
  Pattern p;
  int sz = rand() % 3 + 1;
  for (int i=0; i<sz; i++) {
    p.push_back(MakeRandomVariableTuple());
  }
  return OPattern::Make(p);
}

UpdateNeeds RandomUpdateNeeds() {
  int c = rand() % 3;
  switch(c) {
  case 0: return 1; break;
  case 1: return 3; break;
  case 2: return 7; break;
  }
  return 1;
}
void Blackboard::RandomTest() {
  CL.MakeChangesPermanent();
  rankset<Posting *> postings;
  rankset<LoggingQuerySubscription *> subscriptions;
  int reps = 100000;
  int max_postings = 20;
  for (int rep=0; rep<reps; rep++) {
    if (rand() % 1000 == 0) {
      cout << "Rolling back changelist" << endl;
      CL.Rollback(0);
    }
    if (rand() % 1000 == 0) {
      cout << "Making changes permanent" << endl;
      CL.MakeChangesPermanent();
    }

    int action = rand() % 4;
    switch(action) {
    case 0: // Add a posting
      if (postings.size() == max_postings) break;
      Posting * p = new Posting(RandomConstantTuple(), RandomTime(), this);
      cout << "add posting " << p->tuple_.ToString() << " " 
	   << p->time_.ToString() << endl;
      postings.insert(p);
      break;
    case 1: // Delete a posting
      if (postings.size() == 0) break;
      Posting *p = *(postings.nth(rand() % postings.size()));
      cout << "remove posting " << p->tuple_.ToString() << " " 
	   << p->time_.ToString() << endl;
      p->L1_Erase();
      postings.erase(p);
      break;
    case 2: // Add a query;
      OPattern p = RandomPattern();
      UpdateNeeds needs = RandomUpdateNeeds();
      cout << "Adding a query " << p << " needs=" << needs << endl;
      Query * q = new Query(this, p.Data(), SamplingInfo());
      q->L1_Search(NULL);
      LoggingQuerySubscription *s = new LoggingQuerySubscription(q, needs);
      subscriptions.insert(s);
      break;
    case 3: // Delete a query;
      break;
    }
  }
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
	= b.L1_MakeLoggingWTSubscription(tuple, needs);
      subscriptions.push_back(sub);
      continue;      
    }
    if (command == "ds") {
      int i;
      cin >> i;
      subscriptions[i]->L1_Erase();
      continue;
    }/*
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
      }*/
  };
  
}

