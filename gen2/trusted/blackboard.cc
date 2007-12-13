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
#include "webserver.h"
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
  CHECK(tuple != NULL);
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
  :blackboard_(blackboard), tuple_(first_posting->tuple_) {
  CL.Creating(this);

  // Maintain accurate counts
  CL.AddToMapOfCounts(&blackboard_->lengths_, tuple_.size(), 1);

  CL.InsertIntoMap(&blackboard_->tuple_info_, tuple_, this);
  CL.InsertIntoSet
    (&postings_, make_pair(first_posting->time_, first_posting));
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

string TupleInfo::ToString() {
  string ret = tuple_.ToString();
  forall(run, postings_)
    ret += " " + run->first.ToString();
  return ret;
}

void TupleInfo::L1_Erase() {

  // Maintain accurate counts 
  CL.AddToMapOfCounts(&blackboard_->lengths_, tuple_.size(), -1);

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
  CL.RemoveFromMap
     (&blackboard_->tuple_info_, tuple_);
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
  CL.RemoveFromSet
     (&postings_, make_pair(p->time_, p));
  CL.ChangeValue(&p->time_, new_time);
  CL.InsertIntoSet
     (&postings_, make_pair(p->time_, p));
  Time new_first_time = FirstTime();
  if (old_first_time != new_first_time) {
    L1_ChangeTimesInIndexRows(old_first_time, new_first_time);
  }
}

void TupleInfo::L1_AddPosting(Posting *p) {
  Time old_first_time = FirstTime();
  CL.InsertIntoSet
     (&postings_, make_pair(p->time_, p) );
  Time new_first_time = FirstTime();
  L1_ChangeTimesInIndexRows(old_first_time, new_first_time);
}

void TupleInfo::L1_RemovePosting(Posting *p){
  if (postings_.size()==1) {
    L1_Erase();
    return;
  }
  Time old_first_time = FirstTime();
  CL.RemoveFromSet
     (&postings_, make_pair(p->time_, p) );
  Time new_first_time = FirstTime();
  L1_ChangeTimesInIndexRows(old_first_time, new_first_time);
}

IndexRow::IndexRow(OTuple wildcard_tuple, Blackboard *blackboard)
  :wildcard_tuple_(wildcard_tuple), blackboard_(blackboard){  
  CL.Creating(this);
  CL.InsertIntoMap
     (&blackboard_->index_, wildcard_tuple_, this);
}
void IndexRow::L1_Erase() {
  CL.RemoveFromMap
     (&blackboard_->index_, wildcard_tuple_);
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
void IndexRow::L1_SendCurrentAsUpdates(WTSubscription *sub) {
  forall(run, tuples_){
    sub->Update(SingleWTUpdate::Create(run->second->GetTuple(), run->first));  
  }
}

bool IndexRow::GetRandomTuple(Tuple * result) {
  pair<Time, TupleInfo*> tt;
  if (tuples_.size() == 0) return false;
  GetSampleElement(tuples_, &tt);
  *result = tt.second->tuple_.Data();
  return true;
}

void IndexRow::L1_ChangeTupleTime(TupleInfo *tuple_info, 
				  Time old_time, Time new_time){  

  CL.RemoveFromSet(&tuples_, make_pair(old_time, tuple_info));
  CL.InsertIntoSet(&tuples_, make_pair(new_time, tuple_info));
}

void IndexRow::L1_AddTuple(TupleInfo *tuple_info) {
  Time time = tuple_info->FirstTime();
  CL.InsertIntoSet(&tuples_, make_pair(time, tuple_info));

  if ( (wildcard_tuple_.Data().size() != 0) &&
       (wildcard_tuple_.Data()[0] == WILDCARD) ) {
    CL.AddToMapOfCounts(&first_term_counts_, tuple_info->tuple_.Data()[0], 1);
  }
}

void IndexRow::L1_EraseIfUnnecessary(){
  if (tuples_.size() ||
      subscriptions_.size()) return;
  L1_Erase();
}

void IndexRow::L1_RemoveTuple(TupleInfo *tuple_info) {
  if ( (wildcard_tuple_.Data().size() != 0) &&
       (wildcard_tuple_.Data()[0] == WILDCARD) ) {
    CL.AddToMapOfCounts(&first_term_counts_, tuple_info->tuple_.Data()[0], -1);
  }

  Time time = tuple_info->FirstTime();
  CL.RemoveFromSet(&tuples_, make_pair(time, tuple_info));
  L1_EraseIfUnnecessary();
}

// Note, can't return the times without the substitutions
bool Blackboard::FindSatisfactions
(OPattern pattern,
 const SamplingInfo & sampling,
 vector<Map> * substitutions,
 vector<Time> * times,
 uint64 * num_satisfactions,
 int64 * max_work_now) {

  VLOG(1) << "finding sats for " << pattern << endl;

  Checkpoint before_query = CL.GetCheckpoint();
  Query * q = L1_GetExecuteQuery(pattern, sampling, max_work_now);
  if (q == NULL) {
    CL.Rollback(before_query);
    return false;
  }
  if (num_satisfactions) {
    *num_satisfactions = q->GetCount();
  }
  if (substitutions) {
    q->GetSubstitutions(substitutions, times);
  }
  CL.Rollback(before_query);
  return true;
}

void Blackboard::PrintNonupdateQueries() {
  cout << "Printing nonupdated queries" << endl;
  forall(run, nonupdated_queries_) {
    cout << "  " << (*run)->GetDescription() << endl;
  }
}

void Blackboard::PrintBlackboard() {
  cout << "PrintBlackboard " << endl << " tuples: " << endl;
  forall (run, tuple_info_) {
    cout << "  " << run->second->ToString() << endl;
  }
  cout << "Queries "<< endl;
  forall(run, queries_) {
    Query * q = run->second;
    cout << "  " << run->first << " count_=" << q->GetCount() 
	 << " FlushedQueryUpdate=" << ( (flushed_query_update_%(q))?'T':'F')
	 << " FlushedWTUpdate=" << ( (flushed_wt_update_%(q))?'T':'F')
	 << " queued=" << ((searches_to_flush_ % make_pair((int)q->pattern_.size(), q->search_))?'T':'F')
	 << " needs=" << q->needs_
	 << endl;
  }
}

void Blackboard::Verify() const {
  forall(run, queries_) run->second->Verify();
}

void Blackboard::L1_AddPosting(Posting *p){
  if (nonupdated_queries_.size()) {
    cerr << "Adding a posting while nonupdated queries exist" << endl;
    PrintBlackboard();
    PrintNonupdateQueries();
    CHECK(false);
  }
  TupleInfo *ti = GetTupleInfo(p->tuple_);
  if (ti) {
    ti->L1_AddPosting(p);
    return;
  }
  new TupleInfo(p, this);
}

void Blackboard::L1_RemovePosting(Posting * p){
  if (nonupdated_queries_.size()) {
    cerr << "Removing a posting while nonupdated queries exist" << endl;
    PrintBlackboard();
    PrintNonupdateQueries();
    CHECK(false);
  }
  TupleInfo * ti = GetTupleInfo(p->tuple_);
  CHECK(ti);
  ti->L1_RemovePosting(p);
}



Query * Blackboard::L1_GetExecuteQuery(OPattern p, SamplingInfo sampling, 
				       int64 *max_work_now){
  if (!sampling.sampled_) {
    Query ** find = queries_ % p;
    if (find) return *find;
  }
  Checkpoint cp = CL.GetCheckpoint();
  Query * q = new Query(this, p.Data(), sampling);
  if (!q->L1_Search(max_work_now)) {
    CL.Rollback(cp);
    return NULL;
  }
  if (!sampling.sampled_) CL.InsertIntoMap(&queries_, p, q);
  return q;
}
void Blackboard::L1_DeletingQuery(Query *q){
  OPattern p = OPattern::Make(q->pattern_);
  Query ** stored_query = queries_ % p;
  if (stored_query && *stored_query == q) {
    CL.RemoveFromMap(&queries_, p);
  }
}
TupleInfo * Blackboard::GetTupleInfo(OTuple t){
  TupleInfo *const * find = tuple_info_ % t;
  if (find) return *find;
  return NULL;
}

uint64 Blackboard::GetWildcardMatches(OTuple wildcard_tuple, 
				      vector<OTuple> * results) const {
  if (results) results->clear();
  IndexRow * ir = GetIndexRow(wildcard_tuple);
  if (!ir) return 0;
  if (results) {
    forall(run, ir->tuples_) {
      results->push_back(run->second->tuple_);
    }
  }
  return ir->size();
}

uint64 Blackboard::GetNumWildcardMatches(OTuple wildcard_tuple) const {
  return GetWildcardMatches(wildcard_tuple, NULL);
}

IndexRow * Blackboard::GetIndexRow(OTuple wildcard_tuple) const{
  IndexRow *const* find = index_ % wildcard_tuple;
  if (find) return *find;
  return NULL;
}
IndexRow * Blackboard::GetAddIndexRow(OTuple wildcard_tuple){
  IndexRow ** find = index_ % wildcard_tuple;
  if (find) return *find;
  return new IndexRow(wildcard_tuple, this);
}

const IndexRow * Blackboard::GetConstIndexRow(OTuple wildcard_tuple) const {
  return ((Blackboard *)(this))->GetIndexRow(wildcard_tuple);
}
const TupleInfo * Blackboard::GetConstTupleInfo(OTuple tuple) const {
  return ((Blackboard *)(this))->GetTupleInfo(tuple);
}

// Random tuples
bool Blackboard::GetRandomTuple(Tuple * result) {
  if (tuple_info_.size() == 0) return false;
  pair<OTuple, TupleInfo *> sample;
  GetSampleElement(tuple_info_, &sample);
  *result = sample.first.Data();
  return true;
}

bool Blackboard::GetRandomTupleMatching(Tuple * result, const Tuple& wildcard_t) {
  IndexRow * ir = GetIndexRow(OTuple::Make(wildcard_t));
  if (!ir) return false;
  pair<Time, TupleInfo*> sample;
  GetSampleElement(ir->tuples_, &sample);
  *result = sample.second->GetTuple().Data();
  return true;
}

// We select a random tuple which contains all of the terms.
// If situation_distribution is false, this selection is uniform. 
// If situation_distribution is true, we first select a situation uniformly.  
// A situation  is determined by the length of the tuple, the positions of the
// given terms in the tuple, and the first term in the tuple (which is likely
// to be a relation name).  

bool Blackboard::GetRandomTupleContaining(Tuple * ret, 
					  const set<Object>& terms,
					  bool situation_distribution) {
  vector<Object> v_terms;
  v_terms.insert(v_terms.end(), terms.begin(), terms.end());
  CHECK(ret != NULL);
  ret->clear();

  // if (situation_distribution) then already_considered is the number of situations 
  // considered.  Otherwise, it is the number of tuples already considered.
  int already_considered = 0;
  int n = terms.size();

  // Run through the situation by lengths
  forall(run, lengths_) {
    uint length = run->first;
    if (length < terms.size()) continue;

    // Run through the situation by positioning of the terms
    for (PermutationIterator run_p(n, length); !run_p.done(); ++run_p){

      // Creates a wildcard tuple for this situation
      const vector<int> & perm = run_p.current();
      Tuple t;
      for (uint i=0; i<perm.size(); i++) {
	t.push_back((perm[i]==EMPTY_SLOT)?WILDCARD:v_terms[perm[i]]);
      }

      IndexRow * ir = GetIndexRow(OTuple::Make(t));
      if (!ir) continue;

      if (situation_distribution && (t[0] == WILDCARD)) {
	forall(run, ir->first_term_counts_) {
	  t[0] = run->first;
	  IndexRow * ir_special = GetIndexRow(OTuple::Make(t));
	  CHECK(ir_special);
	  already_considered++;
	  if (rand() % already_considered == 0) {
	    CHECK(ir_special->GetRandomTuple(ret));
	  }
	}
      } else {
	bool to_select = false;
	if (situation_distribution) {
	  already_considered++;
	  to_select = (rand() % already_considered == 0);
	} else {
	  already_considered += ir->size();
	  to_select = (rand() % already_considered < (int) ir->size());
	}
	if (to_select) {
	  CHECK(ir->GetRandomTuple(ret));
	}
      }
    }
  }
  if (ret->size()) return true;
  return false;
}

void Blackboard::L1_FlushUpdates() {
  flushed_query_update_.clear();
  flushed_wt_update_.clear();
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
  alloc_vector<pair<BitSeq, int> > coordinates;
  coordinates.push_back(make_pair(BitSeq(), rand()%3 + 1));
  return Time(coordinates);
}
Object RandomConstant() {
  return Integer::Make(rand() % 3 + 1);
}
Object RandomVariable() {
  return Variable::Make("v" + itoa(rand() % 3) );
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
    p.push_back(RandomVariableTuple());
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
OTime Blackboard::FindTupleTime(OTuple t) const{
  const TupleInfo * ti = GetConstTupleInfo(t);
  if (!ti) {
    cerr << "Couldn't find tuple " << t << endl;
  }
  CHECK(ti);
  return OTime::Make(ti->FirstTime());
}
OTime Blackboard::FindLastTime(const Pattern & p) const {
  OTime last = CREATION;
  for (uint i=0; i<p.size(); i++) last = DataMax(last, FindTupleTime(p[i]));
  return last;
}


void Blackboard::RandomTest() {
  CL.MakeChangesPermanent();
  rankset<Posting *> postings;
  rankset<LoggingQuerySubscription *> subscriptions;
  int reps = 100000;
  uint max_postings = 20;
  uint max_subscriptions = 20;
  bool chacha = true;
  int first_rep = chacha?0:1;
  
  for (int rep=first_rep; rep<reps; rep++) {
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
    case 0: { // Add a posting
      if (postings.size() == max_postings) break;
      OTuple t = RandomConstantTuple();
      Time tm = RandomTime();
      cout << "add posting " << t << " " << tm.ToString() << endl;
      Checkpoint cp = CL.GetCheckpoint();
      for (int rep=first_rep; rep<2; rep++) {
	Posting * p = new Posting(t, tm, this);
	CL.InsertIntoSet(&postings, p);
	if (rep==0) CL.Rollback(cp);
      }
      break;
    }
    case 1: {// Delete a posting
      if (postings.size() == 0) break;
      Posting *p = *(postings.nth(rand() % postings.size()));
      cout << "remove posting " << p->tuple_.ToString() << " " 
	   << p->time_.ToString() << endl;
      Checkpoint cp = CL.GetCheckpoint();
      for (int rep=first_rep; rep<2; rep++) {      
	p->L1_Erase();
	CL.RemoveFromSet(&postings, p);
	if (rep==0) CL.Rollback(cp);
      }
      break;
    }
    case 2: {// Add a query;
      if (subscriptions.size() == max_subscriptions) break;
      OPattern p = RandomPattern();
      UpdateNeeds needs = RandomUpdateNeeds();
      Checkpoint cp = CL.GetCheckpoint();
      for (int rep=0; rep<2; rep++) {      
	cout << "Adding a query " << p << " needs=" << needs << endl;
	Query * q = L1_GetExecuteQuery(p, SamplingInfo(), NULL);
	cout << " Number of satisfactions:  " << q->GetCount() << endl;
	if (q->GetCount() < 10) {
	  vector<Map> subs;
	  vector<Time> times;
	  q->GetSubstitutions(&subs, &times);
	  CHECK(subs.size() == q->GetCount());
	  for (uint i=0; i<subs.size(); i++) {
	    cout << "    " << OMap::Make(subs[i]) << " " << times[i].ToString()
		 << endl;
	  }
	}
	LoggingQuerySubscription *s = new LoggingQuerySubscription(q, needs);
	CL.InsertIntoSet(&subscriptions, s);
	if (rep==0) CL.Rollback(cp);
      }
      break;
    }
    case 3: { // Delete a query;
      if (subscriptions.size()==0) break;
      Checkpoint cp = CL.GetCheckpoint();
      LoggingQuerySubscription * s = 
	*(subscriptions.nth(rand() % subscriptions.size()));
      for (int rep=first_rep; rep<2; rep++) {      
	cout << "Deleting a query " << s->ToString() << endl;
	s->L1_Erase();
	CL.RemoveFromSet(&subscriptions, s);
	if (rep==0) CL.Rollback(cp);
      }
      break;
    }
    }
    Verify();
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
    if (command == "MakePattern") {
      OPattern * p = new OPattern;
      cin >> (*p);
      cout << "Made pattern " << (*p) << endl;
      delete p;
    }
    if (command == "rt") {
      b.RandomTest();
      continue;
    }
    if (command == "rollback"){
      cout << CL.ToString();
      cout << "Rolling back" << endl;      
      CL.Rollback(0);
      postings.clear();
      subscriptions.clear();
    }
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
    }
    if (command == "query") {
      OPattern p;
      cin >> p;
      Query *q = b.L1_GetExecuteQuery(p, SamplingInfo(), NULL);
      CHECK(q);
      cout << q->ToString() << endl;
      cout << "#matches:" << q->GetCount() << endl;
      vector<Map> subs;
      q->GetSubstitutions(&subs, NULL);
      for (uint c=0; c<10 && c<subs.size(); c++) {
	OPattern ps = Substitute(subs[c], p);
	cout << ps << endl;
      }
    }
    b.Verify();
  };
  
}

string Posting::ShortDescription() const {
  string ret = "POSTING " + GetTuple().ToString();
  return ret;
}
Record Posting::GetRecordForDisplay() const { 
  Record ret;
  ret["type"] = "POSTING";
  ret["tuple"] = HTMLLink(blackboard_->GetTupleInfo(tuple_)->GetURL(), 
			  GetTuple().ToString());
  ret["time"] = OTime::Make(time_).ToString();
  return ret;
}

string TupleInfo::ShortDescription() const {
  return "TUPLE " + HTMLLink(GetURL(), tuple_.ToString());
}
string TupleInfo::GetURL() const {
  return "tupleinfo?tuple=" + URLEscape(tuple_.ToString());
}
Record TupleInfo::GetRecordForDisplay() const { 
  Record ret;
  ret["type"] = "TUPLEINFO";
  ret["tuple"] = tuple_.ToString();
  ret["first time"] = OTime::Make(FirstTime()).ToString();
  ret["postings"] += "<table>";
  forall(run, postings_) {
    ret["postings"] += "<tr><td>" + OTime::Make(run->first).ToString()
      + "</td><td>" + run->second->ShortDescription() + "</td></tr>\n";
  }
  ret["postings"] += "</table>";
  ret["generalizations"] = blackboard_->GetIndexRow(tuple_)
    ->GetGeneralizationLinks();
  return ret;
}
string IndexRow::GetURL() const {
  return "indexrow?tuple=" + URLEscape(wildcard_tuple_.ToString());
}
string IndexRow::ShortDescription() const {
  return "INDEXROW " + HTMLLink(GetURL(), wildcard_tuple_.ToString());  
}
Record IndexRow::GetRecordForDisplay() const {
  Record ret;
  ret["type"] = "INDEXROW";
  ret["wildcard tuple"] = wildcard_tuple_.ToString();
  ret["tuples"] += "<table>";
  forall(run, tuples_) {
    ret["tuples"] += "<tr><td>" + OTime::Make(run->first).ToString()
      + "</td><td>" + run->second->ShortDescription() + "</td></tr>\n";
  }
  ret["tuples"] += "</table>";
  ret["num_tuples"] = itoa(tuples_.size());
  ret["generalizations"] = GetGeneralizationLinks();
  return ret;
}
string IndexRow::GetGeneralizationLinks() const {
  string ret = "[ ";
  Tuple t = wildcard_tuple_.Data();
  for (uint i=0; i<t.size(); i++) {
    if (t[i] == WILDCARD) {
      ret += "* ";
      continue;
    }
    Tuple generalized = t;
    generalized[i] = WILDCARD;
    ret += 
      HTMLLink(blackboard_->GetIndexRow(OTuple::Make(generalized))->GetURL(),
	       (t[i].ToString())) + " ";
  }
  ret += "]";
  return ret;
}

string Blackboard::GetURL() const {
  return "blackboard";
}
Record Blackboard::GetRecordForDisplay() const {
  Record ret;
  ret["type"] = "BLACKBOARD";
  ret["num tuples"] = itoa(tuple_info_.size());
  ret["tuples"] = "<table>"; 
  forall(run, tuple_info_) {
    ret["tuples"] += "<tr><td>" + run->second->ShortDescription() 
      + "</td><td>" + run->second->FirstTime().ToString() + "</tr>\n";
  }
  ret["tuples"] += "</table>";
  return ret;
}

string Blackboard::CollectCOUT() const { 
  vector<OTuple> v;
  OTuple t = StringToObject("(COUT *)");
  GetWildcardMatches(t, &v);
  string ret;
  for (uint i=0; i<v.size(); i++) {
    Object second = v[i].Data()[1];
    if (second.GetType()==Object::STRING)
      ret += String(second).Data();
  }
  return ret;
}
