// Copyright (C) 2006 Google Inc. and Georges Harik
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
// Author: Noam Shazeer and Georges Harik

#include "searchtree.h"

// TODO: Changing counts with CL?

// Parents is a count of things that need you to exist
void Query::L1_AddParent() { 
  CL.ChangeValue(&parent_count_, parent_count_+1);
}
void Query::L1_RemoveParent() { 
    CL.ChangeValue(&parent_count_, parent_count_-1);
    CHECK(parent_count_ >=0);
    if (parent_count_==0) L1_Erase();
}
void Query::L1_Erase() {
  CHECK(needs_ == 0);
  blackboard_->L1_ChangeNumNonupdatedQueries(-1);
  CHECK(parent_count_ == 0);
  if (search_) search_->L1_Erase();
  CL.Destroying(this);
}

uint64 Query::GetCount() const {
  return search_->count_;
}

void Query::GetSubstitutions(vector<Map> * substitutions) const {
    return search_->GetSubstitutions(substitutions);
}

void Query::L1_RecomputeUpdateNeeds(){
  UpdateNeeds old_needs = needs_;
  UpdateNeeds new_needs = 0;
  forall(run, subscriptions_) new_needs |= run->first;
  if (new_needs==old_needs) return;
  blackboard_->L1_ChangeNumNonupdatedQueries((new_needs?0:1)-(old_needs?0:1));
  search_->L1_ChangeUpdateNeeds(new_needs);
  CL.ChangeValue(&needs_, new_needs);
}

bool Query::L1_Search(int64 *max_work_now){
  if (pattern_.size() == 0) {
    new NoTuplesSearch(this);
    return search_->L1_Search(max_work_now);
  }
  if (pattern_.size() == 1) {
     const Tuple& t = pattern_[0].Data();
     if (!HasDuplicateVariables(t)) {
       new OneTupleSearch(this);
       return search_->L1_Search(max_work_now);
     }
  }

  // This is now a condition node or a partition node
  vector<uint64> num_matches;
  for (uint i=0; i<pattern_.size(); i++)  {
    const Tuple& t = pattern_[i].Data();
    num_matches.push_back(blackboard_->GetNumWildcardMatches
			  (OTuple::Make(VariablesToWildcards(t))));
  }
  int best_tuple 
    = min_element(num_matches.begin(), num_matches.end())-num_matches.begin();
  uint64 least_matches = num_matches[best_tuple];
  int num_components = GetConnectedComponents(pattern_, NULL);
  if ( (num_components > 1) && (least_matches > 0) ) {
    // Partition node TODO
    new PartitionSearch(this);
    return search_->L1_Search(max_work_now);
    return false;
  }

  // Condition node
  new ConditionSearch(this, best_tuple);
  return search_->L1_Search(max_work_now);
}
 
NoTuplesSearch::NoTuplesSearch(Query *query) 
  : Search(query) {
}

OneTupleSearch::OneTupleSearch(Query *query) 
  : Search(query) {
}

bool OneTupleSearch::L1_Search(int64 * max_work_now) {
  Blackboard & bb = *query_->blackboard_;
  count_ = bb.GetNumWildcardMatches(GetWildcardTuple());
  if (query_->sampling_.sampled_) {
    CHECK(query_->sampling_.position_ == 0);
    count_ = RandomRoundoff(double(count_) * query_->sampling_.fraction_);
  }
  return true;
}

void OneTupleSearch::GetSubstitutions(vector<Map> * substitutions,
				      vector<Time> * times) const {
  substitutions->clear(); 
  if (times) times->clear();
  Blackboard & bb = *query_->blackboard_;
  const Tuple &t = query_->pattern_[0].Data();
  IndexRow * ir = bb.GetIndexRow(GetWildcardTuple());
  if (ir == NULL) {
    return;
  }
  
  IndexRow::TuplesType * tuples = &ir->tuples_;
  IndexRow::TuplesType sampled_tuples;
  if (query_->sampling_.sampled_) {
    GetSample(ir->tuples_, &sampled_tuples, count_);
    tuples = &sampled_tuples;
  }

  forall(run_tuples, *tuples) {
    TupleInfo * ti = run_tuples->second;
    OTuple substituted_t = ti->tuple_;
    Map sub;
    bool res = ComputeSubstitution(t, substituted_t.Data(), &sub);
    // We're not allowing duplicate variables, substitution must work
    CHECK(res);
    substitutions->push_back(sub);
    if (times) times->push_back(ti->FirstTime());
  }
}
void OneTupleSearch::L1_ChangeUpdateNeeds(UpdateNeeds new_needs){
  if (!wt_subscription_) {
    CHECK(query_->needs_);
    wt_subscription_ = 
      new OneTupleWTSub(query_->blackboard_, 
			GetWildcardTuple(), new_needs, this);
    return;
  }
  if (new_needs == 0) {
    wt_subscription_->L1_Erase();
    return;
  }
  wt_subscription_->L1_ChangeNeeds(new_needs);
}
void OneTupleSearch::Update(const WTUpdate &update, 
			    const OneTupleWTSub *subscription){
  count_ += update.count_delta_;
  QueryUpdate out_update;
  out_update.count_delta_ = update.count_delta_;
  QueryUpdate out_update_with_subs = out_update;
  QueryUpdate out_update_with_times = out_update;

  if (query_->needs_ & UPDATE_WHICH) {
    forall(run, update.changes_) {
      Map sub;
      CHECK(ComputeSubstitution(query_->pattern_[0].Data(),
				run->first.Data(), &sub));
      out_update_with_times.changes_.push_back
	(make_pair(OMap::Make(sub), run->second));
      if (!(run->second.first && run->second.second)) {
	out_update_with_subs.changes_.push_back
	  (out_update_with_times.changes_.back());
      }
    }
  }
  forall(run_needs, query_->subscriptions_) {
    // could possibly optimize this by not building a whole new
    // update for every set of needs.
    UpdateNeeds needs = run_needs->first;
    QueryUpdate * out = &out_update;
    if (needs && UPDATE_WHICH) out = &out_update_with_subs;
    if (needs && UPDATE_TIME) out = &out_update_with_times;
    forall(run, run_needs->second) {
      (*run)->Update(*out);
    }
  }
}


PartitionSearch::PartitionSearch(Query *query)
  :Search(query) {}

bool PartitionSearch::L1_Search(int64 * max_work_now) {
  int num_parts = GetConnectedComponents(query_->pattern_, &partition_);
  vector<Pattern> parts(num_parts);
  for(uint i=0; i<query_->pattern_.size(); i++) {
    parts[partition_[i]].push_back(query_->pattern_[i]);
  }
  count_ = 1;
  for (int i=0; i<num_parts; i++) {
    SamplingInfo sub_sampling = query_->sampling_.LimitToPart(partition_, i);
    Query * q = new Query(query_->blackboard_, parts[i], sub_sampling);
    q->L1_AddParent();
    children_.push_back(make_pair(q, (PartitionQSub*)NULL) );
    bool res = q->L1_Search(max_work_now);
    count_ *= q->GetCount();
    if (!res) return false;
  }
  return true;
}

void PartitionSearch::GetSubstitutions(vector<Map> * substitutions
				       vector<Time> * times) const {
  substitutions->clear();
  if (times) times->clear();
  vector<vector<Map> > child_subs(children_.size());
  vector<vector<Time> > child_times(children_.size());
  vector<uint> bounds;
  for (uint i=0; i<children_.size(); i++) {
    children_[i].first->GetSubstitutions(&child_subs[i],
					 times?(&(child_times[i])):NULL);
    bounds.push_back(child_subs[i].size());
  }
  for (ProductIterator run(bounds); !run.done(); ++run) {
    Map m;
    Time t;
    for (uint i=0; i<children_.size(); i++) {
      const Map & cs = child_subs[i][run.Current()[i]];
      m.insert(cs.begin(), cs.end());
      if (times) t = max(t, child_times[i][run.Current()[i]]);
    }
    substitutions->push_back(m);
    if (times) times->push_back(t);
  }
}
void PartitionSearch::L1_EraseSubclass() {
  forall(run, children_) run->first->L1_RemoveParent();
}
void PartitionSearch::Update(const QueryUpdate &update, const PartitionQSub *subscription){
  // TODO
}
void PartitionSearch::L1_ChangeUpdateNeeds(UpdateNeeds new_needs){
  forall(run, children_) {
    PartitionQSub * & sub_ref = run->second;
    if (sub_ref == NULL) {
      CHECK(query_->needs_);
      sub_ref = new PartitionQSub(this, run->first, new_needs);
      continue;
    }
    if (new_needs == 0) {
      sub_ref->L1_Erase();
      sub_ref = NULL;
      continue;
    }
    sub_ref->L1_ChangeNeeds(new_needs);
  }
}

ConditionSearch::ConditionSearch(Query * query, int condition_tuple)
  : Search(query), condition_tuple_(condition_tuple) {
}

// If the specifcation matches the variable tuple, adds a child query.
bool ConditionSearch::MaybeAddChild(OTuple specification, int64 *max_work_now, 
				    Query ** child_query) { 
  OTuple substituted_t = ti->tuple_;
  Map sub;
  bool res = ComputeSubstitution(GetVariableTuple(), 
				 specification.Data(), &sub);
  if (!res) {
    // tuple doesn't match;
    if (child_query) *child_query = NULL; 
    return true;
  };
  SamplingInfo sub_sampling
    = query_->sampling_.RemovePosition(condition_tuple_);
  Pattern sub_pattern 
    = Substitute(sub, RemoveFromVector(query_->pattern_, condition_tuple_));    
  Query * q = new Query(&bb, sub_pattern, sub_sampling);
  q->L1_AddParent();
  children_[substituted_t] = make_pair(q, (ConditionQSub*)NULL);
  res = q->L1_Search(max_work_now);
  count_ += q->GetCount();
  if (!res) return false;
  if (child_query) *child_query = q;
  return true;
}

bool ConditionSearch::L1_Search(int64 * max_work_now) {
  Blackboard & bb = *(query_->blackboard_);
  const Tuple& t = query_->pattern_[condition_tuple_].Data();  
  IndexRow * ir = bb.GetIndexRow(GetWildcardTuple());
  if (ir == NULL) {
    count_ = 0;
    return true;
  }
  // we will either be running over ir->tuples_, or a sampled subset of it.
  IndexRow::TuplesType * tuples = &ir->tuples_;
  IndexRow::TuplesType sampled_tuples;
  if (query_->sampling_.sampled_ 
      && query_->sampling_.position_ == condition_tuple_) {
    int sample_size = RandomRoundoff((double)ir->tuples_.size()
				     *query_->sampling_.fraction_);
    MOREWORK(sample_size);
    GetSample(ir->tuples_, &sampled_tuples, sample_size);
    tuples = &sampled_tuples;
  } else {
    // Have to run through all of these and compute substitutions, so costs work
    MOREWORK(ir->tuples_.size());
  }

  count_ = 0;
  forall(run_tuples, *tuples) {
    if (!MaybeAddChild(run_tuples->second->tuple_, max_work_now, NULL))
      return false;
  }
  return true;
}

void ConditionSearch::GetSubstitutions(vector<Map> * substitutions,
				       vector<Time> * times) const {
  substitutions->clear();
  if (times) times->clear();
  const Tuple& condition_tuple = query_->pattern_[condition_tuple_].Data();  
  forall(run, children_) {
    OTuple specific_tuple = run->first;
    Time tuple_time;
    if (times) {
      TupleInfo *ti = query_->blackboard_->GetTupleInfo(specific_tuple);
      CHECK(ti);
      tuple_time = ti->FirstTime();
    }
    Query * child_query = run->second.first;
    Map sub;
    CHECK(ComputeSubstitution(condition_tuple, specific_tuple.Data(), &sub));
    vector<Map> child_subs;
    vector<Time> child_times;
    child_query->GetSubstitutions(&child_subs, times?(&child_times):NULL);
    for (uint i=0; i<child_subs.size(); i++) {
      Add(&(child_subs[i]), sub);
      substitutions->push_back(child_subs[i]);
      if (times) times->push_back(max(tuple_time, child_times[i]));
    }
  }
}
void ConditionSearch::L1_EraseSubclass() {
  forall(run, children_) run->second.first->L1_RemoveParent();
}

void ConditionSearch::Update(const WTUpdate &update, 
			     const ConditionWTSub *subscription){
  // TODO: what about sampling.  Maybe we won't update sampled queries.
  QueryUpdate out_update, out_update_with_subs, out_update_with_times;
  int64 old_count = count_;

  bool need_subs = query_->needs_ & UPDATE_WHICH;
  bool need_times = query_->needs_ & UPDATE_TIME;
  Time dummy_time;

  forall(run, update.changes_){
    if (run->second.first == NULL) {      
      // add a tuple
      Query * child_query;
      L1_MaybeAddChild(run->first, NULL, &child_query);
      if (child_query) {
	if (need_subs) {
	  vector<Map> subs;
	  vector<Time> times;
	  child_query->GetSubstitutions(&subs, need_times?(&times):NULL);
	  if (need_times) {
	    for (uint i=0; i<times.size(); i++) times[i] 
	      = max(times[i], run->second.second);
	  }
	  for (uint i=0; i<subs.size(); i++) {
	    out_update_with_subs.changes_.push_back
	      (make_pair(
	  }
	}
      }
      continue;
    }
    if (run->second.second == NULL) {
      // remove a tuple
      continue;
    }
    // change time on a tuple
  }
  
  out_update.count_delta_ = 
    out_update_with_subs.count_delta_ = 
    out_update_with_times.count_delta_ = count_ - old_count;


  if (query_->needs_ & UPDATE_WHICH) {
    forall(run, update.changes_) {
      Map sub;
      CHECK(ComputeSubstitution(query_->pattern_[0].Data(),
				run->first.Data(), &sub));
      out_update_with_times.changes_.push_back
	(make_pair(OMap::Make(sub), run->second));
      if (!(run->second.first && run->second.second)) {
	out_update_with_subs.changes_.push_back
	  (out_update_with_times.changes_.back());
      }
    }
  }
  forall(run_needs, query_->subscriptions_) {
    // could possibly optimize this by not building a whole new
    // update for every set of needs.
    UpdateNeeds needs = run_needs->first;
    QueryUpdate * out = &out_update;
    if (needs && UPDATE_WHICH) out = &out_update_with_subs;
    if (needs && UPDATE_TIME) out = &out_update_with_times;
    forall(run, run_needs->second) {
      (*run)->Update(*out);
    }
  }
}
void ConditionSearch::Update(const QueryUpdate &update, 
			     const ConditionQSub *subscription){
  // TODO
}
void ConditionSearch::L1_ChangeUpdateNeeds(UpdateNeeds new_needs){
  forall(run, children_) {
    ConditionQSub * & sub_ref = run->second.second;
    if (sub_ref == NULL) {
      CHECK(query_->needs_);
      sub_ref = new ConditionQSub(this, run->second.first, new_needs);
      continue;
    }
    if (new_needs == 0) {
      sub_ref->L1_Erase();
      sub_ref = NULL;
      continue;
    }
    sub_ref->L1_ChangeNeeds(new_needs);
  }
  if (!wt_subscription_) {
    CHECK(query_->needs_);
    wt_subscription_ = 
      new ConditionWTSub(query_->blackboard_, 
			 query_->pattern_[condition_tuple_], new_needs, this);
    return;
  }
  if (new_needs == 0) {
    wt_subscription_->L1_Erase();
    return;
  }
  wt_subscription_->L1_ChangeNeeds(new_needs);
}


QuerySubscription::QuerySubscription(Query *subscribee, UpdateNeeds needs)
  :subscribee_(subscribee), needs_(needs) {
  CL.Make(new MapOfSetsInsertChange<UpdateNeeds, QuerySubscription*>
	  (&(subscribee_->subscriptions_), needs_, this));
  subscribee_->L1_RecomputeUpdateNeeds();
  subscribee_->L1_AddParent();
}
void QuerySubscription::L1_Erase(){
  CL.Make(new MapOfSetsRemoveChange<UpdateNeeds, QuerySubscription*>
	  (&(subscribee_->subscriptions_), needs_, this));
  subscribee_->L1_RecomputeUpdateNeeds();
  subscribee_->L1_RemoveParent();
}
void QuerySubscription::L1_ChangeNeeds(UpdateNeeds new_needs){
  CL.Make(new MapOfSetsRemoveChange<UpdateNeeds, QuerySubscription*>
	  (&(subscribee_->subscriptions_), needs_, this));
  CL.ChangeValue(&needs_, new_needs);
  CL.Make(new MapOfSetsInsertChange<UpdateNeeds, QuerySubscription*>
	  (&(subscribee_->subscriptions_), needs_, this));
  subscribee_->L1_RecomputeUpdateNeeds();
}

OPattern QuerySubscription::GetPattern() const {
  // TODO
  return OPattern();
}

/*


Model * SearchTree::GetModel() {
  CHECK(precondition_);
  return precondition_->GetModel();
}


SearchTree::SearchTree(Pattern pattern,
		       TupleIndex   *tupleindex,
		       Precondition *precondition,
		       const SamplingInfo & sampling) {
  CHECK (! (tupleindex && precondition) );

  pattern_ = pattern;
  precondition_ = precondition;
  sampling_ = sampling;

  CHECK(!(sampling.sampled_ && LinkedToModel()));

  if (LinkedToModel()) {
    tuple_index_ = GetModel()->GetTupleIndex();
    changelist_ = GetModel()->GetChangelist();
    changelist_->Creating(this);
  } else {
    tuple_index_ = tupleindex,
    changelist_ = new Changelist();
  }
  root_ = new SearchNode(this, NULL);
}

// Called when
// 1 - you're attached and rolling back
// 2 - you've been erased, and it's made permanent
// 3 - you're independent and you're going away
// All of the normal destructors with changelists do nothing but free memory
// This is slightly different because it can be independent if not linked
// to a model
SearchTree::~SearchTree() {
  // You own your own changelist, roll it back and get rid of it
  if (!LinkedToModel()) {
    changelist_->Rollback(0);
    delete changelist_;
  }
}

// This better only be called when you're attached
void SearchTree::L1_Erase() {
  CHECK(LinkedToModel());
  root_->L1_Erase();
  changelist_->Destroying(this);
}

bool SearchTree::L1_Search(int64 *max_work_now) {
  return root_->L1_Search(max_work_now);  
}


SearchNode::SearchNode(SearchTree * tree, SearchNode * parent) {
  tree_ = tree;
  parent_ = parent;
  split_tuple_ = -1;
  num_satisfactions_ = 0;
  work_ = 0;
  GetChangelist()->Creating(this);
  tuple_to_child_ = NULL;
  child_to_tuple_ = NULL;
  partition_ = NULL;
  type_ = BABY;
  BabyCheck();
}

Model * SearchNode::GetModel() const { 
  return tree_->GetModel();
}
Changelist * SearchNode::GetChangelist() const {
  return tree_->changelist_;
}
Precondition * SearchNode::GetPrecondition() const {
  return tree_->precondition_;
}
TupleIndex * SearchNode::GetTupleIndex() const {
  return tree_->tuple_index_;
}
bool SearchNode::LinkedToModel() const {
  return tree_->LinkedToModel();
}
set<SearchNode *> SearchNode::GetChildren() const {
  set<SearchNode *> ret;
  if (type_ == SPLIT) {
    CHECK(child_to_tuple_);
    forall(run, *child_to_tuple_){
      ret.insert(run->first);
    }
  } else if (type_ == PARTITION){
    for (uint i=0; i<partition_->size(); i++) 
      ret.insert((*partition_)[i]);
  }
  return ret;
}

SearchNode * SearchNode::L1_AddSplitChild(Tuple tuple){
  CHECK(type_ == SPLIT);
  SearchNode * newchild = new SearchNode(tree_, this);
  GetChangelist()->Make
    (new MapInsertChange<Tuple, SearchNode *>(tuple_to_child_, 
					      tuple, newchild));
  GetChangelist()->Make
    (new MapInsertChange<SearchNode *, Tuple>(child_to_tuple_, 
					      newchild, tuple));
  return newchild;
}
void SearchNode::L1_RemoveSplitChild(Tuple tuple){
  CHECK(type_ == SPLIT);
  SearchNode * child = (*tuple_to_child_)[tuple];
  CHECK(child);
  child->L1_Erase();
  GetChangelist()->Make
    (new MapRemoveChange<Tuple, SearchNode *>(tuple_to_child_, tuple));
  GetChangelist()->Make
    (new MapRemoveChange<SearchNode *, Tuple>(child_to_tuple_, child));
}
void SearchNode::L1_RemoveAllSplitChildren(){
  CHECK(tuple_to_child_);
  while (tuple_to_child_->size()) {
    L1_RemoveSplitChild(tuple_to_child_->begin()->first);
  }
}

bool SearchNode::L1_MakePartition(int64 * max_work_now) {
  CHECK(type_ == BABY);
  VLOG(2) << "Partition - YEAHH BABY  YEAH BABY!!!!" << endl;
  vector<int> comp;
  Pattern pattern;
  GetPatternAndSampling(&pattern, NULL);
  VLOG(2) << "Make Partition pattern=" << TupleVectorToString(pattern)
	  << " max_work_now=" << (max_work_now?itoa(*max_work_now):"UNLIMITED")
	  << endl;
  GetChangelist()->ChangeValue(&partition_, 
			       new vector<SearchNode *>(pattern.size()));
  GetChangelist()->Creating(partition_);
  int num_components = GetConnectedComponents(pattern, &comp);
  vector<SearchNode *> new_components;
  for (int i=0; i<num_components; i++) 
    new_components.push_back(new SearchNode(tree_, this));
  for (uint i=0; i<pattern.size(); i++) {
    (*partition_)[i] = new_components[comp[i]];
  }
  L1_SetWork(1);
  L1_SetNumSatisfactions(0);
  A1_SetType(PARTITION);
  return true;
}
bool SearchNode::L1_MakeSplit(int split_tuple, int64 * max_work_now){
  CHECK(type_==BABY);
  A1_SetType(SPLIT);
  Pattern pattern;
  SamplingInfo pattern_sampling;
  GetPatternAndSampling(&pattern, &pattern_sampling);
  CHECK(pattern.size()>1);
  VLOG(2) << "Make Split pattern=" << TupleVectorToString(pattern)
	  << " max_work_now=" << (max_work_now?itoa(*max_work_now):"UNLIMITED")
	  << endl;
 
  // create the @#$*ing maps
  GetChangelist()->ChangeValue(&tuple_to_child_, new map<Tuple, SearchNode *>);
  GetChangelist()->Creating(tuple_to_child_);
  GetChangelist()->ChangeValue(&child_to_tuple_, new map<SearchNode *, Tuple>);
  GetChangelist()->Creating(child_to_tuple_);

  Tuple tuple = pattern[split_tuple];
  L1_SetSplitTuple(split_tuple);
  
  SamplingInfo tuple_sampling;
  if (split_tuple == (int)pattern_sampling.position_) 
    tuple_sampling = pattern_sampling;

  uint64 num_matches = 
    GetNumWildcardMatches(tuple, tuple_sampling);
  MOREWORK(num_matches);
  vector<Tuple> matches;
  GetWildcardMatches(tuple, tuple_sampling, &matches);
  for (uint i=0; i<matches.size(); i++) {
    Substitution sub;
    if (!ComputeSubstitution(tuple, matches[i], &sub)) continue;
    L1_AddSplitChild(matches[i]);
  }
  return true;
}
bool SearchNode::L1_MakeNoTuples() {
  CHECK(type_==BABY);
  L1_SetNumSatisfactions(1);
  L1_SetWork(1);
  A1_SetType(NO_TUPLES);
  return true;
}
bool SearchNode::L1_MakeOneTuple(int64* max_work_now) {
  CHECK(type_==BABY);
  A1_SetType(ONE_TUPLE);
  Pattern pattern;
  SamplingInfo sampling;
  GetPatternAndSampling(&pattern, &sampling);
  VLOG(2) << "MakeOneTuple pattern=" << TupleVectorToString(pattern)
	  << " max_work_now=" << (max_work_now?itoa(*max_work_now):"UNLIMITED")
	  << endl;
  CHECK(pattern.size()==1);
  Tuple tuple = pattern[0];
  L1_SetSplitTuple(0);
  bool duplicate_vars = pattern[0].HasDuplicateVariables();
  uint64 num_matches = GetNumWildcardMatches(tuple, sampling);
  L1_SetWork(num_matches);
  if (!duplicate_vars){
    MOREWORK(1);
    L1_SetNumSatisfactions(num_matches);
  } else {
    MOREWORK(num_matches);
    vector<Tuple> matches;
    GetWildcardMatches(pattern[0], sampling, &matches);
    uint64 num_sat = 0;
    for (uint i=0; i<matches.size(); i++) {
      Substitution sub;
      if (!ComputeSubstitution(pattern[0], matches[i], &sub)) continue;
      num_sat++;
    }
    L1_SetNumSatisfactions(num_sat);
  }
  return true;
}
void SearchNode::L1_MakeBaby(){
  CHECK(type_ != BABY);
  if (type_ == ONE_TUPLE) {    
    L1_SetSplitTuple(-1);
  }
  if (type_ == SPLIT) {
    L1_RemoveAllSplitChildren();
    L1_SetSplitTuple(-1);
    GetChangelist()->Destroying(tuple_to_child_);
    GetChangelist()->Destroying(child_to_tuple_);
    GetChangelist()->ChangeValue(&tuple_to_child_, 
				 (typeof(tuple_to_child_))NULL);
    GetChangelist()->ChangeValue(&child_to_tuple_, 
				 (typeof(child_to_tuple_))NULL);
  }
  if (type_ == PARTITION) {
    set<SearchNode*> children_ = GetChildren();
    forall(run, children_) (*run)->L1_Erase();
    GetChangelist()->Destroying(partition_);
    GetChangelist()->ChangeValue(&partition_, (typeof(partition_))NULL);
  } 
  L1_SetWork(0);
  L1_SetNumSatisfactions(0);
  A1_SetType(BABY);  
  BabyCheck();
}

void SearchNode::BabyCheck() const {
  CHECK(type_==BABY);
  CHECK(tuple_to_child_==NULL);
  CHECK(child_to_tuple_==NULL);
  CHECK(partition_==NULL);
  CHECK(num_satisfactions_==0);
  CHECK(work_==0);
  CHECK(split_tuple_ == -1);
}

uint64 SearchNode::GetNumWildcardMatches(Tuple t, SamplingInfo sampling) const {
  return GetTupleIndex()->Lookup(t.VariablesToWildcards(), NULL, &sampling);
}
void SearchNode::
GetWildcardMatches(Tuple t, SamplingInfo sampling, vector<Tuple> *ret) const{
  GetTupleIndex()->Lookup(t.VariablesToWildcards(), ret, &sampling);
}

bool SearchNode::L1_Search(int64 * max_work_now) {
  Pattern pattern;
  SamplingInfo sampling;
  GetPatternAndSampling(&pattern, &sampling);
  VLOG(2) << "Search pattern=" << TupleVectorToString(pattern)
	  << " max_work_now=" << (max_work_now?itoa(*max_work_now):"UNLIMITED")
	  << endl;
    
  if (pattern.size()==0) return L1_MakeNoTuples();
  if (pattern.size()==1) return L1_MakeOneTuple(max_work_now);
  
  vector<uint64> num_matches;
  //HERE
  for (uint i=0; i<pattern.size(); i++) 
    num_matches.push_back
      (GetNumWildcardMatches(pattern[i], sampling.LimitToPosition(i)));
  int best_tuple 
    = min_element(num_matches.begin(), num_matches.end())-num_matches.begin();
  uint64 least_matches = num_matches[best_tuple];
  int num_components = GetConnectedComponents(pattern, NULL);
  if (least_matches == 0 || num_components==1) {
    if (!L1_MakeSplit(best_tuple, max_work_now)) return false;
  } else {
    if (!L1_MakePartition(max_work_now)) return false;
  }
  set<SearchNode *> children = GetChildren();
  forall(run, children) {
    if (!(*run)->L1_Search(max_work_now)) return false;
  }
  VLOG(2) << "Search returning true " << TupleVectorToString(pattern)
	  << endl;
  return true;
}

void SearchNode::
GetPatternAndSampling(Pattern * pattern, SamplingInfo * sampling) const{
  // Note the code would be simpler and slower implemented recursively.

  // includes this node and all ancestors up to but not including the root.
  vector<const SearchNode *> path_to_root;
  for (const SearchNode * n=this; n->parent_ != NULL; n = n->parent_) 
    path_to_root.push_back(n);
  // maps from the tuple in the pattern for the node under consideration
  // to the index of the corresponding tuple in the original pattern
  vector<int> tuple_to_tree_tuple;
  Substitution complete_sub;
  for (uint i=0; i<tree_->pattern_.size(); i++) {
    tuple_to_tree_tuple.push_back(i);
  }
  // Iterate from the root to this
  // the first value of the parent is the root.
  // the last value of child is this.
  // As we iterate, tuple_to_tree_tuple is accurate for parent.  
  for (int i=(int)path_to_root.size()-1; i>=0; i--) {
    const SearchNode *child = path_to_root[i];
    const SearchNode *parent = child->parent_;
    if (parent->type_ == SPLIT) {
      CHECK((*parent->child_to_tuple_) % (SearchNode *)child);
      Tuple variable_tuple 
	= tree_->pattern_[tuple_to_tree_tuple[parent->split_tuple_]];
      Tuple constant_tuple = (*parent->child_to_tuple_)[(SearchNode *)child];
      Substitution mini_sub;
      CHECK(ComputeSubstitution(variable_tuple, constant_tuple, &mini_sub));
      complete_sub.Add(mini_sub);
      tuple_to_tree_tuple 
	= RemoveFromVector(tuple_to_tree_tuple, parent->split_tuple_);
    } else if (parent->type_ == PARTITION) {
      vector<int> new_vec;
      for (uint i=0; i<tuple_to_tree_tuple.size(); i++) {
	if ((*parent->partition_)[i] == child) 
	  new_vec.push_back(tuple_to_tree_tuple[i]);
      }
      tuple_to_tree_tuple = new_vec;
      CHECK(tuple_to_tree_tuple.size() != 0);
    } else {
      CHECK(false);
    }
  }
  if (pattern) {
    *pattern = Pattern();
    for (uint i=0; i<tuple_to_tree_tuple.size(); i++) {
      pattern->push_back(tree_->pattern_[tuple_to_tree_tuple[i]]);
    }
    complete_sub.Substitute(pattern);
  }
  if (sampling) {
    *sampling = SamplingInfo();
    if (tree_->sampling_.sampled_) {
      int tree_tuple = tree_->sampling_.position_;
      for (uint i=0; i<tuple_to_tree_tuple.size(); i++) {
	if (tuple_to_tree_tuple[i]==tree_tuple) {
	  *sampling = tree_->sampling_;
	  sampling->position_ = i;
	}
      }
    }
  }
}

void SearchNode::A1_SetType(NodeType t){
  GetChangelist()->ChangeValue(&type_, t);
}

void SearchNode::L1_SetSplitTuple(int pos){
  CHECK(GetChildren().size()==0);
  CHECK(type_ == SPLIT || type_ == ONE_TUPLE);
  Pattern pattern;
  GetPatternAndSampling(&pattern, NULL);
  if (LinkedToModel() && split_tuple_ != -1) {
    GetChangelist()->Make
      (new MapOfSetsRemoveChange<Tuple, SearchNode *>
       (&GetModel()->wildcard_tuple_to_search_node_, 
	pattern[split_tuple_].VariablesToWildcards(), this));
  }
  GetChangelist()->ChangeValue(&split_tuple_, pos);			
  if (LinkedToModel() && split_tuple_ != -1) {
    GetChangelist()->Make
      (new MapOfSetsInsertChange<Tuple, SearchNode *>
       (&GetModel()->wildcard_tuple_to_search_node_, 
	pattern[split_tuple_].VariablesToWildcards(), this));
  }
}
void SearchNode::L1_SetNumSatisfactions(uint64 new_num_satisfactions){
  uint64 old_num_satisfactions = num_satisfactions_;
  GetChangelist()->ChangeValue(&num_satisfactions_, new_num_satisfactions);
  if (parent_) {
    if (parent_->type_ == SPLIT) {
      parent_->L1_SetNumSatisfactions
	(parent_->num_satisfactions_ + 
	 new_num_satisfactions - old_num_satisfactions);    
    } else if (parent_->type_ == PARTITION) {
      set<SearchNode *> children = parent_->GetChildren();
      uint64 product = 1;
      forall(run, children) {
	product *= (*run)->num_satisfactions_;
      }
      parent_->L1_SetNumSatisfactions(product);
    } else {
      CHECK(false);
    }
  } else {
    // we're the root
    if (LinkedToModel()) {
      GetPrecondition()->L1_AddToNumSatisfactions
	((int64)new_num_satisfactions - (int64)old_num_satisfactions);
    }
  }
}
void SearchNode::L1_SetWork(uint64 new_work){
  if (parent_) {
    parent_->L1_SetWork(parent_->work_ + new_work - work_);
  } else {
    if (LinkedToModel())
      GetModel()->A1_AddToSearchWork((int64)new_work - (int64)work_);
  }
  GetChangelist()->ChangeValue(&work_, new_work);
}
void SearchNode::L1_Erase(){
  L1_MakeBaby();
  GetChangelist()->Destroying(this);
}

void SearchNode::L1_AddTuple(Tuple new_tuple){
  CHECK(type_==ONE_TUPLE || type_==SPLIT);
  CHECK(!tree_->sampling_.sampled_);
  // See the comment in TrueTuple::TrueTuple() for why the tuple might already
  // be there.  (why we need the next line)
  if (type_==SPLIT && (*tuple_to_child_) % new_tuple) return;
  L1_SetWork(work_ + 1);
  Pattern pattern;
  GetPatternAndSampling(&pattern, NULL);
  if (!ComputeSubstitution(pattern[split_tuple_], new_tuple, NULL)) return;
  if (type_==ONE_TUPLE) {
    L1_SetNumSatisfactions(num_satisfactions_+1);
  } else if (type_ == SPLIT) {
    SearchNode * new_child = L1_AddSplitChild(new_tuple);
      new_child->L1_Search(NULL);
  } 
}
void SearchNode::L1_RemoveTuple(Tuple tuple){
  CHECK(type_==ONE_TUPLE || type_==SPLIT);
  CHECK(!tree_->sampling_.sampled_);
  L1_SetWork(work_ - 1);
  Pattern pattern;
  GetPatternAndSampling(&pattern, NULL);
  if (!ComputeSubstitution(pattern[split_tuple_], tuple, NULL)) return;
  if (type_==ONE_TUPLE) {
    L1_SetNumSatisfactions(num_satisfactions_-1);
  } else if (type_ == SPLIT) {
    L1_RemoveSplitChild(tuple);
  }
}

void SearchNode::GetSubstitutions(vector<Substitution> *substitutions) const{
  substitutions->clear();
  Pattern pattern;
  SamplingInfo sampling;
  GetPatternAndSampling(&pattern, &sampling);
  if (type_ == NO_TUPLES) {
    substitutions->push_back(Substitution());
    VerifyNumSatisfactions(substitutions->size());
    return;
  } 
  if (type_ == ONE_TUPLE) {
    vector<Tuple> matches;
    GetWildcardMatches(pattern[0], sampling, &matches);
    for (uint i=0; i<matches.size(); i++) {
      Substitution sub;
      if (!ComputeSubstitution(pattern[0], matches[i], &sub)) continue;
      substitutions->push_back(sub);
    }
    VerifyNumSatisfactions(substitutions->size());
    return;
  } 
  if (type_ == SPLIT) {
    forall(run, *tuple_to_child_) {
      Tuple tuple = run->first;
      vector<Substitution> partial_subs;
      run->second->GetSubstitutions(&partial_subs);
      Substitution additional_sub;
      CHECK(ComputeSubstitution(pattern[split_tuple_], tuple, &additional_sub));
      for (uint i=0; i<partial_subs.size(); i++) {
	partial_subs[i].Add(additional_sub);
	substitutions->push_back(partial_subs[i]);
      }	
    }
    VerifyNumSatisfactions(substitutions->size());
    return;
  }
  if (type_ == PARTITION) {
    set<SearchNode *> children = GetChildren();
    vector<vector<Substitution> > partial_subs;
    uint num_children = children.size();
    vector<uint> sizes;
    forall(run, children){
      partial_subs.push_back(vector<Substitution>());
      (*run)->GetSubstitutions(&partial_subs.back());
      sizes.push_back(partial_subs.back().size());
    }
    CHECK(sizes.size()==num_children);
    CHECK(partial_subs.size() == num_children);
    for (ProductIterator run(sizes); !run.done(); ++run){
      Substitution sub;
      for (uint i=0; i<num_children; i++) {
	sub.Add(partial_subs[i][run.Current()[i]]);
      }
      substitutions->push_back(sub);
    }
    VerifyNumSatisfactions(substitutions->size());
    return;
  }
  CHECK(false);  
}

void SearchNode::VerifyNumSatisfactions(uint64 ns) const{
  if (num_satisfactions_ == ns) return;
  Pattern p;
  GetPatternAndSampling(&p, NULL);
  cerr << "Wrong number of satisfactions " 
       << "num_satisfactions_ = " << num_satisfactions_
       << " ns=" << ns
       << " pattern=" << TupleVectorToString(p)
       << " Node type: " << type_
       << endl;
  CHECK(false);
}
*/

string QueryUpdate::ToString() const {
  string ret = "STUpdate { count_delta_=" + itoa(count_delta_) + "\n";
  for (uint i=0; i<changes_.size(); i++) {
    ret += "   " + changes_[i].first.ToString() + " : " 
	+ TimeToStringOrNothing(changes_[i].second.first) + "->" 
      + TimeToStringOrNothing(changes_[i].second.second) + "\n";
  }
  ret += "}\n";
  return ret;
}
