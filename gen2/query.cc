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

#include "query.h"

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
  blackboard_->L1_DeleteNonupdatedQuery(this);
  blackboard_->L1_DeletingQuery(this);
  CHECK(parent_count_ == 0);
  if (search_) search_->L1_Erase();
  CL.Destroying(this);
}

uint64 Query::GetCount() const {
  if (!search_) {
    cout << "Called GetCount without a search " << endl;
    cout << ToString() << endl;
    CHECK(false);
  }
  return search_->count_;
}

void Query::Verify() const {
  vector<Map> subs;
  vector<Time> times;
  GetSubstitutions(&subs, &times);
  if (subs.size() != search_->count_){
    cout << "Wrong number of substitutions for " << ToString() << endl;
    cout << "count_=" << GetCount() << " #subs=" << subs.size() << endl;    
    blackboard_->PrintBlackboard();
    CHECK(false);    
  }
  set<OMap> subs_set;
  for (uint snum=0; snum<search_->count_; snum++) {    
    Time latest;
    Pattern p_sub = Substitute(subs[snum], pattern_);
    for (uint tnum=0; tnum<pattern_.size(); tnum++) {
      TupleInfo * ti = blackboard_->GetTupleInfo(p_sub[tnum]);
      CHECK(ti);
      latest = max(latest, ti->FirstTime());
    }
    CHECK(latest == times[snum]);
    OMap osub = OMap::Make(subs[snum]);
    CHECK(!(subs_set % osub));
    subs_set.insert(osub);
  }
}

void Query::GetSubstitutions(vector<Map> * substitutions,
			     vector<Time> *times) const {
  return search_->GetSubstitutions(substitutions, times);
}

void Query::L1_RecomputeUpdateNeeds(){
  UpdateNeeds old_needs = needs_;
  UpdateNeeds new_needs = 0;
  forall(run, subscriptions_) new_needs |= run->first;
  if (new_needs==old_needs) return;
  if ((new_needs == 0) && (old_needs != 0))
    blackboard_->L1_AddNonupdatedQuery(this);
  if ((old_needs == 0) && (new_needs != 0))
    blackboard_->L1_DeleteNonupdatedQuery(this);
  search_->L1_ChangeUpdateNeeds(new_needs);
  CL.ChangeValue(&needs_, new_needs);
}

bool Query::L1_Search(int64 *max_work_now){
  CHECK(search_ == NULL);
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
#define NO_PARTITION
#ifndef NO_PARTITION
  uint64 least_matches = num_matches[best_tuple];
  int num_components = GetConnectedComponents(pattern_, NULL);
  if ( (num_components > 1) && (least_matches > 0) ) {
    // Partition node TODO
    new PartitionSearch(this);
    return search_->L1_Search(max_work_now);
    return false;
  }
#endif

  // Condition node
  new ConditionSearch(this, best_tuple);
  return search_->L1_Search(max_work_now);
}
 
NoTuplesSearch::NoTuplesSearch(Query *query) 
  : Search(query) {
}

OneTupleSearch::OneTupleSearch(Query *query) 
  : Search(query), wt_subscription_(NULL) {
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

void OneTupleSearch::Verify() const { 
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
    CHECK(query_->needs_ == 0);
    wt_subscription_ = 
      query_->blackboard_->L1_MakeUpdateWTSubscription<OneTupleSearch>
      (GetWildcardTuple(), new_needs, this);
    return;
  }
  if (new_needs == 0) {
    wt_subscription_->L1_Erase();
    return;
  }
  wt_subscription_->L1_ChangeNeeds(new_needs);
}

// maybe optimize this by doing once per set of needs
void OneTupleSearch::Update(const SingleWTUpdate &update, 
			    const OneTupleWTSub *subscription) {

  // Update the count
  CL.ChangeValue(&count_, count_ + update.GetCountDelta());

  // A subscription could need one of three things
  // It could need a count delta only (no WHICH or TIME)
  // It could need substitutions but not time changes (only WHICH)
  // It could need both substitutions and time changes (WHICH and TIME)
  // So we prepare 3 separate responses
  QueryUpdate out_update;
  out_update.count_delta_ = update.GetCountDelta();
  QueryUpdate out_update_with_subs = out_update;
  QueryUpdate out_update_with_times = out_update;

  // If we need substitutions
  if (query_->needs_ & UPDATE_WHICH) {
    // Get the appropriate substitutions
    Map sub;
    CHECK(ComputeSubstitution(GetVariableTuple().Data(),
			      update.data_.Data(), &sub));
    // Make a SingleQueryUpdate
    SingleQueryUpdate out_single;
    out_single.old_time_ = update.old_time_;
    out_single.new_time_ = update.new_time_;
    out_single.action_   = update.action_;
    out_single.data_ = OMap::Make(sub); 
    
    // Decide where this update goes in the 2 advanced responses
    out_update_with_times.changes_.push_back(out_single);
    if (update.action_ != UPDATE_CHANGE_TIME)
      out_update_with_subs.changes_.push_back(out_single);
  }

  // Run over all the subscriptions of the query and send them updates
  forall(run_needs, query_->subscriptions_) {
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

void PartitionSearch::Verify() const { 
}

bool PartitionSearch::L1_Search(int64 * max_work_now) {
  int num_parts = GetConnectedComponents(query_->pattern_, &partition_);
  vector<Pattern> parts(num_parts);
  for(uint i=0; i<query_->pattern_.size(); i++) {
    parts[partition_[i]].push_back(query_->pattern_[i]);
  }
  count_ = 1;
  for (int i=0; i<num_parts; i++) {
    SamplingInfo sub_sampling = query_->sampling_.LimitToPart(partition_, i);
    Query * q = query_->blackboard_->
      L1_GetExecuteQuery(OPattern::Make(parts[i]),
			 sub_sampling, 
			 max_work_now);
    if (!q) return false;
    q->L1_AddParent();
    children_.push_back(make_pair(q, (PartitionQSub*)NULL) );
    count_ *= q->GetCount();
  }
  return true;
}

void PartitionSearch::GetSubstitutions(vector<Map> * substitutions,
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
void PartitionSearch::Update(const QueryUpdate &update, 
			     const PartitionQSub *subscription,
			     int which) {
  CHECK(!(queued_query_updates_ % which));
  CL.InsertIntoMap(&queued_query_updates_, which, update);
  query_->blackboard_->L1_AddSearchToFlush(this);
}
void PartitionSearch::L1_FlushUpdates() {
  // WORKING TODO
}
void PartitionSearch::L1_ChangeUpdateNeeds(UpdateNeeds new_needs){
  for (uint i=0; i<children_.size(); i++) {
    PartitionQSub * & sub_ref = children_[i].second;
    if (sub_ref == NULL) {
      CHECK(query_->needs_);
      sub_ref = new PartitionQSub(children_[i].first, new_needs, this, i);
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
  : Search(query), condition_tuple_(condition_tuple), wt_subscription_(NULL), 
    queued_wt_update_(NULL) {
}

void ConditionSearch::Verify() const {
}

// If the specifcation matches the variable tuple, adds a child query.
// The return value is true if we havent run out of work_now
bool ConditionSearch::L1_MaybeAddChild(OTuple specification, 
				       int64 *max_work_now, 
				       Query ** child_query) { 
  Map sub;
  bool res = ComputeSubstitution(GetVariableTuple().Data(), 
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
  Query * q = query_->blackboard_->
    L1_GetExecuteQuery(OPattern::Make(sub_pattern), 
		       sub_sampling, 
		       max_work_now);
  if (!q) return false;
  q->L1_AddParent();
  ConditionQSub * new_qsub = NULL;
  if (query_->needs_) {
    new_qsub = new ConditionQSub(q, query_->needs_, this, specification);
  }
  CL.InsertIntoMap(&children_, specification, make_pair(q, new_qsub));
  if (child_query) *child_query = q;
  return true;
}

bool ConditionSearch::L1_Search(int64 * max_work_now) {
  Blackboard & bb = *(query_->blackboard_);
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
    Query * child;
    if (!L1_MaybeAddChild(run_tuples->second->tuple_, max_work_now, &child))
      return false;
    // There's no child if we don't match a repeated variable
    if (child) CL.ChangeValue(&count_, count_ + child->GetCount());
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
      if (!ti) {
	cout << "couldn't find tuple on blackboard" << specific_tuple << endl;
	cout << "query=" << query_->ToString() << endl;
	query_->blackboard_->PrintBlackboard();
      }
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

// Sampled queries are not updated
void ConditionSearch::Update(const SingleWTUpdate &update, 
			     const ConditionWTSub *subscription) {
  CHECK(queued_wt_update_ == NULL);
  CL.ChangeValue(&queued_wt_update_, new SingleWTUpdate(update));
  CL.Creating(queued_wt_update_);
  query_->blackboard_->L1_AddSearchToFlush(this);
}

void ConditionSearch::L1_FlushUpdates() {

  // Three different outputs correspond to not needing which or time,
  // needing which only, or needing both
  QueryUpdate out_update, out_update_with_subs, out_update_with_times;

  int64 count_delta = 0;
  bool need_subs = query_->needs_ & UPDATE_WHICH;
  bool need_times = query_->needs_ & UPDATE_TIME;
  
  // first run over all query updates.
  forall(run_q, queued_query_updates_) {
    query_->blackboard_->flushed_query_update_.insert(query_);
    const QueryUpdate & update = run_q->second;
    count_delta += update.count_delta_;
    if (!need_subs) continue;
    Map additional_sub;
    CHECK(ComputeSubstitution(GetVariableTuple().Data(),
			      run_q->first.Data(), &additional_sub));
    forall(run_c, update.changes_) {
      const SingleQueryUpdate & single = *run_c;
      SingleQueryUpdate out_single = single;
      out_single.data_ = OMap::Make(Union(single.data_.Data(), additional_sub));
      out_update_with_times.changes_.push_back(out_single);
      if (out_single.action_ != UPDATE_CHANGE_TIME)
	out_update_with_subs.changes_.push_back(out_single);
    }
  }
  // Ok now we have processed the updates, delete them
  CL.ChangeValue(&queued_query_updates_, map<OTuple, QueryUpdate>());

  if (queued_wt_update_) { 
    query_->blackboard_->flushed_wt_update_.insert(query_);
    OTuple tuple = queued_wt_update_->data_;
    Map additional_sub;
    if (!ComputeSubstitution(GetVariableTuple().Data(),
			     tuple.Data(), &additional_sub)) 
      goto step2;
    
    // It's a creation
    if (queued_wt_update_->action_ == UPDATE_CREATE) {
      // add a tuple
      Query * child_query;
      L1_MaybeAddChild(tuple, NULL, &child_query);
      if (child_query) {
	count_delta += child_query->GetCount();
	if (need_subs) {
	  vector<Map> subs;
	  vector<Time> times;
	  child_query->GetSubstitutions(&subs, need_times?(&times):NULL);
	  if (need_times) {
	    for (uint i=0; i<times.size(); i++) 
	      times[i] = max(times[i], queued_wt_update_->new_time_);
	  }
	  for (uint i=0; i<subs.size(); i++) {
	    SingleQueryUpdate s 
	      = SingleQueryUpdate::Create(OMap::Make(Union(subs[i], additional_sub)), 
					  need_times?times[i]:Time());
	    out_update_with_subs.changes_.push_back(s);
	    if (need_times) out_update_with_times.changes_.push_back(s);
	  }
	}
      }
      goto step2;
    }

    // It's a deletion or a time change.
    if (children_ % tuple) { // ignore it if it doesn't match
      Query * child_query = children_[tuple].first;
      ConditionQSub *subscription = children_[tuple].second;
      if (queued_wt_update_->action_ == UPDATE_DESTROY) { // it's a tuple deletion
	count_delta -= child_query->GetCount();
	if (need_subs) {
	  vector<Map> subs;
	  vector<Time> times;
	  child_query->GetSubstitutions(&subs, need_times?(&times):NULL);
	  if (need_times) {
	    for (uint i=0; i<times.size(); i++) times[i] 
	      = max(times[i], queued_wt_update_->old_time_);
	  }
	  for (uint i=0; i<subs.size(); i++) {
	    SingleQueryUpdate s 
	      = SingleQueryUpdate::Destroy(OMap::Make(Union(subs[i], additional_sub)),
					   need_times?times[i]:Time());
	    out_update_with_subs.changes_.push_back(s);
	    if (need_times) out_update_with_times.changes_.push_back(s);
	  }
	}
	child_query->L1_RemoveParent();
	subscription->L1_Erase();	
	CL.RemoveFromMap(&children_, tuple);
	goto step2;
      } 
	
      // change time on a tuple
      CHECK(queued_wt_update_->action_ == UPDATE_CHANGE_TIME);
      if (!need_times) goto step2;
      vector<Map> subs;
      vector<Time> times;
      child_query->GetSubstitutions(&subs, &times);
      for (uint i=0; i<subs.size(); i++) {
	Time old_time = max(times[i], queued_wt_update_->old_time_);
	Time new_time = max(times[i], queued_wt_update_->new_time_);
	if (old_time != new_time) {
	  out_update_with_times.changes_.push_back
	    (SingleQueryUpdate::ChangeTime
	     (OMap::Make(Union(subs[i], additional_sub)),
	      old_time, new_time));
	}
      }
    }
  }
  
 step2:
  // Done with the wt update, delete it
  SingleWTUpdate * nullptr = NULL;
  CL.ChangeValue(&queued_wt_update_, nullptr);

  out_update.count_delta_ = 
    out_update_with_subs.count_delta_ = 
    out_update_with_times.count_delta_ = count_delta;

  CL.ChangeValue(&count_, count_ + count_delta);

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
			     const ConditionQSub *subscription,
			     OTuple t) {
  CHECK(!(queued_query_updates_ % t));
  CL.InsertIntoMap(&queued_query_updates_, t, update);
  query_->blackboard_->L1_AddSearchToFlush(this);
}

void ConditionSearch::L1_ChangeUpdateNeeds(UpdateNeeds new_needs){
  forall(run, children_) {
    ConditionQSub * & sub_ref = run->second.second;
    if (sub_ref == NULL) {
      CHECK(query_->needs_ == 0);
      
      CL.ChangeMapValue
	(&children_, run->first, 
	 make_pair(run->second.first, 
		   new ConditionQSub(run->second.first, 
				     new_needs, this, run->first)));
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
    CHECK(query_->needs_ == 0);
    CL.ChangeValue
      (&wt_subscription_, 
       query_->blackboard_->L1_MakeUpdateWTSubscription<ConditionSearch>
       (GetWildcardTuple(), new_needs, this));
    return;
  }
  if (new_needs == 0) {
    wt_subscription_->L1_Erase();
    return;
  }
  wt_subscription_->L1_ChangeNeeds(new_needs);
}
