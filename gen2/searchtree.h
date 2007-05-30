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

#ifndef _SEARCHTREE_H_
#define _SEARCHTREE_H_
#include "util.h"
#include "tuple.h"
#include "blackboard.h"

/*

  Query - A query on a blackboard to find all satisfactions of a pattern
  Search - The implementation of that search - subclasses are
    NoTuplesSearch - Used in the degenerate case where the pattern is empty.
    OneTupleSearch - If the pattern contains one tuple
    ConditionSearch - If we search by conditioning on the value of one tuple.
    PartitionSearch - If we search by partitioning the pattern into 
                       a set of patterns which share no variables. 
  Query and Search are tightly coupled one to one.  They are separate objects
  so that Search can be subclassed and Search objects can be swapped out to
  change search strategies.   
		       
*/

struct Search;
struct QuerySubscription;
class Precondition;
class TupleIndex;
class SamplingInfo;
class Model;
class Changelist;

/* 
   The Search and the Query are coupled 1 to 1.  
   Search is a virtual class.  The subclasses represent different search
   strategies.  The searchtree can be swapped out to change strategies.  The 
   Query maintains the connections to the subscriptions, which 
   remain the same as the strategy changes.
 */

struct Query {
  virtual ~Query() {}
  Query(Blackboard *blackboard, const Pattern &pattern, SamplingInfo sampling) 
    :pattern_(pattern), sampling_(sampling), blackboard_(blackboard), needs_(0),
     parent_count_(0), search_(NULL) {
    CL.Creating(this);
    blackboard_->L1_ChangeNumNonupdatedQueries(1);
  }
  // defining data:
  Pattern pattern_;
  SamplingInfo sampling_;
  Blackboard *blackboard_;
  // What kind of updates does this search need to get?  
  // Equal to the union of the needs of the subscriptions.
  UpdateNeeds needs_;  

  // Parents are things that need you to exist, they may be tracked
  // by you, like subscriptions, or untracked, for example a piece of
  // code that wants to examine your internals
  int parent_count_;

  // The external subscriptions to this query
  // Each subscription is included only once, under its complete needs.
  map<UpdateNeeds, set<QuerySubscription *> > subscriptions_;
  Search *search_;

  // Try to complete the search, or fail and set search_ to NULL.
  // We do this by rolling back on failure.
  bool L1_Search(int64 *max_work_now);
  void L1_AddParent();
  void L1_RemoveParent();
  void L1_Erase();
  uint64 GetCount() const;
  // caution: nondeterministic in the case of sampling
  void GetSubstitutions(vector<pair<Map, Time> > * substitutions) const;
  // called when the update needs have changed (subscriptions have been
  // added, removed, or changed).  Propagates the needs changes to the
  // children.
  void L1_RecomputeUpdateNeeds();
};


// The informaiton passed back by a Query about changes to its results
struct QueryUpdate {
  int64 count_delta_;
  vector<pair<OMap, pair<const Time *, const Time *> > > changes_;
  string ToString() const;
};

// Subclass this to subscribe to changes to a Query.
struct QuerySubscription {
  virtual void Update(const QueryUpdate & update) = 0;
  QuerySubscription(Query *subscribee, UpdateNeeds needs);

  void L1_Erase();
  void L1_ChangeNeeds(UpdateNeeds new_needs);
  virtual ~QuerySubscription(){}
  OPattern GetPattern() const;
  UpdateNeeds Needs() const { return needs_;}
  Query *subscribee_;
  private:
  UpdateNeeds needs_;
};
template <class C>
struct UpdateQuerySubscription : public QuerySubscription {
  C *subscriber_;
  UpdateQuerySubscription(C *subscriber, 
			  Query *subscribee, 
			  UpdateNeeds needs) 
    :QuerySubscription(subscribee, needs), subscriber_(subscriber_) {}
  void Update(const QueryUpdate& update) {
    subscriber_->Update(update, this);
  }
};
class ConditionSearch;
class PartitionSearch;
class OneTupleSearch;
typedef UpdateQuerySubscription<ConditionSearch> ConditionQSub;
typedef UpdateQuerySubscription<PartitionSearch> PartitionQSub;

typedef UpdateWTSubscription<ConditionSearch> ConditionWTSub;
typedef UpdateWTSubscription<OneTupleSearch> OneTupleWTSub;

struct LoggingQuerySubscription : public QuerySubscription {
  LoggingQuerySubscription(Query *subscribee, UpdateNeeds needs) 
    :QuerySubscription(subscribee, needs){}
  string ToString() {
    return "LoggingQuerySubscription(" + 
      OPattern::Make(subscribee_->pattern_).ToString() + ")";
  }
  void Update(const QueryUpdate& update) {
    cout << ToString() + " " + update.ToString();
  }
};


struct Search {
  virtual ~Search() {};
  Query * query_;
  Search(Query *query) 
    :query_(query){
    CL.Creating(this);
    CL.ChangeValue(&query_->search_, this);
  }
  void L1_Erase() { 
    L1_EraseSubclass(); 
    CL.ChangeValue(&query_->search_, (Search*)NULL);    
  }
  virtual void L1_EraseSubclass() {};
  virtual bool L1_Search(int64 *max_work_now) = 0;
  virtual void GetSubstitutions(vector<pair<Map, Time > > * substitutions) const = 0;
  virtual void L1_ChangeUpdateNeeds(UpdateNeeds new_needs) {};
  uint64 count_;
};

struct NoTuplesSearch : public Search {
  NoTuplesSearch(Query *query);
  bool L1_Search(int64 *max_work_now) { count_ = 1; return true;}
  void GetSubstitutions(vector<pair<Map, Time> > *substitutions) const {
    substitutions->clear();
    substitutions->push_back(Map());
  }
};

struct OneTupleSearch : public Search {
  OneTupleSearch(Query *query);
  bool OneTupleSearch::L1_Search(int64 * max_work_now);
  void GetSubstitutions(vector<pair<Map, Time> > * substitutions) const;
  OTuple GetWildcardTuple() const { 
    return OTuple::Make(VariablesToWildcards(GetVariableTuple().Data()));
  }
  OTuple GetVariableTuple() const { 
    return query_->pattern_[condition_tuple_];
  }
  // Receive an update.
  // should be caled L1_Update to keep to convention, but the name is
  // required by the template magic.
  void Update(const WTUpdate &update, const OneTupleWTSub *subscription);
  void L1_ChangeUpdateNeeds(UpdateNeeds new_needs);
  OneTupleWTSub *wt_subscription_;
};

struct ConditionSearch : public Search {
  ConditionSearch(Query *query, int condition_tuple);
  bool L1_MaybeAddChild(OTuple specifcation, int64 * max_work_now, 
			Query ** child_query);
  bool L1_Search(int64 * max_work_now);
  void GetSubstitutions(vector<pair<Map, Time> > * substitutions) const;
  void L1_EraseSubclass();
    OTuple GetWildcardTuple() const { 
      return OTuple::Make(VariablesToWildcards(GetVariableTuple().Data()));
  }
  OTuple GetVariableTuple() const { 
    return query_->pattern_[condition_tuple_].Data();
  }
  // Receive an update.
  // should be caled L1_Update to keep to convention, but the name is
  // required by the template magic.
  void Update(const WTUpdate &update, const ConditionWTSub *subscription);
  void Update(const QueryUpdate &update, const ConditionQSub *subscription);
  void L1_ChangeUpdateNeeds(UpdateNeeds new_needs);
  map<OTuple, pair<Query*, ConditionQSub *> > children_;
  int condition_tuple_;
  ConditionWTSub *wt_subscription_;
};

struct PartitionSearch : public Search {
  PartitionSearch(Query *query);
  bool L1_Search(int64 * max_work_now);
  void GetSubstitutions(vector<pair<Map, Time> > * substitutions) const;
  void L1_EraseSubclass();
  // Receive an update.
  // should be caled L1_Update to keep to convention, but the name is
  // required by the template magic.
  void Update(const QueryUpdate &update, const PartitionQSub *subscription);
  void L1_ChangeUpdateNeeds(UpdateNeeds new_needs);

  vector<int> partition_; // element i says which part contains tuple i.
  vector<pair<Query *, PartitionQSub *> >children_;
};



#endif
