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

#ifndef _QUERY_H_
#define _QUERY_H_
#include "util.h"
#include "tuple.h"
#include "blackboard.h"

/*

  Query - A query on a blackboard to find all satisfactions of a pattern
  Search - The implementation of that search - subclasses are
    NoTuplesSearch - Used in the degenerate case where the pattern is empty.
    OneTupleSearch - If the pattern contains one tuple with no repeated 
                     variables
    ConditionSearch - If we search by conditioning on the value of one tuple.
                      this is the catch-all.
    PartitionSearch - If we search by partitioning the pattern into 
                       a set of patterns which share no variables. 
  Query and Search are tightly coupled one to one.  They are separate objects
  so that Search can be subclassed and Search objects can be swapped out to
  change search strategies.   
		       
*/

struct Search;
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

/*
Some notes about updates:

The main trickiness is that when a tuple changes, it can affect a search through mutple different paths.   Yet, we want the query to send one and only one update that reflects that change.   

Adding tuples:
We first add the tuple to all index rows, then send updates.
Deleting tuples: 
We first send the updates, then delete the tuple from all index rows.


NoTuplesSearch gets no updates.
OneTupleSearch gets WTUpdates
 */

class Query;
typedef SingleUpdate<OMap> SingleQueryUpdate;
typedef CombinedUpdate<OMap> QueryUpdate;
typedef Subscription<QueryUpdate, Query> QuerySubscription; 
typedef LoggingSubscription<QueryUpdate, Query> LoggingQuerySubscription; 

struct Query {
  Query(Blackboard *blackboard, const Pattern &pattern, SamplingInfo sampling) 
    :pattern_(pattern), sampling_(sampling), blackboard_(blackboard), needs_(0),
     parent_count_(0), search_(NULL) {
    CL.Creating(this);
    blackboard_->L1_AddNonupdatedQuery(this);
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
  void L1_AddedSubscription() { L1_RecomputeUpdateNeeds(); L1_AddParent();}
  void L1_RemovedSubscription() { L1_RecomputeUpdateNeeds(); L1_RemoveParent();}
  void L1_ChangedSubscriptionNeeds() { L1_RecomputeUpdateNeeds(); }
  void L1_Erase();
  uint64 GetCount() const;
  // caution: nondeterministic in the case of sampling
  void GetSubstitutions(vector<Map> * substitutions, vector<Time> *times) const;
  // called when the update needs have changed (subscriptions have been
  // added, removed, or changed).  Propagates the needs changes to the
  // children.
  void L1_RecomputeUpdateNeeds();
  string GetDescription() const { 
    return "Query" + OPattern::Make(pattern_).ToString();
  }
  string ToString() const {
    return GetDescription();
  } 
  void Verify() const;

};

class ConditionSearch;
class PartitionSearch;
class OneTupleSearch;
typedef UpdateSubscriptionWithData<QueryUpdate, Query, ConditionSearch, OTuple> ConditionQSub;
typedef UpdateSubscriptionWithData<QueryUpdate, Query, PartitionSearch, int> PartitionQSub;

typedef UpdateSubscription<SingleWTUpdate, IndexRow, ConditionSearch> ConditionWTSub;
typedef UpdateSubscription<SingleWTUpdate, IndexRow, OneTupleSearch> OneTupleWTSub;

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
    CL.Destroying(this);
  }
  virtual void L1_EraseSubclass() {};
  virtual bool L1_Search(int64 *max_work_now) = 0;
  virtual void GetSubstitutions(vector<Map> * substitutions, 
				vector<Time> *times) const = 0;
  virtual void L1_ChangeUpdateNeeds(UpdateNeeds new_needs) {};
  virtual void L1_FlushUpdates() { CHECK(false); }
  virtual void Verify() const {}
  uint64 count_;
};

struct NoTuplesSearch : public Search {
  NoTuplesSearch(Query *query);
  bool L1_Search(int64 *max_work_now) { 
    CL.ChangeValue(&count_, (uint64)1);
    return true;
  }
  void GetSubstitutions(vector<Map> * substitutions, 
			vector<Time> *times) const {
    substitutions->clear();
    substitutions->push_back(Map());
    if (times) {
      times->clear();
      times->push_back(Time());
    }
  }
};

struct OneTupleSearch : public Search {
  OneTupleSearch(Query *query);
  bool OneTupleSearch::L1_Search(int64 * max_work_now);
  void GetSubstitutions(vector<Map> * substitutions, 
			vector<Time> *times) const;
  OTuple GetWildcardTuple() const { 
    return OTuple::Make(VariablesToWildcards(GetVariableTuple().Data()));
  }
  OTuple GetVariableTuple() const { 
    return query_->pattern_[0];
  }
  // Receive an update.
  // should be caled L1_Update to keep to convention, but the name is
  // required by the template magic.
  void Update(const SingleWTUpdate &update, const OneTupleWTSub *subscription);
  void L1_ChangeUpdateNeeds(UpdateNeeds new_needs);
  void Verify() const;
  OneTupleWTSub *wt_subscription_;
};

struct ConditionSearch : public Search {
  ConditionSearch(Query *query, int condition_tuple);
  bool L1_MaybeAddChild(OTuple specifcation, int64 * max_work_now, 
			Query ** child_query);
  bool L1_Search(int64 * max_work_now);
  void GetSubstitutions(vector<Map> * substitutions, 
			vector<Time> *times) const;
  void L1_EraseSubclass();
  OTuple GetWildcardTuple() const { 
    return OTuple::Make(VariablesToWildcards(GetVariableTuple().Data()));
  }
  OTuple GetVariableTuple() const { 
    return query_->pattern_[condition_tuple_];
  }
  // Receive an update.
  // should be caled L1_Update to keep to convention, but the name is
  // required by the template magic.
  void Update(const SingleWTUpdate &update, const ConditionWTSub *subscription);
  void Update(const QueryUpdate &update, const ConditionQSub *subscription,
	      OTuple t);
  void L1_ChangeUpdateNeeds(UpdateNeeds new_needs);
  void L1_FlushUpdates();
  void Verify() const;

  map<OTuple, pair<Query*, ConditionQSub *> > children_;
  int condition_tuple_;
  ConditionWTSub *wt_subscription_;

  SingleWTUpdate * queued_wt_update_;
  map<OTuple, QueryUpdate> queued_query_updates_;
};

struct PartitionSearch : public Search {
  PartitionSearch(Query *query);
  bool L1_Search(int64 * max_work_now);
  void GetSubstitutions(vector<Map> * substitutions, 
			vector<Time> *times) const;
  void L1_EraseSubclass();
  // Receive an update.
  // should be caled L1_Update to keep to convention, but the name is
  // required by the template magic.
  void Update(const QueryUpdate &update, const PartitionQSub *subscription,
	      int which);
  void L1_ChangeUpdateNeeds(UpdateNeeds new_needs);
  void L1_FlushUpdates();
  void Verify() const;

  vector<int> partition_; // element i says which part contains tuple i.
  vector<pair<Query *, PartitionQSub *> >children_;

  map<int, QueryUpdate> queued_query_updates_;
};

#endif
