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

// Declare builtin keyword relations
extern Keyword SUCCESSOR;
extern Keyword SUM_REL;

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

  // For adding builtin relations as keywords
  static void Init();

  Query(Blackboard *blackboard, OPattern pattern, SamplingInfo sampling) 
    :blackboard_(blackboard), needs_(0),
     parent_count_(0), search_(NULL) {
    pair<OPattern, SamplingInfo> simple = SimplifyBuiltins(pattern, sampling);
    pattern_ = simple.first;
    sampling_ = simple.second;
    CL.Creating(this);
    blackboard_->L1_AddNonupdatedQuery(this);
  }

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
  void L1_SendCurrentAsUpdates(QuerySubscription *sub);
  string GetDescription() const { 
    return "Query" + pattern_.ToString();
  }
  string ToString() const {
    return GetDescription();
  } 
  void Verify() const;

  bool FindBuiltinCondition
  (int position,
   int64 * num_substitutions, 
   vector<OMap> * substitutions) const; 

  static Keyword AddBuiltinRelation(string s) {
    Keyword k = Object::AddKeyword(s);
    builtin_relations_.insert(k);
    return k;
  }
  static bool IsBuiltinRelationKeyword(Object relation) {
    if (relation.GetType() != Object::KEYWORD) return false;
    return builtin_relations_ % Keyword(relation);
  }
  static bool IsBuiltinRelationTuple(const Tuple& t) {
    if (t.size() == 0) return false;
    return IsBuiltinRelationKeyword(t[0]);
  }

  // NULL here means the pattern is simplifiable to falseness
  static pair<OPattern, SamplingInfo> 
    SimplifyBuiltins(OPattern p, SamplingInfo sampling);

  // Helper function for simplification
  static bool EvaluateBuiltin(OTuple t);

  // Takes a query which is fully substituted and returns the time
  // at which the last tuple comes true. (builtin relations are ignored,
  // since they occur at creation)
  // Crashes if any of he tuples in the pattern (other than builtin functions)
  // aren't on the blackboard.
  static OTime FindConjunctionTime(const Blackboard &bb, const Pattern &p);

  // defining data:
  OPattern pattern_;
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

  // This part relates to builtin relations
  static set<Keyword> builtin_relations_;
  
};

class ConditionSearch;
class PartitionSearch;
class OneTupleSearch;
class BuiltinSearch;
typedef UpdateSubscriptionWithData<QueryUpdate, Query, ConditionSearch, OTuple> ConditionQSub;
typedef UpdateSubscriptionWithData<QueryUpdate, Query, PartitionSearch, int> PartitionQSub;
typedef UpdateSubscriptionWithData<QueryUpdate, Query, BuiltinSearch, OMap> BuiltinQSub;

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

struct FalseSearch : public Search {
  FalseSearch(Query *query);
  bool L1_Search(int64 *max_work_now) {
    CL.ChangeValue(&count_, (uint64)0);
    return true;
  }
  void GetSubstitutions(vector<Map> * substitutions, 
			vector<Time> *times) const {
    substitutions->clear();
    if (times) times->clear();
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
    return query_->pattern_.Data()[0];
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
    return query_->pattern_.Data()[condition_tuple_];
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

// A given search is given a set of maps to split on, it doesn't need
// to subscribe to anything nor question why these things are given
// It's usually used to split on values of a builtin relation or something
// known because of a builtin relation

struct BuiltinSearch : public Search {
  BuiltinSearch(Query *query, const vector<OMap>& subs);
  bool L1_Search(int64 * max_work_now);

  void GetSubstitutions(vector<Map> * substitutions, 
			vector<Time> *times) const;

  void L1_EraseSubclass();
  void Update(const QueryUpdate &update, const BuiltinQSub *subscription,
	      OMap m);
  void L1_ChangeUpdateNeeds(UpdateNeeds new_needs);
  void L1_FlushUpdates();

  map<OMap, pair<Query*, BuiltinQSub *> > children_;
  map<OMap, QueryUpdate> queued_query_updates_;
  vector<OMap> subs_;
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

struct TimedQuery;
typedef Subscription<QueryUpdate, TimedQuery> TimedQuerySubscription; 

struct TimedQuery {
  // This doesn't accept sampling for now. May want to do it later. 
  // It also might be more efficient to add time limits to the Query object. 
  // but this is simpler for now. 

  typedef UpdateSubscription<QueryUpdate, Query, TimedQuery> SubType;
  
  TimedQuery::TimedQuery(Blackboard *blackboard, 
			 const OPattern &pattern,
			 OTime time_limit);
  OPattern GetPattern() const { return pattern_;}
  OTime GetTimeLimit() const { return time_limit_;}
  void L1_SetTimeLimit(OTime new_time_limit);
  void L1_SetPattern(OPattern new_pattern);
  void L1_SendCurrentAsUpdates(TimedQuerySubscription *sub, bool reverse);  
  void L1_SendUpdate(const QueryUpdate & out_update); // helper
  void Update(const QueryUpdate &update, SubType *sub);  
  void L1_AddParent();
  void L1_RemoveParent();
  void L1_AddedSubscription() { L1_AddParent();}
  void L1_RemovedSubscription() { L1_RemoveParent();}
  void L1_ChangedSubscriptionNeeds() { CHECK(false); }
  void L1_Erase();
  uint64 GetCount() const;
  // caution: nondeterministic in the case of sampling
  void GetSubstitutions(vector<Map> * substitutions, vector<Time> *times) const;
  Time TimeLimitData() const { 
    if (time_limit_ == NULL) return Time();
    return time_limit_.Data();
  }

  OTime time_limit_;
  OPattern pattern_;  
  map<UpdateNeeds, set<TimedQuerySubscription *> > subscriptions_;
  // cached results of the query
  // includes the results regardless of whether they are in time. 
  typedef rankset<pair<Time, OMap> > ResultsType;
  ResultsType results_;
  SubType * query_sub_;
  Blackboard *blackboard_;
  int parent_count_;
};


#endif
