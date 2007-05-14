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

// The informaiton passed back by a Query about changes to its results
struct QueryUpdate {
  int64 count_delta_;
  const vector<pair<OMap, pair<const Time *, const Time *> > > changes_;
  string ToString();
};

// Subclass this to subscribe to changes to a Query.
struct QuerySubscription {
  virtual void Update(const QueryUpdate & update);
  QuerySubscription(Query *subscribee, UpdateNeeds needs);

  void L1_Erase();
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
    :subscriber_(subscriber), QuerySubscription(subscribee, needs) {}
  void Update(const QueryUpdate& update) {
    subscriber_->Update(this, update);
  }
};
struct LoggingQuerySubscription : public QuerySubscription {
  UpdateNeeds needs_;
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


/* the new plan:
There will be no searchnode class.  A node will be a searchtree.  Nodes will not own their children, but will contain subscribers to their children.   A searchtree will require at least one subscriber to exist.  This way, searchtrees will be able to share branches.   A searchtree will definitely track its count, and be able to read off its substitutions and times whien necessary.  Changes to these will be availible for subscription.

question: should subscriptions be changable?  Why? Can't we just add a new subscrption and delete the old one?
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

struct Query {
  virtual ~Query() {}
  Query(Blackboard *blackboard, const Pattern &pattern, SamplingInfo sampling) 
    :pattern_(pattern), search_(NULL), blackboard_(blackboard),
     parent_count_(0), sampling_(sampling) {
    CL.Creating(this);
  }
  // defining data:
  Pattern pattern_;
  SamplingInfo sampling_;
  Blackboard *blackboard_;
  UpdateNeeds needs_;  

  // The external subscriptions to this query
  // Each subscription is included only once, under its complete needs.
  map<UpdateNeeds, set<QuerySubscription *> > subscriptions_;
  int parent_count_;
  Search *search_;

  // Try to complete the search, or fail and set search_ to NULL.
  // We do this by rolling back on failure.
  bool L1_Search(int64 *max_work_now);
  void L1_AddParent();
  void L1_RemvoveParent();
  void L1_Erase();
  void GetSubstitutions(vector<Map> * substitutions) const {
    return search_->GetSubstitutions(substitutions);
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
    CL.ChangeValue(&query_->search_, NULL);    
  }
  virtual void L1_EraseSubclass() {};
  virtual bool L1_Search(int64 *max_work_now) = 0;
  virtual void GetSubstitutions(vector<Map> * substitutions) const;
  uint64 count_;
};
struct NoTuplesSearch : public Search() {
  NoTuplesSearch(Query *query);
  bool L1_Search(int64 *max_work_now) { count_ = 1; return true;}
  void GetSubstitutions(vector<Map> *substitutions) const {
    substitutions->clear();
    substitutions->push_back(Map());
  }
};
struct OneTupleSearch : public Search() {
  OneTupleSearch(Query *query);
  WTSubscription *subscription_;
  OTuple tuple_;
};



// A SearchNode represnets a particular Pattern we want to match.  
// A searchnode can take one of two tactics, splitting or partitioning. 
// Split - We pick a tuple in the pattern and find all matching tuples
//   in the TupleIndex.  If there are additional tuples in the pattern,
//   we have one child node for each matching tuple.  Otherwise, we just
//   keep a count.  
// Partition - We partition the tuples into subsets which share no 
//   variables.  We create one child node for each subset.  The set of 
//   satisfactions of the pattern is the cross-product of the satisfactions 
//   of the subsets.
struct SearchNode{
  enum NodeType { BABY=0, NO_TUPLES, ONE_TUPLE, SPLIT, PARTITION };

  SearchNode(Search *tree, SearchNode *parent);
  bool L1_Search(int64 *max_work_now);
  void GetPatternAndSampling(Pattern *pattern, SamplingInfo *sampling) const;
  Model *GetModel() const;
  Changelist *GetChangelist() const;
  Precondition *GetPrecondition() const;
  set<SearchNode *> GetChildren() const;
  TupleIndex * GetTupleIndex() const;
  bool LinkedToModel() const;
  void GetSubstitutions(vector<Substitution> * substitutions) const;
  // for convenience
  uint64 GetNumWildcardMatches(Tuple t, SamplingInfo sampling) const; 
  void GetWildcardMatches(Tuple t, SamplingInfo sampling, 
			  vector<Tuple> *ret) const;
  void BabyCheck() const; // Checks that the node is in a baby state.
  void VerifyNumSatisfactions(uint64 ns) const;


  void A1_SetType(NodeType t);
  
  void L1_Erase(); //  does not unlink you from parent
  // set the work and propagate up the tree.
  void L1_SetWork(uint64 new_work);
  // set the number of satisfactions and propagate up the tree.
  // also call ComputeSetLnLikelihood() on the precondition if it is linked
  // to the model and is the root of the tree.
  void L1_SetNumSatisfactions(uint64 new_num_satisfactions);
  // Call this from externally after (not before) a tuple which 
  // wildcard-matches your the split tuple is added to the tuple_index 
  // updates the search tree.
  // calls search (with no maximum work) if necessary.
  void L1_AddTuple(Tuple new_tuple);
  // Call this externally after (not before) removing a tuple from the 
  // tuple_index.  Updates and prunes the search tree.
  // Caveat: There is a danger of calling this method on a deleted object if
  // the removed tuple matches multiple clauses of the same pattern.
  void L1_RemoveTuple(Tuple tuple);

  // Adds a new child node for a new value of the split tuple.  Doesn't search.
  SearchNode * L1_AddSplitChild(Tuple tuple);
  // Removes (and erases) a child node of a split tuple.
  void L1_RemoveSplitChild(Tuple tuple);
  void L1_RemoveAllSplitChildren();

  // Sets which tuple we split on, and updates the model indices.
  void L1_SetSplitTuple(int pos);
  void L1_MakeBaby();
  bool L1_MakeNoTuples();
  bool L1_MakeOneTuple(int64 * max_work_now);
  bool L1_MakeSplit(int split_tuple, int64 * max_work_now);
  bool L1_MakePartition(int64 * max_work_now);
  
  SearchNode *parent_;
  Search *tree_;
  uint64 work_;
  uint64 num_satisfactions_;
  NodeType type_;
  
  // If it's a split
  int split_tuple_;
  map<Tuple, SearchNode *> * tuple_to_child_; // if a split
  map<SearchNode *, Tuple> * child_to_tuple_; // if a split

  // If it's a partition
  // this vector is aligned with the pattern.
  // the same SeachNode can occur multiple times in the vector. 
  vector<SearchNode *> *partition_; // if a partition
};

// Pass in a precondition if you want the model updated
// and a tupleindex if you don't
struct Search{
  Search(Pattern pattern, TupleIndex *tupleindex,
	     Precondition *precondition, const SamplingInfo & sampling);
  ~Search();
  void L1_Erase();
  bool LinkedToModel() const { return precondition_; }
  Model * GetModel();
  uint64 GetNumSatisfactions() const { return root_->num_satisfactions_;}
  uint64 GetWork() const { return root_->work_;}
  void GetSubstitutions(vector<Substitution> *substitutions) const{
    root_->GetSubstitutions(substitutions);
  }
  Changelist * changelist_;
  TupleIndex * tuple_index_;
  Precondition * precondition_;
  SearchNode * root_;
  Pattern pattern_;
  SamplingInfo sampling_;
  bool L1_Search(int64 *max_work_now);
};


#endif
