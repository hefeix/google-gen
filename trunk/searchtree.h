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
#include "tupleindex.h"

struct SearchTree;
class Precondition;
class TupleIndex;
class SamplingInfo;
class Model;
class Changelist;


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

  SearchNode(SearchTree *tree, SearchNode *parent);
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
  SearchTree *tree_;
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
struct SearchTree{
  SearchTree(Pattern pattern, TupleIndex *tupleindex,
	     Precondition *precondition, const SamplingInfo & sampling);
  ~SearchTree();
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
