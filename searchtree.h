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

struct SearchTree;


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
  Search(int64 max_work_now, uint64 * work_now);
  SamplingInfo GetSampling() const;
  void GetPatternAndSampling(Pattern *pattern, SamplingInfo *sampling) const;
  Model *GetModel() const;
  Changelist *GetChangelist() const;
  Precondition *GetPrecondition() const;
  set<SearchNode *> GetChildren() const;
  uint64 ComputeWork() const;
  uint64 ComputeNumSatisfactions() const;

  
  void L1_Erase(); //  does not unlink you from parent
  // set the work and propagate up the tree.
  void L1_SetWork(uint64 new_work);
  // set the number of satisfactions and propagate up the tree.
  void L1_SetNumSatisfactions(uint64 new_num_satisfactions);
  // Call this from externally after (not before) a tuple which 
  // wildcard-matches your the split tuple is added to the tuple_index 
  // upadtes the search tree.
  // calls search (with no maximum work) if necessary.
  void L1_AddTuple(Tuple new_tuple);

  
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
  vector<SearchNode *> * partition_; // if a partition
};

// Pass in a precondition if you want the model updated
// and a tupleindex if you don't
struct SearchTree{
  SearchTree(Pattern pattern, Precondition *precondition);
  ~SearchTree();
  void L1_Erase();
  bool LinkedToModel() { return precondition_; }
  Model * GetModel();
  Changelist * changelist_;
  TupleIndex * tuple_index_;
  Precondition * precondition_;
  SearchNode * root_;
  Pattern pattern_;
  SamplingInfo sampling_;
  bool Search(int64 max_work_now, uint64 *work_now);
};


#endif
