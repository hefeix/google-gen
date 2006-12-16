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
#include "model.h"
#include "tupleindex.h"

#define MOREWORK(x) if (max_work_now) {(*max_work_now)-=(x); \
    if (*max_work_now<0) return false;}


Model * SearchTree::GetModel() {
  CHECK(precondition_);
  return precondition_->GetModel();
}

SearchTree::SearchTree(Pattern pattern,
		       TupleIndex   *tupleindex,
		       Precondition *precondition,
		       SamplingInfo sampling) {
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

// TODO
bool SearchTree::Search(int64 max_work_now, uint64 * work_now) {
}

SearchNode::SearchNode(SearchTree * tree, SeachNode * parent) {
  tree_ = tree;
  parent_ = parent;
  split_tuple = -1;
  num_satisfactions_ = 0;
  work_ = 0;
  GetChangelist()->Creating(this);
  tuple_to_child_ = NULL;
  child_to_tuple_ = NULL;
  partition_ = NULL;
  type_ = BABY;
}

SearchNode::L1_AddSplitChild(Tuple tuple){
  Searchnode * newchild = new SearchNode(tree_, this);
  GetChangelist()->Make
    (new MapInsertChange<Tuple, SearchNode *>(tuple_to_child_, 
					      tuple, newchild));
  GetChangelist()->Make
    (new MapInsertChange<SearchNode *, Tuple>(child_to_tuple_, 
					      newchild, tuple));
}

SearchNode::L1_MakePartition(int64 * max_work_now) {
  CHECK(type_ == BABY);
  vector<int> comp;
  Pattern pattern = GetPattern();
  int num_components = GetConnectedComponents(pattern, &comp);
  vector<SearchNode*> * newvector 
    = new vector<SearchNode *>(num_components);
  GetChangelist()->ChangeValue(&partition_, newvector);
  GetChangelist()->Creating(partition_);
  for (int c=0; c<num_components.size(); c++) {
    (*partition_)[c] = new SearchNode(tree_, this);
  }
  A1_SetType(PARTITION);
}
bool SearchNode::L1_MakeSplit(int split_tuple, int64 * max_work_now){
  CHECK(type_==BABY);
  L1_SetType(SPLIT);
  Pattern pattern = GetPattern();
  SamplingInfo sampling = GetSampling();
  CHECK(pattern.size()==1);
  Tuple tuple = pattern[split_tuple];
  L1_SetSplitTuple(split_tuple);
 
  // create the @#$*ing maps
  GetChangelist()->ChangeValue(&tuple_to_child_, new map<Tuple, SearchNode *>);
  GetChangelist()->Creating(tuple_to_child_);
  GetChangelist()->ChangeValue(&child_to_tuple_, new map<SearchNode *, Tuple>);
  GetChangelist()->Creating(child_to_tuple_);

  uint64 num_matches = 
    GetTupleIndex->Lookup(tuple.VariablesToWildcards(), NULL, sampling);
  MOREWORK(num_matches);
  vector<Tuple> matches;
  GetTupleIndex->Lookup(tuple.VariablesToWildcards(), &matches, sampling);
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
  A1_SetType(NO_TUPLES);
  return true;
}
bool SearchNode::L1_MakeOneTuple(int64* max_work_now) {
  CHECK(type==BABY);
  A1_SetType(ONE_TUPLE);
  Pattern pattern = GetPattern();
  SamplingInfo sampling = GetSampling();
  CHECK(pattern.size()==1);
  Tuple tuple = pattern[0];
  L1_SetSplitTuple(0);
  bool duplicate_vars = pattern[0].HasDuplicateVariables();
  uint64 num_matches 
    = GetTupleIndex()->Lookup(tuple.VariablesToWildcards(), 
			    NULL, sampling);
  L1_SetWork(num_matches);
  if (!duplicate_vars){
    L1_SetNumSatisfactions(num_matches);
    MOREWORK(1);
    L1_SetNumSatisfactions(num_matches);
  } else {
    MOREWORK(num_matches);
    vector<Tuple> matches;
    GetTupleIndex->Lookup(pattern[0].VariablesToWildcards(), 
			  &matches, sampling);
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
SearchNode::L1_MakeBaby(){
}

SearchNode::Search() {
  
}

void SearchNode::A1_SetType(NodeType t){
  GetChangelist()->ChangeValue(&type_, t);
}

void SearchNode::L1_SetSplitTuple(int pos){
  CHECK(GetChildren().size()==0);
  CHECK(type_ == SPLIT);
  Pattern pattern = GetPattern();
  if (LinkedToModel() && split_tuple_ != -1) {
    GetChangelist()->Make
      (new MapOfSetsRemoveChange<Tuple, SearchNode *>
       (&GetModel()->wildcard_tuple_to_search_node_, 
	pattern[split_tuple_], this));
  }
  GetChangelist()->ChangeValue(&split_tuple_, pos));			
  if (LinkedToModel && split_tuple_ != -1) {
    GetChangelist()->Make
      (new MapOfSetsInsertChange<Tuple, SearchNode *>
       (&GetModel()->wildcard_tuple_to_search_node_, 
	pattern[split_tuple_], this));
  }
}
void SearchNode::L1_SetNumSatisfactions(uint64 new_num_satisfactions){
  if (parent_) {
    parent_->L1_SetNumSatisfactions
      (parent_->num_satisfactions_ + 
       new_num_satisfactions - num_satisfactions_);    
  }
  GetChangelist()->ChangeValue(&num_satisfactions_, new_num_satisfactions);
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
  forall(run, children_) {
    run->second->L1_Erase();
  }
  L1_SetWork(0);
  L1_SetNumSatisfactions(0);
  L1_SetSpitTuple(-1); //  so that we remove it from the index
  GetChangelist()->Destroying(this);
}

