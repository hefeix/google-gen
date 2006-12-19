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

bool SearchNode::L1_MakePartition(int64 * max_work_now) {
  CHECK(type_ == BABY);
  VLOG(0) << "Partition - YEAHH BABY  YEAH BABY!!!!" << endl;
  vector<int> comp;
  Pattern pattern;
  GetPatternAndSampling(&pattern, NULL);
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
  L1_SetNumSatisfactions(1);
  A1_SetType(PARTITION);
  return true;
}
bool SearchNode::L1_MakeSplit(int split_tuple, int64 * max_work_now){
  CHECK(type_==BABY);
  A1_SetType(SPLIT);
  Pattern pattern;
  SamplingInfo sampling;
  GetPatternAndSampling(&pattern, &sampling);
  CHECK(pattern.size()==1);
  Tuple tuple = pattern[split_tuple];
  L1_SetSplitTuple(split_tuple);
 
  // create the @#$*ing maps
  GetChangelist()->ChangeValue(&tuple_to_child_, new map<Tuple, SearchNode *>);
  GetChangelist()->Creating(tuple_to_child_);
  GetChangelist()->ChangeValue(&child_to_tuple_, new map<SearchNode *, Tuple>);
  GetChangelist()->Creating(child_to_tuple_);

  uint64 num_matches = 
    GetNumWildcardMatches(tuple, sampling);
  MOREWORK(num_matches);
  vector<Tuple> matches;
  GetWildcardMatches(tuple, sampling, &matches);
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
  CHECK(pattern.size()==1);
  Tuple tuple = pattern[0];
  L1_SetSplitTuple(0);
  bool duplicate_vars = pattern[0].HasDuplicateVariables();
  uint64 num_matches = GetNumWildcardMatches(tuple, sampling);
  L1_SetWork(num_matches);
  if (!duplicate_vars){
    L1_SetNumSatisfactions(num_matches);
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
  set<SearchNode*> children_ = GetChildren();  
  forall(run, children_) (*run)->L1_Erase();
  L1_SetWork(0);
  L1_SetNumSatisfactions(0);
  if (type_ == ONE_TUPLE || type_ == SPLIT) {
    L1_SetSplitTuple(-1);
  }
  if (type_ == SPLIT) {
    GetChangelist()->Destroying(tuple_to_child_);
    GetChangelist()->Destroying(child_to_tuple_);
    GetChangelist()->ChangeValue(&tuple_to_child_, 
				 (typeof(tuple_to_child_))NULL);
    GetChangelist()->ChangeValue(&child_to_tuple_, 
				 (typeof(child_to_tuple_))NULL);
  }
  if (type_ == PARTITION) {
    GetChangelist()->Destroying(partition_);
    GetChangelist()->ChangeValue(&partition_, (typeof(partition_))NULL);
  } 
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
  if (pattern.size()==0) return L1_MakeNoTuples();
  if (pattern.size()==1) return L1_MakeOneTuple(max_work_now);
  
  vector<uint64> num_matches;
  for (uint i=0; i<pattern.size(); i++) 
    num_matches.push_back(GetNumWildcardMatches(pattern[i], sampling));
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
    if (parent_->type_ == SPLIT) {
      Tuple variable_tuple 
	= tree_->pattern_[tuple_to_tree_tuple[parent_->split_tuple_]];
      Tuple constant_tuple = (*parent_->child_to_tuple_)[(SearchNode *)child];
      Substitution mini_sub;
      CHECK(ComputeSubstitution(variable_tuple, constant_tuple, &mini_sub));
      complete_sub.Add(mini_sub);
      tuple_to_tree_tuple 
	= RemoveFromVector(tuple_to_tree_tuple, parent_->split_tuple_);
    } else if (parent_->type_ == PARTITION) {
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
      GetPrecondition()->A1_AddToNumSatisfactions
	((int64)new_num_satisfactions - (int64)old_num_satisfactions);
      GetPrecondition()->ComputeSetLnLikelihood();
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
    return;
  }
  CHECK(false);  
}
