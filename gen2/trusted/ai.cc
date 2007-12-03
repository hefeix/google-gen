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

#include "ai.h"

// UNTRUSTED

bool PatternBuilder::TryInitializeFromTuple() {

  for (int trials = 0; trials < 100; trials++) {

    // Get the first example
    Tuple t;
    if (!BB.GetRandomTuple(&t)) return false;

    VLOG(1) << "First Tuple " << OTuple::Make(t) << endl;

    // Get the second example
    // Set a random set of positions to variables and try again
    Tuple wild_tuple = t;
    for (uint c=0; c<wild_tuple.size(); c++) {
      double varprob = 0.5;
      if (c == 0) varprob = 0.1; // Don't variablize the relation as much
      if (RandomFraction() < varprob) wild_tuple[c] = WILDCARD;
    }
    VLOG(1) << "Variablized Tuple " << OTuple::Make(wild_tuple) << endl;
    
    // Look for a random tuple matching the pattern
    Tuple t2;
    bool result = BB.GetRandomTupleMatching(&t2, wild_tuple);

    // Couldn't find a second tuple
    if ( (!result) || (t == t2) ) continue;

    Tuple onlytuple = WildcardsToVariables(wild_tuple);
    Map s1, s2;
    CHECK(ComputeSubstitution(onlytuple, t, &s1));
    CHECK(ComputeSubstitution(onlytuple, t2, &s2));
    subs_.push_back(s1);
    subs_.push_back(s2);
    pattern_.push_back(onlytuple);
    // CollapseEquivalentVariables();
    // CollapseConstantVariables();
    RETURN_TRACK(true);
  }
  RETURN_TRACK(false);
}

string PatternBuilder::ToString() {
  stringstream ret;
  ret << "Patternbuilder" << endl;
  ret << ::ToString(pattern_) << endl;
  for (uint c=0; c<subs_.size(); c++) {
    ret << ::ToString(subs_[c]) << endl;
  }
  return ret.str();
}

bool Optimizer::
MaybeFindRandomManyExamplesRule(CandidateRule *ret, 
				string *comments) {

  PatternBuilder pb;
  if (!pb.TryInitializeFromTuple()) RETURN_TRACK(false);
  VLOG(0) << "Initialized " << pb.ToString() << endl;
  
  // Try to expand to a certain number of clauses
  uint num_clauses = 1; 
  while (RandomFraction() < 0.7) num_clauses++;  
  if (!pb.TryExpandFully(num_clauses)) RETURN_TRACK(false);

  // Return the candidate rule
  MPattern &p = pb.pattern_;
  Tuple dummy = p[0]; p[0] = p[p.size()-1];  p[p.size()-1] = dummy;
  CandidateRule r = SplitOffLast(p);
  *comments = "ManyExamplesRule";
  *ret = r;
  RETURN_TRACK(true);
}

bool PatternBuilder::TryExpandFully(uint size) {
  for (uint trials = 0; trials < 100 + 100 * size; trials++) {
    if (pattern_.size() < size) TryExpandOnce();
  }
  if (pattern_.size() < size) {
    VLOG(0) << "ExpandFully failed at size:" << size
	    << " Pattern " << ::ToString(pattern_) << endl;
    RETURN_TRACK(false);
  }
  RETURN_TRACK(true);
}

// We pick variables from the pattern, and call those the anchors
// Then we look for a new tuple containing all the objects that those variables map to in an example

bool PatternBuilder::TryExpandOnce() {

  // Pick a number of anchors
  uint num_anchors = 1;
  while (RandomFraction() < 0.5) num_anchors++;

  // Pick that size of subset of anchors
  // An anchor is an object that we're going to look for in an expanding tuple
  set<int> anchors;
  while(1) {
    anchors.clear();
    if (num_anchors > subs_[0].size()) num_anchors = subs_[0].size();
    while (anchors.size() < num_anchors) {
      RandomElement(variter, subs_[0]);
      anchors.insert(variter->first);
    }

    // We seem not to want to use the same set of anchors on multiple
    // trials of tryexpandonce. So if we've seen this set before, take
    // it with a lower probability
    int times_used = anchor_sets_tried_[anchors];
    if (RandomFraction() < (1.0/(1+times_used))) break;
  } 
  // Keep track of this new anchor set
  anchor_sets_tried_[anchors]++;
  
  // Find the objects corresponding to the anchors using the first example
  set<Object> object_anchors;
  forall(run, anchors) {
    object_anchors.insert((subs_[0])[*run]);
  }

  // Get a random tuple with all the anchors
  Tuple expansion_tuple;
  bool result = BB.GetRandomTupleContaining(&expansion_tuple, object_anchors);
  if (!result) RETURN_TRACK(false);

  // TODO, maybe check times here to make sure new tuple is later with a high prob

  // Make sure this tuple isnt a duplicate of one of the tuples in the first example
  MPattern sub_0_pattern = pattern_;
  Substitute(subs_[0], &sub_0_pattern);
  for (uint c=0; c<sub_0_pattern.size(); c++)
    if (sub_0_pattern[c] == expansion_tuple)
      RETURN_TRACK(false);

  // Turn some of the constants into variables based on subs_[0]
  Map reverse_map = Reverse(subs_[0]);
  Substitute(reverse_map, &expansion_tuple); 
  // S 3 4, S 4 5, S b 5
  // S 100 101, S 101 5

  // Run through all generalizations
  Tuple good_generalization;
  bool any_good_generalization = false;
  vector<Tuple> expansion_tuples(subs_.size());
  for (GeneralizationIterator gen(expansion_tuple); !gen.done(); ++gen) {
    bool works = true;
    Tuple generalized = gen.Current();
    for(uint i=0; i<subs_.size(); i++) {
      Tuple specified = generalized;
      Substitute(subs_[i], &specified);
      int num_results = BB.GetNumWildcardMatches(OTuple::Make(specified));
      if (num_results != 1) {
	works = false;
	break;
      }
      vector<OTuple> results;
      BB.GetWildcardMatches(OTuple::Make(specified), &results);
      CHECK(results.size()==1);
      expansion_tuples[i] = results[0].Data();
    }
    if (works) {
      any_good_generalization = true;
      good_generalization = generalized;
      break;
    }
  }
  if (!any_good_generalization) RETURN_TRACK(false);

  for (uint i=0; i<good_generalization.size(); i++) {
    if (good_generalization[i] == WILDCARD) {
      Variable var = FirstUnusedVariable(subs_[0]);
      for (uint sub_num=0; sub_num<subs_.size(); sub_num++){ 
	subs_[sub_num][var] = expansion_tuples[sub_num][i];
      }
      good_generalization[i] = var;
    }
  }
  pattern_.push_back(good_generalization);
  //CollapseEquivalentVariables();
  //CollapseConstantVariables();
  if (VERBOSITY >= 2) {
    VLOG(2) << "Expanded to " << ToString() << endl;
  }

  anchor_sets_tried_[anchors] = 0;
  RETURN_TRACK(true);
}

/*
void Optimizer::PatternBuilder::CollapseEquivalentVariables() {
  // the assignment of a variable is the mapping from substitution to constant.
  // this maps the assigment to the set of variables with that assignment. 
  map<vector<int>, set<int> > assingments_to_variables;
  forall(run_var, subs_[0].sub_){
    int var = run_var->first;
    vector<int> assignment;
    for(uint i=0; i<subs_.size(); i++) {
      assignment.push_back(subs_[i].Lookup(var));
    }
    assingments_to_variables[assignment].insert(var);
  }
  Substitution pattern_tweak;
  forall(run_a, assingments_to_variables) if (run_a->second.size() > 1) {
    int canonical_var = *(run_a->second.begin());
    forall(run_var, run_a->second) {
      int var = *run_var;
      if (var == canonical_var) continue;
      for (uint i=0; i<subs_.size(); i++) subs_[i].sub_.erase(var);
      pattern_tweak.Add(var, canonical_var);
    }
  }
  pattern_tweak.Substitute(&pattern_);
}

void Optimizer::PatternBuilder::CollapseConstantVariables() {
  Substitution pattern_tweak;
  CHECK(subs_.size() > 0);
  Substitution example = subs_[0];
  forall(run_var, example.sub_) {
    int var = run_var->first;
    set<int> values;
    for (uint i=0; i<subs_.size(); i++)
      values.insert(subs_[i].Lookup(var));
    if (values.size() == 1) {
      pattern_tweak.Add(var, *values.begin());
      for (uint i=0; i<subs_.size(); i++)
	subs_[i].sub_.erase(var);
    }
  }
  pattern_tweak.Substitute(&pattern_);
}

*/




