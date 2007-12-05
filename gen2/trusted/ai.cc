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

  // Vette the rule, fix the maxwork here
  CandidateRule simple_r;
  bool res = VetteCandidateRule(r, &simple_r, 50000, comments);

  if (res) {
    *ret = simple_r;
    RETURN_TRACK(true);
  } else {
    RETURN_TRACK(false);
  }
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
  set<Object> anchors;
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
  bool result = BB.GetRandomTupleContaining(&expansion_tuple, 
					    object_anchors, true);
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
  CollapseEquivalentVariables();
  CollapseConstantVariables();
  if (VERBOSITY >= 2) {
    VLOG(2) << "Expanded to " << ToString() << endl;
  }

  anchor_sets_tried_[anchors] = 0;
  RETURN_TRACK(true);
}


void PatternBuilder::CollapseEquivalentVariables() {
  // the assignment of a variable is the mapping from substitution to constant.
  // this maps the assigment to the set of variables with that assignment. 
  map<vector<Object>, set<Variable> > assingments_to_variables;
  forall(run_var, subs_[0]){
    Variable var = Variable(run_var->first);
    vector<Object> assignment;
    for(uint i=0; i<subs_.size(); i++) {
      assignment.push_back(subs_[i][var]);
    }
    assingments_to_variables[assignment].insert(var);
  }
  Map pattern_tweak;
  forall(run_a, assingments_to_variables) if (run_a->second.size() > 1) {
    Variable canonical_var = *(run_a->second.begin());
    forall(run_var, run_a->second) {
      Variable var = *run_var;
      if (var == canonical_var) continue;
      for (uint i=0; i<subs_.size(); i++) subs_[i].erase(var);
      pattern_tweak[var] = canonical_var;
    }
  }
  Substitute(pattern_tweak, &pattern_);
}

void PatternBuilder::CollapseConstantVariables() {
  Map pattern_tweak;
  CHECK(subs_.size() > 0);
  Map example = subs_[0];
  forall(run_var, example) {
    Variable var = run_var->first;
    set<Object> values;
    for (uint i=0; i<subs_.size(); i++)
      values.insert(subs_[i][var]);
    if (values.size() == 1) {
      pattern_tweak[var] =  *values.begin();
      for (uint i=0; i<subs_.size(); i++)
	subs_[i].erase(var);
    }
  }
  Substitute(pattern_tweak, &pattern_);
}

bool Optimizer::
FindSampling(const MPattern & p, SamplingInfo * result_sampling, 
	     int64 max_work,
	     vector<Map> * subs, 
	     uint64 * estimated_num_results,
	     uint64 * actual_num_results,
	     set<uint> * bad_clauses, // don't sample these
	     SamplingInfo *hint){
  CHECK(estimated_num_results);
  VLOG(1) << "Finding sampling for " << ToString(p) 
	  << " max_work=" << max_work << endl;

  const int first_denominator = 1024;
  // Sample more aggressively at first, then less agressively
  for (int denominator = first_denominator; denominator>0; denominator >>=1 ) {
    bool all_take_too_long = true;
    
    for (int sample_clause_i=(denominator==first_denominator && hint)?-1:0; 
	 sample_clause_i<((denominator==1)?1:(int)p.size()); 
	 sample_clause_i++) {
      uint sample_clause = sample_clause_i;
      
      VLOG(2) << "Trying sample_clause:" << sample_clause << endl;
      
      SamplingInfo sampling;
      bool sampled = (denominator > 1);
      if (sample_clause_i == -1) {
	sampling = *hint;
      } else if (sampled) {
	sampling = SamplingInfo(sample_clause, 1.0/denominator);
      }
      if (bad_clauses && ((*bad_clauses) % (uint)sampling.position_) 
	  && sampling.sampled_) continue;

      int64 max_work_now = max_work;
      uint64 num_results;

      // Try this sampled query and if it doesnt succeed continue
      bool success = BB.FindSatisfactions
	(MPatternToOPattern(p),
	 sampling,
	 subs,
	 NULL,
	 &num_results,
	 &max_work_now);
      VLOG(2) << "Sample_clause: " 
	      << sample_clause << (success ? " GOOD" : " BAD") << endl;
      if (!success) continue;

      all_take_too_long = false;

      // If we got down to not sampling at all and it works
      if (!sampled) {
	*result_sampling = sampling; 
	if (estimated_num_results)
	  *estimated_num_results = num_results;
	if (actual_num_results)
	  *actual_num_results = num_results;
	VLOG(1) << "Unsampled" << endl;
	RETURN_TRACK(true);
      }

      // If sampled, don't accept too few results
      if (num_results < 5) continue;

      // If we're getting substitutions, check that at least one of the
      // sampled variables has multiple values
      if (subs) {
	CHECK(subs->size() == num_results);
	set<Variable> sampled_tuple_variables 
	  = GetVariables(p[sampling.position_]);
	bool any_multivalued_variables = false;
	forall(run, sampled_tuple_variables){
	  Object compare_to = (*subs)[0][*run];
	  for (uint i=1; i<subs->size(); i++) {
	    if ((*subs)[i][*run] != compare_to) {
	      any_multivalued_variables = true;
	      break;
	    }
	  }
	  if (any_multivalued_variables) break;
	}
	if (!any_multivalued_variables) continue;
      }

      // If so, we're done!!!
      *result_sampling = sampling;
      if (estimated_num_results)
	*estimated_num_results = (uint64)(num_results / sampling.fraction_);
      if (actual_num_results)
	*actual_num_results = num_results;
      VLOG(1) << "Sampling clause " << OTuple::Make(p[sampling.position_])
	      << " d:" << (1/sampling.fraction_) << endl;
      RETURN_TRACK(true);
    }

    // If all searches at a low sampling probability take too long, then
    // this is pretty hopeless, give up
    if (all_take_too_long) {
      VLOG(1) << "Sampling failed at denom:" << denominator << endl;
      RETURN_TRACK(false);
    }
  }

  // Nothing worked
  CHECK(false); // We shouldn't get here
  VLOG(1) << "Sampling failed" << endl;
  RETURN_TRACK(false);
}

bool Optimizer::VetteCandidateRule(CandidateRule r,
				   CandidateRule *simplified_rule,
				   int64 max_work, 
				   string *comments) {
  VLOG(1) << "Raw=" << ToString(r) << endl;
  if (comments) *comments += " Raw=" + ToString(r);
  
  // Make sure existing preconditions limit results
  if ( (GetVariables(r.first).size() != 0) &&
       (Intersection(GetVariables(r.first), 
		     GetVariables(r.second)).size()==0) ) {
    VLOG(1) << "Precondition and result disconnected" << endl;
    RETURN_TRACK(false);
  }

  // Canonicalize 
  r = CanonicalizeRule(r, NULL);
  VLOG(1) << "Canonicalized=" << ToString(r) << endl;

  // look for a good sampling
  SamplingInfo combined_sampling;
  vector<Map> full_subs;
  uint64 estimated_num_firings;
  uint64 actual_num_firings;
  if (!FindSampling(Concat(r), &combined_sampling, 
		    max_work, &full_subs, 
		    &estimated_num_firings, 
		    &actual_num_firings, NULL, NULL)) {
    VLOG(1) << "Couldn't find sampling for rule" << endl;
    RETURN_TRACK(false);
  }
  if (estimated_num_firings > 5 * BB.GetNumTuples()) {
    VLOG(1) << "Too many firings" << endl;
    RETURN_TRACK(false);
  }  
  VLOG(1) << "Set total sampling actual_num_firings:" 
	  << actual_num_firings << endl;

  SamplingInfo precondition_sampling;
  uint64 estimated_num_satisfactions;
  uint64 actual_num_satisfactions;
  SamplingInfo * hint = &combined_sampling;
  if (combined_sampling.sampled_ 
      && combined_sampling.position_ >= (int)r.first.size()) hint = NULL;

  if (!FindSampling(r.first, &precondition_sampling, max_work, NULL,
		    &estimated_num_satisfactions, 
		    &actual_num_satisfactions, NULL, hint)) {
    VLOG(1) << "Couldn't find sampling for precondition" << endl;
    RETURN_TRACK(false);
  }
  if (estimated_num_satisfactions > 2000000) {
    VLOG(1) << "Too many satisfactions" << endl;
    RETURN_TRACK(false);
  }
  VLOG(1) << "Set precondition sampling actual_num_sat:" 
	  << actual_num_satisfactions << endl;

  // Test whether the rule yields a predictive gain
  // Brilliantly, we no longer do this

  // remove boring variables, collapse equal variables, and remove 
  // variable free tuples.
  CandidateRule old_r = r;
  PatternBuilder pb(Concat(r), full_subs);
  pb.CollapseEquivalentVariables();
  pb.CollapseConstantVariables();

  // Pattern builder leaves the old size the same
  r.first = 
    MPattern(pb.pattern_.begin(), pb.pattern_.begin()+r.first.size());
  r.second = 
    MPattern(pb.pattern_.begin()+r.first.size(), pb.pattern_.end());
  r.first = RemoveVariableFreeTuples(r.first);
  r.second = RemoveVariableFreeTuples(r.second);

  // Recanonicalize the new rule if need be
  if (old_r != r) {
    // Estimated num firings may be wrong here 
    r = CanonicalizeRule(r, NULL);
    VLOG(1) << "Collapsed rule " << ToString(r) << endl;
  }

  // Try to remove preconditions that are not very restrictive.
  int since_last_improvement = 0;
  uint remove_clause = 0;
  for (;since_last_improvement < int(r.first.size()); since_last_improvement++){
    if (precondition_sampling.sampled_ &&
	since_last_improvement == int(r.first.size()-1)) {
      remove_clause = precondition_sampling.position_;
    } else {
      remove_clause = (remove_clause + 1) % r.first.size();
      if (precondition_sampling.sampled_ &&
	  (int)remove_clause == precondition_sampling.position_)
	remove_clause = (remove_clause + 1) % r.first.size();
    }

    VLOG(1) << "Considering removing " << remove_clause << " " 
	    << OTuple::Make(r.first[remove_clause]) << endl;
    VLOG(2) << "since_last_improvement=" << since_last_improvement << endl;
    vector<Tuple> simplified_preconditions 
      = RemoveFromVector(r.first, remove_clause);

    // if a variable is in the result and occurs only in this clause the clause is necessary
    if ((Intersection(GetVariables(r.first[remove_clause]), 
		      GetVariables(r.second))
	 - GetVariables(simplified_preconditions)).size()) {
      VLOG(1) << "Test failed - necessary variable" << endl;
      continue;
    }
  
    // Don't disconnect the pattern if it's not already disconnected 
    // TODO: maybe later check the #disconnected components of both are the same
    if (IsConnectedPattern(MPatternToPattern(r.first)) 
	&& !IsConnectedPattern(MPatternToPattern(simplified_preconditions))) {
      VLOG(1) << "Test failed - disconnects preconditions" << endl;
      continue;
    }
    
    if (precondition_sampling.sampled_ && 
	(int)remove_clause == precondition_sampling.position_) {      
      // pick a new sampling clause.
      set<uint> bad_clauses; 
      bad_clauses.insert(remove_clause);
      if (!FindSampling(r.first, &precondition_sampling, max_work, NULL,
			&estimated_num_satisfactions, 
			&actual_num_satisfactions, &bad_clauses, NULL)) 
	break;
      VLOG(1) << "Changed precondition sampling actual_num_satifactions:" 
	      << actual_num_satisfactions << endl;
    }
  
    uint64 simplified_num_satisfactions = 0;
    int64 max_work_now = max_work;
    SamplingInfo simplified_sampling;
    simplified_sampling = precondition_sampling;
    simplified_sampling.RemovePosition(remove_clause);
  
    if (BB.FindSatisfactions
	(MPatternToOPattern(simplified_preconditions),
	 simplified_sampling,
	 NULL, NULL, // Subsitutions & times
	 &simplified_num_satisfactions,
	 &max_work_now)) {
      VLOG(1) << "Test failed ... I can't get no satisfaction" << endl;
      continue;
    }

    VLOG(1) << "test #sat:" << simplified_num_satisfactions 
	    << " <=? #original_sat (*1.1):" << actual_num_satisfactions * 1.1 << endl;
    if (simplified_num_satisfactions  > actual_num_satisfactions * 1.1)
      continue;

    // adjust the samplinginfo object
    VLOG(1) << "Removing clause " << OTuple::Make(r.first[remove_clause]) << endl;
    precondition_sampling = simplified_sampling;
    r.first = RemoveFromVector(r.first, remove_clause);
    actual_num_satisfactions = simplified_num_satisfactions;
    since_last_improvement = -1; // start back at zero.
    VLOG(1) << "removed clause pattern=" << MPatternToOPattern(r.first)
	    << endl;
    VLOG(1) << "sampling=" << simplified_sampling.ToString() << endl;
  }

  r = CanonicalizeRule(r, NULL);
  
  // Maybe we dont want to recheck the rule if it's been checked recently

  CHECK(simplified_rule);
  *simplified_rule = r;
  VLOG(1) << "Candidate=" << ToString(r) << endl;
  RETURN_TRACK(true);
} 






