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

// Contains optimization routines
#include <cmath>
#include "model.h"
#include "optimization.h"

Optimizer::Optimizer(Model *model){
  model_ = model;
}

/*void Optimizer::OptimizeRound(){
  vector<int> creative_rules;
  set<Rule *> rules = model_->GetAllRules();
  forall(run, rules) {
    Rule * r = *run;
    if (r->Exists() && 
	r->GetRuleType()==CREATIVE_RULE) {
      OptimizationCheckpoint cp(this, true);
      TrySpecifyCreativeRule(r);
    }
  }
  }*/

bool Optimizer::CombineRules(int time_limit, string * comments) {
  time_t end_time = time(NULL) + time_limit;
  while (!MaybeCombineRules(comments)) {
    if (time(NULL) >= end_time) return false;
  }
  return true;
}

bool Optimizer::MaybeCombineRules(string * comments) {
  const map<Pattern, set<SubRuleInfo> >& index = 
    model_->GetSubrulePatternToRule();
  RandomElement(p, index);
  typeof(p) q = p;
  ++p;
  while (p->second.size() < 2) {
    if (p == q) break;
    ++p;
    if (p == index.end()) p = index.begin();
  }
  if (p->second.size() < 2) return false;

  // get rid of the postcondition ones, and pick one out of every rule represented more than once
  vector<SubRuleInfo> filtered;
  double rule_count = 0.0;
  forall(sri, p->second) {
    if (sri->postcondition_) continue;
    SubRuleInfo * last = (filtered.size() ? &filtered[filtered.size()-1]:NULL);
    if (last && (last->rule_ == sri->rule_)) {
      rule_count += 1.0;
      if (RandomFraction() > (rule_count/rule_count+1))
	*last = *sri;
    } else {
      rule_count = 0.0;
      filtered.push_back(*sri);
    }
  }
  
  if (filtered.size() <2) return false;
  
  VLOG(0) << "Picked For Combination " << endl
	  << TupleVectorToString(p->first) << endl;
  for (uint c=0; c<filtered.size(); c++)
    VLOG(0) << filtered[c].ToString();

  // Do the combination
  TryCombineRules(p->first, filtered, comments);
  return true;
}

// WORKING
void Optimizer::TryCombineRules(Pattern lhs, 
				const vector<SubRuleInfo> & info,
				string * comments) {
  
  set<int> pre_variables = GetVariables(lhs);
  set<int> post_variables;

  // TODO: This would be a good place to check that the preconditions are reasonable

  // What postcondition variables do we need?
  for (uint c=0; c<info.size(); c++) {
    const SubRuleInfo & sri = info[c];
    CHECK(sri.rule_->GetRuleType() != NEGATIVE_RULE);
    
    // Figure out which post variables this rule needs
    // from the new rule
    Pattern result = sri.rule_->GetResult();
    sri.sub_.Substitute(&result);
    set<int> int_set =
      Intersection(pre_variables, GetVariables(result));
    post_variables.insert(int_set.begin(), int_set.end());
  }

  // Do we need to check whether this exists? who knows?
  string rel_name = model_->FindName(":CR");
  int rel_id = LEXICON.GetAddID(rel_name);

  Tuple rhs_t;
  rhs_t.terms_.push_back(rel_id);
  set<int>::iterator run = post_variables.end();
  while (true) {
    --run;
    rhs_t.terms_.push_back(*run);
    if (run == post_variables.begin()) break;
  }
  Pattern rhs;
  rhs.push_back(rhs_t);

  // Make the new rule and make all its satisfactions true
  Rule * new_rule = model_->MakeNewRule
    (lhs, EncodedNumber(), SIMPLE_RULE, NULL,
     rhs, EncodedNumber("uuue"), EncodedNumber("ddde"));
  new_rule->AddAllSatisfactionsAsFirings();
  VLOG(0) << "Made the new rule and added all its satisfactions" << endl;
  
  // Delete the old rules and for each one make a replacement rule
  VLOG(0) << "About to delete the old rules" << endl;
  
  set<Tuple> new_rule_lhs_set;
  new_rule_lhs_set.insert(lhs.begin(), lhs.end());
  for (uint c=0; c<info.size(); c++) {
    const SubRuleInfo & sri = info[c];

    // Grab the original's full substitutions
    vector<Substitution> original_substitutions;
    vector<Firing *> firings = sri.rule_->Firings();
    for (uint c2=0; c2<firings.size(); c2++) {
      original_substitutions.push_back(firings[c2]->GetFullSubstitution());
    }

    // The left hand side of the rule removes the common tuples, replacing by 
    // the right hand side of the common rule instead
    Pattern prefilter_instead_lhs = sri.rule_->GetPrecondition()->GetPattern();
    sri.sub_.Substitute(&prefilter_instead_lhs);
    Pattern instead_lhs;
    for (uint c2=0; c2<prefilter_instead_lhs.size(); c2++) {
      if (new_rule_lhs_set % prefilter_instead_lhs[c2]) continue;
      instead_lhs.push_back(prefilter_instead_lhs[c2]);
    }
    instead_lhs.insert(instead_lhs.begin(), rhs.begin(), rhs.end());
    // TODO now canonicalize this new instead_lhs jeez...

    // The result of the rule stays the same (modulo substitution)
    Pattern instead_rhs = sri.rule_->GetResult();
    sri.sub_.Substitute(&instead_rhs);

    // Delete the rule 
    VLOG(0) << "About to delete rule " << sri.rule_->GetID() << endl;
    sri.rule_->Erase();
    VLOG(0) << "Deleted" << endl;

    // Add the modified rule instead    
    // Rule * instead_rule = 
      model_->MakeNewRule
      (instead_lhs, sri.rule_->GetDelay(), sri.rule_->GetRuleType(), NULL, 
       instead_rhs, sri.rule_->GetStrength(), sri.rule_->GetStrength2());

    // Now add firings for this modified rule TODO
    
  }
  

}

bool Optimizer::FindRandomCandidateRule(CandidateRule *ret, Tactic tactic,
					int time_limit, string * comments){
  time_t end_time = time(NULL) + time_limit;
  while (!MaybeFindRandomCandidateRule(ret, tactic, comments)){
    if (time(NULL) >= end_time) return false;
  }
  recently_checked_[*ret] = model_->GetLnLikelihood();
  return true;
}
bool Optimizer::MaybeFindRandomCandidateRule(CandidateRule *ret, 
					     Tactic tactic, string *comments){
  switch (tactic) {
  case NEW_RULE:
    return MaybeFindRandomNewRule(ret, comments);
    break;
  default:
    return MaybeFindRandomVariantRule(ret, tactic, comments);
    break;
  }
  return false;
}
TrueTuple * Optimizer::GetRandomTrueTuple(){
  TrueTuple *ret = model_->FindTrueTuple((model_->GetTupleIndex()->RandomTuple()));
  CHECK(ret);
  return ret;
}

int64 Optimizer::StandardMaxWork(){
 return 5  * (int64)model_->GetNumTrueTuples();
}

bool Optimizer::MaybeFindRandomVariantRule(CandidateRule *ret, Tactic tactic,
					   string *comments){
  int64 max_work = StandardMaxWork();
  TrueTuple * tp = GetRandomTrueTuple();
  CHECK(tp->GetCauses().size());
  Firing * f = *(tp->GetCauses().begin());
  RuleSat * rs = f->GetRuleSat();
  const Rule * r = rs->GetRule();
  CandidateRule cand = make_pair(r->GetPrecondition()->GetPattern(), 
				 r->GetResult());
  Substitution full_sub = f->GetFullSubstitution(); 
  switch (tactic) {
  case SPECIFY_ONE: {
    RandomElement(assignment, full_sub.sub_);
    Substitution little_sub;
    *comments = "Specialization of " + CandidateRuleToString(cand);
    little_sub.Add(assignment->first, assignment->second);
    little_sub.Substitute(&cand);
    return VetteCandidateRule(cand, ret, max_work, comments);
  }
  case GENERALIZE_ONE: {
    int literal = -1;
    // This loop just tries to find a random literal in the CandidateRule
    for (int try_num=0; try_num<10; try_num++) {
      // pick either the precondition or the result
      const vector<Tuple> & v = ((rand()%2)?cand.first:cand.second);
      // but not an empty one.
      if (v.size()==0) continue;
      // pick a random tuple
      const Tuple & s = v[rand()%v.size()];
      // pick a random element
      int l = s[rand() % s.size()];
      if (!IsVariable(l)) {
	literal = l;
	break;
      }
    }
    if (literal == -1) return false; // we may have failed.
    set<int> variables = 
      Union(GetVariables(cand.first), GetVariables(cand.second));
    // Figure out what is the first unused variable.
    int variable = Variable(0);
    forall(run, variables) {
      if (Variable(*run) >= Variable(variable))
	variable = Variable(Variable(*run)+1);
    }
    Substitution little_sub;
    *comments = "Generalization of " + CandidateRuleToString(cand);
    little_sub.Add(literal, variable);
    little_sub.Substitute(&cand);
    return VetteCandidateRule(cand, ret, max_work, comments);    
    break;
  }
  default:
    break;
  }
  CHECK(false);
  return false;
}


bool Optimizer::MaybeFindRandomNewRule(CandidateRule *ret, string *comments){
  int64 max_work = 5  * (uint)model_->GetNumTrueTuples();
  uint num_clauses = 1;
  while (RandomFraction() < 0.7) num_clauses++;  
  vector<Tuple> p;
  Substitution sub;
  int next_var = 0;
  set<Tuple> used_tuples;
  Tuple s1 = model_->GetTupleIndex()->RandomTuple();
  used_tuples.insert(s1);
  p.push_back(s1);
  int tries = 100;
  while (p.size() < num_clauses) {
    tries--; if (tries<0) break;
    set<int> anchors;
    uint num_anchors = 1;
    while (RandomFraction() < 0.5) num_anchors++;
    int tries2 = num_anchors+10;
    while (anchors.size() < num_anchors) {
      tries2--; if (tries2<0) break;
      int clause = rand() % p.size();
      int term_in_clause 
	= (RandomFraction()<0.02)?0:(1+(rand() % (p[clause].size()-1)));
      int w = p[clause][term_in_clause];
      if (w<0) continue;
      anchors.insert(w);
    }
    vector<int> v_anchors(anchors.begin(), anchors.end());
    Tuple s;
    bool found_random =  
      model_->GetTupleIndex()->GetRandomTupleContaining(&s, v_anchors, true);
    if (!found_random) continue;
    if (used_tuples % s) continue;
    used_tuples.insert(s);
    p.push_back(s);
    forall(run, anchors){
      sub.Add(*run, Variable(next_var++));
    }
    sub.Substitute(&p);
  }
  sub.Substitute(&p);
  for (uint i=0; i<p.size(); i++) 
    for (uint j=0; j<p[i].size(); j++)
      if (p[i][j]>=0) {
	double make_var_prob = (j==0)?0.01:0.5;
	if (RandomFraction() < make_var_prob) {
	  sub.Add(p[i][j], Variable(next_var));
	  next_var++;
	  sub.Substitute(&p);
	}
      }
  CandidateRule r = SplitOffLast(p);
  *comments = "NewRule";
  return VetteCandidateRule(r, ret, max_work, comments);
}

void Optimizer::RuleInfo::Canonicalize(){
  r_ = CanonicalizeRule(r_);
}

void Optimizer::RuleInfo::FindCandidateFirings(){
  vector<Substitution> subs;
  bool success = 
    optimizer_->model_->GetTupleIndex()->FindSatisfactions
    (Concat(r_),
     &combined_sampling_, 
     &subs_, &sampled_num_firings_, max_work_/denominator_ + 10, NULL);
  if (!success) {
    hopeless_ = true;
    hopeless_cause_ = 1;
    //needs_bigger_sample_ = true;
    return;
  }
  if (sampled_num_firings_ < 2) {
    needs_bigger_sample_ = true;
    return;
  }
  estimated_firings_ = sampled_num_firings_ * denominator_;
  if (max_work_ >=0 && estimated_firings_ > (uint64)max_work_) {
    hopeless_ = true;
    hopeless_cause_ = 2;
    return;
  }

}
const Tuple & Optimizer::RuleInfo::GetSampledTuple(){
  return 
    (sample_postcondition_?r_.second[sample_clause_]:r_.first[sample_clause_]);
}
void Optimizer::RuleInfo::CheckForMultipleValuesOfSampledTuple(){
  // Here we will require also that there be at least two values of the
  // sampled tuple.  
  if (sampled_) {
    set<int> sampled_tuple_variables 
      = GetVariables(GetSampledTuple());
    bool any_multivalued_variables = false;
    forall(run, sampled_tuple_variables){
      int compare_to = subs_[0].Lookup(*run);
      for (uint i=1; i<subs_.size(); i++) {
	if (subs_[i].Lookup(*run) != compare_to) {
	  any_multivalued_variables = true;
	  break;
	}
      }
      if (any_multivalued_variables) break;
    }
    if (!any_multivalued_variables) {
      needs_bigger_sample_ = true;
      return;
    };
  }
}
void Optimizer::RuleInfo::BailIfRecentlyChecked(){
  if ((optimizer_->recently_checked_ % r_) 
      && (optimizer_->recently_checked_[r_] 
	  >= optimizer_->model_->GetLnLikelihood()-1.0)) {
    hopeless_ = true;
  }
}
void Optimizer::RuleInfo::FindNumSatisfactions(){
  // check that the preconditions aren't too much work to searh for.
  bool success = 
    optimizer_->model_->GetTupleIndex()->FindSatisfactions
    (r_.first, &precondition_sampling_, 0, 
     &sampled_num_satisfactions_, 
     max_work_/denominator_+10, 0);
  if (!success) {
    hopeless_ = true;
    hopeless_cause_ = 1;
    // needs_bigger_sample_ = true;
    return;
  }
  estimated_satisfactions_ 
    = sampled_num_satisfactions_ * (sample_postcondition_?1:denominator_);
  
  if (max_work_>=0 && estimated_satisfactions_ > (uint64)max_work_){
    hopeless_ = true;
    hopeless_cause_ = 2;
    return;
  }
}

void Optimizer::RuleInfo::RemoveUnrestrictivePreconditions(){
  // Try to remove preconditions that are not very restrictive.
  bool any_removed = true;
  while (any_removed) {
    any_removed = false;
    for (uint i=0; i<r_.first.size(); i++) {
      vector<Tuple> simplified_preconditions = RemoveFromVector(r_.first, i);
      SamplingInfo simplified_sampling = precondition_sampling_;
      if (sampled_) {
	if ((int)i < sample_clause_) {
	  simplified_sampling.position_--;	  
	} else if ((int)i == sample_clause_){
	  simplified_sampling = SamplingInfo();
	}
      }
      // if a variable is in the result and occurs only in this clause
      // of the precondition, then this clause is necessary.
      if ((Intersection(GetVariables(r_.first[i]), GetVariables(r_.second))
	   - GetVariables(simplified_preconditions)).size()) continue;
      uint64 simplified_num_satisfactions = 0;
      if (optimizer_->model_->GetTupleIndex()->FindSatisfactions
	  (simplified_preconditions, &simplified_sampling, 0,
	   &simplified_num_satisfactions,
	   max_work_, 0)) {
	if (simplified_num_satisfactions
	    <= sampled_num_satisfactions_ * 1.1){
	  // adjust the samplinginfo object
	  CHECK (!sample_postcondition_);
	  if (sampled_) {
	    if ((int)i < sample_clause_) {
	      sample_clause_--;
	      precondition_sampling_.position_--;
	      combined_sampling_.position_--;
	    } else if ((int)i==sample_clause_){
	      precondition_sampling_ = SamplingInfo();
	      sampled_ = false;
	    }
	  }
	  r_.first = RemoveFromVector(r_.first, i);
	  i--;
	  any_removed = true;
	}
      }
    }
  }
}

void Optimizer::RuleInfo::RemoveBoringVariables(){
  map<int, set<int> > replacements;
  for (uint i=0; i<subs_.size(); i++) {
    forall(run, subs_[i].sub_){
      replacements[run->first].insert(run->second);
    }
  }
  Substitution boring_variables;
  forall(run, replacements) {
    if (run->second.size()==1) {
      boring_variables.Add(run->first, *run->second.begin());
    }
      }
  if (GetVerbosity() >= 2) {
    VLOG(2) << "subs=" << endl;
    for (uint i=0; i<subs_.size(); i++) 
      VLOG(3) << subs_[i].ToString() << endl;
  }
  VLOG(2) << "candidate= " << CandidateRuleToString(r_) << endl;
  VLOG(2) << "boring_variables="
	  << boring_variables.ToString() << endl;
  boring_variables.Substitute(&r_.first);
  boring_variables.Substitute(&r_.second);
  if (GetVariables(r_.second).size()==0 ||
      Intersection(GetVariables(r_.first), 
		   GetVariables(r_.second)).size()==0) {
    hopeless_ = true;
    return;
  }
  r_.first = RemoveVariableFreeTuples(r_.first);
  r_.second = RemoveVariableFreeTuples(r_.second);
}

bool Optimizer::RuleInfo::Vette(){
  hopeless_ = false;
  needs_bigger_sample_ = false;
  if (Intersection(GetVariables(r_.first), 
		   GetVariables(r_.second)).size()==0) {
    hopeless_ = true;
    return false;
  }
  comments_ += " Raw=" + CandidateRuleToString(r_);
  VLOG(0) << "Raw=" << CandidateRuleToString(r_) << endl;
  Canonicalize();
  BailIfRecentlyChecked();
  if (hopeless_) {
    VLOG(0) << "Hopeless checked recently" << endl;
    return false;
  }

  // since things within the loops can change r_, we revert it every time
  // to make sure nothing weird is going on.  
  CandidateRule revert_to_rule = r_;

  // Sample more aggressively at first, then less agressively if that doesn't 
  // work.
  for (denominator_ = 1024; denominator_>0; denominator_ >>=1 ) {
    r_ = revert_to_rule;
    // Try sampling each clause in the precondition to see if one works, 
    // or each clause in the postcondition if the precondition is empty.
    sample_postcondition_ = (r_.first.size()==0);
    const Pattern & sample_pattern 
      = sample_postcondition_?r_.second:r_.first;
    for (uint sample_clause = 0; sample_clause<sample_pattern.size(); 
	 sample_clause++) {
      // (since sample_clause_ can be changed in the loop)
      sample_clause_ = sample_clause; 
      r_ = revert_to_rule;
      needs_bigger_sample_ = false;
      sampled_ = (denominator_ > 1);
      //  see if sampling this clause by this denominator works.  
      precondition_sampling_ = combined_sampling_ = SamplingInfo();
      if (sampled_) {
	combined_sampling_ = 
	  SamplingInfo::RandomRange(sample_clause_, denominator_);
	CHECK(sample_postcondition_ 
	      == ((uint)sample_clause_ >= r_.first.size()));
	if (!sample_postcondition_)
	  precondition_sampling_ = combined_sampling_;
      }
      // first find satisfactions of the whole thing.
      FindCandidateFirings();
      if (needs_bigger_sample_) continue; 
      if (hopeless_) {
	VLOG(0) << "Quit after FindCandidateFirings denom:" << denominator_ 
		<< " cause:" << hopeless_cause_ << endl;
	return false;
      }
      
      // count the number of satisfactions of the preconditions
      FindNumSatisfactions();
      if (needs_bigger_sample_) continue; 
      if (hopeless_) {
	VLOG(0) << "Quit after FindNumSatisfactions denom:" << denominator_
		<< " cause:" << hopeless_cause_ << endl;
	return false;
      }

      // TODO: Make estimates of the complexity savings and possibly fail

      // TODO: Try adding clauses to increase precision.

      // Try to remove preconditions that are not very restrictive.
      RemoveUnrestrictivePreconditions();
      if (needs_bigger_sample_) continue; if (hopeless_) return false;

      RemoveBoringVariables();
      if (needs_bigger_sample_) continue; 
      if (hopeless_) {
	VLOG(0) << "Quit after RemoveBoringVariables denom:" << denominator_
		<< endl;
	return false;
      }

      Canonicalize();
      if (needs_bigger_sample_) continue; if (hopeless_) return false;

      BailIfRecentlyChecked();
      if (needs_bigger_sample_) continue; 
      if (hopeless_) {
	VLOG(0) << "Quit after RecentlyChecked(2) denom:" << denominator_ << endl;
	return false;
      }
      return true;
    }
  }

  hopeless_ = true;
  VLOG(0) << "Quit because no sample worked well" << endl;
  return false;
} 

bool Optimizer::VetteCandidateRule(CandidateRule r, 
				   CandidateRule *simplified_rule,
				   int64 max_work, 
				   string *comments){
  RuleInfo ri(this, r, max_work);
  bool ret = ri.Vette();
  if (simplified_rule) *simplified_rule = ri.r_;
  if (comments) *comments = ri.comments_;
  return ret;
}

ComputationResult Optimizer::DependsOn(Component * dependent, 
				       Component * dependee, int64 max_work){
  int64 total_work = 0;
  CHECK(total_work==0);
  multimap<Time, Component *> to_expand;
  to_expand.insert(make_pair(dependent->GetTime(), dependent));
  while (to_expand.size()) {
    if (max_work != UNLIMITED_WORK && total_work >= max_work) 
      return RESULT_GAVE_UP;
    multimap<Time, Component *>::iterator last = to_expand.end(); last--;
    Component * c = last->second;//to_expand.rbegin()->second;
    to_expand.erase(last);
    // multimap<Time, Component*>::iterator(to_expand.rbegin()));
    vector<vector<Component *> > codependents = c->TemporalCodependents();
    total_work += codependents.size();
    for (uint i=0; i<codependents.size(); i++) if (codependents[i].size()==1) {
      total_work += codependents[i]
.size();
      if (codependents[i][0]==dependee) return RESULT_TRUE;
      if (!(codependents[i][0]->GetTime() < dependee->GetTime())) 
	to_expand.insert(make_pair(codependents[i][0]->GetTime(), 
				   codependents[i][0])); 
    }
  }
  return RESULT_FALSE;
}
void Optimizer::TryRemoveFiring(Firing *f){
  // if (f->IsEssential(10, 0) == RESULT_TRUE) return false;
  Rule * r = f->GetRule();
  f->Erase();
  OptimizeStrength(r);
  if (!r->HasFiring() && !r->IsUniversalRule()) {
    OptimizationCheckpoint cp(this, false);
    r->Erase();
  }
}

// Propagates the time change after we change the delay of a rule
// through the true tuples it causes and the negative rule sats in which
// they participate.
// This is to avoid fixing all of the times in the model.
void Optimizer::PushTimesAfterChangeDelay(Rule *rule){
  // Push the time change up through the consequences of the rule.	
  // RecomputeTimesThroughTrueTuples(rule);
  
  forall(run_rs, rule->GetRuleSats()) {
    RuleSat * rs = run_rs->second;
    // the times of the rule sats are already updated.
    //if (!rs->ComputeSetTime()) continue;
    forall(run_f, rs->GetFirings()) {
      Firing * f = run_f->second;
      if (!f->ComputeSetTime()) continue;
      forall(run_tt, f->GetTrueTuples()){
	TrueTuple * tt = *run_tt;
	if (!tt->ComputeSetTime()) continue;
	forall(run_sat, tt->GetSatisfactions()) {
	  Satisfaction *sat = *run_sat;
	  if (!sat->ComputeSetTime()) continue;
	  forall(run_neg_rs, sat->GetRuleSats()) {
	    RuleSat * neg_rs = *run_neg_rs;
	    if (neg_rs->GetRule()->GetRuleType() == NEGATIVE_RULE) 
	      neg_rs->ComputeSetTime();
	  }
	}
      }
    }
  }  
}

// TODO: this now needs to be updated to del properly with tuples that aren't
// required.

void Optimizer::TryAddFirings
(Rule * rule, const vector<Substitution> & subs, int max_recursion) {
  // alternate explanations to remove, grouped by rule
  map<Rule *, set<Firing *> > to_remove;
  for (uint snum=0; snum<subs.size(); snum++) {
    CHECK(rule->Exists());
    const Substitution & sub = subs[snum];
    Firing * f = rule->AddFiring(sub); // was GetAddFiring, problem?
    //VLOG(1) << "Added firing " << sub.ToString() << endl;
    CHECK(f);
    
    set<TrueTuple *> dependents_explained = f->GetTrueTuples();
    
    forall(run, dependents_explained) {
      TrueTuple * tp = *run;
      forall (run_f, tp->GetCauses()) {
	Firing * other_firing = *run_f;
	CHECK(other_firing);
	if (f==other_firing) continue;
	to_remove[other_firing->GetRule()].insert(other_firing);
      }
    }
  }
  OptimizeStrength(rule);
  VLOG(1) << "Added " << subs.size() << " firings " 
	  << " ln_likelihood_=" << model_->GetLnLikelihood() << endl;

  set<CandidateRule> variants;
  forall (run_r, to_remove) {
    // this cp automatically gives the option of leaving in all of the 
    // firings for this alternate rule (duplciate explanations)
    OptimizationCheckpoint rule_cp(this, false);
    rule_cp.logging_ = true;

    Rule * alt_r = run_r->first;
    if (alt_r == rule) continue;
    const set<Firing *> & firings = run_r->second;
    bool is_creative = alt_r->GetRuleType() == CREATIVE_RULE;
    if (!alt_r->Exists()) continue;
    CHECK(alt_r->Exists());
    bool may_want_to_add_negative_rule = false;
    // let's see how many of the first firings for this rule can be cut
    int num_satisfactions = alt_r->GetPrecondition()->GetNumSatisfactions();
    int num_first_firings = alt_r->NumFirstFirings();
    int new_num_first_firings = num_first_firings;
    map<RuleSat *, vector<Firing *> > by_rule_sat;
    forall(run, firings) {
      by_rule_sat[(*run)->GetRuleSat()].push_back(*run);
    }
    forall(run, by_rule_sat) {
      if (run->first->GetFirings().size() == run->second.size())
	new_num_first_firings--;
    }
    CHECK(new_num_first_firings >= 0);
    if (new_num_first_firings < num_first_firings &&
	new_num_first_firings > 0) {
      may_want_to_add_negative_rule = true;
    }
    VLOG(1) << "alt_r_id=" << alt_r->GetID()
	    << " is_creative=" << (is_creative?"t":"f")
	    << " num_sat=" << num_satisfactions
	    << " num_nff=" << num_first_firings
	    << " new_nff=" << new_num_first_firings
	    << endl;

    forall(run, firings) {
      if ((*run)->Exists())
	(*run)->Erase();
    }
    OptimizeStrength(alt_r);
    VLOG(1) << "Removed " << firings.size() 
	    << " firings for rule " << alt_r->GetID()
	    << " ln_likelihood_=" << model_->GetLnLikelihood() << endl;
        
    
    // Try to remove the alternate rule if it has few firings.
    vector<Firing *> remaining_firings = alt_r->Firings();
    // if the remaining firings are at most half of what we just deleted... 
    // (pretty arbitrary) try removing the rule.
    if ((!alt_r->IsUniversalRule()) && 
	remaining_firings.size() < firings.size() * 0.5) {
      OptimizationCheckpoint delete_rule_cp(this, false);
      delete_rule_cp.logging_ = true;
      // figure out which TrueTuples we need to find alternate explanations for 
      set<TrueTuple*> to_explain;
      for (uint i=0; i<remaining_firings.size(); i++) {
	to_explain.insert(remaining_firings[i]->GetTrueTuples().begin(), 
			  remaining_firings[i]->GetTrueTuples().end());
      }
      Pattern lhs = alt_r->GetPrecondition()->GetPattern();
      Pattern rhs = alt_r->GetResult();

      alt_r->Erase();
      if (GetVerbosity() >= 2) {
	model_->VerifyLayer2();
      }

      forall(run, to_explain) {
	if ((*run)->GetCauses().size() == 0) 
	  Explain(*run, NULL, false);
      }
      VLOG(1) << "Erased Rule " 
	      << " ln_likelihood_=" << model_->GetLnLikelihood() << endl;

      // Try to make a variation on the alternate rule that switches
      // the result with one of the preconditions.  
      variants.insert(make_pair(lhs, rhs));
      if (!alt_r->Exists()) continue;
    }  
    if (may_want_to_add_negative_rule) {
      OptimizationCheckpoint cp_negative_rule(this, false);
      cp_negative_rule.logging_ = true;
      // make sure r has a smaller delay than alt_r
      if (!(rule->GetDelay() < alt_r->GetDelay())) {
	EncodedNumber new_delay = alt_r->GetDelay();
	new_delay.bits_.push_back(false);
	rule->ChangeDelay(new_delay);
	PushTimesAfterChangeDelay(rule);
      }
      TryMakeFunctionalNegativeRule(alt_r);
      VLOG(1) << "Made a negative rule. "
	      << " ln_likelihood_=" << model_->GetLnLikelihood() << endl;      
    }
  }
  VLOG(1) << "removed all alternate explanations " 
	  << " ln_likelihood_=" << model_->GetLnLikelihood() << endl;
  if (max_recursion >0) 
    forall(run, variants) {
      OptimizationCheckpoint cp_variation(this, false);
      TryRuleVariations(run->first, run->second, max_recursion-1);
    }
  VLOG(1) << "Added variant rules " 
	  << " ln_likelihood_=" << model_->GetLnLikelihood() << endl;
}
  
void Optimizer::TryMakeFunctionalNegativeRule(Rule *r){
  // TODO: maybe play with the delay.
  Pattern precondition 
    = Concat(r->GetPrecondition()->GetPattern(), r->GetResult());
  Rule * negative_rule = model_->FindNegativeRule(precondition, r);
  if (!negative_rule) {
    negative_rule = 
      model_->MakeNewRule(precondition, 
			  EncodedNumber(),
			  NEGATIVE_RULE, r,
			  vector<Tuple>(),
			  EncodedNumber(),
			  EncodedNumber());
    VLOG(1) << " made new negative rule " << endl;
  } else {
    VLOG(1) << " used existing negative rule " << endl;
  }
  VLOG(1) << "Negative rule:" << negative_rule->GetID() << endl;
  VLOG(1) << "Positive rule:" << r->GetID() << endl;
  // negative_rule->ExplainEncoding();
  //model_->FixTimes(); // TODO: do we need this?
  VLOG(1) 
    << "added negative " << " lnlikelihood=" << model_->GetLnLikelihood() << endl;
  // go back and forth and optimize the rule weights of the negative rule 
  // and the inhibited rule.
  // TODO: optimize it all together?
  for (int rep=0; rep<2; rep++) { 
    OptimizeStrength(negative_rule);
    VLOG(1) << "opt_neg " << " lnlikelihood=" << model_->GetLnLikelihood() 
	    << "  val=" << negative_rule->GetStrengthD() << endl;
    OptimizeStrength(r);
    VLOG(1) << "opt_alt " << " lnlikelihood=" << model_->GetLnLikelihood() 
	    << "  val=" << r->GetStrengthD() << endl;
  }
  if (GetVerbosity() >= 1) model_->ToHTML("html");
}

void Optimizer::TryRuleVariations(const Pattern & preconditions, 
				  const Pattern & result, 
				  int max_recursion){
  if (result.size() > 1) return;
  for (uint i=0; i<preconditions.size(); i++) {
    Pattern lhs = preconditions;
    Pattern rhs = result;
    lhs[i] = result[0];
    rhs[0] = preconditions[i];
    CandidateRule cr = CanonicalizeRule(make_pair(lhs, rhs));
    OptimizationCheckpoint cp(this, false);
    string comments = "recursively added as a variation of "
      + CandidateRuleToString(make_pair(preconditions, result));
    TryAddPositiveRule(cr.first, cr.second, max_recursion-1,
		       comments);
    if (cp.KeepChanges()) break;
  }
}

void Optimizer::TryAddPositiveRule(
			   const Pattern & preconditions,
			   const Pattern & result,
			   int max_recursion,
			   string comments){

  if (GetVerbosity() >= 2) {
    model_->VerifyLayer2();
  }

  vector<Substitution> subs;
  vector<Tuple> combined = preconditions;
  combined.insert(combined.end(), result.begin(), result.end());
  model_->GetTupleIndex()->FindSatisfactions(combined, NULL, &subs, 0, UNLIMITED_WORK, 0);
  set<int> precondition_vars = GetVariables(preconditions);
  set<int> result_vars = GetVariables(result);
  result_vars = result_vars-precondition_vars;
  RuleType type = result_vars.size()?CREATIVE_RULE:SIMPLE_RULE;
  VLOG(0) 
    << string(10-max_recursion, ' ')
    << "Contemplating creating rule " 
    << TupleVectorToString(preconditions)
    << " ->" << TupleVectorToString(result) << endl;
  if (model_->FindPositiveRule(preconditions, result)) {
    VLOG(1) << "rule already exists" << endl;
    return;
  }
  VLOG(1) << "before adding rule ll=" 
	  << model_->GetLnLikelihood() << endl;
  double added_arbitrary_term_ll = -model_->GetArbitraryTermLnLikelihood();
  Rule * r = model_->MakeNewRule(preconditions, EncodedNumber(), 
			    type, 0, result, EncodedNumber(), EncodedNumber());
  r->AddComments(comments);
  added_arbitrary_term_ll += model_->GetArbitraryTermLnLikelihood();
  VLOG(1) << "Rule encoding costs: "
	  << r->GetPrecondition()->GetDirectPatternEncodingLnLikelihood()
	  << " + " 
	  << r->GetDirectPatternEncodingLnLikelihood() 
	  << " + " 
	  << added_arbitrary_term_ll
	  << endl;
  // r->ExplainEncoding();
  VLOG(1) << "after adding rule ll=" 
	  << model_->GetLnLikelihood() << endl;
  TryAddFirings(r, subs, max_recursion-1);
  if (!r->Exists()) return;
  VLOG(1) << "after TryAddFirings: ll=" << model_->GetLnLikelihood() << endl;
  OptimizeStrength(r);
  VLOG(1) << "after OptimizeStrength: ll=" << model_->GetLnLikelihood() << endl;
  // ToHTML("html");
}

/*
void Model::TrySpecifyCreativeRule(Rule *r){
  CHECK(r->type_ == CREATIVE_RULE);
  map<pair<int, int>, vector<Firing *> >m; // maps variable,value->firing
  forall(rs_iter, r->GetRuleSats()) {
    forall(f_iter, rs_iter->second->GetFirings()) {
      Firing * f = f_iter->second;
      forall(s_iter, f->right_substitution_.sub_){
	m[*s_iter].push_back(f);
      }
    }
  }
  forall(run, m){
    int var = run->first.first;
    int value = run->first.second;
    const vector<Firing *> & firings = run->second;
    if (firings.size() >= 3) {
      OptimizationCheckpoint cp2(this, false);
      vector<Tuple> new_result = r->GetResult();
      Substitution additional_sub;
      additional_sub.Add(var, value);
      additional_sub.Substitute(&new_result);
      Rule * nr = MakeNewRule(r->GetPrecondition()->GetPattern(),
			      r->GetDelay(), CREATIVE_RULE, 0, 
			      new_result, r->GetStrength(), r->GetStrength2());
      //nr->ExplainEncoding();
      forall(run_f, firings){
	Firing * f = *run_f;
	if (!f->Exists()) continue;
	Substitution sub = f->GetFullSubstitution();
	sub.sub_.erase(var);
	nr->AddFiring(sub);
	f->erase();
      }
      nr->OptimizeStrength();
      r->OptimizeStrength();
      if (!r->HasFiring()) {
	r->Erase();
	break;
      }
    }
  }  
}
*/
ComputationResult RequiresCodependent(Component *dependent, 
				      Component *codependent){
  ComputationResult ret = RESULT_FALSE;
  vector<vector<Component *> > codep = dependent->TemporalCodependents();
  forall(run_first, codep) 
    forall(run_second, *run_first) {
    if (*run_second==codependent) {
      if (run_first->size()==1) return RESULT_TRUE;
      else {
	forall(run_third, (*run_first)) if (run_third != run_second) {
	  if ((*run_third)->GetTime() < (*run_second)->GetTime() ||
	      ((*run_third)->GetTime() == (*run_second)->GetTime() 
	       && run_third<run_second)) 
	    continue;
	  if (ret==RESULT_FALSE) ret=RESULT_MAYBE;
	}
      }
    }
  }
  return ret;
}

ComputationResult IsEssential(Component *c, int max_work, 
			      int * actual_work){
  if (c->Type() == TRUETUPLE && ((TrueTuple*)c)->IsRequired())
    return RESULT_TRUE;
  int so_far = 1;
  if (actual_work) *actual_work = so_far;
  if (so_far > max_work) return RESULT_GAVE_UP;

  vector<Component *> dependents = c->TemporalDependents();
  bool cant_determine = false;
  forall(run, dependents){
    Component * d = *run;
    int additional_work = 0;
    ComputationResult required = RequiresCodependent(d, c);
    if (required == RESULT_FALSE) continue;
    ComputationResult res = IsEssential(*run, max_work-so_far, 
					&additional_work);
    so_far += additional_work;
    if (actual_work) *actual_work = so_far;
    if (so_far > max_work) return RESULT_GAVE_UP;

    if (res == RESULT_TRUE) {
      if (required == RESULT_MAYBE) cant_determine = true;
      else return RESULT_TRUE;
    } else if (res == RESULT_MAYBE) {
      cant_determine = true;
    } else if (res == RESULT_GAVE_UP) {
      return RESULT_GAVE_UP;
    } else {
      CHECK(res == RESULT_FALSE);      
    }
  }
  if (cant_determine) return RESULT_MAYBE;
  return RESULT_FALSE;
}


OptimizationCheckpoint::OptimizationCheckpoint(Optimizer * optimizer,
					       bool fix_times) {
  optimizer_ = optimizer;
  model_  = optimizer_->model_;
  fix_times_ = fix_times;
  logging_ = false;
  cp_ = model_->GetChangelist()->GetCheckpoint();
  old_ln_likelihood_ = model_->GetLnLikelihood();
}
OptimizationCheckpoint::~OptimizationCheckpoint() {
  if (!KeepChanges()) {
    model_->GetChangelist()->Rollback(cp_);
    if (logging_)
      VLOG(1) << "reverting ln_likelihood_="
	   << model_->GetLnLikelihood() << endl;
  }
}
bool OptimizationCheckpoint::Better() {
  return (model_->MayBeTimeFixable()
	  && (model_->GetLnLikelihood() > old_ln_likelihood_ + 
	      (1+fabs(model_->GetLnLikelihood())) * 1e-14));
}
bool OptimizationCheckpoint::KeepChanges() {
  if (!Better()) return false;
  if (fix_times_) optimizer_->FixTimesFixCircularDependencies();
  return Better();
}

double OptimizationCheckpoint::Gain() {
  return model_->GetLnLikelihood() - old_ln_likelihood_;
}

void Optimizer::OptimizeStrength(Rule *r){
  // TODO, make this better and more principled
  EncodedNumber strength = r->GetStrength();
  EncodedNumber strength2 = r->GetStrength2();
  for (int which=0; which<2; which++) {
    EncodedNumber * to_alter = &strength;
    if (which==1) {
      if (r->GetRuleType() != CREATIVE_RULE) continue;
      to_alter = &strength2;
    }
    bool any_improvement = true;
    while(any_improvement) {
      any_improvement = false;
      int avoid_trying = -1;
      for (int trial=0; trial<3; trial++) {
	if (avoid_trying == trial) continue;
	EncodedNumber old_val = *to_alter;
	VLOG(2) << "trial=" << trial 
		<< " to_alter=" << to_alter->ToSortableString()
		<< " ln_likelihood_=" << model_->GetLnLikelihood() << endl;
	if (trial==0) { // drop last bit
	  if (to_alter->bits_.size()==0) continue;
	  to_alter->bits_.pop_back();	  
	} else {
	  to_alter->bits_.push_back(trial==2);
	}
	OptimizationCheckpoint cp(this, false);
	r->ChangeStrength(strength, strength2);
	VLOG(2) << "   new_val=" << to_alter->ToSortableString()
		<< " new_ln_likelihood=" << model_->GetLnLikelihood() << endl;
	if (cp.KeepChanges()) {
	  any_improvement = true;
	  if (trial==0) avoid_trying = old_val.bits_.back()?2:1;
	  else avoid_trying = 0;
	  break;
	} else {
	  *to_alter = old_val;
	}
      }
    }
  }
}

void Optimizer::Explain(TrueTuple *p, 
	     const set<Component *> *excluded, bool fix_times) {

  // make sure that the TrueTuple has no causes that are not excluded.
  forall(run, p->GetCauses()) { 
    CHECK(excluded && (*excluded % (Component *)(*run)));
  }
  vector<pair<Rule *, Substitution> > explanations;
  Tuple s = p->GetTuple();
  model_->FindExplanationsForResult(s, &explanations, excluded, UNLIMITED_WORK);
  if (explanations.size()==0) {
    Rule * r = model_->GetAddUniversalRule(s.size());
    Substitution right_sub;
    for (int i=0; i<(int)s.size(); i++) {
      right_sub.Add(Variable(i), s[i]);
    }
    explanations.push_back(make_pair(r, right_sub));
  }
  int which = 0; double best = 0;
  for (uint i=0; i<explanations.size(); i++){
    Checkpoint cp = model_->GetChangelist()->GetCheckpoint();
    explanations[i].first->AddFiring(explanations[i].second);
    OptimizeStrength(explanations[i].first);
    if (i==0 || model_->GetLnLikelihood() > best) {
      which=i;
      best = model_->GetLnLikelihood();
    }
    model_->GetChangelist()->Rollback(cp);
  }
  explanations[which].first->AddFiring(explanations[which].second);
  OptimizeStrength(explanations[which].first);
}
void Optimizer::FixTimesFixCircularDependencies() {
  // TODO: make this smarter.  much smarter
  VLOG(1) << " start ln_likelihood_=" << model_->GetLnLikelihood() << endl;
  while (model_->GetTimesDirty().size() || model_->GetRequiredNeverHappen().size()) {
    model_->FixTimes();
    if (model_->GetRequiredNeverHappen().size()) {
      VLOG(1) << "  explaining a required proposition" << endl;
      TrueTuple * p 
	= (TrueTuple *)(*(model_->GetRequiredNeverHappen().begin()));
      Explain(p, &model_->GetNeverHappen(), false);
      VLOG(1) << "   explained " << p->GetID() << endl;
      if (GetVerbosity() >= 1) model_->ToHTML("html");
    }
  }
  model_->DeleteNeverHappeningComponents();
  CHECK(model_->GetRequiredNeverHappen().size() == 0);
  VLOG(1) << " end ln_likelihood_=" << model_->GetLnLikelihood() << endl;
  //if (absent_required_.size()){
  //  ToHTML("html");
  //  CHECK(absent_required_.size()==0);
  // }
}

/*void ExplainEncoding(Rule *r){
  forall(run, r->encoding_) 
    Explain(r->model_, *run, &r->model_->never_happen_, false);
    }*/


