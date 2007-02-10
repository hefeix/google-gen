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

void Optimizer::TryCombineRules(Pattern lhs, 
				const vector<SubRuleInfo> & info,
				string * comments) {
  
  set<int> pre_variables = GetVariables(lhs);
  set<int> post_variables;

  // TODO: This would be a good place to check that the preconditions are reasonable

  // What postcondition variables do we need?
  // Also what is the minimum delay of the rules
  EncodedNumber minimum_delay = info[0].rule_->GetDelay();

  for (uint c=0; c<info.size(); c++) {
    const SubRuleInfo & sri = info[c];
    CHECK(sri.rule_->GetRuleType() != NEGATIVE_RULE);
    
    // Figure out which post variables this rule needs
    // Figure out which tuples will be removed by factoring
    set<Tuple> remove_set;
    Pattern new_lhs = lhs;
    sri.sub_.Reverse().Substitute(&new_lhs);    
    remove_set.insert(new_lhs.begin(), new_lhs.end());

    // Get the preconditions and result and filter them
    Pattern filtered;
    Pattern old_in_rule = Concat(sri.rule_->GetResult(), sri.rule_->GetPrecondition()->GetPattern());
    for (uint c2=0; c2< old_in_rule.size(); c2++)
      if (!(remove_set % old_in_rule[c2])) filtered.push_back(old_in_rule[c2]);

    // Check with post filter variables are in the subsitution
    set<int> rule_post_variables = GetVariables(filtered);
    forall(run, rule_post_variables) {
      const int * new_val = sri.sub_.sub_ % (*run);
      if (new_val)  {
	post_variables.insert(*new_val);
      }
    }

    // Track the minimum delay
    if (sri.rule_->GetDelay() < minimum_delay)
      minimum_delay = sri.rule_->GetDelay();
  }
  
  // Log the post variables
  forall (run, post_variables) {
    VLOG(0) << "RHS variable " << LEXICON.GetString(*run) << endl;
  }


  // Do we need to check whether this rule exists? who knows?

  string rel_name = model_->FindName("");
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
    (lhs, minimum_delay, SIMPLE_RULE, NULL,
     rhs, EncodedNumber("e"), EncodedNumber("e"));
  new_rule->AddAllSatisfactionsAsFirings();
  OptimizeStrength(new_rule);
  VLOG(0) << "Made the new rule and added all its satisfactions" << endl;
  
  // Delete the old rules and for each one make a replacement rule
  VLOG(0) << "About to delete the old rules" << endl;
  
  for (uint c=0; c<info.size(); c++) {
    const SubRuleInfo & sri = info[c];

    VLOG(0) << "Working on rule " << sri.rule_->GetID() << endl;

    // Find the set of tuples in this rule's LHS that must be removed
    set<Tuple> remove_set;
    Pattern new_lhs = lhs;
    sri.sub_.Reverse().Substitute(&new_lhs);    
    remove_set.insert(new_lhs.begin(), new_lhs.end());
    if (GetVerbosity() >= 0) {
      forall(run, remove_set)
	VLOG(0) << "Must remove " << run->ToString() << endl;
    }

    // The left hand side of the rule removes the common tuples, replacing by 
    // the right hand side of the common rule instead
    Pattern old_lhs = sri.rule_->GetPrecondition()->GetPattern();
    Pattern instead_lhs;
    for (uint c2=0; c2< old_lhs.size(); c2++) {
      if (remove_set % old_lhs[c2]) continue;
      instead_lhs.push_back(old_lhs[c2]);
    }
    Pattern extra_instead_lhs = rhs;
    sri.sub_.Reverse().Substitute(&extra_instead_lhs);
    instead_lhs.insert(instead_lhs.begin(), 
		       extra_instead_lhs.begin(),
		       extra_instead_lhs.end());
    VLOG(0) << "New (uncanonicalized) LHS " 
	    << TupleVectorToString(instead_lhs) << endl;

    // The result of the rule stays the same entirely
    Pattern instead_rhs = sri.rule_->GetResult();

    // Ok one last step, canonicalize the new rule
    Substitution c_sub;
    CandidateRule instead = CanonicalizeRule
      (make_pair(instead_lhs,instead_rhs), &c_sub);
    VLOG(0) << "Canonicalization substitution " 
	    << c_sub.ToString() << endl;

    // Find the new substitutions
    vector<Substitution> new_substitutions;
    vector<Firing *> firings = sri.rule_->Firings();
    for (uint c2=0; c2<firings.size(); c2++) {
      Substitution original = firings[c2]->GetFullSubstitution();
      Substitution new_sub;
      forall(run, original.sub_) {
	int * new_var = c_sub.sub_ % run->first;
	if (new_var) {
	  new_sub.sub_[(*new_var)] = run->second;
	}
      }
      new_substitutions.push_back(new_sub);
    }

    // Erase the old rule 
    sri.rule_->Erase();
    VLOG(0) << "Erased" << endl;
    
    // Add the modified rule instead    
    Rule * instead_rule = 
      model_->MakeNewRule
      (instead.first, sri.rule_->GetDelay(), sri.rule_->GetRuleType(), NULL, 
       instead.second, sri.rule_->GetStrength(), sri.rule_->GetStrength2());
    
    // Now add firings for this modified rule
    for (uint c2=0; c2<new_substitutions.size(); c2++) {
      instead_rule->AddFiring(new_substitutions[c2]);
    }
    OptimizeStrength(instead_rule);
  }
  
  // TADA

}

bool Optimizer::FindRandomCandidateRule(CandidateRule *ret, Tactic tactic,
					int time_limit, string * comments){
  time_t end_time = time(NULL) + time_limit;
  while (!MaybeFindRandomCandidateRule(ret, tactic, comments)){
    if (time(NULL) >= end_time) return false;
  }
  recently_checked_[*ret] = model_->GetUtility();
  return true;
}

bool Optimizer::MaybeFindRandomCandidateRule(CandidateRule *ret, 
					     Tactic tactic, string *comments){
  switch (tactic) {
  case NEW_RULE:
    return MaybeFindRandomNewRule(ret, comments);
    break;
  case NEW_MANY_EXAMPLES_RULE:
    return MaybeFindManyExamplesRule(ret, comments);
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

// This assumes the truetuple currently happens at some time
double Optimizer::GuessBenefit(const TrueTuple * tp) {

  // Get the first cause
  Firing * first_cause = tp->GetFirstCause();
  CHECK(first_cause);

  // Get the firing cost
  Rule * rule = first_cause->GetRule();
  RuleSat * rule_sat = first_cause->GetRuleSat();
  double strength = rule->GetStrengthD();
  if (rule_sat->NumFirings() > 1) strength = rule->GetStrength2D();
  double firing_cost = - log (strength);
  VLOG(2) << "Firing cost " << firing_cost << endl;

  // Get the naming cost
  double naming_cost = 0;
  {
    DestructibleCheckpoint checkp(model_->GetChangelist());
    double old_utility = model_->GetUtility();
    Substitution sub = first_cause->GetRightSubstitution();
    forall(run, sub.sub_) {
      int term = run->second;
      int var = run->first;
      Chooser * ch = (*rule->GetChoosers())[var];
      CHECK(ch);
      // We can get rid of naming but not if parent count is 1
      // Revisit this when you have transient objects
      Chooser * original_parent = ch->parent_;
      if (ch->parent_->GetCount(term) == 1)
	ch->parent_ = NULL;
      ch->L1_ChangeObjectCount(term, -1);
      ch->parent_ = original_parent;
    }
    naming_cost = model_->GetUtility() - old_utility;
  }
  VLOG(2) << "Naming cost " << naming_cost << endl;

  return firing_cost + naming_cost;
}

Tuple Optimizer::GetRandomSurprisingTuple() {
  while (true) {
    TrueTuple * tt = GetRandomTrueTuple();
    double bits = GuessBenefit(tt);
    if (RandomFraction() < bits/10)
      return tt->GetTuple();
  }
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

bool Optimizer::MaybeFindManyExamplesRule(CandidateRule *ret, string *comments){
  PatternBuilder pb(this);
  if (!pb.TryInitializeFromSurprisingTuple()) return false;
  
  // Log things here
  if (GetVerbosity() >= 1) {
    VLOG(1) << "Initialized " << pb.ToString() << endl;
  }

  uint num_clauses = 1; while (RandomFraction() < 0.7) num_clauses++;  
  if (!pb.ExpandFully(num_clauses)) return false;

  Pattern &p = pb.pattern_;
  Tuple dummy = p[0]; p[0] = p[p.size()-1];  p[p.size()-1] = dummy;
  CandidateRule r = SplitOffLast(p);
  *comments = "ManyExamplesRule";
  return VetteCandidateRule(r, ret, StandardMaxWork(), comments);
}

bool Optimizer::MaybeFindRandomNewRule(CandidateRule *ret, string *comments){
  uint num_clauses = 1;
  while (RandomFraction() < 0.7) num_clauses++;  
  Pattern p;
  Substitution sub;
  int next_var = 0;
  set<Tuple> used_tuples;

  double best_guess = 0;
  Tuple s1;
  for (uint c=0; c<5; c++) {
    TrueTuple * tt = GetRandomTrueTuple();
    double bits = GuessBenefit(tt);
    if ( (c==0) || (bits > best_guess) ) {
      s1 = tt->GetTuple();
      best_guess = bits;
    }
  }
  VLOG(1) << "Settled on Tuple " << s1.ToString() << endl;

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
  Tuple dummy = p[0]; p[0] = p[p.size()-1];  p[p.size()-1] = dummy;
  CandidateRule r = SplitOffLast(p);
  *comments = "NewRule";
  return VetteCandidateRule(r, ret, StandardMaxWork(), comments);
}

bool Optimizer::PatternBuilder::TryInitializeFromSurprisingTuple() {

  for (int trials = 0; trials < 100; trials++) {

    // We may want to suck in more tuples from this example
    Tuple t = optimizer_->GetRandomSurprisingTuple();
  
    // Get the second example
  
    // Set a random set of positions to variables and try again
    Tuple vartuple = t;
    for (uint c=0; c<vartuple.size(); c++) {
      double varprob = 0.5;
      if (c == 0) varprob = 0.1; // Don't variablize the relation as much
      if (RandomFraction() < varprob) vartuple[c] = WILDCARD;
    }
    
    // Look for a random tuple matching the pattern
    Tuple t2;
    bool result = optimizer_->model_->GetTupleIndex()->GetRandomTupleMatching(vartuple, &t2);

    // Couldn't find a second tuple
    if (!result) continue;

    // Not much of a surprise if we pick the same thing twice
    if (t == t2) continue;

    Tuple onlytuple = vartuple.WildcardsToVariables();
    Substitution s1, s2;
    CHECK(ComputeSubstitution(onlytuple, t, &s1));
    CHECK(ComputeSubstitution(onlytuple, t2, &s2));
    subs_.push_back(s1);
    subs_.push_back(s2);
    pattern_.push_back(onlytuple);
    CollapseEquivalentVariables();
    CollapseConstantVariables();
    return true;
  }
  return false;
}

bool Optimizer::PatternBuilder::ExpandFully(uint size) {
  for (uint trials = 0; trials < 100 + 10 * size; trials++) {
    if (pattern_.size() < size) TryExpandOnce();
  }
  if (pattern_.size() < size) return false;
  return true;
}

bool Optimizer::PatternBuilder::TryExpandOnce() {

  // Pick a number of anchors
  uint num_anchors = 1;
  while (RandomFraction() < 0.5) num_anchors++;

  // Pick that size of subset of anchors
  set<int> anchors;
  if (num_anchors > subs_[0].size()) num_anchors = subs_[0].size();
  while (anchors.size() < num_anchors) {
    RandomElement(variter, subs_[0].sub_);
    anchors.insert(variter->first);
  }

  // Find the objects corresponding to those variables in the first example
  set<int> object_anchors;
  forall(run, anchors) {
    object_anchors.insert(subs_[0].Lookup(*run));
  }
  vector<int> v_object_anchors(object_anchors.begin(), object_anchors.end());

  // Get a random tuple with all the anchors
  // HERE make sure this tuple isnt the same as our pattern substituted with subs_[0]
  TupleIndex * ti = optimizer_->model_->GetTupleIndex();
  Tuple expansion_tuple;
  bool found = ti->GetRandomTupleContaining(&expansion_tuple, v_object_anchors, true);
  if (!found) return false;
  
  // Can amortize this work but for now ...
  Pattern sub_0_pattern = pattern_;
  set<Tuple> pattern0_tuples;
  subs_[0].Substitute(&sub_0_pattern);
  for (uint c=0; c<sub_0_pattern.size(); c++)
    if (sub_0_pattern[c] == expansion_tuple)
      return false;

  // Turn some of the constants into varialbes based on subs_[0]
  subs_[0].Reverse().Substitute(&expansion_tuple);  

  // Run through all generalizations
  Tuple good_generalization;
  bool any_good_generalization = false;
  vector<Tuple> expansion_tuples(subs_.size());
  for (GeneralizationIterator gen(expansion_tuple); !gen.done(); ++gen) {
    bool works = true;
    Tuple generalized = gen.Current();
    for(uint i=0; i<subs_.size(); i++) {
      Tuple specified = generalized;
      subs_[i].Substitute(&specified);
      int num_results = ti->Lookup(specified, NULL);
      if (num_results != 1) {
	works = false;
	break;
      }
      vector<Tuple> results;
      ti->Lookup(specified, &results);
      CHECK(results.size()==1);
      expansion_tuples[i] = results[0];
    }
    if (works) {
      any_good_generalization = true;
      good_generalization = generalized;
      break;
    }
  }
  if (!any_good_generalization) return false;
  for (uint i=0; i<good_generalization.size(); i++) {
    if (good_generalization[i] == WILDCARD) {
      int var = Variable(subs_[0].FirstUnusedVariable());
      for (uint sub_num=0; sub_num<subs_.size(); sub_num++){ 
	subs_[sub_num].Add(var, expansion_tuples[sub_num][i]);
      }
      good_generalization[i] = var;
    }
  }
  pattern_.push_back(good_generalization);
  CollapseEquivalentVariables();
  CollapseConstantVariables();
  if (GetVerbosity() >= 1) {
    VLOG(1) << "Expanded to " << ToString() << endl;
  }

  return true;
}

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
}

string Optimizer::PatternBuilder::ToString() {
  stringstream ret;
  ret << "Patternbuilder" << endl;
  ret << TupleVectorToString(pattern_) << endl;
  for (uint c=0; c<subs_.size(); c++) {
    ret << subs_[c].ToString() << endl;
  }
  return ret.str();
}

void Optimizer::RuleInfo::Canonicalize(){
  r_ = CanonicalizeRule(r_, NULL);
}

void Optimizer::RuleInfo::FindCandidateFirings(){
  vector<Substitution> subs;
  int64 max_work_now = max_work_/denominator_ + 10;
  bool success = 
    optimizer_->model_->GetTupleIndex()->FindSatisfactions
    (Concat(r_),
     combined_sampling_, 
     &subs_, &sampled_num_firings_, &max_work_now);
  if (!success) {
    hopeless_ = true;
    hopeless_cause_ = 1;
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
  // We will require also that there be at least two values of the
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
	  >= optimizer_->model_->GetUtility()-1.0)) {
    hopeless_ = true;
  }
}

void Optimizer::RuleInfo::FindNumSatisfactions(){
  // check that the preconditions aren't too much work to searh for.
  int64 max_work_now = max_work_/denominator_ * 2 +10;
  bool success = 
    optimizer_->model_->GetTupleIndex()->FindSatisfactions
    (r_.first, precondition_sampling_, 
     NULL, // substitutions 
     &sampled_num_satisfactions_, 
     &max_work_now);
  if (!success) {
    hopeless_ = true;
    hopeless_cause_ = 1;
    return;
  }
  estimated_satisfactions_ = sampled_num_satisfactions_ * (sample_postcondition_?1:denominator_);
}

void Optimizer::RuleInfo::RemoveUnrestrictivePreconditions(){
  // Try to remove preconditions that are not very restrictive.
  bool any_removed = true;
  while (any_removed) {
    any_removed = false;
    for (uint i=0; i<r_.first.size(); i++) {
      vector<Tuple> simplified_preconditions = RemoveFromVector(r_.first, i);
      // Test for disconnectedness
      if (!IsConnectedPattern(simplified_preconditions)) {
	VLOG(2) << "Not considering disconnected pattern" << endl;
	continue;
      }
      SamplingInfo simplified_sampling = precondition_sampling_;
      if (sampled_) {
	// TODO: We probably won't be able to remove the sampled precondition
	// do something about this.
	if (i < sample_clause_) {
	  simplified_sampling.position_--;	  
	} else if (i == sample_clause_){
	  simplified_sampling = SamplingInfo();
	}
      }
      // if a variable is in the result and occurs only in this clause
      // of the precondition, then this clause is necessary.
      if ((Intersection(GetVariables(r_.first[i]), GetVariables(r_.second))
	   - GetVariables(simplified_preconditions)).size()) continue;
      uint64 simplified_num_satisfactions = 0;
      int64 max_work_now = max_work_;
      if (optimizer_->model_->GetTupleIndex()->FindSatisfactions
	  (simplified_preconditions, 
	   simplified_sampling, 
	   NULL, // satisfaction
	   &simplified_num_satisfactions,
	   &max_work_now)) {
	if (simplified_num_satisfactions
	    <= sampled_num_satisfactions_ * 1.1){
	  // adjust the samplinginfo object
	  CHECK (!sample_postcondition_);
	  if (sampled_) {
	    if (i < sample_clause_) {
	      sample_clause_--;
	      precondition_sampling_.position_--;
	      combined_sampling_.position_--;
	    } else if (i==sample_clause_){
	      precondition_sampling_ = SamplingInfo::Unsampled();
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

bool Optimizer::RuleInfo::Vette() {

  VLOG(0) << "Raw=" << CandidateRuleToString(r_) << endl;
  comments_ += " Raw=" + CandidateRuleToString(r_);

  // Make sure existing preconditions limit results
  if ( (GetVariables(r_.first).size() != 0) &&
       (Intersection(GetVariables(r_.first), 
		     GetVariables(r_.second)).size()==0) ) {
    hopeless_ = true; return false;
  }

  // Canonicalize and start looking for a good sampling
  Canonicalize();

  // since things within the loops can change r_, we revert it every time
  CandidateRule revert_to_rule = r_;

  // Sample more aggressively at first, then less agressively
  for (denominator_ = 1024; denominator_>0; denominator_ >>=1 ) {
    r_ = revert_to_rule;
    // Try sampling each clause to see if one works. If precondition is empty try postcondition
    sample_postcondition_ = (r_.first.size()==0);
    const Pattern & sample_pattern = sample_postcondition_ ? r_.second:r_.first;

    for (uint sample_clause = 0; sample_clause<sample_pattern.size(); sample_clause++) {
      // (since sample_clause_ can be changed in the loop)
      sample_clause_ = sample_clause; 
      r_ = revert_to_rule;
      needs_bigger_sample_ = false;
      sampled_ = (denominator_ > 1);

      //  see if sampling this clause by this denominator works.  
      precondition_sampling_ = combined_sampling_ = SamplingInfo();
      if (sampled_) {
	combined_sampling_ = SamplingInfo::RandomRange(sample_clause_, denominator_);
	if (!sample_postcondition_) precondition_sampling_ = combined_sampling_;
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

      RemoveBoringVariables();
      if (needs_bigger_sample_) continue; 
      if (hopeless_) {
	VLOG(0) << "Quit after RemoveBoringVariables denom:" << denominator_
		<< endl;
	return false;
      }

      // Try to remove preconditions that are not very restrictive.
      RemoveUnrestrictivePreconditions();
      if (needs_bigger_sample_) continue; if (hopeless_) return false;

      Canonicalize();
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

void Optimizer::PushTimesThroughRuleSats(TrueTuple *tt){
  tt->ComputeSetTime();
  forall(run_sat, tt->GetSatisfactions()) {
    Satisfaction *sat = *run_sat;
    sat->ComputeSetTime();
    forall(run_rs, sat->GetRuleSats()) {
      RuleSat * neg_rs = *run_rs;
      neg_rs->ComputeSetTime();
    }
  }
}

// Propagates the time change after we change the delay of a rule
// through the true tuples it causes and the rule sats in which
// they participate.
// This is to avoid fixing all of the times in the model.
void Optimizer::PushTimesAfterChangeDelay(Rule *rule){
  forall(run_rs, rule->GetRuleSats()) {
    RuleSat * rs = run_rs->second;
    // the times of the rule sats are already updated.
    //if (!rs->ComputeSetTime()) continue;
    forall(run_f, rs->GetFirings()) {
      Firing * f = run_f->second;
      if (!f->ComputeSetTime()) continue;
      forall(run_tt, f->GetTrueTuples()){
	TrueTuple * tt = *run_tt;
	PushTimesThroughRuleSats(tt);
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
	  << " utility_=" << model_->GetUtility() << endl;

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

    vector<Substitution> subs_for_removed_firings;
    forall(run, firings) {
      if ((*run)->Exists()) {
	subs_for_removed_firings.push_back((*run)->GetFullSubstitution());
	set<TrueTuple *> results = (*run)->GetTrueTuples();
	(*run)->Erase();
	forall(run, results) PushTimesThroughRuleSats(*run);
      }
    }
    OptimizeStrength(alt_r);
    VLOG(1) << "Removed " << firings.size() 
	    << " firings for rule " << alt_r->GetID()
	    << " utility_=" << model_->GetUtility() << endl;
        
    
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
	      << " utility_=" << model_->GetUtility() << endl;

      // Try to make a variation on the alternate rule that switches
      // the result with one of the preconditions.  
      variants.insert(make_pair(lhs, rhs));
      if (!alt_r->Exists()) continue;
    }
    if (may_want_to_add_negative_rule) {
      OptimizationCheckpoint cp_negative_rule(this, false);

      Rule * negative_rule = TryMakeFunctionalNegativeRule(alt_r);
      set<RuleSat *> negative_rule_sats;
      for (uint i=0; i<subs_for_removed_firings.size(); i++) {
	RuleSat * rs = negative_rule->FindRuleSat(subs_for_removed_firings[i]);
	if (rs) negative_rule_sats.insert(rs);
      }
      VLOG(1) << "added functional negative rule. "
	      << " utility_=" << model_->GetUtility() << endl;      
      
      bool done_iterating = false;
      int utility_pass = 0;
      FixTimesFixCircularDependencies();
      VLOG(1) << "before utility pass utility " << model_->GetUtility() << endl;
      if (GetVerbosity() >= 1) model_->ToHTML("html.rb" + itoa(alt_r->GetID()));

      do {
	OptimizationCheckpoint speedupthings(this, false);
	VLOG(1) << "utility pass A " << utility_pass << " utility " << model_->GetUtility() << endl;
	MakeNegativeRuleSatsHappenInTime(negative_rule_sats);
	VLOG(1) << "utility pass B " << utility_pass << " utility " << model_->GetUtility() << endl;
	FixTimesFixCircularDependencies();
	VLOG(1) << "utility pass C " << utility_pass << " utility " << model_->GetUtility() << endl;
	done_iterating = !speedupthings.KeepChanges();
	utility_pass++;
	if (GetVerbosity() >= 1) model_->ToHTML("html.ra" + itoa(alt_r->GetID()));
      } while (!done_iterating);

      VLOG(1) << "after MakeNegativeRuleSatsHappenInTime "
	      << " utility_=" << model_->GetUtility() << endl;      

      set<Rule *> to_optimize; 
      to_optimize.insert(negative_rule);
      to_optimize.insert(alt_r);
      OptimizeRuleStrengths(to_optimize);

      VLOG(1) << "after OptimizeRuleStrengths "
	      << " utility_=" << model_->GetUtility() << endl;      
    }
  }

  VLOG(1) << "removed all alternate explanations " 
	  << " utility_=" << model_->GetUtility() << endl;
  if (max_recursion >0) 
    forall(run, variants) {
      OptimizationCheckpoint cp_variation(this, false);
      TryRuleVariations(run->first, run->second, max_recursion-1);
    }
  VLOG(1) << "Added variant rules " 
	  << " utility_=" << model_->GetUtility() << endl;
}

struct DelayMap {
  map<Rule * , pair<EncodedNumber, EncodedNumber> > delays_;
  set<RuleSat *> rulesats_with_nonstandard_delay_;
  /// add a rule with its current delay
  void Add(Rule * r) {
    if (delays_ % r) return;
    delays_[r] = make_pair(r->GetDelay(), r->GetDelay());
  }
  void SetStandardDelay(Rule *r, EncodedNumber new_delay) {
    SetDelay(r, new_delay, false);
  }
  void SetNonstandardDelay(Rule *r, EncodedNumber new_delay) {
    SetDelay(r, new_delay, true);
  }
  void SetDelay(Rule * r, EncodedNumber new_delay, bool nonstandard) {
    Add(r);
    if (nonstandard) delays_[r].second = new_delay;
    else delays_[r].first = new_delay;
    if (delays_[r].first == r->GetDelay() && delays_[r].second == r->GetDelay())
      delays_.erase(r);
  }
  EncodedNumber GetDelay(Rule * r, bool nonstandard){
    if (!(delays_ % r)) return r->GetDelay();
    return nonstandard?delays_[r].second:delays_[r].first;
  }
  EncodedNumber GetDelay(RuleSat *rs){
    return GetDelay(rs->GetRule(), UsesNonstandardDelay(rs));
  }
  void MakeDelayNonstandard(RuleSat * rs){
    rulesats_with_nonstandard_delay_.insert(rs);
  }
  bool UsesNonstandardDelay(RuleSat * rs) const {
    return (rulesats_with_nonstandard_delay_ % rs);
  }
};

// represents the time compuation for a rulesat.
struct RuleSatTimeNode{
  Rule * rule_;
  RuleSat * rulesat_;
  DelayMap * delay_map_;
  Time time_;
  set<RuleSatTimeNode *> children_;
  Time max_unrepresented_child_time_;  
  RuleSatTimeNode(RuleSat * rs, DelayMap * delays){
    rule_ = rs->GetRule();
    rulesat_ = rs;    
    delay_map_ = delays;
  }
  ~RuleSatTimeNode(){
    forall (run, children_) delete (*run);
  }
  void Display(int indentation) {
    Pattern prec = rulesat_->GetRule()->GetPrecondition()->GetPattern();
    Pattern res = rulesat_->GetRule()->GetResult();
    Substitution sub = rulesat_->GetSatisfaction()->GetSubstitution();
    sub.Substitute(&prec);
    sub.Substitute(&res);
    VLOG(0) << string(indentation, ' ') 
	    << "t=" << time_.ToSortableString()
	    << " muct=" << max_unrepresented_child_time_.ToSortableString()
	    << " rs=" << TupleVectorToString(prec) 
	    << "->" << TupleVectorToString(res) << endl;
    forall(run, children_) (*run)->Display(indentation+2);
  }
  bool UsesNonstandardDelay() {
    return delay_map_->UsesNonstandardDelay(rulesat_);
  }
  void MakeDelayNonstandard(){
    delay_map_->MakeDelayNonstandard(rulesat_);
  }
  Time MaxChildTime() {
    Time max_child_time = max_unrepresented_child_time_;
    forall(run, children_) {
      if ((*run)->time_ > max_child_time) max_child_time = (*run)->time_;
    }
    return max_child_time;
  }
  void ComputeTime(){
    forall(run, children_) (*run)->ComputeTime();
    time_ = MaxChildTime();
    if (rule_->GetRuleType() != NEGATIVE_RULE) 
      time_.Increment(delay_map_->GetDelay(rulesat_), 1);
  }
  RuleSatTimeNode * AddChild(RuleSat *rs){
    RuleSatTimeNode * ret = new RuleSatTimeNode(rs, delay_map_);
    children_.insert(ret);
    return ret;
  }
  // adds all decendents that with time >=horizon, but fails if more than 
  // max_nodes.  Returns true on success.
  bool ExpandBackTo(Time horizon, int *max_nodes) {
    max_unrepresented_child_time_ = Time();
    set<TrueTuple *> tuples = rulesat_->GetSatisfaction()->GetTrueTuples();
    VLOG(2) << "expands to " << tuples.size() << " tuples" << endl;
    forall (run_t, tuples){
      Firing * f = (*run_t)->GetFirstCause();
      if (!f) {
	VLOG(1) << "Didn't get first cause" << endl;
	return false;
      }
      RuleSat * rs = f->GetRuleSat();
      if (GetVerbosity() >= 1) {
	VLOG(2) << "rulesat " << rs->GetID() 
		<< " time:" << rs->GetTime().ToSortableString() << endl;
      }
      if (rs->GetTime() < horizon) {
	if (rs->GetTime() > max_unrepresented_child_time_)
	  max_unrepresented_child_time_ = rs->GetTime();
      } else {
	(*max_nodes)--;
	if (*max_nodes<0) {
	  VLOG(2) << "Maxnodes:" << *max_nodes;
	  return false;
	}
	RuleSatTimeNode * child = AddChild(rs);
	VLOG(2) << "Adding child for RuleSat " << rs->GetID() << endl;
	if (!(child->ExpandBackTo(horizon, max_nodes))) {
	  VLOG(2) << "Child expansion failed" << endl;
	  return false;
	}
      }    
    }
    return true;
  }
  void ExpandLinearly(int num_steps) {
    max_unrepresented_child_time_ = Time();
    set<TrueTuple *> tuples = rulesat_->GetSatisfaction()->GetTrueTuples();    
    // find the last one to happen.
    if (tuples.size()==0) return;
    TrueTuple * last = *tuples.begin();
    forall(run, tuples) {
      if ((*run)->GetTime() > last->GetTime()) last = *run;
    }
    if (num_steps == 0){
      max_unrepresented_child_time_ = last->GetTime();
      return;
    }
    forall(run, tuples) {
      if ((*run != last) && ((*run)->GetTime() > max_unrepresented_child_time_))
	max_unrepresented_child_time_ = (*run)->GetTime();
    }
    Firing * f = last->GetFirstCause();
    if (!f) {
      cerr << "weird - tuple with no cause" << endl;
      return;
    }
    RuleSat * rs = f->GetRuleSat();
    RuleSatTimeNode * child = AddChild(rs);
    child->ExpandLinearly(num_steps-1);
    return;
  }
  // returns true on success
  bool SpeedUpToHappenBefore(RuleSatTimeNode * to_beat) {
    forall(run, children_){
      bool result = (*run)->SpeedUpToHappenBefore(to_beat);
      if (!result) return false;
    }
    ComputeTime();
    to_beat->ComputeTime();
    if (time_ < to_beat->time_) return true;
    Time max_child_time = MaxChildTime();
    if (max_child_time >= to_beat->time_) return false;
    // try speeding up this rule. (changing the standard delay) 
    EncodedNumber fast_enough;
    while (max_child_time + fast_enough >= to_beat->time_)
      fast_enough.bits_.push_back(false);    
    EncodedNumber old_delay = delay_map_->GetDelay(rulesat_);
    if (!UsesNonstandardDelay()) {
      delay_map_->SetStandardDelay(rule_, fast_enough);
      ComputeTime();
      to_beat->ComputeTime();
      if (time_ < to_beat->time_) {
	VLOG(1) << "TIMECHANGE Rule:" << rule_->GetID() << " new delay:" << fast_enough.ToSortableString() << endl;
	return true;
      }
      delay_map_->SetStandardDelay(rule_, old_delay);      
    }
    MakeDelayNonstandard();
    if (fast_enough < delay_map_->GetDelay(rule_, true))
      delay_map_->SetNonstandardDelay(rule_, fast_enough);
    ComputeTime();
    to_beat->ComputeTime();
    if (time_ < to_beat->time_) {
      VLOG(1) << "NONSTANDARD TIMECHANGE Rule:" << rule_->GetID() << " nonstandard delay:" << fast_enough.ToSortableString() << endl;
      return true;
    }
    // cerr << "all efforts failed to speed up negative rulesat" << endl;
    return false;
  }
};

void Optimizer::MakeNegativeRuleSatsHappenInTime(const set<RuleSat *> 
						 rule_sats) {

  VLOG(1) << "MakeNegativeRuleSatsHappenInTime #rule_sats=" << rule_sats.size() << endl;

  // Get a suggestion from each rulesat
  map< map<Rule *, pair<EncodedNumber, EncodedNumber> >, pair<int, set<RuleSat*> > > proposals;
  forall(run_rs, rule_sats) {
    // Compute all the RuleSatTimeNodes
    DelayMap delay_map;
    // Get the positive and negative rulesats
    RuleSat * neg_rs = *run_rs;
    RuleSat * pos_rs = neg_rs->GetTarget();
    
    // Form their trees and expand the trees and compute time
    RuleSatTimeNode ntree = RuleSatTimeNode(neg_rs, &delay_map);
    RuleSatTimeNode ptree = RuleSatTimeNode(pos_rs, &delay_map);
    ptree.ExpandLinearly(5);
    ptree.ComputeTime();
    int max_nodes = 10;
    bool expand_result = ntree.ExpandBackTo(ptree.time_, &max_nodes);
    if (!expand_result) {
      VLOG(1) << "Expansion failed" << endl; 
      continue;
    }
    ntree.ComputeTime();

    // Nothing to do
    if (ntree.time_ < ptree.time_) continue;

    // Log the trees
    if (GetVerbosity() >= 1) {
      VLOG(0) << "neg_tree="<< endl; ntree.Display(0);
      VLOG(0) << "pos_tree=" << endl; ptree.Display(0);
    }

    // See if we can beat the positive tree
    if (!ntree.SpeedUpToHappenBefore(&ptree)) continue;
    if (GetVerbosity() >= 1) {
	VLOG(0) << "SPED UP VERSION" << endl;
	VLOG(0) << "neg_tree="<< endl; ntree.Display(0);
	VLOG(0) << "pos_tree=" << endl; ptree.Display(0);
    }
    // Save this result
    (proposals[delay_map.delays_].first)++;
    (proposals[delay_map.delays_].second).insert(delay_map.rulesats_with_nonstandard_delay_.begin(),
						 delay_map.rulesats_with_nonstandard_delay_.end());
    
  }

  // Find the best looking proposal
  if (proposals.size() == 0) {
    VLOG(1) << "Can't think of anything. No proposals" << endl;
    return;
  }
  typeof(proposals.begin()) best = proposals.begin();
  forall (run, proposals) {
    if (run->second.first > best->second.first) best = run;
  }

  // Let's see the proposal
  if (GetVerbosity() >= 1) {
    VLOG(0) << "Propoposal addresses " << best->second.first << " examples\n";
  }

  // Change the standard timing on all rules
  forall (run, best->first) {
    Rule * r = run->first;
    EncodedNumber standard_delay = run->second.first;
    EncodedNumber nonstandard_delay = run->second.second;
    if (standard_delay != r->GetDelay()){
      r->ChangeDelay(standard_delay);
      VLOG(1) << "Changing delay on rule " << r->GetID() << " to " 
	      << standard_delay.ToSortableString() << endl;
    }    
  }

  // make alternate rules where we need to change the delay per rulesat
  // to a non-standard value.
  // TODO: may want to add a functional negative rule here?
  map<Rule *, set<RuleSat *> > by_rule;
  forall(run, best->second.second) { // runs through the nonstandard rulesats
    by_rule[(*run)->GetRule()].insert(*run);    
  }

  // Try to specify the rule
  forall(run_r, by_rule){
    VLOG(1) << "Specifying a rule" << endl;
    Rule * r = run_r->first;
    set<Firing *> old_firings;
    forall(run_rs, run_r->second){
      RuleSat * rs = *run_rs;
      forall(run_f, rs->GetFirings())
	old_firings.insert(run_f->second);
    }
    // figure out if we can easily specify the new rule by replacing
    // any of its variables with constants.
    map<int, map<int, int> > var_to_values;
    forall(run_f, old_firings) {
      Firing *f = *run_f;
      Substitution s = f->GetFullSubstitution();
      forall(run_sub, s.sub_) {
	var_to_values[run_sub->first][run_sub->second]++;
      }
    }
    Substitution simplify_sub;
    forall(run_vars, var_to_values){
      forall(run_values, run_vars->second) {
	if (1.1 * run_values->second >= old_firings.size() && (old_firings.size() > 10)) {
	  simplify_sub.Add(run_vars->first, run_values->first);
	  VLOG(1) << "Specifying " << LEXICON.GetString(run_vars->first) 
		  << " -> " << LEXICON.GetString(run_values->first)
		  << endl;
	}
      }
    }
    Pattern precondition = r->GetPrecondition()->GetPattern();
    Pattern result = r->GetResult();
    simplify_sub.Substitute(&precondition);
    simplify_sub.Substitute(&result);
    set<int> new_rule_vars 
      = Union(GetVariables(precondition), GetVariables(result));

    RuleType type = (GetVariables(result)-GetVariables(precondition)).size()?CREATIVE_RULE:SIMPLE_RULE;

    // TODO maybe we want to find a rule with this precondition and result, and (= or faster? delay)
    const pair<EncodedNumber, EncodedNumber> * find_delay = best->first % r;
    CHECK(find_delay);
    EncodedNumber delay_guess = find_delay->second;
    VLOG(1) << "Making a rule variant on rule:" << r->GetID()
	    << " speed:" << delay_guess.ToSortableString() << endl;

    // Let's look for a new rule or make it
    Rule * new_rule = NULL;
    set<Rule*> potential_rules = model_->FindPositiveRules(precondition, result);
    forall (run, potential_rules) {
      if ((*run)->GetDelay() <= delay_guess) new_rule = *run;
    }
    if (!new_rule)
      new_rule = model_->MakeNewRule(precondition, delay_guess, type, 
				     NULL, result, r->GetStrength(), r->GetStrength2());
    
    forall(run_f, old_firings){
      Firing *f = *run_f;
      Substitution old_sub = f->GetFullSubstitution();
      if (!simplify_sub.IsSubsetOf(old_sub)) continue;
      Substitution new_sub = old_sub.Restrict(new_rule_vars);
      new_rule->AddFiring(new_sub);
      f->Erase();
    }
    set<Rule *> to_optimize;
    FixTimesFixCircularDependencies();
    to_optimize.insert(new_rule);
    to_optimize.insert(r);
    OptimizeRuleStrengths(to_optimize);
  }
}

void Optimizer::OptimizeRuleStrengths(set<Rule *>rules){
  double improvement = 0;
  do {
    double old_utility = model_->GetUtility();
    forall(run, rules) OptimizeStrength(*run);
    improvement = model_->GetUtility() - old_utility;
  } while (improvement > 0.1);
}
  
Rule *  Optimizer::TryMakeFunctionalNegativeRule(Rule *r){
  // TODO: maybe play with the delay.
  Pattern precondition 
    = Concat(r->GetPrecondition()->GetPattern(), r->GetResult());
  Rule * negative_rule = model_->FindNegativeRule(precondition, r);
  if (negative_rule) {
    VLOG(1) << "used existing negative rule" << endl;
    return negative_rule;
  }
  VLOG(1) << "making a new negative rule" << endl;
  return 
    model_->MakeNewRule(precondition, 
			EncodedNumber(),
			NEGATIVE_RULE, r,
			vector<Tuple>(),
			EncodedNumber(),
			EncodedNumber());
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
    CandidateRule cr = CanonicalizeRule(make_pair(lhs, rhs), NULL);
    OptimizationCheckpoint cp(this, false);
    string comments = "recursively added as a variation of "
      + CandidateRuleToString(make_pair(preconditions, result));
    TryAddPositiveRule(cr.first, cr.second, max_recursion,
		       comments);
    if (cp.KeepChanges()) break;
  }
}

void Optimizer::TryAddPositiveRule(const Pattern & preconditions,
				   const Pattern & result,
				   int max_recursion,
				   string comments){  
  if (GetVerbosity() >= 2) {
    model_->VerifyLayer2();
  }

  vector<Substitution> subs;
  vector<Tuple> combined = preconditions;
  combined.insert(combined.end(), result.begin(), result.end());
  int64 max_work_now = StandardMaxWork();
  bool last_ditch = 
    model_->GetTupleIndex()->FindSatisfactions
    (combined, SamplingInfo::Unsampled(), &subs, 
     NULL, // num_satisfactions 
     &max_work_now);
  if (last_ditch == false) {
    VLOG(0) << "Somehow this one got this far! no further!" << endl;
    return;
  }
  set<int> precondition_vars = GetVariables(preconditions);
  set<int> result_vars = GetVariables(result);
  result_vars = result_vars-precondition_vars;
  RuleType type = result_vars.size()?CREATIVE_RULE:SIMPLE_RULE;
  VLOG(0) 
    << string(10 - 3*max_recursion, ' ')
    << "Contemplating creating rule " 
    << TupleVectorToString(preconditions)
    << " ->" << TupleVectorToString(result) << endl;
  if (model_->FindPositiveRule(preconditions, result)) {
    VLOG(1) << "rule already exists" << endl;
    return;
  }
  
  VLOG(1) << "before adding rule utility=" 
	  << model_->GetUtility() << endl;
  double added_arbitrary_term_ll = -model_->GetChooserLnLikelihood();
  Rule * r = model_->MakeNewRule(preconditions, EncodedNumber(), 
			    type, 0, result, EncodedNumber(), EncodedNumber());
  r->AddComments(comments);
  added_arbitrary_term_ll += model_->GetChooserLnLikelihood();
  VLOG(1) << "Rule encoding costs: "
	  << r->GetPrecondition()->GetDirectPatternEncodingLnLikelihood()
	  << " + " 
	  << r->GetDirectPatternEncodingLnLikelihood() 
	  << " + " 
	  << added_arbitrary_term_ll
	  << endl;
  // r->ExplainEncoding();
  VLOG(1) << "after adding rule utility=" 
	  << model_->GetUtility() << endl;
  TryAddFirings(r, subs, max_recursion);
  if (!r->Exists()) return;
  VLOG(1) << "after TryAddFirings: utility=" << model_->GetUtility() << endl;
  OptimizeStrength(r);
  VLOG(1) << "after OptimizeStrength: utility=" << model_->GetUtility() << endl;
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
  old_utility_ = model_->GetUtility();
}
OptimizationCheckpoint::~OptimizationCheckpoint() {
  if (!KeepChanges()) {
    model_->GetChangelist()->Rollback(cp_);
    if (logging_)
      VLOG(1) << "reverting utility="
	   << model_->GetUtility() << endl;
  }
}
bool OptimizationCheckpoint::Better() {
  return (model_->MayBeTimeFixable()
	  && (model_->GetUtility() > old_utility_ + 0.01 + 
	      (1+fabs(model_->GetUtility())) * 1e-14));
}
bool OptimizationCheckpoint::KeepChanges() {
  if (!Better()) return false;
  if (fix_times_) {
    bool result = optimizer_->FixTimesFixCircularDependencies();
    if (!result) return false;
  }
  return Better();
}

double OptimizationCheckpoint::Gain() {
  return model_->GetUtility() - old_utility_;
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
		<< " utility_=" << model_->GetUtility() << endl;
	if (trial==0) { // drop last bit
	  if (to_alter->bits_.size()==0) continue;
	  to_alter->bits_.pop_back();	  
	} else {
	  to_alter->bits_.push_back(trial==2);
	}
	OptimizationCheckpoint cp(this, false);
	r->ChangeStrength(strength, strength2);
	VLOG(2) << "   new_val=" << to_alter->ToSortableString()
		<< " new_utility=" << model_->GetUtility() << endl;
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
  model_->FindExplanationsForResult(s, &explanations, excluded, NULL);
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
    if (i==0 || model_->GetUtility() > best) {
      which=i;
      best = model_->GetUtility();
    }
    model_->GetChangelist()->Rollback(cp);
  }
  explanations[which].first->AddFiring(explanations[which].second);
  OptimizeStrength(explanations[which].first);
}

// Returns false if fails to finish in time
bool Optimizer::FixTimesFixCircularDependencies(int time_limit) {
  // TODO: make this smarter.  much smarter
  time_t end_time = time(NULL) + time_limit;
  
  VLOG(1) << " start utility_=" << model_->GetUtility() << endl;
  while (model_->GetTimesDirty().size() || model_->GetRequiredNeverHappen().size()) {
    bool result = model_->FixTimes();
    if (!result) {
      VLOG(0) << "FixTimes failed" << endl;
      return false;
    }
    if (model_->GetRequiredNeverHappen().size()) {
      VLOG(1) << "  explaining a required proposition" << endl;
      TrueTuple * p 
	= (TrueTuple *)(*(model_->GetRequiredNeverHappen().begin()));
      Explain(p, &model_->GetNeverHappen(), false);
      VLOG(1) << "   explained " << p->GetID() << endl;
      if (GetVerbosity() >= 1) model_->ToHTML("html");
    }
    if (time(NULL) >= end_time) {
      VLOG(0) << "FixTimesFixCircularDependencies failed!!!" << endl;
      return false;
    }
  }
  model_->DeleteNeverHappeningComponents();
  CHECK(model_->GetRequiredNeverHappen().size() == 0);
  VLOG(1) << " end utility_=" << model_->GetUtility() << endl;
  return true;
  //if (absent_required_.size()){
  //  ToHTML("html");
  //  CHECK(absent_required_.size()==0);
  // }
}

/*void ExplainEncoding(Rule *r){
  forall(run, r->encoding_) 
    Explain(r->model_, *run, &r->model_->never_happen_, false);
    }*/



