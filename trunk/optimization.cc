// Copyright (C) 2006 Google Inc.
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
// Author: Noam Shazeer


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

pair<vector<Tuple>, vector<Tuple> >
Optimizer::FindRandomCandidateRule(Tactic tactic){
  pair<vector<Tuple>, vector<Tuple> > ret;
  while (!MaybeFindRandomCandidateRule(&ret, tactic));
  recently_checked_[ret] = model_->GetLnLikelihood();
  return ret;
}
bool Optimizer::MaybeFindRandomCandidateRule(CandidateRule *ret, 
					     Tactic tactic){
  switch (tactic) {
  case NEW_RULE:
    return MaybeFindRandomNewRule(ret);
    break;
  default:
    CHECK(false);
    //return MaybeFindRandomVariantRule(ret, tactic);
    break;
  }
  return false;
}
TrueTuple * Optimizer::GetRandomTrueTuple(){
  TrueTuple *ret = model_->FindTrueTuple(*(model_->GetTupleIndex()->RandomTuple()));
  CHECK(ret);
  return ret;
}

/*
bool MaybeFindRandomVariantRule(Model *m, CandidateRule *ret, Tactic tactic){
  int64 max_work = (uint)(100/pow(RandomFraction(), 1.0));
  TrueTuple * tp = GetRandomTrueTuple(m);
  CHECK(tp->causes_.size());
  Firing * f = *(tp->causes_.begin());
  RuleSat * rs = f->rule_sat_;
  Rule * r = rs->rule_;
  CandidateRule cand = make_pair(r->precondition_->clauses_, r->result_);
  Substitution full_sub = f->GetFullSubstitution();
  switch (tactic) {
  case SPECIFY_ONE: {
    RandomElement(assignment, full_sub.sub_);
    Substitution little_sub;
    little_sub.Add(assignment->first, assignment->second);
    little_sub.Substitute(&cand);
    return VetteCandidateRule(cand, ret, max_work);
    break;
  }
  case GENERALIZE_ONE: {
    int literal = -1;
    for (int try_num=0; try_num<10; try_num++) {
      const vector<Tuple> & v = ((rand()%2)?cand.first:cand.second);
      if (v.size()==0) continue;
      const Tuple & s = v[rand()%v.size()];
      int l = s[rand() % s.size()];
      if (!IsVariable(l)) {
	literal = l;
	break;
      }
    }
    if (literal == -1) return false;
    set<int> variables = 
      Union(GetVariables(cand.first), GetVariables(cand.second));
    int variable = Variable(0);
    forall(run, variables) {
      if (Variable(*run) >= Variable(variable))
	variable = Variable(Variable(*run)+1);
    }
    Substitution little_sub;
    little_sub.Add(literal, variable);
    little_sub.Substitute(&cand);
    return VetteCandidateRule(cand, ret, max_work);    
    break;
  }
  default:
    break;
  }
  CHECK(false);
  return false;
}
*/

bool Optimizer::MaybeFindRandomNewRule(CandidateRule *ret){
  int64 max_work = min ((uint)(10/pow(RandomFraction(), 1.0)), 5  * (uint)model_->GetNumTrueTuples());
  uint num_clauses = 1;
  while (RandomFraction() < 0.7) num_clauses++;  
  vector<Tuple> p;
  Substitution sub;
  int next_var = 0;
  set<const Tuple *> used_tuples;
  const Tuple * s1 = model_->GetTupleIndex()->RandomTuple();
  used_tuples.insert(s1);
  p.push_back(*s1);
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
	= (RandomFraction()<0.05)?0:(1+(rand() % (p[clause].size()-1)));
      int w = p[clause][term_in_clause];
      if (w<0) continue;
      anchors.insert(w);
    }
    vector<int> v_anchors(anchors.begin(), anchors.end());
    const Tuple * s 
      = model_->GetTupleIndex()->GetRandomTupleContaining(v_anchors, true);
    if (!s) continue;
    if (used_tuples % s) continue;
    used_tuples.insert(s);
    p.push_back(*s);
    forall(run, anchors){
      sub.Add(Variable(next_var++), *run);
    }
    sub.Substitute(&p);
  }
  sub.Substitute(&p);
  for (uint i=0; i<p.size(); i++) 
    for (uint j=0; j<p[i].size(); j++)
      if (p[i][j]>=0) {
	double make_var_prob = (j==0)?0.05:0.5;
	if (RandomFraction() < make_var_prob) {
	  sub.Add(p[i][j], Variable(next_var));
	  next_var++;
	  sub.Substitute(&p);
	}
      }
  CandidateRule r = SplitOffLast(p);
  CandidateRule simplified_rule;
  return VetteCandidateRule(r, ret, max_work);
}

bool Optimizer::VetteCandidateRule(CandidateRule r, 
				   CandidateRule * simplified_rule, 
				   int64 max_work) {
  vector<Tuple> p = Concat(r);
  uint64 num_satisfactions;
  vector<Substitution> subs;
  r = CanonicalizeRule(r);
  if ((recently_checked_ % r) 
      && (recently_checked_[r] <= model_->GetLnLikelihood()+1.0)) return false;
  bool success = 
    model_->GetTupleIndex()->FindSatisfactions(p, &subs, &num_satisfactions,
					  max_work, 0);
  if (max_work >=0 && num_satisfactions > (uint64)max_work) success = false;
  if (!success) return false;
  if (num_satisfactions < 2) return false;
  
  // check that the preconditions aren't too much work to searh for.
  vector<Tuple> preconditions = r.first;
  uint64 preconditions_num_satisfactions = 0;
  bool preconditions_success = 
    model_->GetTupleIndex()->FindSatisfactions(preconditions, 0, 
					  &preconditions_num_satisfactions, 
					  max_work, 0);
  if (max_work>=0 && preconditions_num_satisfactions > (uint64)max_work) 
    preconditions_success = false;
  if (!preconditions_success) return false;
  for (uint i=0; i<preconditions.size(); i++) {
    vector<Tuple> simplified_preconditions 
      = RemoveFromVector(preconditions, i);
    uint64 simplified_num_satisfactions = 0;
    if (model_->GetTupleIndex()->FindSatisfactions(simplified_preconditions, 0, 
					     &simplified_num_satisfactions, 
					     max_work, 0)) {
      if (simplified_num_satisfactions == preconditions_num_satisfactions){
	preconditions = RemoveFromVector(preconditions, i);
	p = RemoveFromVector(p, i);
	i--;
      }
    }
  }
  map<int, set<int> > replacements;
  for (uint i=0; i<subs.size(); i++) {
    forall(run, subs[i].sub_){
      replacements[run->first].insert(run->second);
    }
  }
  Substitution boring_variables;
  forall(run, replacements) {
    if (run->second.size()==1) {
      boring_variables.Add(run->first, *run->second.begin());
    }
  }
  boring_variables.Substitute(&r.first);
  boring_variables.Substitute(&r.second);
  if (GetVariables(r.second).size()==0) return false;
  r.first = RemoveVariableFreeTuples(r.first);
  r.second = RemoveVariableFreeTuples(r.second);
  r = CanonicalizeRule(r);
  *simplified_rule = r;
  if ((recently_checked_ % r) 
      && (recently_checked_[r] <= model_->GetLnLikelihood()+1.0)) return false;
  
  VLOG(1) << "max_work=" << max_work << " satisfactions(combined)="
	  << num_satisfactions << " satisfactions(preconditions)="
	  << preconditions_num_satisfactions
	  << " rule=" << TupleVectorToString(r.first)
	  << "->" << TupleVectorToString(r.second)
	  << " simplified rule=" << TupleVectorToString(simplified_rule->first)
	  << "->" << TupleVectorToString(simplified_rule->second) << endl;
    
  return true;
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

void Optimizer::TryAddFirings(Rule * rule, const vector<Substitution> & subs,
		   int max_recursion){
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
  VLOG(1) << "::TryAddFirings Added " << subs.size() << " firings " 
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
    VLOG(1) << "::TryAddFirings alt_r_id=" << alt_r->GetID()
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
    VLOG(1) << "::TryAddFirings Removed " << firings.size() 
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
      if (GetVerbosity() >= 1) model_->VerifyLayer2();
      forall(run, to_explain) {
	if ((*run)->GetCauses().size() == 0) 
	  Explain(*run, NULL, false);
      }
      VLOG(1) << "::TryAddFirings Erased Rule " 
	      << " ln_likelihood_=" << model_->GetLnLikelihood() << endl;

      // Try to make a variation on the alternate rule that switches
      // the result with one of the preconditions.  
      variants.insert(make_pair(lhs, rhs));

      
      if (!alt_r->Exists()) continue;
      /*
	if (may_want_to_add_negative_rule) {
	OptimizationCheckpoint cp_negative_rule(this, true???);
	cp_negative_rule.logging_ = true;
	// make sure r has a smaller delay than alt_r
	if (!(rule->GetDelay() < alt_r->GetDelay())) {
	EncodedNumber new_delay = alt_r->GetDelay();
	new_delay.bits_.push_back(false);
	rule->ChangeDelay(new_delay);
	}
	//TryMakeFunctionalNegativeRule(alt_r);
	VLOG(1) << "::TryAddFirings Made a negative rule. "
	<< " ln_likelihood_=" << model_->GetLnLikelihood() << endl;      
	}
      */
    }
  }
  VLOG(1) << "::TryAddFirings removed all alternate explanations " 
	  << " ln_likelihood_=" << model_->GetLnLikelihood() << endl;
  if (max_recursion >0) 
    forall(run, variants) {
      OptimizationCheckpoint cp_variation(this, false);
      TryRuleVariations(run->first, run->second, max_recursion-1);
    }
  VLOG(1) << "::TryAddFirings Added variant rules " 
	  << " ln_likelihood_=" << model_->GetLnLikelihood() << endl;
}
  
void Optimizer::TryMakeFunctionalNegativeRule(Rule *r){
  // TODO: maybe play with the delay.
  Pattern precondition 
    = Concat(r->GetPrecondition()->GetPattern(), r->GetResult());
  if (model_->FindNegativeRule(precondition, r)) return;
  Rule * negative_rule = 
    model_->MakeNewRule(precondition, 
		   EncodedNumber(),
		   NEGATIVE_RULE, r,
		   vector<Tuple>(),
		   EncodedNumber(),
		   EncodedNumber());
  // negative_rule->ExplainEncoding();
  model_->FixTimes();
  VLOG(1) 
    << "added negative " << " lnlikelihood=" << model_->GetLnLikelihood() << endl;
  // go back and forth and optimize the rule weights.   
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
    TryAddImplicationRule(cr.first, cr.second, max_recursion-1);
    if (cp.KeepChanges()) break;
  }
}

void Optimizer::TryAddImplicationRule(
			   const Pattern & preconditions,
			   const Pattern & result,
			   int max_recursion){
  vector<Substitution> subs;
  vector<Tuple> combined = preconditions;
  combined.insert(combined.end(), result.begin(), result.end());
  model_->GetTupleIndex()->FindSatisfactions(combined, &subs, 0, UNLIMITED_WORK, 0);
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
  VLOG(1) << "old ln_likelihood_=" << model_->GetLnLikelihood() << endl;
  Rule * r = model_->MakeNewRule(preconditions, EncodedNumber(), 
			    type, 0, result, EncodedNumber(), EncodedNumber());
  // r->ExplainEncoding();
  VLOG(1) << "new ln_likelihood_=" << model_->GetLnLikelihood() << endl;
  TryAddFirings(r, subs, max_recursion-1);
  if (!r->Exists()) return;
  VLOG(1) << "with firings: " << model_->GetLnLikelihood() << endl;
  OptimizeStrength(r);
  VLOG(1) << "optimized strength: " << model_->GetLnLikelihood() << endl;
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
      VLOG(1) << "::~OptimizationCheckpoint reverting ln_likelihood_="
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
  VLOG(1) << "::FixTimesFixCircularDependencies" 
	  << " start ln_likelihood_=" << model_->GetLnLikelihood() << endl;
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
  VLOG(1) << "::FixTimesFixCircularDependencies" 
	  << " end ln_likelihood_=" << model_->GetLnLikelihood() << endl;
  //if (absent_required_.size()){
  //  ToHTML("html");
  //  CHECK(absent_required_.size()==0);
  // }
}

/*void ExplainEncoding(Rule *r){
  forall(run, r->encoding_) 
    Explain(r->model_, *run, &r->model_->never_happen_, false);
    }*/


