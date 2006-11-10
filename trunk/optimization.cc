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

/*void OptimizeRound(Model *m){
  vector<int> creative_rules;
  set<Rule *> rules = m->GetAllRules();
  forall(run, rules) {
    Rule * r = *run;
    if (r->Exists() && 
	r->GetRuleType()==CREATIVE_RULE) {
      OptimizationCheckpoint cp(m, true);
      TrySpecifyCreativeRule(m, r);
    }
  }
  }*/

pair<vector<Tuple>, vector<Tuple> >
FindRandomCandidateRule(Model *m, Tactic tactic){
  pair<vector<Tuple>, vector<Tuple> > ret;
  while (!MaybeFindRandomCandidateRule(m, &ret, tactic));
  return ret;
}
bool MaybeFindRandomCandidateRule(Model *m, CandidateRule *ret, 
				  Tactic tactic){
  switch (tactic) {
  case NEW_RULE:
    return MaybeFindRandomNewRule(m, ret);
    break;
  default:
    CHECK(false);
    //return MaybeFindRandomVariantRule(m, ret, tactic);
    break;
  }
  return false;
}
TrueTuple * GetRandomTrueTuple(Model *m){
  TrueTuple *ret = m->FindTrueTuple(*(m->GetTupleIndex()->RandomTuple()));
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

bool MaybeFindRandomNewRule(Model *m, CandidateRule *ret){
  int64 max_work = (uint)(10/pow(RandomFraction(), 1.0));
  uint num_clauses = 1;
  while (RandomFraction() < 0.7) num_clauses++;  
  vector<Tuple> p;
  Substitution sub;
  int next_var = 0;
  set<const Tuple *> used_tuples;
  const Tuple * s1 = m->GetTupleIndex()->RandomTuple();
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
      = m->GetTupleIndex()->GetRandomTupleContaining(v_anchors, true);
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
  return VetteCandidateRule(m, r, ret, max_work);
}

bool VetteCandidateRule(Model *m, CandidateRule r, 
			CandidateRule * simplified_rule, 
			int64 max_work) {
  vector<Tuple> p = Concat(r);
  uint64 num_satisfactions;
  vector<Substitution> subs;
  bool success = 
    m->GetTupleIndex()->FindSatisfactions(p, &subs, &num_satisfactions,
					  max_work, 0);
  if (max_work >=0 && num_satisfactions > (uint64)max_work) success = false;
  if (!success) return false;
  if (num_satisfactions < 2) return false;
  
  // check that the preconditions aren't too much work to searh for.
  vector<Tuple> preconditions = r.first;
  uint64 preconditions_num_satisfactions = 0;
  bool preconditions_success = 
    m->GetTupleIndex()->FindSatisfactions(preconditions, 0, 
					  &preconditions_num_satisfactions, 
					  max_work, 0);
  if (max_work>=0 && preconditions_num_satisfactions > (uint64)max_work) 
    preconditions_success = false;
  if (!preconditions_success) return false;
  for (uint i=0; i<preconditions.size(); i++) {
    vector<Tuple> simplified_preconditions 
      = RemoveFromVector(preconditions, i);
    uint64 simplified_num_satisfactions = 0;
    if (m->GetTupleIndex()->FindSatisfactions(simplified_preconditions, 0, 
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
  *simplified_rule = CanonicalizeRule(r);
  return true;
} 

ComputationResult DependsOn(Model *m, Component * dependent, 
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
void TryRemoveFiring(Firing *f){
  // if (f->IsEssential(10, 0) == RESULT_TRUE) return false;
  Model *m = f->GetModel();
  Rule * r = f->GetRule();
  f->Erase();
  OptimizeStrength(r);
  if (!r->HasFiring()) {
    OptimizationCheckpoint cp(m, false);
    r->Erase();
  }
}

void TryAddFirings(Rule * rule, const vector<Substitution> & subs){
  Model *m = rule->GetModel();
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
	  << " ln_likelihood_=" << m->GetLnLikelihood() << endl;
  
  forall (run_r, to_remove) {
    Rule * alt_r = run_r->first;
    const set<Firing *> & firings = run_r->second;
    bool is_creative = alt_r->GetRuleType() == CREATIVE_RULE;
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

    // this cp automatically gives the option of leaving in all of the 
    // firings for this alternate rule (duplciate explanations)
    OptimizationCheckpoint rule_cp(m, false);
    rule_cp.logging_ = true;

    forall(run, firings) (*run)->Erase();
    OptimizeStrength(alt_r);
    VLOG(1) << "::TryAddFirings Removed " << firings.size() 
	    << " firings for rule " << alt_r->GetID()
	    << " ln_likelihood_=" << m->GetLnLikelihood() << endl;
    
    
    // Try to remove the alternate rule if it has few firings.
    vector<Firing *> remaining_firings = alt_r->Firings();
    // if the remaining firings are at most half of what we just deleted... 
    // (pretty arbitrary) try removing the rule.
    if (remaining_firings.size() < firings.size() * 0.5) {
      OptimizationCheckpoint delete_rule_cp(m, false);
      delete_rule_cp.logging_ = true;
      // figure out which TrueTuples we need to find alternate explanations for 
      set<TrueTuple*> to_explain;
      for (uint i=0; i<remaining_firings.size(); i++) {
	to_explain.insert(remaining_firings[i]->GetTrueTuples().begin(), 
			  remaining_firings[i]->GetTrueTuples().end());
      }
      alt_r->Erase();
      if (GetVerbosity() >= 1) m->VerifyLayer2();
      forall(run, to_explain) {
	if ((*run)->GetCauses().size() == 0) 
	  Explain(*run, NULL, false);
      }
      VLOG(1) << "::TryAddFirings Erased Rule " 
	      << " ln_likelihood_=" << m->GetLnLikelihood() << endl;
    }
    if (!alt_r->Exists()) continue;
    if (may_want_to_add_negative_rule) {
      OptimizationCheckpoint cp_negative_rule(m, true);
      cp_negative_rule.logging_ = true;
      // make sure r has a smaller delay than alt_r
      if (!(rule->GetDelay() < alt_r->GetDelay())) {
	EncodedNumber new_delay = alt_r->GetDelay();
	new_delay.bits_.push_back(false);
	rule->ChangeDelay(new_delay);
      }
      TryMakeFunctionalNegativeRule(alt_r);
      VLOG(1) << "::TryAddFirings Made a negative rule. "
	      << " ln_likelihood_=" << m->GetLnLikelihood() << endl;      
    }
  }
  VLOG(1) << "::TryAddFirings removed all alternate explanations " 
	  << " ln_likelihood_=" << m->GetLnLikelihood() << endl;
}

void TryMakeFunctionalNegativeRule(Rule *r){
  // TODO: maybe play with the delay.
  Model *m = r->GetModel();
  Pattern precondition 
    = Concat(r->GetPrecondition()->GetPattern(), r->GetResult());
  if (m->FindNegativeRule(precondition, r)) return;
  Rule * negative_rule = 
    m->MakeNewRule(precondition, 
		   EncodedNumber(),
		   NEGATIVE_RULE, r,
		   vector<Tuple>(),
		   EncodedNumber(),
		   EncodedNumber());
  // negative_rule->ExplainEncoding();
  m->FixTimes();
  VLOG(1) 
    << "added negative " << " lnlikelihood=" << m->GetLnLikelihood() << endl;
  // go back and forth and optimize the rule weights.   
  // TODO: optimize it all together?
  for (int rep=0; rep<2; rep++) { 
    OptimizeStrength(negative_rule);
    VLOG(1) << "opt_neg " << " lnlikelihood=" << m->GetLnLikelihood() 
	    << "  val=" << negative_rule->GetStrengthD() << endl;
    OptimizeStrength(r);
    VLOG(1) << "opt_alt " << " lnlikelihood=" << m->GetLnLikelihood() 
	    << "  val=" << r->GetStrengthD() << endl;
  }
  if (GetVerbosity() >= 1) m->ToHTML("html");
}

void TryAddImplicationRule(Model *m, 
			   const vector<Tuple> & preconditions,
			   const vector<Tuple> & result){
  vector<Substitution> subs;
  vector<Tuple> combined = preconditions;
  combined.insert(combined.end(), result.begin(), result.end());
  m->GetTupleIndex()->FindSatisfactions(combined, &subs, 0, UNLIMITED_WORK, 0);
  set<int> precondition_vars = GetVariables(preconditions);
  set<int> result_vars = GetVariables(result);
  result_vars = result_vars-precondition_vars;
  RuleType type = result_vars.size()?CREATIVE_RULE:SIMPLE_RULE;
  VLOG(0) << "Contemplating creating rule " 
	  << TupleVectorToString(preconditions)
	  << " ->" << TupleVectorToString(result) << endl;
  if (m->FindPositiveRule(preconditions, result)) {
    VLOG(1) << "rule already exists" << endl;
    return;
  }
  VLOG(1) << "old ln_likelihood_=" << m->GetLnLikelihood() << endl;
  Rule * r = m->MakeNewRule(preconditions, EncodedNumber(), 
			    type, 0, result, EncodedNumber(), EncodedNumber());
  // r->ExplainEncoding();
  VLOG(1) << "new ln_likelihood_=" << m->GetLnLikelihood() << endl;
  TryAddFirings(r, subs);
  if (!r->Exists()) return;
  VLOG(1) << "with firings: " << m->GetLnLikelihood() << endl;
  OptimizeStrength(r);
  VLOG(1) << "optimized strength: " << m->GetLnLikelihood() << endl;
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


OptimizationCheckpoint::OptimizationCheckpoint(Model *model,
					       bool fix_times) {
  model_  = model;
  fix_times_ = fix_times;
  logging_ = false;
  cp_ = model->GetChangelist()->GetCheckpoint();
  old_ln_likelihood_ = model_->GetLnLikelihood();
}
OptimizationCheckpoint::~OptimizationCheckpoint() {
  if (!KeepChanges()) {    
    model_->GetChangelist()->Rollback(cp_);
    if (logging_)
      cerr << "::~OptimizationCheckpoint reverting ln_likelihood_="
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
  if (fix_times_) FixTimesFixCircularDependencies(model_);  
  return Better();
}
double OptimizationCheckpoint::Gain() {
  return model_->GetLnLikelihood() - old_ln_likelihood_;
}

void OptimizeStrength(Rule *r){
  // TODO, make this better and more principled
  Model * m =  r->GetModel();
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
		<< " ln_likelihood_=" << m->GetLnLikelihood() << endl;
	if (trial==0) { // drop last bit
	  if (to_alter->bits_.size()==0) continue;
	  to_alter->bits_.pop_back();	  
	} else {
	  to_alter->bits_.push_back(trial==2);
	}
	OptimizationCheckpoint cp(m, false);
	r->ChangeStrength(strength, strength2);
	VLOG(2) << "   new_val=" << to_alter->ToSortableString()
		<< " new_ln_likelihood=" << m->GetLnLikelihood() << endl;
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

void Explain(TrueTuple *p, 
	     const set<Component *> *excluded, bool fix_times) {
  Model *m = p->GetModel();
  // make sure that the TrueTuple has no causes that are not excluded.
  forall(run, p->GetCauses()) { 
    CHECK(excluded && (*excluded % (Component *)(*run)));
  }
  vector<pair<Rule *, Substitution> > explanations;
  Tuple s = p->GetTuple();
  m->FindExplanationsForResult(s, &explanations, excluded, UNLIMITED_WORK);
  if (explanations.size()==0) {
    Rule * r = m->GetAddNaiveRule(s.size());
    Substitution right_sub;
    for (int i=0; i<(int)s.size(); i++) {
      right_sub.Add(Variable(i), s[i]);
    }
    explanations.push_back(make_pair(r, right_sub));
  }
  int which = 0; double best = 0;
  for (uint i=0; i<explanations.size(); i++){
    Checkpoint cp = m->GetChangelist()->GetCheckpoint();
    explanations[i].first->AddFiring(explanations[i].second);
    OptimizeStrength(explanations[i].first);
    if (i==0 || m->GetLnLikelihood() > best) {
      which=i;
      best = m->GetLnLikelihood();
    }
    m->GetChangelist()->Rollback(cp);
  }
  explanations[which].first->AddFiring(explanations[which].second);
  OptimizeStrength(explanations[which].first);
}
void FixTimesFixCircularDependencies(Model *m) {
  // TODO: make this smarter.  much smarter
  VLOG(1) << "Entered FixTimesFixCircularDependencies" << endl;
  while (m->GetTimesDirty().size() || m->GetRequiredNeverHappen().size()) {
    m->FixTimes();
    if (m->GetRequiredNeverHappen().size()) {
      VLOG(1) << "  explaining a required proposition" << endl;
      TrueTuple * p 
	= (TrueTuple *)(*(m->GetRequiredNeverHappen().begin()));
      Explain(p, &m->GetNeverHappen(), false);
      VLOG(1) << "   explained " << p->GetID() << endl;
      if (GetVerbosity() >= 1) m->ToHTML("html");
    }
  }
  m->DeleteNeverHappeningComponents();
  CHECK(m->GetRequiredNeverHappen().size() == 0);
  //if (absent_required_.size()){
  //  ToHTML("html");
  //  CHECK(absent_required_.size()==0);
  // }
}

/*void ExplainEncoding(Rule *r){
  forall(run, r->encoding_) 
    Explain(r->model_, *run, &r->model_->never_happen_, false);
    }*/



