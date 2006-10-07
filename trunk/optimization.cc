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

void OptimizeRound(Model *m){
  vector<int> creative_rules;
  forall(run, rule_index_) if (run->second->type_==CREATIVE_RULE) 
    creative_rules.push_back(run->second->id_);
  forall(run, creative_rules){
    // ToDot("ALL","model.dot");
    Rule * r = GetComponent<Rule>(*run);
    if (r) TrySpecifyCreativeRule(m, r, REQUIRE_BETTER, true);
  }
}

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
    return MaybeFindRandomVariantRule(m, ret, tactic);
    break;
  }
}
TrueProposition * GetRandomTrueProposition(Model *m){
  return m->index_to_true_proposition_[tuple_index_.RandomTuple()];  
}


bool MaybeFindRandomVariantRule(Model *m, CandidateRule *ret, Tactic tactic){
  int64 max_work = (uint)(100/pow(RandomFraction(), 1.0));
  TrueProposition * tp = GetRandomTrueProposition(m);
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

bool MaybeFindRandomNewRule(Model *m, CandidateRule *ret){
  int64 max_work = (uint)(10/pow(RandomFraction(), 1.0));
  uint num_clauses = 1;
  while (RandomFraction() < 0.7) num_clauses++;  
  vector<Tuple> p;
  Substitution sub;
  int next_var = 0;
  set<const Tuple *> used_tuples;
  const Tuple * s1 = m->tuple_index_.RandomTuple();
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
      = m->tuple_index_.GetRandomTupleContaining(v_anchors, true);
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
    m->tuple_index_.FindSatisfactions(p, &subs, &num_satisfactions,
					 max_work, 0);
  if (max_work >=0 && num_satisfactions > (uint64)max_work) success = false;
  if (!success) return false;
  if (num_satisfactions < 2) return false;
  
  // check that the preconditions aren't too much work to searh for.
  vector<Tuple> preconditions = r.first;
  uint64 preconditions_num_satisfactions = 0;
  bool preconditions_success = 
    m->tuple_index_.FindSatisfactions(preconditions, 0, 
				      &preconditions_num_satisfactions, 
				      max_work, 0);
if (max_work>=0 && preconditions_num_satisfactions > (uint64)max_work) 
    preconditions_success = false;
  if (!preconditions_success) return false;
  for (uint i=0; i<preconditions.size(); i++) {
    vector<Tuple> simplified_preconditions 
      = RemoveFromVector(preconditions, i);
    uint64 simplified_num_satisfactions = 0;
    if (m->tuple_index_.FindSatisfactions(simplified_preconditions, 0, 
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
  to_expand.insert(make_pair(dependent->time_, dependent));
  while (to_expand.size()) {
    if (max_work != -1 && total_work >= max_work) return RESULT_GAVE_UP;
    multimap<Time, Component *>::iterator last = to_expand.end(); last--;
    Component * c = last->second;//to_expand.rbegin()->second;
    to_expand.erase(last);
    // multimap<Time, Component*>::iterator(to_expand.rbegin()));
    vector<vector<Component *> > codependents = c->Codependents();
    total_work += codependents.size();
    for (uint i=0; i<codependents.size(); i++) if (codependents[i].size()==1) {
      total_work += codependents[i]
.size();
      if (codependents[i][0]==dependee) return RESULT_TRUE;
      if (!(codependents[i][0]->time_ < dependee->time_)) 
	to_expand.insert(make_pair(codependents[i][0]->time_, 
				   codependents[i][0])); 
    }
  }
  return RESULT_FALSE;
}
bool Model::TryRemoveRule(Rule *r, RollbackCriterion criterion, 
			  bool fix_times){
  OptimizationCheckpoint cp(this, criterion, fix_times);
  KillComponent(r);
  return cp.KeepChanges();
}
bool Model::TryRemoveFiring(Firing *f, RollbackCriterion criterion,
			    bool fix_times){
  // if (f->IsEssential(10, 0) == RESULT_TRUE) return false;
  OptimizationCheckpoint cp(this, criterion, fix_times);
  Rule * r = f->rule_sat_->rule_;
  KillComponent(f);
  r->OptimizeStrength();
  if (!r->HasFiring()) TryRemoveRule(r, criterion, fix_times);
  return cp.KeepChanges();
}
bool Model::TryAddFirings(Rule * rule, const vector<Substitution> & subs,
			  RollbackCriterion criterion, bool fix_times){
  StablePtr<Rule> r(rule);
  OptimizationCheckpoint cp(this, criterion, fix_times);
  map<StablePtr<Rule>, set<StablePtr<Firing> > > to_remove;
  for (uint snum=0; snum<subs.size(); snum++) {
    if (!r.IsValid()) break;
    const Substitution & sub = subs[snum];
    // we're going to try to check whether any of the preconditions of this
    // firing depend on its results.  If so, skip it. 
    set<Component *> codependents; 
    set<TrueProposition *> dependents;
    vector<Tuple> preconditions = (*r)->precondition_->clauses_;
    sub.Substitute(&preconditions);
    bool preconditions_found = true;
    for (uint i=0; i<preconditions.size(); i++) {
      TrueProposition * tp = FindTrueProposition(preconditions[i]);
      if (!tp) {
	preconditions_found = false;
	break;
      }
      codependents.insert(tp);
    }
    if (!preconditions_found) break;
    codependents.insert(*r);
    vector<Tuple> results = (*r)->result_;
    sub.Substitute(&results);
    for (uint i=0; i<results.size(); i++) {
      TrueProposition * tp = FindTrueProposition(results[i]);
      if (tp) dependents.insert(tp);
    }
    set<TrueProposition *> dependents_explained = dependents;
    /*forall(run_d, dependents) {
      Component * d = *run_d;
      bool depended_on = false;
      forall(run_c, codependents) {
	if (DependsOn(*run_c, d, 100) == RESULT_TRUE) depended_on = true;
      }
      if (!depended_on) dependents_explained.insert(*run_d);
    }
    if (dependents_explained.size()==0) continue;*/
    Firing * f = (*r)->GetAddFiring(sub);
    VLOG(1) << "Added firing " << sub.ToString() << endl;
    CHECK(f);
    forall(run, dependents_explained) {
      TrueProposition * tp = *run;
      forall (run_f, tp->causes_) {
	Firing * other_firing = *run_f;
	if (other_firing == NULL) continue;
	if (f==other_firing) continue;
	to_remove[StablePtr<Rule>(other_firing->GetRule())]
	  .insert(StablePtr<Firing>(other_firing));
      }
    }
  }
  (*r)->OptimizeStrength();
  to_remove.erase(r);
  forall (run_r, to_remove) {
    StablePtr<Rule> alt_r = run_r->first;
    const set<StablePtr<Firing> > & firings = run_r->second;
    bool is_creative = (*alt_r)->type_ == CREATIVE_RULE;
    if (!alt_r.IsValid()) continue;
    bool may_want_negative_rule = false;
    bool may_want_duplicate_explanations = false;
    // let's see how many of the first firings for this rule can be cut
    int num_satisfactions = (*alt_r)->precondition_->num_satisfactions_;
    int num_first_firings = (*alt_r)->NumFirstFirings();
    int new_num_first_firings = num_first_firings;
    map<StablePtr<RuleSat>, vector<StablePtr<Firing> > > by_rule_sat;
    forall(run, firings) {
	by_rule_sat[StablePtr<RuleSat>((**run)->rule_sat_)].push_back(*run);
    }
    forall(run, by_rule_sat) {
      if ((*run->first)->firings_.size() == run->second.size())
	new_num_first_firings--;
    }
    CHECK(new_num_first_firings >= 0);
    if (is_creative) {
      if (new_num_first_firings < num_first_firings &&
	  new_num_first_firings > 0) {
	  may_want_negative_rule = true;
      }	
    } else {
      if (new_num_first_firings > 0) {
	may_want_negative_rule = true;
      }
      if (new_num_first_firings > num_satisfactions - num_first_firings) {
	may_want_duplicate_explanations = true;
      }
    }
    VLOG(1) << "alt_r_id=" << (*alt_r)->id_
	    << " is_creative=" << (is_creative?"t":"f")
	    << " num_sat=" << num_satisfactions
	    << " num_nff=" << num_first_firings
	    << " new_nff=" << new_num_first_firings
	    << endl;
    // this cp automatically gives the option of duplciate explanations
    OptimizationCheckpoint rule_cp(this, REQUIRE_BETTER, false);
    

    forall(run, firings){
      if (run->IsValid()) {
	KillComponent(**run);
	//bool firing_removed = TryRemoveFiring(**run, REQUIRE_VALID, false);
	  //VLOG(1) << "remove firing " << (firing_removed?"success":"fail") 
	  //	  << endl;
      }	
    }
    if (!alt_r.IsValid()) continue;
    if (!(*alt_r)->HasFiring()) {
	TryRemoveRule(*alt_r, REQUIRE_BETTER, false);
	continue;
    }
    (*alt_r)->OptimizeStrength();
    if (may_want_negative_rule) {
      OptimizationCheckpoint cp_negative_rule(this, REQUIRE_BETTER, true);
      // make sure r has a smaller deay than alt_r
      if (!((*alt_r)->delay_ < (*r)->delay_)) {
	EncodedNumber new_delay = (*r)->delay_;
	new_delay.bits_.push_back(false);
	(*r)->ChangeDelay(new_delay);
      }
      Rule * negative_rule = 
	new Rule(new Precondition(this,
				  Concat((*alt_r)->precondition_->clauses_, 
					   (*alt_r)->result_)),
		 EncodedNumber(),
		 NEGATIVE_RULE, *alt_r,
		 vector<Tuple>(),
		 EncodedNumber(),
		 EncodedNumber(),
		 false,
		 true);
      negative_rule->ExplainEncoding();
      FixTimes();
      cerr << "added negative " << " lnlikelihood=" << ln_likelihood_ << endl;
      for (int rep=0; rep<2; rep++) { // TODO: optimize it all together
	negative_rule->OptimizeStrength();
	cerr << "opt_neg " << " lnlikelihood=" << ln_likelihood_ 
	     << "  val=" << negative_rule->strength_d_ << endl;
	(*alt_r)->OptimizeStrength();
	cerr << "opt_alt " << " lnlikelihood=" << ln_likelihood_ 
	     << "  val=" << (*alt_r)->strength_d_ << endl;
      }
      if (GetVerbosity() >= 1) ToHTML("html");
    }
  }
  VLOG(1) << "removed alternate explanations " 
    //<< sub.ToString() 
	  << " new ln_likelihood_="
	  << ln_likelihood_ << endl;
  return cp.KeepChanges();
}

bool Model::TryAddImplicationRule(const vector<Tuple> & preconditions,
				  const vector<Tuple> & result,
				  RollbackCriterion criterion, 
				  bool fix_times) {
  OptimizationCheckpoint cp(this, criterion, fix_times);
  cerr << "TryAddImplicationRule cp=" << cp.cp_ << endl;
  vector<Substitution> subs;
  vector<Tuple> combined = preconditions;
  combined.insert(combined.end(), result.begin(), result.end());
  tuple_index_.FindSatisfactions(combined, &subs, 0, -1, 0);
  set<int>precondition_vars = GetVariables(preconditions);
  set<int> result_vars = GetVariables(result);
  result_vars = result_vars-precondition_vars;
  RuleType type = result_vars.size()?CREATIVE_RULE:SIMPLE_RULE;
  VLOG(0) << "Contemplating creating rule " 
	  << TupleVectorToString(preconditions)
	  << " ->" << TupleVectorToString(result) << endl;
  if (rule_index_ % RuleFingerprint(type, preconditions, 
				    result, vector<Tuple>())) {
    VLOG(2) << "rule already exists" << endl;
    return false;    
  }
  VLOG(2) << "old ln_likelihood_=" << ln_likelihood_ << endl;
  StablePtr<Rule> r(new Rule(GetAddPrecondition(preconditions), EncodedNumber(),
			     type, 0, result, EncodedNumber(), EncodedNumber(), 
			     false, false));
  (*r)->ExplainEncoding();
  VLOG(2) << "new ln_likelihood_=" << ln_likelihood_ << endl;

  TryAddFirings(*r, subs, criterion, fix_times);
  if (!r.IsValid()) return cp.KeepChanges();
  VLOG(2) << "with firings: " << ln_likelihood_ << endl;
  (*r)->OptimizeStrength();
  VLOG(2) << "optimized strength: " << ln_likelihood_ << endl;
  if (cp.KeepChanges()) {
    VLOG(0) << " Created rule "
	    << TupleVectorToString(preconditions)
	    << " ->" << TupleVectorToString(result)
	    << " model likelihood: " << ln_likelihood_
	    << " gain=" << cp.Gain() << endl;
  }
  // ToHTML("html");
  return cp.KeepChanges();
}
bool Model::TrySpecifyCreativeRule(Rule *r, RollbackCriterion criterion, 
				   bool fix_times){
  OptimizationCheckpoint cp(this, criterion, fix_times);
  CHECK(r->type_ == CREATIVE_RULE);
  map<pair<int, int>, 
    vector<StablePtr<Firing> > >m; // maps variable,value->firing
  forall(rs_iter, r->rule_sats_) {
    forall(f_iter, rs_iter->second->firings_) {
      Firing * f = f_iter->second;
      forall(s_iter, f->right_substitution_.sub_){
	m[*s_iter].push_back(StablePtr<Firing>(f));
      }
    }
  }
  forall(run, m){
    int var = run->first.first;
    int value = run->first.second;
    const vector<StablePtr<Firing> > & firings = run->second;
    if (firings.size() >= 3) {
      OptimizationCheckpoint cp2(this, REQUIRE_BETTER, false);
      vector<Tuple> new_result = r->result_;
      Substitution additional_sub;
      additional_sub.Add(var, value);
      additional_sub.Substitute(&new_result);
      Rule * nr = new Rule(r->precondition_, r->delay_, CREATIVE_RULE, 
			   0, new_result, r->strength_, r->strength2_,
			   false, false);
      nr->ExplainEncoding();
      forall(run_f, firings){
	StablePtr<Firing> f = *run_f;
	if (!f.IsValid()) continue;
	Substitution right_sub = (*f)->right_substitution_;
	right_sub.sub_.erase(var);
	new Firing
	  (nr->GetAddRuleSat((*f)->rule_sat_->satisfaction_->substitution_),
	   right_sub, false);
	KillComponent(*f);
      }
      nr->OptimizeStrength();
      r->OptimizeStrength();
      if (r->rule_sats_.size()==0) {
	KillComponent(r);
	break;
      }
    }
  }  
  return cp.KeepChanges();
}

ComputationResult RequiresCodependent(Component *dependent, 
				      Component *codependent){
  ComputationResult ret = RESULT_FALSE;
  vector<vector<Component *> > codep = dependent->Codependents();
  forall(run_first, codep) 
    forall(run_second, *run_first) {
    if (*run_second==codependent) {
      if (run_first->size()==1) return RESULT_TRUE;
      else {
	forall(run_third, (*run_first)) if (run_third != run_second) {
	  if ((*run_third)->time_ < (*run_second)->time_ ||
	      ((*run_third)->time_ == (*run_second)->time_ && run_third<run_second)) 
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
  if (c->Type() == TRUEPROPOSITION && ((TrueProposition*)c)->IsRequired())
    return RESULT_TRUE;
  int so_far = 1;
  if (actual_work) *actual_work = so_far;
  if (so_far > max_work) return RESULT_GAVE_UP;

  vector<Component *> dependents = c->Dependents();
  bool cant_determine = false;
  forall(run, dependents){
    Component * d = *run;
    int additional_work = 0;
    ComputationResult required = d->RequiresCodependent(this);
    if (required == RESULT_FALSE) continue;
    ComputationResult res = (*run)->IsEssential(max_work-so_far, 
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
					       RollbackCriterion c,
					       bool fix_times) {
  model_  = model;
  criterion_ = c;
  fix_times_ = fix_times;
  cp_ = model->MakeCheckpoint();
  old_ln_likelihood_ = model_->ln_likelihood_;
}
OptimizationCheckpoint::~OptimizationCheckpoint() {
  if (!KeepChanges()) model_->Rollback(cp_);
}
bool OptimizationCheckpoint::Better() {
  return (model_->Legal() 
	  && (model_->ln_likelihood_ > old_ln_likelihood_ + 
	      (1+fabs(model_->ln_likelihood_)) * 1e-14));
}
bool OptimizationCheckpoint::KeepChanges() {
  if (criterion_ == REQUIRE_BETTER && !Better()) return false;
  if (!model_->Legal()) return false;
  if (fix_times_) {
    model_->FixTimesFixCircularDependencies();
    model_->DeleteNeverHappeningComponents();
  }
  if (criterion_ == REQUIRE_BETTER) return Better();
  else return model_->Legal();
}
double OptimizationCheckpoint::Gain() {
  return model_->ln_likelihood_ - old_ln_likelihood_;
}

void OptimizeStrength(Rule *r){
  EncodedNumber strength = r->strength_;
  EncodedNumber strength2 = r->strength2_;
  for (int which=0; which<2; which++) {
    EncodedNumber * to_alter = &strength;
    if (which==1) {
      if (type_ != CREATIVE_RULE) continue;
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
		<< " ln_likelihood_=" << model_->ln_likelihood_ << endl;
	if (trial==0) { // drop last bit
	  if (to_alter->bits_.size()==0) continue;
	  to_alter->bits_.pop_back();	  
	} else {
	  to_alter->bits_.push_back(trial==2);
	}
	OptimizationCheckpoint cp(r->model_, REQUIRE_BETTER, false);
	r->ChangeStrength(strength, strength2);
	VLOG(2) << "   new_val=" << to_alter->ToSortableString()
		<< " new_ln_likelihood=" << model_->ln_likelihood_ << endl;
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

void Explain(Model *m, TrueProposition *p, 
	     set<Component *> *excluded, bool fix_times) {
  // CHECK(p->causes_.size()==0);
  forall(run, p->causes_) {
    CHECK(excluded && (*excluded % (Component *)(*run)));
  }
  vector<pair<Rule *, Substitution> > explanations;
  Tuple s = p->proposition_;
  FindExplanationsForResult(m, s, &explanations, excluded, -1);
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
    Checkpoint cp = m->MakeCheckpoint();
    explanations[i].first->GetAddFiring(explanations[i].second);
    if (i==0 || ln_likelihood_ > best) {
      which=i;
      best = ln_likelihood_;
    }
    m->Rollback(cp);
  }
  explanations[which].first->GetAddFiring(explanations[which].second);
}
void Model::FixTimesFixCircularDependencies() {
  // TODO: make this smarter.  much smarter
  VLOG(1) << "Entered FixTimesFixCircularDependencies" << endl;
  while (times_dirty_.size() || required_never_happen_.size()) {
    FixTimes();
    if (required_never_happen_.size()) {
      VLOG(1) << "  explaining a required proposition" << endl;
      TrueProposition * p 
	= (TrueProposition *)(*required_never_happen_.begin());
      Explain(p, &never_happen_, false);
      VLOG(1) << "   explained " << p->id_ << endl;
      if (GetVerbosity() >= 1) ToHTML("html");
    }
  }
  DeleteNeverHappeningComponents();
  CHECK(required_never_happen_.size() == 0);
  //if (absent_required_.size()){
  //  ToHTML("html");
  //  CHECK(absent_required_.size()==0);
  // }
}

int64 FindExplanationsForResult(Model *m, const Tuple & s, 
				vector<pair<Rule *, Substitution> > *results, 
				set<Component *> *excluded_dependents,
				int64 max_work){
  CHECK(results);
  int64 total_work = 0;
  for (GeneralizationIterator run_g(s); !run_g.done(); ++run_g) {
    const set<pair<Rule *, int> > * rules
      = clause_to_result_ % run_g.generalized().Fingerprint();
    if (rules) forall(run, (*rules)) {
      Rule * rule = run->first;
      if (excluded_dependents && ((*excluded_dependents) % ((Component*)rule))) continue;
      int clause_num = run->second;
      Substitution partial_sub;
      vector<Tuple> simplified_precondition = rule->precondition_->clauses_;
      
      if (!ComputeSubstitution(rule->result_[clause_num], s, &partial_sub))
	continue;
      partial_sub.Substitute(&simplified_precondition);
      uint64 num_complete_subs;
      uint64 work = 0;
      vector<Substitution> complete_subs;
      if (!tuple_index_.FindSatisfactions
	  (simplified_precondition, 
	   &complete_subs,
	   &num_complete_subs,
	   (max_work==-1)?-1:max_work-total_work,
	   &work)) return -1;
      if (num_complete_subs == 0) continue;
      set<int> result_vars = GetVariables(rule->result_);
      for (uint i=0; i<complete_subs.size(); i++) {
	complete_subs[i].Add(partial_sub);
	forall(run_v, result_vars) {
	  if (complete_subs[i].Lookup(*run_v)==*run_v) {
	    complete_subs[i].Add(*run_v, LEXICON.GetAddID("whatever"));
	  }
	}
	vector<Tuple> substituted_precondition = rule->precondition_->clauses_;
	complete_subs[i].Substitute(&substituted_precondition);
	if (excluded_dependents) {
	  bool bad = false;
	  forall(run, substituted_precondition) {
	    TrueProposition *prop = FindTrueProposition(*run);
	    CHECK(prop);
	    if ((*excluded_dependents) % ((Component*)prop)) bad = true;
	  }
	  if (bad) continue;
	}
	results->push_back(make_pair(rule, complete_subs[i]));
      }
      total_work += work;
    }
  }
  return total_work;
}

void FulfillRequirements(Model *m) {
  forall(run, m->required_){
    Tuple s = run->second;
    Explain(m, m->GetAddTrueProposition(s), 0, true);
  }
  m->FixTimes();
}

void ExplainEncoding(Rule *r){
  forall(run, r->encoding_) 
    Explain(r->model_, *run, &r->model_->never_happen_, false);
}


