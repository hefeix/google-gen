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


// Contains member functions of Component classes
#include "model.h"
#include "probutil.h"
#include <sstream>
#include <fstream>
#include <math.h>
#include "changelist.h"
#include "component.h"
#include "prohibition.h"
#include "searchtree.h"

bool FLAGS_firing_tuple = false;

char * ComponentTypeName [] = {
    "PRECONDITION",
    "RULE",
    "SATISFACTION",
    "RULESAT",
    "FIRING",
    "TRUETUPLE",
};
ComponentType StringToComponentType(const string & s) {
  for (uint i=0; i<NUM_COMPONENT_TYPES; i++) {
    if (s==ComponentTypeName[i]) return (ComponentType)i;
  }
  // CHECK(false); 
  return NUM_COMPONENT_TYPES;
}
string ComponentTypeToString(ComponentType t) { 
  return ComponentTypeName[t]; 
}
char * RuleTypeName [] = {
  "INVALID_RULE",
  "SIMPLE_RULE",
  "NEGATIVE_RULE",
  "CREATIVE_RULE",
};
RuleType StringToRuleType(const string & s) {
  for (uint i=0; i<NUM_RULE_TYPES; i++) {
    if (s==RuleTypeName[i]) return (RuleType)i;
  }
  CHECK(false);
  return INVALID_RULE;
}
string RuleTypeToString(RuleType t) { return RuleTypeName[t]; }


// COMPONENT
Component::Component(Model * model){
  model_ = model;
  model_->changelist_.Creating(this);
  exists_ = false;
  really_dead_ = true;
  time_ = CREATION;
  time_dirty_ = true;
  model_->A1_InsertIntoTimesDirty(this);
  model_->L1_AssignNewID(this);
  ln_likelihood_ = 0.0;
  A1_SetExists(true);
  A1_SetReallyDead(false);
}
Component::~Component(){
  CHECK(exists_ == false);
}
void Component::A1_SetExists(bool val){
  CHECK(exists_ != val);
  model_->changelist_.Make(new ValueChange<bool>(&exists_, val));
}

void Component::A1_SetReallyDead(bool val){
  CHECK(really_dead_ != val);
  model_->changelist_.Make(new ValueChange<bool>(&really_dead_, val));
}


void Component::A1_SetTime(const Time & new_time){
  if (time_ == new_time) return;
  model_->changelist_.Make(new ValueChange<Time>(&time_, new_time));
}
void Component::A1_SetTimeDirty(bool new_val){
  CHECK(time_dirty_ != new_val);
  model_->changelist_.Make(new ValueChange<bool>(&time_dirty_, new_val));
}
void Component::A1_SetLnLikelihood(double new_ln_likelihood){
  if (new_ln_likelihood == ln_likelihood_) return;
  model_->changelist_.Make(new ValueChange<double>(&ln_likelihood_, new_ln_likelihood));
}
void Component::AddComments(string new_comments){
  model_->changelist_.Make(new ValueChange<string>(&comments_, 
						   comments_+new_comments));
}

void Component::Erase(){
  CHECK(Type() != RULESAT && Type() != SATISFACTION);
  L1_Erase();
}
void Component::L1_Erase(){
  CHECK(exists_);
  A1_SetExists(false);
  model_->A1_SetLnLikelihood(model_->ln_likelihood_ - ln_likelihood_);

  vector<Component *> dep = StructuralDependents();
  vector<Component *> copurposes = Copurposes();
  for (uint i=0; i<dep.size(); i++) {
    if (dep[i]->Exists()) dep[i]->L1_Erase();
  }

  model_->L1_ReleaseID(id_);
  if (time_ == NEVER) model_->A1_RemoveFromNeverHappen(this);
  if (time_dirty_) model_->A1_RemoveFromTimesDirty(this);
  model_->A1_RemoveFromComponentsByType(this);

  L1_EraseSubclass();
  A1_SetReallyDead(true);
  model_->changelist_.Destroying(this);

  for (uint i=0; i<copurposes.size(); i++) {
    if (copurposes[i]->Exists() && copurposes[i]->IsSuperfluous()) 
      copurposes[i]->L1_Erase();
  }
}


Precondition::Precondition(Model * model, 
			   const vector<Tuple> & tuples)
  : Component(model){
  pattern_ = tuples;
  ln_likelihood_per_sat_ = 0.0;
  num_satisfactions_ = 0;

  for (uint i=0; i<tuples.size(); i++) {
    model_->A1_InsertIntoWildcardTupleToPrecondition
      (tuples[i].VariablesToWildcards(),
       this,
       i);
  }

  vector<int> arbitrary_terms;
  direct_pattern_encoding_ln_likelihood_ = 
    PatternLnLikelihood(Pattern(), pattern_, &arbitrary_terms);  
  for (uint i=0; i<arbitrary_terms.size(); i++)
    model_->L1_AddArbitraryTerm(arbitrary_terms[i]);
  model_->A1_InsertIntoPreconditionIndex(pattern_, this);
  ComputeSetTime();
  num_satisfactions_ = 0;
  search_tree_ = new SearchTree(pattern_, NULL, this, SamplingInfo::Unsampled());
  search_tree_->L1_Search(NULL);
  ComputeSetLnLikelihood();
  model_->A1_InsertIntoComponentsByType(this);
}

void Precondition::L1_EraseSubclass(){
  model_->A1_RemoveFromPreconditionIndex(pattern_);

  for (uint i=0; i < pattern_.size(); i++) {
    model_->A1_RemoveFromWildcardTupleToPrecondition
      (pattern_[i].VariablesToWildcards(),
       this,
       i);
  }

  vector<int> arbitrary_terms;
  PatternLnLikelihood(Pattern(), pattern_, &arbitrary_terms);  
  for (uint i=0; i<arbitrary_terms.size(); i++)
    model_->L1_SubtractArbitraryTerm(arbitrary_terms[i]);
  search_tree_->L1_Erase();
}

/*
void Precondition::L1_MakeDirectlyEncoded(){
  vector<int> arbitrary_terms;
  A1_SetDirectTupleEncodingLnLikelihood
    (TuplesLnLikelihood(Pattern(), pattern_, &arbitrary_terms));
  for (uint i=0; i<arbitrary_terms.size(); i++)
    model_->L1_AddArbitraryWord(arbitrary_terms[i]);
  A1_SetTupleEncoded(false);
  ComputeSetLnLikelihood();
}
void Precondition::L1_MakeNotDirectlyEncoded(){
  vector<int> arbitrary_terms;
  TuplesLnLikelihood(Pattern(), pattern_, &arbitrary_terms);
  for (uint i=0; i<arbitrary_terms.size(); i++) {
    model_->L1_SubtractArbitraryWord(arbitrary_terms[i]);
  } 
}
void Precondition::L1_MakeTupleEncoded(){
  vector<Tuple> causes = ComputeCauses();
  for (uint i=0; i<causes.size(); i++) {
    TrueTuple *t = model_->L1_GetAddTrueTuple(causes[i]);
    
  }
}
void Precondition::L1_MakeNotTupleEncoded(){
}*/
void Precondition::A1_AddRule(Rule *r){
  model_->changelist_.Make(new SetInsertChange<Rule *>(&rules_, r));
}
void Precondition::A1_RemoveRule(Rule *r){
  model_->changelist_.Make(new SetRemoveChange<Rule *>(&rules_, r));
}
void Precondition::A1_AddNegativeRule(Rule *r){
  model_->changelist_.Make(new SetInsertChange<Rule *>(&negative_rules_, r));
}
void Precondition::A1_RemoveNegativeRule(Rule *r){
  model_->changelist_.Make(new SetRemoveChange<Rule *>(&negative_rules_, r));
}
void Precondition::A1_AddToNegativeRuleIndex(Rule * target_rule,
					     Rule * negative_rule) {
  model_->changelist_.Make(new MapOfSetsInsertChange<Rule*, Rule*>
			   (&negative_rule_index_,
			    target_rule,
			    negative_rule));
}
void Precondition::A1_RemoveFromNegativeRuleIndex(Rule * target_rule,
						  Rule * negative_rule) {
  model_->changelist_.Make(new MapOfSetsRemoveChange<Rule*, Rule*>
			   (&negative_rule_index_,
			    target_rule,
			    negative_rule));
}
void Precondition::A1_AddToPositiveRuleIndex(const Pattern & result,
					     Rule * rule) {
  model_->changelist_.Make(new MapOfSetsInsertChange<Pattern, Rule*>
			   (&positive_rule_index_,
			    result,
			    rule));
}
void Precondition::A1_RemoveFromPositiveRuleIndex(const Pattern & result,
						  Rule * rule) {
  model_->changelist_.Make(new MapOfSetsRemoveChange<Pattern, Rule*>
			   (&positive_rule_index_,
			    result,
			    rule));
}

void Precondition::A1_AddSatisfaction(Satisfaction * sat){
  model_->changelist_.
    Make(new MapInsertChange<Substitution, Satisfaction *>
	 (&satisfactions_, sat->substitution_, sat));
}
void Precondition::A1_RemoveSatisfaction(Satisfaction *sat){
  model_->changelist_.
    Make(new MapRemoveChange<Substitution, Satisfaction *>
	 (&satisfactions_, sat->substitution_));
}
void Precondition::A1_SetLnLikelihoodPerSat(double val){
  model_->changelist_.
    Make(new ValueChange<double>(&ln_likelihood_per_sat_,val));
}

void Precondition::A1_AddToNumSatisfactions(int delta){
  model_->changelist_.
    Make(new ValueChange<int>(&num_satisfactions_, num_satisfactions_+delta));
}
Satisfaction * Precondition::FindSatisfaction(const Substitution &sub) const{
  Satisfaction * const * sp = satisfactions_ % sub;
  if (sp) return *sp;
  else return NULL;
}
Rule * Precondition::FindPositiveRule(const vector<Tuple> & result) const {
  const set<Rule *> * s = positive_rule_index_ % result;
  if (!s) return NULL;
  CHECK(s->size() > 0);
  return *(s->begin());
}
Rule * Precondition::FindNegativeRule(Rule * target_rule) const{
  const set<Rule *> * s = negative_rule_index_ % target_rule;
  if (!s) return NULL;
  CHECK(s->size() > 0);
  return *(s->begin());
}

Satisfaction * Precondition::L1_GetAddSatisfaction(const Substitution & sub){
  Satisfaction * s = FindSatisfaction(sub);
  if (s) return s;
  return new Satisfaction(this, sub);
}


Satisfaction::Satisfaction(Precondition * precondition, 
			   const Substitution & sub)
  :Component(precondition->model_) {
  precondition_ = precondition;
  substitution_ = sub;
  
  // This loop checks that all of the variables in the precondition are
  // in the substitution.
  vector<Tuple> substituted_precondition = precondition->pattern_;
  sub.Substitute(&substituted_precondition);
  for (uint i=0; i<substituted_precondition.size(); i++) {
    CHECK(substituted_precondition[i].IsConstantTuple());
    TrueTuple * t = model_->FindTrueTuple(substituted_precondition[i]);
    CHECK(t);
    CHECK(t->Exists());
    // we need this if statement, since the substituted precondition can 
    // have a repeated clause.
    if (!(true_tuples_ % t)) { 
      A1_AddTrueTuple(t);
      t->A1_AddSatisfaction(this);
    }
  }
  precondition_->A1_AddSatisfaction(this);
  ComputeSetTime();
  model_->A1_InsertIntoComponentsByType(this);
}

void Satisfaction::L1_EraseSubclass() {
  precondition_->A1_RemoveSatisfaction(this);
  forall(run, true_tuples_) {    
    (*run)->A1_RemoveSatisfaction(this);
  }
}

void Satisfaction::A1_AddTrueTuple(TrueTuple *t){
  model_->changelist_.
    Make(new SetInsertChange<TrueTuple *>(&true_tuples_, t));
}
void Satisfaction::A1_RemoveTrueTueple(TrueTuple *t){
  model_->changelist_.
    Make(new SetRemoveChange<TrueTuple *>(&true_tuples_, t));
}
void Satisfaction::A1_AddRuleSat(RuleSat *rs){
  model_->changelist_.
    Make(new SetInsertChange<RuleSat *>(&rule_sats_, rs));
}
void Satisfaction::A1_RemoveRuleSat(RuleSat *rs){
  model_->changelist_.
    Make(new SetRemoveChange<RuleSat *>(&rule_sats_, rs));
}

Firing * Rule::FindFiring(const Substitution & sub) const {
  Substitution left_sub = sub.Restrict(LeftVariables());
  RuleSat * rs = FindRuleSat(left_sub);
  if (!rs) return NULL;
  return rs->FindFiring(sub.Restrict(RightVariables()));
}
Firing * Rule::AddFiring(const Substitution & sub) {
  RuleSat * rs = L1_GetAddRuleSat(sub.Restrict(LeftVariables()));
  CHECK(rs);
  Firing * ret = rs->AddFiring(sub.Restrict(RightVariables()));
  VLOG(2) << "left sub=" << sub.Restrict(LeftVariables()).ToString()
	  << "left sub=" << sub.Restrict(RightVariables()).ToString() << endl;
  return ret;
}

// Only works for simple rules
void Rule::AddAllSatisfactionsAsFirings() {

  CHECK(type_ == SIMPLE_RULE);

  // Get all the substitutions required
  vector<Substitution> subs;
  precondition_->search_tree_->GetSubstitutions(&subs);

  // Add firings for each substitution
  for (uint c=0; c<subs.size(); c++)
    AddFiring(subs[c]);
}

RuleSat * Rule::FindRuleSat(Satisfaction *sat) const{
  RuleSat * const * rsp = rule_sats_ % sat;
  if (rsp) return *rsp;  
  else return NULL;
}
RuleSat * Rule::L1_GetAddRuleSat(Satisfaction * sat) {
  RuleSat *rs = FindRuleSat(sat);
  if (rs) return rs;
  return new RuleSat(this, sat->substitution_);
}
RuleSat * Rule::FindRuleSat(const Substitution & sub) const {
  Satisfaction *sat = precondition_->FindSatisfaction(sub);
  if (!sat) return NULL;
  return FindRuleSat(sat);
}
RuleSat * Rule::L1_GetAddRuleSat(const Substitution & sub) {
  Satisfaction * sat = precondition_->L1_GetAddSatisfaction(sub);
  CHECK(sat);
  return L1_GetAddRuleSat(sat);
}

bool Rule::IsUniversalRule() const {
  if (precondition_->pattern_.size()) return false;
  if (result_.size() != 1) return false;
  for (uint i=0; i<result_[0].size(); i++) 
    if (result_[0][i] != Variable(i)) return false;
  return true;
}

Rule::Rule(Precondition * precondition, EncodedNumber delay, 
	   RuleType type, Rule * target_rule,
	   vector<Tuple> result, EncodedNumber strength,
	   EncodedNumber strength2)
  : Component(precondition->model_){
  precondition_ = precondition;
  delay_ = delay;
  type_ = type;
  result_ = result;
  target_rule_ = target_rule;
  strength_ = strength;
  strength_d_ = strength.ToOpenInterval();
  strength2_ = strength2;
  strength2_d_ = strength2.ToOpenInterval();
  precondition_->A1_AddRule(this);
  CHECK(!((type == NEGATIVE_RULE) == (target_rule_==NULL)));
  if (type == NEGATIVE_RULE) {
    precondition_->A1_AddNegativeRule(this);
    precondition_->A1_AddToNegativeRuleIndex(target_rule_, this);
    target_rule_->A1_AddInhibitor(this);
  } else {
    precondition_->A1_AddToPositiveRuleIndex(result_, this);
    precondition_->A1_SetLnLikelihoodPerSat
      (precondition_->ln_likelihood_per_sat_ + log(1-strength_d_));
    precondition_->ComputeSetLnLikelihood();
  }
  for (uint i=0; i<result_.size(); i++){
    model_->A1_InsertIntoWildcardTupleToResult
      (result_[i].VariablesToWildcards(), this, i);
  }
  
  // Look at all the subsets of tuples of the rule
  if (type_ != NEGATIVE_RULE) {
    Pattern combined = precondition_->pattern_;
    combined.insert(combined.end(), result_.begin(), result_.end());
    int max = 1 << combined.size();
    for (int mask=0; mask < max; mask++) {

      // Don't index subsets of one element or 0 elements
      if ((mask & (mask-1)) == 0) continue;
      SubRuleInfo sri;
      sri.rule_ = this;
      Pattern      subpattern, c_subpattern;
      for (uint c=0; c < combined.size(); c++) {
	if (mask & (1<<c)) {
	  subpattern.push_back(combined[c]);
	  if (c >= precondition_->pattern_.size())
	    sri.postcondition_ = true;
	}
      }
      if (IsConnectedPattern(subpattern)) {
	c_subpattern = Canonicalize(subpattern, &sri.sub_);
	model_->A1_InsertIntoSubrulePatternToRule(c_subpattern, sri);
      } else {
	VLOG(1) << "Disconnected Pattern " << TupleVectorToString(subpattern) << endl;
      }
    }
  }

  vector<int> arbitrary_terms;
  direct_pattern_encoding_ln_likelihood_ = 
    PatternLnLikelihood(precondition_->pattern_, result_, &arbitrary_terms);
  if (IsUniversalRule()) direct_pattern_encoding_ln_likelihood_ = 0;
  for (uint i=0; i<arbitrary_terms.size(); i++)
    model_->L1_AddArbitraryTerm(arbitrary_terms[i]);

  // TUPLE ENCODING STUFF
  //vector<Tuple> causes = ComputeCauses();
  //for (uint i=0; i<causes.size(); i++) {
  //  TrueTuple * p = model_->GetAddTrueTuple(encoding[i]);
  //  causing_tuples_.push_back(p);
  //  p->A1_AddToRulesCaused(this);
  // }

  ComputeSetTime();  
  if (type == NEGATIVE_RULE) {  // Make all the RuleSats exist.
    vector<Substitution> substitutions;
    precondition_->search_tree_->GetSubstitutions(&substitutions);
    for (uint i=0; i<substitutions.size(); i++) {
      new RuleSat(this, substitutions[i]);
    }
  }
  ComputeSetLnLikelihood();
  model_->A1_InsertIntoComponentsByType(this);
}
 
void Rule::L1_EraseSubclass(){  
  precondition_->A1_RemoveRule(this);
  if (type_ == NEGATIVE_RULE) {
    precondition_->A1_RemoveNegativeRule(this);
    precondition_->A1_RemoveFromNegativeRuleIndex(target_rule_, this);
    target_rule_->A1_RemoveInhibitor(this);
  } else {
    precondition_->A1_RemoveFromPositiveRuleIndex(result_, this);
    precondition_->A1_SetLnLikelihoodPerSat
      (precondition_->ln_likelihood_per_sat_ - log(1-strength_d_));
    precondition_->ComputeSetLnLikelihood();
    for (uint i=0; i<result_.size(); i++){
      model_->A1_RemoveFromWildcardTupleToResult
	(result_[i].VariablesToWildcards(), this, i);
    }
  }

  // Remove from SubrulePatternToRule
  // Maybe we want to pull this out into a separate function
  // That determines the pair <Pattern, SubRuleInfo> based on a rule
  if (type_ != NEGATIVE_RULE) {
    Pattern combined = precondition_->pattern_;
    combined.insert(combined.end(), result_.begin(), result_.end());
    int max = 1 << combined.size();
    for (int mask=0; mask < max; mask++) {
      if ((mask & (mask-1)) == 0) continue;
      SubRuleInfo sri;
      sri.rule_ = this;
      Pattern      subpattern, c_subpattern;
      for (uint c=0; c < combined.size(); c++) {
	if (mask & (1<<c)) {
	  subpattern.push_back(combined[c]);
	  if (c >= precondition_->pattern_.size())
	    sri.postcondition_ = true;
	}
      }
      if (IsConnectedPattern(subpattern)) {
	c_subpattern = Canonicalize(subpattern, &sri.sub_);
	model_->A1_RemoveFromSubrulePatternToRule(c_subpattern, sri);
      }
    } 
  }

  vector<int> arbitrary_terms;
  PatternLnLikelihood(precondition_->pattern_, result_, &arbitrary_terms);  
  for (uint i=0; i<arbitrary_terms.size(); i++)
    model_->L1_SubtractArbitraryTerm(arbitrary_terms[i]);
  //forall(run, causing_tuples_) (*run)->A1_RemoveFromRulesCaused(this);
}

set<int> Rule::RightVariables() const{
  return GetVariables(result_)-LeftVariables();
}
set<int> Rule::LeftVariables() const{
  return GetVariables(precondition_->pattern_);
}

// TODO: Revisit this.   
/*
vector<Tuple> Rule::ComputeTupleCauses() const{
  vector<Tuple> ret;
  Tuple s;
  s.push_back(LEXICON.GetAddID("IS_RULE"));
  s.push_back(LEXICON.GetAddID("RULE_"+itoa(id_)));
  if (type_==NEGATIVE_RULE) {
    s.push_back(LEXICON.GetAddID("NEGATIVE"));
    s.push_back(LEXICON.GetAddID("RULE_"+itoa(target_rule_->id_)));
  } else {
    s.push_back(LEXICON.GetAddID("POSITIVE"));
  }
  ret.push_back(s);
  const vector<Tuple> & pre = precondition_->pattern_;
  bool contains_result = false;
  if (type_ == NEGATIVE_RULE) {
    contains_result = true;
    for (uint i=0; i<target_rule_->result_.size(); i++) {
      if (!(pre % target_rule_->result_[i])) contains_result = false;
    }
    if (contains_result) {
      Tuple x;
      x.push_back(LEXICON.GetAddID("CONTAINS_RESULT"));
      x.push_back(LEXICON.GetAddID("RULE_"+itoa(id_)));      
    }
  }
  for (uint i=0; i<pre.size(); i++) {
    if (type_==NEGATIVE_RULE) {
      if (target_rule_->precondition_->clauses_ % pre[i]) continue;
      if (contains_result && (target_rule_->result_ % pre[i])) continue;
    }
    int name = LEXICON.GetAddID("RULE_" + itoa(id_) + "_PREC_" + itoa(i));
    Tuple x;
    x.push_back(LEXICON.GetAddID("HAS_PRECONDITION"));
    x.push_back(LEXICON.GetAddID("RULE_"+itoa(id_)));      
    x.push_back(name);
    ret.push_back(x);
    vector<Tuple> v = model_->ComputeTupleEncoding(pre[i], name);
    ret.insert(ret.end(), v.begin(), v.end());
  }
  for (uint i=0; i<result_.size(); i++) {
    int name = LEXICON.GetAddID("RULE_" + itoa(id_) + "_RES_" + itoa(i));
    Tuple x;
    x.push_back(LEXICON.GetAddID("HAS_RESULT"));
    x.push_back(LEXICON.GetAddID("RULE_"+itoa(id_)));      
    x.push_back(name);
    ret.push_back(x);
    vector<Tuple> v = model_->ComputeTupleEncoding(result_[i], name);
    ret.insert(ret.end(), v.begin(), v.end());
  }
  return ret;
}
*/
bool Rule::HasFiring() const{
  forall(run, rule_sats_){
    if (run->second->firings_.size()) return true;
  }
  return false;
}

void Rule::ChangeStrength(EncodedNumber new_strength,
			  EncodedNumber new_strength2) {
  double old_strength_d_ = strength_d_;
  A1_SetStrength(new_strength);
  A1_SetStrength2(new_strength2);
  A1_SetStrengthD(strength_.ToOpenInterval());
  A1_SetStrength2D(strength2_.ToOpenInterval());
  if (type_ != NEGATIVE_RULE) {
    precondition_->A1_SetLnLikelihoodPerSat
      (precondition_->ln_likelihood_per_sat_ + 
       log(1-strength_d_) - log(1-old_strength_d_));
    precondition_->ComputeSetLnLikelihood();
  }
  ComputeSetLnLikelihood();
  forall(run, rule_sats_)  run->second->ComputeSetLnLikelihood();
  if (type_ == NEGATIVE_RULE) {
    forall(run, rule_sats_) 
      run->second->target_rule_sat_->ComputeSetLnLikelihood();
  }
}
void Rule::ChangeDelay(EncodedNumber new_delay) {
  A1_SetDelay(new_delay);
  forall(run, rule_sats_) run->second->ComputeSetTime();
  ComputeSetLnLikelihood();
}
int Rule::NumFirings() const {
  int ret = 0;
  forall(run, rule_sats_){
    ret += run->second->firings_.size();
  }
  return ret;
}
int Rule::NumFirstFirings() const {
  // TODO: track this instead of computing it. (for time's sake)
  int ret = 0;
  forall(run, rule_sats_){
    if (run->second->firings_.size() > 0) ret++;
  }
  return ret;
}
vector<Firing *> Rule::Firings() const{
  vector<Firing *> ret;
  forall(run, rule_sats_) {
    forall(run_f, run->second->firings_) {
      ret.push_back(run_f->second);
    }
  }
  return ret;
}

string Rule::ImplicationString() const {
  return TupleVectorToString(precondition_->pattern_) + " -> "
    + TupleVectorToString(result_);
}

void Rule::A1_SetDelay(EncodedNumber value){
  model_->changelist_.Make(new ValueChange<EncodedNumber>(&delay_, value));
}
void Rule::A1_SetStrength(EncodedNumber value){
  model_->changelist_.Make(new ValueChange<EncodedNumber>(&strength_, value));
}
void Rule::A1_SetStrength2(EncodedNumber value){
  model_->changelist_.Make(new ValueChange<EncodedNumber>(&strength2_, value));
}
void Rule::A1_SetStrengthD(double value){
  model_->changelist_.Make(new ValueChange<double>(&strength_d_, value));
}
void Rule::A1_SetStrength2D(double value){
  model_->changelist_.Make(new ValueChange<double>(&strength2_d_, value));
}
void Rule::A1_AddRuleSat(Satisfaction * s, RuleSat * rs){
  model_->changelist_.Make(new MapInsertChange<Satisfaction *, RuleSat *>
			   (&rule_sats_, s, rs));
}
void Rule::A1_RemoveRuleSat(Satisfaction * s){
  model_->changelist_.Make(new MapRemoveChange<Satisfaction *, RuleSat *>
			   (&rule_sats_, s));
}
void Rule::A1_AddInhibitor(Rule * inhibitor){
  model_->changelist_.Make
    (new SetInsertChange<Rule *>(&inhibitors_, inhibitor));
}
void Rule::A1_RemoveInhibitor(Rule * inhibitor){
  model_->changelist_.Make
    (new SetRemoveChange<Rule *>(&inhibitors_, inhibitor));
}


// ----- RULESAT -----

RuleSat::RuleSat(Rule * rule, const Substitution & sub)
  :Component(rule->model_) {
  rule_ = rule;
  satisfaction_ = rule_->precondition_->L1_GetAddSatisfaction(sub);
  satisfaction_->A1_AddRuleSat(this);
  target_rule_sat_ = NULL;
  rule_->A1_AddRuleSat(satisfaction_, this);
  if (rule->type_ == NEGATIVE_RULE) {
    Rule * target_rule = rule_->target_rule_;
    Precondition * target_precondition = target_rule->precondition_;
    Substitution restricted_sub
      = sub.Restrict(GetVariables(target_precondition->pattern_));
    target_rule_sat_ = rule_->target_rule_->L1_GetAddRuleSat(restricted_sub);
    target_rule_sat_->A1_AddInhibitor(this);
  }
  ComputeSetTime();
  if (rule->type_ == NEGATIVE_RULE) target_rule_sat_->ComputeSetLnLikelihood();
  ComputeSetLnLikelihood();
  model_->A1_InsertIntoComponentsByType(this);
}
void RuleSat::L1_EraseSubclass(){
  satisfaction_->A1_RemoveRuleSat(this);
  rule_->A1_RemoveRuleSat(satisfaction_);
  if (rule_->type_ == NEGATIVE_RULE) {
    target_rule_sat_->A1_RemoveInhibitor(this);
    target_rule_sat_->ComputeSetLnLikelihood();
  }
}
void RuleSat::A1_AddFiring(const Substitution & sub, Firing *f){
  model_->changelist_.Make
    (new MapInsertChange<Substitution, Firing *>(&firings_, sub, f));
}
void RuleSat::A1_RemoveFiring(const Substitution & sub){
  model_->changelist_.Make
    (new MapRemoveChange<Substitution, Firing *>(&firings_, sub));
}
void RuleSat::A1_AddInhibitor(RuleSat *rs){
  model_->changelist_.Make
    (new SetInsertChange<RuleSat *>(&inhibitors_, rs));
}
void RuleSat::A1_RemoveInhibitor(RuleSat *rs){
  model_->changelist_.Make
    (new SetRemoveChange<RuleSat *>(&inhibitors_, rs));
}


Firing * RuleSat::FindFiring(const Substitution & right_sub) const{
  Firing * const * f = firings_ % right_sub;
  if (f) return *f;
  return NULL;
}
Firing * RuleSat::AddFiring(const Substitution & right_sub){
  if (firings_ % right_sub) {
    cerr << "Firing already found :" << right_sub.ToString() << endl;
    CHECK(false);
  }
  return new Firing(this, right_sub);
}

string RuleSat::ImplicationString(const Firing * firing) const {
  vector<Tuple> preconditions = rule_->precondition_->pattern_;
  vector<Tuple> results = rule_->result_;
  vector<Tuple> substituted_preconditions = preconditions;
  vector<Tuple> substituted_results = results;
  Substitution sub = satisfaction_->substitution_;
  if (firing) sub.Add(firing->right_substitution_);
  sub.Substitute(&substituted_preconditions);
  sub.Substitute(&substituted_results);

  string ret;
  // ret += rule_sat_->satisfaction_->substitution_.ToString();
  ret += rule_->HTMLLink("r") + " " 
    + satisfaction_->HTMLLink("s") + " " + HTMLLink("rs") + " " ;
  if (firing) ret += firing->HTMLLink("f") + " ";
  for (uint i=0; i<preconditions.size(); i++) {
    TrueTuple * tp
      = model_->FindTrueTuple(substituted_preconditions[i]);
    CHECK(tp);
    ret += tp->HTMLLink(ToString(preconditions[i], sub))
      + " (" + tp->time_.ToSortableString() + ")"
      + " ";
  }
  ret += "-> ";
  for (uint i=0; i<results.size(); i++) {
    if (substituted_results[i].IsConstantTuple()) {
      TrueTuple * tp
	= model_->FindTrueTuple(substituted_results[i]);
      if (tp){
	ret += tp->HTMLLink(ToString(results[i], sub)) + " ("
	  + tp->time_.ToSortableString() + ") ";
      } else {
	ret += "Error - tuple not found  ";
      }
    } else {
      ret += ToString(results[i], sub) + " ";
    }
  }
  return ret;
}


// ----- FIRING -----

Firing::Firing(RuleSat * rule_sat, Substitution right_substitution)
  :Component(rule_sat->model_){
  rule_sat_ = rule_sat;
  right_substitution_ = right_substitution;
  CHECK(!(rule_sat_->FindFiring(right_substitution_)));
  rule_sat_->A1_AddFiring(right_substitution_, this);
  rule_sat_->ComputeSetLnLikelihood();

  // perform the substitution to find the TrueTuples.
  vector<Tuple> results = rule_sat->rule_->result_;
  GetFullSubstitution().Substitute(&results);

  // Now we have the vector of constant tuples which come true.
  for (uint i=0; i<results.size(); i++){
    TrueTuple * tp = model_->GetAddTrueTuple(results[i]);
    true_tuples_.insert(tp); // Note: duplicate insertions OK.
  }

  // Note: we need two loops because of duplicates.
  forall(run, true_tuples_) { 
    (*run)->A1_AddCause(this);
  }

  // For creative rules, this counts the names and adjusts the naming costs.
  forall (run, right_substitution_.sub_)  
    model_->L1_AddArbitraryTerm(run->second);

  ComputeSetTime();
  ComputeSetLnLikelihood();
  model_->A1_InsertIntoComponentsByType(this);
}

void Firing::L1_EraseSubclass() {
  forall(run, true_tuples_) {
    (*run)->A1_RemoveCause(this);
    (*run)->ComputeSetTime();
  }
  rule_sat_->A1_RemoveFiring(right_substitution_);
  rule_sat_->ComputeSetLnLikelihood();
  forall (run, right_substitution_.sub_)
    model_->L1_SubtractArbitraryTerm(run->second);
}
Substitution Firing::GetFullSubstitution() const{
  Substitution ret = right_substitution_;
  ret.Add(rule_sat_->satisfaction_->substitution_);
  return ret;
}
string Firing::ImplicationString() const{
  if (this==NULL) return "SPONTANEOUS";
  return rule_sat_->ImplicationString(this);
}

// ----- TRUETUPLE -----
TrueTuple::TrueTuple(Model * model, Tuple tuple)
  :Component(model){
  tuple_ = tuple;
  required_count_ = 0;
  // Insert the tuple into the tuple index
  model_->changelist_.Make
    (new MemberCall1Change<TupleIndex, Tuple>(&model_->tuple_index_, tuple_,
					     &TupleIndex::Add,
					     &TupleIndex::Remove));
  model_->L1_UpdateSearchTreesAfterAddTuple(tuple_);
  
  // Add it to the map from tuple to TrueTuple
  model_->A1_InsertIntoTupleToTrueTuple(tuple_, this);
  ComputeSetTime();
  // Figure out whether any prohibitions are violated.  
  for (GeneralizationIterator run_g(tuple_); !run_g.done(); ++run_g) {
    set<Prohibition *> * prohibitions 
      = model_->prohibition_index_ % run_g.Current();
    if (prohibitions) forall(run_p, *prohibitions) {
      (*run_p)->L1_CheckAddViolation(this);
    }
  }
  ComputeSetLnLikelihood();
  model_->A1_InsertIntoComponentsByType(this);
}

void TrueTuple::L1_EraseSubclass(){
  // We don't have to deal with firings, since they are structural dependents.
  CHECK(required_count_ == 0);

  // Caveat: we can't directly iterate over violated_prohibitions_, since
  // we are implicitly erasing its elements. 
  while (violated_prohibitions_.size()) {
    (*violated_prohibitions_.begin())->
      L1_RemoveViolationOnTrueTupleDelete(this);
  }

  model_->A1_RemoveFromTupleToTrueTuple(tuple_);
  model_->changelist_.Make
    (new MemberCall1Change<TupleIndex, Tuple>(&model_->tuple_index_, tuple_,
					     &TupleIndex::Remove,
					     &TupleIndex::Add));
  model_->L1_UpdateSearchTreesAfterRemoveTuple(tuple_);
}
 
void TrueTuple::A1_MakeRequired(){
  model_->changelist_.Make
    (new ValueChange<int>(&required_count_, required_count_+1));
}
void TrueTuple::A1_MakeNotRequired(){
  CHECK(required_count_>0);
  model_->changelist_.Make
    (new ValueChange<int>(&required_count_, required_count_-1));
}
void TrueTuple::A1_AddCause(Firing *f){
  model_->changelist_.Make
    (new SetInsertChange<Firing *>(&causes_, f));
}
void TrueTuple::A1_RemoveCause(Firing *f){
  model_->changelist_.Make
    (new SetRemoveChange<Firing *>(&causes_, f));
}
void TrueTuple::A1_AddSatisfaction(Satisfaction *sat){
  model_->changelist_.Make
    (new SetInsertChange<Satisfaction *>(&satisfactions_, sat));
}
void TrueTuple::A1_RemoveSatisfaction(Satisfaction *sat){
  model_->changelist_.Make
    (new SetRemoveChange<Satisfaction *>(&satisfactions_, sat));
}
void TrueTuple::A1_AddViolatedProhibition(Prohibition *p){
  model_->changelist_.Make
    (new SetInsertChange<Prohibition *>(&violated_prohibitions_, p));
}
void TrueTuple::A1_RemoveViolatedProhibition(Prohibition *p){
  model_->changelist_.Make
    (new SetRemoveChange<Prohibition *>(&violated_prohibitions_, p));
}


ComponentType Precondition::Type() const { return PRECONDITION; }
ComponentType Satisfaction::Type() const { return SATISFACTION; }
ComponentType Rule::Type() const { return RULE; }
ComponentType RuleSat::Type() const { return RULESAT; }
ComponentType Firing::Type() const { return FIRING; }
ComponentType TrueTuple::Type() const { return TRUETUPLE; }

string Component::TypeName() const { return ComponentTypeToString(Type());}

string Component::HTMLLink(string text) const{
  if (model_->old_style_display_)
    return string() + "<a href=" + TypeName()
      + ".html#" + itoa(id_) + ">" + text + "</a>";
  return string() + "<a href=command=showcomponent&id="
    + itoa(id_) + ">" + text + "</a>";
}

Record Component::RecordForStorge() const{
  Record r = RecordForStorageSubclass();  
  r["id"] = itoa(id_);  
  r["CT"] = TypeName();
  return r;
}
Record Precondition::RecordForStorageSubclass() const{
  // no need to store the precondition.  The rule creates us. 
  Record r;
  return r;
}
Record Satisfaction::RecordForStorageSubclass() const{
  // We just store the satisfaction so as to get the ID right on load.
  Record r;
  r["precondition"] = itoa(precondition_->id_);
  r["substitution"] = substitution_.ToString();
  return r;
}
Record Rule::RecordForStorageSubclass() const{
  Record r;
  r["precondition"] = TupleVectorToString(precondition_->pattern_);
  r["precondition_id"] = itoa(precondition_->id_);
  r["delay"] = delay_.ToSortableString();
  r["rule_type"] = RuleTypeToString(type_);
  r["result"] = TupleVectorToString(result_);
  r["strength"] = strength_.ToSortableString();
  r["strength2"] = strength2_.ToSortableString();
  if (type_==NEGATIVE_RULE) {
    r["target_id"] = itoa(target_rule_->id_);
  }
  return r;
}
Record RuleSat::RecordForStorageSubclass() const{  
  Record r;
  r["rule"] = itoa(rule_->id_);
  r["satisfaction"] = itoa(satisfaction_->id_);
  return r;
}
Record Firing::RecordForStorageSubclass() const{
  Record r;
  r["rule"] = itoa(rule_sat_->rule_->id_);
  r["full_substitution"] = GetFullSubstitution().ToString();
  return r;
}
Record TrueTuple::RecordForStorageSubclass() const{
  Record r;
  r["tuple"] = tuple_.ToString();
  return r;
}

Record Component::RecordForDisplay() const{
  Record r = RecordForDisplaySubclass();
  r["Comments"] = GetComments();
  if (time_dirty_) r["D"] = "DIRTY";
  r["ID"] = itoa(id_) + "<a name=\"" + itoa(id_) + "\">";
  r["TIME"] = time_.ToSortableString();
  r["LL"] = dtoa(ln_likelihood_);
  if (ln_likelihood_!= LnLikelihood()) 
    r["LL"] += " (" + dtoa(LnLikelihood()) + ")";
  return r;
}
Record Precondition::RecordForDisplaySubclass() const{
  Record r;
  r["precondition"] = TupleVectorToString(pattern_);
  r["dpe LL"] = dtoa(direct_pattern_encoding_ln_likelihood_);
  forall(run, rules_){
    r["rules"] 
      += (*run)->HTMLLink(TupleVectorToString((*run)->result_)) + "<br>";
  }
  r["num_sat(explicit)"]
    = itoa(num_satisfactions_) + " (" + itoa(satisfactions_.size()) + ")";
  return r;
}
Record Rule::RecordForDisplaySubclass() const{
  Record r;
  r["Rule"] = ImplicationString();
  r["Type"] = RuleTypeToString(type_).substr(0, 1);
  r["f/ff/s"] = "f " + itoa(NumFirings()) + "<br>ff " + itoa(NumFirstFirings())
    + "<br>s " + itoa(precondition_->num_satisfactions_); 
  r["dpe LL"] = dtoa(direct_pattern_encoding_ln_likelihood_);
  r["prec."] = delay_.ToSortableString();
  r["str."] = strength_.ToSortableString();
  r["str2."] = strength2_.ToSortableString();
  r["pat."] = precondition_->HTMLLink(itoa(precondition_->id_));
  forall(run, inhibitors_){
    r["inhibitors"] += "Inhibitor: " + (*run)->HTMLLink(itoa((*run)->id_))
      + "<br>";
  }
  if(type_==NEGATIVE_RULE) 
    r["target"] = target_rule_->HTMLLink(itoa(target_rule_->id_));
  vector<Firing *> f = Firings();
  for(uint i=0; i<f.size(); i++) {
    r["firngs"] += f[i]->ImplicationString() + "<br>\n";
  }
  return r;
}
Record Satisfaction::RecordForDisplaySubclass() const{
  Record r;
  r["precondition"] = precondition_->HTMLLink(TupleVectorToString(precondition_->pattern_));
  r["substitution"] = substitution_.ToString();
  return r;
}
Record RuleSat::RecordForDisplaySubclass() const{
  Record r;
  r["rule"] = ImplicationString(NULL);
  r["sat."] = satisfaction_->HTMLLink(itoa(satisfaction_->id_));
  forall(run, firings_) {
    Firing *f = run->second;
    r["firings"] += f->HTMLLink(itoa(f->id_)) + " ";
  }
  //r["substitution"] = satisfaction_->substitution_.ToString();
  if(target_rule_sat_) 
    r["target"] = target_rule_sat_->HTMLLink(itoa(target_rule_sat_->id_));
  if (inhibitors_.size()) {			
    forall (run, inhibitors_) 
      r["inhibitors"] += (*run)->HTMLLink(itoa((*run)->id_)) + ", ";
  }
  return r;
}
Record Firing::RecordForDisplaySubclass() const{
  Record r;
  r["rule_sat"] = rule_sat_->HTMLLink(itoa(rule_sat_->id_));
  r["implication"] = ImplicationString();
  forall(run, true_tuples_)
    r["true_tuples"] += (*run)->HTMLLink((*run)->tuple_.ToString());
  return r;
}
Record TrueTuple::RecordForDisplaySubclass() const {
  Record r;
  r["Tuple"] = tuple_.ToString();
  forall(run, causes_) {
    Firing *f = *run;
    r["causes"] += f->ImplicationString();
  }
  set<Firing *> f = GetResultFirings();
  forall(run, f) r["firings"] += (*run)->ImplicationString() + "<br>\n";
  r["required"] = required_count_?"REQUIRED":"";
  return r;
}
 
vector<Component *> Component::TemporalDependents() const{
  return vector<Component *>();
}
vector<Component *> Precondition::TemporalDependents() const{
  vector<Satisfaction*> v = VectorOfValues(satisfactions_);
  vector<Component *> ret(v.begin(), v.end());
  ret.insert(ret.end(), rules_.begin(), rules_.end());
  return ret;
}
vector<Component *> Satisfaction::TemporalDependents() const {
  return vector<Component *>(rule_sats_.begin(), rule_sats_.end());
}
vector<Component *> Rule::TemporalDependents() const{
  vector<RuleSat*> v = VectorOfValues(rule_sats_);
  vector<Component *> ret(v.begin(), v.end());
  ret.insert(ret.end(), inhibitors_.begin(), inhibitors_.end());
  return ret;
}
vector<Component *> RuleSat::TemporalDependents() const{
  vector<Firing*> v = VectorOfValues(firings_);
  return vector<Component*>(v.begin(), v.end());
}
vector<Component *> Firing::TemporalDependents() const{
  vector<Component *> ret;
  forall(run, true_tuples_){
    ret.push_back(*run);
  }
  return ret;
}
vector<Component *> TrueTuple::TemporalDependents() const{
  vector<Component *> ret;
  ret.insert(ret.end(), satisfactions_.begin(), satisfactions_.end());
  //if (rule_encoded_) ret.push_back(rule_encoded_);
  return ret;
}

vector<Component *> Component::StructuralDependents() const{
  return vector<Component *>();
}
vector<Component *> Precondition::StructuralDependents() const{
  vector<Satisfaction*> v = VectorOfValues(satisfactions_);
  vector<Component *> ret(v.begin(), v.end());
  ret.insert(ret.end(), rules_.begin(), rules_.end());
  return ret;
}
vector<Component *> Satisfaction::StructuralDependents() const {
  return vector<Component *>(rule_sats_.begin(), rule_sats_.end());
}
vector<Component *> Rule::StructuralDependents() const{
  vector<RuleSat*> v = VectorOfValues(rule_sats_);
  vector<Component *> ret(v.begin(), v.end());
  ret.insert(ret.end(), inhibitors_.begin(), inhibitors_.end());
  return ret;
}

vector<Component *> RuleSat::StructuralDependents() const{
  vector<Firing*> v = VectorOfValues(firings_);
  vector<Component*> ret(v.begin(), v.end());
  ret.insert(ret.end(), inhibitors_.begin(), inhibitors_.end());
  return ret;
}

vector<Component *> TrueTuple::StructuralDependents() const{
  vector<Component *> ret;
  ret.insert(ret.end(), satisfactions_.begin(), satisfactions_.end());
  //  if (rule_encoded_) ret.push_back(rule_encoded_);
  return ret;
}


vector<vector<Component *> > Component::TemporalCodependents() const{
  return vector<vector<Component *> >();
}
vector<vector<Component *> > Satisfaction::TemporalCodependents() const {
  vector<vector<Component *> > ret;
  ret.push_back(vector<Component *>(1, precondition_));
  forall(run, true_tuples_) {
    ret.push_back(vector<Component *>(1, *run));
  }
  return ret;
}
vector<vector<Component *> > Rule::TemporalCodependents() const{
  vector<vector<Component *> > ret;
  ret.push_back(vector<Component *>(1, precondition_));
  if (type_==NEGATIVE_RULE) ret.push_back(vector<Component *>(1, target_rule_));
  //for (uint i=0; i<encoding_.size(); i++) 
  //  ret.push_back(vector<Component *>(1, encoding_[i]));
  return ret;
}
vector<vector<Component *> > RuleSat::TemporalCodependents() const{
  vector<vector<Component *> > ret;
  ret.push_back(vector<Component *>(1, satisfaction_));
  ret.push_back(vector<Component *>(1, rule_));
  return ret;
}
vector<vector<Component *> > Firing::TemporalCodependents() const{
  vector<vector<Component *> > ret;
  ret.push_back(vector<Component *>(1, rule_sat_));
  return ret;
}
vector<vector<Component *> > TrueTuple::TemporalCodependents() const{
  vector<vector<Component *> > ret;
  ret.push_back(vector<Component *>(causes_.begin(), causes_.end()));
  return ret;
}

vector<Component *> Component::Purposes() const{
  return vector<Component *>();
}

bool Component::HasPurpose() const { return true; }

vector<Component *> Precondition::Purposes() const{
  return vector<Component *>(rules_.begin(), rules_.end());
}

bool Precondition::HasPurpose() const { return rules_.size(); }

vector<Component *> Satisfaction::Purposes() const{
  return vector<Component *>(rule_sats_.begin(), rule_sats_.end());
}

bool Satisfaction::HasPurpose() const { return rule_sats_.size(); }

vector<Component *> RuleSat::Purposes() const{
  vector<Firing*> v = VectorOfValues(firings_);
  vector<Component*> ret(v.begin(), v.end());
  ret.insert(ret.end(), inhibitors_.begin(), inhibitors_.end());
  return ret;
}

bool RuleSat::HasPurpose() const {return firings_.size()||inhibitors_.size();}

vector<Component *> Component::Copurposes() const{
  return vector<Component *>();
}
vector<Component *> Rule::Copurposes() const{
  vector<Component *> ret;
  ret.push_back(precondition_);
  return ret;  
}
vector<Component *> RuleSat::Copurposes() const{
  vector<Component *> ret;
  ret.push_back(satisfaction_);
  if (target_rule_sat_) ret.push_back(target_rule_sat_);
  return ret;  
}
vector<Component *> Firing::Copurposes() const{
  vector<Component *> ret;
  ret.push_back(rule_sat_);
  return ret;
}

bool Component::NeedsPurpose() const{ return false; }
bool Precondition::NeedsPurpose() const { return true; }
bool Satisfaction::NeedsPurpose() const { return true; }
bool RuleSat::NeedsPurpose() const {
  if (rule_->type_ == NEGATIVE_RULE) return false;
  return true;
}

bool Component::ComputeSetTime(){
  Time new_time = ComputeTime(NULL);
  bool ret = (time_ != new_time); 
  L1_SetTimeMaintainIndices(new_time, true);
  L1_MakeTimeClean();
  return ret;
}
Time Component::ComputeTime(set<Component *> *excluded) const{
  Time ret;
  vector<vector<Component *> > codep = TemporalCodependents();
  for (uint i=0; i<codep.size(); i++) {
    Time first = NEVER;
    for (uint j=0; j<codep[i].size(); j++) {
      if (codep[i][j]==0) {
	first = Time();
      } else {
	if (!excluded || !((*excluded)%codep[i][j])) 
	  first = min(first, codep[i][j]->time_);
      }
    }
    ret=max(ret, first);
  }
  if (HasTimeDelay()) ret.Increment(GetTimeDelay(), 1);
  return ret;
}
void Component::L1_SetTimeMaintainIndices(Time new_time,
					  bool make_dependents_dirty){
  if (new_time == time_) return; 
  if (time_.IsNever()) {
    model_->A1_RemoveFromNeverHappen(this);
    if (Type()==TRUETUPLE && ((TrueTuple*)this)->required_count_)
      model_->A1_RemoveFromRequiredNeverHappen((TrueTuple*)this);
  }
  A1_SetTime(new_time);
  F2_AdjustLnLikelihoodForNewTime();
  if (make_dependents_dirty) {
    vector<Component *> dep = TemporalDependents();
    for(uint i=0; i<dep.size(); i++) {
      if (dep[i]->ComputeTime(NULL) != dep[i]->time_)
	dep[i]->L1_MakeTimeDirty();
    }
  }
  if (time_.IsNever()){
    model_->A1_InsertIntoNeverHappen(this);
    if (Type()==TRUETUPLE && ((TrueTuple*)this)->required_count_)
      model_->A1_InsertIntoRequiredNeverHappen((TrueTuple*)this);
  }
}

void Component::L1_MakeTimeClean(){
  if (time_dirty_  == false) return;
  A1_SetTimeDirty(false);
  model_->A1_RemoveFromTimesDirty(this);
}
void Component::L1_MakeTimeDirty(){
  if (time_dirty_ == true) return;
  A1_SetTimeDirty(true);
  model_->A1_InsertIntoTimesDirty(this);
}
void Component::F2_AdjustLnLikelihoodForNewTime(){}
void RuleSat::F2_AdjustLnLikelihoodForNewTime(){
  if (rule_->type_ == NEGATIVE_RULE){
    CHECK(target_rule_sat_); // if this fails, maybe we are in the constructor?
    target_rule_sat_->ComputeSetLnLikelihood();
  }
  if (inhibitors_.size()) ComputeSetLnLikelihood();
}
double Component::LnLikelihood() const {
  return 0.0;
}
double Precondition::LnLikelihood() const {
  double ret = direct_pattern_encoding_ln_likelihood_;
  ret += num_satisfactions_ * ln_likelihood_per_sat_;
  return ret;
}
double Rule::LnLikelihood() const {  
  double ret = EncodedNumberLnLikelihood(strength_);
  ret += direct_pattern_encoding_ln_likelihood_;
  if (type_ == CREATIVE_RULE) ret += EncodedNumberLnLikelihood(strength2_);
  if (type_ != NEGATIVE_RULE)  ret += EncodedNumberLnLikelihood(delay_);
  return ret;
}

double RuleSat::LnLikelihood() const {
  if (rule_->type_ == NEGATIVE_RULE) return 0;

  // this is what was counted implicitly by the precondition.
  double cancelled_ln_likelihood = log(1-rule_->strength_d_);

  // the multiplier on the prior due to inhibiting rulesats
  double inhibition = 1.0;
  forall(run, inhibitors_){
    bool in_time = ((*run)->time_ < time_);
    if (in_time) inhibition *= (1-(*run)->rule_->strength_d_);
  }
  double prob = rule_->strength_d_;
  prob *= inhibition;
  double new_ln_likelihood = 0;
  int num_firings = firings_.size();
  if (num_firings >= 1) new_ln_likelihood += log(prob);
  else new_ln_likelihood += log(1-prob);
  if (num_firings >= 1 && rule_->type_ == CREATIVE_RULE) {
    double prob2 = rule_->strength2_d_;
    // prob2 *= inhibition; // not sure if this is the right thing to do.
    new_ln_likelihood += log(prob2) * (num_firings-1) + log(1-prob2);
  }
  return new_ln_likelihood - cancelled_ln_likelihood;
}

void Component::ComputeSetLnLikelihood() {
  if (!exists_) return;

  CHECK(!really_dead_);
  double old_val = ln_likelihood_;
  double new_val = LnLikelihood();
  CHECK(finite(old_val));
  CHECK(finite(new_val));
  A1_SetLnLikelihood(new_val);
  model_->A1_AddToLnLikelihood(new_val - old_val);
}

void Component::VerifyLayer2() const {
  CHECK(exists_);
  // Check that the likelihood is up to date.
  if (fabs(LnLikelihood() - ln_likelihood_) > 1e-6){
    cerr << "likelihood for component " << id_ << " out of date"
	 << " stored=" << ln_likelihood_
	 << " computed=" << LnLikelihood()
	 << endl;
    model_->ToHTML("html");
    CHECK(false);
  }
  // Check that the time is up to date or that time_dirty_ is set
  CHECK(time_dirty_ || ComputeTime(NULL) == time_);
  // Check that the component has a purpose if it needs one.
  CHECK(!NeedsPurpose() || Purposes().size());
  VerifyLayer2Subclass();
}
void Component::VerifyLayer2Subclass() const{}
void Precondition::VerifyLayer2Subclass() const{  
  uint64 num_sat;
  model_->tuple_index_.
    FindSatisfactions(pattern_, SamplingInfo::Unsampled(), NULL, &num_sat, NULL);
  CHECK(num_sat == (uint64)num_satisfactions_);
}
void Rule::VerifyLayer2Subclass() const{
  if (type_ == NEGATIVE_RULE) {  // Make all the RuleSats exist.
    vector<Substitution> substitutions;
    precondition_->search_tree_->GetSubstitutions(&substitutions);
    for (uint i=0; i<substitutions.size(); i++) {
      CHECK(FindRuleSat(substitutions[i]));
    }
  }  
}
