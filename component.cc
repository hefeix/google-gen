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
#include <fstream.h>
#include <math.h>

bool FLAGS_firing_tuple = false;

// COMPONENT
Component::Component(Model * model, int id){
  model_->changelist_.Make(new NewChange<Component>(this));
  model_ = model;
  exists_ = false;
  time_ = CREATION;
  time_dirty_ = true;
  if (id==-1) model_->L1_AssignNewID(this);
  else model->L1_AssignSpecificID(this, id);
  ln_likelihood_ = 0.0;
  A1_SetExists(true);
}
Component::~Component(){
  CHECK(exists_ == false);
}
void Component::A1_SetExists(bool val){
  CHECK(exists_ != val);
  model_->changelist_.Make(new ValueChange<bool>(&exists_, val));
}
void Component::A1_SetTime(const Time & new_time){
  if (time_ == new_time) return;
  model_->changelist_.Make(MakeValueChange(&time_, new_time));  
}
void Component::A1_SetTimeDirty(bool new_val){
  CHECK(time_dirty_ != val);
  model_->changelist_.Make(new ValueChange<bool>(&time_dirty_, val));
}
void Component::A1_SetLnLikelihood(double new_ln_likelihood){
  if (new_ln_likelihood == ln_likelihood_) return;
  model_->changelist_.Make(new ValueChange<double>(&model_ln_, val));
}
void Component::Erase(){
  vector<Component *> dep = StructuralDependents();
  vector<Component *> copurposes = Copurposes();
  for (int i=0; i<dep.size(); i++) {
    if (dep[i]->Exists()) dep[i]->Erase();
  }
  if (!Exists()) return;

  L1_EraseSubclass();
  A1_SetExists(false);
  model_->A1_SetLnLikelihood(model_->ln_likelihood_ - ln_likelihood_);

  for (int i=0; i<copurposes.size(); i++) {
    if (copurposes[i].Exists() && copurposes[i].IsSuperfluous()) 
      copurposes[i]->Erase();
  }
}


Precondition::Precondition(Model * model, 
			   const vector<Tuple> & tuples, int id)
  : Component(model, id){
  pattern_ = tuples;
  ln_likelihood_per_sat_ = 0.0;
  num_satisfactions_ = 0;
  
  model_->L1_InsertIntoClauseToPreconditionMap(this);

  uint64 fprint = Fingerprint(pattern_);
  model_->changelist_.
    Make(new HashMapInsertChange<uint64, Precodition *>
	 (precondition_index_, fprint, this));
  ComputeSetTime();
  uint64 num_sat;
  uint64 work;
  model_->tuple_index_.FindSatisfactions(pattern_, 0, &num_sat, -1, &work);
  num_satisfactions_ = num_sat;
  ComputeSetLnLikelihood();
}

void Precondition::EraseSubclass(){
  A1_RemoveFromModel();
  model_->changelist_.
    Make(new HashMapRemoveChange<uint64, Precodition *>
	 (precondition_index_, fprint, this));
  model_->A1_RemoveFromModelClauseToPreconditionMap(this);
}

void Precondition::A1_AddRule(Rule *r){
  model_->changelist_.Make(new SetInsertChange<Rule *>(&rules_, r));
}
void Precondition::A1_RemoveRule(Rule *r){
  model_->changelist_.Make(new SetRemoveChange<Rule *>(&rules_, r));
}
void Precondition::A1_AddNegativeRule(Rule *r){
  model_->changelist_.Make(new SetInsertChange<Rule *>(&negative_rules_, r));
}
void Precondition::A1_RemoveNegatieveRule(Rule *r){
  model_->changelist_.Make(new SetRemoveChange<Rule *>(&negative_rules_, r));
}
void Precondition::A1_AddSatisfaction(Satisfaction * sat){
  model_->changelist_.
    Make(new MapInsertChange<Uint64, Satisfaction *>
	 (&satisfactions_, sat->substitution_.Fingerprint(), sat));
}
void Precondition::A1_RemoveSatisfaction(Satisfaction *sat){
  model_->changelist_.
    Make(new MapRemoveChange<uint64, Satisfaction *>
	 (&satisfactions_, sat->substitution_.Fingerprint()));
}
void Precondition::A1_SetPreconditionLnLikelihood(double val){
  model_->changelist_.
    Make(new ValueChange<double>(&precondition_ln_likelihood_, val));
}
void Precondition::A1_SetLnLikelihoodPerSat(double val){
  model_->changelist_.
    Make(new ValueChange<double>(&ln_likelihood_per_sat_,val));
}

void Precondition::A1_AddToNumSatisfactions(int delta){
  model_->changelist_.
    Make(new ValueChange<int>(&num_satisfactions_, num_satisfactions_+delta));
}
Satisfaction * Precondition::L1_GetAddSatisfaction(const Substitution & sub){
  Satisfaction ** sp = satisfactions_ % sub.Fingerprint();
  if (sp) return *sp;
  Satisfaction * ret = new Satisfaction(this, sub);
  return ret;
}


Satisfaction::Satisfaction(Precondition * precondition, 
			   const Substitution & sub, int id)
  :Component(precondition->model_, id) {
  precondition_ = precondition;
  substitution_ = sub;
  
  // This loop checks that all of the variables in the precondition are
  // in the substitution.
  vector<Tuple> substituted_precondition = precondition->pattern_;
  sub.Substitute(&substituted_precondition);
  for (uint i=0; i<substituted_precondition.size(); i++) {
    CHECK(!substituted_precondition[i].HasVariables());
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
}

void Satisfaction::EraseSubclass()
  precondition_->A1_RemoveSatisfaction(this);
  forall(run, true_tuples_) {    
    (*run)->A1_RemoveSatisfaction(this);
  }
}

void Satisfaction::A1_AddTrueTuple(TrueTuple *t){
  model_->changelist_.
    Make(new SetInsertChange<TruleTuple *>(&true_tuples, t));
}
void Satisfaction::A1_RemoveTrueTueple(TrueTuple *t){
  model_->changelist_.
    Make(new SetRemoveChange<TruleTuple *>(&true_tuples, t));
}
void Satisfaction::A1_AddRuleSat(RuleSat *rs){
  model_->changelist_.
    Make(new SetInsertChange<RuleSat *>(&rule_sats_, rs));
}
void Satisfaction::A1_RemoveRuleSat(RuleSat *rs){
  model_->changelist_.
    Make(new SetRemoveChange<RuleSat *>(&rule_sats_, rs));
}

Firing * Rule::GetFiring(const Substitution & sub) {
  Substitution left_sub = sub.Restrict(LeftVariables());
  RuleSat ** rs = rule_sats_ % left_sub;
  if (!rs) return NULL;
  return (*rs)->GetFiring(sub.Restrict(RightVariables()));
}
Firing * Rule::AddFiring(const Substitution & sub) {
  RuleSat * rs = L1_GetAddRuleSat(sub.Restrict(LeftVariables()));
  CHECK(rs);
  Firing * ret = rs->AddFiring(sub.Restrict(RightVariables()));
  VLOG(2) << "left sub=" << sub.Restrict(LeftVariables()).ToString()
	  << "left sub=" << sub.Restrict(RightVariables()).ToString() << endl;
  return ret;
}
RuleSat * Rule::L1_GetAddRuleSat(Satisfaction * sat) {
  RuleSat ** rsp = rule_sats_ % sat;
   if (rsp) return *rsp;
   return new RuleSat(this, sat->substitution_);
}
RuleSat * Rule::L1_GetAddRuleSat(const Substitution & sub) {
  Satisfaction * sat = precondition_->L1_GetAddSatisfaction(sub);
  CHECK(sat);
  return GetAddRuleSat(sat);
}


Rule::Rule(Precondition * precondition, EncodedNumber delay, 
	   RuleType type, Rule * target_rule,
	   vector<Tuple> result, EncodedNumber strength,
	   EncodedNumber strength2)
  :Component(precondition->model_){
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
  CHECK ((type == NEGATIVE_RULE) ^^ (target_rule_==NULL));
  if (type == NEGATIVE_RULE) {
    precondition_->A1_AddNegativeRule(this);
    precondition_->A1_AddToNegativeRuleIndex(target_rule_, this);
    target_rule_->A1_AddInhiinhibitor(this);
  } else {
    precondition_->A1_AddToPositiveRuleIndex(result_, this);
    precondition_->A1_SetLnLikelihoodPerSat
      (precondition_->ln_likelihood_per_sat_ + log(1-strength_d_));
    precondition_->ComputeSetLnLikelihood();
  }
  for (uint i=0; i<result_.size(); i++){
    model_->A1_InsertIntoWildcardTupleToResultMap
      (VariableToWildcard(result_[i]), this, i);
  }
  vector<Tuple> causes = ComputeCauses();
  for (uint i=0; i<causes.size(); i++) {
    TrueTuple * p = model_->GetAddTrueTuple(encoding[i]);
    causing_tuples_.push_back(p);
    p->A1_AddToRulesCaused(this);
  }
  ComputeSetTime();  
  if (type == NEGATIVE_RULE) {  // Make all the RuleSats exist.
    vector<Substitution> substitutions;
    uint64 num_sat;
    uint64 work;
    model_->tuple_index_.FindSatisfactions(precondition_->pattern_, 
					   &substitutions,
					   &num_sat, -1, &work);
    for (uint i=0; i<substitutions.size(); i++) {
      new RuleSat(this, substitutions[i]);
    }
  }
  ComputeSetLnLikelihood();
}
Rule::EraseSubclass(){  
  precondition_->A1_RemoveRule(this);
  if (type == NEGATIVE_RULE) {
    precondition_->A1_RemoveNegativeRule(this);
    precondition_->A1_RemoveFromNegativeRuleIndex(target_rule_, this);
  } else {
    precondition_->A1_RemoveFromPositiveRuleIndex(result_, this);
    precondition_->A1_SetLnLikelihoodPerSat
      (precondition_->ln_likelihood_per_sat_ - log(1-strength_d_));
    precondition_->ComputeSetLnLikelihood();
    for (uint i=0; i<result_.size(); i++){
      model_->A1_RemoveFromWildcardTupleToResultMap
	(VariableToWildcard(result_[i]), this, i);
    }
  }
  forall(run, causing_tuples_) (*run)->A1_RemoveFromRulesCaused(this);
}

set<int> Rule::RightVariables() const{
  return GetVariables(result_)-LeftVariables();
}
set<int> Rule::LeftVariables() const{
  return GetVariables(precondition_->pattern_);
}

// TODO: Revisit this.   
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
bool Rule::HasFiring() const{
  forall(run, rule_sats_){
    if (run->second->firings_.size()) return true;
  }
  return false;
}

void Rule::ChangeStrength(EncodedNumber new_strength,
			  EncodedNumber new_strength2) {
  model_->RecordChangeStrength(this, strength_, strength2_);
  double old_strength_d_ = strength_d_;
  A1_SetStrength(new_strength);
  A1_SetStrength2(new_strength2);
  A1_SetStrengthD(strength_.ToOpenInterval());
  A1_SetStrengthD(strength2_.ToOpenInterval());
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


// ----- RULESAT -----

RuleSat::RuleSat(Rule * rule, const Substitution & sub) 
  :Component(rule->model_) {
  rule_ = rule;
  satisfaction_ = rule_->precondition_->GetAddSatisfaction(sub);
  satisfaction_->A1_AddRuleSat(this);
  target_rule_sat_ = NULL;
  rule_->A1_AddRuleSat(satisfaction_, this);
  if (rule->type_ == NEGATIVE_RULE) {
    Rule * target_rule = rule_->target_rule_;
    Precondition * target_precondition = target_rule->precondition_;
    Substitution restricted_sub 
      = sub.Restrict(GetVariables(target_precondition->pattern_));
    target_rule_sat_ = target_rule_->GetAddRuleSat(restricted_sub);
    target_rule_sat_->A1_AddInhibitor(this);
    target_rule_sat_->ComputeSetLnLikelihood();
  }
  ComputeSetTime();
  ComputeSetLnLikelihood();
}
void RuleSat::L1_EraseSubclass(){
  satisfaction_->A1_RemoveRuleSat(this);
  rule_->A1_RemoveRuleSat(satisfaction_);
  if (rule->type_ == NEGATIVE_RULE) {
    target_rule_sat_->A1_RemoveInhibitor(this);
    target_rule_sat_->ComputeSetLnLikelihood();
  }
}

Firing * RuleSat::GetFiring(const Substitution & right_sub) const{
  Firing ** f = firings_ % right_sub;
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
  vector<Tuple> preconditions = rule_->precondition_->clauses_;
  vector<Tuple> results = rule_->result_;
  vector<Tuple> substituted_preconditions = preconditions;
  vector<Tuple> substituted_results = results;
  Substitution sub = satisfaction_->substitution_;
  if (firing) sub.Add(firing->right_substitution_);
  sub.Substitute(&substituted_preconditions);
  sub.Substitute(&substituted_results);

  string ret;
  // ret += rule_sat_->satisfaction_->substitution_.ToString();
  ret += rule_->HTMLLink("r") + " " + HTMLLink("rs") + " " ;
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
    if (!substituted_results[i].HasVariables()) {
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
  CHECK(!(rule_sat_->GetFiring(right_substitution_)));
  rule_sat_->A1_AddFiring(right_substitution_, this);
  rule_sat_->ComputeSetLnLikelihood();
  // perform the substitution to find the TrueTuples.
  vector<Tuple> results = rule_sat->rule_->result_;
  GetFullSubstitution().Substitute(&results);
  // Now we have the vector of constant tuples which come true.
  for (uint i=0; i<results.size(); i++){
    TrueTuple * tp = model_->FindTrueTuple(results[i]);
    if (!tp)
      tp = new TrueTuple(model_, results[i]);
    true_tuples_.insert(tp); // Note: duplicate insertions OK.
  }
  // Note: we need two loops because of duplicates.
  forall(run, true_tuples_) { 
    (*run)->AddCause(this);
  }
  // For creative rules, this counts the names and adjusts the naming costs.
  forall (run, right_substitution_.sub_)  
    model_->L1_AddArbitraryTerm(run->second);
  ComputeSetTime();
  ComputeSetLnLikelihood();
}
void Firing::EraseSubclass() {
  forall(run, true_tuples_) {
    (*run)->A1_RemoveCause(this);
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
TrueTuple::TrueTuple(Model * model, 
					const vector<Firing *> & causes, 
					Tuple tuple,
					bool just_this, int id)
  :Component(model, id){
  causes_.insert(causes.begin(), causes.end());
  tuple_ = tuple;
  rule_encoded_ = NULL;
  CheckForbiddenRequired();
  given_ = false;
  forall(run, causes) {
    (*run)->true_tuples_.insert(this);
  }
  const Tuple * prop = model_->tuple_index_.Add(tuple);
  model_->index_to_true_tuple_[prop] = this;
  ComputeSetTime();
  model_->RecordAddComponent(this);
  vector<pair<Precondition *, pair<uint64, vector<Substitution> > > > satisfactions;
  model_->FindSatisfactionsForTuple(*prop, &satisfactions, -1, 
					  true, false);  
  for (uint i=0; i<satisfactions.size(); i++) {
    Precondition * precondition = satisfactions[i].first;
    uint64 num_subs = satisfactions[i].second.first;
    const vector<Substitution> & subs = satisfactions[i].second.second;
    precondition->AddToNumSatisfactions(num_subs);
    if (!just_this) {
      forall (run, precondition->negative_rules_) {
	CHECK(subs.size() == num_subs);
	for (uint j=0; j<subs.size(); j++) {
	  new RuleSat(*run, subs[j]);
	}	
      }
    }
  }
}

void Component::ComponentDestroy() {
  CHECK(StructuralDependents().size()==0);
  SetTime(NEVER, true); // will appropriately flip time dirty bit for dependents
  int id = id_;
  Model *  model = model_;
  model_->RecordRemoveComponent(this);
  model_->ln_likelihood_ -= ln_likelihood_;
  ln_likelihood_ = 0.0;
  MakeTimeClean();
  model_->never_happen_.erase(this);
  model_->required_never_happen_.erase(this);
  Destroy();
  model->ReleaseID(id);
}

void TrueTuple::Destroy(){
  if (required_) model_->absent_required_.insert(tuple_.Fingerprint());
  if (forbidden_) model_->present_forbidden_.erase(this);

  vector<pair<Precondition *, pair<uint64, vector<Substitution> > > >satisfactions;
  model_->FindSatisfactionsForTuple(tuple_, 
					  &satisfactions, -1, false, false);
  for (uint i=0; i<satisfactions.size(); i++) {
    satisfactions[i].first
      ->AddToNumSatisfactions(-satisfactions[i].second.first);
  }
  forall (run, causes_) {
    (*run)->true_tuples_.erase(this);
  }
  const Tuple * prop = model_->tuple_index_.FindTuple(tuple_);
  CHECK(prop);
  CHECK(model_->index_to_true_tuple_[prop] == this);
  model_->tuple_index_.Remove(tuple_);
  model_->index_to_true_tuple_.erase(prop);
}

ComponentType Precondition::Type() const { return PRECONDITION; }
ComponentType Satisfaction::Type() const { return SATISFACTION; }
ComponentType Rule::Type() const { return RULE; }
ComponentType RuleSat::Type() const { return RULESAT; }
ComponentType Firing::Type() const { return FIRING; }
ComponentType TrueTuple::Type() const 
{ return TRUETUPLE; }

string Component::TypeName() const { return ComponentTypeName[Type()]; }

string Component::HTMLLink(string text) const{
  return string() + "<a href=" + ComponentTypeName[Type()] 
    + ".html#" + itoa(id_) + ">" + text + "</a>";
}
Record Component::RecordForDisplay() const{
  Record r = RecordForDisplayInternal();
  if (time_dirty_) r["D"] = "DIRTY";
  r["ID"] = itoa(id_) + "<a name=\"" + itoa(id_) + "\">";
  r["TIME"] = time_.ToSortableString();
  r["LL"] = dtoa(ln_likelihood_) + " (" + dtoa(LnLikelihood()) + ")";
  return r;
}
Record Precondition::RecordForDisplayInternal() const{
  Record r;
  r["precondition"] = TupleVectorToString(pattern_);
  forall(run, rules_){
    r["rules"] += (*run)->HTMLLink(itoa((*run)->id_)) + " " 
      + TupleVectorToString((*run)->result_) + "<br>";
  }
  return r;
}
Record Rule::RecordForDisplayInternal() const{
  Record r;
  r["Rule"] = ImplicationString();
  r["Type"] = RuleTypeToString(type_).substr(0, 1);
  r["f/s"] = itoa(NumFirings()) + "/" + itoa(precondition_->num_satisfactions_);
  r["prec."] = delay_.ToSortableString();
  r["str."] = strength_.ToSortableString();
  r["str2."] = strength2_.ToSortableString();
  r["pat."] = precondition_->HTMLLink(itoa(precondition_->id_));
  if(type_==NEGATIVE_RULE) 
    r["target"] = target_rule_->HTMLLink(itoa(target_rule_->id_));
  vector<Firing *> f = Firings();
  for(uint i=0; i<f.size(); i++) {
    r["firngs"] += f[i]->ImplicationString() + "<br>\n";
  }
  return r;
}
Record Satisfaction::RecordForDisplayInternal() const{
  Record r;
  r["precondition"] = precondition_->HTMLLink(TupleVectorToString(precondition_->pattern_));
  r["substitution"] = substitution_.ToString();
  return r;
}
Record RuleSat::RecordForDisplayInternal() const{
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
Record Firing::RecordForDisplayInternal() const{
  Record r;
  r["rule_sat"] = rule_sat_->HTMLLink(itoa(rule_sat_->id_));
  r["implication"] = ImplicationString();
  forall(run, true_tuples_)
    r["true_tuples"] += (*run)->HTMLLink((*run)->tuple_.ToString());
  return r;
}
Record TrueTuple::RecordForDisplayInternal() const {
  Record r;
  r["Tuple"] = tuple_.ToString();
  forall(run, causes_) {
    Firing *f = *run;
    r["causes"] += f->ImplicationString();
  }
  set<Firing *> f = GetResultFirings();
  forall(run, f) r["firings"] += (*run)->ImplicationString() + "<br>\n";
  r["required"] = required_?"REQUIRED":"";
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
  if (rule_encoded_) ret.push_back(rule_encoded_);
  return ret;
}

vector<vector<Component *> > Component::TemporalCodependents() const{
  return vector<vector<Component *> >();
}
vector<vector<Component *> > Satisfaction::TemporalCodependents() const {
  vector<vector<Component *> > ret;
  ret.push_back(vector<Component *>(1, precondition_));
  forall(run, tuples_) {
    ret.push_back(vector<Component *>(1, *run));
  }
  return ret;
}
vector<vector<Component *> > Rule::TemporalCodependents() const{
  vector<vector<Component *> > ret;
  ret.push_back(vector<Component *>(1, precondition_));
  if (type_==NEGATIVE_RULE) ret.push_back(vector<Component *>(1, target_rule_));
  for (uint i=0; i<encoding_.size(); i++) 
    ret.push_back(vector<Component *>(1, encoding_[i]));
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
  if (!given_) 
    ret.push_back(vector<Component *>(causes_.begin(), causes_.end()));
  return ret;
}

vector<Component *> Component::Purposes() const{
  return vector<Component *>();
}
bool Component::HasPurpose() { return true; }
vector<Component *> Precondition::Purposes() const{
  return vector<Component *>(rules_.begin(), rules_.end());
}
bool Precondition::HasPurpose(){ return rules_.size(); }
vector<Component *> Satisfaction::Purposes() const{
  return vector<Component *>(rule_sats_.begin(), rule_sats_.end());
}
bool Satisfaction::HasPurpose(){ return rule_sats_.size(); }
vector<Component *> RuleSat::Purposes() const{
  vector<Firing*> v = VectorOfValues(firings_);
  vector<Component*> ret(v.begin(), v.end());
  ret.insert(ret.end(), inhibitors_.begin(), inhibitors_.end());
  return ret;
}
bool RuleSat::HasPurpose(){return firings_.size()||inhibitors_.size();}

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

void Component::ComputeSetTime(){
  L1_SetTimeMaintainConsistency(ComputeTime(NULL), true);
}
Time Component::ComputeTime(set<Component *> *excluded){
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
void Component::L1_SetTimeMaintainConsistency(Time new_time, 
					      bool adjust_dirty_bits){
  if (new_time == time_) { L1_MakeTimeDirty(false); return; }
  if (time_.IsNever()) {
    model_->A1_EraseFromNeverHappen(this);
    if (Type()==TRUETUPLE && ((TrueTuple*)this)->required_)
      model_->A1_EraseFromRequiredNeverHappen(this);
  }
  A1_SetTime(new_time);
  AdjustLnLikelihoodForNewTime();
  if (adjust_dirty_bits) {
    vector<Component *> dep = Dependents();
    for(uint i=0; i<dep.size(); i++) {
      if (dep[i]->ComputeTime(NULL) != dep[i]->time_)
	dep[i]->L1_MakeTimeDirty();
    }
    L1_MakeTimeClean();
  }
  if (time_.IsNever()){
    model_->A1_InsertIntoNeverHappen(this);
    if (Type()==TRUETUPLE && ((TrueTuple*)this)->required_)
      model_->A1_InsertIntoRequiredNeverHappen(this);
  }
}

void Component::L1_MakeTimeClean(){
  if (time_dirty_  == false) return;
  A1_SetTimeDirty(false);
  model_->A1_ReomveFromTimesDirty(this);
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
  double ret = precondition_ln_likelihood_;
  ret += num_satisfactions_ * ln_likelihood_per_sat_;
  return ret;
}
double Rule::LnLikelihood() const {
  double ret = EncodedNumberLnLikelihood(strength_);
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
void Component::ComputeSetLnLikelihood(){
  double old_val = ln_likelihood_;
  double new_val = ln_likelihood_;
  CHECK(finite(old_val));
  CHECK(finite(new_val));
  A1_SetLnLikelihood(new_val);
  model_->A1_SetLnLikelihood(model_->ln_likelihood_ + new_val - old_val);
}

void Component::CheckConnections(){
  vector<vector<Component *> > vv = TemporalCodependents();
  forall(run, vv) forall(run2, *run) {
    if (*run2 ==0) continue;
    CHECK((*run2)->Dependents() % this);
  }
  vector<Component *> v;
  v = Purposes();
  for (uint i=0; i<v.size(); i++) 
    CHECK(v[i]->Copurposes() % this);
  v = Copurposes();
  for (uint i=0; i<v.size(); i++) 
    CHECK(v[i]->Purposes() % this);  
}
