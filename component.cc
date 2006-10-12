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
  L1_SetExists(true);
}
Component::~Component(){
  CHECK(exists_ == false);
}
void Component::L1_SetExists(bool val){
  CHECK(exists_ != val);
  model_->changelist_.Make(new ValueChange<bool>(&exists_, val));
}
void Component::L1_SetTime(const Time & new_time){
  if (time_ == new_time) return;
  model_->changelist_.Make(MakeValueChange(&time_, new_time));  
}
void Component::L1_SetTimeDirty(bool new_val){
  if (time_dirty_ == val) return;
  model_->changelist_.Make(new ValueChange<bool>(&time_dirty_, val));
}
void Component::L1_SetLnLikelihood(double new_ln_likelihood){
  if (new_ln_likelihood == ln_likelihood_) return;
  model_->changelist_.Make(new ValueChange<double>(&model_ln_, val));
}
void Component::Erase(){
  EraseSubclass();
  L1_RemoveFromModel;
}
void Component::L1_RemoveFromModel(){
  L1_SetExists(false);
  model_->L1_SetLnLikelihood(model_->ln_likelihood_ - ln_likelihood_);
}


Precondition::Precondition(Model * model, 
			   const vector<Tuple> & tuples, int id)
  : Component(model, id){
  clauses_ = tuples;
  ln_likelihood_per_sat_ = 0.0;
  num_satisfactions_ = 0;
  
  model_->L1_InsertIntoClauseToPreconditionMap(this);

  uint64 fprint = Fingerprint(clauses_);
  model_->changelist_.
    Make(new HashMapInsertChange<uint64, Precodition *>
	 (precondition_index_, fprint, this));
  ComputeSetTime();
  uint64 num_sat;
  uint64 work;
  model_->tuple_index_.FindSatisfactions(clauses_, 0, &num_sat, -1, &work);
  num_satisfactions_ = num_sat;
  ComputeSetLnLikelihood();
}

void Precondition::EraseSubclass(){
  L1_RemoveFromModel();
  model_->changelist_.
    Make(new HashMapRemoveChange<uint64, Precodition *>
	 (precondition_index_, fprint, this));
  model_->L1_RemoveFromModelClauseToPreconditionMap(this);
}

void L1_AddRule(Rule *r){
  model_->changelist_.Make(new SetInsertChange<Rule *>(&rules_, r));
}
void L1_RemoveRule(Rule *r){
  model_->changelist_.Make(new SetRemoveChange<Rule *>(&rules_, r));
}
void L1_AddNegativeRule(Rule *r){
  model_->changelist_.Make(new SetInsertChange<Rule *>(&negative_rules_, r));
}
void L1_RemoveNegatieveRule(Rule *r){
  model_->changelist_.Make(new SetRemoveChange<Rule *>(&negative_rules_, r));
}
void L1_AddSatisfaction(Satisfaction * sat){
  model_->changelist_.
    Make(new MapInsertChange<Uint64, Satisfaction *>
	 (&satisfactions_, sat->substitution_.Fingerprint(), sat));
}
void L1_RemoveSatisfaction(Satisfaction *sat){
  model_->changelist_.
    Make(new MapRemoveChange<uint64, Satisfaction *>
	 (&satisfactions_, sat->substitution_.Fingerprint()));
}
void L1_SetPreconditionLnLikelihood(double val){
  model_->changelist_.
    Make(new ValueChange<double>(&precondition_ln_likelihood_, val));
}
void Precondition::L1_SetLnLikelihoodPerSat(double val){
  model_->changelist_.
    Make(new ValueChange<double>(&ln_likelihood_per_sat_,val));
}

void Precondition::L1_AddToNumSatisfactions(int delta){
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
  vector<Tuple> substituted_precondition = precondition->clauses_;
  sub.Substitute(&substituted_precondition);
  for (uint i=0; i<substituted_precondition.size(); i++) {
    CHECK(!substituted_precondition[i].HasVariables());
    TrueTuple * t = model_->FindTrueTuple(substituted_precondition[i]);
    CHECK(t);
    CHECK(t->Exists());
    // we need this if statement, since the substituted precondition can 
    // have a repeated clause.
    if (!(true_tuples_ % t)) { 
      L1_AddTrueTuple(t);
      t->L1_AddSatisfaction(this);
    }
  }
  precondition_->L1_AddSatisfaction(this);
  ComputeSetTime();
}

void Satisfaction::EraseSubclass()
  precondition_->L1_RemoveSatisfaction(this);
  forall(run, true_tuples_) {    
    (*run)->L1_RemoveSatisfaction(this);
  }
}

void Satisfaction::L1_AddTrueTuple(TrueTuple *t){
  model_->changelist_.
    Make(new SetInsertChange<TruleTuple *>(&true_tuples, t));
}
void Satisfaction::L1_RemoveTrueTueple(TrueTuple *t){
  model_->changelist_.
    Make(new SetRemoveChange<TruleTuple *>(&true_tuples, t));
}
void Satisfaction::L1_AddRuleSat(RuleSat *rs){
  model_->changelist_.
    Make(new SetInsertChange<RuleSat *>(&rule_sats_, rs));
}
void Satisfaction::L1_RemoveRuleSat(RuleSat *rs){
  model_->changelist_.
    Make(new SetRemoveChange<RuleSat *>(&rule_sats_, rs));
}



Rule::Rule(Precondition * precondition, EncodedNumber delay, 
	   RuleType type, Rule * target_rule,
	   vector<Tuple> result, EncodedNumber strength,
	   EncodedNumber strength2,
	   bool just_this, // don't add satisfactions or firing prop
	   bool no_firing_tuple, int id) // don't add a firing tuple
  :Component(precondition->model_, id){
  precondition_ = precondition;
  precondition_->rules_.insert(this);
  if (type == NEGATIVE_RULE) precondition->negative_rules_.insert(this);
  target_rule_ = target_rule;
  if (type == NEGATIVE_RULE) CHECK(target_rule_);
  if (target_rule_) target_rule_->inhibitors_.insert(this);
  delay_ = delay;
  type_ = type;
  // add a firing tuple to the result if it is warranted
  if (FLAGS_firing_tupleosition && type != NEGATIVE_RULE && !just_this
      && !no_firing_tuple) {
    string relation_name = "RULE_" + itoa(id_);
    set<int> vars = Union(GetVariables(result), 
			  GetVariables(precondition_->clauses_));
    Tuple firing_tuple;
    firing_tuple.push_back(LEXICON.GetAddID(relation_name));
    forall(run, vars) firing_tuple.push_back(*run);
    result.push_back(firing_tuple);
  }
  result_ = result;
  for (uint i=0; i<result_.size(); i++){
    model_->clause_to_result_
      [result_[i].MakeVariableInsensitive().Fingerprint()]
      .insert(make_pair(this, i));
  }
  strength_ = strength;
  strength_d_ = strength.ToOpenInterval();
  strength2_ = strength2;
  strength2_d_ = strength2.ToOpenInterval();
  if (type != NEGATIVE_RULE) {
    precondition_->ln_likelihood_per_sat_ += log(1-strength_d_);
    precondition_->ComputeSetLnLikelihood();
  }
  uint64 fprint = RuleFingerprint();
  CHECK(!(model_->rule_index_ % fprint));
  model_->rule_index_[fprint] = this;
  ComputeSetTime();  
  //vector<int> arbitrary_terms;
  rule_ln_likelihood_ = 0.0;
  // =TuplesLnLikelihood(precondition_->clauses_, result_, &arbitrary_terms);
  //  for (uint i=0; i<arbitrary_terms.size(); i++) 
  //   model_->AddArbitraryTerm(arbitrary_terms[i]);
  vector<Tuple> encoding = ComputeEncoding();
  for (uint i=0; i<encoding.size(); i++) {
    TrueTuple * p = model_->GetAddTrueTuple(encoding[i]);
    encoding_.push_back(p);
    p->rule_encoded_ = this;
    // model_->MakeRequired(encoding[i]);
  }
  model_->RecordAddComponent(this);
  if (type == NEGATIVE_RULE) {
    vector<Substitution> substitutions;  
    uint64 num_sat;
    uint64 work;
    model_->tuple_index_.FindSatisfactions(precondition_->clauses_, 
					      &substitutions,
					      &num_sat, -1, &work);
    for (uint i=0; i<substitutions.size(); i++) {
      if (!just_this) {
	new RuleSat(this, substitutions[i]);
      }
    }
  }
  ComputeSetLnLikelihood();
}
Rule::~Rule(){
  model_->rule_index_.erase(RuleFingerprint());
  forall(run, encoding_) (*run)->rule_encoded_ = NULL;
  ComponentDestroy(); 
}
RuleSat::RuleSat(Rule * rule, const Substitution & sub, int id) 
  :Component(rule->model_, id) {
  rule_ = rule;
  satisfaction_ = rule_->precondition_->GetAddSatisfaction(sub);
  satisfaction_->rule_sats_.insert(this);
  target_rule_sat_ = NULL;
  rule->rule_sats_[satisfaction_] = this;
  model_->RecordAddComponent(this);
  if (rule->type_ == NEGATIVE_RULE) {
    Rule * target_rule = rule_->target_rule_;
    Precondition * target_precondition = target_rule->precondition_;
    Substitution restricted_sub 
      = sub.Restrict(GetVariables(target_precondition->clauses_));
    Satisfaction * target_sat = target_precondition->
      GetAddSatisfaction(restricted_sub);
    target_rule_sat_ = target_rule->GetAddRuleSat(target_sat);
    target_rule_sat_->inhibitors_.insert(this);
  }
  ComputeSetLnLikelihood();
  if (target_rule_sat_) target_rule_sat_->ComputeSetLnLikelihood();
  ComputeSetTime();
}
RuleSat::~RuleSat(){ 
  ComponentDestroy();
}
Firing::Firing(RuleSat * rule_sat, Substitution right_substitution,
		      bool just_this, int id) 
  :Component(rule_sat->model_, id){
  rule_sat_ = rule_sat;
  uint64 fprint = right_substitution.Fingerprint();
  CHECK(!(rule_sat->firings_ % fprint));
  rule_sat_->firings_[fprint] = this;
  rule_sat_->ComputeSetLnLikelihood();
  right_substitution_ = right_substitution;
  model_->RecordAddComponent(this);
  vector<Tuple> results = rule_sat->rule_->result_;
  Substitution & left_substitution_ref = rule_sat->satisfaction_->substitution_;
  for (uint i=0; i<results.size(); i++){
    left_substitution_ref.Substitute(&(results[i]));
    right_substitution.Substitute(&(results[i]));
    TrueTuple * tp = model_->FindTrueTuple(results[i]);
    if (tp) {
      tp->AddCause(this);
    } else {
      if (!just_this) {
	new TrueTuple(model_, 
			    vector<Firing *>(1, this), results[i], false);
      }
    }
  }
  ComputeSetTime();
  forall (run, right_substitution_.sub_)  model_->AddArbitraryTerm(run->second);
}
Firing::~Firing(){ 
  ComponentDestroy(); 
}
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
TrueTuple::~TrueTuple(){ 
  ComponentDestroy(); 
}

void Component::ComponentDestroy() {
  CHECK(HardDependents().size()==0);
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

void RuleSat::Destroy() {
  satisfaction_->rule_sats_.erase(this);
  CHECK(rule_->rule_sats_ % satisfaction_);
  rule_->rule_sats_.erase(satisfaction_);
  forall(run, inhibitors_) {
    (*run)->target_rule_sat_ = 0;
  }
  if (target_rule_sat_) 
    target_rule_sat_->inhibitors_.erase(this);
}
void Firing::Destroy(){
  while (true_tuples_.size()){
    TrueTuple * tp = *(true_tuples_.begin());
    //CHECK(tp->causes_.size() > 1);
    tp->RemoveCause(this);
  }
  rule_sat_->firings_.erase(right_substitution_.Fingerprint());
  rule_sat_->ComputeSetLnLikelihood();
  forall (run, right_substitution_.sub_)
    model_->SubtractArbitraryTerm(run->second);
}
void Rule::Destroy(){
  precondition_->rules_.erase(this);
  for (uint i=0; i<result_.size(); i++){
    uint64 fp = result_[i].MakeVariableInsensitive().Fingerprint(); 
    model_->clause_to_result_[fp].erase(make_pair(this, i));
    if (model_->clause_to_result_[fp].size()==0) 
      model_->clause_to_result_.erase(fp);
  }
  if (type_ != NEGATIVE_RULE) {
    precondition_->ln_likelihood_per_sat_ -= log(1-strength_d_);
    precondition_->ComputeSetLnLikelihood();
  }  
  if (type_ == NEGATIVE_RULE) {
    precondition_->negative_rules_.erase(this);
    target_rule_->inhibitors_.erase(this);
  }
  vector<int> arbitrary_terms;
  //TuplesLnLikelihood(precondition_->clauses_, result_, &arbitrary_terms);
  //for (uint i=0; i<arbitrary_terms.size(); i++) 
  //  model_->SubtractArbitraryTerm(arbitrary_terms[i]);
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
  r["precondition"] = TupleVectorToString(clauses_);
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
  r["precondition"] = precondition_->HTMLLink(TupleVectorToString(precondition_->clauses_));
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
 
vector<Component *> Component::Dependents() const{
  return vector<Component *>();
}
vector<Component *> Precondition::Dependents() const{
  vector<Satisfaction*> v = VectorOfValues(satisfactions_);
  vector<Component *> ret(v.begin(), v.end());
  ret.insert(ret.end(), rules_.begin(), rules_.end());
  return ret;
}
vector<Component *> Satisfaction::Dependents() const {
  return vector<Component *>(rule_sats_.begin(), rule_sats_.end());
}
vector<Component *> Rule::Dependents() const{
  vector<RuleSat*> v = VectorOfValues(rule_sats_);
  vector<Component *> ret(v.begin(), v.end());
  ret.insert(ret.end(), inhibitors_.begin(), inhibitors_.end());
  return ret;
}
vector<Component *> RuleSat::Dependents() const{
  vector<Firing*> v = VectorOfValues(firings_);
  return vector<Component*>(v.begin(), v.end());
}
vector<Component *> Firing::Dependents() const{
  vector<Component *> ret;
  forall(run, true_tuples_){
    ret.push_back(*run);
  }
  return ret;
}
vector<Component *> TrueTuple::Dependents() const{
  vector<Component *> ret;
  ret.insert(ret.end(), satisfactions_.begin(), satisfactions_.end());
  if (rule_encoded_) ret.push_back(rule_encoded_);
  return ret;
}

vector<vector<Component *> > Component::Codependents() const{
  return vector<vector<Component *> >();
}
vector<vector<Component *> > Satisfaction::Codependents() const {
  vector<vector<Component *> > ret;
  ret.push_back(vector<Component *>(1, precondition_));
  forall(run, tuples_) {
    ret.push_back(vector<Component *>(1, *run));
  }
  return ret;
}
vector<vector<Component *> > Rule::Codependents() const{
  vector<vector<Component *> > ret;
  ret.push_back(vector<Component *>(1, precondition_));
  if (type_==NEGATIVE_RULE) ret.push_back(vector<Component *>(1, target_rule_));
  for (uint i=0; i<encoding_.size(); i++) 
    ret.push_back(vector<Component *>(1, encoding_[i]));
  return ret;
}
vector<vector<Component *> > RuleSat::Codependents() const{
  vector<vector<Component *> > ret;
  ret.push_back(vector<Component *>(1, satisfaction_));
  ret.push_back(vector<Component *>(1, rule_));
  return ret;
}
vector<vector<Component *> > Firing::Codependents() const{
  vector<vector<Component *> > ret;
  ret.push_back(vector<Component *>(1, rule_sat_));
  return ret;
}
vector<vector<Component *> > TrueTuple::Codependents() const{
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
  SetTime(ComputeTime(NULL), true);
}
Time Component::ComputeTime(set<Component *> *excluded){
  Time ret;
  vector<vector<Component *> > codep = Codependents();
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
  if (new_time == time_) { L1_SetTimeDirty(false); return; }
  if (time_.IsNever()) {
    model_->L1_EraseFromNeverHappen(this);
    if (Type()==TRUETUPLE && ((TrueTuple*)this)->required_)
      model_->L1_EraseFromRequiredNeverHappen(this);
  }
  L1_SetTime(new_time);
  AdjustLnLikelihoodForNewTime();
  if (adjust_dirty_bits) {
    vector<Component *> dep = Dependents();
    for(uint i=0; i<dep.size(); i++) {
      if (dep[i]->ComputeTime(NULL) != dep[i]->time_)
	dep[i]->L1_SetTimeDirty(true);
    }
    L1_SetTimeDirty(false);
  }
  if (time_.IsNever()){
    model_->L1_InsertIntoNeverHappen(this);
    if (Type()==TRUETUPLE && ((TrueTuple*)this)->required_)
      model_->L1_InsertIntoRequiredNeverHappen(this);
  }
}

void Component::MakeTimeClean(){
  time_dirty_ = false;
  model_->times_dirty_.erase(this);
}
void Component::MakeTimeDirty(){
  time_dirty_ = true;
  model_->times_dirty_.insert(this);
}
void Component::AdjustLnLikelihoodForNewTime(){}
void RuleSat::AdjustLnLikelihoodForNewTime(){
  if (rule_->type_ == NEGATIVE_RULE && target_rule_sat_){
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
  double ret = rule_ln_likelihood_ 
    + EncodedNumberLnLikelihood(strength_);
  if (type_ == CREATIVE_RULE) ret += EncodedNumberLnLikelihood(strength2_);
  if (type_ != NEGATIVE_RULE)  ret += EncodedNumberLnLikelihood(delay_);
  return ret;
}
double RuleSat::LnLikelihood() const {
  if (rule_->type_ == NEGATIVE_RULE) return 0;
  double old_ln_likelihood = log(1-rule_->strength_d_);
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
    prob2 *= inhibition;
    new_ln_likelihood += log(prob2) * (num_firings-1) + log(1-prob2);
  }
  return new_ln_likelihood - old_ln_likelihood;
}
void Component::ComputeSetLnLikelihood(){
  double old_val = ln_likelihood_;
  double new_val = ln_likelihood_;
  CHECK(finite(old_val));
  CHECK(finite(new_val));
  L1_SetLnLikelihood(new_val);
  model_->L1_SetLnLikelihood(model_->ln_likelihood_ + new_val - old_val);
}

void Component::CheckConnections(){
  vector<vector<Component *> > vv = Codependents();
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
