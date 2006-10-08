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


// Contains member functions of Component, ComponentEssentials classes
#include "model.h"
#include "probutil.h"
#include <sstream>
#include <fstream.h>
#include <math.h>

bool FLAGS_firing_proposition = false;

// COMPONENT
Model::Component::Component(Model * model, int id){
  model_ = model;
  if (id==-1) model_->AssignNewID(this);
  else model->AssignSpecificID(this, id);
  being_destroyed_ = false;
  ln_likelihood_ = 0.0;
  time_ = Time();
  MakeTimeDirty();  
}
Model::Component::~Component(){
}
Model::Precondition::Precondition(Model * model, 
				  const vector<Tuple> & tuples, int id)
  : Component(model, id){
  clauses_ = tuples;
  for (uint i=0; i<clauses_.size(); i++){
    model_->clause_to_precondition_
      [clauses_[i].MakeVariableInsensitive().Fingerprint()]
      .insert(make_pair(this, i));
  }
  uint64 fprint = Fingerprint(clauses_);
  CHECK(!(model_->precondition_index_ % fprint));
  model_->precondition_index_[fprint] = this;
  ComputeSetTime();
  // vector<int> arbitrary_terms;
  precondition_ln_likelihood_ = 0.0;
  //    = TuplesLnLikelihood(vector<Tuple>(), clauses_, &arbitrary_terms);
  //   for (uint i=0; i<arbitrary_terms.size(); i++) 
  //   model_->AddArbitraryTerm(arbitrary_terms[i]);
  ln_likelihood_per_sat_ = 0.0;
  uint64 num_sat;
  uint64 work;
  model_->tuple_index_.FindSatisfactions(clauses_, 0, &num_sat, -1, &work);
  num_satisfactions_ = num_sat;
  ComputeSetLnLikelihood();
  model_->RecordAddComponent(this);
}
Model::Precondition::~Precondition(){ ComponentDestroy(); }
void Model::Precondition::Destroy(){
  model_->precondition_index_.erase(Fingerprint(clauses_));
  for (uint i=0; i<clauses_.size(); i++){
    uint64 fp = clauses_[i].MakeVariableInsensitive().Fingerprint(); 
    model_->clause_to_precondition_[fp].erase(make_pair(this, i));
    if (model_->clause_to_precondition_[fp].size()==0) 
      model_->clause_to_precondition_.erase(fp);
  }
  //vector<int> arbitrary_terms;
  //TuplesLnLikelihood(vector<Tuple>(), clauses_, &arbitrary_terms);
  //for (uint i=0; i<arbitrary_terms.size(); i++) 
  //  model_->SubtractArbitraryTerm(arbitrary_terms[i]);
}
Model::Satisfaction::Satisfaction(Precondition * precondition, 
				  const Substitution & sub, int id)
  :Component(precondition->model_, id) {
  Satisfaction ** sp = precondition->satisfactions_ % sub.Fingerprint();
  CHECK(!sp);
  vector<Tuple> substituted_precondition = precondition->clauses_;
  sub.Substitute(&substituted_precondition);
  for (uint i=0; i<substituted_precondition.size(); i++) {
    for (uint j=0; j<substituted_precondition[i].size(); j++) {
      if (substituted_precondition[i][j] < 0) {
	cerr << "Bad satisfaction " << endl
	     << TupleVectorToString(precondition->clauses_) << endl
	     << sub.ToString() << endl
	     << TupleVectorToString(substituted_precondition) << endl;
	CHECK(false);
      }
    }
  }
  
  precondition->satisfactions_[sub.Fingerprint()] = this;
  precondition_ = precondition;
  substitution_ = sub;
  vector<Tuple> props = precondition->clauses_;
  sub.Substitute(&props);
  for (uint i=0; i<props.size(); i++) {
    const Tuple * tuple = model_->tuple_index_.FindTuple(props[i]);
    CHECK(tuple);
    TrueTuple * t = model_->index_to_true_proposition_[tuple];
    CHECK(t);
    // we need this if statement, since one proposition can be used twice.
    if (!(propositions_ % t)) { 
      propositions_.insert(t);
      t->satisfactions_.insert(this);
    }
  }
  ComputeSetTime();
  model_->RecordAddComponent(this);
}
Model::Satisfaction::~Satisfaction(){ ComponentDestroy(); }
void Model::Satisfaction::Destroy(){
  precondition_->satisfactions_.erase(substitution_.Fingerprint());
  forall(run, propositions_) {
    (*run)->satisfactions_.erase(this);
  }
}
Model::Rule::Rule(Precondition * precondition, EncodedNumber delay, 
		  RuleType type, Rule * target_rule,
		  vector<Tuple> result, EncodedNumber strength,
		  EncodedNumber strength2,
		  bool just_this, // don't add satisfactions or firing prop
		  bool no_firing_prop, int id) // don't add a firing proposition
  :Component(precondition->model_, id){
  precondition_ = precondition;
  precondition_->rules_.insert(this);
  if (type == NEGATIVE_RULE) precondition->negative_rules_.insert(this);
  target_rule_ = target_rule;
  if (type == NEGATIVE_RULE) CHECK(target_rule_);
  if (target_rule_) target_rule_->inhibitors_.insert(this);
  delay_ = delay;
  type_ = type;
  // add a firing proposition to the result if it is warranted
  if (FLAGS_firing_proposition && type != NEGATIVE_RULE && !just_this
      && !no_firing_prop) {
    string relation_name = "RULE_" + itoa(id_);
    set<int> vars = Union(GetVariables(result), 
			  GetVariables(precondition_->clauses_));
    Tuple firing_prop;
    firing_prop.push_back(LEXICON.GetAddID(relation_name));
    forall(run, vars) firing_prop.push_back(*run);
    result.push_back(firing_prop);
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
Model::Rule::~Rule(){
  model_->rule_index_.erase(RuleFingerprint());
  forall(run, encoding_) (*run)->rule_encoded_ = NULL;
  ComponentDestroy(); 
}
Model::RuleSat::RuleSat(Rule * rule, const Substitution & sub, int id) 
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
Model::RuleSat::~RuleSat(){ 
  ComponentDestroy();
}
Model::Firing::Firing(RuleSat * rule_sat, Substitution right_substitution,
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
Model::Firing::~Firing(){ 
  ComponentDestroy(); 
}
Model::TrueTuple::TrueTuple(Model * model, 
					const vector<Firing *> & causes, 
					Tuple proposition,
					bool just_this, int id)
  :Component(model, id){
  causes_.insert(causes.begin(), causes.end());
  proposition_ = proposition;
  rule_encoded_ = NULL;
  CheckForbiddenRequired();
  given_ = false;
  forall(run, causes) {
    (*run)->true_propositions_.insert(this);
  }
  const Tuple * prop = model_->tuple_index_.Add(proposition);
  model_->index_to_true_proposition_[prop] = this;
  ComputeSetTime();
  model_->RecordAddComponent(this);
  vector<pair<Precondition *, pair<uint64, vector<Substitution> > > > satisfactions;
  model_->FindSatisfactionsForProposition(*prop, &satisfactions, -1, 
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
Model::TrueTuple::~TrueTuple(){ 
  ComponentDestroy(); 
}
Model::ComponentEssentials::~ComponentEssentials(){}

void Model::Component::ComponentDestroy() {
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

void Model::RuleSat::Destroy() {
  satisfaction_->rule_sats_.erase(this);
  CHECK(rule_->rule_sats_ % satisfaction_);
  rule_->rule_sats_.erase(satisfaction_);
  forall(run, inhibitors_) {
    (*run)->target_rule_sat_ = 0;
  }
  if (target_rule_sat_) 
    target_rule_sat_->inhibitors_.erase(this);
}
void Model::Firing::Destroy(){
  while (true_propositions_.size()){
    TrueTuple * tp = *(true_propositions_.begin());
    //CHECK(tp->causes_.size() > 1);
    tp->RemoveCause(this);
  }
  rule_sat_->firings_.erase(right_substitution_.Fingerprint());
  rule_sat_->ComputeSetLnLikelihood();
  forall (run, right_substitution_.sub_)
    model_->SubtractArbitraryTerm(run->second);
}
void Model::Rule::Destroy(){
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
void Model::TrueTuple::Destroy(){
  if (required_) model_->absent_required_.insert(proposition_.Fingerprint());
  if (forbidden_) model_->present_forbidden_.erase(this);

  vector<pair<Precondition *, pair<uint64, vector<Substitution> > > >satisfactions;
  model_->FindSatisfactionsForProposition(proposition_, 
					  &satisfactions, -1, false, false);
  for (uint i=0; i<satisfactions.size(); i++) {
    satisfactions[i].first
      ->AddToNumSatisfactions(-satisfactions[i].second.first);
  }
  forall (run, causes_) {
    (*run)->true_propositions_.erase(this);
  }
  const Tuple * prop = model_->tuple_index_.FindTuple(proposition_);
  CHECK(prop);
  CHECK(model_->index_to_true_proposition_[prop] == this);
  model_->tuple_index_.Remove(proposition_);
  model_->index_to_true_proposition_.erase(prop);
}

void Model::Component::Renumber(int new_id, int revise_history_from){
  for (int i=revise_history_from; i<(int)model_->history_.size(); i++){
    if (model_->history_[i]->component_id_ == id_)
      model_->history_[i]->component_id_ = new_id;
  }
  if (id_==new_id) return;
  model_->id_to_component_.erase(id_);
  model_->id_to_component_[new_id] = this;
  id_ = new_id;
}

Model::ComponentType Model::Precondition::Type() const { return PRECONDITION; }
Model::ComponentType Model::Satisfaction::Type() const { return SATISFACTION; }
Model::ComponentType Model::Rule::Type() const { return RULE; }
Model::ComponentType Model::RuleSat::Type() const { return RULESAT; }
Model::ComponentType Model::Firing::Type() const { return FIRING; }
Model::ComponentType Model::TrueTuple::Type() const 
{ return TRUETUPLE; }
Model::ComponentType Model::Precondition::Essentials::Type() const 
{ return PRECONDITION; }
Model::ComponentType Model::Satisfaction::Essentials::Type() const 
{ return SATISFACTION; }
Model::ComponentType Model::Rule::Essentials::Type() const { return RULE; }
Model::ComponentType Model::RuleSat::Essentials::Type() const 
{ return RULESAT; }
Model::ComponentType Model::Firing::Essentials::Type() const 
{ return FIRING; }
Model::ComponentType Model::TrueTuple::Essentials::Type() const 
{ return TRUETUPLE; }

string Model::Component::TypeName(){ return ComponentTypeName[Type()]; }
string Model::ComponentEssentials::TypeName(){ 
  return ComponentTypeName[Type()]; }

Model::ComponentEssentials * Model::Precondition::ToEssentials() const{
  Essentials *e = new Essentials;
  e->id_ = id_;
  e->clauses_ = clauses_;
  return e;
}
Model::ComponentEssentials * Model::Satisfaction::ToEssentials() const{
  Essentials * e = new Essentials;
  e->id_ = id_;
  e->precondition_id_ = precondition_->id_;
  e->substitution_ = substitution_;
  return e;
}
Model::ComponentEssentials * Model::Rule::ToEssentials() const{
  Essentials * e = new Essentials;
  e->id_ = id_;
  e->precondition_id_ = precondition_->id_;
  e->delay_ = delay_;
  e->type_ = type_;
  e->result_ = result_;
  e->strength_ = strength_;
  e->strength2_ = strength2_;
  if (target_rule_) e->target_rule_id_ = target_rule_->id_;
  else e->target_rule_id_ = -1;
  return e;
}
Model::ComponentEssentials * Model::RuleSat::ToEssentials() const{
  Essentials * e = new Essentials;
  e->id_ = id_;
  e->rule_id_ = rule_->id_;
  e->satisfaction_id_ = satisfaction_->id_;
  return e;
}
Model::ComponentEssentials * Model::Firing::ToEssentials() const{
  Essentials * e = new Essentials;
  e->id_ = id_;
  e->rule_sat_id_ = rule_sat_->id_;
  e->right_substitution_ = right_substitution_;
  return e;
}
Model::ComponentEssentials * Model::TrueTuple::ToEssentials() const{
  Essentials * e = new Essentials;
  e->id_ = id_;
  e->proposition_ = proposition_;
  forall(run, causes_) e->cause_ids_.push_back((*run)->id_);
  return e;
}

Model::Component * Model::ComponentEssentials::AddToModel(Model *m){
  Checkpoint cp = m->MakeCheckpoint();
  Component * ret = AddToModelInternal(m);
  CHECK(ret);
  if (ret) ret->Renumber(id_, cp);
  return ret;
}
Model::Component * Model::Precondition::Essentials::AddToModelInternal(Model *m){
  return m->GetAddPrecondition(clauses_);
}
Model::Component * Model::Satisfaction::Essentials::AddToModelInternal(Model *m){
  Precondition *precondition = m->GetComponent<Precondition>(precondition_id_);
  CHECK(precondition);
  return precondition->GetAddSatisfaction(substitution_);
}
Model::Component * Model::Rule::Essentials::AddToModelInternal(Model *m){
  Precondition *precondition = m->GetComponent<Precondition>(precondition_id_);
  CHECK(precondition);
  Rule * target_rule = 0;
  if (type_ == NEGATIVE_RULE){
    target_rule = m->GetComponent<Rule>(target_rule_id_);
    CHECK(target_rule);
  }
  return new Rule(precondition, 
		  delay_,
		  type_,
		  target_rule,
		  result_, 
		  strength_,
		  strength2_,
		  true,
		  false,
		  id_);
}
Model::Component * Model::RuleSat::Essentials::AddToModelInternal(Model *m){
  Rule *rule = m->GetComponent<Rule>(rule_id_);
  CHECK(rule);
  Satisfaction *satisfaction = m->GetComponent<Satisfaction>(satisfaction_id_);
  CHECK(satisfaction);
  return rule->GetAddRuleSat(satisfaction);
}
Model::Component * Model::Firing::Essentials::AddToModelInternal(Model *m){
  RuleSat *rule_sat = m->GetComponent<RuleSat>(rule_sat_id_);
  CHECK(rule_sat);
  return new Firing(rule_sat, right_substitution_, true, id_);
}
Model::Component * Model::TrueTuple::Essentials::AddToModelInternal(Model *m){
  vector<Firing *> causes;
  forall (run, cause_ids_) {
    Firing * c = m->GetComponent<Firing>(*run);
    if (c) causes.push_back(c);
  }
  return new TrueTuple(m, causes, proposition_, true, id_);
}

string Model::Component::HTMLLink(string text) const{
  return string() + "<a href=" + ComponentTypeName[Type()] 
    + ".html#" + itoa(id_) + ">" + text + "</a>";
}
Record Model::Component::RecordForDisplay() const{
  Record r = RecordForDisplayInternal();
  if (time_dirty_) r["D"] = "DIRTY";
  r["ID"] = itoa(id_) + "<a name=\"" + itoa(id_) + "\">";
  r["TIME"] = time_.ToSortableString();
  r["LL"] = dtoa(ln_likelihood_) + " (" + dtoa(LnLikelihood()) + ")";
  return r;
}
Record Model::Precondition::RecordForDisplayInternal() const{
  Record r;
  r["precondition"] = TupleVectorToString(clauses_);
  forall(run, rules_){
    r["rules"] += (*run)->HTMLLink(itoa((*run)->id_)) + " " 
      + TupleVectorToString((*run)->result_) + "<br>";
  }
  return r;
}
Record Model::Rule::RecordForDisplayInternal() const{
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
Record Model::Satisfaction::RecordForDisplayInternal() const{
  Record r;
  r["precondition"] = precondition_->HTMLLink(TupleVectorToString(precondition_->clauses_));
  r["substitution"] = substitution_.ToString();
  return r;
}
Record Model::RuleSat::RecordForDisplayInternal() const{
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
Record Model::Firing::RecordForDisplayInternal() const{
  Record r;
  r["rule_sat"] = rule_sat_->HTMLLink(itoa(rule_sat_->id_));
  r["implication"] = ImplicationString();
  forall(run, true_propositions_)
    r["true_propositions"] += (*run)->HTMLLink((*run)->proposition_.ToString());
  return r;
}
Record Model::TrueTuple::RecordForDisplayInternal() const {
  Record r;
  r["Proposition"] = proposition_.ToString();
  forall(run, causes_) {
    Firing *f = *run;
    r["causes"] += f->ImplicationString();
  }
  set<Firing *> f = GetResultFirings();
  forall(run, f) r["firings"] += (*run)->ImplicationString() + "<br>\n";
  r["required"] = required_?"REQUIRED":"";
  return r;
}
 
Record Model::Component::ToRecord(){
  ComponentEssentials * e = ToEssentials();
  Record ret = e->ToRecord();
  delete e;
  return ret;
}
Record Model::ComponentEssentials::ToRecord(){
  Record r;
  r["id"] = itoa(id_);
  r["CT"] = TypeName();
  ToRecordInternal(r);
  return r;
}
void Model::Precondition::Essentials::ToRecordInternal(Record &r){
  r["clauses"] = TupleVectorToString(clauses_);
}
void Model::Satisfaction::Essentials::ToRecordInternal(Record &r){
  r["precondition"] = itoa(precondition_id_);
  r["substitution"] = substitution_.ToString();
}
void Model::Rule::Essentials::ToRecordInternal(Record &r){
  r["precondition"] = itoa(precondition_id_);
  r["delay"] = delay_.ToSortableString();
  r["rule_type"] = RuleTypeToString(type_);
  r["result"] = TupleVectorToString(result_);
  r["strength"] = strength_.ToSortableString();
  r["strength2"] = strength2_.ToSortableString();
  if (type_==NEGATIVE_RULE) {
    r["target_id"] = itoa(target_rule_id_);
  }
}
void Model::RuleSat::Essentials::ToRecordInternal(Record &r){
  r["rule"] = itoa(rule_id_);
  r["satisfaction"] = itoa(satisfaction_id_);
}
void Model::Firing::Essentials::ToRecordInternal(Record &r){
  r["rule_sat"] = itoa(rule_sat_id_);
  r["right_substitution"] = right_substitution_.ToString();
}
void Model::TrueTuple::Essentials::ToRecordInternal(Record &r){
  r["proposition"] = proposition_.ToString();
  r["causes"] = IntVectorToString(cause_ids_);
  if (given_) r["given"] = "true";
}

void Model::Precondition::Essentials::FromRecordInternal(Record r){  
  clauses_ = StringToTupleVector(r["clauses"]);
}
void Model::Satisfaction::Essentials::FromRecordInternal(Record r){  
  precondition_id_ = atoi(r["precondition"]);
  substitution_.FromString(r["substitution"]);
}
void Model::Rule::Essentials::FromRecordInternal(Record r){  
  precondition_id_ = atoi(r["precondition"]);
  delay_ = EncodedNumber(r["delay"]);
  type_ = StringToRuleType(r["rule_type"]);
  result_ = StringToTupleVector(r["result"]);
  strength_ = EncodedNumber(r["strength"]);
  strength2_ = EncodedNumber(r["strength2"]);
  if (type_ == NEGATIVE_RULE) target_rule_id_ = atoi(r["target_id"]);
}
void Model::RuleSat::Essentials::FromRecordInternal(Record r){
  rule_id_ = atoi(r["rule"]);
  satisfaction_id_ = atoi(r["satisfaction"]);
}
void Model::Firing::Essentials::FromRecordInternal(Record r){  
  rule_sat_id_ = atoi(r["rule_sat"]);
  right_substitution_.FromString(r["right_substitution"]);
}
void Model::TrueTuple::Essentials::FromRecordInternal(Record r){  
  proposition_.FromString(r["proposition"]);
  cause_ids_ = StringToIntVector(r["causes"]);
  if (r["given"]=="true") given_ = true;
}

vector<Model::Component *> Model::Component::Dependents(){
  return vector<Component *>();
}
vector<Model::Component *> Model::Precondition::Dependents(){
  vector<Satisfaction*> v = VectorOfValues(satisfactions_);
  vector<Component *> ret(v.begin(), v.end());
  ret.insert(ret.end(), rules_.begin(), rules_.end());
  return ret;
}
vector<Model::Component *> Model::Satisfaction::Dependents() {
  return vector<Component *>(rule_sats_.begin(), rule_sats_.end());
}
vector<Model::Component *> Model::Rule::Dependents(){
  vector<RuleSat*> v = VectorOfValues(rule_sats_);
  vector<Component *> ret(v.begin(), v.end());
  ret.insert(ret.end(), inhibitors_.begin(), inhibitors_.end());
  return ret;
}
vector<Model::Component *> Model::RuleSat::Dependents(){
  vector<Firing*> v = VectorOfValues(firings_);
  return vector<Component*>(v.begin(), v.end());
}
vector<Model::Component *> Model::Firing::Dependents(){
  vector<Component *> ret;
  forall(run, true_propositions_){
    ret.push_back(*run);
  }
  return ret;
}
vector<Model::Component *> Model::TrueTuple::Dependents(){
  vector<Component *> ret;
  ret.insert(ret.end(), satisfactions_.begin(), satisfactions_.end());
  if (rule_encoded_) ret.push_back(rule_encoded_);
  return ret;
}

vector<Model::Component *> Model::Component::HardDependents(){
  return Dependents();
}
vector<Model::Component *> Model::Firing::HardDependents(){
  return vector<Component *>();
}


vector<vector<Model::Component *> > Model::Component::Codependents(){
  return vector<vector<Component *> >();
}
vector<vector<Model::Component *> > Model::Satisfaction::Codependents() {
  vector<vector<Component *> > ret;
  ret.push_back(vector<Component *>(1, precondition_));
  forall(run, propositions_) {
    ret.push_back(vector<Component *>(1, *run));
  }
  return ret;
}
vector<vector<Model::Component *> > Model::Rule::Codependents(){
  vector<vector<Component *> > ret;
  ret.push_back(vector<Component *>(1, precondition_));
  if (type_==NEGATIVE_RULE) ret.push_back(vector<Component *>(1, target_rule_));
  for (uint i=0; i<encoding_.size(); i++) 
    ret.push_back(vector<Component *>(1, encoding_[i]));
  return ret;
}
vector<vector<Model::Component *> > Model::RuleSat::Codependents(){
  vector<vector<Component *> > ret;
  ret.push_back(vector<Component *>(1, satisfaction_));
  ret.push_back(vector<Component *>(1, rule_));
  return ret;
}
vector<vector<Model::Component *> > Model::Firing::Codependents(){
  vector<vector<Component *> > ret;
  ret.push_back(vector<Component *>(1, rule_sat_));
  return ret;
}
vector<vector<Model::Component *> > Model::TrueTuple::Codependents(){
  vector<vector<Component *> > ret;
  if (!given_) 
    ret.push_back(vector<Component *>(causes_.begin(), causes_.end()));
  return ret;
}

vector<Model::Component *> Model::Component::Purposes(){
  return vector<Component *>();
}
bool Model::Component::HasPurpose() { return true; }
vector<Model::Component *> Model::Precondition::Purposes(){
  return vector<Component *>(rules_.begin(), rules_.end());
}
bool Model::Precondition::HasPurpose(){ return rules_.size(); }
vector<Model::Component *> Model::Satisfaction::Purposes(){
  return vector<Component *>(rule_sats_.begin(), rule_sats_.end());
}
bool Model::Satisfaction::HasPurpose(){ return rule_sats_.size(); }
vector<Model::Component *> Model::RuleSat::Purposes(){
  vector<Firing*> v = VectorOfValues(firings_);
  vector<Component*> ret(v.begin(), v.end());
  ret.insert(ret.end(), inhibitors_.begin(), inhibitors_.end());
  return ret;
}
bool Model::RuleSat::HasPurpose(){return firings_.size()||inhibitors_.size();}

vector<Model::Component *> Model::Component::Copurposes(){
  return vector<Component *>();
}
vector<Model::Component *> Model::Rule::Copurposes(){
  vector<Component *> ret;
  ret.push_back(precondition_);
  return ret;  
}
vector<Model::Component *> Model::RuleSat::Copurposes(){
  vector<Component *> ret;
  ret.push_back(satisfaction_);
  if (target_rule_sat_) ret.push_back(target_rule_sat_);
  return ret;  
}
vector<Model::Component *> Model::Firing::Copurposes(){
  vector<Component *> ret;
  ret.push_back(rule_sat_);
  return ret;
}
vector<Model::Component *> Model::Component::RequiredCodependents() {
  vector<Component *> ret;
  vector<vector<Component *> > codep = Codependents();
  forall (run, codep) {
    const vector<Component *> & v = *run;
    CHECK(v.size() > 0);
    Component * c = v[0];
    for (uint j=1; j<v.size(); j++) {
      if (v[j]->time_ < c->time_) c = v[j];      
    }
    ret.push_back(c);
  }
  return ret;
}

bool Model::Component::NeedsPurpose(){ return false; }
bool Model::Precondition::NeedsPurpose() { return true; }
bool Model::Satisfaction::NeedsPurpose() { return true; }
bool Model::RuleSat::NeedsPurpose() {
  if (rule_->type_ == NEGATIVE_RULE) return false;
  return true;
}

void Model::Component::ComputeSetTime(){
  SetTime(ComputeTime(NULL), true);
}
Time Model::Component::ComputeTime(set<Component *> *excluded){
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
void Model::Component::SetTime(Time new_time, bool adjust_dirty_bits){
  if (new_time == time_) { MakeTimeClean(); return; }
  if (time_.IsNever()) {
    model_->never_happen_.erase(this);
    if (Type()==TRUETUPLE && ((TrueTuple*)this)->required_)
      model_->required_never_happen_.erase(this);
  }
  time_ = new_time;
  AdjustLnLikelihoodForNewTime();
  if (adjust_dirty_bits) {
    vector<Component *> dep = Dependents();
    for(uint i=0; i<dep.size(); i++) {
      if (dep[i]->ComputeTime(NULL) != dep[i]->time_)
	dep[i]->MakeTimeDirty();
    }
    MakeTimeClean();
  }
  if (time_.IsNever()){
    model_->never_happen_.insert(this);
    if (Type()==TRUETUPLE && ((TrueTuple*)this)->required_)
      model_->required_never_happen_.insert(this);
  }
}

void Model::Component::MakeTimeClean(){
  time_dirty_ = false;
  model_->times_dirty_.erase(this);
}
void Model::Component::MakeTimeDirty(){
  time_dirty_ = true;
  model_->times_dirty_.insert(this);
}
void Model::Component::AdjustLnLikelihoodForNewTime(){}
void Model::RuleSat::AdjustLnLikelihoodForNewTime(){
  if (rule_->type_ == NEGATIVE_RULE && target_rule_sat_){
    target_rule_sat_->ComputeSetLnLikelihood();
  }
  if (inhibitors_.size()) ComputeSetLnLikelihood();
}
double Model::Component::LnLikelihood() const {
  return 0.0;
}
double Model::Precondition::LnLikelihood() const {
  double ret = precondition_ln_likelihood_;
  ret += num_satisfactions_ * ln_likelihood_per_sat_;
  return ret;
}
double Model::Rule::LnLikelihood() const {
  double ret = rule_ln_likelihood_ 
    + EncodedNumberLnLikelihood(strength_);
  if (type_ == CREATIVE_RULE) ret += EncodedNumberLnLikelihood(strength2_);
  if (type_ != NEGATIVE_RULE)  ret += EncodedNumberLnLikelihood(delay_);
  return ret;
}
double Model::RuleSat::LnLikelihood() const {
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
void Model::Component::ComputeSetLnLikelihood(){
  double old_val = ln_likelihood_;
  double new_val = ln_likelihood_ = LnLikelihood();
  CHECK(finite(old_val));
  CHECK(finite(new_val));
  model_->ln_likelihood_ += new_val - old_val;
}

void Model::Component::CheckConnections(){
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
