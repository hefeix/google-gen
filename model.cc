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


// Contains functions declared in model.h, except those in component.cc 
// and optimize.cc
#include "model.h"
#include "probutil.h"
#include <sstream>
#include <deque>
#include <fstream.h>
#include <math.h>

char * Model::ComponentTypeName [] = {
    "PRECONDITION",
    "RULE",
    "SATISFACTION",
    "RULESAT",
    "FIRING",
    "TRUETUPLE",
};
Model::ComponentType Model::StringToComponentType(const string & s) {
  for (int i=0; i<NUM_COMPONENT_TYPES; i++) {
    if (s==ComponentTypeName[i]) return (ComponentType)i;
  }
  CHECK(false);
  return NUM_COMPONENT_TYPES;
}
string Model::ComponentTypeToString(ComponentType t) { 
  return ComponentTypeName[t]; 
}
char * Model::RuleTypeName [] = {
  "INVALID_RULE",
  "SIMPLE_RULE",
  "NEGATIVE_RULE",
  "CREATIVE_RULE",
};
Model::RuleType Model::StringToRuleType(const string & s) {
  for (int i=0; i<NUM_RULE_TYPES; i++) {
    if (s==RuleTypeName[i]) return (RuleType)i;
  }
  CHECK(false);
  return INVALID_RULE;
}
string Model::RuleTypeToString(RuleType t) { return RuleTypeName[t]; }

bool IsForbidden(const Tuple & t){
  for (GerneralizationIterator run_g(t); !run_g.done(); ++run_g) {
    set<Prohibition *> * prohibitions 
      = prohibition_index_ % run_g.generalized();
    if (prohibitions) forall(run_p, *prohibitions) {
      if ((*run_p)->TupleIsProhibited(t)) return true;
    }
  }
  return false;
}

void Model::L1_InsertIntoClauseToPreconditionMap(Precodition *p){
  for (uint i=0; i<p->clauses_.size(); i++){
    model_->changelist_.
      Make(new HashMapOfSetsInsertChange<uint64, pair<Precondition *, int>
	   (&model->clause_to_precondition_, 
	    p->clauses_[i].MakeVariableInsensitive().Fingerprint(), 
	    make_pair(p, i)));
  }
}
void Model::L1_RemoveFromClauseToPreconditionMap(Precondition *p){
  for (uint i=0; i<p->clauses_.size(); i++){
    model_->changelist_.
      Make(new HashMapOfSetsRemoveChange<uint64, pair<Precondition *, int>
	   (&model->clause_to_precondition_, 
	    p->clauses_[i].MakeVariableInsensitive().Fingerprint(), 
	    make_pair(p, i)));
  }
}
string TermEscape(string s){
  if (s[0]=='$' || s[0]=='_') return '_'+s;
  return s;
}
string TermUnescape(string s){
  if (s[0]=='_' && s.size() > 1) return s.substr(1);
  return s;
}
vector<Tuple> Model::ComputeTupleEncoding(const Tuple & s, int name){
  vector<Tuple> ret;
  for (uint i=0; i<s.size(); i++) {
    Tuple x;
    x.push_back(LEXICON.GetAddID("IN_POS"));
    x.push_back(name);
    x.push_back(LEXICON.GetAddID("POS_" + itoa(i)));
    x.push_back(LEXICON.GetAddID(TermEscape(LEXICON.GetString(s[i]))));
    ret.push_back(x);
  }
  return ret; 
}



set<Model::Firing *> Model::TrueTuple::GetResultFirings() const {
  set<Firing *> ret;
  forall(run_s, satisfactions_)
    forall(run_rs, (*run_s)->rule_sats_)
    forall(run_f, (*run_rs)->firings_)
    ret.insert(run_f->second);
  return ret;
}

set<Model::TrueTuple *> 
Model::TrueTuple::GetResultTrueTuples() const{
  set<TrueTuple *> ret;
  forall(run_s, satisfactions_)
    forall(run_rs, (*run_s)->rule_sats_)
    forall(run_f, (*run_rs)->firings_)
    forall(run_tp, run_f->second->true_tuples_)
    ret.insert(*run_tp);
  return ret;  
}

set<Model::TrueTuple *> 
Model::TrueTuple::GetCauseTrueTuples() const{
  set<TrueTuple *> ret;
  forall(run_f, causes_)
    forall(run_tp, (*run_f)->rule_sat_->satisfaction_->tuples_)
    ret.insert(*run_tp);
  return ret; 
}

// MODEL

Model::Model(){
  next_id_ = 0;
  ln_likelihood_ = 0.0;
  arbitrary_term_ln_likelihood_ = 0.0;
  total_arbitrary_terms_ = 0;
}

Model::~Model(){
}

Model::Component * Model::GetComponent(int id) const{
  Component *const* ret = id_to_component_%id;
  if (ret) return *ret;
  return 0;
}


void Model::L1_AssignNewID(Component * component) {
  CHECK(!(id_to_component_ % next_id_));
  Component->A1_SetID(next_id);
  A1_InsertIntoIDToComponent(next_id, component);
  A1_IncrementNextID();
}
void Model::L1_ReleaseID(int id) {
  A1_RemoveFromIDToComponent(id);
}

// if there are k distinct terms and n total terms, and the frequency of
// term i is f_i, and E is our favorite probabilitiy distribution over 
// non-negative integers then arbitrary_term_ln_likelihood_ is the log of:
// E(k) PROD[f_i! E(f_i-1)] k! / n!

// this is the change in arbitrary_term_ln_likelihood_ when adding 1 to f_i
double DProb(int f_i, // before 1 is added
	     int k, // including term i
	     int n) { // before 1 is added
  double d_prob = 0;
  d_prob += log(f_i+1) - log(n+1);
  d_prob += uintQuadraticLnProb(f_i+1-1);
  if (f_i > 0) d_prob -= uintQuadraticLnProb(f_i-1);
  if (f_i == 0) {
    d_prob += log(k);
    d_prob += uintQuadraticLnProb(k)-uintQuadraticLnProb(k-1);
  }
  CHECK(finite(d_prob));
  return d_prob;
}


void Model::L1_AddArbitraryTerm(int w){ // TODO: more to encode here.
  arbitrary_term_counts_[w]++;
  double d_prob = DProb(arbitrary_term_counts_[w]-1, 
			arbitrary_term_counts_.size(), 
			total_arbitrary_terms_);
  total_arbitrary_terms_++;
  arbitrary_term_ln_likelihood_ += d_prob;
  ln_likelihood_ += d_prob;
}
void Model::L1_SubtractArbitraryTerm(int w){
  arbitrary_term_counts_[w]--;
  total_arbitrary_terms_--;
  double d_prob = DProb(arbitrary_term_counts_[w], 
			arbitrary_term_counts_.size(),
			total_arbitrary_terms_);
  if (arbitrary_term_counts_[w]==0) arbitrary_term_counts_.erase(w);
  arbitrary_term_ln_likelihood_ -= d_prob;
  ln_likelihood_ -= d_prob;
}

vector<Model::Component *> 
Model::SortIntoLegalInsertionOrder(const set<Component *> & to_insert) {
  set<Component *> temp = to_insert;
  vector<Component *> ret;
  while (temp.size())
    SortIntoLegalInsertionOrderInternal(&temp, &ret, *temp.begin());
  return ret;
}

void Model::SortIntoLegalInsertionOrderInternal(set<Component *>* to_insert,
						vector<Component*>*result,
						Component * which){
  if (!( (*to_insert) % which)) return;
  to_insert->erase(which);
  vector<Component *> to_do_first = which->RequiredCodependents();
  forall(run, to_do_first) {
    if ((*to_insert) % (*run)){
      SortIntoLegalInsertionOrderInternal(to_insert, result, (*run));
    }
  }
  result->push_back(which);
}

// postcondition: no times dirty.
void Model::FixTimes() {
  multimap<Time, Component *> to_insert;
  //ToHTML("html");
  VLOG(1) << "Fix times " << times_dirty_.size() << " dirty " << endl;
  forall(run, times_dirty_){
    Component * c = *run;
    VLOG(1) << "   Inserting " << c->id_  
	    << " time=" << c->time_.ToSortableString() << endl;
    Time t = min(c->time_, c->ComputeTime(&times_dirty_));
    to_insert.insert(make_pair(t, c));
  }
  while (to_insert.size()){
    Component * c = to_insert.begin()->second;
    Time t = to_insert.begin()->first;
    if (t.IsNever()) break;
    Time new_time = c->ComputeTime(&times_dirty_);
    /*if (GetVerbosity() >= 3) {
      cerr << "id=" << c->id_ << " time=" << t.ToSortableString();
      vector<vector<Component *> > codep = c->TemporalCodependents();
      for (uint i=0; i<codep.size(); i++) {
	cerr << " ; ";
	for (uint j=0; j<codep[i].size(); j++) 
	  cerr << codep[i][j]->id_ << ",";
      }
      cerr << endl;
      cerr << "DIRTY: "; 
      forall (run, times_dirty_) 
	cerr << (*run)->id_ << " ";
      cerr << endl;
      }*/
    to_insert.erase(to_insert.begin());
    if (!c->time_dirty_) continue;
    if (c->time_ != new_time) c->SetTime(new_time, false);
    vector<Component *> dep = c->Dependents();
    bool solid = true;
    forall(run, dep) {
      Component * d = *run;
      if ( (d->time_ < new_time) && 
	   (d->ComputeTime(&times_dirty_) != d->time_)) solid = false;
    }
    if (solid) {
      c->MakeTimeClean();
    } else {
      to_insert.insert(make_pair(new_time, c));
    }
    forall(run, dep) {
      Component * d = *run;
      Time dep_time = d->ComputeTime(&times_dirty_);
      if (dep_time != d->time_) {
	d->MakeTimeDirty();
	VLOG(3) << "making dirty id=" << d->id_ 
		<< " time=" << d->time_.ToSortableString() << endl;
      }
      if (d->time_dirty_) {
	to_insert.insert(make_pair(min(dep_time, d->time_), d));
      }
    }
  }
  while(times_dirty_.size()){
    Component *c = *times_dirty_.begin();
    c->SetTime(NEVER, false);
    c->MakeTimeClean();
  }
  VLOG(1) << "fixed times " << times_dirty_.size() << " dirty" << endl;
  CHECK(times_dirty_.size()==0);
}

void Model::DeleteNeverHappeningComponents() {
  while (never_happen_.size()) {
    Component * c = *never_happen_.begin();
    if (!c->ComputeTime(&never_happen_).IsNever()){
      cerr << "ID: " << c->id_ << endl;
      ToHTML("html");
      CHECK(false);
    }
    KillComponent(c);
  }
}

void Model::KillComponent(Component * to_kill, bool just_this){
  to_kill->being_destroyed_ = true;
  vector<StablePtr<Component> > dep 
    = ToStablePtrVector(to_kill->HardDependents());
  for (uint i=0; i<dep.size(); i++) 
    if (dep[i].IsValid()) KillComponent(*dep[i]);
  vector<StablePtr<Component> > copurposes 
    = ToStablePtrVector(to_kill->Copurposes());
  delete to_kill;
  if (!just_this) {
    forall(run, copurposes) {
      if (run->IsValid() && (**run)->IsSuperfluous() && !(**run)->being_destroyed_)
	KillComponent(**run);
    }
  }

}
Record Model::ModelInfo() {
  Record r;
  r["Ln Likelihood"] = dtoa(ln_likelihood_);
  r["Ln likelihood(arbitrary terms)"] = dtoa(arbitrary_term_ln_likelihood_);
  r["absent required"] = itoa(absent_required_.size());
  r["present forbidden"] = itoa(present_forbidden_.size());
  string awc;
  forall(run, arbitrary_term_counts_) 
    awc += itoa(run->second) + " : " + LEXICON.GetString(run->first) + "<br>\n";
  r["Term Counts (arbitrary terms)"] = awc;
  return r;  
}
string Model::LinkBar(){
  string ret;
  for (int i=0; i<NUM_COMPONENT_TYPES; i++) {
    ret += string() + "<a href=" +ComponentTypeName[i] + ".html>"
      + ComponentTypeName[i] + "</a> ";
  }
  ret += "<br>";
  return ret;
}
void Model::ToHTML(string dirname){
  system(("mkdir " + dirname).c_str());
  ofstream output;
  output.open((dirname+"/model.html").c_str());
  output << LinkBar();
  output << RecordToHTMLTable(ModelInfo());
  output.close();
  
  map<ComponentType, vector<Record> > m;
  forall(run, id_to_component_) {
    Component * c = run->second;
    m[c->Type()].push_back(c->RecordForDisplay());
  }
  forall(run, m) {
    string type_name = ComponentTypeToString(run->first);
    output.open((dirname + "/" + type_name + ".html").c_str());
    output << LinkBar();
    output << "<h1>" << ComponentTypeToString(run->first) << "</h1>" 
	   << endl << RecordVectorToHTMLTable(run->second);
    output.close();
  }
}

void Model::StoreToFile(string filename){
  ofstream output(filename.c_str());
  Store(&output);
}
void Model::Store(ostream * output){
  FixTimes();
  (*output) << next_id_ << endl;
  set<Component *> to_insert;
  forall(run, id_to_component_) to_insert.insert(run->second);
  vector<Component *> store_order = SortIntoLegalInsertionOrder(to_insert);
  for (uint i=0; i<store_order.size(); i++) {
    Component * c = store_order[i];
    Record r = c->ToRecord();
    string s = RecordToString(r);
    (*output) << s << endl;
  }
}

void Model::CheckConnections(){
  forall(run, id_to_component_) run->second->CheckConnections();  
}

void Model::CheckLikelihood(){
  double total = arbitrary_term_ln_likelihood_;
  forall(run, id_to_component_){
    if (fabs(run->second->LnLikelihood() - run->second->ln_likelihood_) > 1e-6){
      cerr << "likelihood for component " << run->second->id_ << " out of date"
	   << " stored=" << run->second->LnLikelihood()
	   << " computed=" << run->second->ln_likelihood_
	   << endl;
      CHECK(false);
    }
    total += run->second->ln_likelihood_;
  }
  if (fabs(total-ln_likelihood_) > 1e-6) {
    cerr << " total likelihood out of date " << endl;
    CHECK(false);
  }
}
int64 Model::FindSatisfactionsForTuple
( const Tuple & s, 
  vector<pair<Precondition *, pair<uint64, vector<Substitution> > > > *results,
  int64 max_work,
  bool return_subs_for_negative_rules = true,
  bool return_subs_for_all_rules = false){
  DestructibleCheckpoint dcp(&changelist_);
  if (!tuple_index_.FindTuple(s))
    changelist_.Make
      (new MemberCallChange<TupleIndex, Tuple>(&model_->tuple_index_, tuple_,
					       &TupleIndex::AddWrapper,
					       &TupleIndex::RemoveWrapper));
  if (results) results->clear();
  int64 total_work = 0;
  for (GeneralizationIterator run_g(s); !run_g.done(); ++run_g) {
    const set<pair<Precondition *, int> > * preconditions
      = clause_to_precondition_ % run_g.generalized().Fingerprint();
    if (preconditions) forall(run, (*preconditions)) {
      Precondition * precondition = run->first;
      int clause_num = run->second;
      Substitution partial_sub;
      vector<Tuple> simplified_precondition = precondition->pattern_;
      // This can fail (gracefully) if the clause contains the same variable 
      // at two positions and the tuple doesn't.
      if (!ComputeSubstitution(precondition->clauses_[clause_num], s, 
			       &partial_sub)) continue;
      partial_sub.Substitute(&simplified_precondition);
      uint64 num_complete_subs;
      uint64 work = 0;
      bool return_subs = return_subs_for_all_rules;
      if (return_subs_for_negative_rules && 
	  (precondition->negative_rules_.size()>0)) {
	return_subs = true;
      }
      vector<Substitution> complete_subs;
      if (!tuple_index_.FindSatisfactions
	  (simplified_precondition, 
	   return_subs?(&complete_subs):NULL,
	   &num_complete_subs,
	   (max_work==-1)?-1:max_work-total_work,
	   &work)) return -1;
      if (num_complete_subs > 0 && results) {
	for (uint i=0; i<complete_subs.size(); i++) {
	  complete_subs[i].Add(partial_sub);
	}
	results->push_back(make_pair(precondition, make_pair(num_complete_subs, 
							complete_subs)));
      }
      total_work += work;
    }
  }
  return total_work;
}

Model::Precondition * Model::GetAddPrecondition(const vector<Tuple> & tuples) {
  Precondition ** p = precondition_index_ % Fingerprint(tuples);
  if (p) { return *p; }
  return new Precondition(this, tuples);
}

Model::TrueTuple * Model::FindTrueTuple(const Tuple & s) {
  const Tuple * tuple = tuple_index_.FindTuple(s);
  if (!tuple) return 0;
  TrueTuple ** tp = index_to_true_tuple_ % tuple;
  if (tp) return *tp;
  return 0;
}
Model::TrueTuple * Model::GetAddTrueTuple(const Tuple & s) {
  TrueTuple * ret = FindTrueTuple(s);
  if (ret) return ret;
  ret = new TrueTuple(this, vector<Firing *>(), s, false);
  return ret;
}

vector<Tuple> GetTupleVector(istream * input) {
  vector<Tuple> result;
  Tuple s;
  string line;
  while (GetLine(*input, &line) && line != "#") {
    if (StripWhiteEnds(line)=="") continue;
    s.FromString(line);
    result.push_back(s);
  }
  return result;
}

void Model::TrueTuple::AddCause(Firing * cause){
  if (cause != NULL) cause->true_tuples_.insert(this);
  causes_.insert(cause);
  ComputeSetTime();
}
void Model::TrueTuple::RemoveCause(Firing * cause){
  cause->true_tuples_.erase(this);
  causes_.erase(cause);
  // CHECK(causes_.size());
  ComputeSetTime(); // TODO: think about this
}

Model::Change::Change() {
  action_ = INVALID_ACTION;
  component_essentials_ = 0;
}
Model::Change::~Change(){
  if (component_essentials_) delete component_essentials_;
}
string Model::Change::ToString() const{
  ostringstream ostr;
  if (action_ == ADD_COMPONENT) ostr<< "ADD_COMPONENT " << component_id_;
  if (action_ == REMOVE_COMPONENT) 
    ostr<< "REMOVE_COMPONENT " 
	<< RecordToString(component_essentials_->ToRecord());
  if (action_ == CHANGE_STRENGTH) ostr<< "CHANGE_STRENGTH " << component_id_
				      << old_val_.ToSortableString()
				      << " " 
				      << old_val2_.ToSortableString();
  if (action_ == CHANGE_DELAY) ostr<< "CHANGE_DELAY " << component_id_
				   << old_val_.ToSortableString();
  return ostr.str();
}
void Model::RecordChange(Change * change){
  VLOG(2) << string(history_.size()%20, ' ') 
	  << "Pushing " << change->ToString() << endl;
  history_.push_back(change);
}
void Model::UndoChange(const Change & change){
  VLOG(2) << string(history_.size()%20, ' ') 
	  << "Popping " << change.ToString() << endl;
  switch(change.action_) {
  case ADD_COMPONENT: {
    cerr << "rolling back add component " << change.component_id_ << endl;//HERE
    Component * c = GetComponent(change.component_id_);
    if (!c) {
      //ToHTML("html");
      CHECK(c);      
    }
    KillComponent(c, true);
    break;
  }
  case REMOVE_COMPONENT: {
    cerr << "Rolling back remove component " 
	 << change.component_essentials_->id_ << endl;
    change.component_essentials_->AddToModel(this);
    break;
  }
  case CHANGE_STRENGTH: {
    Rule * r = GetComponent<Rule>(change.component_id_);
    CHECK(r);
    r->ChangeStrength(change.old_val_, change.old_val2_);
    break;
  }
  case CHANGE_DELAY: {
    Rule * r = GetComponent<Rule>(change.component_id_);
    CHECK(r);
    r->ChangeDelay(change.old_val_);
    break;
  }    
  default:
    CHECK(false);
    break;
  }
}
void Model::Rollback(Checkpoint checkpoint) {
  cerr << "Rolling back to " << checkpoint << endl;
  CHECK(checkpoint <= history_.size());
  while (checkpoint < history_.size()){
    uint i = history_.size()-1;
    Change * c = history_[i];
    history_.pop_back();
    UndoChange(*c);
    delete c;
    while (history_.size() >i) {
      delete history_.back(); 
      history_.pop_back();
    }
  }    
  cerr << "Rolled back to " << checkpoint << endl;
}
void Model::RecordAddComponent(const Component * c){
  Change *ch = new Change;
  ch->action_ = ADD_COMPONENT;
  ch->component_id_ = c->id_;
  RecordChange(ch);
  cerr << "ADD_COMPONENT " << c->id_ << endl;
}

void Model::RecordRemoveComponent(const Component * c){
  Change *ch = new Change;
  ch->action_ = REMOVE_COMPONENT;
  ch->component_essentials_ = c->ToEssentials();
  RecordChange(ch);
}
void Model::RecordChangeStrength(const Rule * r, 
				 EncodedNumber old_strength,
				 EncodedNumber old_strength2){
  Change *ch = new Change;
  ch->action_ = CHANGE_STRENGTH;
  ch->component_id_ = r->id_;
  ch->old_val_ = old_strength;
  ch->old_val2_ = old_strength2;
  RecordChange(ch);
}
void Model::RecordChangeDelay(const Rule * r, 
				   EncodedNumber old_delay){
  Change *ch = new Change;
  ch->action_ = CHANGE_DELAY;
  ch->component_id_ = r->id_;
  ch->old_val_ = old_delay;
  RecordChange(ch);
}

Model::ComponentEssentials * Model::ComponentEssentialsFromRecord(const 
								  Record & r){
  const string * ct = r % string("CT");
  const string * s_id = r % string("id");
  CHECK(ct);
  CHECK(s_id);
  ComponentType type = StringToComponentType(*ct);
  int id = atoi(s_id->c_str());
  ComponentEssentials *ret = 0;
  switch (type){
  case PRECONDITION: 
    ret = new Precondition::Essentials();
    break;
  case SATISFACTION:
    ret = new Satisfaction::Essentials();
    break;
  case RULE:
    ret = new Rule::Essentials();
    break;
  case RULESAT:
    ret = new RuleSat::Essentials();
    break;
  case FIRING:
    ret = new Firing::Essentials();
    break;
  case TRUETUPLE:
    ret = new TrueTuple::Essentials();
    break;
  default:
    CHECK(false);
    break;
  }
  ret->FromRecordInternal(r);
  ret->id_ = id;
  return ret;
}

Model::Component * Model::AddComponentFromRecord(Record r){
  ComponentEssentials * e = ComponentEssentialsFromRecord(r);
  Component * ret = e->AddToModel(this);
  delete e;
  return ret;
}


void Model::Load(istream * input) {
  (*input) >> next_id_;
  Record r;
  while ((*input) >> r) {
    AddComponentFromRecord(r);
  }
}
bool Model::IsRequired(const Tuple & s){
  return required_ % s.Fingerprint();
}
bool Model::IsForbidden(const Tuple & s){
  if (IsRequired(s)) return false;
  for (GeneralizationIterator run(s); !run.done(); ++run) {
    if (forbidden_ % run.generalized().Fingerprint()) 
      return true;
  }
  return false;
}
void Model::TrueTuple::CheckForbiddenRequired(){
  required_ = model_->IsRequired(tuple_);
  forbidden_ = model_->IsForbidden(tuple_);
  if (forbidden_) model_->present_forbidden_.insert(this);
  else model_->present_forbidden_.erase(this);
  if (required_) model_->absent_required_.erase(tuple_.Fingerprint());  
}
void Model::MakeRequired(const Tuple & s){
  required_[s.Fingerprint()] = s;
  TrueTuple * tp = FindTrueTuple(s);
  if (tp) {
    tp->CheckForbiddenRequired();
  } else {
    absent_required_.insert(s.Fingerprint());
  }
}
void Model::MakeNotRequired(const Tuple & s) {
  required_.erase(s.Fingerprint());
  TrueTuple * tp = FindTrueTuple(s);
  if (tp) {
    tp->CheckForbiddenRequired();
  }
  absent_required_.erase(s.Fingerprint());
}
void Model::MakeForbidden(const Tuple & s){
  forbidden_[s.Fingerprint()] = s; 
  vector<const Tuple*> results;
  tuple_index_.Lookup(s, &results);
  for (uint i=0; i<results.size(); i++) {
    if (IsRequired(*(results[i]))) continue;
    index_to_true_tuple_[results[i]]->CheckForbiddenRequired();
  }
}
void Model::MakeNotForbidden(const Tuple & s){
  forbidden_.erase(s.Fingerprint());
  vector<const Tuple*> results;
  tuple_index_.Lookup(s, &results);
  for (uint i=0; i<results.size(); i++) {
    if (IsRequired(*(results[i]))) continue;
    index_to_true_tuple_[results[i]]->CheckForbiddenRequired();
  }
}
void Model::MakeGiven(const Tuple & s){
  TrueTuple *tp = FindTrueTuple(s);
  if (!tp) tp = new TrueTuple(this, vector<Firing *>(), s, false);
  tp->given_ = true;
  tp->ComputeSetTime();
}
void Model::
ReadSpec(istream * input){
  string w;
  while ((*input) >> w) {
    if (w != "[") continue;
    string required = "[ ";
    string forbidden = "[ ";
    bool any_wildcards = false;
    bool any_pure_wildcards = false;
    while ((*input) >> w) {
      if (w == "]") break;
      if (w == "*") {
	any_pure_wildcards = true;
	forbidden += "$0 ";
      } else if (w[0]=='*') {
	any_wildcards = true;
	required += w.substr(1) + " ";
	forbidden += "$0 ";
      } else {
	required += w + " ";
	forbidden += w + " ";
      }
    }
    required += "]";
    forbidden += "]";
    if (any_wildcards && any_pure_wildcards) {
      CHECK(false);
    }
    if (!any_pure_wildcards) {
      Tuple r;
      r.FromString(required);
      MakeRequired(r);
    }
    if (any_wildcards) {
      Tuple f;
      f.FromString(forbidden);
      MakeForbidden(f);
    }
  }
}

Model::Rule * Model::GetAddNaiveRule(int length) {
  vector<Tuple> precondition;
  vector<Tuple> result(1);
  vector<Tuple> target_precondition;
  for (int i=0; i<length; i++) result[0].terms_.push_back(-1-i);
  Rule ** rp = rule_index_ 
    % RuleFingerprint(CREATIVE_RULE, precondition, result, 
		      target_precondition);
  if (rp) return *rp;
  Rule * r = new Rule(GetAddPrecondition(precondition), 
		      EncodedNumber(), CREATIVE_RULE, 0, 
		      result, EncodedNumber(), EncodedNumber(), 
		      false, true);
  forall(run, r->encoding_) {
    MakeGiven((*run)->tuple_);
    (*run)->ComputeSetTime();
  }
  r->ComputeSetTime();
  return r;
}

bool Model::IsLayer3() const{
  if (violated_prohibitions_.size()) return false;
  if (times_dirty_.size()) return false;
  return true;
}

void Model::Shell(istream  * input) {
  string line;
  string command;
  string subset = "ALL";
  map<string, Checkpoint> checkpoints;
  cout << "?";
  while ((*input) >> command) {
    if (command == "q") break;
    else if (command=="id") {
      string s;
      (*input) >> s;
      cout << LEXICON.GetAddID(s) << endl;
    }
    else if (command == "Add") {
      Record r;
      (*input) >> r;
      CHECK(AddComponentFromRecord(r));
    }
    else if (command == "RemoveID") {
      int id;
      (*input) >> id;
      KillComponent(GetComponent(id));
    }
    else if (command == "subset") {
      GetLine((*input), &subset);      
    }
    else if (command=="store") {
      string filename;
      (*input) >> filename;
      StoreToFile(filename);
    }
    else if (command=="load") {
      string filename;
      (*input) >> filename;
      ifstream finput(filename.c_str());
      Load(&finput);
    }
    else if (command == "check") {
      CheckConnections();
      CheckLikelihood();
    }
    else if (command == "checkpoint") {
      string cname;
      (*input) >> cname;
      checkpoints[cname] = MakeCheckpoint();
    }
    else if (command == "rollback") {
      string cname;
      (*input) >> cname;
      Rollback(checkpoints[cname]);
    }
    else if (command == "spec") {
      string fname;
      (*input) >> fname;
      ifstream finput(fname.c_str());
      ReadSpec(&finput);
      finput.close();
      FulfillRequirements();
    }
    else if (command == "strength") {
      int id;
      (*input) >> id;
      GetComponent<Rule>(id)->OptimizeStrength();
    }
    else if (command == "o") {
      OptimizeRound();
    }
    else if (command == "i") {
      int tactic;
      (*input) >> tactic;
      int duration;
      (*input) >> duration;
      time_t end_time = time(0) + duration;
      while (time(0) < end_time) {
	pair<vector<Tuple>, vector<Tuple> > p 
	  = FindRandomCandidateRule(Tactic(tactic));
	TryAddImplicationRule(p.first, p.second, REQUIRE_BETTER, true);
      }
    }
    else if (command == "ispecific"){
      string pat;
      GetLine((*input), &pat);
      vector<Tuple> preconditions = StringToTupleVector(pat);
      GetLine((*input), &pat);
      vector<Tuple> result = StringToTupleVector(pat);
      TryAddImplicationRule(preconditions, result, REQUIRE_BETTER, true);
    } 
    else if (command=="rs"){ // random tuple
      string l;
      GetLine((*input), &l);
      istringstream istr(l);
      vector<int> terms;      
      string w;
      while (istr >> w){
	int wid;
	CHECK(LEXICON.GetID(w, &wid));
	terms.push_back(wid);
      }
      for (int i=0; i<10; i++) {
	const Tuple * s 
	  = tuple_index_.GetRandomTupleContaining(terms, true);
	if(s) {
	  cout << s->ToString() << endl;
	}
      }
    }
    else if (command=="v") {
      int v;
      (*input) >> v;
      SetVerbosity(v);
    }
    else if (command=="pat"){
      /*pair<vector<Tuple>, vector<Tuple> > p = FindRandomCandidateRule();
      cout << TupleVectorToString(p.first) << " -> " 
      << TupleVectorToString(p.second) << endl;*/
    }
    else if (command=="h"){
      ToHTML("html");
    }
    else cerr << "UNKNOWN COMMAND " << command << endl;
    FixTimes();
    //ToHTML("/Users/guest/tmp/model.html");
    cout << "?";
  }
}


 
