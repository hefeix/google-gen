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

char * ComponentTypeName [] = {
    "PRECONDITION",
    "RULE",
    "SATISFACTION",
    "RULESAT",
    "FIRING",
    "TRUETUPLE",
};
ComponentType StringToComponentType(const string & s) {
  for (int i=0; i<NUM_COMPONENT_TYPES; i++) {
    if (s==ComponentTypeName[i]) return (ComponentType)i;
  }
  CHECK(false);
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
  for (int i=0; i<NUM_RULE_TYPES; i++) {
    if (s==RuleTypeName[i]) return (RuleType)i;
  }
  CHECK(false);
  return INVALID_RULE;
}
string RuleTypeToString(RuleType t) { return RuleTypeName[t]; }

// ----- LAYER 3 FUNCTIONS -----



// ----- LAYER 2 FUNCTIONS -----

TrueTuple * AddRequirementToSpec(Tuple t){
  TrueTuple *tt = GetAddTrueTuple(t);
  CHECK(!(spec_requirements_ % tt));
  A1_InsertIntoSpecRequirements(tt);
  tt->A1_MakeRequired();
  if (t->time_ == NEVER && !(required_never_happen_ % tt)) {
    A1_InsertIntoRequiredNeverHappen(tt);
  }
  return tt;
}
Prohibition * AddProhibitionToSpec(Tuple t, vector<Tuple> exceptions){
  Prohibition *p = L1_MakeProhibition(this, t);
  A1_InsertIntoSpecProhibitions(p);
  for (int i=0; i<exceptions.size(); i++) {
    p->L1_AddException(exceptions[i]);
  }
  return p;
}

void Model::ReadSpec(istream * input){
  string w;
  while ((*input) >> w) {
    if (w != "[") continue;
    string required = "[ ";
    string forbidden = "[ ";
    bool any_wildcards = false;
    while ((*input) >> w) {
      if (w == "]") break;
      if (w[0]=='*') {
	CHECK(w.size() > 1);
	any_wildcards = true;
	required += w.substr(1) + " ";
	forbidden += "* ";
      } else {
	required += w + " ";
	forbidden += w + " ";
      }
    }
    required += "]";
    forbidden += "]";
    Tuple r;
    r.FromString(required);
    AddRequirementToSpec(r);
    if (any_wildcards) {
      Tuple f;
      f.FromString(forbidden);
      AddProhibitionToSpec(f, vector<Tuple>(1, r));
    }
  }
}

bool IsForbidden(const Tuple & t) const{
  for (GerneralizationIterator run_g(t); !run_g.done(); ++run_g) {
    set<Prohibition *> * prohibitions 
      = prohibition_index_ % run_g.generalized();
    if (prohibitions) forall(run_p, *prohibitions) {
      if ((*run_p)->TupleIsProhibited(t)) return true;
    }
  }
  return false;
}
void Model::IsLayer3() const{
  if (times_dirty_.size()) return false;
  if (never_happen_.size()) return false;
  if (violated_prohibitions_.size()) return false;
  return true;
}
// FixTime may be able to make the model Layer 3
void Model::MayBeTimeFixable() const{
  // TODO: think about this.
  if (violated_prohibitions_.size()) return false;
  if (required_never_happen_.size()) return false;
  return true;
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
  if (s[0]=='*' || s[0]=='_') return '_'+s;
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



set<Firing *> TrueTuple::GetResultFirings() const {
  set<Firing *> ret;
  forall(run_s, satisfactions_)
    forall(run_rs, (*run_s)->rule_sats_)
    forall(run_f, (*run_rs)->firings_)
    ret.insert(run_f->second);
  return ret;
}

set<TrueTuple *> 
TrueTuple::GetResultTrueTuples() const{
  set<TrueTuple *> ret;
  forall(run_s, satisfactions_)
    forall(run_rs, (*run_s)->rule_sats_)
    forall(run_f, (*run_rs)->firings_)
    forall(run_tp, run_f->second->true_tuples_)
    ret.insert(*run_tp);
  return ret;  
}

set<TrueTuple *> 
TrueTuple::GetCauseTrueTuples() const{
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

Component * Model::GetComponent(int id) const{
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
  A1_AddToArbitraryTermCounts(w, 1);
  A1_AddToTotalArbitraryTerms(1);
  double d_prob = DProb(arbitrary_term_counts_[w]-1, 
			arbitrary_term_counts_.size(), 
			total_arbitrary_terms_-1);
  A1_AddToArbitraryTermLnLikelihood(d_prob);
  A1_AddToLnLikelihood(d_prob);
}
void Model::L1_SubtractArbitraryTerm(int w){
  double d_prob = DProb(arbitrary_term_counts_[w]-1, 
			arbitrary_term_counts_.size(),
			total_arbitrary_terms_-1);
  A1_AddToArbitraryTermCounts(w, -1);
  A1_AddToTotalArbitraryTerms(-1);
  A1_AddToArbitraryTermLnLikelihood(-d_prob);
  A1_AddToLnLikelihood(-d_prob);
}

// postcondition: no times dirty.
void Model::FixTimes() {
  // TODO: Figure out how and why the heck this works.  

  // Within this function, we call ComputeTime using times_dirty_ as the 
  // excluded set.  This means that we pretend that the components which are
  // dirty happen NEVER.   It seems kind of scary that we aren't keeping track
  // of which definition was used to set the time and the dirty flag for each
  // component.  It's OK though, since it is always one of the two definitions,
  // and the two definitions are identical by the end of the function when
  // all of the times are clean.  

  // We have a queue containing all of the dirty components.  Each dirty
  // component is indexed by the min of its current time and its correct time.

  // We don't bother deleting obselete entries from the queue.  It might be 
  // more understandable if we did.  We would also have to keep a 
  // map<Component *, Time> so as to find and delete any old entries for a
  // component when inserting a new entry for that component. 
  multimap<Time, Component *> queue;

  // We initially build the queue.
  forall(run, times_dirty_){
    Component * c = *run;
    VLOG(1) << "   Inserting " << c->id_  
	    << " time=" << c->time_.ToSortableString() << endl;
    Time t = min(c->time_, c->ComputeTime(&times_dirty_));
    queue.insert(make_pair(t, c));
  }

  // We repeatedly deal with the first element of the queue, that is, the 
  // one that is indexed by the earliest time.
  while (queue.size()){
    Component * c = queue.begin()->second;
    Time t = queue.begin()->first;
    // If the time is NEVER, then for all things remaining in the queue, both
    // the current time and the correct time must be NEVER, and we are done.
    if (t.IsNever()) break;
    queue.erase(queue.begin());
    // This queue entry may be no longer valid, so the time might be clean.
    // I think it would make this function much more understandable if we 
    // ignored the entry in all cases when it is obselete.  That way, the 
    // only two cases we would need to deal with  would be moving the time 
    // backwards to the entry time, and moving it forwards from the entry time. 
    // Let's wait until we have everything running before f@#$ing with it.  
    if (!c->time_dirty_) continue;

    Time new_time = c->ComputeTime(&times_dirty_);
    // Set the time for this component.  
    if (c->time_ != new_time) c->L1_SetTimeMaintainIndices(new_time, false);

    // Now we may want to make this component clean and we may not.  Why on
    // earth not, you ask?  Because if there is a circular dependency, making 
    // the time clean when pushing it forward may result in an infinite loop
    // in this function.   A better policy would be to make the time clean when
    // moving the time backwards to the entry time and leaving it dirty when 
    // moving it forwards from the entry time.   I think this can be proven 
    // to terminate.   I tried to do something more clever here, but I can't 
    // remember the reasoning, so it may not be correct.  

    bool solid = true;
    vector<Component *> dep = c->Dependents();
    forall(run, dep) {
      Component * d = *run;
      if ( (d->time_ < new_time) && 
	   (d->ComputeTime(&times_dirty_) != d->time_)) solid = false;
    }
    if (solid) {
      c->L1_MakeTimeClean();
    } else {
      queue.insert(make_pair(new_time, c));
    }

    // Now we need to figure out which of the dependents are now dirty, and 
    // make sure they are in the queue.  
    forall(run, dep) {
      Component * d = *run;
      Time dep_time = d->ComputeTime(&times_dirty_);
      if (dep_time != d->time_) d->L1_MakeTimeDirty();
      if (d->time_dirty_) {
	queue.insert(make_pair(min(dep_time, d->time_), d));
      }
    }
  }
  // Now that all of the dirty stuff happens NEVER, we can make it clean.  
  // TODO, is this really necessary, or could we have just let the previous 
  // loop run until the queue was empty?
  while(times_dirty_.size()){
    Component *c = *times_dirty_.begin();
    c->L1_SetTimeMaintainConsistency(NEVER, false);
    c->L1_MakeTimeClean();
  }
  CHECK(times_dirty_.size()==0);
}

void Model::DeleteNeverHappeningComponents() {
  CHECK(times_dirty_.size()==0);
  while (never_happen_.size()) {
    Component * c = *never_happen_.begin();
    c->L1_Erase();
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

void Model::VerifyLikelihood() const{
  double total = arbitrary_term_ln_likelihood_;
  forall(run, id_to_component_){
    total += run->second->ln_likelihood_;
  }
  if (fabs(total-ln_likelihood_) > 1e-6) {
    cerr << " total likelihood out of date " << endl;
    CHECK(false);
  }
}
void Model::VerifyLinkBidirectionality(){
  set<pair<Component *, Component *> > td1, td2, p1, p2;
  forall(run, id_to_component_){
    Component *c = run->second;
    vector<Component *> p = c->Purposes();
    for (int i=0; i<p.size(); i++) {
      p1.insert(make_pair(c, p[i]));
    }
    vector<Component *> cp = c->Copurposes();
    for (int i=0; i<cp.size(); i++) {
      p2.insert(make_pair(cp[i], c));
    }
    vector<Component *> td = TemporalDependents();
    for (int i=0; i<td.size(); i++) {
      td1.insert(make_pair(c, td[i]));
    }
    vector<vector<Component *> > tcd = TemporalCodependents();
    for (int i=0; i<tcd.size(); i++) for (int j=0; j<tcd[i].size(); j++) {
      td2.insert(make_pair(tcd[i][j], c));
    }
  }  
  CHECK(td1==td2);
  CHECK(p1==p2);
}
void Model::VerifyIndices() const {
  // TODO: implement this
}
void Model::VerifyLayer2() const {
  VerifyLikelihood();
  VerifyLinkBidirectionality();
  VerifyIndices();
  forall(run, id_to_component_) {
    Component *c = run->second;
    c->VerifyLayer2();
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
	   (max_work==UNLIMITED_WORK)?UNLIMITED_WORK:max_work-total_work,
	   &work)) return GAVE_UP;
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

int64 Model::FindExplanationsForResult
(const Tuple & s, vector<pair<Rule *, Substitution> > *results, 
 set<Component *> *excluded_dependents, int64 max_work){
  CHECK(results);
  int64 total_work = 0;
  for (GeneralizationIterator run_g(s); !run_g.done(); ++run_g) {
    const set<pair<Rule *, int> > * rules
      = clause_to_result_ % run_g.generalized().Fingerprint();
    if (rules) forall(run, (*rules)) {
      Rule * rule = run->first;
      if (excluded_dependents && 
	  ((*excluded_dependents) % ((Component*)rule))) continue;
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
	   (max_work==UNLIMITED_WORK)?UNLIMITED_WORK:max_work-total_work,
	   &work)) return GAVE_UP;
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
	    TrueTuple *prop = FindTrueTuple(*run);
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



Precondition * Model::FindPrecondition(const vector<Tuple> & tuples) const{
  Precondition ** p = precondition_index_ % tuples;
  if (p) return *p; 
  else return NULL;
}
Precondition * Model::L1_GetAddPrecondition(const vector<Tuple> & tuples) {
  Precondition * p = FindPrecondition(tuples);
  if (p) { return *p; }
  else return new Precondition(this, tuples);
}

TrueTuple * Model::FindTrueTuple(const Tuple & s) const {
  const Tuple ** tuple = tuple_to_true_tuple_ % s;
  if (tuple) return *tuple;
  return NULL;
}
TrueTuple * Model::GetAddTrueTuple(const Tuple & s) {
  TrueTuple * ret = FindTrueTuple(s);
  if (ret) return ret;
  ret = new TrueTuple(this, s);
  return ret;
}

Rule * FindPositiveRule(vector<Tuple> precondition, vector<Tuple> result)
  const{
  Precondition *p = FindPrecondition(precondition);
  if (!p) return NULL;
  return p->FindPositiveRule(result);
}
Rule * FindNegativeRule(vector<Tuple> precondition, Rule * target_rule) const{
  Precondition *p = FindPrecondition(precondition);
  if (!p) return NULL;
  return p->FindNegativeRule(target_rule);
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

Rule * Model::GetAddNaiveRule(int length) {
  vector<Tuple> precondition;
  vector<Tuple> result(1);
  vector<Tuple> target_precondition;
  for (int i=0; i<length; i++) result[0].terms_.push_back(Variable(i));
  Rule * r = FindPositiveRule(precondition, result);
  if (r) return r;
  return MakeNewRule(precondition, EncodedNumber(), CREATIVE_RULE, NULL,
		     result, EncodedNumber(), EncodedNumber());
}

// Simple L1 modifiers
void Model::A1_SetLnLikelihood(double new_val) {
  changelist_.Make(new ValueChange<double>(&ln_likelihood_, new_val));
}
void Model::A1_InsertIntoIDToComponent(int id, Component *c) {
  changelist_.Make
    (new MapInsertChange<int, Component *>(&id_to_component_,id, c));
}
void Model::A1_RemoveFromIDToComponent(int id) {
  changelist_.Make
    (new MapRemoveChange<int, Component *>(&id_to_component_,id));
}
void Model::A1_InsertIntoTupleToTrueTuple(Tuple t, TrueTuple *tt) {
  changelist_.Make
    (new MapInsertChange<Tuple, TrueTuple *>(&tuple_to_true_tuple_, t, tt));
}
void Model::A1_RemoveFromTupleToTrueTuple(Tuple t) {
  changelist_.Make
    (new MapRemoveChange<Tuple, TrueTuple *>(&tuple_to_true_tuple_, t));
}
void Model::A1_InsertIntoTimesDirty(Component *c) {
  changelist_.Make
    (new SetInsertChange<Component *>(&times_dirty_, c));
}
void Model::A1_RemoveFromTimesDirty(Component *c) {
  changelist_.Make
    (new SetRemoveChange<Component *>(&times_dirty_, c));
}
void Model::A1_InsertIntoNeverHappen(Component *c) {
  changelist_.Make
    (new SetInsertChange<Component *>(&never_happen_, c));
}
void Model::A1_RemoveFromNeverHappen(Component *c) {
  changelist_.Make
    (new SetRemoveChange<Component *>(&never_happen_, c));
}
void Model::A1_InsertIntoRequiredNeverHappen(TrueTuple *c) {
  changelist_.Make
    (new SetInsertChange<TrueTuple *>(&required_never_happen_, c));
}
void Model::A1_RemoveFromRequiredNeverHappen(TrueTuple *c) {
  changelist_.Make
    (new SetRemoveChange<TrueTuple *>(&required_never_happen_, c));
}  
void Model::A1_InsertIntoWildcardTupleToPrecondition
(Tuple t, Precondition *p, int position) {
  changelist_.Make(new MapOfSetsInsertChange<Tuple, pair<Precondition *, int> >
		   (&wildcard_tuple_to_precondition_, t, 
		    make_pair(p, position)));
}
void Model::A1_RemoveFromWildcardTupleToPrecondition
(Tuple t, Precondition *p, int position) {
  changelist_.Make(new MapOfSetsRemoveChange<Tuple, pair<Precondition *, int> >
		   (&wildcard_tuple_to_precondition_, t, 
		    make_pair(p, position)));
}
void Model::A1_InsertIntoWildcardTupleToResult(Tuple t, Rule *r, int position) {
  changelist_.Make(new MapOfSetsInsertChange<Tuple, pair<Rule *, int> >
		   (&wildcard_tuple_to_result_, t, make_pair(p, position)));
}
void Model::A1_RemoveFromWildcardTupleToResult(Tuple t, Rule *r, int position) {
  changelist_.Make(new MapOfSetsRemoveChange<Tuple, pair<Rule *, int> >
		   (&wildcard_tuple_to_result_, t, make_pair(p, position)));
}
void Model::A1_InsertIntoPreconditionIndex(const Pattern &pat, Precondition *p){
  changelist_.Make
    (new MapInsertChange<Pattern, Precondition *>
     (&tuple_to_true_tuple_, pat, p));  
}
void Model::A1_RemoveFromPreconditionIndex(const Pattern &pat) {
  changelist_.Make
    (new MapRemoveChange<Pattern, Precondition *>
     (&tuple_to_true_tuple_, pat));  
}
void Model::A1_InsertIntoProhibitionIndex(Tuple t, Prohibition *p) {
  changelist_.Make
    (new MapOfSetsInsertChange<Tuple, Prohibition *>(&prohibition_index_,
						     t, p));
}
void Model::A1_RemoveFromProhibitionIndex(Tuple t, Prohibition *p) {
  changelist_.Make
    (new MapOfSetsRemoveChange<Tuple, Prohibition *>(&prohibition_index_,
						     t, p));
}
void Model::A1_InsertIntoSpecRequirements(TrueTuple *t) {
  changelist_.Make
    (new SetInsertChange<TrueTuple *>(&spec_requirements_, t));
}
void Model::A1_RemoveFromSpecRequirements(TrueTuple *t) {
  changelist_.Make
    (new SetRemoveChange<TrueTuple *>(&spec_requirements_, t));
}
void Model::A1_InsertIntoSpecProhibitions(Prohibition *p) {
  changelist_.Make
    (new SetInsertChange<Prohibition *>(&spec_prohibitions_, p));
}
void Model::A1_RemoveFromSpecProhibitions(Prohibition *p) {
  changelist_.Make
    (new SetRemoveChange<Prohibition *>(&spec_prohibitions_, p));
}
void Model::A1_AddToArbitraryTermCounts(int t, int delta) {
  changelist_.Make
    (new MapOfCountsAddChange<int, int>(&arbitrary_term_counts_, 
					t, delta));
}
void Model::A1_AddToTotalArbitraryTerms(int delta) {
  changelist_.Make
    (new ValueChange<int>(&total_arbitrary_terms_, 
			  total_arbitrary_terms_+delta));
}
void Model::A1_AddToArbitraryTermLnLikelihood(double delta) {
  changelist_.Make
    (new ValueChange<double>(&arbitrary_term_ln_likelihood_, 
			     arbitrary_term_ln_likelihood_+delta));
}
void Model::A1_AddToLnLikelihood(double delta) {
  changelist_.Make
    (new ValueChange<double>(&ln_likelihood_, ln_likelihood_+delta));
}
void Model::A1_InsertIntoViolatedProhibitions(Prohibition *p) {
  changelist_.Make
    (new SetInsertChange<Prohibition *>(&violated_prohibitions_, p));
}
void Model::A1_RemoveFromViolatedProhibitions(Prohibition *p) {
  changelist_.Make
    (new SetRemoveChange<Prohibition *>(&violated_prohibitions_, p));
}

void Model::Shell(istream  * input) {
  string line;
  string command;
  map<string, Checkpoint> checkpoints;
  cout << "?";
  while ((*input) >> command) {
    if (command == "q") break;
    else if (command=="id") {
      string s;
      (*input) >> s;
      cout << LEXICON.GetAddID(s) << endl;
    }
    else if (command == "RemoveID") {
      int id;
      (*input) >> id;
      GetComponent(id).Erase();
    }
    /*else if (command=="store") {
      string filename;
      (*input) >> filename;
      StoreToFile(filename);
    }
    else if (command=="load") {
      string filename;
      (*input) >> filename;
      ifstream finput(filename.c_str());
      Load(&finput);
      }*/
    else if (command == "verify2") {
      VerifyLayer2();
    }
    else if (command == "checkpoint") {
      string cname;
      (*input) >> cname;
      checkpoints[cname] = changelist_.GetCheckpoint();
    }
    else if (command == "rollback") {
      string cname;
      (*input) >> cname;
      changelist_.Rollback(checkpoints[cname]);
    }
    else if (command == "spec") {
      string fname;
      (*input) >> fname;
      ifstream finput(fname.c_str());
      ReadSpec(&finput);
      finput.close();
      FixTimesFixCircularDependencies(this);
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
	OptimizationCheckpoint cp(this, true);
	TryAddImplicationRule(this, p.first, p.second);
	if (cp.KeepChanges()) {
	  VLOG(0) << " Created rule "
		  << TupleVectorToString(p.first)
		  << " ->" << TupleVectorToString(p.second)
		  << " model likelihood: " << GetLnLikelihood()
		  << " gain=" << cp.Gain() << endl;
	}
      }
    }
    else if (command == "ispecific"){
      string pat;
      GetLine((*input), &pat);
      vector<Tuple> preconditions = StringToTupleVector(pat);
      GetLine((*input), &pat);
      vector<Tuple> result = StringToTupleVector(pat);
      OptimizationCheckpoint cp(this, true);      
      TryAddImplicationRule(preconditions, result, REQUIRE_BETTER, true);
      if (cp.KeepChanges()) {
	VLOG(0) << " Created rule "
		<< TupleVectorToString(preconditions)
		<< " ->" << TupleVectorToString(result)
		<< " model likelihood: " << GetLnLikelihood()
		<< " gain=" << cp.Gain() << endl;
      }      
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


 
