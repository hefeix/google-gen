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
#include "prohibition.h"
#include <sstream>
#include <deque>
#include <fstream>
#include <cmath>


// ----- LAYER 3 FUNCTIONS -----



// ----- LAYER 2 FUNCTIONS -----

TrueTuple * Model::AddRequirementToSpec(Tuple t){
  TrueTuple *tt = GetAddTrueTuple(t);
  CHECK(!(spec_requirements_ % tt));
  A1_InsertIntoSpecRequirements(tt);
  tt->A1_MakeRequired();
  if (tt->time_ == NEVER && !(required_never_happen_ % tt)) {
    A1_InsertIntoRequiredNeverHappen(tt);
  }
  return tt;
}
Prohibition * Model::AddProhibitionToSpec(Tuple t, vector<Tuple> exceptions){
  Prohibition *p = Prohibition::L1_MakeProhibition(this, t);
  A1_InsertIntoSpecProhibitions(p);
  for (uint i=0; i<exceptions.size(); i++) {
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

bool Model::IsForbidden(const Tuple & t) const{
  for (GeneralizationIterator run_g(t); !run_g.done(); ++run_g) {
    set<Prohibition *> const * prohibitions 
      = prohibition_index_ % run_g.generalized();
    if (prohibitions) forall(run_p, *prohibitions) {
      if ((*run_p)->TupleIsProhibited(t)) return true;
    }
  }
  return false;
}
bool Model::IsLayer3() const{
  if (times_dirty_.size()) return false;
  if (never_happen_.size()) return false;
  if (violated_prohibitions_.size()) return false;
  return true;
}
// FixTime may be able to make the model Layer 3
bool Model::MayBeTimeFixable() const{
  // TODO: think about this.
  if (violated_prohibitions_.size()) return false;
  if (required_never_happen_.size()) return false;
  return true;
}

string TermEscape(string s){
  if (s[0]=='*' || s[0]=='_') return '_'+s;
  return s;
}
string TermUnescape(string s){
  if (s[0]=='_' && s.size() > 1) return s.substr(1);
  return s;
}
/*
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
*/


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
    forall(run_tp, (*run_f)->rule_sat_->satisfaction_->true_tuples_)
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
  component->id_ = next_id_;
  A1_InsertIntoIDToComponent(next_id_, component);
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
    vector<Component *> dep = c->TemporalDependents();
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
    c->L1_SetTimeMaintainIndices(NEVER, false);
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

Record Model::ModelInfo() const {
  Record r;
  r["Ln Likelihood"] = dtoa(ln_likelihood_);
  r["Ln likelihood(arbitrary terms)"] = dtoa(arbitrary_term_ln_likelihood_);
  r["required never happen"] = itoa(required_never_happen_.size());
  r["violated prohibitions"] = itoa(violated_prohibitions_.size());
  string awc;
  forall(run, arbitrary_term_counts_) 
    awc += itoa(run->second) + " : " + LEXICON.GetString(run->first) + "<br>\n";
  r["Term Counts (arbitrary terms)"] = awc;
  return r;  
}
string Model::LinkBar() const{
  string ret;
  for (uint i=0; i<NUM_COMPONENT_TYPES; i++) {
    ComponentType ct = ComponentType(i);
    ret += string() + "<a href=" + ComponentTypeToString(ct) + ".html>"
      + ComponentTypeToString(ct) + "</a> ";
  }
  ret += "<br>";
  return ret;
}
void Model::ToHTML(string dirname) const {
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
void Model::VerifyLinkBidirectionality() const {
  set<pair<Component *, Component *> > td1, td2, p1, p2;
  forall(run, id_to_component_){
    Component *c = run->second;
    vector<Component *> p = c->Purposes();
    for (uint i=0; i<p.size(); i++) {
      p1.insert(make_pair(c, p[i]));
    }
    vector<Component *> cp = c->Copurposes();
    for (uint i=0; i<cp.size(); i++) {
      p2.insert(make_pair(cp[i], c));
    }
    vector<Component *> td = c->TemporalDependents();
    for (uint i=0; i<td.size(); i++) {
      td1.insert(make_pair(c, td[i]));
    }
    vector<vector<Component *> > tcd = c->TemporalCodependents();
    for (uint i=0; i<tcd.size(); i++) for (uint j=0; j<tcd[i].size(); j++) {
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
  bool return_subs_for_all_rules = false) {
  DestructibleCheckpoint dcp(&changelist_);
  if (!tuple_index_.FindTuple(s))
    changelist_.Make
      (new MemberCall1Change<TupleIndex, Tuple>(&tuple_index_, s,
					       &TupleIndex::AddWrapper,
					       &TupleIndex::RemoveWrapper));
  if (results) results->clear();
  int64 total_work = 0;
  for (GeneralizationIterator run_g(s); !run_g.done(); ++run_g) {
    const set<pair<Precondition *, int> > * preconditions
      = wildcard_tuple_to_precondition_ % run_g.generalized();
    if (preconditions) forall(run, (*preconditions)) {
      Precondition * precondition = run->first;
      int clause_num = run->second;
      Substitution partial_sub;
      vector<Tuple> simplified_precondition = precondition->pattern_;
      // This can fail (gracefully) if the clause contains the same variable 
      // at two positions and the tuple doesn't.
      if (!ComputeSubstitution(precondition->pattern_[clause_num], s, 
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
 const set<Component *> *excluded_dependents, int64 max_work){
  CHECK(results);
  int64 total_work = 0;
  for (GeneralizationIterator run_g(s); !run_g.done(); ++run_g) {
    const set<pair<Rule *, int> > * rules
      = wildcard_tuple_to_result_ % run_g.generalized();
    if (rules) forall(run, (*rules)) {
      Rule * rule = run->first;
      if (excluded_dependents && 
	  ((*excluded_dependents) % ((Component*)rule))) continue;
      int clause_num = run->second;
      Substitution partial_sub;
      vector<Tuple> simplified_precondition = rule->precondition_->pattern_;
      
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
	vector<Tuple> substituted_precondition = rule->precondition_->pattern_;
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
  Precondition * const * p = precondition_index_ % tuples;
  if (p) return *p; 
  else return NULL;
}

set<Rule *> Model::GetAllRules() const {
  set<Rule *> ret;
  forall(run, precondition_index_){
    ret.insert(run->second->rules_.begin(), 
	       run->second->rules_.end());
  }
  return ret;
}

Precondition * Model::L1_GetAddPrecondition(const vector<Tuple> & tuples) {
  Precondition * p = FindPrecondition(tuples);
  if (p) { return p; }
  else return new Precondition(this, tuples);
}

TrueTuple * Model::FindTrueTuple(const Tuple & s) const {
  TrueTuple * const * tuple = tuple_to_true_tuple_ % s;
  if (tuple) return *tuple;
  return NULL;
}
TrueTuple * Model::GetAddTrueTuple(const Tuple & s) {
  TrueTuple * ret = FindTrueTuple(s);
  if (ret) return ret;
  ret = new TrueTuple(this, s);
  return ret;
}

Rule * Model::FindPositiveRule(vector<Tuple> precondition, vector<Tuple> result)
  const{
  Precondition *p = FindPrecondition(precondition);
  if (!p) return NULL;
  return p->FindPositiveRule(result);
}
Rule * Model::FindNegativeRule(vector<Tuple> precondition, Rule * target_rule) const{
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

Rule * Model::GetAddNaiveRule(uint length) {
  vector<Tuple> precondition;
  vector<Tuple> result(1);
  vector<Tuple> target_precondition;
  for (uint i=0; i<length; i++) result[0].terms_.push_back(Variable(i));
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
		   (&wildcard_tuple_to_result_, t, make_pair(r, position)));
}
void Model::A1_RemoveFromWildcardTupleToResult(Tuple t, Rule *r, int position) {
  changelist_.Make(new MapOfSetsRemoveChange<Tuple, pair<Rule *, int> >
		   (&wildcard_tuple_to_result_, t, make_pair(r, position)));
}
void Model::A1_InsertIntoPreconditionIndex(const Pattern &pat, Precondition *p){
  changelist_.Make
    (new MapInsertChange<Pattern, Precondition *>
     (&precondition_index_, pat, p));  
}
void Model::A1_RemoveFromPreconditionIndex(const Pattern &pat) {
  changelist_.Make
    (new MapRemoveChange<Pattern, Precondition *>
     (&precondition_index_, pat));
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
void Model::A1_IncrementNextID(){
  changelist_.Make(new ValueChange<int>(&next_id_, next_id_+1));
}



 
