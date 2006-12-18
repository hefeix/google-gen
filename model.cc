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

// Contains functions declared in model.h, except those in component.cc 
// and optimize.cc
#include "model.h"
#include "probutil.h"
#include "prohibition.h"
#include "searchtree.h"
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
  Timer readspec_timer(__FUNCTION__, NULL);
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
      = prohibition_index_ % run_g.Current();
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
  forall(run, required_never_happen_) {
    if ((*run)->causes_.size()==0) return false;
  }
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

// Why are these here?
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
  work_penalty_ = 0.01;
  old_style_display_ = false;

  // Load up the words
  ifstream input("words");
  string w;
  while (input >> w) {
    if (!isalpha(w[0])) continue;
    w[0] = toupper(w[0]);
    words_.push_back(w);
  }
  VLOG(0) << "Loaded " << words_.size() << " English words" << endl;
}

Model::~Model(){
    changelist_.Rollback(0);
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
  d_prob += log(f_i+1.0) - log(n+1.0);
  d_prob += uintQuadraticLnProb(f_i+1-1);
  if (f_i > 0) d_prob -= uintQuadraticLnProb(f_i-1);
  if (f_i == 0) {
    d_prob += log((double)k);
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
bool Model::FixTimes() {
  // VLOG(0) << "FT";

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
    VLOG(2) << "   Inserting " << c->id_  
	    << " time=" << c->time_.ToSortableString() << endl;
    Time t = min(c->time_, c->ComputeTime(&times_dirty_));
    queue.insert(make_pair(t, c));
  }

  int qsize = queue.size();
  int original_qsize = max((int)queue.size(), 50);

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

    // Ignore the entry if it's obsolete
    // if it's not the current time or the computed time
    if (c->time_ != t && new_time != t) continue;

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
    if (queue.size() > (uint)2*qsize) {
      VLOG(0) << "queue.size() : " << queue.size() << endl;
      qsize = queue.size();
    }
    if (queue.size() > (uint)20*original_qsize) {
      VLOG(0) << "Aborting FixTimes" << endl;
      return false;
    }
  }
  // Now that all of the dirty stuff happens NEVER, we can make it clean.  
  // TODO, is this really necessary, or could we have just let the previous 
  // loop run until the queue was empty?
  // VLOG(0) << "left main loop" << endl;
  while(times_dirty_.size()){
    Component *c = *times_dirty_.begin();
    c->L1_SetTimeMaintainIndices(NEVER, false);
    c->L1_MakeTimeClean();
  }
  CHECK(times_dirty_.size()==0);
  // VLOG(0) << "left fixtimes" << endl;
  return true;
}

void Model::DeleteNeverHappeningComponents() {
  CHECK(times_dirty_.size()==0);
  while (never_happen_.size()) {
    Component * c = *never_happen_.begin();
    c->L1_Erase();
  }
}

void Model::ChangeID(Component *c, int new_id) {
  if (new_id == c->id_) return;
  int old_id = c->id_;
  Component * displaced = GetComponent(new_id);
  changelist_.Make (new ValueChange<int>(&c->id_, new_id));
  if (displaced) {
    changelist_.Make(new ValueChange<int>(&displaced->id_, old_id));
  }
  A1_RemoveFromIDToComponent(old_id);
  if (displaced) A1_RemoveFromIDToComponent(new_id);
  A1_InsertIntoIDToComponent(new_id, c);
  if (displaced) A1_InsertIntoIDToComponent(old_id, displaced);
  if (!(new_id < next_id_)) {
    changelist_.Make(new ValueChange<int>(&next_id_, new_id+1));
  }  
}

void Model::Store(string filename) const {
  ofstream output(filename.c_str());
  forall(run, spec_requirements_){
    Record r;
    r["CT"] = "spec_requirement";
    r["tuple"] = (*run)->tuple_.ToString();
    output << RecordToString(r) << endl;
  }
  forall(run, spec_prohibitions_){
    Record r;
    r["CT"] = "spec_prohibition";
    r["prohibited"] = (*run)->GetProhibited().ToString();
    vector<Tuple> exceptions((*run)->GetExceptions().begin(), 
			     (*run)->GetExceptions().end());
    r["exceptions"] = TupleVectorToString(exceptions);
    output << RecordToString(r) << endl;
  }

  forall(run, GetComponentsOfType(TRUETUPLE))
    output << RecordToString((*run)->RecordForStorge()) << endl;
  
  for (int pass=0; pass<2; pass++) 
    forall(run, GetComponentsOfType(RULE)) {
      Rule * rule = dynamic_cast<Rule *>(*run);
      if ((rule->GetRuleType()==NEGATIVE_RULE) == (pass==1))
	output << RecordToString(rule->RecordForStorge())<< endl;
    }

  forall(run, GetComponentsOfType(FIRING)) 
    output << RecordToString((*run)->RecordForStorge()) << endl;

  forall(run, GetComponentsOfType(SATISFACTION)) 
    output << RecordToString((*run)->RecordForStorge()) << endl;

  forall(run, GetComponentsOfType(RULESAT)) 
    output << RecordToString((*run)->RecordForStorge()) << endl;

  output.close();
}
void Model::Load(string filename) {
  Timer load_timer(__FUNCTION__, NULL);
  ifstream input(filename.c_str());
  Record r;
  while (input >> r){
    if (r["CT"]=="spec_requirement"){
      AddRequirementToSpec(StringToTuple(r["tuple"]));
    } else if (r["CT"]=="spec_prohibition"){
      AddProhibitionToSpec(StringToTuple(r["prohibited"]),
			   StringToTupleVector(r["exceptions"]));
    } else {
      ComponentType ct = StringToComponentType(r["CT"]);
      if (ct==TRUETUPLE) {
	TrueTuple * tt = GetAddTrueTuple(StringToTuple(r["tuple"]));
	ChangeID(tt, atoi(r["id"]));
      } else if (ct==RULE){
	RuleType type = StringToRuleType(r["rule_type"]);
	Rule * rule = 
	  MakeNewRule(StringToTupleVector(r["precondition"]),
		      EncodedNumber(r["delay"]),
		      type,
		      (type==NEGATIVE_RULE)?
		      GetComponent<Rule>(atoi(r["target_id"])):NULL,
		      StringToTupleVector(r["result"]),
		      EncodedNumber(r["strength"]),
		      EncodedNumber(r["strength2"]) );
	ChangeID(rule, atoi(r["id"]));
	ChangeID(rule->precondition_, atoi(r["precondition_id"]));
      } else if (ct==FIRING){
	Rule * rule = GetComponent<Rule>(atoi(r["rule"]));
	Firing * f 
	  = rule->AddFiring(Substitution(r["full_substitution"]));
	ChangeID(f, atoi(r["id"]));
      } else if (ct==SATISFACTION){
	Precondition * p = GetComponent<Precondition>(atoi(r["precondition"]));
	Satisfaction * sat = 
	  p->FindSatisfaction(Substitution(r["substitution"]));
	CHECK(sat);
	ChangeID(sat, atoi(r["id"]));
      } else if (ct==RULESAT){
	Rule * rule = GetComponent<Rule>(atoi(r["rule"]));
	Satisfaction *sat = GetComponent<Satisfaction>(atoi(r["satisfaction"]));
	RuleSat * rs = rule->FindRuleSat(sat);
	CHECK(rs);
	ChangeID(rs, atoi(r["id"]));
      } else {
	CHECK(false);
      }
    }
  }
  FixTimes();
}


Record Model::ModelInfo() const{ 
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

string Model::DLinkBar() const {
  string ret;
  ret += "<a href=\"command=showmodel\"> MODEL </a>";
  for (uint i=0; i<NUM_COMPONENT_TYPES; i++) {
    ComponentType ct = ComponentType(i);
    string cts = ComponentTypeToString(ct);
    ret += "<a href=command=showcomponentsoftype&type=" + cts 
      + ">" + cts + "</a> ";
  }
  ret += "<br>";
  return ret;
}

string Model::LinkBar() const{
  string ret;
  ret += "<a href=model.html>MODEL</a> ";
  for (uint i=0; i<NUM_COMPONENT_TYPES; i++) {
    ComponentType ct = ComponentType(i);
    ret += string() + "<a href=" + ComponentTypeToString(ct) + ".html>"
      + ComponentTypeToString(ct) + "</a> ";
  }
  ret += "<br>";
  return ret;
}

void Model::ToHTMLByComponentType
(stringstream& out, const set<ComponentType>& ct) {

  map<ComponentType, vector<Record> > m;
  forall(run, id_to_component_) {
    Component * c = run->second;
    if (ct % c->Type()) 
      m[c->Type()].push_back(c->RecordForDisplay());
  }
  forall(run, m) {
    string type_name = ComponentTypeToString(run->first);
    out << "<h1>" << type_name << "</h1>" << endl << RecordVectorToHTMLTable(run->second);
  }
}

void Model::ToHTML(string dirname) const {

  Model * cthis = const_cast<Model*>(this);
  cthis->old_style_display_ = true;

  system(("mkdir -p " + dirname).c_str());
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
  cthis->old_style_display_ = false;
}

void Model::VerifyLikelihood() const{
  double total = arbitrary_term_ln_likelihood_;
  forall(run, id_to_component_){
    total += run->second->ln_likelihood_;
  }
  VLOG(0) << "total=" << total << " ln_likelihood_ = " << ln_likelihood_ 
	  << endl;
  if (fabs(total-ln_likelihood_) > 1e-6) {
    cerr << " total likelihood out of date " 
	 << " store=" << ln_likelihood_ << " computed=" << total << endl;
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
  if (td1 != td2){
    ToHTML("html");
    forall(run, td1) if (!(td2 % *run)) {
      cerr << "Temp. dependent missing temp. codependent " 
	   << run->first->TypeName() << " " 
	   << run->first->id_ << " " << run->second->TypeName() 
	   << " " << run->second->id_ << endl;
    }
    forall(run, td2) if (!(td1 % *run)) {
      cerr << "Temp. codependent missing temp. dependent " 
	   << run->first->TypeName() << " " 
	   << run->first->id_ << " " << run->second->TypeName() 
	   << " " << run->second->id_ << endl;      
    }
    CHECK(false);
  }
  CHECK(p1==p2);
}
void Model::VerifyIndices() const {
  // TODO: implement this
}
void Model::VerifyLayer2() const {
  forall(run, id_to_component_) {
    Component *c = run->second;
    c->VerifyLayer2();
  }
  VerifyIndices();
  VerifyLinkBidirectionality();
  VerifyLikelihood();
}
/*
int64 Model::FindSatisfactionsForTuple
( const Tuple & tuple, 
  map<Precondition *, pair<uint64, vector<Substitution> > > *results,
  int64 max_work,
  bool adding_tuple, // updates search trees
  bool return_subs_for_negative_rules = true,
  bool return_subs_for_all_rules = false) {
  if (adding_tuple) {
    CHECK(max_work == UNLIMITED_WORK);
  }
  DestructibleCheckpoint dcp(&changelist_);
  if (!tuple_index_.Lookup(tuple, NULL, NULL)) {
    CHECK(!adding_tuple);
    changelist_.Make
      (new MemberCall1Change<TupleIndex, Tuple>(&tuple_index_, s,
					       &TupleIndex::Add,
					       &TupleIndex::Remove));
  }
  if (results) results->clear();
  int64 total_work = 0;
  for (GeneralizationIterator run_g(tuple); !run_g.done(); ++run_g) {
    set<SearchNode *> * search_nodes = wildcard_tuple_to_search_node_ %
      run_g.Current();
    if (search_nodes) forall(run, *search_nodes) {
      total_work++;
      if (max_work != UNLIMITED_WORK && total_work > max_work) return GAVE_UP;
      SearchNode *node = *run;
      CHECK(node->split_tuple_ != -1);
      if (adding_tuple) node->L1_SetWork(node->work_+1);
      // The substitution will come in three parts
      // 1. the substitution so far inherent in the search node
      // 2. the substitution for tuple (the split tuple at the search node)
      // 3. the substitution for future tuples.      
      Substitution sub_for_split_tuple; // #2
      if (!ComputeSubstitution(node_->pattern_[node_->split_tuple_], tuple, 
			       &sub_for_split_tuple)) continue;
      if (node->pattern_.size()==1) {
	if (adding_tuple) {
	  node->L1_SetNumSatisfactions(node->num_satisfactions_+1);
	}
	if (!results) continue;
	results[precondition].first++;
	results[precondition].second.push_back(complete_sub);
	
	// TODO: pass back substitution if necessary, and other shit
	continue;
      }
      Precondition * precondition = node->precondition_;
      Pattern simplified_pattern = node->pattern_;
      sub_for_split_tuple.Substitute(&simplified_pattern);
      simplified_pattern 
	= RemoveFromVector(simplified_pattern, node->split_tuple_);
      uint64 num_simplified_subs;
      uint64 work = 0;
      bool return_subs = return_subs_for_all_rules;
      if (return_subs_for_negative_rules && 
	  (precondition->negative_rules_.size()>0)) {
	return_subs = true;
      }
      SearchNode * child_node = NULL;
      if (adding_tuple) {
	child_node = node->L1_CreateChild(tuple, simplified_pattern);
      }
      vector<Substitution> subs_for_simplified_pattern; // #3 above
      if (!tuple_index_.FindSatisfactions
	  (simplified_pattern, 
	   child_node, 
	   NULL,
	   return_subs?(&subs_for_simplified_pattern):NULL,
	   &num_simplified_subs,
	   (max_work==UNLIMITED_WORK)?UNLIMITED_WORK:max_work-total_work,
	   &work)) return GAVE_UP;
      if (num_simplified_subs > 0 && results) {
	(*results)[precondition].first += num_simplified_subs;
	Substitution substitution_so_far = node->GetSubstitutionSoFar(); // #1
	for (uint i=0; i<subs_for_simplified_pattern.size(); i++) {
	  Substitution complete_sub = subs_for_simplified_pattern[i]; // #3
	  complete_sub.Add(substitution_so_far); // #1
	  complete_sub.Add(sub_for_split_tuple); // #2
	  (*results)[precondition].second.push_back(complete_sub);
	}
      }
      total_work += work;
    }
  }
  return total_work;
  }*/

bool Model::FindExplanationsForResult
(const Tuple & s, vector<pair<Rule *, Substitution> > *results, 
 const set<Component *> *excluded_dependents, int64 *max_work_now){
  CHECK(results);
  for (GeneralizationIterator run_g(s); !run_g.done(); ++run_g) {
    const set<pair<Rule *, int> > * rules
      = wildcard_tuple_to_result_ % run_g.Current();
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
      vector<Substitution> complete_subs;
      if (!tuple_index_.FindSatisfactions
	  (simplified_precondition, 
	   Unsampled(),
	   &complete_subs,
	   &num_complete_subs,
	   max_work_now)) return false;
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
    }
  }
  return true;
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

int Model::ArbitraryTermCount(int term) const {
  const int * res = arbitrary_term_counts_ % term;
  if (!res) return 0;
  return *res;
}

Precondition * Model::L1_GetAddPrecondition(const vector<Tuple> & tuples) {
  Precondition * p = FindPrecondition(tuples);
  if (p) { return p; }
  else return new Precondition(this, tuples);
}

// Pick a random word and go with it! (or 2 after 10 tries)
string Model::FindName(string base) {
  int tries = 0;
  while (true) {
    string w1 = base; 
    w1 += words_[RandomUInt32() % words_.size()];
    if (tries > 10) w1 += "." + words_[RandomUInt32() % words_.size()];
    if (!LEXICON.Contains(w1)) return w1;
    tries++;
  }
  return "ERROR";
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

Rule * Model::GetAddUniversalRule(uint length) {
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
void Model::A1_InsertIntoComponentsByType(Component *c){
  changelist_.Make
    (new SetInsertChange<Component *>(&(components_by_type_[c->Type()]), c));
}
void Model::A1_RemoveFromComponentsByType(Component *c){
  changelist_.Make
    (new SetRemoveChange<Component *>(&(components_by_type_[c->Type()]), c));
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
void Model::A1_InsertIntoSubrulePatternToRule(Pattern p, SubRuleInfo s) {
  VLOG(2) << "New subrule Pattern:" << TupleVectorToString(p)
	  << " SRI:" << s.ToString();
  changelist_.Make(new MapOfSetsInsertChange<Pattern, SubRuleInfo>
		   (&subrule_pattern_to_rule_, p, s));
}
void Model::A1_RemoveFromSubrulePatternToRule(Pattern p, SubRuleInfo s) {
  VLOG(2) << "Deleting subrule Pattern:" << TupleVectorToString(p)
	  << " SRI:" << s.ToString();
  changelist_.Make(new MapOfSetsRemoveChange<Pattern, SubRuleInfo>
		   (&subrule_pattern_to_rule_, p, s));
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
void Model::A1_AddToSearchWork(int64 delta) {
  changelist_.Make
    (new ValueChange<uint64>(&search_work_, search_work_+delta));
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


void Model::L1_UpdateSearchTreesAfterAddTuple(Tuple t){
  // Now we let the searchnodes know about the new tuple.  This is a little
  // tricky, as it is possible that the search trees reorganize as we are doing
  // this.  It works to call SearchNode::L1_AddTuple once for each pre-existing
  // matching entry in wildcard_tuple_to_search_node_ which still exists at the
  // time of the call.  Here's why:
  // New searchnodes already know about the new tuple.
  // ONE_TUPLE searchnodes don't change type, so if they still exist, they
  // haven't been touched and need to be updated once. 
  // SPLIT searchnodes may have changed type to PARTITION and back, but if they
  // already know about the new tuple, they know to ignore the call. 
  map<Tuple, set<SearchNode *> > to_update;
  for (GeneralizationIterator gen(t); !gen.done(); ++gen) {
    set<SearchNode *> * s
      = wildcard_tuple_to_search_node_ % gen.Current();
    if (s) to_update[gen.Current()] = *s;    
  }
  forall(run, to_update) {
    Tuple generalized = run->first;
    forall(run2, run->second) {
      SearchNode *n = *run2;
      if (wildcard_tuple_to_search_node_ % generalized 
	  && wildcard_tuple_to_search_node_[generalized] % n) {
	n->L1_AddTuple(t);
      }
    }
  }
}
// Pretty much the same thing as above, but in reverse
void Model::L1_UpdateSearchTreesAfterRemoveTuple(Tuple t){
  map<Tuple, set<SearchNode *> > to_update;
  for (GeneralizationIterator gen(t); !gen.done(); ++gen) {
    set<SearchNode *> * s
      = wildcard_tuple_to_search_node_ % gen.Current();
    if (s) to_update[gen.Current()] = *s;    
  }
  forall(run, to_update) {
    Tuple generalized = run->first;
    forall(run2, run->second) {
      SearchNode *n = *run2;
      if (wildcard_tuple_to_search_node_ % generalized 
	  && wildcard_tuple_to_search_node_[generalized] % n) {
	n->L1_RemoveTuple(t);
      }
    }
  }
}

