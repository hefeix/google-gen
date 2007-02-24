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

LL Model::GetChoosersLnLikelihood() const { 
  LL ret;
  forall(run, all_choosers_) ret += (*run)->ln_likelihood_;
  return ret;
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
  ln_likelihood_ = 0;
  work_penalty_ = LL(0.001);
  old_style_display_ = false;
  chooser_ = new Chooser(this, NULL);
  uint_quadratic_chooser_ = new UintChooser(this);

  precondition_length_chooser_ = new Chooser(this, uint_quadratic_chooser_);
  result_length_chooser_ = new Chooser(this, uint_quadratic_chooser_);
  tuple_length_chooser_ = new Chooser(this, uint_quadratic_chooser_);
  term_type_chooser_ = new Chooser(this, NULL);
  relation_term_type_chooser_ = new Chooser(this, NULL);
  variable_novelty_chooser_ = new Chooser(this, NULL);
  constant_novelty_chooser_ = new Chooser(this, NULL);
  rule_constant_chooser_= new Chooser(this, chooser_);
  rule_relation_constant_chooser_ = new Chooser(this, chooser_);
  variable_backreference_chooser_ = new Chooser(this, NULL);
  constant_backreference_chooser_ = new Chooser(this, NULL);
  

  verify_counter_ = 0;
  verify_interval_ = 1;

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

/*
  To compute the likelihood, we multiply the following:
  1.  The likelihood of the ordering : Prod(count[i]!)/(total!) 
  2.  The number of ways we could have picked these objects
       from the parent distribution: (count.size()!)
  3.  The sequence of object counts:
    since each object occurs at least once, we are allocating 
    (total-count.size()) extra objects among the count.size() objects.
    This can be done using the stars and bars argument with 
    (total - count.size()) objects and (count.size()-1) dividers
    So the number of possibilities is (total-1) choose (count.size()-1).
    a. likelihood = (count.size()-1)! * (total-count.size())! / (total-1)!

  Multiplying it all together, we get:
   Prod(count[i]!) * count.size()! * (count.size()-1)! * (total-count.size())!
    / [ total! * (total-1)! ]

*/
LL Chooser::ComputeLnLikelihood() const {
  LL ret(0);
  if (counts_.size()==0) return ret;
  int total = 0;
  forall(run, counts_) {
    int count = run->second;
    total += count;
    ret += LnFactorial(count);
  }
  CHECK(total>0);
  CHECK(total == total_);
  ret += LnFactorial(counts_.size());
  ret += LnFactorial(counts_.size()-1);
  ret += LnFactorial(total_-counts_.size());
  ret -= LnFactorial(total_);
  ret -= LnFactorial(total_-1);
  return ret;
}

LL UintChooser::ComputeLnLikelihood() const{
  LL total = 0;
  forall(run, counts_) {
    int object = run->first;
    int count = run->second;
    total += count * uintQuadraticLnProb(object);
  }
  return total;  
}

void Chooser::L1_AddToLnLikelihood(LL delta) {
  model_->GetChangelist()->ChangeValue(&ln_likelihood_, 
				       ln_likelihood_ +  delta);
  model_->A1_AddToLnLikelihood(delta);
}

LL Chooser::ComputeLLDelta(int object,
			   int old_count, int new_count, 
			   int old_num_objects, int new_num_objects, 
			   int old_total, int new_total) {
  if (old_total==0 || new_total==0) return 0;
  LL ll_delta = LnFactorial(new_count) - LnFactorial(old_count); 
  ll_delta += LnFactorial(new_num_objects) - LnFactorial(old_num_objects);
  ll_delta += LnFactorial(new_num_objects-1) - LnFactorial(old_num_objects-1);
  ll_delta += LnFactorial(new_total-new_num_objects) 
    - LnFactorial(old_total - old_num_objects);
  ll_delta -= LnFactorial(new_total) - LnFactorial(old_total);
  ll_delta -= LnFactorial(new_total-1) - LnFactorial(old_total-1);
  return ll_delta;
}

LL UintChooser::ComputeLLDelta(int object, 
			       int old_count, int new_count,
			       int old_num_objects, int new_num_objects,
			       int old_total, int new_total){
  return (new_count-old_count) * uintQuadraticLnProb(object);
}

void Chooser::L1_ChangeObjectCount(int object, int delta) {

  CHECK (delta != 0);
  int * look = counts_ % object;
  int old_count = look ? *look : 0;
  int new_count = old_count + delta;
  CHECK (new_count >= 0);
  
  Changelist * cl = model_->GetChangelist();
  int old_num_objects = counts_.size();
  int64 old_total = total_;
  
  cl->Make(new MapOfCountsAddChange<int, int>(&counts_, object, delta));
  cl->ChangeValue(&total_, delta + old_total);
  
  int new_num_objects = counts_.size();
  int64 new_total = total_;

  LL ll_delta = ComputeLLDelta(object, 
			       old_count, new_count, old_num_objects, 
			       new_num_objects, old_total, new_total);
  L1_AddToLnLikelihood(ll_delta);

  // Propagate to parent
  if (!parent_) return;
  if (new_count == 0)
    parent_->L1_ChangeObjectCount(object, -1);
  if (old_count == 0)
    parent_->L1_ChangeObjectCount(object, 1);
}

Chooser::Chooser(Model *model, Chooser *parent){
  parent_ = parent;
  model_ = model;
  CHECK(model_);
  ln_likelihood_ = 0;
  total_ = 0;
  model_->A1_InsertIntoChoosers(this);
  model_->GetChangelist()->Creating(this);
}

void Chooser::L1_Erase(){
  CHECK(counts_.size()==0);
  CHECK(ln_likelihood_ == 0);
  CHECK(total_ ==0);
  model_->A1_RemoveFromChoosers(this);
  model_->GetChangelist()->Destroying(this);
}

Record Chooser::ChooserInfo(bool include_objects) {
  Record r;
  r["Ln Likelihood"] = ln_likelihood_.ToString();
  r["Total"] = itoa(total_);
  r["Num Objects"] = itoa(counts_.size());
  if (!include_objects) return r;

  forall (run, counts_) {
    int object = run->first;
    int count = run->second;
    r["objects"] += LEXICON.GetString(object) + ":" + itoa(count) + "<br>";
  }
  return r;
}

int Chooser::GetCount(int object) const {
  const int * find = counts_ % object;
  if (!find) return 0;
  return *find;
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
  //int original_qsize = max((int)queue.size(), 50);
  
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
      // VLOG(0) << "queue.size() : " << queue.size() << endl;
      qsize = queue.size();
    }
    // GEORGES-WTF?
    /*if (queue.size() > (uint)20*original_qsize) {
      VLOG(0) << "Aborting FixTimes" << endl;
      return false;
      }*/
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
      if ((rule->GetRuleType()==FEATURE_RULE) == (pass==1))
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
		      (type==FEATURE_RULE)?
		      GetComponent<Rule>(atoi(r["target_id"])):NULL,
		      StringToTupleVector(r["result"]));
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
  r["Ln Likelihood"] = ln_likelihood_.ToString();
  r["Ln likelihood(global choosers)"] = GetChoosersLnLikelihood().ToString();
  r["Work"] = itoa(GetSearchWork());
  r["Utility"] = GetUtility().ToString();
  r["required never happen"] = itoa(required_never_happen_.size());
  r["violated prohibitions"] = itoa(violated_prohibitions_.size());
  r["chooser"] = RecordToHTMLTable(chooser_->ChooserInfo(true));
  r["return values"] = "<PRE>" + AllFunctionReturnInfo(true) + "</PRE>";
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
      m[c->Type()].push_back(c->RecordForDisplay(false));// TODO
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
  
  for (int pass=0; pass<2; pass++) { 
    map<ComponentType, vector<Record> > m;
    bool verbose = pass;
    forall(run, id_to_component_) {      
      Component * c = run->second;
      if (verbose && c->Type() != RULE) continue;
      m[c->Type()].push_back(c->RecordForDisplay(verbose));
    }
    forall(run, m) {
      string type_name = ComponentTypeToString(run->first);
      output.open((dirname + "/" + type_name + 
		   (verbose?".v":"")+".html").c_str());
      output << LinkBar();
      output << "<h1>" << ComponentTypeToString(run->first) << "</h1>" 
	     << endl << RecordVectorToHTMLTable(run->second);
      output.close();
    }
  }
  cthis->old_style_display_ = false;
}

void Model::VerifyLikelihood() const{
  LL total = GetChoosersLnLikelihood();
  
  // TODO add in the likelihoods from all choosers
  forall(run, id_to_component_){
    total += run->second->ln_likelihood_;
  }
  VLOG(0) << "total=" << total << " ln_likelihood_ = " << ln_likelihood_ 
	  << endl;
  if (total != ln_likelihood_) {
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
	   SamplingInfo::Unsampled(),
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

// Add the global chooser and the choosers from all rules
set<Chooser *> Model::GetAllObjectChoosers() const {
  set<Chooser *> ret;
  ret.insert(chooser_);
  set<Rule *> rules = GetAllRules();
  forall(run, rules) {
    Rule * rule = *run;
    set<Chooser *> r_choosers = rule->GetAllChoosers();
    ret.insert(r_choosers.begin(), r_choosers.end());
  }
  return ret;
}

// Predict the cost of encoding a rule
// TODO: see if the precondition exists
LL Model::RuleEncodingCost(CandidateRule r) const {
  Model * c_this = const_cast<Model *>(this);
  DestructibleCheckpoint checkp(c_this->GetChangelist());
  LL old_utility = GetUtility();
  LL d1 = c_this->L1_ComputePatternLnLikelihoodUpdateChoosers
    (Pattern(), r.first, false, 1);
  LL d2 = c_this->L1_ComputePatternLnLikelihoodUpdateChoosers
    (r.first, r.second, true, 1);
  LL ret = GetUtility() - old_utility;
  ret += d1;
  ret += d2;
  return ret;
}

Precondition * Model::L1_GetAddPrecondition(const vector<Tuple> & tuples) {
  Precondition * p = FindPrecondition(tuples);
  if (p) { return p; }
  else return new Precondition(this, tuples);
}

// encoding of a pattern:
/* 
choose the number of tuples from precondition_length_chooser_ or 
  result_length_chooser_
Choose tuple lengths from tuple_length_chooser_
For each term in a tuple:
  Choose the type (variable/constant) from term_type_chooser_ or 
     relation_term_type_chooser_, depending on whether it is the first term 
     in the tuple. (boolean)
  If there are already objects of that type, 
     choose whether it's new from variable_novelty_chooser_ or
     constant_novelty_chooser_ (boolean)
  If it's a new constant, pick it from rule_constant_chooser_ or 
     rule_relation_constant_chooser_, depending on whether it's the first item.
  It it's not new, encode it as n, where n is the number of distinct terms
     of this type strictly between the last instance of the term and this one.  
     Choose this from variable_backreference_chooser_ or 
     constant_backreference_chooser_.
*/
LL Model::L1_ComputePatternLnLikelihoodUpdateChoosers(const Pattern &context, 
						      const Pattern &to_encode, 
						      bool is_result,
						      int multiplier){
  CHECK(multiplier != 0);

  LL ret = 0;
  bool encoding = false;

  vector<int> constants_seen;
  vector<int> variables_seen;
  // encode the number of tuples in the pattern
  (is_result?result_length_chooser_:precondition_length_chooser_)
    ->L1_ChangeObjectCount(to_encode.size(), multiplier);
  for (uint i=0; i<context.size()+to_encode.size(); i++) {
    if (i == context.size()) encoding = true;
    const Tuple &s = (encoding ? to_encode[i-context.size()] : context[i]);
    CHECK(s.size() > 0);
 
    // encode the length of the tuple.
    if (encoding) 
      tuple_length_chooser_->L1_ChangeObjectCount(s.size(), multiplier);
    for (uint j=0; j<s.size(); j++) {
      int t = s[j];
      bool is_var = IsVariable(t);
      vector<int> & seen_ref = is_var?variables_seen:constants_seen;
      int last_seen_pos = -1;
      for (uint k=0; k<seen_ref.size(); k++) 
	if (seen_ref[k]==t) last_seen_pos=k;
      bool seen = (last_seen_pos>=0);
      if (encoding) {
	((j==0)?relation_term_type_chooser_:term_type_chooser_)
	  ->L1_ChangeObjectCount(is_var?1:0, multiplier);
	if (seen_ref.size() > 0)
	  ((is_var)?variable_novelty_chooser_:constant_novelty_chooser_)
	    ->L1_ChangeObjectCount(seen?0:1, multiplier);
	if (!seen && !is_var)
	  ((j==0)?rule_relation_constant_chooser_:rule_constant_chooser_)
	    ->L1_ChangeObjectCount(t, multiplier);
	if (seen) {
	  (is_var?variable_backreference_chooser_:
	   constant_backreference_chooser_)
	    ->L1_ChangeObjectCount(seen_ref.size()-last_seen_pos, multiplier);
	}	
      }  
      seen_ref.push_back(t);
    }
  }
  return ret;  
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

set<Rule *> Model::FindPositiveRules(vector<Tuple> precondition, vector<Tuple> result) const
{
  Precondition *p = FindPrecondition(precondition);
  if (!p) return set<Rule*>();
  return p->FindPositiveRules(result);
}

Rule * Model::FindFeatureRule(vector<Tuple> precondition, Rule * target_rule) const{
  Precondition *p = FindPrecondition(precondition);
  if (!p) return NULL;
  return p->FindFeatureRule(target_rule);
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
		     result);
}

// Simple L1 modifiers
void Model::A1_SetLnLikelihood(LL new_val) {
  changelist_.Make(new ValueChange<LL>(&ln_likelihood_, new_val));
}
void Model::A1_InsertIntoIDToComponent(int id, Component *c) {
  changelist_.Make
    (new MapInsertChange<int, Component *>(&id_to_component_,id, c));
}
void Model::A1_RemoveFromIDToComponent(int id) {
  changelist_.Make
    (new MapRemoveChange<int, Component *>(&id_to_component_,id));
}
void Model::A1_InsertIntoChoosers(Chooser *c) {
  changelist_.Make
    (new SetInsertChange<Chooser *>(&all_choosers_, c));		   
}
void Model::A1_RemoveFromChoosers(Chooser *c) {
  changelist_.Make
    (new SetRemoveChange<Chooser *>(&all_choosers_, c));		   
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
void Model::A1_AddToLnLikelihood(LL delta) {
  changelist_.Make
    (new ValueChange<LL>(&ln_likelihood_, ln_likelihood_+delta));
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
  // time of the call.  Why:
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

