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


#include <sstream>
#include <math.h>
#include "tuple.h"
#include "lexicon.h"
#include "probutil.h"

string Tuple::ToString() const{
  string ret;
  ret += "[ ";
  for (uint i=0; i<terms_.size(); i++) {
    ret += LEXICON.GetString(terms_[i]); // + "(" + itoa(terms_[i]) + ")";
    ret += " ";
  }
  ret += "]";
  return ret;
}
void Tuple::FromString(const string & s){
  terms_.clear();
  istringstream istr(s.c_str());
  string w;
  istr >> w;
  while ((istr >> w) && (w!="]")) {
    int i = LEXICON.GetAddID(w);
    terms_.push_back(i);
  }
}
bool Tuple::HasDuplicateVariables() const {
  set<int> vars;
  for (uint i=0; i<terms_.size(); i++) {
    if (terms_[i] < 0) {
      if (vars % terms_[i]) return true;
      vars.insert(terms_[i]);
    }
  }
  return false;
}
bool operator==(const Tuple & s1, const Tuple & s2){
  if (s1.size() != s2.size()) return false;
  for (uint i=0; i<s1.size(); i++) if (s1[i]!=s2[i]) return false;
  return true;
}
Tuple AllVar0(int num_terms){
  Tuple s;
  s.terms_ = vector<int>(num_terms, Variable(0));
  return s;
}
GeneralizationIterator::GeneralizationIterator(const Tuple & s) {
  max_ = 1 << s.size();
  s_ = generalized_ = s;
  pattern_ = 0;
}
void GeneralizationIterator::operator++(){
  int change = pattern_ ^ (pattern_ + 1);
  pattern_++;
  if (pattern_ >= max_) return;
  for (uint i=0; change != 0; i++) {
    if (pattern_ & (1 << i)) generalized_.terms_[i] = -1;
    else generalized_.terms_[i] = s_.terms_[i];
    change >>=1;
  }
}
bool GeneralizationIterator::done() const{ return (pattern_ >= max_); }
const Tuple & GeneralizationIterator::generalized() const { 
  return generalized_; 
}
int GeneralizationIterator::pattern() const{ 
  return pattern_; 
}


int Substitution::Lookup(int term) const{
  const int * look = sub_ % term;
  if (look) return *look;
  return term;
}
void Substitution::Add(int from_term, int to_term){
  sub_[from_term] = to_term;
}
void Substitution::Substitute(Tuple * s) const{
  for (uint i=0; i<s->terms_.size(); i++) {
    s->terms_[i] = Lookup(s->terms_[i]);
  }
}
string Substitution::ToString() const{
  ostringstream ostr;
  forall(run, sub_) {
    ostr << ((run!=sub_.begin())?",":"") 
	 << LEXICON.GetString(run->first) << "=" 
	 << LEXICON.GetString(run->second);
  }
  return ostr.str();
}
void Substitution::FromString(const string & s) {
  sub_.clear();
  if (s=="null") return;
  istringstream istr(s);
  char buf[1000];
  while (istr.getline(buf, 1000, '=')) {
    string old = buf;
    istr.getline(buf, 1000, ',');
    string nu = buf;
    sub_[LEXICON.GetAddID(old)] = LEXICON.GetAddID(nu);
  }
}
Substitution Substitution::Restrict(const set<int> & terms) const{
  Substitution ret;
  forall(run, sub_) {
    if (terms%run->first) ret.Add(run->first, run->second);
  }
  return ret;
}
set<int> GetVariables(const vector<Tuple> & v) {
  set<int> ret;
  for (uint i=0; i<v.size(); i++) 
    for (uint j=0; j<v[i].size(); j++)
      if (v[i][j]<0) ret.insert(v[i][j]);
  return ret;
}
vector<Tuple> RemoveVariableFreeTuples(const vector<Tuple> &v) {
  vector<Tuple> ret;
  for (uint i=0; i<v.size(); i++) {
    if (v[i].HasVariables()) ret.push_back(v[i]);    
  }
  return ret;
}
bool ComputeSubstitution(const Tuple & pre_sub, const Tuple & post_sub,
			 Substitution * sub){
  sub->sub_.clear();
  CHECK(pre_sub.size() == post_sub.size());
  for (uint i=0; i<pre_sub.size(); i++) {
    if (pre_sub[i] < 0) {
      if (sub->sub_.find(pre_sub[i]) != sub->sub_.end() && 
	  sub->sub_[pre_sub[i]] != post_sub[i]) {
	return false;
      }
      sub->sub_[pre_sub[i]] = post_sub[i];
    }
  }
  return true;
}
set<int> GetAllTerms(const vector<Tuple> & v) {
  set<int> ret;
  for (uint i=0; i<v.size(); i++) ret.insert(v[i].terms_.begin(), v[i].terms_.end());
  return ret;
}

Tuple StringToTuple(const string & s){
  Tuple ret;
  ret.FromString(s);
  return ret;
}
string TupleVectorToString(const vector<Tuple> &v){
  vector<string> vs;
  for (uint i=0; i<v.size(); i++) vs.push_back(v[i].ToString());
  return Join(vs, ',');
}
vector<Tuple> StringToTupleVector(const string & s){
  vector<string> v = Split(s, ',');  
  vector<Tuple> ret;
  for (uint i=0; i<v.size(); i++) ret.push_back(StringToTuple(v[i]));
  return ret;
}
string ToString(const Tuple & s, const Substitution & sub){
  string ret;
  ret += "[ ";
  for (uint i=0; i<s.size(); i++) {
    ret += LEXICON.GetString(s[i]);
    if (IsVariable(s[i])) {
      int substituted = sub.Lookup(s[i]);
      if (substituted != s[i])
	ret += "(" + LEXICON.GetString(substituted)  + ")";
    }
    ret += " ";
  }
  ret += "]";
  return ret;
}
double TuplesLnLikelihood(const vector<Tuple> &context, 
			     const vector<Tuple> &to_encode, 
			     vector<int> * arbitrary_terms){
  arbitrary_terms->clear();
  CHECK(arbitrary_terms);
  set<int> terms_seen;
  double ret = 0;
  bool encoding = false;
  // CHEAT: we should really encode the lengths of tuples
  for (uint i=0; i<context.size()+to_encode.size(); i++) {
    if (i == context.size()) encoding = true;
    const Tuple & s 
      = (i<context.size())?context[i]:to_encode[i-context.size()];
    for (uint j=0; j<s.size(); j++) {
      int w = s[j];
      if (terms_seen % w) {
	if (encoding) ret -= log(terms_seen.size());
      } else {
	if (encoding) arbitrary_terms->push_back((w<0)?-1:w);
	terms_seen.insert(w);
      }
    }    
  }
  ret += uintQuadraticLnProb(to_encode.size());
  return ret;  
}

void RenameVariablesInOrder(vector<Tuple> * v, Substitution *s){
  int next_var = 0;
  Substitution sub;
  for (uint i=0; i<v->size(); i++) {
    for (uint j=0; j<(*v)[i].size(); j++) {
      int & w_ref = (*v)[i][j];
      if (IsVariable(w_ref) && !sub.Contains(w_ref)){
	sub.Add(w_ref, Variable(next_var));
	next_var++;
      }
    }
  }
  sub.Substitute(v);
  if (s) *s = sub;
}

vector<Tuple> Canonicalize(const vector<Tuple> & v, Substitution *sub){
  vector<uint64> fprints;
  vector<Tuple> ret;
  map<uint64, int> sorted;
  for (uint i=0; i<v.size(); i++) {
    fprints.push_back(v[i].MakeVariableInsensitive().Fingerprint());
    sorted[fprints[i]] = i;
  }
  if (sorted.size() == v.size()) {
    forall(run, sorted) ret.push_back(v[run->second]);
  } else {
    // OK, we have some identical ones. 
    for (int rep=0; rep<3; rep++) {
      map<int, uint64> var_hashes;
      for (uint i=0; i<v.size(); i++) {
	for (uint j=0; j<v[i].size(); j++) {
	  if (IsVariable(v[i][j])) {
	    var_hashes[v[i][j]] += Fingerprint(fprints[i], j);
	  }
	}	
      }
      for (uint i=0; i<v.size(); i++) {
	for (uint j=0; j<v[i].size(); j++) {
	  if (IsVariable(v[i][j])) {
	    fprints[i] = 
	      Fingerprint(fprints[i], Fingerprint(var_hashes[v[i][j]], j));
	  }
	}
      }
    }
    multimap<uint64, int> order;
    for (uint i=0; i<v.size(); i++) {
      order.insert(make_pair(fprints[i], i));
    }
    forall(run, order) ret.push_back(v[run->second]);
  }
  RenameVariablesInOrder(&ret, sub);
  return ret;
}


CandidateRule CanonicalizeRule(const CandidateRule & r) {
  const vector<Tuple> & preconditions = r.first;
  const vector<Tuple> & result = r.second;
  Substitution sub;
  vector<Tuple> c_pre = Canonicalize(preconditions, &sub);
  int last_variable_in_preconditions = -1;
  forall(run, sub.sub_) 
    last_variable_in_preconditions >?= Variable(run->second);
  int next_var = last_variable_in_preconditions+1;
  vector<Tuple> c_res = result;
  for (uint i=0; i<c_res.size(); i++) 
    for (uint j=0; j<c_res[i].size(); j++) {
      int & w_ref = c_res[i][j];
      if (sub.Contains(w_ref))
	w_ref = (1<<30) + Variable(sub.Lookup(w_ref));
    }
  c_res = Canonicalize(c_res, NULL);
  for (uint i=0; i<c_res.size(); i++) 
    for (uint j=0; j<c_res[i].size(); j++) {
      int & w_ref = c_res[i][j];
      if (IsVariable(w_ref)) w_ref = Variable(Variable(w_ref)+next_var);
      else if (w_ref >= (1<<30)) w_ref = Variable(w_ref-(1<<30));
    }
  return make_pair(c_pre, c_res);
}