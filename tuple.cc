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
bool Tuple::IsConstantTuple() const{
  for (uint i=0; i<size(); i++) 
    if (!IsConstant(terms_[i])) return false;
  return true;
}
bool Tuple::IsVariableTuple() const{
  for (uint i=0; i<size(); i++) 
    if (IsWildcard(terms_[i])) return false;
  return true;
}
bool Tuple::IsWildcardTuple() const{
  for (uint i=0; i<size(); i++) 
    if (IsVariable(terms_[i])) return false;
  return true;
}
bool operator==(const Tuple & s1, const Tuple & s2){
  if (s1.size() != s2.size()) return false;
  for (uint i=0; i<s1.size(); i++) 
    if (s1[i]!=s2[i]) return false;
  return true;
}
Tuple AllWildcards(int num_terms){
  Tuple s;
  s.terms_ = vector<int>(num_terms, WILDCARD);
  return s;
}
// Given a wildcard tuple and a constant tuple, do they match
bool MatchesWildcardTuple(const Tuple & wildcard_tuple, 
			  const Tuple & constant_tuple){  
  CHECK(wildcard_tuple.size() == constant_tuple.size());
  CHECK(wildcard_tuple.IsWildcardTuple());
  CHECK(constant_tuple.IsConstantTuple());
  for (uint i=0; i<wildcard_tuple.size(); i++){
    if (IsConstant(wildcard_tuple[i]) && wildcard_tuple[i] != constant_tuple[i])
      return false;
  }
  return true;
}
GeneralizationIterator::GeneralizationIterator(const Tuple & t) {
  my_tuple_ = generalized_ = t;
  for (uint i=0; i<my_tuple_.size(); i++) {
    if (IsConstant(my_tuple_[i])) {
      constant_positions_.push_back(i);
    }
  }
  max_ = 1 << constant_positions_.size();
  generalize_mask_ = 0;
}
void GeneralizationIterator::operator++(){
  int change = generalize_mask_ ^ (generalize_mask_ + 1);
  generalize_mask_++;
  if (generalize_mask_ >= max_) return;
  for (uint i=0; change != 0; i++) {
    if (generalize_mask_ & (1 << i)) generalized_.terms_[constant_positions_[i]] = WILDCARD;
    else generalized_.terms_[constant_positions_[i]] 
	   = my_tuple_.terms_[constant_positions_[i]];
    change >>=1;
  }
}
bool GeneralizationIterator::done() const{ return (generalize_mask_ >= max_); }
const Tuple & GeneralizationIterator::Current() const { 
  return generalized_; 
}
int GeneralizationIterator::GeneralizeMask() const{ 
  return generalize_mask_;
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

Substitution Substitution::Reverse() const {
  Substitution ret;
  forall(run, sub_)
    ret.sub_.insert(make_pair(run->second, run->first));
  return ret;
}

Substitution Union(const Substitution & s1, const Substitution & s2){
  Substitution u = s1;
  u.Add(s2);
  return u;
}
set<int> GetVariables(const Tuple &t) {
  set<int> ret;
  for (uint i=0; i<t.size(); i++)
    if (IsVariable(t[i])) ret.insert(t[i]);
  return ret;
}
set<int> GetVariables(const Pattern & v) {
  set<int> ret;
  for (uint i=0; i<v.size(); i++) 
    for (uint j=0; j<v[i].size(); j++)
      if (IsVariable(v[i][j])) ret.insert(v[i][j]);
  return ret;
}

// If a pattern's variables are connected
bool IsConnectedPattern(const Pattern& v) {
  return (GetConnectedComponents(v, NULL)<=1);
}
int GetConnectedComponents(const Pattern &p, vector<int> *components){
  map<int, set<int> > var_to_tuple;
  for (uint c=0; c<p.size(); c++)
    for (uint c2=0; c2<p[c].size(); c2++)
      if (IsVariable(p[c][c2])) var_to_tuple[p[c][c2]].insert(c);
  map<int, set<int> > tuple_adjacency;
  forall(run, var_to_tuple) forall(run2, run->second) forall (run3, run->second)
    tuple_adjacency[*run2].insert(*run3);
  return ConnectedComponents(p.size(), tuple_adjacency, components);
}

Pattern RemoveVariableFreeTuples(const Pattern &v) {
  Pattern ret;
  for (uint i=0; i<v.size(); i++) {
    if (!v[i].IsConstantTuple()) 
      ret.push_back(v[i]);
  }
  return ret;
}
bool ComputeSubstitution(const Tuple & pre_sub, const Tuple & post_sub,
			 Substitution * substitution){
  Substitution sub;
  CHECK(pre_sub.size() == post_sub.size());
  for (uint i=0; i<pre_sub.size(); i++) {
    if (pre_sub[i] < 0) {
      if (sub.sub_.find(pre_sub[i]) != sub.sub_.end() && 
	  sub.sub_[pre_sub[i]] != post_sub[i]) {
	return false;
      }
      sub.sub_[pre_sub[i]] = post_sub[i];
    }
  }
  if (substitution) *substitution = sub;
  return true;
}
set<int> GetAllTerms(const Pattern & v) {
  set<int> ret;
  for (uint i=0; i<v.size(); i++) 
    ret.insert(v[i].terms_.begin(), v[i].terms_.end());
  return ret;
}

Tuple StringToTuple(const string & s){
  Tuple ret;
  ret.FromString(s);
  return ret;
}
string TupleVectorToString(const Pattern &v){
  vector<string> vs;
  for (uint i=0; i<v.size(); i++) vs.push_back(v[i].ToString());
  return Join(vs, ',');
}
Pattern StringToTupleVector(const string & s){
  vector<string> v = Split(s, ',');  
  Pattern ret;
  for (uint i=0; i<v.size(); i++) ret.push_back(StringToTuple(v[i]));
  return ret;
}
string CandidateRuleToString(const CandidateRule & r){
  return TupleVectorToString(r.first) + "->" + TupleVectorToString(r.second);
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

void RenameVariablesInOrder(Pattern * v, Substitution *s){
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

// smaller is better.
int PatternReadability(const Pattern & p){
  int ret =0;
  map<int, set<int> > m; // maps variables to sets of positions.
  int pos =0;
  for (uint i=0; i<p.size(); i++) {
    for (uint j=0; j<p[i].size(); j++) {
      pos++;
      if (IsVariable(p[i][j])) m[p[i][j]].insert(pos);
    }
  }
  forall(run, m) forall(run2, run->second) forall(run3, run->second)
    ret += abs(*run2-*run3);
  return ret;
}
Pattern SortPatternForReadability(Pattern p){
  if (p.size() < 7){
    // iterate over all permutations
    Pattern best = p;
    int m = PatternReadability(p);
    for (PermutationIterator iter(p.size(), p.size()); !iter.done(); ++iter){
      Pattern q;
      for (uint i=0; i<p.size(); i++) q.push_back(p[iter.current()[i]]);
      int u = PatternReadability(q);
      if (u<m) {
	m = u;
	best = q;
      }
    }
    return best;
  }
  // heuristic search over permutations.
  while(true){

    bool any_better = false;
    for (uint source=0; source<p.size(); source++) {
      for (uint dest=0; dest<p.size(); dest++) {
	Pattern q = p;
	q[dest] = p[source];
	uint write_ptr = 0; if (write_ptr==dest) write_ptr++;
	for (uint read_ptr=0; read_ptr<p.size(); read_ptr++) {
	  if (read_ptr==source) continue;
	  q[write_ptr] = p[read_ptr];
	  write_ptr++;
	  if (write_ptr==dest) write_ptr++;
	}
	if (PatternReadability(q) < PatternReadability(p)) {
	  p = q;
	  any_better = true;
	}	
      }
    }
    if (!any_better) break;
  }
  return p;
}

Pattern Canonicalize(const Pattern & v, Substitution *sub){

  // Figerprints of all of the tuples, with variables changed to wildcards
  // aligned with the pattern
  vector<uint64> fprints;

  Pattern ret;

  // Map between fingerprints and position in the pattern
  map<uint64, int> sorted;
  for (uint i=0; i<v.size(); i++) {
    fprints.push_back(v[i].VariablesToWildcards().Fingerprint());
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
  ret = SortPatternForReadability(ret);
  RenameVariablesInOrder(&ret, sub);
  return ret;
}


CandidateRule CanonicalizeRule(const CandidateRule & r, Substitution * out_sub) {
  const Pattern & preconditions = r.first;
  const Pattern & result = r.second;
  Substitution sub;
  Pattern c_pre = Canonicalize(preconditions, &sub);
  int last_variable_in_preconditions = -1;
  forall(run, sub.sub_) 
    last_variable_in_preconditions >?= Variable(run->second);
  int next_var = last_variable_in_preconditions+1;
  Pattern c_res = result;
  for (uint i=0; i<c_res.size(); i++) 
    for (uint j=0; j<c_res[i].size(); j++) {
      int & w_ref = c_res[i][j];
      if (sub.Contains(w_ref))
	w_ref = (1<<30) + Variable(sub.Lookup(w_ref));
    }
  Substitution res_sub;
  c_res = Canonicalize(c_res, &res_sub);
  for (uint i=0; i<c_res.size(); i++) 
    for (uint j=0; j<c_res[i].size(); j++) {
      int & w_ref = c_res[i][j];
      if (IsVariable(w_ref)) w_ref = 
			       Variable(Variable(w_ref)+next_var);
      else if (w_ref >= (1<<30)) w_ref = Variable(w_ref-(1<<30));
    }

  if (out_sub) {
    *out_sub = res_sub;
    forall (run, out_sub->sub_)
      run->second = Variable(Variable(run->second)+next_var);
    out_sub->Add(sub);
  }

  return make_pair(c_pre, c_res);
}
