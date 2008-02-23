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
#include "probutil.h"

void VariablesToWildcardsInline(Tuple *t){
  for (uint i=0; i<t->size(); i++) {
    if (IsVariable((*t)[i])) (*t)[i] = WILDCARD;
  }
}  
Tuple VariablesToWildcards(const Tuple &t){
  Tuple ret = t;
  VariablesToWildcardsInline(&ret); 
  return ret;
}

void WildcardsToVariablesInline(Tuple *t) {
  int nextvar = 0;
  for (uint c=0; c<t->size(); c++)
    if ((*t)[c] == WILDCARD) 
      (*t)[c] = IntToVariable(nextvar++);
}
Tuple WildcardsToVariables(const Tuple &t){
  Tuple ret = t;
  WildcardsToVariablesInline(&ret);
  return ret;
}

// Creates an int where each bit is on if and only if the corresponding term
// is either a variable or a wildcard.
int VariableMask(const Tuple & t) {
  CHECK(t.size()<32);
  int ret = 0;
  int x=1;
  for (uint i=0; i<t.size(); i++) {
    if (IsVariable(t[i]) || IsWildcard(t[i])) ret |= x;
    x<<=1;
  }
  return ret;
}

bool HasDuplicateVariables(const Tuple & t) {
  set<Object> vars;
  for (uint i=0; i<t.size(); i++) {
    if (IsVariable(t[i])) {
      if (vars % t[i]) return true;
      vars.insert(t[i]);
    }
  }
  return false;
}
bool IsConstantTuple(const Tuple  & t) {
  for (uint i=0; i<t.size(); i++)
    if (IsVariable(t[i]) || IsWildcard(t[i])) return false;
  return true;
}
bool IsVariableTuple(const Tuple & t) {
  for (uint i=0; i<t.size(); i++)
    if (IsWildcard(t[i])) return false;
  return true;
}
bool IsWildcardTuple(const Tuple & t) {
  for (uint i=0; i<t.size(); i++)
    if (IsVariable(t[i])) return false;
  return true;
}
Tuple AllWildcards(int num_terms){
  return Tuple(num_terms, WILDCARD);
}
// Given a wildcard tuple and a constant tuple, do they match
bool MatchesWildcardTuple(const Tuple & wildcard_tuple, 
			  const Tuple & constant_tuple) {
  CHECK(wildcard_tuple.size() == constant_tuple.size());
  CHECK(IsWildcardTuple(wildcard_tuple));
  CHECK(IsConstantTuple(constant_tuple));
  for (uint i=0; i<wildcard_tuple.size(); i++){
    if ((!IsWildcard(wildcard_tuple[i]))
	 && wildcard_tuple[i] != constant_tuple[i]) return false;
  }
  return true;
}
GeneralizationIterator::GeneralizationIterator(const Tuple & t) {
  my_tuple_ = t;
  generalized_ = my_tuple_;
  for (uint i=0; i<my_tuple_.size(); i++) {
    if (my_tuple_[i] != WILDCARD) {
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
    if (generalize_mask_ & (1 << i)) 
      generalized_[constant_positions_[i]] = WILDCARD;
    else generalized_[constant_positions_[i]] 
	   = my_tuple_[constant_positions_[i]];
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

void Substitute(const Map & m, Tuple *t) {
  for (uint i=0; i<t->size(); i++) (*t)[i] = Replacement(m, (*t)[i]);
};
void Substitute(const Map & m, MPattern *p){
  for (uint i=0; i<p->size(); i++) Substitute(m, &((*p)[i]));
}
void Substitute(const Map & m, CandidateRule *r){
  Substitute(m, &(r->first));
  Substitute(m, &(r->second));
}
OTuple Substitute(const Map & m, OTuple t) {
  Tuple t2 = t.Data();
  Substitute(m, &t2);
  return OTuple::Make(t2);
}
Pattern Substitute(const Map & m, const Pattern &p) {
  Pattern ret;
  for (uint c=0; c<p.size(); c++) {
    ret.push_back(Substitute(m, p[c]));
  }
  return ret;
}
OPattern Substitute(const Map & m, OPattern p) {
  return OPattern::Make(Substitute(m, p.Data()));
}

Object DeepSubstitute(const Map & m, Object o) {
  if (o.GetType() == Object::OPATTERN) {
    Pattern ret;
    OPattern p = o;
    for (uint i=0; i<p.Data().size(); i++)
      ret.push_back(OTuple(DeepSubstitute(m, p.Data()[i])));
    return OPattern::Make(ret);
  }
  if (o.GetType() == Object::OTUPLE) {
    Tuple ret;
    OTuple t = o;
    for (uint i=0; i<t.Data().size(); i++)
      ret.push_back(DeepSubstitute(m, t.Data()[i]));
    return OTuple::Make(ret);
  }
  return Replacement(m, o);
}
bool DeepSubstitutePossible(Object o) {
  if (o.GetType() == Object::OPATTERN) {
    Pattern ret;
    OPattern p = o;
    for (uint i=0; i<p.Data().size(); i++)
      if (DeepSubstitutePossible(p.Data()[i])) return true;
    return false;
  }
  if (o.GetType() == Object::OTUPLE) {
    Tuple ret;
    OTuple t = o;
    for (uint i=0; i<t.Data().size(); i++)
      if (DeepSubstitutePossible(t.Data()[i])) return true;	
    return false;
  }
  if (o.GetType() == Object::VARIABLE 
      || o.GetType() == Object::ESCAPE) return true;
  return false;
}

bool Add(Map * b, Object key, Object value) {
  forall(run, *b) {
    if (run->first == key) return (run->second == value);
  }
  b->push_back(make_pair(key, value));
  return true;
}
bool Add(Map *b, const Map & b2) {
  forall(run, b2) {
    if (!Add(b, run->first, run->second)) return false;   
  }
  return true;
}
Map Reverse(const Map & b) {
  Map ret;
  forall(run, b) {
    Add(&ret, run->second, run->first);
  }
  return ret;
}
Variable FirstUnusedVariable(const Map & m){
  for (uint i=0; true; i++) {
    Variable v = IntToVariable(i);
    if (!(m % Object(v))) return v;
  }
}
bool IsSubsetOf(const Map & m1, const Map & m2) {
  forall (run, m1) {
    const Object * find = m2 % run->first;
    if (!find) return false;
    if (*find != run->second) return false;
  }
  return true;
}

set<Variable> GetVariables(const Tuple & t) {
  set<Variable> ret;
  for (uint i=0; i<t.size(); i++)
    if (IsVariable(t[i])) ret.insert(t[i]);
  return ret;
}
set<Variable> GetVariables(const MPattern & v) {
  set<Variable> ret;
  for (uint i=0; i<v.size(); i++) 
    for (uint j=0; j<v[i].size(); j++)
      if (IsVariable(v[i][j])) ret.insert(v[i][j]);
  return ret;
}
set<Variable> GetVariables(const Pattern & v) {
  set<Variable> ret;
  for (uint i=0; i<v.size(); i++) 
    for (uint j=0; j<v[i].size(); j++)
      if (IsVariable(v[i].Data()[j])) ret.insert(v[i].Data()[j]);
  return ret;
}

// If a pattern's variables are connected
bool IsConnectedPattern(const Pattern& v) {
  return (GetConnectedComponents(v, NULL)<=1);
}
int GetConnectedComponents(const Pattern &p, vector<int> *components){
  map<Variable, set<int> > var_to_tuple;
  for (uint c=0; c<p.size(); c++)
    for (uint c2=0; c2<p[c].size(); c2++)
      if (IsVariable(p[c][c2])) var_to_tuple[p[c][c2]].insert(c);
  map<int, set<int> > tuple_adjacency;
  forall(run, var_to_tuple) forall(run2, run->second) forall (run3, run->second)
    tuple_adjacency[*run2].insert(*run3);
  return ConnectedComponents(p.size(), tuple_adjacency, components);
}

MPattern RemoveVariableFreeTuples(const MPattern &v) {
  MPattern ret;
  for (uint i=0; i<v.size(); i++) {
    if (!IsConstantTuple(v[i])) 
      ret.push_back(v[i]);
  }
  return ret;
}
bool ExtendSubstitution(const Tuple & pre_sub, const Tuple & post_sub, 
			 Map *sub){
  Map tmp;
  if (!sub) sub = &tmp;

  CHECK(pre_sub.size() == post_sub.size());
  for (uint i=0; i<pre_sub.size(); i++) {
    if (IsVariable(pre_sub[i])) {
      if (!Add(sub, pre_sub[i], post_sub[i])) return false;
    }
  }
  return true;
}
set<Object> GetAllTerms(const MPattern & v) {
  set<Object> ret;
  for (uint i=0; i<v.size(); i++) 
    ret.insert(v[i].begin(), v[i].end());
  return ret;
}

string ToString(const MPattern &p) {
  return OPattern::Make(MPatternToPattern(p)).ToString();
}
istream & operator >>(istream & input, MPattern &p){
  OPattern o;
  input >> o;
  p = PatternToMPattern(o.Data());
  return input;
}
string ToString(const Map &b) {
  return OMap::Make(b).ToString();
}
string ToString(const CandidateRule & r){
  return ToString(r.first) + "->" + ToString(r.second);
}

string ToString(const Tuple & s, const Map & sub){
  string ret;
  ret += "( ";
  for (uint i=0; i<s.size(); i++) {
    ret += s[i].ToString();
    Object substituted = Replacement(sub, s[i]);
    if (substituted != s[i])
      ret += "<" + substituted.ToString()  + ">";
    ret += " ";
  }
  ret += ")";
  return ret;
}

void RenameVariablesInOrder(MPattern * v, Map *m){
  int next_var = 0;
  Map sub;
  for (uint i=0; i<v->size(); i++) {
    for (uint j=0; j<(*v)[i].size(); j++) {
      Object & w_ref = (*v)[i][j];
      if (IsVariable(w_ref) && !(sub % w_ref)) {
	Add(&sub, w_ref, IntToVariable(next_var));
	next_var++;
      }
    }
  }
  Substitute(sub, v);
  if (m) *m = sub;
}

// smaller is better.
int PatternReadability(const MPattern & p){
  int ret =0;
  map<Variable, set<int> > m; // maps variables to sets of positions.
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
MPattern SortPatternForReadability(MPattern p){
  if (p.size() < 7){
    // iterate over all permutations
    MPattern best = p;
    int m = PatternReadability(p);
    for (PermutationIterator iter(p.size(), p.size()); !iter.done(); ++iter){
      MPattern q;
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
	MPattern q = p;
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

MPattern Canonicalize(const MPattern & v, Map*sub){

  // Figerprints of all of the tuples, with variables changed to wildcards
  // aligned with the pattern
  vector<uint64> fprints;

  MPattern ret;

  // Map between fingerprints and position in the pattern
  map<uint64, int> sorted;
  for (uint i=0; i<v.size(); i++) {
    fprints.push_back(Hash32(VariablesToWildcards(v[i])));
    sorted[fprints[i]] = i;
  }
  if (sorted.size() == v.size()) {
    forall(run, sorted) ret.push_back(v[run->second]);
  } else {
    // OK, we have some identical ones. 
    for (int rep=0; rep<3; rep++) {
      map<Variable, uint32> var_hashes;
      for (uint i=0; i<v.size(); i++) {
	for (uint j=0; j<v[i].size(); j++) {
	  if (IsVariable(v[i][j])) {
	    var_hashes[Variable(v[i][j])] += Hash32(fprints[i], j);
	  }
	}
      }
      for (uint i=0; i<v.size(); i++) {
	for (uint j=0; j<v[i].size(); j++) {
	  if (IsVariable(v[i][j])) {
	    fprints[i] = 
	      Hash32(fprints[i], Hash32(var_hashes[v[i][j]], j));
	  }
	}
      }
    }
    multimap<uint32, int> order;
    for (uint i=0; i<v.size(); i++) {
      order.insert(make_pair(fprints[i], i));
    }
    forall(run, order) ret.push_back(v[run->second]);
  }
  ret = SortPatternForReadability(ret);
  RenameVariablesInOrder(&ret, sub);
  return ret;
}



CandidateRule CanonicalizeRule(const CandidateRule & r, 
			       Map * out_sub) {
  const MPattern & preconditions = r.first;
  const MPattern & result = r.second;
  Map sub;
  MPattern c_pre = Canonicalize(preconditions, &sub);

  VLOG(1) << "original: " << ToString(r)
	  << " canonical pre: " << MPatternToOPattern(c_pre)
	  << " sub:" << OMap::Make(sub) << endl;

  int last_variable_in_preconditions = -1;
  forall(run, sub) 
    last_variable_in_preconditions >?= VariableToInt(run->second);

  VLOG(1) << "last variable:" << last_variable_in_preconditions << endl;
  int next_var = last_variable_in_preconditions+1;

  MPattern c_res = result;
  // escape the variables in the results that are also in the preconditions
  for (uint i=0; i<c_res.size(); i++) 
    for (uint j=0; j<c_res[i].size(); j++) {
      Object & w_ref = c_res[i][j];
      if (sub % w_ref) {
	w_ref = Escape::Make(Replacement(sub, w_ref));
      }
      else if (w_ref.GetType() == Object::ESCAPE) {
	w_ref = Escape::Make(w_ref);
      }
    }
  VLOG(1) << "step 2 c_res:" << MPatternToOPattern(c_res) << endl;
  Map res_sub;
  // canonicalize the result
  c_res = Canonicalize(c_res, &res_sub);
  VLOG(1) << "step 3 c_res:" << MPatternToOPattern(c_res) << endl;
  // make the new variables in the result not conflict with the
  // variables in the precondition.
  for (uint i=0; i<c_res.size(); i++) 
    for (uint j=0; j<c_res[i].size(); j++) {
      Object & w_ref = c_res[i][j];
      if (w_ref.GetType() == Object::VARIABLE)
	w_ref = IntToVariable(VariableToInt(Variable(w_ref)) + next_var);
      if (w_ref.GetType() == Object::ESCAPE)
	w_ref = Escape(w_ref).Data();
    }
  VLOG(1) << "step 4 c_res:" << MPatternToOPattern(c_res) << endl;
  // report the substitution
  if (out_sub) {
    *out_sub = res_sub;
    forall (run, *out_sub)
      run->second 
      = IntToVariable(VariableToInt(Variable(run->second))+next_var);
    Add(out_sub, sub);
  }
  return make_pair(c_pre, c_res);
}
