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
#include "sentence.h"
#include "lexicon.h"
#include "probutil.h"

string Sentence::ToString() const{
  string ret;
  ret += "[ ";
  for (uint i=0; i<words_.size(); i++) {
    ret += LEXICON.GetString(words_[i]); // + "(" + itoa(words_[i]) + ")";
    ret += " ";
  }
  ret += "]";
  return ret;
}
void Sentence::FromString(const string & s){
  words_.clear();
  istringstream istr(s.c_str());
  string w;
  istr >> w;
  while ((istr >> w) && (w!="]")) {
    int i = LEXICON.GetAddID(w);
    words_.push_back(i);
  }
}
bool Sentence::HasDuplicateVariables() const {
  set<int> vars;
  for (uint i=0; i<words_.size(); i++) {
    if (words_[i] < 0) {
      if (vars % words_[i]) return true;
      vars.insert(words_[i]);
    }
  }
  return false;
}
bool operator==(const Sentence & s1, const Sentence & s2){
  if (s1.size() != s2.size()) return false;
  for (uint i=0; i<s1.size(); i++) if (s1[i]!=s2[i]) return false;
  return true;
}
Sentence AllVar0(int num_words){
  Sentence s;
  s.words_ = vector<int>(num_words, Variable(0));
  return s;
}
GeneralizationIterator::GeneralizationIterator(const Sentence & s) {
  max_ = 1 << s.size();
  s_ = generalized_ = s;
  pattern_ = 0;
}
void GeneralizationIterator::operator++(){
  int change = pattern_ ^ (pattern_ + 1);
  pattern_++;
  if (pattern_ >= max_) return;
  for (uint i=0; change != 0; i++) {
    if (pattern_ & (1 << i)) generalized_.words_[i] = -1;
    else generalized_.words_[i] = s_.words_[i];
    change >>=1;
  }
}
bool GeneralizationIterator::done() const{ return (pattern_ >= max_); }
const Sentence & GeneralizationIterator::generalized() const { 
  return generalized_; 
}
int GeneralizationIterator::pattern() const{ 
  return pattern_; 
}


int Substitution::Lookup(int variable) const{
  const int * look = sub_ % variable;
  if (look) return *look;
  return variable;
}
void Substitution::Add(int variable, int literal){
  sub_[variable] = literal;
}
void Substitution::Substitute(Sentence * s) const{
  for (uint i=0; i<s->words_.size(); i++) {
    s->words_[i] = Lookup(s->words_[i]);
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
Substitution Substitution::Restrict(const set<int> & variables) const{
  Substitution ret;
  forall(run, sub_) {
    if (variables%run->first) ret.Add(run->first, run->second);
  }
  return ret;
}
set<int> GetVariables(const vector<Sentence> & v) {
  set<int> ret;
  for (uint i=0; i<v.size(); i++) 
    for (uint j=0; j<v[i].size(); j++)
      if (v[i][j]<0) ret.insert(v[i][j]);
  return ret;
}
vector<Sentence> RemoveVariableFreeSentences(const vector<Sentence> &v) {
  vector<Sentence> ret;
  for (uint i=0; i<v.size(); i++) {
    if (v[i].HasVariables()) ret.push_back(v[i]);    
  }
  return ret;
}
bool ComputeSubstitution(const Sentence & pre_sub, const Sentence & post_sub,
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
set<int> GetAllWords(const vector<Sentence> & v) {
  set<int> ret;
  for (uint i=0; i<v.size(); i++) ret.insert(v[i].words_.begin(), v[i].words_.end());
  return ret;
}

Sentence StringToSentence(const string & s){
  Sentence ret;
  ret.FromString(s);
  return ret;
}
string SentenceVectorToString(const vector<Sentence> &v){
  vector<string> vs;
  for (uint i=0; i<v.size(); i++) vs.push_back(v[i].ToString());
  return Join(vs, ',');
}
vector<Sentence> StringToSentenceVector(const string & s){
  vector<string> v = Split(s, ',');  
  vector<Sentence> ret;
  for (uint i=0; i<v.size(); i++) ret.push_back(StringToSentence(v[i]));
  return ret;
}
string ToString(const Sentence & s, const Substitution & sub){
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
double SentencesLnLikelihood(const vector<Sentence> &context, 
			     const vector<Sentence> &to_encode, 
			     vector<int> * arbitrary_words){
  arbitrary_words->clear();
  CHECK(arbitrary_words);
  set<int> words_seen;
  double ret = 0;
  bool encoding = false;
  // CHEAT: we should really encode the lengths of sentences
  for (uint i=0; i<context.size()+to_encode.size(); i++) {
    if (i == context.size()) encoding = true;
    const Sentence & s 
      = (i<context.size())?context[i]:to_encode[i-context.size()];
    for (uint j=0; j<s.size(); j++) {
      int w = s[j];
      if (words_seen % w) {
	if (encoding) ret -= log(words_seen.size());
      } else {
	if (encoding) arbitrary_words->push_back((w<0)?-1:w);
	words_seen.insert(w);
      }
    }    
  }
  ret += uintQuadraticLnProb(to_encode.size());
  return ret;  
}

void RenameVariablesInOrder(vector<Sentence> * v, Substitution *s){
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

vector<Sentence> Canonicalize(const vector<Sentence> & v, Substitution *sub){
  vector<uint64> fprints;
  vector<Sentence> ret;
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
  const vector<Sentence> & preconditions = r.first;
  const vector<Sentence> & result = r.second;
  Substitution sub;
  vector<Sentence> c_pre = Canonicalize(preconditions, &sub);
  int last_variable_in_preconditions = -1;
  forall(run, sub.sub_) 
    last_variable_in_preconditions >?= Variable(run->second);
  int next_var = last_variable_in_preconditions+1;
  vector<Sentence> c_res = result;
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
