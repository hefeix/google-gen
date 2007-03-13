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

#ifndef _TUPLE_H_
#define _TUPLE_H_

#include <map>
#include "util.h"
#include "probutil.h"

// We encode terms as integers, non-negative integers representing literals, 
// -1 representing a wildcard, and negative integers representing variables.  
// Variables are numbered starting at 0, and variable i is represented as -2-i.
// Note that the Variable() function is its own inverse.
#define WILDCARD (-1)
inline int Variable(int i) { return -2-i;} // its own inverse
inline bool IsVariable(int i) { return (i<-1);}
inline bool IsConstant(int i) { return (i>=0);}
inline bool IsWildcard(int i) { return (i==-1);}

// A Tuple is a tuple of terms.
struct Tuple{
  vector <int> terms_;
  Tuple() {}
  Tuple(const Tuple & o) { terms_ = o.terms_; }
  Tuple(string s) { FromString(s);}
  uint size() const { return terms_.size(); }
  // A tuple can be represented as a string for human readability and 
  // writablility.  The terms are converted by the LexiconWithVariables LEXICON
  // and are space-separated.  The whole thing is surrounded by brackets.
  // e.g. (-3, 17, 17, 22) might translate to "[ $2 foo foo moo ]" 
  string ToString() const;
  void FromString(const string & s);
  // for convenience
  int & operator [](int i) {return terms_[i];}
  const int & operator [](int i) const {return terms_[i];}
  void push_back(int i) { terms_.push_back(i);}
  // turns all of the variables to wildcards.
  Tuple VariablesToWildcards() const {
    Tuple ret = *this;
    for (uint i=0; i<terms_.size(); i++) 
      if (IsVariable(ret[i])) ret[i] = WILDCARD;
    return ret;
  }  
  Tuple WildcardsToVariables() const {
    Tuple ret = *this;
    int nextvar = 0;
    for (uint c=0; c<size(); c++)
      if (terms_[c] == WILDCARD) 
	ret.terms_[c] = Variable(nextvar++);
    return ret;
  }

  uint64 Fingerprint(uint64 level = 0) const 
  { return ::Fingerprint(terms_, level); }
  // Creates an int where each bit is on if and only if the corresponding term
  // is either a variable or a wildcard.
  int VariableMask() const {
    CHECK(size()<32);
    int ret = 0;
    int x=1;
    for (uint i=0; i<size(); i++) {
      if (!IsConstant(terms_[i])) ret |= x;
      x<<=1;
    }
    return ret;
  }
  uint32 Fingerprint32() const { return (uint32) Fingerprint(0); }

  // Does this tuple meet the requirements to be these kinds of tuples.
  bool IsConstantTuple() const;
  bool IsVariableTuple() const;
  bool IsWildcardTuple() const;
  // are there any variables that occur twice.
  bool HasDuplicateVariables() const;
};
bool operator==(const Tuple & s1, const Tuple & s2);
// Returns a tuple of all wildcards.
Tuple AllWildcards(int num_terms);
inline uint64 Fingerprint(const Tuple & s, uint64 level = 0){
  return s.Fingerprint(level);
}
// Given a wildcard tuple and a constant tuple, do they match
bool MatchesWildcardTuple(const Tuple & wildcard_tuple, 
			  const Tuple & constant_tuple);
			  
// Iterates over generalizations of a tuple.  Iterates over all generalizations
// of a subset of the constants into wildcards.
struct GeneralizationIterator {
  GeneralizationIterator(const Tuple & t) ;
  void operator++();
  bool done() const;
  const Tuple & Current() const;
  int GeneralizeMask() const;
  vector<int> constant_positions_;
  int max_;
  int generalize_mask_;
  Tuple my_tuple_;
  Tuple generalized_;
};

typedef vector<Tuple> Pattern;
inline bool operator <
(const Tuple & a, const Tuple & b){return (a.terms_ < b.terms_);}

// TODO? maybe this stuff doesn't belong in this file
typedef pair<Pattern, Pattern > CandidateRule;
inline static Pattern Concat(const CandidateRule & r) {
  Pattern ret = r.first; 
  ret.insert(ret.end(), r.second.begin(), r.second.end()); return ret; }
inline static CandidateRule SplitOffLast(const Pattern & p) {
  return make_pair(RemoveFromVector(p, p.size()-1), 
		   Pattern(1, p.back())); }

// a substitution from terms to terms.
// either may be a variable or a constant term
struct Substitution {
  map<int, int> sub_;
  Substitution() {}
  Substitution(const string & s) { FromString(s);}
  void Add(int from_term, int to_term);
  void Add(const Substitution & o) {
    sub_.insert(o.sub_.begin(), o.sub_.end());
  }
  int Lookup(int term) const; // returns term if not found
  bool Contains(int term) const {return sub_ % term;}
  void Substitute(Tuple * s) const;
  void Substitute(Pattern * v) const {
    for (uint i=0; i<v->size(); i++) Substitute(&((*v)[i]));
  }
  void Substitute(CandidateRule *c){
    Substitute(&c->first);
    Substitute(&c->second);
  }
  string ToString() const;
  void FromString(const string & s);
  uint64 Fingerprint(uint64 level = 0) const { 
    return ::Fingerprint(sub_, level);
  }
  // Provide a restricted substitution to a set of terms
  Substitution Restrict(const set<int> & terms) const;
  Substitution Reverse() const;

  // First unused variable as a positive number
  uint FirstUnusedVariable() {
    for (int i=0; true; i++) if (!Contains(Variable(i))) return i;
  }
  
  // If a substitution is a subset of another one
  // domain is a subset of other's domain, and ranges are equal
  bool IsSubsetOf(const Substitution& s) {
    forall (run, sub_) {
      if (s.Lookup(run->first) != run->second) return false;
    }
    return true;
  }

  uint size() const { return sub_.size(); }

};

inline bool operator <(const Substitution & a, const Substitution & b){
  return (a.sub_ < b.sub_);
}

inline bool operator==(const Substitution & a, const Substitution & b) {
  return (a.sub_ == b.sub_);
}

inline uint64 Fingerprint(const Substitution & s, uint64 level = 0){
  return s.Fingerprint(level);
}
Substitution Union(const Substitution & s1, const Substitution & s2);

set<int> GetVariables(const Tuple & t);
set<int> GetVariables(const Pattern & v);

// find ths connected components of the pattern.  
// Two tuples are connected if they share variables
// Returns the number of connected components.
// components is optional and is set to a vector aligned with p, represnenting
// a mapping from tuples to densely numbered component ids.
int GetConnectedComponents(const Pattern & p, vector<int> *components);
// Simpler interface if you only care whether the pattern has at most one 
// component.
bool IsConnectedPattern(const Pattern & p);

// removes the tuples that have no variables
Pattern RemoveVariableFreeTuples(const Pattern & v);

// computes the substitution to get from one tuple to another
bool ComputeSubstitution(const Tuple & pre_sub, const Tuple & post_sub,
			 Substitution * sub);

set<int> GetAllTerms(const Pattern & v);

Tuple StringToTuple(const string & s);
string TupleVectorToString(const Pattern &v);
string CandidateRuleToString(const CandidateRule &c);
Pattern StringToTupleVector(const string & s);
// shows both unsubstituted and substituted variables (for display)
string ToString(const Tuple & s, const Substitution & sub); 

// Given a vector of tuples, with variables, we rename the variables
// so that the first variable to occur in the vector of tuples is 
// Variable(0), the next is Variable(1), etc.
void RenameVariablesInOrder(Pattern * v, Substitution *s);

// try to put the pattern in a canonical form
// I believe that this may be NP-hard, but let's at least make an attempt. 
// TODO: comment what is canonical
Pattern Canonicalize(const Pattern & v, Substitution *sub);

// Put a rule (an ordered pair of tuple vectors) in canonical form.
CandidateRule CanonicalizeRule(const CandidateRule & r, Substitution * out_sub);
		 
#endif
