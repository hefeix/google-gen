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
#include "objects.h"

void VariablesToWildcardsInline(Tuple * t);
Tuple VariablesToWildcards(const Tuple &t);

void WildcardsToVariablesInline(Tuple * t);
Tuple WildcardsToVariables(const Tuple &t);

// Creates an int where each bit is on if and only if the corresponding term
// is either a variable or a wildcard.
int VariableMask(const Tuple & t);

// Does this tuple meet the requirements to be these kinds of tuples.
bool IsConstantTuple(const Tuple & t); // There are no variables or wildcards
bool IsVariableTuple(const Tuple & t); // There are no wildcards
bool IsWildcardTuple(const Tuple & t); // There are no variables
// are there any variables that occur twice.
bool HasDuplicateVariables(const Tuple & t);

// Returns a tuple of all wildcards.
Tuple AllWildcards(int num_terms);

inline Tuple Concat(const Tuple & t1, const Tuple &t2) {
  Tuple ret = t1;
  ret.insert(ret.end(), t2.begin(), t2.end());
  return ret;
}

inline int NumWildcards(const Tuple & t) {
  int ret =0;
  for (uint i=0; i<t.size(); i++) if (t[i] == WILDCARD) ret++;
  return ret;
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


// TODO? maybe this stuff doesn't belong in this file
typedef pair<MPattern, MPattern > CandidateRule;

inline static MPattern Concat(const CandidateRule & r) {
  MPattern ret = r.first; 
  ret.insert(ret.end(), r.second.begin(), r.second.end()); return ret; }
inline static CandidateRule SplitOffLast(const MPattern & p) {
  return make_pair(RemoveFromVector(p, p.size()-1), 
		   MPattern(1, p.back())); }

// Either return the value associated with a given key, or return the key if
// it is not in the map.  
inline Object Replacement(const Map & b, const Object &o) {
  forall(run, b) if (run->first == o) return run->second;
  if (o.GetType() == Object::ESCAPE) return Escape(o).Data();
  return o;
};
void Substitute(const Map &m, Tuple * t);
void Substitute(const Map &m,  MPattern * p);
void Substitute(const Map &m, CandidateRule *r);

OTuple Substitute(const Map &m, OTuple t);
Pattern Substitute(const Map &m, const Pattern& p);
OPattern Substitute(const Map &m, OPattern p);

// recursively substitutes Patterns and tuples.  
Object DeepSubstitute(const Map & m, Object o);

// returns true if DeepSubstitute could change this object for some context.
bool DeepSubstitutePossible(Object o);


// Adds a key value pair to a Map or changes the existing value associated
// with the key if the key already exists.
void AddChangeValue(Map *m, Object key, Object value);

// Adds a key-value pair to a Map.
// Returns true if the key is not in the map yet or if the key-value pair
// is in the map.  Returns false (and does not modify the map) if the key 
// is already associated with a different value.
bool Add(Map *b, Object key, Object value);

// union=
// returns true on success (if the addition is comatible) 
bool Add(Map *b, const Map & m2);

// Provide a restricted substitution to a set of keys.  The keys are objects
template<class SType>
OMap Restrict(const OMap & m, const SType & keys){
  return OMap::Make(Restrict(m.Data(), keys));  
}

Map Reverse(const Map & m);

Variable FirstUnusedVariable(const Map & m);

  
// Is m1 a subset of m2.
// domain of m1 is subset of domain of m2, and m1 is the restriction of m2
// onto the domain of m1.
bool IsSubsetOf(const Map  & m1, const Map & m2);

set<Variable> GetVariables(const Tuple & t);
set<Variable> GetVariables(const MPattern & v);
set<Variable> GetVariables(const Pattern & v);

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
MPattern RemoveVariableFreeTuples(const MPattern & v);

// Extends a substitution so that it takes the tuple pre_sub to the tuple 
// post_sub.  
// returns false on any incompatibility.
// Only variables can be substituted for.
bool ExtendSubstitution(const Tuple & pre_sub, const Tuple & post_sub, Map *sub);

set<Object> GetAllTerms(const MPattern & v);

//Pattern OTupleToPattern(OTuple ot);
//OTuple PatternToOTuple(const Pattern &p);
string ToString(const MPattern &p);
istream & operator >>(istream & input, MPattern &p);
string ToString(const Map &m);
istream & operator >>(istream &input, Map &m);
string ToString(const CandidateRule &r);

string ToString(const Tuple & s, const Map & sub); 

inline string ToString(const Tuple & s) { return OTuple::Make(s).ToString(); }

// Given a vector of tuples, with variables, we rename the variables
// so that the first variable to occur in the vector of tuples is 
// Variable(0), the next is Variable(1), etc.
// Set m to the substitution from the old variables to the new variables
void RenameVariablesInOrder(MPattern * v, Map *m);

// try to put the pattern in a canonical form
// I believe that this may be NP-hard, but let's at least make an attempt. 
// TODO: comment what is canonical
MPattern Canonicalize(const MPattern & v, Map *sub);

// Put a rule (an ordered pair of tuple vectors) in canonical form.
CandidateRule CanonicalizeRule(const CandidateRule & r, Map * out_sub);

inline Tuple MakeTuple(const Object & o1){
  Tuple t;
  t.push_back(o1);
  return t;
}
inline Tuple MakeTuple(const Object & o1,
		       const Object & o2){
  Tuple t;
  t.push_back(o1);
  t.push_back(o2);
  return t;
}
inline Tuple MakeTuple(const Object & o1,
		       const Object & o2,
		       const Object & o3){
  Tuple t;
  t.push_back(o1);
  t.push_back(o2);
  t.push_back(o3);
  return t;
}
inline Tuple MakeTuple(const Object & o1,
		       const Object & o2,
		       const Object & o3,
		       const Object & o4){
  Tuple t;
  t.push_back(o1);
  t.push_back(o2);
  t.push_back(o3);
  t.push_back(o4);
  return t;
}
inline Tuple MakeTuple(const Object & o1,
		       const Object & o2,
		       const Object & o3,
		       const Object & o4,
		       const Object & o5){
  Tuple t;
  t.push_back(o1);
  t.push_back(o2);
  t.push_back(o3);
  t.push_back(o4);
  t.push_back(o5);
  return t;
}

#endif
