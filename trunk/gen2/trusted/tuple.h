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
inline Object Replacement(const Map & m, const Object &o) {
  const Object * found = m%o;
  if (found) return *found;
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

VariableSet GetDomainVariables(OMap m);

// Adds a key-value pair to a Map.
void Add(Map *m, Object key, Object value);
// union=
void Add(Map *m, const Map & m2);

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

VariableSet GetVariables(const Tuple & t);
VariableSet GetVariables(const MPattern & v);
VariableSet GetVariables(const Pattern & v);

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

// computes the substitution to get from one tuple to another.
// The substitution may only use Variables as keys.  Returns NULL if
// no substitution exists.
bool ComputeSubstitution(const Tuple & pre_sub, const Tuple & post_sub, 
			 Map *result);

set<Object> GetAllTerms(const MPattern & v);

//Pattern OTupleToPattern(OTuple ot);
//OTuple PatternToOTuple(const Pattern &p);
string ToString(const MPattern &p);
istream & operator >>(istream & input, MPattern &p);
string ToString(const Map &m);
istream & operator >>(istream &input, Map &m);
string ToString(const CandidateRule &r);

string ToString(const Tuple & s, const Map & sub); 

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

#endif
