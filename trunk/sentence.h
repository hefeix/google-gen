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


#ifndef _SENTENCE_H_
#define _SENTENCE_H_

#include <map>
#include "util.h"

// We encode words as integers, non-negative integers being literals, 
// and negative integers being variables.  Variables are numbered starting
// at 0, and variable i is represented as -1-i.  Note that the Variable()
// function is its own inverse.  
inline int Variable(int i) { return -1-i;} // its own inverse
inline bool IsVariable(int i) { return (i<0);}

// A Sentence is a tuple of words.
struct Sentence{
  vector <int> words_;
  Sentence() {}
  Sentence(const Sentence & o) { words_ = o.words_; }
  uint size() const { return words_.size(); }
  // A sentence can be represented as a string for human readability and 
  // writablility.  The words are converted by the LexiconWithVariables LEXICON
  // and are space-separated.  The whole thing is surrounded by brackets.
  // e.g. (-3, 17, 17, 22) might translate to "[ $2 foo foo moo ]" 
  string ToString() const;
  void FromString(const string & s);
  // for convenience
  int & operator [](int i) {return words_[i];}
  const int & operator [](int i) const {return words_[i];}
  void push_back(int i) { words_.push_back(i);}
  // turns all of the variables to Variable(0).
  Sentence MakeVariableInsensitive() const {
    Sentence ret = *this;
    for (uint i=0; i<words_.size(); i++) 
      ret[i] = max(ret[i], -1);
    return ret;
  }  
  uint64 Fingerprint(uint64 level = 0) const 
  { return ::Fingerprint(words_, level); }
  // Creates an int with bits representing whether each word is a variable.
  int Pattern() const {
    CHECK(size()<32)
    int ret = 0;
    int x=1;
    for (uint i=0; i<size(); i++) {
      if (IsVariable(words_[i])) ret |= x;
      x<<=1;
    }
    return ret;
  }
  // are there any variables
  bool HasVariables() const { return Pattern(); }
  // are there any variables that occur twice.
  bool HasDuplicateVariables() const;
};
bool operator==(const Sentence & s1, const Sentence & s2);
Sentence AllVar0(int num_words);
inline uint64 Fingerprint(const Sentence & s, uint64 level = 0){
  return s.Fingerprint(level);
}
// Iterates over generalizations of a sentence.  The sentence must be entirely
// literals, and the generalized sentence iterates over all sentences for which
// a possibly empty or full subset of the words of s have been changed to 
// Variable(0).
struct GeneralizationIterator {
  GeneralizationIterator(const Sentence & s) ;
  void operator++();
  bool done() const;
  const Sentence & generalized() const;
  int pattern() const;
  int max_;
  int pattern_;
  Sentence s_;
  Sentence generalized_;
};

// TODO? maybe this stuff doesn't belong in this file
typedef pair<vector<Sentence>, vector<Sentence> > CandidateRule;
inline static vector<Sentence> Concat(const CandidateRule & r) {
  vector<Sentence> ret = r.first; 
  ret.insert(ret.end(), r.second.begin(), r.second.end()); return ret; }
inline static CandidateRule SplitOffLast(const vector<Sentence> & p) {
  return make_pair(RemoveFromVector(p, p.size()-1), 
		   vector<Sentence>(1, p.back())); }



// a substitution from variables to literals.
struct Substitution {
  map<int, int> sub_;
  Substitution() {}
  Substitution(const string & s) { FromString(s);}
  void Add(int variable, int literal);
  void Add(const Substitution & o) {
    sub_.insert(o.sub_.begin(), o.sub_.end());
  }
  int Lookup(int variable) const; // returns variable if not found
  bool Contains(int variable) const {return sub_ % variable;}
  void Substitute(Sentence * s) const;
  void Substitute(vector<Sentence> * v) const {
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
  Substitution Restrict(const set<int> & variables) const;
};
inline uint64 Fingerprint(const Substitution & s, uint64 level = 0){
  return s.Fingerprint(level);
}

set<int> GetVariables(const vector<Sentence> & v);

// removes the sentences that have no variables
vector<Sentence> RemoveVariableFreeSentences(const vector<Sentence> & v);

// computes the substitution to get from one sentence to another
bool ComputeSubstitution(const Sentence & pre_sub, const Sentence & post_sub,
			 Substitution * sub);

set<int> GetAllWords(const vector<Sentence> & v);

Sentence StringToSentence(const string & s);
string SentenceVectorToString(const vector<Sentence> &v);
vector<Sentence> StringToSentenceVector(const string & s);
// shows both unsubstituted and substituted variables
string ToString(const Sentence & s, const Substitution & sub); 

// The likelihood according to a particular encoding of a vector of sentences,
// given another vector of sentences has already been encoded as context.  
// The user is responsible for adding in the ln likelihood of the words passed
// back in arbitrary_words .
double SentencesLnLikelihood(const vector<Sentence> &context, 
			     const vector<Sentence> &to_encode, 
			     vector<int> * arbitrary_words);

// Given a vector of sentences, with variables, we rename the variables
// so that the first variable to occur in the vector of sentences is 
// Variable(0), the next is Variable(1), etc.
void RenameVariablesInOrder(vector<Sentence> * v, Substitution *s);

// try to put the pattern in a canonical form
// I believe that this may be NP-hard, but let's at least make an attempt. 
vector<Sentence> Canonicalize(const vector<Sentence> & v, Substitution *sub);

// Put a rule (an ordered pair of sentence vectors) in canonical form.
CandidateRule CanonicalizeRule(const CandidateRule & r);
		 
#endif
