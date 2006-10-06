
#ifndef _TUPLE_H_
#define _TUPLE_H_

#include <map>
#include "util.h"

// We encode terms as integers, non-negative integers being literals, 
// and negative integers being variables.  Variables are numbered starting
// at 0, and variable i is represented as -1-i.  Note that the Variable()
// function is its own inverse.  
inline int Variable(int i) { return -1-i;} // its own inverse
inline bool IsVariable(int i) { return (i<0);}

// A Tuple is a tuple of terms.
struct Tuple{
  vector <int> terms_;
  Tuple() {}
  Tuple(const Tuple & o) { terms_ = o.terms_; }
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
  // turns all of the variables to Variable(0).
  Tuple MakeVariableInsensitive() const {
    Tuple ret = *this;
    for (uint i=0; i<terms_.size(); i++) 
      ret[i] = max(ret[i], -1);
    return ret;
  }  
  uint64 Fingerprint(uint64 level = 0) const 
  { return ::Fingerprint(terms_, level); }
  // Creates an int with bits representing whether each term is a variable.
  int Pattern() const {
    CHECK(size()<32)
    int ret = 0;
    int x=1;
    for (uint i=0; i<size(); i++) {
      if (IsVariable(terms_[i])) ret |= x;
      x<<=1;
    }
    return ret;
  }
  // are there any variables
  bool HasVariables() const { return Pattern(); }
  // are there any variables that occur twice.
  bool HasDuplicateVariables() const;
};
bool operator==(const Tuple & s1, const Tuple & s2);
Tuple AllVar0(int num_terms);
inline uint64 Fingerprint(const Tuple & s, uint64 level = 0){
  return s.Fingerprint(level);
}
// Iterates over generalizations of a tuple.  The tuple must be entirely
// literals, and the generalized tuple iterates over all tuples for which
// a possibly empty or full subset of the terms of s have been changed to 
// Variable(0).
struct GeneralizationIterator {
  GeneralizationIterator(const Tuple & s) ;
  void operator++();
  bool done() const;
  const Tuple & generalized() const;
  int pattern() const;
  int max_;
  int pattern_;
  Tuple s_;
  Tuple generalized_;
};

// TODO? maybe this stuff doesn't belong in this file
typedef pair<vector<Tuple>, vector<Tuple> > CandidateRule;
inline static vector<Tuple> Concat(const CandidateRule & r) {
  vector<Tuple> ret = r.first; 
  ret.insert(ret.end(), r.second.begin(), r.second.end()); return ret; }
inline static CandidateRule SplitOffLast(const vector<Tuple> & p) {
  return make_pair(RemoveFromVector(p, p.size()-1), 
		   vector<Tuple>(1, p.back())); }



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
  void Substitute(Tuple * s) const;
  void Substitute(vector<Tuple> * v) const {
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

set<int> GetVariables(const vector<Tuple> & v);

// removes the tuples that have no variables
vector<Tuple> RemoveVariableFreeTuples(const vector<Tuple> & v);

// computes the substitution to get from one tuple to another
bool ComputeSubstitution(const Tuple & pre_sub, const Tuple & post_sub,
			 Substitution * sub);

set<int> GetAllTerms(const vector<Tuple> & v);

Tuple StringToTuple(const string & s);
string TupleVectorToString(const vector<Tuple> &v);
vector<Tuple> StringToTupleVector(const string & s);
// shows both unsubstituted and substituted variables
string ToString(const Tuple & s, const Substitution & sub); 

// The likelihood according to a particular encoding of a vector of tuples,
// given another vector of tuples has already been encoded as context.  
// The user is responsible for adding in the ln likelihood of the terms passed
// back in arbitrary_terms .
double TuplesLnLikelihood(const vector<Tuple> &context, 
			     const vector<Tuple> &to_encode, 
			     vector<int> * arbitrary_terms);

// Given a vector of tuples, with variables, we rename the variables
// so that the first variable to occur in the vector of tuples is 
// Variable(0), the next is Variable(1), etc.
void RenameVariablesInOrder(vector<Tuple> * v, Substitution *s);

// try to put the pattern in a canonical form
// I believe that this may be NP-hard, but let's at least make an attempt. 
vector<Tuple> Canonicalize(const vector<Tuple> & v, Substitution *sub);

// Put a rule (an ordered pair of tuple vectors) in canonical form.
CandidateRule CanonicalizeRule(const CandidateRule & r);
		 
#endif
