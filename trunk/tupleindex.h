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

#ifndef _TUPLEINDEX_H_
#define _TUPLEINDEX_H_

#include "util.h"
#include "tuple.h"

// TODO: Try to make this a class static constant to not pollute global namespace
#define UNLIMITED_WORK (-1) 

class SearchNode;

struct SamplingInfo{
  bool sampled_;
  uint32 position_;
  uint32 start_hash_; // inclusive
  uint32 end_hash_; // inclusive
  double GetFraction() const;
  SamplingInfo(); // creates an unsampled SamplingInfo
  SamplingInfo(int position, uint32 start_hash, uint32 end_hash);
  bool RemovePosition(uint32 position);
  SamplingInfo LimitToPosition(uint32 position) const;
  bool Matches(const Tuple& t) const;
  static SamplingInfo RandomRange(int position, int denominator, int part=-1);
  static SamplingInfo StringToSamplingInfo(const string& s);
  string ToString() const; //  not the inverse of the above.
  static SamplingInfo Unsampled() { return SamplingInfo();}
};

  
// A TupleIndex is, as it suggests, an index over tuples of constants, 
// that allows the tuples to be searched by a pattern, which is a vector 
// of tuples of constants and variables.
//
// When a tuple is added to the index, it is stored internally.  The pointer
// to that tuple returned by Add(), and all pointers to that tuple 
// returned until that tuple is removed will be the same.

class TupleIndex{
 public:
  TupleIndex();
  ~TupleIndex();

  // Adds a constant tuple to the index.  
  void Add(Tuple t);

  // Removes a tuple from the index.
  void Remove (Tuple t);

  // t is composed of constants.
  bool Contains(const Tuple &t){ return tuples_ % t;}

  // A histogram of the lengths of the tuples in the index.
  inline const map<uint64, uint64> & Lengths() const {return lengths_; }

  // returns all tuples containing a term.
  void FindTerm(int w, vector<Tuple>* results); 

  // t is a wildcard tuple.
  // returns number of results.
  // sampling ignores its position field.
  int Lookup(const Tuple & s, vector<Tuple> * results, 
	     const SamplingInfo * sampling = NULL);

  // Searches over the index to match a pattern.
  // Pattern can contain literals and variables.  Multiple instances of the
  // same variable only match the same literal.  The function returns 
  // substitutions for the variables in pattern which correspond to
  // satisfactions.  If you don't need the actual substitutions, and just 
  // want to know how many there are, pass NULL for substitutions and 
  // a non-null pointer for num_satisfactions.  
  bool FindSatisfactions(const vector<Tuple> & pattern,
			 const SamplingInfo & sampling,
			 vector<Substitution> * substitutions, // can be null
			 uint64 * num_satisfactions,  // can be null
			 int64 * max_work_now);

  // Get a random tuple containing all of the given terms.  
  // If funky_distribution is set, we first choose uniformly over the positions
  // in the tuple of the given terms.  This over-represents tuples where
  // the terms take a rare position.  
  bool GetRandomTupleContaining(Tuple * ret,
				const vector<int> & terms, 
				bool funky_distribution);
  
  Tuple RandomTuple() const;
  bool  GetRandomTupleMatching(Tuple wildcard_tuple, Tuple * result);

  // for testing.
  void Shell();  

 private:
  struct Node {
    set<pair<uint32, Tuple> > specifications_;
    // maps first term to number of tuples starting with that term
    // present if first term is variable
    map<int, int> *first_term_counts_; 
    Tuple GetRandomTuple() const;
    void GetRange(const SamplingInfo & s, 
		  set<pair<uint32, Tuple> >::iterator * start,
		  set<pair<uint32, Tuple> >::iterator * end);
  };
  map<Tuple, Node*> nodes_;
  set<Tuple> tuples_;

  uint64 total_tuples_;
  map<uint64, uint64> lengths_; // number of stored tuples with these lengths
};

#endif
