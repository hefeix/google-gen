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


#ifndef _TUPLEINDEX_H_
#define _TUPLEINDEX_H_

#include "util.h"
#include "tuple.h"

// A TupleIndex is, as it suggests, an index over tuples of literals, 
// that allows the tuples to be searched by a pattern, which is a vector 
// of tuples of literals and variables.
//
// When a tuple is added to the index, it is stored internally.  The pointer
// to that tuple returned by Add(), and all pointers to that tuple 
// returned until that tuple is removed will be the same.  
class TupleIndex{
 public:
  TupleIndex();
  ~TupleIndex();
  // Adds a tuple to the index.  Returns a pointer to the static internal 
  // copy of the tuple.
  const Tuple * Add(const Tuple & s);
  // Removes a tuple from the index.
  void Remove (const Tuple & s);
  // s is composed of literals.
  // Returns a pointer to the static internal copy of s, or NULL if absent.
  const Tuple * FindTuple(const Tuple & s);
  // s is a tuple with literals and wildcards.  The results do not 
  // require all instances of wildcard have the same substitution.
  void Lookup(const Tuple & s, vector<const Tuple*> * results);
  // A histogram of the lengths of the tuples in the index.
  inline const map<uint64, uint64> & Lengths() const {return lengths_; }
  // Searches over the index to match a pattern.
  // Pattern can contain literals and variables.  Multiple instances of the
  // same variable only match the same literal.  You can limit the work done
  // by setting max_work to something other than -1.  The function returns 
  // substitutions for the variables in pattern which correspond to
  // satisfactions.  If you don't need the actual substitutions, and just 
  // want to know how many there are, pass NULL for substitutions and 
  // a non-null pointer for num_satisfactions.  
  // You can tell how much work the function did using the parameter actual_work
  // The function returns true if it doesn't run out of time
  bool FindSatisfactions(const vector<Tuple> & pattern, 
			 vector<Substitution> * substitutions, // can be null
			 uint64 * num_satisfactions,  // can be null
			 int64 max_work, // -1 for no limit
			 uint64 * actual_work); // can be null
  // Get a random tuple containing all of the given terms.  
  // If funky_distribution is set, we first choose uniformly over the positions
  // in the tuple of the given terms.  This over-represents tuples where
  // the terms take a rare position.  
  const Tuple * GetRandomTupleContaining(const vector<int> & terms, 
					       bool funky_distribution);
					       
  const Tuple * RandomTuple();

  // returns all tuples containing a term.
  void FindTerm(int w, vector<const Tuple* >* results); 

  // for testing.
  void Shell();  
 private:
  struct FullySpecifiedNode{
    Tuple tuple_;
    int * pos_in_lists_;
  };
  struct UnderspecifiedNode{
    vector<FullySpecifiedNode *> specifications_;
    // maps first term to number of tuples starting with that term
    map<int, int> *first_term_counts_; // present if first term is variable
  };
  hash_map<uint64, FullySpecifiedNode*> fully_specified_;
  hash_map<uint64, UnderspecifiedNode*> underspecified_;

  uint64 total_tuples_;
  map<uint64, uint64> lengths_; // number of stored sencences whith these lengths

  void LookupInternal(const Tuple & s, 
		      FullySpecifiedNode *** results, 
		      uint64 * num_results);
};

#endif
