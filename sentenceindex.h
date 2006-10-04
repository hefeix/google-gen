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


#ifndef _SENTENCEINDEX_H_
#define _SENTENCEINDEX_H_

#include "util.h"
#include "sentence.h"

// A SentenceIndex is, as it suggests, an index over sentences of literals, 
// that allows the sentences to be searched by a pattern, which is a vector 
// of sentences of literals and variables.
//
// When a sentence is added to the index, it is stored internally.  The pointer
// to that sentence returned by Add(), and all pointers to that sentence 
// returned until that sentence is removed will be the same.  
class SentenceIndex{
 public:
  SentenceIndex();
  ~SentenceIndex();
  // Adds a sentence to the index.  Returns a pointer to the static internal 
  // copy of the sentence.
  const Sentence * Add(const Sentence & s);
  // Removes a sentence from the index.
  void Remove (const Sentence & s);
  // s is composed of literals.
  // Returns a pointer to the static internal copy of s, or NULL if absent.
  const Sentence * FindSentence(const Sentence & s);
  // s is a sentence with literals and Variable(0).  The results do not 
  // require all instances of Variable(0) have the same substitution.
  void Lookup(const Sentence & s, vector<const Sentence*> * results);
  // A histogram of the lengths of the sentences in the index.
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
  bool FindSatisfactions(const vector<Sentence> & pattern, 
			 vector<Substitution> * substitutions, // can be null
			 uint64 * num_satisfactions,  // can be null
			 int64 max_work, // -1 for no limit
			 uint64 * actual_work); // can be null
  // Get a random sentence containing all of the given words.  
  // If funky_distribution is set, we first choose uniformly over the positions
  // in the sentence of the given words.  This over-represents sentences where
  // the words take a rare position.  
  const Sentence * GetRandomSentenceContaining(const vector<int> & words, 
					       bool funky_distribution);
					       
  const Sentence * RandomSentence();

  // returns all sentences containing a word.
  void FindWord(int w, vector<const Sentence* >* results); 

  // for testing.
  void Shell();  
 private:
  struct FullySpecifiedNode{
    Sentence sentence_;
    int * pos_in_lists_;
  };
  struct UnderspecifiedNode{
    vector<FullySpecifiedNode *> specifications_;
    // maps first word to number of sentences starting with that word
    map<int, int> *first_word_counts_; // present if first word is variable
  };
  hash_map<uint64, FullySpecifiedNode*> fully_specified_;
  hash_map<uint64, UnderspecifiedNode*> underspecified_;

  uint64 total_sentences_;
  map<uint64, uint64> lengths_; // number of stored sencences whith these lengths

  void LookupInternal(const Sentence & s, 
		      FullySpecifiedNode *** results, 
		      uint64 * num_results);
};

#endif
