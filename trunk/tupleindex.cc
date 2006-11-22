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


#include "tupleindex.h"
#include "lexicon.h"

SamplingInfo::SamplingInfo() {
  sampled_ = false;
}
SamplingInfo::SamplingInfo(int position, uint32 start_hash, uint32 end_hash){
  position_ = position;
  start_hash_ = start_hash;
  end_hash_ = end_hash;
}

SamplingInfo SamplingInfo::RandomRange(int position, int denominator){
  int part = rand() % denominator;
  uint32 start = (0xFFFFFFFFF / denominator) * part;
  uint32 end = (0xFFFFFFFFF / denominator) * (part+1);
  return SamplingInfo(position, start, end);
}

TupleIndex::TupleIndex() {
  total_tuples_ = 0;
}

TupleIndex::~TupleIndex(){}

// Return a pointer to a random tuple
Tuple TupleIndex::RandomTuple() const {

  // First pick a number (0 to total_tuples-1)
  uint n = RandomUint32() % total_tuples_;

  // Run across the histogram till you find the right cardinality tuple
  forall(run, lengths_) {
    if (n < run->second) {
      Node ** np = nodes_ % AllWildcards(run->first);
      CHECK(np);
      Node *n = *np;
      return n->GetRandomTuple();
    } else {
      // Here we are still going forward in the tuple lengths map
      n-=run->second;
    }
  }

  // This should never run
  cerr << "Total Tuples: " << total_tuples_;
  forall(run, lengths_) {
    cerr << run->first << " " << run->second << endl;
  }
  CHECK(false); return 0;
}

// We select a random tuple which contains all of the terms.
// If funky_distribution is false, this selection is uniform. 
// If funky_distribution is true, we first select a situation uniformly.  
// A situation  is determined by the length of the tuple, the positions of the
// given terms in the tuple, and the first term in the tuple (which is likely
// to be a retlation name).  
Tuple
TupleIndex::GetRandomTupleContaining(const vector<int> & terms, 
				     bool funky_distribution){
  // if (funky_distribution) then this is the number of situations already
  // considered.  Otherwise, it is the number of tuples already considered.
  int already_considered = 0;
  Tuple ret;
  int n = terms.size();
  forall(run, lengths_) {
    int length = run->first;
    for (PermutationIterator run_p(n, length); !run_p.done(); ++run_p){
      const vector<int> & perm = run_p.current();
      Tuple t;
      for (uint i=0; i<perm.size(); i++) {
	t.push_back((perm[i]==EMPTY_SLOT)?WILDCARD:terms[perm[i]]);
      }
      if (t.IsConstantTuple()){
	if (tuples_ % t) {
	  already_considered++;
	  if (rand() % already_considered == 0) ret = t;
	}
	continue;
      }
      // the tuple has wildcards in it.  
      Node ** np = nodes_ % t;
      if (!np) continue;
      Node * n = *np;      
      if (funky_distribution && (s[0] == WILDCARD)) {
	CHECK(n->first_term_counts_);
	forall(run, (*n->first_term_counts_)){
	  t[0] = run->first;
	  VLOG(2) << "Expecting " << run->second << " tuples matching " 
		  << s.ToString() << endl;
	  if (t.ConstantTuple()) {
	    CHECK(tuples % t);
	    already_considered++;
	    if (rand() % already_considered == 0) ret = t;
	    continue;
	  }
	  // it's not a constant tuple
	  Node ** n2p = nodes_ % t;
	  CHECK(n2p);
	  Node * n2 = *n2p;
	  already_considered++;
	  if (rand() % already_considered == 0) {
	    ret = n->GetRandomTuple();
	  }
	}
      } else {
	bool to_select = false;
	if (funky_distribution) {
	  already_considered++;
	  to_select = (rand() % already_considered == 0);
	} else {
	  already_considered += n->satisfactions_.size();
	  to_select = (rand() % already_considered < n->satisfactions_.size());
	}
      }
    }
  }
  CHECK(ret.size());
  return ret;
}

void TupleIndex::Add(const Tuple & t) {  

  // Make sure it's a constant tuple, and it's not already in there
  CHECK(s.Pattern()==0);
  CHECK(!FindTuple(t));

  // Track the number of tuples, and their size histograms
  lengths_[t.size()]++; 
  total_tuples_++;

  tuples_.insert(t);
  
  // Run through generalizations and modify underspecified nodes
  for (GeneralizationIterator iter(t); !iter.done(); ++iter) {
    // don't use the actual tuple.
    if (iter.VariableMask()==0) continue;

    // Get the generalized tuple, and its node
    const Tuple & g = iter.generalized();
    Node * n = nodes_[g];
    // Create if necessary
    if (n==NULL) {
      n = nodes_[g] = new Node;
      n->first_term_counts_ = 0;
      if (g[0]==WILDCARD) n->first_term_counts_ = new map<int, int>;
    }

    n->specifications_.insert(make_pair(t->Fingerprint32(), t));

    // If the first term of your generalization is a wildcard
    // Mark your first term in its first term counts
    if (n->first_term_counts_) (*n->first_term_counts_)[t[0]]++;
  }
}

// Remove a constant tuple from the index
void TupleIndex::Remove(const Tuple & t) {

  // Check that it's a constant tuple
  CHECK(s.Pattern()==0);

  tuples_.erase(t);

  // Decrement the size histogram and total tuples
  lengths_[t.size()]--; total_tuples_--;
  if (lengths_[t.size()]==0) lengths_.erase(t.size());

  // Now fix its generalized underspecified nodes
  for (GeneralizationIterator iter(t); !iter.done(); ++iter) {

    // ignore the original constant tuple
    if (iter.VariableMask()==0) continue;

    // Find the underspecified node, make sure it exists
    const Tuple & g = iter.generalized();
    Node * n = nodes_[g];
    CHECK(n!=NULL);

    n->specifications_.erase(make_pair(t->Fingerprint32(), t));
    if (n->first_term_counts_) 
      SparseAdd(n->first_term_counts_, t[0], -1);

    if (n->specifications_.size()==0) {
      nodes_.erase(g);
      if (n->first_term_counts_) delete n->first_term_counts_;
      delete n;
    }
  }
}

void TupleIndex::Node::GetRange(const SamplingInfo &s, 
				set<pair<uint32, Tuple> >::iterator *start,
				set<pair<uint32, Tuple> >::iterator *end){
  *start = specifications_.lower_bound(make_pair(s.start_hash_, Tuple()));
  *end = (s.end_hash_==0xFFFFFFFF)?n->specifications_.end():
    n->specifications_.lower_bound(make_pair(s.end_hash_+1, Tuple()));
}
Tuple TupleIndex::Node::GetRandomTuple(){
  CHECK(specifications_.size());
  set<pair<uint32, Tuple> >::iterator look 
    = specifications_.lower_bound(make_pair(RandomUint32(), Tuple()));
  if (look != specifications_.end()) return *look;
  else return *(specifications_.begin());
}
int TupleIndex::Lookup(const Tuple &t, vector<Tuple> * results, 
		       SamplingInfo *sampling){
  // ignore sampling if it doesn't apply.
  if (sampling && !sampling->sampled_) sampling = NULL;
  CHECK(IsWildcardTuple(t));
  if (results) results->clear();
  if (IsConstantTuple(t)) {
    Tuple * found = tuples_ % t;
    if (!found) return 0;
    if (!sampling || sampling->Matches(*found)){
      results->push_back(*found);
      return 1;
    } else {
      return 0;
    }
  } 
  // not a constant tuple
  Node *n = nodes_ % t;
  if (!n) return 0;
  if (!sampling) {
    if (results) {
      forall(run, n->specifications_)
	results->push_back(run->second);
    }
    return n->specifications_.size();
  }
  
  set<pair<uint32, Tuple> >::iterator start; 
  set<pair<uint32, Tuple> >::iterator end;
  n->GetRange(*sampling, &start, &end);

  if (results){
    for (set<pair<uint32, Tuple> >::iterator run = start; run!=end; run++) {
      results->push_back(run->second);
    }
    return results->size();
  }
  return CountResults(start, end);
}

// Eliminate the simplest tuple first, recursively call this function
// to find matches to the pattern in the tupleindex.
bool TupleIndex::FindSatisfactions(const vector<Tuple> & pattern, 
				   const SamplingInfo * sampling,
				   vector<Substitution> * substitutions,
				   uint64 * num_satisfactions, 
				   int64 max_work,
				   uint64 * actual_work) {
  // ignore sampling if it doesn't apply  
  if (sampling && !sampling->sampled_) sampling = NULL;
  
  // Make sure the result is clean
  if (substitutions) substitutions->clear();

  // If the pattern has no tuples, there is one (trivial) match
  if (pattern.size()==0) {
    if (substitutions) substitutions->push_back(Substitution());
    if (num_satisfactions) *num_satisfactions = 1;
    if (actual_work) *actual_work = 1;
    return true;
  }

  // If the pattern is one tuple, with no duplicate variables,
  // and we don't need the results, we can just look up the answer quickly
  if (pattern.size()==1 && !substitutions) {
    if (sampling) CHECK(sampling->position_ == 0);
    if (!pattern[0].HasDuplicateVariables()) {
      if (num_satisfactions)
	*num_satisfactions = Lookup(pattern[0], 0, sampling);
      if (actual_work) *actual_work = 1; 
      return true;
    }
  }
  
  // Begin with the clause with the least Lookup matches
  int best_clause = -1;
  int64 least_work = 0;
  //FullySpecifiedNode ** matches;
  for (uint i=0; i<pattern.size(); i++) {    
    int num_matches = 
      Lookup(pattern[i], NULL, 
	     (sampling && sampling->position_==i)?sampling:NULL);
    if (i==0 || num_matches < least_work) {
      least_work = num_matches;
      best_clause = i;
    }
  }
  CHECK(best_clause != -1);

  // break out if we've done too much work reading matches
  if (max_work != UNLIMITED_WORK && least_work > max_work) return false;

  vector<Tuple> matches;
  Lookup(pattern[best_clause], &matches, 
	 (sampling && sampling->position_==best_clause)?sampling:NULL);
  
  // Create a simpler pattern without the best_clause
  vector<Tuple> simplified_pattern = pattern;
  simplified_pattern.erase(simplified_pattern.begin()+best_clause);

  // we will point sampling at a local object which we can modify.
  SamplingInfo simplified_sampling;
  if (sampling) {
    simplified_sampling = *sampling;
    sampling = &simplified_sampling;
    if (simplified_sampling.position_ == best_clause) sampling=NULL;
    else if (simplified_sampling.position_ > best_clause) {
      sampling->position_--;
    }
  }

  int total_work = least_work;
  uint64 total_num_satisfactions = 0;
  // Run over the 'matches' of the best clause
  for (uint64 match=0; match<num_matches; match++){

    // Reduce to a subproblem consisting of finding satisfactions to the
    // simplified (without the best clause) and substituted 
    // (for the current match) problem.
    vector<Tuple> substituted_pattern = simplified_pattern;
    Substitution partial_sub;
    const Tuple & pre_sub = pattern[best_clause];
    const Tuple & post_sub = matches[match]->tuple_;
    // Compute substitution of pattern to match. None -> not a real match
    if (!ComputeSubstitution(pre_sub, post_sub, &partial_sub)) continue;
    VLOG(2) << "pre=" << pre_sub.ToString() 
	    << " post=" << post_sub.ToString()
	    << " partial_sub=" << partial_sub.ToString() << endl;
    partial_sub.Substitute(&substituted_pattern);

    uint64 additional_num_satisfactions;
    uint64 added_work = 0;
    if (substitutions) {
      // Look for satisfactions to the simpler problem, but the substitutions
      // are really unions of that for the best_clause and theirs
      vector<Substitution> additional_substitutions;
      if (!FindSatisfactions
	  (substituted_pattern, 
	   sampling,
	   &additional_substitutions, 
	   &additional_num_satisfactions,
	   (max_work==UNLIMITED_WORK)?UNLIMITED_WORK:max_work-total_work,
	   &added_work)) return false;
      for (uint i=0; i<additional_substitutions.size(); i++) {
	VLOG(2) << "partial=" << partial_sub.ToString() 
		<< " additional=" << additional_substitutions[i].ToString();
	additional_substitutions[i].Add(partial_sub);
	VLOG(2) << " total=" << additional_substitutions[i].ToString() << endl;
	// This is the merged substitution, add it in
	substitutions->push_back(additional_substitutions[i]);
      }
    } else {
      // If you don't require actual substitutions this is simple, 
      // just keep looking and keeping tallies
      if (!FindSatisfactions
	  (substituted_pattern,
	   sampling,
	   0, &additional_num_satisfactions, 
	   (max_work==UNLIMITED_WORK)?UNLIMITED_WORK:max_work-total_work,
	   &added_work)) return false;
    }
    // Add up the work done, and the number of satisfactions for this match
    total_work += added_work;
    total_num_satisfactions += additional_num_satisfactions;
  }

  // Here return some housekeeping numbers, total number found and work done.
  if (num_satisfactions) *num_satisfactions = total_num_satisfactions;
  if (actual_work) *actual_work = total_work;
  return true;
}

// Just create all wildcard tuples of all lengths with the term in one position
// and use the internal Lookup
void TupleIndex::FindTerm(int w, vector<const Tuple *> *results){
  results->clear();
  set<const Tuple *> found;
  vector<const Tuple *> foo;
  forall(run, lengths_) {
    int length = run->first;
    for(int i=0; i<length; i++) {
      Tuple s;
      for (int j=0; j<length; j++) s.push_back((j==i)?w:WILDCARD);      
      Lookup(s, &foo);
      found.insert(foo.begin(), foo.end());
    }
  }
  results->insert(results->end(), found.begin(), found.end());
}

void TupleIndex::Shell() {
  string line;
  string command;
  
  while (cin >> command) {
    if (command == "add") {
      GetLine(cin, &line);
      Tuple s;
      s.FromString(line);
      Add(s);
      cout << "Added tuple " << s.ToString() << endl;
    }
    if (command == "remove") {
      GetLine(cin, &line);
      Tuple s;
      s.FromString(line);
      Remove(s);
      cout << "Removed tuple " << s.ToString() << endl;
    }
    if (command == "lookup") {
      Tuple s;
      GetLine(cin, &line);
      s.FromString(line);
      vector<const Tuple *> results;
      cout << "Looking up " << s.ToString() << endl;
      Lookup(s, &results);
      for (uint i=0; i<results.size(); i++) {
	cout << "   Found result " << results[i]->ToString() << endl;
      }
    }
    if (command == "display") {
      for (hash_map<uint64, UnderspecifiedNode*>::iterator 
	     run = underspecified_.begin(); run!=underspecified_.end(); run++) {
	cout << "underspecified: " << run->first << endl;
	for (uint i=0; i<run->second->specifications_.size(); i++) {
	  cout << "   specification: " 
	       << run->second->specifications_[i]->tuple_.ToString()
	       << endl;
	}
      }
       
    }
  }
}


