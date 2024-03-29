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
#include "model.h"
#include "searchtree.h"

SamplingInfo::SamplingInfo() {
  sampled_ = false;
}
SamplingInfo::SamplingInfo(int position, uint32 start_hash, uint32 end_hash){
  sampled_ = true;
  position_ = position;
  start_hash_ = start_hash;
  end_hash_ = end_hash;
}

SamplingInfo SamplingInfo::RandomRange(int position, int denominator, int part){
  if (part < 0) part = rand() % denominator;
  uint32 start = (0xFFFFFFFF / denominator) * part;
  uint32 end = (0xFFFFFFFF / denominator) * (part+1);
  return SamplingInfo(position, start, end);
}

SamplingInfo SamplingInfo::LimitToPosition(uint32 position) const{
  if (!sampled_ || (position != position_)) return SamplingInfo();
  return SamplingInfo(0, start_hash_, end_hash_);
}

bool SamplingInfo::RemovePosition(uint32 position) {
  if (!sampled_) return false;
  if (position < position_) {
    position_--;
    return true;
  }
  if (position == position_) {
    sampled_ = false;
    return false;
  }
  return true;
}

bool SamplingInfo::Matches(const Tuple& t) const {
  if (!sampled_) return true;
  uint32 fp = t.Fingerprint32();
  return (start_hash_ <= fp) && (fp <= end_hash_);
}

SamplingInfo SamplingInfo::StringToSamplingInfo(const string& s) {
  istringstream istr(s);
  int position, denominator, part;
  istr >> position >> denominator >> part;
  return RandomRange(position, denominator, part);
}

string SamplingInfo::ToString() const {
  if (!sampled_) return "Unsampled";
  return "{pos=" + itoa(position_) + " 1/" 
    + dtoa(1.0/GetFraction()) + "}";
}
double SamplingInfo::GetFraction() const {
  if (!sampled_) return 1.0;
  return (end_hash_-start_hash_+1.0)/pow(2,32);
}
TupleIndex::TupleIndex() {
  total_tuples_ = 0;
}

TupleIndex::~TupleIndex(){}

// Return a pointer to a random tuple
Tuple TupleIndex::RandomTuple() const {

  // First pick a number (0 to total_tuples-1)
  uint n = RandomUInt32() % total_tuples_;

  // Run across the histogram till you find the right cardinality tuple
  forall(run, lengths_) {
    if (n < run->second) {
      Node * const * np = nodes_ % AllWildcards(run->first);
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
  CHECK(false); return Tuple();
}

// Needs to get a wildcard tuple
bool TupleIndex::GetRandomTupleMatching(Tuple wildcard_tuple, Tuple * result) {
  CHECK(wildcard_tuple.IsWildcardTuple());

  // Get rid of constants first
  if (wildcard_tuple.IsConstantTuple()) {
    bool found = tuples_ % wildcard_tuple;
    if (!found) return false;
    *result = wildcard_tuple;
    return true;
  }

  // not a constant tuple
  Node * const * np = nodes_ % wildcard_tuple;
  if (!np) return false;
  *result = (*np)->GetRandomTuple();
  return true;
}

// We select a random tuple which contains all of the terms.
// If funky_distribution is false, this selection is uniform. 
// If funky_distribution is true, we first select a situation uniformly.  
// A situation  is determined by the length of the tuple, the positions of the
// given terms in the tuple, and the first term in the tuple (which is likely
// to be a retlation name).  
// What if there's no answer???
bool
TupleIndex::GetRandomTupleContaining(Tuple * ret,
				     const vector<int> & terms, 
				     bool funky_distribution){
  CHECK(ret != NULL);
  ret->terms_.clear();

  // if (funky_distribution) then this is the number of situations already
  // considered.  Otherwise, it is the number of tuples already considered.
  int already_considered = 0;
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
	  if (rand() % already_considered == 0) 
	    *ret = t;
	}
	continue;
      }
      // the tuple has wildcards in it.  
      Node ** np = nodes_ % t;
      if (!np) continue;
      Node * n = *np;      
      if (funky_distribution && (t[0] == WILDCARD)) {
	CHECK(n->first_term_counts_);
	forall(run, (*n->first_term_counts_)){
	  t[0] = run->first;
	  VLOG(2) << "Expecting " << run->second << " tuples matching " 
		  << t.ToString() << endl;
	  if (t.IsConstantTuple()) {
	    CHECK(tuples_ % t);
	    already_considered++;
	    if (rand() % already_considered == 0) 
	      *ret = t;
	    continue;
	  }
	  // it's not a constant tuple
	  Node ** n2p = nodes_ % t;
	  CHECK(n2p);
	  Node * n2 = *n2p;
	  already_considered++;
	  if (rand() % already_considered == 0) {
	    *ret = n2->GetRandomTuple();
	  }
	}
      } else {
	bool to_select = false;
	if (funky_distribution) {
	  already_considered++;
	  to_select = (rand() % already_considered == 0);
	} else {
	  already_considered += n->specifications_.size();
	  to_select = (rand() % already_considered < (int) n->specifications_.size());
	}
      }
    }
  }
  if (ret->size()) return true;
  return false;
}

void TupleIndex::Add(Tuple t) {  

  // Make sure it's a constant tuple, and it's not already in there
  if (!t.IsConstantTuple()) {
    VLOG(0) << "Trying to add Tuple " << t.ToString() << endl;
  }
  CHECK(t.IsConstantTuple());
  CHECK(!(tuples_ % t));

  // Track the number of tuples, and their size histograms
  lengths_[t.size()]++; 
  total_tuples_++;

  tuples_.insert(t);
  
  // Run through generalizations and modify underspecified nodes
  for (GeneralizationIterator iter(t); !iter.done(); ++iter) {
    // don't use the actual tuple.
    if (iter.GeneralizeMask()==0) continue;

    // Get the generalized tuple, and its node
    const Tuple & g = iter.Current();
    Node * n = nodes_[g];
    // Create if necessary
    if (n==NULL) {
      n = nodes_[g] = new Node;
      n->first_term_counts_ = 0;
      if (g[0]==WILDCARD) n->first_term_counts_ = new map<int, int>;
    }

    n->specifications_.insert(make_pair(t.Fingerprint32(), t));

    // If the first term of your generalization is a wildcard
    // Mark your first term in its first term counts
    if (n->first_term_counts_) (*n->first_term_counts_)[t[0]]++;
  }
}

// Remove a constant tuple from the index
void TupleIndex::Remove(Tuple t) {

  // Check that it's a constant tuple
  CHECK(t.IsConstantTuple());

  tuples_.erase(t);

  // Decrement the size histogram and total tuples
  lengths_[t.size()]--; total_tuples_--;
  if (lengths_[t.size()]==0) lengths_.erase(t.size());

  // Now fix its generalized underspecified nodes
  for (GeneralizationIterator iter(t); !iter.done(); ++iter) {

    // ignore the original constant tuple
    if (iter.GeneralizeMask()==0) continue;

    // Find the underspecified node, make sure it exists
    const Tuple & g = iter.Current();
    Node * n = nodes_[g];
    CHECK(n!=NULL);

    n->specifications_.erase(make_pair(t.Fingerprint32(), t));
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
  *end = (s.end_hash_==0xFFFFFFFF)? specifications_.end():
    specifications_.lower_bound(make_pair(s.end_hash_+1, Tuple()));
}
Tuple TupleIndex::Node::GetRandomTuple() const {
  CHECK(specifications_.size());
  set<pair<uint32, Tuple> >::iterator look 
    = specifications_.lower_bound(make_pair(RandomUInt32(), Tuple()));
  if (look != specifications_.end()) return look->second;
  else return (specifications_.begin())->second;
}

int TupleIndex::Lookup(const Tuple &t, vector<Tuple> * results, 
		       const SamplingInfo *sampling) {
  // ignore sampling if it doesn't apply.
  if (sampling && !sampling->sampled_) sampling = NULL;
  CHECK(t.IsWildcardTuple());
  if (results) results->clear();
  if (t.IsConstantTuple()) {
    bool found = tuples_ % t;
    if (!found) return 0;
    if (!sampling || sampling->Matches(t)){
      if (results) results->push_back(t);
      return 1;
    } else {
      return 0;
    }
  } 
  // not a constant tuple
  Node * const * np = nodes_ % t;
  if (!np) return 0;
  Node *n = *np; CHECK(n);
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
  return CountRange(start, end);
}

bool TupleIndex::FindSatisfactions(const vector<Tuple> & pattern, 
				   const SamplingInfo & sampling, 
				   vector<Substitution> *substitutions, 
				   uint64 * num_satisfactions, 
				   int64 * max_work_now){
  SearchTree tree(pattern, this, NULL, sampling);
  if (!tree.L1_Search(max_work_now)) return false;
  uint64 num_sat = tree.GetNumSatisfactions();
  if (num_satisfactions) *num_satisfactions = num_sat;
   if (substitutions) {
     MOREWORK(num_sat);
     tree.GetSubstitutions(substitutions);
     if(substitutions->size() != num_sat){
       cerr << "Wrong number of substitutions for " 
	    << TupleVectorToString(pattern)
	    << " #sat=" << num_sat << " #subs=" << substitutions->size()
	    << endl;
       CHECK(false);
     }
   }
  return true;
}

// Just create all wildcard tuples of all lengths with the term in one position
// and use the internal Lookup
void TupleIndex::FindTerm(int w, vector<Tuple> *results){
  results->clear();
  set<Tuple> found;
  vector<Tuple> foo;
  forall(run, lengths_) {
    int length = run->first;
    for(int i=0; i<length; i++) {
      Tuple t;
      for (int j=0; j<length; j++) t.push_back((j==i)?w:WILDCARD);      
      Lookup(t, &foo);
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
      vector<Tuple> results;
      cout << "Looking up " << s.ToString() << endl;
      Lookup(s.VariablesToWildcards(), &results);
      for (uint i=0; i<results.size(); i++) {
	cout << "   Found result " << results[i].ToString() << endl;
      }
    }

    if (command == "display") {
      /*
      for (hash_map<uint64, UnderspecifiedNode*>::iterator 
	     run = underspecified_.begin(); run!=underspecified_.end(); run++) {
	cout << "underspecified: " << run->first << endl;
	for (uint i=0; i<run->second->specifications_.size(); i++) {
	  cout << "   specification: " 
	       << run->second->specifications_[i]->tuple_.ToString()
	       << endl;
	}
      } 
      */
    }

  }
}


