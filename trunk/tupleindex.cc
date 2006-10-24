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


#include "tupleindex.h"
#include "lexicon.h"

TupleIndex::TupleIndex() {
  total_tuples_ = 0;
}
TupleIndex::~TupleIndex(){
}
const Tuple * TupleIndex::RandomTuple(){
  uint n = RandomInt() % total_tuples_;
  forall(run, lengths_) {
    if (n < run->second) {
      FullySpecifiedNode ** results;
      uint64 num_results;
      LookupInternal(AllVar0(run->first), &results, &num_results);
      CHECK(num_results==run->second);
      return &results[n]->tuple_;
    } else {
      n-=run->second;
    }
  }
  cerr << "Total Tuples: " << total_tuples_;
  forall(run, lengths_) {
    cerr << run->first << " " << run->second << endl;
  }
  CHECK(false); 
  return 0;
}
const Tuple * 
TupleIndex::GetRandomTupleContaining(const vector<int> & terms, 
					   bool funky_distribution){
  int n = terms.size();
  // okay, for now we'll always use the funky distribution
  vector<pair<FullySpecifiedNode **, int> > patterns;
  forall(run, lengths_) {
    int length = run->first;
    for (PermutationIterator run_p(n, length); !run_p.done(); ++run_p){
      const vector<int> & perm = run_p.current();
      Tuple s;
      for (uint i=0; i<perm.size(); i++) {
	s.push_back((perm[i]==EMPTY_SLOT)?WILDCARD:terms[perm[i]]);
      }
      if (funky_distribution && (s[0] == WILDCARD)) {
	UnderspecifiedNode ** unp = underspecified_ % s.Fingerprint();
	if (!unp) continue;
	UnderspecifiedNode * un = *unp;
	CHECK(un->first_term_counts_);
	forall(run, (*un->first_term_counts_)){
	  s[0] = run->first;
	  VLOG(2) << "Expecting " << run->second << " tuples matching " 
		  << s.ToString() << endl;
	  FullySpecifiedNode ** results; uint64 num_results;
	  LookupInternal(s, &results, &num_results);
	  CHECK(num_results == (uint)run->second);
	  patterns.push_back(make_pair(results, num_results));
	}
      } else {
	FullySpecifiedNode ** results; uint64 num_results;
	LookupInternal(s, &results, &num_results);
	if (results) patterns.push_back(make_pair(results, num_results));
      }
    }
  }
  int num_patterns = patterns.size();
  int total_tuples = 0;
  for (uint i=0; i<patterns.size(); i++) total_tuples += patterns[i].second;
  if (total_tuples==0) return 0;
  int pattern_num = 0;
  if (funky_distribution){
    pattern_num = rand() % num_patterns;
  } else {
    int tuple_num = rand() % total_tuples;
    for (uint i=0; i<patterns.size(); i++) {
      tuple_num -= patterns[i].second;
      if (tuple_num<0) {
	pattern_num = i;
	break;
      }
    }
  }
  int tuple_num = rand() % patterns[pattern_num].second;
  return &(patterns[pattern_num].first[tuple_num]->tuple_);
}

/*
const Tuple * TupleIndex::RandomTupleContaining(int w){
  int total=0;
  uint n=0;
  for (int rep=0; rep<2; rep++){
    if (rep==1) {
      CHECK(total_tuples_);
      n = RandomInt() % total;
    }
    forall(run, lengths_) {
      int length = run->first;
      for (int i=0; i<length; i++) {
	Tuple s = AllVar0(length);
	s[i] = w;
	FullySpecifiedNode ** results;
	uint64 num_results;
	LookupInternal(s, &results, &num_results);
	if (rep==0) {
	  total+=num_results;
	} else {
	  if (n<num_results){
	    return &results[n]->tuple_;
	  } else {
	    n-=num_results;
	  }
	}
      }
    }
  }
  CHECK(false); 
  return 0;
}
*/
const Tuple * TupleIndex::GetAdd(const Tuple &s) {
  const Tuple * t = FindTuple(s);
  if (!t) return add(s);
}
const Tuple * TupleIndex::Add(const Tuple & s) {  
  CHECK(s.Pattern()==0);
  CHECK(!FindTuple(s));
  uint64 fp = s.Fingerprint();
  lengths_[s.size()]++; total_tuples_++;
  FullySpecifiedNode * n = new FullySpecifiedNode;
  fully_specified_[fp] = n;
  n->tuple_ = s;
  n->pos_in_lists_ = new int[1 << s.size()];
  for (GeneralizationIterator iter(s); !iter.done(); ++iter) {
    int pattern = iter.pattern();
    if (pattern==0) continue;
    const Tuple & g = iter.generalized();
    uint64 gfp = g.Fingerprint();
    UnderspecifiedNode * un = underspecified_[gfp];
    if (un==0) {
      un = underspecified_[gfp] = new UnderspecifiedNode;
      un->first_term_counts_ = 0;
      if (g[0]==WILDCARD) un->first_term_counts_ = new map<int, int>;
    }
    n->pos_in_lists_[pattern] = un->specifications_.size();
    un->specifications_.push_back(n);
    if (un->first_term_counts_) (*un->first_term_counts_)[s[0]]++;
  }
  return &(n->tuple_);
}

void TupleIndex::Remove(const Tuple & s) {
  CHECK(s.Pattern()==0);
  uint64 fp = s.Fingerprint();
  hash_map<uint64, FullySpecifiedNode*>::iterator 
    look = fully_specified_.find(fp);
  if (look == fully_specified_.end()) {
    cerr << "Tuple does not exist" << endl;
    return;
  }
  lengths_[s.size()]--; total_tuples_--;
  if (lengths_[s.size()]==0) lengths_.erase(s.size());
  FullySpecifiedNode * n = look->second;
  fully_specified_.erase(look);

  for (GeneralizationIterator iter(s); !iter.done(); ++iter) {
    int pattern = iter.pattern();
    if (pattern==0) continue;
    const Tuple & g = iter.generalized();
    uint64 gfp = g.Fingerprint();
    UnderspecifiedNode * un = underspecified_[gfp];
    CHECK(un!=NULL);
    int pos_in_list = n->pos_in_lists_[pattern];
    if (pos_in_list+1 < (int)un->specifications_.size()) {
      un->specifications_[pos_in_list] 
	= un->specifications_[un->specifications_.size()-1];
      un->specifications_[pos_in_list]->pos_in_lists_[pattern] = pos_in_list;
    }
    if (un->first_term_counts_) {
      SparseAdd(un->first_term_counts_, s[0], -1);
    }
    un->specifications_.pop_back();
    if (un->specifications_.size()==0) {
      underspecified_.erase(gfp);
      if (un->first_term_counts_) delete un->first_term_counts_;
      delete un;
    }
  }
  delete n;  
}
void TupleIndex::LookupInternal(const Tuple & s, 
				   FullySpecifiedNode *** results, 
				   uint64 * num_results) {
  uint64 fp = s.MakeVariableInsensitive().Fingerprint();
  if (s.Pattern()==0) {
    FullySpecifiedNode ** look = fully_specified_ % fp; 
    if (results) *results = look;
    if (num_results) *num_results = look ? 1 : 0;
  } else {
    UnderspecifiedNode ** look = underspecified_ % fp;
    if (look) {
      UnderspecifiedNode * n = *look;
      if (results) *results = &(n->specifications_[0]);
      if (num_results) *num_results = n->specifications_.size();
    } else {
      if (results) *results = 0;
      if (num_results) *num_results = 0;
    }
  }
}
void TupleIndex::Lookup(const Tuple & s, 
			   vector<const Tuple*> * results) {
  results->clear();
  FullySpecifiedNode ** internal_results;
  uint64 num_results;
  LookupInternal(s, &internal_results, &num_results);
  for (uint i=0; i<num_results; i++) 
    results->push_back(&internal_results[i]->tuple_);
}
const Tuple * TupleIndex::FindTuple(const Tuple & s) {
  vector<const Tuple *> results;
  Lookup(s, &results);
  if(results.size() > 1) {
    cerr << "More than one result for " << s.ToString() << endl;
    CHECK(results.size() <= 1);
  }
  if (results.size()) return results[0];
  return 0;
}


bool TupleIndex::FindSatisfactions(const vector<Tuple> & pattern, 
				     vector<Substitution> * substitutions,
				     uint64 * num_satisfactions, 
				     int64 max_work,
				     uint64 * actual_work) {
  if (substitutions) substitutions->clear();
  if (pattern.size()==0) {
    if (substitutions) substitutions->push_back(Substitution());
    if (num_satisfactions) *num_satisfactions = 1;
    if (actual_work) *actual_work = 1;
    return true;
  }
  if (pattern.size()==1 && !substitutions) {
    if (!pattern[0].HasDuplicateVariables()) {
      LookupInternal(pattern[0], 0, num_satisfactions);
      if (actual_work) *actual_work = 1; 
      return true;
    }
  }
  int best_clause = -1;
  int64 least_work = 0;
  uint64 num_matches;
  FullySpecifiedNode ** matches;
  for (uint i=0; i<pattern.size(); i++) {
    LookupInternal(pattern[i], &matches, &num_matches);
    if (i==0 || (int)num_matches < least_work) {
      least_work = num_matches;
      best_clause = i;
    }
  }
  CHECK(best_clause != -1);
  LookupInternal(pattern[best_clause], &matches, &num_matches);
  // cerr << pattern[best_clause].ToString() 
  //     << " LookupInternal returned " << num_matches << " results\n"; 
  vector<Tuple> simplified_pattern = pattern;
  simplified_pattern.erase(simplified_pattern.begin()+best_clause);
  if (max_work != UNLIMITED_WORK && least_work > max_work) return false;
  int total_work = least_work;
  uint64 total_num_satisfactions = 0;
  for (uint64 match=0; match<num_matches; match++){
    vector<Tuple> substituted_pattern = simplified_pattern;
    Substitution partial_sub;
    const Tuple & pre_sub = pattern[best_clause];
    const Tuple & post_sub = matches[match]->tuple_;
    if (!ComputeSubstitution(pre_sub, post_sub, &partial_sub)) continue;
    VLOG(2) << "pre=" << pre_sub.ToString() 
	    << " post=" << post_sub.ToString()
	    << " partial_sub=" << partial_sub.ToString() << endl;
    partial_sub.Substitute(&substituted_pattern);
    uint64 additional_num_satisfactions;
    uint64 added_work = 0;
    if (substitutions) {
      vector<Substitution> additional_substitutions;
      if (!FindSatisfactions
	  (substituted_pattern, 
	   &additional_substitutions, 
	   &additional_num_satisfactions,
	   (max_work==UNLIMITED_WORK)?UNLIMITED_WORK:max_work-total_work,
	   &added_work)) return false;
      for (uint i=0; i<additional_substitutions.size(); i++) {
	VLOG(2) << "partial=" << partial_sub.ToString() 
		<< " additional=" << additional_substitutions[i].ToString();
	additional_substitutions[i].Add(partial_sub);
	VLOG(2) << " total=" << additional_substitutions[i].ToString() << endl;
	substitutions->push_back(additional_substitutions[i]);
      }
    } else {
      if (!FindSatisfactions
	  (substituted_pattern, 
	   0, &additional_num_satisfactions, 
	   (max_work==UNLIMITED_WORK)?UNLIMITED_WORK:max_work-total_work,
	   &added_work)) return false;
    }
    total_work += added_work;
    total_num_satisfactions += additional_num_satisfactions;
  }
  if (num_satisfactions) *num_satisfactions = total_num_satisfactions;
  if (actual_work) *actual_work = total_work;
  return true;
}
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


