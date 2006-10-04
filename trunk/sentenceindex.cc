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


#include "sentenceindex.h"
#include "lexicon.h"

SentenceIndex::SentenceIndex() {
  total_sentences_ = 0;
}
SentenceIndex::~SentenceIndex(){
}
const Sentence * SentenceIndex::RandomSentence(){
  uint n = RandomInt() % total_sentences_;
  forall(run, lengths_) {
    if (n < run->second) {
      FullySpecifiedNode ** results;
      uint64 num_results;
      LookupInternal(AllVar0(run->first), &results, &num_results);
      CHECK(num_results==run->second);
      return &results[n]->sentence_;
    } else {
      n-=run->second;
    }
  }
  cerr << "Total Sentences: " << total_sentences_;
  forall(run, lengths_) {
    cerr << run->first << " " << run->second << endl;
  }
  CHECK(false); 
  return 0;
}
const Sentence * 
SentenceIndex::GetRandomSentenceContaining(const vector<int> & words, 
					   bool funky_distribution){
  int n = words.size();
  // okay, for now we'll always use the funky distribution
  vector<pair<FullySpecifiedNode **, int> > patterns;
  forall(run, lengths_) {
    int length = run->first;
    for (PermutationIterator run_p(n, length); !run_p.done(); ++run_p){
      const vector<int> & perm = run_p.current();
      Sentence s;
      for (uint i=0; i<perm.size(); i++) {
	s.push_back((perm[i]==-1)?-1:words[perm[i]]);
      }
      if (funky_distribution && (s[0] == -1)) {
	UnderspecifiedNode ** unp = underspecified_ % s.Fingerprint();
	if (!unp) continue;
	UnderspecifiedNode * un = *unp;
	CHECK(un->first_word_counts_);
	forall(run, (*un->first_word_counts_)){
	  s[0] = run->first;
	  VLOG(2) << "Expecting " << run->second << " sentences matching " 
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
  int total_sentences = 0;
  for (uint i=0; i<patterns.size(); i++) total_sentences += patterns[i].second;
  if (total_sentences==0) return 0;
  int pattern_num = 0;
  if (funky_distribution){
    pattern_num = rand() % num_patterns;
  } else {
    int sentence_num = rand() % total_sentences;
    for (uint i=0; i<patterns.size(); i++) {
      sentence_num -= patterns[i].second;
      if (sentence_num<0) {
	pattern_num = i;
	break;
      }
    }
  }
  int sentence_num = rand() % patterns[pattern_num].second;
  return &(patterns[pattern_num].first[sentence_num]->sentence_);
}

/*
const Sentence * SentenceIndex::RandomSentenceContaining(int w){
  int total=0;
  uint n=0;
  for (int rep=0; rep<2; rep++){
    if (rep==1) {
      CHECK(total_sentences_);
      n = RandomInt() % total;
    }
    forall(run, lengths_) {
      int length = run->first;
      for (int i=0; i<length; i++) {
	Sentence s = AllVar0(length);
	s[i] = w;
	FullySpecifiedNode ** results;
	uint64 num_results;
	LookupInternal(s, &results, &num_results);
	if (rep==0) {
	  total+=num_results;
	} else {
	  if (n<num_results){
	    return &results[n]->sentence_;
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
const Sentence * SentenceIndex::Add(const Sentence & s) {  
  CHECK(s.Pattern()==0);
  uint64 fp = s.Fingerprint();
  if (fully_specified_ % fp) {
    cerr << "Sentence already exists: " << s.ToString() << endl;
    return 0;
  }
  lengths_[s.size()]++; total_sentences_++;
  FullySpecifiedNode * n = new FullySpecifiedNode;
  fully_specified_[fp] = n;
  n->sentence_ = s;
  n->pos_in_lists_ = new int[1 << s.size()];
  for (GeneralizationIterator iter(s); !iter.done(); ++iter) {
    int pattern = iter.pattern();
    if (pattern==0) continue;
    const Sentence & g = iter.generalized();
    uint64 gfp = g.Fingerprint();
    UnderspecifiedNode * un = underspecified_[gfp];
    if (un==0) {
      un = underspecified_[gfp] = new UnderspecifiedNode;
      un->first_word_counts_ = 0;
      if (g[0]==-1) un->first_word_counts_ = new map<int, int>;
    }
    n->pos_in_lists_[pattern] = un->specifications_.size();
    un->specifications_.push_back(n);
    if (un->first_word_counts_) (*un->first_word_counts_)[s[0]]++;
  }
  return &(n->sentence_);
}

void SentenceIndex::Remove(const Sentence & s) {
  CHECK(s.Pattern()==0);
  uint64 fp = s.Fingerprint();
  hash_map<uint64, FullySpecifiedNode*>::iterator 
    look = fully_specified_.find(fp);
  if (look == fully_specified_.end()) {
    cerr << "Sentence does not exist" << endl;
    return;
  }
  lengths_[s.size()]--; total_sentences_--;
  if (lengths_[s.size()]==0) lengths_.erase(s.size());
  FullySpecifiedNode * n = look->second;
  fully_specified_.erase(look);

  for (GeneralizationIterator iter(s); !iter.done(); ++iter) {
    int pattern = iter.pattern();
    if (pattern==0) continue;
    const Sentence & g = iter.generalized();
    uint64 gfp = g.Fingerprint();
    UnderspecifiedNode * un = underspecified_[gfp];
    CHECK(un!=NULL);
    int pos_in_list = n->pos_in_lists_[pattern];
    if (pos_in_list+1 < (int)un->specifications_.size()) {
      un->specifications_[pos_in_list] 
	= un->specifications_[un->specifications_.size()-1];
      un->specifications_[pos_in_list]->pos_in_lists_[pattern] = pos_in_list;
    }
    if (un->first_word_counts_) {
      SparseAdd(un->first_word_counts_, s[0], -1);
    }
    un->specifications_.pop_back();
    if (un->specifications_.size()==0) {
      underspecified_.erase(gfp);
      if (un->first_word_counts_) delete un->first_word_counts_;
      delete un;
    }
  }
  delete n;  
}
void SentenceIndex::LookupInternal(const Sentence & s, 
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
void SentenceIndex::Lookup(const Sentence & s, 
			   vector<const Sentence*> * results) {
  results->clear();
  FullySpecifiedNode ** internal_results;
  uint64 num_results;
  LookupInternal(s, &internal_results, &num_results);
  for (uint i=0; i<num_results; i++) 
    results->push_back(&internal_results[i]->sentence_);
}
const Sentence * SentenceIndex::FindSentence(const Sentence & s) {
  vector<const Sentence *> results;
  Lookup(s, &results);
  if(results.size() > 1) {
    cerr << "More than one result for " << s.ToString() << endl;
    CHECK(results.size() <= 1);
  }
  if (results.size()) return results[0];
  return 0;
}


bool SentenceIndex::FindSatisfactions(const vector<Sentence> & pattern, 
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
  vector<Sentence> simplified_pattern = pattern;
  simplified_pattern.erase(simplified_pattern.begin()+best_clause);
  if (max_work != -1 && least_work > max_work) return false;
  int total_work = least_work;
  uint64 total_num_satisfactions = 0;
  for (uint64 match=0; match<num_matches; match++){
    vector<Sentence> substituted_pattern = simplified_pattern;
    Substitution partial_sub;
    const Sentence & pre_sub = pattern[best_clause];
    const Sentence & post_sub = matches[match]->sentence_;
    if (!ComputeSubstitution(pre_sub, post_sub, &partial_sub)) continue;
    VLOG(2) << "pre=" << pre_sub.ToString() 
	    << " post=" << post_sub.ToString()
	    << " partial_sub=" << partial_sub.ToString() << endl;
    partial_sub.Substitute(&substituted_pattern);
    uint64 additional_num_satisfactions;
    uint64 added_work = 0;
    if (substitutions) {
      vector<Substitution> additional_substitutions;
      if (!FindSatisfactions(substituted_pattern, 
			     &additional_substitutions, 
			     &additional_num_satisfactions,
			     (max_work==-1)?-1:max_work-total_work,
			     &added_work)) return false;
      for (uint i=0; i<additional_substitutions.size(); i++) {
	VLOG(2) << "partial=" << partial_sub.ToString() 
		<< " additional=" << additional_substitutions[i].ToString();
	additional_substitutions[i].Add(partial_sub);
	VLOG(2) << " total=" << additional_substitutions[i].ToString() << endl;
	substitutions->push_back(additional_substitutions[i]);
      }
    } else {
      if (!FindSatisfactions(substituted_pattern, 
			     0, &additional_num_satisfactions, 
			     (max_work==-1)?-1:max_work-total_work,
			     &added_work)) return false;;
    }
    total_work += added_work;
    total_num_satisfactions += additional_num_satisfactions;
  }
  if (num_satisfactions) *num_satisfactions = total_num_satisfactions;
  if (actual_work) *actual_work = total_work;
  return true;
}
void SentenceIndex::FindWord(int w, vector<const Sentence *> *results){
  results->clear();
  set<const Sentence *> found;
  vector<const Sentence *> foo;
  forall(run, lengths_) {
    int length = run->first;
    for(int i=0; i<length; i++) {
      Sentence s;
      for (int j=0; j<length; j++) s.push_back((j==i)?w:-1);      
      Lookup(s, &foo);
      found.insert(foo.begin(), foo.end());
    }
  }
  results->insert(results->end(), found.begin(), found.end());
}

void SentenceIndex::Shell() {
  string line;
  string command;
  
  while (cin >> command) {
    if (command == "add") {
      GetLine(cin, &line);
      Sentence s;
      s.FromString(line);
      Add(s);
      cout << "Added sentence " << s.ToString() << endl;
    }
    if (command == "remove") {
      GetLine(cin, &line);
      Sentence s;
      s.FromString(line);
      Remove(s);
      cout << "Removed sentence " << s.ToString() << endl;
    }
    if (command == "lookup") {
      Sentence s;
      GetLine(cin, &line);
      s.FromString(line);
      vector<const Sentence *> results;
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
	       << run->second->specifications_[i]->sentence_.ToString()
	       << endl;
	}
      }
       
    }
  }
}


