// Copyright (C) 2007 Google Inc. and Georges Harik
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
// Author: Georges Harik and Noam Shazeer

#include "blackboard.h"
#include "tuple.h"
#include "webserver.h"
#include <fstream>

SamplingInfo::SamplingInfo() {
  sampled_ = false;
  position_ = -1;
  fraction_ = 0;
}

SamplingInfo::SamplingInfo(int position, double fraction) {
  sampled_ = true;
  position_ = position;
  fraction_ = fraction;
}

SamplingInfo SamplingInfo::LimitToPosition(int position) const{
  if (!sampled_ || (position != position_)) return SamplingInfo();
  return SamplingInfo(0, fraction_);
}
SamplingInfo SamplingInfo::LimitToPart(const vector<int> &partition, int part) 
  const {
  if (!sampled_ || partition[position_] != part) return SamplingInfo();
  int new_position = 0;
  for (int i=0; i<position_; i++) if (partition[i]==part) new_position++;
  return SamplingInfo(new_position, fraction_);
}

SamplingInfo SamplingInfo::RemovePosition(int position) const {
  if (sampled_ && (position < position_)) {
    return SamplingInfo(position_-1, fraction_);
  }
  return SamplingInfo();
}

SamplingInfo SamplingInfo::StringToSamplingInfo(const string& s) {
  istringstream istr(s);
  int position;
  double fraction;
  istr >> position >> fraction;
  return SamplingInfo(position, fraction);
}

string SamplingInfo::ToString() const {
  if (!sampled_) return "Unsampled";
  return "{pos=" + itoa(position_) + " fraction=1/" + dtoa(1/fraction_) + "}"; 
}

void Blackboard::Row::SplitAtPosition(int position) {
  if (children_[position] != NULL) return;
  children_[position] = new hash_map<Object, Row *>;
  Tuple t;
  for(int i=0; i<NumTuples(); i++) {
    GetTuple(i, &t);
    FindCreateChild(position, t[position])->AddTuple(t);
  }

}
Blackboard::Row * Blackboard::Row::FindChild(int position, Object value, 
					     bool create) {
  CHECK(children_[position]);
  Row * & row_ref = (*children_[position])[value];  
  if (row_ref == NULL) {
    if (!create) return NULL;
    Tuple new_wt = wildcard_tuple_;
    new_wt[position] = value;
    row_ref = New<Row>(new_wt);
  }
  return row_ref;
}

void Blackboard::Row::Init(const Tuple & wildcard_tuple) {
  VLOG(1) << "New row " << wildcard_tuple << endl;
  wildcard_tuple_ = wildcard_tuple;
  num_wildcards_ = NumWildcards(wildcard_tuple_);
  num_tuples_ = 0;
  children_.resize(wildcard_tuple_.size());
}
void Blackboard::Row::AddTuple(const Tuple &t) {
  for (uint i=0; i<t.size(); i++) 
    if (wildcard_tuple_[i] == WILDCARD) data_.push_back(t[i]);
  num_tuples_++;
  forall(run, subscriptions_) {
    (*run)->Update(t);
  }
  for (uint i=0; i<wildcard_tuple_.size(); i++) {
    if (children_[i]) {
      FindCreateChild(i, t[i])->AddTuple(t);
    }
  }
}



Blackboard::Row * Blackboard::GetCreateAllWildcardRow(int size) const{
  vector<Row *> * tlr = 
    (vector<Row *> *)(&top_level_rowinfo_);
  while (tlr->size() <= (uint)size) tlr->push_back(NULL);
  if ((*tlr)[size] == NULL) {
    (*tlr)[size] = New<Row>(AllWildcards(size));
  }
  return (*tlr)[size];
}

Blackboard::Row * Blackboard::GetRow(const Tuple & wildcard_tuple, 
				     bool create_empty) const{
  int size = wildcard_tuple.size();
  // Later, we might search, now let's just put in constants left to right.
  Row * r = GetCreateAllWildcardRow(size);
  for (int position=0; position<size; position++) {
    if (wildcard_tuple[position] != WILDCARD) {
      r->SplitAtPosition(position);      
      r = r->FindChild(position, wildcard_tuple[position], create_empty); 
      if (r == NULL) return r;
    }
  }
  return r;
}

void Blackboard::Post(const Tuple & tuple) {
  VLOG(1) << "Post tuple " << tuple << endl;
  if (Contains(tuple)) return;
  all_tuples_.insert(tuple);
  GetCreateAllWildcardRow(tuple.size())->AddTuple(tuple);
}


void 
Blackboard::GetVariableMatches(const Tuple & variable_tuple, 
			       Map binding_so_far,
			       vector<Map> *results,
			       double sample_fraction) const {
  const Row * wildcard_matches = GetCreateRow
    (VariablesToWildcards(variable_tuple));
  for (int i=0; i<wildcard_matches->NumTuples(); i++) {
    if (sample_fraction != 1.0) {
      double skip = int(log(RandomFraction())/log(1.0-sample_fraction));
      i += skip;
      if (i >= wildcard_matches->NumTuples()) break;
    }
    Map m = binding_so_far;
    Tuple t;
    wildcard_matches->GetTuple(i, &t);
    if (ExtendSubstitution(variable_tuple, t, &m)) {
      results->push_back(m);
    }
  }
}


/*
  bool Blackboard::GetRandomTuple(OTuple * result) {
  if (all_tuples_.size() == 0) return false;
  *result = all_tuples_[RandomUInt32() % all_tuples_.size()].first;
  return true;
}

bool Blackboard::GetRandomTupleMatching(OTuple * result, 
					const OTuple& wildcard_t) {
  const Row * r = GetRow(wildcard_t);
  if (r->size() == 0) return false;
  *result = (*r)[RandomUInt32() % r->size()].first;
  return true;
}

// We select a random tuple which contains all of the terms.
// If situation_distribution is false, this selection is uniform. 
// If situation_distribution is true, we first select a situation uniformly.  
// A situation  is determined by the length of the tuple, the positions of the
// given terms in the tuple, and the first term in the tuple (which is likely
// to be a relation name).  

bool Blackboard::GetRandomTupleContaining(OTuple * ret, 
					  const set<Object>& terms,
					  bool situation_distribution) {
  vector<Object> v_terms;
  v_terms.insert(v_terms.end(), terms.begin(), terms.end());
  CHECK(ret != NULL);
  *ret = NULL;
  
  // if (situation_distribution) then already_considered is the number of 
  // situations considered.  Otherwise, it is the number of tuples already 
  // considered.
  int already_considered = 0;
  int n = terms.size();

  // Run through the situation by lengths
  forall(run, all_lengths_) {
    uint length = *run;
    if (length < terms.size()) continue;
    
    // Run through the situation by positioning of the terms
    for (PermutationIterator run_p(n, length); !run_p.done(); ++run_p){

      // Creates a wildcard tuple for this situation
      const vector<int> & perm = run_p.current();
      Tuple t;
      for (uint i=0; i<perm.size(); i++) {
	t.push_back((perm[i]==EMPTY_SLOT)?WILDCARD:v_terms[perm[i]]);
      }
      
      const RowInfo * ri = index_ % OTuple::Make(t);
      if (!ri) continue;
      
      if (situation_distribution && (t[0] == WILDCARD)) {
	forall(run, ri->first_term_counts_) {
	  t[0] = run->first;
	  already_considered++;
	  if (rand() % already_considered == 0) {
	    GetRandomTupleMatching(ret, OTuple::Make(t));
	  }
	}
      } else {
	bool to_select = false;
	if (situation_distribution) {
	  already_considered++;
	  to_select = (rand() % already_considered == 0);
	} else {
	  already_considered += ri->row_.size();
	  to_select = (rand() % already_considered < (int) ri->row_.size());
	}
	if (to_select) {
	  GetRandomTupleMatching(ret, OTuple::Make(t));
	}
      }
    }
  }
  if (ret->size()) return true;
  return false;
}
*/

void Blackboard::Shell() {
  string command;
  OTuple tuple;
  OTime time;
  OPattern pattern;

  for (;(cin >> command) && command != "q"; cout << endl) {
    if (command == "post") {
      cin >> tuple;
      cout << "Posting " << tuple << endl;
      Post(tuple.Data());
      continue;
    }
    if (command == "postfile") {
      string fn;
      cin >> fn;
      ifstream input(fn.c_str());
      int count = 0;
      while (input >> tuple) {
	Post(tuple.Data());
	count++;
      }
      cout << "Posted " << count << " tuples" << endl;      
    }
    if (command == "query") {
      cin >> pattern;
      cout << "QUERY " << pattern << endl;
      vector<Map> substitutions;
      uint64 num_satisfactions;
      FindSatisfactions(pattern, SamplingInfo(), &substitutions,
			  &num_satisfactions, NULL);
      cout << "#sats: " << num_satisfactions << endl;
      for (uint64 i=0; i<num_satisfactions; i++) {
	cout << "   " << OMap::Make(substitutions[i]) << endl;
      }
    }
  }
}

string Blackboard::GetURL() const {
  return "blackboard";
}
Record Blackboard::GetRecordForDisplay() const {
  Record ret = Base::GetRecordForDisplay();
  ret["type"] = "BLACKBOARD";
  ret["num tuples"] = itoa(GetNumTuples());
  forall(run, all_tuples_) {
    ret["tuples"] += OTuple::Make(*run).ToString() + "<br>\n";
  }
  ret["print"] = "<pre>" + Print(-1) + "</pre>";
  return ret;
}

string Blackboard::Print(int page) const { 
  OTuple t = StringToObject("(PRINT " 
			    + ((page==-1)?"*":itoa(page)) 
			    +  " * * *)");
  const Row * r = GetCreateRow(t.Data());
  
  // page, lines
  map<int, vector<string> > output;
  if (page != -1) output[page];
  
  for (int i=0; i<r->NumTuples(); i++) {
    Tuple t;
    r->GetTuple(i, &t);
    Object page_num_obj = t[1];
    if (page_num_obj.GetType() != Object::INTEGER) continue;
    int page_num = Integer(page_num_obj).Data();
    Object line_num_obj = t[2];
    if (line_num_obj.GetType() != Object::INTEGER) continue;
    int line_num = Integer(line_num_obj).Data();
    Object char_num_obj = t[3];
    if (char_num_obj.GetType() != Object::INTEGER) continue;
    int char_num = Integer(char_num_obj).Data();
    Object char_obj = t[4];
    if (char_obj.GetType() != Object::STRING) continue;
    string s = String(char_obj).Data();
    if (s.size() != 1) continue;
    char c = s[0];

    vector<string> & page_ref = output[page_num];
    while ((int)page_ref.size() <= line_num) page_ref.push_back("");    
    string & line_ref = page_ref[line_num];
    while ((int)line_ref.size() <= char_num) line_ref += ' '; 
    line_ref[char_num] = c;
  }
  string ret;
  forall(run, output) {
    ret += "------------------ p." + itoa(run->first) + '\n';
    forall(run_lines, run->second) {
      ret += (*run_lines) + "\n";
    }
  }
  return ret;
}

// Simple way to query and get results back
bool Blackboard::FindSatisfactions(OPattern pattern,
				   const SamplingInfo & sampling,
				   vector<Map> * substitutions,
				   uint64 * num_satisfactions,
				   int64 * max_work_now) {
  if (substitutions) substitutions->clear();
  CHECK(num_satisfactions);
  *num_satisfactions = 0;

  const Pattern & p = pattern.Data();
  if (p.size() == 0) { 
    MOREWORK(1);
    if (substitutions) substitutions->push_back(Map());
    *num_satisfactions = 1;
    return true;
  }

  vector<uint64> num_matches;
  for (uint i=0; i<p.size(); i++)  {
    const Tuple& t = p[i].Data();
    num_matches.push_back(uint64(GetNumWildcardMatches
				 (VariablesToWildcards(t))
				 * sampling.FractionAtPosition(i)) );
  }
  int condition_tuple
    = min_element(num_matches.begin(), num_matches.end())-num_matches.begin();
  
  /* TODO: we might want to run this as a partition search.
     uint64 least_matches = num_matches[condition_tuple];
     int num_components = GetConnectedComponents(p, NULL);
     if ( (num_components > 1) && (least_matches > 0) ) {
  }

  Also, we might want to save time if the tuple has no repeated variables
  and we don't need the actual substitutions.
  */
  MOREWORK(num_matches[condition_tuple]);

  vector<Map> variable_matches;
  GetVariableMatches(p[condition_tuple].Data(), Map(), &variable_matches,
		     sampling.FractionAtPosition(condition_tuple));
  
  SamplingInfo sub_sampling = sampling.RemovePosition(condition_tuple);
  forall(run, variable_matches) {
    const Map & sub = *run;
    Pattern sub_pattern 
      = Substitute(sub, RemoveFromVector(p, condition_tuple));
    
    vector<Map> sub_substitutions;
    uint64 sub_num_satisfactions;
    bool success = FindSatisfactions(OPattern::Make(sub_pattern), 
				     sub_sampling,
				     substitutions?(&sub_substitutions):0, 
				     &sub_num_satisfactions,
				     max_work_now);
    if (!success) return false;
    *num_satisfactions += sub_num_satisfactions;
    if (substitutions) {
      forall(run, sub_substitutions) {
	Map combined = sub;
	CHECK(Add(&combined, *run));
	substitutions->push_back(combined);
      }
    }
  }
  if (substitutions) CHECK(substitutions->size() == *num_satisfactions);
  return true;
}


