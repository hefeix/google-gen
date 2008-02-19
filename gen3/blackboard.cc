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



struct TimeCompare{
  bool operator() (const pair<OTuple, OTime> & p1,
	
	   const pair<OTuple, OTime> & p2) { 
    return (p1.second.Data() < p2.second.Data());
  }
};
  
Blackboard::Row::const_iterator
Blackboard::FindTimeInRow(const Row &r, OTime time_limit) {
  return lower_bound(r.begin(), r.end(), 
		     make_pair(OTuple(NULL), time_limit),
		     TimeCompare());
}

Blackboard::RowSegment 
Blackboard::GetVariableMatches(OTuple variable_tuple,
			       OTime time_limit,
			       Row * temp_row,
			       double sampling_fraction) const{
  static Row static_temp_row;
  const Row * wildcard_matches = GetRow
    (OTuple::Make(VariablesToWildcards(variable_tuple.Data())));
  RowSegment wildcard_segment = EntireRow(*wildcard_matches);  
  if (Size(wildcard_segment) == 0) return wildcard_segment;
  if (time_limit != NULL) {
    wildcard_segment.second = FindTimeInRow(*wildcard_matches, time_limit);
  }
  if (sampling_fraction == 1.0 
      && !HasDuplicateVariables(variable_tuple.Data())) return wildcard_segment;

  if (!temp_row) temp_row = &static_temp_row;
  temp_row->clear();
  for (Row::const_iterator run = wildcard_segment.first; 
       run < wildcard_segment.second; run++) {
    if (sampling_fraction != 1.0) {
      double skip = int(log(RandomFraction())/log(1.0-sampling_fraction));
      run += skip;
      if (run >= wildcard_segment.second) break;
    }
    OTuple t = run->first;
    if (ComputeSubstitution(variable_tuple.Data(), t.Data(), NULL)) {
      temp_row->push_back(*run);
    }
  }
  return EntireRow(*temp_row);
}

void Blackboard::Post(OTuple tuple, OTime time) {
  CHECK(!(time.Data() < last_time_.Data()));
  last_time_ = time;
  
  if (Contains(tuple)) return;

  pair<OTuple, OTime> p = make_pair(tuple, time);
  for (GeneralizationIterator g_iter(tuple.Data()); !g_iter.done(); ++g_iter){
    const Tuple & generalized = g_iter.Current();
    RowInfo * ri = &(index_[OTuple::Make(generalized)]);
    ri->row_.push_back(p);
    if (generalized.size() > 0 && generalized[0] == WILDCARD) {
      ri->first_term_counts_[tuple.Data()[0]]++;
    }
    // we may want to make two passes so that the blackboard is consistent
    // before it sends updates.
    forall(run, ri->subscriptions_) {
      (*run)->Update(tuple, time);
    }
  }
  all_tuples_.push_back(p);
  all_lengths_.insert(tuple.Data().size());
}

OTime Blackboard::FindTupleTime(OTuple t) const{
  const Row * r = GetRow(t);
  if (r->size() == 0) return NULL;
  CHECK(r->size() == 1);
  return (*r)[0].second;
}

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


void Blackboard::Shell() {
  Blackboard * b_ptr = New<Blackboard>();
  Blackboard &b = *b_ptr;
  string command;
  OTuple tuple;
  OTime time;
  OPattern pattern;

  for (;(cin >> command) && command != "q"; cout << endl) {
    if (command == "post") {
      cin >> tuple;
      cout << "Posting " << tuple << endl;
      b.Post(tuple, CREATION);
      continue;
    }
    if (command == "postfile") {
      string fn;
      cin >> fn;
      ifstream input(fn.c_str());
      int count = 0;
      while (input >> tuple) {
	b.Post(tuple, CREATION);
	count++;
      }
      cout << "Posted " << count << " tuples" << endl;      
    }
    if (command == "query") {
      cin >> pattern >> time;
      vector<OMap> substitutions;
      vector<OTime> times;
      uint64 num_satisfactions;
      b.FindSatisfactions(pattern, time, SamplingInfo(), &substitutions, &times,
			  &num_satisfactions, NULL);
      cout << "#sats: " << num_satisfactions << endl;
      for (uint64 i=0; i<num_satisfactions; i++) {
	cout << "   " << substitutions[i] << "   " << times[i] << endl;
      }
    }
  }
  b_ptr->Erase();
}

string Blackboard::GetURL() const {
  return "blackboard";
}
Record Blackboard::GetRecordForDisplay() const {
  Record ret = Base::GetRecordForDisplay();
  ret["type"] = "BLACKBOARD";
  ret["num tuples"] = itoa(GetNumTuples());
  ret["tuples"] = "<table>"; 
  forall(run, all_tuples_) {
    ret["tuples"] += "<tr><td>" + run->first.ToString()
      + "</td><td>" + run->second.ToString() + "</tr>\n";
  }
  ret["tuples"] += "</table>";
  return ret;
}

string Blackboard::Print(int page) const { 
  OTuple t = StringToObject("(PRINT " 
			    + ((page==-1)?"*":itoa(page)) 
			    +  " * * *)");
  const Row * r = GetRow(t);
  
  // page, lines
  map<int, vector<string> > output;
  if (page != -1) output[page];
  
  forall (run, *r) {
    Tuple t = run->first.Data();
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
				   OTime time_limit,
				   const SamplingInfo & sampling,
				   vector<OMap> * substitutions,
				   vector<OTime> * times,
				   uint64 * num_satisfactions,
				   int64 * max_work_now) {
  if (substitutions) substitutions->clear();
  if (times) times->clear();
  CHECK(num_satisfactions);
  *num_satisfactions = 0;

  const Pattern & p = pattern.Data();
  if (p.size() == 0) { 
    MOREWORK(1);
    if (substitutions) substitutions->push_back(OMap::Default());
    if (times) times->push_back(CREATION);
    *num_satisfactions = 1;
    return true;
  }

  // This is now a condition node or a partition node
  vector<uint64> num_matches;
  for (uint i=0; i<p.size(); i++)  {
    const Tuple& t = p[i].Data();
    num_matches.push_back(uint64(GetNumWildcardMatches
				 (OTuple::Make(VariablesToWildcards(t)))
				 * sampling.FractionAtPosition(i)) );
  }
  int condition_tuple 
    = min_element(num_matches.begin(), num_matches.end())-num_matches.begin();
  
  /* TODO: we might want to run this as a partition search.
     uint64 least_matches = num_matches[condition_tuple];
     int num_components = GetConnectedComponents(p, NULL);
     if ( (num_components > 1) && (least_matches > 0) ) {
  }
  */
  Row temp_row;
  RowSegment s = 
    GetVariableMatches(p[condition_tuple], time_limit, &temp_row,
		       sampling.FractionAtPosition(condition_tuple));
  MOREWORK(Size(s));
  SamplingInfo sub_sampling = sampling.RemovePosition(condition_tuple);
  for (Row::const_iterator run_row = s.first; run_row!=s.second; run_row++) {
    Map sub;
    CHECK(ComputeSubstitution(p[condition_tuple].Data(), 
			      run_row->first.Data(), &sub));
    Pattern sub_pattern 
      = Substitute(sub, RemoveFromVector(p, condition_tuple));
    
    vector<OMap> sub_substitutions;
    vector<OTime> sub_times;
    uint64 sub_num_satisfactions;
    
    bool success = FindSatisfactions(OPattern::Make(sub_pattern), 
				     time_limit, sub_sampling,
				     substitutions?(&sub_substitutions):0, 
				     times?(&sub_times):0, 
				     &sub_num_satisfactions,
				     max_work_now);
    if (!success) return false;
    *num_satisfactions += sub_num_satisfactions;
    if (substitutions) {
      forall(run, sub_substitutions) {
	substitutions->push_back(OMap::Make(Union(sub, run->Data())));	
      }
    }
    if (times) {
      forall(run, sub_times) {
	times->push_back(OTime::Make(max(run_row->second.Data(), run->Data())));
      }
    }				     
  }
  if (times) CHECK(times->size() == *num_satisfactions);
  if (substitutions) CHECK(substitutions->size() == *num_satisfactions);
  return true;
}


