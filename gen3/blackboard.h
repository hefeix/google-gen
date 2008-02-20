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

#ifndef _BLACKBOARD_H_
#define _BLACKBOARD_H_

#include "objects.h"
#include "numbers.h"
#include "record.h"
#include "base.h"

struct SamplingInfo{
  bool   sampled_;
  int    position_;
  double fraction_;

  // Constructors
  SamplingInfo(); // unsampled
  SamplingInfo(int position, double fraction);

  // Getting new sampling infos
  SamplingInfo LimitToPosition(int position) const;
  SamplingInfo LimitToPart(const vector<int> &partition, int part) const;
  SamplingInfo RemovePosition(int position) const;

  double FractionAtPosition(int position) const {
    if (sampled_ && position_ == position) return fraction_;
    return 1.0;
  }

  static SamplingInfo StringToSamplingInfo(const string& s);
  string ToString() const; //  not the inverse of the above.
  static SamplingInfo Unsampled() { return SamplingInfo(); }
};


class Blackboard : public Base{
 public:
  void Init() { 
    Base::Init(); 
    last_time_ = CREATION;
  }
  void Erase() { Base::Erase(); }
  Base::Type GetBaseType() const { return Base::BLACKBOARD;}
  
  string GetURL() const;
  Record GetRecordForDisplay() const;

  // construct text output from all the (print page line char_num char) 
  // tuples on the blackboard
  string Print(int page = -1) const;

  uint64 GetNumTuples() const { return all_tuples_.size(); }

  static void Shell();

  // new stuff
  typedef vector<pair<OTuple, OTime> > Row;
  typedef pair<Row::const_iterator, Row::const_iterator> RowSegment;
  static RowSegment EntireRow(const Row & r) { 
    return make_pair(r.begin(), r.end());}
  static Row::const_iterator FindTimeInRow(const Row &r, OTime time_limit);
  static RowSegment TimeLimetedRow(const Row &r, OTime time_limit) {
    return make_pair(r.begin(), FindTimeInRow(r, time_limit));
  }
  static int Size(RowSegment s) { return s.second - s.first; }
  
  struct Subscription {
    virtual ~Subscription();
    virtual void Update(OTuple tuple, OTime time) = 0;
    void Init(OTuple wildcard_tuple) {
      CHECK(IsWildcardTuple(wildcard_tuple.Data()));
      subscribee_ = &(index_[wildcard_tuple]);
      subscribee_->subscriptions_.push_back(this);      
    }
    void SendCurrentAsUpdates() {
      forall(run, subscribee_->row_) Update(run->first, run->second);
    }
    RowInfo * subscribee_;
  };

  struct RowInfo {
    Row row_;
    // maps object to how often it appears as the first term in the tuple within
    // this row. 
    // Absent if the row indexes a schema where the first term is constant. 
    // Used for sampling random tuples.
    map<Object, int> first_term_counts_;
    vector<GeneralSubscription *> subscriptions_;
  };

  // Modifying the blackboard
  void Post(OTuple tuple, OTime time);  

  // Reading the blackboard

  // returns a pointer to an empty row on failure.
  const Row * GetRow(OTuple wildcard_tuple) const { 
    const RowInfo * ri = index_ % wildcard_tuple;
    if (!ri) return &empty_row_;
    return &(ri->row_);
  }
  int GetNumWildcardMatches(OTuple t) const{
    return GetRow(t)->size();
  }
  bool Contains(OTuple t) const { return GetNumWildcardMatches(t);}

  // Pass in a row if you want this to be thread-safe. 
  RowSegment GetVariableMatches(OTuple variable_tuple, 
				OTime time_limit = NULL, 
				Row * temp_row = 0,
				double sample_fraction = 1.0) const;

  OTime FindTupleTime(OTuple t) const;
  
  // Random tuples
  bool GetRandomTuple(OTuple * result);
  bool GetRandomTupleMatching(OTuple * result, const OTuple& wildcard_t);
  bool GetRandomTupleContaining(OTuple * ret, const set<Object>& terms,
				bool situation_distribution);

  // Simple way to query and get results back
  bool FindSatisfactions(OPattern pattern,
			 OTime time_limit, 
			 const SamplingInfo & sampling,
			 vector<OMap> * substitutions,
			 vector<OTime> * times,
			 uint64 * num_satisfactions,
			 int64 * max_work_now);
			 
  


 private:
  map<OTuple, RowInfo> index_;

  // non-essential stuff - used for analysis
  Row all_tuples_;
  OTime last_time_;
  set<int> all_lengths_;
  map<OTuple, map<Object, int> > first_term_counts_;
  Row empty_row_;

};

#endif
