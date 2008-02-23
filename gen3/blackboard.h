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
#include "tuple.h"

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
  }
  void Erase() { Base::Erase(); }
  Base::Type GetBaseType() const { return Base::BLACKBOARD;}
  
  string GetURL() const;
  Record GetRecordForDisplay() const;

  // construct text output from all the (print page line char_num char) 
  // tuples on the blackboard
  string Print(int page = -1) const;

  uint64 GetNumTuples() const { return all_tuples_.size(); }

  void Shell();
  
  struct Subscription;
  struct Row {
    Tuple wildcard_tuple_;
    int num_wildcards_;
    int num_tuples_; // necessary because num_wildcards_ can be 0.
    // all of the tuples (we only store non-constant positions)
    vector<Object> data_; 
    vector<hash_map<Object, Row *> *> children_;
    vector<Subscription *> subscriptions_;

    void Init(const Tuple & wildcard_tuple);
    void AddTuple(const Tuple &t);
    void SplitAtPosition(int position);
    Row * FindCreateChild(int position, Object value);

    int NumTuples() const { return num_tuples_;}
    void GetTuple(int which, Tuple * result) const {
      *result = wildcard_tuple_;
      const Object * read_ptr = &(data_[which * num_wildcards_]);
      for (uint i=0; i<result->size(); i++) {
	if ((*result)[i] == WILDCARD) 
	  (*result)[i] = *(read_ptr++);
      }
    }
  };

  Row * GetCreateAllWildcardRow(int size) const;
  Row * GetCreateRow(const Tuple & wildcard_tuple) const;
  

  struct Subscription {
    virtual ~Subscription() {}
    virtual void Update(const Tuple & tuple) = 0;
    void Init(const Tuple & wildcard_tuple, Blackboard *blackboard) {
      VLOG(1) << "New subscription to " << wildcard_tuple << endl;
      CHECK(IsWildcardTuple(wildcard_tuple));
      subscribee_ = blackboard->GetCreateRow(wildcard_tuple);
      subscribee_->subscriptions_.push_back(this);
    }
    Row * subscribee_;
  };


  // Modifying the blackboard
  void Post(const Tuple & tuple);

  // Reading the blackboard

  int GetNumWildcardMatches(const Tuple & t) {
    return GetCreateRow(t)->NumTuples();
  }
  bool Contains(const Tuple & t) const { return all_tuples_ % t; }

  // Appends bindings to the results vector
  void GetVariableMatches(const Tuple & variable_tuple, 
			  Map binding_so_far,
			  vector<Map> *results,
			  double sample_fraction = 1.0) const;
  
  // Random tuples
  /*bool GetRandomTuple(OTuple * result);
  bool GetRandomTupleMatching(OTuple * result, const OTuple& wildcard_t);
  bool GetRandomTupleContaining(OTuple * ret, const set<Object>& terms,
				bool situation_distribution);
  */

  // Simple way to query and get results back
  bool FindSatisfactions(OPattern pattern,
			 const SamplingInfo & sampling,
			 vector<Map> * substitutions,
			 uint64 * num_satisfactions,
			 int64 * max_work_now);
			 
  


 private:
  // points from length to rowinfo for a tuple of all wildcards. 
  vector<Row *> top_level_rowinfo_;
  
  hash_set<Tuple> all_tuples_;
};

#endif
