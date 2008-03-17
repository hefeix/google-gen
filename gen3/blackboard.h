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
#include "ranktree.h"

typedef weightedset<Object, int64> Distribution;
int64 DistributionDiscretize(Object o);

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

  uint64 GetNumTuples() const { return tuples_.size(); }

  void Shell();
  
  struct Row;
  struct TupleInfo {
    TupleInfo(const Tuple & t) : tuple_(t) {}
    Tuple tuple_;
    vector<pair<Row *, int> > positions_;
  };

  struct Subscription;
  struct Row {
    Tuple wildcard_tuple_;
    int num_wildcards_;
    vector<TupleInfo *> data_; 
    vector<hash_map<Object, Row *> *> children_;
    vector<Subscription *> subscriptions_;

    void Init(const Tuple & wildcard_tuple);
    // Add the tuple into this row and all children of this row, 
    // and modify the tuple_info
    void AddTuple(TupleInfo *tuple_info);
    // Not parallel to AddTuple - just remove the tuple from this row.
    void RemoveTuple(int position);
    void SplitAtPosition(int position);
    Row * FindChild(int position, Object value, bool create = false);
    Row * FindCreateChild(int position, Object value) {
      return FindChild(position, value, true);
    }

    int NumTuples() const { return data_.size();}
    const Tuple & GetTuple(int which) const { return data_[which]->tuple_; }

    // copy the which'th entry onto the stack, starting at position start_pos
    void CopyBinding(int tuple_num, Tuple *stack, int start_pos) {
      CHECK(tuple_num >= 0 && tuple_num < NumTuples());
      if ((int)stack->size() < start_pos + num_wildcards_) {
	stack->resize(start_pos+num_wildcards_);
      }
      int pos = start_pos;
      const Tuple & t = GetTuple(tuple_num);
      for (uint i=0; i<wildcard_tuple_.size(); i++) {
	if (wildcard_tuple_[i] == WILDCARD) {
	  (*stack)[pos] = t[i];
	  pos++;
	}
      }
    }
  };

  Row * GetCreateAllWildcardRow(int size) const;
  // may split rows internally to arrive at the row, but returns null if
  // the row does not need to exist in the blackboard, unless 
  // create_empty is set to true, in which case it forces the row to exist.
  Row * GetRow (const Tuple & wildcard_tuple, bool create_empty = false) const;
  Row * GetCreateRow(const Tuple & wildcard_tuple) const {
    return GetRow(wildcard_tuple, true);
  }
  
  struct Subscription {
    virtual ~Subscription() {}
    virtual void Update(Row *row, int item_num) = 0;
    void Init(Row *row) {
      CHECK(row);
      subscribee_ = row;
      subscribee_->subscriptions_.push_back(this);
    }
    Row * subscribee_;
  };


  // Modifying the blackboard
  void Post(const Tuple & tuple);
  void Remove(const Tuple &tuple);

  // Reading the blackboard

  int GetNumWildcardMatches(const Tuple & t) {
    return GetCreateRow(t)->NumTuples();
  }
  bool Contains(const Tuple & t) const { return tuples_ % t; }

  // dealing with distributions
  Distribution * GetDistribution(Object identifier) 
    { return distributions_ % identifier; }
  Distribution * GetCreateDistribution(Object identifier);

  // Appends bindings to the results vector
  /*void GetVariableMatches(const Tuple & variable_tuple, 
			  Map binding_so_far,
			  vector<Map> *results,
			  double sample_fraction = 1.0) const;*/
  
  // Random tuples
  /*bool GetRandomTuple(OTuple * result);
  bool GetRandomTupleMatching(OTuple * result, const OTuple& wildcard_t);
  bool GetRandomTupleContaining(OTuple * ret, const set<Object>& terms,
				bool situation_distribution);
  */

  // Simple way to query and get results back
  /*  bool FindSatisfactions(OPattern pattern,
     const SamplingInfo & sampling,
			 vector<Tuple> * stacks,
			 uint64 * num_satisfactions,
			 int64 * max_work_now);*/
			 
  


 private:
  // points from length to rowinfo for a tuple of all wildcards. 
  vector<Row *> top_level_rowinfo_;
  hash_map<Tuple, TupleInfo *> tuples_;

  hash_map<Object, Distribution> distributions_;
};

#endif
