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
#include "changelist.h"
#include "ranktree.h"

struct IndexRow;
struct Blackboard;

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

  static SamplingInfo StringToSamplingInfo(const string& s);
  string ToString() const; //  not the inverse of the above.
  static SamplingInfo Unsampled() { return SamplingInfo(); }
};

// You change the contents of the blackboard by creating and destroying
// postings.  You own your own postings.  
// All Postings must be created with "new".
struct Posting {
  Posting(OTuple tuple, Time time, Blackboard *blackboard);
  void L1_Erase();
  void L1_ChangeTime(Time new_time);
  OTuple tuple_;
  Time time_;
  Blackboard * blackboard_;
};
/*
struct QueryUpdate {
  int satisfaction_delta_;
  vector<Map> new_satisfactions_;
  vector<Map> old_satisfactions_;
};

// You get updated by the blackboard via queries.  You create queries and own
// them.  They give you callbacks when the blackboard contents change in 
// relevant ways.
class Query {
  // todo: write this class.
  Blackboard * blackboard_;
  
  OPattern pattern_;
  Time time_; // satisfactions must happen before this time.

  bool need_substitutions_;
  void callbackfunction(QueryUpdate *) *
};
*/

/*bool operator <=(const Posting & p1, const Posting & p2) {
  if (p1.time_ < p2.time_) return true;
  if (p1.time_ > p2.time_) return false;
  return (p1.tuple_ < p2.tuple_);
  }*/

struct TupleInfo {
  TupleInfo(Posting *first_posting, Blackboard *blackboard);  
  void L1_Erase();
  OTuple tuple_;
  set<pair<Time, Posting *> > postings_; // all postings that make it true.
  Time FirstTime() const;
  void ChangeTimesInIndexRows(Time old_first_time, Time new_first_time);
  void L1_AddPosting(Posting *p);
  void L1_RemovePosting(Posting *p);
  Blackboard * blackboard_;
};

typedef uint32 UpdateNeeds;
#define UPDATE_COUNT 0x1
#define UPDATE_WHICH 0x2
#define UPDATE_TIME 0x4

struct WTUpdate {
  int64 count_delta_;
  // These are the changes.
  // You pass the OTuple, the old time and the new time.
  // If it's an insertion, the old time is NULL.
  // If it's a deletion, the new time is NULL.
  vector<pair<OTuple, pair<const Time *, const Time *> > > changes_;

  WTUpdate() {count_delta_ = 0;}
  string ToString() const {
    string ret = "WTUpdate { count_delta_=" + itoa(count_delta_) + "\n";
    for (uint i=0; i<changes_.size(); i++) {
      ret += "   " + changes_[i].first.ToString() + " : " 
	+ TimeToStringOrNothing(changes_[i].second.first) + "->" 
	+ TimeToStringOrNothing(changes_[i].second.second) + "\n";
    }
    ret += "}\n";
    return ret;
  }
};

struct WTSubscription {
  WTSubscription(Blackboard *blackboard, OTuple wildcard_tuple, 
		 UpdateNeeds needs);
  void L1_Erase();
  virtual ~WTSubscription(){}
  OTuple GetWildcardTuple() const;
  virtual void Update(const WTUpdate & update) = 0;
  virtual UpdateNeeds Needs() const {
    return needs_;
  }
  void L1_ChangeNeeds(UpdateNeeds new_needs);
  IndexRow * subscribee_;
  private:
  UpdateNeeds needs_;
};

struct LoggingWTSubscription : public WTSubscription {
  LoggingWTSubscription(Blackboard *blackboard, OTuple tuple, UpdateNeeds needs)
    :WTSubscription(blackboard, tuple, needs) {}
  string ToString() {
    return "LoggingWTSubscription(" + GetWildcardTuple().ToString() + ")";
  }
  void Update(const WTUpdate &update) {
    cout << ToString() + " " + update.ToString();
  }
};

// a WTSubscription that calls an Update(WTUpdate, WTSubscription*) 
// method on an object
template <class T> 
struct UpdateWTSubscription : public WTSubscription {
  UpdateWTSubscription(Blackboard *blackboard, OTuple tuple, 
		       UpdateNeeds needs, T *subscriber)
    :WTSubscription(blackboard, tuple, needs), subscriber_(subscriber) {}
  void Update(const WTUpdate &update) {
    subscriber_->Update(update, this);
  }
  T * subscriber_;
};




struct IndexRow {
  IndexRow(OTuple wildcard_tuple, Blackboard *blackboard);
  void L1_Erase();
  OTuple wildcard_tuple_;
  void ChangeTupleTime(TupleInfo *tuple_info, Time old_time,
		       Time new_time);
  void AddTuple(TupleInfo * tuple_info); 
  // Must be called before deleting the posting in the tupleinfo.
  // otherwise the time is wrong.
  void RemoveTuple(TupleInfo * tuple_info);
  // erases this row if there are no subscriptions or tuples.
  void EraseIfEmpty();
  OTuple GetWildcardTuple() const {
    return wildcard_tuple_;
  }
  uint32 size() { return tuples_.size(); }
  
  // contains (first time, tupleinfo *) for each tuple on the blackboard
  // that matches.
  typedef rankset<pair<Time, TupleInfo *> > TuplesType;
  TuplesType tuples_;
  Blackboard * blackboard_;

  // The external subscriptions to this wildcard tuple.
  // Each subscription is included only once, under its complete needs.
  map<UpdateNeeds, set<WTSubscription *> > subscriptions_;
};

class Blackboard {
 public:
  friend class IndexRow;
  friend class TupleInfo;
  friend class WTSubscription;
  friend class Posting;
  friend class OneTupleSearch;
  friend class ConditionSearch;

  Blackboard() {num_nonupdated_queries_ = 0;}

  void L1_AddPosting(Posting *p);
  void L1_RemovePosting(Posting *p);

  static void Shell();
  uint64 GetNumWildcardMatches(OTuple wildcard_tuple);

  
  // it is error-prone to change the blackbard when there are searches
  // that are not being updated.  Let's keep track of whether such 
  // searches exist. 
  void L1_ChangeNumNonupdatedQueries(int delta);
  int64 num_nonupdated_queries_;

 private:
  // returns null on failure
  IndexRow * GetIndexRow(OTuple wildcard_tuple);
  IndexRow * GetAddIndexRow(OTuple wildcard_tuple);

  TupleInfo * GetTupleInfo(OTuple tuple);

  map<OTuple, IndexRow *> index_;
  map<OTuple, TupleInfo *> tuple_info_;
};

#endif
