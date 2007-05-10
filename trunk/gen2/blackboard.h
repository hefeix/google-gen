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

#include "objects.h"
#include "numbers.h"
#include "changelist.h"

struct IndexRow;
struct Blackboard;



struct SamplingInfo{
  bool sampled_;
  uint32 position_;
  uint32 start_hash_; // inclusive
  uint32 end_hash_; // inclusive
  double GetFraction() const;
  SamplingInfo(); // creates an unsampled SamplingInfo
  SamplingInfo(int position, uint32 start_hash, uint32 end_hash);
  bool RemovePosition(uint32 position);
  SamplingInfo LimitToPosition(uint32 position) const;
  bool Matches(const Tuple& t) const;
  static SamplingInfo RandomRange(int position, int denominator, int part=-1);
  static SamplingInfo StringToSamplingInfo(const string& s);
  string ToString() const; //  not the inverse of the above.
  static SamplingInfo Unsampled() { return SamplingInfo();}
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
  virtual UpdateNeeds Needs() {
    return needs_;
  }
  IndexRow * index_row_;
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
  SearchTreeWTSubscription(Blackboard *blackboard, OTuple tuple, 
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
  void L1_AddWTSubscription(WTSubscription *sub);
  void L1_RemoveWTSubscription(WTSubscription *sub);
  // erases this row if there are no subscriptions or tuples.
  void EraseIfEmpty();
  OTuple GetWildcardTuple() const {
    return wildcard_tuple_;
  }

  // contains (first time, tupleinfo *) for each tuple on the blackboard
  // that matches.
  set<pair<Time, TupleInfo *> > tuples_;
  Blackboard * blackboard_;
  // the subscriptions that care about time
  set<WTSubscription *> time_matters_subscriptions_;
  // the other subscriptions
  set<WTSubscription *> existence_subscriptions_;
};

class Blackboard {
 public:
  friend class IndexRow;
  friend class TupleInfo;
  friend class WTSubscription;
  friend class Posting;
 
  Blackboard(Changelist *cl);

  void L1_AddPosting(Posting *p);
  void L1_RemovePosting(Posting *p);

  static void Shell();

 private:
  // returns null on failure
  IndexRow * GetIndexRow(OTuple wildcard_tuple);
  IndexRow * GetAddIndexRow(OTuple wildcard_tuple);

  TupleInfo * GetTupleInfo(OTuple tuple);


  map<OTuple, IndexRow *> index_;
  map<OTuple, TupleInfo *> tuple_info_;
  Changelist *changelist_;

};