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

struct Blackboard;

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

struct WTSubscription {
  virtual ~WTSubscription(){}
  virtual OTuple WildcardTuple() = 0;
  virtual void TimeChange(OTuple t, Time old_time, Time new_time) {}
  virtual void AddTuple(OTuple t, Time new_time) {}
  virtual void RemoveTuple(OTuple t, Time old_time) {}
  virtual bool TimeMatters() { return false;}
};

struct LoggingSubscription : public WTSubscription {
  OTuple tuple_;
  bool time_matters_;
  LoggingSubscription(OTuple tuple, bool time_matters)
    :tuple_(tuple), time_matters_(time_matters) {}
  bool TimeMatters() { return time_matters_;}

  OTuple WildcardTuple() { return tuple_;}
  string ToString() {
    return "LoggingSubscription(" + tuple_.ToString() + ")";
  }
  void TimeChange(OTuple t, Time old_time, Time new_time) { 
    cout << ToString() << " TimeChange " << t << " " 
	 << old_time.ToSortableString()
	 << " -> " << new_time.ToSortableString() << endl;
  }
  void AddTuple(OTuple t, Time new_time) {
    cout << ToString() << " AddTuple "<< t << " time=" 
	 << new_time.ToSortableString()
	 << endl;
  }
  void RemoveTuple(OTuple t, Time old_time) {
    cout << ToString() << " RemoveTuple "<< t << " time=" 
	 << old_time.ToSortableString()
	 << endl;
  }
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
  void AddWTSubscription(WTSubscription *sub);
  void RemoveWTSubscription(WTSubscription *sub);
  // erases this row if there are no subscriptions or tuples.
  void EraseIfEmpty();

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

  void L1_AddWTSubscription(WTSubscription *sub);
  void L1_RemoveWTSubscription(WTSubscription *sub);

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
