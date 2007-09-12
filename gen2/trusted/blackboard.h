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

struct Query;
struct Search;


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

struct TupleInfo {
  TupleInfo(Posting *first_posting, Blackboard *blackboard);  
  void L1_Erase();
  OTuple tuple_;
  set<pair<Time, Posting *> > postings_; // all postings that make it true.
  Time FirstTime() const;
  void L1_ChangeTimesInIndexRows(Time old_first_time, Time new_first_time);
  void L1_ChangePostingTime(Posting *p, Time new_time);
  void L1_AddPosting(Posting *p);
  void L1_RemovePosting(Posting *p);
  Blackboard * blackboard_;
  string ToString();
};

typedef uint32 UpdateNeeds;
#define UPDATE_COUNT 0x1
#define UPDATE_WHICH 0x2
#define UPDATE_TIME 0x4

enum UpdateAction {
  UPDATE_CREATE,
  UPDATE_DESTROY,
  UPDATE_CHANGE_TIME,
};
inline string UpdateActionToString(UpdateAction action) {
  switch(action){
  case UPDATE_CREATE: 
    return "CREATE";
  case UPDATE_DESTROY:
    return "DESTROY";
  case UPDATE_CHANGE_TIME:
    return "CHANGE_TIME";
  };
  CHECK(false);
  return "";
}

// We templatize the update class in order to share code.
// T is an OTuple in the case of a SingleWTUpdate, 
// or an OMap in the case of a SingleQueryUpdate. 
template <class T> 
struct SingleUpdate{
  UpdateAction action_;
  T data_; 
  Time old_time_;
  Time new_time_;
  static SingleUpdate Create(const T & data, const Time & new_time) {
    SingleUpdate ret;
    ret.data_ = data;
    ret.action_ = UPDATE_CREATE;
    ret.new_time_ = new_time;
    return ret;
  }
  static SingleUpdate Destroy(const T & data, const Time & old_time) {
    SingleUpdate ret;
    ret.data_ = data;
    ret.action_ = UPDATE_DESTROY;
    ret.old_time_ = old_time;
    return ret;
  }
  static SingleUpdate ChangeTime(const T & data, 
				 const Time & old_time, Time new_time) {
    SingleUpdate ret;
    ret.data_ = data;
    ret.action_ = UPDATE_CHANGE_TIME;
    ret.old_time_ = old_time;
    ret.new_time_ = new_time;
    return ret;
  }
  string ToString() const {
    return "[ " + data_.ToString() + " " + UpdateActionToString(action_) 
      + " " + old_time_.ToString() + " -> " + new_time_.ToString() + " ]";
  }
  int GetCountDelta() const{ 
    if (action_ == UPDATE_CREATE) return 1;
    if (action_ == UPDATE_DESTROY) return -1;
    return 0;
  }
};

typedef SingleUpdate<OTuple> SingleWTUpdate;

template <class T> 
struct CombinedUpdate {
  // You might just need the change in count, so we put it here.
  // This is the number of creations - the number of deletions.
  int64 count_delta_;

  // These are the changes.
  // You pass the OTuple, the old time and the new time.
  // If it's an insertion, the old time is NULL.
  // If it's a deletion, the new time is NULL.
  vector<SingleUpdate<T> > changes_;
  
  CombinedUpdate() {count_delta_ = 0;}
  string ToString() const {
    string ret = "CombinedUpdate { count_delta_=" + itoa(count_delta_) + "\n";
    for (uint i=0; i<changes_.size(); i++) {
      ret += "   " + changes_[i].ToString() + "\n"; 
    }
    ret += "}\n";
    return ret;
  }
};

template <class UpdateType, class SubscribeeType> 
struct Subscription {
  Subscription(SubscribeeType *subscribee, 
	       UpdateNeeds needs){
    CL.Creating(this);
    needs_ = needs;
    subscribee_ = subscribee;
    CL.Make(new MapOfSetsInsertChange<UpdateNeeds, Subscription *>
	    (&(subscribee_->subscriptions_), needs_, this));
    subscribee_->L1_AddedSubscription();
  }
  void L1_Erase(){
    CL.Make(new MapOfSetsRemoveChange<UpdateNeeds, Subscription*>
	    (&(subscribee_->subscriptions_), needs_, this));
    subscribee_->L1_RemovedSubscription();
    CL.Destroying(this);
  }
  virtual ~Subscription(){}
  virtual void Update(const UpdateType & update) = 0;
  virtual UpdateNeeds Needs() const {
    return needs_;
  }
  // Sends the update that would be necessary to go from nothing to the current
  // state of the world.  Often useful when we want to do the same thing upon
  // creating the subscription as we do when the subscription sends an update
  // that something was created.  This lets us not duplicate code.
  void L1_SendCurrentAsUpdates() {
    subscribee_->L1_SendCurrentAsUpdates(this);
  }
  void L1_ChangeNeeds(UpdateNeeds new_needs){
    CL.Make(new MapOfSetsRemoveChange<UpdateNeeds, Subscription*>
	    (&(subscribee_->subscriptions_), needs_, this));
    CL.ChangeValue(&needs_, new_needs);
    CL.Make(new MapOfSetsInsertChange<UpdateNeeds, Subscription*>
	    (&(subscribee_->subscriptions_), needs_, this));
    subscribee_->L1_ChangedSubscriptionNeeds();
  }
  SubscribeeType * subscribee_;
  private:
  UpdateNeeds needs_;
};
class IndexRow;
typedef Subscription<SingleWTUpdate, IndexRow> WTSubscription;

template <class UpdateType, class SubscribeeType> 
struct LoggingSubscription : public Subscription<UpdateType, SubscribeeType> {
  typedef Subscription<UpdateType, SubscribeeType> ParentClass;
  LoggingSubscription(SubscribeeType *subscribee, UpdateNeeds needs)
    :ParentClass(subscribee, needs) {}

  string ToString() const {
    return "LoggingSubscription(" 
      + ParentClass::subscribee_->GetDescription() + ")";
  }
  void Update(const UpdateType &update) {
    cout << ToString() + " " + update.ToString();
  }
};
typedef LoggingSubscription<SingleWTUpdate, IndexRow> LoggingWTSubscription;

// a Subscription that calls an Update(UpdateType, Subscription*) 
// method on an object
template <class UpdateType, class SubscribeeType, class SubscriberType> 
struct UpdateSubscription : public Subscription<UpdateType, SubscribeeType> {
  typedef Subscription<UpdateType, SubscribeeType> ParentClass;
  UpdateSubscription(SubscribeeType *subscribee, 
		     UpdateNeeds needs, SubscriberType *subscriber)
    :ParentClass(subscribee, needs), subscriber_(subscriber) {}

  void Update(const UpdateType &update) {
    subscriber_->Update(update, this);
  }
  SubscriberType * subscriber_;
};

// a Subscription that calls an Update(UpdateType, Subscription*, DataType) 
// method on an object
template <class UpdateType, class SubscribeeType, class SubscriberType,
          class DataType> 
struct UpdateSubscriptionWithData : 
public Subscription<UpdateType, SubscribeeType> {
  typedef Subscription<UpdateType, SubscribeeType> ParentClass;
  UpdateSubscriptionWithData(SubscribeeType *subscribee, 
		     UpdateNeeds needs, 
			     SubscriberType *subscriber,
			     DataType d)
    :ParentClass(subscribee, needs), subscriber_(subscriber), data_(d) {}

  void Update(const UpdateType &update) {
    subscriber_->Update(update, this, data_);
  }
  SubscriberType * subscriber_;
  DataType data_;
};


struct IndexRow {
  IndexRow(OTuple wildcard_tuple, Blackboard *blackboard);
  void L1_Erase();
  OTuple wildcard_tuple_;
  void L1_ChangeTupleTime(TupleInfo *tuple_info, Time old_time,
		       Time new_time);
  void L1_AddTuple(TupleInfo * tuple_info); 
  // Must be called before deleting the posting in the tupleinfo.
  // otherwise the time is wrong.
  void L1_RemoveTuple(TupleInfo * tuple_info);
  // erases this row if there are no subscriptions or tuples.
  void L1_AddedSubscription() {};
  void L1_RemovedSubscription() {L1_EraseIfUnnecessary();}
  void L1_ChangedSubscriptionNeeds() {}
  void L1_EraseIfUnnecessary();
  void L1_SendUpdates(const SingleWTUpdate & update);
  void L1_SendCurrentAsUpdates(WTSubscription * sub);
  OTuple GetWildcardTuple() const {
    return wildcard_tuple_;
  }
  uint32 size() { return tuples_.size(); }
  string GetDescription() const { 
    return "IndexRow" + GetWildcardTuple().ToString();
  }
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
  friend class Subscription<SingleWTUpdate, IndexRow>;
  friend class Posting;
  friend class Query;
  friend class OneTupleSearch;
  friend class ConditionSearch;
  

  Blackboard() {
    current_wt_update_ = NULL;
  }

  void L1_AddPosting(Posting *p);
  void L1_RemovePosting(Posting *p);

  // returns null if the query fails to execute in time
  Query * L1_GetExecuteQuery(OPattern p, SamplingInfo sampling, 
			     int64 * max_work_now);


  void L1_FlushUpdates();

  static void Shell();
  void RandomTest();
  uint64 GetNumWildcardMatches(OTuple wildcard_tuple);

  
  // it is error-prone to change the blackbard when there are searches
  // that are not being updated.  Let's keep track of whether such 
  // searches exist. 
  void L1_AddNonupdatedQuery(Query * q) {
    CL.InsertIntoSet(&nonupdated_queries_, q);
  }
  void L1_DeleteNonupdatedQuery(Query * q) {
    CL.RemoveFromSet(&nonupdated_queries_, q);
  }
  void L1_DeletingQuery(Query *q);
  void PrintNonupdateQueries();
  void PrintBlackboard();
  set<Query *> nonupdated_queries_;

  LoggingWTSubscription * L1_MakeLoggingWTSubscription(OTuple wildcard_tuple, 
						    UpdateNeeds needs){
    return new LoggingWTSubscription(GetAddIndexRow(wildcard_tuple), needs);
  }
  template <class SubscriberType>
    UpdateSubscription<SingleWTUpdate, IndexRow, SubscriberType> * 
    L1_MakeUpdateWTSubscription(OTuple wildcard_tuple, 
			     UpdateNeeds needs,
			     SubscriberType *subscriber){
    return new UpdateSubscription<SingleWTUpdate, IndexRow, SubscriberType>
      (GetAddIndexRow(wildcard_tuple), needs, subscriber);
  }

  // These funtions crash if the tuples are not on the blackboard. 
  OTime FindTupleTime(OTuple t) const;
  OTime FindLastTime(const Pattern & p) const; // pattern is fully substituted

  void L1_AddSearchToFlush(Search * s);
  void Verify() const; // for debugging

 private:
  // returns null on failure
  IndexRow * GetIndexRow(OTuple wildcard_tuple);
  IndexRow * GetAddIndexRow(OTuple wildcard_tuple);
  TupleInfo * GetTupleInfo(OTuple tuple) const;

  map<OTuple, IndexRow *> index_;
  map<OTuple, TupleInfo *> tuple_info_;
  set<pair<int, Search*> > searches_to_flush_;

  SingleWTUpdate * current_wt_update_;

  // all the queries
  map<OPattern, Query *> queries_;


 public:
  set<Query *> flushed_query_update_;// for debugging purposes.
  set<Query *> flushed_wt_update_;// for debugging purposes.
};

extern Blackboard BB;

#endif
