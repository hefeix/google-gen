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
// Author: Noam Shazeer and Georges Harik

#include "violation.h"
#include "model.h"
#include "changelist.h"
#include "element.h"

#undef ITEM
#define ITEM(x) #x
CLASS_ENUM_DEFINE(Violation, Type);

// Static definitions
Violation::MapType Violation::violations_;
map<Violation::Type, set<Violation *> > 
Violation::violations_by_type_;
map<OTime, set<Violation *>, DataCompare<OTime> > 
Violation::violations_by_time_;
int Violation::counts_[100];

// Const functions

// TODO this doesn't do everything we want yet
Record Violation::GetRecordForDisplay() const { 
  Record ret = Base::GetRecordForDisplay();
  ret["Violation Type"] = TypeToString(GetViolationType());
  ret["owner"] = owner_->ShortDescription();    
  if (data_ != NULL)
    ret["data"] = data_.ToString();
  ret["time"] = time_.ToString();
  return ret;
}

// Search functions
Violation *
Violation::Search(const Base * owner, Type type, Object data) {
  Violation ** v = violations_ % make_triple(owner, type, data);
  if (v) return *v;
  return NULL;
}

pair<Violation::MapIteratorType, Violation::MapIteratorType> 
Violation::Search (const Base * owner, Type type) {
  return make_pair
    (violations_.lower_bound(make_triple(owner, type, Object(NULL))),
     violations_.lower_bound(make_triple(owner, Violation::Type(type+1), 
					 Object(NULL))));
}

pair<Violation::MapIteratorType, Violation::MapIteratorType> 
Violation::Search(const Base * owner) {
  return make_pair
    (violations_.lower_bound(make_triple(owner, NO_TYPE, Object(NULL))),
     violations_.lower_bound(make_triple(owner+1, NO_TYPE, Object(NULL))));
}

set<Violation *>
Violation::GetViolations(const pair<MapIteratorType, MapIteratorType> & p) {
  set<Violation *> ret;
  for (MapIteratorType c = p.first; c != p.second; c++) {
    ret.insert(c->second);
  }
  return ret;
}

// Creation and destruction
void Violation::L1_Init
(Base * owner, Object data) {
  Base::L1_Init();
  owner_ = owner;
  CHECK(!owner_->IsErased());
  data_ = data;
  time_ = ComputeTime();
  L1_InsertIntoMaps();
  counts_[GetViolationType()]++;
}

void Violation::L1_Erase() {
  L1_RemoveFromMaps();
  Base::L1_Erase();
}

void Violation::L1_InsertIntoMaps() {
  CL.InsertIntoMap(&violations_, GetTriple(), this);
  CL.InsertIntoMapOfSets(&violations_by_type_, GetViolationType(), this);
  CL.InsertIntoMapOfSets(&violations_by_time_, time_, this);
}

void Violation::L1_RemoveFromMaps() {
  CL.RemoveFromMap(&violations_, GetTriple());
  CL.RemoveFromMapOfSets(&violations_by_type_, GetViolationType(), this);
  CL.RemoveFromMapOfSets(&violations_by_time_, time_, this);
}

void Violation::L1_ChangeTime(OTime new_time) {
  CL.RemoveFromMapOfSets(&violations_by_time_, time_, this);
  CL.ChangeValue(&time_, new_time);
  CL.InsertIntoMapOfSets(&violations_by_time_, time_, this);
}

void Violation::N1_ComputedTimeChanged() {
  OTime new_time = ComputeTime();
  if (new_time != time_) L1_ChangeTime(new_time);
}

/*
template<class Owner, Violation::Type VType>
OTime TypedViolation<Owner, VType>::ComputeTime() const {
  return dynamic_cast<Element *>(owner_)->GetTime();
  }*/

template<>
OTime ProhibitionViolation::ComputeTime() const {
  return BB.FindTupleTime(data_);
}
template<>
OTime MissingOnMatchViolation::ComputeTime() const {
  return 
    DataMax(GetTypedOwner()->GetDynamicOnParent()->GetTime(), 	    
	    Query::FindQueryTime(BB,
				 (Substitute
				  (OMap(data_).Data(), 
				   GetTypedOwner()->GetDynamicOnParent()
				   ->GetStaticOn()->GetPattern().Data()))));
}

template<>
OTime ExtraOnMatchViolation::ComputeTime() const {
  CHECK(GetTypedOwner()->children_ % OMap(data_));
  return GetTypedOwner()->children_[data_]->GetTime();
}
template<>
OTime TimeViolation::ComputeTime() const {
  return DataMin(GetTypedOwner()->ComputeTime(), GetTypedOwner()->GetTime());
}
template<>
OTime PostViolation::ComputeTime() const {
  if (GetTypedOwner()->posting_ == NULL) return GetTypedOwner()->GetTime();
  return DataMin(GetTypedOwner()->GetTime(), 
		 OTime::Make(GetTypedOwner()->posting_->time_));
}



