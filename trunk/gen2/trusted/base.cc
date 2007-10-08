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

#include "base.h"
#include "changelist.h"
#include "webserver.h"

#undef ITEM
#define ITEM(x) #x

CLASS_ENUM_DEFINE(Base, Type);

Namer::Namer() {
  index_.resize(Base::NumTypes());
  current_count_.resize(Base::NumTypes());
  all_time_count_.resize(Base::NumTypes());
}
Base * Namer::Lookup(Base::Type type, Object name) const {
  Base *const * look = Index(type) % name;
  if (look) return *look;
  return NULL;
}

void Base::L1_SetName(Object new_name) {
  Base ** clobbered = N.index_[GetBaseType()] % new_name;
  if (clobbered) {
    // we should only be clobbering automatic names, which are all integers.
    CHECK(new_name.GetType() == Object::INTEGER);
    CL.RemoveFromMap(&N.index_[GetBaseType()], new_name);
  }
  CL.ChangeValue(&name_, new_name);
  CL.InsertIntoMap(&N.index_[GetBaseType()], new_name, this);
  if (clobbered) (*clobbered)->L1_AutomaticallyName();
}

void Base::L1_AutomaticallyName() {
  // do we care if the increment of next_name_ reverts??
  while (N.Index(GetBaseType()) 
	 % (Object)Integer::Make(N.next_name_)) N.next_name_++;
  L1_SetName(Integer::Make(N.next_name_));
}

void Base::L1_Erase() {
  if (name_ != NULL) CL.RemoveFromMap(&N.index_[GetBaseType()], name_);
  #ifdef TRACK_ERASED
  CL.ChangeValue(&erased_, true);
  #endif
  CL.Destroying(this);
}

void Base::L1_Init() {
  CL.Creating(this);
  #ifdef TRACK_ERASED
  erased_ = false;
  #endif
  // L1_AutomaticallyName();
  N.all_time_count_[GetBaseType()]++;
  CL.ChangeValue(&N.current_count_[GetBaseType()],
		 N.current_count_[GetBaseType()]+1);
}

Record Base::GetRecordForDisplay() const { 
  Record ret;
  if (name_ != NULL) ret["name"] = name_.ToString();
  ret["type"] = TypeToString(GetBaseType());
  #ifdef TRACK_ERASED
  if (IsErased()) ret["ERASED"] = "ERASED";
  #endif
  return ret;
}
string Base::GetURL() const { 
  string ret =  "getobject?type=" + URLEscape(TypeToString(GetBaseType()));
  if (name_ != NULL) ret += "&name=" + URLEscape(name_.ToString());
  else ret += "&address=" + URLEscape(ptoa(this));
  return ret;
}
string Base::GetLink(string anchortext) const {
  return HTMLLink(GetURL(), anchortext);
}
string Base::ShortDescription() const { 
  string ret = GetLink(TypeToString(GetBaseType()));
  if (GetName() != NULL) ret += " " + GetName().ToString() ;
  ret += " " + TextIdentifier();
  return ret;
}
string Base::TextIdentifier() const { 
  return ""; // GetName().ToString();
}

