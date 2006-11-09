// Copyright (C) 2006 Google Inc. and Georges Harik
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

#include "prohibition.h"
#include "model.h"c

bool Prohibition::TupleIsProhibited(const Tuple & t) const {
  CHECK(MatchesWildcardTuple(prohibited_, t));
  return (!(exceptions_ % t));
}

Prohibition * Prohibition::L1_MakeProhibition(Model *m, Tuple prohibited){
  return new Prohibition(m, prohibited);
}

Prohibition::Prohibition(Model *m, Tuple prohibited){
  model_ = m;
  prohibited_ = prohibited;
  exists_ = true;
  model_->changelist_.Make(new DeleteOnRollbackChange<Prohibition>(this));
  vector<const Tuple*> matches;
  model_->tuple_index_.Lookup(prohibited_, &matches);
  for (uint i=0; i<matches.size(); i++) {
    TrueTuple *t = model_->FindTrueTuple(*(matches[i]));
    CHECK(t);
    L1_AddViolation(t);
  }
  model_->A1_InsertIntoProhibitionIndex(prohibited_, this);
}

void Prohibition::L1_Erase(){
  while (violations_.size() > 0){
    L1_RemoveViolation(*(violations_.begin()));
  }
  model_->A1_RemoveFromProhibitionIndex(prohibited_, this);
  A1_SetExists(false);
}

// ----- Complicated L1 functions
void Prohibition::L1_AddException(Tuple exception){
  if (!MatchesWildcardTuple(prohibited_, exception)){
    cerr << "prohibited=" << 
      prohibited_.ToString() << " exception=" << exception.ToString() << endl;
    CHECK(false);
  }
  CHECK(!(exceptions_ % exception));
  A1_AddException(exception);
  TrueTuple * tt = model_->FindTrueTuple(exception);
  if (tt) {
    CHECK(violations_ % tt);
    L1_RemoveViolation(tt);
  }
}
void Prohibition::L1_RemoveException(Tuple exception){
  CHECK(exceptions_ % exception);
  A1_RemoveException(exception);
  TrueTuple *tt = model_->FindTrueTuple(exception);
  if (tt) {
    L1_AddViolation(tt);
  }
}
void Prohibition::L1_AddViolation(TrueTuple *t){
  CHECK(!(violations_ % t));
  if (violations_.size()==0) model_->A1_InsertIntoViolatedProhibitions(this);
  A1_AddViolation(t);
  t->A1_AddViolatedProhibition(this);
}
void Prohibition::L1_RemoveViolation(TrueTuple *t){
  CHECK(violations_ % t);
  A1_RemoveViolation(t);
  if (violations_.size()==0) model_->A1_RemoveFromViolatedProhibitions(this);
  t->A1_RemoveViolatedProhibition(this);
}

void Prohibition::L1_CheckAddViolation(TrueTuple *t){
  if (TupleIsProhibited(t->tuple_)) L1_AddViolation(t);
}

void Prohibition::A1_SetExists(bool val){
  CHECK(exists_ != val);
  model_->changelist_.Make(new ValueChange<bool>(&exists_, val));
}
void Prohibition::A1_AddException(Tuple exception){
  model_->changelist_.
    Make(new SetInsertChange<Tuple>(&exceptions_, exception));
}
void Prohibition::A1_RemoveException(Tuple exception){
  model_->changelist_.
    Make(new SetRemoveChange<Tuple>(&exceptions_, exception));
}
void Prohibition::A1_AddViolation(TrueTuple *t){
  model_->changelist_.
    Make(new SetInsertChange<TrueTuple *>(&violations_, t));
}
void Prohibition::A1_RemoveViolation(TrueTuple *t){
  model_->changelist_.
    Make(new SetRemoveChange<TrueTuple *>(&violations_, t));
}
