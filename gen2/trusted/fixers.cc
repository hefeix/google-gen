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

#include "fixers.h"

// UNTRUSTED

bool StaticExecute() {
  while (1) {
    Violation * v = M.GetFirstViolation();
    if (!v) {
      cerr << "No more violations in Khazakhstan" << endl;
      break;
    }
    string tp = Violation::TypeToString(v->GetViolationType());
    cerr << "About to fix " << tp << " " << v->GetName().ToString() << endl;
    bool success = false;
    switch(v->GetViolationType()) {
    case Violation::MISSING_DYNAMIC_ON :
      success = 
	FixMissingDynamicOn(dynamic_cast<MissingDynamicOnViolation*>(v));
      break;
    case Violation::MISSING_ON_MATCH :
      success = FixMissingOnMatch(dynamic_cast<MissingOnMatchViolation*>(v));
      break;
    case Violation::EXTRA_ON_MATCH :
      success = FixExtraOnMatch(dynamic_cast<ExtraOnMatchViolation*>(v));
      break;
    case Violation::MISSING_LINK :
      success = FixMissingLink(dynamic_cast<MissingLinkViolation*>(v));
      break;
    case Violation::VALUE :
      success = FixValue(dynamic_cast<ValueViolation*>(v));
      break;
    case Violation::IF :
      success = FixIf(dynamic_cast<IfViolation*>(v));
      break;
    case Violation::TIME :
      success = FixTime(dynamic_cast<TimeViolation*>(v));
      break;
    case Violation::POST :
      success = FixPost(dynamic_cast<PostViolation*>(v));
      break;
    case Violation::LET :
      success = FixLet(dynamic_cast<LetViolation*>(v));
      break;
    case Violation::BINDING_OLD_VALUES :
      success = FixBindingOldValues
	(dynamic_cast<BindingOldValuesViolation*>(v));
      break;
    default:
      cerr << "Can't fix violation of type " << tp << endl;
      break;
    }
    if (success) {
      cerr << "Great Success" << endl;
    } else {
      cerr << "Devastating Failure" << endl;
      break;
    }
  }
  return true;
}
bool FixMissingLink(MissingLinkViolation *violation) {
  SingleLink * owner = violation->GetOwner();
  if (owner->GetParent()->IsStatic()) {
    cerr << "Couldn't fix missing static link" << endl;
    return false;
  } else {
    DynamicElement *parent = dynamic_cast<DynamicElement *>(owner->GetParent());
    int which_child = owner->WhichChildAmI();
    StaticElement *s_child = parent->GetStatic()->GetChild(which_child);
    if (!s_child) {
      cerr << "Static child doesn't exist" << endl;
      return false;
    }
    MakeDynamicElement(s_child, parent->ComputeChildBinding(which_child));
    cerr << "Fixing missing link" << endl;
    return true;
  }
}
bool FixMissingDynamicOn(MissingDynamicOnViolation *violation){
  cerr << "Fixing missing dynamic on " << endl;
  StaticOn * static_on = violation->GetOwner();
  MakeDynamicElement<DynamicOn>(static_on, OMap::Default());
  return true;
}
bool FixMissingOnMatch(MissingOnMatchViolation *violation) {
  cerr << "Fixing missing on match" << endl;
  DynamicOn * d_on = dynamic_cast<DynamicOn *>(violation->GetOwner()->parent_);
  StaticOn * s_on = d_on->GetStaticOn();
  StaticElement * s_child = s_on->GetChild(StaticOn::CHILD);
  if (!s_child) {
    cerr << "can't fix misisng on match because static on has no child" << endl;
    return false;
  }
  OMap binding = violation->data_;
  MakeDynamicElement(s_child, binding);
  return true;
}
bool FixExtraOnMatch(ExtraOnMatchViolation *violation) {
  cerr << "Fixing extra on match" << endl;
  violation->GetOwner()->GetChild(violation->data_);
  DynamicOn * d_on = dynamic_cast<DynamicOn*>(violation->GetOwner()->parent_);
  StaticOn * s_on = d_on->GetStaticOn();
  StaticElement * s_child = s_on->GetChild(StaticOn::CHILD);
  if (!s_child) {
    cerr << "can't fix misisng on match because static on has no child" << endl;
    return false;
  }
  OMap binding = violation->data_;
  DynamicElement * d_child = s_child->GetDynamic(binding);
  CHECK(d_child);
  d_child->EraseTree();
  return true;
}

bool FixValue(ValueViolation *violation){
  cerr << "Fixing value violation " << endl;
  violation->GetOwner()->ComputeSetValue();
  return true;
}
bool FixIf(IfViolation *violation){
  DynamicIf * d_if = violation->GetOwner();
  StaticIf * s_if = d_if->GetStaticIf();
  StaticElement * s_on_true = s_if->GetChild(StaticIf::ON_TRUE);
  StaticElement * s_on_false = s_if->GetChild(StaticIf::ON_FALSE);
  if (!s_on_true || !s_on_false) {
    cerr << "static if is missing children";
    return false;
  }
  DynamicExpression * expr = d_if->GetConditionExpression();
  if (!expr) {
    cerr << "wtf, this violation shouldn't exist" << endl;
    return false;
  }
  bool val = expr->value_ != Boolean::Make(false);
  DynamicElement * on_true = d_if->GetSingleChild(StaticIf::ON_TRUE);
  DynamicElement * on_false = d_if->GetSingleChild(StaticIf::ON_FALSE);
  if (val && on_false) on_false->EraseTree();
  if (!val && on_true) on_true->EraseTree();
  if (val && !on_true) MakeDynamicElement(s_on_true ,d_if->GetBinding());
  if (!val && !on_false) MakeDynamicElement(s_on_false ,d_if->GetBinding());
  cerr << "Fixed if violation" << endl;
  return true;
}
bool FixTime(TimeViolation *violation) {
  cerr << "Fixing time violation " << endl;
  violation->GetOwner()->ComputeSetTime();
  return true;

}
bool FixPost(PostViolation *violation) {
  cerr << "Fixing post violation " << endl;
  DynamicPost * dp = violation->GetOwner();
  CHECK(dp->NeedsPostViolation());
  Object computed = dp->ComputeTuple();
  if (computed.GetType() != Object::OTUPLE) {
    if (!dp->posting_) { 
      cerr << "Error - why is there a post violation" << endl;
      return true;
    }
    cerr << "Removing posting" << endl;
    dp->RemovePosting();
  } else {
    if (!dp->posting_) {
      cerr << "Adding Posting" << endl;
      dp->AddCorrectPosting();
    } else if (dp->posting_->tuple_ != computed){
      dp->RemovePosting();
      dp->AddCorrectPosting();
    } else {
      CHECK(dp->posting_->time_ != dp->time_.Data());
      cerr << "Correcting posting time from" 
	   << OTime::Make(dp->posting_->time_).ToString()
	   << " to " << dp->time_.ToString() << endl;
      dp->SetCorrectPostingTime();
      cerr << "Now posting time is " 
	   << OTime::Make(dp->posting_->time_).ToString() << endl;
    }
  }
  return true;
}

bool FixLet(LetViolation *violation) {
  cerr << "Fixing let violation" << endl;
  //sleep(1000);
  DynamicLet *dl = violation->GetOwner();
  CHECK(dl->NeedsLetViolation());
  cerr << "dl= " << dl->ShortDescription() << endl;
  //DynamicExpression *value_child 
  //  = dl->GetSingleExpressionChild(StaticLet::VALUE);
  // CHECK(value_child);
  DynamicStatement *child = dl->GetSingleStatementChild(StaticLet::CHILD);  
  CHECK(child);
  cerr << "child= " << child->ShortDescription() << endl;
  return child->ComputeSetBinding();
}
bool FixBindingOldValues(BindingOldValuesViolation *violation){
  cerr << "fixing BINDING_OLD_VALUES violation " << endl;
  DynamicElement *d = violation->GetOwner();
  // TODO: what if the binding isn't determined???
  d->ComputeSetBinding();
  return true;
}
