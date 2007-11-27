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

bool StaticExecutor::FixViolation(Violation *v) {
  CHECK(!v->IsErased());
  CHECK(!v->OwnerIsErased());
  string tp = Violation::TypeToString(v->GetViolationType());
  VLOG(2) << "About to fix " << tp << " " << v->GetName().ToString() << endl;
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
  case Violation::CHILD :
    success = FixChildViolation(dynamic_cast<ChildViolation*>(v));
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
    VLOG(1) << "Can't fix violation of type " << tp << endl;
    break;
  }
  if (success) {
    VLOG(2) << "Great Success" << endl;
  } else {
    VLOG(0) << "Devastating Failure" << endl
	    << "Couldn't fix violation " << v->GetName().ToString()
	    << endl;      
  }
  return success;
}

bool StaticExecutor::Execute() {
  while (1) {
    Violation * v = M.GetFirstViolation();
    if (!v) {
      VLOG(0) << "No more violations in Khazakhstan" << endl;
      break;
    }
    M.SetBatchMode(true);
    bool success = FixViolation(v);
    M.SetBatchMode(false);
    if (!success) return false;
  } 
  return true;
}

/*bool StaticExecutor::FixAllOwnedViolations(Base *owner) {
  while (TODO
  }*/

DynamicElement * StaticExecutor::MakeInstantiateChild(DynamicElement *parent, 
						      int which_child){
  StaticElement *s_child = parent->GetStatic()->GetChild(which_child);
  if (!s_child) {
    VLOG(1) << "Static child doesn't exist" << endl;
    return NULL;
  }
  DynamicElement *ret 
    = MakeDynamicElement(s_child, 
			 parent->ComputeChildBinding(which_child));
  if (!ret) {
    VLOG(1) << "Couldn't make dynamic child " << endl;
    return NULL;
  }
  if (!Instantiate(ret)) {
    VLOG(1) << "Couldn't instantiate new dynamic element " << endl;
    return false;
  };
  return ret;
}

bool StaticExecutor::Instantiate(DynamicElement *e) {
  CHECK(e);
  e->ComputeSetTime();
  if (e->GetBaseType() == Base::DYNAMIC_STATEMENT) {
    return InstantiateStatement(dynamic_cast<DynamicStatement *>(e));
  } else {
    return InstantiateExpression(dynamic_cast<DynamicExpression *>(e));
  }
}

bool StaticExecutor::InstantiateStatement(DynamicStatement *s) {
  CHECK(s);
  for(int i=0; i<s->GetStatic()->NumChildren(); i++) {
    if (s->ChildShouldExist(i))
      if (!MakeInstantiateChild(s, i)) return false;
  }
  if (s->GetFunction() == Element::POST) {
    dynamic_cast<DynamicPost *>(s)->AddCorrectPosting();
  }

  return true;
}

bool StaticExecutor::
InstantiateExpression(DynamicExpression *e){
  CHECK(e);
  for(int i=0; i<e->GetStatic()->NumChildren(); i++) {
    if (!MakeInstantiateChild(e, i)) return false;
  }
  e->ComputeSetValue();
  return true;
}

bool StaticExecutor::FixChildViolation(ChildViolation *violation) {
  Element * e = violation->GetTypedOwner();
  if (e->IsStatic()) {
    VLOG(1) << "Couldn't fix missing static link" << endl;
    return false;
  }
  DynamicElement *de  = dynamic_cast<DynamicElement *>(e);
  for (int which_child=0; which_child<de->NumChildren(); which_child++) {
    if (de->LinkType(which_child) != Link::SINGLE) continue;    
    bool should_exist = de->ChildShouldExist(which_child);
    DynamicElement * does_exist = de->GetChild(which_child);
    if (should_exist && !does_exist) {
      StaticElement *s_child = de->GetStatic()->GetChild(which_child);
      if (!s_child) {
	VLOG(1) << "Static child doesn't exist" << endl;
	return false;
      }
      MakeDynamicElement(s_child, de->ComputeChildBinding(which_child));
      VLOG(2) << "Fixing missing link" << endl;
    }
    if (does_exist && !should_exist) {
      VLOG(2) << "Erasing extra child" << endl;
      does_exist->EraseTree();
    }
  }
  return true;
}
bool StaticExecutor::FixMissingDynamicOn(MissingDynamicOnViolation *violation){
  VLOG(2) << "Fixing missing dynamic on " << endl;
  StaticOn * static_on = violation->GetTypedOwner();
  MakeDynamicElement<DynamicOn>(static_on, OMap::Default());
  return true;
}
bool StaticExecutor::FixMissingOnMatch(MissingOnMatchViolation *violation) {
  VLOG(2) << "Fixing missing on match" << endl;
  DynamicOn * d_on 
    = dynamic_cast<DynamicOn *>(violation->GetTypedOwner()->parent_);
  StaticOn * s_on = d_on->GetStaticOn();
  StaticElement * s_child = s_on->GetChild(StaticOn::CHILD);
  if (!s_child) {
    VLOG(1) 
      << "can't fix misisng on match because static on has no child" << endl;
    return false;
  }
  OMap binding = violation->data_;
  DynamicElement *e = MakeDynamicElement(s_child, binding);
  if (!e) {
    VLOG(1) << "couldn't make dynamic element" << endl;
    return false;
  }
  return StaticExecutor::Instantiate(e);
}
bool StaticExecutor::FixExtraOnMatch(ExtraOnMatchViolation *violation) {
  VLOG(1) << "Fixing extra on match" << endl;
  violation->GetTypedOwner()->GetChild(violation->data_);
  DynamicOn * d_on 
    = dynamic_cast<DynamicOn*>(violation->GetTypedOwner()->parent_);
  StaticOn * s_on = d_on->GetStaticOn();
  StaticElement * s_child = s_on->GetChild(StaticOn::CHILD);
  if (!s_child) {
    VLOG(1) << "can't fix misisng on match because static on has no child" 
	    << endl;
    return false;
  }
  OMap binding = violation->data_;
  DynamicElement * d_child = s_child->GetDynamic(binding);
  CHECK(d_child);
  d_child->EraseTree();
  return true;
}

bool StaticExecutor::FixValue(ValueViolation *violation){
  VLOG(2) << "Fixing value violation " << endl;
  if (violation->GetTypedOwner()->ComputeValue() == NULL) {
    if (violation->GetTypedOwner()->GetFunction()==Element::CHOOSE) {
      DynamicChoose *dc = dynamic_cast<DynamicChoose*>(violation->GetOwner());
      OTuple strategy = OTuple::ConvertOrNull
	(dc->GetChildValue(StaticChoose::STRATEGY));
      VLOG(0) << "Making random choice for strategy " << strategy << endl;
      if (strategy != NULL) {
	Object choice = GC.RandomChoice(strategy);
	VLOG(0) << "Picked a choice " << choice << endl;
	dc->L1_TryMakeChoice(strategy, choice);
      }
    }
  }
  if (violation->GetTypedOwner()->ComputeValue() == NULL) {
    VLOG(0) 
      << "Can't fix value violation because computed value is NULL" << endl;
    return false;
  }
  violation->GetTypedOwner()->ComputeSetValue();
  return true;
}
bool StaticExecutor::FixIf(IfViolation *violation){
  DynamicIf * d_if = violation->GetTypedOwner();
  StaticIf * s_if = d_if->GetStaticIf();
  StaticElement * s_on_true = s_if->GetChild(StaticIf::ON_TRUE);
  StaticElement * s_on_false = s_if->GetChild(StaticIf::ON_FALSE);
  if (!s_on_true || !s_on_false) {
    VLOG(0) << "static if is missing children";
    return false;
  }
  DynamicExpression * expr = d_if->GetConditionExpression();
  if (!expr) {
    VLOG(0) << "wtf, this violation shouldn't exist" << endl;
    return false;
  }
  bool val = expr->value_ != Boolean::Make(false);
  DynamicElement * on_true = d_if->GetSingleChild(StaticIf::ON_TRUE);
  DynamicElement * on_false = d_if->GetSingleChild(StaticIf::ON_FALSE);
  if (val && on_false) on_false->EraseTree();
  if (!val && on_true) on_true->EraseTree();
  if (val && !on_true) MakeDynamicElement(s_on_true ,d_if->GetBinding());
  if (!val && !on_false) MakeDynamicElement(s_on_false ,d_if->GetBinding());
  VLOG(2) << "Fixed if violation" << endl;
  return true;
}
bool StaticExecutor::FixTime(TimeViolation *violation) {
  VLOG(2) << "Fixing time violation " << endl;
  violation->GetTypedOwner()->ComputeSetTime();
  return true;

}
bool StaticExecutor::FixPost(PostViolation *violation) {
  VLOG(2) << "Fixing post violation " << endl;
  DynamicPost * dp = violation->GetTypedOwner();
  CHECK(dp->NeedsPostViolation());
  Object computed = dp->ComputeTuple();
  if (computed.GetType() != Object::OTUPLE) {
    if (!dp->posting_) { 
      VLOG(2) << "Error - why is there a post violation" << endl;
      return true;
    }
    VLOG(2) << "Removing posting" << endl;
    dp->RemovePosting();
    return true;
  }
  if (!dp->posting_) {
    VLOG(2) << "Adding Posting" << endl;
    dp->AddCorrectPosting();
    return true;
  }
  if (dp->posting_->tuple_ != computed){
    dp->RemovePosting();
    dp->AddCorrectPosting();
    VLOG(2) << "removing wrong posting and adding correct posting" << endl;
    return true;
  }
  // So the posting time must be wrong.
  CHECK(dp->posting_->time_ != dp->time_.Data());
  VLOG(2) << "Correcting posting time from" 
       << OTime::Make(dp->posting_->time_).ToString()
       << " to " << dp->time_.ToString() << endl;
  dp->SetCorrectPostingTime();
  return true;
}

bool StaticExecutor::FixLet(LetViolation *violation) {
  VLOG(2) << "Fixing let violation" << endl;
  //sleep(1000);
  DynamicLet *dl = violation->GetTypedOwner();
  CHECK(dl->NeedsLetViolation());
  VLOG(2) << "dl= " << dl->ShortDescription() << endl;
  //DynamicExpression *value_child 
  //  = dl->GetSingleExpressionChild(StaticLet::VALUE);
  // CHECK(value_child);
  DynamicStatement *child = dl->GetSingleStatementChild(StaticLet::CHILD);  
  CHECK(child);
  VLOG(2) << "child= " << child->ShortDescription() << endl;
  return child->ComputeSetBinding();
}
bool StaticExecutor::FixBindingOldValues(BindingOldValuesViolation *violation){
  VLOG(2) << "fixing BINDING_OLD_VALUES violation " << endl;
  DynamicElement *d = violation->GetTypedOwner();
  // TODO: what if the binding isn't determined???
  d->ComputeSetBinding();
  return true;
}
