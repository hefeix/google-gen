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

#include "element.h"
#include "link.h"
#include "changelist.h"
#include "model.h"

#undef ITEM
#define ITEM(x) #x

CLASS_ENUM_DEFINE(Element, Function);

#define FUNCTION(f, F)				\
  CLASS_ENUM_DEFINE(Static##f, ChildName);	\
  CLASS_ENUM_DEFINE(Static##f, ObjectName);
ALL_FUNCTIONS
#undef FUNCTION

void Element::SetTime(OTime new_time) {
  if (time_ == new_time) return;
  CL.ChangeValue(&time_, new_time);
  N1_StoredOrComputedTimeChanged();
  set<Element *> dep = GetAllChildren();
  forall(run, dep) {
    (*run)->N1_StoredOrComputedTimeChanged();
  }  
}

void Element::L1_Erase() {
  CHECK(!parent_); // We always unlink elements before we erase.
  L1_EraseOwnedViolations(this);
  Base::L1_Erase();
}

// The following are called externally when a child is connected to
// or disconnected from this parent node. 
void Element::N1_ChildChanged(int which_child) {
  L1_CheckSetChildViolation();
}

void Element::L1_CheckSetTimeViolation() {
  if (M.batch_mode_) {
    M.L1_AddDelayedCheck(this, &Element::L1_CheckSetTimeViolation);
    return;
  }
  OTime proper_time = ComputeTime();
  Violation * violation = FindViolation(this, Violation::TIME);
  if (proper_time != time_) {
    if (!violation) {
      New<TimeViolation>(this);
    } else {
      violation->N1_ComputedTimeChanged();
    }
  } else {
    if (violation) violation->L1_Erase();
  }
}

void Element::L1_CheckSetChildViolation() {
  if (M.batch_mode_) {
    M.L1_AddDelayedCheck(this, &Element::L1_CheckSetChildViolation);
    return;
  }
  if (NeedsChildViolation()) ChildViolation::L1_CreateIfAbsent(this);
  else ChildViolation::L1_RemoveIfPresent(this);
}

void StaticElement::UnlinkFromParent() {
  CHECK(parent_);
  // First unlink the dynamic nodes.
  forall(run, dynamic_children_->children_) {
    run->second->UnlinkFromParent();
  }
  int which_child_was_i = WhichChildAmI();
  StaticElement *parent = GetParent();
  parent_->L1_RemoveChild(this);
  if (GetFunction() != ON) StaticNoParentViolation::L1_CreateIfAbsent(this);
  N1_StoredOrComputedTimeChanged();
  parent->N1_ChildChanged(which_child_was_i);
}

void StaticElement::LinkToParent(StaticElement *new_parent, int which_child) {
  CHECK(!parent_);
  CHECK(new_parent->ChildType(which_child) == GetBaseType());
  new_parent->static_children_[which_child]->L1_AddChild(this);
  StaticNoParentViolation::L1_RemoveIfPresent(this);
  L1_RecursivelyComputeSetVariables();
  N1_StoredOrComputedTimeChanged();
  GetParent()->N1_ChildChanged(which_child);
}

void StaticElement::EraseTree() {
  for (uint i=0; i<static_children_.size(); i++) {
    StaticElement *child = GetChild(i);
    CHECK(child);
    if (child) child->EraseTree();
  }
  set<Element *> dc = dynamic_children_->GetChildren();
  forall(run, dc) (*run)->L1_Erase();
  UnlinkFromParent();
  L1_Erase();
}
void DynamicElement::EraseTree() {
  for (uint i=0; i<children_.size(); i++) {
    set<Element *> children = children_[i]->GetChildren();
    forall(run, children) {
      DynamicElement * child = dynamic_cast<DynamicElement*>(*run);
      child->EraseTree();
    }
  }
  UnlinkFromParent();
  L1_Erase();
}

void StaticElement::L1_Init() {
  Base::L1_Init();
  L1_AutomaticallyName();
  dynamic_children_ = New<MultiLink>(this);
  for (int i=0; i<NumChildren(); i++) 
    static_children_.push_back(New<SingleLink>(this));  
  objects_.resize(NumObjects());
  N1_StoredOrComputedTimeChanged();
  if (GetFunction() != ON) 
    StaticNoParentViolation::L1_CreateIfAbsent(this);
}
void StaticElement::L1_Erase() {
  for (uint i=0; i<static_children_.size(); i++) {
    static_children_[i]->L1_Erase();
  }
  L1_ClearChoices();
  dynamic_children_->L1_Erase();
  Element::L1_Erase();
}
void StaticElement::L1_ClearChoices() {
  for (uint i=0; i<choices_.size(); i++) choices_[i]->L1_Erase();
  CL.ChangeValue(&choices_, vector<Choice *>());
}
void StaticElement::L1_CreateChoices() {
  L1_ClearChoices();
  Tuple function_choice_strategy;
  function_choice_strategy.push_back(Keyword::Make("set_chooser"));
  function_choice_strategy.push_back(Keyword::Make("functions"));
  CL.PushBack(&choices_, New<Choice>(OTuple::Make(function_choice_strategy),
				     FunctionKeyword()));
}
StaticElement * StaticElement::GetParent() const { 
  if (!parent_) return NULL;
  return dynamic_cast<StaticElement *>(parent_->GetParent());
}
void DynamicElement::L1_Erase() {
  for (uint i=0; i<children_.size(); i++) {
    children_[i]->L1_Erase();
  }  
  Element::L1_Erase();
}

VariableSet StaticElement::ComputeVariables() const { 
  if (!parent_) return VariableSet();
  StaticElement * parent = GetParent();
  return Union(parent->GetVariables(), 
	       parent->GetIntroducedVariables(WhichChildAmI()));
}

void StaticElement::L1_RecursivelyComputeSetVariables() {
  VariableSet new_variables = ComputeVariables();
  if (new_variables == variables_) return;
  CL.ChangeValue(&variables_, new_variables);
  for (int i=0; i<NumChildren(); i++) {
    StaticElement *child = GetChild(i);
    if (child) child->L1_RecursivelyComputeSetVariables();
  }
}

VariableSet StaticElement::GetIntroducedVariables(int which_child) const {
  return VariableSet();
}

StaticElement * StaticElement::GetChild(int which) const { 
  return dynamic_cast<StaticElement *>(static_children_[which]->GetChild());
}
DynamicElement * StaticElement::GetDynamic(OMap binding) const {
  return dynamic_cast<DynamicElement *>(dynamic_children_->GetChild(binding));
}
Statement * StaticElement::GetStatementChild(int which) const {
  CHECK(which < NumStatementChildren());
  return dynamic_cast<Statement *>(GetChild(which + NumExpressionChildren()));
}
vector<Statement *> StaticElement::GetStatementChildren() const {
  vector<Statement *> ret;
  for (int i=0; i<NumStatementChildren(); i++) {
    ret.push_back(GetStatementChild(i));
  }
  return ret;
}
Expression * StaticElement::GetExpressionChild(int which) const{
  return dynamic_cast<Expression *>(GetChild(which));
}
Object StaticElement::GetObject(int which) const{
  return objects_[which];
}
void StaticElement::SetObject(int which, Object new_value) {
  L1_SetObject(which, new_value);
  N1_ObjectChanged(which);
}
void StaticElement::L1_SetObject(int which, Object new_value) {
  CHECK(which < (int)objects_.size());
  CL.ChangeValue(&(objects_[which]), new_value);
}
void StaticElement::L1_LinkChild(int where, StaticElement *child){
  CHECK(where < (int)static_children_.size());
  static_children_[where]->L1_AddChild(child);
}
void StaticElement::L1_UnlinkChild(int where){
  CHECK(where < (int)static_children_.size());
  static_children_[where]->L1_RemoveChild(GetChild(where));
}
set<Element *> StaticElement::GetAllChildren() const {
  set<Element *> ret;
  for (uint i=0; i<static_children_.size(); i++) 
    static_children_[i]->AddChildrenInto(&ret);
  dynamic_children_->AddChildrenInto(&ret);
  return ret;  
}
set<StaticElement *> StaticElement::GetAllStaticChildren() const {
  set<StaticElement *> ret;
  for (uint i=0; i<static_children_.size(); i++) 
    static_children_[i]->AddChildrenInto(&ret);
  return ret;
}

set<Element *> DynamicElement::GetAllChildren() const {
  set<Element *> ret;
  for (uint i=0; i<children_.size(); i++) children_[i]->AddChildrenInto(&ret);
  return ret;  
}
bool Element::NeedsChildViolation() const { 
  for (int i=0; i<NumChildren(); i++) {
    if (LinkType(i) != Link::SINGLE) continue;
    if (ChildShouldExist(i) != bool(GetChild(i))) return true;
  }
  return false;
}

Object DynamicElement::GetChildValue(int which) const { 
  DynamicExpression * child = GetSingleExpressionChild(which);
  if (!child) return Object();
  return child->GetValue();
}

bool DynamicElement::SetBinding(OMap new_binding) {
  // check to see that we will not cause a conflict.
  if (static_parent_->children_ % new_binding) {
    cerr << "couldn't change binding because of conflict" << endl;
    return false;
  }
  Link * p = parent_;
  MultiLink * sp = static_parent_;
  sp->L1_RemoveChild(this);
  if (p) p->L1_RemoveChild(this);
  CL.ChangeValue(&binding_, new_binding);
  sp->L1_AddChild(this);
  if (p) p->L1_AddChild(this);
  N1_BindingChanged();
  return true;
}

void DynamicElement::N1_BindingChanged() {
  L1_CheckSetParentAndBindingViolations();
  set<Element *> all_children = GetAllChildren();
  forall(run, all_children) {
    dynamic_cast<DynamicElement *>(*run)
      ->L1_CheckSetParentAndBindingViolations();
  }
}

void DynamicElement::L1_CheckSetParentAndBindingViolations() {
  if (M.batch_mode_) {
    M.L1_AddDelayedCheck(this, &Element::L1_CheckSetParentAndBindingViolations);
    return;
  }
  // first check that there's a parent if there needs to be.
  if (GetParent() == NULL && GetFunction() != ON) {
    DynamicNoParentViolation::L1_CreateIfAbsent(this);
    BindingVariablesViolation::L1_RemoveIfPresent(this);
    BindingOldValuesViolation::L1_RemoveIfPresent(this);
    return;
  }
  DynamicNoParentViolation::L1_RemoveIfPresent(this);

  // Check that the binding variables are correct
  if (GetStatic()->GetVariables()
      != GetDomainVariables(binding_)) {
    BindingVariablesViolation::L1_CreateIfAbsent(this);
    BindingOldValuesViolation::L1_RemoveIfPresent(this);
    return;
  }
  BindingVariablesViolation::L1_RemoveIfPresent(this);

  // Check that the values of the old variables are correct
  DynamicElement * parent = GetParent();
  if (parent && (Restrict(GetBinding(), parent->GetStatic()->GetVariables())
		 != parent->GetBinding()))
    BindingOldValuesViolation::L1_CreateIfAbsent(this);
  else BindingOldValuesViolation::L1_RemoveIfPresent(this);

  if (parent && parent->GetFunction() == LET) 
    dynamic_cast<DynamicLet *>(parent)->L1_CheckSetLetViolation();
}

DynamicElement * DynamicElement::GetSingleChild(int which) const { 
  SingleLink * l = dynamic_cast<SingleLink *>(children_[which]);
  return (DynamicElement *)(l->GetChild());
}

DynamicExpression * DynamicElement::GetSingleExpressionChild(int which) const {
  return dynamic_cast<DynamicExpression *>(GetSingleChild(which));
}
DynamicStatement * DynamicElement::GetSingleStatementChild(int which) const{
    return dynamic_cast<DynamicStatement *>(GetSingleChild(which));
}
OTime DynamicElement::ComputeTime() const { 
  if (!parent_) return CREATION;
  return parent_->ComputeChildTime(this);
}
string DynamicElement::ToString() const{
  return HTMLEscape(GetBinding().ToString()) 
    + GetStatic()->ToString();
}



void Statement::L1_Init() {
  StaticElement::L1_Init();
}

// Can't erase statements that are linked
void Statement::L1_Erase() {
  CHECK(parent_ == NULL);
  Base::L1_Erase();
}

void DynamicExpression::L1_Init(StaticElement * static_parent, 
				OMap binding){
  DynamicElement::L1_Init(static_parent, binding);
  value_ = NULL;
  N1_StoredValueChanged();
  N1_ComputedValueChanged(); 

  // there should already be a violation at the parent
  // if (GetParent()) GetParent()->L1_ChildExpressionValueChanged();
}
void DynamicExpression::SetValue(Object new_value) {
  CL.ChangeValue(&value_, new_value);
  N1_StoredValueChanged(); 
  if (GetParent()) 
    GetParent()->N1_ChildExpressionValueChanged(WhichChildAmI());  
}

void DynamicExpression::L1_Erase() {
  CHECK(!GetParent());
  DynamicElement::L1_Erase();
}
void DynamicExpression::L1_CheckSetValueViolation() {
  if (M.batch_mode_) {
    M.L1_AddDelayedCheck(this, &Element::L1_CheckSetValueViolation);
    return;
  }
  bool perfect = (value_ != NULL && value_ == ComputeValue());
  Violation * value_violation = FindViolation(this, Violation::VALUE);
  if (perfect && value_violation) {
    value_violation->L1_Erase(); 
    return;
  }
  if (!perfect && !value_violation) {
    New<ValueViolation>(this);
    return;
  }
}



void StaticOn::L1_Init(){
  Statement::L1_Init();
  New<MissingDynamicOnViolation>(this);
}
VariableSet StaticOn::GetIntroducedVariables(int which_child) const {
  return ::GetVariables(GetPattern().Data());
}
void StaticOn::L1_Erase() {
  Statement::L1_Erase();
}
// this thing has no dynamic parent. 
void DynamicOn::L1_Init(StaticElement *static_parent, OMap dummy) {
  DynamicStatement::L1_Init(static_parent, OMap::Default());
  Violation * missing_dynamic 
    = FindViolation(GetStatic(), Violation::MISSING_DYNAMIC_ON);
  CHECK(missing_dynamic);
  missing_dynamic->L1_Erase();
}
void DynamicOn::L1_Erase(){
  CHECK(GetStatic());
  CHECK(!FindViolation(GetStatic(), Violation::MISSING_DYNAMIC_ON));
  New<MissingDynamicOnViolation>(GetStaticOn());
  DynamicStatement::L1_Erase();
}
Record DynamicOn::GetRecordForDisplay() const {
  Record ret = DynamicStatement::GetRecordForDisplay();
  forall(run, GetOnMultilink()->missing_)
    ret["violations"] += run->second->ShortDescription() + "<br>\n";
  forall(run, GetOnMultilink()->extra_)
    ret["violations"] += run->second->ShortDescription() + "<br>\n";
  return ret;
}

OTime DynamicOn::ComputeTime() const {
  return GetStatic()->GetTime();
}

OTime DynamicOn::ComputeChildTime(const Link * link, 
				  const Element * child) const{
  return DataMax(time_,
		 BB.FindLastTime(Substitute(child->GetBinding().Data(), 
					    GetPattern().Data())));
}

void StaticRepeat::L1_Init() {
  Statement::L1_Init();
  L1_SetObject(REPETITION_VARIABLE, M.L1_GetNextUniqueVariable());
}

void StaticDelay::L1_Init() {
  Statement::L1_Init();
}
void StaticLet::L1_Init() {
  Statement::L1_Init();
}
bool DynamicLet::NeedsLetViolation() const {
  DynamicExpression *value_child = GetSingleExpressionChild(StaticLet::VALUE);
  if (!value_child) return false;
  DynamicStatement *child = GetSingleStatementChild(StaticLet::CHILD);
  if (!child) return false;
  if (FindViolation(child, Violation::BINDING_VARIABLES)) return false;
  const Object * binding_value = child->GetBinding().Data()
    % GetStatic()->GetObject(StaticLet::VARIABLE);
  CHECK(binding_value);
  return (value_child->GetValue() != *binding_value);
}
void DynamicLet::L1_CheckSetLetViolation() {
  if (M.batch_mode_) {
    M.L1_AddDelayedCheck(this, &Element::L1_CheckSetLetViolation);
    return;
  }
  if (NeedsLetViolation()) LetViolation::L1_CreateIfAbsent(this);
  else LetViolation::L1_RemoveIfPresent(this);
}

void StaticPost::L1_Init() {
  Statement::L1_Init();
}
Record DynamicPost::GetRecordForDisplay() const {
  Record ret = DynamicStatement::GetRecordForDisplay();
  if (posting_) ret["posting"] = posting_->ShortDescription();
  else ret["posting"] = "NULL";
  return ret;
}
void DynamicPost::SetPostingTime(OTime new_time) {
  if (new_time == time_) return;
  posting_->L1_ChangeTime(new_time.Data());
  L1_CheckSetPostViolation();
}
bool DynamicPost::NeedsPostViolation() const {
  Object computed = ComputeTuple();
  if (computed.GetType() != Object::OTUPLE) {
    // If the computed value is not a tuple, there should be no posting. 
    // It is a posting violation if one exists.
    return posting_;
  }
  if (!posting_) return true;
  if (posting_->tuple_ != computed) return true;
  if (posting_->time_ != time_.Data()) return true;
  return false;
}
void DynamicPost::L1_CheckSetPostViolation() {
  if (M.batch_mode_) {
    M.L1_AddDelayedCheck(this, &Element::L1_CheckSetPostViolation);
    return;
  }
  if (NeedsPostViolation())
    PostViolation::L1_CreateIfAbsent(this);
  else 
    PostViolation::L1_RemoveIfPresent(this);
}
void DynamicPost::AddCorrectPosting() {
  DynamicExpression * expr = GetTupleExpression();
  if (!expr) return;
  Object t = expr->value_;
  if (t.GetType() != Object::OTUPLE) return;
  if (time_ == NULL) {
    cerr << "Cannot add posting because time is null";
    return;
  }
  AddPosting(OTuple(t), time_);
}
void DynamicPost::AddPosting(OTuple t, OTime time) {
  if (posting_) {
    if (posting_->tuple_ == t) return;
    RemovePosting();
  }
  CL.ChangeValue(&posting_, new OwnedPosting(t, time, this));
  L1_CheckSetPostViolation();
}
void DynamicPost::RemovePosting() {
  CHECK(posting_);
  posting_->L1_Erase();
  CL.ChangeValue(&posting_, (OwnedPosting *)NULL);
  L1_CheckSetPostViolation();
}

void StaticIf::L1_Init() {
  Statement::L1_Init();
}

bool DynamicIf::ChildShouldExist(int which_child) const { 
  if (which_child == StaticIf::ON_TRUE
      || which_child == StaticIf::ON_FALSE) {
    DynamicExpression * expr = GetConditionExpression();
    if (!expr) return false;
    bool val = expr->value_ != FALSE;
    if (which_child == StaticIf::ON_TRUE) return val;
    return !val;
  }
  return true;
}

void Expression::L1_Init() {
  StaticElement::L1_Init();
}

string Statement::ToStringRecursive(int indent) const {
  string indentstring;
  for (int i=0; i<indent; i++) indentstring += GetSpace(true);
  string ret = indentstring;
  if (this == NULL) return ret + "null" + GetNewLine(true);
  ret += ToString();
  vector<Statement *> children = GetStatementChildren();
  if (children.size() > 1 || GetFunction() == PARALLEL) {
    ret += " {" + GetNewLine(true);
    for (uint i=0; i<children.size(); i++) 
      ret += children[i]->ToStringRecursive(indent+2);
    ret += indentstring + "}" + GetNewLine(true);
    return ret;
  }
  if (children.size() == 0) {
    return ret + " ;" + GetNewLine(true);
  }
  CHECK(children.size() == 1);
  return ret + GetNewLine(true) + children[0]->ToStringRecursive(indent+2);
}
string Statement::ToString() const {
  return GetLink(FunctionKeyword().ToString()) + ParameterListToString();
}
string Expression::ToString() const {
  return "(" + GetLink(FunctionKeyword().ToString())
    + ParameterListToString() + ")";
}

void StaticConstant::N1_ObjectChanged(int which) {
  CHECK(which == OBJECT);
  forall(run, dynamic_children_->children_) {
    dynamic_cast<DynamicExpression *>(run->second)->L1_CheckSetValueViolation();
  }
}

string StaticConstant::ToString() const {
  Object o = GetObject(OBJECT);
  if (o==NULL) return "null";
  string ret = HTMLEscape(o.ToString());
  if (o.GetType() == Object::OTUPLE) {
    Tuple t = OTuple(o).Data();
    if (t.size() > 0 && t[0].GetType() == Object::KEYWORD) {
      ret =  "(constant " + ret + ")";
    }
  }
  return GetLink(ret);
}
Object DynamicConstant::ComputeValue() const {
  return GetObject(StaticConstant::OBJECT);
}
string StaticElement::ParameterListToString() const {
  string ret;
  for (int i=0; i<NumObjects(); i++) {
    ret += " " + GetObject(i).ToString();
  }
  ret = HTMLEscape(ret);
  for (int i=0; i<NumExpressionChildren(); i++) {
    Expression * expr = GetExpressionChild(i);
    if (expr) ret += " " + expr->ToString();
    else ret += " null";
  }
  return ret;
}
Object DynamicSubstitute::ComputeValue() const {
  DynamicExpression * child 
    = GetSingleExpressionChild(StaticSubstitute::CHILD);
  if (!child) return Object();
  return DeepSubstitute(GetBinding().Data(), child->GetValue());
}
Object DynamicEqual::ComputeValue() const { 
  return Boolean::Make( (GetChildValue(StaticEqual::LHS)
			 == GetChildValue(StaticEqual::RHS) ) );
}
Object DynamicSum::ComputeValue() const {
  Object lhs = GetChildValue(StaticEqual::LHS);
  Object rhs = GetChildValue(StaticEqual::RHS);
  if (lhs.GetType() == Object::INTEGER
      && rhs.GetType() == Object::INTEGER) {
    return Integer::Make(Integer(lhs).Data() + Integer(rhs).Data());
  }
  if (lhs.GetType() == Object::REAL
      && rhs.GetType() == Object::REAL) {
    return Real::Make(Real(lhs).Data() + Real(rhs).Data());
  }
  return Object();
}

bool DynamicChoose::L1_TryMakeChoice(OTuple strategy, Object value) {
  choice_->L1_Change(strategy, value);
  N1_ComputedValueChanged();
  return (value == choice_->value_);
}
bool DynamicChoose::L1_TryMakeCorrectChoice() {
  return L1_TryMakeChoice(GetChildValue(StaticChoose::STRATEGY), value_);
}

Object DynamicChoose::ComputeValue() const { 
  if(choice_)
    return choice_->value_;
  return NULL;
}

Link * DynamicElement::FindDynamicParentLink() const {
  StaticElement * st = GetStatic();
  CHECK(st);
  Link * spl = st->parent_;
  if (!spl) return NULL;
  StaticElement * sp = dynamic_cast<StaticElement *>(spl->GetParent());
  int childnum = -1;
  for (uint i=0; i<sp->static_children_.size(); i++) {
    if (sp->static_children_[i] == spl) childnum = i;
  }
  CHECK(childnum != -1);
  DynamicElement * dp 
    = sp->GetDynamic(Restrict(binding_, sp->GetVariables()));
  if (!dp) {
    cerr << "binding=" << binding_ << endl
	 << "restricted=" << Restrict(binding_, sp->GetVariables()) << endl;
    VariableSet vars = sp->GetVariables();
    forall(run, vars) cerr << (*run) << " ";
    cerr << endl;
  }
  CHECK(dp);
  return dp->children_[childnum];  
}

bool DynamicElement::LinkToParent() {
  Link * parent = FindDynamicParentLink();
  if (!parent) return false;
  parent->L1_AddChild(this);
  CHECK(parent_ == parent);
  L1_CheckSetParentAndBindingViolations();
  GetParent()->N1_ChildChanged(WhichChildAmI());
  N1_StoredOrComputedTimeChanged();
  return true;
}

void DynamicElement::UnlinkFromParent() {
  DynamicElement * dynamic_parent = GetParent();
  int which_child = WhichChildAmI();
  CHECK(parent_);
  parent_->L1_RemoveChild(this);
  dynamic_parent->N1_ChildChanged(which_child);
  L1_CheckSetParentAndBindingViolations();
  N1_StoredOrComputedTimeChanged();
}


void DynamicElement::L1_Init(StaticElement * static_parent, 
			     OMap binding) {
  Element::L1_Init();
  static_parent_ = NULL;
  parent_ = NULL;
  binding_ = binding;
  CHECK(static_parent);
  static_parent->dynamic_children_->L1_AddChild(this);
  CHECK(static_parent_ == static_parent->dynamic_children_);
  LinkToParent();
 
  // L1_ComputeSetTime(); may belong here
  for (int i=0; i<NumChildren(); i++) {
    Link * child;
    switch (LinkType(i)) {
    case Link::MULTI:
      child = New<MultiLink>(this);
      break;
    case Link::SINGLE:
      child = New<SingleLink>(this);
      break;
    case Link::ON:
      child = New<OnMultiLink>(dynamic_cast<DynamicOn *>(this));
      break;
    default:
      CHECK(false);
      break;
    }
    children_.push_back(child);    
  }
  N1_StoredOrComputedTimeChanged();
};

DynamicElement * DynamicElement::FindParent() const {
  CHECK(static_parent_);
  CHECK(static_parent_->parent_);
  return GetStatic()->GetParent()->GetDynamic
    (Restrict(GetBinding(), GetStatic()->GetVariables()));
}

// The following is called externally when a child is connected to
// or disconnected from this parent node. 
void DynamicElement::N1_ChildChanged(int which_child) {
  Element::N1_ChildChanged(which_child);
  if (GetStatic()->IsExpressionChild(which_child))
    N1_ChildExpressionValueChanged(which_child);
}

DynamicElement * MakeDynamicElement(StaticElement *static_parent, 
				    OMap binding) {
  switch(static_parent->GetFunction()) {
#define FUNCTION(f,F) case Element::F:					\
    return MakeDynamicElement<Dynamic##f>(static_parent, binding);
    ALL_FUNCTIONS;
#undef FUNCTION
  default:
    CHECK(false);
  }
  return NULL;
}



Record Element::GetRecordForDisplay() const {
  Record ret = Base::GetRecordForDisplay();
  Element * parent = GetParent();
  if (parent) ret["parent"] = parent->ShortDescription();
  if (Violation::owned_violations_ % (void *)this) {
    forall(run, Violation::owned_violations_[(void *)this]) 
      ret["violations"] += (*run)->ShortDescription() + "<br>\n";
  }
  ret["function"] = FunctionToString(GetFunction());
  ret["description"] = ShortDescription();
  ret["time"] = time_.ToString();
  return ret;  
}

Record StaticElement::GetRecordForDisplay() const {
  Record ret = Element::GetRecordForDisplay();
  ret["program"] = "<tt>" + ToStringRecursive() + "</tt>";
  set<Element *> dynamic_children = dynamic_children_->GetChildren();
  int count =0;
  forall(run, dynamic_children) {
    if (count==10) {
      ret["dynamic"] += "... " + itoa(dynamic_children.size()) + " total";      
      break;
    };
    ret["dynamic"] += HTMLEscape((*run)->GetBinding().ToString()) + "<br>";
  }
  for (int i=0; i<NumChildren(); i++) {
    if (i>0) ret["children"] += "<br>\n";
    ret["children"] += 
      ChildToString(i) + ": " + static_children_[i]->ChildListings();
  }
  for (int i=0; i<NumObjects(); i++) {
    if (i>0) ret["children"] += "<br>\n";
    ret["objects"] += ObjectToString(i) + ": " + GetObject(i).ToString();
  }
  ret["dynamic_children"] = dynamic_children_->ChildListings(5);
  return ret;
}
Record DynamicElement::GetRecordForDisplay() const { 
  Record ret = Element::GetRecordForDisplay();
  ret["static_parent"] = GetStatic()?GetStatic()->ShortDescription():
    "NONE FOUND";
  ret["binding"] = GetBinding().ToString();
  if (!GetStatic()) return ret;
  for (uint i=0; i<children_.size(); i++) {
    if (i>0) ret["children"] += "<br>\n";
    ret["children"] += GetStatic()->ChildToString(i) 
      + ": " + children_[i]->ChildListings(5);
  }
  return ret;
}
Record DynamicExpression::GetRecordForDisplay() const { 
  Record ret = DynamicElement::GetRecordForDisplay();
  ret["value"] = value_.ToString();
  if (value_ != ComputeValue()) {
    ret["value"] += " (computed=" + ComputeValue().ToString() + ")";
  }
  return ret;
}
