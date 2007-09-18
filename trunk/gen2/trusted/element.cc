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


void Element::L1_Erase() {
  if (parent_) parent_->L1_RemoveChild(this);
  L1_EraseOwnedViolations(this);
  Named::L1_Erase();
}

void Element::N1_TimeMayHaveChanged() {
  OTime proper_time = ComputeTime();
  Violation * violation = FindViolation(this, Violation::TIME);
  if (proper_time != time_) {
    if (!violation) {
      New<TimeViolation>(this);
    } else {
      violation->N1_TimeMayHaveChanged();
    }
  } else {
    if (violation) violation->L1_Erase();
  }
}

void StaticElement::UnlinkFromParent() {
  CHECK(parent_);
  // First unlink the dynamic nodes.
  forall(run, dynamic_children_->children_) {
    run->second->UnlinkFromParent();
  }
  parent_->L1_RemoveChild(this);
  if (GetFunction() != ON) 
    StaticNoParentViolation::L1_CreateIfAbsent(this);
}
void StaticElement::LinkToParent(StaticElement *new_parent, int which_child) {
  CHECK(!parent_);
  CHECK(new_parent->ChildType(which_child) == GetNamedType());
  new_parent->static_children_[which_child]->L1_AddChild(this);
  StaticNoParentViolation::L1_RemoveIfPresent(this);
}
void StaticElement::EraseTree() {
  for (uint i=0; i<static_children_.size(); i++) {
    StaticElement *child = GetChild(i);
    CHECK(child);
    if (child) child->EraseTree();
  }
  set<Element *> dc = dynamic_children_->GetChildren();
  forall(run, dc) (*run)->L1_Erase();
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
  L1_Erase();
}

void StaticElement::L1_Init() {
  Named::L1_Init();
  dynamic_children_ = New<MultiLink>(this);
  for (int i=0; i<NumChildren(); i++) 
    static_children_.push_back(New<SingleLink>(this));  
  objects_.resize(NumObjects());
  //L1_MarkStaticNodeChanged();
  if (GetFunction() != ON) 
    StaticNoParentViolation::L1_CreateIfAbsent(this);
}
void StaticElement::L1_Erase() {
  for (uint i=0; i<static_children_.size(); i++) {
    static_children_[i]->L1_Erase();
  }
  dynamic_children_->L1_Erase();
  Element::L1_Erase();
}
StaticElement * StaticElement::GetParent() const { 
  if (!parent_) return NULL;
  return dynamic_cast<StaticElement *>(parent_->GetParent());
}

set<Variable> StaticElement::GetVariables() const { 
  if (!parent_) return set<Variable>();
  StaticElement * parent = GetParent();
  return Union(parent->GetVariables(), parent->GetIntroducedVariables());
}

set<Variable> StaticElement::GetIntroducedVariables() const {
  return set<Variable>();
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
  static_parent_->L1_RemoveChild(this);
  if (parent_) parent_->L1_RemoveChild(this);
  CL.ChangeValue(&binding_, new_binding);
  static_parent_->L1_AddChild(this);
  if (parent_) parent_->L1_AddChild(this);
  L1_CheckSetParentAndBindingViolations();
  return true;
}
void DynamicElement::L1_CheckSetParentAndBindingViolations() {
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
  return dynamic_cast<DynamicElement *>(l->GetChild());
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
string DynamicElement::ToString(bool html) const{
  return GetStatic()->ToString(html) + GetNewLine(html)
    + MaybeHTMLEscape(GetBinding().ToString(), html);
}



void Statement::L1_Init() {
  StaticElement::L1_Init();
}

// Can't erase statements that are linked
void Statement::L1_Erase() {
  CHECK(parent_ == NULL);
  Named::L1_Erase();
}

void DynamicExpression::L1_Init(StaticElement * static_parent, 
				OMap binding){
  DynamicElement::L1_Init(static_parent, binding);
  value_ = NULL;
  L1_CheckSetValueViolation();
  // there should already be a violation at the parent
  // if (GetParent()) GetParent()->L1_ChildExpressionChanged();
}
void DynamicExpression::SetValue(Object new_value) {
  CL.ChangeValue(&value_, new_value);
  L1_CheckSetValueViolation();
  if (GetParent()) GetParent()->N1_ChildExpressionChanged(WhichChild());
}

void DynamicExpression::L1_Erase() {
  DynamicElement * parent = GetParent();
  DynamicElement::L1_Erase();
  parent->N1_ChildExpressionChanged(WhichChild());
}
void DynamicExpression::L1_CheckSetValueViolation() {
  bool perfect = (value_ != NULL) && (value_ == ComputeValue());
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
set<Variable> StaticOn::GetIntroducedVariables() const {
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

OTime DynamicOn::ComputeChildTime(const Link * link, 
				  const Element * child) const{
  return DataMax(time_,
		 BB.FindLastTime(Substitute(child->GetBinding().Data(), 
					    GetPattern().Data())));
}

void StaticRepeat::L1_Init() {
  L1_SetObject(REPETITION_VARIABLE, M.L1_GetNextUniqueVariable());
}

void StaticDelay::L1_Init() {
  Statement::L1_Init();
}
void StaticLet::L1_Init() {
  Statement::L1_Init();
}
bool DynamicLet::HasLetViolation() const {
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
  if (HasLetViolation()) LetViolation::L1_CreateIfAbsent(this);
  else LetViolation::L1_RemoveIfPresent(this);
}

void StaticOutput::L1_Init() {
  Statement::L1_Init();
}
bool DynamicOutput::IsPerfect() const {
  DynamicExpression * expr = GetTupleExpression();
  if (!expr) return false;
  if (!posting_) return false;
  if (posting_->tuple_ != expr->value_) return false;
  if (posting_->time_ != time_.Data()) return false;
  return true;
}
void DynamicOutput::L1_CheckSetPostingViolation() {
  if (IsPerfect()) 
    PostingViolation::L1_RemoveIfPresent(this);
  else 
    PostingViolation::L1_CreateIfAbsent(this);
}
void DynamicOutput::AddCorrectPosting() {
  DynamicExpression * expr = GetTupleExpression();
  if (!expr) return;
  Object t = expr->value_;
  if (t.GetType() != Object::OTUPLE) return;
  AddPosting(OTuple(t));
}
void DynamicOutput::AddPosting(OTuple t) {
  if (posting_) {
    if (posting_->tuple_ == t) return;
    RemovePosting();
  }
  CL.ChangeValue(&posting_, new OwnedPosting(t, CREATION, this));
}
void DynamicOutput::RemovePosting() {
  CHECK(posting_);
  posting_->L1_Erase();
  CL.ChangeValue(&posting_, (OwnedPosting *)NULL);
}

void StaticIf::L1_Init() {
  Statement::L1_Init();
}

bool DynamicIf::IsPerfect() const { 
  DynamicExpression * expr = GetConditionExpression();
  if (!expr) return false;
  if (expr->value_.GetType() != Object::BOOLEAN) return false;
  bool val = Boolean(expr->value_).Data();
  if (val ^ bool(GetSingleChild(StaticIf::ON_TRUE)) ) return false;
  if ((!val) ^ bool(GetSingleChild(StaticIf::ON_FALSE)) ) return false;
  return true;
}
void DynamicIf::L1_CheckSetIfViolation() {
  if (IsPerfect()) 
    IfViolation::L1_RemoveIfPresent(this);
  else 
    IfViolation::L1_CreateIfAbsent(this);
}

void Expression::L1_Init() {
  StaticElement::L1_Init();
}

string Statement::ToString(int indent, bool html) const {
  string indentstring;
  for (int i=0; i<indent; i++) indentstring += GetSpace(html);
  string ret = indentstring;
  if (this == NULL) return ret + "null" + GetNewLine(html);
  ret += ToStringSingle(html);
  vector<Statement *> children = GetStatementChildren();
  if (children.size() > 1 || GetFunction() == PARALLEL) {
    ret += " {" + GetNewLine(html);
    for (uint i=0; i<children.size(); i++) 
      ret += children[i]->ToString(indent+2, html);
    ret += indentstring + "}" + GetNewLine(html);
    return ret;
  }
  if (children.size() == 0) {
    return ret + " ;" + GetNewLine(html);
  }
  CHECK(children.size() == 1);
  return ret + GetNewLine(html) + children[0]->ToString(indent+2, html);
}
string Statement::ToStringSingle(bool html) const {
  string ret;
  string tkw = FunctionKeyword().ToString();
  ret += html?GetLink(tkw):tkw;
  ret += ParameterListToString(html);
  return ret;
}
string Expression::ToString(bool html) const {
  string ret = "(";
  string tkw = FunctionKeyword().ToString();
  ret += html?GetLink(tkw):tkw;
  ret += ParameterListToString(html);
  ret += ")";
  return ret;
}

void StaticConstant::N1_ObjectChanged(int which) {
  CHECK(which == OBJECT);
  forall(run, dynamic_children_->children_) {
    dynamic_cast<DynamicExpression *>(run->second)->L1_CheckSetValueViolation();
  }
}

string StaticConstant::ToString(bool html) const {
  Object o = GetObject(OBJECT);
  if (o==NULL) return "null";
  string ret = o.ToString();
  if (html) ret = HTMLEscape(ret);
  if (o.GetType() == Object::OTUPLE) {
    Tuple t = OTuple(o).Data();
    if (t.size() > 0 && t[0].GetType() == Object::KEYWORD) {
      ret =  "(constant " + ret + ")";
    }
  }
  if (html) return GetLink(ret);
  return ret;
}
Object DynamicConstant::ComputeValue() const {
  return GetObject(StaticConstant::OBJECT);
}
string StaticElement::ParameterListToString(bool html) const {
  string ret;
  for (int i=0; i<NumObjects(); i++) {
    ret += " " + GetObject(i).ToString();
  }
  if (html) ret = HTMLEscape(ret);
  for (int i=0; i<NumExpressionChildren(); i++) {
    Expression * expr = GetExpressionChild(i);
    if (expr) ret += " " + expr->ToString(html);
    else ret += " null";
  }
  return ret;
}
Object DynamicSubstitute::ComputeValue() const { 
  return DeepSubstitute
    (GetBinding().Data(),  
     GetSingleExpressionChild(StaticSubstitute::CHILD)->GetValue());
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
Object DynamicFlakeChoice::ComputeValue() const { 
  return choice_;
}
void DynamicFlakeChoice::L1_AddToChooser(int count_delta) {
  if (chooser_name_ != NULL && choice_ != NULL) {
    M.L1_AddChoiceToFlakeChooser(chooser_name_, choice_, count_delta);
  }
}
void DynamicFlakeChoice::L1_ChangeChooser(Object new_chooser_name){
  L1_AddToChooser(-1);
  CL.ChangeValue(&chooser_name_, new_chooser_name);
  L1_AddToChooser(1);
}
void DynamicFlakeChoice::L1_ChangeChoice(Flake new_choice){
  L1_AddToChooser(-1);
  CL.ChangeValue(&choice_, new_choice);
  L1_AddToChooser(1);
  L1_CheckSetValueViolation();
}

Link * DynamicElement::FindDynamicParentLink() {
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
  CHECK(dp);
  return dp->children_[childnum];  
}

bool DynamicElement::LinkToParent() {
  Link * parent = FindDynamicParentLink();
  if (!parent) return false;
  parent->L1_AddChild(this);
  CHECK(parent_ == parent);
  L1_CheckSetParentAndBindingViolations();
  GetParent()->N1_ChildConnected(parent_, this);
  return true;
}
void DynamicElement::UnlinkFromParent() {
  DynamicElement * dynamic_parent = GetParent();
  Link * parent_link = parent_;
  CHECK(parent_);
  parent_->L1_RemoveChild(this);
  dynamic_parent->N1_ChildDisconnected(parent_link, this);
  L1_CheckSetParentAndBindingViolations();
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
};

DynamicElement * DynamicElement::FindParent() const {
  CHECK(static_parent_);
  CHECK(static_parent_->parent_);
  return GetStatic()->GetParent()->GetDynamic
    (Restrict(GetBinding(), GetStatic()->GetVariables()));
}

// The following are called externally when a child is connected to
// or disconnected from this parent node. 
void DynamicElement::N1_ChildConnected(Link *child_link, 
				       DynamicElement *child) {
  if (child->GetNamedType() == DYNAMIC_EXPRESSION) 
    N1_ChildExpressionChanged(child_link->WhichChild());  
}
void DynamicElement::N1_ChildDisconnected(Link *child_link, 
					  DynamicElement *child) {
  if (child->GetNamedType() == DYNAMIC_EXPRESSION) 
    N1_ChildExpressionChanged(child_link->WhichChild());  
}

DynamicElement * MakeDynamicElement(StaticElement *static_parent, 
				    OMap binding) {
  switch(static_parent->GetFunction()) {
  case Element::PASS: 
    return MakeDynamicElement<DynamicPass>(static_parent, binding);
  case Element::ON: 
    return MakeDynamicElement<DynamicOn>(static_parent, binding);
  case Element::REPEAT: 
    return MakeDynamicElement<DynamicRepeat>(static_parent, binding);
  case Element::DELAY:
    return MakeDynamicElement<DynamicDelay>(static_parent, binding);
  case Element::LET:
    return MakeDynamicElement<DynamicLet>(static_parent, binding);
  case Element::OUTPUT: 
    return MakeDynamicElement<DynamicOutput>(static_parent, binding);
  case Element::IF: 
    return MakeDynamicElement<DynamicIf>(static_parent, binding);
  case Element::PARALLEL: 
    return MakeDynamicElement<DynamicParallel>(static_parent, binding);
  case Element::SUBSTITUTE: 
    return MakeDynamicElement<DynamicSubstitute>(static_parent, binding);
  case Element::FLAKE_CHOICE: 
    return MakeDynamicElement<DynamicFlakeChoice>(static_parent, binding);
  case Element::CONSTANT: 
    return MakeDynamicElement<DynamicConstant>(static_parent, binding);
  case Element::EQUAL: 
    return MakeDynamicElement<DynamicEqual>(static_parent, binding);
  case Element::SUM: 
    return MakeDynamicElement<DynamicSum>(static_parent, binding);

  default:
    CHECK(false);
  }
  return NULL;
}



Record Element::GetRecordForDisplay() const {
  Record ret = Named::GetRecordForDisplay();
  Element * parent = GetParent();
  if (parent) ret["parent"] = parent->ShortDescription();
  if (Violation::owned_violations_ % (void *)this) {
    forall(run, Violation::owned_violations_[(void *)this]) 
      ret["violations"] += (*run)->ShortDescription() + "<br>\n";
  }
  ret["function"] = FunctionToString(GetFunction());
  return ret;  
}

Record StaticElement::GetRecordForDisplay() const {
  Record ret = Element::GetRecordForDisplay();
  ret["program"] = "<tt>" + ToString(true) + "</tt>";
  set<Element *> dynamic_children = dynamic_children_->GetChildren();
  int count =0;
  forall(run, dynamic_children) {
    if (count==10) {
      ret["dynamic"] += "... " + itoa(dynamic_children.size()) + " total";
      break;
    };
    ret["dynamic"] += HTMLEscape((*run)->GetBinding().ToString()) + "<br>";
  }
  return ret;
}
Record DynamicElement::GetRecordForDisplay() const { 
  Record ret = Element::GetRecordForDisplay();
  ret["static_parent"] = GetStatic()->ShortDescription();
  return ret;
}
