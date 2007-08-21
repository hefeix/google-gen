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

void Element::L1_TimeMayHaveChanged() {
  OTime proper_time = ComputeTime();
  Violation * violation = FindViolation(this, Violation::TIME);
  if (proper_time != time_) {
    if (!violation) {
      New<TimeViolation>(this, min(time_, proper_time));
    } else {
      violation->L1_ChangeTime(min(time_, proper_time));
    }
  } else {
    if (violation) violation->L1_Erase();
  }
}


void StaticElement::Init() {
  Named::Init();
  dynamic_children_ = New<MultiLink>(this);
  for (int i=0; i<NumChildren(); i++) 
    static_children_.push_back(New<SingleLink>(this));  
  objects_.resize(NumObjects());
}
void StaticElement::L1_Erase() {
  for (uint i=0; i<static_children_.size(); i++) 
    static_children_[i]->L1_Erase();
  dynamic_children_->L1_Erase();
  EraseOwnedViolations(this);
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

DynamicElement * DynamicElement::GetSingleChild(int which) const { 
  SingleLink * l = dynamic_cast<SingleLink *>(children_[which]);
  return dynamic_cast<DynamicElement *>(l->GetChild());
}

DynamicExpression * DynamicElement::GetSingleExpressionChild(int which) const {
  return dynamic_cast<DynamicExpression *>(GetSingleChild(which));
}
DynamicExpression * DynamicElement::GetSingleStatementChild(int which) const{
    return dynamic_cast<DynamicExpression *>(GetSingleChild(which));
}
OTime DynamicElement::ComputeTime() const { 
  if (!parent_) return CREATION;
  return parent_->ComputeChildTime(this);
}


void Statement::Init() {
  StaticElement::Init();
}

// Can't erase statements that are linked
void Statement::L1_Erase() {
  CHECK(parent_ == NULL);
  Named::L1_Erase();
}

void DynamicExpression::Init(Expression * static_parent, 
			     Link *parent, OMap binding){
  DynamicElement::Init(static_parent, parent, binding);
  value_ = NULL;
  CheckSetValueViolation();
  // there should already be a violation at the parent
  // if (GetParent()) GetParent()->ChildExpressionChanged();
}
void DynamicExpression::SetValue(Object new_value) {
  CL.ChangeValue(&value_, new_value);
  CheckSetValueViolation();
  if (GetParent()) GetParent()->ChildExpressionChanged();
}

void DynamicExpression::L1_Erase() {
  DynamicElement * parent = GetParent();
  DynamicElement::L1_Erase();
  parent->ChildExpressionChanged();
}
void DynamicExpression::CheckSetValueViolation() {
  bool perfect = (value_ != NULL) && (value_ == ComputeValue());
  Violation * value_violation = FindViolation(this, Violation::VALUE);
  if (perfect && value_violation) {
    value_violation->L1_Erase(); 
    return;
  }
  if (!perfect && !value_violation) {
    New<ValueViolation>(this, time_);
    return;
  }
}



void StaticOn::Init(){
  Statement::Init();
  New<MissingDynamicOnViolation>(this, CREATION);
}
set<Variable> StaticOn::GetIntroducedVariables() const {
  return ::GetVariables(GetPattern().Data());
}
void StaticOn::L1_Erase() {
  Statement::L1_Erase();
}
// this thing has no dynamic parent. 
void DynamicOn::Init(StaticOn *static_parent) {
  DynamicStatement::Init(static_parent, NULL, OMap::Default());
  Violation * missing_dynamic 
    = FindViolation(GetStatic(), Violation::MISSING_DYNAMIC_ON);
  CHECK(missing_dynamic);
  missing_dynamic->L1_Erase();
}
void DynamicOn::L1_Erase(){
  CHECK(GetStatic());
  CHECK(!FindViolation(GetStatic(), Violation::MISSING_DYNAMIC_ON));
  New<MissingDynamicOnViolation>(GetStatic(), CREATION);
  DynamicStatement::L1_Erase();
}

OTime DynamicOn::ComputeChildTime(const Link * link, 
				  const Element * child) const{
  Time ret = time_.Data();
  OPattern p = GetPattern();
  ret = max(ret, BB.FindLastTime(Substitute(child->GetBinding().Data(), 
					    p.Data())));
  return OTime::Make(ret);
}

void StaticRepeat::Init() {
  L1_SetObject(REPETITION_VARIABLE, M.L1_GetNextUniqueVariable());
}

void StaticDelay::Init() {
  Statement::Init();
}
void StaticLet::Init() {
  Statement::Init();
}
void OutputStatement::Init() {
  Statement::Init();
}
void DynamicOutput::IsPerfect() {
  DynamicExpression * expr = GetTupleExpression();
  if (!expr) return false;
  if (!posting_) return false;
  if (posting_->tuple_ != expr->value_) return false;
  if (posting_->time_ != time_.Data()) return false;
  return true;
}
void DynamicOutput::CheckSetPostingViolation() {
  bool perfect = IsPerfect();
  Violation * posting_violation = FindViolation(this, Violation::POSTING);
  if (perfect && posting_violation) {
    posting_violation->L1_Erase(); 
    return;
  }
  if (!perfect && !posting_violation) {
    Time time = time_;
    if (posting_) time = min(time, posting_->time_);
    New<PostingViolation>(this, OTime::Make(time));
    return;
  }
}

void StaticIf::Init() {
  Statement::Init();
}

void Expression::Init() {
  StaticElement::Init();
}

Statement * Statement::MakeStatement(Keyword type) {
  if (type.Data() == "pass") return New<PassStatement>();
  if (type.Data() == "on") return New<StaticOn>();
  if (type.Data() == "repeat") return New<StaticRepeat>();
  if (type.Data() == "delay") return New<StaticDelay>();
  if (type.Data() == "let") return New<StaticLet>();
  if (type.Data() == "output") return New<OutputStatement>();
  if (type.Data() == "if") return New<StaticIf>();
  if (type.Data() == "parallel") return New<StaticParallel>();
  CHECK(false);
  return NULL;
}

Statement * Statement::ParseSingle(const Tuple & t, uint * position) {
  cout << "ParseSingle pos=" << *position << " t=" << OTuple::Make(t) << endl;
  Keyword stype = t[(*position)++];
  if (stype == NULL) return NULL;
  Statement * ret = MakeStatement(stype);
  for (int i=0; i<ret->NumObjects(); i++) {
    CHECK(*position < t.size());
    Object o = t[(*position)++];
    ret->L1_SetObject(i, o);
  }
  for (int i=0; i<ret->NumExpressionChildren(); i++) {
    CHECK(*position < t.size());
    Object o = t[(*position)++];
    Expression * e = Expression::Parse(o);
    ret->L1_LinkChild(i, e);
  }
  if (*position < t.size()  && t[*position].Type() == OMAP) {
    OMap m = t[(*position)++];
    forall(run, m.Data()) {
      Keyword key = run->first;
      Object value = run->second;
      if (key.Data() == "name") {
	ret->L1_SetName(value);	
      }
      if (key.Data() == "parent") {
	// TODO
      }
    }
  }  
  return ret;
}

vector<Statement *> Statement::Parse(const Tuple & t) {
  cout << "Statement::Parse " << OTuple::Make(t) << endl;
  uint position = 0;
  Statement * parent = NULL;
  vector<Statement *> ret;
  while (position < t.size()) {
    Object o = t[position];
    if (o == SEMICOLON) {
      CHECK(parent != NULL);
      parent = NULL;
      position++;
    } else if (o.Type() == OTUPLE) {
      vector<Statement *> subs = Parse(OTuple(o).Data());
      if (parent->TypeKeyword().Data() == "parallel") {
	CHECK((int)parent->static_children_.size() 
	      == parent->NumExpressionChildren());
	while (parent->static_children_.size() < 
	       parent->NumExpressionChildren() + subs.size()) {
	  parent->static_children_.push_back(New<SingleLink>(parent));
	}
      }
      CHECK(parent->NumStatementChildren() == (int)subs.size());      
      for (uint i=0; i<subs.size(); i++) {
	parent->L1_LinkChild(parent->NumExpressionChildren()+i, subs[i]);
      }
      parent = NULL;
      position++;
    } else {
      Statement * s = ParseSingle(t, &position);
      if (parent) {
	cout << "Hooking up child " << s->ToString(0) << endl;
	cout << "To parent " << parent->ToString(0) << endl;
	CHECK(parent->NumStatementChildren() == 1);
	parent->L1_LinkChild(parent->NumExpressionChildren(), s);
      } else {
	ret.push_back(s);
      }
      parent = s;
    }
  }
  return ret;
}

Expression * Expression::Parse(const Object & o){
  cout << "Expression::Parse " << o << endl;
  if (o == NULL) return NULL;
  Tuple t;
  Expression *ret;
  if (o.Type() == OTUPLE) t = OTuple(o).Data();
  if (o.Type() != OTUPLE 
      || t.size() == 0
      || t[0].Type() != KEYWORD) {
    ret = New<StaticConstant>();
    ret->L1_SetObject(0, o);
    return ret;
  }
  Keyword type = t[0];
  ret = MakeExpression(type);
  
  CHECK((int)t.size()-1 == ret->NumObjects() + ret->NumChildren());
  for (int i=0; i<ret->NumObjects(); i++) {
    ret->L1_SetObject(i, t[1+i]);
  }
  for (int i=0; i<ret->NumChildren(); i++) {
    Expression * sub = Expression::Parse(t[1+ret->NumObjects()+i]);
    ret->L1_LinkChild(i, sub);
  }
  return ret;
}

string Statement::ToString(int indent) const {
  string ret(indent, ' ');
  if (this == NULL) return ret + "null\n";
  ret += ToStringSingle();
  vector<Statement *> children = GetStatementChildren();
  if (children.size() > 1 || TypeKeyword().Data() == "parallel") {
    ret += " {\n";
    for (uint i=0; i<children.size(); i++) 
      ret += children[i]->ToString(indent+2);
    ret += string(indent, ' ') + "}\n";
    return ret;
  }
  if (children.size() == 0) {
    return ret + " ;\n";
  }
  CHECK(children.size() == 1);
  return ret + "\n" + children[0]->ToString(indent+2);
}
string Statement::ToStringSingle() const {
  string ret = TypeKeyword().ToString();
  ret += ParameterListToString();
  return ret;
}
string Expression::ToString() const {
  string ret = "(";
  ret += TypeKeyword().ToString();
  ret += ParameterListToString();
  ret += ")";
  return ret;
}

Expression * Expression::MakeExpression(Keyword type) {
  if (type.Data() == "substitute") return New<StaticSubstitute>();
  if (type.Data() == "constant") return New<StaticConstant>();
  if (type.Data() == "flake_choice") return New<StaticFlakeChoice>();
  CHECK(false);
  return NULL;
}

string StaticConstant::ToString() const {
  Object o = GetObject(OBJECT);
  if (o==NULL) return "null";
  if (o.Type() == OTUPLE) {
    Tuple t = OTuple(o).Data();
    if (t.size() > 0 && t[0].Type() == KEYWORD) {
      return "(constant " + o.ToString() + ")";
    }
  }
  return o.ToString();
}
Object DynamicConstant::ComputeValue() {
  return GetObject(OBJECT);
}
string StaticElement::ParameterListToString() const {
  string ret;
  for (int i=0; i<NumObjects(); i++) {
    ret += " " + GetObject(i).ToString();
  }
  for (int i=0; i<NumExpressionChildren(); i++) {
    Expression * expr = GetExpressionChild(i);
    if (expr) ret += " " + expr->ToString();
    else ret += " null";
  }
  return ret;
}

Keyword StaticOn::TypeKeyword() const {
  return Keyword::Make("on");
}
Keyword StaticRepeat::TypeKeyword() const {
  return Keyword::Make("repeat");
}
Keyword StaticDelay::TypeKeyword() const {
  return Keyword::Make("delay");
}
Keyword StaticLet::TypeKeyword() const {
  return Keyword::Make("let");
}
Keyword OutputStatement::TypeKeyword() const {
  return Keyword::Make("output");
}
Keyword StaticIf::TypeKeyword() const {
  return Keyword::Make("if");
}

Keyword StaticConstant::TypeKeyword() const {
  return Keyword::Make("output");
}
Keyword StaticSubstitute::TypeKeyword() const {
  return Keyword::Make("substitute");
}
Keyword StaticFlakeChoice::TypeKeyword() const {
  return Keyword::Make("flake_choice");
}
 
void DynamicElement::Init(StaticElement * static_parent, 
			  Link *parent, OMap binding) {
  Element::Init();
  binding_ = binding;
  static_parent_ = parent_ = NULL;
  CHECK(static_parent);
  static_parent->dynamic_children_->L1_AddChild(this);
  CHECK(static_parent_ == static_parent);
  if (parent) {
    parent->L1_AddChild(this);
    CHECK(parent_ == parent);
  } else {
    // This is ok for example for an on statement
    // Check that the static parent has no parent
    CHECK(GetStatic()->parent_ == NULL);
  }
 
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
      chid = New<OnMultilink>(this, dynamic_cast<DynamicOn *>(this)
			      ->GetPattern());
      break;
    default:
      CHECK(false);
      break;
    }
    children_.push_back(child);    
  }
};

void DynamicElement::FindParent() const {
  CHECK(static_parent_);
  CHECK(static_parent_->parent_);
  return static_parent_->GetParent()->dynamic_children_->GetChild
    (Restrict(bindings_, static_parent_->GetParent()->GetVariables()));
}
