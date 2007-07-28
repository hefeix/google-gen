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

#include "static.h"
#include "model.h"

void StaticElement::Init() {
  Named::Init();
  dynamic_children_ = new MultiLink(this);
  for (int i=0; i<NumChildren(); i++) 
    static_children_.push_back(new SingleLink(this));  
  objects_.resize(NumObjects());
}
void StaticElement::L1_Erase() {
  for (uint i=0; i<static_children_.size(); i++) 
    static_children_[i]->L1_Erase();
  dynamic_children_->L1_Erase();
  Named::L1_Erase();
}
Element * StaticElement::GetChild(int which) const { 
  return static_children_[which]->GetChild();
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

void Statement::Init() {
  StaticElement::Init();
}

// Can't erase statements that are linked
void Statement::L1_Erase() {
  CHECK(parent_ == NULL);
  Named::L1_Erase();
}

void OnStatement::Init() {
  Statement::Init();
}

void OnStatement::L1_Subscribe() {
  Query * q = BB.L1_GetExecuteQuery(GetPattern(), SamplingInfo(), NULL);
  subscription_ = new SubType(q, UPDATE_COUNT | UPDATE_WHICH | UPDATE_TIME,
			      this);
  subscription_->L1_SendCurrentAsUpdates();
}
void OnStatement::Update(const QueryUpdate &update, SubType *sub) {
  cout << "TODO: implement on statement update";
}

void RepeatStatement::Init() {
  L1_SetObject(REPETITION_VARIABLE, M.L1_GetNextUniqueVariable());
}

void DelayStatement::Init() {
  Statement::Init();
}
void LetStatement::Init() {
  Statement::Init();
}
void OutputStatement::Init() {
  Statement::Init();
}
void IfStatement::Init() {
  Statement::Init();
}

void Expression::Init() {
  StaticElement::Init();
}

Statement * Statement::MakeStatement(Keyword type) {
  if (type.Data() == "on") return Make<OnStatement>();
  if (type.Data() == "repeat") return Make<RepeatStatement>();
  if (type.Data() == "delay") return Make<DelayStatement>();
  if (type.Data() == "let") return Make<LetStatement>();
  if (type.Data() == "output") return Make<OutputStatement>();
  if (type.Data() == "if") return Make<IfStatement>();
  if (type.Data() == "parallel") return Make<ParallelStatement>();
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
	  parent->static_children_.push_back(new SingleLink(parent));
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
    ret = Make<ConstantExpression>();
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
  if (type.Data() == "substitute") return Make<SubstituteExpression>();
  if (type.Data() == "constant") return Make<ConstantExpression>();
  if (type.Data() == "flake_choice") return Make<FlakeChoiceExpression>();
  CHECK(false);
  return NULL;
}

string ConstantExpression::ToString() const {
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

Keyword OnStatement::TypeKeyword() const {
  return Keyword::Make("on");
}
Keyword RepeatStatement::TypeKeyword() const {
  return Keyword::Make("repeat");
}
Keyword DelayStatement::TypeKeyword() const {
  return Keyword::Make("delay ");
}
Keyword LetStatement::TypeKeyword() const {
  return Keyword::Make("let");
}
Keyword OutputStatement::TypeKeyword() const {
  return Keyword::Make("output");
}
Keyword IfStatement::TypeKeyword() const {
  return Keyword::Make("if");
}

Keyword ConstantExpression::TypeKeyword() const {
  return Keyword::Make("output");
}
Keyword SubstituteExpression::TypeKeyword() const {
  return Keyword::Make("substitute");
}
Keyword FlakeChoiceExpression::TypeKeyword() const {
  return Keyword::Make("flake_choice");
}
 
