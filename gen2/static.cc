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

StaticElement::StaticElement()
  :dynamic_children_(this) {
}

Statement::Statement()
  :child_(this) {
  // don't call CL.Creating(this) because it's called by the superclass Named.
}

// Can't erase statements that are linked
void Statement::L1_Erase() {
  CHECK(GetNumChildren() == 0);
  CHECK(parent_ == NULL);
  Named::L1_Erase();
}

void Statement::ConnectToParent(Statement * parent) {
  CHECK(parent_ == NULL);
  CHECK(parent != NULL);
  CL.ChangeValue(&parent_, parent);
  parent->L1_LinkToChild(this);
}

void Statement::DisconnectFromParent() {
  CHECK(parent_ != NULL);
  parent_->L1_UnlinkChild(this);
  CL.ChangeValue(&parent_, (Statement *)NULL);
}

void Statement::L1_LinkToChild(Statement * child) {
  CL.ChangeValue(&child_, child);
}

void Statement::L1_UnlinkChild(Statement * child) {
  CL.ChangeValue(&child_, (Statement *)NULL);
}

OnStatement::OnStatement(OPattern p) {
  pattern_ = p;
  Query * q = BB.L1_GetExecuteQuery(pattern_, SamplingInfo(), NULL);
  subscription_ = new SubType(q, UPDATE_COUNT | UPDATE_WHICH | UPDATE_TIME,
			      this);
  subscription_->L1_SendCurrentAsUpdates();
}
void OnStatement::Update(const QueryUpdate &update, SubType *sub) {
  cout << "TODO: implement on statement update";
}

RepeatStatement::RepeatStatement(Expression * number_of_repetitions)
  :number_of_repetitions_(this) {
  number_of_repetitions_->L1_AddChild(number_of_repetitions);
  repetition_variable_ = M.L1_GetNextUniqueVariable();
}

DelayStatement::DelayStatement(Expression * delay)
  :delay_(this) {
  delay_.L1_AddChild(delay);
}

LetStatement::LetStatement(Variable variable, Expression *value)
  :value_(this) {
  variable_ = variable;
  value_.L1_AddChild(value);
}

OutputStatement::OutputStatement(Expression * tuple) {
  tuple_ = tuple;
}

FlakeChoice::FlakeChoice(Expression *chooser) {
  chooser_ = chooser;
}
SubstituteExpression::SubstituteExpression(Object object) {
  object_ = object;
}

Statement * Statement::ParseSingle(const Tuple & t, uint * position) {
  Statement * ret;
  Keyword stype = t[*position++];
  if (stype.Data() == "on") {
    OPattern pattern = t[*position++];
    ret = new OnStatement(pattern);
  } 
  if (stype.Data() == "repeat") {
    Expression * expr = Expression::Parse(OTuple(t[*position++]).Data());
    ret = new RepeatStatement(expr);
  }
  if (stype.Data() == "delay") {
    Expression * delay = Expression::Parse(OTuple(t[*position++]).Data());
    ret = new DelayStatement(delay);
  }
  if (stype.Data() == "let") {
    Variable var =  t[*position++];
    Expression * value = Expression::Parse(OTuple(t[*position++]).Data());
    ret = new LetStatement(var, value);
  }
  if (stype.Data() == "output") {
    Expression * tuple = Expression::Parse(OTuple(t[*position++]).Data());
    ret = new OutputStatement(tuple);
  } 
  /*
    more statement types l8r
  if (stype.Data() == "") {
  }
  if (stype.Data() == "") {
  }
  if (stype.Data() == "") {
  }
  if (stype.Data() == "") {
  }*/

  if (*position < t.size()  && t[*position].Type() == OMAP) {
    OMap m = t[*(position++)];
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
      for (uint i=0; i<subs.size(); i++) {
	subs[i]->ConnectToParent(parent);
      }
      parent = NULL;
      position++;
    } else {
      Statement * s = ParseSingle(t, &position);
      if (parent) {
	s->ConnectToParent(parent);
      } else {
	ret.push_back(s);
      }
      parent = s;
    }
  }
  return ret;
}

Expression * Expression::Parse(const Tuple & t){
  if (t.size() == 0) 
    return NULL;
  if (t.size() == 1) {
    return new SubstituteExpression(t[0]);
  }
  Keyword type = t[0];
  if (type.Data() == "flakechoice"){
    OTuple chooser_name_expression = t[1];
    return new FlakeChoice(Expression::Parse(chooser_name_expression.Data()));
  }
  CHECK(false);
  return NULL;
}

string Statement::ToString(int indent) const {
  string ret(indent, ' ');
  ret += ToStringSingle();
  vector<Statement *> children = GetChildren();
  if (children.size() == 0) {
    return ret + " ;\n";
  }
  if (children.size() == 1) {
    return ret + "\n" + children[0]->ToString(indent+2);
  }
  ret += " {\n";
  for (uint i=0; i<children.size(); i++) 
    ret += children[i]->ToString(indent+2);
  ret += string(indent, ' ') + "}\n";
  return ret;
}

string OnStatement::ToStringSingle() const {
  return "on " + pattern_.ToString();
}
string RepeatStatement::ToStringSingle() const {
  return "repeat " + number_of_repetitions_->ToString();
}
string DelayStatement::ToStringSingle() const {
  return "delay " + delay_->ToString();
}
string LetStatement::ToStringSingle() const {
  return "let " + variable_.ToString() + " " + value_->ToString();
}
string OutputStatement::ToStringSingle() const {
  return "output " + tuple_->ToString();
}

string SubstituteExpression::ToString() const {
  return "(" + object_.ToString() + ")";
}
string FlakeChoice::ToString() const {
  return "( flakechoice " + (chooser_?chooser_->ToString():"()") + " )";
}
