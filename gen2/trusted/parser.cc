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

#include "parser.h"

// This is all the code that involves reading and writing the model.
// UNTRUSTED

Statement * MakeStatementByKeyword(Keyword type){
  Element::Function function = Element::StringToFunction(Upcase(type.Data()));
  switch (function) {
  case Element::PASS: return MakeStatement<StaticPass>();
  case Element::ON: return MakeStatement<StaticOn>();
  case Element::REPEAT: return MakeStatement<StaticRepeat>();
  case Element::DELAY: return MakeStatement<StaticDelay>();
  case Element::LET: return MakeStatement<StaticLet>();
  case Element::POST: return MakeStatement<StaticPost>();
  case Element::IF: return MakeStatement<StaticIf>();
  case Element::PARALLEL: return MakeStatement<StaticParallel>();
  default: CHECK(false);
  }
  return NULL;
}

Expression * MakeExpressionByKeyword(Keyword type){
  Element::Function function = Element::StringToFunction(Upcase(type.Data()));
  switch (function) {
  case Element::SUBSTITUTE: return MakeExpression<StaticSubstitute>();
  case Element::CONSTANT: return MakeExpression<StaticConstant>();
  case Element::FLAKE_CHOICE: return MakeExpression<StaticFlakeChoice>();
  case Element::EQUAL: return MakeExpression<StaticEqual>();
  case Element::SUM: return MakeExpression<StaticSum>();
  default: CHECK(false);
  }
  return NULL;
}

Statement * ParseSingleStatement(const Tuple & t, uint * position) {
  Keyword stype = t[(*position)++];
  if (stype == NULL) return NULL;
  Statement * ret = MakeStatementByKeyword(stype);
  for (int i=0; i<ret->NumObjects(); i++) {
    CHECK(*position < t.size());
    Object o = t[(*position)++];
    ret->SetObject(i, o);
  }
  for (int i=0; i<ret->NumExpressionChildren(); i++) {
    CHECK(*position < t.size());
    Object o = t[(*position)++];
    Expression * e = ParseExpression(o);
    e->LinkToParent(ret, i);
  }
  if (*position < t.size()  && t[*position].GetType() == Object::OMAP) {
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

vector<Statement *> ParseStatements(const Tuple & t) {
  uint position = 0;
  Statement * parent = NULL;
  vector<Statement *> ret;
  while (position < t.size()) {
    Object o = t[position];
    if (o == SEMICOLON) {
      CHECK(parent != NULL);
      parent = NULL;
      position++;
    } else if (o.GetType() == Object::OTUPLE) {
      vector<Statement *> subs = ParseStatements(OTuple(o).Data());
      if (parent->GetFunction() == Element::PARALLEL) {
	CHECK((int)parent->static_children_.size() 
	      == parent->NumExpressionChildren());
	while (parent->static_children_.size() < 
	       parent->NumExpressionChildren() + subs.size()) {
	  parent->static_children_.push_back(New<SingleLink>(parent));
	}
      }
      //cerr << "o=" << o.ToString() << endl;
      CHECK(parent->NumStatementChildren() == (int)subs.size());      
      for (uint i=0; i<subs.size(); i++) {
	if (subs[i])
	  subs[i]->LinkToParent(parent, parent->NumExpressionChildren()+i);
      }
      parent = NULL;
      position++;
    } else {
      //int old_position = position;
      Statement * s = ParseSingleStatement(t, &position);
      //cerr << "Parsed single statement " 
      //	   << Element::FunctionToString(s->GetFunction())
      //   << " from positions " << old_position << " to " << position 
      //   << endl;
      CHECK(s);
      if (parent) {
	  CHECK(parent->NumStatementChildren() == 1);
	  s->LinkToParent(parent, parent->NumExpressionChildren());
	  cerr << "linking it onto the parent " << endl;
      } else {
	ret.push_back(s);
	//cerr << "adding it to ret, size=" << ret.size() << endl;
      }
      parent = s;
    }
  }
  //cerr << "Parsed " << ret.size() << " statements from " 
  //     << OTuple::Make(t).ToString()
  //     << endl;
  return ret;
}

Expression * ParseExpression(const Object & o){
  //cout << "Expression::Parse " << o << endl;
  if (o == NULL) return NULL;
  Tuple t;
  Expression *ret;
  if (o.GetType() == Object::OTUPLE) t = OTuple(o).Data();
  if (o.GetType() != Object::OTUPLE 
      || t.size() == 0
      || t[0].GetType() != Object::KEYWORD) {
    ret = New<StaticConstant>();
    ret->SetObject(0, o);
    return ret;
  }
  Keyword type = t[0];
  ret = MakeExpressionByKeyword(type);
  
  CHECK((int)t.size()-1 == ret->NumObjects() + ret->NumChildren());
  for (int i=0; i<ret->NumObjects(); i++) {
    ret->SetObject(i, t[1+i]);
  }
  for (int i=0; i<ret->NumChildren(); i++) {
    Expression * sub = ParseExpression(t[1+ret->NumObjects()+i]);
    sub->LinkToParent(ret, i);
  }
  return ret;
}



