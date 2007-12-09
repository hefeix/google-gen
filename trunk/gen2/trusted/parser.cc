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
  Element::Function function = TypeKeywordToFunction(type);
  CHECK(Element::IsStatementFunction(function));

  switch (function) {
#define FUNCTION(Func, FUNC) case Element::FUNC: \
    return MakeStatement<Static##Func>();
    ALL_STATEMENTS;
#undef FUNCTION
  default: CHECK(false);
  }
  return NULL;
}

Expression * MakeExpressionByKeyword(Keyword type){
  Element::Function function = TypeKeywordToFunction(type);
  CHECK(Element::IsExpressionFunction(function));

  switch (function) {
#define FUNCTION(Func, FUNC) case Element::FUNC: \
    return MakeExpression<Static##Func>();
    ALL_EXPRESSIONS;
#undef FUNCTION
  default: CHECK(false);
  }
  return NULL;
}

Statement * ParseSingleStatement(const Tuple & t, uint * position) {
  if (t[*position].GetType() != Object::KEYWORD) {
    cerr << "expected a statement keyword got" 
	 << t[*position] << endl;
    CHECK(false);
  }
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
      if (parent->HasVariableNumChildren()) {
	CHECK((int)parent->static_children_.size() 
	      == parent->NumExpressionChildren());
	while (parent->static_children_.size() < 
	       parent->NumExpressionChildren() + subs.size()) {
	  parent->static_children_.push_back(New<SingleLink>(parent));
	}
      }
      if (parent->NumStatementChildren() != (int)subs.size()) {
	cerr << "o=" << o.ToString() << endl;
	CHECK(false);
      }
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
      //   << " Tuple:" << OTuple::Make(t) << endl;
      CHECK(s);
      if (parent) {
	  CHECK(parent->NumStatementChildren() == 1);
	  s->LinkToParent(parent, parent->NumExpressionChildren());
	  //cerr << "linking it onto the parent " << endl;
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
  Expression *ret;
  Tuple t;
  Keyword type;
  bool parse_failure = true;
  if (o.GetType() == Object::OTUPLE) {
    t = OTuple(o).Data();   
    if (t.size() > 0 && t[0].GetType() == Object::KEYWORD) {
      type = t[0];
      if (Element::IsExpressionFunction(TypeKeywordToFunction(type))) {
	parse_failure = false;
      }
    }
  }
  if (parse_failure) {
    // here we expected an expression, but got some object that wasn't
    // an OTuple starting with a function name.  We'll assume that it is a 
    // constant expression or a substitute expression. 
    bool substitute = DeepSubstitutePossible(o);
    Expression *constant = New<StaticConstant>();
    constant->SetObject(0, o);
    if (!substitute) return constant;
    ret = New<StaticSubstitute>();
    constant->LinkToParent(ret, StaticSubstitute::CHILD);
    return ret;
  }
  ret = MakeExpressionByKeyword(type);
    
  CHECK(ret->HasVariableNumChildren() || 
	(int)t.size()-1 == ret->NumObjects() + ret->NumChildren());

  for (int i=0; i<ret->NumObjects(); i++) {
    ret->SetObject(i, t[1+i]);
  }

  if (ret->HasVariableNumChildren()) {
    CHECK((int)ret->static_children_.size() == 0);
    while (ret->static_children_.size() + ret->NumObjects() + 1 < t.size())
      ret->static_children_.push_back(New<SingleLink>(ret));
  }

  for (int i=0; i<ret->NumChildren(); i++) {
    Expression * sub = ParseExpression(t[1+ret->NumObjects()+i]);
    sub->LinkToParent(ret, i);
  }
  return ret;
}



