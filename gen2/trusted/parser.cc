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

StaticElement * MakeStaticElementByKeyword(Keyword type){
  Element::Function function = Element::TypeKeywordToFunction(type);
  CHECK(function != -1);

  switch (function) {
#define FUNCTION(Func, FUNC) case Element::FUNC: \
    return MakeStaticElement<Static##Func>();
    ALL_FUNCTIONS;
#undef FUNCTION
  default: CHECK(false);
  }
  return NULL;
}

StaticElement * ParseElement(const Tuple & t, uint * position) {
  Object o = t[(*position)++];
  if (o.GetType() == Object::VARIABLE) { 
    // it's a substitute
    StaticElement *constant = New<StaticConstant>();
    constant->SetObject(StaticConstant::OBJECT, o);
    StaticElement *ret = New<StaticSubstitute>();
    constant->LinkToParent(ret, StaticSubstitute::CHILD);
    return ret;
  }
  if (o.GetType() == Object::OTUPLE) {
    // its a maketuple
    o = Keyword::Make("maketuple");
    (*position)--;
  }
  if (o.GetType() != Object::KEYWORD
      || Element::TypeKeywordToFunction(o) == -1) {
    // it's a constant
    StaticElement *constant = New<StaticConstant>();
    constant->SetObject(StaticConstant::OBJECT, o);
    return constant;
  }
  StaticElement * ret = MakeStaticElementByKeyword(o);
  for (int i=0; i<ret->NumObjects(); i++) {
    CHECK(*position < t.size());
    o = t[(*position)++];
    ret->SetObject(i, o);
  }
  vector<StaticElement *> children;
  if (ret->ChildrenGoInTuple()) {
    CHECK(*position < t.size());
    o = t[(*position)++];
    CHECK(o.GetType() == Object::OTUPLE);
    children = ParseElements(OTuple(o).Data());
  } else {
    CHECK(!ret->HasVariableNumChildren());
    for (int i=0; i<ret->NumChildren(); i++) {
      children.push_back(ParseElement(t, position));
    }
  }
  for (uint i=0; i<children.size(); i++) {
    children[i]->LinkToParent(ret, i);
  }
  /*if (*position < t.size()  && t[*position].GetType() == Object::OMAP) {
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
    } */ 
  return ret;
}

vector<StaticElement *> ParseElements(const Tuple & t) {
  uint position = 0;
  vector<StaticElement *> ret;
  while (position < t.size()) {
    StaticElement *e = ParseElement(t, &position);
    ret.push_back(e);
  }
  return ret;
}



