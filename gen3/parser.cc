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

inline Object GetNext(const Tuple& t, uint * pos) {
  CHECK (*pos < t.size());
  return t[(*pos)++];
}

Element * ParseElement(const Tuple & t, uint * pos) {
  // Get the next object
  Object o = GetNext(t, pos);

  // a single variable object is a substitute with a constant below
  if (o.GetType() == Object::VARIABLE) { 
    ConstantElement *constant = New<ConstantElement>();
    constant->object_ = o;
    Element *ret = New<SubstituteElement>();
    ret->AddChild(constant);
    return ret;
  }

  // Hack so an OTuple interprets as if a maketuple keyword exists before it
  // and then reads its parameters from the otuple as a regular function does
  if (o.GetType() == Object::OTUPLE) {
    o = Keyword::Make("maketuple");
    (*pos)--;
  }

  // If it's not a function name, it's a constant
  if (o.GetType() != Object::KEYWORD
      || Element::TypeKeywordToFunction(o) == -1) {
    ConstantElement *constant = New<ConstantElement>();
    constant->object_ = o;
    return constant;
  }

  // Here, o is now a keyword that specifies a function
  // Create the static element
  Keyword function_keyword = o;
  Element * ret = MakeElementByKeyword(function_keyword);

  if (ret->GetFunction() == Element::CONSTANT) // Read the object
    dynamic_cast<ConstantElement *>(ret)->object_ = GetNext(t, pos);

  // Read its children and hook them up to the parent
  vector<Element *> children;
  if (ret->ChildrenGoInTuple()) {
    o = GetNext(t, pos);
    if (o.GetType() != Object::OTUPLE) {
      cerr << "Expecting a tuple, got:" << o << endl;
      cerr << "While parsing function keyword:" << function_keyword << endl;
      CHECK(false);
    }
    children = ParseElements(OTuple(o).Data());
  } else {
    CHECK(!ret->HasVariableNumChildren());
    for (int i=0; i<ret->NumChildren(); i++) {
      children.push_back(ParseElement(t, pos));
    }
  }
  for (uint i=0; i<children.size(); i++) {
    ret->AddChild(children[i]);
  }
  return ret;
}

vector<Element *> ParseElements(const Tuple & t) {
  uint pos = 0;
  vector<Element *> ret;
  while (pos < t.size()) {
    Element *e = ParseElement(t, &pos);
    ret.push_back(e);
  }
  return ret;
}

Element * MakeElementByKeyword(Keyword type) {
  Element::Function function = Element::TypeKeywordToFunction(type);
  CHECK(function != -1);

  switch (function) {
#define FUNCTION(Func, FUNC) case Element::FUNC: \
    return MakeElement<Func##Element>();
    ALL_FUNCTIONS;
#undef FUNCTION
  default: CHECK(false);
  }
  return NULL;
}



