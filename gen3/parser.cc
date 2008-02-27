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

Element * SimpleParseElement(Object o, Element *parent){
  CHECK(o.GetType() == Object::OTUPLE);
  const Tuple & t = OTuple(o).Data();
  CHECK(t.size() >= 2);
  Element * ret = MakeElementByKeyword(t[0], parent);
  ret->SetObject(t[1]);
  for (uint i=2; i<t.size(); i++) 
    SimpleParseElement(t[i], ret);
  return ret;
}
vector<Element *> SimpleParseElements(const Tuple & t, Element *parent) {
  vector<Element *> ret;
  forall(run, t) ret.push_back(SimpleParseElement(*run, parent));
  return ret;
}


inline Object GetNext(const Tuple& t, uint * pos) {
  CHECK (*pos < t.size());
  return t[(*pos)++];
}

Element * PrettyParseElement(const Tuple & t, uint * pos, Element *parent) {
  // Get the next object
  Object o = GetNext(t, pos);

  // a single variable object is a substitute element
  if (o.GetType() == Object::VARIABLE) {
    if (parent && 
	(parent->GetOutgoingVariables(parent->children_.size()) % o) ) { 
      SubstituteElement *ret = New<SubstituteElement>(parent);
      ret->SetObject(o);
      return ret;
    }
    // It's a new variable.  this is probably part of a match thing
    (*pos)--; return NULL;
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
    ConstantElement *constant = New<ConstantElement>(parent);
    constant->SetObject(o);
    return constant;
  }

  // Here, o is now a keyword that specifies a function
  // Create the static element
  Keyword function_keyword = o;
  Element * ret = MakeElementByKeyword(function_keyword, parent);

  // special case for MakeTuple
  if (ret->GetFunction() == Element::MAKETUPLE) {
    Object o = GetNext(t, pos);
    const Tuple & t2 = OTuple(o).Data();
    PrettyParseElements(t2, ret);
    ret->SetObject(Integer::Make(t2.size()));
    return ret;
  }

  // special case for MatchBase subtypes
  MatchBaseElement *mb = dynamic_cast<MatchBaseElement *>(ret);
  if (mb) {
    Object o = GetNext(t, pos);
    const Tuple & t2 = OTuple(o).Data();
    uint pos2 = 0;
    Tuple object_tuple;
    while (pos2 < t2.size()) {
      Element *e = PrettyParseElement(t2, &pos2, mb);
      if (e) {
	object_tuple.push_back(NULL);
      } else {
	Object new_var = GetNext(t2, &pos2);
	CHECK(new_var.GetType() == Object::VARIABLE);
	CHECK(!mb->IsBound(new_var));
	object_tuple.push_back(new_var);
      }
    }
    mb->SetObject(OTuple::Make(object_tuple));
    if (mb->HasExtraChild()) {
      PrettyParseElement(t, pos, mb);
    }
    return mb;
  }

  if (ret->HasObject()) {
    Object o = GetNext(t, pos);
    ret->SetObject(o);
  }

  // Read its children and hook them up to the parent
  if (ret->ChildrenGoInTuple()) {
    o = GetNext(t, pos);
    if (o.GetType() != Object::OTUPLE) {
      cerr << "Expecting a tuple, got:" << o << endl;
      cerr << "While parsing function keyword:" << function_keyword << endl;
      CHECK(false);
    }
    PrettyParseElements(OTuple(o).Data(), ret);
  } else {
    CHECK(!ret->HasVariableNumChildren());
    for (int i=0; i<ret->RequiredNumChildren(); i++) {
      PrettyParseElement(t, pos, ret);
    }
  }
  return ret;
}

vector<Element *> PrettyParseElements(const Tuple & t, Element *parent) {
  uint pos = 0;
  vector<Element *> ret;
  while (pos < t.size()) {
    Element *e = PrettyParseElement(t, &pos, parent);
    ret.push_back(e);
  }
  return ret;
}

Element * MakeElementByKeyword(Keyword type, Element *parent) {
  Element::Function function = Element::TypeKeywordToFunction(type);
  CHECK(function != -1);
  switch (function) {
#define FUNCTION(Func, FUNC) case Element::FUNC: \
    return MakeElement<Func##Element>(parent);
    ALL_FUNCTIONS;
#undef FUNCTION
  default: CHECK(false);
  }
  return NULL;
}



