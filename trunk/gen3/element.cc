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
#include "execution.h"

#undef ITEM
#define ITEM(x) #x

#define FUNCTION(func, FUNC) ITEM(FUNC),
CLASS_ENUM_DEFINE(Element, Function);
#undef FUNCTION

#define FUNCTION(f, F)				\
  CLASS_ENUM_DEFINE(Static##f, ChildName);	\
  CLASS_ENUM_DEFINE(Static##f, ObjectName);
ALL_FUNCTIONS
#undef FUNCTION

Object Element::Execute(Thread thread) {
  Tuple results;
  for (uint c=0; c<children_.size(); c++) {
    Object return_val = children_[c].Execute(thread);
    results.push_back(return_val);
  }
  return ComputeReturnValue(thread, results);
}

Record Element::GetRecordForDisplay() const {
  Record ret = Base::GetRecordForDisplay();
  Element * parent = GetParent();
  if (parent) ret["parent"] = parent->ShortDescription();
  ret["function"] = FunctionToString(GetFunction());
  ret["description"] = ShortDescription();
  ret["program"] = "<pre>" + ProgramTree() + "</pre>";

  for (uint i=0; i<children_.size(); i++) {
    if (i>0) ret["children"] += "<br>\n";
    ret["children"] += children_[i]->ShortDescription();
  }

  return ret;
}

string Element::ProgramTree(int indent) const {
  string ret = GetLink(FunctionKeyword().ToString());
  if (GetFunction() == Element::MAKETUPLE) {
    ret="";
  }
  if (ChildrenGoInTuple()) {
    if (GetFunction() == Element::MAKETUPLE) ret += GetLink("(");
    else ret += " (";
  } else {
    ret += " ";
  }
  bool separate_line = false;
  for (int i=0; i<NumChildren(); i++) {
    int child_indent = indent;
    Element *child = GetChild(i);
    separate_line = (ChildNeedsSeparateLine(i) 
		     || (child && child->ElementNeedsSeparateLine()));
    if (separate_line) {      
      if (child && child->GetFunction() == MAKETUPLE){
      } else {
	child_indent = indent + 2;
	ret += "\n" + string(child_indent, ' ');
      }
    } else {
      if (i>0) ret += " ";
    }    
    if (child) ret += child->ProgramTree(child_indent);
    else ret += "null";
  }
  if (ChildrenGoInTuple()) {
    if (separate_line) ret += "\n" + string(indent, ' ');
    if (GetFunction() == Element::MAKETUPLE) ret += GetLink(")");
    else ret += ")";
  }
  return ret;
}

Object On::Execute(Thread t) {
  // Get the tuple child
  Object tuple_child = GetChild(TUPLE)->Execute(t);
  if ( (tuple_child.GetType() != Object::OTUPLE) ||
       (!IsVariableTuple(OTuple(tuple_child).Data())) ) {
    cerr << "Tuple child of On not an OTUPLE: " << tuple_child << endl;
    return NULL;
  }

  // Make a new on subscription
  t.element_ = GetChild(CHILD);
  New<OnSubscription>(t, tuple_child);
  return NULL;
}

Object Post::Execute(Thread t) {
  // Get the tuple child
  Object tuple_child = GetChild(TUPLE)->Execute(t);
  if ( (tuple_child.GetType() != Object::OTUPLE) ) {
    cerr << "Tuple child of Post not an OTUPLE: " << tuple_child << endl;
    return NULL;
  }

  t.execution_->AddPost(tuple_child);
  return tuple_child;
}

string Constant::ProgramTree(int indent) const {
  bool can_be_concise = true;
  Object o = object_;
  if (o.GetType() == Object::OTUPLE)  can_be_concise = false;
  if (o.GetType() == Object::VARIABLE) can_be_concise = false;
  if (o.GetType() == Object::KEYWORD) {
    if (Element::TypeKeywordToFunction(Keyword(o)) != -1)
      can_be_concise = false;
  }
  if (!can_be_concise) return Element::ProgramTree(indent);
  string ret = HTMLEscape(o.ToString());
  return GetLink(ret);
}

string Substitute::ProgramTree(int indent) const {
  Element *child = GetChild(CHILD);
  if (child) {
    Constant *constant = dynamic_cast<Constant *>(child);
    if (constant) {
      Object o = constant->object_;
      if (o.GetType() == Object::VARIABLE) { 
	string ret = HTMLEscape(o.ToString());
	return GetLink(ret);
      }
    }
  }
  return Element::ProgramTree();
}
