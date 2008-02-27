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
#include "execution.h"
#include "webserver.h"

#undef ITEM
#define ITEM(x) #x

#define FUNCTION(func, FUNC) ITEM(FUNC),
CLASS_ENUM_DEFINE(Element, Function);
#undef FUNCTION

#define FUNCTION(f, F) CLASS_ENUM_DEFINE(f##Element, ChildName);
ALL_FUNCTIONS
#undef FUNCTION


Object Element::Execute(Thread thread) {
  Tuple results;
  for (uint c=0; c<children_.size(); c++) {
    Object return_val = children_[c]->Execute(thread);
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

Object MatchBaseElement::Execute(Thread thread) {
  // Get the tuple child
  Object variable_tuple = GetChild(TUPLE)->Execute(thread);
  if ( (variable_tuple.GetType() != Object::OTUPLE) ||
       (!IsVariableTuple(OTuple(variable_tuple).Data())) ) {
    cerr << "Tuple child  not an OTUPLE: " 
	 << variable_tuple << endl;
    return NULL;
  }

  // Point the thread to the next executable element
  thread.element_ = GetChild(CHILD);

  return SubclassExecute(thread, variable_tuple);
}

Object OnElement::SubclassExecute(Thread thread, Object variable_tuple) {
  // Run over all existing things
  Execution::MatchAndRun(thread, OTuple(variable_tuple).Data());
  // Make a new on subscription
  New<OnSubscription>(thread, OTuple(variable_tuple).Data());
  return NULL;
}

Object MatchElement::SubclassExecute(Thread thread, Object variable_tuple) {
  // Run over all existing things
  return OTuple::Make
    (Execution::MatchAndRun(thread, OTuple(variable_tuple).Data()));
}

Object MatchRandomElement::SubclassExecute(Thread thread, 
					   Object variable_tuple) {
  // may want to make this work later.
  if (HasDuplicateVariables(variable_tuple)) return NULL;

  Blackboard::Row * row = thread.execution_->blackboard_->GetRow
    (VariablesToWildcards(variable_tuple));
  if (!row) return NULL;

  int num_tuples = row->GetNumTuples();
  if (num_tuples == 0) return NULL;
  int pos = RandomUint32() % num_tuples;
  Tuple constant_tuple;
  row->GetTuple(pos, &constant_tuple);
  CHECK(ExtendSubstitution(variable_tuple, constant_tuple, &thread.binding_));

  return GetChild(CHILD)->Execute(thread);
}

Object MatchLastElement::SubclassExecute(Thread thread, 
					   Object variable_tuple) {
  // may want to make this work later.
  if (HasDuplicateVariables(variable_tuple)) return NULL;

  Blackboard::Row * row = thread.execution_->blackboard_->GetRow
    (VariablesToWildcards(variable_tuple));
  if (!row) return NULL;

  int num_tuples = row->GetNumTuples();
  if (num_tuples == 0) return NULL;
  int pos = num_tuples-1;
  Tuple constant_tuple;
  row->GetTuple(pos, &constant_tuple);
  CHECK(ExtendSubstitution(variable_tuple, constant_tuple, &thread.binding_));

  return GetChild(CHILD)->Execute(thread);
}

Object MatchCountElement::Execute(Thread thread) {
  // Get the tuple child
  Object variable_tuple = GetChild(TUPLE)->Execute(thread);
  if ( (variable_tuple.GetType() != Object::OTUPLE) ||
       (!IsVariableTuple(OTuple(variable_tuple).Data())) ) {
    cerr << "Tuple child not an OTUPLE: " << variable_tuple << endl;
    return NULL;
  }

  Blackboard::Row * row = thread.execution_->blackboard_->GetRow
    (VariablesToWildcards(variable_tuple));
  if (!row) return Integer::Make(0);
  return Integer::Make(row->GetNumTuples());
}


Object LetElement::Execute(Thread thread) {
  // Get the tuple child
  Object variable = GetChild(VARIABLE)->Execute(thread);
  Object value = GetChild(VALUE)->Execute(thread);
  if (variable.GetType() == Object::VARIABLE) {
    AddChangeValue(&thread.binding_, variable, value);
  }
  return GetChild(CHILD)->Execute(thread);
}

Object DelayElement::Execute(Thread thread) {
  // Get the tuple child
  Element * delay_child = GetChild(DIMENSION);
  Object delay = delay_child->Execute(thread);
  if (delay.GetType() != Object::OBITSEQ) {
    delay = OBitSeq::Default();
  }
  thread.element_ = GetChild(CHILD);  
  thread.execution_->Enqueue(thread, OBitSeq(delay).Data());
  return NULL;
}

Object PostElement::Execute(Thread t) {
  // Get the tuple child
  Object tuple_child = GetChild(TUPLE)->Execute(t);
  if ( (tuple_child.GetType() != Object::OTUPLE) ) {
    cerr << "Tuple child of PostElement not an OTUPLE: " << tuple_child << endl;
    return NULL;
  }

  t.execution_->AddPost(OTuple(tuple_child).Data());
  return tuple_child;
}

string ConstantElement::ProgramTree(int indent) const {
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

string SubstituteElement::ProgramTree(int indent) const {
  Element *child = GetChild(CHILD);
  if (child) {
    ConstantElement *constant = dynamic_cast<ConstantElement *>(child);
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

void Element::StaticInit() {
  for (int i=0; i<NumFunctions(); i++) 
    Object::AddKeyword(Downcase(FunctionToString(Function(i))));
}

Object IfElement::Execute(Thread thread) {
  Object condition = GetChild(CONDITION)->Execute(thread);
  // Everything other than FALSE is true for this purpose
  if (condition == FALSE) {
    Element * on_false = GetChild(ON_FALSE);
    return on_false->Execute(thread);
  }
  return GetChild(ON_TRUE)->Execute(thread);
}
