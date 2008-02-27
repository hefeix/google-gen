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


Object Element::Execute(Thread & thread) {
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
  ret["program"] = "<pre>" + PrettyProgramTree() + "</pre>";
  ret["simple_program"] = SimpleProgramTree().ToString();
  ret["incoming_variables"] = OTuple::Make(incoming_variables_).ToString();
  ret["object"] = object_.ToString();
  
  for (uint i=0; i<children_.size(); i++) {
    if (i>0) ret["children"] += "<br>\n";
    ret["children"] += children_[i]->ShortDescription();
  }

  return ret;
}

OTuple Element::SimpleProgramTree() const { 
  Tuple t;
  t.push_back(FunctionKeyword());
  t.push_back(HasObject()?object_:Object(NULL));
  Tuple children;
  for (uint i=0; i<children_.size(); i++) {
    children.push_back(children_[i]->SimpleProgramTree());
  }
  t.push_back(OTuple::Make(children));
  return OTuple::Make(t);
}

string Element::PrettyProgramTree(int indent) const {
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
    if (child) ret += child->PrettyProgramTree(child_indent);
    else ret += "null";
  }
  if (ChildrenGoInTuple()) {
    if (separate_line) ret += "\n" + string(indent, ' ');
    if (GetFunction() == Element::MAKETUPLE) ret += GetLink(")");
    else ret += ")";
  }
  return ret;
}

Object MatchBaseElement::Execute(Thread & thread) {
  // Figure out the tuple we are searching for
  Tuple match_tuple = wildcard_tuple_;
  int child_num = 0;
  for (uint pos=0; pos<wildcard_tuple_.size(); pos++) {
    if (wildcard_tuple_[pos] == NULL) {
      match_tuple[pos] = GetChild(child_num++)->Execute(thread);
      // this is to avoid introducing wildcards during execution. 
      // we might want to unescape on the other side.
      if (match_tuple[pos] == WILDCARD)
	match_tuple[pos] = Escape::Make(match_tuple[pos]);
    }
  }
  // do something subclass-specific
  return SubclassExecute(thread, match_tuple);
}

Object MatchBaseElement::RunForMatchingTuple(Thread &thread, 
					     Blackboard::Row *row,
					     int tuple_num) {
  row->CopyBinding(tuple_num, &thread.stack_, incoming_stack_depth_);
  return GetExtraChild()->Execute(thread);
}
Object OnElement::SubclassExecute(Thread & thread, 
				  const Tuple & wildcard_tuple) {
  Blackboard::Row *row 
    = thread.execution_->blackboard_->GetCreateRow(wildcard_tuple);
  for (int i=0; i<row->NumTuples(); i++) RunForMatchingTuple(thread, row, i);
  thread.element_ = GetExtraChild();
  New<OnSubscription>(thread, row);  
  return NULL;
}

Object MatchElement::SubclassExecute(Thread & thread, 
				     const Tuple & wildcard_tuple) {
  Blackboard::Row *row = thread.execution_->blackboard_->GetRow(wildcard_tuple);
  if (row == NULL) return OTuple::Default();
  Tuple ret;
  for (int i=0; i<row->NumTuples(); i++) 
    ret.push_back(RunForMatchingTuple(thread, row, i));
  return OTuple::Make(ret);
}

Object MatchRandomElement::SubclassExecute(Thread & thread, 
					   const Tuple & wildcard_tuple) {
  Blackboard::Row *row = thread.execution_->blackboard_->GetRow(wildcard_tuple);
  if (!row) return NULL;
  int num_tuples = row->NumTuples();
  if (num_tuples == 0) return NULL;
  int pos = RandomUInt32() % num_tuples;
  return RunForMatchingTuple(thread, row, pos);
}

Object MatchLastElement::SubclassExecute(Thread & thread, 
					 const Tuple & wildcard_tuple) {
  Blackboard::Row *row = thread.execution_->blackboard_->GetRow(wildcard_tuple);
  if (!row) return NULL;
  int num_tuples = row->NumTuples();
  if (num_tuples == 0) return NULL;
  int pos = num_tuples-1;
  return RunForMatchingTuple(thread, row, pos);
}

Object MatchCountElement::SubclassExecute(Thread & thread,
					  const Tuple & wildcard_tuple) {
  Blackboard::Row *row = thread.execution_->blackboard_->GetRow(wildcard_tuple);
  if (!row) return Integer::Make(0);
  int num_tuples = row->NumTuples();
  return Integer::Make(num_tuples);
}


Object LetElement::Execute(Thread & thread) {
  // Get the tuple child
  Object value = GetChild(VALUE)->Execute(thread);
  if ((int)thread.stack_.size() < incoming_stack_depth_ + 1)
    thread.stack_.resize(incoming_stack_depth_ + 1);
  thread.stack_[incoming_stack_depth_] = value;
  return GetChild(CHILD)->Execute(thread);
}

Object DelayElement::Execute(Thread & thread) {
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

Object PostElement::Execute(Thread & t) {
  // Get the tuple child
  Object tuple_child = GetChild(TUPLE)->Execute(t);
  if ( (tuple_child.GetType() != Object::OTUPLE) ) {
    cerr << "Tuple child of PostElement not an OTUPLE: " << tuple_child << endl;
    return NULL;
  }

  t.execution_->AddPost(OTuple(tuple_child).Data());
  return tuple_child;
}

string ConstantElement::PrettyProgramTree(int indent) const {
  bool can_be_concise = true;
  Object o = object_;
  if (o.GetType() == Object::OTUPLE)  can_be_concise = false;
  if (o.GetType() == Object::VARIABLE) can_be_concise = false;
  if (o.GetType() == Object::KEYWORD) {
    if (Element::TypeKeywordToFunction(Keyword(o)) != -1)
      can_be_concise = false;
  }
  if (!can_be_concise) return Element::PrettyProgramTree(indent);
  string ret = HTMLEscape(o.ToString());
  return GetLink(ret);
}

string SubstituteElement::PrettyProgramTree(int indent) const {
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
  return Element::PrettyProgramTree();
}

void Element::StaticInit() {
  for (int i=0; i<NumFunctions(); i++) 
    Object::AddKeyword(Downcase(FunctionToString(Function(i))));
}

Object IfElement::Execute(Thread & thread) {
  Object condition = GetChild(CONDITION)->Execute(thread);
  // Everything other than FALSE is true for this purpose
  if (condition == FALSE) {
    Element * on_false = GetChild(ON_FALSE);
    return on_false->Execute(thread);
  }
  return GetChild(ON_TRUE)->Execute(thread);
}
