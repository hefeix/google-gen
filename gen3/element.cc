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

CLASS_ENUM_DEFINE(ChooseElement, DistributionType);


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
    ret["children"] += " introduced variables = " 
      + ToString(GetIntroducedVariables(i))
      + " outgoing variables = " 
      + ToString(GetOutgoingVariables(i));
  }
  if (!VerifyNode()) ret["verify"] = "BAD";

  return ret;
}

OTuple Element::SimpleProgramTree() const { 
  Tuple t;
  t.push_back(FunctionKeyword());
  t.push_back(HasObject()?object_:Object(NULL));
  for (uint i=0; i<children_.size(); i++) {
    t.push_back(children_[i]->SimpleProgramTree());
  }
  return OTuple::Make(t);
}

string Element::PrettyProgramTree(int indent) const {
  string ret = GetLink(FunctionKeyword().ToString());
  if (HasObject()) ret += " " +  HTMLEscape(object_.ToString());
  if (GetFunction() == Element::MAKETUPLE) ret="";
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
    bool add_else = false;
    if (GetFunction() == Element::IF && i == IfElement::ON_FALSE) {
      if (child && child->GetFunction() == Element::PASS) {
	continue;
      } else {
	add_else = true;
      }
    }
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
    if (add_else) ret += "else ";
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

string MatchBaseElement::PrettyProgramTree(int indent) const { 
  string ret = GetLink(FunctionKeyword().ToString());
  ret += " ( ";
  int child_num = 0;
  for (uint i=0; i<wildcard_tuple_.size(); i++) {
    if (wildcard_tuple_[i] == NULL) {
      Element * child = GetChild(child_num++);
      if (child == NULL) ret += "null";
      else ret += child->PrettyProgramTree(indent+2);
    } else {
      CHECK(wildcard_tuple_[i] == WILDCARD);
      ret += OTuple(object_).Data()[i].ToString();
    }
    ret += " ";
  }
  ret += ") ";    
  if (HasExtraChild()) {
    ret += "\n" + string(indent+2, ' ') 
      + GetExtraChild()->PrettyProgramTree(indent+2);
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

Object RepeatElement::Execute(Thread & thread) {
  Object n_obj = GetChild(N)->Execute(thread);
  if (n_obj.GetType() != Object::INTEGER) return NULL;
  int n = Integer(n_obj).Data();
  if (n<0) return NULL; 
  Tuple ret;
  if ((int)thread.stack_.size() < incoming_stack_depth_ + 1)
    thread.stack_.resize(incoming_stack_depth_ + 1);
  for (int value=0; value<n; value++) {
    thread.stack_[incoming_stack_depth_] = Integer::Make(value);
    ret.push_back(GetChild(CHILD)->Execute(thread));
  }
  return OTuple::Make(ret);
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

Object UnpostElement::Execute(Thread & t) {
  // Get the tuple child
  Object tuple_child = GetChild(TUPLE)->Execute(t);
  if ( (tuple_child.GetType() != Object::OTUPLE) ) {
    cerr << "Tuple child of UnpostElement not an OTUPLE: " 
	 << tuple_child << endl;
    return NULL;
  }
  t.execution_->AddUnpost(OTuple(tuple_child).Data());
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
  Object o = object_;
  string ret = HTMLEscape(o.ToString());
  return GetLink(ret);
}

void Element::StaticInit() {
  for (int i=0; i<NumFunctions(); i++) 
    AddKeyword(Downcase(FunctionToString(Function(i))));
  ChooseElement::InitDistributionTypeKeywords();
}

Object IfElement::Execute(Thread & thread) {
  Object condition = GetChild(CONDITION)->Execute(thread);
  // Everything other than FALSE is true for this purpose
  if (ToBoolean(condition)) 
    return GetChild(ON_TRUE)->Execute(thread);
  return GetChild(ON_FALSE)->Execute(thread);
}

void ChooseElement::InitDistributionTypeKeywords() {
  CHECK(distribution_type_keywords_.size() == 0);
  for (int i=0; i<NumDistributionTypes(); i++) 
    distribution_type_keywords_.push_back
      (AddKeyword
       (Downcase(DistributionTypeToString(DistributionType(i)))));  
}

int g_new_flake_counter = 0;

// Makes a choice given a distribution.
// returns choice and likelihood therof. 
// If suggestion is non-null, forces the choice to be *suggestion.
// if the suggestion is impossible, the returned likelihood will be 0.0

pair<Object, double> ChooseElement::Choose(Execution *execution, 
					   Object distribution, 
					   const Object *suggestion) {
  Object ret;
  if (suggestion) ret = *suggestion;
  
  if (distribution.GetType() != Object::OTUPLE) goto one_element;
  {
    const Tuple & d = OTuple(distribution).Data();
    // todo: cerr on error.
    if ( (d.size() <= 0) ||
	 (d[0].GetType() != Object::KEYWORD) ) goto one_element;
    
    DistributionType distribution_type 
      = KeywordToDistributionType(d[0]);
    
    switch(distribution_type) {
    case ONE_ELEMENT: {
      if (d.size() <= 1) goto one_element;
      distribution = d[1];
      goto one_element;
    }
    case NEW_FLAKE: {
      Flake ret;
      if (suggestion) {
	if (suggestion->GetType() != Object::FLAKE || 
	    (execution->existing_flakes_ % Flake(*suggestion)) )
	  return make_pair(*suggestion, 0);
	ret = *suggestion;
      } else {
	do {
	  ret = Flake::Make("FLAKE_"+itoa(g_new_flake_counter++));
	} while (execution->existing_flakes_ % ret);
      }
      execution->existing_flakes_.insert(ret);
      // TODO: may want to post something to the guide (or main) blackboard. 
      /*execution->AddPost
	(MakeTuple(EXISTING_FLAKE, ret, Real::Make(1.0), NULL));*/
      return make_pair(ret, 1.0);
    }
    case ANY_FLAKE : {
      Flake ret;
      if (suggestion) {
	if (execution->existing_flakes_ % Flake(*suggestion) ) {
	  return make_pair(*suggestion, 
			   0.5 / execution->existing_flakes_.size());
	} else {
	  return make_pair(*suggestion, 0.5);
	}
      }
      if (execution->existing_flakes_.size() == 0 || RandomFraction() < 0.5) {
	// make a new flake
	do {
	  ret = Flake::Make("FLAKE_"+itoa(g_new_flake_counter++));
	} while (execution->existing_flakes_ % ret);
	execution->existing_flakes_.insert(ret);
	return make_pair(ret, 0.5);
      } else {
	// make an existing flake
	ret = *execution->existing_flakes_.nth
	  (RandomUInt64() % execution->existing_flakes_.size());
	return make_pair(ret, 0.5 / execution->existing_flakes_.size());
      }
    }
    case BOOL: {
      if (d.size() <= 1) goto one_element;
      if (d[1].GetType() != Object::REAL) goto one_element;
      double prior = Real(d[1]).Data();
      if (!(prior >= 0 && prior <= 1)) goto one_element;
      
      bool ret = (RandomFraction() < prior);
      if (suggestion) {
	if (suggestion->GetType() == Object::BOOLEAN) {
	  ret = Boolean(*suggestion).Data();
	} else {
	  return make_pair(*suggestion, 0);
	}
      }
      return make_pair(Boolean::Make(ret), ret?prior:(1.0-prior));
    }
    case QUADRATIC_UINT: {
      int ret = RandomUintQuadratic();
      if (suggestion) {
	if (suggestion->GetType() != Object::INTEGER)
	  return make_pair(*suggestion, 0);
	int n = Integer(*suggestion).Data();
	if (n<0) return make_pair(*suggestion, 0);
	ret = n;
      }
      return make_pair(Integer::Make(ret), uintQuadraticProb(ret));
    }
    case BLACKBOARD: {
      if (d.size() <= 1) goto one_element;
      Object identifier = d[1];
      const Distribution & dist = 
	*(execution->blackboard_->GetCreateDistribution(identifier));
      int64 total_weight = dist.TotalWeight();
      if (total_weight == 0) goto one_element;
      if (suggestion) {
	Distribution::const_iterator look = dist.find(*suggestion);
	if (look == dist.end()) return make_pair(*suggestion, 0);      
	return make_pair(*suggestion, double(look->second) / total_weight);
      }
      int64 r = RandomUInt64() % (uint64)total_weight;
      Distribution::const_iterator look = dist.find_by_weight(r);
      return make_pair(look->first, double(look->second) / total_weight);
    }
    default: break;
    }
  }
  // If we get here then it's a one-element distribution, which means
  // the distribution that returns the object which is the value of 
  // the variable "distribution" with probability 1.0
  one_element:
    if (suggestion) {
    if (*suggestion != distribution) return make_pair(*suggestion, 0);
  }
  return make_pair(distribution, 1.0);
}



vector<Keyword> ChooseElement::distribution_type_keywords_;

Object ChooseElement::ComputeReturnValue(Thread & thread, Tuple results) {
  Object distribution = results[DISTRIBUTION];
  Object name = object_;

  pair<Object, double> guide_choice = make_pair(Object(NULL), 1.0);

  Execution *guide = thread.execution_->guide_;
  Object guide_distribution = NULL;
  if (guide) {
    // post the choice on the guide blackboard
    Integer instance = Integer::Make(thread.execution_->choice_counter_++);
    
    if (distribution.GetType() == Object::OTUPLE) {
      const Tuple & distribution_tuple = OTuple(distribution).Data();
      // post [ NEED_CHOICE {unrolled distribution tuple} <instance> ]
      Tuple request;
      request.push_back(NEED_CHOICE);
      request.insert(request.end(), distribution_tuple.begin(), 
		     distribution_tuple.end());
      request.push_back(instance);
      guide->AddPost(request);
      guide->ExecuteForever();

      Tuple response = MakeTuple(CHOICE, instance, WILDCARD);
      Blackboard::Row *row = guide->blackboard_->GetCreateRow(response);
      if (row->NumTuples() == 1) {
	guide_distribution = row->GetTuple(0)[3];

	pair<Object, double> guide_choice =
	  Choose(guide, guide_distribution, NULL);
	
	pair<Object, double> main_choice = 
	  Choose(thread.execution_, 
		 distribution, &guide_choice.first);
	
	// If the guide choice is impossible, we just fall through and 
	// select unguided.
	if (main_choice.second != 0) {
	  thread.execution_->total_bias_ 
	    += log(main_choice.second) - log(guide_choice.second);
	  return main_choice.first;
	}
      }
    }
  }
  pair<Object, LL> choice 
    = Choose(thread.execution_, distribution, NULL);
  return choice.first;
}
