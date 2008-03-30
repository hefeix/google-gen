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

struct Choice {
  // What is chosen
  Object value_;
  // What was its probability or density of value_ being chosen
  double ln_prob_;
  // number of dimensions of the density function
  int dimension_;
  // the distribution keyword was recognized, but distribution is malformed
  bool bad_distribution_;
  // the suggestion is impossible according to the distribution.
  bool impossible_suggestion_;

  void clear() {
  value_ = NULL;
  ln_prob_ = 0.0;
  dimension_ = 0;
  bad_distribution_ = false;
  impossible_suggestion_ = false;  }

  string ToString() {
    ostringstream ostr;
    ostr <<  "choice [value_=" <<  value_ 
	 <<" ln_prob=" << ln_prob_
	 << " dimension_=" << dimension_
	 << " bad_distribution_=" << (bad_distribution_?'T':'F')
	 << " impossible_suggestion_=" << (impossible_suggestion_?'T':'F')
	 << endl;
    return ostr.str();
  }
};

void ChooseElement::ChooseHelper(Choice *choice,
				 Execution *execution, 
				 Object distribution, 
				 const Object *suggestion) {
  choice->clear();
  if (suggestion) choice->value_ = *suggestion;
  
#define BAD_DISTRIBUTION {choice->bad_distribution_ = true; cerr << "bad distribution " << d << " " << __LINE__ << endl; return;}
#define IMPOSSIBLE_SUGGESTION {choice->impossible_suggestion_ = true; /*cerr << "impossible_suggestion_ " << __LINE__ << endl; */return;}


  if (distribution.GetType() != Object::OTUPLE)
    distribution = OTuple::Make
      (MakeTuple(DistributionTypeToKeyword(ONE_ELEMENT), distribution));
  
  Tuple d = OTuple(distribution).Data();
  
  if ( (d.size() <= 0) || (d[0].GetType() != Object::KEYWORD) ) 
    d = MakeTuple(DistributionTypeToKeyword(ONE_ELEMENT), distribution);
  
  DistributionType distribution_type = KeywordToDistributionType(d[0]);
  if (distribution_type == -1) {
    d = (MakeTuple(DistributionTypeToKeyword(ONE_ELEMENT), distribution));
    distribution_type = ONE_ELEMENT;
  }

  
  switch(distribution_type) {
  case ONE_ELEMENT: 
    if (d.size() <= 1) BAD_DISTRIBUTION;
    choice->value_ = d[1];
    if (suggestion && (*suggestion != choice->value_)) IMPOSSIBLE_SUGGESTION;
    return;
  case NEW_FLAKE:{
    if (suggestion) {
      if (suggestion->GetType() != Object::FLAKE || 
	  (execution->existing_flakes_ % Flake(*suggestion)) )
	IMPOSSIBLE_SUGGESTION;
      return;
    }
    Flake ret;
    do {
      ret = Flake::Make("FLAKE_"+itoa(g_new_flake_counter++));
    } while (execution->existing_flakes_ % ret);
    execution->existing_flakes_.insert(ret);
    choice->value_ = ret;
    // TODO: may want to post something to the guide (or main) blackboard. 
    /*execution->AddPost
      (MakeTuple(EXISTING_FLAKE, ret, Real::Make(1.0), NULL));*/
    return;
  }
  case ANY_FLAKE : {
    if (suggestion) {
      if (suggestion->GetType() != Object::FLAKE) IMPOSSIBLE_SUGGESTION;
      if (execution->existing_flakes_ % Flake(*suggestion) ) {
	choice->ln_prob_ = log(0.5 / execution->existing_flakes_.size());
	return;
      }
      choice->ln_prob_ = log(0.5);
      return;
    }
    if (execution->existing_flakes_.size() == 0 || RandomFraction() < 0.5) {
      // make a new flake
      Flake ret;
      do {
	ret = Flake::Make("FLAKE_"+itoa(g_new_flake_counter++));
      } while (execution->existing_flakes_ % ret);
      execution->existing_flakes_.insert(choice->value_);
      choice->value_ = ret;
      choice->ln_prob_ = log(0.5);
      return;
    }
    // make an existing flake
    choice->value_ = *execution->existing_flakes_.nth
      (RandomUInt64() % execution->existing_flakes_.size());
    choice->ln_prob_ = log(0.5 / execution->existing_flakes_.size());
    return;
  }
  case BOOL:
    if (d.size() <= 1) BAD_DISTRIBUTION;
    if (d[1].GetType() != Object::REAL) BAD_DISTRIBUTION;
    double prior = Real(d[1]).Data();
    if (!(prior >= 0 && prior <= 1)) BAD_DISTRIBUTION;      
    bool bool_val = (RandomFraction() < prior);
    if (suggestion) {
      if (suggestion->GetType() == Object::BOOLEAN)
	bool_val = Boolean(*suggestion).Data();
      else IMPOSSIBLE_SUGGESTION;
    }
    choice->value_ = Boolean::Make(bool_val);
    choice->ln_prob_ = log(bool_val?prior:(1.0-prior));
    return;
  case NORMAL: {
    if (d[1].GetType() != Object::REAL) BAD_DISTRIBUTION;
    if (d[2].GetType() != Object::REAL) BAD_DISTRIBUTION;
    double mean = Real(d[1]).Data();
    double std = Real(d[2]).Data();
    if (!finite(std) || !finite(mean) || (std <= 0)) BAD_DISTRIBUTION;
    double ret = RandomNormal() * std + mean;
    if (suggestion) {
      if (suggestion->GetType() != Object::REAL) IMPOSSIBLE_SUGGESTION;
      ret = Real(*suggestion).Data();
      if (!finite(ret)) IMPOSSIBLE_SUGGESTION;
    }
    choice->value_ = Real::Make(ret);
    choice->ln_prob_ = NormalLnDensity(mean, std, ret);
    choice->dimension_ = 1;
    return;
  }
  case UNIFORM: {
    if (d[1].GetType() != Object::REAL) BAD_DISTRIBUTION;
    if (d[2].GetType() != Object::REAL) BAD_DISTRIBUTION;
    double minimum = Real(d[1]).Data();
    double maximum = Real(d[2]).Data();
    if (!(finite(minimum) && finite(maximum) && (minimum < maximum)))
      BAD_DISTRIBUTION;
    double ret = RandomUniform(minimum, maximum);
    if (suggestion) {
      if (suggestion->GetType() != Object::REAL) IMPOSSIBLE_SUGGESTION;
      ret = Real(*suggestion).Data();
      if (!(minimum <= ret && ret < maximum) ) IMPOSSIBLE_SUGGESTION;
    }
    choice->value_ = Real::Make(ret);
    choice->ln_prob_ = UniformLnDensity(minimum, maximum, ret);
    choice->dimension_ = 1;
    return;
  }
  case UNIFORM_DISCRETE: {
    if (d[1].GetType() != Object::INTEGER) BAD_DISTRIBUTION;
    if (d[2].GetType() != Object::INTEGER) BAD_DISTRIBUTION;
    int64 minimum = Integer(d[1]).Data();
    int64 maximum = Integer(d[2]).Data();
    if (!(minimum < maximum)) BAD_DISTRIBUTION;
    int64 ret = RandomUniformDiscrete(minimum, maximum);
    if (suggestion) {
      if (suggestion->GetType() != Object::INTEGER) IMPOSSIBLE_SUGGESTION;
      ret = Integer(*suggestion).Data();
      if (!(minimum <= ret && ret < maximum) ) IMPOSSIBLE_SUGGESTION;
    }
    choice->value_ = Integer::Make(ret);
    choice->ln_prob_ = UniformDiscreteLnProb(minimum, maximum, ret);
    return;
  }
  case EXPONENTIAL: {
    if (d[1].GetType() != Object::REAL) BAD_DISTRIBUTION;
    double lambda = 1.0/Real(d[1]).Data();
    if (!(lambda > 0)) BAD_DISTRIBUTION;
    double ret = RandomExponential(lambda);
    if (suggestion) {
      if (suggestion->GetType() != Object::REAL) IMPOSSIBLE_SUGGESTION;
      ret = Real(*suggestion).Data();
      if (!(ret >= 0)) IMPOSSIBLE_SUGGESTION;
    }
    choice->value_ = Real::Make(ret);
    choice->ln_prob_ = ExponentialLnDensity(lambda, ret);
    choice->dimension_ = 1;
    return;
  }
  case GEOMETRIC: {
    if (d[1].GetType() != Object::REAL) BAD_DISTRIBUTION;
    double lambda = 1.0/Real(d[1]).Data();
    if (!(lambda > 0)) BAD_DISTRIBUTION;
    int64 ret = RandomGeometric(lambda);
    if (suggestion) {
      if (suggestion->GetType() != Object::INTEGER) IMPOSSIBLE_SUGGESTION;
      ret = Integer(*suggestion).Data();
      if (!(ret >= 0)) IMPOSSIBLE_SUGGESTION;
    }
    choice->value_ = Integer::Make(ret);
    choice->ln_prob_ = GeometricLnProb(lambda, ret);
    return;
  }
    
  case BLACKBOARD: {
    if (d.size() <= 1) BAD_DISTRIBUTION;
    Object identifier = d[1];
    const Distribution & dist = 
      *(execution->blackboard_->GetCreateDistribution(identifier));
    int64 total_weight = dist.TotalWeight();
    if (total_weight == 0) BAD_DISTRIBUTION;
    if (suggestion) {
      Distribution::const_iterator look = dist.find(*suggestion);
      if (look == dist.end()) IMPOSSIBLE_SUGGESTION;
      choice->ln_prob_ = log(double(look->second) / total_weight);
      return;
    }
    int64 r = RandomUInt64() % (uint64)total_weight;
    Distribution::const_iterator look = dist.find_by_weight(r);
    choice->value_ = look->first;
    choice->ln_prob_ = log(double(look->second) / total_weight);
    return;
  }
  default: CHECK(false);
  }
  CHECK(false);
}

vector<Keyword> ChooseElement::distribution_type_keywords_;

Object ChooseElement::ComputeReturnValue(Thread & thread, Tuple results) {
  Object distribution = results[DISTRIBUTION];
  Object name = object_;

  Execution *guide = thread.execution_->guide_;
  Object guide_distribution = NULL;

  // We break out if the guide fails.
  for (int retarded = 0; retarded<1; retarded++) {
    if (!guide) break;
    // post the choice on the guide blackboard
    Integer instance = Integer::Make(thread.execution_->choice_counter_++);
    if (distribution.GetType() != Object::OTUPLE) break;
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
    if (row->NumTuples() != 1) break;
    guide_distribution = row->GetTuple(0)[2];
    
    Choice guide_choice;
    ChooseHelper(&guide_choice, guide, guide_distribution, NULL);
    if (guide_choice.bad_distribution_) break;
    Choice main_choice;
    ChooseHelper(&main_choice, thread.execution_, 
		 distribution, &guide_choice.value_);
    
    // If the guide choice is impossible, we just fall through and 
    // select unguided.
    if (main_choice.impossible_suggestion_ || 
	!finite(main_choice.ln_prob_) || 
      (main_choice.dimension_ != guide_choice.dimension_)) break;
    thread.execution_->total_bias_ 
    += main_choice.ln_prob_ - guide_choice.ln_prob_;
    return main_choice.value_;
  }
  Choice choice;
  ChooseHelper(&choice, thread.execution_, distribution, NULL);
  if (choice.bad_distribution_) return distribution;
  //cerr << "Chose with no guide distribution=" << distribution 
  //     << " choice=" << choice.ToString()
  //    << endl;
  return choice.value_;
}
