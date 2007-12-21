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

void Element::SetTime(OTime new_time) {
  if (time_ == new_time) return;
  CL.ChangeValue(&time_, new_time);
  N1_StoredTimeChanged();
  set<Element *> dep = GetAllChildren();
  forall(run, dep) {
    (*run)->N1_ComputedTimeChanged();
  }  
}

void Element::L1_Erase() {
  CHECK(!parent_); // We always unlink elements before we erase.
  Base::L1_Erase();
}

// The following are called externally when a child is connected to
// or disconnected from this parent node. 
void Element::N1_ChildChanged(int which_child) {
  L1_CheckSetChildViolation();
}

void Element::L1_CheckSetTimeViolation() {
  if (IsErased()) return;
  if (M.batch_mode_) {
    M.L1_AddDelayedCheck(this, &Element::L1_CheckSetTimeViolation);
    return;
  }
  OTime proper_time = ComputeTime();
  Violation * violation = Violation::Search(this, Violation::TIME, NULL);
  if (proper_time != time_) {
    if (!violation) {
      New<TimeViolation>(this);
    } else {
      violation->N1_ComputedTimeChanged();
    }
  } else {
    if (violation) violation->L1_Erase();
  }
}

void Element::L1_CheckSetChildViolation() {
  if (IsErased()) return;
  if (M.batch_mode_) {
    M.L1_AddDelayedCheck(this, &Element::L1_CheckSetChildViolation);
    return;
  }
  if (NeedsChildViolation()) ChildViolation::L1_CreateIfAbsent(this);
  else ChildViolation::L1_RemoveIfPresent(this);
}

void StaticElement::UnlinkFromParent() {
  CHECK(parent_);
  // First unlink the dynamic nodes.
  forall(run, dynamic_children_->children_) {
    run->second->UnlinkFromParent();
  }
  int which_child_was_i = WhichChildAmI();
  StaticElement *parent = GetParent();
  parent_->L1_RemoveChild(this);
  if (GetFunction() != ON) StaticNoParentViolation::L1_CreateIfAbsent(this);
  N1_ComputedTimeChanged();
  parent->N1_ChildChanged(which_child_was_i);
}

void StaticElement::LinkToParent(StaticElement *new_parent, int which_child) {
  CHECK(!parent_);
  while(new_parent->NumChildren() <= which_child) {
    CHECK(new_parent->HasVariableNumChildren());    
    new_parent->static_children_.push_back(New<SingleLink>(new_parent));
  }
  new_parent->static_children_[which_child]->L1_AddChild(this);
  StaticNoParentViolation::L1_RemoveIfPresent(this);
  L1_RecursivelyComputeSetVariables();
  N1_ComputedTimeChanged();
  GetParent()->N1_ChildChanged(which_child);
}

void StaticElement::EraseTree() {
  for (uint i=0; i<static_children_.size(); i++) {
    StaticElement *child = GetChild(i);
    CHECK(child);
    if (child) child->EraseTree();
  }
  set<Element *> dc = dynamic_children_->GetChildren();
  forall(run, dc) (*run)->L1_Erase();
  UnlinkFromParent();
  L1_Erase();
}
void DynamicElement::EraseTree() {
  for (uint i=0; i<children_.size(); i++) {
    set<Element *> children = children_[i]->GetChildren();
    forall(run, children) {
      DynamicElement * child = dynamic_cast<DynamicElement*>(*run);
      child->EraseTree();
    }
  }
  UnlinkFromParent();
  L1_Erase();
}

void StaticElement::L1_Init() {
  Base::L1_Init();
  L1_AutomaticallyName();
  dynamic_children_ = New<MultiLink>(this);
  for (int i=0; i<NumChildren(); i++) 
    static_children_.push_back(New<SingleLink>(this));  
  objects_.resize(NumObjects());
  N1_ComputedTimeChanged();
  if (GetFunction() != ON) 
    StaticNoParentViolation::L1_CreateIfAbsent(this);
  L1_CreateChoices();
}
void StaticElement::L1_Erase() {
  CHECK(parent_ == NULL);
  for (uint i=0; i<static_children_.size(); i++) {
    static_children_[i]->L1_Erase();
  }
  L1_ClearChoices();
  dynamic_children_->L1_Erase();
  Element::L1_Erase();
}
void StaticElement::L1_ClearChoices() {
  for (uint i=0; i<choices_.size(); i++) choices_[i]->L1_Erase();
  CL.ChangeValue(&choices_, vector<Base *>());
}

void StaticElement::L1_CreateChoices(set<Variable> * variables_so_far, Object obj) {

  if (obj == NULL) return;

  Object universal_strategy = ::StringToObject("(meta (set universal))");

  if (obj.GetType() == Object::VARIABLE) {
    // Choose the keyword variable from (meta (set misc))
    CL.PushBack(&choices_,(Base *)
		New<Choice>(this, universal_strategy, 
			    Keyword::Make("variable")));
    
    // Choose the backreference or new variable
    CL.PushBack(&choices_,(Base *)
		New<ArbitraryChoice>(this, - Log(variables_so_far->size() + 1), "backref"));
    variables_so_far->insert(Variable(obj));
    return;
  }

  Object tuple_size_strategy = ::StringToObject("(generic (quadratic_uint) tuple_size)");
  if (obj.GetType() == Object::OTUPLE) {
    const Tuple & t = OTuple(obj).Data();
    CL.PushBack(&choices_,(Base *)
		New<Choice>(this, universal_strategy, Keyword::Make("tuple")));
    CL.PushBack(&choices_,(Base *)
		New<Choice>(this, tuple_size_strategy, Integer::Make(t.size())));
    for (uint i=0; i<t.size(); i++)  {
      L1_CreateChoices(variables_so_far, t[i]);
    }
    return;
  }
  
  if (obj.GetType() == Object::OPATTERN) {
    const Pattern & p = OPattern(obj).Data();
    CL.PushBack(&choices_, (Base *)
		New<Choice>(this, universal_strategy, 
			    Keyword::Make("pattern_choice")));
    // TODO: check for duplicate tuples - if so, don't save n!
    CL.PushBack(&choices_, (Base *)
		New<ArbitraryChoice>(this, LnFactorial(p.size()), 
						"pattern symmetry"));

    for (uint i=0; i<p.size(); i++) {
      const Tuple &t = p[i].Data();
      // we don't recurse here because we want to avoid reencoding the fact that each
      // sub-part is a tuple. 
      CL.PushBack(&choices_, (Base *) 
		  New<Choice>(this, tuple_size_strategy, Integer::Make(t.size())));
      for (uint i=0; i<t.size(); i++)  {
	L1_CreateChoices(variables_so_far, t[i]);
      }      
    }
    return;
  }
  CL.PushBack(&choices_, (Base *) New<Choice>(this, universal_strategy, obj));
}

void StaticElement::L1_CreateChoices() {
  L1_ClearChoices();
  Object function_strategy;
  function_strategy = ::StringToObject("{set functions}");
  CL.PushBack(&choices_, 
	      (Base *)New<Choice>(this, function_strategy, FunctionKeyword()));
  
  set<Variable> variables_so_far = variables_;

  for (int c=0; c<NumObjects(); c++) {
    Object obj = GetObject(c);
    L1_CreateChoices(&variables_so_far, obj);
  }
  bool any_bad_choices = false;
  for (uint i=0; i<choices_.size(); i++)  {
    Choice * c = dynamic_cast<Choice *>(choices_[i]);
    if (c && c->value_ == NULL) any_bad_choices = true;
  }
  if (any_bad_choices) StaticChoiceViolation::L1_CreateIfAbsent(this);
  else StaticChoiceViolation::L1_RemoveIfPresent(this);
}
StaticElement * StaticElement::GetParent() const { 
  if (!parent_) return NULL;
  return dynamic_cast<StaticElement *>(parent_->GetParent());
}
void DynamicElement::L1_Erase() {
  for (uint i=0; i<children_.size(); i++) {
    children_[i]->L1_Erase();
  }  
  Element::L1_Erase();
}

set<Variable> StaticElement::ComputeVariables() const { 
  if (!parent_) return set<Variable>();
  StaticElement * parent = GetParent();
  return Union(parent->GetVariables(), 
	       parent->GetIntroducedVariables(WhichChildAmI()));
}

void StaticElement::L1_RecursivelyComputeSetVariables() {
  set<Variable> new_variables = ComputeVariables();
  if (new_variables == variables_) return;
  CL.ChangeValue(&variables_, new_variables);
  L1_CreateChoices();
  for (int i=0; i<NumChildren(); i++) {
    StaticElement *child = GetChild(i);
    if (child) child->L1_RecursivelyComputeSetVariables();
  }
}

set<Variable> StaticElement::GetIntroducedVariables(int which_child) const {
  return set<Variable>();
}

StaticElement * StaticElement::GetChild(int which) const { 
  return dynamic_cast<StaticElement *>(static_children_[which]->GetChild());
}
DynamicElement * StaticElement::GetDynamic(OMap binding) const {
  return dynamic_cast<DynamicElement *>(dynamic_children_->GetChild(binding));
}
Object StaticElement::GetObject(int which) const{
  return objects_[which];
}
void StaticElement::SetObject(int which, Object new_value) {
  L1_SetObject(which, new_value);
  N1_ObjectChanged(which);
}
void StaticElement::L1_SetObject(int which, Object new_value) {
  CHECK(which < (int)objects_.size());
  CL.ChangeValue(&(objects_[which]), new_value);
}
void StaticElement::L1_LinkChild(int where, StaticElement *child){
  CHECK(where < (int)static_children_.size());
  static_children_[where]->L1_AddChild(child);
}
void StaticElement::L1_UnlinkChild(int where){
  CHECK(where < (int)static_children_.size());
  static_children_[where]->L1_RemoveChild(GetChild(where));
}
set<Element *> StaticElement::GetAllChildren() const {
  set<Element *> ret;
  for (uint i=0; i<static_children_.size(); i++) 
    static_children_[i]->AddChildrenInto(&ret);
  dynamic_children_->AddChildrenInto(&ret);
  return ret;  
}
set<StaticElement *> StaticElement::GetAllStaticChildren() const {
  set<StaticElement *> ret;
  for (uint i=0; i<static_children_.size(); i++) 
    static_children_[i]->AddChildrenInto(&ret);
  return ret;
}

set<Element *> DynamicElement::GetAllChildren() const {
  set<Element *> ret;
  for (uint i=0; i<children_.size(); i++) children_[i]->AddChildrenInto(&ret);
  return ret;  
}
bool Element::NeedsChildViolation() const { 
  for (int i=0; i<NumChildren(); i++) {
    if (LinkType(i) != Link::SINGLE) continue;
    if (ChildShouldExist(i) != bool(GetChild(i))) return true;
  }
  return false;
}

Object DynamicElement::GetChildValue(int which) const { 
  DynamicElement * child = GetSingleChild(which);
  if (!child) return Object();
  return child->GetValue();
}

bool DynamicElement::SetBinding(OMap new_binding) {
  // check to see that we will not cause a conflict.
  if (static_parent_->children_ % new_binding) {
    cerr << "couldn't change binding because of conflict" << endl;
    return false;
  }
  Link * p = parent_;
  MultiLink * sp = static_parent_;
  sp->L1_RemoveChild(this);
  if (p) p->L1_RemoveChild(this);
  CL.ChangeValue(&binding_, new_binding);
  sp->L1_AddChild(this);
  if (p) p->L1_AddChild(this);
  N1_BindingChanged();
  return true;
}

void DynamicElement::N1_BindingChanged() {
  L1_CheckSetParentAndBindingViolations();
  set<Element *> all_children = GetAllChildren();
  forall(run, all_children) {
    dynamic_cast<DynamicElement *>(*run)
      ->L1_CheckSetParentAndBindingViolations();
  }
}

void DynamicElement::L1_CheckSetParentAndBindingViolations() {
  if (IsErased()) return;
  if (M.batch_mode_) {
    M.L1_AddDelayedCheck(this, &Element::L1_CheckSetParentAndBindingViolations);
    return;
  }
  // first check that there's a parent if there needs to be.
  if (GetParent() == NULL && GetFunction() != ON) {
    DynamicNoParentViolation::L1_CreateIfAbsent(this);
    BindingVariablesViolation::L1_RemoveIfPresent(this);
    BindingOldValuesViolation::L1_RemoveIfPresent(this);
    return;
  }
  DynamicNoParentViolation::L1_RemoveIfPresent(this);

  // Check that the binding variables are correct
  if (GetStatic()->GetVariables()
      != GetDomainVariables(binding_)) {
    BindingVariablesViolation::L1_CreateIfAbsent(this);
    BindingOldValuesViolation::L1_RemoveIfPresent(this);
    return;
  }
  BindingVariablesViolation::L1_RemoveIfPresent(this);

  // Check that the values of the old variables are correct
  DynamicElement * parent = GetParent();
  if (parent && (Restrict(GetBinding(), parent->GetStatic()->GetVariables())
		 != parent->GetBinding()))
    BindingOldValuesViolation::L1_CreateIfAbsent(this);
  else BindingOldValuesViolation::L1_RemoveIfPresent(this);

  if (parent && parent->GetFunction() == LET) 
    dynamic_cast<DynamicLet *>(parent)->L1_CheckSetLetViolation();
}

DynamicElement * DynamicElement::GetSingleChild(int which) const { 
  SingleLink * l = dynamic_cast<SingleLink *>(children_[which]);
  if (l == NULL) {
    cerr << "Link does not exit for dynamic: " << FunctionToString(GetFunction()) << endl;
    cerr << "Name:" << GetName() << endl;
    CHECK(false);
  }
  return (DynamicElement *)(l->GetChild());
}

OTime DynamicElement::ComputeTime() const { 
  if (!parent_) return CREATION;
  return parent_->ComputeChildTime(this);
}
string DynamicElement::ToString(int indent) const{
  return HTMLEscape(GetBinding().ToString()) 
    + GetStatic()->ToString();
}

void DynamicElement::SetValue(Object new_value) {
  Object old_value = value_;
  if (new_value == old_value) return;
  CL.ChangeValue(&value_, new_value);
  N1_StoredValueChanged(); 
  if (GetParent()) {
    GetParent()->N1_ChildValueChanged(WhichChildAmI(), old_value, new_value);  
  }
}

void DynamicElement::L1_CheckSetValueViolation() {
  if (IsErased()) return;
  if (M.batch_mode_) {
    M.L1_AddDelayedCheck(this, &Element::L1_CheckSetValueViolation);
    return;
  }
  bool perfect = (value_ != NULL && value_ == ComputeValue());
  Violation * value_violation = Violation::Search(this, Violation::VALUE, NULL);
  if (perfect && value_violation) {
    value_violation->L1_Erase(); 
    return;
  }
  if (!perfect && !value_violation) {
    New<ValueViolation>(this);
    return;
  }
}



void StaticOn::L1_Init(){
  StaticElement::L1_Init();
  New<MissingDynamicOnViolation>(this);
}
set<Variable> StaticOn::GetIntroducedVariables(int which_child) const {
  return ::GetVariables(GetPattern().Data());
}
void StaticOn::L1_Erase() {
  StaticElement::L1_Erase();
}
// this thing has no dynamic parent. 
void DynamicOn::L1_Init(StaticElement *static_parent, OMap dummy) {
  DynamicElement::L1_Init(static_parent, OMap::Default());
  Violation * missing_dynamic 
    = Violation::Search(GetStatic(), Violation::MISSING_DYNAMIC_ON, NULL);
  CHECK(missing_dynamic);
  missing_dynamic->L1_Erase();
}
void DynamicOn::L1_Erase(){
  CHECK(GetStatic());
  CHECK(!Violation::Search(GetStatic(), Violation::MISSING_DYNAMIC_ON, NULL));
  New<MissingDynamicOnViolation>(GetStaticOn());
  DynamicElement::L1_Erase();
}
Record DynamicOn::GetRecordForDisplay() const {
  Record ret = DynamicElement::GetRecordForDisplay();
  set<Violation *> missing 
    = Violation::GetViolations
    (Violation::Search(this, Violation::MISSING_ON_MATCH));
  set<Violation *> extra 
    = Violation::GetViolations
    (Violation::Search(this, Violation::EXTRA_ON_MATCH));
  forall(run, missing)
    ret["violations"] += (*run)->ShortDescription() + "<br>\n";
  forall(run, extra)
    ret["violations"] += (*run)->ShortDescription() + "<br>\n";
  return ret;
}

OTime DynamicOn::ComputeTime() const {
  return GetStatic()->GetTime();
}

OTime DynamicOn::ComputeChildTime(const Link * link, 
				  const Element * child) const{
  return OTime::Make
    (max(time_.Data(),
	 BB.FindLastTime(Substitute(child->GetBinding().Data(), 
				    GetPattern().Data())).Data()
	 + BitSeq::Min() ) );
}
/*
void StaticRepeat::L1_Init() {
  StaticElement::L1_Init();
  L1_SetObject(REPETITION_VARIABLE, M.L1_GetNextUniqueVariable());
}
*/



set<Variable> StaticMatch::GetIntroducedVariables(int which_child) const {
  return ::GetVariables(GetPattern().Data()) - GetVariables();
}
Record DynamicMatch::GetRecordForDisplay() const {
  Record ret = DynamicElement::GetRecordForDisplay();
  set<Violation *> missing 
    = Violation::GetViolations
    (Violation::Search(this, Violation::MISSING_MATCH));
  set<Violation *> extra 
    = Violation::GetViolations
    (Violation::Search(this, Violation::EXTRA_MATCH));
  forall(run, missing)
    ret["violations"] += (*run)->ShortDescription() + "<br>\n";
  forall(run, extra)
    ret["violations"] += (*run)->ShortDescription() + "<br>\n";
  ret["pattern (current)"] = GetCurrentPattern().ToString();
  ret["pattern (computed)"] = ComputePattern().ToString();
  ret["time limit"] = GetTimedQuery()->GetTimeLimit().ToString();
  ret["sum"] = itoa(sum_);
  return ret;
}





void StaticDelay::L1_Init() {
  StaticElement::L1_Init();
}
void StaticLet::L1_Init() {
  StaticElement::L1_Init();
}
bool DynamicLet::NeedsLetViolation() const {
  DynamicElement *value_child = GetSingleChild(StaticLet::VALUE);
  if (!value_child) return false;
  DynamicElement *child = GetSingleChild(StaticLet::CHILD);
  if (!child) return false;
  if (Violation::Search(child, Violation::BINDING_VARIABLES), NULL) 
    return false;
  const Object * binding_value = child->GetBinding().Data()
    % GetStatic()->GetObject(StaticLet::VARIABLE);
  CHECK(binding_value);
  return (value_child->GetValue() != *binding_value);
}
void DynamicLet::L1_CheckSetLetViolation() {
  if (IsErased()) return;
  if (M.batch_mode_) {
    M.L1_AddDelayedCheck(this, &Element::L1_CheckSetLetViolation);
    return;
  }
  if (NeedsLetViolation()) LetViolation::L1_CreateIfAbsent(this);
  else LetViolation::L1_RemoveIfPresent(this);
}

void StaticPost::L1_Init() {
  StaticElement::L1_Init();
}
Record DynamicPost::GetRecordForDisplay() const {
  Record ret = DynamicElement::GetRecordForDisplay();
  if (posting_) ret["posting"] = posting_->ShortDescription();
  else ret["posting"] = "NULL";
  return ret;
}
void DynamicPost::SetPostingTime(OTime new_time) {
  if (new_time == time_) return;
  posting_->L1_ChangeTime(new_time.Data());
  L1_CheckSetPostViolation();
}
bool DynamicPost::NeedsPostViolation() const {
  Object computed = ComputeTuple();
  if (computed.GetType() != Object::OTUPLE) {
    // If the computed value is not a tuple, there should be no posting. 
    // It is a posting violation if one exists.
    if (posting_) {
      return true;
    } else {
      return false;
    }
  }
  if (!posting_) {
    return true;
  }
  if (posting_->tuple_ != computed) {
    return true;
  }
  if (posting_->time_ != time_.Data()) {
    return true;
  }
  return false;
}
void DynamicPost::L1_CheckSetPostViolation() {
  if (IsErased()) return;
  if (M.batch_mode_) {
    M.L1_AddDelayedCheck(this, &Element::L1_CheckSetPostViolation);
    return;
  }
  if (NeedsPostViolation())
    PostViolation::L1_CreateIfAbsent(this);
  else 
    PostViolation::L1_RemoveIfPresent(this);
}
void DynamicPost::AddCorrectPosting() {
  DynamicElement * expr = GetSingleChild(StaticPost::TUPLE);
  if (!expr) return;
  Object t = expr->value_;
  if (t.GetType() != Object::OTUPLE) return;
  if (time_ == NULL) {
    cerr << "Cannot add posting because time is null";
    return;
  }
  AddPosting(OTuple(t), time_);
}
void DynamicPost::AddPosting(OTuple t, OTime time) {
  if (posting_) {
    if (posting_->tuple_ == t) return;
    RemovePosting();
  }
  CL.ChangeValue(&posting_, new OwnedPosting(t, time, this));
  L1_CheckSetPostViolation();
}
void DynamicPost::RemovePosting() {
  CHECK(posting_);
  posting_->L1_Erase();
  CL.ChangeValue(&posting_, (OwnedPosting *)NULL);
  L1_CheckSetPostViolation();
}

void StaticIf::L1_Init() {
  StaticElement::L1_Init();
}

bool DynamicIf::ChildShouldExist(int which_child) const { 
  if (which_child == StaticIf::ON_TRUE
      || which_child == StaticIf::ON_FALSE) {
    DynamicElement * expr = GetSingleChild(StaticIf::CONDITION);
    if (!expr) return false;
    bool val = expr->value_ != FALSE;
    if (which_child == StaticIf::ON_TRUE) return val;
    return !val;
  }
  return true;
}

string StaticElement::ToString(int indent) const {
  string ret = GetLink(FunctionKeyword().ToString());
  if (GetFunction() == Element::MAKETUPLE) {
    ret="";
  }
  for (int i=0; i<NumObjects(); i++) {
    ret += " " + HTMLEscape(GetObject(i).ToString());
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
    StaticElement *child = GetChild(i);
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
    if (child) ret += child->ToString(child_indent);
    else ret += "null";
  }
  if (ChildrenGoInTuple()) {
    if (separate_line) ret += "\n" + string(indent, ' ');
    if (GetFunction() == Element::MAKETUPLE) ret += GetLink(")");
    else ret += ")";
  }
  return ret;
}

void StaticConstant::N1_ObjectChanged(int which) {
  StaticElement::N1_ObjectChanged(which);
  CHECK(which == OBJECT);
  forall(run, dynamic_children_->children_) {
    run->second->L1_CheckSetValueViolation();
  }
}

string StaticConstant::ToString(int indent) const {
  bool can_be_concise = true;
  Object o = GetObject(OBJECT);
  if (o.GetType() == Object::OTUPLE)  can_be_concise = false;
  if (o.GetType() == Object::VARIABLE) can_be_concise = false;
  if (o.GetType() == Object::KEYWORD) {
    if (Element::TypeKeywordToFunction(Keyword(o)) != -1)
      can_be_concise = false;
  }
  if (!can_be_concise) return StaticElement::ToString(indent);
  string ret = HTMLEscape(o.ToString());
  return GetLink(ret);
}
Object DynamicConstant::ComputeValue() const {
  return GetObject(StaticConstant::OBJECT);
}
string StaticSubstitute::ToString(int indent) const {
  StaticElement *child = GetChild(StaticSubstitute::CHILD);
  if (child) {
    if (child->GetFunction() == Element::CONSTANT) {
      StaticConstant *constant = dynamic_cast<StaticConstant *>(child);
      Object o = constant->GetObject(StaticConstant::OBJECT);
      if (o.GetType() == Object::VARIABLE) { 
	string ret = HTMLEscape(o.ToString());
	return GetLink(ret);
      }
    }
  }
  return StaticElement::ToString();
}

Object DynamicSubstitute::ComputeValue() const {
  DynamicElement * child 
    = GetSingleChild(StaticSubstitute::CHILD);
  if (!child) return Object();
  return DeepSubstitute(GetBinding().Data(), child->GetValue());
}
Object DynamicEqual::ComputeValue() const { 
  return Boolean::Make( (GetChildValue(StaticEqual::LHS)
			 == GetChildValue(StaticEqual::RHS) ) );
}
Object DynamicSum::ComputeValue() const {
  Object lhs = GetChildValue(StaticEqual::LHS);
  Object rhs = GetChildValue(StaticEqual::RHS);
  VLOG(3) << "lhs=" << lhs << " rhs=" << rhs << endl;
  if (lhs.GetType() == Object::INTEGER
      && rhs.GetType() == Object::INTEGER) {
    return Integer::Make(Integer(lhs).Data() + Integer(rhs).Data());
  }
  if (lhs.GetType() == Object::REAL
      && rhs.GetType() == Object::REAL) {
    return Real::Make(Real(lhs).Data() + Real(rhs).Data());
  }
  return Object();
}
Object DynamicConcat::ComputeValue() const {
  string ret;
  for (int i=0; i<NumChildren(); i++) {
    Object child_val = GetChildValue(i);
    if (child_val.GetType() != Object::STRING) return Object();
    ret += String(child_val).Data();
  }
  return String::Make(ret);
}
Object DynamicMakeTuple::ComputeValue() const {
  Tuple t;
  for (int i=0; i<NumChildren(); i++) {
    t.push_back(GetChildValue(i));
  }
  return OTuple::Make(t);
}
Object DynamicToString::ComputeValue() const { 
  return String::Make(GetChildValue(StaticToString::ARG).ToString());
}

void DynamicChoose::L1_Init(StaticElement * static_parent, OMap binding) {
  choice_ = NULL;
  DynamicElement::L1_Init(static_parent, binding);
}

void DynamicChoose::L1_Erase() {
  if (choice_) choice_->L1_Erase();
  DynamicElement::L1_Erase();
}

bool DynamicChoose::L1_TryMakeChoice(OTuple strategy, Object value) {
  if (!choice_)
    choice_ = New<Choice>(this, strategy, value);
  else choice_->L1_Change(strategy, value);

  N1_ComputedValueChanged();
  return (value == choice_->value_);
}
bool DynamicChoose::L1_TryMakeCorrectChoice() {
  return L1_TryMakeChoice
    (OTuple::ConvertOrNull(GetChildValue(StaticChoose::STRATEGY)), value_);
}

Object DynamicChoose::ComputeValue() const { 
  if(choice_)
    return choice_->value_;
  return NULL;
}

Link * DynamicElement::FindDynamicParentLink() const {
  StaticElement * st = GetStatic();
  CHECK(st);
  Link * spl = st->parent_;
  if (!spl) return NULL;
  StaticElement * sp = dynamic_cast<StaticElement *>(spl->GetParent());
  int childnum = -1;
  for (uint i=0; i<sp->static_children_.size(); i++) {
    if (sp->static_children_[i] == spl) childnum = i;
  }
  CHECK(childnum != -1);
  DynamicElement * dp 
    = sp->GetDynamic(Restrict(binding_, sp->GetVariables()));
  if (!dp) {
    cerr << "binding=" << binding_ << endl
	 << "restricted=" << Restrict(binding_, sp->GetVariables()) << endl;
    set<Variable> vars = sp->GetVariables();
    forall(run, vars) cerr << (*run) << " ";
    cerr << endl;
  }
  CHECK(dp);
  return dp->children_[childnum];  
}

bool DynamicElement::LinkToParent() {
  Link * parent = FindDynamicParentLink();
  if (!parent) return false;
  parent->L1_AddChild(this);
  CHECK(parent_ == parent);
  L1_CheckSetParentAndBindingViolations();
  GetParent()->N1_ChildChanged(WhichChildAmI());
  GetParent()->N1_ChildValueChanged(WhichChildAmI(), NULL, value_);
  N1_ComputedTimeChanged();
  return true;
}

void DynamicElement::UnlinkFromParent() {
  DynamicElement * dynamic_parent = GetParent();
  int which_child = WhichChildAmI();
  Object old_val = value_;
  CHECK(parent_);
  parent_->L1_RemoveChild(this);
  dynamic_parent->N1_ChildValueChanged(which_child, old_val, NULL);
  dynamic_parent->N1_ChildChanged(which_child);
  L1_CheckSetParentAndBindingViolations();
  N1_ComputedTimeChanged();
}


void DynamicElement::L1_Init(StaticElement * static_parent, 
			     OMap binding) {
  Element::L1_Init();
  static_parent_ = NULL;
  parent_ = NULL;
  binding_ = binding;
  CHECK(static_parent);
  static_parent->dynamic_children_->L1_AddChild(this);
  CHECK(static_parent_ == static_parent->dynamic_children_);
  LinkToParent();
 
  // L1_ComputeSetTime(); may belong here
  for (int i=0; i<NumChildren(); i++) {
    Link * child;
    switch (LinkType(i)) {
    case Link::MULTI:
      child = New<MultiLink>(this);
      break;
    case Link::SINGLE:
      child = New<SingleLink>(this);
      break;
    case Link::ON:
      child = New<OnMultiLink>(dynamic_cast<DynamicOn *>(this));
      break;
    case Link::MATCH:
      child = New<MatchMultiLink>(dynamic_cast<DynamicMatch *>(this));
      break;
    default:
      CHECK(false);
      break;
    }
    children_.push_back(child);    
  }
  N1_ComputedTimeChanged();
  L1_CheckSetChildViolation();
  value_ = NULL;
  N1_StoredValueChanged();
  N1_ComputedValueChanged(); 
};

DynamicElement * DynamicElement::FindParent() const {
  CHECK(static_parent_);
  CHECK(static_parent_->parent_);
  return GetStatic()->GetParent()->GetDynamic
    (Restrict(GetBinding(), GetStatic()->GetVariables()));
}

DynamicElement * MakeDynamicElement(StaticElement *static_parent, 
				    OMap binding) {
  switch(static_parent->GetFunction()) {
#define FUNCTION(f,F) case Element::F:					\
    return MakeDynamicElement<Dynamic##f>(static_parent, binding);
    ALL_FUNCTIONS;
#undef FUNCTION
  default:
    CHECK(false);
  }
  return NULL;
}



Record Element::GetRecordForDisplay() const {
  Record ret = Base::GetRecordForDisplay();
  Element * parent = GetParent();
  if (parent) ret["parent"] = parent->ShortDescription();
  ret["function"] = FunctionToString(GetFunction());
  ret["description"] = ShortDescription();
  ret["time"] = time_.ToString();
  return ret;  
}

Record StaticElement::GetRecordForDisplay() const {
  Record ret = Element::GetRecordForDisplay();
  ret["program"] = "<pre>" + ToString() + "</pre>";
  set<Element *> dynamic_children = dynamic_children_->GetChildren();
  int count =0;
  forall(run, dynamic_children) {
    if (count==10) {
      ret["dynamic"] += "... " + itoa(dynamic_children.size()) + " total";      
      break;
    };
    ret["dynamic"] += HTMLEscape((*run)->GetBinding().ToString()) + "<br>";
  }
  for (int i=0; i<NumChildren(); i++) {
    if (i>0) ret["children"] += "<br>\n";
    ret["children"] += ChildToString(i);
    ret["children"] += ": " + static_children_[i]->ChildListings();
  }
  for (int i=0; i<NumObjects(); i++) {
    if (i>0) ret["children"] += "<br>\n";
    ret["objects"] += ObjectToString(i) + ": " + GetObject(i).ToString();
  }
  ret["dynamic_children"] = dynamic_children_->ChildListings(5);
  forall(run, choices_) {
    ret["choices"] += (*run)->ShortDescription() + "<br>\n";
  }
  return ret;
}
Record DynamicElement::GetRecordForDisplay() const { 
  Record ret = Element::GetRecordForDisplay();
  ret["static_parent"] = GetStatic()?GetStatic()->ShortDescription():
    "NONE FOUND";
  ret["binding"] = GetBinding().ToString();
  if (!GetStatic()) return ret;
  for (uint i=0; i<children_.size(); i++) {
    if (i>0) ret["children"] += "<br>\n";
    ret["children"] += GetStatic()->ChildToString(i);
    if (LinkType(i) != Link::SINGLE) {
      ret["children"] += children_[i]->ShortDescription() + "<br>\n";
    }
    ret["children"] + ": " + children_[i]->ChildListings(5);
  }
  ret["value"] = value_.ToString();
  if (value_ != ComputeValue()) {
    ret["value"] += " (computed=" + ComputeValue().ToString() + ")";
  }
  return ret;
}

