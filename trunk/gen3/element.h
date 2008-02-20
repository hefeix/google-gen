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

#ifndef _ELEMENT_H_
#define _ELEMENT_H_

#include "base.h"
#include "execution.h"

#define ALL_FUNCTIONS \
  FUNCTION(On, ON)						\
  FUNCTION(Post, POST)						\
  FUNCTION(MakeTuple, MAKETUPLE)				\
  FUNCTION(Substitute, SUBSTITUTE)				\
  FUNCTION(Constant, CONSTANT)					\
  FUNCTION(Equal, EQUAL)				     	\
  FUNCTION(Sum, SUM)					       	\
  FUNCTION(If, IF)					       	\

       /*
       FUNCTION(ToString, TOSTRING)					\
       FUNCTION(Concat, CONCAT)						\
       FUNCTION(Nth, NTH)					\
       FUNCTION(Let, LET)			\
       FUNCTION(Match, MATCH)			\
       FUNCTION(Delay, DELAY)			\
    
       FUNCTION(Choose, CHOOSE)		\
       */


struct Element : public Base {  
 public:
  
  #define FUNCTION(func, FUNC) ITEM(FUNC), 
  #define ElementFunctionList {					\
    ALL_FUNCTIONS						\
      };
  CLASS_ENUM_DECLARE(Element, Function);
  #undef FUNCTION

  // Creation / destruction
  Element() : parent_(NULL) {};
  void Init() { Base::Init(); }
  void Erase() { Base::Erase(); }

  // Run things
  virtual Object Execute(Thread thread);
  virtual Object ComputeReturnValue(Thread thread, Tuple results) {
    CHECK(false); return NULL;
  }

  // Verification code
  bool VerifyNode() {
    return ( (RequiredNumChildren() == NumChildren()) ||
	    HasVariableNumChildren());
  }

  bool VerifyTree() {
    if (!VerifyNode()) return false;
    for (uint c=0; c<children_.size(); c++) {
      Element * child = children_[c];
      if (!child->VerifyTree()) return false;
    }
    return true;
  }

  // Linking things up together
  void AddChild(Element * child) {
    CHECK(child->parent_ == NULL);
    child->parent_ = this;
    children_.push_back(child);
  }
  
  // This is for parsing
  static Function TypeKeywordToFunction(Keyword type) {
    return StringToFunction(Upcase(type.Data())); 
  }
 
  // Printing
  Record GetRecordForDisplay() const;
  virtual string ProgramTree(int indent = 0) const;
  string TextIdentifier() const { return ProgramTree(); }
  
  // Simple accessor functions for the static tree
  Element * GetParent() const { return parent_; }
  Element * GetChild (int which) const { return children_[which]; }
  virtual int RequiredNumChildren() const = 0;
  int NumChildren() const { return children_.size(); }
  
  // Accessing the function of the element
  Keyword FunctionKeyword() const { 
    return Keyword::Make(Downcase(FunctionToString(GetFunction())));
  }
  virtual Function GetFunction() const = 0;
  
  // Accessing children
  int WhichChildAmI() const { 
    if (parent_) return parent_->WhichChildIsThis(this); 
    return -1; 
  }
  int WhichChildIsThis(const Element * child) const {
    for (uint c=0; c<children_.size(); c++)
      if (children_[c] == child) return c;
    return -1;
  }
  
  virtual bool HasVariableNumChildren() const { return false; }
  
  // Accessing names of children pnemonically
  // These are virtual but don't need to be defined by subclasses other
  // than by the macros
  virtual int StringToChild(string s) const = 0;
  virtual string ChildToString(int c) const = 0;

  // Sometimes overridden
  virtual bool ChildrenGoInTuple() const { return HasVariableNumChildren(); }
  virtual bool ElementNeedsSeparateLine() const { return false; }
  virtual bool ChildNeedsSeparateLine(int which_child) const { return false; }

  // Doesn't need to be overridden
  Base::Type GetBaseType() const { return Base::ELEMENT; }

  static void StaticInit();

  // ---------- data ----------  
  Element * parent_;
  vector<Element *> children_;
};

// We want to call static functions in the right class given a particular
// C object. Therefore, we forward to virtual functions in each class,
// which then call the appropriate static functions. These virtual functions
// are declared and defined by this macro.
#define DECLARE_FUNCTION_ENUMS \
  int StringToChild(string s) const{ return StringToChildName(s); }	\
  string ChildToString(int c) const{					\
    if (HasVariableNumChildren()) return "CHILD(" + itoa(c) + ")";	\
    return ChildNameToString((ChildName)c);}				\
  int RequiredNumChildren() const { return HasVariableNumChildren()?	\
      -1:NumChildNames();}

template <class T> T * MakeElement() {
  CHECK(static_cast<Element *>((T *)NULL) == NULL);
  return New<T>();
}


struct OnElement : public Element {
  #define OnElementChildNameList {				\
        ITEM(TUPLE),						\
	ITEM(CHILD),						\
  };
  CLASS_ENUM_DECLARE(OnElement, ChildName);
  DECLARE_FUNCTION_ENUMS;

  virtual Function GetFunction() const { return ON; }
  bool ElementNeedsSeparateLine() const { return true; }
  bool ChildNeedsSeparateLine(int which_child) const { 
    if (which_child == CHILD) return true;
    return false;
  }

  virtual Object Execute(Thread thread);
};

struct PostElement : public Element {
#define PostElementChildNameList { ITEM(TUPLE) };
  CLASS_ENUM_DECLARE(PostElement, ChildName);
  DECLARE_FUNCTION_ENUMS;
  Function GetFunction() const { return POST;}
  bool ElementNeedsSeparateLine() const { return true; }
  virtual Object Execute(Thread thread);
};

struct ConstantElement : public Element {
#define ConstantElementChildNameList {};
  CLASS_ENUM_DECLARE(ConstantElement, ChildName);
  DECLARE_FUNCTION_ENUMS;
  Function GetFunction() const { return CONSTANT; }
  string ProgramTree(int indent) const;
  void SetObject(Object o) { object_ = o; }
  Object Execute(Thread t) { return object_; }
  Object object_;
};

struct MakeTupleElement : public Element {
#define MakeTupleElementChildNameList {};
  CLASS_ENUM_DECLARE(MakeTupleElement, ChildName);
  DECLARE_FUNCTION_ENUMS;
  Function GetFunction() const { return MAKETUPLE; }
  bool HasVariableNumChildren() const { return true; }
  bool ChildNeedsSeparateLine(int which_child) const {
    for (int c = 0; c<NumChildren(); c++) {
      const Element * e = GetChild(c);
      if (e && e->ElementNeedsSeparateLine()) return true;
    }
    return false;
  }
  Object ComputeReturnValue(Thread thread, Tuple results) {
    return OTuple::Make(results);
  }
};

struct SubstituteElement : public Element {
#define SubstituteElementChildNameList { ITEM(CHILD) };
  CLASS_ENUM_DECLARE(SubstituteElement, ChildName);
  DECLARE_FUNCTION_ENUMS;

  string ProgramTree(int indent) const;
  virtual Function GetFunction() const { return SUBSTITUTE;}
  string ToString() const;
  bool ChildrenGoInTuple() const { return true;}
  Object ComputeReturnValue(Thread thread, Tuple results) {
      return DeepSubstitute(thread.binding_.Data(), results[CHILD]);
  }
};

struct EqualElement : public Element {
#define EqualElementChildNameList { ITEM(LHS), ITEM(RHS), };
  CLASS_ENUM_DECLARE(EqualElement, ChildName);
  DECLARE_FUNCTION_ENUMS;
  virtual Function GetFunction() const { return EQUAL; }
  bool ChildrenGoInTuple() const { return true; }
  Object ComputeReturnValue(Thread thread, Tuple results) {
    if (results[0] == results[1]) return TRUE;
    return FALSE;
  }
};

struct SumElement : public Element {
#define SumElementChildNameList { ITEM(LHS), ITEM(RHS), };
  CLASS_ENUM_DECLARE(SumElement, ChildName);
  DECLARE_FUNCTION_ENUMS;
  Function GetFunction() const { return SUM; }
  bool ChildrenGoInTuple() const { return true; }
  Object ComputeReturnValue(Thread thread, Tuple results) {
    if (results[0].GetType() != Object::INTEGER) return NULL;
    if (results[1].GetType() != Object::INTEGER) return NULL;
    return Integer::Make
      (Integer(results[0]).Data() + Integer(results[1]).Data());
  }
};

struct IfElement : public Element {
#define IfElementChildNameList {					\
    ITEM(CONDITION),							\
      ITEM(ON_TRUE),							\
      ITEM(ON_FALSE),							\
      };
  CLASS_ENUM_DECLARE(IfElement, ChildName);
  DECLARE_FUNCTION_ENUMS;
  bool ChildrenGoInTuple() const { return true; }
  bool ChildNeedsSeparateLine(int which_child) const {
    if (which_child == CONDITION) return false;
    for (int c = ON_TRUE; c<NumChildren(); c++) {
      Element * e = GetChild(c);
      if (e && e->ElementNeedsSeparateLine()) return true;
    }
    return false;
  }
  Function GetFunction() const { return IF; }
  virtual Object Execute(Thread thread);
};



/*

struct Match : public Element {
  #define MatchChildNameList {				\
      ITEM(CHILD),						\
	};
  CLASS_ENUM_DECLARE(Match, ChildName);
  #define MatchObjectNameList {				\
      ITEM(PATTERN),					\
	};
  CLASS_ENUM_DECLARE(Match, ObjectName);
  DECLARE_FUNCTION_ENUMS;

  // ---------- L2 functions ----------  
  // ---------- const functions ----------  
  OPattern GetPattern() const { return GetObject(PATTERN);}
  virtual Function GetFunction() const { return MATCH;}
  set<Variable> GetIntroducedVariables(int which_child) const;
  bool ElementNeedsSeparateLine() const { return true;}
  bool ChildNeedsSeparateLine(int which_child) const { return false;}
  // ---------- L1 functions ----------  
  // ---------- N1 notifiers ----------  
  void N1_ObjectChanged(int which){
    Element::N1_ObjectChanged(which);
    CHECK(NumDynamicChildren() == 0);
  }

  // ---------- data ----------  
};

struct DynamicMatch : public DynamicElement {
  // ---------- L2 functions ----------  
  // ---------- const functions ----------  
  Link::Type LinkType(int which_child) const{ return Link::MATCH;}
  Match *GetMatch() const { 
    return dynamic_cast<Match*>(Get()); }
  MatchMultiLink * GetMatchMultilink() const { 
    return dynamic_cast<MatchMultiLink *>(children_[Match::CHILD]); }
  OPattern ComputePattern() const {
    return Substitute(GetBinding().Data(), GetMatch()->GetPattern()); }
  OPattern GetCurrentPattern() const { 
    CHECK(GetMatchMultilink());
    return GetMatchMultilink()->GetPattern(); }
  TimedQuery * GetTimedQuery() const {
    CHECK(GetMatchMultilink());
    return GetMatchMultilink()->GetTimedQuery();
  }
  Record GetRecordForDisplay() const;
  Object ComputeValue() const { return Integer::Make(sum_);}
  // ---------- L1 functions ----------
  void L1_Init(Element* parent, OMap binding) {
    sum_ = 0;
    DynamicElement::L1_Init(parent, binding);
    GetTimedQuery()->L1_SetTimeLimit(GetTime());
    GetTimedQuery()->L1_SetPattern(ComputePattern());    
  }
  // --------- N1 notifiers ----------
  void N1_ChildValueChanged(int which_child, Object old_val, Object new_val) { 
    int64 diff = 0;
    if (old_val.GetType() == Object::INTEGER) diff -= Integer(old_val).Data();
    if (new_val.GetType() == Object::INTEGER) diff += Integer(new_val).Data();
    if (diff != 0) {
      CL.ChangeValue(&sum_, sum_+diff);
    }
    DynamicElement::N1_ChildValueChanged(which_child, old_val, new_val);
  }
  void N1_BindingChanged() {
    GetTimedQuery()->L1_SetPattern(ComputePattern());    
    DynamicElement::N1_BindingChanged();
  }
  void N1_StoredTimeChanged() {
    DynamicElement::N1_StoredTimeChanged();
    GetTimedQuery()->L1_SetTimeLimit(GetTime());
  }
  // ---------- data ----------
  int64 sum_;
};


struct Delay : public Element { 
  #define DelayChildNameList {				\
      ITEM(DIMENSION),						\
      ITEM(CHILD),						\
	};
  CLASS_ENUM_DECLARE(Delay, ChildName);
  #define DelayObjectNameList {				\
    };
  CLASS_ENUM_DECLARE(Delay, ObjectName);
  DECLARE_FUNCTION_ENUMS;

  // ---------- L2 functions ----------  
  // ---------- const functions ----------  
  Function GetFunction() const { return DELAY;}
  bool ElementNeedsSeparateLine() const { return true;}
  bool ChildNeedsSeparateLine(int which_child) const { 
    return (which_child==CHILD);}

  // ---------- L1 functions ----------  
  void L1_Init();
  // ---------- data ----------    
};
struct DynamicDelay : public DynamicElement {
  // ---------- L2 functions ----------  
  // ---------- const functions ----------  
  OTime ComputeChildTime(const Link * link, const Element *child) const {
    if (link == children_[Delay::CHILD]) {
      if (time_ == NULL) return OTime();
      DynamicElement *delay_element 
	= GetSingleChild(Delay::DIMENSION);
      if (!delay_element) return OTime();
      Object dimension = delay_element->value_;
      if (dimension.GetType() != Object::OBITSEQ) return OTime();
      return OTime::Make
	(time_.Data() 
	 + OBitSeq(dimension).Data());
    }
    return time_;
  }

  // Dynamic lets can't pass values up, that would allow later actions
  // to influence earlier actions
  Object ComputeValue() const { return FALSE;}

  // ---------- L1 functions ----------  
  // ---------- N1 notifiers ----------
  void N1_ChildValueChanged(int which_child, Object old_val, Object new_val) { 
    DynamicElement::N1_ChildValueChanged(which_child, old_val, new_val);
    // todo: check whether the times are right on the children
    DynamicElement * child = GetSingleChild(Delay::CHILD);
    if (child) child->N1_ComputedTimeChanged();
  }
  // ---------- data ----------  
};
  
struct Let : public Element {
  #define LetChildNameList {				\
      ITEM(VALUE),					\
      ITEM(CHILD),					\
	};
  CLASS_ENUM_DECLARE(Let, ChildName);
  #define LetObjectNameList {				\
      ITEM(VARIABLE),					\
	};
  CLASS_ENUM_DECLARE(Let, ObjectName);
  DECLARE_FUNCTION_ENUMS;

  // ---------- L2 functions ----------  
  // ---------- const functions ----------  
  Variable GetVariable() const { return GetObject(VARIABLE);}
  virtual Function GetFunction() const { return LET;}

  set<Variable> GetIntroducedVariables(int which_child) const {
    if (which_child == CHILD) return Singleton<set<Variable> >(GetVariable());
    return set<Variable>();
  }
  // ---------- L1 functions ----------  
  void L1_Init();
  // ---------- N1 notifiers ----------  
  void N1_ObjectChanged(int which){
    Element::N1_ObjectChanged(which);
    CHECK(NumDynamicChildren() == 0);
  }
  bool ElementNeedsSeparateLine() const { return true;}
  bool ChildNeedsSeparateLine(int which_child) const { 
    return (which_child==CHILD);}
  // ---------- data ----------  
};
struct DynamicLet : public DynamicElement {
  // ---------- L2 functions ----------  
  // ---------- const functions ----------  
  Let *GetLet() const { 
    return dynamic_cast<Let*>(Get());
  }
  bool NeedsLetViolation() const; // a let violation should exist.
  OMap GetIntroducedBinding(int which_child) const { 
    if (which_child != Let::CHILD) return OMap::Default();
    Map ret;
    ret[GetLet()->GetVariable()] = GetChildValue(Let::VALUE);
    return OMap::Make(ret);
  }
  Object ComputeValue() const { return GetChildValue(Let::CHILD);}
  // ---------- L1 functions ----------  
  void L1_CheckSetLetViolation(); // (programming with Dr. Seuss)
  void L1_Init(Element * static_parent, OMap binding) {
    DynamicElement::L1_Init(static_parent, binding);
    L1_CheckSetLetViolation();
  }
  // ---------- N1 notifiers ----------
  void N1_ChildValueChanged(int which_child, Object old_val, Object new_val) { 
    DynamicElement::N1_ChildValueChanged(which_child, old_val, new_val);
    L1_CheckSetLetViolation();
  }
  // ---------- data ----------  
};

struct DynamicPost : public DynamicElement {
  // ---------- L2 functions ----------  
  void AddCorrectPosting();
  void AddPosting(OTuple t, OTime time);
  void RemovePosting();
  void SetPostingTime(OTime new_time);
  void SetCorrectPostingTime() { SetPostingTime(time_);}
  // ---------- const functions ----------  
  bool NeedsPostViolation() const;
  Object ComputeTuple() const{
    DynamicElement * expr = GetSingleChild(Post::TUPLE);
    if (!expr) return Object();
    return expr->GetValue();
  }
  OwnedPosting * GetOwnedPosting() const { return posting_;}
  Record GetRecordForDisplay() const;
  Object ComputeValue() const { return GetChildValue(Post::TUPLE);}
  // ---------- L1 functions ----------  
  void L1_Init(Element * static_parent, OMap binding) {
    posting_ = NULL;
    DynamicElement::L1_Init(static_parent, binding);
    L1_CheckSetPostViolation();
  }
  void L1_Erase() {
    if (posting_) posting_->L1_Erase();
    DynamicElement::L1_Erase();
  }
  // see if it's perfect, then create or remove violation if necessary.
  void L1_CheckSetPostViolation();
  // ---------- N1 notifiers ----------
  void N1_ChildValueChanged(int which_child, Object old_val, Object new_val) { 
    DynamicElement::N1_ChildValueChanged(which_child, old_val, new_val);
    L1_CheckSetPostViolation();
  }  
  // ---------- data ----------  
  OwnedPosting * posting_;
};

// if the condition does not evaluate to Boolean::Make(false), 
// then the ON_TRUE should be executed.
struct DynamicIf : public DynamicElement {
  // ---------- L2 functions ----------  
  // ---------- const functions ----------  
  If *GetIf() const { 
    return dynamic_cast<If*>(Get());
  }
  bool ChildShouldExist(int which_child) const;
  Object ComputeValue() const { 
    if (ChildShouldExist(If::ON_TRUE)) 
      return GetChildValue(If::ON_TRUE);
    if (ChildShouldExist(If::ON_FALSE)) 
      return GetChildValue(If::ON_FALSE);
    return NULL;
  }

  // ---------- L1 functions ----------  
  void L1_Init(Element * static_parent, OMap binding) {
    DynamicElement::L1_Init(static_parent, binding);
  }
  // ---------- N1 notifiers ----------
  void N1_ChildValueChanged(int which_child, Object old_val, Object new_val) {
    if (old_val == new_val) return;
    DynamicElement::N1_ChildValueChanged(which_child, old_val, new_val);
    if (which_child == If::CONDITION) L1_CheckSetChildViolation();
  }
  // ---------- data ----------  
};


struct DynamicEqual : public DynamicElement {
  Object ComputeValue() const;
};

struct DynamicSum : public DynamicElement {
  Object ComputeValue() const;
};

struct ToString : public Element {
   #define ToStringChildNameList {	 		\
    ITEM(ARG),						\
      };
  CLASS_ENUM_DECLARE(ToString, ChildName);
  #define ToStringObjectNameList {				\
    };
  CLASS_ENUM_DECLARE(ToString, ObjectName);
  DECLARE_FUNCTION_ENUMS;
  Function GetFunction() const { return TOSTRING;}
  bool ChildrenGoInTuple() const { return true;}
};
struct DynamicToString : public DynamicElement {
  Object ComputeValue() const;
};

struct Concat : public Element {
   #define ConcatChildNameList {	 		\
      };
  CLASS_ENUM_DECLARE(Concat, ChildName);
   #define ConcatObjectNameList {		\
    };
  CLASS_ENUM_DECLARE(Concat, ObjectName);
  DECLARE_FUNCTION_ENUMS;
  Function GetFunction() const { return CONCAT;}
  bool HasVariableNumChildren() const { return true; }
};
struct DynamicConcat : public DynamicElement {
  Object ComputeValue() const;
};

struct DynamicMakeTuple : public DynamicElement {
  Object ComputeValue() const;
};

struct Nth : public Element {
   #define NthChildNameList {	 		\
    ITEM(TUPLE),						\
    ITEM(N),						\
      };
  CLASS_ENUM_DECLARE(Nth, ChildName);
  #define NthObjectNameList {				\
    };
  CLASS_ENUM_DECLARE(Nth, ObjectName);
  DECLARE_FUNCTION_ENUMS;
  Function GetFunction() const { return NTH;}
  bool ChildrenGoInTuple() const { return true;}
};
struct DynamicNth : public DynamicElement {
  Object ComputeValue() const;
};



struct Choose : public Element { 
   #define ChooseChildNameList {	 		\
    ITEM(STRATEGY),					\
      };
  CLASS_ENUM_DECLARE(Choose, ChildName);
  #define ChooseObjectNameList {		\
    };
  CLASS_ENUM_DECLARE(Choose, ObjectName);
  DECLARE_FUNCTION_ENUMS;
  virtual Function GetFunction() const { return CHOOSE;}
};

// For now, we'll make choice_->value_ always match value_, and 
// choice_->parameter_ always match GetChildValue(PARAMETER). 
// This cuts down on violations, but we may need a new violation if the 
// choice is impossible for that parameter.  
struct DynamicChoose : public DynamicElement {
  // ---------- L2 functions ----------  

  // ---------- const functions ----------  
  Object ComputeValue() const;
  
  // ---------- L1 functions ----------  
  void L1_Init(Element * static_parent, OMap binding);
  void L1_Erase();
  bool L1_TryMakeChoice(OTuple strategy, Object value);
  bool L1_TryMakeCorrectChoice();

  // ---------- N1 Notifiers ----------  
  void N1_ChildValueChanged(int which_child, Object old_val, Object new_val) {
    DynamicElement::N1_ChildValueChanged(which_child, old_val, new_val);
    L1_TryMakeCorrectChoice();
  }
  void N1_StoredValueChanged() {
    L1_TryMakeCorrectChoice();
    DynamicElement::N1_StoredValueChanged();
  }

  // ---------- data ----------  
  Choice * choice_; // The current choice.
};

struct DynamicConstant : public DynamicElement {
  // ---------- L2 functions ----------  
  // ---------- const functions ----------  
  Object ComputeValue() const;
  // ---------- L1 functions ----------  
  // ---------- data ----------  
};


template <class T> T * MakeDynamicElement(Element *static_parent, 
					  OMap binding) {
  CHECK(static_cast<DynamicElement *>((T *)NULL) == NULL);
  return New<T>(static_parent, binding);
}

DynamicElement * MakeDynamicElement(Element *static_parent, 
				    OMap binding);

*/

#endif
