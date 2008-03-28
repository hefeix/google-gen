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
  FUNCTION(Pass, PASS)						\
       FUNCTION(Constant, CONSTANT)				\
       FUNCTION(Substitute, SUBSTITUTE)				\
       FUNCTION(Let, LET)					\
       FUNCTION(Post, POST)					\
       FUNCTION(Unpost, UNPOST)					\
       FUNCTION(On, ON)						\
       FUNCTION(Match, MATCH)					\
       FUNCTION(MatchCount, MATCH_COUNT)			\
       FUNCTION(MakeTuple, MAKETUPLE)				\
       FUNCTION(Equal, EQUAL)				     	\
       FUNCTION(Not, NOT)					\
       FUNCTION(Sum, SUM)					\
       FUNCTION(If, IF)					       	\
       FUNCTION(Cout, COUT)					\
       FUNCTION(Nth, NTH)					\
       FUNCTION(Delay, DELAY)					\
       FUNCTION(Choose, CHOOSE)					\
       FUNCTION(Repeat, REPEAT)					\
    
/*
  FUNCTION(AddCode, ADD_CODE)					\
  FUNCTION(NewFlake, NEW_FLAKE)					\
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
  virtual void Init(Element *parent) { 
    Base::Init(); 
    object_ = NULL; 
    parent_ = parent;
    incoming_stack_depth_ = 0;
    if (parent_) {
      int which_child = parent_->children_.size();
      incoming_stack_depth_ = parent_->GetChildStackDepth(which_child);
      incoming_variables_ = parent->GetOutgoingVariables(which_child);
      parent_->children_.push_back(this);
    }
  }
  void Erase() { Base::Erase(); }

  // Run things
  virtual Object Execute(Thread & thread);
  virtual Object ComputeReturnValue(Thread & thread, Tuple results) {
    CHECK(false); return NULL;
  }
  
  // Verification code
  bool VerifyNode() const{
    return (RequiredNumChildren() == NumChildren());
  }

  bool VerifyTree() {
    if (!VerifyNode()) return false;
    for (uint c=0; c<children_.size(); c++) {
      Element * child = children_[c];
      if (!child->VerifyTree()) return false;
    }
    return true;
  }
  
  // This is for parsing
  static Function TypeKeywordToFunction(Keyword type) {
    return StringToFunction(Upcase(type.Data())); 
  }
 
  // Printing
  Record GetRecordForDisplay() const;
  OTuple SimpleProgramTree() const;
  virtual string PrettyProgramTree(int indent = 0) const;
  string TextIdentifier() const { return PrettyProgramTree(); }
  
  // Simple accessor functions for the static tree
  Element * GetParent() const { return parent_; }
  Element * GetChild (int which) const { 
    CHECK(which >= 0 && which < NumChildren());
    return children_[which]; 
  }
  virtual bool HasVariableNumChildren() const { return false; }
  virtual int RequiredNumChildren() const = 0;
  virtual int ComputeRequiredNumChildren() const { CHECK(false); return 0;}
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
  
  virtual bool HasObject() const { return false; }
  virtual void SetObject(Object o) { object_ = o; }
  
  // Accessing names of children pnemonically
  // These are virtual but don't need to be defined by subclasses other
  // than by the macros
  virtual int StringToChild(string s) const = 0;
  virtual string ChildToString(int c) const = 0;

  // Sometimes overridden

  // for pretty code
  virtual bool ChildrenGoInTuple() const { return false; }
  virtual bool ElementNeedsSeparateLine() const { return false; }
  virtual bool ChildNeedsSeparateLine(int which_child) const { return false; }

  int GetIncomingStackDepth() const { return incoming_stack_depth_;}
  const Tuple & GetIncomingVariables() const { return incoming_variables_;}
  Tuple GetOutgoingVariables(int which_child) const {
    return Concat(GetIncomingVariables(), GetIntroducedVariables(which_child));
  }
  int GetChildStackDepth(int which_child) const { 
    return incoming_stack_depth_ + GetNumIntroducedVariables(which_child); }
  virtual int GetNumIntroducedVariables(int which_child) const { return 0; }
  virtual Tuple GetIntroducedVariables(int which_child) const { return Tuple();}
  int StackPosition(Variable variable) const { 
    for (uint i=0; i<incoming_variables_.size(); i++) 
      if (variable == incoming_variables_[i]) return i;
    return -1;
  }
  bool IsBound(Variable variable) const { 
    return (StackPosition(variable) != -1);
  }
  
  // Doesn't need to be overridden
  Base::Type GetBaseType() const { return Base::ELEMENT; }

  static void StaticInit();

  // ---------- data ----------  
  Element * parent_;
  vector<Element *> children_;
  int incoming_stack_depth_;
  Tuple incoming_variables_;
  Object object_; // used in some types of elements
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
      ComputeRequiredNumChildren():NumChildNames();}

template <class T> T * MakeElement(Element *parent) {
  CHECK(static_cast<Element *>((T *)NULL) == NULL);
  return New<T>(parent);
}

struct MatchBaseElement : public Element {
  bool ElementNeedsSeparateLine() const { return true; }
  bool ChildNeedsSeparateLine(int which_child) const { return true; }

  // Figure out the tuple we are searching for (by executing children)
  // then do something subclass-specific
  Object Execute(Thread & thread);

  // the subclass-specific thing
  virtual Object SubclassExecute(Thread & thread, 
				 const Tuple &wildcard_tuple) = 0;

  // Execute the extra child for one tuple that matches the search.
  Object RunForMatchingTuple(Thread &thread, Blackboard::Row *row, 
			     int tuple_num);

  string PrettyProgramTree(int indent) const;
  
  bool HasObject() const { return true; }
  bool HasVariableNumChildren() const { return true; }
  int ComputeRequiredNumChildren() const {
    // This is a minor hack so that pretty-parsing works.
    // When we're pretty-parsing one of these things, we don't know the object
    // or the number of children until after we add all of the children except
    // the extra child. 
    if (object_ == NULL) return -1;
    return wildcard_tuple_.size() - num_wildcards_ + (HasExtraChild()?1:0);
  }
  // the extra child is always at the end.
  virtual bool HasExtraChild() const { return true; }
  Element * GetExtraChild() const { 
    CHECK(HasExtraChild()); 
    return GetChild(NumChildren()-1); 
  }
  int GetNumIntroducedVariables(int which_child) const { 
    if (HasExtraChild() && 
	((1+which_child) == ComputeRequiredNumChildren())) {
      return num_wildcards_;
    }
    return 0;
  }
  Tuple GetIntroducedVariables(int which_child) const { 
    if (HasExtraChild() && 
	((1+which_child) == ComputeRequiredNumChildren())) {
      // cout << "introducing which_child=" << which_child << endl;
      return introduced_variables_;
    }
    return Tuple();
  }
  
  Tuple wildcard_tuple_;
  int num_wildcards_;
  Tuple introduced_variables_;

  void SetObject(Object object) {
    Element::SetObject(object);
    Tuple t = OTuple(object_).Data();
    forall(run, t) {
      Object o = *run;
      if (o.GetType() == Object::VARIABLE) {
	CHECK(!IsBound(o));
	introduced_variables_.push_back(o);
      }
      else CHECK(o == NULL);
    }
    wildcard_tuple_ = VariablesToWildcards(OTuple(object_).Data());
    num_wildcards_ = NumWildcards(wildcard_tuple_);
  }
};

struct OnElement : public MatchBaseElement {
#define OnElementChildNameList {};
  CLASS_ENUM_DECLARE(OnElement, ChildName);
  DECLARE_FUNCTION_ENUMS;
  
  virtual Function GetFunction() const { return ON; }
  Object SubclassExecute(Thread & thread, const Tuple & wildcard_tuple);
};

struct MatchElement : public MatchBaseElement {
#define MatchElementChildNameList {};
  CLASS_ENUM_DECLARE(MatchElement, ChildName);
  DECLARE_FUNCTION_ENUMS;
  
  Function GetFunction() const { return MATCH; }
  Object SubclassExecute(Thread & thread, const Tuple & wildcard_tuple);
};

struct MatchCountElement : public MatchBaseElement {
#define MatchCountElementChildNameList {};
  CLASS_ENUM_DECLARE(MatchCountElement, ChildName);
  DECLARE_FUNCTION_ENUMS;
  
  Function GetFunction() const { return MATCH_COUNT; }
  Object SubclassExecute(Thread & thread, const Tuple & wildcard_tuple);
  bool HasExtraChild() const { return false; }
};

struct PassElement : public Element {
#define PassElementChildNameList {};
  CLASS_ENUM_DECLARE(PassElement, ChildName);
  DECLARE_FUNCTION_ENUMS;
  Function GetFunction() const { return PASS;}
  Object Execute(Thread & thread) { return NULL;}
};

struct PostElement : public Element {
#define PostElementChildNameList { ITEM(TUPLE) };
  CLASS_ENUM_DECLARE(PostElement, ChildName);
  DECLARE_FUNCTION_ENUMS;
  Function GetFunction() const { return POST;}
  bool ElementNeedsSeparateLine() const { return true; }
  Object Execute(Thread & thread);
};

struct UnpostElement : public Element {
#define UnpostElementChildNameList { ITEM(TUPLE) };
  CLASS_ENUM_DECLARE(UnpostElement, ChildName);
  DECLARE_FUNCTION_ENUMS;
  Function GetFunction() const { return UNPOST;}
  bool ElementNeedsSeparateLine() const { return true; }
  Object Execute(Thread & thread);
};

struct ConstantElement : public Element {
#define ConstantElementChildNameList {};
  CLASS_ENUM_DECLARE(ConstantElement, ChildName);
  DECLARE_FUNCTION_ENUMS;
  Function GetFunction() const { return CONSTANT; }
  string PrettyProgramTree(int indent) const;
  Object Execute(Thread & t) { return object_; }
  bool HasObject() const { return true; }
};

struct MakeTupleElement : public Element {
#define MakeTupleElementChildNameList {};
  CLASS_ENUM_DECLARE(MakeTupleElement, ChildName);
  DECLARE_FUNCTION_ENUMS;
  Function GetFunction() const { return MAKETUPLE; }
  bool HasVariableNumChildren() const { return true; }
  bool ChildrenGoInTuple() const { return true; }
  bool ChildNeedsSeparateLine(int which_child) const {
    for (int c = 0; c<NumChildren(); c++) {
      const Element * e = GetChild(c);
      if (e && e->ElementNeedsSeparateLine()) return true;
    }
    return false;
  }
  Object ComputeReturnValue(Thread & thread, Tuple results) {
    return OTuple::Make(results);
  }
  bool HasObject() const { return true;}  
  int ComputeRequiredNumChildren() const { return Integer(object_).Data(); }
  void SetObject(Object object) { 
    Element::SetObject(object);
    CHECK(object_.GetType() == Object::INTEGER);
  }
};

struct SubstituteElement : public Element {
#define SubstituteElementChildNameList { };
  CLASS_ENUM_DECLARE(SubstituteElement, ChildName);
  DECLARE_FUNCTION_ENUMS;
  bool HasObject() const { return true;}
  void SetObject(Object object) { 
    Element::SetObject(object);
    CHECK(object_.GetType() == Object::VARIABLE);
    CHECK(IsBound(object_));
    stack_position_ = StackPosition(object_);
  }
  
  string PrettyProgramTree(int indent) const;
  virtual Function GetFunction() const { return SUBSTITUTE;}
  string ToString() const;
  Object Execute(Thread &thread) { return thread.stack_[stack_position_];}
  int stack_position_;
};

struct EqualElement : public Element {
#define EqualElementChildNameList { ITEM(LHS), ITEM(RHS), };
  CLASS_ENUM_DECLARE(EqualElement, ChildName);
  DECLARE_FUNCTION_ENUMS;
  virtual Function GetFunction() const { return EQUAL; }
  Object ComputeReturnValue(Thread & thread, Tuple results) {
    if (results[0] == results[1]) return TRUE;
    return FALSE;
  }
};

struct NotElement : public Element {
#define NotElementChildNameList { ITEM(CHILD) };
  CLASS_ENUM_DECLARE(NotElement, ChildName);
  DECLARE_FUNCTION_ENUMS;
  virtual Function GetFunction() const { return NOT; }
  Object ComputeReturnValue(Thread & thread, Tuple results) {
    if (ToBoolean(results[CHILD])) return FALSE;
    return TRUE;
  }
};

struct SumElement : public Element {
#define SumElementChildNameList { ITEM(TUPLE), };
  CLASS_ENUM_DECLARE(SumElement, ChildName);
  DECLARE_FUNCTION_ENUMS;
  Function GetFunction() const { return SUM; }
  Object ComputeReturnValue(Thread & thread, Tuple results) {
    if (results[0].GetType() != Object::OTUPLE) return NULL;
    const Tuple & t = OTuple(results[0]).Data();
    int sum = 0;
    forall(run, t) {
      if (run->GetType() == Object::INTEGER) sum += Integer(*run).Data();
    }
    return Integer::Make(sum);
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
  Function GetFunction() const { return IF; }
  bool ChildNeedsSeparateLine(int which_child) const {
    if (which_child == CONDITION) return false;
    for (int c = ON_TRUE; c<NumChildren(); c++) {
      Element * e = GetChild(c);
      if (e && e->ElementNeedsSeparateLine()) return true;
    }
    return false;
  }
  Object Execute(Thread & thread);
};

struct CoutElement : public Element {
#define CoutElementChildNameList { ITEM(CHILD), };
  CLASS_ENUM_DECLARE(CoutElement, ChildName);
  DECLARE_FUNCTION_ENUMS;
  Function GetFunction() const { return COUT; }
  Object ComputeReturnValue(Thread & thread, Tuple results) {
    thread.execution_->output_ += results[0].ToString() + '\n';
    return results[0];
  }
};

struct NthElement : public Element {
#define NthElementChildNameList { ITEM(N), ITEM(TUPLE)};
  CLASS_ENUM_DECLARE(NthElement, ChildName);
  DECLARE_FUNCTION_ENUMS;
  Function GetFunction() const { return NTH; }
  Object ComputeReturnValue(Thread & thread, Tuple results) {
    if (results[N].GetType() != Object::INTEGER) return NULL;
    if (results[TUPLE].GetType() != Object::OTUPLE) return NULL;
    int n = Integer(results[N]).Data();
    const Tuple & t = OTuple(results[TUPLE]).Data();
    if (n<0 || n>=(int)t.size()) return NULL;
    return t[n];
  }
};

struct LetElement : public Element {
#define LetElementChildNameList { ITEM(VALUE), ITEM(CHILD) };
  CLASS_ENUM_DECLARE(LetElement, ChildName);
  DECLARE_FUNCTION_ENUMS;
  Function GetFunction() const { return LET;}
  bool ElementNeedsSeparateLine() const { return true;}
  bool ChildNeedsSeparateLine(int which_child) const { 
    return (which_child==CHILD);}
  Object Execute(Thread & thread);
  bool HasObject() const { return true; }
  int GetNumIntroducedVariables(int which_child) const { 
    if (which_child == CHILD) return 1; return 0;
  }
  Tuple GetIntroducedVariables(int which_child) const { 
    Tuple ret;
    if (which_child == CHILD) ret.push_back(object_);
    return ret;
  }
  void SetObject(Object object) { 
    Element::SetObject(object);
    CHECK(object_.GetType() == Object::VARIABLE);
    CHECK(!IsBound(object_));
  }
};

struct RepeatElement : public Element {
#define RepeatElementChildNameList { ITEM(N), ITEM(CHILD) };
  CLASS_ENUM_DECLARE(RepeatElement, ChildName);
  DECLARE_FUNCTION_ENUMS;
  Function GetFunction() const { return REPEAT;}
  bool ElementNeedsSeparateLine() const { return true;}
  bool ChildNeedsSeparateLine(int which_child) const { 
    return (which_child==CHILD);}
  Object Execute(Thread & thread);
  bool HasObject() const { return true; }
  int GetNumIntroducedVariables(int which_child) const { 
    if (which_child == CHILD) return 1; return 0;
  }
  Tuple GetIntroducedVariables(int which_child) const { 
    Tuple ret;
    if (which_child == CHILD) ret.push_back(object_);
    return ret;
  }
  void SetObject(Object object) { 
    Element::SetObject(object);
    CHECK(object_.GetType() == Object::VARIABLE);
    CHECK(!IsBound(object_));
  }
};

struct DelayElement : public Element {
#define DelayElementChildNameList { ITEM(DIMENSION), ITEM(CHILD) };
  CLASS_ENUM_DECLARE(DelayElement, ChildName);
  DECLARE_FUNCTION_ENUMS;
  Function GetFunction() const { return DELAY;}
  bool ElementNeedsSeparateLine() const { return true;}
  bool ChildNeedsSeparateLine(int which_child) const { 
    return (which_child==CHILD);}
  Object Execute(Thread & thread);
};

struct ChooseElement : public Element { 
#define ChooseElementChildNameList { ITEM(DISTRIBUTION) };
  CLASS_ENUM_DECLARE(ChooseElement, ChildName);
  DECLARE_FUNCTION_ENUMS;
#define ChooseElementDistributionTypeList { \
  ITEM(ONE_ELEMENT),						\
    ITEM(BOOL),							\
    ITEM(QUADRATIC_UINT),	 				\
    ITEM(BLACKBOARD),						\
    ITEM(NEW_FLAKE),						\
    ITEM(ANY_FLAKE)};
  CLASS_ENUM_DECLARE(ChooseElement, DistributionType);
  
  void Init(Element *parent) {
    Element::Init(parent);
    choice_counter_ = 0;
  }

  static vector<Keyword> distribution_type_keywords_;
  static void InitDistributionTypeKeywords();
  static Keyword DistributionTypeToKeyword(DistributionType s) { 
    return distribution_type_keywords_[s];}
  static DistributionType KeywordToDistributionType(Keyword k) {
    for (int i=0; i<NumDistributionTypes(); i++) 
      if (k == distribution_type_keywords_[i]) return (DistributionType)i;
    return (DistributionType)(-1);
  }
  Function GetFunction() const { return CHOOSE;}
  
  // first element of return value is the choice
  // second element is the likelihood of that choice. 
  // If suggestion is non-null, forces the choice to be equal to *suggestion
  // if that has non-zero likelihood
  static pair<Object, double> Choose(Execution *execution,
				     Object distribution, 
				     const Object *suggestion);


  Object ComputeReturnValue(Thread & thread, Tuple results);
  int choice_counter_;
};

/*
struct RandomBoolElement : public Element {
#define RandomBoolElementChildNameList { ITEM(PRIOR), };
  CLASS_ENUM_DECLARE(RandomBoolElement, ChildName);
  DECLARE_FUNCTION_ENUMS;
  Function GetFunction() const { return RANDOM_BOOL; }
  Object ComputeReturnValue(Thread & thread, Tuple results) {
    if (results[PRIOR].GetType() != Object::REAL) return NULL;
    double prior = Real(results[PRIOR]).Data();
    bool ret = (RandomFraction() < prior);
    return Boolean::Make(ret);
  }
};
struct RandomUintElement : public Element {
#define RandomUintElementChildNameList { };
  CLASS_ENUM_DECLARE(RandomUintElement, ChildName);
  DECLARE_FUNCTION_ENUMS;
  Function GetFunction() const { return RANDOM_UINT; }
  Object ComputeReturnValue(Thread & thread, Tuple results) {
    int ret = RandomUintQuadratic();
    return Integer::Make(ret);
  }
};
*/


#endif
