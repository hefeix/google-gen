#include <iostream>
#include "objects.h"
#include "numbers.h"

#undef ITEM
#define ITEM(x) #x
CLASS_ENUM_DEFINE(Object, Type);

OTime CREATION;
OTime NEVER;
Boolean TRUE;
Boolean FALSE;

#define KEYWORD(k, K) Keyword K;
ALL_KEYWORDS;
#undef KEYWORD

void Object::StaticInit(){
  cout << "Calling Object::Init" << endl;
  AddReservedWord("pattern");
  AddReservedWord("null");
  AddReservedWord("never");
  AddReservedWord("time");
  AddReservedWord("true");
  AddReservedWord("false");

  NEVER = OTime::Make(Time::Never());
  CREATION = OTime::Make(Time());
  TRUE = Boolean::Make(true);
  FALSE = Boolean::Make(false);

#define KEYWORD(k, K) K = AddKeyword( #k );
  ALL_KEYWORDS;
#undef KEYWORD
};

void Object::Destroy() {
  cout << "Calling DestroyConstants" << endl;
#define KEYWORD(k, K) K = NULL;
  ALL_KEYWORDS;
#undef KEYWORD
  WILDCARD = NULL;
  NEVER = NULL;
  CREATION = NULL;
  TRUE = NULL;
  FALSE = NULL;
}

static vector<Variable> g_int_to_variable_cache;

int VariableToInt(Variable v){ 
  if (v.Data().GetType() != Object::INTEGER) return -1;
  return Integer(v.Data()).Data();
}

Variable IntToVariable(int i) {
  CHECK(i>=0);
  while ((int)g_int_to_variable_cache.size() <= i) {
    int sz = g_int_to_variable_cache.size();
    Variable v = Variable::Make(Integer::Make(sz));
    g_int_to_variable_cache.push_back(v);
  }
  return g_int_to_variable_cache[i];
}



// There must be some black magic going on here, but the compiler is happy.
template<Object::Type OT, class D>
hash_map<D, class SpecificObject<OT, D>::Definition *> 
SpecificObject<OT, D>::unique_;

// there must be blacker magic going on that this line is necessary.
template<>
hash_map<BitSeq, OBitSeq::Definition *> OBitSeq::unique_ = hash_map<BitSeq, OBitSeq::Definition *>();


template<>
string Flake::Definition::ToStringSpecific(bool verbose) const { return data_; }

template<>
string Keyword::Definition::ToStringSpecific(bool verbose) const { return data_; }

template<>
string Variable::Definition::ToStringSpecific(bool verbose) const 
{ return "_" + data_.ToString();}

template<>
string OTuple::Definition::ToStringSpecific(bool verbose) const {
  string ret = "(";
  for (uint i=0; i<data_.size(); i++) {
    ret += data_[i].ToString(verbose);
    if (i+1<data_.size()) ret += " ";
  }
  ret += ")";
  return ret;
}

template<>
string OMap::Definition::ToStringSpecific(bool verbose) const {
  if (data_.size()==0) return "[:]";
  string ret = "[";
  forall(run, data_) {
    if (run != data_.begin()) ret += " ";
    ret += run->first.ToString(verbose) + ":" + run->second.ToString(verbose);
  }
  ret += "]";
  return ret;
}

template<>
string OPattern::Definition::ToStringSpecific(bool verbose) const { 
  Tuple t = PatternToTuple(data_);
  return "pattern " + OTuple::Make(t).ToString(verbose);
}

template<>
string Boolean::Definition::ToStringSpecific(bool verbose) const {
  return data_?"true":"false";
}

template<>
string String::Definition::ToStringSpecific(bool verbose) const {
  string ret= "\"";
  for (uint i=0; i<data_.size(); i++) {
    char c = data_[i];
    switch(c) {
    case '\n':
      ret += "\\n";
      break;
    case '\t':
      ret += "\\t";
      break;
    case '\\':
      ret += "\\\\";
      break;
    case '\"':
      ret += "\\\"";
      break;
    default:
      ret += c;
    }
  }
  ret += "\"";
  return ret;
}

template<>
string Integer::Definition::ToStringSpecific(bool verbose) const {
  return itoa(data_);
}
template<>
string Real::Definition::ToStringSpecific(bool verbose) const {
  string ret = dtoa(data_);
  if (ret.find('.')==string::npos && ret.find('e')==string::npos) 
    ret += ".0";
  return ret;
}

template<>
string OBitSeq::Definition::ToStringSpecific(bool verbose) const {
  return data_.ToString();
}
template<>
string OTime::Definition::ToStringSpecific(bool verbose) const {
  return data_.ToString();
}
template<>
string Escape::Definition::ToStringSpecific(bool verbose) const {
  return "\'" + data_.ToString();
}

bool IsNumericChar(char c){
  return (isdigit(c) || c=='.' || c=='-' || c=='+' || c=='e');
}
bool IsBeginningNumericChar(char c){
  return (isdigit(c) || c=='.' || c=='-');
}
bool IsBeginningNameChar(char c) { return isalpha(c);}
bool IsNameChar(char c){
  return (isalnum(c) || c=='_');
}
bool IsOpenEnclosure(char c){
  return (c=='(' || c=='[' || c=='{');
}
char MatchingCloseEnclosure(char c){
  switch(c){
  case '(': return ')';
  case '[': return ']';
  case '{': return '}';
  default: CHECK(false); return ' ';
  }
}
void EatCommentsAndWhitespace(istream & input) {
  input >> ws;
  if (input.peek() == '<') {
    char c;
    input >> c;
    int level = 1;
    while (level > 0) {
      CHECK(input >> c);
      if (c=='<') level++;
      if (c=='>') level--;
    }
    EatCommentsAndWhitespace(input);
  }
  if (input.peek()=='/') {
    input.get();
    CHECK(input.peek()=='/');
    char c;
    while (input.get(c)) if (c=='\n') break;
    EatCommentsAndWhitespace(input);
  }
  input >> ws;
}
void EatCommaAndCommentsAndWhitespace(istream & input) {
  EatCommentsAndWhitespace(input);
  if (input.peek() == ',') input.get();
  EatCommentsAndWhitespace(input);
}
istream & operator >>(istream & input, Object & o){
  EatCommentsAndWhitespace(input);
  char firstchar;
  if (!(input >> firstchar)) return input;
  if (IsOpenEnclosure(firstchar)) {
    bool ismap = false;
    bool isfirst = true;
    Map m;
    vector<Object> v;
    Object key;
    Object value;
    EatCommentsAndWhitespace(input);
    if (input.peek() == ':') {
      input.get();
      EatCommentsAndWhitespace(input);
      CHECK(input.peek() == MatchingCloseEnclosure(firstchar));
      input.get();
      o = OMap::Default();
      return input;      
    }
    while (input.peek() != MatchingCloseEnclosure(firstchar)) {
      CHECK(input >> key);
      EatCommentsAndWhitespace(input);
      if (input.peek()==':') {
	if (isfirst) ismap = true;
	else CHECK(ismap);
	input.get();
	EatCommentsAndWhitespace(input);
	CHECK(input >> value);
	m.push_back(make_pair(key, value));
      } else {
	CHECK(!ismap);
	v.push_back(key);
      }
      isfirst = false;
      EatCommaAndCommentsAndWhitespace(input);
    }
    input.get();
    if (ismap) o = OMap::Make(m);
    else o = OTuple::Make(v);
    return input;
  } 
  if (firstchar =='\'') { // it's an escape
    Object sub;
    CHECK(input >> sub);
    o = Escape::Make(sub);
    return input;
  } 
  if (firstchar == '*') {
    o = WILDCARD;
    return input;
  }
  if (firstchar == '#') {
    input.putback('#');
    BitSeq s;
    input >> s;
    o = OBitSeq::Make(s);
    return input;
  }
  if (firstchar == '\"') {
    string s;
    char c;
    while (input.get(c) && (c!='\"')) {
      if (c=='\\') {	
	input.get(c);
	switch(c) {
	case 'n':
	  s += '\n';
	  break;
	case 't':
	  s += '\t';
	  break;
	default:
	  s+=c;
	}
      } else {
	s+= c;
      }
    }
    o = String::Make(s);
    return input;
  }
  if (IsBeginningNumericChar(firstchar) ) {
    // it's an integer/real    
    string s;
    char c;
    s += firstchar;
    while (IsNumericChar(input.peek())) {
      input >> c;
      s+=c;
    }
    if (s.find('.')!=string::npos 
	|| s.find('e')!=string::npos) { // it's a double
      double d = atof(s.c_str());
      o = Real::Make(d);
      return input;
    } 
    // it's an integer
    int i = atoi(s.c_str());
    o = Integer::Make(i);
    return input;
  } 

  if (firstchar == '_'){
    Object sub;
    input >> sub;
    o = Variable::Make(sub); 
    return input;
  }

  // read a contiguous string of a-z A-Z and _s
  CHECK(IsBeginningNameChar(firstchar));
  string s;
  s += firstchar;
  char c;
  while (IsNameChar(input.peek())){
    input >> c;
    s.push_back(c);
  }

  // If it starts with an uppercase, it's a flake
  if (isupper(s[0])) {
    o = Flake::Make(s);
    return input;
  } 

  // These are particular objects.
  if (s=="true") {
    o = TRUE;
    return input;
  }
  if (s=="false") {
    o = FALSE;
    return input;
  }
  if (s=="never") {
    o = NEVER;
    return input;
  }

  // These are object type strings
  if (s=="time") {
    OMap t;
    input >> t;
    vector<pair<int64, int> > coordinates;
    forall(run, t.Data()) {
	Integer dim = run->first;
	Integer n = run->second;
	coordinates.push_back(make_pair(dim.Data(), n.Data()));	
	sort(coordinates.begin(), coordinates.end());
    }
    o = OTime::Make(Time(coordinates));
    return input;
  }
  if (s=="pattern") {
    OTuple t;
    input >> t;
    vector<OTuple> p;
    for (uint i=0; i<t.size(); i++) {
      p.push_back(t[i]);
    }
    o = OPattern::Make(p);
    return input;
  }
  if (s=="null") {
    o = NULL;
    return input;
  }

  // it is a keyword
  o = Keyword::Make(s);
  return input;
}

void ObjectsShell(){
  Object o;
  vector<Object> v;
  while (cin >> o) {
    //    cout << o.ToString() << endl;
    cout << o.ToString(true) << endl << endl;
    v.push_back(o);
    if (o==Flake::Make("Clear")) v = vector<Object>();
    if (o==Flake::Make("Done")) break;
  }
}
  
