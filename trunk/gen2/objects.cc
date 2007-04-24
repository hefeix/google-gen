#include <iostream>
#include "objects.h"

Keyword WILDCARD;

void InitKeywords(){
  WILDCARD = Keyword::Make("*");  
};

// There must be some black magic going on here, but the compiler is happy.
template<ObjectType OT, class D>
map<D, class SpecificObject<OT, D>::Definition *> 
SpecificObject<OT, D>::unique_;

template<>
string Flake::Definition::ToStringSpecific(bool verbose) const { return data_; }

template<>
string Keyword::Definition::ToStringSpecific(bool verbose) const { return data_; }

template<>
string Variable::Definition::ToStringSpecific(bool verbose) const { 
  if (data_ < 26)
    return string() + (char('a' + data_));
  return "v" + itoa(data_);
}

template<>
string OTuple::Definition::ToStringSpecific(bool verbose) const {
  string ret = "(";
  for (uint i=0; i<data_.size(); i++) {
    ret += data_[i].ToString(verbose);
    if (i+1<data_.size()) ret += ", ";
  }
  ret += ")";
  return ret;
}

template<>
string OMap::Definition::ToStringSpecific(bool verbose) const {
  if (data_.size()==0) return ":";
  string ret = "[";
  forall(run, data_) {
    if (run != data_.begin()) ret += " ";
    ret += run->first.ToString(verbose) + ":" + run->second.ToString(verbose);
  }
  ret += "]";
  return ret;
}

template<>
string Boolean::Definition::ToStringSpecific(bool verbose) const {
  return data_?"true":"false";
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
string Escape::Definition::ToStringSpecific(bool verbose) const {
  return "\'" + data_.ToString();
}

bool IsNumericChar(char c){
  return (isdigit(c) || c=='.' || c=='-' || c=='+' || c=='e');
}
bool IsBeginningNumericChar(char c){
  return (isdigit(c) || c=='.' || c=='-');
}
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
  if (firstchar == ':') {
    o = OMap::Make(map<Object, Object>());
    return input;
  }
  if (IsOpenEnclosure(firstchar)) {
    bool ismap = false;
    bool isfirst = true;
    map<Object, Object> m;
    vector<Object> v;
    Object key;
    Object value;
    EatCommentsAndWhitespace(input);
    while (input.peek() != MatchingCloseEnclosure(firstchar)) {
      CHECK(input >> key);
      EatCommentsAndWhitespace(input);
      if (input.peek()==':') {
	if (isfirst) ismap = true;
	else CHECK(ismap);
	input.get();
	EatCommentsAndWhitespace(input);
	CHECK(input >> value);
	m[key] = value;
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

  CHECK(IsNameChar(firstchar));
  string s;
  s += firstchar;
  char c;
  while (IsNameChar(input.peek())){
    input >> c;
    s.push_back(c);
  }
  if (isupper(s[0])) { // it's a flake
    o = Flake::Make(s);
    return input;
  } 
  CHECK(islower(s[0]));
  if (s.size() == 1) {
    o = Variable::Make(s[0]-'a');
    return input;
  } 
  if (isdigit(s[1])) {
    CHECK(s[0]=='v')
    o = Variable::Make(atoi(&(s[1])));
    return input;
  } else {
    if (s=="true") {
      o = Boolean::Make(true);
      return input;
    }
    if (s=="false") {
      o = Boolean::Make(false);
      return input;
    }
    if (s=="null") {
      o = NULL;
      return input;
    }
    o = Keyword::Make(s);
    return input;
  }
  CHECK(false);
  return input;
}

int main() {
  InitKeywords();
  Object o;
  vector<Object> v;
  while (cin >> o) {
    //    cout << o.ToString() << endl;
    cout << o.ToString(true) << endl << endl;
    v.push_back(o);
    if (o==Keyword::Make("clear")) v = vector<Object>();
    if (o==Keyword::Make("done")) break;
  }
  /*Variable seven = Variable::Make(7);
  Flake n = Flake::Make("Noam");
  Flake n2 = Flake::Make("Noam");
  Flake g = Flake::Make("Georges");
  Flake g2 = g;
  cout << "n=n2 " << ((n==n2)?"T":"F") << endl;
  cout << "n=g " << ((n==g)?"T":"F") << endl;
  cout << "g=g2 " << ((g==g2)?"T":"F") << endl;
  vector<Object> v;
  v.push_back(n);
  v.push_back(g);
  v.push_back(seven);
  Tuple t = Tuple::Make(v);
  cout << n.ToString() << " " 
       << g.ToString() << " " 
       << seven.ToString() << " " 
       << t.ToString() << endl;
  */

}
