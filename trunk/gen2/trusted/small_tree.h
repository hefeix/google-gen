// Copyright (C) 2006 Google Inc. and Georges Harik
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
// Author: Noam Shazeer and Georges Harik

#ifndef _SMALL_SET_H_
#define _SMALL_SET_H_

#include <vector>
#include <iostream>
#include <string>
#include <map>
#include <sstream>
#include "shorthand.h"
#include "ranktree.h" // they share the projection code
#include "hash.h"

/*
  These are very small sets and maps which are implemented in vectors.
*/

using namespace std;

template<class Projection> 
class SmallTree {
  typedef typename Projection::KeyType Key;
  typedef typename Projection::DataType Data;
  vector<Data> v_;

  bool ProjectLT(const Data & d1, const Data & d2) {
    return (Projection::ToKey(d1) < Projection::ToKey(d2));
  }

 public:
  typedef typename vector<Data>::iterator iterator;
  typedef typename vector<Data>::const_iterator const_iterator;

  const vector<Data> & GetVector() { return v_;} 
  uint size() const { return v_.size();}

  iterator insert(const Data & d) {
    iterator run = begin();
    for (; run!=end(); ++run) {
      if (Projection::ToKey(d) < Projection::ToKey(*run)) break;
      if (Projection::ToKey(d) == Projection::ToKey(*run)) {
	*run = d;
	return run;
      }
    }
    return v_.insert(run, d);
  }
  void erase(const Key & k) {
    forall(run, *this) {
      if (Projection::ToKey(*run) == k) {
	v_.erase(run);
	return;
      }
    }
  }
  iterator begin() { return v_.begin();}
  iterator end() { return v_.end();}
  const_iterator begin() const { return v_.begin();}
  const_iterator end() const { return v_.end();}
  const_iterator find(const Key &k) const{ 
    forall(run, *this) if (Projection::ToKey(*run) == k) return run;
    return end();
  }
  iterator find(const Key &k) { 
    forall(run, *this) if (Projection::ToKey(*run) == k) return run;
    return end();
  }
  
  iterator nth(uint i){ return begin()+i;}
  const_iterator nth(uint i) const{ return begin()+i; }

  uint index(const iterator& iter) const { return iter-begin();}
  uint index(const const_iterator& iter) const { return iter-begin(); }
  
  uint num_lt(const Key & k) const {
    uint ret;
    forall(run, *this) if (Projection::ToKey(*run) < k) ret++;
    return ret;
  }
  uint num_ge(const Key &k) const { return size() - num_lt(k); }
  // range includes lower bound but not upper bound
  uint range_count(const Key & lower, const Key & upper) const {
    return num_lt(upper) - num_lt(lower);
  }
  void clear() {
    v_->clear();
  }
  template<class I> void insert(const I & b, const I & e) {
    for (I run=b; run!=e; ++run) {
      insert(*run);
    }
  }
  string ToString() {
    ostringstream output;
    output << "{ ";
    forall(run, *this) {
      output << *run << " ";
    }
    output << "}" << endl;
    return output.str();
  }
  void Check() {
    for (uint i=0; i+1<size() ; i++) 
      CHECK(Projection::ToKey(v_[i]) < Projection::ToKey(v_[i+1]));
  }
};

template <class T>
class small_set : public SmallTree<IdentityProjection<T> >{
 public:
  typedef typename SmallTree<IdentityProjection<T> >::iterator iterator;
  typedef typename SmallTree<IdentityProjection<T> >::const_iterator 
    const_iterator;
};

template<class K, class V>
class small_map : public SmallTree<FirstProjection<K,V> >{
 public:
  typedef typename SmallTree<FirstProjection<K,V> >::iterator iterator;
  typedef typename SmallTree<FirstProjection<K,V> >::const_iterator 
    const_iterator;
  V & operator [](const K & k) {
    iterator look = find(k);
    if (look != SmallTree<FirstProjection<K,V> >::end()) return look->second;
    return insert(make_pair(k, V()))->second;
  }
};

template<class A> bool operator %(const small_set<A> & m, const A & a){
  return m.find(a) != m.end();
}

template <class A, class B> B * operator %(small_map<A, B> & m, const A & a){
  typedef typeof(m.begin()) iter_type;
  iter_type look = m.find(a);
  if (look != m.end()) return &(look->second);
  return 0;
}

template<class A, class B> const B * operator %(const small_map<A, B> & m, 
						const A & a){
  typedef typeof(m.begin()) iter_type;
  iter_type look = m.find(a);
  if (look != m.end()) return &(look->second);
  return 0;
}

template<class Projection> 
bool operator <(const SmallTree<Projection> & x, 
		const SmallTree<Projection> & y) {
  return lexicographical_compare(x.begin(), x.end(), y.begin(), y.end());
}
template<class Projection> 
bool operator ==(const SmallTree<Projection> & x, 
		 const SmallTree<Projection> & y) {
  return ((x.size() == y.size()) && 
	  equal(x.begin(), x.end(), y.begin()) );
}
template<class Projection> 
bool operator !=(const SmallTree<Projection> & x, 
		 const SmallTree<Projection> & y) {
  return (!(x==y));
}


template<class SK, class MK, class V> 
  small_map<MK, V> Restrict(const small_map<MK,V> & m, 
			    const small_set<SK> & s) {
  small_map<MK, V> ret;
  forall(run, m) {
    if (s % SK(run->first)) ret[run->first] = run->second;
  }
  return ret;
}

template <class A, class B> uint32 Hash32(const small_set<A> & s, 
					       uint32 level = 0){
  return Hash32Iterator(s.begin(), s.end(), level);
}
DEFINE_HASH_CLASS_1(small_set);

template <class A, class B> uint32 Hash32(const small_map<A,B> & s, 
					       uint32 level = 0){
  return Hash32Iterator(s.begin(), s.end(), level);
}
DEFINE_HASH_CLASS_2(small_map);

void TestSmallSet();
void TestSmallMap();

template<class T> small_set<T> SingletonSmallSet(const T & t) {
  small_set<T> ret;
  ret.insert(t);
  return ret;
}

//template <class T>
//typedef SmallTree<IdentityProjection<T> > smallset;


//template <class K, class V> 
//class o

#endif
