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

#ifndef _RANKTREE_H_
#define _RANKTREE_H_

#include <vector>
#include <iostream>
#include <string>
#include <map>
#include "shorthand.h"

/*
  An orderset or rankmap is our reimplementation of stl set and map.
  Most of the stl functions work.  In addition, we have the following
  member functions:

  // returns pointer to the nth element
  (const_)iterator nth(uint n); 

  // first iterator not less than k
  (const_)iterator lower_bound(const Key &k);

  // returns the rank.
  uint index(const (const_)iterator &iter) const;

  // number of elements <k
  uint num_lt(const Key & k) const;

  // number of elements >=k
  uint num_ge(const Key &k) const;
  
  // range includes lower bound but not upper bound
  uint range_count(const Key & lower, const Key & upper) const;
 
*/

using namespace std;

template <class NPtr> 
NPtr NextNode(NPtr current) {
  if (!current) return NULL;
  if (current->right_) { // you have a right child
    current = current->right_;
    while (current->left_) current = current->left_;
    return current;
  }
  // while you're a right child, head up
  while (current->WhichChild() == 1) {
    current = current->parent_;	
  }
  return current->parent_;
}

// Projcetions of data onto keys
template<class K>
struct IdentityProjection {
  typedef K KeyType;
  typedef K DataType;
  static const KeyType & ToKey(const DataType &k) { return k;}
  static const DataType & ToData(const KeyType &k) { return k;}
  static bool KeyIsData() { return true; }
};
template<class K, class V> 
  struct FirstProjection {
  typedef K KeyType;
  typedef pair<K,V> DataType;
  typedef V ValueType;
  
  static const KeyType & ToKey(const DataType &p) { return p.first;}  
  static DataType ToData(const KeyType & k) { return make_pair(k, V());}
  static bool KeyIsData() { return false; }
};


template<class Projection> 
class RankTree {
  typedef typename Projection::KeyType Key;
  typedef typename Projection::DataType Data;
  
  bool ProjectLT(const Data & d1, const Data & d2) {
    return (Projection::ToKey(d1) < Projection::ToKey(d2));
  }

 public:
  uint size() const { return root_?root_->subtree_size_:0;}

  void insert(const Data & d) {
    Node *n = GetAddNode(Projection::ToKey(d));
    if (!Projection::KeyIsData())
      n->data_ = d;
  }
  void erase(const Key & k) {
    Node *parent;
    Node ** look = search(k, &parent);
    if (*look) Delete(*look);
  }
  RankTree() {root_ = NULL;}
  RankTree(const RankTree<Projection> & other) {
    root_ = NULL;
    *this = other;
  }
  string ToString(){
    return "tree\n" + ToString(root_, "", "");
  }

 protected:
  enum Color {
    RED,
    BLACK,
  };

  struct Node{
    Data data_;
    Node * parent_;
    Node * left_;
    Node * right_;
    Color color_;
    uint subtree_size_;
    static Node * DeepCopy(Node * n) {
      if (n==NULL) return NULL;
      Node *ret = new Node(*n);
      if (n->left_) {
	ret->left_ = DeepCopy(n->left_);
	ret->left_->parent_ = ret;
      }
      if (n->right_) {
	ret->right_ = DeepCopy(n->right_);
	ret->right_->parent_ = ret;
      }
      return ret;
    }
    Node(const Node &o) 
      :data_(o.data_), parent_(o.parent_), left_(o.left_), right_(o.right_), 
       color_(o.color_), subtree_size_(o.subtree_size_) {}
    int WhichChild() const{ 
      if (!parent_) return 0;
      if (this == parent_->left_) return -1;
      return 1;
    }
    Node * Grandparent() {
      CHECK(parent_->parent_);
      // if (parent_ == NULL) return NULL;
      return parent_->parent_;
    }
    Node * Uncle() {
      CHECK(parent_);
      return parent_->Sibling();
    }
    Node * Sibling() const {
      CHECK(parent_);
      return ( (this==parent_->right_)?parent_->left_:parent_->right_ );
    }
    // where should point to the parent's left_ or right_, or to the
    // tree's root_
    Node(Node **where, Node *parent, const Data & data) {
      *where = this;
      parent_ = parent;
      left_ = right_ = NULL;
      color_ = RED;
      subtree_size_ = 0;
      data_ = data;
      for (Node *current = this; current; current = current->parent_) {
	current->subtree_size_++;
      }
    }   
    void Check() {
      if (parent_) CHECK(parent_->left_==this || parent_->right_==this);
      if (left_) CHECK(left_->parent_==this);
      if (right_) CHECK(right_->parent_==this);
      if (parent_) CHECK(parent_->color_ == BLACK || color_ == BLACK);
      CHECK(subtree_size_ == 1 + SubtreeSize(left_) + SubtreeSize(right_));
    }    
  };
  Node * root_;

  string ToString(Node * n, string lineheadtop, string lineheadbottom) {
    if (n==NULL) {
      return lineheadtop + "NULL\n";
    }
    string ret;
    ret += ToString(n->left_, lineheadtop+"   ", lineheadtop+" | ");
    ret += lineheadtop + n->data_ + " (" 
      + string((n->color_==RED)?"RED":"BLACK")
      + ")\n";
    ret += ToString(n->right_, lineheadbottom+" | ", lineheadbottom + "   ");
    return ret;
  } 
  
 public:
  
  // returns the number of black nodes in every path down from a given node.
  int CheckNode(Node *n) {
    if (n) {
      n->Check();
      int count1 = CheckNode(n->left_);
      int count2 = CheckNode(n->right_);
      CHECK(count1==count2);
      return count1 + (n->color_==BLACK)?1:0;
    }
    return 1;    
  }
  void Check() {
    CheckNode(root_);
    
  }


  struct iterator {
    Node * current_;
    iterator(Node *n) :current_(n){}
    void operator ++(){ current_ = NextNode(current_); }
    Data & operator *() {
      return current_->data_;
    }
    Data * operator->() {
      return &(**this);
    }
    bool operator ==(const iterator o) { return current_==o.current_;}
    bool operator !=(const iterator o) { return current_!=o.current_;}
  };
  struct const_iterator {
    Node * current_;
    const_iterator(Node *n) :current_(n){}
    const_iterator(const iterator &i) :current_(i.current_){}
    void operator ++(){ current_ = NextNode(current_); }
    const Data & operator *() {
      return current_->data_;
    }
    const Data * operator->() {
      return &(**this);
    }
    bool operator ==(const const_iterator & o) { return current_==o.current_;}
    bool operator !=(const const_iterator & o) { return current_!=o.current_;}
  };

  Node* _begin() const {
    Node * c = root_;
    if (c) while (c && c->left_) c = c->left_;
    return c;
  }
  iterator begin() { return _begin(); }
  const_iterator begin() const { return _begin(); }

  iterator end() { return NULL; }
  const_iterator end() const { return NULL; }

  Node* _find(const Key & k) const{
    Node ** look = search(k, NULL);
    if (*look) return *look;
    return NULL;
  }
  iterator find(const Key &k) { return _find(k);}
  const_iterator find(const Key &k) const{ return _find(k);}

  // first element greater than or equal to k
  Node * _lower_bound(const Key &k) const {
    Node *parent;
    Node **look = search(k, &parent);
    if (*look) return *look;
    if (!parent) return NULL;
    if (look == &(parent->left_)) {
      return parent;
    }
    Node *n = parent;
    while (n->WhichChild()) n = n->parent_;
    return n->parent_;
  }
  iterator lower_bound(const Key &k) { return _lower_bound(k); }
  const_iterator lower_bound(const Key &k) const { return _lower_bound(k); }

  Node * _nth(uint i) const {
    CHECK(i<size());
    Node *n = root_;
    while(1) {
      uint leftsize = SubtreeSize(n->left_);
      if (i<leftsize) n = n->left_;
      else if (i==leftsize) return n;
      else {
	n = n->right_;
	i -= (leftsize + 1);
      }
    }
  }
  iterator nth(uint i){ return _nth(i); }
  const_iterator nth(uint i) const{ return _nth(i); }

  uint _index(const Node * n) const{
    if (!n) return size();
    int ret = SubtreeSize(n->left_);
    while(n) {
      int which = n->WhichChild();
      n = n->parent_;
      if (which==1) ret += 1 + SubtreeSize(n->left_);
    }
    return ret;
  }
  uint index(const iterator& iter) const { return _index(iter.current_); }
  uint index(const const_iterator& iter) const { return _index(iter.current_); }
  
  uint num_lt(const Key & k) const {
    // TODO: can be made faster.
    const_iterator find = lower_bound(k);
    return index(find);
  }
  uint num_ge(const Key &k) const {
    return size() - num_lt(k);
  }
  // range includes lower bound but not upper bound
  uint range_count(const Key & lower, const Key & upper) const {
    return num_lt(upper) - num_lt(lower);
  }
  void clear() {
    RecursiveDelete(root_);
    root_ = NULL;
  }
  template<class I> void insert(const I & b, const I & e) {
    for (I run=b; run!=e; ++run) {
      insert(*run);
    }
  }
  void operator=(const RankTree & o) {
    clear();
    root_ = Node::DeepCopy(o.root_);
     //insert(o.begin(), o.end());
  }

 protected:
  // just blow the thing away
  void RecursiveDelete(Node *n) {
    if (!n) return;
    RecursiveDelete(n->left_);
    RecursiveDelete(n->right_);
    delete n;
  }


  void Delete(Node * n) { // deletes a node, maintaining the tree
    if (!(n->left_ && n->right_)) {
      DeleteOneChild(n);
      return;
    }
    Node * swap_in = n->left_;
    while (swap_in->right_) swap_in = swap_in->right_;
    // We swap the places of n and swap_in
    // We would have rather just swapped the data, but that would invalidate
    // iterators.
    // This code is a little delicate when swap_in == n->left_, 
    // but it works.
    
    if (swap_in->left_) swap(n->left_->parent_, swap_in->left_->parent_);
    else n->left_->parent_ = swap_in;
    swap(n->left_, swap_in->left_);
    swap(*GetPointerTo(swap_in), *GetPointerTo(n));
    swap(n->parent_, swap_in->parent_);
    if (swap_in->right_) swap(n->right_->parent_, swap_in->right_->parent_);
    else n->right_->parent_ = swap_in;
    swap(n->right_, swap_in->right_);
    swap(n->color_, swap_in->color_);
    swap(n->subtree_size_, swap_in->subtree_size_);
    DeleteOneChild(n);
  }
  void DeleteOneChild(Node *n) { // deletes a node with at most one child
    if (n->right_==NULL && n->left_ == NULL) {
      if (n->color_ == BLACK) DeleteCase1(n);
      ReplaceAndDestroy(n, NULL);
      return;
    }
    Node *child = (n->right_==NULL) ? n->left_ : n->right_;
    Color n_color = n->color_;
    ReplaceAndDestroy(n, child);
    if (n_color == BLACK) {
      if (ColorOf(child) == RED)
	child->color_ = BLACK;
      else
	DeleteCase1(child);
    }
  }
  Node ** GetPointerTo(Node *n) {
    int which = n->WhichChild();
    if (which==0) return &root_;
    if (which==1) return &(n->parent_->right_);
    return &(n->parent_->left_);
  }
  
  void RotateLeft(Node *n) {
    Node * newtop = n->right_;
    //CHECK(newtop);
    *(GetPointerTo(n)) = newtop;
    n->right_ = newtop->left_;
    if (n->right_) n->right_->parent_ = n;
    newtop->left_ = n;
    newtop->parent_ = n->parent_;
    n->parent_ = newtop;
    n->subtree_size_ = 1 + SubtreeSize(n->left_) + SubtreeSize(n->right_);
    newtop->subtree_size_ = 1 + SubtreeSize(newtop->left_) 
      + SubtreeSize(newtop->right_);
  }
  void RotateRight(Node *n) {
    Node * newtop = n->left_;
    //CHECK(newtop);
    *(GetPointerTo(n)) = newtop;
    n->left_ = newtop->right_;
    if (n->left_) n->left_->parent_ = n;
    newtop->right_ = n;
    newtop->parent_ = n->parent_;
    n->parent_ = newtop;
    n->subtree_size_ = 1 + SubtreeSize(n->left_) + SubtreeSize(n->right_);
    newtop->subtree_size_ = 1 + SubtreeSize(newtop->left_) 
      + SubtreeSize(newtop->right_);
  }

  void InsertCase1(Node *n) {
    if (n->parent_ == NULL) n->color_ = BLACK;
    else InsertCase2(n);
  }
  void InsertCase2(Node *n) {
    if (n->parent_->color_==BLACK) return;
    else InsertCase3(n);
  }
  void InsertCase3(Node *n) {
    Node *uncle = n->Uncle();
    if (uncle && (uncle->color_==RED)) {
      n->parent_->color_ = BLACK;
      uncle->color_ = BLACK;
      n->Grandparent()->color_ = RED;
      InsertCase1(n->Grandparent());
    } else InsertCase4(n);
  }
  void InsertCase4(Node *n) {
    if (n->WhichChild() == 1 && n->parent_->WhichChild() == -1) {
      RotateLeft(n->parent_);
      n = n->left_;
    } else if (n->WhichChild() == -1 && n->parent_->WhichChild() == 1) {
      RotateRight(n->parent_);
      n = n->right_;
    }
    InsertCase5(n);
  }
  void InsertCase5(Node *n) {
    Node *gp = n->Grandparent();
    n->parent_->color_ = BLACK;
    gp->color_ = RED;
    if (n->WhichChild() == -1 && n->parent_->WhichChild() == -1) {
      RotateRight(gp);
    } else {
      //CHECK(n->WhichChild() == 1 && n->parent_->WhichChild()==1);
      RotateLeft(gp);
    }
  }

  void DeleteCase1(Node *n) {
    if (n->parent_==NULL) return;
    DeleteCase2(n);
  }
  void DeleteCase2(Node *n) {
    Node *sib = n->Sibling();
    if (ColorOf(sib) == RED) {
      n->parent_->color_ = RED;
      sib->color_ = BLACK;
      if (n->WhichChild() == -1)
	RotateLeft(n->parent_);
      else
	RotateRight(n->parent_);
    }
    DeleteCase3(n);
  }
  void DeleteCase3(Node *n) {
    Node *sib = n->Sibling();
    if (n->parent_->color_ == BLACK &&
        sib->color_ == BLACK &&
        ColorOf(sib->left_) == BLACK && 
        ColorOf(sib->right_) == BLACK)
      {
        sib->color_ = RED;
        DeleteCase1(n->parent_);
      }
    else
      DeleteCase4(n);
  }
  void DeleteCase4(Node *n) {
    Node *sib = n->Sibling();
    if (n->parent_->color_ == RED &&
        sib->color_ == BLACK &&	
        ColorOf(sib->left_) == BLACK &&
	ColorOf(sib->right_) == BLACK) {
        sib->color_ = RED;
        n->parent_->color_ = BLACK;
    }
    else DeleteCase5(n);    
  }
  void DeleteCase5(Node *n) {
    Node *sib = n->Sibling();
    int which = n->WhichChild();
    if (which == -1 &&
        sib->color_ == BLACK &&
        ColorOf(sib->left_) == RED &&
        ColorOf(sib->right_) == BLACK) {
      sib->color_ = RED;
      sib->left_->color_ = BLACK;
      RotateRight(sib);
    }
    else if (which == 1 &&
             sib->color_ == BLACK &&
             ColorOf(sib->right_) == RED &&
             ColorOf(sib->left_) == BLACK)
      {
        sib->color_ = RED;
        sib->right_->color_ = BLACK;
        RotateLeft(sib);
      }
    DeleteCase6(n);    
  }
  void DeleteCase6(Node *n) {
    Node *sib = n->Sibling();
    sib->color_ = n->parent_->color_;
    n->parent_->color_ = BLACK;
    if (n == n->parent_->left_) {
      //CHECK(sib->right_->color_ == RED);
      sib->right_->color_ = BLACK;
      RotateLeft(n->parent_);
    } else {
      //CHECK(sib->left_->color_ == RED);
      sib->left_->color_ = BLACK;
      RotateRight(n->parent_);
    }   
  }

  // Gets a node or adds it using the projection's conversion from 
  // key to Data (in the case of a map, we will tack on a default value).
  Node * GetAddNode(const Key & k) {
    Node *parent;
    Node ** where = search(k, &parent);
    if (*where) return *where;
    Node * ret = new Node(where, parent, Projection::ToData(k));
    InsertCase1(ret);
    return ret;
  }

  // where should this thing go if we were to insert it? 
  // returns a pointer to the pointer that would point to the node.
  // sets *parent to what the new node's parent pointer should be.
  // if a matching node already exists, then *(the return value) 
  // will be non-null and point to it.
  Node ** search(const Key & k, Node **parent) const {
    Node *dummy;
    if (!parent) parent = &dummy;
    Node * const *current = &root_;
    *parent = NULL;
    while (*current) {
      if ( k < Projection::ToKey((*current)->data_) ) {
	*parent = *current;
	current = &((*current)->left_);
      }
      else if ( Projection::ToKey((*current)->data_) < k) {
	*parent = *current;
	current = &((*current)->right_);
      }
      else return (Node**) current;
    }
    return (Node**) current;
  }
  static Color ColorOf(const Node * p)  {
    if (p) return p->color_;
    return BLACK;
  }
  static uint SubtreeSize(const Node *n)  {
    if (!n) return 0;
    return n->subtree_size_;
  }
  // puts displacer where victim was and deletes victim
  void ReplaceAndDestroy(Node *victim, Node *displacer) {
    //CHECK(victim);
    int count_delta = SubtreeSize(displacer) - SubtreeSize(victim);
    Node * parent = victim->parent_;
    if (displacer) displacer->parent_ = parent;
    *GetPointerTo(victim) = displacer;
    delete victim;
    for (Node *n = parent; n; n = n->parent_) {
      n->subtree_size_ += count_delta;
    }
  }
};

template <class T> 
class rankset : public RankTree<IdentityProjection<T> >{
};

template<class K, class V>
class rankmap : public RankTree<FirstProjection<K,V> >{
 public:
  typedef typename RankTree<FirstProjection<K,V> >::Node Node;
  V & operator [](const K & k) {
    Node *n = GetAddNode(k);
    return n->data_.second;    
  }
};

template<class A> bool operator %(const rankset<A> & m, const A & a){
  return m.find(a) != m.end();
}

template <class A, class B> B * operator %(rankmap<A, B> & m, const A & a){
  typedef typeof(m.begin()) iter_type;
  iter_type look = m.find(a);
  if (look != m.end()) return &(look->second);
  return 0;
}

template<class A, class B> const B * operator %(const rankmap<A, B> & m, 
						const A & a){
  typedef typeof(m.begin()) iter_type;
  iter_type look = m.find(a);
  if (look != m.end()) return &(look->second);
  return 0;
}

template<class Projection> 
bool operator <(const RankTree<Projection> & x, 
		const RankTree<Projection> & y) {
  return lexicographical_compare(x.begin(), x.end(), y.begin(), y.end());
}
template<class Projection> 
bool operator ==(const RankTree<Projection> & x, 
		 const RankTree<Projection> & y) {
  return ((x.size() == y.size()) && 
	  equal(x.begin(), x.end(), y.begin()) );
}


template<class SK, class MK, class V> 
  rankmap<MK, V> Restrict(const rankmap<MK,V> & m, 
			   const set<SK> & s) {
  rankmap<MK, V> ret;
  forall(run, m) {
    if (s % SK(run->first)) ret[run->first] = run->second;
  }
  return ret;
}


inline void TestRankSet() {
  set<int> foo;
  //rankset<int> foo;
  int start = time(0);
  for (int i=0; i<100000000; i++) { 
    if (!(i & (i-1))) { 
      cout << "Testing " << i 
	   << " size= " << foo.size() << endl;
      //foo.Check();
    }
    if (rand() % 2) foo.insert(rand() % 1000);
    else foo.erase(rand() % 1000);
  }
  cout << "time=" << time(0)-start << endl;
}
inline void TestRankMap() {
  rankmap<string, int> foo;
  foo["noam"] = 1;
  foo["georges"] = 4;
  foo["georges"] = -3;
  foo["ralph"];
  foo.erase("noam");
  foo.Check();
  
  forall(run, foo) {
    cout << "foo[" << run->first << "] = " << run->second << endl;
  }
  const rankmap<string, int> & bar = foo;
  forall(run, bar) {
    cout << "foo[" << run->first << "] = " << run->second << endl;
  }
  
}

//template <class T>
//typedef RankTree<IdentityProjection<T> > rankset;


//template <class K, class V> 
//class o

#endif
