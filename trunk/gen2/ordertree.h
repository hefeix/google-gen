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

#include <vector>
#include <iostream>
#include <string>
#include <map>
#include "shorthand.h"

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
  static const K & Key(const K &k) { return k;}
  typedef K KeyType;
  typedef K DataType;
};
template<class K, class V> 
struct FirstProjection {
  static const K & Key(const pair<K,V> &p) { return p.first;}  
  typedef K KeyType;
  typedef pair<K,V> DataType;
};


template<class Projection> 
class RBTree {
  typedef typename Projection::KeyType Key;
  typedef typename Projection::DataType Data;
  
  bool ProjectLT(const Data & d1, const Data & d2) {
    return (Projection::Key(d1) < Projection::Key(d2));
  }

 public:
  uint size() const { return root_?root_->subtree_size_:0;}

  void insert(const Data & d) {
    GetAddNode(d);
  }
  void erase(const Key & k) {
    Node *parent;
    Node ** look = search(k, &parent);
    if (*look) Delete(*look);
  }
  RBTree() {root_ = NULL;}
  string ToString(){
    return "tree\n" + ToString(root_, "", "");
  }

 private:
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
    int WhichChild() const{ 
      if (!parent_) return 0;
      if (this == parent_->left_) return -1;
      return 1;
    }
    Node * Grandparent() {
      if (parent_ == NULL) return NULL;
      return parent_->parent_;
    }
    Node Uncle() {
      if (!Grandparent()) return NULL;
      if (parent_ == Grandparent()->left_)
        return Grandparent()->right_;
      else
	return Grandparent()->left_;
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
    }    
  };
  Node * root_;

  string ToString(Node * n, string lineheadtop, string lineheadbottom) {
    if (n==NULL) {
      return lineheadtop + "NULL\n";
    }
    string ret;
    ret += ToString(n->left_, lineheadtop+"  ", lineheadtop+" | ");
    ret += lineheadtop + n->data_ + "\n";
    ret += ToString(n->right_, lineheadbottom+" | ", lineheadbottom + "   ");
    return ret;
  } 
  
 public:
  
  void CheckNode(Node *n) {
    if (n) {
      n->Check();
      CheckNode(n->left_);
      CheckNode(n->right_);
    }
  }
  void Check() {
    CheckNode(root_);
  }


  struct iterator {
    Node * current_;
    iterator(Node *n) :current_(n){}
    void operator ++(){
      current_ = NextNode(current_);
    }
    Data operator *() {
      return current_->data_;
    }
  };
  iterator begin() {
    Node * c = root_;
    if (c) while (c && c->left_) c = c->left_;
    return iterator(c);
  }
  iterator end() { return NULL; }

 private:

  void Delete(Node * n) { // deletes a node
    if (!(n->left_ && n->right_)) DeleteOneChild(n);
    Node * current = n->left_;
    while (current->right_) current = current->right_;
    n->data_ = current->data_;
    DeleteOneChild(current);
  }
  void DeleteOneChild(Node *n) { // deletes a node with at most one child
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

  void DeleteCase1(Node *n) {
  }

  Node * GetAddNode(const Data & data) {
    Node *parent;
    Node ** where = search(data, &parent);
    if (*where) return *where;
    Node * ret = new Node(where, parent, data);
    // TODO rebalance and color here.
    return ret;
  }

  // where should this thing go if we were to insert it? 
  // returns a pointer to the pointer that would point to the node.
  // sets *parent to what the new node's parent pointer should be.
  // if a matching node already exists, then *(the return value) 
  // will be non-null and point to it.
  Node ** search(const Key & k, Node **parent) {
    Node **current = &root_;
    *parent = NULL;
    while (*current) {
      if ( k < Projection::Key((*current)->data_) ) {
	*parent = *current;
	current = &((*current)->left_);
      }
      else if ( Projection::Key((*current)->data_) < k) {
	*parent = *current;
	current = &((*current)->right_);
      }
      else return current;
    }
    return current;
  }
  static Color ColorOf(const Node * p) {
    if (p) return p->color_;
    return BLACK;
  }
  // puts displacer where victim was and deletes victim
  void ReplaceAndDestroy(Node *victim, Node *displacer) {
    int count_delta = SubtreeSize(displacer) - SubtreeSize(victim);
    if (displacer) displacer->parent_ = (*victim)->parent_;
    *GetPointerTo(victim) = displacer;
    delete victim;
    if (displacer) {
      for (Node *n = displacer->parent_; n; n = n->parent_) {
	n->subtree_size_ += count_delta;
      }
    }
  }
};

template<class Iterator>
bool operator ==(const Iterator & i1,
		 const Iterator & i2){
  return i1.current_ == i2.current_;
}
template<class Iterator>
bool operator !=(const Iterator & i1,
		 const Iterator & i2){
  return i1.current_ != i2.current_;
}

template <class T> 
class orderset : public RBTree<IdentityProjection<T> >{
};


//template <class T>
//typedef RBTree<IdentityProjection<T> > orderset;


//template <class K, class V> 
//class o
