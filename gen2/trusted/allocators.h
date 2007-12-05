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


#ifndef _ALLOCATORS_H_
#define _ALLOCATORS_H_

#include <set>
#include <map>
#include "shorthand.h"
#include "hash.h"

#define LINEAR_ALLOCATOR_BLOCK_SIZE (16 << 20)
// maximum size (in *s)
#define GENERAL_ALLOCATOR_MAX_ITEM_SIZE 65
#define GENERAL_ALLOCATOR_BLOCK_BYTES (1 << 20)

class GeneralAllocator {
 private:
  struct SizeInfo;
  struct Block {
    // The block contains a contiguous chunk of memory (data_), which is divided
    // into num_items_ items.  Each item consists of first a Block *, which 
    // points at the block if it is occupied, or is null otherwise.
    // The remainder of the item is a chunk of item_size_ (Block *)s, which
    // is the memory allocated to the client.
    Block(SizeInfo *size_info, int num_items) {
      size_info_ = size_info;
      item_size_ = size_info_->item_size_;
      num_items_ = num_items;
      data_ = new Block *[(item_size_ + 1)  * num_items_];
      num_occupied_ = 0;
    }
    ~Block() {
      delete [] data_;
    }
    // If the density falls below this, scavenge the block for more allocations.
    int UsabilityThreshold() { return num_items_ * 3 / 4;}
    Block ** DataEnd() { return data_ + num_items_ * (item_size_+ 1);}
    Block ** GetItem(int item_num) { return data_ + item_num * (item_size_+1); }
    Block *& IsOccupied(int item_num) { 
      return data_[item_num * (item_size_+1)]; }
    int item_size_; // in *s
    int num_items_;
    int num_occupied_;
    SizeInfo * size_info_;
    Block ** data_;
  };
  int max_item_size_; // items of this size (in ints) or greater just get new'd

  struct SizeInfo {
    int item_size_; // in *s
    Block *current_;
    set<Block *> usable_; // usable blocks (other than current_)
    int current_position_in_block_;
    GeneralAllocator *allocator_;
    bool block_is_virgin_;
    SizeInfo(int item_size, GeneralAllocator *allocator) {
      current_ = NULL;
      item_size_ = item_size;
      allocator_ = allocator;
    }
    void *Allocate() {
      AdvanceToUsablePosition();
      Block ** item = current_->GetItem(current_position_in_block_);
      CHECK(*item == NULL);
      *item = current_;
      current_->num_occupied_++;
      current_position_in_block_++;
      return item + 1;
    }
    void FindNewBlock() {
      block_is_virgin_ = false;
      if (current_ && current_->num_occupied_ < current_->UsabilityThreshold()){
	current_position_in_block_ = 0;
	return;
      }
      if (usable_.size()) {
	Block *b = *usable_.begin();
	usable_.erase(usable_.begin());
	current_ = b;
	current_position_in_block_ = 0;
	return;
      }
      current_ = new Block(this, GENERAL_ALLOCATOR_BLOCK_BYTES 
			   / ((item_size_+1) * sizeof(Block *) ) );
      //cerr << "Creating block size=" << item_size_ << " " << current_->data_
      //   << " - " << current_->DataEnd() << endl;
      current_position_in_block_ = 0;
      block_is_virgin_ = true;
      return;
    }
    void AdvanceToUsablePosition() {
      if (current_ == NULL) FindNewBlock();
      if (!block_is_virgin_) {
	while (current_position_in_block_ < current_->num_items_
	       && current_->IsOccupied(current_position_in_block_)) 
	  current_position_in_block_++;
      }
      if (current_position_in_block_ == current_->num_items_) {
	FindNewBlock();
	AdvanceToUsablePosition();
      }
    }
  };
  vector<SizeInfo *> size_info_;
  
 public:
  GeneralAllocator() {
    max_item_size_ = GENERAL_ALLOCATOR_MAX_ITEM_SIZE;
    for (int i=0; i<max_item_size_; i++) {
      size_info_.push_back(new SizeInfo(i, this));
    }
  }
  void *allocate(size_t s) {
    int item_size = max(1UL, (s + sizeof(Block*) - 1) / sizeof(Block*));
    /*if (item_size == 0) {
      cerr << "Allocating an object of size o??? s=" << s << endl;
      }
      CHECK(item_size > 0);*/
    if (item_size >= max_item_size_) {
      Block ** m = new Block*[item_size+1];
      m[0] = NULL;
      return m+1;
    }
    void *ret = size_info_[item_size]->Allocate();    
    //cerr << "allocated " << s << " bytes: " << ret << endl;
    return ret;
  }
  void deallocate(void * ptr) {
    //cerr << "deallocating " << ptr << endl;
    Block **p = (Block **)ptr;
    Block * b = *(p-1);
    if (b==0) {
      delete [] (p-1);
      return;
    }
    *(p-1) = NULL; // mark the item unoccupied
    b->num_occupied_--;
    if ( (b != b->size_info_->current_) && 
	 (b->num_occupied_ == b->UsabilityThreshold()-1) ) {
      b->size_info_->usable_.insert(b);
    }
    if (b->num_occupied_ == 0) {
      //cerr << "Erasing block size=" << b->item_size_ << " " << b->data_
      //   << " - " << b->DataEnd() << endl;
      b->size_info_->usable_.erase(b);
      b->size_info_->current_ = NULL;
      delete b;
    }
  }
};


class LinearAllocator {
 public:
  void * allocate(size_t size){
    CHECK(size <= LINEAR_ALLOCATOR_BLOCK_SIZE * sizeof(int) );
    int ints_needed = (size + (sizeof(int)-1)) / sizeof(int);
    int space_left = LINEAR_ALLOCATOR_BLOCK_SIZE - 
      (last_block_end_ - blocks_[blocks_.size()-1]);
    if (ints_needed > space_left) AddBlock();
    void * ret = last_block_end_;
    last_block_end_ += ints_needed;
    return ret;
  }

  void deallocate(void * ptr){
    return;
    int block_num = blocks_.size()-1;
    while (!(ptr >= blocks_[block_num] 
	     && ptr < blocks_[block_num]+LINEAR_ALLOCATOR_BLOCK_SIZE)) {
      block_num--;
      if (block_num<0) {
	CHECK(false);
      }
    }
    while ((uint)block_num+1 < blocks_.size()) {
      RemoveLastBlock();
    }
    last_block_end_ = (int*)ptr;
  }

  LinearAllocator();
  void Clear();

 private:
  void AddBlock();
  void RemoveLastBlock();
  
  vector<int *> blocks_;
  int * last_block_end_; // ending position of each block
};

inline void *operator new(size_t size, LinearAllocator &a) {
  return a.allocate(size); 
}

extern LinearAllocator CL_ALLOC;
extern GeneralAllocator GEN_ALLOC;

template <class T> class GenAlloc;

// specialize for void:
template <> class GenAlloc<void> {
 public:
  typedef void*       pointer;
  typedef const void* const_pointer;
  // reference to void members are impossible.
  typedef void value_type;
  template <class U> struct rebind { typedef GenAlloc<U> other; };
};

template <class T>
class GenAlloc {
public:
  // type definitions
  typedef T        value_type;
  typedef T*       pointer;
  typedef const T* const_pointer;
  typedef T&       reference;
  typedef const T& const_reference;
  typedef std::size_t    size_type;
  typedef std::ptrdiff_t difference_type;

  // rebind allocator to type U
  template <class U >
    struct rebind {
      typedef GenAlloc< U > other;
    };

  // return address of values
  pointer address (reference value) const {
    return &value;
  }
  const_pointer address (const_reference value) const {
    return &value;
  }

  /* constructors and destructor
  * - nothing to do because the allocator has no state
  */
  
  GenAlloc() {}
  GenAlloc(const GenAlloc& src) {}
  template <class U > GenAlloc (const GenAlloc< U > &src) {}
  ~GenAlloc() {}
  

  // return maximum number of elements that can be allocated
  size_type max_size () const throw() {
    return LINEAR_ALLOCATOR_BLOCK_SIZE / sizeof(T);
  }

  // initialize elements of allocated storage p with value value
  void construct (pointer p, const T& value) {
    // initialize memory with placement new
    new((void*)p)T(value);
  }

  // destroy elements of initialized storage p
  void destroy (pointer p) {
    // destroy objects by calling their destructor
    p->~T();
  }

  // allocate but don't initialize num elements of type T
  pointer allocate (size_type num, const void* = 0) {
    return (pointer)GEN_ALLOC.allocate(num * sizeof(T) ); 
  }

  // deallocate storage p of deleted elements
  void deallocate (pointer p, size_type num) {
    // do not deallocate memory
    GEN_ALLOC.deallocate(p);
  }
};

// return that all specializations of this allocator are interchangeable
template <class T1, class T2>
bool operator== (const GenAlloc<T1>&, const GenAlloc<T2>&) throw() {
  return true;
}
template <class T1, class T2>
bool operator!= (const GenAlloc<T1>&, const GenAlloc<T2>&) throw() {
  return false;
}

template<class A, class Comp = less<A> > 
class alloc_set : public set<A, Comp, GenAlloc<A> >{};
//class alloc_set : public set<A>{};

template<class A, class B, class Comp = less<A> > 
  class alloc_map : public map<A, B, Comp, GenAlloc<pair<A const, B> > >{};
  //  class alloc_map : public map<A, B>{};

template<class A>
class alloc_vector : public vector<A, GenAlloc<A> >{
 public:
  alloc_vector(int i, const A & a) 
    :vector<A, GenAlloc<A> >(i, a){}
  alloc_vector(int i) 
    :vector<A, GenAlloc<A> >(i){}
  alloc_vector() : vector<A, GenAlloc<A> >(){}
  alloc_vector(const alloc_vector<A>&o) 
    :vector<A, GenAlloc<A> >(o){}
  template<class InputIterator>
    alloc_vector(InputIterator begin, InputIterator end) {
    for (InputIterator run = begin; run != end; run++)
      this->push_back(*run);
  }
};

DEFINE_HASH_CLASS_1(alloc_vector);
DEFINE_HASH_CLASS_1(alloc_set);
DEFINE_HASH_CLASS_2(alloc_map);

#define set alloc_set
#define map alloc_map
#define vector alloc_vector

#endif;
