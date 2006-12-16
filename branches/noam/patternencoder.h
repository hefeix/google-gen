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

// A prohibition says that tuples matching a particular tuple with wildcards.
// are forbidden except for a list of exceptions.   
// Wildcards are represented by $0, but multiple wildcards can match different
// literals.
// Creating prohibitions causes the model to automatically track their 
// violations.  Prohibitions can be violated at layer 2 (local consistency) 
// but not at layer 3 (global consistency).

#ifndef _PATTERNENCODER_H_
#define _PATTERNENCODER_H_

#include "component.h"

class PatternEncoder{
 public:
  PatternEncoder(Component * owner,
		 Pattern context,
		 Pattern to_encode);

  // change encoding schemes.
  void SwitchToTupleEncoding();
  void SwitchToDirectEncoding();


 private:

  //  These functions add and remove the global costs associated with the
  // different encoding schemes.   
  void L1_MakeDirectlyEncoded();
  void L1_MakeNotDirectlyEncoded();
  void L1_MakeTupleEncoded();
  void L1_MakeNotTupleEncoded();
    
  Model *model_;
  bool tuple_encoded_; // is this tuple encoded vs. directly encoded
  double direct_encoding_ln_likelihood_;
  set<Tuple> tuple_encoding_;
  set<TrueTuple *> tuple_causes_;
}

#endif
