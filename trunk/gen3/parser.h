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

#ifndef _PARSER_H_
#define _PARSER_H_

// This is all the code that involves reading the model.
// It's a little odd that the converse ToString() is in element.h, but 
// oh well. 

// UNTRUSTED

#include "element.h"

Element * SimpleParseElement(Object o, Element *parent = 0);
vector<Element *> SimpleParseElements(const Tuple & t, Element *parent = 0); 

// ad hoc parser.
// position points to where to start parsing, and is changed by the function
// to the end of what was parsed.
Element * PrettyParseElement(const Tuple & t, uint * pos, Element *parent = 0);
vector<Element *> PrettyParseElements(const Tuple & t, Element *parent =0); 


Element * MakeElementByKeyword(Keyword type, Element *parent);

#endif
