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

// Extensions of lower level things that work and that we don't want to break.

#ifndef _EXTENSIONS_H_
#define _EXTENSIONS_H_

#include "blackboard.h"
#include "base.h"

Base * GetPostingOwner(const Posting *posting);

struct OwnedPosting : public Posting {
  OwnedPosting(OTuple tuple, OTime time, Base *owner);
  bool IsOwned() const { return true;}
  string GetURL() const;
  string ShortDescription() const;
  Record GetRecordForDisplay() const;
  Base *owner_;
};

#endif
