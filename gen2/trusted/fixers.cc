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

#include "fixers.h"

// UNTRUSTED

bool StaticExecute() {
  while (1) {
    {
      Violation *v = M.GetViolationOfType(Violation::MISSING_DYNAMIC_ON);
      if (v) { 
	FixMissingDynamicOn(dynamic_cast<MissingDynamicOnViolation*>(v));
	continue;
      }
    }
    {
      Violation *v = M.GetViolationOfType(Violation::MISSING_ON_MATCH);
      if (v) {
	FixMissingOnMatch(dynamic_cast<MissingOnMatchViolation*>(v));
	continue;
      }
    }
    {
      Violation *v = M.GetViolationOfType(Violation::EXTRA_ON_MATCH);
      if (v) {
	FixExtraOnMatch(dynamic_cast<ExtraOnMatchViolation*>(v));
	continue;
      }
    }
    {
      Violation *v = M.GetViolationOfType(Violation::MISSING_LINK);
      if (v) {
	FixMissingLink(dynamic_cast<MissingLinkViolation*>(v));
	continue;
      }
    }
    cerr << "Nothing more to fix";    
    break;
  }
  return true;
}
bool FixMissingLink(MissingLinkViolation *violation) {
  if (violation->owner
}
bool FixMissingDynamicOn(MissingDynamicOnViolation *violation){
  cerr << "Fixing missing dynamic on " << endl;
  StaticOn * static_on = violation->GetOwner();
  MakeDynamicElement<DynamicOn>(static_on, OMap::Default());
  return true;
}
bool FixMissingOnMatch(MissingOnMatchViolation *violation) {
  cerr << "Fixing missing on match" << endl;
  DynamicOn * d_on = dynamic_cast<DynamicOn *>(violation->GetOwner()->parent_);
  StaticOn * s_on = d_on->GetStaticOn();
  StaticElement * s_child = s_on->GetChild(StaticOn::CHILD);
  if (!s_child) {
    cerr << "can't fix misisng on match because static on has no child" << endl;
    return false;
  }
  OMap binding = violation->data_;
  MakeDynamicElement(s_child, binding);
  return true;
}
bool FixExtraOnMatch(ExtraOnMatchViolation *violation) {
  cerr << "Fixing extra on match" << endl;
  violation->GetOwner()->GetChild(violation->data_);
  DynamicOn * d_on = dynamic_cast<DynamicOn*>(violation->GetOwner()->parent_);
  StaticOn * s_on = d_on->GetStaticOn();
  StaticElement * s_child = s_on->GetChild(StaticOn::CHILD);
  if (!s_child) {
    cerr << "can't fix misisng on match because static on has no child" << endl;
    return false;
  }
  OMap binding = violation->data_;
  MakeDynamicElement(s_child, binding);
  return true;
}


