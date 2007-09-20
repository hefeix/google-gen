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

// UNTRUSTED

#ifndef _FIXERS_H_
#define _FIXERS_H_

#include "violation.h"
#include "model.h"
#include "element.h"

bool StaticExecute();
bool FixMissingDynamicOn(MissingDynamicOnViolation *violation);
bool FixMissingOnMatch(MissingOnMatchViolation *violation);
bool FixExtraOnMatch(ExtraOnMatchViolation *violation);
bool FixMissingLink(MissingLinkViolation *violation);
bool FixValue(ValueViolation *violation);
bool FixIf(IfViolation *violation);
bool FixTime(TimeViolation *violation);
bool FixPost(PostViolation *violation);
bool FixLet(LetViolation *violation);
bool FixBindingOldValues(BindingOldValuesViolation *violation);
#endif
