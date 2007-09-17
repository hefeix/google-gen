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

#include "extensions.h"
#include "webserver.h"

Named * GetPostingOwner(const Posting *posting){
  if (!posting->IsOwned()) return NULL;
  return dynamic_cast<const OwnedPosting *>(posting)->owner_;
}

OwnedPosting::OwnedPosting(OTuple tuple, OTime time, Named *owner) 
  :Posting(tuple, time.Data(), &BB) {
  owner_ = owner;
}
string OwnedPosting::GetURL() const {
  return owner_->GetURL() + "&ownedposting=true";
}
string OwnedPosting::ShortDescription() const {
  return "POSTING " + HTMLLink(GetURL(), tuple_.ToString())
    + " owned by " + owner_->ShortDescription();
}
Record OwnedPosting::GetRecordForDisplay() const {
  Record ret = Posting::GetRecordForDisplay();
  ret["owner"] = owner_->ShortDescription();
  ret["type"] = "OWNED_POSTING";
  return ret;
}
