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

#ifndef _GEN_REQUESTHANDLER_H_
#define _GEN_REQUESTHANDLER_H_

#include "webserver.h"
#include "record.h"
#include "named.h"

class GenRequestHandler : public RequestHandler {
  // handle a web request
  string Handle(Record params);
  string TopNavHTML() const;
  string TypeListHTML(Named::Type type) const;


};

#endif;
