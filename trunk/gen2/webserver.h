// Copyright (C) 2007 Georges Harik and Google Inc.
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

/*
webserver.h and webserver.cc contain a very easy-to-use webserver.  

It is currently mildly deficient in that the entire request must be received in 
one packet.  A SelectServer loop would be better. 

To use this class, create a subclass of RequestHandler.  
You need to override the following function:
string Handle(const map<string, string> & params);
  parms will contain the parameters parsed from the HTTP request. 
  In addition it will contain the following (key,value) pairs.
    _command : whatever follows the final slash and preceeds ? in the URL
    _method  : the request method, e.g. "GET"
    _uri     : the URI
    _version : the HTTP version
    _request_line : the line containing (method, uri, version)
    _headers : the complete HTTP headers
    _header_<x> : the content of the header line which starts with <x>:
    _content : The content of the HTTP request

As an example of how to write a RequestHandler, look at EchoHandler in this 
file. 
Running ws_main() in webserver.cc is also useful to see the webserver working. 

*/

#ifndef _WEBSERVER_H_
#define _WEBSERVER_H_

#include "shorthand.h"
#include <map>
#include <netinet/in.h>
#include <pthread.h>

struct RequestHandler{
  virtual string Handle(const map<string, string> & params) = 0;
  virtual ~RequestHandler(){};
};

struct WebServer {
  WebServer(RequestHandler *handler, int port=8000) { 
    handler_ = handler;
    port_ = port;
  }
  // returns true if it worked
  bool Start();
  void StartInThread();
  void HandleRequest(int fd);
  static void Test(int port);
  RequestHandler * handler_;
  static void WebServer::Parse(string request, 
			       map<string, string> *params);
    
  
  private:
  int port_;
  pthread_t server_thread_;
};

string SimpleHTMLHeader() {
  return "HTTP/1.0 200 OK\r\nContent-Type: text/html;\r\n\r\n";
}

struct EchoHandler : public RequestHandler {
  string Handle(const map<string, string> & params) {
    string ret = SimpleHTMLHeader();
    ret += "<pre>\n";
    forall(run, params) 
      ret += "  " + run->first + " : " + run->second + "\n";
    ret += "</pre>";
    return ret;
  }
};

string Unquote(string s);
int CharToHex(char c);
vector<string> SplitString(string s, string delimiters);
string Plus2Space(string s);
map<string, string> ParseQSL(string s);


#endif
