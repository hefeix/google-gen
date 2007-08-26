// Copyright (C) 2007 Georges Harik and  Google Inc.
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


#include "webserver.h"
#include <sstream>

bool WebServer::Start(){
  // Bind to the server port
  int sockfd = socket(AF_INET, SOCK_STREAM, 0);
  if (sockfd == -1) { 
    cerr << "couldn't create socket";
    return false;
  }
  sockaddr_in my_addr;
  my_addr.sin_family = AF_INET;
  my_addr.sin_port = htons(port_);
  my_addr.sin_addr.s_addr = INADDR_ANY;
  bzero(&(my_addr.sin_zero), 8);
  if (bind(sockfd, (sockaddr*)&my_addr, sizeof(sockaddr)) == -1) {
    cerr << "couldn't bind to port " << port_ << endl;
    return false;
  }
  if (listen(sockfd, 10) == -1) {
    cerr << "couldn't listen" << endl;
    return false;
  }

  // Handle requests
  socklen_t socklen = sizeof(sockaddr_in);
  while(1) {
    sockaddr_in remote_address;
    int fd = accept(sockfd, (sockaddr *) &remote_address, &socklen);
    if (fd == -1) {
      cerr << "couldn't accept connection" << endl;
      return false;
    }
    int fk = fork();
    if (fk != 0) continue; // you're the parent
    
    // parse the command and call Handle.    
    char inbuffer[9910];
    if (recv(fd, inbuffer, sizeof(inbuffer), 0) == -1) {
      cerr << "Error receiving" << endl;
      Die(fd);
    }
    map<string, string> params;
    Parse(inbuffer, &params);
    string result = 
      handler_->Handle(params);
    send(fd, result.c_str(), result.size(), 0);
    Die(fd);
  }
};

// gets a line terminated by "\r\n"
string GetTerminatedLine(istream & input) {
  string ret;
  char c;
  while (input.get(c)) { 
    ret += c;
    if (ret.size() >= 2 && ret.substr(ret.size()-2) == "\r\n") {
      return ret.substr(0, ret.size()-2);
    }
  }
  return "ERROR";
}
int CharToHex(char c) {
  if (isdigit(c)) return c-'0';
  if (isupper(c)) return c-'A'+10;
  if (islower(c)) return c-'a'+10;
  return -1;
}
string Unquote(string s) {
  string ret;
  istringstream input(s);
  char c;
  while (input.get(c)) {
    if (c=='%') {
      char c1, c2;
      input.get(c1);
      input.get(c2);
      ret += (unsigned char)(CharToHex(c1) * 16 + CharToHex(c2));
    } else {
      ret += c;
    }
  }
  return ret;
}
vector<string> SplitString(string s, string delimiters){
  set<char> delim(delimiters.begin(), delimiters.end());
  string current;
  vector<string> ret;
  for (unsigned int i=0; i<s.size(); i++) {
    if (delim % s[i]) {
      if (current.size()) ret.push_back(current);
      current = "";
    }
    else current+=s[i];
  }
  if (current.size()) ret.push_back(current);
  return ret;
}
string Plus2Space(string s){
  string ret = s;
  for (unsigned int i=0; i<s.size(); i++) if (ret[i]=='+') ret[i]=' ';
  return ret;
}
map<string, string> ParseQSL(string s) {
  map<string, string> ret;
  vector<string> parts = SplitString(s, ";&");
  for (unsigned int i=0; i<parts.size(); i++) {
    string part = parts[i];
    unsigned int equalpos = part.find("=");
    if (equalpos == string::npos) continue;
    string key = Unquote(Plus2Space(part.substr(0, equalpos)));
    string value = Unquote(Plus2Space(part.substr(equalpos+1)));
    ret[key] = value;
  }
  return ret;
}


void WebServer::Parse(string request, 
		      map<string, string> *params){
  params->clear();
  istringstream input(request);
  string request_line = GetTerminatedLine(input);
  (*params)["_request_line"] = request_line;
  while (1) {
    string header_line = GetTerminatedLine(input);
    (*params)["_headers"] += header_line + "\r\n";
    if (header_line=="") break;
    unsigned int colonpos = header_line.find(": ");
    if (colonpos != string::npos) {
      (*params)["_header_"+header_line.substr(0, colonpos)]
	= header_line.substr(colonpos+2);
    }
  }
  string content;
  char c;
  while (input.get(c)) content += c;
  istringstream req_input(request_line);
  string method, uri, version;
  req_input >> method >> uri >> version;
  (*params)["_method"] = method;
  (*params)["_uri"] = uri;
  (*params)["_version"] = version;

  
  vector<string> splituri = SplitString(uri, "?");
  if (splituri.size() == 2) {
    map<string, string> urlparams = ParseQSL(splituri[1]);    
    params->insert(urlparams.begin(), urlparams.end());
  }
  (*params)["_command"] = "";
  if (splituri.size() >= 1) {
    vector<string> v = SplitString(splituri[0], "/");
    if (v.size() > 0) (*params)["_command"] = v[v.size()-1];
  }
  (*params)["_content"] = content;
  map<string, string> contentparams = ParseQSL(content);
  params->insert(contentparams.begin(), contentparams.end());  
}

void WebServer::Die(int fd) {
    shutdown(fd, SHUT_RDWR);
    close(fd);
    _exit(0);
}

void WebServer::Test(int port){
  WebServer ws(new EchoHandler(), port);
  ws.Start();
}

int ws_main(int argc, char ** argv) {
  WebServer::Test((argc>1)?atoi(argv[1]):8000);
  return 0;
}