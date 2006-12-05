%module gen
%include "std_string.i"
%include "std_map.i"

%{

extern std::map<std::string, std::string> ModelShellHandleExternal(std::map<std::string, std::string> parameters);

%}

%template(attributeList) std::map<std::string, std::string>;
extern std::map<std::string, std::string> ModelShellHandleExternal(std::map<std::string, std::string> parameters);
