%module gen
%include "std_string.i"

%{

extern std::string ModelShellHandleExternal(std::string command);

%}

extern std::string ModelShellHandleExternal(std::string command);
