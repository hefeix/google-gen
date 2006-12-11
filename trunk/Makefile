# This defines the compiler and standard ways to use it
CXX = g++
CXXFLAGS = -Wall -Wno-deprecated -g
CXXMOSTFLAGS = -Wall -Wno-deprecated

# These are the Gen sources, and computed object and header files
SRC = optimization.cc hash.cc util.cc probutil.cc record.cc numbers.cc lexicon.cc tuple.cc tupleindex.cc model.cc component.cc changelist.cc prohibition.cc modelshell.cc
INC = $(addsuffix .h, $(basename $(SRC)))
OBJ = $(addsuffix .o, $(basename $(SRC)))

# This defines things to do with python and SWIG
# Where to find python includes
# How to link the python library
# How to make a loadable object
PYTHONINCLUDES = -I"/System/Library/Frameworks/Python.framework/Versions/2.3/Headers"
PYTHONLINK = -framework Python
LOADABLE = -bundle
ifeq ($(shell uname),Linux)
   $(warning Linux)
   PYTHONINCLUDES = -I"/usr/include/python2.4"
   PYTHONLINK = 

   LOADABLE = -shared
endif

# These are the SWIG interface files and wrappers
SWIGSRC = gen.i
SWIGWRAP = $(SWIGSRC:.i=_wrap.cxx)
SWIGOBJ = $(SWIGSRC:.i=_wrap.o)
SWIGPYTHON = $(SWIGSRC:.i=.py)

# This is a pattern rule to make a c wrapper from a .i SWIG file
# This has the side effect of making a .py file as well
%.py %_wrap.cxx: %.i
	swig -python -c++ $<

# This is a pattern rule for compiling python _wrap.cxx files
# You could include the includes in the rule below at risk of ugliness
%_wrap.o: %_wrap.cxx
	$(CXX) $(CXXMOSTFLAGS) $(PYTHONINCLUDES) -c $<

# Pattern rule making .o files from .cc
%.o : %.cc
	$(CXX) $(CXXFLAGS) -c $<

# This is the standard command line runnable
t: $(OBJ)
	$(CXX) $(CXXFLAGS) $(OBJ) -o $@ 

# This is the python compatible shared module
_gen.so: $(OBJ) $(SWIGOBJ)
	$(CXX) $(CXXFLAGS) $(PYTHONLINK) $(LOADABLE) $(OBJ) $(SWIGOBJ) -o _gen.so

Makefile: depend
-include depend

depend: $(SRC) $(INC)
	$(CXX) -MM $(CXXFLAGS) $(SRC) > depend

.PHONY: clean all
clean:
	rm -f $(OBJ) $(SWIGOBJ) $(SWIGPYTHON) $(SWIGWRAP)

all: _gen.so $(SWIGPYTHON) $(SWIGWRAP) t