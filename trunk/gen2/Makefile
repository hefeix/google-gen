# This defines the compiler and standard ways to use it
CXX = g++
CXXFLAGS = -Wall -Wno-deprecated -g
CXXMOSTFLAGS = -Wall -Wno-deprecated

# These are the Gen sources, and computed object and header files
SRC = main.cc objects.cc hash.cc util.cc changelist.cc tuple.cc \
    numbers.cc blackboard.cc query.cc probutil.cc ranktree.cc \
    violation.cc link.cc spec.cc record.cc chooser.cc globals.cc model.cc \
    namer.cc element.cc
INC = $(addsuffix .h, $(basename $(SRC)))
OBJ = $(addsuffix .o, $(basename $(SRC)))

# This defines things to do with python and SWIG
# Where to find python includes
# How to link the python library
# How to make a loadable object

# Pattern rule making .o files from .cc
%.o : %.cc
	$(CXX) $(CXXFLAGS) -c $<

# This is the standard command line runnable
gen: $(OBJ)
	$(CXX) $(CXXFLAGS) $(OBJ) -o $@ 

Makefile: depend
-include depend

depend: $(SRC) $(INC)
	$(CXX) -MM $(CXXFLAGS) $(SRC) > depend

.PHONY: clean all
clean:
	rm -f $(OBJ)
