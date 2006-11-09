# Copyright (C) 2006 Google Inc.
# 
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# 
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#
# Author: Noam Shazeer


#-----Macros---------------------------------

INCLUDES = -I.

# set up compiler and options
CXX = g++
CXXFLAGS = -Wall -Wno-deprecated -g $(INCLUDES)

#-----Suffix Rules---------------------------
# set up C++ suffixes and relationship between .cc and .o files

.SUFFIXES: .cc

.cc.o:
	$(CXX) $(CXXFLAGS) -c $< 

.cc :
	$(CXX) $(CXXFLAGS) $< -o $@

#-----File Dependencies----------------------

SRC = optimization.cc hash.cc util.cc probutil.cc record.cc numbers.cc lexicon.cc tuple.cc tupleindex.cc model.cc component.cc changelist.cc prohibition.cc modelshell.cc

OBJ = $(addsuffix .o, $(basename $(SRC)))

t: $(OBJ)
	$(CXX) $(CXXFLAGS) -o $@ $(OBJ)

#-----Other stuff----------------------------
depend:
	cc -MM $(CXXFLAGS) $(SRC)

clean:
	rm -f $(OBJ)
# DO NOT DELETE

util.o: util.h
probutil.o: probutil.h
record.o: record.h util.h
numbers.o: numbers.h util.h
lexicon.o: lexicon.h util.h
tuple.o: tuple.h util.h lexicon.h
tupleindex.o: tupleindex.h util.h tuple.h lexicon.h
component.o: model.h util.h tuple.h numbers.h tupleindex.h
optimization.o: model.h util.h tuple.h numbers.h tupleindex.h
model.o: model.h util.h tuple.h numbers.h tupleindex.h
modelshell.o: model.h util.h tuple.h numbers.h tupleindex.h
changelist.o: changelist.h 
