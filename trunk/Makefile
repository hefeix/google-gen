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
optimization.o: optimization.cc model.h util.h hash.h tuple.h numbers.h \
  tupleindex.h lexicon.h record.h component.h changelist.h optimization.h
hash.o: hash.cc
util.o: util.cc util.h hash.h
probutil.o: probutil.cc probutil.h util.h hash.h numbers.h
record.o: record.cc record.h util.h hash.h
numbers.o: numbers.cc numbers.h util.h hash.h
lexicon.o: lexicon.cc lexicon.h util.h hash.h tuple.h
tuple.o: tuple.cc tuple.h util.h hash.h lexicon.h probutil.h numbers.h
tupleindex.o: tupleindex.cc tupleindex.h util.h hash.h tuple.h lexicon.h
model.o: model.cc model.h util.h hash.h tuple.h numbers.h tupleindex.h \
  lexicon.h record.h component.h changelist.h probutil.h prohibition.h
component.o: component.cc model.h util.h hash.h tuple.h numbers.h \
  tupleindex.h lexicon.h record.h component.h changelist.h probutil.h \
  prohibition.h
changelist.o: changelist.cc changelist.h util.h hash.h
prohibition.o: prohibition.cc prohibition.h component.h util.h hash.h \
  record.h tuple.h numbers.h model.h tupleindex.h lexicon.h changelist.h
modelshell.o: modelshell.cc model.h util.h hash.h tuple.h numbers.h \
  tupleindex.h lexicon.h record.h component.h changelist.h optimization.h
