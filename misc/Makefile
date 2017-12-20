#
# to print a variable use: # $(info $$var is [${var}])
# 

#
# directory layout
# root
#  |- src  # where all *.cpp and *.h files are
#  |- deps # where generated *.d files are; removed by make clean
#  |- objs # where generated *.o files are; removed by make clean
#  |- bin  # where executable is; removed by make clean
#

CXXFLAGS= -std=c++17 -Wall -Wextra
DEPFLAGS= -MMD -MF $(patsubst objs/%.o, deps/%.d, $@)

INCLUDES=-isystem third_party/beast/include
LIBS= -lboost_system \
      -pthread

#
# files
#
SOURCES=$(wildcard src/*.cpp)
OBJECTS=$(patsubst src/%.cpp, objs/%.o, $(SOURCES))
DEPS= $(patsubst src/%.cpp, deps/%.d, $(SOURCES))

#
# rules
#

EXECUTABLE=bin/serve

all: mkdirs executable 

executable: $(EXECUTABLE)

$(EXECUTABLE): $(OBJECTS)
	g++ -o $@ $(OBJECTS) $(LIBS)

$(OBJECTS): objs/%.o: src/%.cpp 
	g++ -c $< -o $@ $(CXXFLAGS) $(INCLUDES) $(DEPFLAGS)

mkdirs:
	@mkdir -p objs
	@mkdir -p bin
	@mkdir -p deps

#
#
#
.PHONY: clean release debug

clean:
	-rm -rf bin objs deps

release: CXXFLAGS += -O3 -DNDEBUG -ffast-math 
release: mkdirs executable

debug: CXXFLAGS += -g
debug: mkdirs executable

#
#
#
-include deps/*.d
