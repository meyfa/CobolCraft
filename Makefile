# Compiler
COBC = cobc
CC = g++

# Directories
ROOT_DIR = .
OBJECTS_DIR = out
DATA_DIR = data
CODEGEN_OUT_DIR = $(OBJECTS_DIR)/generated

# C++ library: sources, headers, compiled objects
CPP_SRC = $(wildcard $(ROOT_DIR)/cpp/*.cpp)
CPP_HEADERS = $(wildcard $(ROOT_DIR)/cpp/*.h)
CPP_OBJECTS = $(patsubst $(ROOT_DIR)/cpp/%.cpp, $(OBJECTS_DIR)/cpp/%.o, $(CPP_SRC))

# Data extraction from Mojang's server.jar
SERVER_JAR_URL = https://piston-data.mojang.com/v1/objects/4707d00eb834b446575d89a61a11b5d548d8c001/server.jar
SERVER_JAR_EXTRACTED = $(DATA_DIR)/versions/1.21.4/server-1.21.4.jar

# Code generator: templates, sources (without templates), objects, and binary
CODEGEN_TPL_DIR = $(ROOT_DIR)/codegen/templates
CODEGEN_TPL = $(wildcard $(CODEGEN_TPL_DIR)/*.tpl.cob $(CODEGEN_TPL_DIR)/*/*.tpl.cob $(CODEGEN_TPL_DIR)/*/*/*.tpl.cob)
CODEGEN_CPY_DIR = $(ROOT_DIR)/codegen/_copybooks
CODEGEN_CPY = $(wildcard $(CODEGEN_CPY_DIR)/*.cpy $(CODEGEN_CPY_DIR)/*/*.cpy)
CODEGEN_MAIN_SRC = $(ROOT_DIR)/codegen/codegen.cob
CODEGEN_SRC = $(filter-out $(CODEGEN_TPL), $(wildcard $(ROOT_DIR)/codegen/*.cob $(ROOT_DIR)/codegen/*/*.cob $(ROOT_DIR)/codegen/*/*/*.cob))
CODEGEN_OBJECTS = $(patsubst $(ROOT_DIR)/codegen/%.cob, $(OBJECTS_DIR)/codegen/%.o, $(CODEGEN_SRC))
CODEGEN_BIN = $(OBJECTS_DIR)/codegen_bin

# By convention, each source file codegen/generators/%.cob should generate a single output (generated/%.cob)
CODEGEN_OUT_SRC = $(patsubst $(ROOT_DIR)/codegen/generators/%.cob, $(CODEGEN_OUT_DIR)/%.cob, $(wildcard $(ROOT_DIR)/codegen/generators/*.cob))
CODEGEN_OUT_OBJECTS = $(patsubst %.cob, %.o, $(CODEGEN_OUT_SRC))

# To avoid redoing codegen when any source file changes, list the codegen binary's dependencies explicitly
CODEGEN_BIN_DEPS = $(CPP_OBJECTS) $(OBJECTS_DIR)/files.o $(OBJECTS_DIR)/encoding/strings.o $(OBJECTS_DIR)/encoding/json-parse.o

# Application: copybooks, sources, objects, and binary
CPY_DIR = $(ROOT_DIR)/src/_copybooks
CPY = $(wildcard $(CPY_DIR)/*.cpy $(CPY_DIR)/*/*.cpy)
MAIN_SRC = $(ROOT_DIR)/src/main.cob
SRC = $(filter-out $(MAIN_SRC), $(wildcard $(ROOT_DIR)/src/*.cob $(ROOT_DIR)/src/*/*.cob $(ROOT_DIR)/src/*/*/*.cob $(ROOT_DIR)/src/*/*/*/*.cob))
OBJECTS = $(patsubst $(ROOT_DIR)/src/%.cob, $(OBJECTS_DIR)/%.o, $(SRC))
BIN = cobolcraft

# Unit tests: sources, objects, and binary
TEST_MAIN_SRC = $(ROOT_DIR)/tests/test.cob
TEST_SRC = $(filter-out $(TEST_MAIN_SRC), $(wildcard $(ROOT_DIR)/tests/*.cob $(ROOT_DIR)/tests/*/*.cob))
TEST_OBJECTS = $(patsubst $(ROOT_DIR)/tests/%.cob, $(OBJECTS_DIR)/tests/%.o, $(TEST_SRC))
TEST_BIN = $(OBJECTS_DIR)/test_bin

# GnuCOBOL >=3.2 added functions that drastically improve performance. Query the installed version (may have one sub-digit)
GCVERSION := $(shell $(COBC) --version | head -n1 | sed 's/.*\s\([0-9]\+\)\.\([0-9]\).*/\1\2/g' )

# Common compiler options, note: COB_FLAGS can be user-specified
COBC_OPTS = -free -DGCVERSION=$(GCVERSION) $(patsubst %,-I %,$(wildcard $(CPY_DIR)/*)) $(COB_FLAGS)
COBC_OPTS += -O2 --debug -Wall -fnotrunc -fstatic-call

ifeq ($(shell test $(GCVERSION) -ge 32; echo $$?),0)
COBC_OPTS += -Werror=typing
COB_MTFLAGS = -MF $(basename $@).d -MT $@
GLOBALDEPS =
# include dependency files for all existing object files
-include $(OBJECTS:.o=.d)
else
# if dependency generation is not available: use global copy dependency
COB_MTFLAGS =
GLOBALDEPS = $(wildcard $(CPY_DIR)/*.cpy $(CPY_DIR)/*/*.cpy)
endif

.SUFFIXES:
.PHONY: all clean cobclean data codegen run test

# Default target
all: $(BIN) data

# Run the application
run: all
	./$(BIN)

# Run unit tests
test: $(TEST_BIN)
	./$(TEST_BIN)

# Clean compiled objects and binaries
cobclean:
	rm -f $(BIN)
	rm -f $(TEST_BIN)
	rm -f $(CODEGEN_BIN)
	rm -rf $(CODEGEN_OUT_DIR)
	rm -rf $(OBJECTS_DIR)
	rm -f *.d

# Clean everything
clean: cobclean
	rm -f $(JSON_DATA)
	rm -rf data

# Run the data extraction
data: $(SERVER_JAR_EXTRACTED)

# Run the code generator
codegen: $(CODEGEN_OUT_SRC)

$(CPP_OBJECTS): $(CPP_SRC) $(CPP_HEADERS)
	@mkdir -p $(@D)
	$(CC) -c -Wall -O2 -fPIC -o $@ $<

$(SERVER_JAR_EXTRACTED):
	@mkdir -p $(DATA_DIR)
	curl -o data/server.jar $(SERVER_JAR_URL)
	cd $(DATA_DIR) && java -DbundlerMainClass="net.minecraft.data.Main" -jar server.jar --reports --server

$(OBJECTS): $(OBJECTS_DIR)/%.o: $(ROOT_DIR)/src/%.cob $(GLOBALDEPS)
	@mkdir -p $(@D)
	$(COBC) -c $(COBC_OPTS) $(COB_MTFLAGS) -o $@ $<

$(CODEGEN_OBJECTS): $(OBJECTS_DIR)/codegen/%.o: $(ROOT_DIR)/codegen/%.cob $(GLOBALDEPS) $(CODEGEN_CPY)
	@mkdir -p $(@D)
	$(COBC) -c $(COBC_OPTS) $(COB_MTFLAGS) -I $(CODEGEN_CPY_DIR) -o $@ $<

$(CODEGEN_BIN): $(CODEGEN_MAIN_SRC) $(CODEGEN_OBJECTS) $(CODEGEN_BIN_DEPS)
	@mkdir -p $(@D)
	$(COBC) -x $(COBC_OPTS) $(COB_MTFLAGS) -I $(CODEGEN_CPY_DIR) -lstdc++ -lz -o $@ $^

$(CODEGEN_OUT_SRC): $(SERVER_JAR_EXTRACTED) $(CODEGEN_BIN) $(CODEGEN_TPL)
	@mkdir -p $(@D)
	./$(CODEGEN_BIN) $(DATA_DIR) $(CODEGEN_OUT_DIR) $(CODEGEN_TPL_DIR)

$(CODEGEN_OUT_OBJECTS): $(CODEGEN_OUT_SRC)
	@mkdir -p $(@D)
	$(COBC) -c $(COBC_OPTS) $(COB_MTFLAGS) -o $@ $<

$(BIN): $(MAIN_SRC) $(OBJECTS) $(CPP_OBJECTS) $(CODEGEN_OUT_OBJECTS)
	@mkdir -p $(@D)
	$(COBC) -x $(COBC_OPTS) $(COB_MTFLAGS) -lstdc++ -lz -o $@ $^

$(TEST_OBJECTS): $(OBJECTS_DIR)/tests/%.o: $(ROOT_DIR)/tests/%.cob $(GLOBALDEPS)
	@mkdir -p $(@D)
	$(COBC) -c $(COBC_OPTS) $(COB_MTFLAGS) -o $@ $<

$(TEST_BIN): $(TEST_MAIN_SRC) $(TEST_OBJECTS) $(OBJECTS) $(CPP_OBJECTS) $(CODEGEN_OUT_OBJECTS)
	@mkdir -p $(@D)
	$(COBC) -x $(COBC_OPTS) $(COB_MTFLAGS) -lstdc++ -lz -o $@ $^
