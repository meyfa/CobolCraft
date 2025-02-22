# Compiler
COBC = cobc
CC = g++

# Directories
OBJECTS_DIR = out
ROOT_DIR = .

# Sources, copybooks, and binary
SRC = $(wildcard $(ROOT_DIR)/src/*.cob $(ROOT_DIR)/src/*/*.cob $(ROOT_DIR)/src/*/*/*.cob $(ROOT_DIR)/src/*/*/*/*.cob)
MAIN_SRC = $(ROOT_DIR)/main.cob
OBJECTS = $(patsubst $(ROOT_DIR)/src/%.cob, $(OBJECTS_DIR)/%.o, $(SRC))
CPY_DIR = $(ROOT_DIR)/src/copybooks
CPP_SRC = $(wildcard $(ROOT_DIR)/cpp/*.cpp)
CPP_HEADERS = $(wildcard $(ROOT_DIR)/cpp/*.h)
CPP_OBJECTS = $(patsubst $(ROOT_DIR)/cpp/%.cpp, $(OBJECTS_DIR)/cpp/%.o, $(CPP_SRC))

BIN = cobolcraft

all: $(BIN) data

# Data extraction from Mojang's server.jar
SERVER_URL = https://piston-data.mojang.com/v1/objects/4707d00eb834b446575d89a61a11b5d548d8c001/server.jar
SERVER_JAR_EXTRACTED = data/versions/1.21.4/server-1.21.4.jar

# Test sources and binary
TEST_SRC = $(wildcard $(ROOT_DIR)/tests/*.cob $(ROOT_DIR)/tests/*/*.cob)
TEST_MAIN_SRC = $(ROOT_DIR)/test.cob
TEST_OBJECTS = $(patsubst $(ROOT_DIR)/tests/%.cob, $(OBJECTS_DIR)/tests/%.o, $(TEST_SRC))
TEST_BIN = test

# GnuCOBOL >=3.2 added functions that drastically improve performance. Query the installed version (may have one sub-digit)
GCVERSION := $(shell $(COBC) --version | head -n1 | sed 's/.*\s\([0-9]\+\)\.\([0-9]\).*/\1\2/g' )

# Common compiler options, note: COB_FLAGS can be user-specified
COBC_OPTS = -free -DGCVERSION=$(GCVERSION) -I $(CPY_DIR) $(COB_FLAGS)
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
GLOBALDEPS = $(wildcard $(CPY_DIR)/*.cpy)
endif

.SUFFIXES:
.PHONY: all clean cobclean data run test

cobclean:
	rm -rf $(OBJECTS_DIR)
	rm -f $(BIN)
	rm -f $(TEST_BIN)

clean: cobclean
	rm -f $(JSON_DATA)
	rm -rf data

run: all
	./$(BIN)

data: $(SERVER_JAR_EXTRACTED)

$(SERVER_JAR_EXTRACTED):
	mkdir -p data
	curl -o data/server.jar $(SERVER_URL)
	cd data && java -DbundlerMainClass="net.minecraft.data.Main" -jar server.jar --reports --server

$(OBJECTS): $(OBJECTS_DIR)/%.o: $(ROOT_DIR)/src/%.cob $(GLOBALDEPS)
	@mkdir -p $(@D)
	$(COBC) -c $(COBC_OPTS) $(COB_MTFLAGS) -o $@ $<

$(CPP_OBJECTS): $(CPP_SRC) $(CPP_HEADERS)
	@mkdir -p $(@D)
	$(CC) -c -Wall -O2 -fPIC -o $@ $<

$(BIN): $(MAIN_SRC) $(OBJECTS) $(CPP_OBJECTS)
	$(COBC) -x $(COBC_OPTS) $(COB_MTFLAGS) -lstdc++ -lz -o $@ $(MAIN_SRC) $(OBJECTS) $(CPP_OBJECTS)

$(TEST_OBJECTS): $(OBJECTS_DIR)/tests/%.o: $(ROOT_DIR)/tests/%.cob $(GLOBALDEPS)
	@mkdir -p $(@D)
	$(COBC) -c $(COBC_OPTS) $(COB_MTFLAGS) -o $@ $<

test: $(TEST_MAIN_SRC) $(TEST_OBJECTS) $(OBJECTS) $(CPP_OBJECTS)
	$(COBC) -x $(COBC_OPTS) $(COB_MTFLAGS) -lstdc++ -lz -o $@ $(TEST_MAIN_SRC) $(TEST_OBJECTS) $(OBJECTS) $(CPP_OBJECTS)
	./$(TEST_BIN)
