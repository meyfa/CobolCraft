# Compiler
COBC = cobc
CC = g++

# Sources, copybooks, and binary
SRC = $(wildcard src/*.cob src/*/*.cob src/*/*/*.cob src/*/*/*/*.cob)
MAIN_SRC = main.cob
OBJECTS_DIR = out
OBJECTS = $(patsubst src/%.cob, out/%.o, $(SRC))
CPY_DIR = src/copybooks
CPY = $(wildcard src/copybooks/*.cpy)
CPP_SRC = $(wildcard cpp/*.cpp)
CPP_HEADERS = $(wildcard cpp/*.h)
CPP_OBJECTS = $(patsubst cpp/%.cpp, out/cpp/%.o, $(CPP_SRC))

BIN = cobolcraft

# Data extraction from Mojang's server.jar
SERVER_URL = https://piston-data.mojang.com/v1/objects/4707d00eb834b446575d89a61a11b5d548d8c001/server.jar
SERVER_JAR_EXTRACTED = data/versions/1.21.4/server-1.21.4.jar

# Test sources and binary
TEST_SRC = test.cob $(wildcard tests/*.cob)
TEST_BIN = test

# GnuCOBOL >=3.2 added functions that drastically improve performance. Set this to 32 to enable them.
GCVERSION=31

# Common compiler options
COBC_OPTS = -O2 -debug -Wall -fnotrunc --free -fstatic-call -DGCVERSION=$(GCVERSION)

.PHONY: all clean data run test

all: $(BIN) data

clean:
	rm -rf $(OBJECTS_DIR)
	rm -f $(BIN)
	rm -f $(TEST_BIN)
	rm -f $(JSON_DATA)
	rm -rf data

data: $(SERVER_JAR_EXTRACTED)

run: all
	./$(BIN)

$(SERVER_JAR_EXTRACTED):
	mkdir -p data
	curl -o data/server.jar $(SERVER_URL)
	cd data && java -DbundlerMainClass="net.minecraft.data.Main" -jar server.jar --reports --server

$(OBJECTS): out/%.o: src/%.cob $(CPY)
	@mkdir -p $(@D)
	$(COBC) -c $(COBC_OPTS) -I $(CPY_DIR) -o $@ $<

$(CPP_OBJECTS): $(CPP_SRC) $(CPP_HEADERS)
	@mkdir -p $(@D)
	$(CC) -c -Wall -O2 -fPIC -o $@ $<

$(BIN): $(CPY) $(MAIN_SRC) $(OBJECTS) $(CPP_OBJECTS)
	$(COBC) -x $(COBC_OPTS) -I $(CPY_DIR) -lstdc++ -lz -o $@ $(MAIN_SRC) $(OBJECTS) $(CPP_OBJECTS)

test: $(CPY) $(TEST_SRC) $(OBJECTS) $(CPP_OBJECTS)
	$(COBC) -x $(COBC_OPTS) -I $(CPY_DIR) -lstdc++ -lz -o $@ $(TEST_SRC) $(OBJECTS) $(CPP_OBJECTS)
	./$(TEST_BIN)
