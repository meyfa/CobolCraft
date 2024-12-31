# Compiler
COBC = cobc

# Libraries
UTIL_LIB = COBOLCRAFT_UTIL.so

# Sources, copybooks, and binary
SRC = $(wildcard src/*.cob src/*/*.cob)
MAIN_SRC = main.cob
OBJECTS_DIR = out
OBJECTS = $(patsubst src/%.cob, out/%.o, $(SRC))
CPY_DIR = src/copybooks
CPY = $(wildcard src/copybooks/*.cpy)
BIN = cobolcraft

# Data extraction from Mojang's server.jar
SERVER_URL = https://piston-data.mojang.com/v1/objects/4707d00eb834b446575d89a61a11b5d548d8c001/server.jar
SERVER_JAR_EXTRACTED = data/versions/1.21.4/server-1.21.4.jar

# Test sources and binary
TEST_SRC = test.cob $(wildcard tests/*.cob)
TEST_BIN = test

.PHONY: all clean data run test

all: $(BIN) $(UTIL_LIB) data

clean:
	rm -rf $(OBJECTS_DIR)
	rm -f $(BIN)
	rm -f $(UTIL_LIB)
	rm -f $(TEST_BIN)
	rm -f $(JSON_DATA)
	rm -rf data

data: $(SERVER_JAR_EXTRACTED)

run: all
	COB_PRE_LOAD=COBOLCRAFT_UTIL ./$(BIN)

$(UTIL_LIB): cpp/cobolcraft_util.cpp
	g++ -shared -Wall -O2 -fPIC -lz -o $@ $<

$(SERVER_JAR_EXTRACTED):
	mkdir -p data
	curl -o data/server.jar $(SERVER_URL)
	cd data && java -DbundlerMainClass="net.minecraft.data.Main" -jar server.jar --reports --server

$(OBJECTS): out/%.o: src/%.cob $(CPY)
	@mkdir -p $(@D)
	$(COBC) -c -O2 -debug -Wall -fnotrunc --free -I $(CPY_DIR) -o $@ $<

$(BIN): $(CPY) $(MAIN_SRC) $(OBJECTS)
	$(COBC) -x -O2 -debug -Wall -fnotrunc --free -I $(CPY_DIR) -o $@ $(MAIN_SRC) $(OBJECTS)

test: $(TEST_SRC) $(SRC) $(CPY) $(UTIL_LIB)
	$(COBC) -x -debug -Wall -fnotrunc --free -lstdc++ -I $(CPY_DIR) -o $@ $(TEST_SRC) $(SRC)
	COB_PRE_LOAD=COBOLCRAFT_UTIL ./$(TEST_BIN)
