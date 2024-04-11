# Compiler
COBC = cobc

# Libraries
SOCKET_LIB = CBL_GC_SOCKET.so
UTIL_LIB = COBOLCRAFT_UTIL.so

# Sources and binary
SRC = main.cob src/*.cob src/*/*.cob
BIN = cobolcraft

# Data extraction from Mojang's server.jar
JSON_DATA = data/generated/reports/registries.json data/generated/reports/blocks.json

# Test sources and binary
TEST_SRC = test.cob tests/*.cob
TEST_BIN = test

all: $(BIN) $(JSON_DATA)

$(SOCKET_LIB):
	cd CBL_GC_SOCKET && ./build.sh
	mv CBL_GC_SOCKET/CBL_GC_SOCKET.so .

$(UTIL_LIB): cpp/cobolcraft_util.cpp
	g++ -shared -Wall -O2 -fPIC -o $@ $<

$(JSON_DATA):
	mkdir -p data
	curl -o data/server.jar https://piston-data.mojang.com/v1/objects/8dd1a28015f51b1803213892b50b7b4fc76e594d/server.jar
	cd data && java -DbundlerMainClass="net.minecraft.data.Main" -jar server.jar --reports

$(BIN): $(SOCKET_LIB) $(UTIL_LIB) $(SRC)
	$(COBC) -x -debug -Wall -fnotrunc --free -lstdc++ -o $@ $(SRC)

clean:
	rm $(BIN)
	rm $(SOCKET_LIB)
	rm $(UTIL_LIB)
	rm $(TEST_BIN)
	rm -r data/

run: $(BIN) $(JSON_DATA)
	COB_PRE_LOAD=CBL_GC_SOCKET:COBOLCRAFT_UTIL ./$(BIN)

$(TEST_BIN): $(TEST_SRC) $(SRC) $(UTIL_LIB)
	$(COBC) -x -debug -Wall -fnotrunc --free -lstdc++ -o $@ $^
	COB_PRE_LOAD=COBOLCRAFT_UTIL ./$(TEST_BIN)
	rm -f $(TEST_BIN)
