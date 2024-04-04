# Compiler
COBC = cobc

SOCKET_LIB = CBL_GC_SOCKET.so
UTIL_LIB = COBOLCRAFT_UTIL.so
SRC = main.cob src/*.cob src/*/*.cob
BIN = cobolcraft

all: $(BIN)

$(SOCKET_LIB):
	cd CBL_GC_SOCKET && ./build.sh
	mv CBL_GC_SOCKET/CBL_GC_SOCKET.so .

$(UTIL_LIB): cpp/cobolcraft_util.cpp
	g++ -shared -Wall -O2 -fPIC -o $@ $<

$(BIN): $(SOCKET_LIB) $(UTIL_LIB) $(SRC)
	$(COBC) -x -debug -Wall -fnotrunc --free -lstdc++ -o $@ $(SRC)

clean:
	rm -f $(BIN)
	rm -f $(SOCKET_LIB)
	rm -f $(UTIL_LIB)

run: $(BIN)
	COB_PRE_LOAD=CBL_GC_SOCKET:COBOLCRAFT_UTIL ./$(BIN)
