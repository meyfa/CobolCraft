# Compiler
COBC = cobc

LIB = CBL_GC_SOCKET/CBL_GC_SOCKET.so
SRC = server.cob
BIN = server

all: $(BIN)

$(LIB):
	cd CBL_GC_SOCKET && ./build.sh

$(BIN): $(LIB) $(SRC)
	$(COBC) -x -debug -Wall -fnotrunc --free -lstdc++ -o $@ $(SRC)

clean:
	rm -f $(BIN)
	rm -f $(LIB)

run: $(BIN)
	COB_PRE_LOAD=CBL_GC_SOCKET:CBL_GC_SOCKET/CBL_GC_SOCKET.so ./$(BIN)
