#!/bin/bash

# Script to build release-version of CBL_GC_SOCKET under GNU/Linux
# and other POSIX-compatible systems (including cygwin) + MinGW

# you may replace the defaults with the environment variables
# CXX, CPPFLAGS, CXXFLAGS, LDFLAGS, LIBS

# additional flags may be passed specifying using the environment variables
# CPPFLAGS_ADD CXXFLAGS_ADD LIBS_ADD

# for example Solaris (depending on the ABI needs): CXX="gcc -m64" LIBS_ADD"-lsocket"

# defaults:
if test "x$CXX" = "x"; then CXX="gcc"; fi
if test "x$CPPFLAGS" = "x"; then CPPFLAGS="-DHAVE_STRING_H"; fi
if test "x$CXXFLAGS " = "x"; then CXXFLAGS =""; fi

if test "x$CXXFLAGS" = "x"; then
  if test "x$MSYSTEM" = "x"; then
    CXXFLAGS="-shared -Wall -O2 -fPIC"
  else
    CXXFLAGS="-shared -Wall -O2"
  fi
fi

if test "x$LIBS" = "x"; then
  if test "x$MSYSTEM" = "x"; then
    LIBS="-lstdc++"
  else
    LIBS="-lstdc++ -lws2_32"
  fi
fi

CPPFLAGS="$CPPFLAGS $CPPFLAGS_ADD"
CXXFLAGS="$CXXFLAGS $CXXFLAGS_ADD"
LIBS="$LIBS $LIBS_ADD"

SOURCES=cob_socket.cpp

echo $CXX $CPPFLAGS $CXXFLAGS $SOURCES $LDFLAGS $LIBS -o CBL_GC_SOCKET.so
$CXX $CPPFLAGS $CXXFLAGS $SOURCES $LDFLAGS $LIBS -o CBL_GC_SOCKET.so
