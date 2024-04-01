/*
   Copyright (C) 2010-2014,2016,2019 Free Software Foundation, Inc.

   cob_socket including this file is in preparation to be a part of GnuCOBOL.

   The GnuCOBOL runtime library is free software: you can redistribute it
   and/or modify it under the terms of the GNU Lesser General Public License
   as published by the Free Software Foundation, either version 3 of the
   License, or (at your option) any later version.

   cob_socket is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public License
   along with cob_socket.  If not, see <http://www.gnu.org/licenses/>.
*/


// macros for DLL export
//
#ifdef __cplusplus
#define EXTERN_DECL extern "C"
#else
#define EXTERN_DECL extern
#endif

#ifdef WIN32
	#ifdef COB_SOCKET_EXPORTS
	#define COB_SOCKET_API EXTERN_DECL __declspec(dllexport)
	#else
	#define COB_SOCKET_API EXTERN_DECL __declspec(dllimport)
	#endif
#else
	#define COB_SOCKET_API EXTERN_DECL
#endif

// data types
//
#ifdef WIN32
	#define COBSOCK_socket_t	SOCKET	// socket descriptor
	#define COBSOCK_socklen_t	int		// type for length of socket descriptor
#else
	#define COBSOCK_socket_t	int
	#define COBSOCK_socklen_t	socklen_t
#endif


// return values
//
#ifdef WIN32
	#define COBSOCK_SOCKET_ERROR	SOCKET_ERROR	// error at socket action
	#define COBSOCK_INVALID_SOCKET	INVALID_SOCKET	// invalid socket descriptor

	#define COBSOCK_EINVAL			WSAEINVAL
	#define COBSOCK_EADDRINUSE		WSAEADDRINUSE
	#define COBSOCK_ENETUNREACH		WSAENETUNREACH
	#define COBSOCK_ECONNREFUSED	WSAECONNREFUSED
	#define COBSOCK_EISCONN			WSAEISCONN
	#define COBSOCK_EINVAL          WSAEINVAL
#else
	#define COBSOCK_SOCKET_ERROR	-1
	#define COBSOCK_INVALID_SOCKET	-1

	#define COBSOCK_EINVAL			EINVAL
	#define COBSOCK_EADDRINUSE		EADDRINUSE
	#define COBSOCK_ENETUNREACH		ENETUNREACH
	#define COBSOCK_ECONNREFUSED	ECONNREFUSED
	#define COBSOCK_EISCONN			EISCONN
	#define COBSOCK_EINVAL          EINVAL
#endif


// functions
//
#ifdef WIN32
	#define COBSOCK_read(x,y,z)		recv(x,y,z,0)	// read from socket
	#define COBSOCK_write(x,y,z)	send(x,y,z,0)	// write to socket
	#define COBSOCK_close			closesocket		// close socket
	#define snprintf				_snprintf
#else
	#define COBSOCK_read(x,y,z)		read(x,y,z)
	#define COBSOCK_write(x,y,z)	write(x,y,z)
	#define COBSOCK_close			close
#endif

// function header for external call
//
COB_SOCKET_API int CBL_GC_SOCKET (char* p_code, char* p1, char* p2, char* p3, char* p4, char* p5, char* p6, char* pdummy);
COB_SOCKET_API int CBL_OC_SOCKET (char* p_code, char* p1, char* p2, char* p3, char* p4, char* p5, char* p6, char* pdummy);

// Some pragmas
//
#ifdef _MSC_VER
	#pragma warning(disable:4786)   
#endif
