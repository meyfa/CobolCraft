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

//#define DEBUG 1

// our own headers
//
#define COB_SOCKET_EXPORTS
#include "cob_socket.h"

// includes for sockets
//
#ifdef WIN32
	#include <windows.h>
	#include <winerror.h>
	#include <winsock.h>
#else
	#include <sys/types.h>
	#include <sys/socket.h>
	#include <netinet/in.h>
	#include <netdb.h>
	#include <unistd.h>
	#include <arpa/inet.h>
	#include <errno.h>
#endif
#ifdef HAVE_STRING_H
	#include <string.h>
#endif

// includes independent from OS
//
#include <string>
#include <set>
#include <stdio.h>

// main DLL function, only Windows
//
#ifdef WIN32
	BOOL APIENTRY DllMain(HANDLE hModule, DWORD ul_reason_for_call, LPVOID lpReserved)
	{
		switch (ul_reason_for_call)
		{
			case DLL_PROCESS_ATTACH:
			case DLL_THREAD_ATTACH:
			case DLL_THREAD_DETACH:
			case DLL_PROCESS_DETACH:
				break;
		}
		return TRUE;
	}
#endif

//
// specific functions which should not be accessed from outside

// conversion function

static unsigned cob2unsigned(char* p_data, unsigned int p_size)
{
   unsigned l_res = 0;
   if(p_data)
   {
	unsigned int l_i;
	for(l_i = 0; l_i < p_size; l_i++)
	{
	   if(l_i == 0 && *(p_data+l_i) == '+') continue; // ignore + at beginning
	   if(*(p_data+l_i) < '0' || *(p_data+l_i) > '9') break; // only numbers
	   if(l_i != 0) l_res *= 10;
	   l_res += *(p_data+l_i) - '0';
	}
   }

   return l_res;
}

//

// static buffer for errno
//
static int st_errno = 0;

// static set of connections for the pool
//
std::set<COBSOCK_socket_t> st_sockets;
/***********************************************************************************/
// function to save errno
//
int _cobsocket_save_errno()
{
#ifdef WIN32
	st_errno = WSAGetLastError();
#else
	st_errno = errno;
#endif
#ifdef DEBUG
	printf("\t_cobsocket_save_errno called: %d\r\n", st_errno);
#endif
	return st_errno;
}
/***********************************************************************************/
// Simple method which saves errno and return a specific code
//
int _cobsocket_raise_error(int p_code)
{
#ifdef DEBUG
	printf("\t_cobsocket_raise_error called: %d\r\n", p_code);
#endif
	_cobsocket_save_errno();
	return p_code;
}
/***********************************************************************************/
int _cobsocket_lasterror(char* p_msgbuf = NULL)
{
#ifdef DEBUG
	printf("\t_cobsocket_lasterror called: %d\r\n", st_errno);
#endif
	
	// retrieve error message if wished
	//
	if(p_msgbuf)
	{
		const size_t l_buffsize = 256;

		// fill buffer with spaces
		//
		memset(p_msgbuf, ' ', l_buffsize);

#ifdef WIN32
		// Windows needs more handwork
		//
		std::string l_msg = "";
		DWORD l_ret;
		LPTSTR l_tmp = NULL;
		l_ret = FormatMessage( FORMAT_MESSAGE_ALLOCATE_BUFFER | FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_ARGUMENT_ARRAY,	NULL, st_errno, LANG_NEUTRAL, (LPTSTR)&l_tmp, 0, NULL);

		// handle socket errors
		//
		if(l_ret) l_msg.assign(l_tmp);
		else
		{
			switch(st_errno) {
			case WSAEINTR:
				l_msg.assign("A blocking operation was interrupted by a call to WSACancelBlockingCall.");
				break;

			case WSAEBADF:
				l_msg.assign("The socket handle supplied is not valid.");
				break;

			case WSAEACCES:
				l_msg.assign("An attempt was made to access a socket in a way forbidden by its access permissions.");
				break;

			case WSAEFAULT:
				l_msg.assign("The system detected an invalid pointer address in attempting to use a pointer argument in a call.");
				break;

			case WSAEINVAL:
				l_msg.assign("An invalid argument was supplied.");
				break;

			case WSAEMFILE:
				l_msg.assign("Too many open sockets.");
				break;

			case WSAEWOULDBLOCK:
				l_msg.assign("A non-blocking socket operation could not be completed immediately.");
				break;

			case WSAEINPROGRESS:
				l_msg.assign("A blocking operation is currently executing.");
				break;

			case WSAEALREADY:
				l_msg.assign("An operation was attempted on a non-blocking socket that already had an operation in progress.");
				break;

			case WSAENOTSOCK:
				l_msg.assign("An operation was attempted on something that is not a socket.");
				break;

			case WSAEDESTADDRREQ:
				l_msg.assign("A required address was omitted from an operation on a socket.");
				break;

			case WSAEMSGSIZE:
				l_msg.assign("A message sent on a datagram socket was larger than the internal message buffer or some other network limit, or the buffer used to receive a datagram into was smaller than the datagram itself.");
				break;

			case WSAEPROTOTYPE:
				l_msg.assign("A protocol was specified in the socket function call that does not support the semantics of the socket type requested.");
				break;

			case WSAENOPROTOOPT:
				l_msg.assign("An unknown, invalid, or unsupported option or level was specified in a getsockopt or setsockopt call.");
				break;

			case WSAEPROTONOSUPPORT:
				l_msg.assign("The requested protocol has not been configured into the system, or no implementation for it exists.");
				break;

			case WSAESOCKTNOSUPPORT:
				l_msg.assign("The support for the specified socket type does not exist in this address family.");
				break;

			case WSAEOPNOTSUPP:
				l_msg.assign("The attempted operation is not supported for the type of object referenced.");
				break;

			case WSAEPFNOSUPPORT:
				l_msg.assign("The protocol family has not been configured into the system or no implementation for it exists.");
				break;

			case WSAEAFNOSUPPORT:
				l_msg.assign("An address incompatible with the requested protocol was used.");
				break;

			case WSAEADDRINUSE:
				l_msg.assign("Only one usage of each socket address (protocol/network address/port) is normally permitted.");
				break;

			case WSAEADDRNOTAVAIL:
				l_msg.assign("The requested address is not valid in its context.");
				break;

			case WSAENETDOWN:
				l_msg.assign("A socket operation encountered a dead network.");
				break;

			case WSAENETUNREACH:
				l_msg.assign("A socket operation was attempted to an unreachable network.");
				break;

			case WSAENETRESET:
				l_msg.assign("The connection has been broken due to keep-alive activity detecting a failure while the operation was in progress.");
				break;

			case WSAECONNABORTED:
				l_msg.assign("An established connection was aborted by the software in your host machine.");
				break;

			case WSAECONNRESET:
				l_msg.assign("An existing connection was forcibly closed by the remote host.");
				break;

			case WSAENOBUFS:
				l_msg.assign("An operation on a socket could not be performed because the system lacked sufficient buffer space or because a queue was full.");
				break;

			case WSAEISCONN:
				l_msg.assign("A connect request was made on an already connected socket.");
				break;

			case WSAENOTCONN:
				l_msg.assign("A request to send or receive data was disallowed because the socket is not connected and (when sending on a datagram socket using a sendto call) no address was supplied.");
				break;

			case WSAESHUTDOWN:
				l_msg.assign("A request to send or receive data was disallowed because the socket had already been shut down in that direction with a previous shutdown call.");
				break;

			case WSAETOOMANYREFS:
				l_msg.assign("Too many references to some kernel object.");
				break;

			case WSAETIMEDOUT:
				l_msg.assign("A connection attempt failed because the connected party did not properly respond after a period of time, or established connection failed because connected host has failed to respond.");
				break;

			case WSAECONNREFUSED:
				l_msg.assign("No connection could be made because the target machine actively refused it.");
				break;

			case WSAELOOP:
				l_msg.assign("Cannot translate name.");
				break;

			case WSAENAMETOOLONG:
				l_msg.assign("Name component or name was too long.");
				break;

			case WSAEHOSTDOWN:
				l_msg.assign("A socket operation failed because the destination host was down.");
				break;

			case WSAEHOSTUNREACH:
				l_msg.assign("A socket operation was attempted to an unreachable host.");
				break;

			case WSAENOTEMPTY:
				l_msg.assign("Cannot remove a directory that is not empty.");
				break;

			case WSAEPROCLIM:
				l_msg.assign("A Windows Sockets implementation may have a limit on the number of applications that may use it simultaneously.");
				break;

			case WSAEUSERS:
				l_msg.assign("Ran out of quota.");
				break;

			case WSAEDQUOT:
				l_msg.assign("Ran out of disk quota.");
				break;

			case WSAESTALE:
				l_msg.assign("File handle reference is no longer available.");
				break;

			case WSAEREMOTE:
				l_msg.assign("Item is not available locally.");
				break;

			case WSASYSNOTREADY:
				l_msg.assign("WSAStartup cannot function at this time because the underlying system it uses to provide network services is currently unavailable.");
				break;

			case WSAVERNOTSUPPORTED:
				l_msg.assign("The Windows Sockets version requested is not supported.");
				break;

			case WSANOTINITIALISED:
				l_msg.assign("Either the application has not called WSAStartup, or WSAStartup failed.");
				break;

			case WSAEDISCON:
				l_msg.assign("Returned by WSARecv or WSARecvFrom to indicate the remote party has initiated a graceful shutdown sequence.");
				break;

			case WSAHOST_NOT_FOUND:
				l_msg.assign("No such host is known.");
				break;

			case WSATRY_AGAIN:
				l_msg.assign("This is usually a temporary error during hostname resolution and means that the local server did not receive a response from an authoritative server.");
				break;

			case WSANO_RECOVERY:
				l_msg.assign("A non-recoverable error occurred during a database lookup.");
				break;

			case WSANO_DATA:
				l_msg.assign("The requested name is valid and was found in the database, but it does not have the correct associated data being resolved for.");
				break;

			default:
				l_msg.assign("Unknown error.");
				break;
			}
		}

		// cleanup global buffer
		//
		if(NULL != l_tmp) LocalFree((HLOCAL)l_tmp);

		if(l_msg.size() > 0)
		{
			if(l_msg.size() > l_buffsize) memcpy(p_msgbuf, l_msg.c_str(), l_buffsize);
			else memcpy(p_msgbuf, l_msg.c_str(), l_msg.size());

		}

#else
		// UNIX is easy
		//
		char* l_msg = strerror(st_errno);
		if(l_msg && strlen(l_msg) > 0)
		{
			if(strlen(l_msg) > l_buffsize) memcpy(p_msgbuf, l_msg, l_buffsize);
			else memcpy(p_msgbuf, l_msg, strlen(l_msg));
		}
#endif
	}
	
	// return last error code
	//
	return st_errno;
}
/***********************************************************************************/
// method to dump the set of active sockets
//
void _cobsocket_dump_sockets()
{
	std::set<COBSOCK_socket_t>::const_iterator l_iter = st_sockets.begin();

#ifdef DEBUG
	printf("\t_cobsocket_dump_sockets called\r\n");
#endif

	printf("\tNumber of active sockets: %d\r\n", (int)st_sockets.size());
	for(unsigned int l_i = 0; l_i < st_sockets.size(); l_i++)
	{
		printf("\t%X\r\n", *l_iter);
		l_iter++;
	}
}
/***********************************************************************************/
// add socket to list aof active sockets
//
void _cobsocket_add(const COBSOCK_socket_t& p_socket)
{
#ifdef DEBUG
	printf("\t_cobsocket_add called\r\n");
	printf("\ttry to add socket: %X\r\n", p_socket);
#endif

	st_sockets.insert(p_socket);

#ifdef DEBUG
	_cobsocket_dump_sockets();
#endif
}
/***********************************************************************************/
// remove socket from list of active sockets
//
int _cobsocket_remove(const COBSOCK_socket_t& p_socket)
{
#ifdef DEBUG
	printf("\t_cobsocket_remove called\r\n");
	printf("\ttry to remove socket: %X\r\n", p_socket);
#endif

	if(st_sockets.size() > 0) st_sockets.erase(p_socket);

#ifdef DEBUG
	_cobsocket_dump_sockets();
#endif

	return 0;
}
/***********************************************************************************/
int _cobsocket_open(unsigned p_port, char* p_socket) // open socket to listen
{	
    COBSOCK_socket_t l_socket;
    struct sockaddr_in l_srv;
	
#ifdef DEBUG
	printf("\t_cobsocket_open called\r\n");
#endif
	
    if(!p_port) return 99;
    if(!p_socket) return 99;
	
#ifdef DEBUG
	printf("\t_cobsocket_open: p_port: %d\r\n",p_port);
#endif

	// open socket
	//
#ifdef DEBUG
	printf("\t_cobsocket_open: socket()\r\n");
#endif
	l_socket = socket(AF_INET,SOCK_STREAM,0);
	if(l_socket == COBSOCK_INVALID_SOCKET) return _cobsocket_raise_error(2);

	// create address for the port
	//
	l_srv.sin_family      = AF_INET;
	l_srv.sin_addr.s_addr = htonl(INADDR_ANY);
	l_srv.sin_port        = htons(p_port);

	// set option for fast reuse
	//
	int l_flag = 1;
	if(setsockopt(l_socket, SOL_SOCKET, SO_REUSEADDR, (char *)&l_flag, sizeof(l_flag)) == COBSOCK_SOCKET_ERROR) return _cobsocket_raise_error(98);

	// bind the socket to the requested port
	//
#ifdef DEBUG
	printf("\t_cobsocket_open: bind()\r\n");
#endif
	if(bind(l_socket, (struct sockaddr *) &l_srv,sizeof(l_srv)) == COBSOCK_SOCKET_ERROR) 
	{
		_cobsocket_save_errno();
		switch(_cobsocket_lasterror())
		{
			case COBSOCK_EINVAL:
			case COBSOCK_EADDRINUSE:
				return 1;
		}
		return 98;
	}

	// listen to socket
	//
#ifdef DEBUG
	printf("\t_cobsocket_open: listen()\r\n");
#endif
	if(listen(l_socket,1) == COBSOCK_SOCKET_ERROR) return _cobsocket_raise_error(2);

	// store the socket handle in buffer
	//
	memcpy(p_socket,&l_socket,sizeof(COBSOCK_socket_t));
	
	return 0;
}
/***********************************************************************************/
int _cobsocket_accept(char* p_socket, char* p_connection = NULL) // accept connection over socket
{
	COBSOCK_socket_t l_sock, l_clnt;

	sockaddr_in l_cl_adr;
	COBSOCK_socklen_t l_adr_len = sizeof(l_cl_adr);
   
#ifdef DEBUG
	printf("\t_cobsocket_accept called\r\n");
#endif

	if(!p_socket) return 99;

#ifdef DEBUG
	printf("\t_cobsocket_accept()\r\n");
#endif

	// get the handle
	//
	memcpy(&l_sock,p_socket,sizeof(COBSOCK_socket_t));

	// wait for incoming connections
	//
#ifdef DEBUG
	printf("\t_cobsocket_accept: accept()\r\n");
#endif
	l_clnt = accept(l_sock, (struct sockaddr*) &l_cl_adr, &l_adr_len);
	if(l_clnt == COBSOCK_INVALID_SOCKET)
	{
		_cobsocket_save_errno();
		switch(_cobsocket_lasterror())
		{
#ifdef WIN32
		case WSAENOTSOCK: // no socket
#else
		case EBADF: // no socket
#endif
			return 1;

		case COBSOCK_EINVAL: // not listening 
			return 2;
		}
		return 98;
	}

	// close the listening socket, but not when listening socket should be rmeain active
	//
	if(!p_connection) 
	{
	   COBSOCK_close(l_sock);

	   // our connection is the new handle
	   //
	   memcpy(p_socket,&l_clnt,sizeof(COBSOCK_socket_t));
	}
	else
	{
	   memcpy(p_connection,&l_clnt,sizeof(COBSOCK_socket_t));
	}
	
	return 0;
}
/***********************************************************************************/
int _cobsocket_connect(const char* p_addr, unsigned p_port, char* p_socket) // connect to socket
{
	COBSOCK_socket_t l_socket;
	struct sockaddr_in l_srv;
	struct hostent* l_host;

#ifdef DEBUG
	printf("\t_cobsocket_connect called\r\n");
#endif
	
	if(!p_addr) return 99;
	if(!p_port) return 99;
	if(!p_socket) return 99;

#ifdef DEBUG
	printf("\t_cobsocket_connect: p_addr: %s, p_port: %d\r\n",p_addr,p_port);
#endif

	// open the socket
	//
#ifdef DEBUG
	printf("\t_cobsocket_connect: socket()\r\n");
#endif
	l_socket = socket(AF_INET,SOCK_STREAM,0);
	if(l_socket == COBSOCK_INVALID_SOCKET) return _cobsocket_raise_error(3);


	// Prepare address structure
	//
	l_srv.sin_family      = AF_INET;
	l_srv.sin_addr.s_addr = inet_addr(p_addr);
	l_srv.sin_port        = htons(p_port);

	// Try resolve of name
	//
#ifdef DEBUG
	printf("\t_cobsocket_connect: gethostbyname\r\n");
#endif
	l_host = gethostbyname(p_addr);
	if(l_host)
	{
		l_srv.sin_family      = l_host->h_addrtype;
		memcpy(&l_srv.sin_addr, l_host->h_addr, sizeof(struct in_addr));
	}
	
	// connect to server
	//
#ifdef DEBUG
	printf("\t_cobsocket_connect: connect()\r\n");
#endif
	
	if(connect(l_socket, (struct sockaddr*) &l_srv, sizeof(l_srv)) == COBSOCK_SOCKET_ERROR)
	{
		_cobsocket_save_errno();
		switch(_cobsocket_lasterror())
		{
		case COBSOCK_ENETUNREACH: // address unreachable
		case COBSOCK_ECONNREFUSED:// none listen at this port
			return 1;

		case COBSOCK_EISCONN: // already connected
			return 2;
		}
		return 98;
	}

	// copy our socket descriptor into buffer
	//
	memcpy(p_socket,&l_socket,sizeof(COBSOCK_socket_t));
	
	return 0;
}
/***********************************************************************************/
int _cobsocket_writen(char* p_socket, unsigned p_bytes, char* p_data) // send data over socket
{
	COBSOCK_socket_t l_socket;
	int l_written, l_left;
   
#ifdef DEBUG
	printf("\t_cobsocket_writen called\r\n");
#endif
	
	if(!p_socket) return 99;
	if(!p_bytes || p_bytes < 0) return 99;
	if(!p_data) return 99;

#ifdef DEBUG
	printf("\t_cobsocket_writen: p_bytes: %d\r\n",p_bytes);
#endif

	// get our socket from char buffer
	//
	memcpy(&l_socket,p_socket,sizeof(COBSOCK_socket_t));

	// write data to socket
	//
	l_left = p_bytes;
	l_written = 0;

	while(l_left > 0)
	{
#ifdef DEBUG
	printf("\t_cobsocket_connect: write()\r\n");
#endif
	   l_written = COBSOCK_write(l_socket,p_data,l_left);
	   if(l_written == COBSOCK_SOCKET_ERROR) 
	   {
			_cobsocket_save_errno();
			switch(_cobsocket_lasterror())
			{
#ifdef WIN32
			case WSAENOTSOCK: // not a socket 
			case WSAENOTCONN: // no connection
#else
			case EBADF: // invalid socket or not ready for writing
#endif
				return 3;

#ifdef WIN32
			case WSAEMSGSIZE: // data exeeds limit for writing
#else
			case EFBIG: // data exeeds limit for writing
#endif
				return 4;
			}
			return 98;
	   }

	   l_left -= l_written;
	   p_data += l_written;
	}

	// check if nothing is left
	//
	if(l_written < (int)p_bytes) return 2;

	return 0;
}
/***********************************************************************************/
int _cobsocket_selectread(const std::set<COBSOCK_socket_t>& p_sockets, unsigned int p_timeout, COBSOCK_socket_t& p_result, COBSOCK_socket_t* p_begin_after = NULL) // wait for read ops on sockets
{
	if(p_sockets.size() == 0) return 99;

	unsigned int l_i;
	int l_ret;
	fd_set l_readsocks;
	struct timeval l_time;
	COBSOCK_socket_t l_highest = -1;
	std::set<COBSOCK_socket_t>::const_iterator l_iterator;

	// set timeout
	//
	l_time.tv_sec = p_timeout / 1000;
	l_time.tv_usec = (p_timeout%1000)*1000;

	// fill the set of FDs
	//
	FD_ZERO(&l_readsocks);
	l_iterator = p_sockets.begin();
	for(l_i = 0; l_i < p_sockets.size(); l_i++)
	{
		if(*l_iterator > l_highest) l_highest = *l_iterator;
		FD_SET(*l_iterator, &l_readsocks);
		l_iterator++;
	}

	// wait
	//
	l_ret = select(l_highest + 1, &l_readsocks, NULL, NULL, (p_timeout == 0) ? NULL : &l_time);
	if(l_ret == COBSOCK_SOCKET_ERROR)
	{
		_cobsocket_save_errno();
		return COBSOCK_SOCKET_ERROR;
	}

	// timeout expired
	//
	if(l_ret == 0) return 0;

	// check which socket is readable
	//
	if(p_begin_after) 
	{
	   l_iterator = p_sockets.find(*p_begin_after);
	   if(l_iterator != p_sockets.end()) l_iterator++;
	}
	else l_iterator = p_sockets.end();

	if(l_iterator == p_sockets.end()) l_iterator = p_sockets.begin();
	for(l_i = 0; l_i < p_sockets.size(); l_i++)
	{
		if(FD_ISSET(*l_iterator, &l_readsocks))
		{
			p_result = *l_iterator;
			break;
		}
		l_iterator++;
		if(l_iterator == p_sockets.end()) l_iterator = p_sockets.begin();
	}

	return 1;
}
/***********************************************************************************/
int _cobsocket_selectread(COBSOCK_socket_t& p_socket, unsigned int p_timeout) // wait for read ops on sockets
{
	std::set<COBSOCK_socket_t> l_set;
	COBSOCK_socket_t l_dummy;
	l_set.insert(p_socket);
	return _cobsocket_selectread(l_set, p_timeout, l_dummy);
}
/***********************************************************************************/
/** Reads a specific number of bytes from socket.
* @param p_socket Socket-Hadnle as char-array
* @param p_bytes Number of bytes to receive, after call number of received bytes
* @param p_timeout Maximum period of witing until the data arreives, zero ist infinite
* @return 0 - success
*         1 - number of bytes to read exceeds 64000
*         2 - less bytes received than requested
*         3 - no socket or invalid handle
*         4 - timeout expired
*        98 - common error
*        99 - invalid parameter 
*/
int _cobsocket_readn(char* p_socket, unsigned int& p_bytes, char* p_data, unsigned int p_timeout = 0)
{
	COBSOCK_socket_t l_socket;
	unsigned int l_read, l_left, l_to_read;
	
#ifdef DEBUG
	printf("\t_cobsocket_readn(2) called\r\n");
#endif
	
	if(!p_socket) return 99;
	if(!p_bytes) return 99;
	if(!p_data) return 99;
	
	l_to_read = p_bytes;
	if(l_to_read > 64000) return 1;

#ifdef DEBUG
	printf("\t_cobsocket_readn(2): p_bytes: %d\r\n",l_to_read);
#endif

	// get our socket
	//
	memcpy(&l_socket,p_socket,sizeof(COBSOCK_socket_t));

	// use timeout
	//
	if(p_timeout > 0)
	{
		int l_ret = _cobsocket_selectread(l_socket, p_timeout);

		// timeout expired
		//
		if(l_ret == 0) return 4;

		// error 
		//
		if(l_ret == COBSOCK_SOCKET_ERROR)
		{
			switch(_cobsocket_lasterror())
			{
#ifdef WIN32
			case WSAENOTSOCK: // no socket
#else
			case EBADF: // no socket
#endif
				return 3;
			}
			return 98;
		}

	} // if(p_timeout > 0)

	// read data from socket
	//
	l_left = l_to_read;
	l_read = 0;
	while(l_left > 0)
	{
	   int l_bytes;
	   
#ifdef DEBUG
	printf("\t_cobsocket_readn: read()\r\n");
#endif
		l_bytes = COBSOCK_read(l_socket,p_data,l_left);

		if(l_bytes == COBSOCK_SOCKET_ERROR)
		{
			_cobsocket_save_errno();

#ifdef DEBUG
			printf("\t_cobsocket_readn: lasterror: %d\r\n", _cobsocket_lasterror());
#endif

		    switch(_cobsocket_lasterror())
			{
#ifdef WIN32
			case WSAECONNRESET:
			case WSAECONNABORTED:
				l_bytes = 0;
				break;

			case WSAENOTSOCK: // no socket
			case WSAENOTCONN: // no connection
#else
			case EBADF: // no socket
			case EINVAL: // no reading
#endif
				return 3;
			}
			if(l_bytes) return 98;
		}

		// connection closed by peer
		//
		if(l_bytes == 0) break;

		l_left -= l_bytes;
		l_read += l_bytes;
		p_data += l_bytes;
	}

	// store the number of bytes we have read into buffer
	//
	p_bytes = l_read;

	// check if we had lesser bytes than wanted
	//
	if(l_read < l_to_read) return 2;

	return 0;

}
/***********************************************************************************/
int _cobsocket_readn(char* p_socket, char* p_bytes, char* p_data, unsigned int p_timeout = 0) // read data from socket
{
	unsigned int l_bytes;
	char l_buf[6];
	
#ifdef DEBUG
	printf("\t_cobsocket_readn called\r\n");
#endif
	
	if(!p_socket) return 99;
	if(!p_bytes) return 99;
	if(!p_data) return 99;

	l_bytes = cob2unsigned(p_bytes,5);


#ifdef DEBUG
	printf("\t_cobsocket_readn: p_bytes: %d\r\n",l_bytes);
#endif

	int l_ret = _cobsocket_readn(p_socket, l_bytes, p_data, p_timeout);

	if(l_ret != 2) return l_ret;

	// store the number of bytes we have read into buffer
	//
	snprintf(l_buf,5,"%.5d",l_bytes);
	memcpy(p_bytes,l_buf,5);

	return l_ret;
}
/***********************************************************************************/
int _cobsocket_next_read(char* p_socket, char* p_connection, unsigned int p_timeout = 0) // Check for new connection or read action in existing sockets
{
	if(!p_socket) return 99;
	if(!p_connection) return 99;

	static COBSOCK_socket_t lst_last_sock = COBSOCK_INVALID_SOCKET;
	
	int l_ret;
	COBSOCK_socket_t l_srvsock, l_readsock;
	std::set<COBSOCK_socket_t> l_socks = st_sockets;
	
	// Add the server socket to the set we want wait for but 
	// first check if it is in the set of active conns
	//
	memcpy(&l_srvsock,p_socket,sizeof(COBSOCK_socket_t));
	if(l_socks.find(l_srvsock) != l_socks.end()) return 3;
	l_socks.insert(l_srvsock);

	// Lets wait
	//
	//l_ret = _cobsocket_selectread(l_socks, p_timeout, l_readsock);
	l_ret = _cobsocket_selectread(l_socks, p_timeout, l_readsock, &lst_last_sock);
	
	// check for errors
	//
	if(l_ret == COBSOCK_SOCKET_ERROR)
	{
		switch(_cobsocket_lasterror())
		{
#ifdef WIN32
		case WSAENOTSOCK: // no socket
#else
		case EBADF: // no socket
#endif
			return 4;
		}
		return 98;
	}	

	// something has moved
	//
	if(l_ret >  0)
	{
	   // check if we had to accept a connection
	   //
	   if(l_readsock == l_srvsock)
	   {
		   // accept the connection
		   //
		   l_ret = _cobsocket_accept(p_socket, p_connection);
		   
		   // on success add new connection to set
		   //
		   if(l_ret == 0) 
		   {
			   COBSOCK_socket_t l_socket;
			   memcpy(&l_socket,p_connection,sizeof(COBSOCK_socket_t));
			   //st_sockets.insert(l_socket);
			   _cobsocket_add(l_socket);
		   }
		   
		   return l_ret;
	   }

	   // it must be one of the active connections
	   //
	   memcpy(p_connection,&l_readsock,sizeof(COBSOCK_socket_t));
	   lst_last_sock = l_readsock;
	   return 0;

	} // if(l_ret > 0)
	
	// timout occured
	//
	return 5;
}
/***********************************************************************************/
int _cobsocket_close(char* p_socket) // close socket
{
	COBSOCK_socket_t l_socket;
   
#ifdef DEBUG
	printf("\t_cobsocket_close called\r\n");
#endif

   if(!p_socket) return 99;

#ifdef DEBUG
	printf("\t_cobsocket_close\r\n");
#endif
	
	// get our socket
	//
	memcpy(&l_socket,p_socket,sizeof(COBSOCK_socket_t));

	// remove the socket from list
	//
	_cobsocket_remove(l_socket);
	
	// close it
	//
	if(COBSOCK_close(l_socket) < 0)
	{
		_cobsocket_save_errno();
#ifdef WIN32
		if(_cobsocket_lasterror() == WSAENOTSOCK) return 1; // invalid descriptor
#else
		if(_cobsocket_lasterror() == EBADF) return 1; // invalid descriptor
#endif
		return 98;
	}

	// clear the socket descriptor
	//
	memset(p_socket,0,sizeof(COBSOCK_socket_t));
   
	return 0;
}

/***********************************************************************************/
// generic wrapper function for access from COBOL
//
COB_SOCKET_API
int CBL_GC_SOCKET (char *p_code, char *p1, char *p2, char *p3, char *p4, char *p5, char *p6, char *pdummy)
{
	int l_code, l_ret; 
	int l_i;
	char l_adr[16];
	
#ifdef DEBUG  
	printf("\tcobsocket called\r\n");
#endif

	// get function code
	//
	if(!p_code) return 99;
	l_code = cob2unsigned(p_code,2);
#ifdef DEBUG  
	printf("\tcobsocket: p_code: %d '%s'\r\n",l_code, p_code);
#endif

	// If Windows then call WSAStartup before
	//
#ifdef WIN32
	WORD l_ver = 256*1+1; // We want Version 1.1
	WSADATA l_wsdata;
	memset(&l_wsdata,0,sizeof(WSADATA));
	if(WSAStartup(l_ver,&l_wsdata)) return 97;
#endif

	// analyse it
	//
	switch(l_code)
	{
	// Open server socket
	//
	case 0: 
		
		if(!p1) return 99;
		if(!p2) return 99;   
		
		return _cobsocket_open(cob2unsigned(p1,5),p2);

	// Wait for connection (server sockets only)
	//
	case 7: // listening socket remains open

		if(!p2) return 99;

	case 1: // after accept the listening socket is closed
		
		if(!p1) return 99;

		return _cobsocket_accept(p1, (l_code == 7) ? p2 : NULL);	
	
	// Connect to socket
	//
	case 2: 

		if(!p1) return 99;
		if(!p2) return 99;
		if(!p3) return 99;
		
		memcpy(l_adr,p1,15);
		l_adr[15]='\0';

		// Trim address
		for(l_i = (int)strlen(l_adr)-1; l_i >= 0; l_i--)
		{
			if(l_adr[l_i] != ' ') break;
			else l_adr[l_i] = '\0';
		}
				
		return _cobsocket_connect(l_adr,cob2unsigned(p2,5),p3);

	// Write data
	//
	case 3: 
		
		if(!p1) return 99;
		if(!p2) return 99;
		if(!p3) return 99;
		
		return _cobsocket_writen(p1,cob2unsigned(p2,5),p3);

	// Read data
	//
	case 8: // with timeout

		if(!p4) return 99;

	case 4: // without timeout
		
		if(!p1) return 99;
		if(!p2) return 99;
		if(!p3) return 99;
		
		return _cobsocket_readn(p1,p2,p3, (l_code == 8) ? cob2unsigned(p4,6) : 0);

	// Write data and wait for response
	//
	case 5: 

		if(!p1) return 99;
		if(!p2) return 99;
		if(!p3) return 99;
		if(!p4) return 99;

		// first send some data
		//
	   	l_ret = _cobsocket_writen(p1,cob2unsigned(p2,5),p4);
		switch(l_ret)
		{
		case 0: // no error
			break;

		case 2: // not all data were sent
			return 1;

		case 3: // unknown socket/no connection
			return 5;

		case 4: // too much data to send
			return 2;

		case 5: // common error
			return 6;

		case 99:
			return 99;

		default: // unknown
			return 98;
		}

		// wait for response
		//
		l_ret = _cobsocket_readn(p1,p3,p4);
		switch(l_ret)
		{
		case 0: // no error
			return 0;
			
		case 1: // bytes to receive over 64K
			return 3;

		case 2: // lesser bytes received than requested
			return 4;

		case 3: // unknown socket/no connection
			return 5;

		case 4: // common error
			return 6;

		case 99:
			return 99;
			
		default: // unknown
			return 98;
		}

	// Close connection/server socket
	//
	case 6: 
		
		if(!p1) return 99;
	
		return _cobsocket_close(p1);

	// Get next accepterd connection or the next socket which has data to read
	//
	case 10:
		
		if(!p3) return 99;

	case 9:

		if(!p1) return 99;
		if(!p2) return 99;

		return _cobsocket_next_read(p1, p2, (l_code == 10) ? cob2unsigned(p3,6) : 0);

	// Get last error message from OS or socket-subsystem
	//
	case 98:
		return _cobsocket_lasterror(p1);

	// Get last error from OS or socket-subsystem
	//
	case 99:
		return _cobsocket_lasterror();
		
	default:
		break;
	}
   
	// general error
	//
	return 99;

}

// entry point under old name for delagation
COB_SOCKET_API
int CBL_OC_SOCKET (char *p_code, char *p1, char *p2, char *p3, char *p4, char *p5, char *p6, char *pdummy)
{
	return CBL_GC_SOCKET (p_code, p1, p2, p3, p4, p5, p6, pdummy);
}
