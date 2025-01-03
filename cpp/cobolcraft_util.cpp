#include "cobolcraft_util.h"
#include <cstring>
#include <chrono>
#include <csignal>
#include <fcntl.h>
#include <errno.h>
#include <dirent.h>
#include <zlib.h>
#include <set>
#include <vector>
#include <sys/socket.h>
#include <netinet/in.h>
#include <poll.h>

#define ERRNO_PARAMS 99
#define ERRNO_SYSTEM 98

std::set<socket_t> CLIENT_SOCKETS;
constexpr int SOCKET_CHUNK_LIMIT = 64000;

EXTERN_DECL int SystemTimeMicros(long long *timestamp)
{
    if (!timestamp)
    {
        return ERRNO_PARAMS;
    }
    auto time = std::chrono::system_clock::now().time_since_epoch();
    *timestamp = std::chrono::duration_cast<std::chrono::microseconds>(time).count();
    return 0;
}

EXTERN_DECL int SystemTimeMillis(long long *timestamp)
{
    if (!timestamp)
    {
        return ERRNO_PARAMS;
    }
    auto time = std::chrono::system_clock::now().time_since_epoch();
    *timestamp = std::chrono::duration_cast<std::chrono::milliseconds>(time).count();
    return 0;
}

EXTERN_DECL int SetConsoleNonBlocking()
{
    // set stdin to non-blocking mode
    int flags = fcntl(STDIN_FILENO, F_GETFL, 0);
    if (flags == -1)
    {
        return ERRNO_SYSTEM;
    }
    if (fcntl(STDIN_FILENO, F_SETFL, flags | O_NONBLOCK) == -1)
    {
        return ERRNO_SYSTEM;
    }
    return 0;
}

EXTERN_DECL int IgnoreSIGPIPE()
{
    signal(SIGPIPE, SIG_IGN);
    return 0;
}

EXTERN_DECL int ReadConsole(char *buffer, unsigned long *count)
{
    if (!buffer || !count)
    {
        return ERRNO_PARAMS;
    }

    // read from stdin
    int max_length = *count;
    int actual_length = 0;
    while (actual_length < max_length)
    {
        char c;
        int result = read(STDIN_FILENO, &c, 1);
        // If we get EAGAIN or EWOLDBLOCK, it means there is no more data to read.
        // Anything else is an actual error.
        if (result == -1 && errno != EAGAIN && errno != EWOULDBLOCK)
        {
            return ERRNO_SYSTEM;
        }
        if (result <= 0)
        {
            break;
        }
        buffer[actual_length] = c;
        ++actual_length;
    }

    // update actual length
    *count = actual_length;

    return 0;
}

EXTERN_DECL int LeadingZeros32(unsigned long *value, unsigned long *count)
{
    if (!value || !count)
    {
        return ERRNO_PARAMS;
    }
    // input of 0 has undefined behavior in __builtin_clz
    if (*value == 0)
    {
        *count = 32;
        return 0;
    }
    *count = __builtin_clz(*value);
    return 0;
}

EXTERN_DECL int OpenDirectory(char *path, unsigned long *path_length, unsigned long long *handle)
{
    static char buffer[65536];
    if (!path || !path_length || *path_length == 0 || *path_length >= sizeof(buffer))
    {
        return ERRNO_PARAMS;
    }
    memcpy(buffer, path, *path_length);
    buffer[*path_length] = '\0';
    DIR *dir;
    if ((dir = opendir(buffer)) == NULL)
    {
        return ERRNO_SYSTEM;
    }
    *handle = (unsigned long long)dir;
    return 0;
}

EXTERN_DECL int ReadDirectory(unsigned long long *handle, char *entry)
{
    if (!handle || !entry)
    {
        return ERRNO_PARAMS;
    }
    DIR *dir = (DIR *)(*handle);
    struct dirent *result;
    do
    {
        result = readdir(dir);
        if (result == NULL)
        {
            return ERRNO_SYSTEM;
        }
    } while (strcmp(result->d_name, ".") == 0 || strcmp(result->d_name, "..") == 0);
    // Invariant: entry is at least 255 bytes long - but it will be space-padded instead of null-terminated.
    size_t length = strlen(result->d_name);
    if (length > 255)
    {
        length = 255;
    }
    memcpy(entry, result->d_name, length);
    memset(entry + length, ' ', 255 - length);
    return 0;
}

EXTERN_DECL int CloseDirectory(unsigned long long *handle)
{
    if (!handle)
    {
        return ERRNO_PARAMS;
    }
    DIR *dir = (DIR *)(*handle);
    if (closedir(dir) != 0)
    {
        return ERRNO_SYSTEM;
    }
    return 0;
}

EXTERN_DECL int ZlibCompress(char *decompressed, unsigned long *decompressed_length, char *compressed, unsigned long *compressed_length)
{
    if (!decompressed || !decompressed_length || !compressed || !compressed_length)
    {
        return ERRNO_PARAMS;
    }

    if (*decompressed_length == 0)
    {
        *compressed_length = 0;
        return 0;
    }

    z_stream stream;
    stream.zalloc = Z_NULL;
    stream.zfree = Z_NULL;
    stream.opaque = Z_NULL;
    stream.avail_in = *decompressed_length;
    stream.next_in = (Bytef *)decompressed;
    stream.avail_out = *compressed_length;
    stream.next_out = (Bytef *)compressed;

    int result = deflateInit(&stream, Z_DEFAULT_COMPRESSION);
    if (result != Z_OK)
    {
        return ERRNO_SYSTEM;
    }

    result = deflate(&stream, Z_FINISH);
    if (result != Z_STREAM_END)
    {
        deflateEnd(&stream);
        return ERRNO_SYSTEM;
    }

    *compressed_length = stream.total_out;
    deflateEnd(&stream);

    return 0;
}

EXTERN_DECL int ZlibDecompress(char *compressed, unsigned long *compressed_length, char *decompressed, unsigned long *decompressed_length)
{
    if (!compressed || !compressed_length || !decompressed || !decompressed_length)
    {
        return ERRNO_PARAMS;
    }

    if (*compressed_length == 0)
    {
        *decompressed_length = 0;
        return 0;
    }

    z_stream stream;
    stream.zalloc = Z_NULL;
    stream.zfree = Z_NULL;
    stream.opaque = Z_NULL;
    stream.avail_in = *compressed_length;
    stream.next_in = (Bytef *)compressed;
    stream.avail_out = *decompressed_length;
    stream.next_out = (Bytef *)decompressed;

    int result = inflateInit(&stream);
    if (result != Z_OK)
    {
        return ERRNO_SYSTEM;
    }

    result = inflate(&stream, Z_FINISH);
    if (result != Z_STREAM_END)
    {
        inflateEnd(&stream);
        return ERRNO_SYSTEM;
    }

    *decompressed_length = stream.total_out;
    inflateEnd(&stream);

    return 0;
}

EXTERN_DECL int GzipCompress(char *decompressed, unsigned long *decompressed_length, char *compressed, unsigned long *compressed_length)
{
    if (!decompressed || !decompressed_length || !compressed || !compressed_length)
    {
        return ERRNO_PARAMS;
    }

    if (*decompressed_length == 0)
    {
        *compressed_length = 0;
        return 0;
    }

    z_stream stream;
    stream.zalloc = Z_NULL;
    stream.zfree = Z_NULL;
    stream.opaque = Z_NULL;
    stream.avail_in = *decompressed_length;
    stream.next_in = (Bytef *)decompressed;
    stream.avail_out = *compressed_length;
    stream.next_out = (Bytef *)compressed;

    int result = deflateInit2(&stream, Z_DEFAULT_COMPRESSION, Z_DEFLATED, 16 + MAX_WBITS, 8, Z_DEFAULT_STRATEGY);
    if (result != Z_OK)
    {
        return ERRNO_SYSTEM;
    }

    result = deflate(&stream, Z_FINISH);
    if (result != Z_STREAM_END)
    {
        deflateEnd(&stream);
        return ERRNO_SYSTEM;
    }

    *compressed_length = stream.total_out;
    deflateEnd(&stream);

    return 0;
}

EXTERN_DECL int GzipDecompress(char *compressed, unsigned long *compressed_length, char *decompressed, unsigned long *decompressed_length)
{
    if (!compressed || !compressed_length || !decompressed || !decompressed_length)
    {
        return ERRNO_PARAMS;
    }

    if (compressed_length == 0)
    {
        *decompressed_length = 0;
        return 0;
    }

    z_stream stream;
    stream.zalloc = Z_NULL;
    stream.zfree = Z_NULL;
    stream.opaque = Z_NULL;
    stream.avail_in = *compressed_length;
    stream.next_in = (Bytef *)compressed;
    stream.avail_out = *decompressed_length;
    stream.next_out = (Bytef *)decompressed;

    int result = inflateInit2(&stream, 16 + MAX_WBITS);
    if (result != Z_OK)
    {
        return ERRNO_SYSTEM;
    }

    result = inflate(&stream, Z_FINISH);
    if (result != Z_STREAM_END)
    {
        inflateEnd(&stream);
        return ERRNO_SYSTEM;
    }

    *decompressed_length = stream.total_out;
    inflateEnd(&stream);

    return 0;
}

static int SocketSelectRead(const std::set<socket_t>& sockets, socket_t begin_after, socket_t& selected_socket)
{
    std::vector<pollfd> pollfds(sockets.size());

    auto socket_iterator = sockets.find(begin_after);
    if (socket_iterator != sockets.end())
        socket_iterator++;

    for (size_t i = 0, n = sockets.size(); i < n; ++i)
    {
        if (socket_iterator == sockets.end())
            socket_iterator = sockets.begin();
        pollfds.push_back({*socket_iterator, POLLIN, 0});
        socket_iterator++;
    }

    int poll_result = poll(pollfds.data(), pollfds.size(), 0);
    if (poll_result == -1 || poll_result == 0)
        return poll_result;

    for (const auto& item : pollfds)
    {
        if (item.revents & POLLIN)
        {
            selected_socket = item.fd;
            return 1;
        }
    }

    return 0;
}

EXTERN_DECL int SocketListen(unsigned short *port, socket_t *server)
{
    if (!port || !server)
        return ERRNO_PARAMS;

    socket_t sock = socket(AF_INET, SOCK_STREAM, 0);
    if (sock == -1)
        return ERRNO_SYSTEM;

    int opt = 1;
    if (setsockopt(sock, SOL_SOCKET, SO_REUSEADDR, &opt, sizeof(opt)) == -1)
        return ERRNO_SYSTEM;

    sockaddr_in srv;
    srv.sin_family = AF_INET;
    srv.sin_port = htons(*port);
    srv.sin_addr.s_addr = htonl(INADDR_ANY);

    if (bind(sock, (sockaddr *)&srv, sizeof(srv)) == -1)
        return ERRNO_SYSTEM;

    if (listen(sock, 1) == -1)
        return ERRNO_SYSTEM;

    *server = sock;

    return 0;
}

EXTERN_DECL int SocketClose(socket_t *socket)
{
    if (!socket)
        return ERRNO_PARAMS;

    CLIENT_SOCKETS.erase(*socket);

    if (close(*socket) == -1)
        return ERRNO_SYSTEM;

    return 0;
}

EXTERN_DECL int SocketAccept(socket_t *server, socket_t *client)
{
    if (!server || !client)
        return ERRNO_PARAMS;

    sockaddr_in client_addr;
    socklen_t client_addr_length = sizeof(client_addr);

    *client = accept(*server, (sockaddr *)&client_addr, &client_addr_length);
    if (*client == -1)
        return ERRNO_SYSTEM;

    CLIENT_SOCKETS.insert(*client);

    return 0;
}

EXTERN_DECL int SocketPoll(socket_t *server, socket_t *client)
{
    // Remember the last socket that was read, so we can cycle through the sockets in the set.
    static socket_t last_read_socket = -1;

    if (!server || !client)
        return ERRNO_PARAMS;

    // Assert that no client socket was passed
    if (CLIENT_SOCKETS.find(*server) != CLIENT_SOCKETS.end())
        return ERRNO_PARAMS;

    socket_t selected_socket;
    std::set<socket_t> poll_sockets = CLIENT_SOCKETS;
    poll_sockets.insert(*server);

    int poll_result = SocketSelectRead(poll_sockets, last_read_socket, selected_socket);
    if (poll_result == -1)
        return ERRNO_SYSTEM;

    if (poll_result == 0)
    {
        *client = 0;
        return 0;
    }

    // If the server was selected, there is a connection to accept
    if (selected_socket == *server)
        return SocketAccept(server, client);

    *client = selected_socket;
    last_read_socket = selected_socket;

    return 0;
}

EXTERN_DECL int SocketRead(socket_t *socket, unsigned long *count, char *buffer)
{
    if (!socket || !count || !buffer)
        return ERRNO_PARAMS;
    if (*count < 0 || *count > SOCKET_CHUNK_LIMIT)
        return ERRNO_PARAMS;

    if (*count == 0)
        return 0;

    // Check if the socket is ready to read
    pollfd poll_socket = {*socket, POLLIN, 0};
    int poll_result = poll(&poll_socket, 1, 0);
    if (poll_result == -1)
        return ERRNO_SYSTEM;

    if (poll_result == 0)
    {
        *count = 0;
        return 0;
    }

    int result = read(*socket, buffer, *count);
    if (result == -1)
        return ERRNO_SYSTEM;

    *count = result;

    return 0;
}

EXTERN_DECL int SocketWrite(socket_t *socket, unsigned long *count, char *buffer)
{
    if (!socket || !count || !buffer)
        return ERRNO_PARAMS;
    if (*count < 0)
        return ERRNO_PARAMS;

    if (*count == 0)
        return 0;

    unsigned long remaining = *count;
    unsigned long chunk_size;

    while (remaining > 0)
    {
        chunk_size = remaining > SOCKET_CHUNK_LIMIT ? SOCKET_CHUNK_LIMIT : remaining;
        int result = write(*socket, buffer, chunk_size);
        if (result == -1)
            return ERRNO_SYSTEM;
        remaining -= result;
        buffer += result;
    }

    return 0;
}
