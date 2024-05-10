#include "cobolcraft_util.h"
#include <chrono>
#include <csignal>
#include <fcntl.h>
#include <errno.h>
#include <zlib.h>

#define ERRNO_PARAMS 99
#define ERRNO_SYSTEM 98

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
