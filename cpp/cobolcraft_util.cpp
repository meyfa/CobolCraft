#include "cobolcraft_util.h"
#include <chrono>
#include <cstring>
#include <cstdio>
#include <csignal>
#include <fcntl.h>
#include <errno.h>

#define ERRNO_PARAMS 99
#define ERRNO_SYSTEM 98

static unsigned int convert_cob_uint(char *p, unsigned int len)
{
    unsigned int result = 0;
    for (unsigned int i = 0; i < len; ++i)
    {
        // just ignore non-numeric characters for simplicity
        if (p[i] < '0' || p[i] > '9')
        {
            continue;
        }
        result = result * 10 + (p[i] - '0');
    }
    return result;
}

static int system_time_millis(char *p1)
{
    if (!p1)
    {
        return ERRNO_PARAMS;
    }

    // get current time in milliseconds
    auto time = std::chrono::system_clock::now().time_since_epoch();
    long long millis = std::chrono::duration_cast<std::chrono::milliseconds>(time).count();

    // convert to string
    char buffer[16];
    snprintf(buffer, 16, "%.15lld", (unsigned long long)millis);
    memcpy(p1, buffer, 15);

    return 0;
}

static int double_get_bytes(char *in, char *out)
{
    if (!in || !out)
    {
        return ERRNO_PARAMS;
    }

    double *out_double = (double *)out;
    *out_double = *((double *)in);

    // convert to big-endian
    for (unsigned int i = 0; i < 4; ++i)
    {
        char temp = out[i];
        out[i] = out[7 - i];
        out[7 - i] = temp;
    }

    return 0;
}

static int double_from_bytes(char *in, char *out)
{
    if (!in || !out)
    {
        return ERRNO_PARAMS;
    }

    // convert to little-endian
    static char in_copy[8];
    for (unsigned int i = 0; i < 8; ++i)
    {
        in_copy[i] = in[7 - i];
    }

    double *out_double = (double *)out;
    *out_double = *((double *)in_copy);

    return 0;
}

static int float_get_bytes(char *in, char *out)
{
    if (!in || !out)
    {
        return ERRNO_PARAMS;
    }

    float *out_float = (float *)out;
    *out_float = *((float *)in);

    // convert to big-endian
    for (unsigned int i = 0; i < 2; ++i)
    {
        char temp = out[i];
        out[i] = out[3 - i];
        out[3 - i] = temp;
    }

    return 0;
}

static int float_from_bytes(char *in, char *out)
{
    if (!in || !out)
    {
        return ERRNO_PARAMS;
    }

    // convert to little-endian
    static char in_copy[4];
    for (unsigned int i = 0; i < 4; ++i)
    {
        in_copy[i] = in[3 - i];
    }

    float *out_float = (float *)out;
    *out_float = *((float *)in_copy);

    return 0;
}

static int console_set_nonblock()
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

static int console_read(char *p1, char *p2)
{
    if (!p1 || !p2)
    {
        return ERRNO_PARAMS;
    }

    // read from stdin
    int max_length = *((int *)p2);
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
        p1[actual_length] = c;
        ++actual_length;
    }

    // update actual length
    *((int *)p2) = actual_length;

    return 0;
}

static int leading_zero_bits_uint32(char *p1, char *p2)
{
    if (!p1 || !p2)
    {
        return ERRNO_PARAMS;
    }

    unsigned int value = *((unsigned int *)p1);
    unsigned int *out = (unsigned int *)p2;

    // input of 0 has undefined behavior in __builtin_clz
    if (value == 0)
    {
        *out = 32;
        return 0;
    }

    *out = __builtin_clz(value);

    return 0;
}

extern "C" int COBOLCRAFT_UTIL(char *p_code, char *p1, char *p2)
{
    if (!p_code)
    {
        return ERRNO_PARAMS;
    }

    unsigned int code = convert_cob_uint(p_code, 2);
    switch (code)
    {
    // System time in milliseconds.
    // p1: X(15) - output
    case 0:
        return system_time_millis(p1);

    // Ignore SIGPIPE.
    case 1:
        signal(SIGPIPE, SIG_IGN);
        return 0;

    // Convert IEEE-754 double-precision to byte array (big-endian).
    case 2:
        return double_get_bytes(p1, p2);

    // Convert byte array to IEEE-754 double-precision (big-endian).
    case 3:
        return double_from_bytes(p1, p2);

    // Convert IEEE-754 single-precision to byte array (big-endian).
    case 4:
        return float_get_bytes(p1, p2);

    // Convert byte array to IEEE-754 single-precision (big-endian).
    case 5:
        return float_from_bytes(p1, p2);

    // Setup stdin for non-blocking read.
    case 6:
        return console_set_nonblock();

    // Read from stdin.
    // p1: PIC X ANY LENGTH - output
    // p2: BINARY-LONG - input: max length / output: actual length
    case 7:
        return console_read(p1, p2);

    // Count number of leading zero bits in a 32-bit integer.
    // p1: BINARY-LONG UNSIGNED - input
    // p2: BINARY-LONG [UNSIGNED] - output
    case 8:
        return leading_zero_bits_uint32(p1, p2);
    }

    return ERRNO_PARAMS;
}
