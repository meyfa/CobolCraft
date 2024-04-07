#include "cobolcraft_util.h"
#include <chrono>
#include <cstring>
#include <cstdio>
#include <csignal>

#define ERRNO_PARAMS 99

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
    snprintf(buffer, 16, "%.15lld", millis);
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
    }

    return ERRNO_PARAMS;
}
