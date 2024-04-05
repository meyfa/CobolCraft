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

extern "C" int COBOLCRAFT_UTIL(char *p_code, char *p1)
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
    }

    return ERRNO_PARAMS;
}
