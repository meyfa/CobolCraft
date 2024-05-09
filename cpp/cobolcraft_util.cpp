#include "cobolcraft_util.h"
#include <chrono>
#include <csignal>
#include <fcntl.h>
#include <errno.h>

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
