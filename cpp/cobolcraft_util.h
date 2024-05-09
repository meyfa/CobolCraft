#define EXTERN_DECL extern "C"

/**
 * Get the current system time in milliseconds.
 */
EXTERN_DECL int SystemTimeMillis(long long *timestamp);

/**
 * Setup stdin for non-blocking read.
 */
EXTERN_DECL int SetConsoleNonBlocking();

/**
 * Ignore SIGPIPE.
 */
EXTERN_DECL int IgnoreSIGPIPE();

/**
 * Read from stdin. The second parameter is the number of bytes to read; after reading, it contains the
 * actual number of bytes read.
 */
EXTERN_DECL int ReadConsole(char *buffer, unsigned long *count);

/**
 * Count the number of leading zero bits in a 32-bit integer.
 */
EXTERN_DECL int LeadingZeros32(unsigned long *value, unsigned long *count);
