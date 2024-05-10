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

/**
 * Compress a buffer using zlib (deflate). The compressed length indicates the size of the compressed buffer; after compression,
 * it contains the actual size of the compressed data.
 */
EXTERN_DECL int ZlibCompress(char *decompressed, unsigned long *decompressed_length, char *compressed, unsigned long *compressed_length);

/**
 * Decompress a zlib-compressed buffer. The decompressed length indicates the size of the decompressed buffer; after
 * decompression, it contains the actual size of the decompressed data.
 */
EXTERN_DECL int ZlibDecompress(char *compressed, unsigned long *compressed_length, char *decompressed, unsigned long *decompressed_length);

/**
 * Compress a buffer using gzip. The compressed length indicates the size of the compressed buffer; after compression,
 * it contains the actual size of the compressed data.
 */
EXTERN_DECL int GzipCompress(char *decompressed, unsigned long *decompressed_length, char *compressed, unsigned long *compressed_length);

/**
 * Decompress a gzip-compressed buffer. The decompressed length indicates the size of the decompressed buffer; after
 * decompression, it contains the actual size of the decompressed data.
 */
EXTERN_DECL int GzipDecompress(char *compressed, unsigned long *compressed_length, char *decompressed, unsigned long *decompressed_length);
