*> --- Util-SystemTimeMillis ---
*> Calls the function "00" of CobolCraft's utility module written in C++.
*> This retrieves the current system time in milliseconds.
IDENTIFICATION DIVISION.
PROGRAM-ID. Util-SystemTimeMillis.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 BUFFER           PIC X(15).
    01 ERRNO            PIC 9(3).
LINKAGE SECTION.
    01 LK-INT64         BINARY-LONG-LONG.

PROCEDURE DIVISION USING LK-INT64.
    CALL "COBOLCRAFT_UTIL" USING "00" BUFFER GIVING ERRNO
    IF ERRNO NOT = 0
        MOVE 0 TO LK-INT64
        GOBACK
    END-IF
    MOVE BUFFER TO LK-INT64
    GOBACK.

END PROGRAM Util-SystemTimeMillis.

*> --- Util-IgnoreSIGPIPE ---
*> Calls the function "01" of CobolCraft's utility module written in C++.
*> This sets the SIGPIPE signal to be ignored to avoid crashing the program when writing to a closed socket.
IDENTIFICATION DIVISION.
PROGRAM-ID. Util-IgnoreSIGPIPE.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 ERRNO            PIC 9(3).

PROCEDURE DIVISION.
    CALL "COBOLCRAFT_UTIL" USING "01" GIVING ERRNO
    IF ERRNO NOT = 0
        DISPLAY "Failed to ignore SIGPIPE signal."
    END-IF
    GOBACK.

END PROGRAM Util-IgnoreSIGPIPE.

*> --- Util-DoubleGetBytes ---
*> Convert a FLOAT-LONG (IEEE 754 double-precision floating-point number) to a big-endian byte array.
IDENTIFICATION DIVISION.
PROGRAM-ID. Util-DoubleGetBytes.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 ERRNO            PIC 9(3).
LINKAGE SECTION.
    01 LK-VALUE         FLOAT-LONG.
    01 LK-BUFFER        PIC X ANY LENGTH.

PROCEDURE DIVISION USING LK-VALUE LK-BUFFER.
    CALL "COBOLCRAFT_UTIL" USING "02" LK-VALUE LK-BUFFER GIVING ERRNO
    IF ERRNO NOT = 0
        MOVE X"0000000000000000" TO LK-BUFFER
    END-IF
    GOBACK.

END PROGRAM Util-DoubleGetBytes.

*> --- Util-DoubleFromBytes ---
*> Convert a big-endian byte array to a FLOAT-LONG (IEEE 754 double-precision floating-point number).
IDENTIFICATION DIVISION.
PROGRAM-ID. Util-DoubleFromBytes.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 ERRNO            PIC 9(3).
LINKAGE SECTION.
    01 LK-BUFFER        PIC X ANY LENGTH.
    01 LK-VALUE         FLOAT-LONG.

PROCEDURE DIVISION USING LK-BUFFER LK-VALUE.
    CALL "COBOLCRAFT_UTIL" USING "03" LK-BUFFER LK-VALUE GIVING ERRNO
    IF ERRNO NOT = 0
        MOVE 0 TO LK-VALUE
    END-IF
    GOBACK.

END PROGRAM Util-DoubleFromBytes.

*> --- Util-FloatGetBytes ---
*> Convert a FLOAT-SHORT (IEEE 754 single-precision floating-point number) to a big-endian byte array.
IDENTIFICATION DIVISION.
PROGRAM-ID. Util-FloatGetBytes.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 ERRNO            PIC 9(3).
LINKAGE SECTION.
    01 LK-VALUE         FLOAT-SHORT.
    01 LK-BUFFER        PIC X ANY LENGTH.

PROCEDURE DIVISION USING LK-VALUE LK-BUFFER.
    CALL "COBOLCRAFT_UTIL" USING "04" LK-VALUE LK-BUFFER GIVING ERRNO
    IF ERRNO NOT = 0
        MOVE X"00000000" TO LK-BUFFER
    END-IF
    GOBACK.

END PROGRAM Util-FloatGetBytes.

*> --- Util-FloatFromBytes ---
*> Convert a big-endian byte array to a FLOAT-SHORT (IEEE 754 single-precision floating-point number).
IDENTIFICATION DIVISION.
PROGRAM-ID. Util-FloatFromBytes.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 ERRNO            PIC 9(3).
LINKAGE SECTION.
    01 LK-BUFFER        PIC X ANY LENGTH.
    01 LK-VALUE         FLOAT-SHORT.

PROCEDURE DIVISION USING LK-BUFFER LK-VALUE.
    CALL "COBOLCRAFT_UTIL" USING "05" LK-BUFFER LK-VALUE GIVING ERRNO
    IF ERRNO NOT = 0
        MOVE 0 TO LK-VALUE
    END-IF
    GOBACK.

END PROGRAM Util-FloatFromBytes.

*> --- Util-SetConsoleNonBlocking ---
*> Set the console input to non-blocking mode.
IDENTIFICATION DIVISION.
PROGRAM-ID. Util-SetConsoleNonBlocking.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 ERRNO            PIC 9(3).

PROCEDURE DIVISION.
    CALL "COBOLCRAFT_UTIL" USING "06" GIVING ERRNO
    IF ERRNO NOT = 0
        DISPLAY "Failed to set console to non-blocking mode."
    END-IF
    GOBACK.

END PROGRAM Util-SetConsoleNonBlocking.

*> --- Util-ReadConsole ---
*> Read console input in a non-blocking manner. Returns the number of bytes read. Enter is indicated by a newline char.
IDENTIFICATION DIVISION.
PROGRAM-ID. Util-ReadConsole.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 ERRNO            PIC 9(3).
LINKAGE SECTION.
    01 LK-BUFFER        PIC X ANY LENGTH.
    01 LK-SIZE          BINARY-LONG.

PROCEDURE DIVISION USING LK-BUFFER LK-SIZE.
    CALL "COBOLCRAFT_UTIL" USING "07" LK-BUFFER LK-SIZE GIVING ERRNO
    IF ERRNO NOT = 0
        MOVE 0 TO LK-SIZE
    END-IF
    GOBACK.

END PROGRAM Util-ReadConsole.

*> --- Util-LeadingZeros32 ---
*> Count the number of leading zeros in a 32-bit (unsigned) integer.
IDENTIFICATION DIVISION.
PROGRAM-ID. Util-LeadingZeros32.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 ERRNO            PIC 9(3).
LINKAGE SECTION.
    01 LK-VALUE         BINARY-LONG UNSIGNED.
    01 LK-COUNT         BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-VALUE LK-COUNT.
    CALL "COBOLCRAFT_UTIL" USING "08" LK-VALUE LK-COUNT GIVING ERRNO
    IF ERRNO NOT = 0
        DISPLAY "ERROR: COBOLCRAFT_UTIL(08) failed:" ERRNO
        STOP RUN
    END-IF
    GOBACK.

END PROGRAM Util-LeadingZeros32.
