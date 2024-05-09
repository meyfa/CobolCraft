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
