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
    IF ERRNO NOT = 0 THEN
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
    IF ERRNO NOT = 0 THEN
        DISPLAY "Failed to ignore SIGPIPE signal."
    END-IF
    GOBACK.

END PROGRAM Util-IgnoreSIGPIPE.
