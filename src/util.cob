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
    01 LK-INT64         PIC 9(20).

PROCEDURE DIVISION USING LK-INT64.
    CALL "COBOLCRAFT_UTIL" USING "00" BUFFER GIVING ERRNO
    IF ERRNO NOT = 0 THEN
        MOVE 0 TO LK-INT64
        GOBACK
    END-IF
    MOVE BUFFER TO LK-INT64
    GOBACK.

END PROGRAM Util-SystemTimeMillis.
