*> --- Read-Raw ---
*> Read a raw byte array from the socket.
IDENTIFICATION DIVISION.
PROGRAM-ID. Read-Raw.

DATA DIVISION.
LINKAGE SECTION.
    01 LK-HNDL              PIC X(4).
    01 LK-READ-COUNT        PIC 9(5).
    01 LK-ERRNO             PIC 9(3).
    01 LK-VALUE             PIC X(64000).

PROCEDURE DIVISION USING BY REFERENCE LK-HNDL LK-READ-COUNT LK-ERRNO LK-VALUE.
    IF LK-READ-COUNT < 1
        MOVE 0 TO LK-ERRNO
        EXIT PROGRAM
    END-IF
    CALL "CBL_GC_SOCKET" USING "04" LK-HNDL LK-READ-COUNT LK-VALUE GIVING LK-ERRNO
    GOBACK.

END PROGRAM Read-Raw.

*> --- Decode-VarInt ---
*> Decode a VarInt from a buffer into an S9(10) field.
IDENTIFICATION DIVISION.
PROGRAM-ID. Decode-VarInt.

DATA DIVISION.
LOCAL-STORAGE SECTION.
    01 VARINT-READ-COUNT    PIC 9(1) COMP   VALUE 0.
    01 VARINT-BYTE          PIC 9(3) COMP   VALUE 0.
    01 VARINT-BYTE-VALUE    PIC 9(3) COMP   VALUE 0.
    01 VARINT-MULTIPLIER    PIC 9(10) COMP  VALUE 1.
    01 VARINT-CONTINUE      PIC 9 COMP      VALUE 1.
LINKAGE SECTION.
    01 LK-BUFFER            PIC X(2100000).
    01 LK-BUFFERPOS         PIC 9(10).
    01 LK-VALUE             PIC S9(10).

PROCEDURE DIVISION USING BY REFERENCE LK-BUFFER LK-BUFFERPOS LK-VALUE.
    MOVE 0 TO LK-VALUE
    PERFORM UNTIL VARINT-CONTINUE = 0
        *> Read the next byte
        COMPUTE VARINT-BYTE = FUNCTION ORD(LK-BUFFER(LK-BUFFERPOS:1)) - 1
        ADD 1 TO LK-BUFFERPOS
        ADD 1 TO VARINT-READ-COUNT
        *> Extract the lower 7 bits
        MOVE FUNCTION MOD(VARINT-BYTE, 128) TO VARINT-BYTE-VALUE
        *> This yields the value when multiplied by the position multiplier
        MULTIPLY VARINT-BYTE-VALUE BY VARINT-MULTIPLIER GIVING VARINT-BYTE-VALUE
        ADD VARINT-BYTE-VALUE TO LK-VALUE
        MULTIPLY VARINT-MULTIPLIER BY 128 GIVING VARINT-MULTIPLIER
        *> Check if we need to continue (if the high bit is set and the maximum number of bytes has not been reached)
        IF VARINT-BYTE < 128 OR VARINT-READ-COUNT >= 5
            MOVE 0 TO VARINT-CONTINUE
        END-IF
    END-PERFORM
    *> Check if the number is negative (i.e., larger than 2^31-1) and compute the two's complement (2^32 - value)
    IF LK-VALUE > 2147483647
        COMPUTE LK-VALUE = 4294967296 - LK-VALUE
    END-IF
    GOBACK.

END PROGRAM Decode-VarInt.

*> --- Decode-String ---
*> Decode a string from a buffer. The string is prefixed with a VarInt length.
IDENTIFICATION DIVISION.
PROGRAM-ID. Decode-String.

DATA DIVISION.
LOCAL-STORAGE SECTION.
    01 LENGTH-INT32         PIC S9(10)  VALUE 0.
LINKAGE SECTION.
    01 LK-BUFFER            PIC X(2100000).
    01 LK-BUFFERPOS         PIC 9(10).
    01 LK-STR-LENGTH        PIC 9(5).
    01 LK-VALUE             PIC X(64000).

PROCEDURE DIVISION USING BY REFERENCE LK-BUFFER LK-BUFFERPOS LK-STR-LENGTH LK-VALUE.
    *> Read the length
    CALL "Decode-VarInt" USING LK-BUFFER LK-BUFFERPOS LENGTH-INT32
    IF LENGTH-INT32 < 0 OR LENGTH-INT32 > 64000
        *> TODO: Handle error
        EXIT PROGRAM
    END-IF
    MOVE LENGTH-INT32 TO LK-STR-LENGTH
    DISPLAY "LENGTH-INT32: " LENGTH-INT32 ", LK-STR-LENGTH: " LK-STR-LENGTH
    *> Read the string
    MOVE LK-BUFFER(LK-BUFFERPOS:LK-STR-LENGTH) TO LK-VALUE(1:LK-STR-LENGTH)
    ADD LK-STR-LENGTH TO LK-BUFFERPOS
    GOBACK.

END PROGRAM Decode-String.
