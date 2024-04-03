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
    CALL "CBL_GC_SOCKET" USING "04" LK-HNDL LK-READ-COUNT LK-VALUE GIVING LK-ERRNO.

END PROGRAM Read-Raw.

*> --- Read-VarInt ---
*> Read a VarInt from the socket into an S9(10) field.
IDENTIFICATION DIVISION.
PROGRAM-ID. Read-VarInt.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 BUFFER               PIC X.
    01 BYTE-COUNT            PIC 9(5).
LOCAL-STORAGE SECTION.
    01 VARINT-BYTE          PIC 9(3) COMP   VALUE 0.
    01 VARINT-BYTE-VALUE    PIC 9(3) COMP   VALUE 0.
    01 VARINT-MULTIPLIER    PIC 9(10) COMP  VALUE 1.
    01 VARINT-CONTINUE      PIC 9 COMP      VALUE 1.
LINKAGE SECTION.
    01 LK-HNDL              PIC X(4).
    01 LK-ERRNO             PIC 9(3).
    01 LK-READ-COUNT        PIC 9(5).
    01 LK-VALUE             PIC S9(10).

PROCEDURE DIVISION USING BY REFERENCE LK-HNDL LK-ERRNO LK-READ-COUNT LK-VALUE.
    MOVE 0 TO LK-VALUE.
    MOVE 0 TO LK-READ-COUNT.
    PERFORM UNTIL VARINT-CONTINUE = 0
        *> Receive the next byte
        MOVE 1 TO BYTE-COUNT
        CALL "CBL_GC_SOCKET" USING "04" LK-HNDL BYTE-COUNT BUFFER GIVING LK-ERRNO
        IF LK-ERRNO NOT = 0
            EXIT PROGRAM
        END-IF
        ADD 1 TO LK-READ-COUNT
        MOVE FUNCTION ORD(BUFFER(1:1)) TO VARINT-BYTE
        SUBTRACT 1 FROM VARINT-BYTE
        *> Extract the lower 7 bits
        MOVE FUNCTION MOD(VARINT-BYTE, 128) TO VARINT-BYTE-VALUE
        *> This yields the value when multiplied by the position multiplier
        MULTIPLY VARINT-BYTE-VALUE BY VARINT-MULTIPLIER GIVING VARINT-BYTE-VALUE
        ADD VARINT-BYTE-VALUE TO LK-VALUE
        MULTIPLY VARINT-MULTIPLIER BY 128 GIVING VARINT-MULTIPLIER
        *> Check if we need to continue (if the high bit is set and the maximum number of bytes has not been reached)
        IF VARINT-BYTE < 128 OR LK-READ-COUNT >= 5
            MOVE 0 TO VARINT-CONTINUE
        END-IF
    END-PERFORM
    *> Check if the number is negative (i.e., larger than 2^31-1) and compute the two's complement (2^32 - value)
    IF LK-VALUE > 2147483647
        COMPUTE LK-VALUE = 4294967296 - LK-VALUE
    END-IF.

END PROGRAM Read-VarInt.

*> --- Read-String ---
*> Read a string from the socket. The string is prefixed with a VarInt length.
IDENTIFICATION DIVISION.
PROGRAM-ID. Read-String.

DATA DIVISION.
LOCAL-STORAGE SECTION.
    01 READ-COUNT           PIC 9(5)    VALUE 0.
    01 TEMP-LENGTH          PIC S9(10)  VALUE 0.
LINKAGE SECTION.
    01 LK-HNDL              PIC X(4).
    01 LK-ERRNO             PIC 9(3).
    01 LK-STR-LENGTH        PIC 9(5).
    01 LK-VALUE             PIC X(64000).

PROCEDURE DIVISION USING BY REFERENCE LK-HNDL LK-ERRNO LK-STR-LENGTH LK-VALUE.
    *> Read the length
    CALL "Read-VarInt" USING LK-HNDL LK-ERRNO READ-COUNT TEMP-LENGTH.
    IF LK-ERRNO NOT = 0
        EXIT PROGRAM
    END-IF
    IF TEMP-LENGTH < 0 OR TEMP-LENGTH > 64000
        *> TODO: Handle error
        EXIT PROGRAM
    END-IF
    MOVE TEMP-LENGTH TO LK-STR-LENGTH
    *> Read the string
    CALL "CBL_GC_SOCKET" USING "04" LK-HNDL LK-STR-LENGTH LK-VALUE GIVING LK-ERRNO.
    IF LK-ERRNO NOT = 0
        EXIT PROGRAM
    END-IF.

END PROGRAM Read-String.
