*> --- Encode-VarInt ---
*> Encode a signed 32-bit integer to a VarInt and store it in a buffer.
IDENTIFICATION DIVISION.
PROGRAM-ID. Encode-VarInt.

DATA DIVISION.
LOCAL-STORAGE SECTION.
    01 CURRENT-VALUE        PIC 9(10).
    01 CURRENT-VALUE-BITS   PIC 9(3).
LINKAGE SECTION.
    01 LK-VALUE-IN          PIC S9(10).
    01 LK-VALUE-OUT         PIC X(5).
    01 LK-OUT-LENGTH        PIC 9(5).

PROCEDURE DIVISION USING BY REFERENCE LK-VALUE-IN LK-VALUE-OUT LK-OUT-LENGTH.
    MOVE LK-VALUE-IN TO CURRENT-VALUE
    *> If the number is negative, we need to take the two's complement.
    *> This is done by computing 2^32 - |x|, where x is the input number.
    IF LK-VALUE-IN < 0
        COMPUTE CURRENT-VALUE = 4294967296 - CURRENT-VALUE
    END-IF
    *> Perform the conversion to VarInt
    MOVE 0 TO LK-OUT-LENGTH
    PERFORM UNTIL EXIT
        ADD 1 TO LK-OUT-LENGTH
        MOVE FUNCTION MOD(CURRENT-VALUE, 128) TO CURRENT-VALUE-BITS
        DIVIDE CURRENT-VALUE BY 128 GIVING CURRENT-VALUE
        IF CURRENT-VALUE = 0
            MOVE FUNCTION CHAR(CURRENT-VALUE-BITS + 1) TO LK-VALUE-OUT(LK-OUT-LENGTH:1)
            EXIT PERFORM
        END-IF
        MOVE FUNCTION CHAR(CURRENT-VALUE-BITS + 128 + 1) TO LK-VALUE-OUT(LK-OUT-LENGTH:1)
    END-PERFORM.

END PROGRAM Encode-VarInt.

*> --- Encode-Long ---
*> Encode a 64-bit integer and store it in a buffer (big-endian).
IDENTIFICATION DIVISION.
PROGRAM-ID. Encode-Long.

DATA DIVISION.
LOCAL-STORAGE SECTION.
    01 CURRENT-VALUE        PIC 9(20).
    01 CURRENT-VALUE-BYTE   PIC 9(3).
    01 I                    PIC 9(3).
LINKAGE SECTION.
    01 LK-VALUE-IN          PIC S9(20).
    01 LK-VALUE-OUT         PIC X(8).
    01 LK-OUT-LENGTH        PIC 9(5).

PROCEDURE DIVISION USING BY REFERENCE LK-VALUE-IN LK-VALUE-OUT LK-OUT-LENGTH.
    MOVE LK-VALUE-IN TO CURRENT-VALUE
    *> If the number is negative, we need to take the two's complement.
    *> This is done by computing 2^64 - |x|, where x is the input number.
    IF LK-VALUE-IN < 0
        COMPUTE CURRENT-VALUE = 18446744073709551616 - CURRENT-VALUE
    END-IF
    *> Perform the conversion to bytes
    MOVE 8 TO LK-OUT-LENGTH
    PERFORM VARYING I FROM 1 BY 1 UNTIL I > LK-OUT-LENGTH
        DIVIDE CURRENT-VALUE BY 256 GIVING CURRENT-VALUE REMAINDER CURRENT-VALUE-BYTE
        MOVE FUNCTION CHAR(CURRENT-VALUE-BYTE + 1) TO LK-VALUE-OUT(LK-OUT-LENGTH + 1 - I:1)
    END-PERFORM.

END PROGRAM Encode-Long.
