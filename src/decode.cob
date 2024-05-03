*> --- Decode-Byte ---
*> Decode a byte from a buffer into an 8-bit integer (BINARY-CHAR).
IDENTIFICATION DIVISION.
PROGRAM-ID. Decode-Byte.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 BYTE-ALPHA.
        02 BYTE-VALUE           BINARY-CHAR UNSIGNED.
LINKAGE SECTION.
    01 LK-BUFFER            PIC X ANY LENGTH.
    01 LK-BUFFERPOS         BINARY-LONG UNSIGNED.
    01 LK-VALUE             BINARY-CHAR.

PROCEDURE DIVISION USING LK-BUFFER LK-BUFFERPOS LK-VALUE.
    MOVE LK-BUFFER(LK-BUFFERPOS:1) TO BYTE-ALPHA
    IF BYTE-VALUE > 127
        COMPUTE LK-VALUE = BYTE-VALUE - 256
    ELSE
        MOVE BYTE-VALUE TO LK-VALUE
    END-IF
    ADD 1 TO LK-BUFFERPOS
    GOBACK.

END PROGRAM Decode-Byte.

*> --- Decode-UnsignedShort ---
*> Decode a big-endian short from a buffer into a 16-bit unsigned integer (BINARY-SHORT UNSIGNED).
IDENTIFICATION DIVISION.
PROGRAM-ID. Decode-UnsignedShort.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 I                    BINARY-CHAR UNSIGNED.
    01 BYTES                PIC X(2).
    01 UNSIGNED-VALUE       REDEFINES BYTES BINARY-SHORT UNSIGNED.
LINKAGE SECTION.
    01 LK-BUFFER            PIC X ANY LENGTH.
    01 LK-BUFFERPOS         BINARY-LONG UNSIGNED.
    01 LK-VALUE             BINARY-SHORT UNSIGNED.

PROCEDURE DIVISION USING LK-BUFFER LK-BUFFERPOS LK-VALUE.
    MOVE LK-BUFFER(LK-BUFFERPOS:1) TO BYTES(2:1)
    MOVE LK-BUFFER(LK-BUFFERPOS + 1:1) TO BYTES(1:1)
    ADD 2 TO LK-BUFFERPOS
    MOVE UNSIGNED-VALUE TO LK-VALUE
    GOBACK.

END PROGRAM Decode-UnsignedShort.

*> --- Decode-Short ---
*> Decode a big-endian short from a buffer into a 16-bit integer (BINARY-SHORT).
IDENTIFICATION DIVISION.
PROGRAM-ID. Decode-Short.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 UINT-VALUE           BINARY-SHORT UNSIGNED.
LINKAGE SECTION.
    01 LK-BUFFER            PIC X ANY LENGTH.
    01 LK-BUFFERPOS         BINARY-LONG UNSIGNED.
    01 LK-VALUE             BINARY-SHORT.

PROCEDURE DIVISION USING LK-BUFFER LK-BUFFERPOS LK-VALUE.
    CALL "Decode-UnsignedShort" USING LK-BUFFER LK-BUFFERPOS UINT-VALUE
    IF UINT-VALUE > 32767
        COMPUTE LK-VALUE = UINT-VALUE - 65536
    ELSE
        MOVE UINT-VALUE TO LK-VALUE
    END-IF
    GOBACK.

END PROGRAM Decode-Short.

*> --- Decode-UnsignedInt ---
*> Decode a big-endian integer from a buffer into a 32-bit unsigned integer (BINARY-LONG UNSIGNED).
IDENTIFICATION DIVISION.
PROGRAM-ID. Decode-UnsignedInt.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 I                    BINARY-CHAR UNSIGNED.
    01 BYTES                PIC X(4).
    01 UNSIGNED-VALUE       REDEFINES BYTES BINARY-LONG UNSIGNED.
LINKAGE SECTION.
    01 LK-BUFFER            PIC X ANY LENGTH.
    01 LK-BUFFERPOS         BINARY-LONG UNSIGNED.
    01 LK-VALUE             BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-BUFFER LK-BUFFERPOS LK-VALUE.
    MOVE LK-BUFFER(LK-BUFFERPOS:1) TO BYTES(4:1)
    MOVE LK-BUFFER(LK-BUFFERPOS + 1:1) TO BYTES(3:1)
    MOVE LK-BUFFER(LK-BUFFERPOS + 2:1) TO BYTES(2:1)
    MOVE LK-BUFFER(LK-BUFFERPOS + 3:1) TO BYTES(1:1)
    ADD 4 TO LK-BUFFERPOS
    MOVE UNSIGNED-VALUE TO LK-VALUE
    GOBACK.

END PROGRAM Decode-UnsignedInt.

*> --- Decode-Int ---
*> Decode a big-endian integer from a buffer into a 32-bit integer (BINARY-LONG).
IDENTIFICATION DIVISION.
PROGRAM-ID. Decode-Int.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 UINT-VALUE           BINARY-LONG UNSIGNED.
LINKAGE SECTION.
    01 LK-BUFFER            PIC X ANY LENGTH.
    01 LK-BUFFERPOS         BINARY-LONG UNSIGNED.
    01 LK-VALUE             BINARY-LONG.

PROCEDURE DIVISION USING LK-BUFFER LK-BUFFERPOS LK-VALUE.
    CALL "Decode-UnsignedInt" USING LK-BUFFER LK-BUFFERPOS UINT-VALUE
    IF UINT-VALUE > 2147483647
        COMPUTE LK-VALUE = UINT-VALUE - 4294967296
    ELSE
        MOVE UINT-VALUE TO LK-VALUE
    END-IF
    GOBACK.

END PROGRAM Decode-Int.

*> --- Decode-VarInt ---
*> Decode a VarInt from a buffer into a 32-bit integer (BINARY-LONG).
IDENTIFICATION DIVISION.
PROGRAM-ID. Decode-VarInt.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 BYTE-ALPHA.
        02 BYTE-VALUE           BINARY-CHAR UNSIGNED.
LOCAL-STORAGE SECTION.
    01 VARINT-MULTIPLIER    BINARY-LONG UNSIGNED    VALUE 1.
    01 UINT-VALUE           BINARY-LONG UNSIGNED    VALUE 0.
    *> The maximum number of bytes a VarInt can have is 5
    01 VARINT-CONTINUE      BINARY-CHAR UNSIGNED    VALUE 5.
LINKAGE SECTION.
    01 LK-BUFFER            PIC X ANY LENGTH.
    01 LK-BUFFERPOS         BINARY-LONG UNSIGNED.
    01 LK-VALUE             BINARY-LONG.

PROCEDURE DIVISION USING LK-BUFFER LK-BUFFERPOS LK-VALUE.
    PERFORM UNTIL VARINT-CONTINUE = 0
        *> Read the next byte
        MOVE LK-BUFFER(LK-BUFFERPOS:1) TO BYTE-ALPHA
        ADD 1 TO LK-BUFFERPOS
        SUBTRACT 1 FROM VARINT-CONTINUE
        *> The lower 7 bits of the byte are the value, and the high bit indicates if there are more bytes
        IF BYTE-VALUE < 128
            COMPUTE UINT-VALUE = UINT-VALUE + BYTE-VALUE * VARINT-MULTIPLIER
            MOVE 0 TO VARINT-CONTINUE
        ELSE
            COMPUTE UINT-VALUE = UINT-VALUE + (BYTE-VALUE - 128) * VARINT-MULTIPLIER
            MULTIPLY VARINT-MULTIPLIER BY 128 GIVING VARINT-MULTIPLIER
        END-IF
    END-PERFORM
    *> Check if the number should be negative (i.e., is larger than 2^31-1) and compute its signed value
    IF UINT-VALUE > 2147483647
        COMPUTE LK-VALUE = UINT-VALUE - 4294967296
    ELSE
        MOVE UINT-VALUE TO LK-VALUE
    END-IF
    GOBACK.

END PROGRAM Decode-VarInt.

*> --- Decode-UnsignedLong ---
*> Decode a big-endian long from a buffer into a 64-bit unsigned integer (BINARY-LONG-LONG UNSIGNED).
IDENTIFICATION DIVISION.
PROGRAM-ID. Decode-UnsignedLong.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 I                    BINARY-CHAR UNSIGNED.
    01 BYTES                PIC X(8).
    01 UNSIGNED-VALUE       REDEFINES BYTES BINARY-LONG-LONG UNSIGNED.
LINKAGE SECTION.
    01 LK-BUFFER            PIC X ANY LENGTH.
    01 LK-BUFFERPOS         BINARY-LONG UNSIGNED.
    01 LK-VALUE             BINARY-LONG-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-BUFFER LK-BUFFERPOS LK-VALUE.
    MOVE LK-BUFFER(LK-BUFFERPOS:1) TO BYTES(8:1)
    MOVE LK-BUFFER(LK-BUFFERPOS + 1:1) TO BYTES(7:1)
    MOVE LK-BUFFER(LK-BUFFERPOS + 2:1) TO BYTES(6:1)
    MOVE LK-BUFFER(LK-BUFFERPOS + 3:1) TO BYTES(5:1)
    MOVE LK-BUFFER(LK-BUFFERPOS + 4:1) TO BYTES(4:1)
    MOVE LK-BUFFER(LK-BUFFERPOS + 5:1) TO BYTES(3:1)
    MOVE LK-BUFFER(LK-BUFFERPOS + 6:1) TO BYTES(2:1)
    MOVE LK-BUFFER(LK-BUFFERPOS + 7:1) TO BYTES(1:1)
    ADD 8 TO LK-BUFFERPOS
    MOVE UNSIGNED-VALUE TO LK-VALUE
    GOBACK.

END PROGRAM Decode-UnsignedLong.

*> --- Decode-Long ---
*> Decode a big-endian long from a buffer into a 64-bit integer (BINARY-LONG-LONG).
IDENTIFICATION DIVISION.
PROGRAM-ID. Decode-Long.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 UINT-VALUE           BINARY-LONG-LONG UNSIGNED.
LINKAGE SECTION.
    01 LK-BUFFER            PIC X ANY LENGTH.
    01 LK-BUFFERPOS         BINARY-LONG UNSIGNED.
    01 LK-VALUE             BINARY-LONG-LONG.

PROCEDURE DIVISION USING LK-BUFFER LK-BUFFERPOS LK-VALUE.
    CALL "Decode-UnsignedLong" USING LK-BUFFER LK-BUFFERPOS UINT-VALUE
    *> Check if the number should be negative (i.e., is larger than 2^63-1) and compute its signed value
    IF UINT-VALUE > 9223372036854775807
        COMPUTE LK-VALUE = UINT-VALUE - 18446744073709551616
    ELSE
        MOVE UINT-VALUE TO LK-VALUE
    END-IF
    GOBACK.

END PROGRAM Decode-Long.

*> --- Decode-Double ---
*> Decode a big-endian double from a buffer into a double-precision floating-point number (FLOAT-LONG).
IDENTIFICATION DIVISION.
PROGRAM-ID. Decode-Double.

DATA DIVISION.
LINKAGE SECTION.
    01 LK-BUFFER            PIC X ANY LENGTH.
    01 LK-BUFFERPOS         BINARY-LONG UNSIGNED.
    01 LK-VALUE             FLOAT-LONG.

PROCEDURE DIVISION USING LK-BUFFER LK-BUFFERPOS LK-VALUE.
    CALL "Util-DoubleFromBytes" USING LK-BUFFER(LK-BUFFERPOS:) LK-VALUE
    ADD 8 TO LK-BUFFERPOS
    GOBACK.

END PROGRAM Decode-Double.

*> --- Decode-Float ---
*> Decode a big-endian float from a buffer into a single-precision floating-point number (FLOAT).
IDENTIFICATION DIVISION.
PROGRAM-ID. Decode-Float.

DATA DIVISION.
LINKAGE SECTION.
    01 LK-BUFFER            PIC X ANY LENGTH.
    01 LK-BUFFERPOS         BINARY-LONG UNSIGNED.
    01 LK-VALUE             FLOAT.

PROCEDURE DIVISION USING LK-BUFFER LK-BUFFERPOS LK-VALUE.
    CALL "Util-FloatFromBytes" USING LK-BUFFER(LK-BUFFERPOS:) LK-VALUE
    ADD 4 TO LK-BUFFERPOS
    GOBACK.

END PROGRAM Decode-Float.

*> --- Decode-String ---
*> Decode a string from a buffer. The string is prefixed with a VarInt length.
IDENTIFICATION DIVISION.
PROGRAM-ID. Decode-String.

DATA DIVISION.
LINKAGE SECTION.
    01 LK-BUFFER            PIC X ANY LENGTH.
    01 LK-BUFFERPOS         BINARY-LONG UNSIGNED.
    01 LK-STR-LENGTH        BINARY-LONG.
    01 LK-VALUE             PIC X ANY LENGTH.

PROCEDURE DIVISION USING LK-BUFFER LK-BUFFERPOS LK-STR-LENGTH LK-VALUE.
    *> Read the length
    CALL "Decode-VarInt" USING LK-BUFFER LK-BUFFERPOS LK-STR-LENGTH
    IF LK-STR-LENGTH < 0 OR LK-STR-LENGTH > 64000
        *> TODO: Handle error
        EXIT PROGRAM
    END-IF
    *> Read the string
    MOVE LK-BUFFER(LK-BUFFERPOS:LK-STR-LENGTH) TO LK-VALUE(1:LK-STR-LENGTH)
    ADD LK-STR-LENGTH TO LK-BUFFERPOS
    GOBACK.

END PROGRAM Decode-String.

*> --- Decode-Position ---
*> Decode a block position from a buffer. The position is encoded as a 64-bit integer (BINARY-LONG-LONG).
*> The 26 least-significant bits are X, the middle 12 bits are Y, and the 26 most-significant bits are Z.
*> Each of the bit sections is signed (two's complement).
IDENTIFICATION DIVISION.
PROGRAM-ID. Decode-Position.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 UINT-VALUE           BINARY-LONG-LONG UNSIGNED.
LINKAGE SECTION.
    01 LK-BUFFER            PIC X ANY LENGTH.
    01 LK-BUFFERPOS         BINARY-LONG UNSIGNED.
    01 LK-VALUE.
        02 LK-X             BINARY-LONG.
        02 LK-Y             BINARY-LONG.
        02 LK-Z             BINARY-LONG.

PROCEDURE DIVISION USING LK-BUFFER LK-BUFFERPOS LK-VALUE.
    CALL "Decode-UnsignedLong" USING LK-BUFFER LK-BUFFERPOS UINT-VALUE

    *> Take the last 12 bits as Y, and shift right by 12 bits
    DIVIDE UINT-VALUE BY 4096 GIVING UINT-VALUE REMAINDER LK-Y
    IF LK-Y > 2047
        SUBTRACT 4096 FROM LK-Y
    END-IF

    *> Take the next 26 bits as Z, and shift right by 26 bits
    DIVIDE UINT-VALUE BY 67108864 GIVING UINT-VALUE REMAINDER LK-Z
    IF LK-Z > 33554431
        SUBTRACT 67108864 FROM LK-Z
    END-IF

    *> Take the remaining 26 bits as X
    IF UINT-VALUE > 33554431
        SUBTRACT 67108864 FROM UINT-VALUE GIVING LK-X
    ELSE
        MOVE UINT-VALUE TO LK-X
    END-IF

    GOBACK.

END PROGRAM Decode-Position.
