*> --- Encode-UnsignedShort ---
*> Encode a 16-bit unsigned integer and store it in a buffer (big-endian).
IDENTIFICATION DIVISION.
PROGRAM-ID. Encode-UnsignedShort.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 VALUE-NUMERIC        BINARY-SHORT UNSIGNED.
    01 VALUE-BYTES REDEFINES VALUE-NUMERIC PIC X(2).
LINKAGE SECTION.
    01 LK-VALUE-IN          BINARY-SHORT UNSIGNED.
    01 LK-VALUE-OUT         PIC X ANY LENGTH.
    01 LK-OUT-LENGTH        BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-VALUE-IN LK-VALUE-OUT LK-OUT-LENGTH.
    MOVE LK-VALUE-IN TO VALUE-NUMERIC
    MOVE FUNCTION REVERSE(VALUE-BYTES) TO LK-VALUE-OUT(1:2)
    MOVE 2 TO LK-OUT-LENGTH
    GOBACK.

END PROGRAM Encode-UnsignedShort.

*> --- Encode-Short ---
*> Encode a 16-bit integer and store it in a buffer (big-endian).
IDENTIFICATION DIVISION.
PROGRAM-ID. Encode-Short.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 UNSIGNED-VALUE       BINARY-SHORT UNSIGNED.
LINKAGE SECTION.
    01 LK-VALUE-IN          BINARY-SHORT.
    01 LK-VALUE-OUT         PIC X ANY LENGTH.
    01 LK-OUT-LENGTH        BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-VALUE-IN LK-VALUE-OUT LK-OUT-LENGTH.
    *> If the number is negative, we need to take the two's complement.
    *> This is done by computing 2^16 - |x|, where x is the input number.
    IF LK-VALUE-IN < 0
        COMPUTE UNSIGNED-VALUE = 65536 + LK-VALUE-IN
    ELSE
        MOVE LK-VALUE-IN TO UNSIGNED-VALUE
    END-IF
    *> Perform the conversion to bytes
    CALL "Encode-UnsignedShort" USING UNSIGNED-VALUE LK-VALUE-OUT LK-OUT-LENGTH
    GOBACK.

END PROGRAM Encode-Short.

*> --- Encode-UnsignedInt ---
*> Encode a 32-bit unsigned integer and store it in a buffer (big-endian).
IDENTIFICATION DIVISION.
PROGRAM-ID. Encode-UnsignedInt.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 VALUE-NUMERIC        BINARY-LONG UNSIGNED.
    01 VALUE-BYTES REDEFINES VALUE-NUMERIC PIC X(4).
LINKAGE SECTION.
    01 LK-VALUE-IN          BINARY-LONG UNSIGNED.
    01 LK-VALUE-OUT         PIC X ANY LENGTH.
    01 LK-OUT-LENGTH        BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-VALUE-IN LK-VALUE-OUT LK-OUT-LENGTH.
    MOVE LK-VALUE-IN TO VALUE-NUMERIC
    MOVE FUNCTION REVERSE(VALUE-BYTES) TO LK-VALUE-OUT(1:4)
    MOVE 4 TO LK-OUT-LENGTH
    GOBACK.

END PROGRAM Encode-UnsignedInt.

*> --- Encode-Int ---
*> Encode a 32-bit integer and store it in a buffer (big-endian).
IDENTIFICATION DIVISION.
PROGRAM-ID. Encode-Int.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 UNSIGNED-VALUE       BINARY-LONG UNSIGNED.
LINKAGE SECTION.
    01 LK-VALUE-IN          BINARY-LONG.
    01 LK-VALUE-OUT         PIC X ANY LENGTH.
    01 LK-OUT-LENGTH        BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-VALUE-IN LK-VALUE-OUT LK-OUT-LENGTH.
    *> If the number is negative, we need to take the two's complement.
    *> This is done by computing 2^32 - |x|, where x is the input number.
    IF LK-VALUE-IN < 0
        COMPUTE UNSIGNED-VALUE = 4294967296 + LK-VALUE-IN
    ELSE
        MOVE LK-VALUE-IN TO UNSIGNED-VALUE
    END-IF
    *> Perform the conversion to bytes
    CALL "Encode-UnsignedInt" USING UNSIGNED-VALUE LK-VALUE-OUT LK-OUT-LENGTH
    GOBACK.

END PROGRAM Encode-Int.

*> --- Encode-VarInt ---
*> Encode a signed 32-bit integer to a VarInt and store it in a buffer.
IDENTIFICATION DIVISION.
PROGRAM-ID. Encode-VarInt.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 CURRENT-VALUE        BINARY-LONG UNSIGNED.
    01 BYTE-NUMERIC         BINARY-CHAR UNSIGNED.
    01 BYTE-BINARY REDEFINES BYTE-NUMERIC PIC X.
LINKAGE SECTION.
    01 LK-VALUE-IN          BINARY-LONG.
    01 LK-VALUE-OUT         PIC X ANY LENGTH.
    01 LK-OUT-LENGTH        BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-VALUE-IN LK-VALUE-OUT LK-OUT-LENGTH.
    *> If the number is negative, we need to take the two's complement.
    *> This is done by computing 2^32 - |x|, where x is the input number.
    IF LK-VALUE-IN < 0
        COMPUTE CURRENT-VALUE = 4294967296 + LK-VALUE-IN
    ELSE
        MOVE LK-VALUE-IN TO CURRENT-VALUE
    END-IF
    *> Perform the conversion to VarInt
    MOVE 0 TO LK-OUT-LENGTH
    PERFORM UNTIL EXIT
        ADD 1 TO LK-OUT-LENGTH
        DIVIDE CURRENT-VALUE BY 128 GIVING CURRENT-VALUE REMAINDER BYTE-NUMERIC
        IF CURRENT-VALUE = 0
            MOVE BYTE-BINARY TO LK-VALUE-OUT(LK-OUT-LENGTH:1)
            EXIT PERFORM
        END-IF
        ADD 128 TO BYTE-NUMERIC
        MOVE BYTE-BINARY TO LK-VALUE-OUT(LK-OUT-LENGTH:1)
    END-PERFORM
    GOBACK.

END PROGRAM Encode-VarInt.

*> --- Encode-GetVarIntLength ---
*> Compute the number of bytes required to encode a signed 32-bit integer as a VarInt.
IDENTIFICATION DIVISION.
PROGRAM-ID. Encode-GetVarIntLength.

DATA DIVISION.
LINKAGE SECTION.
    01 LK-VALUE-IN          BINARY-LONG.
    01 LK-OUT-LENGTH        BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-VALUE-IN LK-OUT-LENGTH.
    EVALUATE LK-VALUE-IN
        WHEN < 0
            MOVE 5 TO LK-OUT-LENGTH
        WHEN >= 268435456
            MOVE 5 TO LK-OUT-LENGTH
        WHEN >= 2097152
            MOVE 4 TO LK-OUT-LENGTH
        WHEN >= 16384
            MOVE 3 TO LK-OUT-LENGTH
        WHEN >= 128
            MOVE 2 TO LK-OUT-LENGTH
        WHEN OTHER
            MOVE 1 TO LK-OUT-LENGTH
    END-EVALUATE
    GOBACK.

END PROGRAM Encode-GetVarIntLength.

*> --- Encode-UnsignedLong ---
*> Encode a 64-bit unsigned integer and store it in a buffer (big-endian).
IDENTIFICATION DIVISION.
PROGRAM-ID. Encode-UnsignedLong.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 VALUE-NUMERIC        BINARY-LONG-LONG UNSIGNED.
    01 VALUE-BYTES REDEFINES VALUE-NUMERIC PIC X(8).
LINKAGE SECTION.
    01 LK-VALUE-IN          BINARY-LONG-LONG UNSIGNED.
    01 LK-VALUE-OUT         PIC X ANY LENGTH.
    01 LK-OUT-LENGTH        BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-VALUE-IN LK-VALUE-OUT LK-OUT-LENGTH.
    MOVE LK-VALUE-IN TO VALUE-NUMERIC
    MOVE FUNCTION REVERSE(VALUE-BYTES) TO LK-VALUE-OUT(1:8)
    MOVE 8 TO LK-OUT-LENGTH
    GOBACK.

END PROGRAM Encode-UnsignedLong.

*> --- Encode-Long ---
*> Encode a 64-bit integer and store it in a buffer (big-endian).
IDENTIFICATION DIVISION.
PROGRAM-ID. Encode-Long.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 UNSIGNED-VALUE       BINARY-LONG-LONG UNSIGNED.
LINKAGE SECTION.
    01 LK-VALUE-IN          BINARY-LONG-LONG.
    01 LK-VALUE-OUT         PIC X ANY LENGTH.
    01 LK-OUT-LENGTH        BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-VALUE-IN LK-VALUE-OUT LK-OUT-LENGTH.
    *> If the number is negative, we need to take the two's complement.
    *> This is done by computing 2^64 - |x|, where x is the input number.
    IF LK-VALUE-IN < 0
        COMPUTE UNSIGNED-VALUE = 18446744073709551616 + LK-VALUE-IN
    ELSE
        MOVE LK-VALUE-IN TO UNSIGNED-VALUE
    END-IF
    *> Perform the conversion to bytes
    CALL "Encode-UnsignedLong" USING UNSIGNED-VALUE LK-VALUE-OUT LK-OUT-LENGTH
    GOBACK.

END PROGRAM Encode-Long.

*> --- Encode-Double ---
*> Encode a 64-bit floating point number (FLOAT-LONG) and store it in a buffer (8 bytes IEEE 754).
IDENTIFICATION DIVISION.
PROGRAM-ID. Encode-Double.

DATA DIVISION.
LINKAGE SECTION.
    01 LK-VALUE-IN          FLOAT-LONG.
    01 LK-VALUE-OUT         PIC X ANY LENGTH.
    01 LK-OUT-LENGTH        BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-VALUE-IN LK-VALUE-OUT LK-OUT-LENGTH.
    CALL "Util-DoubleGetBytes" USING LK-VALUE-IN LK-VALUE-OUT
    MOVE 8 TO LK-OUT-LENGTH
    GOBACK.

END PROGRAM Encode-Double.

*> --- Encode-Float ---
*> Encode a 32-bit floating point number (FLOAT-SHORT) and store it in a buffer (4 bytes IEEE 754).
IDENTIFICATION DIVISION.
PROGRAM-ID. Encode-Float.

DATA DIVISION.
LINKAGE SECTION.
    01 LK-VALUE-IN          FLOAT-SHORT.
    01 LK-VALUE-OUT         PIC X ANY LENGTH.
    01 LK-OUT-LENGTH        BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-VALUE-IN LK-VALUE-OUT LK-OUT-LENGTH.
    CALL "Util-FloatGetBytes" USING LK-VALUE-IN LK-VALUE-OUT
    MOVE 4 TO LK-OUT-LENGTH
    GOBACK.

END PROGRAM Encode-Float.

*> --- Encode-Angle ---
*> Encode a floating-point angle into a single byte (0-255) in steps of 1/256th of a full circle.
IDENTIFICATION DIVISION.
PROGRAM-ID. Encode-Angle.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 BYTE-NUMERIC         BINARY-CHAR UNSIGNED.
    01 BYTE-BINARY REDEFINES BYTE-NUMERIC PIC X.
LINKAGE SECTION.
    01 LK-VALUE-IN          FLOAT-SHORT.
    01 LK-VALUE-OUT         PIC X ANY LENGTH.
    01 LK-OUT-LENGTH        BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-VALUE-IN LK-VALUE-OUT LK-OUT-LENGTH.
    MOVE FUNCTION MOD(LK-VALUE-IN * 256.0 / 360.0, 256) TO BYTE-NUMERIC
    MOVE BYTE-BINARY TO LK-VALUE-OUT
    MOVE 1 TO LK-OUT-LENGTH
    GOBACK.

END PROGRAM Encode-Angle.

*> --- Encode-Position ---
*> Encode a block position into a buffer. The position is encoded as a 64-bit integer where the 26 least-significant bits are X,
*> the next 12 bits are Y, and the last 26 bits are Z (all of them signed).
IDENTIFICATION DIVISION.
PROGRAM-ID. Encode-Position.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 UINT-VALUE           BINARY-LONG-LONG UNSIGNED.
LINKAGE SECTION.
    01 LK-VALUE-IN.
        02 LK-X             BINARY-LONG.
        02 LK-Y             BINARY-LONG.
        02 LK-Z             BINARY-LONG.
    01 LK-VALUE-OUT         PIC X ANY LENGTH.
    01 LK-OUT-LENGTH        BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-VALUE-IN LK-VALUE-OUT LK-OUT-LENGTH.
    *> Place X into the 26 most-significant bits
    IF LK-X < 0
        COMPUTE UINT-VALUE = (67108864 + LK-X) * (2 ** 38)
    ELSE
        COMPUTE UINT-VALUE = LK-X * (2 ** 38)
    END-IF

    *> Place Z into the 26 middle bits
    IF LK-Z < 0
        COMPUTE UINT-VALUE = UINT-VALUE + (67108864 + LK-Z) * (2 ** 12)
    ELSE
        COMPUTE UINT-VALUE = UINT-VALUE + LK-Z * (2 ** 12)
    END-IF

    *> Place Y into the 12 least-significant bits
    IF LK-Y < 0
        COMPUTE UINT-VALUE = UINT-VALUE + 4096 + LK-Y
    ELSE
        COMPUTE UINT-VALUE = UINT-VALUE + LK-Y
    END-IF

    *> Encode as an unsigned long
    CALL "Encode-UnsignedLong" USING UINT-VALUE LK-VALUE-OUT LK-OUT-LENGTH

    GOBACK.

END PROGRAM Encode-Position.
