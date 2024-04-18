*> --- Encode-UnsignedShort ---
*> Encode a 16-bit unsigned integer and store it in a buffer (big-endian).
IDENTIFICATION DIVISION.
PROGRAM-ID. Encode-UnsignedShort.

DATA DIVISION.
LOCAL-STORAGE SECTION.
    01 CURRENT-VALUE        BINARY-SHORT UNSIGNED.
    01 CURRENT-VALUE-BYTE   BINARY-CHAR UNSIGNED.
    01 I                    INDEX.
LINKAGE SECTION.
    01 LK-VALUE-IN          BINARY-SHORT UNSIGNED.
    01 LK-VALUE-OUT         PIC X(2).
    01 LK-OUT-LENGTH        BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-VALUE-IN LK-VALUE-OUT LK-OUT-LENGTH.
    MOVE LK-VALUE-IN TO CURRENT-VALUE
    MOVE 2 TO LK-OUT-LENGTH
    PERFORM VARYING I FROM 1 BY 1 UNTIL I > LK-OUT-LENGTH
        DIVIDE CURRENT-VALUE BY 256 GIVING CURRENT-VALUE REMAINDER CURRENT-VALUE-BYTE
        MOVE FUNCTION CHAR(CURRENT-VALUE-BYTE + 1) TO LK-VALUE-OUT(LK-OUT-LENGTH + 1 - I:1)
    END-PERFORM
    GOBACK.

END PROGRAM Encode-UnsignedShort.

*> --- Encode-Short ---
*> Encode a 16-bit integer and store it in a buffer (big-endian).
IDENTIFICATION DIVISION.
PROGRAM-ID. Encode-Short.

DATA DIVISION.
LOCAL-STORAGE SECTION.
    01 UNSIGNED-VALUE       BINARY-SHORT UNSIGNED.
LINKAGE SECTION.
    01 LK-VALUE-IN          BINARY-SHORT.
    01 LK-VALUE-OUT         PIC X(2).
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

*> --- Encode-Int ---
*> Encode a 32-bit integer and store it in a buffer (big-endian).
IDENTIFICATION DIVISION.
PROGRAM-ID. Encode-Int.

DATA DIVISION.
LOCAL-STORAGE SECTION.
    01 CURRENT-VALUE        BINARY-LONG UNSIGNED.
    01 CURRENT-VALUE-BYTE   BINARY-CHAR UNSIGNED.
    01 I                    INDEX.
LINKAGE SECTION.
    01 LK-VALUE-IN          BINARY-LONG.
    01 LK-VALUE-OUT         PIC X(4).
    01 LK-OUT-LENGTH        BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-VALUE-IN LK-VALUE-OUT LK-OUT-LENGTH.
    *> If the number is negative, we need to take the two's complement.
    *> This is done by computing 2^32 - |x|, where x is the input number.
    IF LK-VALUE-IN < 0
        COMPUTE CURRENT-VALUE = 4294967296 + LK-VALUE-IN
    ELSE
        MOVE LK-VALUE-IN TO CURRENT-VALUE
    END-IF
    *> Perform the conversion to bytes
    MOVE 4 TO LK-OUT-LENGTH
    PERFORM VARYING I FROM 1 BY 1 UNTIL I > LK-OUT-LENGTH
        DIVIDE CURRENT-VALUE BY 256 GIVING CURRENT-VALUE REMAINDER CURRENT-VALUE-BYTE
        MOVE FUNCTION CHAR(CURRENT-VALUE-BYTE + 1) TO LK-VALUE-OUT(LK-OUT-LENGTH + 1 - I:1)
    END-PERFORM
    GOBACK.

END PROGRAM Encode-Int.

*> --- Encode-VarInt ---
*> Encode a signed 32-bit integer to a VarInt and store it in a buffer.
IDENTIFICATION DIVISION.
PROGRAM-ID. Encode-VarInt.

DATA DIVISION.
LOCAL-STORAGE SECTION.
    01 CURRENT-VALUE        BINARY-LONG UNSIGNED.
    01 CURRENT-VALUE-BITS   BINARY-CHAR UNSIGNED.
LINKAGE SECTION.
    01 LK-VALUE-IN          BINARY-LONG.
    01 LK-VALUE-OUT         PIC X(5).
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
        MOVE FUNCTION MOD(CURRENT-VALUE, 128) TO CURRENT-VALUE-BITS
        DIVIDE CURRENT-VALUE BY 128 GIVING CURRENT-VALUE
        IF CURRENT-VALUE = 0
            MOVE FUNCTION CHAR(CURRENT-VALUE-BITS + 1) TO LK-VALUE-OUT(LK-OUT-LENGTH:1)
            EXIT PERFORM
        END-IF
        MOVE FUNCTION CHAR(CURRENT-VALUE-BITS + 128 + 1) TO LK-VALUE-OUT(LK-OUT-LENGTH:1)
    END-PERFORM
    GOBACK.

END PROGRAM Encode-VarInt.

*> --- Encode-UnsignedLong ---
*> Encode a 64-bit unsigned integer and store it in a buffer (big-endian).
IDENTIFICATION DIVISION.
PROGRAM-ID. Encode-UnsignedLong.

DATA DIVISION.
LOCAL-STORAGE SECTION.
    01 CURRENT-VALUE        BINARY-LONG-LONG UNSIGNED.
    01 CURRENT-VALUE-BYTE   BINARY-CHAR UNSIGNED.
    01 I                    INDEX.
LINKAGE SECTION.
    01 LK-VALUE-IN          BINARY-LONG-LONG UNSIGNED.
    01 LK-VALUE-OUT         PIC X(8).
    01 LK-OUT-LENGTH        BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-VALUE-IN LK-VALUE-OUT LK-OUT-LENGTH.
    MOVE LK-VALUE-IN TO CURRENT-VALUE
    MOVE 8 TO LK-OUT-LENGTH
    PERFORM VARYING I FROM 1 BY 1 UNTIL I > LK-OUT-LENGTH
        DIVIDE CURRENT-VALUE BY 256 GIVING CURRENT-VALUE REMAINDER CURRENT-VALUE-BYTE
        MOVE FUNCTION CHAR(CURRENT-VALUE-BYTE + 1) TO LK-VALUE-OUT(LK-OUT-LENGTH + 1 - I:1)
    END-PERFORM
    GOBACK.

END PROGRAM Encode-UnsignedLong.

*> --- Encode-Long ---
*> Encode a 64-bit integer and store it in a buffer (big-endian).
IDENTIFICATION DIVISION.
PROGRAM-ID. Encode-Long.

DATA DIVISION.
LOCAL-STORAGE SECTION.
    01 UNSIGNED-VALUE       BINARY-LONG-LONG UNSIGNED.
LINKAGE SECTION.
    01 LK-VALUE-IN          BINARY-LONG-LONG.
    01 LK-VALUE-OUT         PIC X(8).
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
LOCAL-STORAGE SECTION.
    01 SIGNBIT              BINARY-CHAR UNSIGNED.
    01 EXPONENT             BINARY-LONG.
    01 TEMP                 FLOAT-LONG.
LINKAGE SECTION.
    01 LK-VALUE-IN          FLOAT-LONG.
    01 LK-VALUE-OUT         PIC X(8).
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
LOCAL-STORAGE SECTION.
    01 SIGNBIT              BINARY-CHAR UNSIGNED.
    01 EXPONENT             BINARY-SHORT.
    01 TEMP                 FLOAT-SHORT.
LINKAGE SECTION.
    01 LK-VALUE-IN          FLOAT-SHORT.
    01 LK-VALUE-OUT         PIC X(4).
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
LOCAL-STORAGE SECTION.
    01 ANGLE                FLOAT-SHORT.
    01 ANGLE-BYTE           BINARY-CHAR UNSIGNED.
LINKAGE SECTION.
    01 LK-VALUE-IN          FLOAT-SHORT.
    01 LK-VALUE-OUT         PIC X(1).
    01 LK-OUT-LENGTH        BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-VALUE-IN LK-VALUE-OUT LK-OUT-LENGTH.
    COMPUTE ANGLE = LK-VALUE-IN * 256.0 / 360.0
    COMPUTE ANGLE-BYTE = FUNCTION MOD(ANGLE, 256)
    MOVE FUNCTION CHAR(ANGLE-BYTE + 1) TO LK-VALUE-OUT
    MOVE 1 TO LK-OUT-LENGTH
    GOBACK.

END PROGRAM Encode-Angle.

*> --- Encode-Position ---
*> Encode a block position into a buffer. The position is encoded as a 64-bit integer where the 26 least-significant bits are X,
*> the next 12 bits are Y, and the last 26 bits are Z (all of them signed).
IDENTIFICATION DIVISION.
PROGRAM-ID. Encode-Position.

DATA DIVISION.
LOCAL-STORAGE SECTION.
    01 UINT-VALUE           BINARY-LONG-LONG UNSIGNED.
LINKAGE SECTION.
    01 LK-VALUE-IN.
        02 LK-X             BINARY-LONG.
        02 LK-Y             BINARY-LONG.
        02 LK-Z             BINARY-LONG.
    01 LK-VALUE-OUT         PIC X(8).
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
