*> --- Encode-Byte ---
*> Encode a signed byte and store it in a buffer.
IDENTIFICATION DIVISION.
PROGRAM-ID. Encode-Byte.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 VALUE-BYTES.
        02 VALUE-NUMERIC    BINARY-CHAR.
LINKAGE SECTION.
    01 LK-VALUE-IN          BINARY-CHAR.
    01 LK-VALUE-OUT         PIC X ANY LENGTH.
    01 LK-OUT-OFFSET        BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-VALUE-IN LK-VALUE-OUT LK-OUT-OFFSET.
    MOVE LK-VALUE-IN TO VALUE-NUMERIC
    MOVE VALUE-BYTES TO LK-VALUE-OUT(LK-OUT-OFFSET:1)
    ADD 1 TO LK-OUT-OFFSET
    GOBACK.

END PROGRAM Encode-Byte.

*> --- Encode-UnsignedShort ---
*> Encode a 16-bit unsigned integer and store it in a buffer (big-endian).
IDENTIFICATION DIVISION.
PROGRAM-ID. Encode-UnsignedShort.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 BYTE-COUNT           BINARY-CHAR UNSIGNED            VALUE 2.
    01 VALUE-BYTES.
        02 VALUE-NUMERIC    BINARY-SHORT UNSIGNED.
LINKAGE SECTION.
    01 LK-VALUE-IN          BINARY-SHORT UNSIGNED.
    01 LK-VALUE-OUT         PIC X ANY LENGTH.
    01 LK-OUT-OFFSET        BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-VALUE-IN LK-VALUE-OUT LK-OUT-OFFSET.
    MOVE LK-VALUE-IN TO VALUE-NUMERIC
    MOVE FUNCTION REVERSE(VALUE-BYTES) TO LK-VALUE-OUT(LK-OUT-OFFSET:BYTE-COUNT)
    ADD BYTE-COUNT TO LK-OUT-OFFSET
    GOBACK.

END PROGRAM Encode-UnsignedShort.

*> --- Encode-Short ---
*> Encode a 16-bit integer and store it in a buffer (big-endian).
IDENTIFICATION DIVISION.
PROGRAM-ID. Encode-Short.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 BYTE-COUNT           BINARY-CHAR UNSIGNED            VALUE 2.
    01 VALUE-BYTES.
        02 VALUE-NUMERIC    BINARY-SHORT.
LINKAGE SECTION.
    01 LK-VALUE-IN          BINARY-SHORT.
    01 LK-VALUE-OUT         PIC X ANY LENGTH.
    01 LK-OUT-OFFSET        BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-VALUE-IN LK-VALUE-OUT LK-OUT-OFFSET.
    MOVE LK-VALUE-IN TO VALUE-NUMERIC
    MOVE FUNCTION REVERSE(VALUE-BYTES) TO LK-VALUE-OUT(LK-OUT-OFFSET:BYTE-COUNT)
    ADD BYTE-COUNT TO LK-OUT-OFFSET
    GOBACK.

END PROGRAM Encode-Short.

*> --- Encode-UnsignedInt ---
*> Encode a 32-bit unsigned integer and store it in a buffer (big-endian).
IDENTIFICATION DIVISION.
PROGRAM-ID. Encode-UnsignedInt.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 BYTE-COUNT           BINARY-CHAR UNSIGNED            VALUE 4.
    01 VALUE-BYTES.
        02 VALUE-NUMERIC    BINARY-LONG UNSIGNED.
LINKAGE SECTION.
    01 LK-VALUE-IN          BINARY-LONG UNSIGNED.
    01 LK-VALUE-OUT         PIC X ANY LENGTH.
    01 LK-OUT-OFFSET        BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-VALUE-IN LK-VALUE-OUT LK-OUT-OFFSET.
    MOVE LK-VALUE-IN TO VALUE-NUMERIC
    MOVE FUNCTION REVERSE(VALUE-BYTES) TO LK-VALUE-OUT(LK-OUT-OFFSET:BYTE-COUNT)
    ADD BYTE-COUNT TO LK-OUT-OFFSET
    GOBACK.

END PROGRAM Encode-UnsignedInt.

*> --- Encode-Int ---
*> Encode a 32-bit integer and store it in a buffer (big-endian).
IDENTIFICATION DIVISION.
PROGRAM-ID. Encode-Int.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 BYTE-COUNT           BINARY-CHAR UNSIGNED            VALUE 4.
    01 VALUE-BYTES.
        02 VALUE-NUMERIC    BINARY-LONG.
LINKAGE SECTION.
    01 LK-VALUE-IN          BINARY-LONG.
    01 LK-VALUE-OUT         PIC X ANY LENGTH.
    01 LK-OUT-OFFSET        BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-VALUE-IN LK-VALUE-OUT LK-OUT-OFFSET.
    MOVE LK-VALUE-IN TO VALUE-NUMERIC
    MOVE FUNCTION REVERSE(VALUE-BYTES) TO LK-VALUE-OUT(LK-OUT-OFFSET:BYTE-COUNT)
    ADD BYTE-COUNT TO LK-OUT-OFFSET
    GOBACK.

END PROGRAM Encode-Int.

*> --- Encode-VarInt ---
*> Encode a signed 32-bit integer to a VarInt and store it in a buffer.
IDENTIFICATION DIVISION.
PROGRAM-ID. Encode-VarInt.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 SIGNED-VALUE         BINARY-LONG.
    01 CURRENT-VALUE        REDEFINES SIGNED-VALUE BINARY-LONG UNSIGNED.
    01 BYTE-ALPHA.
        02 BYTE-NUMERIC         BINARY-CHAR UNSIGNED.
LINKAGE SECTION.
    01 LK-VALUE-IN          BINARY-LONG.
    01 LK-VALUE-OUT         PIC X ANY LENGTH.
    01 LK-OUT-OFFSET        BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-VALUE-IN LK-VALUE-OUT LK-OUT-OFFSET.
    MOVE LK-VALUE-IN TO SIGNED-VALUE
    PERFORM UNTIL EXIT
        >>IF GCVERSION >= 32
            COMPUTE BYTE-NUMERIC = CURRENT-VALUE B-AND 127
            COMPUTE CURRENT-VALUE = CURRENT-VALUE B-SHIFT-R 7
        >>ELSE
            DIVIDE CURRENT-VALUE BY 128 GIVING CURRENT-VALUE REMAINDER BYTE-NUMERIC
        >>END-IF
        IF CURRENT-VALUE = 0
            MOVE BYTE-ALPHA TO LK-VALUE-OUT(LK-OUT-OFFSET:1)
            ADD 1 TO LK-OUT-OFFSET
            EXIT PERFORM
        END-IF
        ADD 128 TO BYTE-NUMERIC
        MOVE BYTE-ALPHA TO LK-VALUE-OUT(LK-OUT-OFFSET:1)
        ADD 1 TO LK-OUT-OFFSET
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
    01 LK-RESULT            BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-VALUE-IN LK-RESULT.
    EVALUATE LK-VALUE-IN
        WHEN < 0
            MOVE 5 TO LK-RESULT
        WHEN >= 268435456
            MOVE 5 TO LK-RESULT
        WHEN >= 2097152
            MOVE 4 TO LK-RESULT
        WHEN >= 16384
            MOVE 3 TO LK-RESULT
        WHEN >= 128
            MOVE 2 TO LK-RESULT
        WHEN OTHER
            MOVE 1 TO LK-RESULT
    END-EVALUATE
    GOBACK.

END PROGRAM Encode-GetVarIntLength.

*> --- Encode-UnsignedLong ---
*> Encode a 64-bit unsigned integer and store it in a buffer (big-endian).
IDENTIFICATION DIVISION.
PROGRAM-ID. Encode-UnsignedLong.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 BYTE-COUNT           BINARY-CHAR UNSIGNED            VALUE 8.
    01 VALUE-BYTES.
        02 VALUE-NUMERIC    BINARY-LONG-LONG UNSIGNED.
LINKAGE SECTION.
    01 LK-VALUE-IN          BINARY-LONG-LONG UNSIGNED.
    01 LK-VALUE-OUT         PIC X ANY LENGTH.
    01 LK-OUT-OFFSET        BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-VALUE-IN LK-VALUE-OUT LK-OUT-OFFSET.
    MOVE LK-VALUE-IN TO VALUE-NUMERIC
    MOVE FUNCTION REVERSE(VALUE-BYTES) TO LK-VALUE-OUT(LK-OUT-OFFSET:BYTE-COUNT)
    ADD BYTE-COUNT TO LK-OUT-OFFSET
    GOBACK.

END PROGRAM Encode-UnsignedLong.

*> --- Encode-Long ---
*> Encode a 64-bit integer and store it in a buffer (big-endian).
IDENTIFICATION DIVISION.
PROGRAM-ID. Encode-Long.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 BYTE-COUNT           BINARY-CHAR UNSIGNED            VALUE 8.
    01 VALUE-BYTES.
        02 VALUE-NUMERIC    BINARY-LONG-LONG.
LINKAGE SECTION.
    01 LK-VALUE-IN          BINARY-LONG-LONG.
    01 LK-VALUE-OUT         PIC X ANY LENGTH.
    01 LK-OUT-OFFSET        BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-VALUE-IN LK-VALUE-OUT LK-OUT-OFFSET.
    MOVE LK-VALUE-IN TO VALUE-NUMERIC
    MOVE FUNCTION REVERSE(VALUE-BYTES) TO LK-VALUE-OUT(LK-OUT-OFFSET:BYTE-COUNT)
    ADD BYTE-COUNT TO LK-OUT-OFFSET
    GOBACK.

END PROGRAM Encode-Long.

*> --- Encode-Double ---
*> Encode a 64-bit floating point number (FLOAT-LONG) and store it in a buffer (8 bytes IEEE 754).
IDENTIFICATION DIVISION.
PROGRAM-ID. Encode-Double.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 BYTE-COUNT           BINARY-CHAR UNSIGNED            VALUE 8.
    01 VALUE-BYTES.
        02 VALUE-DOUBLE     FLOAT-LONG.
LINKAGE SECTION.
    01 LK-VALUE-IN          FLOAT-LONG.
    01 LK-VALUE-OUT         PIC X ANY LENGTH.
    01 LK-OUT-OFFSET        BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-VALUE-IN LK-VALUE-OUT LK-OUT-OFFSET.
    MOVE LK-VALUE-IN TO VALUE-DOUBLE
    MOVE FUNCTION REVERSE(VALUE-BYTES) TO LK-VALUE-OUT(LK-OUT-OFFSET:BYTE-COUNT)
    ADD BYTE-COUNT TO LK-OUT-OFFSET
    GOBACK.

END PROGRAM Encode-Double.

*> --- Encode-Float ---
*> Encode a 32-bit floating point number (FLOAT-SHORT) and store it in a buffer (4 bytes IEEE 754).
IDENTIFICATION DIVISION.
PROGRAM-ID. Encode-Float.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 BYTE-COUNT           BINARY-CHAR UNSIGNED            VALUE 4.
    01 VALUE-BYTES.
        02 VALUE-FLOAT      FLOAT-SHORT.
LINKAGE SECTION.
    01 LK-VALUE-IN          FLOAT-SHORT.
    01 LK-VALUE-OUT         PIC X ANY LENGTH.
    01 LK-OUT-OFFSET        BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-VALUE-IN LK-VALUE-OUT LK-OUT-OFFSET.
    MOVE LK-VALUE-IN TO VALUE-FLOAT
    MOVE FUNCTION REVERSE(VALUE-BYTES) TO LK-VALUE-OUT(LK-OUT-OFFSET:BYTE-COUNT)
    ADD BYTE-COUNT TO LK-OUT-OFFSET
    GOBACK.

END PROGRAM Encode-Float.

*> --- Encode-String ---
*> Encode a string into a buffer. The string is prefixed with its length as a VarInt.
IDENTIFICATION DIVISION.
PROGRAM-ID. Encode-String.

DATA DIVISION.
LINKAGE SECTION.
    01 LK-STRING            PIC X ANY LENGTH.
    01 LK-STRING-LENGTH     BINARY-LONG UNSIGNED.
    01 LK-VALUE-OUT         PIC X ANY LENGTH.
    01 LK-OUT-OFFSET        BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-STRING LK-STRING-LENGTH LK-VALUE-OUT LK-OUT-OFFSET.
    CALL "Encode-VarInt" USING LK-STRING-LENGTH LK-VALUE-OUT LK-OUT-OFFSET
    MOVE LK-STRING(1:LK-STRING-LENGTH) TO LK-VALUE-OUT(LK-OUT-OFFSET:LK-STRING-LENGTH)
    ADD LK-STRING-LENGTH TO LK-OUT-OFFSET
    GOBACK.

END PROGRAM Encode-String.

*> --- Encode-Angle ---
*> Encode a floating-point angle into a single byte (0-255) in steps of 1/256th of a full circle.
IDENTIFICATION DIVISION.
PROGRAM-ID. Encode-Angle.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 BYTE-BINARY.
        02 BYTE-NUMERIC     BINARY-CHAR UNSIGNED.
LINKAGE SECTION.
    01 LK-VALUE-IN          FLOAT-SHORT.
    01 LK-VALUE-OUT         PIC X ANY LENGTH.
    01 LK-OUT-OFFSET        BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-VALUE-IN LK-VALUE-OUT LK-OUT-OFFSET.
    MOVE FUNCTION MOD(LK-VALUE-IN * 256.0 / 360.0, 256) TO BYTE-NUMERIC
    MOVE BYTE-BINARY TO LK-VALUE-OUT(LK-OUT-OFFSET:1)
    ADD 1 TO LK-OUT-OFFSET
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
    01 LK-OUT-OFFSET        BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-VALUE-IN LK-VALUE-OUT LK-OUT-OFFSET.
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
    CALL "Encode-UnsignedLong" USING UINT-VALUE LK-VALUE-OUT LK-OUT-OFFSET

    GOBACK.

END PROGRAM Encode-Position.

*> --- Encode-InventorySlot ---
*> Encode an inventory slot into a buffer, using the network protocol format.
IDENTIFICATION DIVISION.
PROGRAM-ID. Encode-InventorySlot.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 INT32                    BINARY-LONG.
LINKAGE SECTION.
    01 LK-SLOT.
        COPY DD-INVENTORY-SLOT REPLACING LEADING ==PREFIX== BY ==LK==.
    01 LK-VALUE-OUT             PIC X ANY LENGTH.
    01 LK-OUT-OFFSET            BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-SLOT LK-VALUE-OUT LK-OUT-OFFSET.
    MOVE LK-SLOT-COUNT TO INT32
    CALL "Encode-VarInt" USING INT32 LK-VALUE-OUT LK-OUT-OFFSET

    IF INT32 > 0
        CALL "Encode-VarInt" USING LK-SLOT-ID LK-VALUE-OUT LK-OUT-OFFSET
        MOVE LK-SLOT-NBT-DATA(1:LK-SLOT-NBT-LENGTH) TO LK-VALUE-OUT(LK-OUT-OFFSET:LK-SLOT-NBT-LENGTH)
        ADD LK-SLOT-NBT-LENGTH TO LK-OUT-OFFSET
    END-IF

    GOBACK.

END PROGRAM Encode-InventorySlot.
