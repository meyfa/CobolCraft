>>IF GCVERSION >= 32
  *> GnuCOBOL before 3.2 errors on any compile-time known ref-mode length
  REPLACE ==BYTE-COUNT== BY ==LENGTH OF VALUE-BYTES==.
>>END-IF

*> --- Decode-Byte ---
*> Decode a byte from a buffer into an 8-bit integer (BINARY-CHAR).
IDENTIFICATION DIVISION.
PROGRAM-ID. Decode-Byte.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 VALUE-BYTES.
        02 VALUE-NUMERIC    BINARY-CHAR.
>>IF GCVERSION < 32
    01 BYTE-COUNT           BINARY-CHAR UNSIGNED       VALUE LENGTH OF VALUE-BYTES.
>>END-IF
LINKAGE SECTION.
    01 LK-BUFFER            PIC X ANY LENGTH.
    01 LK-BUFFERPOS         BINARY-LONG UNSIGNED.
    01 LK-VALUE             BINARY-CHAR.

PROCEDURE DIVISION USING LK-BUFFER LK-BUFFERPOS LK-VALUE.
    MOVE LK-BUFFER(LK-BUFFERPOS:BYTE-COUNT) TO VALUE-BYTES
    MOVE VALUE-NUMERIC TO LK-VALUE
    ADD BYTE-COUNT TO LK-BUFFERPOS
    GOBACK.

END PROGRAM Decode-Byte.

*> --- Decode-UnsignedShort ---
*> Decode a big-endian short from a buffer into a 16-bit unsigned integer (BINARY-SHORT UNSIGNED).
IDENTIFICATION DIVISION.
PROGRAM-ID. Decode-UnsignedShort.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 VALUE-BYTES.
        02 VALUE-NUMERIC    PIC  9(4) USAGE COMP.
>>IF GCVERSION < 32
    01 BYTE-COUNT           BINARY-CHAR UNSIGNED       VALUE LENGTH OF VALUE-BYTES.
>>END-IF
LINKAGE SECTION.
    01 LK-BUFFER            PIC X ANY LENGTH.
    01 LK-BUFFERPOS         BINARY-LONG UNSIGNED.
    01 LK-VALUE             BINARY-SHORT UNSIGNED.

PROCEDURE DIVISION USING LK-BUFFER LK-BUFFERPOS LK-VALUE.
    MOVE LK-BUFFER(LK-BUFFERPOS:BYTE-COUNT) TO VALUE-BYTES
    MOVE VALUE-NUMERIC TO LK-VALUE
    ADD BYTE-COUNT TO LK-BUFFERPOS
    GOBACK.

END PROGRAM Decode-UnsignedShort.

*> --- Decode-Short ---
*> Decode a big-endian short from a buffer into a 16-bit integer (BINARY-SHORT).
IDENTIFICATION DIVISION.
PROGRAM-ID. Decode-Short.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 VALUE-BYTES.
        02 VALUE-NUMERIC    PIC S9(4) USAGE COMP.
>>IF GCVERSION < 32
    01 BYTE-COUNT           BINARY-CHAR UNSIGNED       VALUE LENGTH OF VALUE-BYTES.
>>END-IF
LINKAGE SECTION.
    01 LK-BUFFER            PIC X ANY LENGTH.
    01 LK-BUFFERPOS         BINARY-LONG UNSIGNED.
    01 LK-VALUE             BINARY-SHORT.

PROCEDURE DIVISION USING LK-BUFFER LK-BUFFERPOS LK-VALUE.
    MOVE LK-BUFFER(LK-BUFFERPOS:BYTE-COUNT) TO VALUE-BYTES
    MOVE VALUE-NUMERIC TO LK-VALUE
    ADD BYTE-COUNT TO LK-BUFFERPOS
    GOBACK.

END PROGRAM Decode-Short.

*> --- Decode-UnsignedInt ---
*> Decode a big-endian integer from a buffer into a 32-bit unsigned integer (BINARY-LONG UNSIGNED).
IDENTIFICATION DIVISION.
PROGRAM-ID. Decode-UnsignedInt.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 VALUE-BYTES.
        02 VALUE-NUMERIC    PIC  9(9) USAGE COMP.
>>IF GCVERSION < 32
    01 BYTE-COUNT           BINARY-CHAR UNSIGNED       VALUE LENGTH OF VALUE-BYTES.
>>END-IF
LINKAGE SECTION.
    01 LK-BUFFER            PIC X ANY LENGTH.
    01 LK-BUFFERPOS         BINARY-LONG UNSIGNED.
    01 LK-VALUE             BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-BUFFER LK-BUFFERPOS LK-VALUE.
    MOVE LK-BUFFER(LK-BUFFERPOS:BYTE-COUNT) TO VALUE-BYTES
    MOVE VALUE-NUMERIC TO LK-VALUE
    ADD BYTE-COUNT TO LK-BUFFERPOS
    GOBACK.

END PROGRAM Decode-UnsignedInt.

*> --- Decode-Int ---
*> Decode a big-endian integer from a buffer into a 32-bit integer (BINARY-LONG).
IDENTIFICATION DIVISION.
PROGRAM-ID. Decode-Int.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 VALUE-BYTES.
        02 VALUE-NUMERIC    PIC S9(9) USAGE COMP.
>>IF GCVERSION < 32
    01 BYTE-COUNT           BINARY-CHAR UNSIGNED       VALUE LENGTH OF VALUE-BYTES.
>>END-IF
LINKAGE SECTION.
    01 LK-BUFFER            PIC X ANY LENGTH.
    01 LK-BUFFERPOS         BINARY-LONG UNSIGNED.
    01 LK-VALUE             BINARY-LONG.

PROCEDURE DIVISION USING LK-BUFFER LK-BUFFERPOS LK-VALUE.
    MOVE LK-BUFFER(LK-BUFFERPOS:BYTE-COUNT) TO VALUE-BYTES
    MOVE VALUE-NUMERIC TO LK-VALUE
    ADD BYTE-COUNT TO LK-BUFFERPOS
    GOBACK.

END PROGRAM Decode-Int.

*> --- Decode-VarInt ---
*> Decode a VarInt from a buffer into a 32-bit integer (BINARY-LONG).
IDENTIFICATION DIVISION.
PROGRAM-ID. Decode-VarInt.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 BYTE-ALPHA.
        02 BYTE-NUMERIC     BINARY-CHAR UNSIGNED.
    01 SIGNED-VALUE         BINARY-LONG.
    01 CURRENT-VALUE        REDEFINES SIGNED-VALUE BINARY-LONG UNSIGNED.
    01 VARINT-MULTIPLIER    BINARY-LONG UNSIGNED.
LINKAGE SECTION.
    01 LK-BUFFER            PIC X ANY LENGTH.
    01 LK-BUFFERPOS         BINARY-LONG UNSIGNED.
    01 LK-VALUE             BINARY-LONG.

PROCEDURE DIVISION USING LK-BUFFER LK-BUFFERPOS LK-VALUE.
    *> The maximum number of bytes a VarInt can have is 5. The first loop iteration is unrolled for performance:
    *> Since a single-byte VarInt is always positive, fewer instructions are required. Note that this benefits all
    *> VarInts, not just single-byte ones.
    MOVE LK-BUFFER(LK-BUFFERPOS:1) TO BYTE-ALPHA
    ADD 1 TO LK-BUFFERPOS
    IF BYTE-NUMERIC < 128
        MOVE BYTE-NUMERIC TO LK-VALUE
        GOBACK
    END-IF
    *> Not a single-byte VarInt.
    >>IF GCVERSION >= 32
        COMPUTE CURRENT-VALUE = BYTE-NUMERIC B-AND 127
        MOVE 7 TO VARINT-MULTIPLIER
        PERFORM 4 TIMES
            MOVE LK-BUFFER(LK-BUFFERPOS:1) TO BYTE-ALPHA
            ADD 1 TO LK-BUFFERPOS
            IF BYTE-NUMERIC < 128
                COMPUTE CURRENT-VALUE = CURRENT-VALUE B-OR (BYTE-NUMERIC B-SHIFT-L VARINT-MULTIPLIER)
                EXIT PERFORM
            END-IF
            COMPUTE CURRENT-VALUE = CURRENT-VALUE B-OR ((BYTE-NUMERIC B-AND 127) B-SHIFT-L VARINT-MULTIPLIER)
            ADD 7 TO VARINT-MULTIPLIER
        END-PERFORM
    >>ELSE
        SUBTRACT 128 FROM BYTE-NUMERIC GIVING CURRENT-VALUE
        MOVE 128 TO VARINT-MULTIPLIER
        PERFORM 4 TIMES
            MOVE LK-BUFFER(LK-BUFFERPOS:1) TO BYTE-ALPHA
            ADD 1 TO LK-BUFFERPOS
            IF BYTE-NUMERIC < 128
                COMPUTE CURRENT-VALUE = CURRENT-VALUE + BYTE-NUMERIC * VARINT-MULTIPLIER
                EXIT PERFORM
            END-IF
            COMPUTE CURRENT-VALUE = CURRENT-VALUE + (BYTE-NUMERIC - 128) * VARINT-MULTIPLIER
            MULTIPLY VARINT-MULTIPLIER BY 128 GIVING VARINT-MULTIPLIER
        END-PERFORM
    >>END-IF
    MOVE SIGNED-VALUE TO LK-VALUE
    GOBACK.

END PROGRAM Decode-VarInt.

*> --- Decode-UnsignedLong ---
*> Decode a big-endian long from a buffer into a 64-bit unsigned integer (BINARY-LONG-LONG UNSIGNED).
IDENTIFICATION DIVISION.
PROGRAM-ID. Decode-UnsignedLong.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 VALUE-BYTES.
        02 VALUE-NUMERIC    PIC  9(18) USAGE COMP.
>>IF GCVERSION < 32
    01 BYTE-COUNT           BINARY-CHAR UNSIGNED       VALUE LENGTH OF VALUE-BYTES.
>>END-IF
LINKAGE SECTION.
    01 LK-BUFFER            PIC X ANY LENGTH.
    01 LK-BUFFERPOS         BINARY-LONG UNSIGNED.
    01 LK-VALUE             BINARY-LONG-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-BUFFER LK-BUFFERPOS LK-VALUE.
    MOVE LK-BUFFER(LK-BUFFERPOS:BYTE-COUNT) TO VALUE-BYTES
    MOVE VALUE-NUMERIC TO LK-VALUE
    ADD BYTE-COUNT TO LK-BUFFERPOS
    GOBACK.

END PROGRAM Decode-UnsignedLong.

*> --- Decode-Long ---
*> Decode a big-endian long from a buffer into a 64-bit integer (BINARY-LONG-LONG).
IDENTIFICATION DIVISION.
PROGRAM-ID. Decode-Long.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 VALUE-BYTES.
        02 VALUE-NUMERIC    PIC S9(18) USAGE COMP.
>>IF GCVERSION < 32
    01 BYTE-COUNT           BINARY-CHAR UNSIGNED       VALUE LENGTH OF VALUE-BYTES.
>>END-IF
LINKAGE SECTION.
    01 LK-BUFFER            PIC X ANY LENGTH.
    01 LK-BUFFERPOS         BINARY-LONG UNSIGNED.
    01 LK-VALUE             BINARY-LONG-LONG.

PROCEDURE DIVISION USING LK-BUFFER LK-BUFFERPOS LK-VALUE.
    MOVE LK-BUFFER(LK-BUFFERPOS:BYTE-COUNT) TO VALUE-BYTES
    MOVE VALUE-NUMERIC TO LK-VALUE
    ADD BYTE-COUNT TO LK-BUFFERPOS
    GOBACK.

END PROGRAM Decode-Long.

*> --- Decode-Double ---
*> Decode a big-endian double from a buffer into a double-precision floating-point number (FLOAT-LONG).
IDENTIFICATION DIVISION.
PROGRAM-ID. Decode-Double.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 VALUE-BYTES.
        02 VALUE-DOUBLE     FLOAT-LONG.
>>IF GCVERSION < 32
    01 BYTE-COUNT           BINARY-CHAR UNSIGNED       VALUE LENGTH OF VALUE-BYTES.
>>END-IF
LINKAGE SECTION.
    01 LK-BUFFER            PIC X ANY LENGTH.
    01 LK-BUFFERPOS         BINARY-LONG UNSIGNED.
    01 LK-VALUE             FLOAT-LONG.

PROCEDURE DIVISION USING LK-BUFFER LK-BUFFERPOS LK-VALUE.
    MOVE FUNCTION REVERSE(LK-BUFFER(LK-BUFFERPOS:BYTE-COUNT)) TO VALUE-BYTES
    MOVE VALUE-DOUBLE TO LK-VALUE
    ADD BYTE-COUNT TO LK-BUFFERPOS
    GOBACK.

END PROGRAM Decode-Double.

*> --- Decode-Float ---
*> Decode a big-endian float from a buffer into a single-precision floating-point number (FLOAT).
IDENTIFICATION DIVISION.
PROGRAM-ID. Decode-Float.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 VALUE-BYTES.
        02 VALUE-FLOAT      FLOAT-SHORT.
>>IF GCVERSION < 32
    01 BYTE-COUNT           BINARY-CHAR UNSIGNED       VALUE LENGTH OF VALUE-BYTES.
>>END-IF
LINKAGE SECTION.
    01 LK-BUFFER            PIC X ANY LENGTH.
    01 LK-BUFFERPOS         BINARY-LONG UNSIGNED.
    01 LK-VALUE             FLOAT.

PROCEDURE DIVISION USING LK-BUFFER LK-BUFFERPOS LK-VALUE.
    MOVE FUNCTION REVERSE(LK-BUFFER(LK-BUFFERPOS:BYTE-COUNT)) TO VALUE-BYTES
    MOVE VALUE-FLOAT TO LK-VALUE
    ADD BYTE-COUNT TO LK-BUFFERPOS
    GOBACK.

END PROGRAM Decode-Float.

*> --- Decode-String ---
*> Decode a string from a buffer. The string is prefixed with its length as a VarInt.
*> The string is read into a target buffer which must be large enough to hold the entire string, or the string will be
*> truncated.
IDENTIFICATION DIVISION.
PROGRAM-ID. Decode-String.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 VALUE-LENGTH         BINARY-LONG UNSIGNED.
LINKAGE SECTION.
    01 LK-BUFFER            PIC X ANY LENGTH.
    01 LK-BUFFERPOS         BINARY-LONG UNSIGNED.
    01 LK-STR-LENGTH        BINARY-LONG.
    01 LK-VALUE             PIC X ANY LENGTH.

PROCEDURE DIVISION USING LK-BUFFER LK-BUFFERPOS LK-STR-LENGTH LK-VALUE.
    *> Read the length
    CALL "Decode-VarInt" USING LK-BUFFER LK-BUFFERPOS LK-STR-LENGTH
    IF LK-STR-LENGTH < 0
        GOBACK
    END-IF
    *> Read the string. If the target buffer is too small, read only as much as fits.
    COMPUTE VALUE-LENGTH = FUNCTION MIN(LK-STR-LENGTH, FUNCTION LENGTH(LK-VALUE))
    MOVE LK-BUFFER(LK-BUFFERPOS:VALUE-LENGTH) TO LK-VALUE(1:VALUE-LENGTH)
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

*> --- Decode-InventorySlot ---
*> Decode an inventory slot from a buffer, using the network protocol format.
IDENTIFICATION DIVISION.
PROGRAM-ID. Decode-InventorySlot.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 INT32                    BINARY-LONG.
    01 COMPONENTS-OFFSET        BINARY-LONG UNSIGNED.
    01 COMPONENTS-ADD-COUNT     BINARY-LONG.
    01 COMPONENTS-REMOVE-COUNT  BINARY-LONG.
    01 COMPONENTS-LENGTH        BINARY-LONG UNSIGNED.
    01 COMPONENT-ID             BINARY-LONG.
LINKAGE SECTION.
    01 LK-BUFFER                PIC X ANY LENGTH.
    01 LK-BUFFERPOS             BINARY-LONG UNSIGNED.
    01 LK-SLOT.
        COPY DD-INVENTORY-SLOT REPLACING LEADING ==PREFIX== BY ==LK==.

PROCEDURE DIVISION USING LK-BUFFER LK-BUFFERPOS LK-SLOT.
    *> TODO Actually parse the data components

    *> count
    CALL "Decode-VarInt" USING LK-BUFFER LK-BUFFERPOS INT32
    MOVE INT32 TO LK-SLOT-COUNT

    IF LK-SLOT-COUNT > 0
        *> id
        CALL "Decode-VarInt" USING LK-BUFFER LK-BUFFERPOS LK-SLOT-ID

        *> components
        MOVE LK-BUFFERPOS TO COMPONENTS-OFFSET
        CALL "Decode-VarInt" USING LK-BUFFER COMPONENTS-OFFSET COMPONENTS-ADD-COUNT
        CALL "Decode-VarInt" USING LK-BUFFER COMPONENTS-OFFSET COMPONENTS-REMOVE-COUNT
        PERFORM COMPONENTS-ADD-COUNT TIMES
            CALL "Components-LengthOf" USING LK-BUFFER COMPONENTS-OFFSET COMPONENTS-LENGTH
            ADD COMPONENTS-LENGTH TO COMPONENTS-OFFSET
        END-PERFORM
        PERFORM COMPONENTS-REMOVE-COUNT TIMES
            CALL "Decode-VarInt" USING LK-BUFFER COMPONENTS-OFFSET COMPONENT-ID
        END-PERFORM

        COMPUTE LK-SLOT-NBT-LENGTH = COMPONENTS-OFFSET - LK-BUFFERPOS
        COPY ASSERT REPLACING COND BY ==LK-SLOT-NBT-LENGTH <= LENGTH OF LK-SLOT-NBT-DATA==,
            MSG BY =="Item data exceeds buffer size: " LK-SLOT-NBT-LENGTH==.

        MOVE LK-BUFFER(LK-BUFFERPOS:LK-SLOT-NBT-LENGTH) TO LK-SLOT-NBT-DATA(1:LK-SLOT-NBT-LENGTH)

        MOVE COMPONENTS-OFFSET TO LK-BUFFERPOS
    END-IF

    GOBACK.

END PROGRAM Decode-InventorySlot.
