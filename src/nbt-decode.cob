*> --- NbtDecode-ReadString ---
IDENTIFICATION DIVISION.
PROGRAM-ID. NbtDecode-ReadString.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 UINT16           BINARY-SHORT UNSIGNED.
LINKAGE SECTION.
    01 LK-BUFFER        PIC X ANY LENGTH.
    01 LK-OFFSET        BINARY-LONG UNSIGNED.
    01 LK-STRING        PIC X ANY LENGTH.
    01 LK-STRING-LENGTH BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-BUFFER LK-OFFSET LK-STRING LK-STRING-LENGTH.
    CALL "Decode-UnsignedShort" USING LK-BUFFER LK-OFFSET UINT16
    MOVE UINT16 TO LK-STRING-LENGTH
    MOVE LK-BUFFER(LK-OFFSET:UINT16) TO LK-STRING
    ADD UINT16 TO LK-OFFSET
    GOBACK.

END PROGRAM NbtDecode-ReadString.

*> --- NbtDecode-SkipString ---
IDENTIFICATION DIVISION.
PROGRAM-ID. NbtDecode-SkipString.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 UINT16           BINARY-SHORT UNSIGNED.
LINKAGE SECTION.
    01 LK-BUFFER        PIC X ANY LENGTH.
    01 LK-OFFSET        BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-BUFFER LK-OFFSET.
    CALL "Decode-UnsignedShort" USING LK-BUFFER LK-OFFSET UINT16
    ADD UINT16 TO LK-OFFSET
    GOBACK.

END PROGRAM NbtDecode-SkipString.

*> --- NbtDecode-End ---
IDENTIFICATION DIVISION.
PROGRAM-ID. NbtDecode-End.

DATA DIVISION.
LINKAGE SECTION.
    COPY DD-NBT-DECODER REPLACING LEADING ==NBT-DECODER== BY ==LK==.
    01 LK-BUFFER        PIC X ANY LENGTH.
    01 LK-OFFSET        BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-STATE LK-BUFFER LK-OFFSET.
    IF LK-OFFSET > LENGTH OF LK-BUFFER OR LK-BUFFER(LK-OFFSET:1) NOT = X"00"
        *> TODO handle error
        GOBACK
    END-IF
    ADD 1 TO LK-OFFSET

    *> Pop the stack
    SUBTRACT 1 FROM LK-LEVEL

    GOBACK.

END PROGRAM NbtDecode-End.

*> --- NbtDecode-Long ---
IDENTIFICATION DIVISION.
PROGRAM-ID. NbtDecode-Long.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 TAG              PIC X.
    01 INT8             BINARY-CHAR.
    01 INT16            BINARY-SHORT.
    01 INT32            BINARY-LONG.
    01 INT64            BINARY-LONG-LONG.
LINKAGE SECTION.
    COPY DD-NBT-DECODER REPLACING LEADING ==NBT-DECODER== BY ==LK==.
    01 LK-BUFFER        PIC X ANY LENGTH.
    01 LK-OFFSET        BINARY-LONG UNSIGNED.
    01 LK-VALUE         BINARY-LONG-LONG.

PROCEDURE DIVISION USING LK-STATE LK-BUFFER LK-OFFSET LK-VALUE.
    *> Accept any integer type in the NBT data, and return it as a 64-bit signed integer
    IF LK-OFFSET > LENGTH OF LK-BUFFER
        GOBACK
    END-IF

    *> Read the tag type
    MOVE LK-BUFFER(LK-OFFSET:1) TO TAG
    ADD 1 TO LK-OFFSET

    *> If in a compound, skip the name. The caller will have gotten this using NbtDecode-Peek.
    IF LK-LEVEL > 0 AND LK-STACK-TYPE(LK-LEVEL) = X"0A"
        CALL "NbtDecode-SkipString" USING LK-BUFFER LK-OFFSET
    END-IF

    EVALUATE TAG
        WHEN X"01" *> byte
            CALL "Decode-Byte" USING LK-BUFFER LK-OFFSET INT8
            MOVE INT8 TO LK-VALUE
        WHEN X"02" *> short
            CALL "Decode-Short" USING LK-BUFFER LK-OFFSET INT16
            MOVE INT16 TO LK-VALUE
        WHEN X"03" *> int
            CALL "Decode-Int" USING LK-BUFFER LK-OFFSET INT32
            MOVE INT32 TO LK-VALUE
        WHEN X"04" *> long
            CALL "Decode-Long" USING LK-BUFFER LK-OFFSET INT64
            MOVE INT64 TO LK-VALUE
        WHEN OTHER
            *> TODO handle error
            GOBACK
    END-EVALUATE

    GOBACK.

END PROGRAM NbtDecode-Long.

*> --- NbtDecode-Compound ---
IDENTIFICATION DIVISION.
PROGRAM-ID. NbtDecode-Compound.

DATA DIVISION.
LINKAGE SECTION.
    COPY DD-NBT-DECODER REPLACING LEADING ==NBT-DECODER== BY ==LK==.
    01 LK-BUFFER        PIC X ANY LENGTH.
    01 LK-OFFSET        BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-STATE LK-BUFFER LK-OFFSET.
    *> Read the tag
    IF LK-OFFSET > LENGTH OF LK-BUFFER OR LK-BUFFER(LK-OFFSET:1) NOT = X"0A"
        *> TODO handle error
        GOBACK
    END-IF
    ADD 1 TO LK-OFFSET

    *> If in a compound, skip the name
    IF LK-LEVEL > 0 AND LK-STACK-TYPE(LK-LEVEL) = X"0A"
        CALL "NbtDecode-SkipString" USING LK-BUFFER LK-OFFSET
    END-IF

    *> Push the compound onto the stack
    ADD 1 TO LK-LEVEL
    MOVE X"0A" TO LK-STACK-TYPE(LK-LEVEL)

    GOBACK.

END PROGRAM NbtDecode-Compound.

*> --- NbtDecode-RootCompound ---
IDENTIFICATION DIVISION.
PROGRAM-ID. NbtDecode-RootCompound.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 TAG-EXTENT       BINARY-LONG UNSIGNED.
LINKAGE SECTION.
    COPY DD-NBT-DECODER REPLACING LEADING ==NBT-DECODER== BY ==LK==.
    01 LK-BUFFER        PIC X ANY LENGTH.
    01 LK-OFFSET        BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-STATE LK-BUFFER LK-OFFSET.
    *> The root compound is special because it always has a name (the empty string) even without any wrapping compound.
    *> Since networking code does not use a name for root-level compounds, this needs to be a separate subroutine.
    COMPUTE TAG-EXTENT = LK-OFFSET + 2
    IF TAG-EXTENT > LENGTH OF LK-BUFFER OR LK-BUFFER(LK-OFFSET:1) NOT = X"0A"
        *> TODO handle error
        GOBACK
    END-IF
    ADD 1 TO LK-OFFSET
    IF LK-BUFFER(LK-OFFSET:1) NOT = X"00" OR LK-BUFFER(LK-OFFSET + 1:1) NOT = X"00"
        *> TODO handle error
        GOBACK
    END-IF
    ADD 2 TO LK-OFFSET
    *> Push the compound onto the stack
    ADD 1 TO LK-LEVEL
    MOVE X"0A" TO LK-STACK-TYPE(LK-LEVEL)
    GOBACK.

END PROGRAM NbtDecode-RootCompound.

*> --- NbtDecode-Peek ---
*> Peek at the name of the next tag in the buffer, without advancing the offset.
*> In case the end tag is reached, a flag is set to indicate this.
IDENTIFICATION DIVISION.
PROGRAM-ID. NbtDecode-Peek.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 NAME-OFFSET      BINARY-LONG UNSIGNED.
LINKAGE SECTION.
    COPY DD-NBT-DECODER REPLACING LEADING ==NBT-DECODER== BY ==LK==.
    01 LK-BUFFER        PIC X ANY LENGTH.
    01 LK-OFFSET        BINARY-LONG UNSIGNED.
    01 LK-AT-END        BINARY-CHAR UNSIGNED.
    01 LK-NAME          PIC X ANY LENGTH.
    01 LK-NAME-LENGTH   BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-STATE LK-BUFFER LK-OFFSET LK-AT-END LK-NAME LK-NAME-LENGTH.
    IF LK-OFFSET <= LENGTH OF LK-BUFFER AND LK-BUFFER(LK-OFFSET:1) = X"00"
        MOVE 1 TO LK-AT-END
        MOVE 0 TO LK-NAME-LENGTH
        GOBACK
    END-IF
    MOVE 0 TO LK-AT-END
    COMPUTE NAME-OFFSET = LK-OFFSET + 1
    CALL "NbtDecode-ReadString" USING LK-BUFFER NAME-OFFSET LK-NAME LK-NAME-LENGTH
    GOBACK.

END PROGRAM NbtDecode-Peek.

*> --- NbtDecode-Skip ---
IDENTIFICATION DIVISION.
PROGRAM-ID. NbtDecode-Skip IS RECURSIVE.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 TAG              PIC X.
LINKAGE SECTION.
    COPY DD-NBT-DECODER REPLACING LEADING ==NBT-DECODER== BY ==LK==.
    01 LK-BUFFER        PIC X ANY LENGTH.
    01 LK-OFFSET        BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-STATE LK-BUFFER LK-OFFSET.
    IF LK-OFFSET > LENGTH OF LK-BUFFER
        GOBACK
    END-IF

    *> Read the tag type
    MOVE LK-BUFFER(LK-OFFSET:1) TO TAG
    ADD 1 TO LK-OFFSET

    *> If in a compound, skip the tag name
    IF LK-LEVEL > 0 AND LK-STACK-TYPE(LK-LEVEL) = X"0A"
        CALL "NbtDecode-SkipString" USING LK-BUFFER LK-OFFSET
    END-IF

    *> Skip the value
    CALL "NbtDecode-SkipValue" USING LK-STATE LK-BUFFER LK-OFFSET TAG

    GOBACK.

END PROGRAM NbtDecode-Skip.

*> --- NbtDecode-SkipValue ---
*> Skip just the value of the current tag, where the tag type has already been read.
IDENTIFICATION DIVISION.
PROGRAM-ID. NbtDecode-SkipValue IS RECURSIVE.

DATA DIVISION.
LOCAL-STORAGE SECTION.
    01 INT32            BINARY-LONG.
    01 LIST-TAG         PIC X.
LINKAGE SECTION.
    COPY DD-NBT-DECODER REPLACING LEADING ==NBT-DECODER== BY ==LK==.
    01 LK-BUFFER        PIC X ANY LENGTH.
    01 LK-OFFSET        BINARY-LONG UNSIGNED.
    01 LK-TAG           PIC X.

PROCEDURE DIVISION USING LK-STATE LK-BUFFER LK-OFFSET LK-TAG.
    EVALUATE LK-TAG
        WHEN X"00" *> end
            *> Pop the stack
            SUBTRACT 1 FROM LK-LEVEL
            CONTINUE

        WHEN X"01" *> byte
            ADD 1 TO LK-OFFSET

        WHEN X"02" *> short
            ADD 2 TO LK-OFFSET

        WHEN X"03" *> int
            ADD 4 TO LK-OFFSET

        WHEN X"04" *> long
            ADD 8 TO LK-OFFSET

        WHEN X"05" *> float
            ADD 4 TO LK-OFFSET

        WHEN X"06" *> double
            ADD 8 TO LK-OFFSET

        WHEN X"07" *> byte array
            CALL "Decode-Int" USING LK-BUFFER LK-OFFSET INT32
            ADD INT32 TO LK-OFFSET

        WHEN X"08" *> string
            CALL "NbtDecode-SkipString" USING LK-BUFFER LK-OFFSET

        WHEN X"09" *> list
            *> The first byte of the list is the type of the elements, followed by the length as an int
            MOVE LK-BUFFER(LK-OFFSET:1) TO LIST-TAG
            ADD 1 TO LK-OFFSET
            CALL "Decode-Int" USING LK-BUFFER LK-OFFSET INT32
            IF INT32 <= 0 OR LIST-TAG = X"00"
                GOBACK
            END-IF

            *> Push the list onto the stack
            ADD 1 TO LK-LEVEL
            MOVE LK-TAG TO LK-STACK-TYPE(LK-LEVEL)

            *> Skip the elements
            PERFORM INT32 TIMES
                CALL "NbtDecode-SkipValue" USING LK-STATE LK-BUFFER LK-OFFSET LIST-TAG
            END-PERFORM

            *> Pop the stack
            SUBTRACT 1 FROM LK-LEVEL

        WHEN X"0A" *> compound
            ADD 1 TO LK-LEVEL
            MOVE LK-TAG TO LK-STACK-TYPE(LK-LEVEL)
            PERFORM UNTIL LK-BUFFER(LK-OFFSET:1) = X"00"
                CALL "NbtDecode-Skip" USING LK-STATE LK-BUFFER LK-OFFSET
            END-PERFORM
            CALL "NbtDecode-End" USING LK-STATE LK-BUFFER LK-OFFSET

        WHEN X"0B" *> int array
            CALL "Decode-Int" USING LK-BUFFER LK-OFFSET INT32
            COMPUTE LK-OFFSET = LK-OFFSET + INT32 * 4

        WHEN X"0C" *> long array
            CALL "Decode-Int" USING LK-BUFFER LK-OFFSET INT32
            COMPUTE LK-OFFSET = LK-OFFSET + INT32 * 8

        WHEN OTHER
            *> TODO handle error
            GOBACK
    END-EVALUATE

    GOBACK.

END PROGRAM NbtDecode-SkipValue.
