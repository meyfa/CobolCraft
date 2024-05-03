*> --- Test: nbt-decode.cob ---
IDENTIFICATION DIVISION.
PROGRAM-ID. Test-NbtDecode.

PROCEDURE DIVISION.
    DISPLAY "Test: nbt-decode.cob"
    CALL "Test-NbtDecode-Long"
    CALL "Test-NbtDecode-Compound"
    CALL "Test-NbtDecode-RootCompound"
    CALL "Test-NbtDecode-Peek"
    CALL "Test-NbtDecode-Skip"
    GOBACK.

    *> --- Test: Test-NbtDecode-Long ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Test-NbtDecode-Long.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        COPY DD-NBT-DECODER.
        01 BUFFER       PIC X(16).
        01 OFFSET       BINARY-LONG UNSIGNED.
        01 RESULT       BINARY-LONG-LONG UNSIGNED.

    PROCEDURE DIVISION.
        DISPLAY "  Test: NbtDecode-Long".
    BasicLong.
        DISPLAY "    Case: root level, long tag - " WITH NO ADVANCING
        MOVE 0 TO NBT-DECODER-LEVEL
        MOVE X"0000040000000000000102FFFFFFFFFF" TO BUFFER
        MOVE 3 TO OFFSET
        CALL "NbtDecode-Long" USING NBT-DECODER-STATE BUFFER OFFSET RESULT
        IF OFFSET = 12 AND RESULT = H'0102'
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    TagByte.
        DISPLAY "    Case: root level, byte tag - " WITH NO ADVANCING
        MOVE 0 TO NBT-DECODER-LEVEL
        MOVE X"00000142FFFFFFFFFFFFFFFFFFFFFFFF" TO BUFFER
        MOVE 3 TO OFFSET
        CALL "NbtDecode-Long" USING NBT-DECODER-STATE BUFFER OFFSET RESULT
        IF OFFSET = 5 AND RESULT = H'42'
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    TagShort.
        DISPLAY "    Case: root level, short tag - " WITH NO ADVANCING
        MOVE 0 TO NBT-DECODER-LEVEL
        MOVE X"0000021234FFFFFFFFFFFFFFFFFFFFFF" TO BUFFER
        MOVE 3 TO OFFSET
        CALL "NbtDecode-Long" USING NBT-DECODER-STATE BUFFER OFFSET RESULT
        IF OFFSET = 6 AND RESULT = H'1234'
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    TagInt.
        DISPLAY "    Case: root level, int tag - " WITH NO ADVANCING
        MOVE 0 TO NBT-DECODER-LEVEL
        MOVE X"00000301020304FFFFFFFFFFFFFFFFFF" TO BUFFER
        MOVE 3 TO OFFSET
        CALL "NbtDecode-Long" USING NBT-DECODER-STATE BUFFER OFFSET RESULT
        IF OFFSET = 8 AND RESULT = H'01020304'
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    WithinCompound.
        DISPLAY "    Case: within compound, empty name - " WITH NO ADVANCING
        MOVE 0 TO NBT-DECODER-LEVEL
        MOVE X"0A040000000000000000020300FFFFFF" TO BUFFER
        MOVE 1 TO OFFSET
        CALL "NbtDecode-Compound" USING NBT-DECODER-STATE BUFFER OFFSET
        CALL "NbtDecode-Long" USING NBT-DECODER-STATE BUFFER OFFSET RESULT
        IF OFFSET = 13 AND RESULT = H'0203'
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.

        GOBACK.

    END PROGRAM Test-NbtDecode-Long.

    *> --- Test: Test-NbtDecode-Compound ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Test-NbtDecode-Compound.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        COPY DD-NBT-DECODER.
        01 BUFFER       PIC X(12).
        01 OFFSET       BINARY-LONG UNSIGNED.

    PROCEDURE DIVISION.
        DISPLAY "  Test: NbtDecode-Compound".
    Basic.
        DISPLAY "    Case: root level - " WITH NO ADVANCING
        MOVE 0 TO NBT-DECODER-LEVEL
        MOVE X"00000A000400FFFFFFFFFFFF" TO BUFFER
        MOVE 3 TO OFFSET
        CALL "NbtDecode-Compound" USING NBT-DECODER-STATE BUFFER OFFSET
        IF OFFSET = 4 AND NBT-DECODER-LEVEL = 1 AND NBT-DECODER-STACK-TYPE(1) = X"0A"
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    NestedCompound.
        DISPLAY "    Case: nested compound - " WITH NO ADVANCING
        MOVE 0 TO NBT-DECODER-LEVEL
        MOVE X"00000A0A000444617461FFFF" TO BUFFER
        MOVE 3 TO OFFSET
        CALL "NbtDecode-Compound" USING NBT-DECODER-STATE BUFFER OFFSET
        CALL "NbtDecode-Compound" USING NBT-DECODER-STATE BUFFER OFFSET
        IF OFFSET = 11 AND NBT-DECODER-LEVEL = 2 AND NBT-DECODER-STACK-TYPE(2) = X"0A"
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.

        GOBACK.

    END PROGRAM Test-NbtDecode-Compound.

    *> --- Test: Test-NbtDecode-RootCompound ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Test-NbtDecode-RootCompound.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        COPY DD-NBT-DECODER.
        01 BUFFER       PIC X(12).
        01 OFFSET       BINARY-LONG UNSIGNED.

    PROCEDURE DIVISION.
        DISPLAY "  Test: NbtDecode-RootCompound".
    Basic.
        DISPLAY "    Case: root level, empty name - " WITH NO ADVANCING
        MOVE 0 TO NBT-DECODER-LEVEL
        MOVE X"00000A000000FFFFFFFFFFFF" TO BUFFER
        MOVE 3 TO OFFSET
        CALL "NbtDecode-RootCompound" USING NBT-DECODER-STATE BUFFER OFFSET
        IF OFFSET = 6 AND NBT-DECODER-LEVEL = 1 AND NBT-DECODER-STACK-TYPE(1) = X"0A"
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.

        GOBACK.

    END PROGRAM Test-NbtDecode-RootCompound.

    *> --- Test: Test-NbtDecode-Peek ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Test-NbtDecode-Peek.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        COPY DD-NBT-DECODER.
        01 BUFFER       PIC X(12).
        01 OFFSET       BINARY-LONG UNSIGNED.
        01 AT-END       BINARY-CHAR UNSIGNED.
        01 NAME-VALUE   PIC X(12).
        01 NAME-LEN     BINARY-LONG UNSIGNED.

    PROCEDURE DIVISION.
        DISPLAY "  Test: NbtDecode-Peek".
    AtEnd.
        DISPLAY "    Case: at end - " WITH NO ADVANCING
        MOVE 0 TO NBT-DECODER-LEVEL
        MOVE X"0A010004446174614200FFFF" TO BUFFER
        MOVE 10 TO OFFSET
        MOVE 0 TO AT-END
        CALL "NbtDecode-Peek" USING NBT-DECODER-STATE BUFFER OFFSET AT-END NAME-VALUE NAME-LEN
        IF OFFSET = 10 AND AT-END = 1
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    NotAtEnd.
        DISPLAY "    Case: not at end - " WITH NO ADVANCING
        MOVE 0 TO NBT-DECODER-LEVEL
        MOVE X"0A010004446174614200FFFF" TO BUFFER
        MOVE 2 TO OFFSET
        MOVE 1 TO AT-END
        MOVE "failure" TO NAME-VALUE
        MOVE 7 TO NAME-LEN
        CALL "NbtDecode-Peek" USING NBT-DECODER-STATE BUFFER OFFSET AT-END NAME-VALUE NAME-LEN
        IF OFFSET = 2 AND AT-END = 0 AND NAME-VALUE = "Data" AND NAME-LEN = 4
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.

        GOBACK.

    END PROGRAM Test-NbtDecode-Peek.

    *> --- Test: Test-NbtDecode-Skip ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Test-NbtDecode-Skip.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        COPY DD-NBT-DECODER.
        01 BUFFER       PIC X(16).
        01 OFFSET       BINARY-LONG UNSIGNED.

    PROCEDURE DIVISION.
        DISPLAY "  Test: NbtDecode-Skip".
    Basic.
        DISPLAY "    Case: root level, int tag - " WITH NO ADVANCING
        MOVE 0 TO NBT-DECODER-LEVEL
        MOVE X"00000311223344FFFFFFFFFFFFFFFFFF" TO BUFFER
        MOVE 3 TO OFFSET
        CALL "NbtDecode-Skip" USING NBT-DECODER-STATE BUFFER OFFSET
        IF OFFSET = 8
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    WithinCompound.
        DISPLAY "    Case: within compound, named long tag - " WITH NO ADVANCING
        MOVE 0 TO NBT-DECODER-LEVEL
        MOVE X"0A04000454696D650102030405060708" TO BUFFER
        MOVE 1 TO OFFSET
        CALL "NbtDecode-Compound" USING NBT-DECODER-STATE BUFFER OFFSET
        CALL "NbtDecode-Skip" USING NBT-DECODER-STATE BUFFER OFFSET
        IF OFFSET = 17
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    ByteArray.
        DISPLAY "    Case: byte array - " WITH NO ADVANCING
        MOVE 0 TO NBT-DECODER-LEVEL
        MOVE X"00000700000003010203FFFFFFFFFFFF" TO BUFFER
        MOVE 3 TO OFFSET
        CALL "NbtDecode-Skip" USING NBT-DECODER-STATE BUFFER OFFSET
        IF OFFSET = 11
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    IntArray.
        DISPLAY "    Case: int array - " WITH NO ADVANCING
        MOVE 0 TO NBT-DECODER-LEVEL
        MOVE X"00000B000000020000000800000009FF" TO BUFFER
        MOVE 3 TO OFFSET
        CALL "NbtDecode-Skip" USING NBT-DECODER-STATE BUFFER OFFSET
        IF OFFSET = 16
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    LongArray.
        DISPLAY "    Case: long array - " WITH NO ADVANCING
        MOVE 0 TO NBT-DECODER-LEVEL
        MOVE X"00000C000000010000000000000042FF" TO BUFFER
        MOVE 3 TO OFFSET
        CALL "NbtDecode-Skip" USING NBT-DECODER-STATE BUFFER OFFSET
        IF OFFSET = 16
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    StringTag.
        DISPLAY "    Case: string tag - " WITH NO ADVANCING
        MOVE 0 TO NBT-DECODER-LEVEL
        MOVE X"000008000454657374FFFFFFFFFFFFFF" TO BUFFER
        MOVE 3 TO OFFSET
        CALL "NbtDecode-Skip" USING NBT-DECODER-STATE BUFFER OFFSET
        IF OFFSET = 10
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    List.
        DISPLAY "    Case: list (short) - " WITH NO ADVANCING
        MOVE 0 TO NBT-DECODER-LEVEL
        MOVE X"000009020000000200010002FFFFFFFF" TO BUFFER
        MOVE 3 TO OFFSET
        CALL "NbtDecode-Skip" USING NBT-DECODER-STATE BUFFER OFFSET
        IF OFFSET = 13 AND NBT-DECODER-LEVEL = 0
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL, offset: " OFFSET
        END-IF.
    EmptyList.
        DISPLAY "    Case: empty list - " WITH NO ADVANCING
        MOVE 0 TO NBT-DECODER-LEVEL
        MOVE X"0000090000000000FFFFFFFFFFFFFFFF" TO BUFFER
        MOVE 3 TO OFFSET
        CALL "NbtDecode-Skip" USING NBT-DECODER-STATE BUFFER OFFSET
        IF OFFSET = 9 AND NBT-DECODER-LEVEL = 0
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    NegativeLengthList.
        DISPLAY "    Case: negative length list (int) - " WITH NO ADVANCING
        MOVE 0 TO NBT-DECODER-LEVEL
        MOVE X"00000903FFFFFFF1FFFFFFFFFFFFFFFF" TO BUFFER
        MOVE 3 TO OFFSET
        CALL "NbtDecode-Skip" USING NBT-DECODER-STATE BUFFER OFFSET
        IF OFFSET = 9 AND NBT-DECODER-LEVEL = 0
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    EmptyCompound.
        DISPLAY "    Case: empty compound - " WITH NO ADVANCING
        MOVE 0 TO NBT-DECODER-LEVEL
        MOVE X"00000A00FFFFFFFFFFFFFFFFFFFFFFFF" TO BUFFER
        MOVE 3 TO OFFSET
        CALL "NbtDecode-Skip" USING NBT-DECODER-STATE BUFFER OFFSET
        IF OFFSET = 5 AND NBT-DECODER-LEVEL = 0
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    NestedCompound.
        DISPLAY "    Case: nested compound - " WITH NO ADVANCING
        MOVE 0 TO NBT-DECODER-LEVEL
        MOVE X"00000A0A0001440A000157000000FFFF" TO BUFFER
        MOVE 3 TO OFFSET
        CALL "NbtDecode-Skip" USING NBT-DECODER-STATE BUFFER OFFSET
        IF OFFSET = 15 AND NBT-DECODER-LEVEL = 0
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.

        GOBACK.

    END PROGRAM Test-NbtDecode-Skip.

END PROGRAM Test-NbtDecode.
