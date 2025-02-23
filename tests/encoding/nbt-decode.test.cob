*> --- Test: nbt-decode.cob ---
IDENTIFICATION DIVISION.
PROGRAM-ID. Test-NbtDecode.

PROCEDURE DIVISION.
    DISPLAY "Test: nbt-decode.cob"
    CALL "Test-NbtDecode-Byte"
    CALL "Test-NbtDecode-Int"
    CALL "Test-NbtDecode-Short"
    CALL "Test-NbtDecode-Long"
    CALL "Test-NbtDecode-Float"
    CALL "Test-NbtDecode-Double"
    CALL "Test-NbtDecode-String"
    CALL "Test-NbtDecode-List"
    CALL "Test-NbtDecode-Compound"
    CALL "Test-NbtDecode-RootCompound"
    CALL "Test-NbtDecode-EndCompound"
    CALL "Test-NbtDecode-Peek"
    CALL "Test-NbtDecode-Skip"
    CALL "Test-NbtDecode-UUID"
    GOBACK.

    *> --- Test: Test-NbtDecode-Byte ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Test-NbtDecode-Byte.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        COPY DD-NBT-DECODER.
        01 BUFFER       PIC X(16).
        01 RESULT       BINARY-CHAR.

    PROCEDURE DIVISION.
        DISPLAY "  Test: NbtDecode-Byte".
    TagByte.
        DISPLAY "    Case: from byte tag - " WITH NO ADVANCING
        INITIALIZE NBTDEC
        MOVE X"0000012AFFFFFFFFFFFFFFFFFFFFFFFF" TO BUFFER
        MOVE 3 TO NBTDEC-OFFSET
        CALL "NbtDecode-Byte" USING NBTDEC BUFFER RESULT
        IF NBTDEC-OFFSET = 5 AND RESULT = 42
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    TagShort.
        DISPLAY "    Case: from short tag - " WITH NO ADVANCING
        INITIALIZE NBTDEC
        MOVE X"000002002AFFFFFFFFFFFFFFFFFFFFFF" TO BUFFER
        MOVE 3 TO NBTDEC-OFFSET
        CALL "NbtDecode-Byte" USING NBTDEC BUFFER RESULT
        IF NBTDEC-OFFSET = 6 AND RESULT = 42
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    TagInt.
        DISPLAY "    Case: from int tag - " WITH NO ADVANCING
        INITIALIZE NBTDEC
        MOVE X"0000030000002AFFFFFFFFFFFFFFFFFF" TO BUFFER
        MOVE 3 TO NBTDEC-OFFSET
        CALL "NbtDecode-Byte" USING NBTDEC BUFFER RESULT
        IF NBTDEC-OFFSET = 8 AND RESULT = 42
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    TagLong.
        DISPLAY "    Case: from long tag - " WITH NO ADVANCING
        INITIALIZE NBTDEC
        MOVE X"000004000000000000002AFFFFFFFFFF" TO BUFFER
        MOVE 3 TO NBTDEC-OFFSET
        CALL "NbtDecode-Byte" USING NBTDEC BUFFER RESULT
        IF NBTDEC-OFFSET = 12 AND RESULT = 42
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.

        GOBACK.

    END PROGRAM Test-NbtDecode-Byte.

    *> --- Test: Test-NbtDecode-Short ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Test-NbtDecode-Short.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        COPY DD-NBT-DECODER.
        01 BUFFER       PIC X(16).
        01 RESULT       BINARY-SHORT.

    PROCEDURE DIVISION.
        DISPLAY "  Test: NbtDecode-Short".
    TagByte.
        DISPLAY "    Case: from byte tag - " WITH NO ADVANCING
        INITIALIZE NBTDEC
        MOVE X"0000012AFFFFFFFFFFFFFFFFFFFFFFFF" TO BUFFER
        MOVE 3 TO NBTDEC-OFFSET
        CALL "NbtDecode-Short" USING NBTDEC BUFFER RESULT
        IF NBTDEC-OFFSET = 5 AND RESULT = 42
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    TagShort.
        DISPLAY "    Case: from short tag - " WITH NO ADVANCING
        INITIALIZE NBTDEC
        MOVE X"000002002AFFFFFFFFFFFFFFFFFFFFFF" TO BUFFER
        MOVE 3 TO NBTDEC-OFFSET
        CALL "NbtDecode-Short" USING NBTDEC BUFFER RESULT
        IF NBTDEC-OFFSET = 6 AND RESULT = 42
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    TagInt.
        DISPLAY "    Case: from int tag - " WITH NO ADVANCING
        INITIALIZE NBTDEC
        MOVE X"0000030000002AFFFFFFFFFFFFFFFFFF" TO BUFFER
        MOVE 3 TO NBTDEC-OFFSET
        CALL "NbtDecode-Short" USING NBTDEC BUFFER RESULT
        IF NBTDEC-OFFSET = 8 AND RESULT = 42
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    TagLong.
        DISPLAY "    Case: from long tag - " WITH NO ADVANCING
        INITIALIZE NBTDEC
        MOVE X"000004000000000000002AFFFFFFFFFF" TO BUFFER
        MOVE 3 TO NBTDEC-OFFSET
        CALL "NbtDecode-Short" USING NBTDEC BUFFER RESULT
        IF NBTDEC-OFFSET = 12 AND RESULT = 42
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.

        GOBACK.

    END PROGRAM Test-NbtDecode-Short.

    *> --- Test: Test-NbtDecode-Int ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Test-NbtDecode-Int.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        COPY DD-NBT-DECODER.
        01 BUFFER       PIC X(16).
        01 RESULT       BINARY-LONG.

    PROCEDURE DIVISION.
        DISPLAY "  Test: NbtDecode-Int".
    TagByte.
        DISPLAY "    Case: from byte tag - " WITH NO ADVANCING
        INITIALIZE NBTDEC
        MOVE X"0000012AFFFFFFFFFFFFFFFFFFFFFFFF" TO BUFFER
        MOVE 3 TO NBTDEC-OFFSET
        CALL "NbtDecode-Int" USING NBTDEC BUFFER RESULT
        IF NBTDEC-OFFSET = 5 AND RESULT = 42
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    TagShort.
        DISPLAY "    Case: from short tag - " WITH NO ADVANCING
        INITIALIZE NBTDEC
        MOVE X"000002002AFFFFFFFFFFFFFFFFFFFFFF" TO BUFFER
        MOVE 3 TO NBTDEC-OFFSET
        CALL "NbtDecode-Int" USING NBTDEC BUFFER RESULT
        IF NBTDEC-OFFSET = 6 AND RESULT = 42
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    TagInt.
        DISPLAY "    Case: from int tag - " WITH NO ADVANCING
        INITIALIZE NBTDEC
        MOVE X"0000030000002AFFFFFFFFFFFFFFFFFF" TO BUFFER
        MOVE 3 TO NBTDEC-OFFSET
        CALL "NbtDecode-Int" USING NBTDEC BUFFER RESULT
        IF NBTDEC-OFFSET = 8 AND RESULT = 42
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    TagLong.
        DISPLAY "    Case: from long tag - " WITH NO ADVANCING
        INITIALIZE NBTDEC
        MOVE X"000004000000000000002AFFFFFFFFFF" TO BUFFER
        MOVE 3 TO NBTDEC-OFFSET
        CALL "NbtDecode-Int" USING NBTDEC BUFFER RESULT
        IF NBTDEC-OFFSET = 12 AND RESULT = 42
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.

        GOBACK.

    END PROGRAM Test-NbtDecode-Int.

    *> --- Test: Test-NbtDecode-Long ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Test-NbtDecode-Long.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        COPY DD-NBT-DECODER.
        01 BUFFER       PIC X(16).
        01 RESULT       BINARY-LONG-LONG.

    PROCEDURE DIVISION.
        DISPLAY "  Test: NbtDecode-Long".
    BasicLong.
        DISPLAY "    Case: root level, long tag - " WITH NO ADVANCING
        INITIALIZE NBTDEC
        MOVE X"0000040000000000000102FFFFFFFFFF" TO BUFFER
        MOVE 3 TO NBTDEC-OFFSET
        CALL "NbtDecode-Long" USING NBTDEC BUFFER RESULT
        IF NBTDEC-OFFSET = 12 AND RESULT = H'0102'
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    TagByte.
        DISPLAY "    Case: root level, byte tag - " WITH NO ADVANCING
        INITIALIZE NBTDEC
        MOVE X"00000142FFFFFFFFFFFFFFFFFFFFFFFF" TO BUFFER
        MOVE 3 TO NBTDEC-OFFSET
        CALL "NbtDecode-Long" USING NBTDEC BUFFER RESULT
        IF NBTDEC-OFFSET = 5 AND RESULT = H'42'
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    TagShort.
        DISPLAY "    Case: root level, short tag - " WITH NO ADVANCING
        INITIALIZE NBTDEC
        MOVE X"0000021234FFFFFFFFFFFFFFFFFFFFFF" TO BUFFER
        MOVE 3 TO NBTDEC-OFFSET
        CALL "NbtDecode-Long" USING NBTDEC BUFFER RESULT
        IF NBTDEC-OFFSET = 6 AND RESULT = H'1234'
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    TagInt.
        DISPLAY "    Case: root level, int tag - " WITH NO ADVANCING
        INITIALIZE NBTDEC
        MOVE X"00000301020304FFFFFFFFFFFFFFFFFF" TO BUFFER
        MOVE 3 TO NBTDEC-OFFSET
        CALL "NbtDecode-Long" USING NBTDEC BUFFER RESULT
        IF NBTDEC-OFFSET = 8 AND RESULT = H'01020304'
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    WithinCompound.
        DISPLAY "    Case: within compound, empty name - " WITH NO ADVANCING
        INITIALIZE NBTDEC
        MOVE X"0A040000000000000000020300FFFFFF" TO BUFFER
        MOVE 1 TO NBTDEC-OFFSET
        CALL "NbtDecode-Compound" USING NBTDEC BUFFER
        CALL "NbtDecode-Long" USING NBTDEC BUFFER RESULT
        IF NBTDEC-OFFSET = 13 AND RESULT = H'0203'
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.

        GOBACK.

    END PROGRAM Test-NbtDecode-Long.

    *> --- Test: Test-NbtDecode-Float ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Test-NbtDecode-Float.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        COPY DD-NBT-DECODER.
        01 BUFFER       PIC X(16).
        01 RESULT       FLOAT-SHORT.

    PROCEDURE DIVISION.
        DISPLAY "  Test: NbtDecode-Float".
    FromFloat.
        DISPLAY "    Case: from float tag - " WITH NO ADVANCING
        INITIALIZE NBTDEC
        MOVE X"00000541420000FFFFFFFFFFFFFFFFFF" TO BUFFER
        MOVE 3 TO NBTDEC-OFFSET
        CALL "NbtDecode-Float" USING NBTDEC BUFFER RESULT
        IF NBTDEC-OFFSET = 8 AND RESULT = 12.125
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    FromDouble.
        DISPLAY "    Case: from double tag - " WITH NO ADVANCING
        INITIALIZE NBTDEC
        MOVE X"0000064028400000000000FFFFFFFFFF" TO BUFFER
        MOVE 3 TO NBTDEC-OFFSET
        CALL "NbtDecode-Float" USING NBTDEC BUFFER RESULT
        IF NBTDEC-OFFSET = 12 AND RESULT = 12.125
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.

        GOBACK.

    END PROGRAM Test-NbtDecode-Float.

    *> --- Test: Test-NbtDecode-Double ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Test-NbtDecode-Double.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        COPY DD-NBT-DECODER.
        01 BUFFER       PIC X(16).
        01 RESULT       FLOAT-LONG.

    PROCEDURE DIVISION.
        DISPLAY "  Test: NbtDecode-Double".
    FromFloat.
        DISPLAY "    Case: from float tag - " WITH NO ADVANCING
        INITIALIZE NBTDEC
        MOVE X"00000541420000FFFFFFFFFFFFFFFFFF" TO BUFFER
        MOVE 3 TO NBTDEC-OFFSET
        CALL "NbtDecode-Double" USING NBTDEC BUFFER RESULT
        IF NBTDEC-OFFSET = 8 AND RESULT = 12.125
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    FromDouble.
        DISPLAY "    Case: from double tag - " WITH NO ADVANCING
        INITIALIZE NBTDEC
        MOVE X"0000064028400000000000FFFFFFFFFF" TO BUFFER
        MOVE 3 TO NBTDEC-OFFSET
        CALL "NbtDecode-Double" USING NBTDEC BUFFER RESULT
        IF NBTDEC-OFFSET = 12 AND RESULT = 12.125
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.

        GOBACK.

    END PROGRAM Test-NbtDecode-Double.

    *> --- Test: Test-NbtDecode-String ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Test-NbtDecode-String.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        COPY DD-NBT-DECODER.
        01 BUFFER       PIC X(16).
        01 STR          PIC X(12).
        01 STR-LEN      BINARY-LONG UNSIGNED.

    PROCEDURE DIVISION.
        DISPLAY "  Test: NbtDecode-String".
    EmptyString.
        DISPLAY "    Case: empty string - " WITH NO ADVANCING
        INITIALIZE NBTDEC
        MOVE X"0000080000FFFFFFFFFFFFFFFFFFFFFF" TO BUFFER
        MOVE 3 TO NBTDEC-OFFSET
        CALL "NbtDecode-String" USING NBTDEC BUFFER STR STR-LEN
        IF NBTDEC-OFFSET = 6 AND STR-LEN = 0
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    NonEmptyString.
        DISPLAY "    Case: non-empty string - " WITH NO ADVANCING
        INITIALIZE NBTDEC
        MOVE X"000008000454657374FFFFFFFFFFFFFF" TO BUFFER
        MOVE 3 TO NBTDEC-OFFSET
        CALL "NbtDecode-String" USING NBTDEC BUFFER STR STR-LEN
        IF NBTDEC-OFFSET = 10 AND STR-LEN = 4 AND STR = "Test"
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.

        GOBACK.

    END PROGRAM Test-NbtDecode-String.

    *> --- Test: Test-NbtDecode-List ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Test-NbtDecode-List.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        COPY DD-NBT-DECODER.
        01 BUFFER       PIC X(16).
        01 LIST-LENGTH  BINARY-LONG UNSIGNED.

    PROCEDURE DIVISION.
        DISPLAY "  Test: NbtDecode-List".
    EmptyList.
        DISPLAY "    Case: empty list (type=end) - " WITH NO ADVANCING
        INITIALIZE NBTDEC
        MOVE X"0000090000000000FFFFFFFFFFFFFFFF" TO BUFFER
        MOVE 3 TO NBTDEC-OFFSET
        CALL "NbtDecode-List" USING NBTDEC BUFFER LIST-LENGTH
        IF NBTDEC-OFFSET = 9 AND LIST-LENGTH = 0 AND NBTDEC-LEVEL = 1 AND NBTDEC-STACK-TYPE(1) = X"09" AND NBTDEC-STACK-LIST-TYPE(1) = X"00"
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    EmptyByteArray.
        DISPLAY "    Case: empty byte array - " WITH NO ADVANCING
        INITIALIZE NBTDEC
        MOVE X"00000700000000FFFFFFFFFFFFFFFFFF" TO BUFFER
        MOVE 3 TO NBTDEC-OFFSET
        CALL "NbtDecode-List" USING NBTDEC BUFFER LIST-LENGTH
        IF NBTDEC-OFFSET = 8 AND LIST-LENGTH = 0 AND NBTDEC-LEVEL = 1 AND NBTDEC-STACK-TYPE(1) = X"07" AND NBTDEC-STACK-LIST-TYPE(1) = X"01"
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    EmptyIntArray.
        DISPLAY "    Case: empty int array - " WITH NO ADVANCING
        INITIALIZE NBTDEC
        MOVE X"00000B00000000FFFFFFFFFFFFFFFFFF" TO BUFFER
        MOVE 3 TO NBTDEC-OFFSET
        CALL "NbtDecode-List" USING NBTDEC BUFFER LIST-LENGTH
        IF NBTDEC-OFFSET = 8 AND LIST-LENGTH = 0 AND NBTDEC-LEVEL = 1 AND NBTDEC-STACK-TYPE(1) = X"0B" AND NBTDEC-STACK-LIST-TYPE(1) = X"03"
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    EmptyLongArray.
        DISPLAY "    Case: empty long array - " WITH NO ADVANCING
        INITIALIZE NBTDEC
        MOVE X"00000C00000000FFFFFFFFFFFFFFFFFF" TO BUFFER
        MOVE 3 TO NBTDEC-OFFSET
        CALL "NbtDecode-List" USING NBTDEC BUFFER LIST-LENGTH
        IF NBTDEC-OFFSET = 8 AND LIST-LENGTH = 0 AND NBTDEC-LEVEL = 1 AND NBTDEC-STACK-TYPE(1) = X"0C" AND NBTDEC-STACK-LIST-TYPE(1) = X"04"
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    NonEmptyList.
        DISPLAY "    Case: non-empty list - " WITH NO ADVANCING
        INITIALIZE NBTDEC
        MOVE X"000009020000000200010002FFFFFFFF" TO BUFFER
        MOVE 3 TO NBTDEC-OFFSET
        CALL "NbtDecode-List" USING NBTDEC BUFFER LIST-LENGTH
        IF NBTDEC-OFFSET = 9 AND LIST-LENGTH = 2 AND NBTDEC-LEVEL = 1 AND NBTDEC-STACK-TYPE(1) = X"09" AND NBTDEC-STACK-LIST-TYPE(1) = X"02"
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    NonEmptyByteArray.
        DISPLAY "    Case: non-empty byte array - " WITH NO ADVANCING
        INITIALIZE NBTDEC
        MOVE X"00000700000003010203FFFFFFFFFFFF" TO BUFFER
        MOVE 3 TO NBTDEC-OFFSET
        CALL "NbtDecode-List" USING NBTDEC BUFFER LIST-LENGTH
        IF NBTDEC-OFFSET = 8 AND LIST-LENGTH = 3 AND NBTDEC-LEVEL = 1 AND NBTDEC-STACK-TYPE(1) = X"07" AND NBTDEC-STACK-LIST-TYPE(1) = X"01"
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    NonEmptyIntArray.
        DISPLAY "    Case: non-empty int array - " WITH NO ADVANCING
        INITIALIZE NBTDEC
        MOVE X"00000B000000020000000100000002FF" TO BUFFER
        MOVE 3 TO NBTDEC-OFFSET
        CALL "NbtDecode-List" USING NBTDEC BUFFER LIST-LENGTH
        IF NBTDEC-OFFSET = 8 AND LIST-LENGTH = 2 AND NBTDEC-LEVEL = 1 AND NBTDEC-STACK-TYPE(1) = X"0B" AND NBTDEC-STACK-LIST-TYPE(1) = X"03"
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    NonEmptyLongArray.
        DISPLAY "    Case: non-empty long array - " WITH NO ADVANCING
        INITIALIZE NBTDEC
        MOVE X"00000C000000010000000000000001FF" TO BUFFER
        MOVE 3 TO NBTDEC-OFFSET
        CALL "NbtDecode-List" USING NBTDEC BUFFER LIST-LENGTH
        IF NBTDEC-OFFSET = 8 AND LIST-LENGTH = 1 AND NBTDEC-LEVEL = 1 AND NBTDEC-STACK-TYPE(1) = X"0C" AND NBTDEC-STACK-LIST-TYPE(1) = X"04"
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.

        GOBACK.

    END PROGRAM Test-NbtDecode-List.

    *> --- Test: Test-NbtDecode-Compound ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Test-NbtDecode-Compound.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        COPY DD-NBT-DECODER.
        01 BUFFER       PIC X(12).

    PROCEDURE DIVISION.
        DISPLAY "  Test: NbtDecode-Compound".
    Basic.
        DISPLAY "    Case: root level - " WITH NO ADVANCING
        INITIALIZE NBTDEC
        MOVE X"00000A000400FFFFFFFFFFFF" TO BUFFER
        MOVE 3 TO NBTDEC-OFFSET
        CALL "NbtDecode-Compound" USING NBTDEC BUFFER
        IF NBTDEC-OFFSET = 4 AND NBTDEC-LEVEL = 1 AND NBTDEC-STACK-TYPE(1) = X"0A"
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    NestedCompound.
        DISPLAY "    Case: nested compound - " WITH NO ADVANCING
        INITIALIZE NBTDEC
        MOVE X"00000A0A000444617461FFFF" TO BUFFER
        MOVE 3 TO NBTDEC-OFFSET
        CALL "NbtDecode-Compound" USING NBTDEC BUFFER
        CALL "NbtDecode-Compound" USING NBTDEC BUFFER
        IF NBTDEC-OFFSET = 11 AND NBTDEC-LEVEL = 2 AND NBTDEC-STACK-TYPE(2) = X"0A"
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

    PROCEDURE DIVISION.
        DISPLAY "  Test: NbtDecode-RootCompound".
    Basic.
        DISPLAY "    Case: root level, empty name - " WITH NO ADVANCING
        INITIALIZE NBTDEC
        MOVE X"00000A000000FFFFFFFFFFFF" TO BUFFER
        MOVE 3 TO NBTDEC-OFFSET
        CALL "NbtDecode-RootCompound" USING NBTDEC BUFFER
        IF NBTDEC-OFFSET = 6 AND NBTDEC-LEVEL = 1 AND NBTDEC-STACK-TYPE(1) = X"0A"
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.

        GOBACK.

    END PROGRAM Test-NbtDecode-RootCompound.

    *> --- Test: Test-NbtDecode-EndCompound ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Test-NbtDecode-EndCompound.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        COPY DD-NBT-DECODER.
        01 BUFFER       PIC X(12).

    PROCEDURE DIVISION.
        DISPLAY "  Test: NbtDecode-EndCompound".
    Basic.
        DISPLAY "    Case: basic - " WITH NO ADVANCING
        INITIALIZE NBTDEC
        MOVE X"00000A0A000000FFFFFFFFFF" TO BUFFER
        MOVE 3 TO NBTDEC-OFFSET
        CALL "NbtDecode-Compound" USING NBTDEC BUFFER
        CALL "NbtDecode-Compound" USING NBTDEC BUFFER
        CALL "NbtDecode-EndCompound" USING NBTDEC BUFFER
        IF NBTDEC-OFFSET = 8 AND NBTDEC-LEVEL = 1
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.

        GOBACK.

    END PROGRAM Test-NbtDecode-EndCompound.

    *> --- Test: Test-NbtDecode-Peek ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Test-NbtDecode-Peek.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        COPY DD-NBT-DECODER.
        01 BUFFER       PIC X(12).
        01 AT-END       BINARY-CHAR UNSIGNED.
        01 NAME-VALUE   PIC X(12).

    PROCEDURE DIVISION.
        DISPLAY "  Test: NbtDecode-Peek".
    AtEnd.
        DISPLAY "    Case: at end - " WITH NO ADVANCING
        INITIALIZE NBTDEC
        MOVE X"0A010004446174614200FFFF" TO BUFFER
        MOVE 10 TO NBTDEC-OFFSET
        MOVE 0 TO AT-END
        CALL "NbtDecode-Peek" USING NBTDEC BUFFER AT-END NAME-VALUE
        IF NBTDEC-OFFSET = 10 AND AT-END = 1
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    NotAtEnd.
        DISPLAY "    Case: not at end - " WITH NO ADVANCING
        INITIALIZE NBTDEC
        MOVE X"0A010004446174614200FFFF" TO BUFFER
        MOVE 2 TO NBTDEC-OFFSET
        MOVE 1 TO AT-END
        MOVE "failure" TO NAME-VALUE
        CALL "NbtDecode-Peek" USING NBTDEC BUFFER AT-END NAME-VALUE
        IF NBTDEC-OFFSET = 2 AND AT-END = 0 AND NAME-VALUE = "Data"
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
        01 LIST-LENGTH  BINARY-LONG UNSIGNED.

    PROCEDURE DIVISION.
        DISPLAY "  Test: NbtDecode-Skip".
    Basic.
        DISPLAY "    Case: root level, int tag - " WITH NO ADVANCING
        INITIALIZE NBTDEC
        MOVE X"00000311223344FFFFFFFFFFFFFFFFFF" TO BUFFER
        MOVE 3 TO NBTDEC-OFFSET
        CALL "NbtDecode-Skip" USING NBTDEC BUFFER
        IF NBTDEC-OFFSET = 8
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    WithinCompound.
        DISPLAY "    Case: within compound, named long tag - " WITH NO ADVANCING
        INITIALIZE NBTDEC
        MOVE X"0A04000454696D650102030405060708" TO BUFFER
        MOVE 1 TO NBTDEC-OFFSET
        CALL "NbtDecode-Compound" USING NBTDEC BUFFER
        CALL "NbtDecode-Skip" USING NBTDEC BUFFER
        IF NBTDEC-OFFSET = 17
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    ByteArray.
        DISPLAY "    Case: byte array - " WITH NO ADVANCING
        INITIALIZE NBTDEC
        MOVE X"00000700000003010203FFFFFFFFFFFF" TO BUFFER
        MOVE 3 TO NBTDEC-OFFSET
        CALL "NbtDecode-Skip" USING NBTDEC BUFFER
        IF NBTDEC-OFFSET = 11
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    IntArray.
        DISPLAY "    Case: int array - " WITH NO ADVANCING
        INITIALIZE NBTDEC
        MOVE X"00000B000000020000000800000009FF" TO BUFFER
        MOVE 3 TO NBTDEC-OFFSET
        CALL "NbtDecode-Skip" USING NBTDEC BUFFER
        IF NBTDEC-OFFSET = 16
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    LongArray.
        DISPLAY "    Case: long array - " WITH NO ADVANCING
        INITIALIZE NBTDEC
        MOVE X"00000C000000010000000000000042FF" TO BUFFER
        MOVE 3 TO NBTDEC-OFFSET
        CALL "NbtDecode-Skip" USING NBTDEC BUFFER
        IF NBTDEC-OFFSET = 16
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    StringTag.
        DISPLAY "    Case: string tag - " WITH NO ADVANCING
        INITIALIZE NBTDEC
        MOVE X"000008000454657374FFFFFFFFFFFFFF" TO BUFFER
        MOVE 3 TO NBTDEC-OFFSET
        CALL "NbtDecode-Skip" USING NBTDEC BUFFER
        IF NBTDEC-OFFSET = 10
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    List.
        DISPLAY "    Case: list (short) - " WITH NO ADVANCING
        INITIALIZE NBTDEC
        MOVE X"000009020000000200010002FFFFFFFF" TO BUFFER
        MOVE 3 TO NBTDEC-OFFSET
        CALL "NbtDecode-Skip" USING NBTDEC BUFFER
        IF NBTDEC-OFFSET = 13 AND NBTDEC-LEVEL = 0
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    EmptyList.
        DISPLAY "    Case: empty list - " WITH NO ADVANCING
        INITIALIZE NBTDEC
        MOVE X"0000090000000000FFFFFFFFFFFFFFFF" TO BUFFER
        MOVE 3 TO NBTDEC-OFFSET
        CALL "NbtDecode-Skip" USING NBTDEC BUFFER
        IF NBTDEC-OFFSET = 9 AND NBTDEC-LEVEL = 0
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    NegativeLengthList.
        DISPLAY "    Case: negative length list (int) - " WITH NO ADVANCING
        INITIALIZE NBTDEC
        MOVE X"00000903FFFFFFF1FFFFFFFFFFFFFFFF" TO BUFFER
        MOVE 3 TO NBTDEC-OFFSET
        CALL "NbtDecode-Skip" USING NBTDEC BUFFER
        IF NBTDEC-OFFSET = 9 AND NBTDEC-LEVEL = 0
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    EmptyCompound.
        DISPLAY "    Case: empty compound - " WITH NO ADVANCING
        INITIALIZE NBTDEC
        MOVE X"00000A00FFFFFFFFFFFFFFFFFFFFFFFF" TO BUFFER
        MOVE 3 TO NBTDEC-OFFSET
        CALL "NbtDecode-Skip" USING NBTDEC BUFFER
        IF NBTDEC-OFFSET = 5 AND NBTDEC-LEVEL = 0
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    NestedCompound.
        DISPLAY "    Case: nested compound - " WITH NO ADVANCING
        INITIALIZE NBTDEC
        MOVE X"00000A0A0001440A000157000000FFFF" TO BUFFER
        MOVE 3 TO NBTDEC-OFFSET
        CALL "NbtDecode-Skip" USING NBTDEC BUFFER
        IF NBTDEC-OFFSET = 15 AND NBTDEC-LEVEL = 0
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    WithinList.
        DISPLAY "    Case: within list - " WITH NO ADVANCING
        INITIALIZE NBTDEC
        MOVE X"000009020000000200010002FFFFFFFF" TO BUFFER
        MOVE 3 TO NBTDEC-OFFSET
        CALL "NbtDecode-List" USING NBTDEC BUFFER LIST-LENGTH
        CALL "NbtDecode-Skip" USING NBTDEC BUFFER
        IF NBTDEC-OFFSET NOT = 11 OR NBTDEC-LEVEL NOT = 1
            DISPLAY "FAIL"
            EXIT PARAGRAPH
        END-IF
        CALL "NbtDecode-Skip" USING NBTDEC BUFFER
        IF NBTDEC-OFFSET NOT = 13 OR NBTDEC-LEVEL NOT = 1
            DISPLAY "FAIL"
            EXIT PARAGRAPH
        END-IF
        DISPLAY "PASS".

        GOBACK.

    END PROGRAM Test-NbtDecode-Skip.

    *> --- Test: NbtDecode-UUID ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Test-NbtDecode-UUID.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        COPY DD-NBT-DECODER.
        01 BUFFER       PIC X(32).
        01 UUID         PIC X(16).

    PROCEDURE DIVISION.
        DISPLAY "  Test: Test-NbtDecode-UUID".
    Basic.
        DISPLAY "    Case: UUID - " WITH NO ADVANCING
        INITIALIZE NBTDEC
        MOVE 3 TO NBTDEC-OFFSET
        MOVE x"FFFF0B00000004000102030405060708090A0B0C0D0E0FFFFFFFFFFFFFFFFFFF" TO BUFFER
        MOVE ALL X"00" TO UUID
        CALL "NbtDecode-UUID" USING NBTDEC BUFFER UUID
        IF NBTDEC-OFFSET = 24 AND UUID = X"000102030405060708090A0B0C0D0E0F"
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.

        GOBACK.

    END PROGRAM Test-NbtDecode-UUID.

END PROGRAM Test-NbtDecode.
