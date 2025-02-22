*> --- Test: nbt-encode.cob ---
IDENTIFICATION DIVISION.
PROGRAM-ID. Test-NbtEncode.

PROCEDURE DIVISION.
    DISPLAY "Test: nbt-encode.cob"
    CALL "Test-NbtEncode-Byte"
    CALL "Test-NbtEncode-Short"
    CALL "Test-NbtEncode-Int"
    CALL "Test-NbtEncode-Long"
    CALL "Test-NbtEncode-Float"
    CALL "Test-NbtEncode-Double"
    CALL "Test-NbtEncode-String"
    CALL "Test-NbtEncode-ByteArray"
    CALL "Test-NbtEncode-List"
    CALL "Test-NbtEncode-Compound"
    CALL "Test-NbtEncode-RootCompound"
    CALL "Test-NbtEncode-EndCompound"
    CALL "Test-NbtEncode-IntArray"
    CALL "Test-NbtEncode-LongArray"
    CALL "Test-NbtEncode-UUID"
    GOBACK.

    *> --- Test: NbtEncode-Byte ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Test-NbtEncode-Byte.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        COPY DD-NBT-ENCODER.
        01 BUFFER       PIC X(16).
        01 NAME-VALUE   PIC X(10).
        01 NAME-LEN     BINARY-LONG UNSIGNED.
        01 VALUE-IN     BINARY-CHAR.

    PROCEDURE DIVISION.
        DISPLAY "  Test: Test-NbtEncode-Byte".
    UnnamedByte.
        DISPLAY "    Case: unnamed byte - " WITH NO ADVANCING
        INITIALIZE NBT-ENCODER-STATE
        MOVE X"FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF" TO BUFFER
        MOVE 3 TO NBT-ENCODER-OFFSET
        MOVE 42 TO VALUE-IN
        CALL "NbtEncode-Byte" USING NBT-ENCODER-STATE BUFFER OMITTED OMITTED VALUE-IN
        IF BUFFER = X"FFFF012AFFFFFFFFFFFFFFFFFFFFFFFF" AND NBT-ENCODER-OFFSET = 5
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    NamedByte.
        DISPLAY "    Case: named byte - " WITH NO ADVANCING
        INITIALIZE NBT-ENCODER-STATE
        MOVE X"FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF" TO BUFFER
        MOVE 3 TO NBT-ENCODER-OFFSET
        MOVE 42 TO VALUE-IN
        MOVE "Time" TO NAME-VALUE
        MOVE 4 TO NAME-LEN
        CALL "NbtEncode-Byte" USING NBT-ENCODER-STATE BUFFER NAME-VALUE NAME-LEN VALUE-IN
        IF BUFFER = X"FFFF01000454696D652AFFFFFFFFFFFF" AND NBT-ENCODER-OFFSET = 11
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.

        GOBACK.

    END PROGRAM Test-NbtEncode-Byte.

    *> --- Test: NbtEncode-Short ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Test-NbtEncode-Short.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        COPY DD-NBT-ENCODER.
        01 BUFFER       PIC X(16).
        01 NAME-VALUE   PIC X(10).
        01 NAME-LEN     BINARY-LONG UNSIGNED.
        01 VALUE-IN     BINARY-SHORT.

    PROCEDURE DIVISION.
        DISPLAY "  Test: Test-NbtEncode-Short".
    UnnamedShort.
        DISPLAY "    Case: unnamed short - " WITH NO ADVANCING
        INITIALIZE NBT-ENCODER-STATE
        MOVE X"FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF" TO BUFFER
        MOVE 3 TO NBT-ENCODER-OFFSET
        MOVE H'0203' TO VALUE-IN
        CALL "NbtEncode-Short" USING NBT-ENCODER-STATE BUFFER OMITTED OMITTED VALUE-IN
        IF BUFFER = X"FFFF020203FFFFFFFFFFFFFFFFFFFFFF" AND NBT-ENCODER-OFFSET = 6
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    NamedShort.
        DISPLAY "    Case: named short - " WITH NO ADVANCING
        INITIALIZE NBT-ENCODER-STATE
        MOVE X"FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF" TO BUFFER
        MOVE 3 TO NBT-ENCODER-OFFSET
        MOVE H'0203' TO VALUE-IN
        MOVE "Time" TO NAME-VALUE
        MOVE 4 TO NAME-LEN
        CALL "NbtEncode-Short" USING NBT-ENCODER-STATE BUFFER NAME-VALUE NAME-LEN VALUE-IN
        IF BUFFER = X"FFFF02000454696D650203FFFFFFFFFF" AND NBT-ENCODER-OFFSET = 12
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.

        GOBACK.

    END PROGRAM Test-NbtEncode-Short.

    *> --- Test: NbtEncode-Int ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Test-NbtEncode-Int.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        COPY DD-NBT-ENCODER.
        01 BUFFER       PIC X(16).
        01 NAME-VALUE   PIC X(10).
        01 NAME-LEN     BINARY-LONG UNSIGNED.
        01 VALUE-IN     BINARY-LONG.

    PROCEDURE DIVISION.
        DISPLAY "  Test: Test-NbtEncode-Int".
    UnnamedInt.
        DISPLAY "    Case: unnamed int - " WITH NO ADVANCING
        INITIALIZE NBT-ENCODER-STATE
        MOVE X"FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF" TO BUFFER
        MOVE 3 TO NBT-ENCODER-OFFSET
        MOVE H'01020304' TO VALUE-IN
        CALL "NbtEncode-Int" USING NBT-ENCODER-STATE BUFFER OMITTED OMITTED VALUE-IN
        IF BUFFER = X"FFFF0301020304FFFFFFFFFFFFFFFFFF" AND NBT-ENCODER-OFFSET = 8
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    NamedInt.
        DISPLAY "    Case: named int - " WITH NO ADVANCING
        INITIALIZE NBT-ENCODER-STATE
        MOVE X"FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF" TO BUFFER
        MOVE 3 TO NBT-ENCODER-OFFSET
        MOVE H'01020304' TO VALUE-IN
        MOVE "Time" TO NAME-VALUE
        MOVE 4 TO NAME-LEN
        CALL "NbtEncode-Int" USING NBT-ENCODER-STATE BUFFER NAME-VALUE NAME-LEN VALUE-IN
        IF BUFFER = X"FFFF03000454696D6501020304FFFFFF" AND NBT-ENCODER-OFFSET = 14
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.

        GOBACK.

    END PROGRAM Test-NbtEncode-Int.

    *> --- Test: NbtEncode-Long ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Test-NbtEncode-Long.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        COPY DD-NBT-ENCODER.
        01 BUFFER       PIC X(16).
        01 NAME-VALUE   PIC X(10).
        01 NAME-LEN     BINARY-LONG UNSIGNED.
        01 VALUE-IN     BINARY-LONG-LONG.

    PROCEDURE DIVISION.
        DISPLAY "  Test: Test-NbtEncode-Long".
    UnnamedLong.
        DISPLAY "    Case: unnamed long - " WITH NO ADVANCING
        INITIALIZE NBT-ENCODER-STATE
        MOVE X"FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF" TO BUFFER
        MOVE 3 TO NBT-ENCODER-OFFSET
        MOVE H'0102030405060708' TO VALUE-IN
        CALL "NbtEncode-Long" USING NBT-ENCODER-STATE BUFFER OMITTED OMITTED VALUE-IN
        IF BUFFER = X"FFFF040102030405060708FFFFFFFFFF" AND NBT-ENCODER-OFFSET = 12
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    Basic.
        DISPLAY "    Case: named long - " WITH NO ADVANCING
        INITIALIZE NBT-ENCODER-STATE
        MOVE X"FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF" TO BUFFER
        MOVE 1 TO NBT-ENCODER-OFFSET
        MOVE H'0102030405060708' TO VALUE-IN
        MOVE "Time" TO NAME-VALUE
        MOVE 4 TO NAME-LEN
        CALL "NbtEncode-Long" USING NBT-ENCODER-STATE BUFFER NAME-VALUE NAME-LEN VALUE-IN
        IF BUFFER = X"04000454696D650102030405060708FF" AND NBT-ENCODER-OFFSET = 16
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.

        GOBACK.

    END PROGRAM Test-NbtEncode-Long.

    *> --- Test: NbtEncode-Float ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Test-NbtEncode-Float.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        COPY DD-NBT-ENCODER.
        01 BUFFER       PIC X(16).
        01 NAME-VALUE   PIC X(10).
        01 NAME-LEN     BINARY-LONG UNSIGNED.
        01 VALUE-IN     FLOAT-SHORT.

    PROCEDURE DIVISION.
        DISPLAY "  Test: Test-NbtEncode-Float".
    UnnamedFloat.
        DISPLAY "    Case: unnamed float (+12.125) - " WITH NO ADVANCING
        INITIALIZE NBT-ENCODER-STATE
        MOVE X"FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF" TO BUFFER
        MOVE 3 TO NBT-ENCODER-OFFSET
        MOVE 12.125 TO VALUE-IN
        CALL "NbtEncode-Float" USING NBT-ENCODER-STATE BUFFER OMITTED OMITTED VALUE-IN
        IF BUFFER = X"FFFF0541420000FFFFFFFFFFFFFFFFFF" AND NBT-ENCODER-OFFSET = 8
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    NamedFloat.
        DISPLAY "    Case: named float (-12.125) - " WITH NO ADVANCING
        INITIALIZE NBT-ENCODER-STATE
        MOVE X"FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF" TO BUFFER
        MOVE 3 TO NBT-ENCODER-OFFSET
        MOVE -12.125 TO VALUE-IN
        MOVE "Time" TO NAME-VALUE
        MOVE 4 TO NAME-LEN
        CALL "NbtEncode-Float" USING NBT-ENCODER-STATE BUFFER NAME-VALUE NAME-LEN VALUE-IN
        IF BUFFER = X"FFFF05000454696D65C1420000FFFFFF" AND NBT-ENCODER-OFFSET = 14
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.

        GOBACK.

    END PROGRAM Test-NbtEncode-Float.

    *> --- Test: NbtEncode-Double ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Test-NbtEncode-Double.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        COPY DD-NBT-ENCODER.
        01 BUFFER       PIC X(16).
        01 NAME-VALUE   PIC X(10).
        01 NAME-LEN     BINARY-LONG UNSIGNED.
        01 VALUE-IN     FLOAT-LONG.

    PROCEDURE DIVISION.
        DISPLAY "  Test: Test-NbtEncode-Double".
    UnnamedDouble.
        DISPLAY "    Case: unnamed double (+12.125) - " WITH NO ADVANCING
        INITIALIZE NBT-ENCODER-STATE
        MOVE X"FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF" TO BUFFER
        MOVE 3 TO NBT-ENCODER-OFFSET
        MOVE 12.125 TO VALUE-IN
        CALL "NbtEncode-Double" USING NBT-ENCODER-STATE BUFFER OMITTED OMITTED VALUE-IN
        IF BUFFER = X"FFFF064028400000000000FFFFFFFFFF" AND NBT-ENCODER-OFFSET = 12
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    NamedDouble.
        DISPLAY "    Case: named double (-12.125) - " WITH NO ADVANCING
        INITIALIZE NBT-ENCODER-STATE
        MOVE X"FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF" TO BUFFER
        MOVE 2 TO NBT-ENCODER-OFFSET
        MOVE -12.125 TO VALUE-IN
        MOVE "Time" TO NAME-VALUE
        MOVE 4 TO NAME-LEN
        CALL "NbtEncode-Double" USING NBT-ENCODER-STATE BUFFER NAME-VALUE NAME-LEN VALUE-IN
        IF BUFFER = X"FF06000454696D65C028400000000000" AND NBT-ENCODER-OFFSET = 17
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.

        GOBACK.

    END PROGRAM Test-NbtEncode-Double.

    *> --- Test: NbtEncode-String ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Test-NbtEncode-String.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        COPY DD-NBT-ENCODER.
        01 BUFFER       PIC X(24).
        01 NAME-VALUE   PIC X(10).
        01 NAME-LEN     BINARY-LONG UNSIGNED.
        01 STR          PIC X(10).
        01 STR-LEN      BINARY-LONG UNSIGNED.

    PROCEDURE DIVISION.
        DISPLAY "  Test: Test-NbtEncode-String".
    UnnamedEmptyString.
        DISPLAY "    Case: unnamed empty string - " WITH NO ADVANCING
        INITIALIZE NBT-ENCODER-STATE
        MOVE X"FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF" TO BUFFER
        MOVE 3 TO NBT-ENCODER-OFFSET
        MOVE 0 TO STR-LEN
        CALL "NbtEncode-String" USING NBT-ENCODER-STATE BUFFER OMITTED OMITTED STR STR-LEN
        IF BUFFER = X"FFFF080000FFFFFFFFFFFFFFFFFFFFFF" AND NBT-ENCODER-OFFSET = 6
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    NamedString.
        DISPLAY "    Case: named non-empty string - " WITH NO ADVANCING
        INITIALIZE NBT-ENCODER-STATE
        MOVE X"FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF" TO BUFFER
        MOVE 3 TO NBT-ENCODER-OFFSET
        MOVE "Hello" TO STR
        MOVE 5 TO STR-LEN
        MOVE "Time" TO NAME-VALUE
        MOVE 4 TO NAME-LEN
        CALL "NbtEncode-String" USING NBT-ENCODER-STATE BUFFER NAME-VALUE NAME-LEN STR STR-LEN
        IF BUFFER = X"FFFF08000454696D65000548656C6C6F" AND NBT-ENCODER-OFFSET = 17
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.

        GOBACK.

    END PROGRAM Test-NbtEncode-String.

    *> --- Test: NbtEncode-ByteArray ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Test-NbtEncode-ByteArray.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        COPY DD-NBT-ENCODER.
        01 BUFFER       PIC X(24).
        01 NAME-VALUE   PIC X(10).
        01 NAME-LEN     BINARY-LONG UNSIGNED.
        01 ARRAY-LEN    BINARY-LONG UNSIGNED.
        01 VALUE-IN     BINARY-CHAR.

    PROCEDURE DIVISION.
        DISPLAY "  Test: Test-NbtEncode-ByteArray".
    UnnamedEmptyArray.
        DISPLAY "    Case: unnamed empty array - " WITH NO ADVANCING
        INITIALIZE NBT-ENCODER-STATE
        MOVE X"FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF" TO BUFFER
        MOVE 3 TO NBT-ENCODER-OFFSET
        MOVE 0 TO ARRAY-LEN
        CALL "NbtEncode-ByteArray" USING NBT-ENCODER-STATE BUFFER OMITTED OMITTED ARRAY-LEN
        IF BUFFER = X"FFFF0700000000FFFFFFFFFFFFFFFFFF" AND NBT-ENCODER-OFFSET = 8 AND NBT-ENCODER-LEVEL = 0
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    NamedEmptyArray.
        DISPLAY "    Case: named empty array - " WITH NO ADVANCING
        INITIALIZE NBT-ENCODER-STATE
        MOVE X"FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF" TO BUFFER
        MOVE 3 TO NBT-ENCODER-OFFSET
        MOVE "Data" TO NAME-VALUE
        MOVE 4 TO NAME-LEN
        MOVE 0 TO ARRAY-LEN
        CALL "NbtEncode-ByteArray" USING NBT-ENCODER-STATE BUFFER NAME-VALUE NAME-LEN ARRAY-LEN
        IF BUFFER = X"FFFF0700044461746100000000FFFFFF" AND NBT-ENCODER-OFFSET = 14 AND NBT-ENCODER-LEVEL = 0
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    NonEmptyNamedArray.
        DISPLAY "    Case: named non-empty array - " WITH NO ADVANCING
        INITIALIZE NBT-ENCODER-STATE
        MOVE X"FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF" TO BUFFER
        MOVE 3 TO NBT-ENCODER-OFFSET
        MOVE "Data" TO NAME-VALUE
        MOVE 4 TO NAME-LEN
        MOVE 2 TO ARRAY-LEN
        CALL "NbtEncode-ByteArray" USING NBT-ENCODER-STATE BUFFER NAME-VALUE NAME-LEN ARRAY-LEN
        IF NBT-ENCODER-LEVEL NOT = 1 OR NBT-ENCODER-STACK-TYPE(1) NOT = X"07"
            DISPLAY "FAIL"
            EXIT PARAGRAPH
        END-IF
        MOVE 1 TO VALUE-IN
        CALL "NbtEncode-Byte" USING NBT-ENCODER-STATE BUFFER OMITTED OMITTED VALUE-IN
        MOVE 2 TO VALUE-IN
        CALL "NbtEncode-Byte" USING NBT-ENCODER-STATE BUFFER OMITTED OMITTED VALUE-IN
        IF BUFFER = X"FFFF07000444617461000000020102FF" AND NBT-ENCODER-OFFSET = 16 AND NBT-ENCODER-LEVEL = 0
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.

        GOBACK.

    END PROGRAM Test-NbtEncode-ByteArray.

    *> --- Test: NbtEncode-List ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Test-NbtEncode-List.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        COPY DD-NBT-ENCODER.
        01 BUFFER       PIC X(24).
        01 NAME-VALUE   PIC X(10).
        01 NAME-LEN     BINARY-LONG UNSIGNED.

    PROCEDURE DIVISION.
        DISPLAY "  Test: Test-NbtEncode-List".
    Empty.
        DISPLAY "    Case: empty list - " WITH NO ADVANCING
        INITIALIZE NBT-ENCODER-STATE
        MOVE X"FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF" TO BUFFER
        MOVE 3 TO NBT-ENCODER-OFFSET
        CALL "NbtEncode-List" USING NBT-ENCODER-STATE BUFFER OMITTED OMITTED
        IF NBT-ENCODER-OFFSET NOT = 9 OR NBT-ENCODER-LEVEL NOT = 1 OR NBT-ENCODER-STACK-TYPE(1) NOT = X"09"
            DISPLAY "FAIL"
            EXIT PARAGRAPH
        END-IF
        CALL "NbtEncode-EndList" USING NBT-ENCODER-STATE BUFFER
        IF BUFFER = X"FFFF090000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF" AND NBT-ENCODER-OFFSET = 9 AND NBT-ENCODER-LEVEL = 0
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    LongList.
        DISPLAY "    Case: list of long - " WITH NO ADVANCING
        INITIALIZE NBT-ENCODER-STATE
        MOVE X"FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF" TO BUFFER
        MOVE 3 TO NBT-ENCODER-OFFSET
        CALL "NbtEncode-List" USING NBT-ENCODER-STATE BUFFER OMITTED OMITTED
        IF NBT-ENCODER-OFFSET NOT = 9 OR NBT-ENCODER-LEVEL NOT = 1 OR NBT-ENCODER-STACK-TYPE(1) NOT = X"09"
            DISPLAY "FAIL"
            EXIT PARAGRAPH
        END-IF
        CALL "NbtEncode-Long" USING NBT-ENCODER-STATE BUFFER OMITTED OMITTED H'0102030405060708'
        CALL "NbtEncode-Long" USING NBT-ENCODER-STATE BUFFER OMITTED OMITTED H'1122334455667788'
        CALL "NbtEncode-EndList" USING NBT-ENCODER-STATE BUFFER
        IF BUFFER = X"FFFF09040000000201020304050607081122334455667788" AND NBT-ENCODER-OFFSET = 25 AND NBT-ENCODER-LEVEL = 0
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    NamedList.
        DISPLAY "    Case: named list - " WITH NO ADVANCING
        INITIALIZE NBT-ENCODER-STATE
        MOVE X"FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF" TO BUFFER
        MOVE 3 TO NBT-ENCODER-OFFSET
        MOVE "Data" TO NAME-VALUE
        MOVE 4 TO NAME-LEN
        CALL "NbtEncode-List" USING NBT-ENCODER-STATE BUFFER NAME-VALUE NAME-LEN
        IF NBT-ENCODER-OFFSET NOT = 15 OR NBT-ENCODER-LEVEL NOT = 1 OR NBT-ENCODER-STACK-TYPE(1) NOT = X"09"
            DISPLAY "FAIL"
            EXIT PARAGRAPH
        END-IF
        CALL "NbtEncode-Long" USING NBT-ENCODER-STATE BUFFER OMITTED OMITTED H'0102030405060708'
        CALL "NbtEncode-EndList" USING NBT-ENCODER-STATE BUFFER
        IF BUFFER = X"FFFF0900044461746104000000010102030405060708FFFF" AND NBT-ENCODER-OFFSET = 23 AND NBT-ENCODER-LEVEL = 0
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.

        GOBACK.

    END PROGRAM Test-NbtEncode-List.

    *> --- Test: NbtEncode-Compound ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Test-NbtEncode-Compound.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        COPY DD-NBT-ENCODER.
        01 BUFFER       PIC X(16).
        01 NAME-VALUE   PIC X(10).
        01 NAME-LEN     BINARY-LONG UNSIGNED.

    PROCEDURE DIVISION.
        DISPLAY "  Test: Test-NbtEncode-Compound".
    UnnamedCompound.
        DISPLAY "    Case: unnamed compound - " WITH NO ADVANCING
        INITIALIZE NBT-ENCODER-STATE
        MOVE X"FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF" TO BUFFER
        MOVE 3 TO NBT-ENCODER-OFFSET
        CALL "NbtEncode-Compound" USING NBT-ENCODER-STATE BUFFER OMITTED OMITTED
        IF BUFFER = X"FFFF0AFFFFFFFFFFFFFFFFFFFFFFFFFF" AND NBT-ENCODER-OFFSET = 4
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    NamedCompound.
        DISPLAY "    Case: named compound - " WITH NO ADVANCING
        INITIALIZE NBT-ENCODER-STATE
        MOVE X"FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF" TO BUFFER
        MOVE 3 TO NBT-ENCODER-OFFSET
        MOVE "Data" TO NAME-VALUE
        MOVE 4 TO NAME-LEN
        CALL "NbtEncode-Compound" USING NBT-ENCODER-STATE BUFFER NAME-VALUE NAME-LEN
        IF BUFFER = X"FFFF0A000444617461FFFFFFFFFFFFFF" AND NBT-ENCODER-OFFSET = 10
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.

        GOBACK.

    END PROGRAM Test-NbtEncode-Compound.

    *> --- Test: NbtEncode-RootCompound ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Test-NbtEncode-RootCompound.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        COPY DD-NBT-ENCODER.
        01 BUFFER       PIC X(16).

    PROCEDURE DIVISION.
        DISPLAY "  Test: Test-NbtEncode-RootCompound".
    Basic.
        DISPLAY "    Case: root compound - " WITH NO ADVANCING
        INITIALIZE NBT-ENCODER-STATE
        MOVE X"FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF" TO BUFFER
        MOVE 3 TO NBT-ENCODER-OFFSET
        CALL "NbtEncode-RootCompound" USING NBT-ENCODER-STATE BUFFER
        IF BUFFER = X"FFFF0A0000FFFFFFFFFFFFFFFFFFFFFF" AND NBT-ENCODER-OFFSET = 6
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.

        GOBACK.

    END PROGRAM Test-NbtEncode-RootCompound.

    *> --- Test: Test-NbtEncode-EndCompound ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Test-NbtEncode-EndCompound.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        COPY DD-NBT-ENCODER.
        01 BUFFER       PIC X(4).

    PROCEDURE DIVISION.
        DISPLAY "  Test: Test-NbtEncode-EndCompound".
    Basic.
        DISPLAY "    Case: end compound - " WITH NO ADVANCING
        INITIALIZE NBT-ENCODER-STATE
        MOVE X"12345678" TO BUFFER
        MOVE 2 TO NBT-ENCODER-OFFSET
        CALL "NbtEncode-Compound" USING NBT-ENCODER-STATE BUFFER OMITTED OMITTED
        CALL "NbtEncode-EndCompound" USING NBT-ENCODER-STATE BUFFER
        IF BUFFER = X"120A0078" AND NBT-ENCODER-OFFSET = 4
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.

        GOBACK.

    END PROGRAM Test-NbtEncode-EndCompound.

    *> --- Test: NbtEncode-IntArray ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Test-NbtEncode-IntArray.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        COPY DD-NBT-ENCODER.
        01 BUFFER       PIC X(24).
        01 NAME-VALUE   PIC X(10).
        01 NAME-LEN     BINARY-LONG UNSIGNED.
        01 ARRAY-LEN    BINARY-LONG UNSIGNED.

    PROCEDURE DIVISION.
        DISPLAY "  Test: Test-NbtEncode-IntArray".
    UnnamedEmptyArray.
        DISPLAY "    Case: unnamed empty array - " WITH NO ADVANCING
        INITIALIZE NBT-ENCODER-STATE
        MOVE X"FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF" TO BUFFER
        MOVE 3 TO NBT-ENCODER-OFFSET
        MOVE 0 TO ARRAY-LEN
        CALL "NbtEncode-IntArray" USING NBT-ENCODER-STATE BUFFER OMITTED OMITTED ARRAY-LEN
        IF BUFFER = X"FFFF0B00000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF" AND NBT-ENCODER-OFFSET = 8 AND NBT-ENCODER-LEVEL = 0
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    NamedEmptyArray.
        DISPLAY "    Case: named empty array - " WITH NO ADVANCING
        INITIALIZE NBT-ENCODER-STATE
        MOVE X"FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF" TO BUFFER
        MOVE 3 TO NBT-ENCODER-OFFSET
        MOVE "Data" TO NAME-VALUE
        MOVE 4 TO NAME-LEN
        MOVE 0 TO ARRAY-LEN
        CALL "NbtEncode-IntArray" USING NBT-ENCODER-STATE BUFFER NAME-VALUE NAME-LEN ARRAY-LEN
        IF BUFFER = X"FFFF0B00044461746100000000FFFFFFFFFFFFFFFFFFFFFF" AND NBT-ENCODER-OFFSET = 14 AND NBT-ENCODER-LEVEL = 0
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    NonEmptyNamedArray.
        DISPLAY "    Case: named non-empty array - " WITH NO ADVANCING
        INITIALIZE NBT-ENCODER-STATE
        MOVE X"FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF" TO BUFFER
        MOVE 3 TO NBT-ENCODER-OFFSET
        MOVE "Data" TO NAME-VALUE
        MOVE 4 TO NAME-LEN
        MOVE 1 TO ARRAY-LEN
        CALL "NbtEncode-IntArray" USING NBT-ENCODER-STATE BUFFER NAME-VALUE NAME-LEN ARRAY-LEN
        IF NBT-ENCODER-LEVEL NOT = 1 OR NBT-ENCODER-STACK-TYPE(1) NOT = X"0B"
            DISPLAY "FAIL"
            EXIT PARAGRAPH
        END-IF
        CALL "NbtEncode-Int" USING NBT-ENCODER-STATE BUFFER OMITTED OMITTED H'01020304'
        IF BUFFER = X"FFFF0B0004446174610000000101020304FFFFFFFFFFFFFF" AND NBT-ENCODER-OFFSET = 18 AND NBT-ENCODER-LEVEL = 0
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.

        GOBACK.

    END PROGRAM Test-NbtEncode-IntArray.

    *> --- Test: NbtEncode-LongArray ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Test-NbtEncode-LongArray.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        COPY DD-NBT-ENCODER.
        01 BUFFER       PIC X(24).
        01 NAME-VALUE   PIC X(10).
        01 NAME-LEN     BINARY-LONG UNSIGNED.
        01 ARRAY-LEN    BINARY-LONG UNSIGNED.

    PROCEDURE DIVISION.
        DISPLAY "  Test: Test-NbtEncode-LongArray".
    UnnamedEmptyArray.
        DISPLAY "    Case: unnamed empty array - " WITH NO ADVANCING
        INITIALIZE NBT-ENCODER-STATE
        MOVE X"FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF" TO BUFFER
        MOVE 3 TO NBT-ENCODER-OFFSET
        MOVE 0 TO ARRAY-LEN
        CALL "NbtEncode-LongArray" USING NBT-ENCODER-STATE BUFFER OMITTED OMITTED ARRAY-LEN
        IF BUFFER = X"FFFF0C00000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF" AND NBT-ENCODER-OFFSET = 8 AND NBT-ENCODER-LEVEL = 0
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    NamedEmptyArray.
        DISPLAY "    Case: named empty array - " WITH NO ADVANCING
        INITIALIZE NBT-ENCODER-STATE
        MOVE X"FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF" TO BUFFER
        MOVE 3 TO NBT-ENCODER-OFFSET
        MOVE "Data" TO NAME-VALUE
        MOVE 4 TO NAME-LEN
        MOVE 0 TO ARRAY-LEN
        CALL "NbtEncode-LongArray" USING NBT-ENCODER-STATE BUFFER NAME-VALUE NAME-LEN ARRAY-LEN
        IF BUFFER = X"FFFF0C00044461746100000000FFFFFFFFFFFFFFFFFFFFFF" AND NBT-ENCODER-OFFSET = 14 AND NBT-ENCODER-LEVEL = 0
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    NonEmptyNamedArray.
        DISPLAY "    Case: named non-empty array - " WITH NO ADVANCING
        INITIALIZE NBT-ENCODER-STATE
        MOVE X"FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF" TO BUFFER
        MOVE 3 TO NBT-ENCODER-OFFSET
        MOVE "Data" TO NAME-VALUE
        MOVE 4 TO NAME-LEN
        MOVE 1 TO ARRAY-LEN
        CALL "NbtEncode-LongArray" USING NBT-ENCODER-STATE BUFFER NAME-VALUE NAME-LEN ARRAY-LEN
        IF NBT-ENCODER-LEVEL NOT = 1 OR NBT-ENCODER-STACK-TYPE(1) NOT = X"0C"
            DISPLAY "FAIL"
            EXIT PARAGRAPH
        END-IF
        CALL "NbtEncode-Long" USING NBT-ENCODER-STATE BUFFER OMITTED OMITTED H'0102030405060708'
        IF BUFFER = X"FFFF0C000444617461000000010102030405060708FFFFFF" AND NBT-ENCODER-OFFSET = 22 AND NBT-ENCODER-LEVEL = 0
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.

        GOBACK.

    END PROGRAM Test-NbtEncode-LongArray.

    *> --- Test: NbtEncode-UUID ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Test-NbtEncode-UUID.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        COPY DD-NBT-ENCODER.
        01 BUFFER       PIC X(32).
        01 UUID         PIC X(16).

    PROCEDURE DIVISION.
        DISPLAY "  Test: Test-NbtEncode-UUID".
    Basic.
        DISPLAY "    Case: UUID - " WITH NO ADVANCING
        INITIALIZE NBT-ENCODER-STATE
        MOVE 3 TO NBT-ENCODER-OFFSET
        MOVE ALL X"FF" TO BUFFER
        MOVE X"000102030405060708090A0B0C0D0E0F" TO UUID
        CALL "NbtEncode-UUID" USING NBT-ENCODER-STATE BUFFER OMITTED OMITTED UUID
        IF BUFFER = X"FFFF0B00000004000102030405060708090A0B0C0D0E0FFFFFFFFFFFFFFFFFFF" AND NBT-ENCODER-OFFSET = 24
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.

        GOBACK.

    END PROGRAM Test-NbtEncode-UUID.

END PROGRAM Test-NbtEncode.
