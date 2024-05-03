*> --- Test: nbt-encode.cob ---
IDENTIFICATION DIVISION.
PROGRAM-ID. Test-NbtEncode.

PROCEDURE DIVISION.
    DISPLAY "Test: nbt-encode.cob"
    CALL "Test-NbtEncode-EndCompound"
    CALL "Test-NbtEncode-Long"
    CALL "Test-NbtEncode-Compound"
    CALL "Test-NbtEncode-RootCompound"
    GOBACK.

    *> --- Test: Test-NbtEncode-EndCompound ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Test-NbtEncode-EndCompound.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        COPY DD-NBT-ENCODER.
        01 BUFFER       PIC X(4).
        01 OFFSET       BINARY-LONG UNSIGNED.

    PROCEDURE DIVISION.
        DISPLAY "  Test: Test-NbtEncode-EndCompound".
    Basic.
        DISPLAY "    Case: end compound - " WITH NO ADVANCING
        INITIALIZE NBT-ENCODER-STATE
        MOVE X"12345678" TO BUFFER
        MOVE 2 TO OFFSET
        CALL "NbtEncode-Compound" USING NBT-ENCODER-STATE BUFFER OFFSET OMITTED OMITTED
        CALL "NbtEncode-EndCompound" USING NBT-ENCODER-STATE BUFFER OFFSET
        IF BUFFER = X"120A0078" AND OFFSET = 4
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.

        GOBACK.

    END PROGRAM Test-NbtEncode-EndCompound.

    *> --- Test: NbtEncode-Long ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Test-NbtEncode-Long.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        COPY DD-NBT-ENCODER.
        01 BUFFER       PIC X(16).
        01 OFFSET       BINARY-LONG UNSIGNED.
        01 NAME-VALUE   PIC X(10).
        01 NAME-LEN     BINARY-LONG UNSIGNED.
        01 VALUE-IN     BINARY-LONG-LONG.

    PROCEDURE DIVISION.
        DISPLAY "  Test: Test-NbtEncode-Long".
    UnnamedLong.
        DISPLAY "    Case: unnamed long - " WITH NO ADVANCING
        INITIALIZE NBT-ENCODER-STATE
        MOVE X"FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF" TO BUFFER
        MOVE 3 TO OFFSET
        MOVE H'0102030405060708' TO VALUE-IN
        CALL "NbtEncode-Long" USING NBT-ENCODER-STATE BUFFER OFFSET OMITTED OMITTED VALUE-IN
        IF BUFFER = X"FFFF040102030405060708FFFFFFFFFF" AND OFFSET = 12
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    Basic.
        DISPLAY "    Case: named long - " WITH NO ADVANCING
        INITIALIZE NBT-ENCODER-STATE
        MOVE X"FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF" TO BUFFER
        MOVE 1 TO OFFSET
        MOVE H'0102030405060708' TO VALUE-IN
        MOVE "Time" TO NAME-VALUE
        MOVE 4 TO NAME-LEN
        CALL "NbtEncode-Long" USING NBT-ENCODER-STATE BUFFER OFFSET NAME-VALUE NAME-LEN VALUE-IN
        IF BUFFER = X"04000454696D650102030405060708FF" AND OFFSET = 16
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.

        GOBACK.

    END PROGRAM Test-NbtEncode-Long.

    *> --- Test: NbtEncode-Compound ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Test-NbtEncode-Compound.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        COPY DD-NBT-ENCODER.
        01 BUFFER       PIC X(16).
        01 OFFSET       BINARY-LONG UNSIGNED.
        01 NAME-VALUE   PIC X(10).
        01 NAME-LEN     BINARY-LONG UNSIGNED.

    PROCEDURE DIVISION.
        DISPLAY "  Test: Test-NbtEncode-Compound".
    UnnamedCompound.
        DISPLAY "    Case: unnamed compound - " WITH NO ADVANCING
        INITIALIZE NBT-ENCODER-STATE
        MOVE X"FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF" TO BUFFER
        MOVE 3 TO OFFSET
        CALL "NbtEncode-Compound" USING NBT-ENCODER-STATE BUFFER OFFSET OMITTED OMITTED
        IF BUFFER = X"FFFF0AFFFFFFFFFFFFFFFFFFFFFFFFFF" AND OFFSET = 4
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    NamedCompound.
        DISPLAY "    Case: named compound - " WITH NO ADVANCING
        INITIALIZE NBT-ENCODER-STATE
        MOVE X"FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF" TO BUFFER
        MOVE 3 TO OFFSET
        MOVE "Data" TO NAME-VALUE
        MOVE 4 TO NAME-LEN
        CALL "NbtEncode-Compound" USING NBT-ENCODER-STATE BUFFER OFFSET NAME-VALUE NAME-LEN
        IF BUFFER = X"FFFF0A000444617461FFFFFFFFFFFFFF" AND OFFSET = 10
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
        01 OFFSET       BINARY-LONG UNSIGNED.

    PROCEDURE DIVISION.
        DISPLAY "  Test: Test-NbtEncode-RootCompound".
    Basic.
        DISPLAY "    Case: root compound - " WITH NO ADVANCING
        INITIALIZE NBT-ENCODER-STATE
        MOVE X"FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF" TO BUFFER
        MOVE 3 TO OFFSET
        CALL "NbtEncode-RootCompound" USING NBT-ENCODER-STATE BUFFER OFFSET
        IF BUFFER = X"FFFF0A0000FFFFFFFFFFFFFFFFFFFFFF" AND OFFSET = 6
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.

        GOBACK.

    END PROGRAM Test-NbtEncode-RootCompound.

END PROGRAM Test-NbtEncode.
