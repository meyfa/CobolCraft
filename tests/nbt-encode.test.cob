*> --- Test: nbt-encode.cob ---
IDENTIFICATION DIVISION.
PROGRAM-ID. Test-NbtEncode.

PROCEDURE DIVISION.
    DISPLAY "Test: nbt-encode.cob"
    CALL "Test-NbtEncode-End"
    CALL "Test-NbtEncode-Long"
    CALL "Test-NbtEncode-NamedLong"
    CALL "Test-NbtEncode-NamedCompound"
    CALL "Test-NbtEncode-RootCompound"
    GOBACK.

    *> --- Test: NbtEncode-End ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Test-NbtEncode-End.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        01 BUFFER       PIC X(4).
        01 OFFSET       BINARY-LONG UNSIGNED.

    PROCEDURE DIVISION.
        DISPLAY "  Test: Test-NbtEncode-End".
    Basic.
        DISPLAY "    Case: end - " WITH NO ADVANCING
        MOVE X"12345678" TO BUFFER
        MOVE 3 TO OFFSET
        CALL "NbtEncode-End" USING BUFFER OFFSET
        IF BUFFER = X"12340078" AND OFFSET = 4
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.

        GOBACK.

    END PROGRAM Test-NbtEncode-End.

    *> --- Test: NbtEncode-Long ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Test-NbtEncode-Long.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        01 BUFFER       PIC X(16).
        01 OFFSET       BINARY-LONG UNSIGNED.
        01 VALUE-IN     BINARY-LONG-LONG.

    PROCEDURE DIVISION.
        DISPLAY "  Test: Test-NbtEncode-Long".
    Basic.
        DISPLAY "    Case: long - " WITH NO ADVANCING
        MOVE X"FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF" TO BUFFER
        MOVE 3 TO OFFSET
        MOVE H'0102030405060708' TO VALUE-IN
        CALL "NbtEncode-Long" USING BUFFER OFFSET VALUE-IN
        IF BUFFER = X"FFFF040102030405060708FFFFFFFFFF" AND OFFSET = 12
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.

        GOBACK.

    END PROGRAM Test-NbtEncode-Long.

    *> --- Test: NbtEncode-NamedLong ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Test-NbtEncode-NamedLong.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        01 BUFFER       PIC X(16).
        01 OFFSET       BINARY-LONG UNSIGNED.
        01 NAME-VALUE   PIC X(10).
        01 NAME-LEN     BINARY-LONG UNSIGNED.
        01 VALUE-IN     BINARY-LONG-LONG.

    PROCEDURE DIVISION.
        DISPLAY "  Test: Test-NbtEncode-NamedLong".
    Basic.
        DISPLAY "    Case: named long - " WITH NO ADVANCING
        MOVE X"FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF" TO BUFFER
        MOVE 1 TO OFFSET
        MOVE H'0102030405060708' TO VALUE-IN
        MOVE "Time" TO NAME-VALUE
        MOVE 4 TO NAME-LEN
        CALL "NbtEncode-NamedLong" USING BUFFER OFFSET NAME-VALUE NAME-LEN VALUE-IN
        IF BUFFER = X"04000454696D650102030405060708FF" AND OFFSET = 16
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.

        GOBACK.

    END PROGRAM Test-NbtEncode-NamedLong.

    *> --- Test: NbtEncode-NamedCompound ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Test-NbtEncode-NamedCompound.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        01 BUFFER       PIC X(16).
        01 OFFSET       BINARY-LONG UNSIGNED.
        01 NAME-VALUE   PIC X(10).
        01 NAME-LEN     BINARY-LONG UNSIGNED.

    PROCEDURE DIVISION.
        DISPLAY "  Test: Test-NbtEncode-NamedCompound".
    Basic.
        DISPLAY "    Case: named compound - " WITH NO ADVANCING
        MOVE X"FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF" TO BUFFER
        MOVE 3 TO OFFSET
        MOVE "Data" TO NAME-VALUE
        MOVE 4 TO NAME-LEN
        CALL "NbtEncode-NamedCompound" USING BUFFER OFFSET NAME-VALUE NAME-LEN
        IF BUFFER = X"FFFF0A000444617461FFFFFFFFFFFFFF" AND OFFSET = 10
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.

        GOBACK.

    END PROGRAM Test-NbtEncode-NamedCompound.

    *> --- Test: NbtEncode-RootCompound ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Test-NbtEncode-RootCompound.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        01 BUFFER       PIC X(16).
        01 OFFSET       BINARY-LONG UNSIGNED.

    PROCEDURE DIVISION.
        DISPLAY "  Test: Test-NbtEncode-RootCompound".
    Basic.
        DISPLAY "    Case: root compound - " WITH NO ADVANCING
        MOVE X"FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF" TO BUFFER
        MOVE 3 TO OFFSET
        CALL "NbtEncode-RootCompound" USING BUFFER OFFSET
        IF BUFFER = X"FFFF0A0000FFFFFFFFFFFFFFFFFFFFFF" AND OFFSET = 6
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.

        GOBACK.

    END PROGRAM Test-NbtEncode-RootCompound.

END PROGRAM Test-NbtEncode.
