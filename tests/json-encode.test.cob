*> --- Test: json-encode.cob ---
IDENTIFICATION DIVISION.
PROGRAM-ID. Test-JsonEncode.

PROCEDURE DIVISION.
    DISPLAY "Test: json-encode.cob"
    CALL "Test-JsonEncode-ObjectStart"
    CALL "Test-JsonEncode-ObjectEnd"
    CALL "Test-JsonEncode-ArrayStart"
    CALL "Test-JsonEncode-ArrayEnd"
    CALL "Test-JsonEncode-Comma"
    CALL "Test-JsonEncode-String"
    CALL "Test-JsonEncode-ObjectKey"
    CALL "Test-JsonEncode-Integer"
    GOBACK.

    *> --- Test: JsonEncode-ObjectStart ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Test-JsonEncode-ObjectStart.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        01 BUFFER       PIC X(100).
        01 OFFSET       BINARY-LONG UNSIGNED.

    PROCEDURE DIVISION.
        DISPLAY "  Test: JsonEncode-ObjectStart".
    Basic.
        DISPLAY "    Case: basic - " WITH NO ADVANCING
        INITIALIZE BUFFER
        MOVE 2 TO OFFSET
        CALL "JsonEncode-ObjectStart" USING BUFFER OFFSET
        IF OFFSET = 3 AND BUFFER = " {"
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.

        GOBACK.

    END PROGRAM Test-JsonEncode-ObjectStart.

    *> --- Test: JsonEncode-ObjectEnd ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Test-JsonEncode-ObjectEnd.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        01 BUFFER       PIC X(100).
        01 OFFSET       BINARY-LONG UNSIGNED.

    PROCEDURE DIVISION.
        DISPLAY "  Test: JsonEncode-ObjectEnd".
    Basic.
        DISPLAY "    Case: basic - " WITH NO ADVANCING
        INITIALIZE BUFFER
        MOVE 2 TO OFFSET
        CALL "JsonEncode-ObjectEnd" USING BUFFER OFFSET
        IF OFFSET = 3 AND BUFFER = " }"
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.

        GOBACK.

    END PROGRAM Test-JsonEncode-ObjectEnd.

    *> --- Test: JsonEncode-ArrayStart ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Test-JsonEncode-ArrayStart.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        01 BUFFER       PIC X(100).
        01 OFFSET       BINARY-LONG UNSIGNED.

    PROCEDURE DIVISION.
        DISPLAY "  Test: JsonEncode-ArrayStart".
    Basic.
        DISPLAY "    Case: basic - " WITH NO ADVANCING
        INITIALIZE BUFFER
        MOVE 2 TO OFFSET
        CALL "JsonEncode-ArrayStart" USING BUFFER OFFSET
        IF OFFSET = 3 AND BUFFER = " ["
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.

        GOBACK.

    END PROGRAM Test-JsonEncode-ArrayStart.

    *> --- Test: JsonEncode-ArrayEnd ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Test-JsonEncode-ArrayEnd.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        01 BUFFER       PIC X(100).
        01 OFFSET       BINARY-LONG UNSIGNED.

    PROCEDURE DIVISION.
        DISPLAY "  Test: JsonEncode-ArrayEnd".
    Basic.
        DISPLAY "    Case: basic - " WITH NO ADVANCING
        INITIALIZE BUFFER
        MOVE 2 TO OFFSET
        CALL "JsonEncode-ArrayEnd" USING BUFFER OFFSET
        IF OFFSET = 3 AND BUFFER = " ]"
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.

        GOBACK.

    END PROGRAM Test-JsonEncode-ArrayEnd.

    *> --- JsonEncode-Comma ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Test-JsonEncode-Comma.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        01 BUFFER       PIC X(100).
        01 OFFSET       BINARY-LONG UNSIGNED.

    PROCEDURE DIVISION.
        DISPLAY "  Test: JsonEncode-Comma".
    Basic.
        DISPLAY "    Case: basic - " WITH NO ADVANCING
        INITIALIZE BUFFER
        MOVE 2 TO OFFSET
        CALL "JsonEncode-Comma" USING BUFFER OFFSET
        IF OFFSET = 3 AND BUFFER = " ,"
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.

        GOBACK.

    END PROGRAM Test-JsonEncode-Comma.

    *> --- Test: JsonEncode-String ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Test-JsonEncode-String.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        01 BUFFER       PIC X(100).
        01 OFFSET       BINARY-LONG UNSIGNED.
        01 STR          PIC X(100).
        01 STR-LEN      BINARY-LONG UNSIGNED.

    PROCEDURE DIVISION.
        DISPLAY "  Test: JsonEncode-String".
    EmptyString.
        DISPLAY "    Case: '' - " WITH NO ADVANCING
        MOVE SPACES TO STR
        MOVE FUNCTION STORED-CHAR-LENGTH(STR) TO STR-LEN
        INITIALIZE BUFFER
        MOVE 2 TO OFFSET
        CALL "JsonEncode-String" USING BUFFER OFFSET STR STR-LEN
        IF OFFSET = 4 AND BUFFER = " """""
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    Simple.
        DISPLAY "    Case: 'abc' - " WITH NO ADVANCING
        MOVE "abc" TO STR
        MOVE FUNCTION STORED-CHAR-LENGTH(STR) TO STR-LEN
        INITIALIZE BUFFER
        MOVE 2 TO OFFSET
        CALL "JsonEncode-String" USING BUFFER OFFSET STR STR-LEN
        IF OFFSET = 7 AND BUFFER = " ""abc"""
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    EscapedChar.
        DISPLAY "    Case: '1""2\\3/4\b5\f6\n7\r8\t9' - " WITH NO ADVANCING
        MOVE X"3122325C332F3408350C360A370D380939" TO STR
        MOVE FUNCTION STORED-CHAR-LENGTH(STR) TO STR-LEN
        INITIALIZE BUFFER
        MOVE 2 TO OFFSET
        CALL "JsonEncode-String" USING BUFFER OFFSET STR STR-LEN
        IF OFFSET = 28 AND BUFFER = " ""1\""2\\3/4\b5\f6\n7\r8\t9"""
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    EscapedUnicodeChar.
        *> TODO: add this test - requires support for non-ASCII characters

        GOBACK.

    END PROGRAM Test-JsonEncode-String.

    *> --- Test: JsonEncode-ObjectKey ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Test-JsonEncode-ObjectKey.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        01 BUFFER       PIC X(100).
        01 OFFSET       BINARY-LONG UNSIGNED.
        01 STR          PIC X(100).
        01 STR-LEN      BINARY-LONG UNSIGNED.

    PROCEDURE DIVISION.
        DISPLAY "  Test: JsonEncode-ObjectKey".
    Simple.
        DISPLAY "    Case: 'foo ""bar""' - " WITH NO ADVANCING
        MOVE "foo ""bar""" TO STR
        MOVE FUNCTION STORED-CHAR-LENGTH(STR) TO STR-LEN
        INITIALIZE BUFFER
        MOVE 2 TO OFFSET
        CALL "JsonEncode-ObjectKey" USING BUFFER OFFSET STR STR-LEN
        IF OFFSET = 16 AND BUFFER = " ""foo \""bar\"""":"
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.

        GOBACK.

    END PROGRAM Test-JsonEncode-ObjectKey.

    *> --- Test: JsonEncode-Integer ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Test-JsonEncode-Integer.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        01 BUFFER       PIC X(100).
        01 OFFSET       BINARY-LONG UNSIGNED.
        01 INT64        BINARY-LONG.

    PROCEDURE DIVISION.
        DISPLAY "  Test: JsonEncode-Integer".
    Int0.
        DISPLAY "    Case: 0 - " WITH NO ADVANCING
        MOVE 0 TO INT64
        INITIALIZE BUFFER
        MOVE 2 TO OFFSET
        CALL "JsonEncode-Integer" USING BUFFER OFFSET INT64
        IF OFFSET = 3 AND BUFFER = " 0"
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    Simple.
        DISPLAY "    Case: 123 - " WITH NO ADVANCING
        MOVE 123 TO INT64
        INITIALIZE BUFFER
        MOVE 2 TO OFFSET
        CALL "JsonEncode-Integer" USING BUFFER OFFSET INT64
        IF OFFSET = 5 AND BUFFER = " 123"
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    NegativeInt.
        DISPLAY "    Case: -123 - " WITH NO ADVANCING
        MOVE -123 TO INT64
        INITIALIZE BUFFER
        MOVE 2 TO OFFSET
        CALL "JsonEncode-Integer" USING BUFFER OFFSET INT64
        IF OFFSET = 6 AND BUFFER = " -123"
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.

        GOBACK.

    END PROGRAM Test-JsonEncode-Integer.

END PROGRAM Test-JsonEncode.
