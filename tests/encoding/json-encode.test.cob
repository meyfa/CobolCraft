*> --- Test: json-encode.cob ---
IDENTIFICATION DIVISION.
PROGRAM-ID. Test-JsonEncode.

PROCEDURE DIVISION.
    COPY TEST-SUITE REPLACING ==NAME== BY =="encoding/json-encode.cob"==.
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
        COPY TEST-UNIT REPLACING ==NAME== BY =="JsonEncode-ObjectStart"==.
    Basic.
        COPY TEST-CASE REPLACING ==NAME== BY =="basic"==.
        INITIALIZE BUFFER
        MOVE 2 TO OFFSET
        CALL "JsonEncode-ObjectStart" USING BUFFER OFFSET
        COPY TEST-ASSERT REPLACING COND BY ==OFFSET = 3 AND BUFFER = " {"==.

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
        COPY TEST-UNIT REPLACING ==NAME== BY =="JsonEncode-ObjectEnd"==.
    Basic.
        COPY TEST-CASE REPLACING ==NAME== BY =="basic"==.
        INITIALIZE BUFFER
        MOVE 2 TO OFFSET
        CALL "JsonEncode-ObjectEnd" USING BUFFER OFFSET
        COPY TEST-ASSERT REPLACING COND BY ==OFFSET = 3 AND BUFFER = " }"==.

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
        COPY TEST-UNIT REPLACING ==NAME== BY =="JsonEncode-ArrayStart"==.
    Basic.
        COPY TEST-CASE REPLACING ==NAME== BY =="basic"==.
        INITIALIZE BUFFER
        MOVE 2 TO OFFSET
        CALL "JsonEncode-ArrayStart" USING BUFFER OFFSET
        COPY TEST-ASSERT REPLACING COND BY ==OFFSET = 3 AND BUFFER = " ["==.

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
        COPY TEST-UNIT REPLACING ==NAME== BY =="JsonEncode-ArrayEnd"==.
    Basic.
        COPY TEST-CASE REPLACING ==NAME== BY =="basic"==.
        INITIALIZE BUFFER
        MOVE 2 TO OFFSET
        CALL "JsonEncode-ArrayEnd" USING BUFFER OFFSET
        COPY TEST-ASSERT REPLACING COND BY ==OFFSET = 3 AND BUFFER = " ]"==.

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
        COPY TEST-UNIT REPLACING ==NAME== BY =="JsonEncode-Comma"==.
    Basic.
        COPY TEST-CASE REPLACING ==NAME== BY =="basic"==.
        INITIALIZE BUFFER
        MOVE 2 TO OFFSET
        CALL "JsonEncode-Comma" USING BUFFER OFFSET
        COPY TEST-ASSERT REPLACING COND BY ==OFFSET = 3 AND BUFFER = " ,"==.

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
        COPY TEST-UNIT REPLACING ==NAME== BY =="JsonEncode-String"==.
    EmptyString.
        COPY TEST-CASE REPLACING ==NAME== BY =="''"==.
        MOVE SPACES TO STR
        MOVE FUNCTION STORED-CHAR-LENGTH(STR) TO STR-LEN
        INITIALIZE BUFFER
        MOVE 2 TO OFFSET
        CALL "JsonEncode-String" USING BUFFER OFFSET STR STR-LEN
        COPY TEST-ASSERT REPLACING COND BY ==OFFSET = 4 AND BUFFER = " """""==.
    Simple.
        COPY TEST-CASE REPLACING ==NAME== BY =="'abc'"==.
        MOVE "abc" TO STR
        MOVE FUNCTION STORED-CHAR-LENGTH(STR) TO STR-LEN
        INITIALIZE BUFFER
        MOVE 2 TO OFFSET
        CALL "JsonEncode-String" USING BUFFER OFFSET STR STR-LEN
        COPY TEST-ASSERT REPLACING COND BY ==OFFSET = 7 AND BUFFER = " ""abc"""==.
    EscapedChar.
        COPY TEST-CASE REPLACING ==NAME== BY =="'1""2\\3/4\b5\f6\n7\r8\t9'"==.
        MOVE X"3122325C332F3408350C360A370D380939" TO STR
        MOVE FUNCTION STORED-CHAR-LENGTH(STR) TO STR-LEN
        INITIALIZE BUFFER
        MOVE 2 TO OFFSET
        CALL "JsonEncode-String" USING BUFFER OFFSET STR STR-LEN
        COPY TEST-ASSERT REPLACING COND BY ==OFFSET = 28 AND BUFFER = " ""1\""2\\3/4\b5\f6\n7\r8\t9"""==.
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
        COPY TEST-UNIT REPLACING ==NAME== BY =="JsonEncode-ObjectKey"==.
    Simple.
        COPY TEST-CASE REPLACING ==NAME== BY =="'foo ""bar""'"==.
        MOVE "foo ""bar""" TO STR
        MOVE FUNCTION STORED-CHAR-LENGTH(STR) TO STR-LEN
        INITIALIZE BUFFER
        MOVE 2 TO OFFSET
        CALL "JsonEncode-ObjectKey" USING BUFFER OFFSET STR STR-LEN
        COPY TEST-ASSERT REPLACING COND BY ==OFFSET = 16 AND BUFFER = " ""foo \""bar\"""":"==.

        GOBACK.

    END PROGRAM Test-JsonEncode-ObjectKey.

    *> --- Test: JsonEncode-Integer ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Test-JsonEncode-Integer.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        01 BUFFER       PIC X(100).
        01 OFFSET       BINARY-LONG UNSIGNED.
        01 INT64        BINARY-LONG-LONG.

    PROCEDURE DIVISION.
        COPY TEST-UNIT REPLACING ==NAME== BY =="JsonEncode-Integer"==.
    Int0.
        COPY TEST-CASE REPLACING ==NAME== BY =="0"==.
        MOVE 0 TO INT64
        INITIALIZE BUFFER
        MOVE 2 TO OFFSET
        CALL "JsonEncode-Integer" USING BUFFER OFFSET INT64
        COPY TEST-ASSERT REPLACING COND BY ==OFFSET = 3 AND BUFFER = " 0"==.
    Simple.
        COPY TEST-CASE REPLACING ==NAME== BY =="123"==.
        MOVE 123 TO INT64
        INITIALIZE BUFFER
        MOVE 2 TO OFFSET
        CALL "JsonEncode-Integer" USING BUFFER OFFSET INT64
        COPY TEST-ASSERT REPLACING COND BY ==OFFSET = 5 AND BUFFER = " 123"==.
    NegativeInt.
        COPY TEST-CASE REPLACING ==NAME== BY =="-123"==.
        MOVE -123 TO INT64
        INITIALIZE BUFFER
        MOVE 2 TO OFFSET
        CALL "JsonEncode-Integer" USING BUFFER OFFSET INT64
        COPY TEST-ASSERT REPLACING COND BY ==OFFSET = 6 AND BUFFER = " -123"==.

        GOBACK.

    END PROGRAM Test-JsonEncode-Integer.

END PROGRAM Test-JsonEncode.
