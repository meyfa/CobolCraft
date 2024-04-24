*> --- JsonParse-SkipWhitespace ---
IDENTIFICATION DIVISION.
PROGRAM-ID. JsonParse-SkipWhitespace.

DATA DIVISION.
LOCAL-STORAGE SECTION.
    01 CHARCODE         BINARY-CHAR UNSIGNED.
LINKAGE SECTION.
    01 LK-INPUT         PIC X ANY LENGTH.
    01 LK-OFFSET        BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-INPUT LK-OFFSET.
    PERFORM UNTIL LK-OFFSET > LENGTH OF LK-INPUT
        COMPUTE CHARCODE = FUNCTION ORD(LK-INPUT(LK-OFFSET:1)) - 1
        *> Exit the loop once a non-whitespace character is found
        IF CHARCODE NOT = 9 AND CHARCODE NOT = 10 AND CHARCODE NOT = 11 AND CHARCODE NOT = 13 AND CHARCODE NOT = 32
            EXIT PERFORM
        END-IF
        ADD 1 TO LK-OFFSET
    END-PERFORM
    GOBACK.

END PROGRAM JsonParse-SkipWhitespace.

*> --- JsonParse-ObjectStart ---
IDENTIFICATION DIVISION.
PROGRAM-ID. JsonParse-ObjectStart.

DATA DIVISION.
LINKAGE SECTION.
    01 LK-INPUT         PIC X ANY LENGTH.
    01 LK-OFFSET        BINARY-LONG UNSIGNED.
    01 LK-FLAG          BINARY-CHAR UNSIGNED.

PROCEDURE DIVISION USING LK-INPUT LK-OFFSET LK-FLAG.
    MOVE 0 TO LK-FLAG
    CALL "JsonParse-SkipWhitespace" USING LK-INPUT LK-OFFSET
    IF LK-OFFSET > (LENGTH OF LK-INPUT) OR LK-INPUT(LK-OFFSET:1) NOT = "{"
        MOVE 1 TO LK-FLAG
        GOBACK
    END-IF
    ADD 1 TO LK-OFFSET
    GOBACK.

END PROGRAM JsonParse-ObjectStart.

*> --- JsonParse-ObjectEnd ---
IDENTIFICATION DIVISION.
PROGRAM-ID. JsonParse-ObjectEnd.

DATA DIVISION.
LINKAGE SECTION.
    01 LK-INPUT         PIC X ANY LENGTH.
    01 LK-OFFSET        BINARY-LONG UNSIGNED.
    01 LK-FLAG          BINARY-CHAR UNSIGNED.

PROCEDURE DIVISION USING LK-INPUT LK-OFFSET LK-FLAG.
    MOVE 0 TO LK-FLAG
    CALL "JsonParse-SkipWhitespace" USING LK-INPUT LK-OFFSET
    IF LK-OFFSET > (LENGTH OF LK-INPUT) OR LK-INPUT(LK-OFFSET:1) NOT = "}"
        MOVE 1 TO LK-FLAG
        GOBACK
    END-IF
    ADD 1 TO LK-OFFSET
    GOBACK.

END PROGRAM JsonParse-ObjectEnd.

*> --- JsonParse-ArrayStart ---
IDENTIFICATION DIVISION.
PROGRAM-ID. JsonParse-ArrayStart.

DATA DIVISION.
LINKAGE SECTION.
    01 LK-INPUT         PIC X ANY LENGTH.
    01 LK-OFFSET        BINARY-LONG UNSIGNED.
    01 LK-FLAG          BINARY-CHAR UNSIGNED.

PROCEDURE DIVISION USING LK-INPUT LK-OFFSET LK-FLAG.
    MOVE 0 TO LK-FLAG
    CALL "JsonParse-SkipWhitespace" USING LK-INPUT LK-OFFSET
    IF LK-OFFSET > (LENGTH OF LK-INPUT) OR LK-INPUT(LK-OFFSET:1) NOT = "["
        MOVE 1 TO LK-FLAG
        GOBACK
    END-IF
    ADD 1 TO LK-OFFSET
    GOBACK.

END PROGRAM JsonParse-ArrayStart.

*> --- JsonParse-ArrayEnd ---
IDENTIFICATION DIVISION.
PROGRAM-ID. JsonParse-ArrayEnd.

DATA DIVISION.
LINKAGE SECTION.
    01 LK-INPUT         PIC X ANY LENGTH.
    01 LK-OFFSET        BINARY-LONG UNSIGNED.
    01 LK-FLAG          BINARY-CHAR UNSIGNED.

PROCEDURE DIVISION USING LK-INPUT LK-OFFSET LK-FLAG.
    MOVE 0 TO LK-FLAG
    CALL "JsonParse-SkipWhitespace" USING LK-INPUT LK-OFFSET
    IF LK-OFFSET > (LENGTH OF LK-INPUT) OR LK-INPUT(LK-OFFSET:1) NOT = "]"
        MOVE 1 TO LK-FLAG
        GOBACK
    END-IF
    ADD 1 TO LK-OFFSET
    GOBACK.

END PROGRAM JsonParse-ArrayEnd.

*> --- JsonParse-Comma ---
IDENTIFICATION DIVISION.
PROGRAM-ID. JsonParse-Comma.

DATA DIVISION.
LINKAGE SECTION.
    01 LK-INPUT         PIC X ANY LENGTH.
    01 LK-OFFSET        BINARY-LONG UNSIGNED.
    01 LK-FLAG          BINARY-CHAR UNSIGNED.

PROCEDURE DIVISION USING LK-INPUT LK-OFFSET LK-FLAG.
    MOVE 0 TO LK-FLAG
    CALL "JsonParse-SkipWhitespace" USING LK-INPUT LK-OFFSET
    IF LK-OFFSET > (LENGTH OF LK-INPUT) OR LK-INPUT(LK-OFFSET:1) NOT = ","
        MOVE 1 TO LK-FLAG
        GOBACK
    END-IF
    ADD 1 TO LK-OFFSET
    GOBACK.

END PROGRAM JsonParse-Comma.

*> --- JsonParse-String ---
IDENTIFICATION DIVISION.
PROGRAM-ID. JsonParse-String.

DATA DIVISION.
LOCAL-STORAGE SECTION.
    01 I                BINARY-LONG UNSIGNED.
    01 CHARCODE         BINARY-CHAR UNSIGNED.
    01 ESCAPING         BINARY-CHAR UNSIGNED    VAlUE 0.
LINKAGE SECTION.
    01 LK-INPUT         PIC X ANY LENGTH.
    01 LK-OFFSET        BINARY-LONG UNSIGNED.
    01 LK-FLAG          BINARY-CHAR UNSIGNED.
    01 LK-STRING        PIC X ANY LENGTH.

PROCEDURE DIVISION USING LK-INPUT LK-OFFSET LK-FLAG LK-STRING.
    MOVE 0 TO LK-FLAG
    CALL "JsonParse-SkipWhitespace" USING LK-INPUT LK-OFFSET
    *> consume the start quote
    IF LK-OFFSET > (LENGTH OF LK-INPUT) OR LK-INPUT(LK-OFFSET:1) NOT = '"'
        MOVE 1 TO LK-FLAG
        GOBACK
    END-IF
    ADD 1 TO LK-OFFSET
    *> string content
    MOVE SPACES TO LK-STRING
    MOVE 1 TO I
    PERFORM UNTIL LK-OFFSET > LENGTH OF LK-INPUT
        COMPUTE CHARCODE = FUNCTION ORD(LK-INPUT(LK-OFFSET:1)) - 1
        IF ESCAPING = 1
            EVALUATE CHARCODE
                WHEN 34 *> quote
                    MOVE '"' TO LK-STRING(I:1)
                WHEN 92 *> backslash
                    MOVE "\" TO LK-STRING(I:1)
                WHEN 47 *> slash
                    MOVE "/" TO LK-STRING(I:1)
                WHEN 98 *> backspace
                    MOVE FUNCTION CHAR(9) TO LK-STRING(I:1)
                WHEN 102 *> formfeed
                    MOVE FUNCTION CHAR(13) TO LK-STRING(I:1)
                WHEN 110 *> newline
                    MOVE FUNCTION CHAR(11) TO LK-STRING(I:1)
                WHEN 114 *> carriage return
                    MOVE FUNCTION CHAR(14) TO LK-STRING(I:1)
                WHEN 116 *> tab
                    MOVE FUNCTION CHAR(10) TO LK-STRING(I:1)
                WHEN 117 *> unicode
                    ADD 1 TO LK-OFFSET
                    CALL "JsonParse-String-Unicode" USING LK-INPUT LK-OFFSET LK-FLAG CHARCODE
                    IF LK-FLAG = 1
                        GOBACK
                    END-IF
                    MOVE FUNCTION CHAR(CHARCODE + 1) TO LK-STRING(I:1)
                    ADD 1 TO I
                WHEN OTHER
                    MOVE 1 TO LK-FLAG
                    GOBACK
            END-EVALUATE
            IF CHARCODE NOT = 117
                ADD 1 TO LK-OFFSET
                ADD 1 TO I
            END-IF
            MOVE 0 TO ESCAPING
        ELSE
            EVALUATE CHARCODE
                WHEN 34 *> quote
                    EXIT PERFORM
                WHEN 92 *> backslash
                    MOVE 1 TO ESCAPING
                    ADD 1 TO LK-OFFSET
                WHEN OTHER
                    MOVE LK-INPUT(LK-OFFSET:1) TO LK-STRING(I:1)
                    ADD 1 TO LK-OFFSET
                    ADD 1 TO I
            END-EVALUATE
        END-IF
    END-PERFORM
    *> consume the end quote
    IF LK-OFFSET > (LENGTH OF LK-INPUT) OR LK-INPUT(LK-OFFSET:1) NOT = '"'
        MOVE 1 TO LK-FLAG
        GOBACK
    END-IF
    ADD 1 TO LK-OFFSET
    GOBACK.

    *> --- JsonParse-String-Unicode ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. JsonParse-String-Unicode.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        01 CURRENT-CHAR     PIC X.
        01 NIBBLE           BINARY-CHAR UNSIGNED.
    LINKAGE SECTION.
        01 LK-INPUT         PIC X ANY LENGTH.
        01 LK-OFFSET        BINARY-LONG UNSIGNED.
        01 LK-FLAG          BINARY-CHAR UNSIGNED.
        01 LK-CHARCODE      BINARY-CHAR UNSIGNED.

    PROCEDURE DIVISION USING LK-INPUT LK-OFFSET LK-FLAG LK-CHARCODE.
        MOVE 0 TO LK-CHARCODE
        PERFORM 4 TIMES
            IF LK-OFFSET > (LENGTH OF LK-INPUT)
                MOVE 1 TO LK-FLAG
                GOBACK
            END-IF
            MOVE LK-INPUT(LK-OFFSET:1) TO CURRENT-CHAR
            CALL "DecodeHexChar" USING CURRENT-CHAR NIBBLE
            COMPUTE LK-CHARCODE = LK-CHARCODE * 16 + NIBBLE
            ADD 1 TO LK-OFFSET
        END-PERFORM
        GOBACK.

    END PROGRAM JsonParse-String-Unicode.

END PROGRAM JsonParse-String.

*> --- JsonParse-ObjectKey ---
IDENTIFICATION DIVISION.
PROGRAM-ID. JsonParse-ObjectKey.

DATA DIVISION.
LINKAGE SECTION.
    01 LK-INPUT         PIC X ANY LENGTH.
    01 LK-OFFSET        BINARY-LONG UNSIGNED.
    01 LK-FLAG          BINARY-CHAR UNSIGNED.
    01 LK-KEY           PIC X ANY LENGTH.

PROCEDURE DIVISION USING LK-INPUT LK-OFFSET LK-FLAG LK-KEY.
    MOVE 0 TO LK-FLAG
    CALL "JsonParse-String" USING LK-INPUT LK-OFFSET LK-FLAG LK-KEY
    IF LK-FLAG = 1
        GOBACK
    END-IF
    CALL "JsonParse-SkipWhitespace" USING LK-INPUT LK-OFFSET
    IF LK-OFFSET > (LENGTH OF LK-INPUT) OR LK-INPUT(LK-OFFSET:1) NOT = ":"
        MOVE 1 TO LK-FLAG
        GOBACK
    END-IF
    ADD 1 TO LK-OFFSET
    GOBACK.

END PROGRAM JsonParse-ObjectKey.

*> --- JsonParse-Null ---
IDENTIFICATION DIVISION.
PROGRAM-ID. JsonParse-Null.

DATA DIVISION.
LINKAGE SECTION.
    01 LK-INPUT         PIC X ANY LENGTH.
    01 LK-OFFSET        BINARY-LONG UNSIGNED.
    01 LK-FLAG          BINARY-CHAR UNSIGNED.

PROCEDURE DIVISION USING LK-INPUT LK-OFFSET LK-FLAG.
    MOVE 0 TO LK-FLAG
    CALL "JsonParse-SkipWhitespace" USING LK-INPUT LK-OFFSET
    *> This may seem redundant; however, I'm pretty sure there is a compiler bug here, as if we check for
    *> LK-INPUT(LK-OFFSET:4) directly, GnuCOBOL complains about "length of 'LK-INPUT' out of bounds: 4".
    *> Meanwhile, doing it this way works fine.
    IF LK-OFFSET + 3 > (LENGTH OF LK-INPUT) OR NOT (LK-INPUT(LK-OFFSET:1) = "n" AND LK-INPUT(LK-OFFSET + 1:1) = "u" AND LK-INPUT(LK-OFFSET + 2:1) = "l" AND LK-INPUT(LK-OFFSET + 3:1) = "l")
        MOVE 1 TO LK-FLAG
        GOBACK
    END-IF
    ADD 4 TO LK-OFFSET
    GOBACK.

END PROGRAM JsonParse-Null.

*> --- JsonParse-Boolean ---
IDENTIFICATION DIVISION.
PROGRAM-ID. JsonParse-Boolean.

DATA DIVISION.
LINKAGE SECTION.
    01 LK-INPUT         PIC X ANY LENGTH.
    01 LK-OFFSET        BINARY-LONG UNSIGNED.
    01 LK-FLAG          BINARY-CHAR UNSIGNED.
    01 LK-VALUE         BINARY-CHAR UNSIGNED.

PROCEDURE DIVISION USING LK-INPUT LK-OFFSET LK-FLAG LK-VALUE.
    MOVE 0 TO LK-FLAG
    CALL "JsonParse-SkipWhitespace" USING LK-INPUT LK-OFFSET
    IF LK-OFFSET > (LENGTH OF LK-INPUT)
        MOVE 1 TO LK-FLAG
        GOBACK
    END-IF
    *> See note in JsonParse-Null regarding the length check
    EVALUATE LK-INPUT(LK-OFFSET:1)
        WHEN "t"
            IF LK-OFFSET + 3 > (LENGTH OF LK-INPUT) OR NOT (LK-INPUT(LK-OFFSET + 1:1) = "r" AND LK-INPUT(LK-OFFSET + 2:1) = "u" AND LK-INPUT(LK-OFFSET + 3:1) = "e")
                MOVE 1 TO LK-FLAG
                GOBACK
            END-IF
            MOVE 1 TO LK-VALUE
            ADD 4 TO LK-OFFSET
        WHEN "f"
            IF LK-OFFSET + 4 > (LENGTH OF LK-INPUT) OR NOT (LK-INPUT(LK-OFFSET + 1:1) = "a" AND LK-INPUT(LK-OFFSET + 2:1) = "l" AND LK-INPUT(LK-OFFSET + 3:1) = "s" AND LK-INPUT(LK-OFFSET + 4:1) = "e")
                MOVE 1 TO LK-FLAG
                GOBACK
            END-IF
            MOVE 0 TO LK-VALUE
            ADD 5 TO LK-OFFSET
        WHEN OTHER
            MOVE 1 TO LK-FLAG
    END-EVALUATE
    GOBACK.

END PROGRAM JsonParse-Boolean.

*> --- JsonParse-Integer ---
IDENTIFICATION DIVISION.
PROGRAM-ID. JsonParse-Integer.

DATA DIVISION.
LOCAL-STORAGE SECTION.
    01 VALUE-SIGN       BINARY-CHAR             VALUE 1.
    01 VALUE-ABS        BINARY-LONG UNSIGNED    VALUE 0.
    01 CHAR-COUNT       BINARY-LONG UNSIGNED    VALUE 0.
    01 CHARCODE         BINARY-CHAR UNSIGNED.
LINKAGE SECTION.
    01 LK-INPUT         PIC X ANY LENGTH.
    01 LK-OFFSET        BINARY-LONG UNSIGNED.
    01 LK-FLAG          BINARY-CHAR UNSIGNED.
    01 LK-VALUE         BINARY-LONG.

PROCEDURE DIVISION USING LK-INPUT LK-OFFSET LK-FLAG LK-VALUE.
    MOVE 0 TO LK-FLAG
    CALL "JsonParse-SkipWhitespace" USING LK-INPUT LK-OFFSET
    IF LK-OFFSET > (LENGTH OF LK-INPUT)
        MOVE 1 TO LK-FLAG
        GOBACK
    END-IF
    *> check if negative
    IF LK-INPUT(LK-OFFSET:1) = "-"
        MOVE -1 TO VALUE-SIGN
        ADD 1 TO LK-OFFSET
    END-IF
    *> read digits and convert to unsigned integer
    PERFORM UNTIL LK-OFFSET > LENGTH OF LK-INPUT
        COMPUTE CHARCODE = FUNCTION ORD(LK-INPUT(LK-OFFSET:1)) - 1
        *> exit the loop once a non-digit character is found
        IF CHARCODE < 48 OR CHARCODE > 57
            EXIT PERFORM
        END-IF
        COMPUTE VALUE-ABS = VALUE-ABS * 10 + CHARCODE - 48
        ADD 1 TO LK-OFFSET
        ADD 1 TO CHAR-COUNT
    END-PERFORM
    *> check if at least one digit was found
    IF CHAR-COUNT = 0
        MOVE 1 TO LK-FLAG
        GOBACK
    END-IF
    *> comoute the final value
    COMPUTE LK-VALUE = VALUE-SIGN * VALUE-ABS
    GOBACK.

END PROGRAM JsonParse-Integer.

*> --- JsonParse-Float ---
IDENTIFICATION DIVISION.
PROGRAM-ID. JsonParse-Float.

DATA DIVISION.
LOCAL-STORAGE SECTION.
    01 INT-VALUE        BINARY-LONG.
    01 CHARCODE         BINARY-CHAR UNSIGNED.
    01 CHAR-COUNT       BINARY-LONG UNSIGNED    VALUE 0.
    01 MULTIPLIER       FLOAT-LONG.
LINKAGE SECTION.
    01 LK-INPUT         PIC X ANY LENGTH.
    01 LK-OFFSET        BINARY-LONG UNSIGNED.
    01 LK-FLAG          BINARY-CHAR UNSIGNED.
    01 LK-VALUE         FLOAT-LONG.

PROCEDURE DIVISION USING LK-INPUT LK-OFFSET LK-FLAG LK-VALUE.
    *> TODO support scientific notation
    *> start by reading the integer part
    CALL "JsonParse-Integer" USING LK-INPUT LK-OFFSET LK-FLAG INT-VALUE
    MOVE INT-VALUE TO LK-VALUE
    *> abort on error, or if there is no decimal point
    IF LK-FLAG NOT = 0 OR LK-OFFSET > (LENGTH OF LK-INPUT) OR LK-INPUT(LK-OFFSET:1) NOT = "."
        GOBACK
    END-IF
    ADD 1 TO LK-OFFSET
    *> read digits after the decimal point
    IF LK-VALUE < 0
        MOVE -0.1 TO MULTIPLIER
    ELSE
        MOVE 0.1 TO MULTIPLIER
    END-IF
    PERFORM UNTIL LK-OFFSET > (LENGTH OF LK-INPUT)
        COMPUTE CHARCODE = FUNCTION ORD(LK-INPUT(LK-OFFSET:1)) - 1
        *> exit the loop once a non-digit character is found
        IF CHARCODE < 48 OR CHARCODE > 57
            EXIT PERFORM
        END-IF
        COMPUTE LK-VALUE = LK-VALUE + (CHARCODE - 48) * MULTIPLIER
        COMPUTE MULTIPLIER = MULTIPLIER / 10
        ADD 1 TO LK-OFFSET
        ADD 1 TO CHAR-COUNT
    END-PERFORM
    *> check if at least one digit was found
    IF CHAR-COUNT = 0
        MOVE 1 TO LK-FLAG
        GOBACK
    END-IF
    GOBACK.

END PROGRAM JsonParse-Float.

*> --- JsonParse-SkipValue ---
IDENTIFICATION DIVISION.
PROGRAM-ID. JsonParse-SkipValue IS RECURSIVE.

DATA DIVISION.
LOCAL-STORAGE SECTION.
    01 EXIT-LOOP        BINARY-CHAR UNSIGNED.
    01 DUMMY-STR        PIC X(1000).
    01 DUMMY-FLOAT      FLOAT-LONG.
    01 DUMMY-BOOL       BINARY-CHAR UNSIGNED.
LINKAGE SECTION.
    01 LK-INPUT         PIC X ANY LENGTH.
    01 LK-OFFSET        BINARY-LONG UNSIGNED.
    01 LK-FLAG          BINARY-CHAR UNSIGNED.

PROCEDURE DIVISION USING LK-INPUT LK-OFFSET LK-FLAG.
    MOVE 0 TO LK-FLAG

    CALL "JsonParse-SkipWhitespace" USING LK-INPUT LK-OFFSET
    IF LK-OFFSET > (LENGTH OF LK-INPUT)
        MOVE 1 TO LK-FLAG
        GOBACK
    END-IF

    *> Determine the type of the value and skip it
    EVALUATE LK-INPUT(LK-OFFSET:1)
        WHEN "t" *> true
            CALL "JsonParse-Boolean" USING LK-INPUT LK-OFFSET LK-FLAG DUMMY-BOOL

        WHEN "f" *> false
            CALL "JsonParse-Boolean" USING LK-INPUT LK-OFFSET LK-FLAG DUMMY-BOOL

        WHEN "n" *> null
            CALL "JsonParse-Null" USING LK-INPUT LK-OFFSET LK-FLAG

        WHEN '"' *> string
            CALL "JsonParse-String" USING LK-INPUT LK-OFFSET LK-FLAG DUMMY-STR

        WHEN "-" *> float or integer
            CALL "JsonParse-Float" USING LK-INPUT LK-OFFSET LK-FLAG DUMMY-FLOAT

        WHEN "0" THRU "9" *> float or integer
            CALL "JsonParse-Float" USING LK-INPUT LK-OFFSET LK-FLAG DUMMY-FLOAT

        *> TODO: implement floating point numbers

        WHEN "[" *> array
            CALL "JsonParse-ArrayStart" USING LK-INPUT LK-OFFSET LK-FLAG
            IF LK-FLAG = 1
                GOBACK
            END-IF

            *> Consume the closing bracket, if possible
            CALL "JsonParse-ArrayEnd" USING LK-INPUT LK-OFFSET LK-FLAG
            IF LK-FLAG = 0
                GOBACK
            END-IF
            MOVE 0 TO LK-FLAG

            *> Error consuming closing bracket - parse array content
            PERFORM UNTIL LK-FLAG = 1
                CALL "JsonParse-SkipValue" USING LK-INPUT LK-OFFSET LK-FLAG
                IF LK-FLAG = 1
                    GOBACK
                END-IF
                *> If there is a comma, consume it and continue the loop; otherwise, exit
                CALL "JsonParse-Comma" USING LK-INPUT LK-OFFSET EXIT-LOOP
                IF EXIT-LOOP = 1
                    EXIT PERFORM
                END-IF
            END-PERFORM

            *> Consume the closing bracket
            CALL "JsonParse-ArrayEnd" USING LK-INPUT LK-OFFSET LK-FLAG

        WHEN "{" *> object
            CALL "JsonParse-ObjectStart" USING LK-INPUT LK-OFFSET LK-FLAG
            IF LK-FLAG = 1
                GOBACK
            END-IF

            *> Consume the closing brace, if possible
            CALL "JsonParse-ObjectEnd" USING LK-INPUT LK-OFFSET LK-FLAG
            IF LK-FLAG = 0
                GOBACK
            END-IF
            MOVE 0 TO LK-FLAG

            *> Error consuming closing brace - parse object content
            PERFORM UNTIL LK-FLAG = 1
                CALL "JsonParse-ObjectKey" USING LK-INPUT LK-OFFSET LK-FLAG DUMMY-STR
                IF LK-FLAG = 1
                    GOBACK
                END-IF
                CALL "JsonParse-SkipValue" USING LK-INPUT LK-OFFSET LK-FLAG
                IF LK-FLAG = 1
                    GOBACK
                END-IF
                *> If there is a comma, consume it and continue the loop; otherwise, exit
                CALL "JsonParse-Comma" USING LK-INPUT LK-OFFSET EXIT-LOOP
                IF EXIT-LOOP = 1
                    EXIT PERFORM
                END-IF
            END-PERFORM

            *> Consume the closing brace
            CALL "JsonParse-ObjectEnd" USING LK-INPUT LK-OFFSET LK-FLAG

        WHEN OTHER
            MOVE 1 TO LK-FLAG
    END-EVALUATE

    GOBACK.

END PROGRAM JsonParse-SkipValue.
