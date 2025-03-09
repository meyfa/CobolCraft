*> --- JsonParse-ObjectStart ---
IDENTIFICATION DIVISION.
PROGRAM-ID. JsonParse-ObjectStart.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 INPUT-LENGTH     BINARY-LONG UNSIGNED.
LINKAGE SECTION.
    01 LK-INPUT         PIC X ANY LENGTH.
    01 LK-OFFSET        BINARY-LONG UNSIGNED.
    01 LK-FLAG          BINARY-CHAR UNSIGNED.

PROCEDURE DIVISION USING LK-INPUT LK-OFFSET LK-FLAG.
    MOVE FUNCTION LENGTH(LK-INPUT) TO INPUT-LENGTH
    COPY PROC-SKIP-WHITESPACE REPLACING ==STR== BY ==LK-INPUT== ==LEN== BY ==INPUT-LENGTH== ==IDX== BY ==LK-OFFSET==.
    IF LK-OFFSET > INPUT-LENGTH OR LK-INPUT(LK-OFFSET:1) NOT = "{"
        MOVE 1 TO LK-FLAG
        GOBACK
    END-IF
    MOVE 0 TO LK-FLAG
    ADD 1 TO LK-OFFSET
    GOBACK.

END PROGRAM JsonParse-ObjectStart.

*> --- JsonParse-ObjectEnd ---
IDENTIFICATION DIVISION.
PROGRAM-ID. JsonParse-ObjectEnd.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 INPUT-LENGTH     BINARY-LONG UNSIGNED.
LINKAGE SECTION.
    01 LK-INPUT         PIC X ANY LENGTH.
    01 LK-OFFSET        BINARY-LONG UNSIGNED.
    01 LK-FLAG          BINARY-CHAR UNSIGNED.

PROCEDURE DIVISION USING LK-INPUT LK-OFFSET LK-FLAG.
    MOVE FUNCTION LENGTH(LK-INPUT) TO INPUT-LENGTH
    COPY PROC-SKIP-WHITESPACE REPLACING ==STR== BY ==LK-INPUT== ==LEN== BY ==INPUT-LENGTH== ==IDX== BY ==LK-OFFSET==.
    IF LK-OFFSET > INPUT-LENGTH OR LK-INPUT(LK-OFFSET:1) NOT = "}"
        MOVE 1 TO LK-FLAG
        GOBACK
    END-IF
    MOVE 0 TO LK-FLAG
    ADD 1 TO LK-OFFSET
    GOBACK.

END PROGRAM JsonParse-ObjectEnd.

*> --- JsonParse-ArrayStart ---
IDENTIFICATION DIVISION.
PROGRAM-ID. JsonParse-ArrayStart.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 INPUT-LENGTH     BINARY-LONG UNSIGNED.
LINKAGE SECTION.
    01 LK-INPUT         PIC X ANY LENGTH.
    01 LK-OFFSET        BINARY-LONG UNSIGNED.
    01 LK-FLAG          BINARY-CHAR UNSIGNED.

PROCEDURE DIVISION USING LK-INPUT LK-OFFSET LK-FLAG.
    MOVE FUNCTION LENGTH(LK-INPUT) TO INPUT-LENGTH
    COPY PROC-SKIP-WHITESPACE REPLACING ==STR== BY ==LK-INPUT== ==LEN== BY ==INPUT-LENGTH== ==IDX== BY ==LK-OFFSET==.
    IF LK-OFFSET > INPUT-LENGTH OR LK-INPUT(LK-OFFSET:1) NOT = "["
        MOVE 1 TO LK-FLAG
        GOBACK
    END-IF
    MOVE 0 TO LK-FLAG
    ADD 1 TO LK-OFFSET
    GOBACK.

END PROGRAM JsonParse-ArrayStart.

*> --- JsonParse-ArrayEnd ---
IDENTIFICATION DIVISION.
PROGRAM-ID. JsonParse-ArrayEnd.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 INPUT-LENGTH     BINARY-LONG UNSIGNED.
LINKAGE SECTION.
    01 LK-INPUT         PIC X ANY LENGTH.
    01 LK-OFFSET        BINARY-LONG UNSIGNED.
    01 LK-FLAG          BINARY-CHAR UNSIGNED.

PROCEDURE DIVISION USING LK-INPUT LK-OFFSET LK-FLAG.
    MOVE FUNCTION LENGTH(LK-INPUT) TO INPUT-LENGTH
    COPY PROC-SKIP-WHITESPACE REPLACING ==STR== BY ==LK-INPUT== ==LEN== BY ==INPUT-LENGTH== ==IDX== BY ==LK-OFFSET==.
    IF LK-OFFSET > INPUT-LENGTH OR LK-INPUT(LK-OFFSET:1) NOT = "]"
        MOVE 1 TO LK-FLAG
        GOBACK
    END-IF
    MOVE 0 TO LK-FLAG
    ADD 1 TO LK-OFFSET
    GOBACK.

END PROGRAM JsonParse-ArrayEnd.

*> --- JsonParse-Comma ---
IDENTIFICATION DIVISION.
PROGRAM-ID. JsonParse-Comma.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 INPUT-LENGTH     BINARY-LONG UNSIGNED.
LINKAGE SECTION.
    01 LK-INPUT         PIC X ANY LENGTH.
    01 LK-OFFSET        BINARY-LONG UNSIGNED.
    01 LK-FLAG          BINARY-CHAR UNSIGNED.

PROCEDURE DIVISION USING LK-INPUT LK-OFFSET LK-FLAG.
    MOVE FUNCTION LENGTH(LK-INPUT) TO INPUT-LENGTH
    COPY PROC-SKIP-WHITESPACE REPLACING ==STR== BY ==LK-INPUT== ==LEN== BY ==INPUT-LENGTH== ==IDX== BY ==LK-OFFSET==.
    IF LK-OFFSET > INPUT-LENGTH OR LK-INPUT(LK-OFFSET:1) NOT = ","
        MOVE 1 TO LK-FLAG
        GOBACK
    END-IF
    MOVE 0 TO LK-FLAG
    ADD 1 TO LK-OFFSET
    GOBACK.

END PROGRAM JsonParse-Comma.

*> --- JsonParse-String ---
IDENTIFICATION DIVISION.
PROGRAM-ID. JsonParse-String.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 INPUT-LENGTH     BINARY-LONG UNSIGNED.
    01 CHAR-ALPHA.
        02 CHARCODE         BINARY-CHAR UNSIGNED.
LOCAL-STORAGE SECTION.
    01 I                BINARY-LONG UNSIGNED    VALUE 1.
    01 ESCAPING         BINARY-CHAR UNSIGNED    VALUE 0.
LINKAGE SECTION.
    01 LK-INPUT         PIC X ANY LENGTH.
    01 LK-OFFSET        BINARY-LONG UNSIGNED.
    01 LK-FLAG          BINARY-CHAR UNSIGNED.
    01 LK-STRING        PIC X ANY LENGTH.

PROCEDURE DIVISION USING LK-INPUT LK-OFFSET LK-FLAG LK-STRING.
    MOVE FUNCTION LENGTH(LK-INPUT) TO INPUT-LENGTH
    COPY PROC-SKIP-WHITESPACE REPLACING ==STR== BY ==LK-INPUT== ==LEN== BY ==INPUT-LENGTH== ==IDX== BY ==LK-OFFSET==.
    *> consume the start quote
    IF LK-OFFSET > INPUT-LENGTH OR LK-INPUT(LK-OFFSET:1) NOT = '"'
        MOVE 1 TO LK-FLAG
        GOBACK
    END-IF
    MOVE 0 TO LK-FLAG
    ADD 1 TO LK-OFFSET
    *> string content
    MOVE SPACES TO LK-STRING
    PERFORM UNTIL LK-OFFSET > INPUT-LENGTH
        MOVE LK-INPUT(LK-OFFSET:1) TO CHAR-ALPHA
        IF ESCAPING = 1
            EVALUATE CHARCODE
                WHEN 34 *> quote
                    MOVE '"' TO LK-STRING(I:1)
                WHEN 47 *> slash
                    MOVE "/" TO LK-STRING(I:1)
                WHEN 92 *> backslash
                    MOVE "\" TO LK-STRING(I:1)
                WHEN 98 *> backspace
                    MOVE X"08" TO LK-STRING(I:1)
                WHEN 102 *> formfeed
                    MOVE X"0C" TO LK-STRING(I:1)
                WHEN 110 *> newline
                    MOVE X"0A" TO LK-STRING(I:1)
                WHEN 114 *> carriage return
                    MOVE X"0D" TO LK-STRING(I:1)
                WHEN 116 *> tab
                    MOVE X"09" TO LK-STRING(I:1)
                WHEN 117 *> unicode
                    ADD 1 TO LK-OFFSET
                    IF LK-OFFSET + 4 > INPUT-LENGTH
                        MOVE 1 TO LK-FLAG
                        GOBACK
                    END-IF
                    CALL "JsonParse-String-Unicode" USING LK-INPUT LK-OFFSET CHARCODE
                    MOVE FUNCTION CHAR(CHARCODE + 1) TO LK-STRING(I:1)
                    SUBTRACT 1 FROM LK-OFFSET
                WHEN OTHER
                    MOVE 1 TO LK-FLAG
                    GOBACK
            END-EVALUATE
            ADD 1 TO LK-OFFSET
            ADD 1 TO I
            MOVE 0 TO ESCAPING
        ELSE
            EVALUATE CHARCODE
                WHEN 34 *> end quote
                    ADD 1 TO LK-OFFSET
                    GOBACK
                WHEN 92 *> backslash
                    MOVE 1 TO ESCAPING
                WHEN OTHER
                    MOVE LK-INPUT(LK-OFFSET:1) TO LK-STRING(I:1)
                    ADD 1 TO I
            END-EVALUATE
            ADD 1 TO LK-OFFSET
        END-IF
    END-PERFORM
    *> end quote not found
    MOVE 1 TO LK-FLAG
    GOBACK.

    *> --- JsonParse-String-Unicode ---
    *> Note: It is the caller's responsibility to ensure that the input contains at least 4 characters after the offset.
    IDENTIFICATION DIVISION.
    PROGRAM-ID. JsonParse-String-Unicode.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        01 NIBBLE           BINARY-CHAR UNSIGNED.
    LINKAGE SECTION.
        01 LK-INPUT         PIC X ANY LENGTH.
        01 LK-OFFSET        BINARY-LONG UNSIGNED.
        01 LK-CHARCODE      BINARY-CHAR UNSIGNED.

    PROCEDURE DIVISION USING LK-INPUT LK-OFFSET LK-CHARCODE.
        MOVE 0 TO LK-CHARCODE
        PERFORM 4 TIMES
            CALL "DecodeHexChar" USING LK-INPUT(LK-OFFSET:1) NIBBLE
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
WORKING-STORAGE SECTION.
    01 INPUT-LENGTH     BINARY-LONG UNSIGNED.
LINKAGE SECTION.
    01 LK-INPUT         PIC X ANY LENGTH.
    01 LK-OFFSET        BINARY-LONG UNSIGNED.
    01 LK-FLAG          BINARY-CHAR UNSIGNED.
    01 LK-KEY           PIC X ANY LENGTH.

PROCEDURE DIVISION USING LK-INPUT LK-OFFSET LK-FLAG LK-KEY.
    CALL "JsonParse-String" USING LK-INPUT LK-OFFSET LK-FLAG LK-KEY
    MOVE FUNCTION LENGTH(LK-INPUT) TO INPUT-LENGTH
    COPY PROC-SKIP-WHITESPACE REPLACING ==STR== BY ==LK-INPUT== ==LEN== BY ==INPUT-LENGTH== ==IDX== BY ==LK-OFFSET==.
    IF LK-OFFSET > INPUT-LENGTH OR LK-INPUT(LK-OFFSET:1) NOT = ":"
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
WORKING-STORAGE SECTION.
    01 INPUT-LENGTH     BINARY-LONG UNSIGNED.
LINKAGE SECTION.
    01 LK-INPUT         PIC X ANY LENGTH.
    01 LK-OFFSET        BINARY-LONG UNSIGNED.
    01 LK-FLAG          BINARY-CHAR UNSIGNED.

PROCEDURE DIVISION USING LK-INPUT LK-OFFSET LK-FLAG.
    MOVE FUNCTION LENGTH(LK-INPUT) TO INPUT-LENGTH
    COPY PROC-SKIP-WHITESPACE REPLACING ==STR== BY ==LK-INPUT== ==LEN== BY ==INPUT-LENGTH== ==IDX== BY ==LK-OFFSET==.
    *> This parser doesn't need to be strict. Once we see "n", assume the rest is correct.
    IF LK-OFFSET + 3 > INPUT-LENGTH OR LK-INPUT(LK-OFFSET:1) NOT = "n"
        MOVE 1 TO LK-FLAG
        GOBACK
    END-IF
    MOVE 0 TO LK-FLAG
    ADD 4 TO LK-OFFSET
    GOBACK.

END PROGRAM JsonParse-Null.

*> --- JsonParse-Boolean ---
IDENTIFICATION DIVISION.
PROGRAM-ID. JsonParse-Boolean.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 INPUT-LENGTH     BINARY-LONG UNSIGNED.
LINKAGE SECTION.
    01 LK-INPUT         PIC X ANY LENGTH.
    01 LK-OFFSET        BINARY-LONG UNSIGNED.
    01 LK-FLAG          BINARY-CHAR UNSIGNED.
    01 LK-VALUE         BINARY-CHAR UNSIGNED.

PROCEDURE DIVISION USING LK-INPUT LK-OFFSET LK-FLAG LK-VALUE.
    MOVE FUNCTION LENGTH(LK-INPUT) TO INPUT-LENGTH
    COPY PROC-SKIP-WHITESPACE REPLACING ==STR== BY ==LK-INPUT== ==LEN== BY ==INPUT-LENGTH== ==IDX== BY ==LK-OFFSET==.
    IF LK-OFFSET > INPUT-LENGTH
        MOVE 1 TO LK-FLAG
        GOBACK
    END-IF
    MOVE 0 TO LK-FLAG
    *> Same as with "null", we don't need to compare the whole string, but are satisfied with the first character.
    EVALUATE LK-INPUT(LK-OFFSET:1)
        WHEN "t"
            IF LK-OFFSET + 3 > INPUT-LENGTH
                MOVE 1 TO LK-FLAG
                GOBACK
            END-IF
            MOVE 1 TO LK-VALUE
            ADD 4 TO LK-OFFSET
        WHEN "f"
            IF LK-OFFSET + 4 > INPUT-LENGTH
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
WORKING-STORAGE SECTION.
    01 INPUT-LENGTH     BINARY-LONG UNSIGNED.
    01 CHAR-ALPHA.
        02 CHARCODE         BINARY-CHAR UNSIGNED.
LOCAL-STORAGE SECTION.
    01 VALUE-SIGN       BINARY-CHAR             VALUE 1.
    01 VALUE-ABS        BINARY-LONG UNSIGNED    VALUE 0.
LINKAGE SECTION.
    01 LK-INPUT         PIC X ANY LENGTH.
    01 LK-OFFSET        BINARY-LONG UNSIGNED.
    01 LK-FLAG          BINARY-CHAR UNSIGNED.
    01 LK-VALUE         BINARY-LONG.

PROCEDURE DIVISION USING LK-INPUT LK-OFFSET LK-FLAG LK-VALUE.
    MOVE FUNCTION LENGTH(LK-INPUT) TO INPUT-LENGTH
    COPY PROC-SKIP-WHITESPACE REPLACING ==STR== BY ==LK-INPUT== ==LEN== BY ==INPUT-LENGTH== ==IDX== BY ==LK-OFFSET==.
    IF LK-OFFSET > INPUT-LENGTH
        MOVE 1 TO LK-FLAG
        GOBACK
    END-IF
    *> check if negative
    IF LK-INPUT(LK-OFFSET:1) = "-"
        MOVE -1 TO VALUE-SIGN
        ADD 1 TO LK-OFFSET
    END-IF
    *> read digits and convert to unsigned integer
    MOVE 1 TO LK-FLAG
    PERFORM UNTIL LK-OFFSET > INPUT-LENGTH
        MOVE LK-INPUT(LK-OFFSET:1) TO CHAR-ALPHA
        *> exit the loop once a non-digit character is found
        IF CHARCODE < 48 OR CHARCODE > 57
            EXIT PERFORM
        END-IF
        COMPUTE VALUE-ABS = VALUE-ABS * 10 + CHARCODE - 48
        ADD 1 TO LK-OFFSET
        *> Found a digit, so reset the flag
        MOVE 0 TO LK-FLAG
    END-PERFORM
    *> compute the final value
    COMPUTE LK-VALUE = VALUE-SIGN * VALUE-ABS
    GOBACK.

END PROGRAM JsonParse-Integer.

*> --- JsonParse-Float ---
IDENTIFICATION DIVISION.
PROGRAM-ID. JsonParse-Float.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 INPUT-LENGTH     BINARY-LONG UNSIGNED.
    01 INT-VALUE        BINARY-LONG.
    01 CHAR-ALPHA.
        02 CHARCODE         BINARY-CHAR UNSIGNED.
    01 MULTIPLIER       FLOAT-LONG.
LINKAGE SECTION.
    01 LK-INPUT         PIC X ANY LENGTH.
    01 LK-OFFSET        BINARY-LONG UNSIGNED.
    01 LK-FLAG          BINARY-CHAR UNSIGNED.
    01 LK-VALUE         FLOAT-LONG.

PROCEDURE DIVISION USING LK-INPUT LK-OFFSET LK-FLAG LK-VALUE.
    MOVE FUNCTION LENGTH(LK-INPUT) TO INPUT-LENGTH
    *> start by reading the integer part
    CALL "JsonParse-Integer" USING LK-INPUT LK-OFFSET LK-FLAG INT-VALUE
    MOVE INT-VALUE TO LK-VALUE
    *> abort on error, or if there is no decimal point
    IF LK-FLAG NOT = 0 OR LK-OFFSET > INPUT-LENGTH OR LK-INPUT(LK-OFFSET:1) NOT = "."
        GOBACK
    END-IF
    ADD 1 TO LK-OFFSET
    *> read digits after the decimal point
    IF LK-VALUE < 0
        MOVE -0.1 TO MULTIPLIER
    ELSE
        MOVE 0.1 TO MULTIPLIER
    END-IF
    MOVE 1 TO LK-FLAG
    PERFORM UNTIL LK-OFFSET > INPUT-LENGTH
        MOVE LK-INPUT(LK-OFFSET:1) TO CHAR-ALPHA
        *> exit the loop once a non-digit character is found
        IF CHARCODE < 48 OR CHARCODE > 57
            EXIT PERFORM
        END-IF
        COMPUTE LK-VALUE = LK-VALUE + (CHARCODE - 48) * MULTIPLIER
        DIVIDE MULTIPLIER BY 10 GIVING MULTIPLIER
        ADD 1 TO LK-OFFSET
        *> Found a digit, so reset the flag
        MOVE 0 TO LK-FLAG
    END-PERFORM
    *> abort if there is no exponent
    IF LK-OFFSET > INPUT-LENGTH OR NOT (LK-INPUT(LK-OFFSET:1) = "e" OR LK-INPUT(LK-OFFSET:1) = "E")
        GOBACK
    END-IF
    ADD 1 TO LK-OFFSET
    *> ignore plus sign because integer parsing does not allow it
    IF LK-OFFSET <= INPUT-LENGTH AND LK-INPUT(LK-OFFSET:1) = "+"
        ADD 1 TO LK-OFFSET
    END-IF
    *> read the exponent
    CALL "JsonParse-Integer" USING LK-INPUT LK-OFFSET LK-FLAG INT-VALUE
    IF LK-FLAG NOT = 0
        GOBACK
    END-IF
    *> compute the final value
    COMPUTE LK-VALUE = LK-VALUE * (10 ** INT-VALUE)
    GOBACK.

END PROGRAM JsonParse-Float.

*> --- JsonParse-SkipValue ---
IDENTIFICATION DIVISION.
PROGRAM-ID. JsonParse-SkipValue IS RECURSIVE.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 INPUT-LENGTH     BINARY-LONG UNSIGNED.
    01 DUMMY-FLOAT      FLOAT-LONG.
LOCAL-STORAGE SECTION.
    01 EXIT-LOOP        BINARY-CHAR UNSIGNED    VALUE 0.
    01 ESCAPING         BINARY-CHAR UNSIGNED    VALUE 0.
LINKAGE SECTION.
    01 LK-INPUT         PIC X ANY LENGTH.
    01 LK-OFFSET        BINARY-LONG UNSIGNED.
    01 LK-FLAG          BINARY-CHAR UNSIGNED.

PROCEDURE DIVISION USING LK-INPUT LK-OFFSET LK-FLAG.
    MOVE FUNCTION LENGTH(LK-INPUT) TO INPUT-LENGTH

    COPY PROC-SKIP-WHITESPACE REPLACING ==STR== BY ==LK-INPUT== ==LEN== BY ==INPUT-LENGTH== ==IDX== BY ==LK-OFFSET==.
    IF LK-OFFSET > INPUT-LENGTH
        MOVE 1 TO LK-FLAG
        GOBACK
    END-IF

    MOVE 0 TO LK-FLAG

    *> Determine the type of the value and skip it
    EVALUATE LK-INPUT(LK-OFFSET:1)
        WHEN "t" *> true
            ADD 4 TO LK-OFFSET

        WHEN "f" *> false
            ADD 5 TO LK-OFFSET

        WHEN "n" *> null
            ADD 4 TO LK-OFFSET

        WHEN '"' *> string
            *> This is performance sensitive, so don't use JsonParse-String here, since we don't need the value.
            ADD 1 TO LK-OFFSET
            PERFORM UNTIL LK-OFFSET > INPUT-LENGTH
                EVALUATE LK-INPUT(LK-OFFSET:1)
                    WHEN '"'
                        ADD 1 TO LK-OFFSET
                        EXIT PERFORM
                    WHEN "\"
                        ADD 2 TO LK-OFFSET
                    WHEN OTHER
                        ADD 1 TO LK-OFFSET
                END-EVALUATE
            END-PERFORM

        WHEN "-" *> float or integer
            CALL "JsonParse-Float" USING LK-INPUT LK-OFFSET LK-FLAG DUMMY-FLOAT

        WHEN "0" THRU "9" *> float or integer
            CALL "JsonParse-Float" USING LK-INPUT LK-OFFSET LK-FLAG DUMMY-FLOAT

        WHEN "[" *> array
            ADD 1 TO LK-OFFSET

            *> Consume the closing bracket, if possible
            COPY PROC-SKIP-WHITESPACE REPLACING ==STR== BY ==LK-INPUT== ==LEN== BY ==INPUT-LENGTH== ==IDX== BY ==LK-OFFSET==.
            IF LK-OFFSET > INPUT-LENGTH
                MOVE 1 TO LK-FLAG
                GOBACK
            END-IF
            IF LK-INPUT(LK-OFFSET:1) = "]"
                ADD 1 TO LK-OFFSET
                GOBACK
            END-IF

            *> Array has content, skip it
            PERFORM UNTIL LK-FLAG = 1 OR EXIT-LOOP = 1
                CALL "JsonParse-SkipValue" USING LK-INPUT LK-OFFSET LK-FLAG
                *> If there is a comma, consume it and continue the loop; otherwise, exit
                CALL "JsonParse-Comma" USING LK-INPUT LK-OFFSET EXIT-LOOP
            END-PERFORM

            *> Consume the closing bracket. Whitespace was already skipped by JsonParse-Comma.
            IF LK-OFFSET > INPUT-LENGTH OR LK-INPUT(LK-OFFSET:1) NOT = "]"
                MOVE 1 TO LK-FLAG
                GOBACK
            END-IF
            ADD 1 TO LK-OFFSET

        WHEN "{" *> object
            ADD 1 TO LK-OFFSET

            *> Consume the closing brace, if possible
            COPY PROC-SKIP-WHITESPACE REPLACING ==STR== BY ==LK-INPUT== ==LEN== BY ==INPUT-LENGTH== ==IDX== BY ==LK-OFFSET==.
            IF LK-OFFSET > INPUT-LENGTH
                MOVE 1 TO LK-FLAG
                GOBACK
            END-IF
            IF LK-INPUT(LK-OFFSET:1) = "}"
                ADD 1 TO LK-OFFSET
                GOBACK
            END-IF

            *> Object has content, skip it
            PERFORM UNTIL LK-FLAG = 1 OR EXIT-LOOP = 1
                COPY PROC-SKIP-WHITESPACE REPLACING ==STR== BY ==LK-INPUT== ==LEN== BY ==INPUT-LENGTH== ==IDX== BY ==LK-OFFSET==.
                *> Skip the key
                IF LK-OFFSET > INPUT-LENGTH OR LK-INPUT(LK-OFFSET:1) NOT = '"'
                    MOVE 1 TO LK-FLAG
                    GOBACK
                END-IF
                ADD 1 TO LK-OFFSET
                PERFORM UNTIL LK-OFFSET > INPUT-LENGTH
                    EVALUATE LK-INPUT(LK-OFFSET:1)
                        WHEN '"'
                            ADD 1 TO LK-OFFSET
                            EXIT PERFORM
                        WHEN "\"
                            ADD 2 TO LK-OFFSET
                        WHEN OTHER
                            ADD 1 TO LK-OFFSET
                    END-EVALUATE
                END-PERFORM
                *> Skip whitespace and colon
                COPY PROC-SKIP-WHITESPACE REPLACING ==STR== BY ==LK-INPUT== ==LEN== BY ==INPUT-LENGTH== ==IDX== BY ==LK-OFFSET==.
                IF LK-OFFSET > INPUT-LENGTH OR LK-INPUT(LK-OFFSET:1) NOT = ":"
                    MOVE 1 TO LK-FLAG
                    GOBACK
                END-IF
                ADD 1 TO LK-OFFSET
                *> Skip the value
                CALL "JsonParse-SkipValue" USING LK-INPUT LK-OFFSET LK-FLAG
                *> If there is a comma, consume it and continue the loop; otherwise, exit
                CALL "JsonParse-Comma" USING LK-INPUT LK-OFFSET EXIT-LOOP
            END-PERFORM

            *> Consume the closing brace. Whitespace was already skipped by JsonParse-Comma.
            IF LK-OFFSET > INPUT-LENGTH OR LK-INPUT(LK-OFFSET:1) NOT = "}"
                MOVE 1 TO LK-FLAG
                GOBACK
            END-IF
            ADD 1 TO LK-OFFSET

        WHEN OTHER
            MOVE 1 TO LK-FLAG
    END-EVALUATE

    GOBACK.

END PROGRAM JsonParse-SkipValue.

*> --- JsonParse-FindValue ---
*> Iterate the JSON object and find the value of the given key. The absolute offset of the value is returned, so it
*> can then be read by the caller. If the key is not found, the offset is not changed.
*>
*> NOTE: This should not be called repeatedly on the same object. Use a PERFORM loop instead.
IDENTIFICATION DIVISION.
PROGRAM-ID. JsonParse-FindValue.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 OFFSET           BINARY-LONG UNSIGNED.
    01 STR              PIC X(256).
LINKAGE SECTION.
    01 LK-INPUT         PIC X ANY LENGTH.
    01 LK-OFFSET        BINARY-LONG UNSIGNED.
    01 LK-KEY           PIC X ANY LENGTH.
    01 LK-FLAG          BINARY-CHAR UNSIGNED.

PROCEDURE DIVISION USING LK-INPUT LK-OFFSET LK-KEY LK-FLAG.
    MOVE 0 TO LK-FLAG
    MOVE LK-OFFSET TO OFFSET

    PERFORM UNTIL EXIT
        CALL "JsonParse-ObjectKey" USING LK-INPUT OFFSET LK-FLAG STR
        IF LK-FLAG = 1 OR STR = LK-KEY
            EXIT PERFORM
        END-IF

        CALL "JsonParse-SkipValue" USING LK-INPUT OFFSET LK-FLAG
        IF LK-FLAG = 1
            EXIT PERFORM
        END-IF

        CALL "JsonParse-Comma" USING LK-INPUT OFFSET LK-FLAG
        IF LK-FLAG = 1
            EXIT PERFORM
        END-IF
    END-PERFORM

    IF LK-FLAG = 0
        MOVE OFFSET TO LK-OFFSET
    END-IF

    GOBACK.

END PROGRAM JsonParse-FindValue.
