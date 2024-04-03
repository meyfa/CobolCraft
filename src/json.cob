*> --- JsonEncode-ObjectStart ---
IDENTIFICATION DIVISION.
PROGRAM-ID. JsonEncode-ObjectStart.

DATA DIVISION.
LINKAGE SECTION.
    01 LK-BUFFER        PIC X(64000).
    01 LK-BUFFER-POS    PIC 9(5).

PROCEDURE DIVISION USING BY REFERENCE LK-BUFFER LK-BUFFER-POS.
    MOVE "{" TO LK-BUFFER(LK-BUFFER-POS:1)
    ADD 1 TO LK-BUFFER-POS
    GOBACK.

END PROGRAM JsonEncode-ObjectStart.

*> --- JsonEncode-ObjectEnd ---
IDENTIFICATION DIVISION.
PROGRAM-ID. JsonEncode-ObjectEnd.

DATA DIVISION.
LINKAGE SECTION.
    01 LK-BUFFER        PIC X(64000).
    01 LK-BUFFER-POS    PIC 9(5).

PROCEDURE DIVISION USING BY REFERENCE LK-BUFFER LK-BUFFER-POS.
    MOVE "}" TO LK-BUFFER(LK-BUFFER-POS:1)
    ADD 1 TO LK-BUFFER-POS
    GOBACK.

END PROGRAM JsonEncode-ObjectEnd.

*> --- JsonEncode-ArrayStart ---
IDENTIFICATION DIVISION.
PROGRAM-ID. JsonEncode-ArrayStart.

DATA DIVISION.
LINKAGE SECTION.
    01 LK-BUFFER        PIC X(64000).
    01 LK-BUFFER-POS    PIC 9(5).

PROCEDURE DIVISION USING BY REFERENCE LK-BUFFER LK-BUFFER-POS.
    MOVE "[" TO LK-BUFFER(LK-BUFFER-POS:1)
    ADD 1 TO LK-BUFFER-POS
    GOBACK.

END PROGRAM JsonEncode-ArrayStart.

*> --- JsonEncode-ArrayEnd ---
IDENTIFICATION DIVISION.
PROGRAM-ID. JsonEncode-ArrayEnd.

DATA DIVISION.
LINKAGE SECTION.
    01 LK-BUFFER        PIC X(64000).
    01 LK-BUFFER-POS    PIC 9(5).

PROCEDURE DIVISION USING BY REFERENCE LK-BUFFER LK-BUFFER-POS.
    MOVE "]" TO LK-BUFFER(LK-BUFFER-POS:1)
    ADD 1 TO LK-BUFFER-POS
    GOBACK.

END PROGRAM JsonEncode-ArrayEnd.

*> --- JsonEncode-String ---
IDENTIFICATION DIVISION.
PROGRAM-ID. JsonEncode-String.

DATA DIVISION.
LOCAL-STORAGE SECTION.
    01 I                PIC 9(5).
    01 CURRENT-CHAR     PIC X.
    01 CURRENT-ORD      PIC 9(5).
LINKAGE SECTION.
    01 LK-BUFFER        PIC X(64000).
    01 LK-BUFFER-POS    PIC 9(5).
    01 LK-STRING        PIC X(64000).
    01 LK-STRING-LEN    PIC 9(5).

PROCEDURE DIVISION USING BY REFERENCE LK-BUFFER LK-BUFFER-POS LK-STRING LK-STRING-LEN.
    *> beginning of string
    MOVE '"' TO LK-BUFFER(LK-BUFFER-POS:1)
    ADD 1 TO LK-BUFFER-POS
    *> string content (escaped)
    PERFORM VARYING I FROM 1 BY 1 UNTIL I > LK-STRING-LEN
        MOVE LK-STRING(I:I) TO CURRENT-CHAR
        MOVE FUNCTION ORD(CURRENT-CHAR) TO CURRENT-ORD
        EVALUATE TRUE
            WHEN CURRENT-CHAR = '"' OR CURRENT-CHAR = '\'
                MOVE "\" TO LK-BUFFER(LK-BUFFER-POS:1)
                MOVE LK-STRING(I:I) TO LK-BUFFER(LK-BUFFER-POS + 1:1)
                ADD 2 TO LK-BUFFER-POS
            WHEN CURRENT-ORD = 10
                MOVE "\t" TO LK-BUFFER(LK-BUFFER-POS:2)
                ADD 2 TO LK-BUFFER-POS
            WHEN CURRENT-ORD = 11
                MOVE "\n" TO LK-BUFFER(LK-BUFFER-POS:2)
                ADD 2 TO LK-BUFFER-POS
            WHEN CURRENT-ORD = 14
                MOVE "\r" TO LK-BUFFER(LK-BUFFER-POS:2)
                ADD 2 TO LK-BUFFER-POS
            WHEN CURRENT-ORD < 32
                CALL "JsonEncode-String-UnicodeChar" USING LK-BUFFER LK-BUFFER-POS CURRENT-CHAR
            WHEN OTHER
                MOVE LK-STRING(I:I) TO LK-BUFFER(LK-BUFFER-POS:1)
                ADD 1 TO LK-BUFFER-POS
        END-EVALUATE
    END-PERFORM
    *> end of string
    MOVE '"' TO LK-BUFFER(LK-BUFFER-POS:1)
    ADD 1 TO LK-BUFFER-POS
    GOBACK.

END PROGRAM JsonEncode-String.

*> --- JsonEncode-String-UnicodeChar ---
*> A helper to convert any character into a Unicode escape sequence (e.g. \u00A9).
IDENTIFICATION DIVISION.
PROGRAM-ID. JsonEncode-String-UnicodeChar.

DATA DIVISION.
LOCAL-STORAGE SECTION.
    01 NIBBLE           PIC 9(3).
    01 HEX-CHAR         PIC X.
LINKAGE SECTION.
    01 LK-BUFFER        PIC X(64000).
    01 LK-BUFFER-POS    PIC 9(5).
    01 LK-CHAR          PIC X(1).

PROCEDURE DIVISION USING BY REFERENCE LK-BUFFER LK-BUFFER-POS LK-CHAR.
    MOVE "\u00" TO LK-BUFFER(LK-BUFFER-POS:4)
    ADD 4 TO LK-BUFFER-POS
    *> first hex character
    COMPUTE NIBBLE = (FUNCTION ORD(LK-CHAR) - 1) / 16
    CALL "EncodeHexChar" USING NIBBLE HEX-CHAR
    MOVE HEX-CHAR TO LK-BUFFER(LK-BUFFER-POS:1)
    ADD 1 TO LK-BUFFER-POS
    *> second hex character
    COMPUTE NIBBLE = FUNCTION MOD(FUNCTION ORD(LK-CHAR) - 1, 16)
    CALL "EncodeHexChar" USING NIBBLE HEX-CHAR
    MOVE HEX-CHAR TO LK-BUFFER(LK-BUFFER-POS:1)
    ADD 1 TO LK-BUFFER-POS
    GOBACK.

END PROGRAM JsonEncode-String-UnicodeChar.

*> --- JsonEncode-ObjectKey ---
IDENTIFICATION DIVISION.
PROGRAM-ID. JsonEncode-ObjectKey.

DATA DIVISION.
LINKAGE SECTION.
    01 LK-BUFFER        PIC X(64000).
    01 LK-BUFFER-POS    PIC 9(5).
    01 LK-STRING        PIC X(64000).
    01 LK-STRING-LEN    PIC 9(5).

PROCEDURE DIVISION USING BY REFERENCE LK-BUFFER LK-BUFFER-POS LK-STRING LK-STRING-LEN.
    CALL "JsonEncode-String" USING LK-BUFFER LK-BUFFER-POS LK-STRING LK-STRING-LEN
    MOVE ":" TO LK-BUFFER(LK-BUFFER-POS:1)
    ADD 1 TO LK-BUFFER-POS
    GOBACK.

END PROGRAM JsonEncode-ObjectKey.

*> --- JsonEncode-Comma ---
IDENTIFICATION DIVISION.
PROGRAM-ID. JsonEncode-Comma.

DATA DIVISION.
LINKAGE SECTION.
    01 LK-BUFFER        PIC X(64000).
    01 LK-BUFFER-POS    PIC 9(5).

PROCEDURE DIVISION USING BY REFERENCE LK-BUFFER LK-BUFFER-POS.
    MOVE "," TO LK-BUFFER(LK-BUFFER-POS:1)
    ADD 1 TO LK-BUFFER-POS
    GOBACK.

END PROGRAM JsonEncode-Comma.

*> --- JsonEncode-Integer ---
IDENTIFICATION DIVISION.
PROGRAM-ID. JsonEncode-Integer.

DATA DIVISION.
LOCAL-STORAGE SECTION.
    01 VALUE-INVERSE    PIC S9(10).
    01 VALUE-STRING     PIC X(10).
    01 ZERO-COUNT       PIC 9(5).
LINKAGE SECTION.
    01 LK-BUFFER        PIC X(64000).
    01 LK-BUFFER-POS    PIC 9(5).
    01 LK-VALUE         PIC S9(10).

PROCEDURE DIVISION USING BY REFERENCE LK-BUFFER LK-BUFFER-POS LK-VALUE.
    *> shortcut for zero
    IF LK-VALUE = 0
        MOVE "0" TO LK-BUFFER(LK-BUFFER-POS:1)
        ADD 1 TO LK-BUFFER-POS
        GOBACK
    END-IF
    *> handle negative numbers and convert to string
    IF LK-VALUE < 0
        MOVE "-" TO LK-BUFFER(LK-BUFFER-POS:1)
        ADD 1 TO LK-BUFFER-POS
        COMPUTE VALUE-INVERSE = -LK-VALUE
        MOVE VALUE-INVERSE TO VALUE-STRING
    ELSE
        MOVE LK-VALUE TO VALUE-STRING
    END-IF
    *> skip leading zeros
    MOVE 0 TO ZERO-COUNT
    PERFORM UNTIL VALUE-STRING(ZERO-COUNT + 1:1) NOT = "0"
        ADD 1 TO ZERO-COUNT
    END-PERFORM
    *> write the number to the buffer
    MOVE VALUE-STRING(ZERO-COUNT + 1:10 - ZERO-COUNT) TO LK-BUFFER(LK-BUFFER-POS:10 - ZERO-COUNT)
    COMPUTE LK-BUFFER-POS = LK-BUFFER-POS + 10 - ZERO-COUNT
    GOBACK.

END PROGRAM JsonEncode-Integer.
