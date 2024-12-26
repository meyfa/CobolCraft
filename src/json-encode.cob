*> --- JsonEncode-ObjectStart ---
IDENTIFICATION DIVISION.
PROGRAM-ID. JsonEncode-ObjectStart.

DATA DIVISION.
LINKAGE SECTION.
    01 LK-BUFFER        PIC X ANY LENGTH.
    01 LK-BUFFER-POS    BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-BUFFER LK-BUFFER-POS.
    MOVE "{" TO LK-BUFFER(LK-BUFFER-POS:1)
    ADD 1 TO LK-BUFFER-POS
    GOBACK.

END PROGRAM JsonEncode-ObjectStart.

*> --- JsonEncode-ObjectEnd ---
IDENTIFICATION DIVISION.
PROGRAM-ID. JsonEncode-ObjectEnd.

DATA DIVISION.
LINKAGE SECTION.
    01 LK-BUFFER        PIC X ANY LENGTH.
    01 LK-BUFFER-POS    BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-BUFFER LK-BUFFER-POS.
    MOVE "}" TO LK-BUFFER(LK-BUFFER-POS:1)
    ADD 1 TO LK-BUFFER-POS
    GOBACK.

END PROGRAM JsonEncode-ObjectEnd.

*> --- JsonEncode-ArrayStart ---
IDENTIFICATION DIVISION.
PROGRAM-ID. JsonEncode-ArrayStart.

DATA DIVISION.
LINKAGE SECTION.
    01 LK-BUFFER        PIC X ANY LENGTH.
    01 LK-BUFFER-POS    BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-BUFFER LK-BUFFER-POS.
    MOVE "[" TO LK-BUFFER(LK-BUFFER-POS:1)
    ADD 1 TO LK-BUFFER-POS
    GOBACK.

END PROGRAM JsonEncode-ArrayStart.

*> --- JsonEncode-ArrayEnd ---
IDENTIFICATION DIVISION.
PROGRAM-ID. JsonEncode-ArrayEnd.

DATA DIVISION.
LINKAGE SECTION.
    01 LK-BUFFER        PIC X ANY LENGTH.
    01 LK-BUFFER-POS    BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-BUFFER LK-BUFFER-POS.
    MOVE "]" TO LK-BUFFER(LK-BUFFER-POS:1)
    ADD 1 TO LK-BUFFER-POS
    GOBACK.

END PROGRAM JsonEncode-ArrayEnd.

*> --- JsonEncode-String ---
IDENTIFICATION DIVISION.
PROGRAM-ID. JsonEncode-String.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 I                BINARY-LONG UNSIGNED.
    01 CURRENT-CHAR.
        02 CURRENT-CHARCODE BINARY-CHAR UNSIGNED.
LINKAGE SECTION.
    01 LK-BUFFER        PIC X ANY LENGTH.
    01 LK-BUFFER-POS    BINARY-LONG UNSIGNED.
    01 LK-STRING        PIC X ANY LENGTH.
    01 LK-STRING-LEN    BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-BUFFER LK-BUFFER-POS LK-STRING LK-STRING-LEN.
    *> beginning of string
    MOVE '"' TO LK-BUFFER(LK-BUFFER-POS:1)
    ADD 1 TO LK-BUFFER-POS
    *> string content (escaped)
    PERFORM VARYING I FROM 1 BY 1 UNTIL I > LK-STRING-LEN
        MOVE LK-STRING(I:1) TO CURRENT-CHAR
        EVALUATE TRUE
            WHEN CURRENT-CHAR = '"' OR CURRENT-CHAR = '\'
                MOVE "\" TO LK-BUFFER(LK-BUFFER-POS:1)
                MOVE LK-STRING(I:1) TO LK-BUFFER(LK-BUFFER-POS + 1:1)
                ADD 2 TO LK-BUFFER-POS
            WHEN CURRENT-CHARCODE = 8
                MOVE "\b" TO LK-BUFFER(LK-BUFFER-POS:)
                ADD 2 TO LK-BUFFER-POS
            WHEN CURRENT-CHARCODE = 9
                MOVE "\t" TO LK-BUFFER(LK-BUFFER-POS:)
                ADD 2 TO LK-BUFFER-POS
            WHEN CURRENT-CHARCODE = 10
                MOVE "\n" TO LK-BUFFER(LK-BUFFER-POS:)
                ADD 2 TO LK-BUFFER-POS
            WHEN CURRENT-CHARCODE = 12
                MOVE "\f" TO LK-BUFFER(LK-BUFFER-POS:)
                ADD 2 TO LK-BUFFER-POS
            WHEN CURRENT-CHARCODE = 13
                MOVE "\r" TO LK-BUFFER(LK-BUFFER-POS:)
                ADD 2 TO LK-BUFFER-POS
            WHEN CURRENT-CHARCODE < 32
                CALL "JsonEncode-String-UnicodeChar" USING LK-BUFFER LK-BUFFER-POS CURRENT-CHAR
            WHEN OTHER
                MOVE LK-STRING(I:1) TO LK-BUFFER(LK-BUFFER-POS:1)
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
WORKING-STORAGE SECTION.
    01 BYTE-ALPHA.
        02 BYTE-VALUE       BINARY-CHAR UNSIGNED.
    01 NIBBLE-HIGH      BINARY-CHAR UNSIGNED.
    01 NIBBLE-LOW       BINARY-CHAR UNSIGNED.
    01 HEX-CHAR         PIC X.
LINKAGE SECTION.
    01 LK-BUFFER        PIC X ANY LENGTH.
    01 LK-BUFFER-POS    BINARY-LONG UNSIGNED.
    01 LK-CHAR          PIC X.

PROCEDURE DIVISION USING LK-BUFFER LK-BUFFER-POS LK-CHAR.
    MOVE "\u00" TO LK-BUFFER(LK-BUFFER-POS:)
    ADD 4 TO LK-BUFFER-POS
    *> convert byte to two hex characters
    MOVE LK-CHAR TO BYTE-ALPHA
    DIVIDE BYTE-VALUE BY 16 GIVING NIBBLE-HIGH REMAINDER NIBBLE-LOW
    CALL "EncodeHexChar" USING NIBBLE-HIGH LK-BUFFER(LK-BUFFER-POS:1)
    ADD 1 TO LK-BUFFER-POS
    CALL "EncodeHexChar" USING NIBBLE-LOW LK-BUFFER(LK-BUFFER-POS:1)
    ADD 1 TO LK-BUFFER-POS
    GOBACK.

END PROGRAM JsonEncode-String-UnicodeChar.

*> --- JsonEncode-ObjectKey ---
IDENTIFICATION DIVISION.
PROGRAM-ID. JsonEncode-ObjectKey.

DATA DIVISION.
LINKAGE SECTION.
    01 LK-BUFFER        PIC X ANY LENGTH.
    01 LK-BUFFER-POS    BINARY-LONG UNSIGNED.
    01 LK-STRING        PIC X ANY LENGTH.
    01 LK-STRING-LEN    BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-BUFFER LK-BUFFER-POS LK-STRING LK-STRING-LEN.
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
    01 LK-BUFFER        PIC X ANY LENGTH.
    01 LK-BUFFER-POS    BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-BUFFER LK-BUFFER-POS.
    MOVE "," TO LK-BUFFER(LK-BUFFER-POS:1)
    ADD 1 TO LK-BUFFER-POS
    GOBACK.

END PROGRAM JsonEncode-Comma.

*> --- JsonEncode-Integer ---
IDENTIFICATION DIVISION.
PROGRAM-ID. JsonEncode-Integer.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 VALUE-DISPLAY    PIC -(9)9.
    01 ENCODED          PIC X(10).
    01 ENCODED-LENGTH   BINARY-LONG UNSIGNED.
LINKAGE SECTION.
    01 LK-BUFFER        PIC X ANY LENGTH.
    01 LK-BUFFER-POS    BINARY-LONG UNSIGNED.
    01 LK-VALUE         BINARY-LONG.

PROCEDURE DIVISION USING LK-BUFFER LK-BUFFER-POS LK-VALUE.
    MOVE LK-VALUE TO VALUE-DISPLAY
    MOVE FUNCTION TRIM(VALUE-DISPLAY) TO ENCODED
    MOVE FUNCTION STORED-CHAR-LENGTH(ENCODED) TO ENCODED-LENGTH
    MOVE ENCODED(1:ENCODED-LENGTH) TO LK-BUFFER(LK-BUFFER-POS:ENCODED-LENGTH)
    ADD ENCODED-LENGTH TO LK-BUFFER-POS
    GOBACK.

END PROGRAM JsonEncode-Integer.
