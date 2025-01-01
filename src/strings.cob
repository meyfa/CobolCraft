*> --- DecodeHexChar ---
*> Decode a single hex character to a byte value.
IDENTIFICATION DIVISION.
PROGRAM-ID. DecodeHexChar.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 NIBBLE-ALPHA.
        02 NIBBLE-VALUE     BINARY-CHAR UNSIGNED.
LINKAGE SECTION.
    01 LK-HEX-CHAR-IN   PIC X.
    01 LK-BYTE-OUT      BINARY-CHAR UNSIGNED.

PROCEDURE DIVISION USING LK-HEX-CHAR-IN LK-BYTE-OUT.
    MOVE LK-HEX-CHAR-IN TO NIBBLE-ALPHA
    EVALUATE NIBBLE-VALUE
        WHEN >= 97  *> "a"
            SUBTRACT 87 FROM NIBBLE-VALUE GIVING LK-BYTE-OUT
        WHEN >= 65  *> "A"
            SUBTRACT 55 FROM NIBBLE-VALUE GIVING LK-BYTE-OUT
        WHEN OTHER  *> "0"
            SUBTRACT 48 FROM NIBBLE-VALUE GIVING LK-BYTE-OUT
    END-EVALUATE
    GOBACK.

END PROGRAM DecodeHexChar.

*> --- DecodeHexString ---
*> Decode a hex string to a buffer. Spaces between pairs of hex characters are ignored.
IDENTIFICATION DIVISION.
PROGRAM-ID. DecodeHexString.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 INPUT-INDEX          BINARY-LONG UNSIGNED.
    01 NIBBLE-VALUE-HIGH    BINARY-CHAR UNSIGNED.
    01 NIBBLE-VALUE-LOW     BINARY-CHAR UNSIGNED.
    01 BYTE-ALPHA.
        02 BYTE-VALUE           BINARY-CHAR UNSIGNED.
LINKAGE SECTION.
    01 LK-HEX-STRING-IN     PIC X ANY LENGTH.
    01 LK-HEX-STRING-LEN    BINARY-LONG UNSIGNED.
    01 LK-BUFFER-OUT        PIC X ANY LENGTH.
    01 LK-BYTES-WRITTEN     BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-HEX-STRING-IN LK-HEX-STRING-LEN LK-BUFFER-OUT LK-BYTES-WRITTEN.
    MOVE 0 TO LK-BYTES-WRITTEN
    PERFORM VARYING INPUT-INDEX FROM 1 BY 1 UNTIL INPUT-INDEX > LK-HEX-STRING-LEN
        *> Skip spaces
        IF LK-HEX-STRING-IN(INPUT-INDEX:1) NOT = " "
            *> Convert the current character to the upper part of the byte
            CALL "DecodeHexChar" USING LK-HEX-STRING-IN(INPUT-INDEX:1) NIBBLE-VALUE-HIGH
            *> Convert the next character to the lower part of the byte
            ADD 1 TO INPUT-INDEX
            CALL "DecodeHexChar" USING LK-HEX-STRING-IN(INPUT-INDEX:1) NIBBLE-VALUE-LOW
            COMPUTE BYTE-VALUE = NIBBLE-VALUE-HIGH * 16 + NIBBLE-VALUE-LOW
            *> Write the byte to the output buffer
            ADD 1 TO LK-BYTES-WRITTEN
            MOVE BYTE-ALPHA TO LK-BUFFER-OUT(LK-BYTES-WRITTEN:1)
        END-IF
    END-PERFORM
    GOBACK.

END PROGRAM DecodeHexString.

*> --- EncodeHexChar ---
*> Encode a nibble to a hex character.
IDENTIFICATION DIVISION.
PROGRAM-ID. EncodeHexChar.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 HEX-CHARS        PIC X(16) VALUE "0123456789abcdef".
LINKAGE SECTION.
    01 LK-NIBBLE-IN     BINARY-CHAR UNSIGNED.
    01 LK-HEX-CHAR-OUT  PIC X.

PROCEDURE DIVISION USING LK-NIBBLE-IN LK-HEX-CHAR-OUT.
    MOVE HEX-CHARS(LK-NIBBLE-IN + 1:1) TO LK-HEX-CHAR-OUT
    GOBACK.

END PROGRAM EncodeHexChar.

*> --- EncodeHexString ---
*> Encode a buffer to a hex string.
IDENTIFICATION DIVISION.
PROGRAM-ID. EncodeHexString.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 BYTE-INDEX           BINARY-LONG UNSIGNED.
    01 BYTE-ALPHA.
        02 BYTE-VALUE           BINARY-CHAR UNSIGNED.
    01 NIBBLE-VALUE         BINARY-CHAR UNSIGNED.
LOCAL-STORAGE SECTION.
    01 STRING-INDEX         BINARY-LONG UNSIGNED        VALUE 1.
LINKAGE SECTION.
    01 LK-BUFFER-IN         PIC X ANY LENGTH.
    01 LK-BUFFER-LEN        BINARY-LONG UNSIGNED.
    01 LK-HEX-STRING-OUT    PIC X ANY LENGTH.

PROCEDURE DIVISION USING LK-BUFFER-IN LK-BUFFER-LEN LK-HEX-STRING-OUT.
    PERFORM VARYING BYTE-INDEX FROM 1 BY 1 UNTIL BYTE-INDEX > LK-BUFFER-LEN
        MOVE LK-BUFFER-IN(BYTE-INDEX:1) TO BYTE-ALPHA
        DIVIDE BYTE-VALUE BY 16 GIVING NIBBLE-VALUE REMAINDER BYTE-VALUE
        CALL "EncodeHexChar" USING NIBBLE-VALUE LK-HEX-STRING-OUT(STRING-INDEX:1)
        ADD 1 TO STRING-INDEX
        CALL "EncodeHexChar" USING BYTE-VALUE LK-HEX-STRING-OUT(STRING-INDEX:1)
        ADD 1 TO STRING-INDEX
    END-PERFORM
    GOBACK.

END PROGRAM EncodeHexString.
