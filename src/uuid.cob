*> --- UUID-ToString ---
*> Convert a UUID encoded as a 128-bit big-endian integer to a 36-character string.
IDENTIFICATION DIVISION.
PROGRAM-ID. UUID-ToString.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 INPUT-INDEX          BINARY-LONG UNSIGNED.
    01 BYTE-ALPHA.
        02 BYTE-VALUE           BINARY-CHAR UNSIGNED.
    01 NIBBLE-MSB           BINARY-CHAR UNSIGNED.
    01 NIBBLE-LSB           BINARY-CHAR UNSIGNED.
LOCAL-STORAGE SECTION.
    01 OUTPUT-INDEX         BINARY-LONG UNSIGNED    VALUE 1.
LINKAGE SECTION.
    01 LK-BUFFER            PIC X(16).
    01 LK-VALUE             PIC X(36).

PROCEDURE DIVISION USING LK-BUFFER LK-VALUE.
    PERFORM VARYING INPUT-INDEX FROM 1 BY 1 UNTIL INPUT-INDEX > 16
        *> read one unsigned byte
        MOVE LK-BUFFER(INPUT-INDEX:1) TO BYTE-ALPHA
        *> output two hex digits
        DIVIDE BYTE-VALUE BY 16 GIVING NIBBLE-MSB REMAINDER NIBBLE-LSB
        CALL "EncodeHexChar" USING NIBBLE-MSB LK-VALUE(OUTPUT-INDEX:1)
        ADD 1 TO OUTPUT-INDEX
        CALL "EncodeHexChar" USING NIBBLE-LSB LK-VALUE(OUTPUT-INDEX:1)
        ADD 1 TO OUTPUT-INDEX
        *> insert dashes
        IF INPUT-INDEX = 4 OR INPUT-INDEX = 6 OR INPUT-INDEX = 8 OR INPUT-INDEX = 10
            MOVE "-" TO LK-VALUE(OUTPUT-INDEX:1)
            ADD 1 TO OUTPUT-INDEX
        END-IF
    END-PERFORM
    GOBACK.

END PROGRAM UUID-ToString.

*> --- UUID-FromString ---
*> Convert a 36-character UUID string to a 128-bit big-endian integer.
IDENTIFICATION DIVISION.
PROGRAM-ID. UUID-FromString.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 OUTPUT-INDEX         BINARY-LONG UNSIGNED.
    01 NIBBLE-VALUE         BINARY-CHAR UNSIGNED.
    01 BYTE-ALPHA.
        02 BYTE-VALUE           BINARY-CHAR UNSIGNED.
LOCAL-STORAGE SECTION.
    01 INPUT-INDEX          BINARY-LONG UNSIGNED    VALUE 1.
LINKAGE SECTION.
    01 LK-VALUE-IN          PIC X(36).
    01 LK-VALUE-OUT         PIC X(16).

PROCEDURE DIVISION USING LK-VALUE-IN LK-VALUE-OUT.
    PERFORM VARYING OUTPUT-INDEX FROM 1 BY 1 UNTIL OUTPUT-INDEX > 16
        *> decode the hex value into a single byte
        CALL "DecodeHexChar" USING LK-VALUE-IN(INPUT-INDEX:1) BYTE-VALUE
        ADD 1 TO INPUT-INDEX
        CALL "DecodeHexChar" USING LK-VALUE-IN(INPUT-INDEX:1) NIBBLE-VALUE
        ADD 1 TO INPUT-INDEX
        COMPUTE BYTE-VALUE = BYTE-VALUE * 16 + NIBBLE-VALUE
        MOVE BYTE-ALPHA TO LK-VALUE-OUT(OUTPUT-INDEX:1)
        *> skip dashes in the input
        IF OUTPUT-INDEX = 4 OR OUTPUT-INDEX = 6 OR OUTPUT-INDEX = 8 OR OUTPUT-INDEX = 10
            ADD 1 TO INPUT-INDEX
        END-IF
    END-PERFORM
    GOBACK.

END PROGRAM UUID-FromString.
