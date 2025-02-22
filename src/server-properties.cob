*> --- ServerProperties-Read ---
*> Read the server.properties file. Default values are used if the file is not found or keys are missing.
IDENTIFICATION DIVISION.
PROGRAM-ID. ServerProperties-Read.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 SERVER-PROPERTIES-FILE   PIC X(32)                   VALUE "server.properties".
    01 READ-FAILURE             BINARY-CHAR UNSIGNED.
    01 BUFFER                   PIC X(64000).
    01 BUFFER-LENGTH            BINARY-LONG UNSIGNED.
LINKAGE SECTION.
    01 LK-FAILURE               BINARY-CHAR UNSIGNED.

PROCEDURE DIVISION USING LK-FAILURE.
    CALL "Files-ReadAll" USING SERVER-PROPERTIES-FILE BUFFER BUFFER-LENGTH READ-FAILURE
    IF READ-FAILURE NOT = 0
        INITIALIZE BUFFER
        MOVE 0 TO BUFFER-LENGTH
    END-IF
    CALL "ServerProperties-Deserialize" USING BUFFER BUFFER-LENGTH LK-FAILURE
    GOBACK.

END PROGRAM ServerProperties-Read.

*> --- ServerProperties-Write ---
*> Write the server.properties file.
IDENTIFICATION DIVISION.
PROGRAM-ID. ServerProperties-Write.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 SERVER-PROPERTIES-FILE   PIC X(32)                   VALUE "server.properties".
    01 BUFFER                   PIC X(64000).
    01 BUFFER-LENGTH            BINARY-LONG UNSIGNED.
LINKAGE SECTION.
    01 LK-FAILURE               BINARY-CHAR UNSIGNED.

PROCEDURE DIVISION USING LK-FAILURE.
    CALL "ServerProperties-Serialize" USING BUFFER BUFFER-LENGTH LK-FAILURE
    CALL "Files-WriteAll" USING SERVER-PROPERTIES-FILE BUFFER BUFFER-LENGTH LK-FAILURE
    GOBACK.

END PROGRAM ServerProperties-Write.

*> --- ServerProperties-Deserialize ---
IDENTIFICATION DIVISION.
PROGRAM-ID. ServerProperties-Deserialize.

DATA DIVISION.
WORKING-STORAGE SECTION.
    COPY DD-SERVER-PROPERTIES.
    01 NEWLINE                  PIC X                       VALUE X"0A".
    01 BUFFER-POS               BINARY-LONG UNSIGNED.
    01 ENTRY-OFFSET             BINARY-LONG UNSIGNED.
    01 ENTRY-KEY                PIC X(255).
    01 ENTRY-VALUE              PIC X(255).
LINKAGE SECTION.
    01 LK-BUFFER                PIC X ANY LENGTH.
    01 LK-BUFFER-LENGTH         BINARY-LONG UNSIGNED.
    01 LK-FAILURE               BINARY-CHAR UNSIGNED.

PROCEDURE DIVISION USING LK-BUFFER LK-BUFFER-LENGTH LK-FAILURE.
    *> Start with default values
    MOVE 25565 TO SP-PORT
    MOVE "world" TO SP-LEVEL-NAME
    MOVE 0 TO SP-WHITELIST-ENABLE
    MOVE "CobolCraft" TO SP-MOTD
    MOVE 10 TO MAX-PLAYERS
    MOVE 10 TO MAX-CLIENTS

    PERFORM VARYING BUFFER-POS FROM 1 BY 1 UNTIL BUFFER-POS > LK-BUFFER-LENGTH
        EVALUATE LK-BUFFER(BUFFER-POS:1)
            WHEN "#"
                *> skip to the end of the line
                PERFORM UNTIL BUFFER-POS >= LK-BUFFER-LENGTH
                    ADD 1 TO BUFFER-POS
                    IF LK-BUFFER(BUFFER-POS:1) = NEWLINE
                        EXIT PERFORM
                    END-IF
                END-PERFORM

            WHEN X"0A"
                *> skip empty lines
                CONTINUE

            WHEN OTHER
                *> read key
                INITIALIZE ENTRY-KEY
                MOVE 1 TO ENTRY-OFFSET
                PERFORM UNTIL BUFFER-POS > LK-BUFFER-LENGTH
                    IF LK-BUFFER(BUFFER-POS:1) = "=" OR " " OR NEWLINE
                        EXIT PERFORM
                    END-IF
                    MOVE LK-BUFFER(BUFFER-POS:1) TO ENTRY-KEY(ENTRY-OFFSET:1)
                    ADD 1 TO BUFFER-POS
                    ADD 1 TO ENTRY-OFFSET
                END-PERFORM

                *> TODO handle key longer than 255 characters
                *> TODO handle multiple equals signs

                *> skip spaces and the equals sign
                PERFORM UNTIL BUFFER-POS > LK-BUFFER-LENGTH
                    IF LK-BUFFER(BUFFER-POS:1) NOT = " " AND LK-BUFFER(BUFFER-POS:1) NOT = "="
                        EXIT PERFORM
                    END-IF
                    ADD 1 TO BUFFER-POS
                END-PERFORM

                *> read value
                INITIALIZE ENTRY-VALUE
                MOVE 1 TO ENTRY-OFFSET
                PERFORM UNTIL BUFFER-POS > LK-BUFFER-LENGTH
                    IF LK-BUFFER(BUFFER-POS:1) = NEWLINE
                        EXIT PERFORM
                    END-IF
                    MOVE LK-BUFFER(BUFFER-POS:1) TO ENTRY-VALUE(ENTRY-OFFSET:1)
                    ADD 1 TO BUFFER-POS
                    ADD 1 TO ENTRY-OFFSET
                END-PERFORM

                EVALUATE ENTRY-KEY
                    WHEN "server-port"
                        MOVE FUNCTION NUMVAL(ENTRY-VALUE) TO SP-PORT
                        IF SP-PORT < 1 OR SP-PORT > 65535
                            MOVE 1 TO LK-FAILURE
                            GOBACK
                        END-IF

                    WHEN "level-name"
                        MOVE FUNCTION TRIM(ENTRY-VALUE) TO SP-LEVEL-NAME
                        if FUNCTION STORED-CHAR-LENGTH(SP-LEVEL-NAME) = 0
                            MOVE 1 TO LK-FAILURE
                            GOBACK
                        END-IF

                    WHEN "white-list"
                        IF ENTRY-VALUE = "true"
                            MOVE 1 TO SP-WHITELIST-ENABLE
                        ELSE
                            MOVE 0 TO SP-WHITELIST-ENABLE
                        END-IF

                    WHEN "motd"
                        MOVE FUNCTION TRIM(ENTRY-VALUE) TO SP-MOTD

                    WHEN "max-players"
                        MOVE FUNCTION NUMVAL(ENTRY-VALUE) TO MAX-PLAYERS MAX-CLIENTS
                        IF MAX-PLAYERS < 1
                           MOVE 1 TO LK-FAILURE
                           GOBACK
                        END-IF
                END-EVALUATE
        END-EVALUATE
    END-PERFORM

    GOBACK.

END PROGRAM ServerProperties-Deserialize.

*> --- ServerProperties-Serialize ---
IDENTIFICATION DIVISION.
PROGRAM-ID. ServerProperties-Serialize.

DATA DIVISION.
WORKING-STORAGE SECTION.
    COPY DD-SERVER-PROPERTIES.
    01 NEWLINE                  PIC X                       VALUE X"0A".
    *> temporary data
    01 ENTRY-KEY                PIC X(255).
    01 ENTRY-VALUE              PIC X(255).
    01 INT-TO-STR               PIC -(9)9.
    01 BYTE-COUNT               BINARY-LONG UNSIGNED.
LINKAGE SECTION.
    01 LK-BUFFER                PIC X ANY LENGTH.
    01 LK-BUFFER-LENGTH         BINARY-LONG UNSIGNED.
    01 LK-FAILURE               BINARY-CHAR UNSIGNED.

PROCEDURE DIVISION USING LK-BUFFER LK-BUFFER-LENGTH LK-FAILURE.
    MOVE "#CobolCraft server properties" TO LK-BUFFER
    MOVE FUNCTION STORED-CHAR-LENGTH(LK-BUFFER) TO LK-BUFFER-LENGTH

    PERFORM AppendNewline

    MOVE "server-port" TO ENTRY-KEY
    MOVE SP-PORT TO INT-TO-STR
    MOVE FUNCTION TRIM(INT-TO-STR) TO ENTRY-VALUE
    PERFORM AppendKeyValue

    MOVE "level-name" TO ENTRY-KEY
    MOVE SP-LEVEL-NAME TO ENTRY-VALUE
    PERFORM AppendKeyValue

    MOVE "white-list" TO ENTRY-KEY
    IF SP-WHITELIST-ENABLE = 1
        MOVE "true" TO ENTRY-VALUE
    ELSE
        MOVE "false" TO ENTRY-VALUE
    END-IF
    PERFORM AppendKeyValue

    MOVE "motd" TO ENTRY-KEY
    MOVE SP-MOTD TO ENTRY-VALUE
    PERFORM AppendKeyValue

    MOVE "max-players" TO ENTRY-KEY
    MOVE MAX-PLAYERS TO INT-TO-STR
    MOVE FUNCTION TRIM(INT-TO-STR) TO ENTRY-VALUE
    PERFORM AppendKeyValue

    GOBACK.

AppendKeyValue.
    MOVE FUNCTION STORED-CHAR-LENGTH(ENTRY-KEY) TO BYTE-COUNT
    MOVE ENTRY-KEY TO LK-BUFFER(LK-BUFFER-LENGTH + 1:BYTE-COUNT)
    ADD BYTE-COUNT TO LK-BUFFER-LENGTH

    MOVE "=" TO LK-BUFFER(LK-BUFFER-LENGTH + 1:1)
    ADD 1 TO LK-BUFFER-LENGTH

    MOVE FUNCTION STORED-CHAR-LENGTH(ENTRY-VALUE) TO BYTE-COUNT
    MOVE ENTRY-VALUE TO LK-BUFFER(LK-BUFFER-LENGTH + 1:BYTE-COUNT)
    ADD BYTE-COUNT TO LK-BUFFER-LENGTH

    PERFORM AppendNewline
    .

AppendNewline.
    MOVE NEWLINE TO LK-BUFFER(LK-BUFFER-LENGTH + 1:1)
    ADD 1 TO LK-BUFFER-LENGTH
    .

END PROGRAM ServerProperties-Serialize.
