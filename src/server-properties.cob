*> --- ServerProperties-Read ---
*> Read the server.properties file. Default values are used if the file is not found or keys are missing.
IDENTIFICATION DIVISION.
PROGRAM-ID. ServerProperties-Read.

DATA DIVISION.
WORKING-STORAGE SECTION.
    *> constants
    01 SERVER-PROPERTIES-FILE   PIC X(32)                   VALUE "server.properties".
    01 NEWLINE                  PIC X                       VALUE X"0A".
    *> shared data
    COPY DD-SERVER-PROPERTIES.
    *> file buffer
    01 READ-FAILURE             BINARY-CHAR UNSIGNED.
    01 BUFFER                   PIC X(64000).
    01 BUFFER-LENGTH            BINARY-LONG UNSIGNED.
    *> parser state
    01 BUFFER-POS               BINARY-LONG UNSIGNED.
    01 ENTRY-OFFSET             BINARY-LONG UNSIGNED.
    01 ENTRY-KEY                PIC X(255).
    01 ENTRY-VALUE              PIC X(255).
LINKAGE SECTION.
    01 LK-FAILURE               BINARY-CHAR UNSIGNED.

PROCEDURE DIVISION USING LK-FAILURE.
    *> Start with default values
    MOVE "25565" TO SP-PORT
    MOVE 0 TO SP-WHITELIST-ENABLE
    MOVE "CobolCraft" TO SP-MOTD

    *> Attempt to read file, but ignore if missing
    CALL "Files-ReadAll" USING SERVER-PROPERTIES-FILE BUFFER BUFFER-LENGTH READ-FAILURE
    IF READ-FAILURE NOT = 0
        GOBACK
    END-IF

    PERFORM VARYING BUFFER-POS FROM 1 BY 1 UNTIL BUFFER-POS > BUFFER-LENGTH
        EVALUATE BUFFER(BUFFER-POS:1)
            WHEN "#"
                *> skip to the end of the line
                PERFORM UNTIL BUFFER-POS >= BUFFER-LENGTH
                    ADD 1 TO BUFFER-POS
                    IF BUFFER(BUFFER-POS:1) = NEWLINE
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
                PERFORM UNTIL BUFFER-POS > BUFFER-LENGTH
                    IF BUFFER(BUFFER-POS:1) = "=" OR BUFFER(BUFFER-POS:1) = " " OR BUFFER(BUFFER-POS:1) = NEWLINE
                        EXIT PERFORM
                    END-IF
                    MOVE BUFFER(BUFFER-POS:1) TO ENTRY-KEY(ENTRY-OFFSET:1)
                    ADD 1 TO BUFFER-POS
                    ADD 1 TO ENTRY-OFFSET
                END-PERFORM

                *> TODO handle key longer than 255 characters
                *> TODO handle multiple equals signs

                *> skip spaces and the equals sign
                PERFORM UNTIL BUFFER-POS > BUFFER-LENGTH
                    IF BUFFER(BUFFER-POS:1) NOT = " " AND BUFFER(BUFFER-POS:1) NOT = "="
                        EXIT PERFORM
                    END-IF
                    ADD 1 TO BUFFER-POS
                END-PERFORM

                *> read value
                INITIALIZE ENTRY-VALUE
                MOVE 1 TO ENTRY-OFFSET
                PERFORM UNTIL BUFFER-POS > BUFFER-LENGTH
                    IF BUFFER(BUFFER-POS:1) = NEWLINE
                        EXIT PERFORM
                    END-IF
                    MOVE BUFFER(BUFFER-POS:1) TO ENTRY-VALUE(ENTRY-OFFSET:1)
                    ADD 1 TO BUFFER-POS
                    ADD 1 TO ENTRY-OFFSET
                END-PERFORM

                EVALUATE ENTRY-KEY
                    WHEN "server-port"
                        MOVE FUNCTION TRIM(ENTRY-VALUE) TO SP-PORT
                        IF SP-PORT IS NOT NUMERIC
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
                END-EVALUATE
        END-EVALUATE
    END-PERFORM

    GOBACK.

END PROGRAM ServerProperties-Read.

*> --- ServerProperties-Write ---
*> Write the server.properties file.
IDENTIFICATION DIVISION.
PROGRAM-ID. ServerProperties-Write.

DATA DIVISION.
WORKING-STORAGE SECTION.
    *> constants
    01 SERVER-PROPERTIES-FILE   PIC X(32)                   VALUE "server.properties".
    01 NEWLINE                  PIC X                       VALUE X"0A".
    *> shared data
    COPY DD-SERVER-PROPERTIES.
    *> file buffer
    01 BUFFER                   PIC X(64000).
    01 BUFFER-LENGTH            BINARY-LONG UNSIGNED.
    *> temporary data
    01 ENTRY-KEY                PIC X(255).
    01 ENTRY-VALUE              PIC X(255).
    01 BYTE-COUNT               BINARY-LONG UNSIGNED.
LINKAGE SECTION.
    01 LK-FAILURE               BINARY-CHAR UNSIGNED.

PROCEDURE DIVISION USING LK-FAILURE.
    MOVE "#CobolCraft server properties" TO BUFFER
    MOVE FUNCTION STORED-CHAR-LENGTH(BUFFER) TO BUFFER-LENGTH

    PERFORM AppendNewline

    MOVE "server-port" TO ENTRY-KEY
    MOVE SP-PORT TO ENTRY-VALUE
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

    CALL "Files-WriteAll" USING SERVER-PROPERTIES-FILE BUFFER BUFFER-LENGTH LK-FAILURE

    GOBACK.

AppendKeyValue SECTION.
    MOVE FUNCTION STORED-CHAR-LENGTH(ENTRY-KEY) TO BYTE-COUNT
    MOVE ENTRY-KEY TO BUFFER(BUFFER-LENGTH + 1:BYTE-COUNT)
    ADD BYTE-COUNT TO BUFFER-LENGTH

    MOVE "=" TO BUFFER(BUFFER-LENGTH + 1:1)
    ADD 1 TO BUFFER-LENGTH

    MOVE FUNCTION STORED-CHAR-LENGTH(ENTRY-VALUE) TO BYTE-COUNT
    MOVE ENTRY-VALUE TO BUFFER(BUFFER-LENGTH + 1:BYTE-COUNT)
    ADD BYTE-COUNT TO BUFFER-LENGTH

    PERFORM AppendNewline

    EXIT SECTION.

AppendNewline SECTION.
    MOVE NEWLINE TO BUFFER(BUFFER-LENGTH + 1:1)
    ADD 1 TO BUFFER-LENGTH

    EXIT SECTION.

END PROGRAM ServerProperties-Write.
