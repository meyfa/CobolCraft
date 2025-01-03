*> --- Test: server-properties.cob ---
IDENTIFICATION DIVISION.
PROGRAM-ID. Test-ServerProperties.

PROCEDURE DIVISION.
    DISPLAY "Test: server-properties.cob"
    CALL "Test-ServerProperties-S"
    CALL "Test-ServerProperties-D"
    GOBACK.

    *> --- Test: Test-ServerProperties-S ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Test-ServerProperties-S.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        COPY DD-SERVER-PROPERTIES.
        01 NEWLINE                  PIC X                       VALUE X"0A".
        01 BUFFER                   PIC X(1024).
        01 BUFFER-LENGTH            BINARY-LONG UNSIGNED.
        01 FAILURE                  BINARY-CHAR UNSIGNED.
        01 EXPECTED                 PIC X(1024).

    PROCEDURE DIVISION.
        DISPLAY "  Test: ServerProperties-Serialize".
    Basic.
        DISPLAY "    Case: basic - " WITH NO ADVANCING
        *> initialize the server properties
        INITIALIZE SERVER-PROPERTIES
        MOVE 12345 TO SP-PORT
        MOVE 0 TO SP-WHITELIST-ENABLE
        MOVE "test-motd" TO SP-MOTD
        MOVE 42 TO MAX-PLAYERS
        MOVE 42 TO MAX-CLIENTS
        *> serialize
        INITIALIZE BUFFER
        MOVE 0 TO BUFFER-LENGTH
        CALL "ServerProperties-Serialize" USING BUFFER BUFFER-LENGTH FAILURE
        *> expected
        INITIALIZE EXPECTED
        STRING
            "#CobolCraft server properties" NEWLINE
            "server-port=12345" NEWLINE
            "white-list=false" NEWLINE
            "motd=test-motd" NEWLINE
            "max-players=42" NEWLINE
        INTO EXPECTED
        IF FAILURE = 0 AND BUFFER(1:BUFFER-LENGTH) = EXPECTED
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
            DISPLAY "--- expected ---"
            DISPLAY FUNCTION TRIM(EXPECTED)
            DISPLAY "--- actual ---"
            DISPLAY BUFFER(1:BUFFER-LENGTH)
            DISPLAY "---"
        END-IF.

        GOBACK.

    END PROGRAM Test-ServerProperties-S.

    *> --- Test: Test-ServerProperties-D ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Test-ServerProperties-D.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        COPY DD-SERVER-PROPERTIES.
        01 NEWLINE                  PIC X                       VALUE X"0A".
        01 BUFFER                   PIC X(1024).
        01 BUFFER-LENGTH            BINARY-LONG UNSIGNED.
        01 FAILURE                  BINARY-CHAR UNSIGNED.

    PROCEDURE DIVISION.
        DISPLAY "  Test: ServerProperties-Deserialize".
    Defaults.
        DISPLAY "    Case: defaults - " WITH NO ADVANCING
        INITIALIZE SERVER-PROPERTIES
        INITIALIZE BUFFER
        MOVE 0 TO BUFFER-LENGTH
        CALL "ServerProperties-Deserialize" USING BUFFER BUFFER-LENGTH FAILURE
        IF FAILURE = 0
                AND SP-PORT = 25565
                AND SP-WHITELIST-ENABLE = 0
                AND SP-MOTD = "CobolCraft"
                AND MAX-PLAYERS = 10
                AND MAX-CLIENTS = 10
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    FullProperties.
        DISPLAY "    Case: full properties - " WITH NO ADVANCING
        INITIALIZE SERVER-PROPERTIES
        STRING
            "server-port=1337" NEWLINE
            "white-list=true" NEWLINE
            "motd=foobar 42" NEWLINE
            "max-players=31" NEWLINE
        INTO BUFFER
        MOVE FUNCTION STORED-CHAR-LENGTH(BUFFER) TO BUFFER-LENGTH
        CALL "ServerProperties-Deserialize" USING BUFFER BUFFER-LENGTH FAILURE
        IF FAILURE = 0
                AND SP-PORT = 1337
                AND SP-WHITELIST-ENABLE = 1
                AND SP-MOTD = "foobar 42"
                AND MAX-PLAYERS = 31
                AND MAX-CLIENTS = 31
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    EmptyMotd.
        DISPLAY "    Case: empty motd - " WITH NO ADVANCING
        INITIALIZE SERVER-PROPERTIES
        MOVE "motd=" TO BUFFER
        MOVE FUNCTION STORED-CHAR-LENGTH(BUFFER) TO BUFFER-LENGTH
        CALL "ServerProperties-Deserialize" USING BUFFER BUFFER-LENGTH FAILURE
        IF FAILURE = 0 AND SP-MOTD = SPACES
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    InvalidPort.
        DISPLAY "    Case: invalid port - " WITH NO ADVANCING
        INITIALIZE SERVER-PROPERTIES
        MOVE "server-port=abc" TO BUFFER
        MOVE FUNCTION STORED-CHAR-LENGTH(BUFFER) TO BUFFER-LENGTH
        CALL "ServerProperties-Deserialize" USING BUFFER BUFFER-LENGTH FAILURE
        IF FAILURE = 1
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    ZeroMaxPlayers.
        DISPLAY "    Case: zero max players - " WITH NO ADVANCING
        INITIALIZE SERVER-PROPERTIES
        MOVE "max-players=0" TO BUFFER
        MOVE FUNCTION STORED-CHAR-LENGTH(BUFFER) TO BUFFER-LENGTH
        CALL "ServerProperties-Deserialize" USING BUFFER BUFFER-LENGTH FAILURE
        IF FAILURE = 1
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.

        GOBACK.

    END PROGRAM Test-ServerProperties-D.

END PROGRAM Test-ServerProperties.
