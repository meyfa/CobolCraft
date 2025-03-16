*> --- Test: server-properties.cob ---
IDENTIFICATION DIVISION.
PROGRAM-ID. Test-ServerProperties.

PROCEDURE DIVISION.
    COPY TEST-SUITE REPLACING ==NAME== BY =="server-properties.cob"==.
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
        COPY TEST-UNIT REPLACING ==NAME== BY =="ServerProperties-Serialize"==.
    Basic.
        COPY TEST-CASE REPLACING ==NAME== BY =="basic"==.
        *> initialize the server properties
        INITIALIZE SERVER-PROPERTIES
        MOVE 12345 TO SP-PORT
        MOVE "sp-test-level" TO SP-LEVEL-NAME
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
            "level-name=sp-test-level" NEWLINE
            "white-list=false" NEWLINE
            "motd=test-motd" NEWLINE
            "max-players=42" NEWLINE
        INTO EXPECTED
        COPY TEST-ASSERT REPLACING COND BY ==FAILURE = 0 AND BUFFER(1:BUFFER-LENGTH) = EXPECTED==.
        .

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
        COPY TEST-UNIT REPLACING ==NAME== BY =="ServerProperties-Deserialize"==.
    Defaults.
        COPY TEST-CASE REPLACING ==NAME== BY =="defaults"==.
        MOVE 0 TO FAILURE
        INITIALIZE SERVER-PROPERTIES
        INITIALIZE BUFFER
        MOVE 0 TO BUFFER-LENGTH
        CALL "ServerProperties-Deserialize" USING BUFFER BUFFER-LENGTH FAILURE
        COPY TEST-ASSERT REPLACING COND BY ==FAILURE = 0
            AND SP-PORT = 25565
            AND SP-LEVEL-NAME = "world"
            AND SP-WHITELIST-ENABLE = 0
            AND SP-MOTD = "CobolCraft"
            AND MAX-PLAYERS = 10 AND MAX-CLIENTS = 10==.
    FullProperties.
        COPY TEST-CASE REPLACING ==NAME== BY =="full properties"==.
        MOVE 0 TO FAILURE
        INITIALIZE SERVER-PROPERTIES
        STRING
            "server-port=1337" NEWLINE
            "level-name=foobar" NEWLINE
            "white-list=true" NEWLINE
            "motd=foobar 42" NEWLINE
            "max-players=31" NEWLINE
        INTO BUFFER
        MOVE FUNCTION STORED-CHAR-LENGTH(BUFFER) TO BUFFER-LENGTH
        CALL "ServerProperties-Deserialize" USING BUFFER BUFFER-LENGTH FAILURE
        COPY TEST-ASSERT REPLACING COND BY ==FAILURE = 0
            AND SP-PORT = 1337
            AND SP-LEVEL-NAME = "foobar"
            AND SP-WHITELIST-ENABLE = 1
            AND SP-MOTD = "foobar 42"
            AND MAX-PLAYERS = 31
            AND MAX-CLIENTS = 31==.
    EmptyLevelName.
        COPY TEST-CASE REPLACING ==NAME== BY =="empty level-name"==.
        MOVE 0 TO FAILURE
        INITIALIZE SERVER-PROPERTIES
        MOVE "level-name=" TO BUFFER
        MOVE FUNCTION STORED-CHAR-LENGTH(BUFFER) TO BUFFER-LENGTH
        CALL "ServerProperties-Deserialize" USING BUFFER BUFFER-LENGTH FAILURE
        COPY TEST-ASSERT REPLACING COND BY ==FAILURE = 1==.
    EmptyMotd.
        COPY TEST-CASE REPLACING ==NAME== BY =="empty motd"==.
        MOVE 0 TO FAILURE
        INITIALIZE SERVER-PROPERTIES
        MOVE "motd=" TO BUFFER
        MOVE FUNCTION STORED-CHAR-LENGTH(BUFFER) TO BUFFER-LENGTH
        CALL "ServerProperties-Deserialize" USING BUFFER BUFFER-LENGTH FAILURE
        COPY TEST-ASSERT REPLACING COND BY ==FAILURE = 0 AND SP-MOTD = SPACES==.
    InvalidPort.
        COPY TEST-CASE REPLACING ==NAME== BY =="invalid port"==.
        MOVE 0 TO FAILURE
        INITIALIZE SERVER-PROPERTIES
        MOVE "server-port=abc" TO BUFFER
        MOVE FUNCTION STORED-CHAR-LENGTH(BUFFER) TO BUFFER-LENGTH
        CALL "ServerProperties-Deserialize" USING BUFFER BUFFER-LENGTH FAILURE
        COPY TEST-ASSERT REPLACING COND BY ==FAILURE = 1==.
    ZeroMaxPlayers.
        COPY TEST-CASE REPLACING ==NAME== BY =="zero max players"==.
        MOVE 0 TO FAILURE
        INITIALIZE SERVER-PROPERTIES
        MOVE "max-players=0" TO BUFFER
        MOVE FUNCTION STORED-CHAR-LENGTH(BUFFER) TO BUFFER-LENGTH
        CALL "ServerProperties-Deserialize" USING BUFFER BUFFER-LENGTH FAILURE
        COPY TEST-ASSERT REPLACING COND BY ==FAILURE = 1==.

        GOBACK.

    END PROGRAM Test-ServerProperties-D.

END PROGRAM Test-ServerProperties.
