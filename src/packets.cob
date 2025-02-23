*> --- Packets-Parse ---
*> Parse the packets.json file to get packet metadata.
IDENTIFICATION DIVISION.
PROGRAM-ID. Packets-Parse.

DATA DIVISION.
WORKING-STORAGE SECTION.
    COPY DD-PACKET-IDS.
    COPY DD-CLIENT-STATES.
    COPY DD-PACKET-DIRECTIONS.
    01 STATE-NAME               PIC X(128).
    01 DIRECTION-NAME           PIC X(128).
    01 PACKET-NAME              PIC X(128).
    01 PACKET-REFERENCE         PIC X(128).
    01 OBJECT-KEY               PIC X(128).
    01 STATE-ID                 BINARY-LONG.
    01 DIRECTION-ID             BINARY-LONG.
    01 PACKET-ID                BINARY-LONG.
LOCAL-STORAGE SECTION.
    01 OFFSET                   BINARY-LONG UNSIGNED        VALUE 1.
    01 EXIT-LOOP                BINARY-CHAR UNSIGNED        VALUE 0.
LINKAGE SECTION.
    01 LK-JSON                  PIC X ANY LENGTH.
    01 LK-JSON-LEN              BINARY-LONG UNSIGNED.
    01 LK-FAILURE               BINARY-CHAR UNSIGNED.

PROCEDURE DIVISION USING LK-JSON LK-JSON-LEN LK-FAILURE.
    MOVE 0 TO PACKET-ID-COUNT
    INITIALIZE PACKET-REFERENCES

    CALL "JsonParse-ObjectStart" USING LK-JSON OFFSET LK-FAILURE
    IF LK-FAILURE NOT = 0
        GOBACK
    END-IF

    PERFORM UNTIL EXIT-LOOP NOT = 0
        CALL "JsonParse-ObjectKey" USING LK-JSON OFFSET LK-FAILURE STATE-NAME
        IF LK-FAILURE NOT = 0
            GOBACK
        END-IF
        EVALUATE STATE-NAME
            WHEN "handshake"
                MOVE CLIENT-STATE-HANDSHAKE TO STATE-ID
            WHEN "status"
                MOVE CLIENT-STATE-STATUS TO STATE-ID
            WHEN "login"
                MOVE CLIENT-STATE-LOGIN TO STATE-ID
            WHEN "configuration"
                MOVE CLIENT-STATE-CONFIGURATION TO STATE-ID
            WHEN "play"
                MOVE CLIENT-STATE-PLAY TO STATE-ID
            WHEN OTHER
                DISPLAY "Unknown client state '" FUNCTION TRIM(STATE-NAME) "'"
                MOVE 1 TO LK-FAILURE
                GOBACK
        END-EVALUATE
        PERFORM ParseState
        CALL "JsonParse-Comma" USING LK-JSON OFFSET EXIT-LOOP
    END-PERFORM

    CALL "JsonParse-ObjectEnd" USING LK-JSON OFFSET LK-FAILURE
    IF LK-FAILURE NOT = 0
        GOBACK
    END-IF

    GOBACK.

ParseState.
    CALL "JsonParse-ObjectStart" USING LK-JSON OFFSET LK-FAILURE
    IF LK-FAILURE NOT = 0
        GOBACK
    END-IF

    PERFORM UNTIL EXIT-LOOP NOT = 0
        CALL "JsonParse-ObjectKey" USING LK-JSON OFFSET LK-FAILURE DIRECTION-NAME
        IF LK-FAILURE NOT = 0
            GOBACK
        END-IF
        EVALUATE DIRECTION-NAME
            WHEN "serverbound"
                MOVE PACKET-DIRECTION-SERVERBOUND TO DIRECTION-ID
            WHEN "clientbound"
                MOVE PACKET-DIRECTION-CLIENTBOUND TO DIRECTION-ID
            WHEN OTHER
                DISPLAY "Unknown packet direction '" FUNCTION TRIM(DIRECTION-NAME) "'"
                MOVE 1 TO LK-FAILURE
                GOBACK
        END-EVALUATE
        PERFORM ParseDirection
        CALL "JsonParse-Comma" USING LK-JSON OFFSET EXIT-LOOP
    END-PERFORM

    CALL "JsonParse-ObjectEnd" USING LK-JSON OFFSET LK-FAILURE
    .

ParseDirection.
    CALL "JsonParse-ObjectStart" USING LK-JSON OFFSET LK-FAILURE
    IF LK-FAILURE NOT = 0
        GOBACK
    END-IF

    PERFORM UNTIL EXIT-LOOP NOT = 0
        CALL "JsonParse-ObjectKey" USING LK-JSON OFFSET LK-FAILURE PACKET-NAME
        IF LK-FAILURE NOT = 0
            GOBACK
        END-IF
        PERFORM ParsePacket
        CALL "JsonParse-Comma" USING LK-JSON OFFSET EXIT-LOOP
    END-PERFORM

    CALL "JsonParse-ObjectEnd" USING LK-JSON OFFSET LK-FAILURE
    .

ParsePacket.
    INITIALIZE PACKET-REFERENCE
    STRING FUNCTION TRIM(STATE-NAME) "/" FUNCTION TRIM(DIRECTION-NAME) "/" FUNCTION TRIM(PACKET-NAME) INTO PACKET-REFERENCE
    MOVE -1 TO PACKET-ID

    CALL "JsonParse-ObjectStart" USING LK-JSON OFFSET LK-FAILURE
    IF LK-FAILURE NOT = 0
        GOBACK
    END-IF

    PERFORM UNTIL EXIT-LOOP NOT = 0
        CALL "JsonParse-ObjectKey" USING LK-JSON OFFSET LK-FAILURE OBJECT-KEY
        IF LK-FAILURE NOT = 0
            GOBACK
        END-IF
        EVALUATE OBJECT-KEY
            WHEN "protocol_id"
                CALL "JsonParse-Integer" USING LK-JSON OFFSET LK-FAILURE PACKET-ID
            WHEN OTHER
                CALL "JsonParse-SkipValue" USING LK-JSON OFFSET LK-FAILURE
        END-EVALUATE
        IF LK-FAILURE NOT = 0
            GOBACK
        END-IF
        CALL "JsonParse-Comma" USING LK-JSON OFFSET EXIT-LOOP
    END-PERFORM

    CALL "JsonParse-ObjectEnd" USING LK-JSON OFFSET LK-FAILURE

    IF PACKET-ID = -1
        DISPLAY "Missing protocol_id for packet '" FUNCTION TRIM(PACKET-REFERENCE) "'"
        MOVE 1 TO LK-FAILURE
        GOBACK
    END-IF

    ADD 1 TO PACKET-ID-COUNT
    MOVE PACKET-REFERENCE TO PACKET-ID-REFERENCE(PACKET-ID-COUNT)
    MOVE PACKET-ID TO PACKET-ID-NUMBER(PACKET-ID-COUNT)

    MOVE PACKET-NAME TO PACKET-REF-NAME(STATE-ID + 1, DIRECTION-ID + 1, PACKET-ID + 1)
    .

END PROGRAM Packets-Parse.

*> --- Packets-GetCount ---
*> Get the number of packets defined in the packets.json file.
IDENTIFICATION DIVISION.
PROGRAM-ID. Packets-GetCount.

DATA DIVISION.
WORKING-STORAGE SECTION.
    COPY DD-PACKET-IDS.
LINKAGE SECTION.
    01 LK-COUNT                 BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-COUNT.
    MOVE PACKET-ID-COUNT TO LK-COUNT
    GOBACK.

END PROGRAM Packets-GetCount.

*> --- Packets-GetId ---
*> Look up the numeric ID of a packet by its string reference (e.g. "play/clientbound/minecraft:block_changed_ack").
IDENTIFICATION DIVISION.
PROGRAM-ID. Packets-GetId.

DATA DIVISION.
WORKING-STORAGE SECTION.
    COPY DD-PACKET-IDS.
    01 PACKET-INDEX             BINARY-LONG UNSIGNED.
LINKAGE SECTION.
    01 PACKET-REFERENCE         PIC X ANY LENGTH.
    01 PACKET-ID                BINARY-LONG.

PROCEDURE DIVISION USING PACKET-REFERENCE PACKET-ID.
    PERFORM VARYING PACKET-INDEX FROM 1 BY 1 UNTIL PACKET-INDEX > PACKET-ID-COUNT
        IF PACKET-REFERENCE = PACKET-ID-REFERENCE(PACKET-INDEX)
            MOVE PACKET-ID-NUMBER(PACKET-INDEX) TO PACKET-ID
            GOBACK
        END-IF
    END-PERFORM
    MOVE -1 TO PACKET-ID
    GOBACK.

END PROGRAM Packets-GetId.

*> --- Packets-GetName ---
*> Look up the name of a packet by its client state, direction (serverbound/clientbound), and numeric ID.
IDENTIFICATION DIVISION.
PROGRAM-ID. Packets-GetReference.

DATA DIVISION.
WORKING-STORAGE SECTION.
    COPY DD-PACKET-IDS.
    COPY DD-CLIENT-STATES.
    COPY DD-PACKET-DIRECTIONS.
LINKAGE SECTION.
    01 CLIENT-STATE             BINARY-CHAR.            *> from DD-CLIENT-STATES
    01 PACKET-DIRECTION         BINARY-LONG UNSIGNED.   *> from DD-PACKET-DIRECTIONS
    01 PACKET-ID                BINARY-LONG.
    01 PACKET-NAME              PIC X ANY LENGTH.

PROCEDURE DIVISION USING CLIENT-STATE PACKET-DIRECTION PACKET-ID PACKET-NAME.
    EVALUATE TRUE
        WHEN CLIENT-STATE < 0 OR > CLIENT-STATE-PLAY
            DISPLAY "Error: Packets-GetReference: Invalid client state " CLIENT-STATE
            MOVE SPACES TO PACKET-NAME

        WHEN NOT (PACKET-DIRECTION = PACKET-DIRECTION-SERVERBOUND OR PACKET-DIRECTION-CLIENTBOUND)
            DISPLAY "Error: Packets-GetReference: Invalid packet direction " PACKET-DIRECTION
            MOVE SPACES TO PACKET-NAME

        WHEN PACKET-ID < 0 OR >= PACKET-REF-ENTRY-LENGTH
            DISPLAY "Error: Packets-GetReference: Invalid packet ID " PACKET-ID
            MOVE SPACES TO PACKET-NAME

        WHEN OTHER
            MOVE PACKET-REF-NAME(CLIENT-STATE + 1, PACKET-DIRECTION + 1, PACKET-ID + 1) TO PACKET-NAME
    END-EVALUATE

    GOBACK.

END PROGRAM Packets-GetReference.

*> --- InitializePacketHandlers ---
*> Initialize the packet handler lookup table.
IDENTIFICATION DIVISION.
PROGRAM-ID. InitializePacketHandlers.

DATA DIVISION.
WORKING-STORAGE SECTION.
    COPY DD-PACKET-HANDLERS.

PROCEDURE DIVISION.
    INITIALIZE PACKET-HANDLERS-TABLE
    GOBACK.

END PROGRAM InitializePacketHandlers.

*> --- RegisterPacketHandler ---
*> Register a packet handler for a specific client state and incoming packet ID.
IDENTIFICATION DIVISION.
PROGRAM-ID. RegisterPacketHandler.

DATA DIVISION.
WORKING-STORAGE SECTION.
    COPY DD-PACKET-HANDLERS.
    COPY DD-CLIENT-STATES.
    01 PACKET-REFERENCE         PIC X(255).
    01 PACKET-ID                BINARY-LONG.
    01 HANDLER-PTR              PROGRAM-POINTER.
LINKAGE SECTION.
    01 LK-CLIENT-STATE          BINARY-CHAR.   *> from DD-CLIENT-STATES
    *> packet identifier, such as "minecraft:container_close"
    01 LK-PACKET-NAME           PIC X ANY LENGTH.
    *> reference to the packet handler program
    01 LK-HANDLER-NAME          PIC X ANY LENGTH.

PROCEDURE DIVISION USING LK-CLIENT-STATE LK-PACKET-NAME LK-HANDLER-NAME.
    INITIALIZE PACKET-REFERENCE
    EVALUATE LK-CLIENT-STATE
        WHEN CLIENT-STATE-HANDSHAKE
            STRING "handshake/serverbound/" FUNCTION TRIM(LK-PACKET-NAME) INTO PACKET-REFERENCE
        WHEN CLIENT-STATE-STATUS
            STRING "status/serverbound/" FUNCTION TRIM(LK-PACKET-NAME)INTO PACKET-REFERENCE
        WHEN CLIENT-STATE-LOGIN
            STRING "login/serverbound/" FUNCTION TRIM(LK-PACKET-NAME) INTO PACKET-REFERENCE
        WHEN CLIENT-STATE-CONFIGURATION
            STRING "configuration/serverbound/" FUNCTION TRIM(LK-PACKET-NAME) INTO PACKET-REFERENCE
        WHEN CLIENT-STATE-PLAY
            STRING "play/serverbound/" FUNCTION TRIM(LK-PACKET-NAME) INTO PACKET-REFERENCE
        WHEN OTHER
            COPY ASSERT-FAILED REPLACING MSG BY =="Error: RegisterPacketHandler: Invalid client state " LK-CLIENT-STATE==.
    END-EVALUATE

    CALL "Packets-GetId" USING PACKET-REFERENCE PACKET-ID

    COPY ASSERT REPLACING COND BY ==PACKET-ID >= 0==,
        MSG BY =="RegisterPacketHandler: Invalid packet reference '" FUNCTION TRIM(PACKET-REFERENCE) "'"==.
    COPY ASSERT REPLACING COND BY ==PACKET-ID < PACKET-HANDLERS-CAPACITY==,
        MSG BY =="RegisterPacketHandler: ID of packet '" FUNCTION TRIM(PACKET-REFERENCE) "' exceeds capacity"==.

    SET HANDLER-PTR TO ENTRY LK-HANDLER-NAME

    COPY ASSERT REPLACING COND BY ==HANDLER-PTR NOT = NULL==,
        MSG BY =="RegisterPacketHandler: Invalid handler name '" FUNCTION TRIM(LK-HANDLER-NAME) "'"==.

    MOVE HANDLER-PTR TO PACKET-HANDLER(LK-CLIENT-STATE + 1, PACKET-ID + 1)
    GOBACK.

END PROGRAM RegisterPacketHandler.

*> --- HandlePacket ---
*> Handle an incoming packet from the client.
IDENTIFICATION DIVISION.
PROGRAM-ID. HandlePacket.

DATA DIVISION.
WORKING-STORAGE SECTION.
    COPY DD-PACKET-HANDLERS.
    COPY DD-CLIENT-STATES.
    COPY DD-PACKET-DIRECTIONS.
    01 PACKET-DIRECTION         BINARY-LONG UNSIGNED        VALUE PACKET-DIRECTION-SERVERBOUND.
    01 HANDLER-PTR              PROGRAM-POINTER.
    01 CONSUMED                 BINARY-LONG UNSIGNED.
    01 PACKET-REFERENCE         PIC X(255).
LINKAGE SECTION.
    01 LK-CLIENT                BINARY-LONG UNSIGNED.
    01 LK-CLIENT-STATE          BINARY-CHAR.
    01 LK-PACKET-ID             BINARY-LONG.
    01 LK-PAYLOAD               PIC X ANY LENGTH.
    01 LK-PAYLOAD-LENGTH        BINARY-LONG UNSIGNED.
    01 LK-OFFSET                BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-CLIENT LK-CLIENT-STATE LK-PACKET-ID LK-PAYLOAD LK-PAYLOAD-LENGTH LK-OFFSET.
    IF LK-CLIENT-STATE < 0 OR > CLIENT-STATE-PLAY
        DISPLAY "Error: HandlePacket: Invalid client state " LK-CLIENT-STATE
        CALL "Server-DisconnectClient" USING LK-CLIENT
        GOBACK
    END-IF
    IF LK-PACKET-ID < 0 OR >= PACKET-HANDLERS-CAPACITY
        DISPLAY "[state=" LK-CLIENT-STATE "] Unexpected packet: " LK-PACKET-ID
        GOBACK
    END-IF
    MOVE PACKET-HANDLER(LK-CLIENT-STATE + 1, LK-PACKET-ID + 1) TO HANDLER-PTR
    IF HANDLER-PTR NOT = NULL
        CALL HANDLER-PTR USING LK-CLIENT LK-PAYLOAD LK-OFFSET
        COMPUTE CONSUMED = LK-OFFSET - 1
        IF CONSUMED NOT = LK-PAYLOAD-LENGTH
            CALL "Packets-GetReference" USING LK-CLIENT-STATE PACKET-DIRECTION LK-PACKET-ID PACKET-REFERENCE
            DISPLAY "[state=" LK-CLIENT-STATE "] Packet " FUNCTION TRIM(PACKET-REFERENCE) " handler consumed "
                CONSUMED " of " LK-PAYLOAD-LENGTH " bytes"
        END-IF
    END-IF
    GOBACK.

END PROGRAM HandlePacket.

*> --- SendPacket ---
*> Send a raw packet to the client.
IDENTIFICATION DIVISION.
PROGRAM-ID. SendPacket.

DATA DIVISION.
WORKING-STORAGE SECTION.
    COPY DD-CLIENTS.
    01 NUM-BYTES                BINARY-LONG UNSIGNED.
    01 HEADER                   PIC X(10).
    01 HEADER-OFFSET            BINARY-LONG UNSIGNED.
    01 TOTAL-LENGTH             BINARY-LONG UNSIGNED.
    01 HNDL                     PIC X(4).
    01 ERRNO                    PIC 9(3).
LINKAGE SECTION.
    01 LK-CLIENT                BINARY-LONG UNSIGNED.
    01 LK-PACKET-ID             BINARY-LONG.
    01 LK-PAYLOAD               PIC X ANY LENGTH.
    01 LK-PAYLOAD-LENGTH        BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-CLIENT LK-PACKET-ID LK-PAYLOAD LK-PAYLOAD-LENGTH.
    *> Don't send packet if the client is already in an error state. It will be disconnected on the next tick.
    IF CLIENT-ERRNO-SEND(LK-CLIENT) NOT = 0
        EXIT PROGRAM
    END-IF
    MOVE CLIENT-HNDL(LK-CLIENT) TO HNDL

    *> Packet length = length of packet ID + length of payload
    CALL "Encode-GetVarIntLength" USING LK-PACKET-ID NUM-BYTES
    COMPUTE TOTAL-LENGTH = NUM-BYTES + LK-PAYLOAD-LENGTH

    *> Send header: payload length, packet ID
    MOVE 1 TO HEADER-OFFSET
    CALL "Encode-VarInt" USING TOTAL-LENGTH HEADER HEADER-OFFSET
    CALL "Encode-VarInt" USING LK-PACKET-ID HEADER HEADER-OFFSET
    COMPUTE NUM-BYTES = HEADER-OFFSET - 1
    CALL "SocketWrite" USING HNDL NUM-BYTES HEADER GIVING ERRNO
    PERFORM HandleError

    *> Send packet data
    CALL "SocketWrite" USING HNDL LK-PAYLOAD-LENGTH LK-PAYLOAD GIVING ERRNO
    PERFORM HandleError

    GOBACK.

HandleError.
    IF ERRNO NOT = 0
        *> Mark the client as errored
        MOVE ERRNO TO CLIENT-ERRNO-SEND(LK-CLIENT)
        GOBACK
    END-IF
    .

END PROGRAM SendPacket.
