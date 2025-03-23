*> --- CG-Packets ---
IDENTIFICATION DIVISION.
PROGRAM-ID. CG-Packets.

DATA DIVISION.
WORKING-STORAGE SECTION.
    *> output buffer
    01 BUFFER                       PIC X(1000000).
    01 BUFFERLEN                    BINARY-LONG UNSIGNED.
    *> template
    01 REPLACE-PTR                  PROGRAM-POINTER.
    COPY DD-CODEGEN-TEMPLATE REPLACING LEADING ==PREFIX== BY ==MAIN==.

PROCEDURE DIVISION.
    SET REPLACE-PTR TO ENTRY "CG-Packets-Main"
    CALL "Codegen-TemplateLoad" USING "packets/main.tpl.cob" REPLACE-PTR MAIN-TPL

    CALL "Codegen-Start" USING "packets.cob"

    MOVE 0 TO BUFFERLEN
    CALL "Codegen-TemplateEval" USING MAIN-TPL BUFFER BUFFERLEN
    CALL "Codegen-Append" USING BUFFER(1:BUFFERLEN)

    CALL "Codegen-End"

    GOBACK.

END PROGRAM CG-Packets.

*> --- CG-Packets-Main ---
IDENTIFICATION DIVISION.
PROGRAM-ID. CG-Packets-Main.

DATA DIVISION.
WORKING-STORAGE SECTION.
    *> initialized once
    01 INIT-DONE                    BINARY-CHAR UNSIGNED.
    01 REPLACE-PTR                  PROGRAM-POINTER.
    COPY DD-CODEGEN-TEMPLATE REPLACING LEADING ==PREFIX== BY ==STATE==.
    *> shared state
    COPY DD-CODEGEN-JSON.
    01 STATE-NAME                   PIC X(64)                   EXTERNAL.
    *> dynamic variables
    01 JSONLEN                      BINARY-LONG UNSIGNED.
    01 FAILURE                      BINARY-CHAR UNSIGNED.
LINKAGE SECTION.
    01 LK-VARNAME                   PIC X ANY LENGTH.
    01 LK-BUFFER                    PIC X ANY LENGTH.
    01 LK-LENGTH                    BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-VARNAME LK-BUFFER LK-LENGTH.
    IF INIT-DONE = 0
        SET REPLACE-PTR TO ENTRY "CG-Packets-State"
        CALL "Codegen-TemplateLoad" USING "packets/state.tpl.cob" REPLACE-PTR STATE-TPL

        CALL "Codegen-ReadDataFile" USING "generated/reports/packets.json" JSONBUF JSONLEN

        MOVE 1 TO INIT-DONE
    END-IF

    MOVE 1 TO JSONPOS

    EVALUATE LK-VARNAME
        WHEN "registrations"
            CALL "JsonParse-ObjectStart" USING JSONBUF JSONPOS FAILURE
            PERFORM AssertOk

            PERFORM UNTIL EXIT
                CALL "JsonParse-ObjectKey" USING JSONBUF JSONPOS FAILURE STATE-NAME
                PERFORM AssertOk

                CALL "Codegen-TemplateEval" USING STATE-TPL LK-BUFFER LK-LENGTH

                CALL "JsonParse-Comma" USING JSONBUF JSONPOS FAILURE
                IF FAILURE > 0 EXIT PERFORM END-IF
            END-PERFORM

            CALL "JsonParse-ObjectEnd" USING JSONBUF JSONPOS FAILURE
            PERFORM AssertOk
    END-EVALUATE

    GOBACK.

AssertOk.
    COPY ASSERT REPLACING COND BY ==FAILURE = 0==, MSG BY =="CG-Packets-Main: Failed to parse JSON"==.
    .

END PROGRAM CG-Packets-Main.

*> --- CG-Packets-State ---
IDENTIFICATION DIVISION.
PROGRAM-ID. CG-Packets-State.

DATA DIVISION.
WORKING-STORAGE SECTION.
    *> initialized once
    01 INIT-DONE                    BINARY-CHAR UNSIGNED.
    01 REPLACE-PTR                  PROGRAM-POINTER.
    COPY DD-CODEGEN-TEMPLATE REPLACING LEADING ==PREFIX== BY ==DIRECTION==.
    *> shared state
    COPY DD-CODEGEN-JSON.
    01 STATE-NAME                   PIC X(64)                   EXTERNAL.
    01 DIRECTION-NAME               PIC X(64)                   EXTERNAL.
    *> dynamic variables
    01 FAILURE                      BINARY-CHAR UNSIGNED.
LINKAGE SECTION.
    01 LK-VARNAME                   PIC X ANY LENGTH.
    01 LK-BUFFER                    PIC X ANY LENGTH.
    01 LK-LENGTH                    BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-VARNAME LK-BUFFER LK-LENGTH.
    IF INIT-DONE = 0
        SET REPLACE-PTR TO ENTRY "CG-Packets-Direction"
        CALL "Codegen-TemplateLoad" USING "packets/direction.tpl.cob" REPLACE-PTR DIRECTION-TPL
        MOVE 1 TO INIT-DONE
    END-IF

    EVALUATE LK-VARNAME
        WHEN "state"
            PERFORM ReplaceState
        WHEN "body"
            PERFORM ReplaceBody
    END-EVALUATE

    GOBACK.

ReplaceState.
    EVALUATE STATE-NAME
        WHEN "handshake"
            STRING "CLIENT-STATE-HANDSHAKE" INTO LK-BUFFER(LK-LENGTH + 1:)
            ADD 22 TO LK-LENGTH
        WHEN "status"
            STRING "CLIENT-STATE-STATUS" INTO LK-BUFFER(LK-LENGTH + 1:)
            ADD 19 TO LK-LENGTH
        WHEN "login"
            STRING "CLIENT-STATE-LOGIN" INTO LK-BUFFER(LK-LENGTH + 1:)
            ADD 18 TO LK-LENGTH
        WHEN "configuration"
            STRING "CLIENT-STATE-CONFIGURATION" INTO LK-BUFFER(LK-LENGTH + 1:)
            ADD 26 TO LK-LENGTH
        WHEN "play"
            STRING "CLIENT-STATE-PLAY" INTO LK-BUFFER(LK-LENGTH + 1:)
            ADD 17 TO LK-LENGTH
        WHEN OTHER
            COPY ASSERT-FAILED REPLACING MSG BY =="CG-Packets-State: Invalid state: " STATE-NAME==.
    END-EVALUATE
    .

ReplaceBody.
    CALL "JsonParse-ObjectStart" USING JSONBUF JSONPOS FAILURE
    PERFORM AssertOk

    PERFORM UNTIL EXIT
        CALL "JsonParse-ObjectKey" USING JSONBUF JSONPOS FAILURE DIRECTION-NAME
        PERFORM AssertOk

        CALL "Codegen-TemplateEval" USING DIRECTION-TPL LK-BUFFER LK-LENGTH

        CALL "JsonParse-Comma" USING JSONBUF JSONPOS FAILURE
        IF FAILURE > 0 EXIT PERFORM END-IF
    END-PERFORM

    CALL "JsonParse-ObjectEnd" USING JSONBUF JSONPOS FAILURE
    PERFORM AssertOk
    .

AssertOk.
    COPY ASSERT REPLACING COND BY ==FAILURE = 0==, MSG BY =="CG-Packets-State: Failed to parse JSON"==.
    .

END PROGRAM CG-Packets-State.

*> --- CG-Packets-Direction ---
IDENTIFICATION DIVISION.
PROGRAM-ID. CG-Packets-Direction.

DATA DIVISION.
WORKING-STORAGE SECTION.
    *> initialized once
    01 INIT-DONE                    BINARY-CHAR UNSIGNED.
    01 REPLACE-PTR                  PROGRAM-POINTER.
    COPY DD-CODEGEN-TEMPLATE REPLACING LEADING ==PREFIX== BY ==PACKET==.
    *> shared state
    COPY DD-CODEGEN-JSON.
    01 DIRECTION-NAME               PIC X(64)                   EXTERNAL.
    01 PACKET-NAME                  PIC X(64)                   EXTERNAL.
    *> dynamic variables
    01 FAILURE                      BINARY-CHAR UNSIGNED.
LINKAGE SECTION.
    01 LK-VARNAME                   PIC X ANY LENGTH.
    01 LK-BUFFER                    PIC X ANY LENGTH.
    01 LK-LENGTH                    BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-VARNAME LK-BUFFER LK-LENGTH.
    IF INIT-DONE = 0
        SET REPLACE-PTR TO ENTRY "CG-Packets-Packet"
        CALL "Codegen-TemplateLoad" USING "packets/packet.tpl.cob" REPLACE-PTR PACKET-TPL
        MOVE 1 TO INIT-DONE
    END-IF

    EVALUATE LK-VARNAME
        WHEN "direction"
            PERFORM ReplaceDirection
        WHEN "body"
            PERFORM ReplaceBody
    END-EVALUATE

    GOBACK.

ReplaceDirection.
    EVALUATE DIRECTION-NAME
        WHEN "clientbound"
            STRING "PACKET-DIRECTION-CLIENTBOUND" INTO LK-BUFFER(LK-LENGTH + 1:)
            ADD 28 TO LK-LENGTH
        WHEN "serverbound"
            STRING "PACKET-DIRECTION-SERVERBOUND" INTO LK-BUFFER(LK-LENGTH + 1:)
            ADD 28 TO LK-LENGTH
        WHEN OTHER
            COPY ASSERT-FAILED REPLACING MSG BY =="CG-Packets-Direction: Invalid direction: " DIRECTION-NAME==.
    END-EVALUATE
    .

ReplaceBody.
    CALL "JsonParse-ObjectStart" USING JSONBUF JSONPOS FAILURE
    PERFORM AssertOk

    PERFORM UNTIL EXIT
        CALL "JsonParse-ObjectKey" USING JSONBUF JSONPOS FAILURE PACKET-NAME
        PERFORM AssertOk

        CALL "Codegen-TemplateEval" USING PACKET-TPL LK-BUFFER LK-LENGTH

        CALL "JsonParse-Comma" USING JSONBUF JSONPOS FAILURE
        IF FAILURE > 0 EXIT PERFORM END-IF
    END-PERFORM

    CALL "JsonParse-ObjectEnd" USING JSONBUF JSONPOS FAILURE
    PERFORM AssertOk
    .

AssertOk.
    COPY ASSERT REPLACING COND BY ==FAILURE = 0==, MSG BY =="CG-Packets-Direction: Failed to parse JSON"==.
    .

END PROGRAM CG-Packets-Direction.

*> --- CG-Packets-Packet ---
IDENTIFICATION DIVISION.
PROGRAM-ID. CG-Packets-Packet.

DATA DIVISION.
WORKING-STORAGE SECTION.
    *> shared state
    COPY DD-CODEGEN-JSON.
    01 PACKET-NAME                  PIC X(64)                   EXTERNAL.
    *> dynamic variables
    01 FAILURE                      BINARY-CHAR UNSIGNED.
    01 STR                          PIC X(64).
    01 PACKET-ID                    BINARY-LONG.
LINKAGE SECTION.
    01 LK-VARNAME                   PIC X ANY LENGTH.
    01 LK-BUFFER                    PIC X ANY LENGTH.
    01 LK-LENGTH                    BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-VARNAME LK-BUFFER LK-LENGTH.
    EVALUATE LK-VARNAME
        WHEN "packet-name"
            STRING PACKET-NAME INTO LK-BUFFER(LK-LENGTH + 1:)
            ADD FUNCTION STORED-CHAR-LENGTH(PACKET-NAME) TO LK-LENGTH
        WHEN "packet-id"
            PERFORM ReplacePacketId
    END-EVALUATE

    GOBACK.

ReplacePacketId.
    MOVE -1 TO PACKET-ID

    CALL "JsonParse-ObjectStart" USING JSONBUF JSONPOS FAILURE
    PERFORM AssertOk

    PERFORM UNTIL EXIT
        CALL "JsonParse-ObjectKey" USING JSONBUF JSONPOS FAILURE STR
        PERFORM AssertOk

        EVALUATE STR
            WHEN "protocol_id"
                CALL "JsonParse-Integer" USING JSONBUF JSONPOS FAILURE PACKET-ID
                PERFORM AssertOk
            WHEN OTHER
                COPY ASSERT-FAILED REPLACING MSG BY =="CG-Packets-Packet: Unexpected key: " STR==.
        END-EVALUATE

        CALL "JsonParse-Comma" USING JSONBUF JSONPOS FAILURE
        IF FAILURE > 0 EXIT PERFORM END-IF
    END-PERFORM

    CALL "JsonParse-ObjectEnd" USING JSONBUF JSONPOS FAILURE
    PERFORM AssertOk

    COPY ASSERT REPLACING COND BY ==PACKET-ID >= 0==, MSG BY =="CG-Packets-Packet: Packet ID not set"==.
    CALL "Codegen-WriteInt" USING PACKET-ID LK-BUFFER LK-LENGTH
    .

AssertOk.
    COPY ASSERT REPLACING COND BY ==FAILURE = 0==, MSG BY =="CG-Packets-Packet: Failed to parse JSON"==.
    .

END PROGRAM CG-Packets-Packet.
