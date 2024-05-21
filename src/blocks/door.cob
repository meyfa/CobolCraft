*> --- RegisterBlock-Door ---
IDENTIFICATION DIVISION.
PROGRAM-ID. RegisterBlock-Door.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 C-MINECRAFT-DOOR                 PIC X(32) GLOBAL    VALUE "minecraft:door".
    01 DESTROY-PTR                      PROGRAM-POINTER.
    01 INTERACT-PTR                     PROGRAM-POINTER.
    01 FACE-PTR                         PROGRAM-POINTER.
    01 BLOCK-COUNT                      BINARY-LONG UNSIGNED.
    01 BLOCK-INDEX                      BINARY-LONG UNSIGNED.
    01 BLOCK-TYPE                       PIC X(64).
    01 BLOCK-MINIMUM-STATE-ID           BINARY-LONG.
    01 BLOCK-MAXIMUM-STATE-ID           BINARY-LONG.
    01 STATE-ID                         BINARY-LONG.

PROCEDURE DIVISION.
    SET DESTROY-PTR TO ENTRY "Callback-Destroy"
    SET INTERACT-PTR TO ENTRY "Callback-Interact"
    SET FACE-PTR TO ENTRY "Callback-Face"

    *> Loop over all blocks and register the callback for each matching block type
    CALL "Blocks-GetCount" USING BLOCK-COUNT
    PERFORM VARYING BLOCK-INDEX FROM 1 BY 1 UNTIL BLOCK-INDEX > BLOCK-COUNT
        CALL "Blocks-Iterate-Type" USING BLOCK-INDEX BLOCK-TYPE
        *> TODO check for door block type (e.g., iron doors cannot be opened by clicking)
        IF BLOCK-TYPE = C-MINECRAFT-DOOR
            CALL "Blocks-Iterate-StateIds" USING BLOCK-INDEX BLOCK-MINIMUM-STATE-ID BLOCK-MAXIMUM-STATE-ID
            PERFORM VARYING STATE-ID FROM BLOCK-MINIMUM-STATE-ID BY 1 UNTIL STATE-ID > BLOCK-MAXIMUM-STATE-ID
                CALL "SetCallback-BlockDestroy" USING STATE-ID DESTROY-PTR
                CALL "SetCallback-BlockInteract" USING STATE-ID INTERACT-PTR
                CALL "SetCallback-BlockFace" USING STATE-ID FACE-PTR
            END-PERFORM
        END-IF
    END-PERFORM

    GOBACK.

    *> --- Callback-Destroy ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Callback-Destroy.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        01 AIR-BLOCK-STATE          BINARY-LONG             VALUE 0.
        01 NULL-CLIENT              BINARY-LONG             VALUE 0.
        01 C-HALF                   PIC X(4)                VALUE "half".
        01 BLOCK-ID                 BINARY-LONG.
        COPY DD-BLOCK-STATE REPLACING LEADING ==PREFIX== BY ==CLICKED==.
        COPY DD-BLOCK-STATE REPLACING LEADING ==PREFIX== BY ==OTHER-HALF==.
        01 HALF-VALUE-CLICKED       PIC X(16).
        01 HALF-VALUE-OTHER         PIC X(16).
        01 OPEN-VALUE               PIC X(16).
        01 BLOCK-POSITION.
            02 BLOCK-X              BINARY-LONG.
            02 BLOCK-Y              BINARY-LONG.
            02 BLOCK-Z              BINARY-LONG.
        COPY DD-PLAYERS.
    LINKAGE SECTION.
        COPY DD-CALLBACK-BLOCK-DESTROY.

    PROCEDURE DIVISION USING LK-PLAYER LK-POSITION LK-FACE.
        *> Obtain the clicked block state description
        CALL "World-GetBlock" USING LK-POSITION BLOCK-ID
        CALL "Blocks-Get-StateDescription" USING BLOCK-ID CLICKED-DESCRIPTION

        *> Set the clicked block to air
        CALL "World-SetBlock" USING PLAYER-CLIENT(LK-PLAYER) LK-POSITION AIR-BLOCK-STATE

        *> Find the other half
        CALL "Blocks-Description-GetValue" USING CLICKED-DESCRIPTION C-HALF HALF-VALUE-CLICKED
        MOVE LK-POSITION TO BLOCK-POSITION
        IF HALF-VALUE-CLICKED = "upper"
            SUBTRACT 1 FROM BLOCK-Y
        ELSE
            ADD 1 TO BLOCK-Y
        END-IF
        CALL "World-GetBlock" USING BLOCK-POSITION BLOCK-ID
        CALL "Blocks-Get-StateDescription" USING BLOCK-ID OTHER-HALF-DESCRIPTION

        *> Check if the block matches (normally there shouldn't be single-block doors, but just in case)
        IF OTHER-HALF-NAME NOT = CLICKED-NAME
            GOBACK
        END-IF
        CALL "Blocks-Description-GetValue" USING OTHER-HALF-DESCRIPTION C-HALF HALF-VALUE-OTHER
        IF HALF-VALUE-CLICKED = HALF-VALUE-OTHER
            GOBACK
        END-IF

        *> Set the other half to air
        *> Note: We don't pass the player client here because they should receive the particle and sound effects, too.
        *>       For the clicked block, the client has already predicted the removal and played the effects.
        CALL "World-SetBlock" USING NULL-CLIENT BLOCK-POSITION AIR-BLOCK-STATE

        GOBACK.

    END PROGRAM Callback-Destroy.

    *> --- Callback-Interact ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Callback-Interact.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        01 C-HALF                   PIC X(4)                VALUE "half".
        01 C-OPEN                   PIC X(4)                VALUE "open".
        01 BLOCK-ID                 BINARY-LONG.
        COPY DD-BLOCK-STATE REPLACING LEADING ==PREFIX== BY ==CLICKED==.
        COPY DD-BLOCK-STATE REPLACING LEADING ==PREFIX== BY ==OTHER-HALF==.
        01 HALF-VALUE-CLICKED       PIC X(16).
        01 HALF-VALUE-OTHER         PIC X(16).
        01 OPEN-VALUE               PIC X(16).
        01 BLOCK-POSITION.
            02 BLOCK-X              BINARY-LONG.
            02 BLOCK-Y              BINARY-LONG.
            02 BLOCK-Z              BINARY-LONG.
        COPY DD-PLAYERS.
    LINKAGE SECTION.
        COPY DD-CALLBACK-BLOCK-INTERACT.

    PROCEDURE DIVISION USING LK-PLAYER LK-ITEM-NAME LK-POSITION LK-FACE LK-CURSOR.
        *> Obtain the current block state description
        CALL "World-GetBlock" USING LK-POSITION BLOCK-ID
        CALL "Blocks-Get-StateDescription" USING BLOCK-ID CLICKED-DESCRIPTION

        *> Toggle the "open" property for the clicked half
        CALL "Blocks-Description-GetValue" USING CLICKED-DESCRIPTION C-OPEN OPEN-VALUE
        IF OPEN-VALUE = "true"
            MOVE "false" TO OPEN-VALUE
        ELSE
            MOVE "true" TO OPEN-VALUE
        END-IF
        CALL "Blocks-Description-SetValue" USING CLICKED-DESCRIPTION C-OPEN OPEN-VALUE
        CALL "Blocks-Get-StateId" USING CLICKED-DESCRIPTION BLOCK-ID
        CALL "World-SetBlock" USING PLAYER-CLIENT(LK-PLAYER) LK-POSITION BLOCK-ID

        *> Find the other half
        CALL "Blocks-Description-GetValue" USING CLICKED-DESCRIPTION C-HALF HALF-VALUE-CLICKED
        MOVE LK-POSITION TO BLOCK-POSITION
        IF HALF-VALUE-CLICKED = "upper"
            SUBTRACT 1 FROM BLOCK-Y
        ELSE
            ADD 1 TO BLOCK-Y
        END-IF
        CALL "World-GetBlock" USING BLOCK-POSITION BLOCK-ID
        CALL "Blocks-Get-StateDescription" USING BLOCK-ID OTHER-HALF-DESCRIPTION

        *> Check if the block matches (normally there shouldn't be single-block doors, but just in case)
        IF OTHER-HALF-NAME NOT = CLICKED-NAME
            GOBACK
        END-IF
        CALL "Blocks-Description-GetValue" USING OTHER-HALF-DESCRIPTION C-HALF HALF-VALUE-OTHER
        IF HALF-VALUE-CLICKED = HALF-VALUE-OTHER
            GOBACK
        END-IF

        *> Toggle the "open" property for the other half
        CALL "Blocks-Description-SetValue" USING OTHER-HALF-DESCRIPTION C-OPEN OPEN-VALUE
        CALL "Blocks-Get-StateId" USING OTHER-HALF-DESCRIPTION BLOCK-ID
        CALL "World-SetBlock" USING PLAYER-CLIENT(LK-PLAYER) BLOCK-POSITION BLOCK-ID

        GOBACK.

    END PROGRAM Callback-Interact.

    *> --- Callback-Face ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Callback-Face.

    DATA DIVISION.
    LINKAGE SECTION.
        COPY DD-CALLBACK-BLOCK-FACE.

    PROCEDURE DIVISION USING LK-BLOCK-STATE LK-FACE LK-RESULT.
        *> Doors have no solid faces.
        MOVE 0 TO LK-RESULT
        GOBACK.

    END PROGRAM Callback-Face.

END PROGRAM RegisterBlock-Door.
