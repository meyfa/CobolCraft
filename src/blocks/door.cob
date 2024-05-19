*> --- RegisterBlock-Door ---
IDENTIFICATION DIVISION.
PROGRAM-ID. RegisterBlock-Door.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 C-MINECRAFT-DOOR                 PIC X(32) GLOBAL    VALUE "minecraft:door".
    01 USE-PTR                          PROGRAM-POINTER.
    01 BLOCK-COUNT                      BINARY-LONG UNSIGNED.
    01 BLOCK-INDEX                      BINARY-LONG UNSIGNED.
    01 BLOCK-TYPE                       PIC X(64).
    01 BLOCK-MINIMUM-STATE-ID           BINARY-LONG.
    01 BLOCK-MAXIMUM-STATE-ID           BINARY-LONG.
    01 STATE-ID                         BINARY-LONG.

PROCEDURE DIVISION.
    SET USE-PTR TO ENTRY "Callback-Interact"

    *> Loop over all blocks and register the callback for each door
    CALL "Blocks-GetCount" USING BLOCK-COUNT
    PERFORM VARYING BLOCK-INDEX FROM 1 BY 1 UNTIL BLOCK-INDEX > BLOCK-COUNT
        CALL "Blocks-Iterate-Type" USING BLOCK-INDEX BLOCK-TYPE
        *> TODO check for door block type (e.g., iron doors cannot be opened by clicking)
        IF BLOCK-TYPE = C-MINECRAFT-DOOR
            CALL "Blocks-Iterate-StateIds" USING BLOCK-INDEX BLOCK-MINIMUM-STATE-ID BLOCK-MAXIMUM-STATE-ID
            PERFORM VARYING STATE-ID FROM BLOCK-MINIMUM-STATE-ID BY 1 UNTIL STATE-ID > BLOCK-MAXIMUM-STATE-ID
                CALL "SetCallback-BlockInteract" USING STATE-ID USE-PTR
            END-PERFORM
        END-IF
    END-PERFORM

    GOBACK.

    *> --- Callback-Interact ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Callback-Interact.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        01 C-HALF                   PIC X(4)                VALUE "half".
        01 C-OPEN                   PIC X(4)                VALUE "open".
        COPY DD-PLAYERS.
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

END PROGRAM RegisterBlock-Door.
