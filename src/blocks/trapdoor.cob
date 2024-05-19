*> --- RegisterBlock-Trapdoor ---
IDENTIFICATION DIVISION.
PROGRAM-ID. RegisterBlock-Trapdoor.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 C-MINECRAFT-TRAPDOOR             PIC X(32) GLOBAL    VALUE "minecraft:trapdoor".
    01 USE-PTR                          PROGRAM-POINTER.
    01 BLOCK-COUNT                      BINARY-LONG UNSIGNED.
    01 BLOCK-INDEX                      BINARY-LONG UNSIGNED.
    01 BLOCK-TYPE                       PIC X(64).
    01 BLOCK-MINIMUM-STATE-ID           BINARY-LONG.
    01 BLOCK-MAXIMUM-STATE-ID           BINARY-LONG.
    01 STATE-ID                         BINARY-LONG.

PROCEDURE DIVISION.
    SET USE-PTR TO ENTRY "Callback-Interact"

    *> Loop over all blocks and register the callback for each trapdoor
    CALL "Blocks-GetCount" USING BLOCK-COUNT
    PERFORM VARYING BLOCK-INDEX FROM 1 BY 1 UNTIL BLOCK-INDEX > BLOCK-COUNT
        CALL "Blocks-Iterate-Type" USING BLOCK-INDEX BLOCK-TYPE
        *> TODO check for trapdoor block type (e.g., iron trapdoors cannot be opened by clicking)
        IF BLOCK-TYPE = C-MINECRAFT-TRAPDOOR
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
        01 C-OPEN                   PIC X(4)                VALUE "open".
        COPY DD-PLAYERS.
        01 BLOCK-ID                 BINARY-LONG.
        COPY DD-BLOCK-STATE REPLACING LEADING ==PREFIX== BY ==CURRENT==.
        01 OPEN-VALUE               PIC X(16).
    LINKAGE SECTION.
        COPY DD-CALLBACK-BLOCK-INTERACT.

    PROCEDURE DIVISION USING LK-PLAYER LK-ITEM-NAME LK-POSITION LK-FACE LK-CURSOR.
        *> Obtain the current block state description
        CALL "World-GetBlock" USING LK-POSITION BLOCK-ID
        CALL "Blocks-Get-StateDescription" USING BLOCK-ID CURRENT-DESCRIPTION

        *> Toggle the "open" property
        CALL "Blocks-Description-GetValue" USING CURRENT-DESCRIPTION C-OPEN OPEN-VALUE
        IF OPEN-VALUE = "true"
            MOVE "false" TO OPEN-VALUE
        ELSE
            MOVE "true" TO OPEN-VALUE
        END-IF
        CALL "Blocks-Description-SetValue" USING CURRENT-DESCRIPTION C-OPEN OPEN-VALUE

        *> Set the new block state
        CALL "Blocks-Get-StateId" USING CURRENT-DESCRIPTION BLOCK-ID
        CALL "World-SetBlock" USING PLAYER-CLIENT(LK-PLAYER) LK-POSITION BLOCK-ID

        GOBACK.

    END PROGRAM Callback-Interact.

END PROGRAM RegisterBlock-Trapdoor.
