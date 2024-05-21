*> --- RegisterBlock-Bed ---
IDENTIFICATION DIVISION.
PROGRAM-ID. RegisterBlock-Bed.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 C-MINECRAFT-BED                  PIC X(32) GLOBAL    VALUE "minecraft:bed".
    01 DESTROY-PTR                      PROGRAM-POINTER.
    01 FACE-PTR                         PROGRAM-POINTER.
    01 BLOCK-COUNT                      BINARY-LONG UNSIGNED.
    01 BLOCK-INDEX                      BINARY-LONG UNSIGNED.
    01 BLOCK-TYPE                       PIC X(64).
    01 BLOCK-MINIMUM-STATE-ID           BINARY-LONG.
    01 BLOCK-MAXIMUM-STATE-ID           BINARY-LONG.
    01 STATE-ID                         BINARY-LONG.

PROCEDURE DIVISION.
    SET DESTROY-PTR TO ENTRY "Callback-Destroy"
    SET FACE-PTR TO ENTRY "Callback-Face"

    *> Loop over all blocks and register the callback for each matching block type
    CALL "Blocks-GetCount" USING BLOCK-COUNT
    PERFORM VARYING BLOCK-INDEX FROM 1 BY 1 UNTIL BLOCK-INDEX > BLOCK-COUNT
        CALL "Blocks-Iterate-Type" USING BLOCK-INDEX BLOCK-TYPE
        IF BLOCK-TYPE = C-MINECRAFT-BED
            CALL "Blocks-Iterate-StateIds" USING BLOCK-INDEX BLOCK-MINIMUM-STATE-ID BLOCK-MAXIMUM-STATE-ID
            PERFORM VARYING STATE-ID FROM BLOCK-MINIMUM-STATE-ID BY 1 UNTIL STATE-ID > BLOCK-MAXIMUM-STATE-ID
                CALL "SetCallback-BlockDestroy" USING STATE-ID DESTROY-PTR
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
        01 C-FACING                 PIC X(6)                VALUE "facing".
        01 C-PART                   PIC X(4)                VALUE "part".
        01 BLOCK-ID                 BINARY-LONG.
        COPY DD-BLOCK-STATE REPLACING LEADING ==PREFIX== BY ==CLICKED==.
        COPY DD-BLOCK-STATE REPLACING LEADING ==PREFIX== BY ==OTHER-PART==.
        01 FACING-VALUE-CLICKED     PIC X(16).
        01 PART-VALUE-CLICKED       PIC X(16).
        01 PART-VALUE-OTHER         PIC X(16).
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
        CALL "Blocks-Description-GetValue" USING CLICKED-DESCRIPTION C-FACING FACING-VALUE-CLICKED
        CALL "Blocks-Description-GetValue" USING CLICKED-DESCRIPTION C-PART PART-VALUE-CLICKED
        MOVE LK-POSITION TO BLOCK-POSITION
        EVALUATE FACING-VALUE-CLICKED ALSO PART-VALUE-CLICKED
            WHEN "north" ALSO "foot"
                SUBTRACT 1 FROM BLOCK-Z
            WHEN "north" ALSO "head"
                ADD 1 TO BLOCK-Z
            WHEN "south" ALSO "foot"
                ADD 1 TO BLOCK-Z
            WHEN "south" ALSO "head"
                SUBTRACT 1 FROM BLOCK-Z
            WHEN "east" ALSO "foot"
                ADD 1 TO BLOCK-X
            WHEN "east" ALSO "head"
                SUBTRACT 1 FROM BLOCK-X
            WHEN "west" ALSO "foot"
                SUBTRACT 1 FROM BLOCK-X
            WHEN "west" ALSO "head"
                ADD 1 TO BLOCK-X
        END-EVALUATE
        CALL "World-GetBlock" USING BLOCK-POSITION BLOCK-ID
        CALL "Blocks-Get-StateDescription" USING BLOCK-ID OTHER-PART-DESCRIPTION

        *> Check if the block matches (normally there shouldn't be single-block beds, but just in case)
        IF OTHER-PART-NAME NOT = CLICKED-NAME
            GOBACK
        END-IF
        CALL "Blocks-Description-GetValue" USING OTHER-PART-DESCRIPTION C-PART PART-VALUE-OTHER
        IF PART-VALUE-CLICKED = PART-VALUE-OTHER
            GOBACK
        END-IF

        *> Set the other half to air
        *> Note: We don't pass the player client here because they should receive the particle and sound effects, too.
        *>       For the clicked block, the client has already predicted the removal and played the effects.
        CALL "World-SetBlock" USING NULL-CLIENT BLOCK-POSITION AIR-BLOCK-STATE

        GOBACK.

    END PROGRAM Callback-Destroy.

    *> --- Callback-Face ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Callback-Face.

    DATA DIVISION.
    LINKAGE SECTION.
        COPY DD-CALLBACK-BLOCK-FACE.

    PROCEDURE DIVISION USING LK-BLOCK-STATE LK-FACE LK-RESULT.
        *> Beds have no solid faces.
        MOVE 0 TO LK-RESULT
        GOBACK.

    END PROGRAM Callback-Face.

END PROGRAM RegisterBlock-Bed.
