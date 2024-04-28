*> --- RegisterItem-Slab ---
IDENTIFICATION DIVISION.
PROGRAM-ID. RegisterItem-Slab.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 C-MINECRAFT-AIR                  PIC X(32) GLOBAL    VALUE "minecraft:air".
    01 C-MINECRAFT-STONE_SLAB           PIC X(32) GLOBAL    VALUE "minecraft:stone_slab".
    01 USE-PTR                          PROGRAM-POINTER.

PROCEDURE DIVISION.
    SET USE-PTR TO ENTRY "Callback-Use"
    *> TODO implement all slab types - can be done by checking "definition"."type" == "minecraft:slab" in blocks.json
    CALL "SetCallback-ItemUse" USING C-MINECRAFT-STONE_SLAB USE-PTR
    GOBACK.

    *> --- Callback-Use ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Callback-Use.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        01 C-TYPE                   PIC X(4)                VALUE "type".
        *> TODO move this to a copybook somehow
        01 CURRENT-DESCRIPTION.
            02 CURRENT-NAME         PIC X(64).
            02 CURRENT-PROPS        BINARY-LONG UNSIGNED.
            02 CURRENT-PROP OCCURS 16 TIMES.
                03 CURRENT-PROP-NAME    PIC X(64).
                03 CURRENT-PROP-VALUE   PIC X(64).
        01 CURRENT-TYPE             PIC X(16).
        01 SLAB-DESCRIPTION.
            02 SLAB-NAME            PIC X(64).
            02 SLAB-PROPS           BINARY-LONG UNSIGNED.
            02 SLAB-PROP OCCURS 16 TIMES.
                03 SLAB-PROP-NAME       PIC X(64).
                03 SLAB-PROP-VALUE      PIC X(64).
        01 BLOCK-POSITION.
            02 BLOCK-X              BINARY-LONG.
            02 BLOCK-Y              BINARY-LONG.
            02 BLOCK-Z              BINARY-LONG.
        01 FACING                   PIC X(16).
        01 BOUNDS-CHECK             BINARY-CHAR UNSIGNED.
        01 BLOCK-ID                 BINARY-LONG.
    LINKAGE SECTION.
        COPY DD-CALLBACK-ITEM-USE.

    PROCEDURE DIVISION USING LK-PLAYER LK-ITEM-NAME LK-POSITION LK-FACE LK-CURSOR.
        MOVE LK-ITEM-NAME TO SLAB-NAME

        MOVE 2 TO SLAB-PROPS
        MOVE C-TYPE TO SLAB-PROP-NAME(1)
        MOVE "waterlogged" TO SLAB-PROP-NAME(2)
        MOVE "false" TO SLAB-PROP-VALUE(2)

        CALL "Facing-ToString" USING LK-FACE FACING
        EVALUATE FACING
            WHEN "up"
                MOVE "bottom" TO SLAB-PROP-VALUE(1)
            WHEN "down"
                MOVE "top" TO SLAB-PROP-VALUE(1)
            WHEN OTHER
                IF LK-CURSOR-Y > 0.5
                    MOVE "top" TO SLAB-PROP-VALUE(1)
                ELSE
                    MOVE "bottom" TO SLAB-PROP-VALUE(1)
                END-IF
        END-EVALUATE

        *> Check whether the clicked position is a slab that can be doubled
        MOVE LK-POSITION TO BLOCK-POSITION
        CALL "World-GetBlock" USING LK-POSITION BLOCK-ID
        CALL "Blocks-Get-StateDescription" USING BLOCK-ID CURRENT-DESCRIPTION
        IF CURRENT-NAME = SLAB-NAME
            CALL "Blocks-Description-GetValue" USING CURRENT-DESCRIPTION C-TYPE CURRENT-TYPE
            EVALUATE FACING ALSO CURRENT-TYPE
                WHEN "up" ALSO "bottom"
                    MOVE "double" TO SLAB-PROP-VALUE(1)
                WHEN "down" ALSO "top"
                    MOVE "double" TO SLAB-PROP-VALUE(1)
            END-EVALUATE
        END-IF

        *> Unless it can be doubled in-place, the slab should be placed next to the clicked block
        IF SLAB-PROP-VALUE(1) NOT = "double"
            CALL "Facing-GetRelative" USING LK-FACE BLOCK-POSITION
            CALL "World-CheckBounds" USING BLOCK-POSITION BOUNDS-CHECK
            IF BOUNDS-CHECK NOT = 0
                GOBACK
            END-IF

            *> Check the new position for an existing single slab
            CALL "World-GetBlock" USING BLOCK-POSITION BLOCK-ID
            CALL "Blocks-Get-StateDescription" USING BLOCK-ID CURRENT-DESCRIPTION
            IF CURRENT-NAME = SLAB-NAME
                CALL "Blocks-Description-GetValue" USING CURRENT-DESCRIPTION C-TYPE CURRENT-TYPE
                EVALUATE SLAB-PROP-VALUE(1) ALSO CURRENT-TYPE
                    WHEN "top" ALSO "bottom"
                        MOVE "double" TO SLAB-PROP-VALUE(1)
                    WHEN "bottom" ALSO "top"
                        MOVE "double" TO SLAB-PROP-VALUE(1)
                    WHEN OTHER
                        GOBACK
                END-EVALUATE
            END-IF
        END-IF

        *> Allow replacing air or the same slab type
        IF CURRENT-NAME = C-MINECRAFT-AIR OR CURRENT-NAME = SLAB-NAME
            CALL "Blocks-Get-StateId" USING SLAB-DESCRIPTION BLOCK-ID
            CALL "World-SetBlock" USING BLOCK-POSITION BLOCK-ID
        END-IF

        GOBACK.

    END PROGRAM Callback-Use.

END PROGRAM RegisterItem-Slab.
