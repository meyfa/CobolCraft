*> --- Facing-GetRelative ---
*> Move a block position by one unit in the given facing direction (enum value).
IDENTIFICATION DIVISION.
PROGRAM-ID. Facing-GetRelative.

DATA DIVISION.
LINKAGE SECTION.
    01 LK-FACING        BINARY-LONG.
    01 LK-POSITION.
        02 LK-X         BINARY-LONG.
        02 LK-Y         BINARY-LONG.
        02 LK-Z         BINARY-LONG.

PROCEDURE DIVISION USING LK-FACING LK-POSITION.
    EVALUATE LK-FACING
        WHEN 0
            COMPUTE LK-Y = LK-Y - 1
        WHEN 1
            COMPUTE LK-Y = LK-Y + 1
        WHEN 2
            COMPUTE LK-Z = LK-Z - 1
        WHEN 3
            COMPUTE LK-Z = LK-Z + 1
        WHEN 4
            COMPUTE LK-X = LK-X - 1
        WHEN 5
            COMPUTE LK-X = LK-X + 1
    END-EVALUATE
    GOBACK.

END PROGRAM Facing-GetRelative.

*> --- Facing-ToString ---
*> Convert a facing direction (enum value) to a string.
*> Possible values: "down", "up", "north", "south", "west", "east".
IDENTIFICATION DIVISION.
PROGRAM-ID. Facing-ToString.

DATA DIVISION.
LINKAGE SECTION.
    01 LK-FACING        BINARY-LONG.
    01 LK-STRING        PIC X ANY LENGTH.

PROCEDURE DIVISION USING LK-FACING LK-STRING.
    EVALUATE LK-FACING
        WHEN 0
            MOVE "down" TO LK-STRING
        WHEN 1
            MOVE "up" TO LK-STRING
        WHEN 2
            MOVE "north" TO LK-STRING
        WHEN 3
            MOVE "south" TO LK-STRING
        WHEN 4
            MOVE "west" TO LK-STRING
        WHEN 5
            MOVE "east" TO LK-STRING
    END-EVALUATE
    GOBACK.

END PROGRAM Facing-ToString.

*> --- Facing-FromString ---
*> Convert a string to a facing direction (enum value).
*> Possible values: "down", "up", "north", "south", "west", "east".
IDENTIFICATION DIVISION.
PROGRAM-ID. Facing-FromString.

DATA DIVISION.
LINKAGE SECTION.
    01 LK-STRING        PIC X ANY LENGTH.
    01 LK-FACING        BINARY-LONG.

PROCEDURE DIVISION USING LK-STRING LK-FACING.
    EVALUATE LK-STRING
        WHEN "down"
            MOVE 0 TO LK-FACING
        WHEN "up"
            MOVE 1 TO LK-FACING
        WHEN "north"
            MOVE 2 TO LK-FACING
        WHEN "south"
            MOVE 3 TO LK-FACING
        WHEN "west"
            MOVE 4 TO LK-FACING
        WHEN "east"
            MOVE 5 TO LK-FACING
        WHEN OTHER
            MOVE -1 TO LK-FACING
    END-EVALUATE
    GOBACK.

END PROGRAM Facing-FromString.
