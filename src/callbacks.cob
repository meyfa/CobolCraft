*> --- SetCallback-ItemUse ---
IDENTIFICATION DIVISION.
PROGRAM-ID. SetCallback-ItemUse.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 C-MINECRAFT-ITEM             PIC X(16) VALUE "minecraft:item".
    01 PROTOCOL-ID                  BINARY-LONG.
    COPY DD-CALLBACKS.
LINKAGE SECTION.
    01 LK-ITEM-NAME                 PIC X ANY LENGTH.
    01 LK-CB-PTR                    PROGRAM-POINTER.

PROCEDURE DIVISION USING LK-ITEM-NAME LK-CB-PTR.
    CALL "Registries-Get-EntryId" USING C-MINECRAFT-ITEM LK-ITEM-NAME PROTOCOL-ID
    MOVE LK-CB-PTR TO CB-PTR-USE(PROTOCOL-ID + 1)
    GOBACK.

END PROGRAM SetCallback-ItemUse.

*> --- GetCallback-ItemUse ---
IDENTIFICATION DIVISION.
PROGRAM-ID. GetCallback-ItemUse.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 C-MINECRAFT-ITEM             PIC X(16) VALUE "minecraft:item".
    01 PROTOCOL-ID                  BINARY-LONG.
    COPY DD-CALLBACKS.
LINKAGE SECTION.
    01 LK-ITEM-NAME                 PIC X ANY LENGTH.
    01 LK-CB-PTR                    PROGRAM-POINTER.

PROCEDURE DIVISION USING LK-ITEM-NAME LK-CB-PTR.
    CALL "Registries-Get-EntryId" USING C-MINECRAFT-ITEM LK-ITEM-NAME PROTOCOL-ID
    MOVE CB-PTR-USE(PROTOCOL-ID + 1) TO LK-CB-PTR
    GOBACK.

END PROGRAM GetCallback-ItemUse.

*> --- SetCallback-BlockDestroy ---
IDENTIFICATION DIVISION.
PROGRAM-ID. SetCallback-BlockDestroy.

DATA DIVISION.
WORKING-STORAGE SECTION.
    COPY DD-CALLBACKS.
LINKAGE SECTION.
    01 LK-BLOCK-STATE-ID            BINARY-LONG.
    01 LK-CB-PTR                    PROGRAM-POINTER.

PROCEDURE DIVISION USING LK-BLOCK-STATE-ID LK-CB-PTR.
    MOVE LK-CB-PTR TO CB-PTR-DESTROY(LK-BLOCK-STATE-ID + 1)
    GOBACK.

END PROGRAM SetCallback-BlockDestroy.

*> --- GetCallback-BlockDestroy ---
IDENTIFICATION DIVISION.
PROGRAM-ID. GetCallback-BlockDestroy.

DATA DIVISION.
WORKING-STORAGE SECTION.
    COPY DD-CALLBACKS.
LINKAGE SECTION.
    01 LK-BLOCK-STATE-ID            BINARY-LONG.
    01 LK-CB-PTR                    PROGRAM-POINTER.

PROCEDURE DIVISION USING LK-BLOCK-STATE-ID LK-CB-PTR.
    IF LK-BLOCK-STATE-ID < 0
        SET LK-CB-PTR TO NULL
        GOBACK
    END-IF
    MOVE CB-PTR-DESTROY(LK-BLOCK-STATE-ID + 1) TO LK-CB-PTR
    GOBACK.

END PROGRAM GetCallback-BlockDestroy.

*> --- SetCallback-BlockInteract ---
IDENTIFICATION DIVISION.
PROGRAM-ID. SetCallback-BlockInteract.

DATA DIVISION.
WORKING-STORAGE SECTION.
    COPY DD-CALLBACKS.
LINKAGE SECTION.
    01 LK-BLOCK-STATE-ID            BINARY-LONG.
    01 LK-CB-PTR                    PROGRAM-POINTER.

PROCEDURE DIVISION USING LK-BLOCK-STATE-ID LK-CB-PTR.
    MOVE LK-CB-PTR TO CB-PTR-INTERACT(LK-BLOCK-STATE-ID + 1)
    GOBACK.

END PROGRAM SetCallback-BlockInteract.

*> --- GetCallback-BlockInteract ---
IDENTIFICATION DIVISION.
PROGRAM-ID. GetCallback-BlockInteract.

DATA DIVISION.
WORKING-STORAGE SECTION.
    COPY DD-CALLBACKS.
LINKAGE SECTION.
    01 LK-BLOCK-STATE-ID            BINARY-LONG.
    01 LK-CB-PTR                    PROGRAM-POINTER.

PROCEDURE DIVISION USING LK-BLOCK-STATE-ID LK-CB-PTR.
    IF LK-BLOCK-STATE-ID < 0
        SET LK-CB-PTR TO NULL
        GOBACK
    END-IF
    MOVE CB-PTR-INTERACT(LK-BLOCK-STATE-ID + 1) TO LK-CB-PTR
    GOBACK.

END PROGRAM GetCallback-BlockInteract.
