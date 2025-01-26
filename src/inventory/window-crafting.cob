*> --- RegisterWindow-Crafting ---
IDENTIFICATION DIVISION.
PROGRAM-ID. RegisterWindow-Crafting.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 WINDOW-TYPE                  BINARY-LONG.
    01 CLOSE-PTR                    PROGRAM-POINTER.

PROCEDURE DIVISION.
    CALL "Registries-Get-EntryId" USING "minecraft:menu" "minecraft:crafting" WINDOW-TYPE
    IF WINDOW-TYPE < 0
        DISPLAY "RegisterBlock-CraftingTable: Failed to get window type ID"
        GOBACK
    END-IF

    SET CLOSE-PTR TO ENTRY "Callback-Close"
    CALL "SetCallback-WindowClose" USING WINDOW-TYPE CLOSE-PTR

    GOBACK.

    *> --- Callback-Close ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Callback-Close.

    DATA DIVISION.
    LINKAGE SECTION.
        COPY DD-CALLBACK-WINDOW-CLOSE.

    PROCEDURE DIVISION USING LK-PLAYER.
        *> TODO handle carried item (mouse item) - transfer to inventory when closing
        GOBACK.

    END PROGRAM Callback-Close.

END PROGRAM RegisterWindow-Crafting.
