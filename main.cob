IDENTIFICATION DIVISION.
PROGRAM-ID. Main.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 SERVER-CONFIG.
        02 PORT                 PIC X(5)    VALUE "25565".
        *> To configure the whitelist, use console/in-game commands or edit whitelist.json.
        02 WHITELIST-ENABLE     BINARY-CHAR VALUE 0.
        02 MOTD                 PIC X(64)   VALUE "CobolCraft".

PROCEDURE DIVISION.
    CALL "Server" USING SERVER-CONFIG.

END PROGRAM Main.
