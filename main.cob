IDENTIFICATION DIVISION.
PROGRAM-ID. Main.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 SERVER-CONFIG.
        02 PORT                 PIC X(5)    VALUE "25565".
        02 WHITELIST-ENABLE     PIC 9(1)    VALUE 0.
        02 WHITELIST-PLAYER     PIC X(16)   VALUE "Notch".
        02 MOTD                 PIC X(64)   VALUE "CobolCraft".

PROCEDURE DIVISION.
    CALL "Server" USING SERVER-CONFIG.

END PROGRAM Main.
