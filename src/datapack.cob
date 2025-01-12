*> --- Datapack-Load ---
*> Load registry contents from a datapack path.
IDENTIFICATION DIVISION.
PROGRAM-ID. Datapack-Load.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 REGISTRY-COUNT               BINARY-LONG UNSIGNED.
    01 REGISTRY-INDEX               BINARY-LONG UNSIGNED.
    01 REGISTRY-NAME                PIC X(255).
LINKAGE SECTION.
    01 LK-ROOT-PATH                 PIC X ANY LENGTH.
    01 LK-PACK-NAME                 PIC X ANY LENGTH.
    01 LK-FAILURE                   BINARY-CHAR UNSIGNED.

PROCEDURE DIVISION USING LK-ROOT-PATH LK-PACK-NAME LK-FAILURE.
    MOVE 0 TO LK-FAILURE

    *> For each known registry, load the contents from the datapack (if it exists).
    CALL "Registries-GetCount" USING REGISTRY-COUNT
    PERFORM VARYING REGISTRY-INDEX FROM 1 BY 1 UNTIL REGISTRY-INDEX > REGISTRY-COUNT
        CALL "Registries-Iterate-Name" USING REGISTRY-INDEX REGISTRY-NAME
        *> We want to remove the "minecraft:" prefix from registry names to match the directory structure
        IF REGISTRY-NAME(1:10) = "minecraft:"
            CALL "Datapack-LoadContent" USING LK-ROOT-PATH LK-PACK-NAME REGISTRY-NAME(11:) REGISTRY-INDEX LK-FAILURE
            IF LK-FAILURE NOT = 0
                EXIT PERFORM
            END-IF
        END-IF
    END-PERFORM

    GOBACK.

END PROGRAM Datapack-Load.

*> --- Datapack-LoadContent ---
*> Load content from a datapack path, adding entries to the registry.
IDENTIFICATION DIVISION.
PROGRAM-ID. Datapack-LoadContent.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 DIR-PATH                     PIC X(255).
    01 DIR-PATH-LENGTH              BINARY-LONG UNSIGNED.
    01 DIR-HANDLE                   PIC X(8).
    01 EOF                          BINARY-CHAR UNSIGNED.
    01 DIR-ENTRY                    PIC X(255).
    01 ENTRY-ID                     BINARY-LONG UNSIGNED.
    01 ENTRY-NAME                   PIC X(255).
LINKAGE SECTION.
    01 LK-ROOT-PATH                 PIC X ANY LENGTH.
    01 LK-PACK-NAME                 PIC X ANY LENGTH.
    01 LK-REGISTRY-NAME             PIC X ANY LENGTH.
    01 LK-REGISTRY-INDEX            BINARY-LONG UNSIGNED.
    01 LK-FAILURE                   BINARY-CHAR UNSIGNED.

PROCEDURE DIVISION USING LK-ROOT-PATH LK-PACK-NAME LK-REGISTRY-NAME LK-REGISTRY-INDEX LK-FAILURE.
    MOVE 0 TO LK-FAILURE

    INITIALIZE DIR-PATH
    STRING FUNCTION TRIM(LK-ROOT-PATH) FUNCTION TRIM(LK-PACK-NAME) "/" FUNCTION TRIM(LK-REGISTRY-NAME) INTO DIR-PATH
    MOVE FUNCTION STORED-CHAR-LENGTH(DIR-PATH) TO DIR-PATH-LENGTH

    CALL "OpenDirectory" USING DIR-PATH DIR-PATH-LENGTH DIR-HANDLE GIVING LK-FAILURE
    IF LK-FAILURE NOT = 0
        *> The directory is likely missing, which isn't an error.
        MOVE 0 TO LK-FAILURE
        GOBACK
    END-IF

    *> The entry protocol IDs will be numbered starting from the registry length.
    CALL "Registries-GetRegistryLength" USING LK-REGISTRY-INDEX ENTRY-ID

    PERFORM UNTIL EXIT
        CALL "ReadDirectory" USING DIR-HANDLE DIR-ENTRY GIVING EOF
        IF EOF NOT = 0
            EXIT PERFORM
        END-IF

        *> Prepend the namespace and remove the file extension
        INITIALIZE ENTRY-NAME
        STRING FUNCTION TRIM(LK-PACK-NAME) ":" FUNCTION TRIM(DIR-ENTRY) INTO ENTRY-NAME
        CALL "TrimFileExt" USING ENTRY-NAME

        *> Register the entry
        CALL "Registries-CreateEntry" USING LK-REGISTRY-INDEX ENTRY-NAME ENTRY-ID
        ADD 1 TO ENTRY-ID
    END-PERFORM

    CALL "CloseDirectory" USING DIR-HANDLE

    GOBACK.

END PROGRAM Datapack-LoadContent.
