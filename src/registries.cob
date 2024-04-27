*> --- Registries-Parse ---
*> Parse the registries.json file. Afterwards, the parsed contents are stored in the REGISTRIES shared data structure.
IDENTIFICATION DIVISION.
PROGRAM-ID. Registries-Parse.

DATA DIVISION.
WORKING-STORAGE SECTION.
    *> shared data
    COPY DD-REGISTRIES.
LOCAL-STORAGE SECTION.
    01 OFFSET           BINARY-LONG UNSIGNED    VALUE 1.
    01 EXIT-LOOP        BINARY-CHAR UNSIGNED    VALUE 0.
LINKAGE SECTION.
    01 LK-JSON          PIC X ANY LENGTH.
    01 LK-JSON-LEN      BINARY-LONG UNSIGNED.
    01 LK-FAILURE       BINARY-CHAR UNSIGNED.

PROCEDURE DIVISION USING LK-JSON LK-JSON-LEN LK-FAILURE.
    MOVE 0 TO REGISTRIES-COUNT

    *> Expect start of the root object.
    CALL "JsonParse-ObjectStart" USING LK-JSON OFFSET LK-FAILURE

    *> Loop over each key in the root object, each representing a registry.
    PERFORM UNTIL LK-FAILURE NOT = 0 OR EXIT-LOOP = 1
        *> Read the registry.
        ADD 1 TO REGISTRIES-COUNT
        CALL "Registries-Parse-Registry" USING LK-JSON OFFSET LK-FAILURE REGISTRIES-COUNT

        *> Continue reading if there is a comma.
        CALL "JsonParse-Comma" USING LK-JSON OFFSET EXIT-LOOP
    END-PERFORM

    *> Expect end of the root object.
    CALL "JsonParse-ObjectEnd" USING LK-JSON OFFSET LK-FAILURE

    GOBACK.

    *> --- Registries-Parse-Registry ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Registries-Parse-Registry.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        01 EXIT-LOOP        BINARY-CHAR UNSIGNED.
        01 OBJECT-KEY       PIC X(100).
        *> shared data
        COPY DD-REGISTRIES.
    LINKAGE SECTION.
        01 LK-JSON          PIC X ANY LENGTH.
        01 LK-OFFSET        BINARY-LONG UNSIGNED.
        01 LK-FAILURE       BINARY-CHAR UNSIGNED.
        01 LK-REGISTRY      BINARY-LONG UNSIGNED.

    PROCEDURE DIVISION USING LK-JSON LK-OFFSET LK-FAILURE LK-REGISTRY.
        MOVE 0 TO REGISTRY-ENTRIES-COUNT(LK-REGISTRY)

        *> Read the registry name.
        CALL "JsonParse-ObjectKey" USING LK-JSON LK-OFFSET LK-FAILURE REGISTRY-NAME(LK-REGISTRY)
        IF LK-FAILURE NOT = 0
            GOBACK
        END-IF

        *> Expect start of the registry object.
        CALL "JsonParse-ObjectStart" USING LK-JSON LK-OFFSET LK-FAILURE
        IF LK-FAILURE NOT = 0
            GOBACK
        END-IF

        *> Loop over each key in the registry object. We expect the keys "entries" and "protocol_id".
        PERFORM UNTIL EXIT
            *> Read the key.
            CALL "JsonParse-ObjectKey" USING LK-JSON LK-OFFSET LK-FAILURE OBJECT-KEY
            IF LK-FAILURE NOT = 0
                GOBACK
            END-IF

            EVALUATE OBJECT-KEY
                WHEN "protocol_id"
                    CALL "JsonParse-Integer" USING LK-JSON LK-OFFSET LK-FAILURE REGISTRY-ID(LK-REGISTRY)
                    IF LK-FAILURE NOT = 0
                        GOBACK
                    END-IF

                WHEN "entries"
                    *> Expect the start of the entries object.
                    CALL "JsonParse-ObjectStart" USING LK-JSON LK-OFFSET LK-FAILURE
                    IF LK-FAILURE NOT = 0
                        GOBACK
                    END-IF

                    *> Loop over each key in the entries object, each representing an entry.
                    PERFORM UNTIL EXIT
                        *> Read the entry.
                        ADD 1 TO REGISTRY-ENTRIES-COUNT(LK-REGISTRY)
                        CALL "Registries-Parse-Entry" USING LK-JSON LK-OFFSET LK-FAILURE REGISTRY-ENTRY(LK-REGISTRY, REGISTRY-ENTRIES-COUNT(LK-REGISTRY))
                        IF LK-FAILURE NOT = 0
                            GOBACK
                        END-IF

                        *> Check if there is a comma; if not, exit the loop.
                        CALL "JsonParse-Comma" USING LK-JSON LK-OFFSET EXIT-LOOP
                        IF EXIT-LOOP = 1
                            EXIT PERFORM
                        END-IF
                    END-PERFORM

                    *> Expect the end of the entries object.
                    CALL "JsonParse-ObjectEnd" USING LK-JSON LK-OFFSET LK-FAILURE
                    IF LK-FAILURE NOT = 0
                        GOBACK
                    END-IF

                WHEN OTHER
                    CALL "JsonParse-SkipValue" USING LK-JSON LK-OFFSET LK-FAILURE
                    IF LK-FAILURE NOT = 0
                        GOBACK
                    END-IF
            END-EVALUATE

            *> Check if there is a comma; if not, exit the loop.
            CALL "JsonParse-Comma" USING LK-JSON LK-OFFSET EXIT-LOOP
            IF EXIT-LOOP = 1
                EXIT PERFORM
            END-IF
        END-PERFORM

        *> Expect the end of the registry object.
        CALL "JsonParse-ObjectEnd" USING LK-JSON LK-OFFSET LK-FAILURE

        GOBACK.

        *> --- Registries-Parse-Entry ---
        IDENTIFICATION DIVISION.
        PROGRAM-ID. Registries-Parse-Entry.

        DATA DIVISION.
        WORKING-STORAGE SECTION.
            01 EXIT-LOOP        BINARY-CHAR UNSIGNED.
            01 OBJECT-KEY       PIC X(100).
        LINKAGE SECTION.
            01 LK-JSON          PIC X ANY LENGTH.
            01 LK-OFFSET        BINARY-LONG UNSIGNED.
            01 LK-FAILURE       BINARY-CHAR UNSIGNED.
            01 LK-REGISTRY-ENTRY.
                02 LK-REGISTRY-ENTRY-NAME PIC X(100).
                02 LK-REGISTRY-ENTRY-ID BINARY-LONG UNSIGNED.

        PROCEDURE DIVISION USING LK-JSON LK-OFFSET LK-FAILURE LK-REGISTRY-ENTRY.
            *> Read the entry name.
            CALL "JsonParse-ObjectKey" USING LK-JSON LK-OFFSET LK-FAILURE LK-REGISTRY-ENTRY-NAME
            IF LK-FAILURE NOT = 0
                GOBACK
            END-IF

            *> Expect the start of the entry object.
            CALL "JsonParse-ObjectStart" USING LK-JSON LK-OFFSET LK-FAILURE
            IF LK-FAILURE NOT = 0
                GOBACK
            END-IF

            *> Loop over each key looking for "protocol_id".
            PERFORM UNTIL EXIT
                *> Read the key.
                CALL "JsonParse-ObjectKey" USING LK-JSON LK-OFFSET LK-FAILURE OBJECT-KEY
                IF LK-FAILURE NOT = 0
                    GOBACK
                END-IF

                EVALUATE OBJECT-KEY
                    WHEN "protocol_id"
                        CALL "JsonParse-Integer" USING LK-JSON LK-OFFSET LK-FAILURE LK-REGISTRY-ENTRY-ID

                    WHEN OTHER
                        CALL "JsonParse-SkipValue" USING LK-JSON LK-OFFSET LK-FAILURE
                END-EVALUATE

                IF LK-FAILURE NOT = 0
                    GOBACK
                END-IF

                *> Check if there is a comma; if not, exit the loop.
                CALL "JsonParse-Comma" USING LK-JSON LK-OFFSET EXIT-LOOP
                IF EXIT-LOOP = 1
                    EXIT PERFORM
                END-IF
            END-PERFORM

            *> Expect the end of the entry object.
            CALL "JsonParse-ObjectEnd" USING LK-JSON LK-OFFSET LK-FAILURE

            GOBACK.

        END PROGRAM Registries-Parse-Entry.

    END PROGRAM Registries-Parse-Registry.

END PROGRAM Registries-Parse.

*> --- Registries-GetRegistryIndex ---
*> Get the index of a registry by its name for faster access.
IDENTIFICATION DIVISION.
PROGRAM-ID. Registries-GetRegistryIndex.

DATA DIVISION.
WORKING-STORAGE SECTION.
    *> shared data
    COPY DD-REGISTRIES.
LINKAGE SECTION.
    01 LK-REGISTRY-NAME     PIC X ANY LENGTH.
    01 LK-REGISTRY-INDEX    BINARY-LONG.

PROCEDURE DIVISION USING LK-REGISTRY-NAME LK-REGISTRY-INDEX.
    PERFORM VARYING LK-REGISTRY-INDEX FROM 1 BY 1 UNTIL LK-REGISTRY-INDEX > REGISTRIES-COUNT
        IF LK-REGISTRY-NAME = REGISTRY-NAME(LK-REGISTRY-INDEX)
            GOBACK
        END-IF
    END-PERFORM
    MOVE -1 TO LK-REGISTRY-INDEX
    GOBACK.

END PROGRAM Registries-GetRegistryIndex.

*> --- Registries-GetRegistryLength ---
*> Get the number of entries in a registry by its index.
IDENTIFICATION DIVISION.
PROGRAM-ID. Registries-GetRegistryLength.

DATA DIVISION.
WORKING-STORAGE SECTION.
    *> shared data
    COPY DD-REGISTRIES.
LINKAGE SECTION.
    01 LK-REGISTRY-INDEX    BINARY-LONG UNSIGNED.
    01 LK-REGISTRY-LENGTH   BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-REGISTRY-INDEX LK-REGISTRY-LENGTH.
    MOVE REGISTRY-ENTRIES-COUNT(LK-REGISTRY-INDEX) TO LK-REGISTRY-LENGTH
    GOBACK.

END PROGRAM Registries-GetRegistryLength.

*> --- Registries-Iterate-EntryName ---
*> Get the name of an entry by its index.
IDENTIFICATION DIVISION.
PROGRAM-ID. Registries-Iterate-EntryName.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 REGISTRY-INDEX       BINARY-LONG UNSIGNED.
    01 ENTRY-INDEX          BINARY-LONG UNSIGNED.
    *> shared data
    COPY DD-REGISTRIES.
LINKAGE SECTION.
    01 LK-REGISTRY-INDEX    BINARY-LONG UNSIGNED.
    01 LK-ENTRY-INDEX       BINARY-LONG UNSIGNED.
    01 LK-ENTRY-NAME        PIC X ANY LENGTH.

PROCEDURE DIVISION USING LK-REGISTRY-INDEX LK-ENTRY-INDEX LK-ENTRY-NAME.
    MOVE REGISTRY-ENTRY-NAME(LK-REGISTRY-INDEX, LK-ENTRY-INDEX) TO LK-ENTRY-NAME
    GOBACK.

END PROGRAM Registries-Iterate-EntryName.

*> --- Registries-Get-EntryId ---
IDENTIFICATION DIVISION.
PROGRAM-ID. Registries-Get-EntryId.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 REGISTRY-INDEX   BINARY-LONG.
    01 ENTRY-INDEX      BINARY-LONG UNSIGNED.
    *> shared data
    COPY DD-REGISTRIES.
LINKAGE SECTION.
    01 LK-REGISTRY-NAME PIC X ANY LENGTH.
    01 LK-ENTRY-NAME    PIC X ANY LENGTH.
    01 LK-ENTRY-ID      BINARY-LONG.

PROCEDURE DIVISION USING LK-REGISTRY-NAME LK-ENTRY-NAME LK-ENTRY-ID.
    MOVE -1 TO LK-ENTRY-ID
    CALL "Registries-GetRegistryIndex" USING LK-REGISTRY-NAME REGISTRY-INDEX
    IF REGISTRY-INDEX = -1
        GOBACK
    END-IF
    PERFORM VARYING ENTRY-INDEX FROM 1 BY 1 UNTIL ENTRY-INDEX > REGISTRY-ENTRIES-COUNT(REGISTRY-INDEX)
        IF LK-ENTRY-NAME = REGISTRY-ENTRY-NAME(REGISTRY-INDEX, ENTRY-INDEX)
            MOVE REGISTRY-ENTRY-ID(REGISTRY-INDEX, ENTRY-INDEX) TO LK-ENTRY-ID
            GOBACK
        END-IF
    END-PERFORM
    GOBACK.

END PROGRAM Registries-Get-EntryId.

*> --- Registries-Get-EntryName ---
IDENTIFICATION DIVISION.
PROGRAM-ID. Registries-Get-EntryName.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 REGISTRY-INDEX   BINARY-LONG.
    01 ENTRY-INDEX      BINARY-LONG UNSIGNED.
    *> shared data
    COPY DD-REGISTRIES.
LINKAGE SECTION.
    01 LK-REGISTRY-NAME PIC X ANY LENGTH.
    01 LK-ENTRY-ID      BINARY-LONG.
    01 LK-ENTRY-NAME    PIC X ANY LENGTH.

PROCEDURE DIVISION USING LK-REGISTRY-NAME LK-ENTRY-ID LK-ENTRY-NAME.
    MOVE SPACES TO LK-ENTRY-NAME
    CALL "Registries-GetRegistryIndex" USING LK-REGISTRY-NAME REGISTRY-INDEX
    IF REGISTRY-INDEX = -1
        GOBACK
    END-IF
    PERFORM VARYING ENTRY-INDEX FROM 1 BY 1 UNTIL ENTRY-INDEX > REGISTRY-ENTRIES-COUNT(REGISTRY-INDEX)
        IF LK-ENTRY-ID = REGISTRY-ENTRY-ID(REGISTRY-INDEX, ENTRY-INDEX)
            MOVE REGISTRY-ENTRY-NAME(REGISTRY-INDEX, ENTRY-INDEX) TO LK-ENTRY-NAME
            GOBACK
        END-IF
    END-PERFORM
    GOBACK.

END PROGRAM Registries-Get-EntryName.
