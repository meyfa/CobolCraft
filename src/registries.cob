*> --- Registries-Parse ---
*> Parse the registries.json file. Afterwards, the parsed contents are stored in the REGISTRIES shared data structure.
IDENTIFICATION DIVISION.
PROGRAM-ID. Registries-Parse.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 OFFSET           BINARY-LONG UNSIGNED.
    01 EXIT-LOOP        BINARY-CHAR UNSIGNED.
    *> shared data
    01 REGISTRIES EXTERNAL.
        02 REGISTRIES-COUNT BINARY-LONG UNSIGNED.
        02 REGISTRY OCCURS 100 TIMES.
            03 REGISTRY-NAME PIC X(100).
            03 REGISTRY-ID BINARY-LONG UNSIGNED.
            03 REGISTRY-ENTRIES-COUNT BINARY-LONG UNSIGNED.
            03 REGISTRY-ENTRY OCCURS 2000 TIMES.
                04 REGISTRY-ENTRY-NAME PIC X(100).
                04 REGISTRY-ENTRY-ID BINARY-LONG UNSIGNED.
LINKAGE SECTION.
    01 LK-JSON          PIC X ANY LENGTH.
    01 LK-JSON-LEN      BINARY-LONG UNSIGNED.
    01 LK-FAILURE       BINARY-CHAR UNSIGNED.

PROCEDURE DIVISION USING BY REFERENCE LK-JSON LK-JSON-LEN LK-FAILURE.
    MOVE 0 TO REGISTRIES-COUNT

    MOVE 1 TO OFFSET

    *> Expect start of the root object.
    CALL "JsonParse-ObjectStart" USING LK-JSON OFFSET LK-FAILURE
    IF LK-FAILURE NOT = 0
        GOBACK
    END-IF

    *> Loop over each key in the root object, each representing a registry.
    PERFORM UNTIL EXIT
        *> Read the registry.
        ADD 1 TO REGISTRIES-COUNT
        CALL "Registries-Parse-Registry" USING LK-JSON OFFSET LK-FAILURE REGISTRY(REGISTRIES-COUNT)
        IF LK-FAILURE NOT = 0
            GOBACK
        END-IF

        *> Check if there is a comma; if not, exit the loop.
        CALL "JsonParse-Comma" USING LK-JSON OFFSET EXIT-LOOP
        IF EXIT-LOOP = 1
            EXIT PERFORM
        END-IF
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
    LINKAGE SECTION.
        01 LK-JSON          PIC X ANY LENGTH.
        01 LK-OFFSET        BINARY-LONG UNSIGNED.
        01 LK-FAILURE       BINARY-CHAR UNSIGNED.
        01 LK-REGISTRY.
            02 LK-REGISTRY-NAME PIC X(100).
            02 REGISTRY-ID BINARY-LONG UNSIGNED.
            02 LK-REGISTRY-ENTRIES-COUNT BINARY-LONG UNSIGNED.
            02 LK-REGISTRY-ENTRY OCCURS 2000 TIMES.
                03 LK-REGISTRY-ENTRY-NAME PIC X(100).
                03 LK-REGISTRY-ENTRY-ID BINARY-LONG UNSIGNED.

    PROCEDURE DIVISION USING BY REFERENCE LK-JSON LK-OFFSET LK-FAILURE LK-REGISTRY.
        MOVE 0 TO LK-REGISTRY-ENTRIES-COUNT

        *> Read the registry name.
        CALL "JsonParse-ObjectKey" USING LK-JSON LK-OFFSET LK-FAILURE LK-REGISTRY-NAME
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
                    CALL "JsonParse-Integer" USING LK-JSON LK-OFFSET LK-FAILURE REGISTRY-ID
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
                        ADD 1 TO LK-REGISTRY-ENTRIES-COUNT
                        CALL "Registries-Parse-Entry" USING LK-JSON LK-OFFSET LK-FAILURE LK-REGISTRY-ENTRY(LK-REGISTRY-ENTRIES-COUNT)
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

        PROCEDURE DIVISION USING BY REFERENCE LK-JSON LK-OFFSET LK-FAILURE LK-REGISTRY-ENTRY.
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

*> --- Registries-Get-EntryId ---
IDENTIFICATION DIVISION.
PROGRAM-ID. Registries-Get-EntryId.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 REGISTRY-INDEX   BINARY-LONG UNSIGNED.
    01 ENTRY-INDEX      BINARY-LONG UNSIGNED.
    *> shared data
    01 REGISTRIES EXTERNAL.
        02 REGISTRIES-COUNT BINARY-LONG UNSIGNED.
        02 REGISTRY OCCURS 100 TIMES.
            03 REGISTRY-NAME PIC X(100).
            03 REGISTRY-ID BINARY-LONG UNSIGNED.
            03 REGISTRY-ENTRIES-COUNT BINARY-LONG UNSIGNED.
            03 REGISTRY-ENTRY OCCURS 2000 TIMES.
                04 REGISTRY-ENTRY-NAME PIC X(100).
                04 REGISTRY-ENTRY-ID BINARY-LONG UNSIGNED.
LINKAGE SECTION.
    01 LK-REGISTRY-NAME PIC X ANY LENGTH.
    01 LK-ENTRY-NAME    PIC X ANY LENGTH.
    01 LK-ENTRY-ID      BINARY-LONG.

PROCEDURE DIVISION USING BY REFERENCE LK-REGISTRY-NAME LK-ENTRY-NAME LK-ENTRY-ID.
    MOVE -1 TO LK-ENTRY-ID
    PERFORM VARYING REGISTRY-INDEX FROM 1 BY 1 UNTIL REGISTRY-INDEX > REGISTRIES-COUNT
        IF LK-REGISTRY-NAME = REGISTRY-NAME(REGISTRY-INDEX)
            PERFORM VARYING ENTRY-INDEX FROM 1 BY 1 UNTIL ENTRY-INDEX > REGISTRY-ENTRIES-COUNT(REGISTRY-INDEX)
                IF LK-ENTRY-NAME = REGISTRY-ENTRY-NAME(REGISTRY-INDEX, ENTRY-INDEX)
                    MOVE REGISTRY-ENTRY-ID(REGISTRY-INDEX, ENTRY-INDEX) TO LK-ENTRY-ID
                    EXIT PERFORM
                END-IF
            END-PERFORM
        END-IF
    END-PERFORM
    GOBACK.

END PROGRAM Registries-Get-EntryId.
