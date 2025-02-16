*> --- Items-Parse ---
*> Parse the items.json file. Afterwards, the parsed contents are stored in the ITEMS shared data structure.
IDENTIFICATION DIVISION.
PROGRAM-ID. Items-Parse.

DATA DIVISION.
WORKING-STORAGE SECTION.
    COPY DD-ITEMS.
LOCAL-STORAGE SECTION.
    01 OFFSET                   BINARY-LONG UNSIGNED        VALUE 1.
    01 EXIT-LOOP                BINARY-CHAR UNSIGNED        VALUE 0.
LINKAGE SECTION.
    01 LK-JSON                  PIC X ANY LENGTH.
    01 LK-JSON-LEN              BINARY-LONG UNSIGNED.
    01 LK-FAILURE               BINARY-CHAR UNSIGNED.

PROCEDURE DIVISION USING LK-JSON LK-JSON-LEN LK-FAILURE.
    INITIALIZE ITEMS

    CALL "JsonParse-ObjectStart" USING LK-JSON OFFSET LK-FAILURE

    PERFORM UNTIL LK-FAILURE NOT = 0 OR EXIT-LOOP = 1
        CALL "Items-Parse-Item" USING LK-JSON OFFSET LK-FAILURE
        IF LK-FAILURE = 0
            ADD 1 TO ITEMS-COUNT
        END-IF
        CALL "JsonParse-Comma" USING LK-JSON OFFSET EXIT-LOOP
    END-PERFORM

    CALL "JsonParse-ObjectEnd" USING LK-JSON OFFSET LK-FAILURE

    GOBACK.

    *> --- Items-Parse-Item ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Items-Parse-Item.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        COPY DD-ITEMS.
        01 EXIT-LOOP            BINARY-CHAR UNSIGNED.
        01 OBJECT-KEY           PIC X(50).
        01 ITEM-ID              BINARY-LONG.
    LINKAGE SECTION.
        01 LK-JSON              PIC X ANY LENGTH.
        01 LK-OFFSET            BINARY-LONG UNSIGNED.
        01 LK-FAILURE           BINARY-CHAR UNSIGNED.

    PROCEDURE DIVISION USING LK-JSON LK-OFFSET LK-FAILURE.
        *> Read the item name and lookup its protocol ID, which is used to index the items table.
        CALL "JsonParse-ObjectKey" USING LK-JSON LK-OFFSET LK-FAILURE OBJECT-KEY
        CALL "Registries-Get-EntryId" USING "minecraft:item" OBJECT-KEY ITEM-ID
        IF ITEM-ID < 0
            MOVE 1 TO LK-FAILURE
            GOBACK
        END-IF

        CALL "JsonParse-ObjectStart" USING LK-JSON LK-OFFSET LK-FAILURE

        PERFORM UNTIL LK-FAILURE NOT = 0
            CALL "JsonParse-ObjectKey" USING LK-JSON LK-OFFSET LK-FAILURE OBJECT-KEY
            IF LK-FAILURE NOT = 0
                GOBACK
            END-IF

            EVALUATE OBJECT-KEY
                WHEN "components"
                    CALL "JsonParse-ObjectStart" USING LK-JSON LK-OFFSET LK-FAILURE

                    PERFORM UNTIL LK-FAILURE NOT = 0
                        CALL "JsonParse-ObjectKey" USING LK-JSON LK-OFFSET LK-FAILURE OBJECT-KEY
                        IF LK-FAILURE NOT = 0
                            GOBACK
                        END-IF

                        EVALUATE OBJECT-KEY
                            WHEN "minecraft:max_stack_size"
                                CALL "JsonParse-Integer" USING LK-JSON LK-OFFSET LK-FAILURE ITEM-ENTRY-MAX-STACK-SIZE(ITEM-ID + 1)
                            WHEN OTHER
                                CALL "JsonParse-SkipValue" USING LK-JSON LK-OFFSET LK-FAILURE
                        END-EVALUATE

                        CALL "JsonParse-Comma" USING LK-JSON LK-OFFSET EXIT-LOOP
                        IF EXIT-LOOP = 1
                            EXIT PERFORM
                        END-IF
                    END-PERFORM

                    CALL "JsonParse-ObjectEnd" USING LK-JSON LK-OFFSET LK-FAILURE

                WHEN OTHER
                    CALL "JsonParse-SkipValue" USING LK-JSON LK-OFFSET LK-FAILURE
            END-EVALUATE

            CALL "JsonParse-Comma" USING LK-JSON LK-OFFSET EXIT-LOOP
            IF EXIT-LOOP = 1
                EXIT PERFORM
            END-IF
        END-PERFORM

        CALL "JsonParse-ObjectEnd" USING LK-JSON LK-OFFSET LK-FAILURE

        GOBACK.

    END PROGRAM Items-Parse-Item.

END PROGRAM Items-Parse.

*> --- Items-GetCount ---
IDENTIFICATION DIVISION.
PROGRAM-ID. Items-GetCount.

DATA DIVISION.
WORKING-STORAGE SECTION.
    COPY DD-ITEMS.
LINKAGE SECTION.
    01 LK-ITEM-COUNT            BINARY-LONG.

PROCEDURE DIVISION USING LK-ITEM-COUNT.
    MOVE ITEMS-COUNT TO LK-ITEM-COUNT
    GOBACK.

END PROGRAM Items-GetCount.

*> --- Items-Get-MaxStackSize ---
IDENTIFICATION DIVISION.
PROGRAM-ID. Items-Get-MaxStackSize.

DATA DIVISION.
WORKING-STORAGE SECTION.
    COPY DD-ITEMS.
LINKAGE SECTION.
    01 LK-ITEM-ID               BINARY-LONG UNSIGNED.
    01 LK-MAX-STACK-SIZE        BINARY-LONG.

PROCEDURE DIVISION USING LK-ITEM-ID LK-MAX-STACK-SIZE.
    MOVE ITEM-ENTRY-MAX-STACK-SIZE(LK-ITEM-ID + 1) TO LK-MAX-STACK-SIZE
    GOBACK.

END PROGRAM Items-Get-MaxStackSize.
