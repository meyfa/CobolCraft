REPLACE ==DEFINE-BLOCK-ENTITY-STRUCT== BY ==
    01 STRUCT                       BASED.
        02 STRUCT-WAXED             BINARY-CHAR UNSIGNED.
        02 STRUCT-FRONT-GLOWING     BINARY-CHAR UNSIGNED.
        02 STRUCT-FRONT-COLOR       PIC X(16).
        02 STRUCT-FRONT-LINES.
            03 STRUCT-FRONT-LINE OCCURS 4 TIMES.
                04 STRUCT-FRONT-LINE-LEN BINARY-LONG UNSIGNED.
                04 STRUCT-FRONT-LINE-STR PIC X(256).
        02 STRUCT-BACK-GLOWING      BINARY-CHAR UNSIGNED.
        02 STRUCT-BACK-COLOR        PIC X(16).
        02 STRUCT-BACK-LINES.
            03 STRUCT-BACK-LINE OCCURS 4 TIMES.
                04 STRUCT-BACK-LINE-LEN BINARY-LONG UNSIGNED.
                04 STRUCT-BACK-LINE-STR PIC X(256).
==.

*> --- RegisterBlockEntity-Sign ---
IDENTIFICATION DIVISION.
PROGRAM-ID. RegisterBlockEntity-Sign.

DATA DIVISION.
WORKING-STORAGE SECTION.
    COPY DD-CALLBACKS.
    01 BLOCK-ENTITY-TYPE        BINARY-LONG UNSIGNED.

PROCEDURE DIVISION.
    CALL "Registries-Get-EntryId" USING "minecraft:block_entity_type" "minecraft:sign" BLOCK-ENTITY-TYPE
    SET CB-PTR-BLOCK-ENTITY-ALLOCATE(BLOCK-ENTITY-TYPE + 1) TO ENTRY "Callback-Allocate"
    SET CB-PTR-BLOCK-ENTITY-SERIALIZE(BLOCK-ENTITY-TYPE + 1) TO ENTRY "Callback-Serialize"
    SET CB-PTR-BLOCK-ENTITY-DESERIALIZE(BLOCK-ENTITY-TYPE + 1) TO ENTRY "Callback-Deserialize"
    GOBACK.

    *> --- Callback-Allocate ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Callback-Allocate.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        DEFINE-BLOCK-ENTITY-STRUCT
    LINKAGE SECTION.
        COPY DD-CALLBACK-BLOCK-ENTITY-ALLOCATE.

    PROCEDURE DIVISION USING LK-PTR.
        ALLOCATE STRUCT INITIALIZED RETURNING LK-PTR
        MOVE "black" TO STRUCT-FRONT-COLOR STRUCT-BACK-COLOR
        *> All lines are empty JSON strings
        INITIALIZE STRUCT-FRONT-LINES REPLACING ALPHANUMERIC BY '""' NUMERIC BY 2
        INITIALIZE STRUCT-BACK-LINES REPLACING ALPHANUMERIC BY '""' NUMERIC BY 2
        GOBACK.

    END PROGRAM Callback-Allocate.

    *> --- Callback-Serialize ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Callback-Serialize.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        01 STR                  PIC X(256).
        01 LEN                  BINARY-LONG UNSIGNED.
        01 MESSAGE-INDEX        BINARY-LONG UNSIGNED.
        DEFINE-BLOCK-ENTITY-STRUCT
    LINKAGE SECTION.
        COPY DD-CALLBACK-BLOCK-ENTITY-SERIALIZE.

    PROCEDURE DIVISION USING LK-BLOCK-ENTITY LK-NBTENC LK-BUFFER.
        SET ADDRESS OF STRUCT TO LK-BLOCK-ENTITY-DATA

        CALL "NbtEncode-Byte" USING LK-NBTENC LK-BUFFER "is_waxed" STRUCT-WAXED

        CALL "NbtEncode-Compound" USING LK-NBTENC LK-BUFFER "front_text"
        CALL "NbtEncode-Byte" USING LK-NBTENC LK-BUFFER "has_glowing_text" STRUCT-FRONT-GLOWING
        MOVE FUNCTION STORED-CHAR-LENGTH(STRUCT-FRONT-COLOR) TO LEN
        CALL "NbtEncode-String" USING LK-NBTENC LK-BUFFER "color" STRUCT-FRONT-COLOR LEN
        CALL "NbtEncode-List" USING LK-NBTENC LK-BUFFER "messages"
        PERFORM VARYING MESSAGE-INDEX FROM 1 BY 1 UNTIL MESSAGE-INDEX > 4
            CALL "NbtEncode-String" USING LK-NBTENC LK-BUFFER OMITTED STRUCT-FRONT-LINE-STR(MESSAGE-INDEX) STRUCT-FRONT-LINE-LEN(MESSAGE-INDEX)
        END-PERFORM
        CALL "NbtEncode-EndList" USING LK-NBTENC LK-BUFFER
        CALL "NbtEncode-EndCompound" USING LK-NBTENC LK-BUFFER

        CALL "NbtEncode-Compound" USING LK-NBTENC LK-BUFFER "back_text"
        CALL "NbtEncode-Byte" USING LK-NBTENC LK-BUFFER "has_glowing_text" STRUCT-BACK-GLOWING
        MOVE FUNCTION STORED-CHAR-LENGTH(STRUCT-BACK-COLOR) TO LEN
        CALL "NbtEncode-String" USING LK-NBTENC LK-BUFFER "color" STRUCT-BACK-COLOR LEN
        CALL "NbtEncode-List" USING LK-NBTENC LK-BUFFER "messages"
        PERFORM VARYING MESSAGE-INDEX FROM 1 BY 1 UNTIL MESSAGE-INDEX > 4
            CALL "NbtEncode-String" USING LK-NBTENC LK-BUFFER OMITTED STRUCT-BACK-LINE-STR(MESSAGE-INDEX) STRUCT-BACK-LINE-LEN(MESSAGE-INDEX)
        END-PERFORM
        CALL "NbtEncode-EndList" USING LK-NBTENC LK-BUFFER
        CALL "NbtEncode-EndCompound" USING LK-NBTENC LK-BUFFER

        GOBACK.

    END PROGRAM Callback-Serialize.

    *> --- Callback-Deserialize ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Callback-Deserialize.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        01 AT-END               BINARY-CHAR UNSIGNED.
        01 LEN                  BINARY-LONG UNSIGNED.
        01 TAG                  PIC X(16).
        01 STR                  PIC X(256).
        01 MESSAGE-COUNT        BINARY-LONG.
        01 MESSAGE-INDEX        BINARY-LONG UNSIGNED.
        DEFINE-BLOCK-ENTITY-STRUCT
    LINKAGE SECTION.
        COPY DD-CALLBACK-BLOCK-ENTITY-DESERIALIZE.

    PROCEDURE DIVISION USING LK-BLOCK-ENTITY LK-NBTDEC LK-BUFFER LK-TAG.
        SET ADDRESS OF STRUCT TO LK-BLOCK-ENTITY-DATA

        EVALUATE LK-TAG
            WHEN "is_waxed"
                CALL "NbtDecode-Byte" USING LK-NBTDEC LK-BUFFER STRUCT-WAXED
            WHEN "front_text"
                PERFORM DeserializeFrontText
            WHEN "back_text"
                PERFORM DeserializeBackText
            WHEN OTHER
                *> TODO keepPacked
                *> Note: Remove this in case we add a "base class" for block entities
                CALL "NbtDecode-Skip" USING LK-NBTDEC LK-BUFFER
        END-EVALUATE

        GOBACK.

    DeserializeFrontText.
        CALL "NbtDecode-Compound" USING LK-NBTDEC LK-BUFFER
        PERFORM UNTIL EXIT
            CALL "NbtDecode-Peek" USING LK-NBTDEC LK-BUFFER AT-END TAG
            IF AT-END > 0
                EXIT PERFORM
            END-IF
            EVALUATE TAG
                WHEN "has_glowing_text"
                    CALL "NbtDecode-Byte" USING LK-NBTDEC LK-BUFFER STRUCT-FRONT-GLOWING
                WHEN "color"
                    CALL "NbtDecode-String" USING LK-NBTDEC LK-BUFFER STR LEN
                    MOVE STR(1:LEN) TO STRUCT-FRONT-COLOR
                WHEN "messages"
                    CALL "NbtDecode-List" USING LK-NBTDEC LK-BUFFER MESSAGE-COUNT
                    PERFORM VARYING MESSAGE-INDEX FROM 1 BY 1 UNTIL MESSAGE-INDEX > MESSAGE-COUNT
                        CALL "NbtDecode-String" USING LK-NBTDEC LK-BUFFER STR LEN
                        IF MESSAGE-INDEX <= 4
                            MOVE LEN TO STRUCT-FRONT-LINE-LEN(MESSAGE-INDEX)
                            MOVE STR(1:LEN) TO STRUCT-FRONT-LINE-STR(MESSAGE-INDEX)
                        END-IF
                    END-PERFORM
                    CALL "NbtDecode-EndList" USING LK-NBTDEC LK-BUFFER
                WHEN OTHER
                    CALL "NbtDecode-Skip" USING LK-NBTDEC LK-BUFFER
            END-EVALUATE
        END-PERFORM
        CALL "NbtDecode-EndCompound" USING LK-NBTDEC LK-BUFFER
        .

    DeserializeBackText.
        CALL "NbtDecode-Compound" USING LK-NBTDEC LK-BUFFER
        PERFORM UNTIL EXIT
            CALL "NbtDecode-Peek" USING LK-NBTDEC LK-BUFFER AT-END TAG
            IF AT-END > 0
                EXIT PERFORM
            END-IF
            EVALUATE TAG
                WHEN "has_glowing_text"
                    CALL "NbtDecode-Byte" USING LK-NBTDEC LK-BUFFER STRUCT-BACK-GLOWING
                WHEN "color"
                    CALL "NbtDecode-String" USING LK-NBTDEC LK-BUFFER STR LEN
                    MOVE STR(1:LEN) TO STRUCT-BACK-COLOR
                WHEN "messages"
                    CALL "NbtDecode-List" USING LK-NBTDEC LK-BUFFER MESSAGE-COUNT
                    PERFORM VARYING MESSAGE-INDEX FROM 1 BY 1 UNTIL MESSAGE-INDEX > MESSAGE-COUNT
                        CALL "NbtDecode-String" USING LK-NBTDEC LK-BUFFER STR LEN
                        IF MESSAGE-INDEX <= 4
                            MOVE LEN TO STRUCT-BACK-LINE-LEN(MESSAGE-INDEX)
                            MOVE STR(1:LEN) TO STRUCT-BACK-LINE-STR(MESSAGE-INDEX)
                        END-IF
                    END-PERFORM
                    CALL "NbtDecode-EndList" USING LK-NBTDEC LK-BUFFER
                WHEN OTHER
                    CALL "NbtDecode-Skip" USING LK-NBTDEC LK-BUFFER
            END-EVALUATE
        END-PERFORM
        CALL "NbtDecode-EndCompound" USING LK-NBTDEC LK-BUFFER
        .

    END PROGRAM Callback-Deserialize.

END PROGRAM RegisterBlockEntity-Sign.
