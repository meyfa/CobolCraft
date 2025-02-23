*> --- Components-LengthOf ---
*> Get the length of a structured component (in bytes) from a data buffer, including the component ID.
IDENTIFICATION DIVISION.
PROGRAM-ID. Components-LengthOf IS RECURSIVE.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 SUB-STR                  PIC X(1024).
LOCAL-STORAGE SECTION.
    01 STARTPOS                 BINARY-LONG UNSIGNED.
    01 BUFFERPOS                BINARY-LONG UNSIGNED.
    01 COMPONENT                BINARY-LONG.
    01 TEMP-INT32               BINARY-LONG.
    01 TEMP-INT8                BINARY-CHAR.
    01 SUB-INT32                BINARY-LONG.
    01 SUB-INT8                 BINARY-CHAR.
    COPY DD-NBT-DECODER.
    01 SLOT-COMP-ADD            BINARY-LONG UNSIGNED.
    01 SLOT-COMP-REMOVE         BINARY-LONG UNSIGNED.
    01 SLOT-COMP-LENGTH         BINARY-LONG UNSIGNED.
LINKAGE SECTION.
    01 LK-BUFFER                PIC X ANY LENGTH.
    01 LK-OFFSET                BINARY-LONG UNSIGNED.
    01 LK-COMPONENT-LENGTH      BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-BUFFER LK-OFFSET LK-COMPONENT-LENGTH.
    MOVE LK-OFFSET TO STARTPOS BUFFERPOS

    CALL "Decode-VarInt" USING LK-BUFFER BUFFERPOS COMPONENT

    *> TODO avoid hardcoding these IDs

    *> https://minecraft.wiki/w/Minecraft_Wiki:Projects/wiki.vg_merge/Slot_Data#Structured_components
    EVALUATE COMPONENT
        WHEN 0
            PERFORM Nbt
        WHEN 1
            Perform VarInt
        WHEN 2
            PERFORM VarInt
        WHEN 3
            PERFORM VarInt
        WHEN 4
            PERFORM Bool
        WHEN 5
            PERFORM TextComponent
        WHEN 6
            PERFORM TextComponent
        WHEN 7
            PERFORM Identifier
        WHEN 8
            CALL "Decode-VarInt" USING LK-BUFFER BUFFERPOS TEMP-INT32
            PERFORM TEMP-INT32 TIMES
                PERFORM TextComponent
            END-PERFORM
        WHEN 9
            PERFORM VarInt
        WHEN 10
            CALL "Decode-VarInt" USING LK-BUFFER BUFFERPOS TEMP-INT32
            PERFORM TEMP-INT32 TIMES
                PERFORM VarInt
                PERFORM VarInt
            END-PERFORM
            PERFORM Bool
        WHEN 11
            CALL "Decode-VarInt" USING LK-BUFFER BUFFERPOS TEMP-INT32
            PERFORM TEMP-INT32 TIMES
                PERFORM BlockPredicate
            END-PERFORM
            PERFORM Bool
        WHEN 12
            CALL "Decode-VarInt" USING LK-BUFFER BUFFERPOS TEMP-INT32
            PERFORM TEMP-INT32 TIMES
                PERFORM BlockPredicate
            END-PERFORM
            PERFORM Bool
        WHEN 13
            CALL "Decode-VarInt" USING LK-BUFFER BUFFERPOS TEMP-INT32
            PERFORM TEMP-INT32 TIMES
                PERFORM VarInt
                PERFORM Identifier
                PERFORM FloatDouble
                PERFORM VarInt
                PERFORM VarInt
            END-PERFORM
            PERFORM Bool
        WHEN 14
            CALL "Decode-VarInt" USING LK-BUFFER BUFFERPOS TEMP-INT32
            PERFORM TEMP-INT32 TIMES
                PERFORM FloatShort
            END-PERFORM
            CALL "Decode-VarInt" USING LK-BUFFER BUFFERPOS TEMP-INT32
            PERFORM TEMP-INT32 TIMES
                PERFORM Bool
            END-PERFORM
            CALL "Decode-VarInt" USING LK-BUFFER BUFFERPOS TEMP-INT32
            PERFORM TEMP-INT32 TIMES
                PERFORM Str
            END-PERFORM
            CALL "Decode-VarInt" USING LK-BUFFER BUFFERPOS TEMP-INT32
            PERFORM TEMP-INT32 TIMES
                PERFORM Int
            END-PERFORM
        WHEN 15
            CONTINUE
        WHEN 16
            CONTINUE
        WHEN 17
            PERFORM VarInt
        WHEN 18
            CONTINUE
        WHEN 19
            PERFORM Bool
        WHEN 20
            CONTINUE
        WHEN 21
            PERFORM VarInt
            PERFORM FloatShort
            PERFORM Bool
        WHEN 22
            PERFORM FloatShort
            PERFORM VarInt
            *> inline (=0) or ID + 1
            CALL "Decode-VarInt" USING LK-BUFFER BUFFERPOS TEMP-INT32
            IF TEMP-INT32 = 0
                PERFORM SoundEvent
            END-IF
        WHEN 23
            PERFORM Slot
        WHEN 24
            PERFORM FloatShort
            CALL "Decode-Byte" USING LK-BUFFER BUFFERPOS TEMP-INT8
            IF TEMP-INT8 NOT = 0
                PERFORM Identifier
            END-IF
        WHEN 25
            PERFORM Identifier
        WHEN 26
            CALL "Decode-VarInt" USING LK-BUFFER BUFFERPOS TEMP-INT32
            PERFORM TEMP-INT32 TIMES
                PERFORM IdSet
                CALL "Decode-Byte" USING LK-BUFFER BUFFERPOS TEMP-INT8
                IF TEMP-INT8 NOT = 0
                    PERFORM FloatShort
                END-IF
                CALL "Decode-Byte" USING LK-BUFFER BUFFERPOS TEMP-INT8
                IF TEMP-INT8 NOT = 0
                    PERFORM Bool
                END-IF
            END-PERFORM
            PERFORM FloatShort
            PERFORM VarInt
        WHEN 27
            PERFORM VarInt
        WHEN 28
            PERFORM VarInt
            *> inline (=0) or ID + 1
            CALL "Decode-VarInt" USING LK-BUFFER BUFFERPOS TEMP-INT32
            IF TEMP-INT32 = 0
                PERFORM SoundEvent
            END-IF
            CALL "Decode-Byte" USING LK-BUFFER BUFFERPOS TEMP-INT8
            IF TEMP-INT8 NOT = 0
                PERFORM Identifier
            END-IF
            CALL "Decode-Byte" USING LK-BUFFER BUFFERPOS TEMP-INT8
            IF TEMP-INT8 NOT = 0
                PERFORM Identifier
            END-IF
            CALL "Decode-Byte" USING LK-BUFFER BUFFERPOS TEMP-INT8
            IF TEMP-INT8 NOT = 0
                PERFORM IdSet
            END-IF
            PERFORM Bool
            PERFORM Bool
            PERFORM Bool
        WHEN 29
            PERFORM IdSet
        WHEN 30
            CONTINUE
        WHEN 31
            PERFORM Identifier
        WHEN 32
            CALL "Decode-VarInt" USING LK-BUFFER BUFFERPOS TEMP-INT32
            PERFORM TEMP-INT32 TIMES
                *> consume effect
                *> TODO avoid hardcoding these IDs
                CALL "Decode-VarInt" USING LK-BUFFER BUFFERPOS TEMP-INT32
                EVALUATE TEMP-INT32
                    WHEN 0
                        *> wiki says this is a plain array, but that is not possible?!
                        CALL "Decode-VarInt" USING LK-BUFFER BUFFERPOS TEMP-INT32
                        PERFORM TEMP-INT32 TIMES
                            PERFORM PotionEffect
                        END-PERFORM
                        PERFORM FloatShort
                    WHEN 1
                        PERFORM IdSet
                    WHEN 2
                        CONTINUE
                    WHEN 3
                        PERFORM FloatShort
                    WHEN 4
                        PERFORM SoundEvent
                END-EVALUATE
            END-PERFORM
        WHEN 33
            CALL "Decode-VarInt" USING LK-BUFFER BUFFERPOS TEMP-INT32
            PERFORM TEMP-INT32 TIMES
                PERFORM VarInt
                PERFORM VarInt
            END-PERFORM
            PERFORM Bool
        WHEN 34
            PERFORM Int
            PERFORM Bool
        WHEN 35
            PERFORM Int
        WHEN 36
            PERFORM VarInt
        WHEN 37
            PERFORM Nbt
        WHEN 38
            PERFORM VarInt
        WHEN 39
            CALL "Decode-VarInt" USING LK-BUFFER BUFFERPOS TEMP-INT32
            PERFORM TEMP-INT32 TIMES
                PERFORM Slot
            END-PERFORM
        WHEN 40
            CALL "Decode-VarInt" USING LK-BUFFER BUFFERPOS TEMP-INT32
            PERFORM TEMP-INT32 TIMES
                PERFORM Slot
            END-PERFORM
        WHEN 41
            CALL "Decode-Byte" USING LK-BUFFER BUFFERPOS TEMP-INT8
            IF TEMP-INT8 NOT = 0
                PERFORM VarInt
            END-IF
            CALL "Decode-Byte" USING LK-BUFFER BUFFERPOS TEMP-INT8
            IF TEMP-INT8 NOT = 0
                PERFORM Int
            END-IF
            CALL "Decode-VarInt" USING LK-BUFFER BUFFERPOS TEMP-INT32
            PERFORM TEMP-INT32 TIMES
                PERFORM PotionEffect
            END-PERFORM
            PERFORM Str
        WHEN 42
            CALL "Decode-VarInt" USING LK-BUFFER BUFFERPOS TEMP-INT32
            PERFORM TEMP-INT32 TIMES
                PERFORM VarInt
                PERFORM VarInt
            END-PERFORM
        WHEN 43
            CALL "Decode-VarInt" USING LK-BUFFER BUFFERPOS TEMP-INT32
            PERFORM TEMP-INT32 TIMES
                PERFORM Str
                CALL "Decode-Byte" USING LK-BUFFER BUFFERPOS TEMP-INT8
                IF TEMP-INT8 NOT = 0
                    PERFORM Str
                END-IF
            END-PERFORM
        WHEN 44
            PERFORM Str
            CALL "Decode-Byte" USING LK-BUFFER BUFFERPOS TEMP-INT8
            IF TEMP-INT8 NOT = 0
                PERFORM Str
            END-IF
            PERFORM Str
            PERFORM VarInt
            CALL "Decode-VarInt" USING LK-BUFFER BUFFERPOS TEMP-INT32
            PERFORM TEMP-INT32 TIMES
                PERFORM TextComponent
                CALL "Decode-Byte" USING LK-BUFFER BUFFERPOS TEMP-INT8
                IF TEMP-INT8 NOT = 0
                    PERFORM TextComponent
                END-IF
            END-PERFORM
            PERFORM Bool
        WHEN 45
            CALL "Decode-VarInt" USING LK-BUFFER BUFFERPOS TEMP-INT32
            IF TEMP-INT32 = 0
                PERFORM TrimMaterial
            END-IF
            CALL "Decode-VarInt" USING LK-BUFFER BUFFERPOS TEMP-INT32
            IF TEMP-INT32 = 0
                PERFORM TrimPattern
            END-IF
            PERFORM Bool
        WHEN 46
            PERFORM Nbt
        WHEN 47
            PERFORM Nbt
        WHEN 48
            PERFORM Nbt
        WHEN 49
            PERFORM Nbt
        WHEN 50
            CALL "Decode-VarInt" USING LK-BUFFER BUFFERPOS TEMP-INT32
            IF TEMP-INT32 = 0
                PERFORM Instrument
            END-IF
        WHEN 51
            PERFORM VarInt
        WHEN 52
            CALL "Decode-Byte" USING LK-BUFFER BUFFERPOS TEMP-INT8
            IF TEMP-INT8 NOT = 0
                CALL "Decode-VarInt" USING LK-BUFFER BUFFERPOS TEMP-INT32
                IF TEMP-INT32 = 0
                    PERFORM JukeboxSong
                END-IF
            ELSE
                PERFORM Identifier
            END-IF
            PERFORM Bool
        WHEN 53
            PERFORM Nbt
        WHEN 54
            CALL "Decode-Byte" USING LK-BUFFER BUFFERPOS TEMP-INT8
            IF TEMP-INT8 NOT = 0
                PERFORM Identifier
                PERFORM Position8
            END-IF
            PERFORM Bool
        WHEN 55
            PERFORM FireworkExplosion
        WHEN 56
            PERFORM VarInt
            CALL "Decode-VarInt" USING LK-BUFFER BUFFERPOS TEMP-INT32
            PERFORM TEMP-INT32 TIMES
                PERFORM FireworkExplosion
            END-PERFORM
        WHEN 57
            CALL "Decode-Byte" USING LK-BUFFER BUFFERPOS TEMP-INT8
            IF TEMP-INT8 NOT = 0
                PERFORM Str
            END-IF
            CALL "Decode-Byte" USING LK-BUFFER BUFFERPOS TEMP-INT8
            IF TEMP-INT8 NOT = 0
                PERFORM Uuid
            END-IF
            CALL "Decode-VarInt" USING LK-BUFFER BUFFERPOS TEMP-INT32
            PERFORM TEMP-INT32 TIMES
                PERFORM Str
                PERFORM Str
                CALL "Decode-Byte" USING LK-BUFFER BUFFERPOS TEMP-INT8
                IF TEMP-INT8 NOT = 0
                    PERFORM Str
                END-IF
            END-PERFORM
        WHEN 58
            PERFORM Identifier
        WHEN 59
            CALL "Decode-VarInt" USING LK-BUFFER BUFFERPOS TEMP-INT32
            PERFORM TEMP-INT32 TIMES
                CALL "Decode-VarInt" USING LK-BUFFER BUFFERPOS TEMP-INT32
                IF TEMP-INT32 = 0
                    PERFORM Identifier
                    PERFORM Identifier
                END-IF
                PERFORM DyeColor
            END-PERFORM
        WHEN 60
            PERFORM DyeColor
        WHEN 61
            CALL "Decode-VarInt" USING LK-BUFFER BUFFERPOS TEMP-INT32
            PERFORM TEMP-INT32 TIMES
                PERFORM VarInt
            END-PERFORM
        WHEN 62
            CALL "Decode-VarInt" USING LK-BUFFER BUFFERPOS TEMP-INT32
            PERFORM TEMP-INT32 TIMES
                PERFORM Slot
            END-PERFORM
        WHEN 63
            CALL "Decode-VarInt" USING LK-BUFFER BUFFERPOS TEMP-INT32
            PERFORM TEMP-INT32 TIMES
                PERFORM Str
                PERFORM Str
            END-PERFORM
        WHEN 64
            CALL "Decode-VarInt" USING LK-BUFFER BUFFERPOS TEMP-INT32
            PERFORM TEMP-INT32 TIMES
                PERFORM Nbt
                PERFORM VarInt
                PERFORM VarInt
            END-PERFORM
        WHEN 65
            PERFORM Nbt
        WHEN 66
            PERFORM Nbt

        WHEN OTHER
            DISPLAY "Unknown component ID: " COMPONENT
            MOVE 0 TO LK-COMPONENT-LENGTH
            GOBACK
    END-EVALUATE

    COMPUTE LK-COMPONENT-LENGTH = BUFFERPOS - STARTPOS

    GOBACK.

VarInt.
    CALL "Decode-VarInt" USING LK-BUFFER BUFFERPOS SUB-INT32
    .

Int.
    ADD 4 TO BUFFERPOS
    .

Bool.
    ADD 1 TO BUFFERPOS
    .

Str.
    CALL "Decode-String" USING LK-BUFFER BUFFERPOS SUB-INT32 SUB-STR
    .

Identifier.
    PERFORM Str
    .

FloatShort.
    ADD 4 TO BUFFERPOS
    .

FloatDouble.
    ADD 8 TO BUFFERPOS
    .

Nbt.
    MOVE BUFFERPOS TO NBTDEC-OFFSET
    MOVE 0 TO NBTDEC-LEVEL
    CALL "NbtDecode-Skip" USING NBTDEC LK-BUFFER
    MOVE NBTDEC-OFFSET TO BUFFERPOS
    .

TextComponent.
    PERFORM Nbt
    .

SoundEvent.
    PERFORM Identifier
    CALL "Decode-Byte" USING LK-BUFFER BUFFERPOS SUB-INT8
    IF SUB-INT8 NOT = 0
        PERFORM FloatShort
    END-IF
    .

PotionEffect.
    PERFORM VarInt
    PERFORM VarInt
    PERFORM VarInt
    PERFORM Bool
    PERFORM Bool
    PERFORM Bool
    CALL "Decode-Byte" USING LK-BUFFER BUFFERPOS SUB-INT8
    IF SUB-INT8 NOT = 0
        PERFORM Bool
    END-IF
    .

BlockPredicate.
    CALL "Decode-Byte" USING LK-BUFFER BUFFERPOS SUB-INT8
    IF SUB-INT8 NOT = 0
        PERFORM IdSet
    END-IF
    CALL "Decode-Byte" USING LK-BUFFER BUFFERPOS SUB-INT8
    IF SUB-INT8 NOT = 0
        CALL "Decode-VarInt" USING LK-BUFFER BUFFERPOS SUB-INT32
        PERFORM SUB-INT32 TIMES
            PERFORM Str
            CALL "Decode-Byte" USING LK-BUFFER BUFFERPOS SUB-INT8
            IF SUB-INT8 NOT = 0
                PERFORM Str
            ELSE
                PERFORM Str
                PERFORM Str
            END-IF
        END-PERFORM
    END-IF
    CALL "Decode-Byte" USING LK-BUFFER BUFFERPOS SUB-INT8
    IF SUB-INT8 NOT = 0
        PERFORM Nbt
    END-IF
    .

IdSet.
    CALL "Decode-VarInt" USING LK-BUFFER BUFFERPOS SUB-INT32
    IF SUB-INT32 = 0
        PERFORM Identifier
    ELSE
        PERFORM TEMP-INT32 TIMES
            PERFORM VarInt
        END-PERFORM
    END-IF
    .

Position8.
    ADD 8 TO BUFFERPOS
    .

Uuid.
    ADD 16 TO BUFFERPOS
    .

Slot.
    *> count
    CALL "Decode-VarInt" USING LK-BUFFER BUFFERPOS SUB-INT32
    IF SUB-INT32 > 0
        *> item id
        PERFORM VarInt
        CALL "Decode-VarInt" USING LK-BUFFER BUFFERPOS SLOT-COMP-ADD
        CALL "Decode-VarInt" USING LK-BUFFER BUFFERPOS SLOT-COMP-REMOVE
        PERFORM SLOT-COMP-ADD TIMES
            *> recursive!
            CALL "Components-LengthOf" USING LK-BUFFER BUFFERPOS SLOT-COMP-LENGTH
            ADD SLOT-COMP-LENGTH TO BUFFERPOS
        END-PERFORM
        PERFORM SLOT-COMP-REMOVE TIMES
            PERFORM VarInt
        END-PERFORM
    END-IF
    .

TrimMaterial.
    PERFORM Str
    PERFORM VarInt
    PERFORM FloatShort
    CALL "Decode-VarInt" USING LK-BUFFER BUFFERPOS SUB-INT32
    PERFORM SUB-INT32 TIMES
        PERFORM VarInt
        PERFORM Str
    END-PERFORM
    PERFORM TextComponent
    .

TrimPattern.
    PERFORM Str
    PERFORM VarInt
    PERFORM TextComponent
    PERFORM Bool
    .

Instrument.
    CALL "Decode-VarInt" USING LK-BUFFER BUFFERPOS SUB-INT32
    IF SUB-INT32 = 0
        PERFORM SoundEvent
    END-IF
    PERFORM FloatShort
    PERFORM FloatShort
    PERFORM TextComponent
    .

JukeboxSong.
    CALL "Decode-VarInt" USING LK-BUFFER BUFFERPOS SUB-INT32
    IF SUB-INT32 = 0
        PERFORM SoundEvent
    END-IF
    PERFORM TextComponent
    PERFORM FloatShort
    PERFORM VarInt
    .

FireworkExplosion.
    PERFORM VarInt
    CALL "Decode-VarInt" USING LK-BUFFER BUFFERPOS SUB-INT32
    PERFORM SUB-INT32 TIMES
        PERFORM Int
    END-PERFORM
    CALL "Decode-VarInt" USING LK-BUFFER BUFFERPOS SUB-INT32
    PERFORM SUB-INT32 TIMES
        PERFORM Int
    END-PERFORM
    PERFORM Bool
    PERFORM Bool
    .

DyeColor.
    PERFORM VarInt
    .

END PROGRAM Components-LengthOf.
