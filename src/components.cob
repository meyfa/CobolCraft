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
        WHEN 0 *> minecraft:custom_data
            PERFORM Nbt
        WHEN 1 *> minecraft:max_stack_size
            Perform VarInt
        WHEN 2 *> minecraft:max_damage
            PERFORM VarInt
        WHEN 3 *> minecraft:damage
            PERFORM VarInt
        WHEN 4 *> minecraft:unbreakable
            PERFORM Bool
        WHEN 5 *> minecraft:custom_name
            PERFORM TextComponent
        WHEN 6 *> minecraft:item_name
            PERFORM TextComponent
        WHEN 7 *> minecraft:item_model
            PERFORM Identifier
        WHEN 8 *> minecraft:lore
            CALL "Decode-VarInt" USING LK-BUFFER BUFFERPOS TEMP-INT32
            PERFORM TEMP-INT32 TIMES
                PERFORM TextComponent
            END-PERFORM
        WHEN 9 *> minecraft:rarity
            PERFORM VarInt
        WHEN 10 *> minecraft:enchantments
            CALL "Decode-VarInt" USING LK-BUFFER BUFFERPOS TEMP-INT32
            PERFORM TEMP-INT32 TIMES
                PERFORM VarInt
                PERFORM VarInt
            END-PERFORM
            PERFORM Bool
        WHEN 11 *> minecraft:can_place_on
            CALL "Decode-VarInt" USING LK-BUFFER BUFFERPOS TEMP-INT32
            PERFORM TEMP-INT32 TIMES
                PERFORM BlockPredicate
            END-PERFORM
            PERFORM Bool
        WHEN 12 *> minecraft:can_break
            CALL "Decode-VarInt" USING LK-BUFFER BUFFERPOS TEMP-INT32
            PERFORM TEMP-INT32 TIMES
                PERFORM BlockPredicate
            END-PERFORM
            PERFORM Bool
        WHEN 13 *> minecraft:attribute_modifiers
            CALL "Decode-VarInt" USING LK-BUFFER BUFFERPOS TEMP-INT32
            PERFORM TEMP-INT32 TIMES
                PERFORM VarInt
                PERFORM Identifier
                PERFORM FloatDouble
                PERFORM VarInt
                PERFORM VarInt
            END-PERFORM
            PERFORM Bool
        WHEN 14 *> minecraft:custom_model_data
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
        WHEN 15 *> minecraft:tooltip_display
            *> TODO: add (the wiki does not have this yet)
            CONTINUE
        WHEN 16 *> minecraft:repair_cost
            PERFORM VarInt
        WHEN 17 *> minecraft:creative_slot_lock
            CONTINUE
        WHEN 18 *> minecraft:enchantment_glint_override
            PERFORM Bool
        WHEN 19 *> minecraft:intangible_projectile
            PERFORM Nbt
        WHEN 20 *> minecraft:food
            PERFORM VarInt
            PERFORM FloatShort
            PERFORM Bool
        WHEN 21 *> minecraft:consumable
            PERFORM FloatShort
            PERFORM VarInt
            *> inline (=0) or ID + 1
            CALL "Decode-VarInt" USING LK-BUFFER BUFFERPOS TEMP-INT32
            IF TEMP-INT32 = 0
                PERFORM SoundEvent
            END-IF
            PERFORM Bool
            CALL "Decode-VarInt" USING LK-BUFFER BUFFERPOS TEMP-INT32
            PERFORM TEMP-INT32 TIMES
                PERFORM ConsumeEffect
            END-PERFORM
        WHEN 22 *> minecraft:use_remainder
            PERFORM Slot
        WHEN 23 *> minecraft:use_cooldown
            PERFORM FloatShort
            CALL "Decode-Byte" USING LK-BUFFER BUFFERPOS TEMP-INT8
            IF TEMP-INT8 NOT = 0
                PERFORM Identifier
            END-IF
        WHEN 24 *> minecraft:damage_resistant
            PERFORM Identifier
        WHEN 25 *> minecraft:tool
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
        WHEN 26 *> minecraft:weapon
            PERFORM VarInt
            PERFORM FloatShort
        WHEN 27 *> minecraft:enchantable
            PERFORM VarInt
        WHEN 28 *> minecraft:equippable
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
        WHEN 29 *> minecraft:repairable
            PERFORM IdSet
        WHEN 30 *> minecraft:glider
            CONTINUE
        WHEN 31 *> minecraft:tooltip_style
            PERFORM Identifier
        WHEN 32 *> minecraft:death_protection
            CALL "Decode-VarInt" USING LK-BUFFER BUFFERPOS TEMP-INT32
            PERFORM TEMP-INT32 TIMES
                PERFORM ConsumeEffect
            END-PERFORM
        WHEN 33 *> minecraft:blocks_attacks
            *> TODO: add (the wiki does not have this yet)
            CONTINUE
        WHEN 34 *> minecraft:stored_enchantments
            CALL "Decode-VarInt" USING LK-BUFFER BUFFERPOS TEMP-INT32
            PERFORM TEMP-INT32 TIMES
                PERFORM VarInt
                PERFORM VarInt
            END-PERFORM
            PERFORM Bool
        WHEN 35 *> minecraft:dyed_color
            PERFORM Int
            PERFORM Bool
        WHEN 36 *> minecraft:map_color
            PERFORM Int
        WHEN 37 *> minecraft:map_id
            PERFORM VarInt
        WHEN 38 *> minecraft:map_decorations
            PERFORM Nbt
        WHEN 39 *> minecraft:map_post_processing
            PERFORM VarInt
        WHEN 40 *> minecraft:charged_projectiles
            CALL "Decode-VarInt" USING LK-BUFFER BUFFERPOS TEMP-INT32
            PERFORM TEMP-INT32 TIMES
                PERFORM Slot
            END-PERFORM
        WHEN 41 *> minecraft:bundle_contents
            CALL "Decode-VarInt" USING LK-BUFFER BUFFERPOS TEMP-INT32
            PERFORM TEMP-INT32 TIMES
                PERFORM Slot
            END-PERFORM
        WHEN 42 *> minecraft:potion_contents
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
        WHEN 43 *> minecraft:potion_duration_scale
            PERFORM FloatShort
        WHEN 44 *> minecraft:suspicious_stew_effects
            CALL "Decode-VarInt" USING LK-BUFFER BUFFERPOS TEMP-INT32
            PERFORM TEMP-INT32 TIMES
                PERFORM VarInt
                PERFORM VarInt
            END-PERFORM
        WHEN 45 *> minecraft:writable_book_content
            CALL "Decode-VarInt" USING LK-BUFFER BUFFERPOS TEMP-INT32
            PERFORM TEMP-INT32 TIMES
                PERFORM Str
                CALL "Decode-Byte" USING LK-BUFFER BUFFERPOS TEMP-INT8
                IF TEMP-INT8 NOT = 0
                    PERFORM Str
                END-IF
            END-PERFORM
        WHEN 46 *> minecraft:written_book_content
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
        WHEN 47 *> minecraft:trim
            CALL "Decode-VarInt" USING LK-BUFFER BUFFERPOS TEMP-INT32
            IF TEMP-INT32 = 0
                PERFORM TrimMaterial
            END-IF
            CALL "Decode-VarInt" USING LK-BUFFER BUFFERPOS TEMP-INT32
            IF TEMP-INT32 = 0
                PERFORM TrimPattern
            END-IF
            PERFORM Bool
        WHEN 48 *> minecraft:debug_stick_state
            PERFORM Nbt
        WHEN 49 *> minecraft:entity_data
            PERFORM Nbt
        WHEN 50 *> minecraft:bucket_entity_data
            PERFORM Nbt
        WHEN 51 *> minecraft:block_entity_data
            PERFORM Nbt
        WHEN 52 *> minecraft:instrument
            CALL "Decode-VarInt" USING LK-BUFFER BUFFERPOS TEMP-INT32
            IF TEMP-INT32 = 0
                PERFORM Instrument
            END-IF
        WHEN 53 *> minecraft:provides_trim_material
            *> TODO: add (the wiki does not have this yet)
            CONTINUE
        WHEN 54 *> minecraft:ominous_bottle_amplifier
            PERFORM VarInt
        WHEN 55 *> minecraft:jukebox_playable
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
        WHEN 56 *> minecraft:provides_banner_pattern
            *> TODO: add (the wiki does not have this yet)
            CONTINUE
        WHEN 57 *> minecraft:recipes
            PERFORM Nbt
        WHEN 58 *> minecraft:lodestone_tracker
            CALL "Decode-Byte" USING LK-BUFFER BUFFERPOS TEMP-INT8
            IF TEMP-INT8 NOT = 0
                PERFORM Identifier
                PERFORM Position8
            END-IF
            PERFORM Bool
        WHEN 59 *> minecraft:firework_explosion
            PERFORM FireworkExplosion
        WHEN 60 *> minecraft:fireworks
            PERFORM VarInt
            CALL "Decode-VarInt" USING LK-BUFFER BUFFERPOS TEMP-INT32
            PERFORM TEMP-INT32 TIMES
                PERFORM FireworkExplosion
            END-PERFORM
        WHEN 61 *> minecraft:profile
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
        WHEN 62 *> minecraft:note_block_sound
            PERFORM Identifier
        WHEN 63 *> minecraft:banner_patterns
            CALL "Decode-VarInt" USING LK-BUFFER BUFFERPOS TEMP-INT32
            PERFORM TEMP-INT32 TIMES
                CALL "Decode-VarInt" USING LK-BUFFER BUFFERPOS TEMP-INT32
                IF TEMP-INT32 = 0
                    PERFORM Identifier
                    PERFORM Identifier
                END-IF
                PERFORM DyeColor
            END-PERFORM
        WHEN 64 *> minecraft:base_color
            PERFORM DyeColor
        WHEN 65 *> minecraft:pot_decorations
            CALL "Decode-VarInt" USING LK-BUFFER BUFFERPOS TEMP-INT32
            PERFORM TEMP-INT32 TIMES
                PERFORM VarInt
            END-PERFORM
        WHEN 66 *> minecraft:container
            CALL "Decode-VarInt" USING LK-BUFFER BUFFERPOS TEMP-INT32
            PERFORM TEMP-INT32 TIMES
                PERFORM Slot
            END-PERFORM
        WHEN 67 *> minecraft:block_state
            CALL "Decode-VarInt" USING LK-BUFFER BUFFERPOS TEMP-INT32
            PERFORM TEMP-INT32 TIMES
                PERFORM Str
                PERFORM Str
            END-PERFORM
        WHEN 68 *> minecraft:bees
            CALL "Decode-VarInt" USING LK-BUFFER BUFFERPOS TEMP-INT32
            PERFORM TEMP-INT32 TIMES
                PERFORM Nbt
                PERFORM VarInt
                PERFORM VarInt
            END-PERFORM
        WHEN 69 *> minecraft:lock
            PERFORM Nbt
        WHEN 70 *> minecraft:container_loot
            PERFORM Nbt
        WHEN 71 *> minecraft:break_sound
            PERFORM SoundEvent

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
    *> parse detail; loop until "has hidden effect" is false
    MOVE 1 TO SUB-INT8
    PERFORM UNTIL SUB-INT8 = 0
        PERFORM VarInt
        PERFORM VarInt
        PERFORM Bool
        PERFORM Bool
        PERFORM Bool
        CALL "Decode-Byte" USING LK-BUFFER BUFFERPOS SUB-INT8
    END-PERFORM
    .

ConsumeEffect.
    *> TODO avoid hardcoding these IDs
    CALL "Decode-VarInt" USING LK-BUFFER BUFFERPOS SUB-INT32
    EVALUATE SUB-INT32
        WHEN 0
            *> wiki says this is a plain array, but that is not possible?!
            CALL "Decode-VarInt" USING LK-BUFFER BUFFERPOS SUB-INT32
            PERFORM SUB-INT32 TIMES
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
