*> --- Copybook: registry tags ---

*> It would consume way too much memory to keep all tags of all registries loaded, including the string identifiers
*> of each entry. Instead, we keep only the tags of a single registry (the one being processed), then expand its
*> references, and overall keep only the numeric identifiers of the entries.

78 TAGS-REGISTRY-CAPACITY VALUE 16.
78 TAGS-PER-REGISTRY-CAPACITY VALUE 256.
78 TAGS-ENTRY-CAPACITY VALUE 512.

01 TAGS EXTERNAL.
    02 TAGS-REGISTRY-COUNT BINARY-LONG UNSIGNED.
    02 TAGS-REGISTRY OCCURS TAGS-REGISTRY-CAPACITY TIMES.
        *> Registry identifier, such as "minecraft:item"
        03 TAGS-REGISTRY-NAME PIC X(64).
        *> Number of tags in this registry
        03 TAGS-REGISTRY-LENGTH BINARY-LONG UNSIGNED.
        *> Tags for this registry
        03 TAGS-REGISTRY-TAG OCCURS TAGS-PER-REGISTRY-CAPACITY TIMES.
            *> Tag name, such as "minecraft:enchantable/chest_armor"
            04 TAGS-REGISTRY-TAG-NAME PIC X(64).
            *> Number of registry entries with this tag
            04 TAGS-REGISTRY-TAG-LENGTH BINARY-LONG UNSIGNED.
            *> IDs of registry entries with this tag
            04 TAGS-REGISTRY-TAG-ENTRY OCCURS TAGS-ENTRY-CAPACITY TIMES BINARY-LONG UNSIGNED.

*> The current registry being processed
01 TAGS-CURRENT EXTERNAL.
    02 TAGS-CURRENT-NAME PIC X(64).
    02 TAGS-CURRENT-LENGTH BINARY-LONG UNSIGNED.
    02 TAGS-CURRENT-TAG OCCURS TAGS-PER-REGISTRY-CAPACITY TIMES.
        03 TAGS-CURRENT-TAG-NAME PIC X(64).
        03 TAGS-CURRENT-TAG-LENGTH BINARY-LONG UNSIGNED.
        *> Registry entries with this tag, such as "minecraft:diamond_chestplate";
        *> may contain references to other tags, such as "#minecraft:chest_armor"
        03 TAGS-CURRENT-TAG-ENTRY OCCURS TAGS-ENTRY-CAPACITY TIMES PIC X(64).
        *> Number of references that must be expanded
        03 TAGS-CURRENT-REFERENCES BINARY-LONG UNSIGNED.
