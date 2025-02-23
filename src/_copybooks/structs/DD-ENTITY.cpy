*> --- Copybook: entity attributes ---

30 ENTITY-ID                BINARY-LONG.
30 ENTITY-UUID              PIC X(16).

30 ENTITY-TYPE              BINARY-LONG.

30 ENTITY-POSITION.
    31 ENTITY-X             FLOAT-LONG.
    31 ENTITY-Y             FLOAT-LONG.
    31 ENTITY-Z             FLOAT-LONG.
30 ENTITY-ROTATION.
    31 ENTITY-YAW           FLOAT-SHORT.
    31 ENTITY-PITCH         FLOAT-SHORT.
30 ENTITY-VELOCITY.
    31 ENTITY-VELOCITY-X    FLOAT-LONG.
    31 ENTITY-VELOCITY-Y    FLOAT-LONG.
    31 ENTITY-VELOCITY-Z    FLOAT-LONG.
30 ENTITY-ON-GROUND         BINARY-CHAR UNSIGNED.

30 ENTITY-NO-GRAVITY        BINARY-CHAR UNSIGNED.

*> The chunk the entity is in (chunk coordinates, i.e. position/16).
*> This may diverge from the entity's position while the entity is being moved. It is used to determine which chunk to
*> remove the entity from before adding it to its new chunk.
30 ENTITY-CHUNK-X           BINARY-LONG.
30 ENTITY-CHUNK-Z           BINARY-LONG.

*> TODO make entity data more generic
*> for item entities: item stack
30 ENTITY-ITEM-SLOT.
    COPY DD-INVENTORY-SLOT REPLACING LEADING ==PREFIX== BY ==ENTITY-ITEM==.
30 ENTITY-AGE               BINARY-LONG.
30 ENTITY-ITEM-PICKUP-DELAY BINARY-LONG.
