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
30 ENTITY-AGE               BINARY-LONG.
*> TODO make entity data more generic
*> for item entities: item stack
30 ENTITY-ITEM-SLOT.
    COPY DD-INVENTORY-SLOT REPLACING LEADING ==PREFIX== BY ==ENTITY-ITEM==.
30 ENTITY-ITEM-PICKUP-DELAY BINARY-LONG.
