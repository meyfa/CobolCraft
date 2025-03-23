*> --- Copybook: callback parameters for block entity serialization (saving to NBT) ---

*> The block entity to serialize
01 LK-BLOCK-ENTITY.
    COPY DD-BLOCK-ENTITY REPLACING LEADING ==BLOCK-ENTITY== BY ==LK-BLOCK-ENTITY==.

*> The NBT encoder state and buffer
COPY DD-NBT-ENCODER REPLACING LEADING ==NBTENC== BY ==LK-NBTENC==.
01 LK-BUFFER PIC X ANY LENGTH.
