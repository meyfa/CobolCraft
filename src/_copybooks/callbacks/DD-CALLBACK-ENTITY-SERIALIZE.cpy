*> --- Copybook: callback parameters for entity serialization (saving to NBT) ---

*> The entity to serialize
01 LK-ENTITY.
    COPY DD-ENTITY REPLACING LEADING ==ENTITY== BY ==LK-ENTITY==.

*> The NBT encoder state and buffer
COPY DD-NBT-ENCODER REPLACING LEADING ==NBT-ENCODER== BY ==LK-NBT-ENCODER==.
01 LK-BUFFER PIC X ANY LENGTH.
