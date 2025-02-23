*> --- Copybook: callback parameters for entity deserialization (loading from NBT) ---

*> The entity to deserialize
01 LK-ENTITY.
    COPY DD-ENTITY REPLACING LEADING ==ENTITY== BY ==LK-ENTITY==.

*> The NBT decoder state and buffer
COPY DD-NBT-DECODER REPLACING LEADING ==NBTDEC== BY ==LK-NBTDEC==.
01 LK-BUFFER PIC X ANY LENGTH.

*> The tag of the current field
01 LK-TAG PIC X ANY LENGTH.
