*> --- Copybook: data structure for inventory slots ---
*> Usage: COPY DD-INVENTORY-SLOT REPLACING LEADING ==PREFIX== BY ==<name>==.

*> If no item is present, the count and ID must both be 0.
40 PREFIX-SLOT-ID         BINARY-LONG.
40 PREFIX-SLOT-COUNT      BINARY-CHAR UNSIGNED.
40 PREFIX-SLOT-NBT-LENGTH BINARY-LONG UNSIGNED.
40 PREFIX-SLOT-NBT-DATA   PIC X(1024).
