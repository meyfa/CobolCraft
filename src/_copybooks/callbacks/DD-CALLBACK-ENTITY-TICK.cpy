*> --- Copybook: callback parameters for entity update during game tick ---

01 LK-ENTITY.
    COPY DD-ENTITY REPLACING LEADING ==ENTITY== BY ==LK-ENTITY==.

*> Pre-computed player bounding boxes
01 LK-PLAYER-AABBS.
    02 LK-PLAYER-AABB OCCURS 100 TIMES.
        COPY DD-AABB REPLACING LEADING ==PREFIX== BY ==LK-PLAYER==.

*> Whether the entity should be removed from the world
01 LK-REMOVE                    BINARY-CHAR UNSIGNED.
