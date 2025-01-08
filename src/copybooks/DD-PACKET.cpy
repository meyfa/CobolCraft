*> --- Copybook: data used by packet initialization ---

01 PACKET-META.
    *> Needs to be initialized by looking up the packet reference in data/generated/reports/packets.json
    02 PACKET-ID                BINARY-LONG                 VALUE -1.

    *> The packet reference - format: "state/direction/name", e.g. "play/clientbound/minecraft:block_changed_ack".
    02 PACKET-REFERENCE         PIC X(255)                  VALUE IDENTIFIER.
