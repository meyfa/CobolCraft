*> --- Copybook: lookup tables for packet IDs (serverbound and clientbound), loaded from the data generator ---

78 PACKET-REF-ENTRY-LENGTH VALUE 256.

*> Table of packet references and associated numeric IDs
01 PACKET-IDS EXTERNAL.
    02 PACKET-ID-COUNT          BINARY-LONG UNSIGNED.
    02 PACKET-ID-ENTRY OCCURS 0 TO 512 TIMES DEPENDING ON PACKET-ID-COUNT.
        *> Packet reference, e.g. "play/clientbound/minecraft:block_changed_ack"
        03 PACKET-ID-REFERENCE  PIC X(128).
        03 PACKET-ID-NUMBER     BINARY-LONG.

*> Table for looking up packet names by numeric IDs: client state -> direction -> packet ID
01 PACKET-REFERENCES EXTERNAL.
    02 PACKET-REF-STATE OCCURS 5 TIMES.
        03 PACKET-REF-DIRECTION OCCURS 2 TIMES.
            04 PACKET-REF-ENTRY OCCURS PACKET-REF-ENTRY-LENGTH TIMES.
                *> The packet name, without the state/direction prefix, e.g. "minecraft:block_changed_ack"
                05 PACKET-REF-NAME PIC X(64).
