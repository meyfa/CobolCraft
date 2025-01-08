*> --- Copybook: initialize a packet ---

IF PACKET-ID < 0
    CALL "Packets-GetId" USING PACKET-REFERENCE PACKET-ID
    IF PACKET-ID < 0
        DISPLAY "Error: Packet ID not found for reference '" FUNCTION TRIM(PACKET-REFERENCE) "'"
    END-IF
END-IF
