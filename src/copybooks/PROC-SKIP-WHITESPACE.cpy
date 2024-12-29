*> --- Copybook: skip whitespace in a string starting at a given index ---
PERFORM UNTIL IDX > LEN
    IF NOT (STR(IDX:1) = X"20" OR STR(IDX:1) = X"09" OR STR(IDX:1) = X"0A" OR STR(IDX:1) = X"0D")
        EXIT PERFORM
    END-IF
    ADD 1 TO IDX
END-PERFORM
