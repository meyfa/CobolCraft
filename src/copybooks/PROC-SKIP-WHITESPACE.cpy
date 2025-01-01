*> --- Copybook: skip whitespace in a string starting at a given index ---
PERFORM UNTIL IDX > LEN
    IF NOT (STR(IDX:1) = X"20" OR X"09" OR X"0A" OR X"0D")
        EXIT PERFORM
    END-IF
    ADD 1 TO IDX
END-PERFORM
