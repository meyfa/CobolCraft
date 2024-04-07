*> --- Test: encode.cob ---
IDENTIFICATION DIVISION.
PROGRAM-ID. Test-Encode.

PROCEDURE DIVISION.
    DISPLAY "Test: encode.cob"
    CALL "Test-Encode-Double"
    CALL "Test-Encode-Float"
    GOBACK.

    *> --- Test: Encode-Double ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Test-Encode-Double.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        01 VALUE-IN     FLOAT-LONG.
        01 BUFFER       PIC X(10).
        01 BUFFERLEN    BINARY-LONG UNSIGNED.

    PROCEDURE DIVISION.
        DISPLAY "  Test: Encode-Double".
    PositiveZero.
        DISPLAY "    Case: +0.0 - " WITH NO ADVANCING
        MOVE 0.0 TO VALUE-IN
        CALL "Encode-Double" USING VALUE-IN BUFFER BUFFERLEN
        IF BUFFER = X"0000000000000000" AND BUFFERLEN = 8
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    NegativeZero.
        *> Note: I've found no way to differentiate +0.0 and -0.0 in COBOL, so they're encoded the same.
        DISPLAY "    Case: -0.0 - " WITH NO ADVANCING
        MOVE -0.0 TO VALUE-IN
        CALL "Encode-Double" USING VALUE-IN BUFFER BUFFERLEN
        IF BUFFER = X"0000000000000000" AND BUFFERLEN = 8
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    PositiveInfinity.
        *> Note: FLOAT-INFINITY is not supported in GnuCOBOL.
        DISPLAY "    Case: +Infinity - " WITH NO ADVANCING
        DISPLAY "SKIP".
    NegativeInfinity.
        *> Note: FLOAT-INFINITY is not supported in GnuCOBOL.
        DISPLAY "    Case: -Infinity - " WITH NO ADVANCING
        DISPLAY "SKIP".
    NaN.
        *> Note: FLOAT-NOT-A-NUMBER is not supported in GnuCOBOL.
        DISPLAY "    Case: NaN - " WITH NO ADVANCING
        DISPLAY "SKIP".
    Positive1.
        DISPLAY "    Case: +1.0 - " WITH NO ADVANCING
        MOVE 1.0 TO VALUE-IN
        CALL "Encode-Double" USING VALUE-IN BUFFER BUFFERLEN
        IF BUFFER = X"3FF0000000000000" AND BUFFERLEN = 8
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    Negative1.
        DISPLAY "    Case: -1.0 - " WITH NO ADVANCING
        MOVE -1.0 TO VALUE-IN
        CALL "Encode-Double" USING VALUE-IN BUFFER BUFFERLEN
        IF BUFFER = X"BFF0000000000000" AND BUFFERLEN = 8
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    PositiveDecimal.
        DISPLAY "    Case: +12.125 - " WITH NO ADVANCING
        MOVE 12.125 TO VALUE-IN
        CALL "Encode-Double" USING VALUE-IN BUFFER BUFFERLEN
        IF BUFFER = X"4028400000000000" AND BUFFERLEN = 8
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    NegativeDecimal.
        DISPLAY "    Case: -12.125 - " WITH NO ADVANCING
        MOVE -12.125 TO VALUE-IN
        CALL "Encode-Double" USING VALUE-IN BUFFER BUFFERLEN
        IF BUFFER = X"C028400000000000" AND BUFFERLEN = 8
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    PositiveMax.
        DISPLAY "    Case: +1.79769E+308 - " WITH NO ADVANCING
        MOVE 1.79769313486231570814527423731704357E+308 TO VALUE-IN
        CALL "Encode-Double" USING VALUE-IN BUFFER BUFFERLEN
        IF BUFFER = X"7FEFFFFFFFFFFFFF" AND BUFFERLEN = 8
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    NegativeMax.
        DISPLAY "    Case: -1.79769E+308 - " WITH NO ADVANCING
        MOVE -1.79769313486231570814527423731704357E+308 TO VALUE-IN
        CALL "Encode-Double" USING VALUE-IN BUFFER BUFFERLEN
        IF BUFFER = X"FFEFFFFFFFFFFFFF" AND BUFFERLEN = 8
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.

        GOBACK.

    END PROGRAM Test-Encode-Double.

    *> --- Test: Encode-Float ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Test-Encode-Float.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        01 VALUE-IN     FLOAT-SHORT.
        01 BUFFER       PIC X(10).
        01 BUFFERLEN    BINARY-LONG UNSIGNED.

    PROCEDURE DIVISION.
        DISPLAY "  Test: Encode-Float".
    PositiveZero.
        DISPLAY "    Case: +0.0 - " WITH NO ADVANCING
        MOVE 0.0 TO VALUE-IN
        CALL "Encode-Float" USING VALUE-IN BUFFER BUFFERLEN
        IF BUFFER = X"00000000" AND BUFFERLEN = 4
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    NegativeZero.
        *> Note: I've found no way to differentiate +0.0 and -0.0 in COBOL, so they're encoded the same.
        DISPLAY "    Case: -0.0 - " WITH NO ADVANCING
        MOVE -0.0 TO VALUE-IN
        CALL "Encode-Float" USING VALUE-IN BUFFER BUFFERLEN
        IF BUFFER = X"00000000" AND BUFFERLEN = 4
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    PositiveInfinity.
        *> Note: FLOAT-INFINITY is not supported in GnuCOBOL.
        DISPLAY "    Case: +Infinity - " WITH NO ADVANCING
        DISPLAY "SKIP".
    NegativeInfinity.
        *> Note: FLOAT-INFINITY is not supported in GnuCOBOL.
        DISPLAY "    Case: -Infinity - " WITH NO ADVANCING
        DISPLAY "SKIP".
    NaN.
        *> Note: FLOAT-NOT-A-NUMBER is not supported in GnuCOBOL.
        DISPLAY "    Case: NaN - " WITH NO ADVANCING
        DISPLAY "SKIP".
    Positive1.
        DISPLAY "    Case: +1.0 - " WITH NO ADVANCING
        MOVE 1.0 TO VALUE-IN
        CALL "Encode-Float" USING VALUE-IN BUFFER BUFFERLEN
        IF BUFFER = X"3F800000" AND BUFFERLEN = 4
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    Negative1.
        DISPLAY "    Case: -1.0 - " WITH NO ADVANCING
        MOVE -1.0 TO VALUE-IN
        CALL "Encode-Float" USING VALUE-IN BUFFER BUFFERLEN
        IF BUFFER = X"BF800000" AND BUFFERLEN = 4
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    PositiveDecimal.
        DISPLAY "    Case: +12.125 - " WITH NO ADVANCING
        MOVE 12.125 TO VALUE-IN
        CALL "Encode-Float" USING VALUE-IN BUFFER BUFFERLEN
        IF BUFFER = X"41420000" AND BUFFERLEN = 4
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    NegativeDecimal.
        DISPLAY "    Case: -12.125 - " WITH NO ADVANCING
        MOVE -12.125 TO VALUE-IN
        CALL "Encode-Float" USING VALUE-IN BUFFER BUFFERLEN
        IF BUFFER = X"C1420000" AND BUFFERLEN = 4
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    PositiveMax.
        DISPLAY "    Case: +3.40282E+38 - " WITH NO ADVANCING
        MOVE 3.40282346638528859811704183484516925E+38 TO VALUE-IN
        CALL "Encode-Float" USING VALUE-IN BUFFER BUFFERLEN
        IF BUFFER = X"7F7FFFFF" AND BUFFERLEN = 4
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    NegativeMax.
        DISPLAY "    Case: -3.40282E+38 - " WITH NO ADVANCING
        MOVE -3.40282346638528859811704183484516925E+38 TO VALUE-IN
        CALL "Encode-Float" USING VALUE-IN BUFFER BUFFERLEN
        IF BUFFER = X"FF7FFFFF" AND BUFFERLEN = 4
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.

        GOBACK.

    END PROGRAM Test-Encode-Float.

END PROGRAM Test-Encode.
