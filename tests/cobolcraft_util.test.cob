*> --- Test: cobolcraft_util.cpp ---
IDENTIFICATION DIVISION.
PROGRAM-ID. Test-Util.

PROCEDURE DIVISION.
    DISPLAY "Test: cobolcraft_util.cpp"
    CALL "Test-LeadingZeros32"
    GOBACK.

    *> --- Test: Test-LeadingZeros32 ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Test-LeadingZeros32.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        01 VALUE-IN     BINARY-LONG UNSIGNED.
        01 RESULT       BINARY-LONG UNSIGNED.

    PROCEDURE DIVISION.
        DISPLAY "  Test: Test-LeadingZeros32".
    Int0.
        DISPLAY "    Case: 0 - " WITH NO ADVANCING
        MOVE 0 TO VALUE-IN
        CALL "LeadingZeros32" USING VALUE-IN RESULT
        IF RESULT = 32
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL, result: " RESULT
        END-IF.
    Int1.
        DISPLAY "    Case: 1 - " WITH NO ADVANCING
        MOVE 1 TO VALUE-IN
        CALL "LeadingZeros32" USING VALUE-IN RESULT
        IF RESULT = 31
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    Int2.
        DISPLAY "    Case: 2 - " WITH NO ADVANCING
        MOVE 2 TO VALUE-IN
        CALL "LeadingZeros32" USING VALUE-IN RESULT
        IF RESULT = 30
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    Int3.
        DISPLAY "    Case: 3 - " WITH NO ADVANCING
        MOVE 3 TO VALUE-IN
        CALL "LeadingZeros32" USING VALUE-IN RESULT
        IF RESULT = 30
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    IntMax.
        DISPLAY "    Case: 2^32-1 - " WITH NO ADVANCING
        MOVE 4294967295 TO VALUE-IN
        CALL "LeadingZeros32" USING VALUE-IN RESULT
        IF RESULT = 0
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.

        GOBACK.

    END PROGRAM Test-LeadingZeros32.

END PROGRAM Test-Util.
