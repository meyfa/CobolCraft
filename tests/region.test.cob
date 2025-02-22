*> --- Test: region.cob ---
IDENTIFICATION DIVISION.
PROGRAM-ID. Test-Region.

PROCEDURE DIVISION.
    DISPLAY "Test: region.cob"
    CALL "Test-Region-RegionFileName"
    GOBACK.

    *> --- Test: Region-RegionFileName ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Test-Region-RegionFileName.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        COPY DD-SERVER-PROPERTIES.
        01 X-IN         BINARY-LONG.
        01 Z-IN         BINARY-LONG.
        01 RESULT       PIC X(255).

    PROCEDURE DIVISION.
        DISPLAY "  Test: Region-RegionFileName"
        MOVE "test-world-name" TO SP-LEVEL-NAME
        .
    AllZero.
        DISPLAY "    Case: 0 0 - " WITH NO ADVANCING
        MOVE 0 TO X-IN
        MOVE 0 TO Z-IN
        MOVE SPACES TO RESULT
        CALL "Region-RegionFileName" USING X-IN Z-IN RESULT
        IF RESULT = "test-world-name/region/r.0.0.mca"
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL, actual: " FUNCTION TRIM(RESULT)
        END-IF.
    NegativeX.
        DISPLAY "    Case: -1 0 - " WITH NO ADVANCING
        MOVE -1 TO X-IN
        MOVE 0 TO Z-IN
        MOVE SPACES TO RESULT
        CALL "Region-RegionFileName" USING X-IN Z-IN RESULT
        IF RESULT = "test-world-name/region/r.-1.0.mca"
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL, actual: " FUNCTION TRIM(RESULT)
        END-IF.
    NegativeZ.
        DISPLAY "    Case: 0 -1 - " WITH NO ADVANCING
        MOVE 0 TO X-IN
        MOVE -1 TO Z-IN
        MOVE SPACES TO RESULT
        CALL "Region-RegionFileName" USING X-IN Z-IN RESULT
        IF RESULT = "test-world-name/region/r.0.-1.mca"
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL, actual: " FUNCTION TRIM(RESULT)
        END-IF.
    LargerValues.
        DISPLAY "    Case: 1234 -5678 - " WITH NO ADVANCING
        MOVE 1234 TO X-IN
        MOVE -5678 TO Z-IN
        MOVE SPACES TO RESULT
        CALL "Region-RegionFileName" USING X-IN Z-IN RESULT
        IF RESULT = "test-world-name/region/r.1234.-5678.mca"
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL, actual: " FUNCTION TRIM(RESULT)
        END-IF.

        GOBACK.

    END PROGRAM Test-Region-RegionFileName.

END PROGRAM Test-Region.
