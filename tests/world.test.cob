*> --- Test: world.cob ---
IDENTIFICATION DIVISION.
PROGRAM-ID. Test-World.

PROCEDURE DIVISION.
    DISPLAY "Test: world.cob"
    CALL "Test-World-ChunkFileName"
    GOBACK.

    *> --- Test: World-ChunkFileName ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Test-World-ChunkFileName.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        01 CHUNK-X      BINARY-LONG.
        01 CHUNK-Z      BINARY-LONG.
        01 RESULT       PIC X(255).

    PROCEDURE DIVISION.
        DISPLAY "  Test: World-ChunkFileName".
    AllZero.
        DISPLAY "    Case: 0 0 - " WITH NO ADVANCING
        MOVE 0 TO CHUNK-X
        MOVE 0 TO CHUNK-Z
        MOVE SPACES TO RESULT
        CALL "World-ChunkFileName" USING CHUNK-X CHUNK-Z RESULT
        IF RESULT = "save/overworld/chunk_0000000000_0000000000.dat"
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL, actual: " FUNCTION TRIM(RESULT)
        END-IF.
    NegativeX.
        DISPLAY "    Case: -1 0 - " WITH NO ADVANCING
        MOVE -1 TO CHUNK-X
        MOVE 0 TO CHUNK-Z
        MOVE SPACES TO RESULT
        CALL "World-ChunkFileName" USING CHUNK-X CHUNK-Z RESULT
        IF RESULT = "save/overworld/chunk_-0000000001_0000000000.dat"
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL, actual: " FUNCTION TRIM(RESULT)
        END-IF.
    NegativeZ.
        DISPLAY "    Case: 0 -1 - " WITH NO ADVANCING
        MOVE 0 TO CHUNK-X
        MOVE -1 TO CHUNK-Z
        MOVE SPACES TO RESULT
        CALL "World-ChunkFileName" USING CHUNK-X CHUNK-Z RESULT
        IF RESULT = "save/overworld/chunk_0000000000_-0000000001.dat"
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL, actual: " FUNCTION TRIM(RESULT)
        END-IF.
    LargerValues.
        DISPLAY "    Case: 1234 -5678 - " WITH NO ADVANCING
        MOVE 1234 TO CHUNK-X
        MOVE -5678 TO CHUNK-Z
        MOVE SPACES TO RESULT
        CALL "World-ChunkFileName" USING CHUNK-X CHUNK-Z RESULT
        IF RESULT = "save/overworld/chunk_0000001234_-0000005678.dat"
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL, actual: " FUNCTION TRIM(RESULT)
        END-IF.

        GOBACK.

    END PROGRAM Test-World-ChunkFileName.

END PROGRAM Test-World.
