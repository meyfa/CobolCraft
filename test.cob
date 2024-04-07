*> --- TestMain ---
IDENTIFICATION DIVISION.
PROGRAM-ID. TestMain.

PROCEDURE DIVISION.
    DISPLAY "Running tests..."
    CALL "Test-Decode"
    CALL "Test-Encode"
    STOP RUN.

END PROGRAM TestMain.
