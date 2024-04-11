*> --- TestMain ---
IDENTIFICATION DIVISION.
PROGRAM-ID. TestMain.

PROCEDURE DIVISION.
    DISPLAY "Running tests..."
    CALL "Test-Decode"
    CALL "Test-Encode"
    CALL "Test-JsonParse"
    STOP RUN.

END PROGRAM TestMain.
