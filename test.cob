*> --- TestMain ---
IDENTIFICATION DIVISION.
PROGRAM-ID. TestMain.

PROCEDURE DIVISION.
    DISPLAY "Running tests..."
    CALL "Test-Util"
    CALL "Test-Decode"
    CALL "Test-Encode"
    CALL "Test-NbtEncode"
    CALL "Test-NbtDecode"
    CALL "Test-JsonParse"
    CALL "Test-World"
    STOP RUN.

END PROGRAM TestMain.
