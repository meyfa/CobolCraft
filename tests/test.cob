*> --- TestMain ---
IDENTIFICATION DIVISION.
PROGRAM-ID. TestMain.

PROCEDURE DIVISION.
    DISPLAY "Running tests..."
    CALL "Test-CPP"
    CALL "Test-Strings"
    CALL "Test-UUID"
    CALL "Test-Decode"
    CALL "Test-Encode"
    CALL "Test-NbtEncode"
    CALL "Test-NbtDecode"
    CALL "Test-JsonParse"
    CALL "Test-JsonEncode"
    CALL "Test-Region"
    CALL "Test-ServerProperties"
    STOP RUN.

END PROGRAM TestMain.
