*> --- Test: strings.cob ---
IDENTIFICATION DIVISION.
PROGRAM-ID. Test-Strings.

PROCEDURE DIVISION.
    DISPLAY "Test: uuid.cob"
    CALL "Test-Strings-TrimFileExt"
    GOBACK.

    *> --- Test: TrimFileExt ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Test-Strings-TrimFileExt.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        01 BUFFER       PIC X(20).

    PROCEDURE DIVISION.
        DISPLAY "  Test: TrimFileExt".
    AllSpaces.
        DISPLAY "    Case: spaces - " WITH NO ADVANCING
        MOVE SPACES TO BUFFER
        CALL "TrimFileExt" USING BUFFER
        IF BUFFER = SPACES
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    AllDots.
        DISPLAY "    Case: dots - " WITH NO ADVANCING
        MOVE ALL "." TO BUFFER
        CALL "TrimFileExt" USING BUFFER
        IF BUFFER = SPACES
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    NoExt.
        DISPLAY "    Case: no ext - " WITH NO ADVANCING
        MOVE "foo/bar" TO BUFFER
        CALL "TrimFileExt" USING BUFFER
        IF BUFFER = "foo/bar"
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    Ext.
        DISPLAY "    Case: ext - " WITH NO ADVANCING
        MOVE "foo/bar.ext" TO BUFFER
        CALL "TrimFileExt" USING BUFFER
        IF BUFFER = "foo/bar"
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    Ext2.
        DISPLAY "    Case: ext2 - " WITH NO ADVANCING
        MOVE "foo/bar.tar.gz" TO BUFFER
        CALL "TrimFileExt" USING BUFFER
        IF BUFFER = "foo/bar"
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.

        GOBACK.

    END PROGRAM Test-Strings-TrimFileExt.

END PROGRAM Test-Strings.
