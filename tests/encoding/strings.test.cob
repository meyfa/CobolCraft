*> --- Test: strings.cob ---
IDENTIFICATION DIVISION.
PROGRAM-ID. Test-Strings.

PROCEDURE DIVISION.
    COPY TEST-SUITE REPLACING ==NAME== BY =="encoding/strings.cob"==.
    CALL "Test-Strings-TrimFileExt"
    GOBACK.

    *> --- Test: TrimFileExt ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Test-Strings-TrimFileExt.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        01 BUFFER       PIC X(20).

    PROCEDURE DIVISION.
        COPY TEST-UNIT REPLACING ==NAME== BY =="TrimFileExt"==.
    AllSpaces.
        COPY TEST-CASE REPLACING ==NAME== BY =="spaces"==.
        MOVE SPACES TO BUFFER
        CALL "TrimFileExt" USING BUFFER
        COPY TEST-ASSERT REPLACING COND BY ==BUFFER = SPACES==.
    AllDots.
        COPY TEST-CASE REPLACING ==NAME== BY =="dots"==.
        MOVE ALL "." TO BUFFER
        CALL "TrimFileExt" USING BUFFER
        COPY TEST-ASSERT REPLACING COND BY ==BUFFER = SPACES==.
    NoExt.
        COPY TEST-CASE REPLACING ==NAME== BY =="no ext"==.
        MOVE "foo/bar" TO BUFFER
        CALL "TrimFileExt" USING BUFFER
        COPY TEST-ASSERT REPLACING COND BY ==BUFFER = "foo/bar"==.
    Ext.
        COPY TEST-CASE REPLACING ==NAME== BY =="ext"==.
        MOVE "foo/bar.ext" TO BUFFER
        CALL "TrimFileExt" USING BUFFER
        COPY TEST-ASSERT REPLACING COND BY ==BUFFER = "foo/bar"==.
    Ext2.
        COPY TEST-CASE REPLACING ==NAME== BY =="ext2"==.
        MOVE "foo/bar.tar.gz" TO BUFFER
        CALL "TrimFileExt" USING BUFFER
        COPY TEST-ASSERT REPLACING COND BY ==BUFFER = "foo/bar"==.

        GOBACK.

    END PROGRAM Test-Strings-TrimFileExt.

END PROGRAM Test-Strings.
