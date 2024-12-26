*> --- Files-WriteAll ---
*> Given a file name and a buffer, write the specified amount of data from the buffer to the file.
*> The file will end up with the exact size specified. If the file is missing or empty, it will be created.
IDENTIFICATION DIVISION.
PROGRAM-ID. Files-WriteAll.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 FILE-HANDLE              PIC X(4) USAGE COMP-X.
    01 FILE-OFFSET              PIC X(8) USAGE COMP-X.
    01 FILE-LENGTH              PIC X(4) USAGE COMP-X.
LINKAGE SECTION.
    01 LK-FILENAME              PIC X ANY LENGTH.
    01 LK-BUFFER                PIC X ANY LENGTH.
    01 LK-WRITE-COUNT           BINARY-LONG UNSIGNED.
    01 LK-FAILURE               BINARY-CHAR UNSIGNED.

PROCEDURE DIVISION USING LK-FILENAME LK-BUFFER LK-WRITE-COUNT LK-FAILURE.
    MOVE 0 TO LK-FAILURE

    *> We use Micro Focus extensions here to ensure files are created with the correct size.
    *> Otherwise, we could only write in fixed-size chunks, so the file would usually have unwanted padding.

    CALL "CBL_CREATE_FILE" USING LK-FILENAME 2 0 0 FILE-HANDLE
    IF RETURN-CODE NOT = 0
        MOVE 1 TO LK-FAILURE
        GOBACK
    END-IF

    MOVE 0 TO FILE-OFFSET
    MOVE LK-WRITE-COUNT TO FILE-LENGTH
    CALL "CBL_WRITE_FILE" USING FILE-HANDLE FILE-OFFSET FILE-LENGTH 0 LK-BUFFER
    IF RETURN-CODE NOT = 0
        MOVE 1 TO LK-FAILURE
    END-IF

    CALL "CBL_CLOSE_FILE" USING FILE-HANDLE
    IF RETURN-CODE NOT = 0
        MOVE 1 TO LK-FAILURE
    END-IF

    GOBACK.

END PROGRAM Files-WriteAll.

*> --- Files-ReadAll ---
*> Given a file name, read as much of the file into the buffer as possible. If the file is missing or cannot be read,
*> an error will be returned.
IDENTIFICATION DIVISION.
PROGRAM-ID. Files-ReadAll.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 FILE-HANDLE              PIC X(4) USAGE COMP-X.
    01 FILE-OFFSET              PIC X(8) USAGE COMP-X.
    01 FILE-LENGTH              PIC X(4) USAGE COMP-X.
LINKAGE SECTION.
    01 LK-FILENAME              PIC X ANY LENGTH.
    01 LK-BUFFER                PIC X ANY LENGTH.
    01 LK-READ-COUNT            BINARY-LONG UNSIGNED.
    01 LK-FAILURE               BINARY-CHAR UNSIGNED.

PROCEDURE DIVISION USING LK-FILENAME LK-BUFFER LK-READ-COUNT LK-FAILURE.
    MOVE 0 TO LK-FAILURE

    CALL "CBL_OPEN_FILE" USING LK-FILENAME 1 0 0 FILE-HANDLE
    IF RETURN-CODE NOT = 0
        MOVE 1 TO LK-FAILURE
        GOBACK
    END-IF

    *> First, determine the length (flag=128), then read it (flag=0). GnuCOBOL 3.2 behaves according to this spec,
    *> however, GnuCOBOL 3.1 will happily read the file even with flag=128.
    *> See: https://github.com/meyfa/CobolCraft/issues/209

    MOVE 0 TO FILE-OFFSET
    MOVE 0 TO FILE-LENGTH
    CALL "CBL_READ_FILE" USING FILE-HANDLE FILE-OFFSET FILE-LENGTH 128 LK-BUFFER
    IF RETURN-CODE NOT = 0
        MOVE 1 TO LK-FAILURE
    END-IF
    MOVE FILE-OFFSET TO LK-READ-COUNT

    MOVE 0 TO FILE-OFFSET
    MOVE FUNCTION BYTE-LENGTH(LK-BUFFER) TO FILE-LENGTH
    CALL "CBL_READ_FILE" USING FILE-HANDLE FILE-OFFSET FILE-LENGTH 0 LK-BUFFER
    IF RETURN-CODE NOT = 0
        MOVE 1 TO LK-FAILURE
    END-IF

    CALL "CBL_CLOSE_FILE" USING FILE-HANDLE
    IF RETURN-CODE NOT = 0
        MOVE 1 TO LK-FAILURE
    END-IF

    GOBACK.

END PROGRAM Files-ReadAll.
