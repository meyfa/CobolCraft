*> --- Socket-Listen ---
IDENTIFICATION DIVISION.
PROGRAM-ID. Socket-Listen.

DATA DIVISION.
LINKAGE SECTION.
    01 LK-PORT              BINARY-SHORT UNSIGNED.
    01 LK-LISTEN            PIC X(4).
    01 LK-ERRNO             PIC 9(3).

PROCEDURE DIVISION USING LK-PORT LK-LISTEN LK-ERRNO.
    CALL "SocketListen" USING LK-PORT LK-LISTEN GIVING LK-ERRNO.

END PROGRAM Socket-Listen.

*> --- Socket-Close ---
IDENTIFICATION DIVISION.
PROGRAM-ID. Socket-Close.

DATA DIVISION.
LINKAGE SECTION.
    01 LK-HNDL              PIC X(4).
    01 LK-ERRNO             PIC 9(3).

PROCEDURE DIVISION USING LK-HNDL LK-ERRNO.
    CALL "SocketClose" USING LK-HNDL GIVING LK-ERRNO.

END PROGRAM Socket-Close.

*> --- Socket-Poll ---
*> Poll the server socket to retrieve a connection that wants to be accepted or send data.
*> Only connections immediately available are returned. If no connections are available, the handle will be zero.
IDENTIFICATION DIVISION.
PROGRAM-ID. Socket-Poll.

DATA DIVISION.
LINKAGE SECTION.
    01 LK-SERVER-HNDL       PIC X(4).
    01 LK-ERRNO             PIC 9(3).
    01 LK-CLIENT-HNDL       PIC X(4).

PROCEDURE DIVISION USING LK-SERVER-HNDL LK-ERRNO LK-CLIENT-HNDL.
    CALL "SocketPoll" USING LK-SERVER-HNDL LK-CLIENT-HNDL GIVING LK-ERRNO.

END PROGRAM Socket-Poll.

*> --- Socket-Read ---
*> Read a raw byte array from the socket. At most 64000 bytes can be read at once.
*> Only bytes that are immediately available are read, and the number is returned in LK-READ-COUNT.
IDENTIFICATION DIVISION.
PROGRAM-ID. Socket-Read.

DATA DIVISION.
LINKAGE SECTION.
    01 LK-HNDL              PIC X(4).
    01 LK-ERRNO             PIC 9(3).
    01 LK-READ-COUNT        BINARY-LONG UNSIGNED.
    01 LK-BUFFER            PIC X ANY LENGTH.

PROCEDURE DIVISION USING LK-HNDL LK-ERRNO LK-READ-COUNT LK-BUFFER.
    CALL "SocketRead" USING LK-HNDL LK-READ-COUNT LK-BUFFER GIVING LK-ERRNO.

END PROGRAM Socket-Read.

*> --- Socket-Write ---
*> Write a buffer to the client.
IDENTIFICATION DIVISION.
PROGRAM-ID. Socket-Write.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 CHUNK-BUFFER         PIC X(64000).
    01 BYTES-WRITTEN        BINARY-LONG UNSIGNED.
    01 REMAINING            BINARY-LONG UNSIGNED.
    01 CHUNK-SIZE           BINARY-LONG UNSIGNED.
LINKAGE SECTION.
    01 LK-HNDL              PIC X(4).
    01 LK-ERRNO             PIC 9(3).
    01 LK-WRITE-COUNT       BINARY-LONG UNSIGNED.
    01 LK-BUFFER            PIC X ANY LENGTH.

PROCEDURE DIVISION USING LK-HNDL LK-ERRNO LK-WRITE-COUNT LK-BUFFER.
    *> If the number of bytes to write is at most 64000, we can write it all at once
    IF LK-WRITE-COUNT <= 64000
        MOVE LK-WRITE-COUNT TO CHUNK-SIZE
        CALL "SocketWrite" USING LK-HNDL CHUNK-SIZE LK-BUFFER GIVING LK-ERRNO
        GOBACK
    END-IF

    *> Write in chunks of up to 64000 bytes
    MOVE 0 TO BYTES-WRITTEN
    PERFORM UNTIL BYTES-WRITTEN >= LK-WRITE-COUNT
        COMPUTE REMAINING = LK-WRITE-COUNT - BYTES-WRITTEN
        COMPUTE CHUNK-SIZE = FUNCTION MIN(64000, REMAINING)
        MOVE LK-BUFFER(BYTES-WRITTEN + 1:CHUNK-SIZE) TO CHUNK-BUFFER(1:CHUNK-SIZE)
        CALL "SocketWrite" USING LK-HNDL CHUNK-SIZE CHUNK-BUFFER GIVING LK-ERRNO
        IF LK-ERRNO NOT = 0
            EXIT PERFORM
        END-IF
        ADD CHUNK-SIZE TO BYTES-WRITTEN
    END-PERFORM

    GOBACK.

END PROGRAM Socket-Write.
