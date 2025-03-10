*> --- Region-Init ---
IDENTIFICATION DIVISION.
PROGRAM-ID. Region-Init.

DATA DIVISION.
WORKING-STORAGE SECTION.
    COPY DD-REGION-FILES.

PROCEDURE DIVISION.
    MOVE 0 TO REGION-FILE-COUNT
    GOBACK.

END PROGRAM Region-Init.

*> --- Region-RegionFileName ---
IDENTIFICATION DIVISION.
PROGRAM-ID. Region-RegionFileName.

DATA DIVISION.
WORKING-STORAGE SECTION.
    COPY DD-REGION-FILES.
    COPY DD-SERVER-PROPERTIES.
    01 DISPLAY-X                PIC -(9)9.
    01 DISPLAY-Z                PIC -(9)9.
LINKAGE SECTION.
    *> Either FILE-TYPE-REGION or FILE-TYPE-ENTITY
    01 LK-TYPE                  BINARY-CHAR UNSIGNED.
    *> Region coordinates = (chunk coordinates / 32) = (block coordinates / 512)
    01 LK-X                     BINARY-LONG.
    01 LK-Z                     BINARY-LONG.
    *> Return value
    01 LK-FILE-NAME             PIC X ANY LENGTH.

PROCEDURE DIVISION USING LK-TYPE LK-X LK-Z LK-FILE-NAME.
    MOVE LK-X TO DISPLAY-X
    MOVE LK-Z TO DISPLAY-Z
    INITIALIZE LK-FILE-NAME
    IF LK-TYPE = FILE-TYPE-ENTITY
        STRING FUNCTION TRIM(SP-LEVEL-NAME) "/entities/r." FUNCTION TRIM(DISPLAY-X) "." FUNCTION TRIM(DISPLAY-Z) ".mca" INTO LK-FILE-NAME
    ELSE
        STRING FUNCTION TRIM(SP-LEVEL-NAME) "/region/r." FUNCTION TRIM(DISPLAY-X) "." FUNCTION TRIM(DISPLAY-Z) ".mca" INTO LK-FILE-NAME
    END-IF
    GOBACK.

END PROGRAM Region-RegionFileName.

*> --- Region-Open ---
IDENTIFICATION DIVISION.
PROGRAM-ID. Region-Open.

DATA DIVISION.
WORKING-STORAGE SECTION.
    COPY DD-REGION-FILES.
    COPY DD-REGION-FILE-REF.
    01 REGION-INDEX             BINARY-LONG UNSIGNED.
    01 SECTOR-INDEX             BINARY-LONG UNSIGNED.
    01 SECTOR-BUFFER            PIC X(REGION-SECTOR-BYTES).
    01 FILE-OFFSET              BINARY-LONG UNSIGNED.
    01 FILE-LENGTH              BINARY-LONG UNSIGNED.
    01 CHUNK-SECTOR-LOCATION    BINARY-LONG UNSIGNED.
    01 CHUNK-SECTOR-OFFSET      BINARY-LONG UNSIGNED.
    01 CHUNK-SECTOR-LENGTH      BINARY-LONG UNSIGNED.
    01 OFFSET                   BINARY-LONG UNSIGNED.
LINKAGE SECTION.
    *> Either FILE-TYPE-REGION or FILE-TYPE-ENTITY
    01 LK-TYPE                  BINARY-CHAR UNSIGNED.
    01 LK-REGION-X              BINARY-LONG.
    01 LK-REGION-Z              BINARY-LONG.
    01 LK-ALLOW-CREATE          BINARY-CHAR UNSIGNED.
    01 LK-FAILURE               BINARY-CHAR UNSIGNED.
    01 LK-REGION-INDEX          BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-TYPE LK-REGION-X LK-REGION-Z LK-ALLOW-CREATE LK-FAILURE LK-REGION-INDEX.
    MOVE 0 TO LK-FAILURE
    MOVE 0 TO LK-REGION-INDEX

    *> Check if already open
    PERFORM VARYING REGION-INDEX FROM 1 BY 1 UNTIL REGION-INDEX > REGION-FILE-COUNT
        IF REGION-FILE-TYPE(REGION-INDEX) = LK-TYPE
            SET ADDRESS OF REGION-FILE TO REGION-FILE-POINTER(REGION-INDEX)

            IF REGION-FILE-X = LK-REGION-X AND REGION-FILE-Z = LK-REGION-Z
                MOVE REGION-INDEX TO LK-REGION-INDEX
                *> Update last used time
                CALL "SystemTimeMillis" USING REGION-FILE-ACCESS-TIME
                GOBACK
            END-IF
        END-IF
    END-PERFORM

    *> If no open slots, close the least recently used
    IF REGION-FILE-COUNT = MAX-REGION-FILES
        CALL "Region-CloseLeastRecentlyUsed" USING LK-FAILURE
        IF LK-FAILURE NOT = 0
            GOBACK
        END-IF
    END-IF

    *> At this point, we are guaranteed to have capacity for a new file at the end
    ADD 1 TO REGION-FILE-COUNT
    MOVE REGION-FILE-COUNT TO LK-REGION-INDEX

    ALLOCATE REGION-FILE
    SET REGION-FILE-POINTER(LK-REGION-INDEX) TO ADDRESS OF REGION-FILE
    MOVE LK-TYPE TO REGION-FILE-TYPE(LK-REGION-INDEX)

    INITIALIZE REGION-FILE
    MOVE LK-REGION-X TO REGION-FILE-X
    MOVE LK-REGION-Z TO REGION-FILE-Z
    CALL "SystemTimeMillis" USING REGION-FILE-ACCESS-TIME
    MOVE 1 TO REGION-FILE-USED(1)
    MOVE 1 TO REGION-FILE-USED(2)

    PERFORM OpenFile

    IF LK-FAILURE NOT = 0
        SUBTRACT 1 FROM REGION-FILE-COUNT
        FREE REGION-FILE
        GOBACK
    END-IF

    GOBACK.

OpenFile.
    CALL "Region-RegionFileName" USING LK-TYPE LK-REGION-X LK-REGION-Z REGION-FILE-NAME

    CALL "CBL_OPEN_FILE" USING REGION-FILE-NAME 3 0 0 REGION-FILE-HANDLE
    IF RETURN-CODE NOT = 0 AND RETURN-CODE NOT = 35
        MOVE 1 TO LK-FAILURE
        EXIT PARAGRAPH
    END-IF

    IF RETURN-CODE = 35
        *> A return code of 35 means that the file does not exist, so create it if allowed
        IF LK-ALLOW-CREATE = 0
            MOVE 1 TO LK-FAILURE
            EXIT PARAGRAPH
        END-IF

        CALL "CBL_CREATE_FILE" USING REGION-FILE-NAME 2 0 0 REGION-FILE-HANDLE
        IF RETURN-CODE NOT = 0
            MOVE 1 TO LK-FAILURE
            EXIT PARAGRAPH
        END-IF

        *> Write an empty header (offset section, timestamp section)
        MOVE ALL X"00" TO REGION-FILE-OFFSET-BYTES
        MOVE REGION-FILE-OFFSET-BYTES TO SECTOR-BUFFER
        MOVE 0 TO FILE-OFFSET
        MOVE REGION-SECTOR-BYTES TO FILE-LENGTH
        PERFORM 2 TIMES
            CALL "Region-WriteToHandle" USING REGION-FILE-HANDLE FILE-OFFSET FILE-LENGTH SECTOR-BUFFER LK-FAILURE
            IF LK-FAILURE NOT = 0
                CALL "CBL_CLOSE_FILE" USING REGION-FILE-HANDLE
                EXIT PARAGRAPH
            END-IF
            ADD REGION-SECTOR-BYTES TO FILE-OFFSET
        END-PERFORM

        *> Close the file so we can reopen it for read/write
        CALL "CBL_CLOSE_FILE" USING REGION-FILE-HANDLE
        IF RETURN-CODE NOT = 0
            MOVE 1 TO LK-FAILURE
            EXIT PARAGRAPH
        END-IF
        CALL "CBL_OPEN_FILE" USING REGION-FILE-NAME 3 0 0 REGION-FILE-HANDLE
        IF RETURN-CODE NOT = 0
            MOVE 1 TO LK-FAILURE
            EXIT PARAGRAPH
        END-IF
    ELSE
        *> The file exists, so read the header
        MOVE 0 TO FILE-OFFSET
        MOVE REGION-SECTOR-BYTES TO FILE-LENGTH
        CALL "Region-ReadFromHandle" USING REGION-FILE-HANDLE FILE-OFFSET FILE-LENGTH REGION-FILE-OFFSET-BYTES LK-FAILURE
        IF LK-FAILURE NOT = 0
            CALL "CBL_CLOSE_FILE" USING REGION-FILE-HANDLE
            EXIT PARAGRAPH
        END-IF

        *> Mark all used sectors.
        MOVE 1 TO OFFSET
        PERFORM REGION-SECTOR-INTS TIMES
            CALL "Decode-UnsignedInt" USING REGION-FILE-OFFSET-BYTES OFFSET CHUNK-SECTOR-LOCATION
            >>IF GCVERSION >= 32
                COMPUTE CHUNK-SECTOR-LENGTH = CHUNK-SECTOR-LOCATION B-AND H'FF'
                COMPUTE CHUNK-SECTOR-OFFSET = CHUNK-SECTOR-LOCATION B-SHIFT-R 8
            >>ELSE
                DIVIDE CHUNK-SECTOR-LOCATION BY 256 GIVING CHUNK-SECTOR-OFFSET REMAINDER CHUNK-SECTOR-LENGTH
            >>END-IF
            IF CHUNK-SECTOR-LENGTH > 0
                COMPUTE SECTOR-INDEX = CHUNK-SECTOR-OFFSET + 1
                PERFORM CHUNK-SECTOR-LENGTH TIMES
                    MOVE 1 TO REGION-FILE-USED(SECTOR-INDEX)
                    ADD 1 TO SECTOR-INDEX
                END-PERFORM
            END-IF
        END-PERFORM
    END-IF
    .

END PROGRAM Region-Open.

*> --- Region-Close ---
IDENTIFICATION DIVISION.
PROGRAM-ID. Region-Close.

DATA DIVISION.
WORKING-STORAGE SECTION.
    COPY DD-REGION-FILES.
    COPY DD-REGION-FILE-REF.
LINKAGE SECTION.
    01 LK-REGION-INDEX          BINARY-LONG UNSIGNED.
    01 LK-FAILURE               BINARY-CHAR UNSIGNED.

PROCEDURE DIVISION USING LK-REGION-INDEX LK-FAILURE.
    MOVE 0 TO LK-FAILURE

    SET ADDRESS OF REGION-FILE TO REGION-FILE-POINTER(LK-REGION-INDEX)

    CALL "CBL_CLOSE_FILE" USING REGION-FILE-HANDLE
    IF RETURN-CODE NOT = 0
        MOVE 1 TO LK-FAILURE
        GOBACK
    END-IF

    FREE REGION-FILE

    MOVE REGION-FILE-POINTER(REGION-FILE-COUNT) TO REGION-FILE-POINTER(LK-REGION-INDEX)
    SUBTRACT 1 FROM REGION-FILE-COUNT

    GOBACK.

END PROGRAM Region-Close.

*> --- Region-CloseAll ---
IDENTIFICATION DIVISION.
PROGRAM-ID. Region-CloseAll.

DATA DIVISION.
WORKING-STORAGE SECTION.
    COPY DD-REGION-FILES.
    01 REGION-INDEX             BINARY-LONG UNSIGNED.
LINKAGE SECTION.
    01 LK-FAILURE               BINARY-CHAR UNSIGNED.

PROCEDURE DIVISION USING LK-FAILURE.
    PERFORM UNTIL REGION-FILE-COUNT <= 0
        MOVE REGION-FILE-COUNT TO REGION-INDEX
        CALL "Region-Close" USING REGION-INDEX LK-FAILURE
        IF LK-FAILURE NOT = 0
            GOBACK
        END-IF
    END-PERFORM
    GOBACK.

END PROGRAM Region-CloseAll.

*> --- Region-CloseLeastRecentlyUsed ---
IDENTIFICATION DIVISION.
PROGRAM-ID. Region-CloseLeastRecentlyUsed.

DATA DIVISION.
WORKING-STORAGE SECTION.
    COPY DD-REGION-FILES.
    COPY DD-REGION-FILE-REF.
    01 REGION-INDEX             BINARY-LONG UNSIGNED.
    01 LRU-INDEX                BINARY-LONG UNSIGNED.
    01 LRU-ACCESS-TIME          BINARY-LONG-LONG.
LINKAGE SECTION.
    01 LK-FAILURE               BINARY-CHAR UNSIGNED.

PROCEDURE DIVISION USING LK-FAILURE.
    MOVE 0 TO LK-FAILURE
    MOVE 0 TO LRU-INDEX

    PERFORM VARYING REGION-INDEX FROM 1 BY 1 UNTIL REGION-INDEX > REGION-FILE-COUNT
        SET ADDRESS OF REGION-FILE TO REGION-FILE-POINTER(REGION-INDEX)

        IF LRU-INDEX = 0 OR REGION-FILE-ACCESS-TIME < LRU-ACCESS-TIME
            MOVE REGION-INDEX TO LRU-INDEX
            MOVE REGION-FILE-ACCESS-TIME TO LRU-ACCESS-TIME
        END-IF
    END-PERFORM

    IF LRU-INDEX = 0
        MOVE 1 TO LK-FAILURE
        GOBACK
    END-IF

    CALL "Region-Close" USING LRU-INDEX LK-FAILURE

    GOBACK.

END PROGRAM Region-CloseLeastRecentlyUsed.

*> --- Region-ReadFromHandle ---
IDENTIFICATION DIVISION.
PROGRAM-ID. Region-ReadFromHandle.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 IO-OFFSET                PIC X(8) USAGE COMP-X.
    01 IO-LENGTH                PIC X(4) USAGE COMP-X.
LINKAGE SECTION.
    01 LK-HANDLE                PIC X(4) USAGE COMP-X.
    01 LK-OFFSET                BINARY-LONG UNSIGNED.
    01 LK-LENGTH                BINARY-LONG UNSIGNED.
    01 LK-BUFFER                PIC X ANY LENGTH.
    01 LK-FAILURE               BINARY-CHAR UNSIGNED.

PROCEDURE DIVISION USING LK-HANDLE LK-OFFSET LK-LENGTH LK-BUFFER LK-FAILURE.
    MOVE 0 TO LK-FAILURE
    MOVE LK-OFFSET TO IO-OFFSET
    MOVE LK-LENGTH TO IO-LENGTH
    CALL "CBL_READ_FILE" USING LK-HANDLE IO-OFFSET IO-LENGTH 0 LK-BUFFER
    IF RETURN-CODE NOT = 0
        MOVE 1 TO LK-FAILURE
    END-IF
    GOBACK.

END PROGRAM Region-ReadFromHandle.

*> --- Region-WriteToHandle ---
IDENTIFICATION DIVISION.
PROGRAM-ID. Region-WriteToHandle.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 IO-OFFSET                PIC X(8) USAGE COMP-X.
    01 IO-LENGTH                PIC X(4) USAGE COMP-X.
LINKAGE SECTION.
    01 LK-HANDLE                PIC X(4) USAGE COMP-X.
    01 LK-OFFSET                BINARY-LONG UNSIGNED.
    01 LK-LENGTH                BINARY-LONG UNSIGNED.
    01 LK-BUFFER                PIC X ANY LENGTH.
    01 LK-FAILURE               BINARY-CHAR UNSIGNED.

PROCEDURE DIVISION USING LK-HANDLE LK-OFFSET LK-LENGTH LK-BUFFER LK-FAILURE.
    MOVE 0 TO LK-FAILURE
    MOVE LK-OFFSET TO IO-OFFSET
    MOVE LK-LENGTH TO IO-LENGTH
    CALL "CBL_WRITE_FILE" USING LK-HANDLE IO-OFFSET IO-LENGTH 0 LK-BUFFER
    IF RETURN-CODE NOT = 0
        MOVE 1 TO LK-FAILURE
    END-IF
    GOBACK.

END PROGRAM Region-WriteToHandle.

*> --- Region-ReadChunkData ---
IDENTIFICATION DIVISION.
PROGRAM-ID. Region-ReadChunkData.

DATA DIVISION.
WORKING-STORAGE SECTION.
    COPY DD-REGION-FILES.
    COPY DD-REGION-FILE-REF.
    01 CREATE-REGION-ON-READ    BINARY-CHAR UNSIGNED        VALUE 0.
    01 REGION-X                 BINARY-LONG.
    01 REGION-Z                 BINARY-LONG.
    01 REGION-INDEX             BINARY-LONG UNSIGNED.
    01 LOCATION-INDEX           BINARY-LONG UNSIGNED.
    01 CHUNK-SECTOR-LOCATION    BINARY-LONG UNSIGNED.
    01 CHUNK-SECTOR-OFFSET      BINARY-LONG UNSIGNED.
    01 CHUNK-SECTOR-LENGTH      BINARY-LONG UNSIGNED.
    01 OFFSET                   BINARY-LONG UNSIGNED.
    01 FILE-OFFSET              BINARY-LONG UNSIGNED.
    01 FILE-LENGTH              BINARY-LONG UNSIGNED.
    01 CHUNK-HEADER             PIC X(5).
    01 CHUNK-COMPRESSION        BINARY-CHAR UNSIGNED.
    *> This is marked EXTERNAL so it can be used by both reading and writing programs.
    01 CHUNK-COMPRESSION-BUFFER PIC X(1048576) EXTERNAL.
    01 CHUNK-COMPRESSED-LENGTH  BINARY-LONG UNSIGNED.
LINKAGE SECTION.
    *> The kind of data to read (FILE-TYPE-REGION or FILE-TYPE-ENTITY)
    01 LK-TYPE                  BINARY-CHAR UNSIGNED.
    01 LK-CHUNK-X               BINARY-LONG.
    01 LK-CHUNK-Z               BINARY-LONG.
    01 LK-DATA                  PIC X ANY LENGTH.
    01 LK-DATA-LENGTH           BINARY-LONG UNSIGNED.
    01 LK-FAILURE               BINARY-CHAR UNSIGNED.

PROCEDURE DIVISION USING LK-TYPE LK-CHUNK-X LK-CHUNK-Z LK-DATA LK-DATA-LENGTH LK-FAILURE.
    MOVE 0 TO LK-FAILURE

    DIVIDE LK-CHUNK-X BY REGION-CHUNK-WIDTH GIVING REGION-X ROUNDED MODE IS TOWARD-LESSER
    DIVIDE LK-CHUNK-Z BY REGION-CHUNK-WIDTH GIVING REGION-Z ROUNDED MODE IS TOWARD-LESSER

    CALL "Region-Open" USING LK-TYPE REGION-X REGION-Z CREATE-REGION-ON-READ LK-FAILURE REGION-INDEX
    IF LK-FAILURE NOT = 0
        GOBACK
    END-IF

    SET ADDRESS OF REGION-FILE TO REGION-FILE-POINTER(REGION-INDEX)

    *> Retrieve the chunk's location information
    COMPUTE LOCATION-INDEX = FUNCTION MOD(LK-CHUNK-X, REGION-CHUNK-WIDTH) + FUNCTION MOD(LK-CHUNK-Z, REGION-CHUNK-WIDTH) * REGION-CHUNK-WIDTH + 1
    MOVE 1 TO OFFSET
    CALL "Decode-UnsignedInt" USING REGION-FILE-OFFSET(LOCATION-INDEX) OFFSET CHUNK-SECTOR-LOCATION
    >>IF GCVERSION >= 32
        COMPUTE CHUNK-SECTOR-LENGTH = CHUNK-SECTOR-LOCATION B-AND H'FF'
        COMPUTE CHUNK-SECTOR-OFFSET = CHUNK-SECTOR-LOCATION B-SHIFT-R 8
    >>ELSE
        DIVIDE CHUNK-SECTOR-LOCATION BY 256 GIVING CHUNK-SECTOR-OFFSET REMAINDER CHUNK-SECTOR-LENGTH
    >>END-IF

    *> Check if the chunk exists
    IF CHUNK-SECTOR-OFFSET = 0 OR CHUNK-SECTOR-LENGTH = 0
        MOVE 1 TO LK-FAILURE
        GOBACK
    END-IF

    *> Read the chunk header (5 bytes). Its structure is as follows:
    *> 4 byte signed integer: length of chunk data in bytes (length itself not included, but the compression type is)
    *> 1 byte: chunk version / compression type (1 = gzip, 2 = zlib, 0 = uncompressed)
    COMPUTE FILE-OFFSET = CHUNK-SECTOR-OFFSET * REGION-SECTOR-BYTES
    MOVE 5 TO FILE-LENGTH
    CALL "Region-ReadFromHandle" USING REGION-FILE-HANDLE FILE-OFFSET FILE-LENGTH CHUNK-HEADER LK-FAILURE
    IF LK-FAILURE NOT = 0
        GOBACK
    END-IF

    MOVE 1 TO OFFSET
    CALL "Decode-Int" USING CHUNK-HEADER OFFSET LK-DATA-LENGTH
    SUBTRACT 1 FROM LK-DATA-LENGTH
    CALL "Decode-Byte" USING CHUNK-HEADER OFFSET CHUNK-COMPRESSION

    EVALUATE CHUNK-COMPRESSION
        *> gzip compression
        WHEN 1
            COMPUTE FILE-OFFSET = CHUNK-SECTOR-OFFSET * REGION-SECTOR-BYTES + 5
            MOVE LK-DATA-LENGTH TO FILE-LENGTH
            CALL "Region-ReadFromHandle" USING REGION-FILE-HANDLE FILE-OFFSET FILE-LENGTH CHUNK-COMPRESSION-BUFFER LK-FAILURE
            IF LK-FAILURE NOT = 0
                GOBACK
            END-IF
            MOVE LK-DATA-LENGTH TO CHUNK-COMPRESSED-LENGTH
            MOVE FUNCTION LENGTH(LK-DATA) TO LK-DATA-LENGTH
            CALL "GzipDecompress" USING CHUNK-COMPRESSION-BUFFER CHUNK-COMPRESSED-LENGTH LK-DATA LK-DATA-LENGTH LK-FAILURE
            IF LK-FAILURE NOT = 0
                GOBACK
            END-IF

        *> zlib compression
        WHEN 2
            COMPUTE FILE-OFFSET = CHUNK-SECTOR-OFFSET * REGION-SECTOR-BYTES + 5
            MOVE LK-DATA-LENGTH TO FILE-LENGTH
            CALL "Region-ReadFromHandle" USING REGION-FILE-HANDLE FILE-OFFSET FILE-LENGTH CHUNK-COMPRESSION-BUFFER LK-FAILURE
            IF LK-FAILURE NOT = 0
                GOBACK
            END-IF
            MOVE LK-DATA-LENGTH TO CHUNK-COMPRESSED-LENGTH
            MOVE FUNCTION LENGTH(LK-DATA) TO LK-DATA-LENGTH
            CALL "ZlibDecompress" USING CHUNK-COMPRESSION-BUFFER CHUNK-COMPRESSED-LENGTH LK-DATA LK-DATA-LENGTH LK-FAILURE
            IF LK-FAILURE NOT = 0
                GOBACK
            END-IF

        *> no compression
        WHEN 3
            COMPUTE FILE-OFFSET = CHUNK-SECTOR-OFFSET * REGION-SECTOR-BYTES + 5
            MOVE LK-DATA-LENGTH TO FILE-LENGTH
            CALL "Region-ReadFromHandle" USING REGION-FILE-HANDLE FILE-OFFSET FILE-LENGTH LK-DATA LK-FAILURE

        *> unknown version
        WHEN OTHER
            DISPLAY "Error: Unknown chunk compression: " CHUNK-COMPRESSION
            MOVE 1 TO LK-FAILURE
    END-EVALUATE

    GOBACK.

END PROGRAM Region-ReadChunkData.

*> --- Region-WriteChunkData ---
IDENTIFICATION DIVISION.
PROGRAM-ID. Region-WriteChunkData.

DATA DIVISION.
WORKING-STORAGE SECTION.
    COPY DD-REGION-FILES.
    COPY DD-REGION-FILE-REF.
    01 CREATE-REGION-ON-WRITE   BINARY-CHAR UNSIGNED        VALUE 1.
    01 REGION-X                 BINARY-LONG.
    01 REGION-Z                 BINARY-LONG.
    01 REGION-INDEX             BINARY-LONG UNSIGNED.
    01 LOCATION-INDEX           BINARY-LONG UNSIGNED.
    01 SECTOR-INDEX             BINARY-LONG UNSIGNED.
    01 RUN-START                BINARY-LONG UNSIGNED.
    01 RUN-LENGTH               BINARY-LONG UNSIGNED.
    01 CHUNK-DATA-LENGTH        BINARY-LONG UNSIGNED.
    01 CHUNK-PAYLOAD-LENGTH     BINARY-LONG UNSIGNED.
    01 REQUIRED-SECTOR-COUNT    BINARY-LONG UNSIGNED.
    01 CHUNK-SECTOR-LOCATION    BINARY-LONG UNSIGNED.
    01 CHUNK-SECTOR-OFFSET      BINARY-LONG UNSIGNED.
    01 CHUNK-SECTOR-LENGTH      BINARY-LONG UNSIGNED.
    01 OFFSET                   BINARY-LONG UNSIGNED.
    01 FILE-OFFSET              BINARY-LONG UNSIGNED.
    01 FILE-LENGTH              BINARY-LONG UNSIGNED.
    01 CHUNK-HEADER             PIC X(5).
    01 CHUNK-COMPRESSION        BINARY-CHAR UNSIGNED.
    *> This is marked EXTERNAL so it can be used by both reading and writing programs.
    01 CHUNK-COMPRESSION-BUFFER PIC X(1048576) EXTERNAL.
    01 CHUNK-COMPRESSED-LENGTH  BINARY-LONG UNSIGNED.
LINKAGE SECTION.
    *> The kind of data to write (FILE-TYPE-REGION or FILE-TYPE-ENTITY)
    01 LK-TYPE                  BINARY-CHAR UNSIGNED.
    01 LK-CHUNK-X               BINARY-LONG.
    01 LK-CHUNK-Z               BINARY-LONG.
    01 LK-DATA                  PIC X ANY LENGTH.
    01 LK-DATA-LENGTH           BINARY-LONG UNSIGNED.
    01 LK-FAILURE               BINARY-CHAR UNSIGNED.

PROCEDURE DIVISION USING LK-TYPE LK-CHUNK-X LK-CHUNK-Z LK-DATA LK-DATA-LENGTH LK-FAILURE.
    MOVE 0 TO LK-FAILURE

    DIVIDE LK-CHUNK-X BY REGION-CHUNK-WIDTH GIVING REGION-X ROUNDED MODE IS TOWARD-LESSER
    DIVIDE LK-CHUNK-Z BY REGION-CHUNK-WIDTH GIVING REGION-Z ROUNDED MODE IS TOWARD-LESSER
    CALL "Region-Open" USING LK-TYPE REGION-X REGION-Z CREATE-REGION-ON-WRITE LK-FAILURE REGION-INDEX
    IF LK-FAILURE NOT = 0
        GOBACK
    END-IF

    SET ADDRESS OF REGION-FILE TO REGION-FILE-POINTER(REGION-INDEX)

    *> Compress the chunk data (zlib compression = type 2)
    MOVE 2 TO CHUNK-COMPRESSION
    MOVE LENGTH OF CHUNK-COMPRESSION-BUFFER TO CHUNK-COMPRESSED-LENGTH
    CALL "ZlibCompress" USING LK-DATA LK-DATA-LENGTH CHUNK-COMPRESSION-BUFFER CHUNK-COMPRESSED-LENGTH GIVING LK-FAILURE
    IF LK-FAILURE NOT = 0
        GOBACK
    END-IF

    *> Retrieve the chunk's location information
    COMPUTE LOCATION-INDEX = FUNCTION MOD(LK-CHUNK-X, REGION-CHUNK-WIDTH) + FUNCTION MOD(LK-CHUNK-Z, REGION-CHUNK-WIDTH) * REGION-CHUNK-WIDTH + 1
    MOVE 1 TO OFFSET
    CALL "Decode-UnsignedInt" USING REGION-FILE-OFFSET(LOCATION-INDEX) OFFSET CHUNK-SECTOR-LOCATION
    >>IF GCVERSION >= 32
        COMPUTE CHUNK-SECTOR-LENGTH = CHUNK-SECTOR-LOCATION B-AND H'FF'
        COMPUTE CHUNK-SECTOR-OFFSET = CHUNK-SECTOR-LOCATION B-SHIFT-R 8
    >>ELSE
        DIVIDE CHUNK-SECTOR-LOCATION BY 256 GIVING CHUNK-SECTOR-OFFSET REMAINDER CHUNK-SECTOR-LENGTH
    >>END-IF

    *> If the chunk exists and is the correct size, we can write to it directly. Otherwise, we need to find a new
    *> location for the chunk.
    COMPUTE CHUNK-PAYLOAD-LENGTH = CHUNK-COMPRESSED-LENGTH + 5
    DIVIDE CHUNK-PAYLOAD-LENGTH BY REGION-SECTOR-BYTES GIVING REQUIRED-SECTOR-COUNT ROUNDED MODE IS TOWARD-GREATER
    IF REQUIRED-SECTOR-COUNT > 255
        DISPLAY "Error: Chunk too large."
        MOVE 1 TO LK-FAILURE
        GOBACK
    END-IF

    *> The chunk exists and is the correct size
    IF CHUNK-SECTOR-LENGTH > 0 AND CHUNK-SECTOR-LENGTH NOT = REQUIRED-SECTOR-COUNT
        *> Free the previous location
        MOVE X"00000000" TO REGION-FILE-OFFSET(LOCATION-INDEX)
        COMPUTE SECTOR-INDEX = CHUNK-SECTOR-OFFSET + 1
        PERFORM CHUNK-SECTOR-LENGTH TIMES
            MOVE 0 TO REGION-FILE-USED(SECTOR-INDEX)
            ADD 1 TO SECTOR-INDEX
        END-PERFORM
        MOVE 0 TO CHUNK-SECTOR-LOCATION
    END-IF

    *> The chunk does not exist or was the wrong size
    IF CHUNK-SECTOR-LOCATION = 0
        *> Find a run of free sectors
        MOVE 0 TO RUN-LENGTH
        MOVE 0 TO RUN-START
        PERFORM VARYING SECTOR-INDEX FROM 1 BY 1 UNTIL SECTOR-INDEX > REGION-SECTOR-COUNT
            IF REGION-FILE-USED(SECTOR-INDEX) = 0
                ADD 1 TO RUN-LENGTH
                IF RUN-LENGTH = 1
                    MOVE SECTOR-INDEX TO RUN-START
                END-IF
                IF RUN-LENGTH >= REQUIRED-SECTOR-COUNT
                    EXIT PERFORM
                END-IF
            ELSE
                MOVE 0 TO RUN-LENGTH
                MOVE 0 TO RUN-START
            END-IF
        END-PERFORM

        IF RUN-LENGTH < REQUIRED-SECTOR-COUNT
            MOVE 1 TO LK-FAILURE
            GOBACK
        END-IF

        *> Mark the sectors as used
        MOVE RUN-START TO SECTOR-INDEX
        PERFORM RUN-LENGTH TIMES
            MOVE 1 TO REGION-FILE-USED(SECTOR-INDEX)
            ADD 1 TO SECTOR-INDEX
        END-PERFORM

        *> Update the location information
        COMPUTE CHUNK-SECTOR-OFFSET = RUN-START - 1
        MOVE RUN-LENGTH TO CHUNK-SECTOR-LENGTH
        COMPUTE CHUNK-SECTOR-LOCATION = CHUNK-SECTOR-OFFSET * 256 + CHUNK-SECTOR-LENGTH
        MOVE 1 TO OFFSET
        CALL "Encode-UnsignedInt" USING CHUNK-SECTOR-LOCATION REGION-FILE-OFFSET(LOCATION-INDEX) OFFSET

        *> Write the location information to the region file header
        COMPUTE FILE-OFFSET = (LOCATION-INDEX - 1) * 4
        MOVE 4 TO FILE-LENGTH
        CALL "Region-WriteToHandle" USING REGION-FILE-HANDLE FILE-OFFSET FILE-LENGTH REGION-FILE-OFFSET(LOCATION-INDEX) LK-FAILURE
        IF LK-FAILURE NOT = 0
            GOBACK
        END-IF
    END-IF

    *> Write the chunk header (5 bytes: 4 byte signed integer length, 1 byte compression type)
    MOVE 1 TO OFFSET
    COMPUTE CHUNK-DATA-LENGTH = CHUNK-COMPRESSED-LENGTH + 1
    CALL "Encode-Int" USING CHUNK-DATA-LENGTH CHUNK-HEADER OFFSET
    CALL "Encode-Byte" USING CHUNK-COMPRESSION CHUNK-HEADER OFFSET

    COMPUTE FILE-OFFSET = CHUNK-SECTOR-OFFSET * REGION-SECTOR-BYTES
    MOVE 5 TO FILE-LENGTH
    CALL "Region-WriteToHandle" USING REGION-FILE-HANDLE FILE-OFFSET FILE-LENGTH CHUNK-HEADER LK-FAILURE
    IF LK-FAILURE NOT = 0
        GOBACK
    END-IF

    *> Write the chunk data
    COMPUTE FILE-OFFSET = CHUNK-SECTOR-OFFSET * REGION-SECTOR-BYTES + 5
    MOVE CHUNK-COMPRESSED-LENGTH TO FILE-LENGTH
    CALL "Region-WriteToHandle" USING REGION-FILE-HANDLE FILE-OFFSET FILE-LENGTH CHUNK-COMPRESSION-BUFFER LK-FAILURE

    GOBACK.

END PROGRAM Region-WriteChunkData.
