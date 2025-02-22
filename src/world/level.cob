*> --- World-SaveLevel ---
IDENTIFICATION DIVISION.
PROGRAM-ID. World-SaveLevel.

DATA DIVISION.
WORKING-STORAGE SECTION.
    *> File name and data
    01 LEVEL-FILE-NAME          PIC X(255)                  VALUE "save/level.dat".
    01 ERRNO                    BINARY-LONG.
    01 NBT-BUFFER               PIC X(64000).
    01 NBT-BUFFER-LENGTH        BINARY-LONG UNSIGNED.
    01 COMPRESSED-BUFFER        PIC X(64000).
    01 COMPRESSED-LENGTH        BINARY-LONG UNSIGNED.
    *> Temporary variables
    01 TAG-NAME                 PIC X(256).
    01 NAME-LEN                 BINARY-LONG UNSIGNED.
    *> World data
    COPY DD-WORLD.
LOCAL-STORAGE SECTION.
    COPY DD-NBT-ENCODER.
LINKAGE SECTION.
    01 LK-FAILURE               BINARY-CHAR UNSIGNED.

PROCEDURE DIVISION USING LK-FAILURE.
    MOVE 0 TO LK-FAILURE
    MOVE ALL X"00" TO NBT-BUFFER

    *> root tag
    MOVE 1 TO NBT-ENCODER-OFFSET
    CALL "NbtEncode-RootCompound" USING NBT-ENCODER-STATE NBT-BUFFER

    *> "Data" tag
    MOVE "Data" TO TAG-NAME
    MOVE 4 TO NAME-LEN
    CALL "NbtEncode-Compound" USING NBT-ENCODER-STATE NBT-BUFFER TAG-NAME NAME-LEN

    *> "Time": world age
    MOVE "Time" TO TAG-NAME
    MOVE 4 TO NAME-LEN
    CALL "NbtEncode-Long" USING NBT-ENCODER-STATE NBT-BUFFER TAG-NAME NAME-LEN WORLD-AGE

    *> "DayTime": world time
    MOVE "DayTime" TO TAG-NAME
    MOVE 7 TO NAME-LEN
    CALL "NbtEncode-Long" USING NBT-ENCODER-STATE NBT-BUFFER TAG-NAME NAME-LEN WORLD-TIME

    *> "SpawnX", "SpawnY", "SpawnZ": spawn point
    MOVE 6 TO NAME-LEN
    MOVE "SpawnX" TO TAG-NAME
    CALL "NbtEncode-Int" USING NBT-ENCODER-STATE NBT-BUFFER TAG-NAME NAME-LEN WORLD-SPAWN-X
    MOVE "SpawnY" TO TAG-NAME
    CALL "NbtEncode-Int" USING NBT-ENCODER-STATE NBT-BUFFER TAG-NAME NAME-LEN WORLD-SPAWN-Y
    MOVE "SpawnZ" TO TAG-NAME
    CALL "NbtEncode-Int" USING NBT-ENCODER-STATE NBT-BUFFER TAG-NAME NAME-LEN WORLD-SPAWN-Z

    *> hardcore mode
    MOVE "hardcore" TO TAG-NAME
    MOVE 8 TO NAME-LEN
    CALL "NbtEncode-Byte" USING NBT-ENCODER-STATE NBT-BUFFER TAG-NAME NAME-LEN WORLD-HARDCORE

    *> end "Data" and root tags
    CALL "NbtEncode-EndCompound" USING NBT-ENCODER-STATE NBT-BUFFER
    CALL "NbtEncode-EndCompound" USING NBT-ENCODER-STATE NBT-BUFFER

    *> write the data to disk in gzip-compressed form
    COMPUTE NBT-BUFFER-LENGTH = NBT-ENCODER-OFFSET - 1
    MOVE LENGTH OF COMPRESSED-BUFFER TO COMPRESSED-LENGTH
    CALL "GzipCompress" USING NBT-BUFFER NBT-BUFFER-LENGTH COMPRESSED-BUFFER COMPRESSED-LENGTH GIVING ERRNO
    IF ERRNO NOT = 0
        MOVE 1 TO LK-FAILURE
        GOBACK
    END-IF
    CALL "Files-WriteAll" USING LEVEL-FILE-NAME COMPRESSED-BUFFER COMPRESSED-LENGTH LK-FAILURE

    GOBACK.

END PROGRAM World-SaveLevel.

*> --- World-LoadLevel ---
IDENTIFICATION DIVISION.
PROGRAM-ID. World-LoadLevel.

DATA DIVISION.
WORKING-STORAGE SECTION.
    *> File name and data
    01 LEVEL-FILE-NAME          PIC X(255)                  VALUE "save/level.dat".
    01 ERRNO                    BINARY-LONG.
    01 COMPRESSED-BUFFER        PIC X(64000).
    01 COMPRESSED-LENGTH        BINARY-LONG UNSIGNED.
    01 NBT-BUFFER               PIC X(64000).
    01 NBT-BUFFER-LENGTH        BINARY-LONG UNSIGNED.
    *> Temporary variables
    01 STR-VALUE                PIC X(256).
    01 STR-LEN                  BINARY-LONG UNSIGNED.
    01 AT-END                   BINARY-CHAR UNSIGNED.
    *> World data
    COPY DD-WORLD.
LOCAL-STORAGE SECTION.
    COPY DD-NBT-DECODER.
LINKAGE SECTION.
    01 LK-FAILURE               BINARY-CHAR UNSIGNED.

PROCEDURE DIVISION USING LK-FAILURE.
    MOVE 0 TO LK-FAILURE

    *> Set defaults
    MOVE 0 TO WORLD-AGE WORLD-TIME
    MOVE 0 TO WORLD-SPAWN-X WORLD-SPAWN-Y WORLD-SPAWN-Z
    MOVE 0 TO WORLD-HARDCORE

    *> Read the file
    CALL "Files-ReadAll" USING LEVEL-FILE-NAME NBT-BUFFER NBT-BUFFER-LENGTH LK-FAILURE
    IF LK-FAILURE NOT = 0 OR NBT-BUFFER-LENGTH = 0
        GOBACK
    END-IF

    *> Check for the gzip magic number, and decompress if present
    IF NBT-BUFFER(1:2) = X"1F8B"
        MOVE NBT-BUFFER(1:NBT-BUFFER-LENGTH) TO COMPRESSED-BUFFER(1:NBT-BUFFER-LENGTH)
        MOVE NBT-BUFFER-LENGTH TO COMPRESSED-LENGTH
        MOVE LENGTH OF NBT-BUFFER TO NBT-BUFFER-LENGTH
        CALL "GzipDecompress" USING COMPRESSED-BUFFER COMPRESSED-LENGTH NBT-BUFFER NBT-BUFFER-LENGTH GIVING ERRNO
        IF ERRNO NOT = 0
            MOVE 1 TO LK-FAILURE
            GOBACK
        END-IF
    END-IF

    *> root tag containing the "Data" compound
    MOVE 1 TO NBT-DECODER-OFFSET
    CALL "NbtDecode-RootCompound" USING NBT-DECODER-STATE NBT-BUFFER
    CALL "NbtDecode-Compound" USING NBT-DECODER-STATE NBT-BUFFER

    PERFORM UNTIL EXIT
        CALL "NbtDecode-Peek" USING NBT-DECODER-STATE NBT-BUFFER AT-END STR-VALUE STR-LEN
        IF AT-END > 0
            EXIT PERFORM
        END-IF
        EVALUATE STR-VALUE(1:STR-LEN)
            WHEN "Time"
                CALL "NbtDecode-Long" USING NBT-DECODER-STATE NBT-BUFFER WORLD-AGE
            WHEN "DayTime"
                CALL "NbtDecode-Long" USING NBT-DECODER-STATE NBT-BUFFER WORLD-TIME
            WHEN "SpawnX"
                CALL "NbtDecode-Int" USING NBT-DECODER-STATE NBT-BUFFER WORLD-SPAWN-X
            WHEN "SpawnY"
                CALL "NbtDecode-Int" USING NBT-DECODER-STATE NBT-BUFFER WORLD-SPAWN-Y
            WHEN "SpawnZ"
                CALL "NbtDecode-Int" USING NBT-DECODER-STATE NBT-BUFFER WORLD-SPAWN-Z
            WHEN "hardcore"
                CALL "NbtDecode-Byte" USING NBT-DECODER-STATE NBT-BUFFER WORLD-HARDCORE
            WHEN OTHER
                CALL "NbtDecode-Skip" USING NBT-DECODER-STATE NBT-BUFFER
        END-EVALUATE
    END-PERFORM

    *> end of "Data" and root tags
    CALL "NbtDecode-EndCompound" USING NBT-DECODER-STATE NBT-BUFFER
    CALL "NbtDecode-EndCompound" USING NBT-DECODER-STATE NBT-BUFFER

    GOBACK.

END PROGRAM World-LoadLevel.

*> --- World-GetAge ---
IDENTIFICATION DIVISION.
PROGRAM-ID. World-GetAge.

DATA DIVISION.
WORKING-STORAGE SECTION.
    COPY DD-WORLD.
LINKAGE SECTION.
    01 LK-AGE                   BINARY-LONG-LONG.

PROCEDURE DIVISION USING LK-AGE.
    MOVE WORLD-AGE TO LK-AGE
    GOBACK.

END PROGRAM World-GetAge.

*> --- World-GetTime ---
IDENTIFICATION DIVISION.
PROGRAM-ID. World-GetTime.

DATA DIVISION.
WORKING-STORAGE SECTION.
    COPY DD-WORLD.
LINKAGE SECTION.
    01 LK-TIME                  BINARY-LONG-LONG.

PROCEDURE DIVISION USING LK-TIME.
    MOVE WORLD-TIME TO LK-TIME
    GOBACK.

END PROGRAM World-GetTime.

*> --- World-SetTime ---
IDENTIFICATION DIVISION.
PROGRAM-ID. World-SetTime.

DATA DIVISION.
WORKING-STORAGE SECTION.
    COPY DD-WORLD.
LINKAGE SECTION.
    01 LK-TIME                  BINARY-LONG-LONG.

PROCEDURE DIVISION USING LK-TIME.
    MOVE LK-TIME TO WORLD-TIME
    GOBACK.

END PROGRAM World-SetTime.

*> --- World-IsHardcore ---
IDENTIFICATION DIVISION.
PROGRAM-ID. World-IsHardcore.

DATA DIVISION.
WORKING-STORAGE SECTION.
    COPY DD-WORLD.
LINKAGE SECTION.
    01 LK-HARDCORE              BINARY-CHAR UNSIGNED.

PROCEDURE DIVISION USING LK-HARDCORE.
    MOVE WORLD-HARDCORE TO LK-HARDCORE
    GOBACK.

END PROGRAM World-IsHardcore.
