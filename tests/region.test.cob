*> --- Test: region.cob ---
IDENTIFICATION DIVISION.
PROGRAM-ID. Test-Region.

PROCEDURE DIVISION.
    COPY TEST-SUITE REPLACING ==NAME== BY =="region.cob"==.
    CALL "Test-Region-RegionFileName"
    GOBACK.

    *> --- Test: Region-RegionFileName ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Test-Region-RegionFileName.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        COPY DD-REGION-FILES.
        COPY DD-SERVER-PROPERTIES.
        01 TYPE-IN      BINARY-CHAR UNSIGNED.
        01 X-IN         BINARY-LONG.
        01 Z-IN         BINARY-LONG.
        01 RESULT       PIC X(255).

    PROCEDURE DIVISION.
        COPY TEST-UNIT REPLACING ==NAME== BY =="Region-RegionFileName"==.
        MOVE "test-world-name" TO SP-LEVEL-NAME
        .
    AllZero.
        COPY TEST-CASE REPLACING ==NAME== BY =="0 0"==.
        MOVE FILE-TYPE-REGION TO TYPE-IN
        MOVE 0 TO X-IN
        MOVE 0 TO Z-IN
        MOVE SPACES TO RESULT
        CALL "Region-RegionFileName" USING TYPE-IN X-IN Z-IN RESULT
        COPY TEST-ASSERT REPLACING COND BY ==RESULT = "test-world-name/region/r.0.0.mca"==.
    NegativeX.
        COPY TEST-CASE REPLACING ==NAME== BY =="-1 0"==.
        MOVE FILE-TYPE-REGION TO TYPE-IN
        MOVE -1 TO X-IN
        MOVE 0 TO Z-IN
        MOVE SPACES TO RESULT
        CALL "Region-RegionFileName" USING TYPE-IN X-IN Z-IN RESULT
        COPY TEST-ASSERT REPLACING COND BY ==RESULT = "test-world-name/region/r.-1.0.mca"==.
    NegativeZ.
        COPY TEST-CASE REPLACING ==NAME== BY =="0 -1"==.
        MOVE FILE-TYPE-REGION TO TYPE-IN
        MOVE 0 TO X-IN
        MOVE -1 TO Z-IN
        MOVE SPACES TO RESULT
        CALL "Region-RegionFileName" USING TYPE-IN X-IN Z-IN RESULT
        COPY TEST-ASSERT REPLACING COND BY ==RESULT = "test-world-name/region/r.0.-1.mca"==.
    LargerValues.
        COPY TEST-CASE REPLACING ==NAME== BY =="1234 -5678"==.
        MOVE FILE-TYPE-REGION TO TYPE-IN
        MOVE 1234 TO X-IN
        MOVE -5678 TO Z-IN
        MOVE SPACES TO RESULT
        CALL "Region-RegionFileName" USING TYPE-IN X-IN Z-IN RESULT
        COPY TEST-ASSERT REPLACING COND BY ==RESULT = "test-world-name/region/r.1234.-5678.mca"==.
    EntityData.
        MOVE FILE-TYPE-ENTITY TO TYPE-IN
        COPY TEST-CASE REPLACING ==NAME== BY =="entity data"==.
        MOVE -123 TO X-IN
        MOVE 456 TO Z-IN
        MOVE SPACES TO RESULT
        CALL "Region-RegionFileName" USING TYPE-IN X-IN Z-IN RESULT
        COPY TEST-ASSERT REPLACING COND BY ==RESULT = "test-world-name/entities/r.-123.456.mca"==.

        GOBACK.

    END PROGRAM Test-Region-RegionFileName.

END PROGRAM Test-Region.
