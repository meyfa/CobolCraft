*> --- Test: cobolcraft_util.cpp ---
IDENTIFICATION DIVISION.
PROGRAM-ID. Test-Util.

PROCEDURE DIVISION.
    DISPLAY "Test: cobolcraft_util.cpp"
    CALL "Test-LeadingZeros32"
    CALL "Test-GzipCompress"
    CALL "Test-GzipDecompress"
    GOBACK.

    *> --- Test: Test-LeadingZeros32 ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Test-LeadingZeros32.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        01 VALUE-IN     BINARY-LONG UNSIGNED.
        01 RESULT       BINARY-LONG UNSIGNED.

    PROCEDURE DIVISION.
        DISPLAY "  Test: Test-LeadingZeros32".
    Int0.
        DISPLAY "    Case: 0 - " WITH NO ADVANCING
        MOVE 0 TO VALUE-IN
        CALL "LeadingZeros32" USING VALUE-IN RESULT
        IF RESULT = 32
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL, result: " RESULT
        END-IF.
    Int1.
        DISPLAY "    Case: 1 - " WITH NO ADVANCING
        MOVE 1 TO VALUE-IN
        CALL "LeadingZeros32" USING VALUE-IN RESULT
        IF RESULT = 31
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    Int2.
        DISPLAY "    Case: 2 - " WITH NO ADVANCING
        MOVE 2 TO VALUE-IN
        CALL "LeadingZeros32" USING VALUE-IN RESULT
        IF RESULT = 30
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    Int3.
        DISPLAY "    Case: 3 - " WITH NO ADVANCING
        MOVE 3 TO VALUE-IN
        CALL "LeadingZeros32" USING VALUE-IN RESULT
        IF RESULT = 30
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    IntMax.
        DISPLAY "    Case: 2^32-1 - " WITH NO ADVANCING
        MOVE 4294967295 TO VALUE-IN
        CALL "LeadingZeros32" USING VALUE-IN RESULT
        IF RESULT = 0
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.

        GOBACK.

    END PROGRAM Test-LeadingZeros32.

    *> --- Test: Test-GzipCompress ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Test-GzipCompress.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        01 INPUT-BUFFER         PIC X(100).
        01 OUTPUT-BUFFER        PIC X(100).
        01 INPUT-LENGTH         BINARY-LONG UNSIGNED.
        01 OUTPUT-LENGTH        BINARY-LONG UNSIGNED.
        01 ERRNO                BINARY-LONG UNSIGNED.

    PROCEDURE DIVISION.
        DISPLAY "  Test: Test-GzipCompress".
    Basic.
        DISPLAY "    Case: Basic - " WITH NO ADVANCING
        MOVE "Hello, World!" TO INPUT-BUFFER
        MOVE 13 TO INPUT-LENGTH
        MOVE SPACES TO OUTPUT-BUFFER
        MOVE LENGTH OF OUTPUT-BUFFER TO OUTPUT-LENGTH
        CALL "GzipCompress" USING INPUT-BUFFER INPUT-LENGTH OUTPUT-BUFFER OUTPUT-LENGTH GIVING ERRNO
        IF ERRNO = 0 AND OUTPUT-LENGTH = 33 AND OUTPUT-BUFFER(1:33) = X"1F8B0800000000000003F348CDC9C9D75108CF2FCA49510400D0C34AEC0D000000"
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.

        GOBACK.

    END PROGRAM Test-GzipCompress.

    *> --- Test: Test-GzipDecompress ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Test-GzipDecompress.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        01 INPUT-BUFFER         PIC X(100).
        01 OUTPUT-BUFFER        PIC X(100).
        01 INPUT-LENGTH         BINARY-LONG UNSIGNED.
        01 OUTPUT-LENGTH        BINARY-LONG UNSIGNED.
        01 ERRNO                BINARY-LONG UNSIGNED.

    PROCEDURE DIVISION.
        DISPLAY "  Test: Test-GzipDecompress".
    Basic.
        DISPLAY "    Case: Basic - " WITH NO ADVANCING
        MOVE X"1F8B0800000000000003F348CDC9C9D75108CF2FCA49510400D0C34AEC0D000000" TO INPUT-BUFFER
        MOVE 33 TO INPUT-LENGTH
        MOVE SPACES TO OUTPUT-BUFFER
        MOVE LENGTH OF OUTPUT-BUFFER TO OUTPUT-LENGTH
        CALL "GzipDecompress" USING INPUT-BUFFER INPUT-LENGTH OUTPUT-BUFFER OUTPUT-LENGTH GIVING ERRNO
        IF ERRNO = 0 AND OUTPUT-LENGTH = 13 AND OUTPUT-BUFFER(1:13) = "Hello, World!"
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.

        GOBACK.

    END PROGRAM Test-GzipDecompress.

END PROGRAM Test-Util.
