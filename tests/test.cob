*> --- TestMain ---
IDENTIFICATION DIVISION.
PROGRAM-ID. TestMain.

DATA DIVISION.
WORKING-STORAGE SECTION.
    *> Number of total and failed test assertions
    01 TEST-COUNT                   BINARY-LONG UNSIGNED        EXTERNAL.
    01 TEST-FAILED                  BINARY-LONG UNSIGNED        EXTERNAL.
    01 TEST-SKIPPED                 BINARY-LONG UNSIGNED        EXTERNAL.
    *> Current test suite, unit and case names
    01 CURRENT-TEST-SUITE           PIC X(100)                  EXTERNAL.
    01 CURRENT-TEST-UNIT            PIC X(100)                  EXTERNAL.
    01 CURRENT-TEST-CASE            PIC X(100)                  EXTERNAL.
    *> Number of children per suite, unit, and case; used to detect empty suites and missing assertions
    01 SUITE-CHILDREN               BINARY-LONG UNSIGNED        EXTERNAL.
    01 UNIT-CHILDREN                BINARY-LONG UNSIGNED        EXTERNAL.
    01 CASE-CHILDREN                BINARY-LONG UNSIGNED        EXTERNAL.
    *> Table of failed tests
    01 FAILED-TESTS                                             EXTERNAL.
        02 FAILED-TEST              OCCURS 100 TIMES.
            03 FAILED-TEST-SUITE    PIC X(100).
            03 FAILED-TEST-UNIT     PIC X(100).
            03 FAILED-TEST-CASE     PIC X(100).
    01 DISPLAY-INT                  PIC -(9)9.
    01 IDX                          BINARY-LONG UNSIGNED.

PROCEDURE DIVISION.
    INITIALIZE TEST-COUNT
    INITIALIZE TEST-FAILED
    INITIALIZE TEST-SKIPPED

    INITIALIZE CURRENT-TEST-SUITE
    INITIALIZE CURRENT-TEST-UNIT
    INITIALIZE CURRENT-TEST-CASE

    INITIALIZE SUITE-CHILDREN
    INITIALIZE UNIT-CHILDREN
    INITIALIZE CASE-CHILDREN

    INITIALIZE FAILED-TESTS

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

    DISPLAY "---"

    MOVE TEST-COUNT TO DISPLAY-INT
    DISPLAY "Tests run: " FUNCTION TRIM(DISPLAY-INT)
    MOVE TEST-FAILED TO DISPLAY-INT
    DISPLAY "Tests failed: " FUNCTION TRIM(DISPLAY-INT)
    MOVE TEST-SKIPPED TO DISPLAY-INT
    DISPLAY "Tests skipped: " FUNCTION TRIM(DISPLAY-INT)

    IF TEST-COUNT <= 0
        DISPLAY "Error: No tests run" UPON STDERR
        STOP RUN RETURNING 1
    END-IF

    CALL "TestSuitePostValidate"

    IF TEST-FAILED > 0
        DISPLAY "---"
        DISPLAY "Failures:"

        PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > TEST-FAILED
            IF IDX = 1 OR FAILED-TEST-SUITE(IDX) NOT = FAILED-TEST-SUITE(IDX - 1)
                DISPLAY X"0A" "Suite: " FUNCTION TRIM(FAILED-TEST-SUITE(IDX))
            END-IF
            IF IDX = 1 OR FAILED-TEST-UNIT(IDX) NOT = FAILED-TEST-UNIT(IDX - 1)
                DISPLAY "  Test: " FUNCTION TRIM(FAILED-TEST-UNIT(IDX))
            END-IF
            DISPLAY "    Case: " FUNCTION TRIM(FAILED-TEST-CASE(IDX))
        END-PERFORM

        STOP RUN RETURNING 1
    END-IF

    STOP RUN.

END PROGRAM TestMain.

*> --- TestSuiteStarted ---
IDENTIFICATION DIVISION.
PROGRAM-ID. TestSuiteStarted.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 CURRENT-TEST-SUITE           PIC X(100)                  EXTERNAL.
    01 SUITE-CHILDREN               BINARY-LONG UNSIGNED        EXTERNAL.
LINKAGE SECTION.
    01 LK-NAME                      PIC X ANY LENGTH.

PROCEDURE DIVISION USING LK-NAME.
    IF CURRENT-TEST-SUITE NOT = SPACES
        CALL "TestSuitePostValidate"
    END-IF
    MOVE LK-NAME TO CURRENT-TEST-SUITE
    MOVE 0 TO SUITE-CHILDREN
    GOBACK.

END PROGRAM TestSuiteStarted.

*> --- TestUnitStarted ---
IDENTIFICATION DIVISION.
PROGRAM-ID. TestUnitStarted.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 CURRENT-TEST-UNIT            PIC X(100)                  EXTERNAL.
    01 SUITE-CHILDREN               BINARY-LONG UNSIGNED        EXTERNAL.
    01 UNIT-CHILDREN                BINARY-LONG UNSIGNED        EXTERNAL.
LINKAGE SECTION.
    01 LK-NAME                      PIC X ANY LENGTH.

PROCEDURE DIVISION USING LK-NAME.
    IF CURRENT-TEST-UNIT NOT = SPACES
        CALL "TestUnitPostValidate"
    END-IF
    MOVE LK-NAME TO CURRENT-TEST-UNIT
    ADD 1 TO SUITE-CHILDREN
    MOVE 0 TO UNIT-CHILDREN
    GOBACK.

END PROGRAM TestUnitStarted.

*> --- TestCaseStarted ---
IDENTIFICATION DIVISION.
PROGRAM-ID. TestCaseStarted.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 CURRENT-TEST-CASE            PIC X(100)                  EXTERNAL.
    01 UNIT-CHILDREN                BINARY-LONG UNSIGNED        EXTERNAL.
    01 CASE-CHILDREN                BINARY-LONG UNSIGNED        EXTERNAL.
LINKAGE SECTION.
    01 LK-NAME                      PIC X ANY LENGTH.

PROCEDURE DIVISION USING LK-NAME.
    IF CURRENT-TEST-CASE NOT = SPACES
        CALL "TestCasePostValidate"
    END-IF
    MOVE LK-NAME TO CURRENT-TEST-CASE
    ADD 1 TO UNIT-CHILDREN
    MOVE 0 TO CASE-CHILDREN
    GOBACK.

END PROGRAM TestCaseStarted.

*> --- TestAssertFailed ---
IDENTIFICATION DIVISION.
PROGRAM-ID. TestAssertFailed.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 TEST-COUNT                   BINARY-LONG UNSIGNED        EXTERNAL.
    01 TEST-FAILED                  BINARY-LONG UNSIGNED        EXTERNAL.
    01 CURRENT-TEST-SUITE           PIC X(100)                  EXTERNAL.
    01 CURRENT-TEST-UNIT            PIC X(100)                  EXTERNAL.
    01 CURRENT-TEST-CASE            PIC X(100)                  EXTERNAL.
    01 CASE-CHILDREN                BINARY-LONG UNSIGNED        EXTERNAL.
    01 FAILED-TESTS                                             EXTERNAL.
        02 FAILED-TEST              OCCURS 100 TIMES.
            03 FAILED-TEST-SUITE    PIC X(100).
            03 FAILED-TEST-UNIT     PIC X(100).
            03 FAILED-TEST-CASE     PIC X(100).

PROCEDURE DIVISION.
    ADD 1 TO TEST-COUNT
    ADD 1 TO TEST-FAILED
    ADD 1 TO CASE-CHILDREN

    IF TEST-FAILED <= 100
        MOVE CURRENT-TEST-SUITE TO FAILED-TEST-SUITE(TEST-FAILED)
        MOVE CURRENT-TEST-UNIT TO FAILED-TEST-UNIT(TEST-FAILED)
        MOVE CURRENT-TEST-CASE TO FAILED-TEST-CASE(TEST-FAILED)
    END-IF

    GOBACK.

END PROGRAM TestAssertFailed.

*> --- TestAssertPassed ---
IDENTIFICATION DIVISION.
PROGRAM-ID. TestAssertPassed.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 TEST-COUNT                   BINARY-LONG UNSIGNED        EXTERNAL.
    01 CASE-CHILDREN                BINARY-LONG UNSIGNED        EXTERNAL.

PROCEDURE DIVISION.
    ADD 1 TO TEST-COUNT
    ADD 1 TO CASE-CHILDREN
    GOBACK.

END PROGRAM TestAssertPassed.

*> --- TestAssertSkipped ---
IDENTIFICATION DIVISION.
PROGRAM-ID. TestAssertSkipped.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 TEST-COUNT                   BINARY-LONG UNSIGNED        EXTERNAL.
    01 TEST-SKIPPED                 BINARY-LONG UNSIGNED        EXTERNAL.
    01 CASE-CHILDREN                BINARY-LONG UNSIGNED        EXTERNAL.

PROCEDURE DIVISION.
    ADD 1 TO TEST-COUNT
    ADD 1 TO TEST-SKIPPED
    ADD 1 TO CASE-CHILDREN
    GOBACK.

END PROGRAM TestAssertSkipped.

*> --- TestSuitePostValidate ---
IDENTIFICATION DIVISION.
PROGRAM-ID. TestSuitePostValidate.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 CURRENT-TEST-SUITE           PIC X(100)                  EXTERNAL.
    01 SUITE-CHILDREN               BINARY-LONG UNSIGNED        EXTERNAL.

PROCEDURE DIVISION.
    IF SUITE-CHILDREN = 0
        DISPLAY "ERROR: Suite " FUNCTION TRIM(CURRENT-TEST-SUITE) " has no tests" UPON STDERR
        STOP RUN RETURNING 1
    END-IF
    CALL "TestUnitPostValidate"
    GOBACK.

END PROGRAM TestSuitePostValidate.

*> --- TestUnitPostValidate ---
IDENTIFICATION DIVISION.
PROGRAM-ID. TestUnitPostValidate.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 CURRENT-TEST-SUITE           PIC X(100)                  EXTERNAL.
    01 CURRENT-TEST-UNIT            PIC X(100)                  EXTERNAL.
    01 UNIT-CHILDREN                BINARY-LONG UNSIGNED        EXTERNAL.

PROCEDURE DIVISION.
    IF UNIT-CHILDREN = 0
        DISPLAY "ERROR: Unit """ FUNCTION TRIM(CURRENT-TEST-UNIT) """ in suite """ FUNCTION TRIM(CURRENT-TEST-SUITE) """ has no tests" UPON STDERR
        STOP RUN RETURNING 1
    END-IF
    CALL "TestCasePostValidate"
    GOBACK.

END PROGRAM TestUnitPostValidate.

*> --- TestCasePostValidate ---
IDENTIFICATION DIVISION.
PROGRAM-ID. TestCasePostValidate.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 CURRENT-TEST-SUITE           PIC X(100)                  EXTERNAL.
    01 CURRENT-TEST-UNIT            PIC X(100)                  EXTERNAL.
    01 CURRENT-TEST-CASE            PIC X(100)                  EXTERNAL.
    01 CASE-CHILDREN                BINARY-LONG UNSIGNED        EXTERNAL.

PROCEDURE DIVISION.
    IF CASE-CHILDREN = 0
        DISPLAY "ERROR: Case """ FUNCTION TRIM(CURRENT-TEST-CASE) """ in unit """ FUNCTION TRIM(CURRENT-TEST-UNIT) """ "
            "in suite """ FUNCTION TRIM(CURRENT-TEST-SUITE) """ has no assertion" UPON STDERR
        STOP RUN RETURNING 1
    END-IF
    GOBACK.

END PROGRAM TestCasePostValidate.
