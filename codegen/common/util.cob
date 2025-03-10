IDENTIFICATION DIVISION.
PROGRAM-ID. Codegen-ProgramId.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 STR                          PIC X(100).
    01 LEN                          BINARY-LONG UNSIGNED.
    01 STR-IDX                      BINARY-LONG UNSIGNED.
    01 OUT-IDX                      BINARY-LONG UNSIGNED.
LINKAGE SECTION.
    01 LK-PREFIX                    PIC X ANY LENGTH.
    01 LK-NAME                      PIC X ANY LENGTH.
    *> result
    01 LK-PROGRAM-ID                PIC X ANY LENGTH.

PROCEDURE DIVISION USING LK-PREFIX LK-NAME LK-PROGRAM-ID.
    INITIALIZE LK-PROGRAM-ID

    INITIALIZE STR
    STRING FUNCTION TRIM(LK-PREFIX) FUNCTION TRIM(LK-NAME) INTO STR

    *> Copy only legal characters into the program id
    MOVE 1 TO STR-IDX
    MOVE 1 TO OUT-IDX
    PERFORM UNTIL STR-IDX > FUNCTION LENGTH(STR) OR OUT-IDX > 30
        IF STR(STR-IDX:1) IS ALPHABETIC OR STR(STR-IDX:1) IS NUMERIC OR STR(STR-IDX:1) = "-"
            MOVE STR(STR-IDX:1) TO LK-PROGRAM-ID(OUT-IDX:1)
            ADD 1 TO OUT-IDX
        END-IF
        ADD 1 TO STR-IDX
    END-PERFORM

    *> Remove trailing hyphens
    PERFORM VARYING OUT-IDX FROM 30 BY -1 UNTIL OUT-IDX < 1
        IF LK-PROGRAM-ID(OUT-IDX:1) = "-"
            MOVE SPACES TO LK-PROGRAM-ID(OUT-IDX:1)
        ELSE
            EXIT PERFORM
        END-IF
    END-PERFORM

    GOBACK.

END PROGRAM Codegen-ProgramId.
