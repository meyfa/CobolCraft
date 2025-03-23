*> --- Codegen-SetTemplateDirectory ---
IDENTIFICATION DIVISION.
PROGRAM-ID. Codegen-SetTemplateDirectory.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 CODEGEN-TPL-DIR              PIC X(255)                  EXTERNAL.
LINKAGE SECTION.
    01 LK-TPL-DIR                   PIC X ANY LENGTH.

PROCEDURE DIVISION USING LK-TPL-DIR.
    MOVE LK-TPL-DIR TO CODEGEN-TPL-DIR
    GOBACK.

END PROGRAM Codegen-SetTemplateDirectory.

*> --- Codegen-TemplateLoad ---
IDENTIFICATION DIVISION.
PROGRAM-ID. Codegen-TemplateLoad.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 CODEGEN-TPL-DIR              PIC X(255)                  EXTERNAL.
    01 FAILURE                      BINARY-CHAR UNSIGNED.
    01 IDX                          BINARY-LONG UNSIGNED.
    01 IDX2                         BINARY-LONG UNSIGNED.
    01 TRANSFORM-IDX                BINARY-LONG UNSIGNED.
    01 TRANSFORM-NAME               PIC X(16).
    01 TRANSFORM-VALUE              PIC X(16).
LINKAGE SECTION.
    01 LK-TPLNAME                   PIC X ANY LENGTH.
    01 LK-REPLACE-PTR               PROGRAM-POINTER.
    COPY DD-CODEGEN-TEMPLATE REPLACING LEADING ==PREFIX== BY ==LK==.

PROCEDURE DIVISION USING LK-TPLNAME OPTIONAL LK-REPLACE-PTR LK-TPL.
    INITIALIZE LK-TPL-FILENAME
    STRING FUNCTION TRIM(CODEGEN-TPL-DIR) "/" FUNCTION TRIM(LK-TPLNAME) INTO LK-TPL-FILENAME

    CALL "Files-ReadAll" USING LK-TPL-FILENAME LK-TPL-BUFFER LK-TPL-LENGTH FAILURE
    COPY ASSERT REPLACING COND BY ==FAILURE = 0==,
        MSG BY =="Codegen: Failed to read template: " FUNCTION TRIM(LK-TPL-FILENAME)==.

    IF LK-REPLACE-PTR IS OMITTED OR LK-REPLACE-PTR = NULL
        SET LK-TPL-REPLACE TO NULL
    ELSE
        SET LK-TPL-REPLACE TO LK-REPLACE-PTR
    END-IF

    MOVE 0 TO LK-TPL-VARS

    PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > LK-TPL-LENGTH
        IF LK-TPL-BUFFER(IDX:1) = "$"
            *> find the closing "$"
            COMPUTE IDX2 = IDX + 1
            MOVE 0 TO TRANSFORM-IDX
            PERFORM UNTIL IDX2 > LK-TPL-LENGTH OR LK-TPL-BUFFER(IDX2:1) = "$"
                *> If we hit a ":" on the way, this variable has a transform (such as indent)
                IF LK-TPL-BUFFER(IDX2:1) = ":"
                    MOVE IDX2 TO TRANSFORM-IDX
                END-IF
                ADD 1 TO IDX2
            END-PERFORM

            ADD 1 TO LK-TPL-VARS
            INITIALIZE LK-TPL-VAR(LK-TPL-VARS)
            MOVE IDX TO LK-TPL-VAR-START(LK-TPL-VARS)
            MOVE IDX2 TO LK-TPL-VAR-END(LK-TPL-VARS)

            IF TRANSFORM-IDX > 0
                MOVE LK-TPL-BUFFER(IDX + 1:TRANSFORM-IDX - IDX - 1) TO LK-TPL-VAR-NAME(LK-TPL-VARS)

                *> Split the transform into name and value
                COMPUTE IDX = TRANSFORM-IDX + 1
                INITIALIZE TRANSFORM-NAME
                PERFORM UNTIL IDX >= IDX2
                    IF LK-TPL-BUFFER(IDX:1) = "="
                        MOVE LK-TPL-BUFFER(TRANSFORM-IDX + 1:IDX - TRANSFORM-IDX - 1) TO TRANSFORM-NAME
                        MOVE LK-TPL-BUFFER(IDX + 1:IDX2 - IDX - 1) TO TRANSFORM-VALUE
                        EXIT PERFORM
                    END-IF
                    ADD 1 TO IDX
                END-PERFORM

                EVALUATE TRANSFORM-NAME
                    WHEN "indent"
                        MOVE FUNCTION NUMVAL(TRANSFORM-VALUE) TO LK-TPL-VAR-INDENT(LK-TPL-VARS)
                    WHEN "newline"
                        EVALUATE TRANSFORM-VALUE
                            WHEN "after"
                                MOVE 1 TO LK-TPL-VAR-NEWLINE(LK-TPL-VARS)
                            WHEN OTHER
                                COPY ASSERT-FAILED REPLACING MSG BY =="Codegen: Unknown newline transform value: " FUNCTION TRIM(TRANSFORM-VALUE)==.
                        END-EVALUATE
                    WHEN OTHER
                        COPY ASSERT-FAILED REPLACING MSG BY =="Codegen: Unknown transform: " FUNCTION TRIM(TRANSFORM-NAME)==.
                END-EVALUATE
            ELSE
                MOVE LK-TPL-BUFFER(IDX + 1:IDX2 - IDX - 1) TO LK-TPL-VAR-NAME(LK-TPL-VARS)
            END-IF

            MOVE IDX2 TO IDX
        END-IF
    END-PERFORM

    GOBACK.

END PROGRAM Codegen-TemplateLoad.

*> --- Codegen-TemplateEval ---
IDENTIFICATION DIVISION.
PROGRAM-ID. Codegen-TemplateEval IS RECURSIVE.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 CURRENT-INDENT               BINARY-LONG UNSIGNED.
LOCAL-STORAGE SECTION.
    01 TEMPLATE-POSITION            BINARY-LONG UNSIGNED        VALUE 1.
    01 COPY-COUNT                   BINARY-LONG UNSIGNED.
    01 VAR-INDEX                    BINARY-LONG UNSIGNED.
    01 PREV-OUT-LENGTH              BINARY-LONG UNSIGNED.
LINKAGE SECTION.
    *> input template
    COPY DD-CODEGEN-TEMPLATE REPLACING LEADING ==PREFIX== BY ==LK==.
    *> output buffer
    01 LK-OUT-BUFFER                PIC X ANY LENGTH.
    01 LK-OUT-LENGTH                BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-TPL LK-OUT-BUFFER LK-OUT-LENGTH.
    PERFORM VARYING VAR-INDEX FROM 1 BY 1 UNTIL VAR-INDEX > LK-TPL-VARS
        *> copy until the beginning of the variable
        COMPUTE COPY-COUNT = LK-TPL-VAR-START(VAR-INDEX) - TEMPLATE-POSITION
        PERFORM CopyTemplate

        *> replace the variable
        IF LK-TPL-REPLACE NOT = NULL
            MOVE LK-OUT-LENGTH TO PREV-OUT-LENGTH
            ADD LK-TPL-VAR-INDENT(VAR-INDEX) TO CURRENT-INDENT

            CALL LK-TPL-REPLACE USING LK-TPL-VAR-NAME(VAR-INDEX) LK-OUT-BUFFER LK-OUT-LENGTH

            SUBTRACT LK-TPL-VAR-INDENT(VAR-INDEX) FROM CURRENT-INDENT

            *> Ensure a single newline after the variable if requested
            IF LK-TPL-VAR-NEWLINE(VAR-INDEX) NOT = 0
                MOVE X"0A" TO LK-OUT-BUFFER(LK-OUT-LENGTH + 1:1)
                ADD 1 TO LK-OUT-LENGTH
            ELSE
                *> Otherwise, remove any trailing newlines
                PERFORM UNTIL LK-OUT-LENGTH <= PREV-OUT-LENGTH OR LK-OUT-BUFFER(LK-OUT-LENGTH:1) NOT = X"0A"
                    SUBTRACT 1 FROM LK-OUT-LENGTH
                END-PERFORM
            END-IF
        END-IF

        *> move to after the variable
        COMPUTE TEMPLATE-POSITION = LK-TPL-VAR-END(VAR-INDEX) + 1
    END-PERFORM

    *> no more variables, copy the remaining template to the output
    IF TEMPLATE-POSITION < LK-TPL-LENGTH
        COMPUTE COPY-COUNT = LK-TPL-LENGTH - TEMPLATE-POSITION + 1
        PERFORM CopyTemplate
    END-IF

    GOBACK.

CopyTemplate.
    PERFORM COPY-COUNT TIMES
        IF CURRENT-INDENT > 0 AND LK-OUT-LENGTH > 0 AND LK-OUT-BUFFER(LK-OUT-LENGTH:1) = X"0A"
                AND (TEMPLATE-POSITION >= LENGTH OF LK-TPL-BUFFER OR LK-TPL-BUFFER(TEMPLATE-POSITION:1) NOT = X"0A")
            MOVE ALL SPACES TO LK-OUT-BUFFER(LK-OUT-LENGTH + 1:CURRENT-INDENT)
            ADD CURRENT-INDENT TO LK-OUT-LENGTH
        END-IF

        MOVE LK-TPL-BUFFER(TEMPLATE-POSITION:1) TO LK-OUT-BUFFER(LK-OUT-LENGTH + 1:1)
        ADD 1 TO LK-OUT-LENGTH
        ADD 1 TO TEMPLATE-POSITION
    END-PERFORM
    .

END PROGRAM Codegen-TemplateEval.
