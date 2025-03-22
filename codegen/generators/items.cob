*> --- CG-Items ---
IDENTIFICATION DIVISION.
PROGRAM-ID. CG-Items.

DATA DIVISION.
WORKING-STORAGE SECTION.
    *> output buffer
    01 BUFFER                       PIC X(1000000).
    01 BUFFERLEN                    BINARY-LONG UNSIGNED.
    *> template
    01 REPLACE-PTR                  PROGRAM-POINTER.
    COPY DD-CODEGEN-TEMPLATE REPLACING LEADING ==PREFIX== BY ==MAIN==.

PROCEDURE DIVISION.
    SET REPLACE-PTR TO ENTRY "CG-Items-Main"
    CALL "Codegen-TemplateLoad" USING "items/main.tpl.cob" REPLACE-PTR MAIN-TPL

    CALL "Codegen-Start" USING "items.cob"

    MOVE 0 TO BUFFERLEN
    CALL "Codegen-TemplateEval" USING MAIN-TPL BUFFER BUFFERLEN
    CALL "Codegen-Append" USING BUFFER(1:BUFFERLEN)

    CALL "Codegen-End"

    GOBACK.

END PROGRAM CG-Items.

*> --- CG-Items-Main ---
IDENTIFICATION DIVISION.
PROGRAM-ID. CG-Items-Main.

DATA DIVISION.
WORKING-STORAGE SECTION.
    *> initialized once
    01 INIT-DONE                    BINARY-CHAR UNSIGNED.
    01 REPLACE-PTR                  PROGRAM-POINTER.
    COPY DD-CODEGEN-TEMPLATE REPLACING LEADING ==PREFIX== BY ==REGISTRATION==.
    *> shared state
    COPY DD-CODEGEN-JSON.
    01 ITEM-NAME                    PIC X(64)                   EXTERNAL.
    *> dynamic variables
    01 JSONLEN                      BINARY-LONG UNSIGNED.
    01 FAILURE                      BINARY-CHAR UNSIGNED.
LINKAGE SECTION.
    01 LK-VARNAME                   PIC X ANY LENGTH.
    01 LK-BUFFER                    PIC X ANY LENGTH.
    01 LK-LENGTH                    BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-VARNAME LK-BUFFER LK-LENGTH.
    IF INIT-DONE = 0
        SET REPLACE-PTR TO ENTRY "CG-Items-Register"
        CALL "Codegen-TemplateLoad" USING "items/registration.tpl.cob" REPLACE-PTR REGISTRATION-TPL

        CALL "Codegen-ReadDataFile" USING "generated/reports/items.json" JSONBUF JSONLEN

        MOVE 1 TO INIT-DONE
    END-IF

    MOVE 1 TO JSONPOS

    EVALUATE LK-VARNAME
        WHEN "registrations"
            CALL "JsonParse-ObjectStart" USING JSONBUF JSONPOS FAILURE
            PERFORM AssertOk

            PERFORM UNTIL EXIT
                *> TODO lookup item ID (avoid unnecessary strings in generated source)
                CALL "JsonParse-ObjectKey" USING JSONBUF JSONPOS FAILURE ITEM-NAME
                PERFORM AssertOk

                CALL "Codegen-TemplateEval" USING REGISTRATION-TPL LK-BUFFER LK-LENGTH

                CALL "JsonParse-Comma" USING JSONBUF JSONPOS FAILURE
                IF FAILURE > 0 EXIT PERFORM END-IF
            END-PERFORM

            CALL "JsonParse-ObjectEnd" USING JSONBUF JSONPOS FAILURE
            PERFORM AssertOk
    END-EVALUATE

    GOBACK.

AssertOk.
    COPY ASSERT REPLACING COND BY ==FAILURE = 0==, MSG BY =="CG-Items-Main: Failed to parse JSON"==.
    .

END PROGRAM CG-Items-Main.

*> --- CG-Items-Register ---
IDENTIFICATION DIVISION.
PROGRAM-ID. CG-Items-Register.

DATA DIVISION.
WORKING-STORAGE SECTION.
    *> shared state
    COPY DD-CODEGEN-JSON.
    01 ITEM-NAME                    PIC X(64)                   EXTERNAL.
    *> dynamic variables
    01 STRLEN                       BINARY-LONG UNSIGNED.
    01 STR                          PIC X(64).
    01 FAILURE                      BINARY-CHAR UNSIGNED.
    01 MAX-STACK-SIZE               BINARY-LONG.
LINKAGE SECTION.
    01 LK-VARNAME                   PIC X ANY LENGTH.
    01 LK-BUFFER                    PIC X ANY LENGTH.
    01 LK-LENGTH                    BINARY-LONG UNSIGNED.

PROCEDURE DIVISION USING LK-VARNAME LK-BUFFER LK-LENGTH.
    EVALUATE LK-VARNAME
        WHEN "mss"
            PERFORM ReplaceMaxStackSize
        WHEN "item-name"
            MOVE FUNCTION STORED-CHAR-LENGTH(ITEM-NAME) TO STRLEN
            STRING ITEM-NAME(1:STRLEN) INTO LK-BUFFER(LK-LENGTH + 1:)
            ADD STRLEN TO LK-LENGTH
    END-EVALUATE

    GOBACK.

ReplaceMaxStackSize.
    MOVE 0 TO MAX-STACK-SIZE

    CALL "JsonParse-ObjectStart" USING JSONBUF JSONPOS FAILURE
    PERFORM AssertOk

    PERFORM UNTIL EXIT
        CALL "JsonParse-ObjectKey" USING JSONBUF JSONPOS FAILURE STR
        PERFORM AssertOk

        EVALUATE STR
            WHEN "components"
                CALL "JsonParse-ObjectStart" USING JSONBUF JSONPOS FAILURE
                PERFORM AssertOk

                PERFORM UNTIL EXIT
                    CALL "JsonParse-ObjectKey" USING JSONBUF JSONPOS FAILURE STR
                    PERFORM AssertOk

                    EVALUATE STR
                        WHEN "minecraft:max_stack_size"
                            CALL "JsonParse-Integer" USING JSONBUF JSONPOS FAILURE MAX-STACK-SIZE
                            PERFORM AssertOk
                        WHEN OTHER
                            CALL "JsonParse-SkipValue" USING JSONBUF JSONPOS FAILURE
                    END-EVALUATE

                    CALL "JsonParse-Comma" USING JSONBUF JSONPOS FAILURE
                    IF FAILURE > 0 EXIT PERFORM END-IF
                END-PERFORM

                CALL "JsonParse-ObjectEnd" USING JSONBUF JSONPOS FAILURE
                PERFORM AssertOk

            WHEN OTHER
                CALL "JsonParse-SkipValue" USING JSONBUF JSONPOS FAILURE
                PERFORM AssertOk
        END-EVALUATE

        CALL "JsonParse-Comma" USING JSONBUF JSONPOS FAILURE
        IF FAILURE > 0 EXIT PERFORM END-IF
    END-PERFORM

    CALL "JsonParse-ObjectEnd" USING JSONBUF JSONPOS FAILURE
    PERFORM AssertOk

    COPY ASSERT REPLACING COND BY ==MAX-STACK-SIZE > 0==, MSG BY =="CG-Items-Register: Missing max stack size"==.
    CALL "Codegen-WriteInt" USING MAX-STACK-SIZE LK-BUFFER LK-LENGTH
    .

AssertOk.
    COPY ASSERT REPLACING COND BY ==FAILURE = 0==, MSG BY =="CG-Items-Register: Failed to parse JSON"==.
    .

END PROGRAM CG-Items-Register.
