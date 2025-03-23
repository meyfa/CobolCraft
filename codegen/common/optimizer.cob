*> --- Codegen-Optimize ---
*> Perform rudimentary static analysis to optimize generated COBOL code.
IDENTIFICATION DIVISION.
PROGRAM-ID. Codegen-Optimize.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 DYN-BUFFER-LEN               BINARY-LONG UNSIGNED.
    01 DYN-BUFFER-ADDRESS           POINTER.
    01 INPUT-LENGTH                 BINARY-LONG UNSIGNED.
    01 OUTPUT-LENGTH                BINARY-LONG UNSIGNED.
    01 INPUT-POS                    BINARY-LONG UNSIGNED.
    01 THIS-LINE.
        02 THIS-LINE-START          BINARY-LONG UNSIGNED.
        02 THIS-LINE-END            BINARY-LONG UNSIGNED.
        02 THIS-LINE-ANALYSIS.
            *> line should be skipped due to analysis of the previous line
            03 THIS-LINE-SKIP       BINARY-CHAR UNSIGNED.
            *> line contains only whitespace
            03 THIS-LINE-EMPTY      BINARY-CHAR UNSIGNED.
            *> line contains only an END-xxx instruction
            03 THIS-LINE-END-INSN   BINARY-CHAR UNSIGNED.
            *> line contains only a MOVE instruction
            03 THIS-LINE-MOVE       BINARY-CHAR UNSIGNED.
            *> the variable being moved from, and to
            03 THIS-LINE-MOVE-FROM  PIC X(64).
            03 THIS-LINE-MOVE-TO    PIC X(64).
    01 PREV-LINE.
        02 PREV-LINE-START          BINARY-LONG UNSIGNED.
        02 PREV-LINE-END            BINARY-LONG UNSIGNED.
        02 PREV-LINE-ANALYSIS.
            03 PREV-LINE-SKIP       BINARY-CHAR UNSIGNED.
            03 PREV-LINE-EMPTY      BINARY-CHAR UNSIGNED.
            03 PREV-LINE-END-INSN   BINARY-CHAR UNSIGNED.
            03 PREV-LINE-MOVE       BINARY-CHAR UNSIGNED.
            03 PREV-LINE-MOVE-FROM  PIC X(64).
            03 PREV-LINE-MOVE-TO    PIC X(64).
    01 IDX                          BINARY-LONG UNSIGNED.
    01 IDX2                         BINARY-LONG UNSIGNED.
LINKAGE SECTION.
    01 LK-INPUT                     PIC X ANY LENGTH.

    01 DYN-BUFFER.
        02 FILLER               PIC X OCCURS UNBOUNDED DEPENDING ON DYN-BUFFER-LEN.

PROCEDURE DIVISION USING LK-INPUT.
    MOVE LENGTH OF LK-INPUT TO INPUT-LENGTH

    *> Assume that optimized output is never longer than the input.
    MOVE INPUT-LENGTH TO DYN-BUFFER-LEN
    ALLOCATE DYN-BUFFER-LEN CHARACTERS RETURNING DYN-BUFFER-ADDRESS
    SET ADDRESS OF DYN-BUFFER TO DYN-BUFFER-ADDRESS

    *> Optimization may lead to new (additional) opportunities for optimization, so repeat until no more optimizations are found.
    PERFORM UNTIL EXIT
        PERFORM Optimize
        IF OUTPUT-LENGTH = INPUT-LENGTH
            EXIT PERFORM
        END-IF
        MOVE DYN-BUFFER(1:OUTPUT-LENGTH) TO LK-INPUT
        MOVE OUTPUT-LENGTH TO INPUT-LENGTH
    END-PERFORM

    FREE DYN-BUFFER-ADDRESS

    GOBACK.

Optimize.
    MOVE 0 TO OUTPUT-LENGTH

    MOVE 1 TO INPUT-POS
    MOVE 1 TO THIS-LINE-START

    *> This uses a sliding context window, where the previous line is not emitted until the next line is read.
    PERFORM VARYING INPUT-POS FROM 1 BY 1 UNTIL INPUT-POS > INPUT-LENGTH
        IF LK-INPUT(INPUT-POS:1) = X"0A"
            MOVE INPUT-POS TO THIS-LINE-END
            PERFORM AnalyzeLine
            IF THIS-LINE-START > 1
                PERFORM OptimizeLine
            END-IF
            MOVE THIS-LINE TO PREV-LINE
            COMPUTE THIS-LINE-START = INPUT-POS + 1
        END-IF
    END-PERFORM

    *> Append the last line.
    MOVE INPUT-LENGTH TO THIS-LINE-END
    PERFORM AnalyzeLine
    PERFORM OptimizeLine
    .

*> Set the current line as previous, then analyze the current line.
AnalyzeLine.
    INITIALIZE THIS-LINE-ANALYSIS
    MOVE 1 TO THIS-LINE-EMPTY

    PERFORM VARYING IDX FROM THIS-LINE-START BY 1 UNTIL IDX > THIS-LINE-END
        EVALUATE LK-INPUT(IDX:1)
            WHEN X"20"
            WHEN X"0A"
                CONTINUE
            WHEN OTHER
                MOVE 0 TO THIS-LINE-EMPTY
                IF LK-INPUT(IDX:1) = "E" AND LK-INPUT(IDX + 1:1) = "N" AND LK-INPUT(IDX + 2:1) = "D" AND LK-INPUT(IDX + 3:1) = "-"
                    MOVE 1 TO THIS-LINE-END-INSN
                END-IF
                IF LK-INPUT(IDX:1) = "M" AND LK-INPUT(IDX + 1:1) = "O" AND LK-INPUT(IDX + 2:1) = "V" AND LK-INPUT(IDX + 3:1) = "E"
                    PERFORM AnalyzeMove
                END-IF
                EXIT PERFORM
        END-EVALUATE
    END-PERFORM
    .

AnalyzeMove.
    *> "MOVE<whitespace>"
    COMPUTE IDX = IDX + 4
    COPY PROC-SKIP-WHITESPACE REPLACING STR BY LK-INPUT, LEN BY THIS-LINE-END.

    *> skip past the " TO "
    PERFORM VARYING IDX2 FROM IDX BY 1 UNTIL IDX2 > THIS-LINE-END
        IF LK-INPUT(IDX2:1) = " " AND LK-INPUT(IDX2 + 1:1) = "T" AND LK-INPUT(IDX2 + 2:1) = "O" AND LK-INPUT(IDX2 + 3:1) = " "
            *> set the MOVE source
            MOVE LK-INPUT(IDX:IDX2 - IDX) TO THIS-LINE-MOVE-FROM
            ADD 4 TO IDX2
            EXIT PERFORM
        END-IF
    END-PERFORM

    MOVE IDX2 TO IDX

    *> subtract whitespace from the end
    PERFORM VARYING IDX2 FROM THIS-LINE-END BY -1 UNTIL IDX2 <= IDX OR NOT (LK-INPUT(IDX2:1) = X"20" OR X"0A")
        CONTINUE
    END-PERFORM

    IF IDX2 > IDX
        MOVE 1 TO THIS-LINE-MOVE
        MOVE LK-INPUT(IDX:IDX2 - IDX + 1) TO THIS-LINE-MOVE-TO
    END-IF
    .

*> Emit an optimized version of the previous line, based on information from the current line.
OptimizeLine.
    *> Do not emit lines marked for skipping.
    IF PREV-LINE-SKIP > 0
        EXIT PARAGRAPH
    END-IF

    *> Do not emit empty lines that are followed by END-xxx instructions.
    IF THIS-LINE-END-INSN = 1 AND PREV-LINE-EMPTY = 1
        EXIT PARAGRAPH
    END-IF

    *> Do not emit MOVE instructions that are followed by MOVE instructions to the same variable.
    IF THIS-LINE-MOVE = 1 AND PREV-LINE-MOVE = 1 AND THIS-LINE-MOVE-TO = PREV-LINE-MOVE-TO
        EXIT PARAGRAPH
    END-IF

    *> Mark the current line as redundant (skip on next iteration) if we encounter:
    *> MOVE <var1> TO <var2>; MOVE <var2> TO <var1>
    IF THIS-LINE-MOVE = 1 AND PREV-LINE-MOVE = 1 AND THIS-LINE-MOVE-TO = PREV-LINE-MOVE-FROM AND THIS-LINE-MOVE-FROM = PREV-LINE-MOVE-TO
        MOVE 1 TO THIS-LINE-SKIP
    END-IF

    PERFORM AppendLineAsIs
    .

AppendLineAsIs.
    STRING LK-INPUT(PREV-LINE-START:PREV-LINE-END - PREV-LINE-START + 1) INTO DYN-BUFFER(OUTPUT-LENGTH + 1:)
    COMPUTE OUTPUT-LENGTH = OUTPUT-LENGTH + PREV-LINE-END - PREV-LINE-START + 1
    .

END PROGRAM Codegen-Optimize.
