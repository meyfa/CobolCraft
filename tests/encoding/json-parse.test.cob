*> --- Test: json-parse.cob ---
IDENTIFICATION DIVISION.
PROGRAM-ID. Test-JsonParse.

PROCEDURE DIVISION.
    DISPLAY "Test: json-parse.cob"
    CALL "Test-JsonParse-ObjectStart"
    CALL "Test-JsonParse-ObjectEnd"
    CALL "Test-JsonParse-ArrayStart"
    CALL "Test-JsonParse-ArrayEnd"
    CALL "Test-JsonParse-Comma"
    CALL "Test-JsonParse-String"
    CALL "Test-JsonParse-ObjectKey"
    CALL "Test-JsonParse-Null"
    CALL "Test-JsonParse-Boolean"
    CALL "Test-JsonParse-Integer"
    CALL "Test-JsonParse-Float"
    CALL "Test-JsonParse-SkipValue"
    CALL "Test-JsonParse-FindValue"
    GOBACK.

    *> --- Test: JsonParse-ObjectStart ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Test-JsonParse-ObjectStart.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        01 STR          PIC X(100).
        01 OFFSET       BINARY-LONG UNSIGNED.
        01 FLAG         BINARY-CHAR UNSIGNED.

    PROCEDURE DIVISION.
        DISPLAY "  Test: JsonParse-ObjectStart".
    Simple.
        DISPLAY "    Case: '    {' - " WITH NO ADVANCING
        MOVE "    {" TO STR
        MOVE 1 TO OFFSET
        MOVE 1 TO FLAG
        CALL "JsonParse-ObjectStart" USING STR OFFSET FLAG
        IF OFFSET = 6 AND FLAG = 0
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    Missing.
        DISPLAY "    Case: '   ' - " WITH NO ADVANCING
        MOVE "   " TO STR
        MOVE 1 TO OFFSET
        MOVE 0 TO FLAG
        CALL "JsonParse-ObjectStart" USING STR OFFSET FLAG
        IF FLAG = 1
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.

        GOBACK.

    END PROGRAM Test-JsonParse-ObjectStart.

    *> --- Test: JsonParse-ObjectEnd ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Test-JsonParse-ObjectEnd.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        01 STR          PIC X(100).
        01 OFFSET       BINARY-LONG UNSIGNED.
        01 FLAG         BINARY-CHAR UNSIGNED.

    PROCEDURE DIVISION.
        DISPLAY "  Test: JsonParse-ObjectEnd".
    Simple.
        DISPLAY "    Case: '    }' - " WITH NO ADVANCING
        MOVE "    }" TO STR
        MOVE 1 TO OFFSET
        MOVE 1 TO FLAG
        CALL "JsonParse-ObjectEnd" USING STR OFFSET FLAG
        IF OFFSET = 6 AND FLAG = 0
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    Missing.
        DISPLAY "    Case: '   ' - " WITH NO ADVANCING
        MOVE "   " TO STR
        MOVE 1 TO OFFSET
        MOVE 0 TO FLAG
        CALL "JsonParse-ObjectEnd" USING STR OFFSET FLAG
        IF FLAG = 1
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.

        GOBACK.

    END PROGRAM Test-JsonParse-ObjectEnd.

    *> --- Test: JsonParse-ArrayStart ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Test-JsonParse-ArrayStart.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        01 STR          PIC X(100).
        01 OFFSET       BINARY-LONG UNSIGNED.
        01 FLAG         BINARY-CHAR UNSIGNED.

    PROCEDURE DIVISION.
        DISPLAY "  Test: JsonParse-ArrayStart".
    Simple.
        DISPLAY "    Case: '    [' - " WITH NO ADVANCING
        MOVE "    [" TO STR
        MOVE 1 TO OFFSET
        MOVE 1 TO FLAG
        CALL "JsonParse-ArrayStart" USING STR OFFSET FLAG
        IF OFFSET = 6 AND FLAG = 0
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    Missing.
        DISPLAY "    Case: '   ' - " WITH NO ADVANCING
        MOVE "   " TO STR
        MOVE 1 TO OFFSET
        MOVE 0 TO FLAG
        CALL "JsonParse-ArrayStart" USING STR OFFSET FLAG
        IF FLAG = 1
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.

        GOBACK.

    END PROGRAM Test-JsonParse-ArrayStart.

    *> --- Test: JsonParse-ArrayEnd ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Test-JsonParse-ArrayEnd.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        01 STR          PIC X(100).
        01 OFFSET       BINARY-LONG UNSIGNED.
        01 FLAG         BINARY-CHAR UNSIGNED.

    PROCEDURE DIVISION.
        DISPLAY "  Test: JsonParse-ArrayEnd".
    Simple.
        DISPLAY "    Case: '    ]' - " WITH NO ADVANCING
        MOVE "    ]" TO STR
        MOVE 1 TO OFFSET
        MOVE 1 TO FLAG
        CALL "JsonParse-ArrayEnd" USING STR OFFSET FLAG
        IF OFFSET = 6 AND FLAG = 0
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    Missing.
        DISPLAY "    Case: '   ' - " WITH NO ADVANCING
        MOVE "   " TO STR
        MOVE 1 TO OFFSET
        MOVE 0 TO FLAG
        CALL "JsonParse-ArrayEnd" USING STR OFFSET FLAG
        IF FLAG = 1
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.

        GOBACK.

    END PROGRAM Test-JsonParse-ArrayEnd.

    *> --- JsonParse-Comma ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Test-JsonParse-Comma.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        01 STR          PIC X(100).
        01 OFFSET       BINARY-LONG UNSIGNED.
        01 FLAG         BINARY-CHAR UNSIGNED.

    PROCEDURE DIVISION.
        DISPLAY "  Test: JsonParse-Comma".
    Simple.
        DISPLAY "    Case: '    ,  ' - " WITH NO ADVANCING
        MOVE "    ,  " TO STR
        MOVE 1 TO OFFSET
        MOVE 1 TO FLAG
        CALL "JsonParse-Comma" USING STR OFFSET FLAG
        IF OFFSET = 6 AND FLAG = 0
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    Missing.
        DISPLAY "    Case: '   ' - " WITH NO ADVANCING
        MOVE "   " TO STR
        MOVE 1 TO OFFSET
        MOVE 0 TO FLAG
        CALL "JsonParse-Comma" USING STR OFFSET FLAG
        IF FLAG = 1
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.

        GOBACK.

    END PROGRAM Test-JsonParse-Comma.

    *> --- Test: JsonParse-String ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Test-JsonParse-String.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        01 STR          PIC X(100).
        01 OFFSET       BINARY-LONG UNSIGNED.
        01 FLAG         BINARY-CHAR UNSIGNED.
        01 RESULT       PIC X(100).

    PROCEDURE DIVISION.
        DISPLAY "  Test: JsonParse-String".
    Simple.
        DISPLAY "    Case: '    ""abc""  ' - " WITH NO ADVANCING
        MOVE "    ""abc""  " TO STR
        MOVE 1 TO OFFSET
        MOVE 1 TO FLAG
        CALL "JsonParse-String" USING STR OFFSET FLAG RESULT
        IF OFFSET = 10 AND FLAG = 0 AND RESULT = "abc"
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    Missing.
        DISPLAY "    Case: '   ' - " WITH NO ADVANCING
        MOVE "   " TO STR
        MOVE 1 TO OFFSET
        MOVE 0 TO FLAG
        CALL "JsonParse-String" USING STR OFFSET FLAG RESULT
        IF FLAG = 1
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    MissingEnd.
        DISPLAY "    Case: '    ""abc' - " WITH NO ADVANCING
        MOVE SPACES TO STR
        MOVE "    ""abc" TO STR
        MOVE 1 TO OFFSET
        MOVE 0 TO FLAG
        CALL "JsonParse-String" USING STR OFFSET FLAG RESULT
        IF FLAG = 1
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    EscapedChar.
        DISPLAY "    Case: '    ""1\""2\\3\/4\b5\f6\n7\r8\t9""  ' - " WITH NO ADVANCING
        MOVE "    ""1\""2\\3\/4\b5\f6\n7\r8\t9""  " TO STR
        MOVE 1 TO OFFSET
        MOVE 1 TO FLAG
        CALL "JsonParse-String" USING STR OFFSET FLAG RESULT
        IF OFFSET = 32 AND FLAG = 0 AND RESULT = X"3122325C332F3408350C360A370D380939"
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    EscapedUnicodeChar.
        *> TODO: test for unsupported unicode characters (i.e., outside the ASCII range)
        DISPLAY "    Case: '    ""foo \u002D bar""  ' - " WITH NO ADVANCING
        MOVE "    ""foo \u002D bar""  " TO STR
        MOVE 1 TO OFFSET
        MOVE 1 TO FLAG
        CALL "JsonParse-String" USING STR OFFSET FLAG RESULT
        IF OFFSET = 21 AND FLAG = 0 AND RESULT = "foo - bar"
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.

        GOBACK.

    END PROGRAM Test-JsonParse-String.

    *> --- Test: JsonParse-ObjectKey ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Test-JsonParse-ObjectKey.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        01 STR          PIC X(100).
        01 OFFSET       BINARY-LONG UNSIGNED.
        01 FLAG         BINARY-CHAR UNSIGNED.
        01 RESULT       PIC X(100).

    PROCEDURE DIVISION.
        DISPLAY "  Test: JsonParse-ObjectKey".
    Simple.
        DISPLAY "    Case: '    ""abc""  :  ' - " WITH NO ADVANCING
        MOVE "    ""abc""  :  " TO STR
        MOVE 1 TO OFFSET
        MOVE 1 TO FLAG
        CALL "JsonParse-ObjectKey" USING STR OFFSET FLAG RESULT
        IF OFFSET = 13 AND FLAG = 0 AND RESULT = "abc"
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    Missing.
        DISPLAY "    Case: '   ' - " WITH NO ADVANCING
        MOVE "   " TO STR
        MOVE 1 TO OFFSET
        MOVE 0 TO FLAG
        CALL "JsonParse-ObjectKey" USING STR OFFSET FLAG RESULT
        IF FLAG = 1
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    MissingEnd.
        DISPLAY "    Case: '    ""abc' - " WITH NO ADVANCING
        MOVE SPACES TO STR
        MOVE "    ""abc" TO STR
        MOVE 1 TO OFFSET
        MOVE 0 TO FLAG
        CALL "JsonParse-ObjectKey" USING STR OFFSET FLAG RESULT
        IF FLAG = 1
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    MissingColon.
        DISPLAY "    Case: '    ""abc""  ' - " WITH NO ADVANCING
        MOVE "    ""abc""  " TO STR
        MOVE 1 TO OFFSET
        MOVE 0 TO FLAG
        CALL "JsonParse-ObjectKey" USING STR OFFSET FLAG RESULT
        IF FLAG = 1
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    Consecutive.
        DISPLAY "    Case: '    ""abc"" ""def""  ' - " WITH NO ADVANCING
        MOVE "    ""abc"" ""def""  " TO STR
        MOVE 1 TO OFFSET
        MOVE 0 TO FLAG
        CALL "JsonParse-ObjectKey" USING STR OFFSET FLAG RESULT
        IF FLAG = 1
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.

        GOBACK.

    END PROGRAM Test-JsonParse-ObjectKey.

    *> --- Test: JsonParse-Null ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Test-JsonParse-Null.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        01 STR          PIC X(100).
        01 OFFSET       BINARY-LONG UNSIGNED.
        01 FLAG         BINARY-CHAR UNSIGNED.

    PROCEDURE DIVISION.
        DISPLAY "  Test: JsonParse-Null".
    Simple.
        DISPLAY "    Case: '    null  ' - " WITH NO ADVANCING
        MOVE "    null  " TO STR
        MOVE 1 TO OFFSET
        MOVE 1 TO FLAG
        CALL "JsonParse-Null" USING STR OFFSET FLAG
        IF OFFSET = 9 AND FLAG = 0
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    BooleanLiteral.
        DISPLAY "    Case: '    true  ' - " WITH NO ADVANCING
        MOVE "    true  " TO STR
        MOVE 1 TO OFFSET
        MOVE 0 TO FLAG
        CALL "JsonParse-Null" USING STR OFFSET FLAG
        IF FLAG = 1
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    Missing.
        DISPLAY "    Case: '   ' - " WITH NO ADVANCING
        MOVE "   " TO STR
        MOVE 1 TO OFFSET
        MOVE 0 TO FLAG
        CALL "JsonParse-Null" USING STR OFFSET FLAG
        IF FLAG = 1
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.

        GOBACK.

    END PROGRAM Test-JsonParse-Null.

    *> --- Test: JsonParse-Boolean ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Test-JsonParse-Boolean.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        01 STR          PIC X(100).
        01 OFFSET       BINARY-LONG UNSIGNED.
        01 FLAG         BINARY-CHAR UNSIGNED.
        01 RESULT       BINARY-CHAR UNSIGNED.

    PROCEDURE DIVISION.
        DISPLAY "  Test: JsonParse-Boolean".
    TrueValue.
        DISPLAY "    Case: '    true  ' - " WITH NO ADVANCING
        MOVE "    true  " TO STR
        MOVE 1 TO OFFSET
        MOVE 1 TO FLAG
        CALL "JsonParse-Boolean" USING STR OFFSET FLAG RESULT
        IF OFFSET = 9 AND FLAG = 0 AND RESULT = 1
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    FalseValue.
        DISPLAY "    Case: '    false  ' - " WITH NO ADVANCING
        MOVE "    false  " TO STR
        MOVE 1 TO OFFSET
        MOVE 1 TO FLAG
        CALL "JsonParse-Boolean" USING STR OFFSET FLAG RESULT
        IF OFFSET = 10 AND FLAG = 0 AND RESULT = 0
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    NullLiteral.
        DISPLAY "    Case: '    null  ' - " WITH NO ADVANCING
        MOVE "    null  " TO STR
        MOVE 1 TO OFFSET
        MOVE 0 TO FLAG
        CALL "JsonParse-Boolean" USING STR OFFSET FLAG RESULT
        IF FLAG = 1
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    Missing.
        DISPLAY "    Case: '   ' - " WITH NO ADVANCING
        MOVE "   " TO STR
        MOVE 1 TO OFFSET
        MOVE 0 TO FLAG
        CALL "JsonParse-Boolean" USING STR OFFSET FLAG RESULT
        IF FLAG = 1
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.

        GOBACK.

    END PROGRAM Test-JsonParse-Boolean.

    *> --- Test: JsonParse-Integer ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Test-JsonParse-Integer.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        01 STR          PIC X(100).
        01 OFFSET       BINARY-LONG UNSIGNED.
        01 FLAG         BINARY-CHAR UNSIGNED.
        01 RESULT       BINARY-LONG.

    PROCEDURE DIVISION.
        DISPLAY "  Test: JsonParse-Integer".
    Simple.
        DISPLAY "    Case: '    123  ' - " WITH NO ADVANCING
        MOVE "    123  " TO STR
        MOVE 1 TO OFFSET
        MOVE 1 TO FLAG
        CALL "JsonParse-Integer" USING STR OFFSET FLAG RESULT
        IF OFFSET = 8 AND FLAG = 0 AND RESULT = 123
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    Missing.
        DISPLAY "    Case: '   ' - " WITH NO ADVANCING
        MOVE "   " TO STR
        MOVE 1 TO OFFSET
        MOVE 0 TO FLAG
        CALL "JsonParse-Integer" USING STR OFFSET FLAG RESULT
        IF FLAG = 1
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    NegativeInt.
        DISPLAY "    Case: '    -123  ' - " WITH NO ADVANCING
        MOVE "    -123  " TO STR
        MOVE 1 TO OFFSET
        MOVE 1 TO FLAG
        CALL "JsonParse-Integer" USING STR OFFSET FLAG RESULT
        IF OFFSET = 9 AND FLAG = 0 AND RESULT = -123
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    MinusOnly.
        DISPLAY "    Case: '    -  ' - " WITH NO ADVANCING
        MOVE "    -  " TO STR
        MOVE 1 TO OFFSET
        MOVE 0 TO FLAG
        CALL "JsonParse-Integer" USING STR OFFSET FLAG RESULT
        IF FLAG = 1
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.

        GOBACK.

    END PROGRAM Test-JsonParse-Integer.

    *> --- Test: JsonParse-Float ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Test-JsonParse-Float.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        01 STR          PIC X(100).
        01 OFFSET       BINARY-LONG UNSIGNED.
        01 FLAG         BINARY-CHAR UNSIGNED.
        01 RESULT       FLOAT-LONG.

    PROCEDURE DIVISION.
        DISPLAY "  Test: JsonParse-Float".
    FloatZero.
        DISPLAY "    Case: '  0.0  ' - " WITH NO ADVANCING
        MOVE "  0.0  " TO STR
        MOVE 1 TO OFFSET
        MOVE 1 TO FLAG
        CALL "JsonParse-Float" USING STR OFFSET FLAG RESULT
        IF OFFSET = 6 AND FLAG = 0 AND RESULT = 0.0
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    PositiveInt.
        DISPLAY "    Case: '  123  ' - " WITH NO ADVANCING
        MOVE "  123  " TO STR
        MOVE 1 TO OFFSET
        MOVE 1 TO FLAG
        CALL "JsonParse-Float" USING STR OFFSET FLAG RESULT
        IF OFFSET = 6 AND FLAG = 0 AND RESULT = 123
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    NegativeInt.
        DISPLAY "    Case: '  -123  ' - " WITH NO ADVANCING
        MOVE "  -123  " TO STR
        MOVE 1 TO OFFSET
        MOVE 1 TO FLAG
        CALL "JsonParse-Float" USING STR OFFSET FLAG RESULT
        IF OFFSET = 7 AND FLAG = 0 AND RESULT = -123
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    PositiveFloat.
        DISPLAY "    Case: '  123.456  ' - " WITH NO ADVANCING
        MOVE "  123.456  " TO STR
        MOVE 1 TO OFFSET
        MOVE 1 TO FLAG
        CALL "JsonParse-Float" USING STR OFFSET FLAG RESULT
        IF OFFSET = 10 AND FLAG = 0 AND RESULT = 123.456
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    NegativeFloat.
        DISPLAY "    Case: '  -123.456  ' - " WITH NO ADVANCING
        MOVE "  -123.456  " TO STR
        MOVE 1 TO OFFSET
        MOVE 1 TO FLAG
        CALL "JsonParse-Float" USING STR OFFSET FLAG RESULT
        IF OFFSET = 11 AND FLAG = 0 AND RESULT = -123.456
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    Missing.
        DISPLAY "    Case: '  ' - " WITH NO ADVANCING
        MOVE "  " TO STR
        MOVE 1 TO OFFSET
        MOVE 0 TO FLAG
        CALL "JsonParse-Float" USING STR OFFSET FLAG RESULT
        IF FLAG = 1
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    ScientificNotationMinus.
        DISPLAY "    Case: '  1.23e-4  ' - " WITH NO ADVANCING
        MOVE "  1.23e-4  " TO STR
        MOVE 1 TO OFFSET
        MOVE 1 TO FLAG
        CALL "JsonParse-Float" USING STR OFFSET FLAG RESULT
        IF OFFSET = 10 AND FLAG = 0 AND RESULT = 1.23e-4
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    ScientificNotationUpper.
        DISPLAY "    Case: '  1.23E-4  ' - " WITH NO ADVANCING
        MOVE "  1.23E-4  " TO STR
        MOVE 1 TO OFFSET
        MOVE 1 TO FLAG
        CALL "JsonParse-Float" USING STR OFFSET FLAG RESULT
        IF OFFSET = 10 AND FLAG = 0 AND RESULT = 1.23e-4
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    ScientificNotationPlus.
        DISPLAY "    Case: '  1.23e+4  ' - " WITH NO ADVANCING
        MOVE "  1.23e+4  " TO STR
        MOVE 1 TO OFFSET
        MOVE 1 TO FLAG
        CALL "JsonParse-Float" USING STR OFFSET FLAG RESULT
        IF OFFSET = 10 AND FLAG = 0 AND RESULT = 12300.0
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    ScientificNotationNegative.
        DISPLAY "    Case: '  -1.23e-4  ' - " WITH NO ADVANCING
        MOVE "  -1.23e-4  " TO STR
        MOVE 1 TO OFFSET
        MOVE 1 TO FLAG
        CALL "JsonParse-Float" USING STR OFFSET FLAG RESULT
        IF OFFSET = 11 AND FLAG = 0 AND RESULT = -1.23e-4
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.

        GOBACK.

    END PROGRAM Test-JsonParse-Float.

    *> --- Test: JsonParse-SkipValue ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Test-JsonParse-SkipValue.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        01 STR          PIC X(100).
        01 OFFSET       BINARY-LONG UNSIGNED.
        01 FLAG         BINARY-CHAR UNSIGNED.

    PROCEDURE DIVISION.
        DISPLAY "  Test: JsonParse-SkipValue".
    PositiveInt.
        DISPLAY "    Case: '    123  ' - " WITH NO ADVANCING
        MOVE "    123  " TO STR
        MOVE 1 TO OFFSET
        MOVE 1 TO FLAG
        CALL "JsonParse-SkipValue" USING STR OFFSET FLAG
        IF OFFSET = 8 AND FLAG = 0
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    NegativeInt.
        DISPLAY "    Case: '    -123  ' - " WITH NO ADVANCING
        MOVE "    -123  " TO STR
        MOVE 1 TO OFFSET
        MOVE 1 TO FLAG
        CALL "JsonParse-SkipValue" USING STR OFFSET FLAG
        IF OFFSET = 9 AND FLAG = 0
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    PositiveFloat.
        DISPLAY "    Case: '    123.456  ' - " WITH NO ADVANCING
        MOVE "    123.456  " TO STR
        MOVE 1 TO OFFSET
        MOVE 1 TO FLAG
        CALL "JsonParse-SkipValue" USING STR OFFSET FLAG
        IF OFFSET = 12 AND FLAG = 0
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    NegativeFloat.
        DISPLAY "    Case: '    -123.456  ' - " WITH NO ADVANCING
        MOVE "    -123.456  " TO STR
        MOVE 1 TO OFFSET
        MOVE 1 TO FLAG
        CALL "JsonParse-SkipValue" USING STR OFFSET FLAG
        IF OFFSET = 13 AND FLAG = 0
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    ScientificNotation.
        DISPLAY "    Case: '    1.23e-4  ' - " WITH NO ADVANCING
        MOVE "    1.23e-4  " TO STR
        MOVE 1 TO OFFSET
        MOVE 1 TO FLAG
        CALL "JsonParse-SkipValue" USING STR OFFSET FLAG
        IF OFFSET = 12 AND FLAG = 0
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    StringValue.
        DISPLAY "    Case: '    ""abc""  ' - " WITH NO ADVANCING
        MOVE "    ""abc""  " TO STR
        MOVE 1 TO OFFSET
        MOVE 1 TO FLAG
        CALL "JsonParse-SkipValue" USING STR OFFSET FLAG
        IF OFFSET = 10 AND FLAG = 0
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    StringEscape.
        DISPLAY "    Case: '    ""1\""2\\3\/4\b5\f6\n7\r8\t9""  ' - " WITH NO ADVANCING
        MOVE "    ""1\""2\\3\/4\b5\f6\n7\r8\t9""  " TO STR
        MOVE 1 TO OFFSET
        MOVE 1 TO FLAG
        CALL "JsonParse-SkipValue" USING STR OFFSET FLAG
        IF OFFSET = 32 AND FLAG = 0
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    SimpleObject.
        DISPLAY "    Case: '    {  }  ' - " WITH NO ADVANCING
        MOVE "    {  }  " TO STR
        MOVE 1 TO OFFSET
        MOVE 1 TO FLAG
        CALL "JsonParse-SkipValue" USING STR OFFSET FLAG
        IF OFFSET = 9 AND FLAG = 0
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    ComplexObject.
        DISPLAY "    Case: '    {""abc"": 123, ""foo"": { ""bar"": ""baz"" }, ""bool"": true, ""null"": null }  ' - " WITH NO ADVANCING
        MOVE "    {""abc"": 123, ""foo"": { ""bar"": ""baz"" }, ""bool"": true, ""null"": null }  " TO STR
        MOVE 1 TO OFFSET
        MOVE 1 TO FLAG
        CALL "JsonParse-SkipValue" USING STR OFFSET FLAG
        IF OFFSET = 71 AND FLAG = 0
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    SimpleArray.
        DISPLAY "    Case: '    [  ]  ' - " WITH NO ADVANCING
        MOVE "    [  ]  " TO STR
        MOVE 1 TO OFFSET
        MOVE 1 TO FLAG
        CALL "JsonParse-SkipValue" USING STR OFFSET FLAG
        IF OFFSET = 9 AND FLAG = 0
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    NestedArray.
        DISPLAY "    Case: '    [ 1,2, [3,   4], 5  ]  ' - " WITH NO ADVANCING
        MOVE "    [ 1,2, [3,   4], 5  ]  " TO STR
        MOVE 1 TO OFFSET
        MOVE 1 TO FLAG
        CALL "JsonParse-SkipValue" USING STR OFFSET FLAG
        IF OFFSET = 26 AND FLAG = 0
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    ObjectAndArray.
        DISPLAY "    Case: '    {""foo"":[{}, {""bar"":42}]}  ' - " WITH NO ADVANCING
        MOVE "    {""foo"":[{}, {""bar"":42}]}  " TO STR
        MOVE 1 TO OFFSET
        MOVE 1 TO FLAG
        CALL "JsonParse-SkipValue" USING STR OFFSET FLAG
        IF OFFSET = 29 AND FLAG = 0
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.

        GOBACK.

    END PROGRAM Test-JsonParse-SkipValue.

    *> --- Test: JsonParse-FindValue ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Test-JsonParse-FindValue.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        01 STR          PIC X(100).
        01 OFFSET       BINARY-LONG UNSIGNED.
        01 FLAG         BINARY-CHAR UNSIGNED.

    PROCEDURE DIVISION.
        DISPLAY "  Test: JsonParse-FindValue".
    EmptyObj.
        DISPLAY "    Case: '    {}' - " WITH NO ADVANCING
        MOVE "    {}" TO STR
        MOVE 6 TO OFFSET
        MOVE 0 TO FLAG
        CALL "JsonParse-FindValue" USING STR OFFSET "foo" FLAG
        IF OFFSET = 6 AND FLAG = 1
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    Found.
        DISPLAY "    Case: '    {""foo"": 42}' - " WITH NO ADVANCING
        MOVE "    {  ""foo"": 42  }" TO STR
        MOVE 6 TO OFFSET
        MOVE 0 TO FLAG
        CALL "JsonParse-FindValue" USING STR OFFSET "foo" FLAG
        IF OFFSET = 14 AND FLAG = 0
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    NotFound.
        DISPLAY "    Case: '    {""bar"": 42}' - " WITH NO ADVANCING
        MOVE "    {  ""bar"": 42  }" TO STR
        MOVE 6 TO OFFSET
        MOVE 0 TO FLAG
        CALL "JsonParse-FindValue" USING STR OFFSET "foo" FLAG
        IF OFFSET = 6 AND FLAG = 1
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    NestedObj.
        DISPLAY "    Case: '    {""bar"": {""foo"": 42}, ""foo"": 37}}' - " WITH NO ADVANCING
        MOVE "    {  ""bar"": {""foo"": 42}, ""foo"": 37}" TO STR
        MOVE 6 TO OFFSET
        MOVE 0 TO FLAG
        CALL "JsonParse-FindValue" USING STR OFFSET "foo" FLAG
        IF OFFSET = 34 AND FLAG = 0
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.

        GOBACK.

    END PROGRAM Test-JsonParse-FindValue.

END PROGRAM Test-JsonParse.
