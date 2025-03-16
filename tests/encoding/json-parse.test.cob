*> --- Test: json-parse.cob ---
IDENTIFICATION DIVISION.
PROGRAM-ID. Test-JsonParse.

PROCEDURE DIVISION.
    COPY TEST-SUITE REPLACING ==NAME== BY =="encoding/json-parse.cob"==.
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
        COPY TEST-UNIT REPLACING ==NAME== BY =="JsonParse-ObjectStart"==.
    Simple.
        COPY TEST-CASE REPLACING ==NAME== BY =="'    {'"==.
        MOVE "    {" TO STR
        MOVE 1 TO OFFSET
        MOVE 1 TO FLAG
        CALL "JsonParse-ObjectStart" USING STR OFFSET FLAG
        COPY TEST-ASSERT REPLACING COND BY ==OFFSET = 6 AND FLAG = 0==.
    Missing.
        COPY TEST-CASE REPLACING ==NAME== BY =="'   '"==.
        MOVE "   " TO STR
        MOVE 1 TO OFFSET
        MOVE 0 TO FLAG
        CALL "JsonParse-ObjectStart" USING STR OFFSET FLAG
        COPY TEST-ASSERT REPLACING COND BY ==FLAG = 1==.

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
        COPY TEST-UNIT REPLACING ==NAME== BY =="JsonParse-ObjectEnd"==.
    Simple.
        COPY TEST-CASE REPLACING ==NAME== BY =="'    }'"==.
        MOVE "    }" TO STR
        MOVE 1 TO OFFSET
        MOVE 1 TO FLAG
        CALL "JsonParse-ObjectEnd" USING STR OFFSET FLAG
        COPY TEST-ASSERT REPLACING COND BY ==OFFSET = 6 AND FLAG = 0==.
    Missing.
        COPY TEST-CASE REPLACING ==NAME== BY =="'   '"==.
        MOVE "   " TO STR
        MOVE 1 TO OFFSET
        MOVE 0 TO FLAG
        CALL "JsonParse-ObjectEnd" USING STR OFFSET FLAG
        COPY TEST-ASSERT REPLACING COND BY ==FLAG = 1==.

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
        COPY TEST-UNIT REPLACING ==NAME== BY =="JsonParse-ArrayStart"==.
    Simple.
        COPY TEST-CASE REPLACING ==NAME== BY =="'    ['"==.
        MOVE "    [" TO STR
        MOVE 1 TO OFFSET
        MOVE 1 TO FLAG
        CALL "JsonParse-ArrayStart" USING STR OFFSET FLAG
        COPY TEST-ASSERT REPLACING COND BY ==OFFSET = 6 AND FLAG = 0==.
    Missing.
        COPY TEST-CASE REPLACING ==NAME== BY =="'   '"==.
        MOVE "   " TO STR
        MOVE 1 TO OFFSET
        MOVE 0 TO FLAG
        CALL "JsonParse-ArrayStart" USING STR OFFSET FLAG
        COPY TEST-ASSERT REPLACING COND BY ==FLAG = 1==.

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
        COPY TEST-UNIT REPLACING ==NAME== BY =="JsonParse-ArrayEnd"==.
    Simple.
        COPY TEST-CASE REPLACING ==NAME== BY =="'    ]'"==.
        MOVE "    ]" TO STR
        MOVE 1 TO OFFSET
        MOVE 1 TO FLAG
        CALL "JsonParse-ArrayEnd" USING STR OFFSET FLAG
        COPY TEST-ASSERT REPLACING COND BY ==OFFSET = 6 AND FLAG = 0==.
    Missing.
        COPY TEST-CASE REPLACING ==NAME== BY =="'   '"==.
        MOVE "   " TO STR
        MOVE 1 TO OFFSET
        MOVE 0 TO FLAG
        CALL "JsonParse-ArrayEnd" USING STR OFFSET FLAG
        COPY TEST-ASSERT REPLACING COND BY ==FLAG = 1==.

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
        COPY TEST-UNIT REPLACING ==NAME== BY =="JsonParse-Comma"==.
    Simple.
        COPY TEST-CASE REPLACING ==NAME== BY =="'    ,  '"==.
        MOVE "    ,  " TO STR
        MOVE 1 TO OFFSET
        MOVE 1 TO FLAG
        CALL "JsonParse-Comma" USING STR OFFSET FLAG
        COPY TEST-ASSERT REPLACING COND BY ==OFFSET = 6 AND FLAG = 0==.
    Missing.
        COPY TEST-CASE REPLACING ==NAME== BY =="'   '"==.
        MOVE "   " TO STR
        MOVE 1 TO OFFSET
        MOVE 0 TO FLAG
        CALL "JsonParse-Comma" USING STR OFFSET FLAG
        COPY TEST-ASSERT REPLACING COND BY ==FLAG = 1==.

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
        COPY TEST-UNIT REPLACING ==NAME== BY =="JsonParse-String"==.
    Simple.
        COPY TEST-CASE REPLACING ==NAME== BY =="'    ""abc""  '"==.
        MOVE "    ""abc""  " TO STR
        MOVE 1 TO OFFSET
        MOVE 1 TO FLAG
        CALL "JsonParse-String" USING STR OFFSET FLAG RESULT
        COPY TEST-ASSERT REPLACING COND BY ==OFFSET = 10 AND FLAG = 0 AND RESULT = "abc"==.
    Missing.
        COPY TEST-CASE REPLACING ==NAME== BY =="'   '"==.
        MOVE "   " TO STR
        MOVE 1 TO OFFSET
        MOVE 0 TO FLAG
        CALL "JsonParse-String" USING STR OFFSET FLAG RESULT
        COPY TEST-ASSERT REPLACING COND BY ==FLAG = 1==.
    MissingEnd.
        COPY TEST-CASE REPLACING ==NAME== BY =="'    ""abc'"==.
        MOVE SPACES TO STR
        MOVE "    ""abc" TO STR
        MOVE 1 TO OFFSET
        MOVE 0 TO FLAG
        CALL "JsonParse-String" USING STR OFFSET FLAG RESULT
        COPY TEST-ASSERT REPLACING COND BY ==FLAG = 1==.
    EscapedChar.
        COPY TEST-CASE REPLACING ==NAME== BY =="'    ""1\""2\\3\/4\b5\f6\n7\r8\t9""  '"==.
        MOVE "    ""1\""2\\3\/4\b5\f6\n7\r8\t9""  " TO STR
        MOVE 1 TO OFFSET
        MOVE 1 TO FLAG
        CALL "JsonParse-String" USING STR OFFSET FLAG RESULT
        COPY TEST-ASSERT REPLACING COND BY ==OFFSET = 32 AND FLAG = 0 AND RESULT = X"3122325C332F3408350C360A370D380939"==.
    EscapedUnicodeChar.
        *> TODO: test for unsupported unicode characters (i.e., outside the ASCII range)
        COPY TEST-CASE REPLACING ==NAME== BY =="'    ""foo \u002D bar""  '"==.
        MOVE "    ""foo \u002D bar""  " TO STR
        MOVE 1 TO OFFSET
        MOVE 1 TO FLAG
        CALL "JsonParse-String" USING STR OFFSET FLAG RESULT
        COPY TEST-ASSERT REPLACING COND BY ==OFFSET = 21 AND FLAG = 0 AND RESULT = "foo - bar"==.

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
        COPY TEST-UNIT REPLACING ==NAME== BY =="JsonParse-ObjectKey"==.
    Simple.
        COPY TEST-CASE REPLACING ==NAME== BY =="'    ""abc""  :  '"==.
        MOVE "    ""abc""  :  " TO STR
        MOVE 1 TO OFFSET
        MOVE 1 TO FLAG
        CALL "JsonParse-ObjectKey" USING STR OFFSET FLAG RESULT
        COPY TEST-ASSERT REPLACING COND BY ==OFFSET = 13 AND FLAG = 0 AND RESULT = "abc"==.
    Missing.
        COPY TEST-CASE REPLACING ==NAME== BY =="'   '"==.
        MOVE "   " TO STR
        MOVE 1 TO OFFSET
        MOVE 0 TO FLAG
        CALL "JsonParse-ObjectKey" USING STR OFFSET FLAG RESULT
        COPY TEST-ASSERT REPLACING COND BY ==FLAG = 1==.
    MissingEnd.
        COPY TEST-CASE REPLACING ==NAME== BY =="'    ""abc'"==.
        MOVE SPACES TO STR
        MOVE "    ""abc" TO STR
        MOVE 1 TO OFFSET
        MOVE 0 TO FLAG
        CALL "JsonParse-ObjectKey" USING STR OFFSET FLAG RESULT
        COPY TEST-ASSERT REPLACING COND BY ==FLAG = 1==.
    MissingColon.
        COPY TEST-CASE REPLACING ==NAME== BY =="'    ""abc""  '"==.
        MOVE "    ""abc""  " TO STR
        MOVE 1 TO OFFSET
        MOVE 0 TO FLAG
        CALL "JsonParse-ObjectKey" USING STR OFFSET FLAG RESULT
        COPY TEST-ASSERT REPLACING COND BY ==FLAG = 1==.
    Consecutive.
        COPY TEST-CASE REPLACING ==NAME== BY =="'    ""abc"" ""def""  '"==.
        MOVE "    ""abc"" ""def""  " TO STR
        MOVE 1 TO OFFSET
        MOVE 0 TO FLAG
        CALL "JsonParse-ObjectKey" USING STR OFFSET FLAG RESULT
        COPY TEST-ASSERT REPLACING COND BY ==FLAG = 1==.

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
        COPY TEST-UNIT REPLACING ==NAME== BY =="JsonParse-Null"==.
    Simple.
        COPY TEST-CASE REPLACING ==NAME== BY =="'    null  '"==.
        MOVE "    null  " TO STR
        MOVE 1 TO OFFSET
        MOVE 1 TO FLAG
        CALL "JsonParse-Null" USING STR OFFSET FLAG
        COPY TEST-ASSERT REPLACING COND BY ==OFFSET = 9 AND FLAG = 0==.
    BooleanLiteral.
        COPY TEST-CASE REPLACING ==NAME== BY =="'    true  '"==.
        MOVE "    true  " TO STR
        MOVE 1 TO OFFSET
        MOVE 0 TO FLAG
        CALL "JsonParse-Null" USING STR OFFSET FLAG
        COPY TEST-ASSERT REPLACING COND BY ==FLAG = 1==.
    Missing.
        COPY TEST-CASE REPLACING ==NAME== BY =="'   '"==.
        MOVE "   " TO STR
        MOVE 1 TO OFFSET
        MOVE 0 TO FLAG
        CALL "JsonParse-Null" USING STR OFFSET FLAG
        COPY TEST-ASSERT REPLACING COND BY ==FLAG = 1==.

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
        COPY TEST-UNIT REPLACING ==NAME== BY =="JsonParse-Boolean"==.
    TrueValue.
        COPY TEST-CASE REPLACING ==NAME== BY =="'    true  '"==.
        MOVE "    true  " TO STR
        MOVE 1 TO OFFSET
        MOVE 1 TO FLAG
        CALL "JsonParse-Boolean" USING STR OFFSET FLAG RESULT
        COPY TEST-ASSERT REPLACING COND BY ==OFFSET = 9 AND FLAG = 0 AND RESULT = 1==.
    FalseValue.
        COPY TEST-CASE REPLACING ==NAME== BY =="'    false  '"==.
        MOVE "    false  " TO STR
        MOVE 1 TO OFFSET
        MOVE 1 TO FLAG
        CALL "JsonParse-Boolean" USING STR OFFSET FLAG RESULT
        COPY TEST-ASSERT REPLACING COND BY ==OFFSET = 10 AND FLAG = 0 AND RESULT = 0==.
    NullLiteral.
        COPY TEST-CASE REPLACING ==NAME== BY =="'    null  '"==.
        MOVE "    null  " TO STR
        MOVE 1 TO OFFSET
        MOVE 0 TO FLAG
        CALL "JsonParse-Boolean" USING STR OFFSET FLAG RESULT
        COPY TEST-ASSERT REPLACING COND BY ==FLAG = 1==.
    Missing.
        COPY TEST-CASE REPLACING ==NAME== BY =="'   '"==.
        MOVE "   " TO STR
        MOVE 1 TO OFFSET
        MOVE 0 TO FLAG
        CALL "JsonParse-Boolean" USING STR OFFSET FLAG RESULT
        COPY TEST-ASSERT REPLACING COND BY ==FLAG = 1==.

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
        COPY TEST-UNIT REPLACING ==NAME== BY =="JsonParse-Integer"==.
    Simple.
        COPY TEST-CASE REPLACING ==NAME== BY =="'    123  '"==.
        MOVE "    123  " TO STR
        MOVE 1 TO OFFSET
        MOVE 1 TO FLAG
        CALL "JsonParse-Integer" USING STR OFFSET FLAG RESULT
        COPY TEST-ASSERT REPLACING COND BY ==OFFSET = 8 AND FLAG = 0 AND RESULT = 123==.
    Missing.
        COPY TEST-CASE REPLACING ==NAME== BY =="'   '"==.
        MOVE "   " TO STR
        MOVE 1 TO OFFSET
        MOVE 0 TO FLAG
        CALL "JsonParse-Integer" USING STR OFFSET FLAG RESULT
        COPY TEST-ASSERT REPLACING COND BY ==FLAG = 1==.
    NegativeInt.
        COPY TEST-CASE REPLACING ==NAME== BY =="'    -123  '"==.
        MOVE "    -123  " TO STR
        MOVE 1 TO OFFSET
        MOVE 1 TO FLAG
        CALL "JsonParse-Integer" USING STR OFFSET FLAG RESULT
        COPY TEST-ASSERT REPLACING COND BY ==OFFSET = 9 AND FLAG = 0 AND RESULT = -123==.
    MinusOnly.
        COPY TEST-CASE REPLACING ==NAME== BY =="'    -  '"==.
        MOVE "    -  " TO STR
        MOVE 1 TO OFFSET
        MOVE 0 TO FLAG
        CALL "JsonParse-Integer" USING STR OFFSET FLAG RESULT
        COPY TEST-ASSERT REPLACING COND BY ==FLAG = 1==.

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
        COPY TEST-UNIT REPLACING ==NAME== BY =="JsonParse-Float"==.
    FloatZero.
        COPY TEST-CASE REPLACING ==NAME== BY =="'  0.0  '"==.
        MOVE "  0.0  " TO STR
        MOVE 1 TO OFFSET
        MOVE 1 TO FLAG
        CALL "JsonParse-Float" USING STR OFFSET FLAG RESULT
        COPY TEST-ASSERT REPLACING COND BY ==OFFSET = 6 AND FLAG = 0 AND RESULT = 0.0==.
    PositiveInt.
        COPY TEST-CASE REPLACING ==NAME== BY =="'  123  '"==.
        MOVE "  123  " TO STR
        MOVE 1 TO OFFSET
        MOVE 1 TO FLAG
        CALL "JsonParse-Float" USING STR OFFSET FLAG RESULT
        COPY TEST-ASSERT REPLACING COND BY ==OFFSET = 6 AND FLAG = 0 AND RESULT = 123==.
    NegativeInt.
        COPY TEST-CASE REPLACING ==NAME== BY =="'  -123  '"==.
        MOVE "  -123  " TO STR
        MOVE 1 TO OFFSET
        MOVE 1 TO FLAG
        CALL "JsonParse-Float" USING STR OFFSET FLAG RESULT
        COPY TEST-ASSERT REPLACING COND BY ==OFFSET = 7 AND FLAG = 0 AND RESULT = -123==.
    PositiveFloat.
        COPY TEST-CASE REPLACING ==NAME== BY =="'  123.456  '"==.
        MOVE "  123.456  " TO STR
        MOVE 1 TO OFFSET
        MOVE 1 TO FLAG
        CALL "JsonParse-Float" USING STR OFFSET FLAG RESULT
        COPY TEST-ASSERT REPLACING COND BY ==OFFSET = 10 AND FLAG = 0 AND RESULT = 123.456==.
    NegativeFloat.
        COPY TEST-CASE REPLACING ==NAME== BY =="'  -123.456  '"==.
        MOVE "  -123.456  " TO STR
        MOVE 1 TO OFFSET
        MOVE 1 TO FLAG
        CALL "JsonParse-Float" USING STR OFFSET FLAG RESULT
        COPY TEST-ASSERT REPLACING COND BY ==OFFSET = 11 AND FLAG = 0 AND RESULT = -123.456==.
    Missing.
        COPY TEST-CASE REPLACING ==NAME== BY =="'  '"==.
        MOVE "  " TO STR
        MOVE 1 TO OFFSET
        MOVE 0 TO FLAG
        CALL "JsonParse-Float" USING STR OFFSET FLAG RESULT
        COPY TEST-ASSERT REPLACING COND BY ==FLAG = 1==.
    ScientificNotationMinus.
        COPY TEST-CASE REPLACING ==NAME== BY =="'  1.23e-4  '"==.
        MOVE "  1.23e-4  " TO STR
        MOVE 1 TO OFFSET
        MOVE 1 TO FLAG
        CALL "JsonParse-Float" USING STR OFFSET FLAG RESULT
        COPY TEST-ASSERT REPLACING COND BY ==OFFSET = 10 AND FLAG = 0 AND RESULT = 1.23e-4==.
    ScientificNotationUpper.
        COPY TEST-CASE REPLACING ==NAME== BY =="'  1.23E-4  '"==.
        MOVE "  1.23E-4  " TO STR
        MOVE 1 TO OFFSET
        MOVE 1 TO FLAG
        CALL "JsonParse-Float" USING STR OFFSET FLAG RESULT
        COPY TEST-ASSERT REPLACING COND BY ==OFFSET = 10 AND FLAG = 0 AND RESULT = 1.23e-4==.
    ScientificNotationPlus.
        COPY TEST-CASE REPLACING ==NAME== BY =="'  1.23e+4  '"==.
        MOVE "  1.23e+4  " TO STR
        MOVE 1 TO OFFSET
        MOVE 1 TO FLAG
        CALL "JsonParse-Float" USING STR OFFSET FLAG RESULT
        COPY TEST-ASSERT REPLACING COND BY ==OFFSET = 10 AND FLAG = 0 AND RESULT = 12300.0==.
    ScientificNotationNegative.
        COPY TEST-CASE REPLACING ==NAME== BY =="'  -1.23e-4  '"==.
        MOVE "  -1.23e-4  " TO STR
        MOVE 1 TO OFFSET
        MOVE 1 TO FLAG
        CALL "JsonParse-Float" USING STR OFFSET FLAG RESULT
        COPY TEST-ASSERT REPLACING COND BY ==OFFSET = 11 AND FLAG = 0 AND RESULT = -1.23e-4==.

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
        COPY TEST-UNIT REPLACING ==NAME== BY =="JsonParse-SkipValue"==.
    PositiveInt.
        COPY TEST-CASE REPLACING ==NAME== BY =="'    123  '"==.
        MOVE "    123  " TO STR
        MOVE 1 TO OFFSET
        MOVE 1 TO FLAG
        CALL "JsonParse-SkipValue" USING STR OFFSET FLAG
        COPY TEST-ASSERT REPLACING COND BY ==OFFSET = 8 AND FLAG = 0==.
    NegativeInt.
        COPY TEST-CASE REPLACING ==NAME== BY =="'    -123  '"==.
        MOVE "    -123  " TO STR
        MOVE 1 TO OFFSET
        MOVE 1 TO FLAG
        CALL "JsonParse-SkipValue" USING STR OFFSET FLAG
        COPY TEST-ASSERT REPLACING COND BY ==OFFSET = 9 AND FLAG = 0==.
    PositiveFloat.
        COPY TEST-CASE REPLACING ==NAME== BY =="'    123.456  '"==.
        MOVE "    123.456  " TO STR
        MOVE 1 TO OFFSET
        MOVE 1 TO FLAG
        CALL "JsonParse-SkipValue" USING STR OFFSET FLAG
        COPY TEST-ASSERT REPLACING COND BY ==OFFSET = 12 AND FLAG = 0==.
    NegativeFloat.
        COPY TEST-CASE REPLACING ==NAME== BY =="'    -123.456  '"==.
        MOVE "    -123.456  " TO STR
        MOVE 1 TO OFFSET
        MOVE 1 TO FLAG
        CALL "JsonParse-SkipValue" USING STR OFFSET FLAG
        COPY TEST-ASSERT REPLACING COND BY ==OFFSET = 13 AND FLAG = 0==.
    ScientificNotation.
        COPY TEST-CASE REPLACING ==NAME== BY =="'    1.23e-4  '"==.
        MOVE "    1.23e-4  " TO STR
        MOVE 1 TO OFFSET
        MOVE 1 TO FLAG
        CALL "JsonParse-SkipValue" USING STR OFFSET FLAG
        COPY TEST-ASSERT REPLACING COND BY ==OFFSET = 12 AND FLAG = 0==.
    StringValue.
        COPY TEST-CASE REPLACING ==NAME== BY =="'    ""abc""  '"==.
        MOVE "    ""abc""  " TO STR
        MOVE 1 TO OFFSET
        MOVE 1 TO FLAG
        CALL "JsonParse-SkipValue" USING STR OFFSET FLAG
        COPY TEST-ASSERT REPLACING COND BY ==OFFSET = 10 AND FLAG = 0==.
    StringEscape.
        COPY TEST-CASE REPLACING ==NAME== BY =="'    ""1\""2\\3\/4\b5\f6\n7\r8\t9""  '"==.
        MOVE "    ""1\""2\\3\/4\b5\f6\n7\r8\t9""  " TO STR
        MOVE 1 TO OFFSET
        MOVE 1 TO FLAG
        CALL "JsonParse-SkipValue" USING STR OFFSET FLAG
        COPY TEST-ASSERT REPLACING COND BY ==OFFSET = 32 AND FLAG = 0==.
    SimpleObject.
        COPY TEST-CASE REPLACING ==NAME== BY =="'    {  }  '"==.
        MOVE "    {  }  " TO STR
        MOVE 1 TO OFFSET
        MOVE 1 TO FLAG
        CALL "JsonParse-SkipValue" USING STR OFFSET FLAG
        COPY TEST-ASSERT REPLACING COND BY ==OFFSET = 9 AND FLAG = 0==.
    ComplexObject.
        COPY TEST-CASE REPLACING ==NAME== BY =="'    {""abc"": 123, ""foo"": { ""bar"": ""baz"" }, ""bool"": true, ""null"": null }  '"==.
        MOVE "    {""abc"": 123, ""foo"": { ""bar"": ""baz"" }, ""bool"": true, ""null"": null }  " TO STR
        MOVE 1 TO OFFSET
        MOVE 1 TO FLAG
        CALL "JsonParse-SkipValue" USING STR OFFSET FLAG
        COPY TEST-ASSERT REPLACING COND BY ==OFFSET = 71 AND FLAG = 0==.
    SimpleArray.
        COPY TEST-CASE REPLACING ==NAME== BY =="'    [  ]  '"==.
        MOVE "    [  ]  " TO STR
        MOVE 1 TO OFFSET
        MOVE 1 TO FLAG
        CALL "JsonParse-SkipValue" USING STR OFFSET FLAG
        COPY TEST-ASSERT REPLACING COND BY ==OFFSET = 9 AND FLAG = 0==.
    NestedArray.
        COPY TEST-CASE REPLACING ==NAME== BY =="'    [ 1,2, [3,   4], 5  ]  '"==.
        MOVE "    [ 1,2, [3,   4], 5  ]  " TO STR
        MOVE 1 TO OFFSET
        MOVE 1 TO FLAG
        CALL "JsonParse-SkipValue" USING STR OFFSET FLAG
        COPY TEST-ASSERT REPLACING COND BY ==OFFSET = 26 AND FLAG = 0==.
    ObjectAndArray.
        COPY TEST-CASE REPLACING ==NAME== BY =="'    {""foo"":[{}, {""bar"":42}]}  '"==.
        MOVE "    {""foo"":[{}, {""bar"":42}]}  " TO STR
        MOVE 1 TO OFFSET
        MOVE 1 TO FLAG
        CALL "JsonParse-SkipValue" USING STR OFFSET FLAG
        COPY TEST-ASSERT REPLACING COND BY ==OFFSET = 29 AND FLAG = 0==.

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
        COPY TEST-UNIT REPLACING ==NAME== BY =="JsonParse-FindValue"==.
    EmptyObj.
        COPY TEST-CASE REPLACING ==NAME== BY =="'    {}'"==.
        MOVE "    {}" TO STR
        MOVE 6 TO OFFSET
        MOVE 0 TO FLAG
        CALL "JsonParse-FindValue" USING STR OFFSET "foo" FLAG
        COPY TEST-ASSERT REPLACING COND BY ==OFFSET = 6 AND FLAG = 1==.
    Found.
        COPY TEST-CASE REPLACING ==NAME== BY =="'    {""foo"": 42}'"==.
        MOVE "    {  ""foo"": 42  }" TO STR
        MOVE 6 TO OFFSET
        MOVE 0 TO FLAG
        CALL "JsonParse-FindValue" USING STR OFFSET "foo" FLAG
        COPY TEST-ASSERT REPLACING COND BY ==OFFSET = 14 AND FLAG = 0==.
    NotFound.
        COPY TEST-CASE REPLACING ==NAME== BY =="'    {""bar"": 42}'"==.
        MOVE "    {  ""bar"": 42  }" TO STR
        MOVE 6 TO OFFSET
        MOVE 0 TO FLAG
        CALL "JsonParse-FindValue" USING STR OFFSET "foo" FLAG
        COPY TEST-ASSERT REPLACING COND BY ==OFFSET = 6 AND FLAG = 1==.
    NestedObj.
        COPY TEST-CASE REPLACING ==NAME== BY =="'    {""bar"": {""foo"": 42}, ""foo"": 37}}'"==.
        MOVE "    {  ""bar"": {""foo"": 42}, ""foo"": 37}" TO STR
        MOVE 6 TO OFFSET
        MOVE 0 TO FLAG
        CALL "JsonParse-FindValue" USING STR OFFSET "foo" FLAG
        COPY TEST-ASSERT REPLACING COND BY ==OFFSET = 34 AND FLAG = 0==.

        GOBACK.

    END PROGRAM Test-JsonParse-FindValue.

END PROGRAM Test-JsonParse.
