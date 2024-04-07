*> --- Test: decode.cob ---
IDENTIFICATION DIVISION.
PROGRAM-ID. Test-Decode.

PROCEDURE DIVISION.
    DISPLAY "Test: decode.cob"
    CALL "Test-Decode-VarInt"
    CALL "Test-Decode-Long"
    CALL "Test-Decode-Double"
    CALL "Test-Decode-Float"
    GOBACK.

    *> --- Test: Decode-VarInt ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Test-Decode-VarInt.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        01 BUFFER       PIC X(10).
        01 BUFFERPOS    BINARY-LONG UNSIGNED.
        01 RESULT       BINARY-LONG.

    PROCEDURE DIVISION.
        DISPLAY "  Test: Decode-VarInt".
    OneByte0.
        DISPLAY "    Case: 0x00 - " WITH NO ADVANCING
        MOVE X"00" TO BUFFER
        MOVE 1 TO BUFFERPOS
        CALL "Decode-VarInt" USING BUFFER BUFFERPOS RESULT
        IF RESULT = 0 AND BUFFERPOS = 2
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    OneByte1.
        DISPLAY "    Case: 0x01 = 1 - " WITH NO ADVANCING
        MOVE X"01" TO BUFFER
        MOVE 1 TO BUFFERPOS
        CALL "Decode-VarInt" USING BUFFER BUFFERPOS RESULT
        IF RESULT = 1 AND BUFFERPOS = 2
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    OneByte127.
        DISPLAY "    Case: 0x7F = 127 - " WITH NO ADVANCING
        MOVE X"7F" TO BUFFER
        MOVE 1 TO BUFFERPOS
        CALL "Decode-VarInt" USING BUFFER BUFFERPOS RESULT
        IF RESULT = 127 AND BUFFERPOS = 2
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    FiveByte.
        DISPLAY "    Case: 0xFF 0x80 0x80 0x80 0x00 = 127 - " WITH NO ADVANCING
        MOVE X"FF80808000" TO BUFFER
        MOVE 1 TO BUFFERPOS
        CALL "Decode-VarInt" USING BUFFER BUFFERPOS RESULT
        IF RESULT = 127 AND BUFFERPOS = 6
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    SixByte.
        *> Note: Six bytes is illegal for a VarInt, so the last "continue" bit should be ignored.
        DISPLAY "    Case: 0xFF 0x80 0x80 0x80 0x80 0x00 = 127 - " WITH NO ADVANCING
        MOVE X"FF8080808000" TO BUFFER
        MOVE 1 TO BUFFERPOS
        CALL "Decode-VarInt" USING BUFFER BUFFERPOS RESULT
        IF RESULT = 127 AND BUFFERPOS = 6
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    IntMax.
        DISPLAY "    Case: 0xFF 0xFF 0xFF 0xFF 0x07 = 2147483647 - " WITH NO ADVANCING
        MOVE X"FFFFFFFF07" TO BUFFER
        MOVE 1 TO BUFFERPOS
        CALL "Decode-VarInt" USING BUFFER BUFFERPOS RESULT
        IF RESULT = 2147483647 AND BUFFERPOS = 6
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    Negative1.
        DISPLAY "    Case: 0xFF 0xFF 0xFF 0xFF 0x0F = -1 - " WITH NO ADVANCING
        MOVE X"FFFFFFFF0F" TO BUFFER
        MOVE 1 TO BUFFERPOS
        CALL "Decode-VarInt" USING BUFFER BUFFERPOS RESULT
        IF RESULT = -1 AND BUFFERPOS = 6
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    Negative2.
        DISPLAY "    Case: 0xFE 0xFF 0xFF 0xFF 0x0F = -2 - " WITH NO ADVANCING
        MOVE X"FEFFFFFF0F" TO BUFFER
        MOVE 1 TO BUFFERPOS
        CALL "Decode-VarInt" USING BUFFER BUFFERPOS RESULT
        IF RESULT = -2 AND BUFFERPOS = 6
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    NegativeMax.
        DISPLAY "    Case: 0x80 0x80 0x80 0x80 0x08 = -2147483648 - " WITH NO ADVANCING
        MOVE X"8080808008" TO BUFFER
        MOVE 1 TO BUFFERPOS
        CALL "Decode-VarInt" USING BUFFER BUFFERPOS RESULT
        IF RESULT = -2147483648 AND BUFFERPOS = 6
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.

        GOBACK.

    END PROGRAM Test-Decode-VarInt.

    *> --- Test: Decode-Long ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Test-Decode-Long.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        01 BUFFER       PIC X(10).
        01 BUFFERPOS    BINARY-LONG UNSIGNED.
        01 RESULT       BINARY-LONG-LONG.

    PROCEDURE DIVISION.
        DISPLAY "  Test: Decode-Long".
    Long0.
        DISPLAY "    Case: 0 - " WITH NO ADVANCING
        MOVE X"0000000000000000" TO BUFFER
        MOVE 1 TO BUFFERPOS
        CALL "Decode-Long" USING BUFFER BUFFERPOS RESULT
        IF RESULT = 0 AND BUFFERPOS = 9
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    Long1.
        DISPLAY "    Case: 1 - " WITH NO ADVANCING
        MOVE X"0000000000000001" TO BUFFER
        MOVE 1 TO BUFFERPOS
        CALL "Decode-Long" USING BUFFER BUFFERPOS RESULT
        IF RESULT = 1 AND BUFFERPOS = 9
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    PositiveMax.
        DISPLAY "    Case: 9223372036854775807 - " WITH NO ADVANCING
        MOVE X"7FFFFFFFFFFFFFFF" TO BUFFER
        MOVE 1 TO BUFFERPOS
        CALL "Decode-Long" USING BUFFER BUFFERPOS RESULT
        IF RESULT = 9223372036854775807 AND BUFFERPOS = 9
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    Negative1.
        DISPLAY "    Case: -1 - " WITH NO ADVANCING
        MOVE X"FFFFFFFFFFFFFFFF" TO BUFFER
        MOVE 1 TO BUFFERPOS
        CALL "Decode-Long" USING BUFFER BUFFERPOS RESULT
        IF RESULT = -1 AND BUFFERPOS = 9
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    NegativeMax.
        DISPLAY "    Case: -9223372036854775808 - " WITH NO ADVANCING
        MOVE X"8000000000000000" TO BUFFER
        MOVE 1 TO BUFFERPOS
        CALL "Decode-Long" USING BUFFER BUFFERPOS RESULT
        *> Unfortunately in this case, the C compiler doesn't like the literal -9223372036854775808.
        *> So instead, we add 1 to the result and compare it to (-9223372036854775808 + 1).
        ADD 1 TO RESULT
        IF RESULT = -9223372036854775807 AND BUFFERPOS = 9
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.

        GOBACK.

    END PROGRAM Test-Decode-Long.

    *> --- Test: Decode-Double ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Test-Decode-Double.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        01 BUFFER       PIC X(10).
        01 BUFFERPOS    BINARY-LONG UNSIGNED.
        01 RESULT       FLOAT-LONG.

    PROCEDURE DIVISION.
        DISPLAY "  Test: Decode-Double".
    PositiveZero.
        DISPLAY "    Case: +0.0 - " WITH NO ADVANCING
        MOVE X"0000000000000000" TO BUFFER
        MOVE 1 TO BUFFERPOS
        CALL "Decode-Double" USING BUFFER BUFFERPOS RESULT
        IF RESULT = 0.0 AND BUFFERPOS = 9
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    NegativeZero.
        DISPLAY "    Case: -0.0 - " WITH NO ADVANCING
        MOVE X"8000000000000000" TO BUFFER
        MOVE 1 TO BUFFERPOS
        CALL "Decode-Double" USING BUFFER BUFFERPOS RESULT
        IF RESULT = -0.0 AND BUFFERPOS = 9
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    Positive1.
        DISPLAY "    Case: +1.0 - " WITH NO ADVANCING
        MOVE X"3FF0000000000000" TO BUFFER
        MOVE 1 TO BUFFERPOS
        CALL "Decode-Double" USING BUFFER BUFFERPOS RESULT
        IF RESULT = 1.0 AND BUFFERPOS = 9
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    Negative1.
        DISPLAY "    Case: -1.0 - " WITH NO ADVANCING
        MOVE X"BFF0000000000000" TO BUFFER
        MOVE 1 TO BUFFERPOS
        CALL "Decode-Double" USING BUFFER BUFFERPOS RESULT
        IF RESULT = -1.0 AND BUFFERPOS = 9
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    PositiveMax.
        DISPLAY "    Case: +1.79769E+308 - " WITH NO ADVANCING
        MOVE X"7FEFFFFFFFFFFFFF" TO BUFFER
        MOVE 1 TO BUFFERPOS
        CALL "Decode-Double" USING BUFFER BUFFERPOS RESULT
        IF RESULT = 1.79769313486231570814527423731704357E+308 AND BUFFERPOS = 9
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    NegativeMax.
        DISPLAY "    Case: -1.79769E+308 - " WITH NO ADVANCING
        MOVE X"FFEFFFFFFFFFFFFF" TO BUFFER
        MOVE 1 TO BUFFERPOS
        CALL "Decode-Double" USING BUFFER BUFFERPOS RESULT
        IF RESULT = -1.79769313486231570814527423731704357E+308 AND BUFFERPOS = 9
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.

        GOBACK.

    END PROGRAM Test-Decode-Double.

    *> --- Test: Decode-Float ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Test-Decode-Float.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        01 BUFFER       PIC X(10).
        01 BUFFERPOS    BINARY-LONG UNSIGNED.
        01 RESULT       FLOAT-SHORT.

    PROCEDURE DIVISION.
        DISPLAY "  Test: Decode-Float".
    PositiveZero.
        DISPLAY "    Case: +0.0 - " WITH NO ADVANCING
        MOVE X"00000000" TO BUFFER
        MOVE 1 TO BUFFERPOS
        CALL "Decode-Float" USING BUFFER BUFFERPOS RESULT
        IF RESULT = 0.0 AND BUFFERPOS = 5
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    NegativeZero.
        DISPLAY "    Case: -0.0 - " WITH NO ADVANCING
        MOVE X"80000000" TO BUFFER
        MOVE 1 TO BUFFERPOS
        CALL "Decode-Float" USING BUFFER BUFFERPOS RESULT
        IF RESULT = -0.0 AND BUFFERPOS = 5
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    Positive1.
        DISPLAY "    Case: +1.0 - " WITH NO ADVANCING
        MOVE X"3F800000" TO BUFFER
        MOVE 1 TO BUFFERPOS
        CALL "Decode-Float" USING BUFFER BUFFERPOS RESULT
        IF RESULT = 1.0 AND BUFFERPOS = 5
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    Negative1.
        DISPLAY "    Case: -1.0 - " WITH NO ADVANCING
        MOVE X"BF800000" TO BUFFER
        MOVE 1 TO BUFFERPOS
        CALL "Decode-Float" USING BUFFER BUFFERPOS RESULT
        IF RESULT = -1.0 AND BUFFERPOS = 5
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    PositiveMax.
        DISPLAY "    Case: +3.40282E+38 - " WITH NO ADVANCING
        MOVE X"7F7FFFFF" TO BUFFER
        MOVE 1 TO BUFFERPOS
        CALL "Decode-Float" USING BUFFER BUFFERPOS RESULT
        IF RESULT = 3.40282346638528859811704183484516925E+38 AND BUFFERPOS = 5
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    NegativeMax.
        DISPLAY "    Case: -3.40282E+38 - " WITH NO ADVANCING
        MOVE X"FF7FFFFF" TO BUFFER
        MOVE 1 TO BUFFERPOS
        CALL "Decode-Float" USING BUFFER BUFFERPOS RESULT
        IF RESULT = -3.40282346638528859811704183484516925E+38 AND BUFFERPOS = 5
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.

        GOBACK.

    END PROGRAM Test-Decode-Float.

END PROGRAM Test-Decode.
