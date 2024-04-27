*> --- Test: encode.cob ---
IDENTIFICATION DIVISION.
PROGRAM-ID. Test-Encode.

PROCEDURE DIVISION.
    DISPLAY "Test: encode.cob"
    CALL "Test-Encode-UnsignedShort"
    CALL "Test-Encode-UnsignedInt"
    CALL "Test-Encode-VarInt"
    CALL "Test-Encode-GetVarIntLength"
    CALL "Test-Encode-UnsignedLong"
    CALL "Test-Encode-Double"
    CALL "Test-Encode-Float"
    CALL "Test-Encode-Angle"
    CALL "Test-Encode-Position"
    GOBACK.

    *> --- Test: Encode-UnsignedShort ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Test-Encode-UnsignedShort.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        01 VALUE-IN     BINARY-SHORT UNSIGNED.
        01 BUFFER       PIC X(10).
        01 BUFFERLEN    BINARY-LONG UNSIGNED.

    PROCEDURE DIVISION.
        DISPLAY "  Test: Encode-UnsignedShort".
    Short0.
        DISPLAY "    Case: 0 - " WITH NO ADVANCING
        MOVE 0 TO VALUE-IN
        CALL "Encode-UnsignedShort" USING VALUE-IN BUFFER BUFFERLEN
        IF BUFFER = X"0000" AND BUFFERLEN = 2
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    Short1.
        DISPLAY "    Case: 1 - " WITH NO ADVANCING
        MOVE 1 TO VALUE-IN
        CALL "Encode-UnsignedShort" USING VALUE-IN BUFFER BUFFERLEN
        IF BUFFER = X"0001" AND BUFFERLEN = 2
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    ShortMax.
        DISPLAY "    Case: 65535 - " WITH NO ADVANCING
        MOVE 65535 TO VALUE-IN
        CALL "Encode-UnsignedShort" USING VALUE-IN BUFFER BUFFERLEN
        IF BUFFER = X"FFFF" AND BUFFERLEN = 2
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.

        GOBACK.

    END PROGRAM Test-Encode-UnsignedShort.

    *> --- Test: Encode-UnsignedInt ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Test-Encode-UnsignedInt.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        01 VALUE-IN     BINARY-LONG UNSIGNED.
        01 BUFFER       PIC X(10).
        01 BUFFERLEN    BINARY-LONG UNSIGNED.

    PROCEDURE DIVISION.
        DISPLAY "  Test: Encode-UnsignedInt".
    Int0.
        DISPLAY "    Case: 0 - " WITH NO ADVANCING
        MOVE 0 TO VALUE-IN
        CALL "Encode-UnsignedInt" USING VALUE-IN BUFFER BUFFERLEN
        IF BUFFER = X"00000000" AND BUFFERLEN = 4
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    Int1.
        DISPLAY "    Case: 1 - " WITH NO ADVANCING
        MOVE 1 TO VALUE-IN
        CALL "Encode-UnsignedInt" USING VALUE-IN BUFFER BUFFERLEN
        IF BUFFER = X"00000001" AND BUFFERLEN = 4
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    IntMax.
        DISPLAY "    Case: 4294967295 - " WITH NO ADVANCING
        MOVE 4294967295 TO VALUE-IN
        CALL "Encode-UnsignedInt" USING VALUE-IN BUFFER BUFFERLEN
        IF BUFFER = X"FFFFFFFF" AND BUFFERLEN = 4
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.

        GOBACK.

    END PROGRAM Test-Encode-UnsignedInt.

    *> --- Test: Encode-VarInt ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Test-Encode-VarInt.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        01 VALUE-IN     BINARY-LONG.
        01 BUFFER       PIC X(10).
        01 BUFFERLEN    BINARY-LONG UNSIGNED.

    PROCEDURE DIVISION.
        DISPLAY "  Test: Encode-VarInt".
    Int0.
        DISPLAY "    Case: 0 - " WITH NO ADVANCING
        MOVE 0 TO VALUE-IN
        CALL "Encode-VarInt" USING VALUE-IN BUFFER BUFFERLEN
        IF BUFFER = X"00" AND BUFFERLEN = 1
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    Int1.
        DISPLAY "    Case: 1 - " WITH NO ADVANCING
        MOVE 1 TO VALUE-IN
        CALL "Encode-VarInt" USING VALUE-IN BUFFER BUFFERLEN
        IF BUFFER = X"01" AND BUFFERLEN = 1
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    Int127.
        DISPLAY "    Case: 127 - " WITH NO ADVANCING
        MOVE 127 TO VALUE-IN
        CALL "Encode-VarInt" USING VALUE-IN BUFFER BUFFERLEN
        IF BUFFER = X"7F" AND BUFFERLEN = 1
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    Int128.
        DISPLAY "    Case: 128 - " WITH NO ADVANCING
        MOVE 128 TO VALUE-IN
        CALL "Encode-VarInt" USING VALUE-IN BUFFER BUFFERLEN
        IF BUFFER = X"8001" AND BUFFERLEN = 2
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    Int255.
        DISPLAY "    Case: 255 - " WITH NO ADVANCING
        MOVE 255 TO VALUE-IN
        CALL "Encode-VarInt" USING VALUE-IN BUFFER BUFFERLEN
        IF BUFFER = X"FF01" AND BUFFERLEN = 2
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    Int25565.
        DISPLAY "    Case: 25565 - " WITH NO ADVANCING
        MOVE 25565 TO VALUE-IN
        CALL "Encode-VarInt" USING VALUE-IN BUFFER BUFFERLEN
        IF BUFFER = X"DDC701" AND BUFFERLEN = 3
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    Int2097151.
        DISPLAY "    Case: 2097151 - " WITH NO ADVANCING
        MOVE 2097151 TO VALUE-IN
        CALL "Encode-VarInt" USING VALUE-IN BUFFER BUFFERLEN
        IF BUFFER = X"FFFF7F" AND BUFFERLEN = 3
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    IntMax.
        DISPLAY "    Case: 2147483647 - " WITH NO ADVANCING
        MOVE 2147483647 TO VALUE-IN
        CALL "Encode-VarInt" USING VALUE-IN BUFFER BUFFERLEN
        IF BUFFER = X"FFFFFFFF07" AND BUFFERLEN = 5
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    IntNegative1.
        DISPLAY "    Case: -1 - " WITH NO ADVANCING
        MOVE -1 TO VALUE-IN
        CALL "Encode-VarInt" USING VALUE-IN BUFFER BUFFERLEN
        IF BUFFER = X"FFFFFFFF0F" AND BUFFERLEN = 5
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    IntMin.
        DISPLAY "    Case: -2147483648 - " WITH NO ADVANCING
        MOVE -2147483648 TO VALUE-IN
        CALL "Encode-VarInt" USING VALUE-IN BUFFER BUFFERLEN
        IF BUFFER = X"8080808008" AND BUFFERLEN = 5
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.

        GOBACK.

    END PROGRAM Test-Encode-VarInt.

    *> --- Test: Encode-GetVarIntLength ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Test-Encode-GetVarIntLength.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        01 VALUE-IN     BINARY-LONG.
        01 BUFFERLEN    BINARY-LONG UNSIGNED.

    PROCEDURE DIVISION.
        DISPLAY "  Test: Encode-GetVarIntLength".
    Int0.
        DISPLAY "    Case: 0 - " WITH NO ADVANCING
        MOVE 0 TO VALUE-IN
        CALL "Encode-GetVarIntLength" USING VALUE-IN BUFFERLEN
        IF BUFFERLEN = 1
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    Int1.
        DISPLAY "    Case: 1 - " WITH NO ADVANCING
        MOVE 1 TO VALUE-IN
        CALL "Encode-GetVarIntLength" USING VALUE-IN BUFFERLEN
        IF BUFFERLEN = 1
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    Int127.
        DISPLAY "    Case: 127 - " WITH NO ADVANCING
        MOVE 127 TO VALUE-IN
        CALL "Encode-GetVarIntLength" USING VALUE-IN BUFFERLEN
        IF BUFFERLEN = 1
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    Int128.
        DISPLAY "    Case: 128 - " WITH NO ADVANCING
        MOVE 128 TO VALUE-IN
        CALL "Encode-GetVarIntLength" USING VALUE-IN BUFFERLEN
        IF BUFFERLEN = 2
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    Int255.
        DISPLAY "    Case: 255 - " WITH NO ADVANCING
        MOVE 255 TO VALUE-IN
        CALL "Encode-GetVarIntLength" USING VALUE-IN BUFFERLEN
        IF BUFFERLEN = 2
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    Int25565.
        DISPLAY "    Case: 25565 - " WITH NO ADVANCING
        MOVE 25565 TO VALUE-IN
        CALL "Encode-GetVarIntLength" USING VALUE-IN BUFFERLEN
        IF BUFFERLEN = 3
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    IntMax.
        DISPLAY "    Case: 2147483647 - " WITH NO ADVANCING
        MOVE 2147483647 TO VALUE-IN
        CALL "Encode-GetVarIntLength" USING VALUE-IN BUFFERLEN
        IF BUFFERLEN = 5
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    IntNegative1.
        DISPLAY "    Case: -1 - " WITH NO ADVANCING
        MOVE -1 TO VALUE-IN
        CALL "Encode-GetVarIntLength" USING VALUE-IN BUFFERLEN
        IF BUFFERLEN = 5
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    IntMin.
        DISPLAY "    Case: -2147483648 - " WITH NO ADVANCING
        MOVE -2147483648 TO VALUE-IN
        CALL "Encode-GetVarIntLength" USING VALUE-IN BUFFERLEN
        IF BUFFERLEN = 5
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.

        GOBACK.

    END PROGRAM Test-Encode-GetVarIntLength.

    *> --- Test: Encode-UnsignedLong ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Test-Encode-UnsignedLong.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        01 VALUE-IN     BINARY-LONG-LONG UNSIGNED.
        01 BUFFER       PIC X(10).
        01 BUFFERLEN    BINARY-LONG UNSIGNED.

    PROCEDURE DIVISION.
        DISPLAY "  Test: Encode-UnsignedLong".
    Long0.
        DISPLAY "    Case: 0 - " WITH NO ADVANCING
        MOVE 0 TO VALUE-IN
        CALL "Encode-UnsignedLong" USING VALUE-IN BUFFER BUFFERLEN
        IF BUFFER = X"0000000000000000" AND BUFFERLEN = 8
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    Long1.
        DISPLAY "    Case: 1 - " WITH NO ADVANCING
        MOVE 1 TO VALUE-IN
        CALL "Encode-UnsignedLong" USING VALUE-IN BUFFER BUFFERLEN
        IF BUFFER = X"0000000000000001" AND BUFFERLEN = 8
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    Long25565.
        DISPLAY "    Case: 25565 - " WITH NO ADVANCING
        MOVE 25565 TO VALUE-IN
        CALL "Encode-UnsignedLong" USING VALUE-IN BUFFER BUFFERLEN
        IF BUFFER = X"00000000000063DD" AND BUFFERLEN = 8
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    LongMax.
        DISPLAY "    Case: 18446744073709551615 - " WITH NO ADVANCING
        MOVE 18446744073709551615 TO VALUE-IN
        CALL "Encode-UnsignedLong" USING VALUE-IN BUFFER BUFFERLEN
        IF BUFFER = X"FFFFFFFFFFFFFFFF" AND BUFFERLEN = 8
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.

        GOBACK.

    END PROGRAM Test-Encode-UnsignedLong.

    *> --- Test: Encode-Double ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Test-Encode-Double.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        01 VALUE-IN     FLOAT-LONG.
        01 BUFFER       PIC X(10).
        01 BUFFERLEN    BINARY-LONG UNSIGNED.

    PROCEDURE DIVISION.
        DISPLAY "  Test: Encode-Double".
    PositiveZero.
        DISPLAY "    Case: +0.0 - " WITH NO ADVANCING
        MOVE 0.0 TO VALUE-IN
        CALL "Encode-Double" USING VALUE-IN BUFFER BUFFERLEN
        IF BUFFER = X"0000000000000000" AND BUFFERLEN = 8
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    NegativeZero.
        *> Note: I've found no way to differentiate +0.0 and -0.0 in COBOL, so they're encoded the same.
        DISPLAY "    Case: -0.0 - " WITH NO ADVANCING
        MOVE -0.0 TO VALUE-IN
        CALL "Encode-Double" USING VALUE-IN BUFFER BUFFERLEN
        IF BUFFER = X"0000000000000000" AND BUFFERLEN = 8
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    PositiveInfinity.
        *> Note: FLOAT-INFINITY is not supported in GnuCOBOL.
        DISPLAY "    Case: +Infinity - " WITH NO ADVANCING
        DISPLAY "SKIP".
    NegativeInfinity.
        *> Note: FLOAT-INFINITY is not supported in GnuCOBOL.
        DISPLAY "    Case: -Infinity - " WITH NO ADVANCING
        DISPLAY "SKIP".
    NaN.
        *> Note: FLOAT-NOT-A-NUMBER is not supported in GnuCOBOL.
        DISPLAY "    Case: NaN - " WITH NO ADVANCING
        DISPLAY "SKIP".
    Positive1.
        DISPLAY "    Case: +1.0 - " WITH NO ADVANCING
        MOVE 1.0 TO VALUE-IN
        CALL "Encode-Double" USING VALUE-IN BUFFER BUFFERLEN
        IF BUFFER = X"3FF0000000000000" AND BUFFERLEN = 8
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    Negative1.
        DISPLAY "    Case: -1.0 - " WITH NO ADVANCING
        MOVE -1.0 TO VALUE-IN
        CALL "Encode-Double" USING VALUE-IN BUFFER BUFFERLEN
        IF BUFFER = X"BFF0000000000000" AND BUFFERLEN = 8
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    PositiveDecimal.
        DISPLAY "    Case: +12.125 - " WITH NO ADVANCING
        MOVE 12.125 TO VALUE-IN
        CALL "Encode-Double" USING VALUE-IN BUFFER BUFFERLEN
        IF BUFFER = X"4028400000000000" AND BUFFERLEN = 8
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    NegativeDecimal.
        DISPLAY "    Case: -12.125 - " WITH NO ADVANCING
        MOVE -12.125 TO VALUE-IN
        CALL "Encode-Double" USING VALUE-IN BUFFER BUFFERLEN
        IF BUFFER = X"C028400000000000" AND BUFFERLEN = 8
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    PositiveMax.
        DISPLAY "    Case: +1.79769E+308 - " WITH NO ADVANCING
        MOVE 1.79769313486231570814527423731704357E+308 TO VALUE-IN
        CALL "Encode-Double" USING VALUE-IN BUFFER BUFFERLEN
        IF BUFFER = X"7FEFFFFFFFFFFFFF" AND BUFFERLEN = 8
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    NegativeMax.
        DISPLAY "    Case: -1.79769E+308 - " WITH NO ADVANCING
        MOVE -1.79769313486231570814527423731704357E+308 TO VALUE-IN
        CALL "Encode-Double" USING VALUE-IN BUFFER BUFFERLEN
        IF BUFFER = X"FFEFFFFFFFFFFFFF" AND BUFFERLEN = 8
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.

        GOBACK.

    END PROGRAM Test-Encode-Double.

    *> --- Test: Encode-Float ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Test-Encode-Float.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        01 VALUE-IN     FLOAT-SHORT.
        01 BUFFER       PIC X(10).
        01 BUFFERLEN    BINARY-LONG UNSIGNED.

    PROCEDURE DIVISION.
        DISPLAY "  Test: Encode-Float".
    PositiveZero.
        DISPLAY "    Case: +0.0 - " WITH NO ADVANCING
        MOVE 0.0 TO VALUE-IN
        CALL "Encode-Float" USING VALUE-IN BUFFER BUFFERLEN
        IF BUFFER = X"00000000" AND BUFFERLEN = 4
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    NegativeZero.
        *> Note: I've found no way to differentiate +0.0 and -0.0 in COBOL, so they're encoded the same.
        DISPLAY "    Case: -0.0 - " WITH NO ADVANCING
        MOVE -0.0 TO VALUE-IN
        CALL "Encode-Float" USING VALUE-IN BUFFER BUFFERLEN
        IF BUFFER = X"00000000" AND BUFFERLEN = 4
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    PositiveInfinity.
        *> Note: FLOAT-INFINITY is not supported in GnuCOBOL.
        DISPLAY "    Case: +Infinity - " WITH NO ADVANCING
        DISPLAY "SKIP".
    NegativeInfinity.
        *> Note: FLOAT-INFINITY is not supported in GnuCOBOL.
        DISPLAY "    Case: -Infinity - " WITH NO ADVANCING
        DISPLAY "SKIP".
    NaN.
        *> Note: FLOAT-NOT-A-NUMBER is not supported in GnuCOBOL.
        DISPLAY "    Case: NaN - " WITH NO ADVANCING
        DISPLAY "SKIP".
    Positive1.
        DISPLAY "    Case: +1.0 - " WITH NO ADVANCING
        MOVE 1.0 TO VALUE-IN
        CALL "Encode-Float" USING VALUE-IN BUFFER BUFFERLEN
        IF BUFFER = X"3F800000" AND BUFFERLEN = 4
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    Negative1.
        DISPLAY "    Case: -1.0 - " WITH NO ADVANCING
        MOVE -1.0 TO VALUE-IN
        CALL "Encode-Float" USING VALUE-IN BUFFER BUFFERLEN
        IF BUFFER = X"BF800000" AND BUFFERLEN = 4
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    PositiveDecimal.
        DISPLAY "    Case: +12.125 - " WITH NO ADVANCING
        MOVE 12.125 TO VALUE-IN
        CALL "Encode-Float" USING VALUE-IN BUFFER BUFFERLEN
        IF BUFFER = X"41420000" AND BUFFERLEN = 4
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    NegativeDecimal.
        DISPLAY "    Case: -12.125 - " WITH NO ADVANCING
        MOVE -12.125 TO VALUE-IN
        CALL "Encode-Float" USING VALUE-IN BUFFER BUFFERLEN
        IF BUFFER = X"C1420000" AND BUFFERLEN = 4
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    PositiveMax.
        DISPLAY "    Case: +3.40282E+38 - " WITH NO ADVANCING
        MOVE 3.40282346638528859811704183484516925E+38 TO VALUE-IN
        CALL "Encode-Float" USING VALUE-IN BUFFER BUFFERLEN
        IF BUFFER = X"7F7FFFFF" AND BUFFERLEN = 4
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    NegativeMax.
        DISPLAY "    Case: -3.40282E+38 - " WITH NO ADVANCING
        MOVE -3.40282346638528859811704183484516925E+38 TO VALUE-IN
        CALL "Encode-Float" USING VALUE-IN BUFFER BUFFERLEN
        IF BUFFER = X"FF7FFFFF" AND BUFFERLEN = 4
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.

        GOBACK.

    END PROGRAM Test-Encode-Float.

    *> --- Test: Encode-Angle ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Test-Encode-Angle.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        01 VALUE-IN     FLOAT-SHORT.
        01 BUFFER       PIC X(10).
        01 BUFFERLEN    BINARY-LONG UNSIGNED.

    PROCEDURE DIVISION.
        DISPLAY "  Test: Encode-Angle".
    ZeroDeg.
        DISPLAY "    Case: 0.0deg - " WITH NO ADVANCING
        MOVE 0.0 TO VALUE-IN
        CALL "Encode-Angle" USING VALUE-IN BUFFER BUFFERLEN
        IF BUFFER = X"00" AND BUFFERLEN = 1
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    HalfRotation.
        DISPLAY "    Case: 180.0deg - " WITH NO ADVANCING
        MOVE 180.0 TO VALUE-IN
        CALL "Encode-Angle" USING VALUE-IN BUFFER BUFFERLEN
        IF BUFFER = X"80" AND BUFFERLEN = 1
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    Over360.
        DISPLAY "    Case: 560.0deg - " WITH NO ADVANCING
        MOVE 560.0 TO VALUE-IN
        CALL "Encode-Angle" USING VALUE-IN BUFFER BUFFERLEN
        IF BUFFER = X"8E" AND BUFFERLEN = 1
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    NegativeDeg.
        DISPLAY "    Case: -90.0deg - " WITH NO ADVANCING
        MOVE -90.0 TO VALUE-IN
        CALL "Encode-Angle" USING VALUE-IN BUFFER BUFFERLEN
        IF BUFFER = X"C0" AND BUFFERLEN = 1
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.

        GOBACK.

    END PROGRAM Test-Encode-Angle.

    *> --- Test: Encode-Position ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Test-Encode-Position.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        01 VALUE-IN.
            02 VALUE-X  BINARY-LONG.
            02 VALUE-Y  BINARY-LONG.
            02 VALUE-Z  BINARY-LONG.
        01 BUFFER       PIC X(10).
        01 BUFFERLEN    BINARY-LONG UNSIGNED.
        01 HEXSTR       PIC X(20).

    PROCEDURE DIVISION.
        DISPLAY "  Test: Encode-Position".
    AllZero.
        DISPLAY "    Case: (0, 0, 0) - " WITH NO ADVANCING
        MOVE 0 TO VALUE-X
        MOVE 0 TO VALUE-Y
        MOVE 0 TO VALUE-Z
        CALL "Encode-Position" USING VALUE-IN BUFFER BUFFERLEN
        IF BUFFER = X"0000000000000000" AND BUFFERLEN = 8
            DISPLAY "PASS"
        ELSE
            DISPLAY "FAIL"
        END-IF.
    WikiVgExample.
        DISPLAY "    Case: 18357644 831 -20882616 - " WITH NO ADVANCING
        MOVE 18357644 TO VALUE-X
        MOVE 831 TO VALUE-Y
        MOVE -20882616 TO VALUE-Z
        CALL "Encode-Position" USING VALUE-IN BUFFER BUFFERLEN
        IF BUFFER = X"4607632C15B4833F" AND BUFFERLEN = 8
            DISPLAY "PASS"
        ELSE
            CALL "EncodeHexString" USING BUFFER BUFFERLEN HEXSTR
            DISPLAY "FAIL, actual: " HEXSTR(1:BUFFERLEN * 2)
        END-IF.

        GOBACK.

    END PROGRAM Test-Encode-Position.

END PROGRAM Test-Encode.
