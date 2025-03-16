*> --- Test: nbt-encode.cob ---
IDENTIFICATION DIVISION.
PROGRAM-ID. Test-NbtEncode.

PROCEDURE DIVISION.
    COPY TEST-SUITE REPLACING ==NAME== BY =="encoding/nbt-encode.cob"==.
    CALL "Test-NbtEncode-Byte"
    CALL "Test-NbtEncode-Short"
    CALL "Test-NbtEncode-Int"
    CALL "Test-NbtEncode-Long"
    CALL "Test-NbtEncode-Float"
    CALL "Test-NbtEncode-Double"
    CALL "Test-NbtEncode-String"
    CALL "Test-NbtEncode-ByteArray"
    CALL "Test-NbtEncode-List"
    CALL "Test-NbtEncode-Compound"
    CALL "Test-NbtEncode-RootCompound"
    CALL "Test-NbtEncode-EndCompound"
    CALL "Test-NbtEncode-IntArray"
    CALL "Test-NbtEncode-LongArray"
    CALL "Test-NbtEncode-UUID"
    GOBACK.

    *> --- Test: NbtEncode-Byte ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Test-NbtEncode-Byte.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        COPY DD-NBT-ENCODER.
        01 BUFFER       PIC X(16).
        01 VALUE-IN     BINARY-CHAR.

    PROCEDURE DIVISION.
        COPY TEST-UNIT REPLACING ==NAME== BY =="Test-NbtEncode-Byte"==.
    UnnamedByte.
        COPY TEST-CASE REPLACING ==NAME== BY =="unnamed byte"==.
        INITIALIZE NBTENC
        MOVE X"FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF" TO BUFFER
        MOVE 3 TO NBTENC-OFFSET
        MOVE 42 TO VALUE-IN
        CALL "NbtEncode-Byte" USING NBTENC BUFFER OMITTED VALUE-IN
        COPY TEST-ASSERT REPLACING COND BY ==BUFFER = X"FFFF012AFFFFFFFFFFFFFFFFFFFFFFFF" AND NBTENC-OFFSET = 5==.
    NamedByte.
        COPY TEST-CASE REPLACING ==NAME== BY =="named byte"==.
        INITIALIZE NBTENC
        MOVE X"FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF" TO BUFFER
        MOVE 3 TO NBTENC-OFFSET
        MOVE 42 TO VALUE-IN
        CALL "NbtEncode-Byte" USING NBTENC BUFFER "Time" VALUE-IN
        COPY TEST-ASSERT REPLACING COND BY ==BUFFER = X"FFFF01000454696D652AFFFFFFFFFFFF" AND NBTENC-OFFSET = 11==.

        GOBACK.

    END PROGRAM Test-NbtEncode-Byte.

    *> --- Test: NbtEncode-Short ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Test-NbtEncode-Short.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        COPY DD-NBT-ENCODER.
        01 BUFFER       PIC X(16).
        01 VALUE-IN     BINARY-SHORT.

    PROCEDURE DIVISION.
        COPY TEST-UNIT REPLACING ==NAME== BY =="Test-NbtEncode-Short"==.
    UnnamedShort.
        COPY TEST-CASE REPLACING ==NAME== BY =="unnamed short"==.
        INITIALIZE NBTENC
        MOVE X"FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF" TO BUFFER
        MOVE 3 TO NBTENC-OFFSET
        MOVE H'0203' TO VALUE-IN
        CALL "NbtEncode-Short" USING NBTENC BUFFER OMITTED VALUE-IN
        COPY TEST-ASSERT REPLACING COND BY ==BUFFER = X"FFFF020203FFFFFFFFFFFFFFFFFFFFFF" AND NBTENC-OFFSET = 6==.
    NamedShort.
        COPY TEST-CASE REPLACING ==NAME== BY =="named short"==.
        INITIALIZE NBTENC
        MOVE X"FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF" TO BUFFER
        MOVE 3 TO NBTENC-OFFSET
        MOVE H'0203' TO VALUE-IN
        CALL "NbtEncode-Short" USING NBTENC BUFFER "Time" VALUE-IN
        COPY TEST-ASSERT REPLACING COND BY ==BUFFER = X"FFFF02000454696D650203FFFFFFFFFF" AND NBTENC-OFFSET = 12==.

        GOBACK.

    END PROGRAM Test-NbtEncode-Short.

    *> --- Test: NbtEncode-Int ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Test-NbtEncode-Int.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        COPY DD-NBT-ENCODER.
        01 BUFFER       PIC X(16).
        01 VALUE-IN     BINARY-LONG.

    PROCEDURE DIVISION.
        COPY TEST-UNIT REPLACING ==NAME== BY =="Test-NbtEncode-Int"==.
    UnnamedInt.
        COPY TEST-CASE REPLACING ==NAME== BY =="unnamed int"==.
        INITIALIZE NBTENC
        MOVE X"FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF" TO BUFFER
        MOVE 3 TO NBTENC-OFFSET
        MOVE H'01020304' TO VALUE-IN
        CALL "NbtEncode-Int" USING NBTENC BUFFER OMITTED VALUE-IN
        COPY TEST-ASSERT REPLACING COND BY ==BUFFER = X"FFFF0301020304FFFFFFFFFFFFFFFFFF" AND NBTENC-OFFSET = 8==.
    NamedInt.
        COPY TEST-CASE REPLACING ==NAME== BY =="named int"==.
        INITIALIZE NBTENC
        MOVE X"FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF" TO BUFFER
        MOVE 3 TO NBTENC-OFFSET
        MOVE H'01020304' TO VALUE-IN
        CALL "NbtEncode-Int" USING NBTENC BUFFER "Time" VALUE-IN
        COPY TEST-ASSERT REPLACING COND BY ==BUFFER = X"FFFF03000454696D6501020304FFFFFF" AND NBTENC-OFFSET = 14==.

        GOBACK.

    END PROGRAM Test-NbtEncode-Int.

    *> --- Test: NbtEncode-Long ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Test-NbtEncode-Long.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        COPY DD-NBT-ENCODER.
        01 BUFFER       PIC X(16).
        01 VALUE-IN     BINARY-LONG-LONG.

    PROCEDURE DIVISION.
        COPY TEST-UNIT REPLACING ==NAME== BY =="Test-NbtEncode-Long"==.
    UnnamedLong.
        COPY TEST-CASE REPLACING ==NAME== BY =="unnamed long"==.
        INITIALIZE NBTENC
        MOVE X"FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF" TO BUFFER
        MOVE 3 TO NBTENC-OFFSET
        MOVE H'0102030405060708' TO VALUE-IN
        CALL "NbtEncode-Long" USING NBTENC BUFFER OMITTED VALUE-IN
        COPY TEST-ASSERT REPLACING COND BY ==BUFFER = X"FFFF040102030405060708FFFFFFFFFF" AND NBTENC-OFFSET = 12==.
    Basic.
        COPY TEST-CASE REPLACING ==NAME== BY =="named long"==.
        INITIALIZE NBTENC
        MOVE X"FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF" TO BUFFER
        MOVE 1 TO NBTENC-OFFSET
        MOVE H'0102030405060708' TO VALUE-IN
        CALL "NbtEncode-Long" USING NBTENC BUFFER "Time" VALUE-IN
        COPY TEST-ASSERT REPLACING COND BY ==BUFFER = X"04000454696D650102030405060708FF" AND NBTENC-OFFSET = 16==.

        GOBACK.

    END PROGRAM Test-NbtEncode-Long.

    *> --- Test: NbtEncode-Float ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Test-NbtEncode-Float.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        COPY DD-NBT-ENCODER.
        01 BUFFER       PIC X(16).
        01 VALUE-IN     FLOAT-SHORT.

    PROCEDURE DIVISION.
        COPY TEST-UNIT REPLACING ==NAME== BY =="Test-NbtEncode-Float"==.
    UnnamedFloat.
        COPY TEST-CASE REPLACING ==NAME== BY =="unnamed float (+12.125)"==.
        INITIALIZE NBTENC
        MOVE X"FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF" TO BUFFER
        MOVE 3 TO NBTENC-OFFSET
        MOVE 12.125 TO VALUE-IN
        CALL "NbtEncode-Float" USING NBTENC BUFFER OMITTED VALUE-IN
        COPY TEST-ASSERT REPLACING COND BY ==BUFFER = X"FFFF0541420000FFFFFFFFFFFFFFFFFF" AND NBTENC-OFFSET = 8==.
    NamedFloat.
        COPY TEST-CASE REPLACING ==NAME== BY =="named float (-12.125)"==.
        INITIALIZE NBTENC
        MOVE X"FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF" TO BUFFER
        MOVE 3 TO NBTENC-OFFSET
        MOVE -12.125 TO VALUE-IN
        CALL "NbtEncode-Float" USING NBTENC BUFFER "Time" VALUE-IN
        COPY TEST-ASSERT REPLACING COND BY ==BUFFER = X"FFFF05000454696D65C1420000FFFFFF" AND NBTENC-OFFSET = 14==.

        GOBACK.

    END PROGRAM Test-NbtEncode-Float.

    *> --- Test: NbtEncode-Double ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Test-NbtEncode-Double.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        COPY DD-NBT-ENCODER.
        01 BUFFER       PIC X(16).
        01 VALUE-IN     FLOAT-LONG.

    PROCEDURE DIVISION.
        COPY TEST-UNIT REPLACING ==NAME== BY =="Test-NbtEncode-Double"==.
    UnnamedDouble.
        COPY TEST-CASE REPLACING ==NAME== BY =="unnamed double (+12.125)"==.
        INITIALIZE NBTENC
        MOVE X"FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF" TO BUFFER
        MOVE 3 TO NBTENC-OFFSET
        MOVE 12.125 TO VALUE-IN
        CALL "NbtEncode-Double" USING NBTENC BUFFER OMITTED VALUE-IN
        COPY TEST-ASSERT REPLACING COND BY ==BUFFER = X"FFFF064028400000000000FFFFFFFFFF" AND NBTENC-OFFSET = 12==.
    NamedDouble.
        COPY TEST-CASE REPLACING ==NAME== BY =="named double (-12.125)"==.
        INITIALIZE NBTENC
        MOVE X"FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF" TO BUFFER
        MOVE 2 TO NBTENC-OFFSET
        MOVE -12.125 TO VALUE-IN
        CALL "NbtEncode-Double" USING NBTENC BUFFER "Time" VALUE-IN
        COPY TEST-ASSERT REPLACING COND BY ==BUFFER = X"FF06000454696D65C028400000000000" AND NBTENC-OFFSET = 17==.

        GOBACK.

    END PROGRAM Test-NbtEncode-Double.

    *> --- Test: NbtEncode-String ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Test-NbtEncode-String.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        COPY DD-NBT-ENCODER.
        01 BUFFER       PIC X(24).
        01 STR          PIC X(10).
        01 STR-LEN      BINARY-LONG UNSIGNED.

    PROCEDURE DIVISION.
        COPY TEST-UNIT REPLACING ==NAME== BY =="Test-NbtEncode-String"==.
    UnnamedEmptyString.
        COPY TEST-CASE REPLACING ==NAME== BY =="unnamed empty string"==.
        INITIALIZE NBTENC
        MOVE X"FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF" TO BUFFER
        MOVE 3 TO NBTENC-OFFSET
        MOVE 0 TO STR-LEN
        CALL "NbtEncode-String" USING NBTENC BUFFER OMITTED STR STR-LEN
        COPY TEST-ASSERT REPLACING COND BY ==BUFFER = X"FFFF080000FFFFFFFFFFFFFFFFFFFFFF" AND NBTENC-OFFSET = 6==.
    NamedString.
        COPY TEST-CASE REPLACING ==NAME== BY =="named non-empty string"==.
        INITIALIZE NBTENC
        MOVE X"FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF" TO BUFFER
        MOVE 3 TO NBTENC-OFFSET
        MOVE "Hello" TO STR
        MOVE 5 TO STR-LEN
        CALL "NbtEncode-String" USING NBTENC BUFFER "Time" STR STR-LEN
        COPY TEST-ASSERT REPLACING COND BY ==BUFFER = X"FFFF08000454696D65000548656C6C6F" AND NBTENC-OFFSET = 17==.

        GOBACK.

    END PROGRAM Test-NbtEncode-String.

    *> --- Test: NbtEncode-ByteArray ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Test-NbtEncode-ByteArray.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        COPY DD-NBT-ENCODER.
        01 BUFFER       PIC X(24).
        01 ARRAY-LEN    BINARY-LONG UNSIGNED.
        01 VALUE-IN     BINARY-CHAR.

    PROCEDURE DIVISION.
        COPY TEST-UNIT REPLACING ==NAME== BY =="Test-NbtEncode-ByteArray"==.
    UnnamedEmptyArray.
        COPY TEST-CASE REPLACING ==NAME== BY =="unnamed empty array"==.
        INITIALIZE NBTENC
        MOVE X"FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF" TO BUFFER
        MOVE 3 TO NBTENC-OFFSET
        MOVE 0 TO ARRAY-LEN
        CALL "NbtEncode-ByteArray" USING NBTENC BUFFER OMITTED ARRAY-LEN
        COPY TEST-ASSERT REPLACING COND BY ==BUFFER = X"FFFF0700000000FFFFFFFFFFFFFFFFFF" AND NBTENC-OFFSET = 8==.
    NamedEmptyArray.
        COPY TEST-CASE REPLACING ==NAME== BY =="named empty array"==.
        INITIALIZE NBTENC
        MOVE X"FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF" TO BUFFER
        MOVE 3 TO NBTENC-OFFSET
        MOVE 0 TO ARRAY-LEN
        CALL "NbtEncode-ByteArray" USING NBTENC BUFFER "Data" ARRAY-LEN
        COPY TEST-ASSERT REPLACING COND BY ==BUFFER = X"FFFF0700044461746100000000FFFFFF" AND NBTENC-OFFSET = 14 AND NBTENC-LEVEL = 0==.
    NonEmptyNamedArray.
        COPY TEST-CASE REPLACING ==NAME== BY =="named non-empty array"==.
        INITIALIZE NBTENC
        MOVE X"FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF" TO BUFFER
        MOVE 3 TO NBTENC-OFFSET
        MOVE 2 TO ARRAY-LEN
        CALL "NbtEncode-ByteArray" USING NBTENC BUFFER "Data" ARRAY-LEN
        COPY TEST-ASSERT REPLACING COND BY ==NBTENC-LEVEL = 1 AND NBTENC-STACK-TYPE(1) = X"07"==.
    NonEmptyNamedArrayValues.
        COPY TEST-CASE REPLACING ==NAME== BY =="named non-empty array (values)"==.
        MOVE 1 TO VALUE-IN
        CALL "NbtEncode-Byte" USING NBTENC BUFFER OMITTED VALUE-IN
        MOVE 2 TO VALUE-IN
        CALL "NbtEncode-Byte" USING NBTENC BUFFER OMITTED VALUE-IN
        COPY TEST-ASSERT REPLACING COND BY ==BUFFER = X"FFFF07000444617461000000020102FF" AND NBTENC-OFFSET = 16 AND NBTENC-LEVEL = 0==.

        GOBACK.

    END PROGRAM Test-NbtEncode-ByteArray.

    *> --- Test: NbtEncode-List ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Test-NbtEncode-List.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        COPY DD-NBT-ENCODER.
        01 BUFFER       PIC X(24).

    PROCEDURE DIVISION.
        COPY TEST-UNIT REPLACING ==NAME== BY =="Test-NbtEncode-List"==.
    Empty.
        COPY TEST-CASE REPLACING ==NAME== BY =="empty list"==.
        INITIALIZE NBTENC
        MOVE X"FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF" TO BUFFER
        MOVE 3 TO NBTENC-OFFSET
        CALL "NbtEncode-List" USING NBTENC BUFFER OMITTED
        COPY TEST-ASSERT REPLACING COND BY ==NBTENC-OFFSET = 9 AND NBTENC-LEVEL = 1 AND NBTENC-STACK-TYPE(1) = X"09"==.
    EmptyEnd.
        COPY TEST-CASE REPLACING ==NAME== BY =="empty list (end)"==.
        CALL "NbtEncode-EndList" USING NBTENC BUFFER
        COPY TEST-ASSERT REPLACING COND BY ==BUFFER = X"FFFF090000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF" AND NBTENC-OFFSET = 9 AND NBTENC-LEVEL = 0==.
    LongList.
        COPY TEST-CASE REPLACING ==NAME== BY =="list of long"==.
        INITIALIZE NBTENC
        MOVE X"FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF" TO BUFFER
        MOVE 3 TO NBTENC-OFFSET
        CALL "NbtEncode-List" USING NBTENC BUFFER OMITTED
        COPY TEST-ASSERT REPLACING COND BY ==NBTENC-OFFSET = 9 AND NBTENC-LEVEL = 1 AND NBTENC-STACK-TYPE(1) = X"09"==.
    LongListValues.
        COPY TEST-CASE REPLACING ==NAME== BY =="list of long (values)"==.
        CALL "NbtEncode-Long" USING NBTENC BUFFER OMITTED H'0102030405060708'
        CALL "NbtEncode-Long" USING NBTENC BUFFER OMITTED H'1122334455667788'
        CALL "NbtEncode-EndList" USING NBTENC BUFFER
        COPY TEST-ASSERT REPLACING COND BY ==BUFFER = X"FFFF09040000000201020304050607081122334455667788" AND NBTENC-OFFSET = 25 AND NBTENC-LEVEL = 0==.
    NamedList.
        COPY TEST-CASE REPLACING ==NAME== BY =="named list"==.
        INITIALIZE NBTENC
        MOVE X"FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF" TO BUFFER
        MOVE 3 TO NBTENC-OFFSET
        CALL "NbtEncode-List" USING NBTENC BUFFER "Data"
        COPY TEST-ASSERT REPLACING COND BY ==NBTENC-OFFSET = 15 AND NBTENC-LEVEL = 1 AND NBTENC-STACK-TYPE(1) = X"09"==.
    NamedListValues.
        COPY TEST-CASE REPLACING ==NAME== BY =="named list (values)"==.
        CALL "NbtEncode-Long" USING NBTENC BUFFER OMITTED H'0102030405060708'
        CALL "NbtEncode-EndList" USING NBTENC BUFFER
        COPY TEST-ASSERT REPLACING COND BY ==BUFFER = X"FFFF0900044461746104000000010102030405060708FFFF" AND NBTENC-OFFSET = 23 AND NBTENC-LEVEL = 0==.

        GOBACK.

    END PROGRAM Test-NbtEncode-List.

    *> --- Test: NbtEncode-Compound ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Test-NbtEncode-Compound.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        COPY DD-NBT-ENCODER.
        01 BUFFER       PIC X(16).

    PROCEDURE DIVISION.
        COPY TEST-UNIT REPLACING ==NAME== BY =="Test-NbtEncode-Compound"==.
    UnnamedCompound.
        COPY TEST-CASE REPLACING ==NAME== BY =="unnamed compound"==.
        INITIALIZE NBTENC
        MOVE X"FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF" TO BUFFER
        MOVE 3 TO NBTENC-OFFSET
        CALL "NbtEncode-Compound" USING NBTENC BUFFER OMITTED
        COPY TEST-ASSERT REPLACING COND BY ==BUFFER = X"FFFF0AFFFFFFFFFFFFFFFFFFFFFFFFFF" AND NBTENC-OFFSET = 4==.
    NamedCompound.
        COPY TEST-CASE REPLACING ==NAME== BY =="named compound"==.
        INITIALIZE NBTENC
        MOVE X"FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF" TO BUFFER
        MOVE 3 TO NBTENC-OFFSET
        CALL "NbtEncode-Compound" USING NBTENC BUFFER "Data"
        COPY TEST-ASSERT REPLACING COND BY ==BUFFER = X"FFFF0A000444617461FFFFFFFFFFFFFF" AND NBTENC-OFFSET = 10==.

        GOBACK.

    END PROGRAM Test-NbtEncode-Compound.

    *> --- Test: NbtEncode-RootCompound ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Test-NbtEncode-RootCompound.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        COPY DD-NBT-ENCODER.
        01 BUFFER       PIC X(16).

    PROCEDURE DIVISION.
        COPY TEST-UNIT REPLACING ==NAME== BY =="Test-NbtEncode-RootCompound"==.
    Basic.
        COPY TEST-CASE REPLACING ==NAME== BY =="root compound"==.
        INITIALIZE NBTENC
        MOVE X"FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF" TO BUFFER
        MOVE 3 TO NBTENC-OFFSET
        CALL "NbtEncode-RootCompound" USING NBTENC BUFFER
        COPY TEST-ASSERT REPLACING COND BY ==BUFFER = X"FFFF0A0000FFFFFFFFFFFFFFFFFFFFFF" AND NBTENC-OFFSET = 6==.

        GOBACK.

    END PROGRAM Test-NbtEncode-RootCompound.

    *> --- Test: Test-NbtEncode-EndCompound ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Test-NbtEncode-EndCompound.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        COPY DD-NBT-ENCODER.
        01 BUFFER       PIC X(4).

    PROCEDURE DIVISION.
        COPY TEST-UNIT REPLACING ==NAME== BY =="Test-NbtEncode-EndCompound"==.
    Basic.
        COPY TEST-CASE REPLACING ==NAME== BY =="end compound"==.
        INITIALIZE NBTENC
        MOVE X"12345678" TO BUFFER
        MOVE 2 TO NBTENC-OFFSET
        CALL "NbtEncode-Compound" USING NBTENC BUFFER OMITTED
        CALL "NbtEncode-EndCompound" USING NBTENC BUFFER
        COPY TEST-ASSERT REPLACING COND BY ==BUFFER = X"120A0078" AND NBTENC-OFFSET = 4==.

        GOBACK.

    END PROGRAM Test-NbtEncode-EndCompound.

    *> --- Test: NbtEncode-IntArray ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Test-NbtEncode-IntArray.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        COPY DD-NBT-ENCODER.
        01 BUFFER       PIC X(24).
        01 ARRAY-LEN    BINARY-LONG UNSIGNED.

    PROCEDURE DIVISION.
        COPY TEST-UNIT REPLACING ==NAME== BY =="Test-NbtEncode-IntArray"==.
    UnnamedEmptyArray.
        COPY TEST-CASE REPLACING ==NAME== BY =="unnamed empty array"==.
        INITIALIZE NBTENC
        MOVE X"FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF" TO BUFFER
        MOVE 3 TO NBTENC-OFFSET
        MOVE 0 TO ARRAY-LEN
        CALL "NbtEncode-IntArray" USING NBTENC BUFFER OMITTED ARRAY-LEN
        COPY TEST-ASSERT REPLACING COND BY ==BUFFER = X"FFFF0B00000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF" AND NBTENC-OFFSET = 8 AND NBTENC-LEVEL = 0==.
    NamedEmptyArray.
        COPY TEST-CASE REPLACING ==NAME== BY =="named empty array"==.
        INITIALIZE NBTENC
        MOVE X"FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF" TO BUFFER
        MOVE 3 TO NBTENC-OFFSET
        MOVE 0 TO ARRAY-LEN
        CALL "NbtEncode-IntArray" USING NBTENC BUFFER "Data" ARRAY-LEN
        COPY TEST-ASSERT REPLACING COND BY ==BUFFER = X"FFFF0B00044461746100000000FFFFFFFFFFFFFFFFFFFFFF" AND NBTENC-OFFSET = 14 AND NBTENC-LEVEL = 0==.
    NonEmptyNamedArray.
        COPY TEST-CASE REPLACING ==NAME== BY =="named non-empty array"==.
        INITIALIZE NBTENC
        MOVE X"FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF" TO BUFFER
        MOVE 3 TO NBTENC-OFFSET
        MOVE 1 TO ARRAY-LEN
        CALL "NbtEncode-IntArray" USING NBTENC BUFFER "Data" ARRAY-LEN
        COPY TEST-ASSERT REPLACING COND BY ==NBTENC-LEVEL = 1 AND NBTENC-STACK-TYPE(1) = X"0B"==.
    NonEmptyNamedArrayValues.
        COPY TEST-CASE REPLACING ==NAME== BY =="named non-empty array (values)"==.
        CALL "NbtEncode-Int" USING NBTENC BUFFER OMITTED H'01020304'
        COPY TEST-ASSERT REPLACING COND BY ==BUFFER = X"FFFF0B0004446174610000000101020304FFFFFFFFFFFFFF" AND NBTENC-OFFSET = 18 AND NBTENC-LEVEL = 0==.

        GOBACK.

    END PROGRAM Test-NbtEncode-IntArray.

    *> --- Test: NbtEncode-LongArray ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Test-NbtEncode-LongArray.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        COPY DD-NBT-ENCODER.
        01 BUFFER       PIC X(24).
        01 ARRAY-LEN    BINARY-LONG UNSIGNED.

    PROCEDURE DIVISION.
        COPY TEST-UNIT REPLACING ==NAME== BY =="Test-NbtEncode-LongArray"==.
    UnnamedEmptyArray.
        COPY TEST-CASE REPLACING ==NAME== BY =="unnamed empty array"==.
        INITIALIZE NBTENC
        MOVE X"FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF" TO BUFFER
        MOVE 3 TO NBTENC-OFFSET
        MOVE 0 TO ARRAY-LEN
        CALL "NbtEncode-LongArray" USING NBTENC BUFFER OMITTED ARRAY-LEN
        COPY TEST-ASSERT REPLACING COND BY ==BUFFER = X"FFFF0C00000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF" AND NBTENC-OFFSET = 8 AND NBTENC-LEVEL = 0==.
    NamedEmptyArray.
        COPY TEST-CASE REPLACING ==NAME== BY =="named empty array"==.
        INITIALIZE NBTENC
        MOVE X"FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF" TO BUFFER
        MOVE 3 TO NBTENC-OFFSET
        MOVE 0 TO ARRAY-LEN
        CALL "NbtEncode-LongArray" USING NBTENC BUFFER "Data" ARRAY-LEN
        COPY TEST-ASSERT REPLACING COND BY ==BUFFER = X"FFFF0C00044461746100000000FFFFFFFFFFFFFFFFFFFFFF" AND NBTENC-OFFSET = 14 AND NBTENC-LEVEL = 0==.
    NonEmptyNamedArray.
        COPY TEST-CASE REPLACING ==NAME== BY =="named non-empty array"==.
        INITIALIZE NBTENC
        MOVE X"FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF" TO BUFFER
        MOVE 3 TO NBTENC-OFFSET
        MOVE 1 TO ARRAY-LEN
        CALL "NbtEncode-LongArray" USING NBTENC BUFFER "Data" ARRAY-LEN
        COPY TEST-ASSERT REPLACING COND BY ==NBTENC-LEVEL = 1 AND NBTENC-STACK-TYPE(1) = X"0C"==.
    NonEmptyNamedArrayValues.
        COPY TEST-CASE REPLACING ==NAME== BY =="named non-empty array (values)"==.
        CALL "NbtEncode-Long" USING NBTENC BUFFER OMITTED H'0102030405060708'
        COPY TEST-ASSERT REPLACING COND BY ==BUFFER = X"FFFF0C000444617461000000010102030405060708FFFFFF" AND NBTENC-OFFSET = 22 AND NBTENC-LEVEL = 0==.

        GOBACK.

    END PROGRAM Test-NbtEncode-LongArray.

    *> --- Test: NbtEncode-UUID ---
    IDENTIFICATION DIVISION.
    PROGRAM-ID. Test-NbtEncode-UUID.

    DATA DIVISION.
    WORKING-STORAGE SECTION.
        COPY DD-NBT-ENCODER.
        01 BUFFER       PIC X(32).
        01 UUID         PIC X(16).

    PROCEDURE DIVISION.
        COPY TEST-UNIT REPLACING ==NAME== BY =="Test-NbtEncode-UUID"==.
    Basic.
        COPY TEST-CASE REPLACING ==NAME== BY =="UUID"==.
        INITIALIZE NBTENC
        MOVE 3 TO NBTENC-OFFSET
        MOVE ALL X"FF" TO BUFFER
        MOVE X"000102030405060708090A0B0C0D0E0F" TO UUID
        CALL "NbtEncode-UUID" USING NBTENC BUFFER OMITTED UUID
        COPY TEST-ASSERT REPLACING COND BY ==BUFFER = X"FFFF0B00000004000102030405060708090A0B0C0D0E0FFFFFFFFFFFFFFFFFFF" AND NBTENC-OFFSET = 24==.

        GOBACK.

    END PROGRAM Test-NbtEncode-UUID.

END PROGRAM Test-NbtEncode.
