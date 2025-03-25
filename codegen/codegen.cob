IDENTIFICATION DIVISION.
PROGRAM-ID. CodegenMain.

DATA DIVISION.
WORKING-STORAGE SECTION.
    01 DATADIR                  PIC X(256).
    01 OUTDIR                   PIC X(256).
    01 TPLDIR                   PIC X(256).

PROCEDURE DIVISION CHAINING OPTIONAL DATADIR OPTIONAL OUTDIR OPTIONAL TPLDIR.
    IF (DATADIR IS OMITTED OR DATADIR = SPACES)
            OR (OUTDIR IS OMITTED OR OUTDIR = SPACES)
            OR (TPLDIR IS OMITTED OR TPLDIR = SPACES)
        DISPLAY "Usage: codegen <datadir> <outdir> <tpldir>" UPON STDERR
        STOP RUN RETURNING 1
    END-IF

    CALL "Codegen-SetDataDirectory" USING DATADIR
    CALL "Codegen-SetOutputDirectory" USING OUTDIR
    CALL "Codegen-SetTemplateDirectory" USING TPLDIR

    *> Registry data is helpful in multiple generators, so load it into memory
    DISPLAY "Loading registries..."
    CALL "CG-LoadRegistries" USING DATADIR

    CALL "CG-Packets" USING OUTDIR TPLDIR
    CALL "CG-Registries" USING OUTDIR TPLDIR
    CALL "CG-Items" USING OUTDIR TPLDIR
    CALL "CG-BlocksLootTable" USING OUTDIR TPLDIR

    STOP RUN.

END PROGRAM CodegenMain.
