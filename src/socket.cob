*> --- Socket-Listen ---
IDENTIFICATION DIVISION.
PROGRAM-ID. Socket-Listen.

DATA DIVISION.
LINKAGE SECTION.
01 LK-PORT          PIC X(5).
01 LK-LISTEN        PIC X(4).
01 LK-ERRNO         PIC 9(3).

PROCEDURE DIVISION USING BY REFERENCE LK-PORT LK-LISTEN LK-ERRNO.
    CALL "CBL_GC_SOCKET" USING "00" LK-PORT LK-LISTEN GIVING LK-ERRNO.

END PROGRAM Socket-Listen.

*> --- Socket-Accept ---
IDENTIFICATION DIVISION.
PROGRAM-ID. Socket-Accept.

DATA DIVISION.
LINKAGE SECTION.
01 LK-LISTEN        PIC X(4).
01 LK-HNDL          PIC X(4).
01 LK-ERRNO         PIC 9(3).

PROCEDURE DIVISION USING BY REFERENCE LK-LISTEN LK-HNDL LK-ERRNO.
    CALL "CBL_GC_SOCKET" USING "07" LK-LISTEN LK-HNDL GIVING LK-ERRNO.

END PROGRAM Socket-Accept.

*> --- Socket-Close ---
IDENTIFICATION DIVISION.
PROGRAM-ID. Socket-Close.

DATA DIVISION.
LINKAGE SECTION.
01 LK-HNDL          PIC X(4).
01 LK-ERRNO         PIC 9(3).

PROCEDURE DIVISION USING BY REFERENCE LK-HNDL LK-ERRNO.
    CALL "CBL_GC_SOCKET" USING "06" LK-HNDL GIVING LK-ERRNO.

END PROGRAM Socket-Close.
