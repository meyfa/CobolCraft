IF NOT ( COND )
    DISPLAY "FAIL"
    CALL "TestAssertFailed"
ELSE
    DISPLAY "PASS"
    CALL "TestAssertPassed"
END-IF.
