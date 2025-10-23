package com.grapeup.cobol.blocks;

import com.grapeup.cobol.support.CobolContext;
import com.grapeup.cobol.support.CobolProgramRunner;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

class BlocksMaximumStateIdTest {

    @Test
    void TC_1_testBlocksMaximumStateIdSubprogram() {
        // Set up input context - no input parameters needed for this program
        var inputs = new CobolContext();

        // Run the Main program (note: no file extension, executable name only)
        var result = CobolProgramRunner.runBuiltProgram("blocks/MainBlocksMaximumStateId_TC-1", inputs);

        // Assert the output parameters
        assertEquals(1000, result.getInt("LK-MAXIMUM-ID"));
    }

    @Test
    void TC_2_testRetrieveMaximumStateIdAfterInitialization() {
        // Set up input context - no input parameters needed
        var inputs = new CobolContext();

        // Run the Main program
        var result = CobolProgramRunner.runBuiltProgram("blocks/MainBlocksMaximumStateId_TC-2", inputs);

        // Assert the output parameters
        assertEquals(0, result.getInt("LK-MAXIMUM-ID"));
    }

    @Test
    void TC_3_testRetrieveMaximumStateIdAfterAddingBlocks() {
        // No input context needed as the Main program initializes the test data
        var inputs = new CobolContext();

        // Run the Main program
        var result = CobolProgramRunner.runBuiltProgram("blocks/MainBlocksMaximumStateId_TC-3", inputs);

        // Assert the output parameter
        assertEquals(2500, result.getInt("LK-MAXIMUM-ID"));
    }

    @Test
    void TC_4_testRetrieveMaximumStateIdAtCapacity() {
        // Set up input context - no input parameters needed for this test
        var inputs = new CobolContext();

        // Run the Main program
        var result = CobolProgramRunner.runBuiltProgram("blocks/MainBlocksMaximumStateId_TC-4", inputs);

        // Assert the output parameter - maximum value for BINARY-LONG UNSIGNED (4294967295)
        assertEquals(4294967295L, result.getInt("LK-MAXIMUM-ID"));
    }

    @Test
    void TC_5_testRetrieveMaximumStateIdAfterBlockRemoval() {
        // Set up input context - no input parameters needed for this program
        var inputs = new CobolContext();

        // Run the Main program (note: no file extension, executable name only)
        var result = CobolProgramRunner.runBuiltProgram("blocks/MainBlocksMaximumStateId_TC-5", inputs);

        // Assert the output parameters
        assertEquals(1200, result.getInt("LK-MAXIMUM-ID"));
    }
}

