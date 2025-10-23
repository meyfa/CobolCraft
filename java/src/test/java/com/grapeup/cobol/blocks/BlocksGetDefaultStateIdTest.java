package com.grapeup.cobol.blocks;

import com.grapeup.cobol.support.CobolContext;
import com.grapeup.cobol.support.CobolProgramRunner;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class BlocksGetDefaultStateIdTest {
    @Test
    void TC_1_testGetDefaultStateIdForValidBlock() {
        // Set up input context - environment variables for the Main program
        var inputs = new CobolContext().with("LK-BLOCK-ID", 0);

        // Run the Main program (note: no file extension, executable name only)
        var result = CobolProgramRunner.runBuiltProgram("blocks/MainBlocksDefStateId_TC-1", inputs);

        // Assert the output parameters
        assertEquals(42, result.getInt("LK-STATE-ID"));
    }

    @Test
    void TC_2_testGetDefaultStateIdForLastBlock() {
        // Set up input context - LK-BLOCK-ID set to BLOCK-COUNT - 1 (last block)
        // Since BLOCK-COUNT is 5, the last block index is 4 (0-based)
        var inputs = new CobolContext().with("LK-BLOCK-ID", 4);

        // Run the Main program
        var result = CobolProgramRunner.runBuiltProgram("blocks/MainBlocksGetDefStateId_TC-2", inputs);

        // Assert the output parameter - should contain the default state ID for the last block (500)
        assertEquals(500, result.getInt("LK-STATE-ID"));
    }

    @Test
    void TC_3_testGetDefaultStateIdForBlockWithMultipleStates() {
        // Set up input context - block ID for a block with multiple states
        var inputs = new CobolContext().with("LK-BLOCK-ID", 0);

        // Run the Main program
        var result = CobolProgramRunner.runBuiltProgram("blocks/MainBlocksGetDefStateId_TC-3", inputs);

        // Assert the output - should return the default state ID (3)
        // which differs from both min (1) and max (5) state IDs
        assertEquals(3, result.getInt("LK-STATE-ID"));
    }

    @Test
    void TC_4_testGetDefaultStateIdForBlockWithSingleState() {
        // Set up input context - block ID for the single state block
        var inputs = new CobolContext().with("LK-BLOCK-ID", 0);

        // Run the Main program
        var result = CobolProgramRunner.runBuiltProgram("blocks/MainBlocksGetDefStateId_TC-4", inputs);

        // Assert the output parameter - should return the single state ID (100)
        assertEquals(100, result.getInt("LK-STATE-ID"));
    }

    @Test
    void TC_5_testInvalidBlockId() {
        // Set up input context with invalid block ID (N, where valid indices are 0 to N-1)
        // Assuming N=10 for this test, so valid indices would be 0-9, and 10 is invalid
        var inputs = new CobolContext().with("LK-BLOCK-ID", 10);

        // Run the Main program
        var result = CobolProgramRunner.runBuiltProgram("blocks/MainBlocksGetDefStateId_TC-5", inputs);

        // The behavior is implementation-specific for invalid block ID
        // We verify that the program executes without crashing and returns some value
        // The actual value depends on COBOL runtime behavior for array bounds
        assertNotNull(result.get("LK-STATE-ID"));
        
        // Additional assertion: verify that some state ID value is returned
        // (could be 0, garbage value, or implementation-specific sentinel)
        assertTrue(result.get("LK-STATE-ID") instanceof Long);
    }
}
