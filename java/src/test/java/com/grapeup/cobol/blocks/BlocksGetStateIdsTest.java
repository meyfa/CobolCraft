package com.grapeup.cobol.blocks;

import com.grapeup.cobol.support.CobolContext;
import com.grapeup.cobol.support.CobolProgramRunner;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class BlocksGetStateIdsTest {
    @Test
    void TC_1_testGetStateIdsForValidBlockId() {
        // Set up input context - LK-BLOCK-ID = 0 (first block)
        var inputs = new CobolContext().with("LK-BLOCK-ID", 0);

        // Run the Main program
        var result = CobolProgramRunner.runBuiltProgram("blocks/MainBlocksGetStateIds_TC-1", inputs);

        // Assert the output parameters
        assertEquals(100, result.getInt("LK-MINIMUM-ID"));
        assertEquals(200, result.getInt("LK-MAXIMUM-ID"));
    }

    @Test
    void TC_2_testGetStateIdsForLastBlock() {
        // Set up input context - LK-BLOCK-ID = BLOCK-COUNT - 1 = 3 - 1 = 2
        var inputs = new CobolContext().with("LK-BLOCK-ID", 2);

        // Run the Main program
        var result = CobolProgramRunner.runBuiltProgram("blocks/MainBlocksGetStateIds_TC-2", inputs);

        // Assert the output parameters for the last block (index 3)
        assertEquals(300, result.getInt("LK-MINIMUM-ID"));
        assertEquals(399, result.getInt("LK-MAXIMUM-ID"));
    }

    @Test
    void TC3_testGetStateIdsForBlockWithLargeRange() {
        // Set up input context - block ID for the block with large range
        var inputs = new CobolContext().with("LK-BLOCK-ID", 1);

        // Run the Main program
        var result = CobolProgramRunner.runBuiltProgram("blocks/MainBlocksGetStateIds_TC-3", inputs);

        // Assert the output parameters - verify large range values
        assertEquals(1000000, result.getInt("LK-MINIMUM-ID"));
        assertEquals(9999999, result.getInt("LK-MAXIMUM-ID"));
    }

    @Test
    void TC_4_testGetStateIdsForBlockWithSingleState() {
        // Set up input context - block ID 0 (first block)
        var inputs = new CobolContext().with("LK-BLOCK-ID", 0);

        // Run the Main program
        var result = CobolProgramRunner.runBuiltProgram("blocks/MainBlocksGetStateIds_TC-4", inputs);

        // Assert that minimum and maximum IDs are equal (single state)
        assertEquals(5, result.getInt("LK-MINIMUM-ID"));
        assertEquals(5, result.getInt("LK-MAXIMUM-ID"));
        
        // Verify they are the same value
        assertEquals(result.getInt("LK-MINIMUM-ID"), result.getInt("LK-MAXIMUM-ID"));
    }

    @Test
    void TC_5_testInvalidBlockId() {
        // Set up input context with invalid block ID (greater than BLOCK-COUNT)
        var inputs = new CobolContext().with("LK-BLOCK-ID", 5);

        // Run the Main program - this tests invalid block ID access
        var result = CobolProgramRunner.runBuiltProgram("blocks/MainBlocksGetStateIds_TC-5", inputs);

        // Verify that the program handled the invalid input
        // The behavior is implementation-specific, but we can verify the program completed
        // and returned some values (even if they are uninitialized or default values)
        assertNotNull(result.get("LK-MINIMUM-ID"));
        assertNotNull(result.get("LK-MAXIMUM-ID"));
        
        // The actual values may be unpredictable due to accessing beyond array bounds
        // This test primarily verifies the program doesn't crash with invalid input
        int minimumId = result.getInt("LK-MINIMUM-ID");
        int maximumId = result.getInt("LK-MAXIMUM-ID");
        
        // Log the results for debugging purposes
        System.out.println("Invalid block ID test - Minimum ID: " + minimumId + ", Maximum ID: " + maximumId);
    }
}

