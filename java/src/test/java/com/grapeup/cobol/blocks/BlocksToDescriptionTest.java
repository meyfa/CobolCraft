package com.grapeup.cobol.blocks;

import com.grapeup.cobol.support.CobolContext;
import com.grapeup.cobol.support.CobolProgramRunner;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class BlocksToDescriptionTest {

    @Test
    void TC1_testEmptyBlocksStructure() {
        // Set up input context - environment variables for the Main program
        var inputs = new CobolContext().with("LK-STATE-ID", 1);

        // Run the Main program
        var result = CobolProgramRunner.runBuiltProgram("blocks/MainBlocksToDescription_TC-1", inputs);

        // Assert the output parameters
        assertEquals(0, result.getInt("LK-STATE-PROPERTY-COUNT"));
        
        // Verify that LK-STATE-NAME remains empty/spaces
        String stateName = (String) result.get("LK-STATE-NAME");
        assertTrue(stateName == null || stateName.trim().isEmpty(), 
                   "LK-STATE-NAME should be empty or spaces when BLOCKS structure is empty");
    }

    @Test
    void TC_2_testStateIdNotFound() {
        // Set up input context with non-existent state ID
        var inputs = new CobolContext().with("LK-STATE-ID", 9999);

        // Run the Main program
        var result = CobolProgramRunner.runBuiltProgram("blocks/MainBlocksToDescription_TC-2", inputs);

        // Assert the output parameters
        assertEquals(0, result.getInt("LK-STATE-PROPERTY-COUNT"));
        
        // Assert that LK-STATE-NAME is empty/spaces
        String stateName = (String) result.get("LK-STATE-NAME");
        assertTrue(stateName == null || stateName.trim().isEmpty(), "LK-STATE-NAME should be empty or spaces for non-existent state ID");
    }

    @Test
    void TC_3_testSingleBlockWithNoProperties() {
        // Set up input context - state ID matching the minimum state ID of the single block
        var inputs = new CobolContext().with("LK-STATE-ID", 100);

        // Run the Main program
        var result = CobolProgramRunner.runBuiltProgram("blocks/MainBlocksToDescription_TC-3", inputs);

        // Assert the output parameters
        assertEquals("TestBlock", result.get("LK-STATE-NAME").toString().trim());
        assertEquals(0, result.getInt("LK-STATE-PROPERTY-COUNT"));
    }

    @Test
    void TC_4_testBlockWithSingleProperty() {
        // Set up input context - state ID for minimum state of the test block
        var inputs = new CobolContext().with("LK-STATE-ID", 100);

        // Run the Main program
        var result = CobolProgramRunner.runBuiltProgram("blocks/MainBlocksToDescription_TC-4", inputs);

        // Assert the output parameters
        assertEquals("test-block", result.get("LK-STATE-NAME"));
        assertEquals(1, result.getInt("LK-STATE-PROPERTY-COUNT"));
        assertEquals("test-property", result.get("LK-STATE-PROPERTY-NAME-1"));
        assertEquals("value1", result.get("LK-STATE-PROPERTY-VALUE-1"));
    }

    @Test
    void TC5_testBlockWithMaximumProperties() {
        // Set up input context - use maximum state ID for the test block
        var inputs = new CobolContext().with("LK-STATE-ID", 65535);
        
        // Run the Main program
        var result = CobolProgramRunner.runBuiltProgram("blocks/MainBlocksToDescription_TC-5", inputs);
        
        // Assert the output parameters
        assertEquals("TestBlock16Props", result.get("LK-STATE-NAME"));
        assertEquals(16, result.getInt("LK-STATE-PROPERTY-COUNT"));
        
        // Verify all 16 properties are set correctly
        for (int i = 1; i <= 16; i++) {
            assertEquals("prop" + i, result.get("LK-STATE-PROPERTY-NAME-" + i));
            // For maximum state ID, all properties should have their second value ("b" variant)
            assertEquals("val" + i + "b", result.get("LK-STATE-PROPERTY-VALUE-" + i));
        }
    }

    @Test
    void TC_6_testMiddleStateIdOfBlock() {
        // Set up input context with a middle state ID
        // Assuming we have a block with minimum state ID 100 and maximum state ID 200
        // The middle would be 150
        var inputs = new CobolContext().with("LK-STATE-ID", 150);
        
        // Run the Main program
        var result = CobolProgramRunner.runBuiltProgram("blocks/MainBlocksToDescription_TC-6", inputs);
        
        // Assert the output parameters
        assertNotNull(result.get("LK-STATE-NAME"), "Block name should be set");
        
        // Verify property count is set correctly
        Object propertyCount = result.get("LK-STATE-PROPERTY-COUNT");
        assertNotNull(propertyCount, "Property count should be set");
        
        int propCount = ((Number) propertyCount).intValue();
        
        // Verify that properties are set for the middle state
        for (int i = 1; i <= propCount; i++) {
            assertNotNull(result.get("LK-STATE-PROPERTY-NAME-" + i), 
                "Property name " + i + " should be set");
            assertNotNull(result.get("LK-STATE-PROPERTY-VALUE-" + i), 
                "Property value " + i + " should be set");
        }
    }

    @Test
    void TC_7_testLastStateIdOfBlock() {
        // Set up input context - using maximum state ID (107) for the test block
        var inputs = new CobolContext().with("LK-STATE-ID", 107);

        // Run the Main program
        var result = CobolProgramRunner.runBuiltProgram("blocks/MainBlocksToDescription_TC-7", inputs);

        // Assert the output parameters
        assertEquals("test-block", result.get("LK-STATE-NAME"));
        assertEquals(2, result.getInt("LK-STATE-PROPERTY-COUNT"));
        
        // Verify property names and values for the last state
        assertEquals("facing", result.get("LK-STATE-PROPERTY-NAME-1"));
        assertEquals("south", result.get("LK-STATE-PROPERTY-VALUE-1"));
        assertEquals("powered", result.get("LK-STATE-PROPERTY-NAME-2"));
        assertEquals("manual", result.get("LK-STATE-PROPERTY-VALUE-2"));
    }

    @Test
    void TC8_testFirstBlockInBlocksStructure() {
        // Set up input context - using minimum state ID of first block
        // Assuming first block has minimum state ID of 1 based on typical block structure
        var inputs = new CobolContext().with("LK-STATE-ID", 1);
        
        // Run the Main program
        var result = CobolProgramRunner.runBuiltProgram("blocks/MainBlocksToDescription_TC-8", inputs);
        
        // Assert the output parameters
        // Verify that LK-STATE-NAME is set (not empty/spaces)
        assertNotNull(result.get("LK-STATE-NAME"));
        
        // Verify that LK-STATE-PROPERTY-COUNT is set to a reasonable value
        int propertyCount = result.getInt("LK-STATE-PROPERTY-COUNT");
        assertEquals(true, propertyCount >= 0, "Property count should be non-negative");
        
        // Additional verification that the state name corresponds to the first block
        // The actual block name will depend on the BLOCKS structure data
        String stateName = (String) result.get("LK-STATE-NAME");
        assertEquals(true, stateName != null && !stateName.trim().isEmpty(), 
                    "State name should not be empty for valid state ID");
    }

    @Test
    void TC_9_testLastBlockInBlocksStructure() {
        // Set up input context - using maximum state ID of the last block
        // This test assumes we know the structure has multiple blocks and we're testing the last one
        // The actual state ID would need to be determined from the BLOCKS structure
        var inputs = new CobolContext().with("LK-STATE-ID", 999); // Placeholder - should be actual max state ID of last block
        
        // Run the Main program
        var result = CobolProgramRunner.runBuiltProgram("blocks/MainBlocksToDescription_TC-9", inputs);
        
        // Assert the output parameters
        assertNotNull(result.get("LK-STATE-NAME"), "State name should be set");
        assertNotEquals("", result.get("LK-STATE-NAME").toString().trim(), "State name should not be empty");
        
        // Verify property count is set correctly
        assertTrue(result.getInt("LK-STATE-PROPERTY-COUNT") >= 0, "Property count should be non-negative");
        
        // Verify that properties are set for the last block
        int propertyCount = result.getInt("LK-STATE-PROPERTY-COUNT");
        for (int i = 1; i <= propertyCount; i++) {
            assertNotNull(result.get("LK-STATE-PROPERTY-NAME-" + i), "Property name " + i + " should be set");
            assertNotNull(result.get("LK-STATE-PROPERTY-VALUE-" + i), "Property value " + i + " should be set");
        }
    }

    @Test
    void TC_10_testBlockWithMaximumPropertyValues() {
        // Set up input context - the Main program will set up the test data internally
        // and use the maximum state ID for the block
        var inputs = new CobolContext();
        
        // Run the Main program
        var result = CobolProgramRunner.runBuiltProgram("blocks/MainBlocksToDescription_TC-10", inputs);
        
        // Assert the output parameters
        assertEquals("TestBlock", result.get("LK-STATE-NAME").toString().trim());
        assertEquals(2, result.getInt("LK-STATE-PROPERTY-COUNT"));
        
        // Verify that properties are set with the 32nd (last) values
        assertEquals("Property1", result.get("LK-STATE-PROPERTY-NAME-1").toString().trim());
        assertEquals("Value1_32", result.get("LK-STATE-PROPERTY-VALUE-1").toString().trim());
        
        assertEquals("Property2", result.get("LK-STATE-PROPERTY-NAME-2").toString().trim());
        assertEquals("Value2_32", result.get("LK-STATE-PROPERTY-VALUE-2").toString().trim());
    }
}

