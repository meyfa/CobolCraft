# Adding a COBOL Example Test

Follow these steps to add a new JUnit test for a COBOL program using the shared harness.

1. **Create `Main` program to call the target program (if needed)**
   - If the COBOL program is not already a standalone executable, add a `Main` program that calls it. 
   The `Main` should not have a `USING` clause in its `PROCEDURE DIVISION` if it is to be run directly.
   - Place the `Main` program under `java/src/test/resources/programs/<path>/<MainProgramName>.cob`, where:
     - `<path>` can be nested folders reflecting the program's logical grouping (e.g., `blocks/`, `encoding/json/`, etc.)
     - Program name follows the pattern `Main<TargetProgram>_<TestCaseId>` (e.g., `MainBlocksCount_TC-1.cob` for testing `Blocks-Count`)
   - For Main program structure and requirements, see `MAIN_GENERATION_RULES.md` in the repository root.

2. **Create test data (if needed)**
   - If the COBOL program needs input files or other data, add them under `java/src/test/resources/programs/<path>/` or appropriate subdirectories.
   
3. **Ensure the executable is produced**
   - COBOL programs compile into `out/` directory, mirroring their location in `src/` or `java/src/test/resources/`.
   - Run `make -j$(sysctl -n hw.ncpu)` for macOS or `make -j$(nproc)` for Linux from the repository root to build all COBOL programs.
   - Run `make test-programs` to compile all test programs under `java/src/test/resources/` into `out/`.
   - Nested folder structure is preserved in the `out/` directory.

4. **Create expected output (optional)**
   - The testing framework automatically handles result file creation and parsing.
   - Expected outputs are verified through assertions in the JUnit test.
   - Result files are written to `target/cobol-test/<path>/<MainProgramName>.txt`.
   
5. **Add a test class**
   - Add JUnit test under `java/src/test/java/com/grapeup/cobol/<package>/` where package reflects the COBOL program's domain (e.g., `blocks` for block-related programs).
   - Test class name should match the target COBOL program name with "Test" suffix (e.g., `BlocksCountTest` for `Blocks-Count`).
   - Test name should match the specific functionality being tested with test case id prefix, eg. `TC-1_testBlocksCountSubprogram`.
   - Use `CobolProgramRunner.runBuiltProgram(String mainExecutable, CobolContext inputContext)` to execute the binary.
   - Use `CobolContext` to set up input parameters and retrieve output results. (Don't set input parameters in CobolContext if the target program doesn't need them.)
   - **Program Path**: Include the full path relative to the `out/` directory:
     - Simple: `"MainBlocksCount"` for programs in root
     - Nested: `"blocks/MainBlocksCount"` or `"encoding/json/MainJsonParser"` for programs in subfolders
   - Example tests:
```java
// Simple program in root folder
package com.grapeup.cobol.blocks;

import com.grapeup.cobol.support.CobolContext;
import com.grapeup.cobol.support.CobolProgramRunner;
import org.junit.jupiter.api.Test;

import static java.lang.Integer.valueOf;
import static org.junit.jupiter.api.Assertions.assertEquals;

class BlocksCountTest {
    @Test
    void testBlocksCountSubprogram() {
        // Set up input context - environment variables for the Main program
        var inputs = new CobolContext().with("BLOCK-COUNT", 5);

        // Run the Main program (note: no file extension, executable name only)
        var result = CobolProgramRunner.runBuiltProgram("MainBlocksCount", inputs);

        // Assert the output parameters
        assertEquals(5, result.getInt("LK-COUNT"));
    }
}

// Nested program in subfolder
class JsonParserTest {
    @Test
    void testJsonParserSubprogram() {
        var inputs = new CobolContext().with("INPUT-JSON", "{'key':'value'}");

        // Program located in encoding/json/ subfolder
        var result = CobolProgramRunner.runBuiltProgram("encoding/json/MainJsonParser", inputs);

        assertEquals("value", result.get("PARSED-VALUE"));
    }
}
```

6. **Framework Details**
   - **Input Parameters**: Use `CobolContext.with(String name, Object value)` to set environment variables that the Main program will read.
   - **Output Parameters**: The framework automatically parses the result file and makes outputs available via `CobolContext.getInt(String name)` or `CobolContext.get(String name)`.
   - **Environment Setup**: The framework automatically sets:
     - `COB_LIBRARY_PATH` to `../out` for shared libraries
     - `RESULT_FILE_PATH` to `target/cobol-test/<path>/<MainProgram>.txt` (preserving folder structure)
     - All input parameters as uppercase environment variables
   - **Error Handling**: The framework captures COBOL DISPLAY output for debugging and throws RuntimeException on non-zero exit codes.

7. **Reproduce locally**
   - Run `mvn -f java/pom.xml test` from repository root.
   - Can also run from IntelliJ or other IDEs with JUnit support.
   - The framework logs all COBOL DISPLAY output with `[COBOL DISPLAY]` prefix.
   - Test failures include captured stdout/stderr for debugging.