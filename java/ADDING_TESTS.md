# Adding a COBOL Example Test

Follow these steps to add a new JUnit test for a COBOL using the shared harness.

1. **Create `Main` program to call the target program (if needed)**
   - If the COBOL program is not already a standalone executable, add a `Main` program that calls it. 
   The `Main` should not have a `USING` clause in its `PROCEDURE DIVISION` if it is to be run directly.
   - Place the `Main` program under `java/src/test/resources/<relative/path>`, where `<relative/path>` matches the COBOL source structure under `src/`.
   - If your test needs to read files or prepare input, implement that logic in Main or provide any helpers in the same test resource directory.
     Do not assume any helpers exist outside your test directory.

2. **Create test data (if needed)**
   - If the COBOL program needs input files or other data, add them under `java/src/test/resources/<relative/path>/`.
   
3. **Ensure the executable is produced**
   - COBOL program compiles into `out/<relative/path>`, mirroring its location in `src/`.

4. **Create expected output (optional)**
   - Place any expected output or state files under `java/src/test/resources/<relative/path>`, matching the location of your test program and data.
   - These files can include expected stdout, result files, or any state your test will verify.
   - Use plain text files; newline normalization is handled by FixtureLoader if used.
   
5. **Add a test class**
   - Add JUnit test under `java/src/test/java/com/grapeup/cobol/<relative/path>`.
   - Extend `CobolProgramRunner.runBuiltProgram(Path relativeExecutable)` to execute the binary (no compilation inside tests).
   - Example test:
```aiignore
package com.grapeup.cobol.example;

import com.grapeup.cobol.support.CobolProgramRunner;
import org.junit.jupiter.api.Test;
import java.nio.file.Path;

class MainTest {
    @Test
    void runsSuccessfully() {
       var result = CobolProgramRunner.runBuiltProgram(Path.of("example/Main"));
       Add assertions as needed
    }
}
```

6. **Reproduce locally**
   - Run `mvn -f java/pom.xml test` from repository root.
   - Confirm the new test logs the command it runs; failures print captured stdout/stderr.

