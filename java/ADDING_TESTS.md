# Adding a COBOL Example Test

Follow these steps to add a new JUnit test for a COBOL using the shared harness.

1. **Ensure the executable is produced**
   - Update the GitHub Actions build step so the COBOL program compiles into `out/<relative/path>`, mirroring its location in `src/`.
   - Rebuild locally with `make -j$(sysctl -n hw.ncpu)`.

2. **Create expected output fixtures (optional)**
   - Add files under `java/src/test/resources/fixtures/<category>/<program>/`.
   - Use plain text files; newline-normalisation is handled by `FixtureLoader`.

3. **Add a test class**
   - Add JUnit test under `java/src/test/java/com/grapeup/cobol/<relative/path>`.
   - Extend `CobolProgramRunner.runBuiltProgram(Path relativeExecutable)` to execute the binary (no compilation inside tests).

4. **Reproduce locally**
   - Run `mvn -f java/pom.xml test` from repository root.
   - Confirm the new test logs the command it runs; failures print captured stdout/stderr.

5. **Document special setup**
   - If the example needs sample data or environment variables, update `java/README.md` accordingly.

