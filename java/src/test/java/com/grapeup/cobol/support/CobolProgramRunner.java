package com.grapeup.cobol.support;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.HashMap;
import java.util.Map;

public class CobolProgramRunner {

    /**
     * Runs any COBOL subprogram via its Main wrapper.
     * Input variables provided via CobolContext.
     * Output variables read from a structured result file.
     */
    public static CobolContext runBuiltProgram(String mainExecutable, CobolContext inputContext) {
        Path executable = Path.of("../out").resolve(Path.of(mainExecutable));
        Path resultFile = Path.of("target/cobol-test/", mainExecutable + ".txt");

        try {
            // Prepare process
            ProcessBuilder builder = new ProcessBuilder(executable.toString());
            builder.environment().put("COB_LIBRARY_PATH", Path.of("../out").toAbsolutePath().toString());
            builder.environment().put("RESULT_FILE_PATH", resultFile.toString());
            // Inject input variables as environment variables
            inputContext.asMap().forEach((k, v) ->
                    builder.environment().put(k.toUpperCase(), String.valueOf(v))
            );

            builder.redirectErrorStream(true); // merge stderr into stdout

            // Run COBOL binary
            Process process = builder.start();

            // Capture DISPLAY output
            try (BufferedReader reader = new BufferedReader(
                    new InputStreamReader(process.getInputStream()))) {
                String line;
                while ((line = reader.readLine()) != null) {
                    System.out.println("[COBOL DISPLAY] " + line); // debug log
                }
            }

            int exitCode = process.waitFor();
            if (exitCode != 0) throw new RuntimeException("COBOL program failed");

            // Read outputs from result file
            Map<String, Object> vars = new HashMap<>();
            if (!Files.exists(resultFile.getParent())) {
                    Files.createDirectories(resultFile.getParent());
                }
            if (!Files.exists(resultFile)) {
                    Files.createFile(resultFile);
                }
            try (BufferedReader reader = Files.newBufferedReader(resultFile)) {
                String line;
                while ((line = reader.readLine()) != null) {
                    line = line.trim();
                    if (line.isEmpty()) continue;
                    String[] parts = line.split("=", 2);
                    if (parts.length == 2) {
                        String name = parts[0].trim().toUpperCase();
                        String val = parts[1].trim();
                        vars.put(name, Integer.parseInt(val)); // parse as integer
                    }
                }
            }

            return new CobolContext(vars);

        } catch (IOException | InterruptedException e) {
            throw new RuntimeException(e);
        }
    }
}
