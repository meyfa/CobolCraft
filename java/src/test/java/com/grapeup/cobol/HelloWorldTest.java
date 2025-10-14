package com.grapeup.cobol;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.nio.file.Path;

import com.grapeup.cobol.support.CobolProgramRunner;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import com.grapeup.cobol.support.ProcessResult;

class HelloWorldTest {

    private static final Path PROGRAM_PATH = Path.of("blocks");
    private static final String EXPECTED_STDOUT_RESOURCE =
            "blocks/expected_stdout.txt";

    @Test
    @DisplayName("Hello world program prints the expected greeting")
    void helloWorldDisplaysGreeting() {
        ProcessResult execution = CobolProgramRunner.runBuiltProgram(PROGRAM_PATH);

        assertEquals(0, execution.exitCode(), execution::describe);

    }
}
