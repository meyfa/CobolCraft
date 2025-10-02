package com.grapeup.cobol.support;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.time.Duration;
import java.util.List;
import java.util.concurrent.TimeUnit;

public final class CobolProgramRunner {

    private static final Path MODULE_ROOT = Paths.get("").toAbsolutePath().normalize();
    private static final Path REPO_ROOT = locateRepoRoot(MODULE_ROOT);
    private static final Path BUILD_ROOT = REPO_ROOT.resolve("out");
    private static final Duration DEFAULT_TIMEOUT = Duration.ofSeconds(30);

    private CobolProgramRunner() {
    }

    public static Path getRepoRoot() {
        return REPO_ROOT;
    }

    public static Path getBuildRoot() {
        return BUILD_ROOT;
    }

    public static ProcessResult runBuiltProgram(Path relativeExecutable) {
        return runBuiltProgram(relativeExecutable, DEFAULT_TIMEOUT);
    }

    public static ProcessResult runBuiltProgram(Path relativeExecutable, Duration timeout) {
        Path executable = BUILD_ROOT.resolve(relativeExecutable);
        if (Files.notExists(executable)) {
            throw new IllegalStateException("Expected compiled executable not found: " + executable);
        }

        System.out.printf("[CobolProgramRunner] Running %s (cwd=%s)%n", executable, BUILD_ROOT);
        try {
            return runProcess(List.of(executable.toString()), BUILD_ROOT, timeout);
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            throw new RuntimeException("Interrupted while executing COBOL program: " + executable, e);
        } catch (IOException e) {
            throw new RuntimeException("Failed to execute COBOL program: " + executable, e);
        }
    }
    private static ProcessResult runProcess(List<String> command, Path workingDir, Duration timeout)
            throws IOException, InterruptedException {
        ProcessBuilder builder = new ProcessBuilder(command);
        builder.directory(workingDir.toFile());
        builder.redirectErrorStream(false);

        Process process = builder.start();
        boolean finished = process.waitFor(timeout.toMillis(), TimeUnit.MILLISECONDS);
        if (!finished) {
            process.destroyForcibly();
            throw new IOException("Process timed out: " + command);
        }

        try (var stderrStream = process.getErrorStream()) {
            String stderr = new String(stderrStream.readAllBytes(), StandardCharsets.UTF_8)
                    .replace("\r\n", "\n");
            return new ProcessResult(process.exitValue(), stderr);
        }
    }

    private static Path locateRepoRoot(Path start) {
        Path current = start;
        for (int depth = 0; depth < 6 && current != null; depth++) {
            if (Files.exists(current.resolve("codegen"))) {
                return current;
            }
            current = current.getParent();
        }
        throw new IllegalStateException("Unable to locate repository root starting from " + start);
    }
}
