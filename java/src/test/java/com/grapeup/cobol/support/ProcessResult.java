package com.grapeup.cobol.support;

public record ProcessResult(int exitCode, String stderr) {

    public String describe() {
        return describe("Process execution");
    }

    public String describe(String header) {
        return String.format("%s%nExit code: %d%nSTDERR:%n%s",
                header,
                exitCode,
                stderr.isBlank() ? "<empty>" : stderr);
    }
}