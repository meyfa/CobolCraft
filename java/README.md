# Java Module

This module collects shared Java utilities and integration tests used to exercise the COBOL examples. It is self-contained so additional Java tooling (such as future adapters or verifiers) can live alongside the tests. Maven with JUnit 5 powers the suite locally and in CI.

## Running Tests Locally

```bash
mvn -f java/pom.xml test
```

Ensure the GnuCOBOL toolchain is on your `PATH` (`cobc` binary). 
On macOS you can install it with `brew install gnu-cobol gcc make zlib curl openjdk@23`.
Update .zshrc with
```aiignore
# Use Homebrew gmake as "make"
export PATH="/opt/homebrew/opt/make/libexec/gnubin:$PATH"

# Ensure Java 23 is found first
export JAVA_HOME=$(/usr/libexec/java_home -v23)
export PATH="$JAVA_HOME/bin:$PATH"

# Compiler flags for zlib and curl
export LDFLAGS="-L/opt/homebrew/opt/zlib/lib -L/opt/homebrew/opt/curl/lib"
export CPPFLAGS="-I/opt/homebrew/opt/zlib/include -I/opt/homebrew/opt/curl/include"
```

Update makefile with
```aiignore
GCVERSION := $(shell $(COBC) --version | head -n1 | sed -E 's/.* ([0-9]+)\.([0-9]+).*/\1\2/')
```

### Build with
```bash
make -j$(sysctl -n hw.ncpu)
```

### Build specific files
```bash
mkdir -p out
cobc -free -O2 --debug -Wall -fnotrunc -fstatic-call \
  $(find src/_copybooks -type d -exec printf -- '-I %s ' {} \;) \
  -c -o out/json-parse.o src/encoding/json-parse.cob
```

## Adding a New COBOL Test

1. **Ensure the program is built.** Update the CI build script so the COBOL executable lands under `out/<relative/path>` (mirroring the `src/` tree) before tests run. 
2. **Create fixtures (optional).** Place any expected outputs under `src/test/resources/fixtures/<category>/<program>/`.
3. **Write the test class.** Add a JUnit test in `src/test/java`. Use `CobolProgramRunner` utilities to execute the pre-built binary and capture stdout/stderr.
4. **Assert behaviour.** Compare the captured output against literal strings or fixture files and assert the process exits with code `0` using the shared `ProcessResult` helpers.
5. **Update documentation.** Expand this README, `ADDING_TESTS.md`, or other docs if the program needs additional setup (sample data, environment variables, etc.).

Run `mvn -f java/pom.xml test` before opening a PR to ensure all scenarios pass.
