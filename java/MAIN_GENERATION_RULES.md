# COBOL Main Program Generation Rules

## Overview
Rules for automatically generating Main wrapper programs for any given COBOL program.

## Template Structure

### 1. IDENTIFICATION DIVISION
```cobol
IDENTIFICATION DIVISION.
PROGRAM-ID. Main-{TargetProgramName}.
```
**Rule**: Always prefix with "Main-" followed by the target program name.

### 2. ENVIRONMENT DIVISION
```cobol
ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
    SELECT RESULT-FILE ASSIGN TO WS-RESULT-FILE-PATH
        ORGANIZATION IS LINE SEQUENTIAL.
```
**Rule**: Always include file control for output file with sequential organization.

### 3. DATA DIVISION

#### FILE SECTION
```cobol
FILE SECTION.
FD RESULT-FILE.
01 RESULT-REC PIC X(80).
```
**Rule**: Always include 80-character result record for output.

#### WORKING-STORAGE SECTION
```cobol
WORKING-STORAGE SECTION.
    {COPY statements for required copybooks}
    {Input parameters based on target program LINKAGE}
    {Output parameters based on target program LINKAGE}
    01 {PARAM}_DISPLAY PIC 9(10).  [for numeric outputs]
    01 OUTPUT-LINE PIC X(80) VALUE SPACES.
    01 WS-RESULT-FILE-PATH PIC X(100).
```

**Rules for WORKING-STORAGE**:
- Copy all required copybooks used by target program
- Create variables for each LINKAGE parameter with appropriate data types
- For numeric outputs, create corresponding display variables (PIC 9(10))
- Always include OUTPUT-LINE and WS-RESULT-FILE-PATH

### 4. PROCEDURE DIVISION Structure

#### Startup Message
```cobol
PROCEDURE DIVISION.
    DISPLAY "Main-{TargetProgramName} started".
```
**Rule**: Always display startup message with program name.

#### Environment Input Reading
```cobol
    *> Accept result file path from environment
    ACCEPT WS-RESULT-FILE-PATH FROM ENVIRONMENT "RESULT_FILE_PATH"

    *> Accept inputs dynamically from environment
    {For each input parameter}
    ACCEPT {PARAM-NAME} FROM ENVIRONMENT "{PARAM-NAME}"
```
**Rules**:
- Always read result file path from RESULT_FILE_PATH environment variable
- Read each input parameter from environment using parameter name as environment variable name
- Use exact parameter names from target program's LINKAGE section

#### Data Structure Initialization
```cobol
    {Initialize any shared data structures if copybooks are used}
```
**Rule**: If target program uses copybooks with shared data (EXTERNAL), ensure proper initialization.

#### Target Program Call
```cobol
    *> Call the subprogram
    CALL '{TargetProgramName}' USING {parameter-list}
    DISPLAY "{TargetProgramName} returned {output-params}"
```
**Rules**:
- Use exact target program name in CALL statement
- Pass parameters in order defined in target program's LINKAGE section
- Display return values for debugging

#### Output File Writing
```cobol
    *> Write outputs to file
    OPEN OUTPUT RESULT-FILE
    {For each output parameter}
    MOVE {PARAM} TO {PARAM}_DISPLAY  [if numeric]
    STRING "{PARAM}=" DELIMITED BY SIZE
           {PARAM}_DISPLAY DELIMITED BY SIZE  [or PARAM if non-numeric]
           INTO OUTPUT-LINE
    END-STRING
    MOVE OUTPUT-LINE TO RESULT-REC
    WRITE RESULT-REC
    CLOSE RESULT-FILE
```
**Rules**:
- Convert numeric outputs to display format before writing
- Use "PARAM=" format for each output variable
- Write one line per output parameter

#### Program Termination
```cobol
    GOBACK.
END PROGRAM Main-{TargetProgramName}.
```
**Rule**: Always use GOBACK and proper program termination.

## Parameter Detection Rules

### Analyzing Target Program LINKAGE Section
1. **Input Parameters**: Parameters that are read-only or modified by called program
2. **Output Parameters**: Parameters that are set/returned by called program
3. **Data Types**: 
   - `BINARY-LONG UNSIGNED` → Environment input, display output
   - `PIC X` → Direct environment input/output
   - `BINARY-CHAR UNSIGNED` → Environment input, display output
   - Complex structures → May need special handling

### Copybook Dependencies
1. Scan target program for `COPY` statements
2. Include all required copybooks in Main program WORKING-STORAGE
3. If copybooks define EXTERNAL data, ensure proper initialization

## Environment Variable Naming Convention
- Input parameters: Use exact parameter name from LINKAGE section
- Output file: Always "RESULT_FILE_PATH"
- Example: `BLOCK-COUNT` parameter → `BLOCK-COUNT` environment variable

## Output Format Rules
- Each output parameter gets its own line in result file
- Format: `PARAMETER-NAME=value`
- Numeric values: Convert to display format (PIC 9(10))
- String values: Use directly

## Error Handling (Optional Enhancement)
- Check file operations for success
- Validate environment variable reading
- Handle CALL statement failures

## Example Application
For program `Blocks-Count` with:
- LINKAGE: `01 LK-COUNT BINARY-LONG UNSIGNED`
- Uses: `COPY DD-BLOCKS`

Generated Main would:
1. Include `COPY DD-BLOCKS`
2. Create `LK-COUNT BINARY-LONG UNSIGNED` in WORKING-STORAGE
3. Accept no inputs (Blocks-Count takes output parameter only)
4. Call `Blocks-Count` with `LK-COUNT`
5. Write `LK-COUNT=value` to result file
