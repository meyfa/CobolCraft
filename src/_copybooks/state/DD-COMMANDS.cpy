*> --- Copybook: registered commands ---

*> The number of commands that can be registered.
78 MAX-COMMANDS VALUE 100.

*> Registered commands.
01 COMMANDS EXTERNAL.
    02 COMMAND-COUNT BINARY-LONG UNSIGNED.
    02 COMMAND OCCURS MAX-COMMANDS TIMES.
        *> The command name.
        03 COMMAND-NAME PIC X(100).
        *> The command help text.
        *> TODO: Synthesize this from the command's tree structure.
        03 COMMAND-HELP PIC X(255).
        *> Called when the command is executed.
        03 COMMAND-PTR-EXECUTE USAGE PROGRAM-POINTER.
