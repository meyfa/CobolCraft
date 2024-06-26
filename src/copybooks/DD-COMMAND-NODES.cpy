*> --- Copybook: data structure for command parser nodes ---
*> Usage: COPY DD-COMMAND-NODES REPLACING LEADING ==PREFIX== BY ==<name>==.

01 PREFIX-NODES.
    02 PREFIX-NODE-COUNT                            BINARY-LONG UNSIGNED.
    02 PREFIX-NODE OCCURS 256 TIMES.
        03 PREFIX-NODE-FLAGS.
            04 PREFIX-NODE-TYPE                     BINARY-CHAR UNSIGNED.
            04 PREFIX-NODE-EXECUTABLE               BINARY-CHAR UNSIGNED.
            04 PREFIX-NODE-HAS-REDIRECT             BINARY-CHAR UNSIGNED.
            04 PREFIX-NODE-HAS-SUGGESTIONS-TYPE     BINARY-CHAR UNSIGNED.
        03 PREFIX-NODE-CHILDREN.
            04 PREFIX-NODE-CHILD-COUNT              BINARY-LONG UNSIGNED.
            04 PREFIX-NODE-CHILD OCCURS 256 TIMES.
                05 PREFIX-NODE-CHILD-INDEX          BINARY-LONG UNSIGNED.
        03 PREFIX-NODE-REDIRECT-INDEX               BINARY-LONG UNSIGNED.
        03 PREFIX-NODE-NAME                         PIC X(256).
        03 PREFIX-NODE-PARSER                       BINARY-LONG UNSIGNED.
        03 PREFIX-NODE-PROPERTIES-LENGTH            BINARY-LONG UNSIGNED.
        03 PREFIX-NODE-PROPERTIES                   PIC X(256).
        03 PREFIX-NODE-SUGGESTIONS-TYPE             PIC X(32).
