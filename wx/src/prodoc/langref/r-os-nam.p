DEFINE VARIABLE sourcefile AS CHARACTER NO-UNDO.
DEFINE VARIABLE targetfile AS CHARACTER FORMAT "x(20)" VIEW-AS FILL-IN.
DEFINE VARIABLE OKpressed AS LOGICAL INITIAL TRUE.

Main:
REPEAT:
        SYSTEM-DIALOG GET-FILE sourcefile
                TITLE "Choose a File or Directory to Rename"
                MUST-EXIST
                USE-FILENAME
                UPDATE OKpressed.

        IF OKpressed = FALSE THEN
                LEAVE Main.
        UPDATE targetfile WITH FRAME newnameframe.
        OS-RENAME VALUE(sourcefile) VALUE(targetfile).
END.

