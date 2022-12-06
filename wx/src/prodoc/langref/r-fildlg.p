/* r-fildlg.p */

DEFINE VARIABLE procname AS CHARACTER NO-UNDO.
DEFINE VARIABLE OKpressed AS LOGICAL INITIAL TRUE.

Main: REPEAT:
    SYSTEM-DIALOG GET-FILE procname
        TITLE      "Choose Procedure to Run ..."
        FILTERS    "Source Files (*.p)"   "*.p",
                   "R-code Files (*.r)"   "*.r"
        MUST-EXIST
        USE-FILENAME
	UPDATE OKpressed
    .
      
    IF OKpressed = TRUE THEN
        RUN VALUE(procname).
    ELSE      
        LEAVE Main.            
END.

