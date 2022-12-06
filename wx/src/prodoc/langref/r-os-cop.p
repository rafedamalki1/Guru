
DEFINE VARIABLE sourcefilename AS CHARACTER NO-UNDO.
DEFINE VARIABLE copyfilename AS CHARACTER FORMAT "x(20)" VIEW-AS FILL-IN.
DEFINE VARIABLE OKpressed AS LOGICAL INITIAL TRUE.

Main:
REPEAT:
    SYSTEM-DIALOG GET-FILE sourcefilename
	TITLE  "Choose File to Copy"
	MUST-EXIST
	USE-FILENAME
	UPDATE OKpressed.
      
    IF OKpressed = FALSE THEN
	LEAVE Main.
    
    UPDATE copyfilename WITH FRAME copyframe.      
    OS-COPY VALUE(sourcefilename) VALUE(copyfilename).     
END.
