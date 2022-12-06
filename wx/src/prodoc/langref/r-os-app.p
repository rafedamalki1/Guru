
DEFINE VARIABLE sourcefile AS CHARACTER NO-UNDO.
DEFINE VARIABLE targetfile AS CHARACTER FORMAT "x(20)" VIEW-AS FILL-IN.
DEFINE VARIABLE OKpressed AS LOGICAL INITIAL TRUE.

Main:
 REPEAT:
	SYSTEM-DIALOG GET-FILE sourcefile
		TITLE "Choose Source File For Append"
		MUST-EXIST
		USE-FILENAME
		UPDATE OKpressed.
	
	IF OKpressed = FALSE THEN
		LEAVE Main.

	UPDATE targetfile WITH FRAME appendframe.
	OS-APPEND VALUE(sourcefile) VALUE(targetfile).
END.