DEFINE BUTTON stop-it LABEL "STOP".

DEFINE VARIABLE i AS INTEGER.

DEFINE VARIABLE stop-sel AS LOGICAL INITIAL FALSE.

DISPLAY stop-it.

ON CHOOSE OF stop-it
   stop-sel = TRUE.

ENABLE stop-it.
    
   
DO i = 1 TO 1000:  
	DISPLAY i VIEW-AS TEXT.
	
	PROCESS EVENTS.
	 
	IF stop-sel 
	THEN LEAVE. 
END.
 