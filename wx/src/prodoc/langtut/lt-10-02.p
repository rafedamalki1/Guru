/**********  DEFINE WIDGETS  **********/
DEFINE VARIABLE Bal-due AS LOGICAL LABEL "Balance Due?" 
    VIEW-AS TOGGLE-BOX.
DEFINE VARIABLE Rep-Editor AS CHARACTER VIEW-AS EDITOR 
    SCROLLBAR-VERTICAL SIZE 76 BY 13.
DEFINE VARIABLE Stat AS LOGICAL.

DEFINE BUTTON b-rep LABEL "Report".
DEFINE BUTTON b-exit LABEL "Exit".
DEFINE BUTTON b-ok LABEL "OK" AUTO-GO.

/**********  DEFINE FRAMES  **********/
DEFINE FRAME Frame1
    SKIP(1)
    b-rep SKIP(1)
    b-exit  
        WITH NO-BOX CENTERED THREE-D.
        
DEFINE FRAME Dialog1
    Rep-Editor SKIP(1)
    b-ok TO 40 SKIP(1)  
        WITH NO-LABELS VIEW-AS DIALOG-BOX SCROLLABLE. 
         
/**********  DEFINE TRIGGERS  **********/
ON CHOOSE of b-rep
DO:  
    OUTPUT TO "tut-temp.txt".
    FOR EACH Customer WITH STREAM-IO:
        IF Balance > 0 THEN Bal-due = YES. 
        ELSE Bal-due = NO.
        DISPLAY Name Bal-due.
    END.
    OUTPUT CLOSE.
    
    ASSIGN Rep-Editor:READ-ONLY IN FRAME Dialog1 = YES
         Rep-Editor:SENSITIVE IN FRAME Dialog1 = YES 
         FRAME Dialog1:TITLE = "Report Output"
         Stat = Rep-Editor:READ-FILE("tut-temp.txt") IN FRAME Dialog1. 
             
    IF Stat THEN DO:
        ENABLE Rep-Editor b-ok WITH FRAME Dialog1.
        WAIT-FOR GO OF FRAME Dialog1.
        HIDE FRAME Dialog1.
    END.
END.

/**********  MAIN LOGIC  **********/  
ASSIGN Rep-Editor:FONT = 3.
ENABLE ALL WITH FRAME Frame1.
WAIT-FOR CHOOSE OF b-exit.





