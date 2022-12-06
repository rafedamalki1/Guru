{lt-10-in.i} /* Common Interface Setup Code */  
 
/**********  DEFINE TRIGGERS  **********/
ON CHOOSE of b-rep
DO:  
    OUTPUT TO "tut-temp.txt".
    FOR EACH Customer BREAK BY Sales-rep WITH STREAM-IO:
        DISPLAY Sales-rep Name 
            Balance (COUNT TOTAL AVERAGE BY Sales-rep).
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
ENABLE ALL WITH FRAME Frame1.
WAIT-FOR CHOOSE OF b-exit.
