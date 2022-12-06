{lt-10-in.i} /* Common Interface Setup Code */  
 
/**********  DEFINE TRIGGERS  **********/
ON CHOOSE of b-rep
DO:  
    OUTPUT TO "tut-temp.txt".
    FOR EACH Customer FIELDS (Balance Postal-Code Contact Name Address 
        Address2 City St) WHERE Balance >= 1400 BY Postal-Code 
        WITH STREAM-IO:
        PUT Contact SKIP
            Name SKIP
            Address SKIP
            Address2 SKIP
            City St Postal-Code SKIP(1).
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

        
