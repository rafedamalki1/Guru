{lt-10-in.i} /* Common Interface Setup Code */  
  
/**********  DEFINE TRIGGERS  **********/
ON CHOOSE of b-rep
DO:  
    DEFINE VARIABLE Holder AS CHARACTER FORMAT "x(25)".

    DEFINE FRAME f-body
        Name NO-LABEL
        Balance AT 40 FORMAT "$zzz,zz9.99 CR" SKIP
        Contact
        Credit-Limit AT 40 FORMAT "$zzz,zz9.99 CR" SKIP
        Address NO-LABEL SKIP
        Holder NO-LABEL SKIP
        Phone SKIP(2)
            WITH SIDE-LABELS STREAM-IO.
    
    OUTPUT TO "tut-temp.txt" PAGE-SIZE 25.
        
    FOR EACH Customer FIELDS (Balance Sales-Rep Name Contact Credit-Limit
      Address City State Postal-Code Phone)
      WHERE Balance >= 1400 BREAK BY Sales-Rep:
    
        DEFINE FRAME f-hdr HEADER
            "Date:" TODAY "Customer Report" AT 25
            sales-rep AT 55
            "Page" AT 65
            PAGE-NUMBER FORMAT ">>9" SKIP(1)
                WITH PAGE-TOP FRAME f-hdr STREAM-IO.

        DEFINE FRAME f-ftr HEADER 
            "Customer Report"
            "continued next page"
                WITH FRAME f-ftr PAGE-BOTTOM CENTERED STREAM-IO.
            
        VIEW FRAME f-hdr.
        VIEW FRAME f-ftr.
        DISPLAY Name Balance Contact Credit-Limit Address
            (City + ", " + St + ", " + Postal-Code) @ Holder Phone
            WITH FRAME f-body.
            
        IF LAST-OF(Sales-Rep) THEN DO:
            HIDE FRAME f-ftr.
            PAGE.
        END.
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
