/**********  DEFINE WIDGETS  **********/
{lt-11-mn.i} /* Menu definition */

DEFINE VARIABLE Rep-Editor AS CHARACTER VIEW-AS EDITOR 
    SCROLLBAR-VERTICAL SIZE 76 BY 13.
DEFINE VARIABLE Stat AS LOGICAL.

/**********  DEFINE FRAMES  **********/
DEFINE FRAME Frame1
    Rep-Editor WITH NO-LABELS ROW 2 CENTERED TITLE "Report Output".
    
/**********  DEFINE TRIGGERS  **********/
ON CHOOSE OF MENU-ITEM mi-Exit 
    APPLY "CLOSE-WINDOW" TO DEFAULT-WINDOW.  
     
ON CHOOSE OF MENU-ITEM mi-Labels IN MENU sm-Reports
    RUN p-Report.

/**********  MAIN LOGIC  **********/
ASSIGN Rep-Editor:READ-ONLY IN FRAME Frame1 = YES
       Rep-Editor:FONT = 3.
ENABLE ALL WITH FRAME Frame1.
WAIT-FOR CHOOSE OF MENU-ITEM mi-Exit.

/**********  INTERNAL PROCEDURES  **********/
PROCEDURE p-Report:    
OUTPUT TO "tut-temp.txt".
FOR EACH Customer FIELDS (Balance Postal-Code Contact Name Address
    Address2 City St) WHERE Balance >= 1400 BY Postal-Code 
    WITH STREAM-IO:
    PUT Contact SKIP
        Name SKIP
        Address SKIP.     
    IF Address2 NE "" THEN PUT Address2 SKIP.    
    PUT City + "," + St + " " + STRING(Postal-Code, "99999")
        FORMAT "x(23)" SKIP(1).
    IF Address2 EQ "" THEN PUT SKIP(1).
END. 
OUTPUT CLOSE.
    
ASSIGN Stat = Rep-Editor:READ-FILE("tut-temp.txt") IN FRAME Frame1. 
END PROCEDURE.


        
