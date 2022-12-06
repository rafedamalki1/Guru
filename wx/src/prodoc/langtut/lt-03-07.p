 /**********  DEFINE FIELD-LEVEL WIDGETS  **********/
DEFINE RECTANGLE rct-Border SIZE-CHARS 40 BY 7 NO-FILL EDGE-CHARS 1.
DEFINE VARIABLE Name1 AS CHARACTER FORMAT "x(19)"
    INITIAL "A L L   A R O U N D" VIEW-AS TEXT.
DEFINE VARIABLE Name2 AS CHARACTER FORMAT "x(19)" 
    INITIAL "    S P O R T S    " VIEW-AS TEXT.
DEFINE VARIABLE Username AS CHARACTER FORMAT "x(3)" LABEL "Initials".
DEFINE BUTTON btn-Cust LABEL "Customers".
DEFINE BUTTON btn-Order LABEL "Order Entry". 
DEFINE BUTTON btn-Inv LABEL "Inventory".
DEFINE BUTTON btn-Exit LABEL "Exit".

/**********  DEFINE FRAMES  **********/
DEFINE FRAME Frame1
    rct-Border AT ROW  2 COLUMN 21 
    Name1      AT ROW  4 COLUMN 31
    Name2      AT ROW  6 COLUMN 31
    Username   AT ROW 11 COLUMN 34
        WITH SIDE-LABELS NO-BOX THREE-D.    
           
DEFINE FRAME Frame2 
    btn-Cust btn-Order btn-Inv btn-Exit
        WITH NO-BOX AT ROW 13 COLUMN 21 THREE-D.
           
/**********  DEFINE TRIGGERS  **********/            
ON CHOOSE OF btn-Cust, btn-Order, btn-Inv
DO:
    HIDE MESSAGE NO-PAUSE.
    MESSAGE "Sorry, that function is not yet implemented.".
END.  /* ON CHOOSE OF btn-Cust, btn-Order, but-Inv */

/**********  MAIN LOGIC  **********/
DISPLAY Name1 NO-LABEL Name2 NO-LABEL Username WITH FRAME Frame1.
ENABLE Username WITH FRAME Frame1.
WAIT-FOR RETURN OF Username.
DISABLE Username WITH FRAME Frame1.

ENABLE ALL WITH FRAME Frame2.
WAIT-FOR CHOOSE OF btn-Exit.



