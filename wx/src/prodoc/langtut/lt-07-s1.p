/* Solution to Language Tutorial Problem 7-1 */

/**********  DEFINE WIDGETS  **********/
DEFINE VARIABLE List-Price AS DECIMAL LABEL "List Price".
DEFINE VARIABLE Disc AS DECIMAL FORMAT ">>9.99%" LABEL "Discount".
DEFINE VARIABLE Tax-Rate AS DECIMAL FORMAT ">>9.99%" LABEL "Tax Rate".
DEFINE VARIABLE Adj-Price LIKE List-Price LABEL "Adjusted Price" 
    VIEW-AS TEXT.
DEFINE VARIABLE Tax AS DECIMAL LABEL "Tax" VIEW-AS TEXT.
DEFINE VARIABLE Tot AS DECIMAL LABEL "Total Cost" VIEW-AS TEXT.
DEFINE BUTTON btn-Exit LABEL "Exit".
       
/**********  DEFINE FRAMES  **********/
DEFINE FRAME Frame1
    SKIP (1)
    List-Price COLON 12  Adj-price COLON 42
    Disc       COLON 12  Tax       COLON 42
    Tax-Rate   COLON 12  Tot       COLON 42 SKIP (2)
    btn-Exit   COLON 12       
        WITH SIDE-LABELS NO-BOX CENTERED THREE-D.
              
/**********  DEFINE TRIGGERS  **********/             
ON LEAVE OF Tax-Rate
DO:
    ASSIGN List-Price Disc Tax-Rate   
           Adj-Price = List-Price - (List-Price * (Disc / 100.0))
           Tax = Adj-Price * (Tax-Rate / 100.0)
           Tot = Adj-Price + Tax.            
    DISPLAY Adj-Price Tax Tot WITH FRAME Frame1.       
END.

/**********  MAIN LOGIC  **********/
ASSIGN List-Price:HELP = "Enter List Price"
       Disc:HELP = "Enter Discount Rate"
       Tax-Rate:HELP = "Enter Tax Rate".             
ENABLE ALL WITH FRAME Frame1.
WAIT-FOR CHOOSE OF btn-Exit.


      
                                                                                                        
                                                                                                                                          
       
