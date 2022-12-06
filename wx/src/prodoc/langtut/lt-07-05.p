/**********  DEFINE WIDGETS  **********/
DEFINE VARIABLE Item AS CHARACTER INITIAL "Backpack". 
DEFINE VARIABLE Price AS DECIMAL INITIAL 29.95.
DEFINE VARIABLE Tax AS DECIMAL INITIAL 0.00.
DEFINE VARIABLE TOTAL AS DECIMAL.
DEFINE VARIABLE Taxable AS LOGICAL LABEL "Taxable Sale?" 
    VIEW-AS TOGGLE-BOX TOOLTIP "Checkmark indicates tax is included". 
DEFINE BUTTON btn-Exit Label "Exit".

/**********  DEFINE FRAMES  ************/
DEFINE FRAME Frame1                                                        
    SKIP(1) Item COLON 8 SKIP
    Price COLON 8 SKIP
    Tax COLON 8 SKIP
    TOTAL COLON 8 SKIP(2)
    Taxable SKIP(2)
    btn-Exit
        WITH NO-BOX CENTERED SIDE-LABELS USE-TEXT THREE-D.

/**********  DEFINE TRIGGERS  **********/
ON VALUE-CHANGED OF Taxable
DO:
    ASSIGN Taxable.
    IF Taxable = YES THEN
        ASSIGN Tax = Price * 0.05
               TOTAL = Price + Tax.
    ELSE
        ASSIGN Tax = 0.00
               TOTAL = Price + Tax.
    DISPLAY Tax Total WITH FRAME Frame1.
END.

/********** MAIN LOGIC  **********/
DISPLAY Item Price Tax TOTAL Taxable WITH FRAME Frame1.
ENABLE ALL WITH FRAME Frame1.

WAIT-FOR CHOOSE OF btn-Exit.



