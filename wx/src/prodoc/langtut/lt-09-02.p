/**********  DEFINE QUERIES  **********/
DEFINE QUERY New-Query FOR Item FIELDS (Item-Num Item-Name Price).

/**********  DEFINE WIDGETS  **********/
DEFINE BROWSE New-Browse QUERY New-Query
    DISPLAY Item-Num Item-Name (Price / 100) LABEL "Shipping Insurance"
        WITH 12 DOWN. 
DEFINE BUTTON btn-Exit LABEL "Exit".

/**********  DEFINE FRAMES  **********/
DEFINE FRAME Frame1 
    New-Browse AT ROW 1 COLUMN 2
    btn-Exit   AT ROW 1 COLUMN 60
        WITH NO-BOX THREE-D. 
       

/**********  MAIN LOGIC  **********/
OPEN QUERY New-Query FOR EACH Item BY (PRICE / 100) DESCENDING.
DISPLAY New-Browse WITH FRAME Frame1.
ENABLE ALL WITH FRAME Frame1.
WAIT-FOR CHOOSE OF btn-Exit.



