/**********  DEFINE QUERY  **********/
DEFINE QUERY Item-Query FOR Item SCROLLING.

/**********  DEFINE VARIABLES  **********/
DEFINE VARIABLE Method-Status AS LOGICAL.
DEFINE VARIABLE Current-Record AS ROWID.

/**********  DEFINE BROWSE  **********/
DEFINE BROWSE Item-Browse QUERY Item-Query DISPLAY Item-Num 
    Item-Name Price On-hand Cat-Description WIDTH 18
    ENABLE Item-Name Price On-hand Cat-Description WITH 10 DOWN SEPARATORS.

/**********  DEFINE WIDGETS  **********/
DEFINE BUTTON btn-Add LABEL "Add".
DEFINE BUTTON btn-Delete LABEL "Delete".
DEFINE BUTTON btn-Exit LABEL "Exit".

/**********  DEFINE FRAMES  **********/
DEFINE FRAME Frame1
    btn-Add AT COLUMN 2 ROW 12 btn-Delete btn-Exit SKIP(1) 
    Item-Browse AT ROW 2 COLUMN 2
        WITH SIDE-LABELS USE-TEXT CENTERED THREE-D 
        ROW 2 TITLE "Database Access Form for the Item Table".

/**********  DEFINE TRIGGERS  **********/
ON CHOOSE OF btn-Add DO:
    Method-Status = Item-Browse:INSERT-ROW("AFTER").
END.

ON ROW-LEAVE OF BROWSE Item-Browse DO:
    IF Item-Browse:NEW-ROW IN FRAME Frame1 THEN DO:
        CREATE Item.
        ASSIGN Item-Num = NEXT-VALUE(next-item-num) 
            INPUT BROWSE Item-Browse Item-Name Price On-hand Cat-Description.
        Method-Status = Item-Browse:CREATE-RESULT-LIST-ENTRY(). 
        DISPLAY Item-Num WITH BROWSE Item-Browse.
        END.
    END.

ON CHOOSE OF btn-Delete DO:
    GET CURRENT Item-Query EXCLUSIVE-LOCK NO-WAIT.
    DELETE Item.
    Method-Status = Item-Browse:DELETE-SELECTED-ROWS().
END.

/**********  MAIN LOGIC  **********/
OPEN QUERY Item-Query FOR EACH Item NO-LOCK.
ENABLE ALL WITH FRAME Frame1.
WAIT-FOR CHOOSE OF btn-Exit.
CLOSE QUERY Item-Query.




