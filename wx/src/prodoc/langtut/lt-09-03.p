/**********  DEFINE QUERIES  **********/
DEFINE QUERY New-Query FOR Item FIELDS (Item-Num Item-Name Price).

/**********  DEFINE WIDGETS  **********/
DEFINE BROWSE New-Browse QUERY New-Query
    DISPLAY Item-Num Item-Name Price WITH 12 DOWN.
DEFINE BUTTON btn-Q1 LABEL "BY ASCENDING Price ".
DEFINE BUTTON btn-Q2 LABEL "BY DESCENDING Price".
DEFINE BUTTON btn-Q3 LABEL "USE-INDEX Item-Num ".
DEFINE BUTTON btn-Exit LABEL "Exit".

/**********  DEFINE FRAMES  **********/
DEFINE FRAME Frame1 
    SKIP(1) btn-Q1 COLON 50 SPACE(2) SKIP(1) btn-Q2 COLON 50 SKIP(1) 
    btn-Q3 COLON 50 SKIP(2) btn-Exit COLON 50
    New-Browse AT ROW 2 COLUMN 2 SKIP(1)
        WITH TITLE "Query Results" AT ROW 2 COLUMN 2 THREE-D.

/**********  DEFINE TRGGERS  **********/
ON CHOOSE OF btn-Q1
DO:
    OPEN QUERY New-Query FOR EACH Item WHERE Item-Name BEGINS "S" 
        BY Price.
    DISPLAY New-Browse WITH FRAME Frame1.
    MESSAGE "FOR EACH Item WHERE Item-Name BEGINS ""S"" BY Price".
END.
ON CHOOSE OF btn-Q2
DO:
    OPEN QUERY New-Query FOR EACH Item WHERE Item-Name BEGINS "S"
        BY Price DESCENDING.
    DISPLAY New-Browse WITH FRAME Frame1.
    MESSAGE "FOR EACH Item WHERE Item-Name BEGINS ""S"" BY Price DESCENDING".
END.
ON CHOOSE OF btn-Q3
DO:
    OPEN QUERY New-Query FOR EACH Item WHERE Item-Name BEGINS "S" 
        USE-INDEX Item-Num.
    DISPLAY New-Browse WITH FRAME Frame1.
    MESSAGE "FOR EACH Item WHERE Item-Name BEGINS ""S"" USE-INDEX Item-Num". 
END.

/**********  MAIN LOGIC  **********/
ENABLE ALL WITH FRAME Frame1.
WAIT-FOR CHOOSE OF btn-Exit.



