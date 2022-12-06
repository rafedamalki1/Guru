/**********  DEFINE QUERIES  **********/
DEFINE QUERY New-Query FOR Item FIELDS (Item-Num Item-Name Price).

/**********  DEFINE WIDGETS  **********/
DEFINE BROWSE New-Browse QUERY New-Query
    DISPLAY Item-Num Item-Name Price WITH 12 DOWN.
DEFINE BUTTON btn-Q1 LABEL "WHERE Expression Query".
DEFINE BUTTON btn-Q2 LABEL "WHERE BEGINS Query".
DEFINE BUTTON btn-Q3 LABEL "WHERE MATCHES Query".
DEFINE BUTTON btn-Q4 LABEL "USING Item-Num Query".
DEFINE BUTTON btn-OK LABEL "OK" AUTO-GO.
DEFINE BUTTON btn-Cancel LABEL "Cancel" AUTO-ENDKEY.
DEFINE BUTTON btn-Exit LABEL "Exit".

/**********  DEFINE FRAMES  **********/
DEFINE FRAME Frame1 
    SKIP(1) btn-Q1 COLON 50 SPACE(2) SKIP(1) btn-Q2 COLON 50 SKIP(1) 
    btn-Q3 COLON 50 SKIP(1) btn-Q4 COLON 50 SKIP(2) btn-Exit COLON 50
    New-Browse AT ROW 2 COLUMN 2 SKIP(1) 
        WITH TITLE "Query Results" AT ROW 2 COLUMN 2 THREE-D.

/**********  DEFINE TRIGGERS  **********/
ON CHOOSE OF btn-Q1
DO:
    OPEN QUERY New-Query FOR EACH Item WHERE Item-Num > 45.
    DISPLAY New-Browse WITH FRAME Frame1.
    MESSAGE "FOR EACH Item WHERE Item-Num > 45".
END.
ON CHOOSE OF btn-Q2
DO:
    OPEN QUERY New-Query FOR EACH Item WHERE Item-Name BEGINS "ski".
    DISPLAY New-Browse WITH FRAME Frame1.
    MESSAGE "FOR EACH Item WHERE Item-Name BEGINS ""ski""".
END.
ON CHOOSE OF btn-Q3
DO:
    OPEN QUERY New-Query FOR EACH Item WHERE Item-Name MATCHES "*ball".
    DISPLAY New-Browse WITH FRAME Frame1.
    MESSAGE "FOR EACH Item WHERE Item-Name MATCHES ""*ball""".
END.
ON CHOOSE OF btn-Q4
DO:
    PROMPT-FOR Item.Item-Num SKIP btn-OK btn-Cancel
        WITH FRAME Frame3 VIEW-AS DIALOG-BOX TITLE "New Query".
    OPEN QUERY New-Query FOR EACH Item USING Item-Num.
    DISPLAY New-Browse WITH FRAME Frame1.
    MESSAGE "FOR EACH Item USING Item-Num".
END.

/**********  MAIN LOGIC  **********/
ENABLE ALL WITH FRAME Frame1.
WAIT-FOR CHOOSE OF btn-Exit.




