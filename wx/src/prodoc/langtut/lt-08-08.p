 /**********  DEFINE WIDGETS  **********/
DEFINE VARIABLE Reps AS CHARACTER VIEW-AS SELECTION-LIST 
    INNER-CHARS 25 INNER-LINES 9 SORT.
DEFINE VARIABLE Stat AS LOGICAL.
DEFINE BUTTON btn-Exit LABEL "Exit".


/**********  DEFINE FRAMES  **********/
DEFINE FRAME Frame1
    Reps NO-LABEL      AT ROW 2 COLUMN 3
    Salesrep.Sales-Rep AT ROW 2 COLUMN 35
    Salesrep.Rep-Name  FORMAT "x(20)" AT ROW 3 COLUMN 35
    Salesrep.Region    AT ROW 4 COLUMN 38
    btn-Exit           AT ROW 9 COLUMN 3 SKIP(1)
      WITH SIDE-LABELS CENTERED ROW 2 TITLE "Update Sales Rep Info"
       THREE-D.


/**********  DEFINE TRIGGERS  **********/
ON DEFAULT-ACTION OF Reps
DO:
    ASSIGN Reps.
    FIND FIRST Salesrep WHERE Salesrep.Rep-Name = Reps.
    DISPLAY Salesrep.Sales-Rep Salesrep.Rep-Name Salesrep.Region 
        WITH FRAME Frame1.
END.


/**********  MAIN LOGIC  **********/
Reps:DELIMITER = "*".
FOR EACH Salesrep FIELDS (Rep-Name) BY Salesrep.Rep-Name:
    Stat = Reps:ADD-LAST(Salesrep.Rep-Name).
END.
FIND FIRST Salesrep.
DISPLAY Reps Salesrep.Sales-Rep Salesrep.Rep-Name Salesrep.Region
    WITH FRAME Frame1.
ENABLE ALL WITH FRAME Frame1.
WAIT-FOR CHOOSE of btn-Exit.



