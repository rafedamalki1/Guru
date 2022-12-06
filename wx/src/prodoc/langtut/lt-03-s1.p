/* Solution for Language Tutorial Practice Problem 3-1 */

/**********  DEFINE WIDGETS  **********/
DEFINE RECTANGLE rct-Border SIZE-CHARS 33 BY 8 NO-FILL EDGE-CHARS 1.
DEFINE VARIABLE Name AS CHARACTER FORMAT "x(30)"
    INITIAL "Personal Productivity Tracker" VIEW-AS TEXT.    
DEFINE VARIABLE Work-Day AS DATE LABEL "Date".
DEFINE VARIABLE Hours AS DECIMAL LABEL "Hours".
DEFINE VARIABLE Breaks AS INTEGER LABEL "Breaks".
DEFINE BUTTON btn-Save LABEL "Save".
DEFINE BUTTON btn-Date LABEL "Find Date".
DEFINE BUTTON btn-Print LABEL "Print Report".
DEFINE BUTTON btn-Exit LABEL "Exit".

/**********  DEFINE FRAMES  **********/
DEFINE FRAME Frame1
    rct-Border AT ROW 2 COLUMN 6
    Name AT ROW 3 COLUMN 8
    Work-Day AT ROW 5 COLUMN 8
    Hours AT ROW 6 COLUMN 8
    Breaks AT ROW 7 COLUMN 8
    btn-Save AT ROW 2 COLUMN 40
    btn-Date AT ROW 4 COLUMN 40
    btn-Print AT ROW 6 COLUMN 40
    btn-Exit AT ROW 8 COLUMN 40
        WITH SIDE-LABELS NO-BOX THREE-D.

/**********  DEFINE TRIGGERS  **********/        
ON CHOOSE of btn-Save, btn-Date, btn-Print
DO:
    HIDE MESSAGE NO-PAUSE.
    MESSAGE "Sorry, that function is not yet implemented.".
END.

/**********  MAIN LOGIC  **********/
DISPLAY Name NO-LABEL WITH FRAME Frame1.
ENABLE Work-Day Hours Breaks btn-Save btn-Date btn-Print btn-Exit 
    WITH FRAME Frame1.
WAIT-FOR CHOOSE OF btn-Exit.




