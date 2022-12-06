/**********  DEFINE WIDGETS  **********/
DEFINE VARIABLE Var1 AS CHARACTER LABEL "Column Label" 
    INITIAL "Wonderful" FORMAT "x(3)" VIEW-AS FILL-IN.
DEFINE BUTTON btn-one LABEL "DEFINE VARIABLE Options".
DEFINE BUTTON btn-two LABEL "DEFINE FRAME Options".
DEFINE BUTTON btn-three LABEL "Screen Output Options".
DEFINE BUTTON btn-exit LABEL "Exit" TOOLTIP "This is a TOOLTIP".
/**********  DEFINE FRAMES  **********/    
DEFINE FRAME Frame1 Var1 WITH THREE-D.
DEFINE FRAME Frame2
    Var1 AT ROW 3 COLUMN 10 NO-LABELS FORMAT "x(6)" VIEW-AS TEXT SKIP(2)
    btn-one SKIP(1)
    btn-two SKIP(1)
    btn-three SKIP(1)
    btn-exit WITH SIDE-LABELS THREE-D.   
/********** MAIN LOGIC  **********/
FRAME Frame1:HIDDEN = TRUE.
FRAME Frame2:HIDDEN = TRUE.
ENABLE Var1 SKIP(2) btn-one SKIP(1) btn-two SKIP(1) btn-three SKIP(1) 
    btn-exit WITH FRAME Frame1.
DISPLAY Var1 WITH FRAME Frame1.
ENABLE ALL WITH FRAME Frame2.
DISPLAY Var1 WITH FRAME Frame2.
ENABLE Var1 AT ROW 2 COLUMN 2 LABEL "Side Label" FORMAT "x(9)" 
    VIEW-AS EDITOR SIZE 12 BY 3 SKIP(2) btn-one SKIP(1) btn-two SKIP(1) 
    btn-three SKIP(1) btn-exit WITH FRAME Frame3 SIDE-LABELS THREE-D.
FRAME Frame3:HIDDEN = TRUE.
DISPLAY Var1 WITH FRAME Frame3.
VIEW FRAME Frame1.
/********** DEFINE TRIGGERS  **********/
ON CHOOSE OF btn-one IN FRAME Frame1, btn-one IN FRAME Frame2,
    btn-one IN FRAME Frame3 DO:
        HIDE FRAME Frame2 FRAME Frame3.
        VIEW FRAME Frame1.
END.
ON CHOOSE OF btn-two IN FRAME Frame1, btn-two IN FRAME Frame2,
    btn-two IN FRAME Frame3 DO:
        HIDE FRAME Frame1 FRAME Frame3.
        VIEW FRAME Frame2.
END.
ON CHOOSE OF btn-three IN FRAME Frame1, btn-three IN FRAME Frame2,
    btn-three IN FRAME Frame3 DO:
        HIDE FRAME Frame1 FRAME Frame2.
        VIEW FRAME Frame3.
END.
/**********  WAIT-FOR  **********/
WAIT-FOR CHOOSE OF btn-Exit IN FRAME Frame1, 
    btn-Exit IN FRAME Frame2, btn-Exit IN FRAME Frame3.
    
    
    
    
    
