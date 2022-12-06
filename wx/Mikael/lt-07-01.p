/* lt-07-01.p */

/****************** DEFINE WIDGETS **********************/
DEFINE VARIABLE var1 AS CHARACTER LABEL "Column Label"
   INITIAL "Wonderful" FORMAT "x(3)" VIEW-AS FILL-IN.
DEFINE BUTTON BTN-ONE LABEL "DEFINE VARIABLE Options".
DEFINE BUTTON BTN-TWO LABEL "DEFINE FRAME Options".
DEFINE BUTTON BTN-THREE LABEL "Screen Output Options".
DEFINE BUTTON BTN-EXIT LABEL "Exit" TOOLTIP "This is a ToolTip".

/******************* DEFINE FRAMES **********************/
DEFINE FRAME Frame1 var1 WITH THREE-D. /*THREE-D.*/
DEFINE FRAME Frame2
   var1 AT ROW 3 COLUMN 10 NO-LABELS FORMAT "x(6)"
   VIEW-AS TEXT SKIP(2)
   BTN-ONE SKIP BTN-TWO SKIP BTN-THREE SKIP
   BTN-EXIT WITH SIDE-LABELS THREE-D.

/******************** MAIN LOGIC ************************/
FRAME Frame1:HIDDEN = TRUE.
FRAME Frame2:HIDDEN = TRUE.
ENABLE var1 SKIP(2) BTN-ONE SKIP BTN-TWO SKIP BTN-THREE SKIP
   BTN-EXIT WITH FRAME Frame1.
DISPLAY var1 WITH FRAME Frame1.
ENABLE ALL WITH FRAME Frame2.
DISPLAY var1 WITH FRAME Frame2.
ENABLE var1 AT ROW 2 COLUMN 2 LABEL "Side Label" FORMAT "x(9)"
   VIEW-AS EDITOR SIZE 12 BY 3 SKIP(2) BTN-ONE SKIP BTN-TWO SKIP 
   BTN-THREE SKIP BTN-EXIT WITH FRAME Frame3 SIDE-LABELS THREE-D.
FRAME Frame3:HIDDEN = TRUE.
DISPLAY var1 WITH FRAME Frame3.
VIEW FRAME Frame1.
/****************** DEFINE TRIGGERS *********************/
ON CHOOSE OF BTN-ONE IN FRAME Frame1, BTN-ONE IN FRAME Frame2, 
   BTN-ONE IN FRAME Frame3
DO:
   HIDE FRAME Frame2 FRAME Frame3.
   VIEW FRAME Frame1.
END.
ON CHOOSE OF BTN-TWO IN FRAME Frame1, BTN-TWO IN FRAME Frame2, 
   BTN-TWO IN FRAME Frame3
DO:
   HIDE FRAME Frame1 FRAME Frame3.
   VIEW FRAME Frame2.
END.
ON CHOOSE OF BTN-THREE IN FRAME Frame1, BTN-THREE IN FRAME Frame2, 
   BTN-THREE IN FRAME Frame3
DO:
   HIDE FRAME Frame2 FRAME Frame1.
   VIEW FRAME Frame3.
END.


/******************** WAIT-FOR ************************/
WAIT-FOR CHOOSE OF BTN-EXIT IN FRAME Frame1,
   BTN-EXIT IN FRAME Frame2, BTN-EXIT IN FRAME Frame3.
