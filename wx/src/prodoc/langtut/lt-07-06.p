/**********  DEFINE WIDGETS  **********/
DEFINE VARIABLE Pickup AS INTEGER INITIAL 1 VIEW-AS RADIO-SET  
   HORIZONTAL RADIO-BUTTONS "Quarter Ton", 1, "Half Ton", 2, 
   "One Ton", 3, "Two Ton", 4 TOOLTIP "Select one Pickup".
DEFINE VARIABLE Engine AS INTEGER INITIAL 1 VIEW-AS RADIO-SET 
    HORIZONTAL RADIO-BUTTONS "4 Cylinder", 1, "6 Cylinder", 2, 
    "8 Cylinder", 3 TOOLTIP "Select one Engine".
DEFINE VARIABLE P-code AS CHARACTER LABEL "Product Code" INITIAL "11". 
DEFINE BUTTON btn-Exit LABEL "Exit".

/**********  DEFINE FRAMES  **********/
DEFINE FRAME Frame1
    SKIP(1) 
    Pickup SKIP
    Engine SKIP
    P-code SKIP(1)
    btn-Exit
        WITH SIDE-LABELS CENTERED ROW 2 THREE-D.
        
/**********  DEFINE TRIGGERS  **********/
ON VALUE-CHANGED OF Pickup, Engine
DO:
    ASSIGN Pickup
           Engine
           P-code = STRING(Pickup) + STRING(Engine).
    DISPLAY P-code WITH FRAME Frame1.
END.
        
/**********  MAIN LOGIC  **********/
DISPLAY Pickup Engine P-code WITH FRAME Frame1.
ENABLE ALL WITH FRAME Frame1.
WAIT-FOR CHOOSE OF btn-Exit.


