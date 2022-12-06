 /* lt-07-06.p */

/****************** DEFINE WIDGETS **********************/
DEFINE VARIABLE pickup AS INTEGER INITIAL 1 LABEL "Pickup" VIEW-AS RADIO-SET
   HORIZONTAL RADIO-BUTTONS "Quarter Ton", 1, "Half Ton", 2,
   "One Ton", 3, "Two Ton", 4 TOOLTIP "Select one Pickup.".
DEFINE VARIABLE engine AS INTEGER INITIAL 1 LABEL "Engine" VIEW-AS RADIO-SET
   HORIZONTAL RADIO-BUTTONS "4 Cylinder", 1, "6 Cylinder", 2,
   "8 Cylinder", 3 TOOLTIP "Select one Engine.".
DEFINE VARIABLE p-code AS CHARACTER LABEL "Product Code" INITIAL "6".
DEFINE BUTTON BTN-EXIT LABEL "Exit".

/******************* DEFINE FRAMES **********************/
DEFINE FRAME Frame1
   SKIP(1) pickup SKIP engine SKIP p-code SKIP(1)
   BTN-EXIT WITH SIDE-LABELS CENTERED ROW 2 THREE-D.

/****************** DEFINE TRIGGERS *********************/
ON VALUE-CHANGED OF  pickup, engine 
DO:
   ASSIGN 
   pickup
   engine
   p-code = STRING(pickup) + STRING(engine).
   DISPLAY p-code WITH FRAME Frame1 USE-TEXT.
END.

/******************** MAIN LOGIC ************************/
DISPLAY pickup engine p-code WITH FRAME Frame1.
ENABLE ALL WITH FRAME Frame1.

/******************** WAIT-FOR ************************/
WAIT-FOR CHOOSE OF BTN-EXIT IN FRAME Frame1.

