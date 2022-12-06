/* p-threed.p */

DEFINE BUTTON bOK LABEL "OK".
DEFINE VARIABLE vSlide AS INTEGER LABEL "Slider"
    VIEW-AS SLIDER MAX-VALUE 10 MIN-VALUE 1.
DEFINE VARIABLE vToggle AS LOGICAL LABEL "Toggle"
    VIEW-AS TOGGLE-BOX.
DEFINE VARIABLE vRadio AS INTEGER LABEL "Radio Set"
    VIEW-AS RADIO-SET RADIO-BUTTONS 
    "First", 1, "Middle", 2, "Last", 3.
DEFINE VARIABLE vEdit AS CHARACTER LABEL "Editor"
    VIEW-AS EDITOR SIZE 60 by 2.
DEFINE VARIABLE vCombo AS CHARACTER LABEL "Combo Box"
    VIEW-AS COMBO-BOX LIST-ITEMS "Red", "White",
     "Blue", "Purple".
DEFINE FRAME D2-Frame 
    vSlide vToggle vRadio SKIP vEdit SKIP vCombo SKIP bOK
WITH AT COLUMN 8 ROW 1.1 SIDE-LABELS 
     TITLE "Two Dimensional Frame".
DEFINE FRAME D3-Frame
    vSlide vToggle vRadio SKIP vEdit SKIP vCombo SKIP bOK
WITH AT COLUMN 8 ROW 9 THREE-D SIDE-LABELS 
     TITLE "Three Dimensional Frame". 
     
CURRENT-WINDOW:TITLE = "".
ENABLE ALL WITH FRAME D2-Frame.
ENABLE ALL WITH FRAME D3-Frame.

WAIT-FOR CHOOSE OF bOK IN FRAME D2-Frame OR
         CHOOSE OF bOK IN FRAME D3-Frame.

