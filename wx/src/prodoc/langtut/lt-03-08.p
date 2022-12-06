/**********  DEFINE FIELD-LEVEL WIDGETS  **********/
DEFINE BUTTON btn-One LABEL "#1".
DEFINE BUTTON btn-Two LABEL "#2".
DEFINE BUTTON btn-Exit LABEL "Exit".

/**********  DEFINE FRAMES  **********/
DEFINE FRAME Frame1
    SKIP(2) btn-One btn-Two btn-Exit
        WITH NO-BOX CENTERED THREE-D.    
	
/**********  DEFINE TRIGGERS  **********/
ON CHOOSE OF btn-One
DO:
    ENABLE btn-Two WITH FRAME Frame1.
    DISABLE btn-One WITH FRAME Frame1.
END. /* ON CHOOSE OF btn-One */

ON CHOOSE OF btn-Two
DO:
    ENABLE btn-One WITH FRAME Frame1.
    DISABLE btn-Two WITH FRAME Frame1.
END. /* ON CHOOSE OF btn-Two */

/**********  MAIN LOGIC  **********/
ENABLE btn-One btn-Exit WITH FRAME Frame1.

WAIT-FOR CHOOSE OF btn-Exit.



