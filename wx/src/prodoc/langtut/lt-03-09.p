/**********  DEFINE FIELD-LEVEL WIDGETS  **********/
DEFINE BUTTON btn-View-F LABEL "View Frame".
DEFINE BUTTON btn-Hide-F LABEL "Hide Frame".
DEFINE BUTTON btn-View-W LABEL "View Widget".
DEFINE BUTTON btn-Hide-W LABEL "Hide Widget".
DEFINE BUTTON btn-Widget LABEL "Widget".
DEFINE BUTTON btn-Exit LABEL "Exit".

/**********  DEFINE FRAMES  **********/
DEFINE FRAME Frame1
        SKIP(1) btn-View-F btn-Hide-F SKIP(1)
        btn-View-W btn-Hide-W SKIP(1) 
        btn-Exit SKIP(1)
     WITH NO-BOX CENTERED THREE-D.
DEFINE FRAME Frame2
        SKIP(1) btn-Widget SKIP(1)
     WITH TITLE "Frame" CENTERED THREE-D.

/**********  DEFINE TRIGGERS  **********/
ON CHOOSE OF btn-View-F
DO:
    VIEW FRAME Frame2.
END. /* ON CHOOSE OF btn-View-F */
ON CHOOSE OF btn-Hide-F
DO:
    HIDE FRAME Frame2.
END. /* ON CHOOSE OF btn-Hide-F */
ON CHOOSE OF btn-View-W 
DO:
    VIEW btn-Widget IN FRAME Frame2.
END. /* ON CHOOSE OF btn-View-W */
ON CHOOSE OF btn-Hide-W
DO:
    HIDE btn-Widget IN FRAME Frame2.
END. /* ON CHOOSE OF btn-Hide-W */

/**********  MAIN LOGIC  **********/
ENABLE btn-View-F btn-Hide-F btn-View-W btn-Hide-W btn-Exit 
    WITH FRAME Frame1.
ENABLE btn-Widget WITH FRAME Frame2.
WAIT-FOR CHOOSE OF btn-Exit.



