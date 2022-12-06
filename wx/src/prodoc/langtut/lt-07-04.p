/**********  DEFINE WIDGETS  **********/
DEFINE VARIABLE Field1 AS CHARACTER INITIAL "Home"
    FORMAT "x(10)" LABEL "Start".
DEFINE VARIABLE Field2 AS CHARACTER INITIAL "Joe's Deli" 
    FORMAT "x(10)" LABEL "Destination".
DEFINE BUTTON btn-Exit LABEL "Exit".

/**********  DEFINE FRAMES  **********/
DEFINE FRAME Frame1
    SKIP(2) Field1 SPACE (10) Field2 SKIP(2)
    btn-Exit
        WITH NO-BOX CENTERED SIDE-LABELS THREE-D.

/**********  DEFINE TRIGGERS **********/
ON ENTRY OF Field2 
DO:
    MESSAGE "Can I take your order, please?".
END.
ON LEAVE OF Field2
DO:
    MESSAGE "Hey, where's the tip!".
END.

/*********** MAIN LOGIC  **********/
DISPLAY Field1 Field2 WITH FRAME Frame1.
ENABLE ALL WITH FRAME Frame1.
WAIT-FOR CHOOSE OF btn-Exit.


