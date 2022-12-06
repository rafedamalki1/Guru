/**********  DEFINE WIDGETS  **********/
DEFINE VARIABLE i AS INTEGER.
DEFINE VARIABLE Alphabet AS CHARACTER FORMAT "x(30)".
DEFINE BUTTON btn-Exit LABEL "Exit".

/**********  DEFINE FRAME  **********/
DEFINE FRAME Frame1
    SKIP(1)
    Alphabet SKIP(1)
    btn-Exit
        WITH SIDE-LABELS NO-BOX CENTERED THREE-D.

/**********  MAIN LOGIC  **********/
ASSIGN i = ASC("a").
ENABLE ALL WITH FRAME Frame1.
APPLY "ENTRY" TO Alphabet.
REPEAT:
    APPLY CHR(i) TO Alphabet.
    IF CHR(i) = "z" THEN
        LEAVE.
    i = i + 1.
END.

WAIT-FOR CHOOSE OF btn-Exit.



    
    


