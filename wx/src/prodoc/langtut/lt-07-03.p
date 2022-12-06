/**********  DEFINE WIDGETS  **********/
DEFINE VARIABLE CharField AS CHARACTER INITIAL "A string". 
DEFINE VARIABLE IntField AS INTEGER INITIAL 200.
DEFINE BUTTON btn-Save LABEL "Save".
DEFINE BUTTON btn-Reset LABEL "Reset".
DEFINE BUTTON btn-Exit LABEL "Exit".

/********** DEFINE FRAMES  **********/
DEFINE FRAME Frame1
    SKIP(2) CharField COLON 10 VALIDATE( CharField BEGINS "a", 
        "Entry must begin with the letter A.") SKIP
    IntField COLON 10 VALIDATE(IntField > 100, 
        "Entry must be greater than 100.") SKIP(1)
    btn-Save btn-Reset btn-Exit
        WITH NO-BOX CENTERED SIDE-LABELS THREE-D.

/**********  DEFINE TRIGGERS  **********/
ON CHOOSE OF btn-Save
DO:
    IF CharField:MODIFIED THEN
        ASSIGN CharField.
    IF IntField:MODIFIED THEN
        ASSIGN IntField.
END.
ON CHOOSE OF btn-Reset
DO:
    DISPLAY CharField IntField WITH FRAME Frame1.
END.

/**********  MAIN LOGIC  **********/
DISPLAY CharField IntField WITH FRAME Frame1.
ENABLE ALL WITH FRAME Frame1.
WAIT-FOR CHOOSE OF btn-Exit.


