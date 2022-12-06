/* p-ctrt0.p */

DEFINE VARIABLE xvar AS INTEGER VIEW-AS FILL-IN.
DEFINE VARIABLE yvar AS INTEGER VIEW-AS SLIDER MAX-VALUE 16 MIN-VALUE 1.
DEFINE VARIABLE zvar AS INTEGER VIEW-AS FILL-IN.
DEFINE VARIABLE fact-compute AS LOGICAL.
DEFINE BUTTON bOK LABEL "OK".
DEFINE BUTTON BCancel LABEL "Cancel" AUTO-GO.

FORM SKIP (.05)
    "Enter upper factorial to compute:" yvar FORMAT ">>" SKIP(.05)
    "Factorial of" xvar FORMAT ">>" "=" zvar SKIP(.05)
    bOK bCancel 
WITH FRAME zFrame NO-LABELS.

ON CHOOSE OF bOK DO:
    ASSIGN  yvar
            zvar = 1.
    DO xvar = 1 TO yvar WITH FRAME zFrame:
        zvar = xvar * zvar.
        DISPLAY xvar zvar FORMAT ">,>>>,>>>,>>>" WITH FRAME zFrame.
        MESSAGE "Compute next factorial" VIEW-AS ALERT-BOX
            QUESTION BUTTONS YES-NO UPDATE fact-compute.
        IF NOT fact-compute THEN LEAVE.                        
    END.
END.
ENABLE yvar bOK bCancel WITH FRAME zFrame.
WAIT-FOR GO OF FRAME zFrame.
&MESSAGE This procedure has {&LINE-NUMBER} source lines.

