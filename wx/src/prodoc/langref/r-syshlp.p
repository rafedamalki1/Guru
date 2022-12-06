/* r-syshlp.p */

DEFINE VAR helpfile as CHAR.

DEFINE BUTTON b_help LABEL "CONTENTS Call".
DEFINE BUTTON b_h3 LABEL "CONTEXT Call".
DEFINE BUTTON b_h2 LABEL "PARTIAL-KEY Call".
DEFINE BUTTON b_h1 LABEL "HELP Call".
DEFINE BUTTON b_quit LABEL "Quit".

FORM 

    skip(1) space(1) b_help space(1)
    skip(1) space(1) b_h3 space(1)
    skip(1) space(1) b_h2 space(1)
    skip(1) space(1) b_h1 space(1)
    skip(1) space(1) b_quit space(1)
    skip(1) WITH FRAME x.

ENABLE ALL WITH FRAME x.

helpfile = "editeng.hlp". 

ON CHOOSE OF b_help IN FRAME x
DO:
    SYSTEM-HELP helpfile CONTENTS.
END.

ON CHOOSE OF b_h3 IN FRAME x
DO:
    SYSTEM-HELP helpfile CONTEXT 49154.
END.

ON CHOOSE OF b_h2 IN FRAME x
DO:
    SYSTEM-HELP helpfile PARTIAL-KEY "".
END.

ON CHOOSE OF b_h1 IN FRAME x
DO:
    SYSTEM-HELP helpfile HELP.
END.

ON CHOOSE OF b_quit IN FRAME x
DO: 
    SYSTEM-HELP helpfile QUIT.
    RETURN.
END.    

WAIT-FOR GO OF FRAME x.





