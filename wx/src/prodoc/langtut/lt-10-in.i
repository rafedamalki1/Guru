/**** Common Code for Language Tutorial Chapter 10 Exercises ****/

/**********  DEFINE WIDGETS  **********/
DEFINE VARIABLE Rep-Editor AS CHARACTER VIEW-AS EDITOR LARGE 
    SCROLLBAR-VERTICAL SCROLLBAR-HORIZONTAL SIZE 76 BY 13.
DEFINE VARIABLE Stat AS LOGICAL.
DEFINE BUTTON b-rep LABEL "Report".
DEFINE BUTTON b-exit LABEL "Exit".
DEFINE BUTTON b-ok LABEL "OK" AUTO-GO.

/**********  DEFINE FRAMES  **********/
DEFINE FRAME Frame1
    SKIP(1)
    b-rep SKIP(1)
    b-exit  
        WITH NO-BOX CENTERED THREE-D.
        
DEFINE FRAME Dialog1
    Rep-Editor SKIP(1)
    b-ok TO 40 SKIP(1)  
        WITH NO-LABELS VIEW-AS DIALOG-BOX SCROLLABLE THREE-D.      
        
ASSIGN Rep-Editor:FONT = 3.
