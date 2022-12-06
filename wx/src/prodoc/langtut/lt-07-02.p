/**********  DEFINE WIDGETS  **********/ 
DEFINE VARIABLE Where-focus AS CHARACTER 
    LABEL "Input focus is currently on" VIEW-AS TEXT.
DEFINE VARIABLE Which-chosen AS CHARACTER 
    LABEL "Last button chosen" VIEW-AS TEXT.
DEFINE BUTTON Button1.
DEFINE BUTTON Button2.
DEFINE BUTTON Button3.
DEFINE BUTTON Exit.
   
/**********  DEFINE FRAMES  **********/ 
DEFINE FRAME Frame1
    SKIP(1)
    Where-focus SKIP
    Which-chosen SKIP
        WITH SIDE-LABELS NO-BOX CENTERED THREE-D.      
DEFINE FRAME Frame2
    SKIP(1)
    Button1 Button2 Button3 Exit SKIP
        WITH NO-BOX CENTERED THREE-D.
                
/**********  DEFINE TRIGGERS  **********/
ON ENTRY OF Button1 IN FRAME Frame2, Button2 IN FRAME Frame2,
            Button3 IN FRAME Frame2, Exit IN FRAME Frame2
DO:
    APPLY "ENTRY" TO SELF.
    ASSIGN Where-focus = FOCUS:LABEL.
    DISPLAY Where-focus WITH FRAME Frame1.
END.
ON CHOOSE OF Button1 IN FRAME Frame2, Button2 IN FRAME Frame2,
             Button3 IN FRAME Frame2
DO:
    ASSIGN Which-chosen = SELF:LABEL.
    DISPLAY Which-chosen WITH FRAME Frame1.
END.

/**********  MAIN LOGIC  **********/
VIEW FRAME Frame1.
ENABLE ALL WITH FRAME Frame2.
WAIT-FOR CHOOSE OF Exit IN FRAME Frame2.




