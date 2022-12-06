/* p-wow1.p */

DEFINE VARIABLE whandle1 AS HANDLE.
DEFINE VARIABLE whandle2 AS HANDLE.
DEFINE VARIABLE whandle3 AS HANDLE.
DEFINE BUTTON bviewp LABEL "VIEW".
DEFINE BUTTON bviewc LABEL "VIEW".
DEFINE BUTTON bviewgc LABEL "VIEW".
DEFINE BUTTON bhidep LABEL "HIDE".
DEFINE BUTTON bhidec LABEL "HIDE".
DEFINE BUTTON bhidegc LABEL "HIDE".
DEFINE BUTTON bhiddp LABEL "HIDDEN".
DEFINE BUTTON bhiddc LABEL "HIDDEN".
DEFINE BUTTON bhiddgc LABEL "HIDDEN".
DEFINE FRAME alpha SKIP
    "Parent" AT 11 "Child" AT 37 "Grand Child" AT 64 SKIP
    bviewp AT 11 bviewc AT 37 bviewgc AT 64 SKIP(.5)
    bhidep AT 11 bhidec AT 37 bhidegc AT 64 SKIP(.5)
    bhiddp AT 11 bhiddc AT 37 bhiddgc AT 64
WITH SIZE 80 BY 6.

CREATE WINDOW whandle1
    ASSIGN  TITLE = "Parent Window"
            HEIGHT-CHARS = 5
            WIDTH-CHARS = 27
            PARENT = CURRENT-WINDOW.
CREATE WINDOW whandle2
    ASSIGN  TITLE = "Child Window"
            HEIGHT-CHARS = 5
            WIDTH-CHARS = 27
            PARENT = whandle1.
CREATE WINDOW whandle3
    ASSIGN  TITLE = "Grand Child Window"
            HEIGHT-CHARS = 5
            WIDTH-CHARS = 27
            PARENT = whandle2.
                            
ON CHOOSE OF bviewp DO: /* View Parent */
    VIEW whandle1.
    RUN win-status IN THIS-PROCEDURE.
END.
ON CHOOSE OF bhidep DO: /* Hide Parent */
    HIDE whandle1.
    RUN win-status IN THIS-PROCEDURE.
END.
ON CHOOSE OF bhiddp DO: /* Hidden Parent */
    whandle1:HIDDEN = TRUE.
    RUN win-status IN THIS-PROCEDURE.
END.
ON CHOOSE OF bviewc DO: /* View Child */
    VIEW whandle2.
    RUN win-status IN THIS-PROCEDURE.
END.
ON CHOOSE OF bhidec DO: /* Hide Child */
    HIDE whandle2.
    RUN win-status IN THIS-PROCEDURE.
END.
ON CHOOSE OF bhiddc DO: /* Hidden Child */
    whandle2:HIDDEN = TRUE.
    RUN win-status IN THIS-PROCEDURE.
END.
ON CHOOSE OF bviewgc DO: /* View Grand Child */
    VIEW whandle3.
    RUN win-status IN THIS-PROCEDURE.
END.
ON CHOOSE OF bhidegc DO: /* Hide Grand Child */
    HIDE whandle3.
    RUN win-status IN THIS-PROCEDURE.
END.
ON CHOOSE OF bhiddgc DO: /* Hidden Grand Child */
    whandle3:HIDDEN = TRUE.
    RUN win-status IN THIS-PROCEDURE.
END.

ON WINDOW-MINIMIZED OF whandle1, whandle2, whandle3
    RUN win-status IN THIS-PROCEDURE.    

ON WINDOW-RESTORED OF whandle1, whandle2, whandle3
    RUN win-status IN THIS-PROCEDURE.    

ON WINDOW-MAXIMIZED OF whandle1, whandle2, whandle3
    RUN win-status IN THIS-PROCEDURE.    

CURRENT-WINDOW:TITLE = "Control Panel".
CURRENT-WINDOW:HEIGHT-CHARS = 6.
ENABLE ALL IN WINDOW CURRENT-WINDOW WITH FRAME alpha.
WAIT-FOR WINDOW-CLOSE OF CURRENT-WINDOW.
                     
PROCEDURE win-status:
    MESSAGE "Parent HIDDEN:" whandle1:HIDDEN 
            "/ Parent VISIBLE:" whandle1:VISIBLE
            "/ Child HIDDEN:" whandle2:HIDDEN 
            "/ Child VISIBLE:" whandle2:VISIBLE.
    MESSAGE "Grand Child HIDDEN:" whandle3:HIDDEN
            "/ Grand Child VISIBLE:" whandle3:VISIBLE.    
END.
