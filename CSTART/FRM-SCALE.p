/*---------------------------------------------------------
  File: FRM-SCALE.P
  Purpose: Changes the size and shape of the panel.  This
  routine spaces the buttons to fill the available space.
  
  Input Parameters: 
    ph_frame    - the handle of the frame
    pd_height-p - the desired height (in pixels)
    pd_width-p  - the desired width  (in pixels)

  Notes:
    This procedure assumes that the target frame:
       - is not hidden
       - is not scrollable
       - has a single iteration without column labels
---------------------------------------------------------*/
DEFINE INPUT PARAMETER ph_frame    AS HANDLE  NO-UNDO.
DEFINE VARIABLE pd_height-p AS INTEGER NO-UNDO.
DEFINE VARIABLE pd_width-p  AS INTEGER NO-UNDO.
DEFINE VARIABLE x-border AS INTEGER NO-UNDO.
DEFINE VARIABLE y-border AS INTEGER NO-UNDO.
DEFINE VARIABLE x-mult AS DECIMAL NO-UNDO.
DEFINE VARIABLE y-mult AS DECIMAL NO-UNDO.
DEFINE VARIABLE h AS HANDLE  NO-UNDO.
DEFINE VARIABLE hidvar AS LOGICAL NO-UNDO.
DEFINE VARIABLE fh AS HANDLE NO-UNDO.

{FINNSBEN.I}
IF Guru.Konstanter:globstorh < 740 THEN RETURN.
IF Guru.Konstanter:globstorb < 1024 THEN RETURN.
IF ph_frame:NAME = "FRAME-ALMAN" THEN RETURN.
hidvar = ph_frame:HIDDEN.
ASSIGN 
ph_frame:HIDDEN     = NO 
ph_frame:SCROLLABLE = YES.
/*
ASSIGN
pd_height-p = SESSION:WORK-AREA-HEIGHT-PIXELS
pd_width-p = SESSION:WORK-AREA-WIDTH-PIXELS.
*/                
ASSIGN
pd_height-p = Guru.Konstanter:globstorh
pd_width-p = Guru.Konstanter:globstorb.    

/*
ASSIGN 
x-border = pd_width-p - 1000
y-border = pd_height-p - 682.
*/
                    /*
ASSIGN 
x-mult = (pd_width-p - x-border) / 1000 
y-mult = (pd_height-p - y-border) / 682.
*/
 
IF CURRENT-WINDOW:WINDOW-STATE = 3 THEN DO:
   pd_height-p = Guru.Konstanter:globDefaultstorh .
   pd_width-p =  Guru.Konstanter:globDefaultstorb. 
END.
ASSIGN
x-mult = (pd_width-p) / 1000 
y-mult = (pd_height-p) / 682
x-border = ph_frame:BORDER-LEFT-PIXELS + ph_frame:BORDER-RIGHT-PIXELS
y-border = ph_frame:BORDER-TOP-PIXELS + ph_frame:BORDER-BOTTOM-PIXELS.

IF Guru.GlobalaVariabler:fonsteHojd > 0 THEN DO:
   y-mult = Guru.GlobalaVariabler:fonsteHojd / (ph_frame:HEIGHT-PIXELS + 50).
   Guru.GlobalaVariabler:fonsteHojd = 0.
END.   
IF Guru.GlobalaVariabler:fonsteBredd > 0 THEN DO:
   x-mult = Guru.GlobalaVariabler:fonsteBredd / (ph_frame:WIDTH-PIXELS + 50).
   Guru.GlobalaVariabler:fonsteBredd = 0.
END.   
IF ph_frame:WIDTH-PIXELS  * x-mult > (SESSION:WORK-AREA-WIDTH-PIXELS  - 10) THEN DO:
   x-mult = (SESSION:WORK-AREA-WIDTH-PIXELS  - 10) / 1000.
   ph_frame:WIDTH-PIXELS = SESSION:WORK-AREA-WIDTH-PIXELS  - 10.
END.
ELSE ph_frame:WIDTH-PIXELS = ph_frame:WIDTH-PIXELS * x-mult. 
IF ph_frame:HEIGHT-PIXELS * y-mult > (SESSION:WORK-AREA-HEIGHT-PIXELS - 40) THEN DO:
   y-mult = (SESSION:WORK-AREA-HEIGHT-PIXELS - 40) / 682.
   ph_frame:HEIGHT-PIXELS = (SESSION:WORK-AREA-HEIGHT-PIXELS - 40).
END.
ELSE ph_frame:HEIGHT-PIXELS = ph_frame:HEIGHT-PIXELS * y-mult. 



/*
ASSIGN 
ph_frame:WIDTH-PIXELS  = ph_frame:WIDTH-PIXELS  * x-mult 
ph_frame:HEIGHT-PIXELS = ph_frame:HEIGHT-PIXELS * y-mult. 

IF ph_frame:WIDTH-PIXELS > (SESSION:WORK-AREA-WIDTH-PIXELS  - 10) THEN DO:
   x-mult = (SESSION:WORK-AREA-WIDTH-PIXELS  - 10) / 1000.
   ph_frame:WIDTH-PIXELS = SESSION:WORK-AREA-WIDTH-PIXELS  - 10.
END.
IF ph_frame:HEIGHT-PIXELS > (SESSION:WORK-AREA-HEIGHT-PIXELS - 40) THEN DO:
   y-mult = (SESSION:WORK-AREA-HEIGHT-PIXELS - 40) / 682.
   ph_frame:HEIGHT-PIXELS = (SESSION:WORK-AREA-HEIGHT-PIXELS - 40).
END.
*/

ASSIGN 
h = ph_frame:CURRENT-ITERATION /* The field-group */
h = h:FIRST-CHILD.             /* First true child */
DO WHILE VALID-HANDLE(h):
   fh = h:FRAME.  
   IF h:TYPE eq "FRAME":U THEN DO:     
      RUN VALUE(THIS-PROCEDURE:FILE-NAME)
               (INPUT h).
      /*
         ,INPUT h:HEIGHT-PIXELS * y-mult,INPUT h:WIDTH-PIXELS * x-mult).   
         */
   END.
   ELSE DO:
      IF h:TYPE = "BUTTON" THEN DO:
         IF h:HEIGHT = 1.92 AND h:WIDTH = 7.5 THEN DO:
            x-mult = x-mult. 
         END.
         ELSE IF h:HEIGHT = 2.33 AND h:WIDTH = 7.25 THEN DO:
            x-mult = x-mult. 
         END.
         ELSE RUN flyttaXY_UI.        
      END.
      ELSE IF h:TYPE = "TEXT" THEN DO:        
         RUN flyttaXY_UI.
      END.
      ELSE IF h:TYPE = "LITERAL" THEN DO:
         RUN flyttaXY_UI.
      END.  
      ELSE IF h:TYPE = "FILL-IN" THEN DO:
         RUN flyttaXY_UI.        
      END.
      ELSE IF h:TYPE = "RECTANGLE" THEN DO:       
         IF h:HEIGHT = 0.08 THEN DO:  
            h:WIDTH-PIXELS = h:WIDTH-PIXELS * x-mult.
         END.
         ELSE DO:
            RUN flyttaXY_UI.
            h:WIDTH-PIXELS = h:WIDTH-PIXELS * x-mult NO-ERROR.
            h:HEIGHT-PIXELS = h:HEIGHT-PIXELS * y-mult NO-ERROR.         
         END.
      END.
      ELSE IF h:TYPE = "BROWSE" THEN DO: 
         RUN flyttaXY_UI.
         RUN finnsben_UI.
         ASSIGN
         h:WIDTH-PIXELS = h:WIDTH-PIXELS * x-mult NO-ERROR.
         h:HEIGHT-PIXELS = h:HEIGHT-PIXELS * y-mult NO-ERROR.
         IF finnsben = TRUE THEN RUN colwidth_UI.
      END.    
      ELSE DO: 
         RUN flyttaXY_UI.
         ASSIGN
         h:WIDTH-PIXELS = h:WIDTH-PIXELS * x-mult NO-ERROR.
         h:HEIGHT-PIXELS = h:HEIGHT-PIXELS * y-mult NO-ERROR.
      END.
      Guru.Konstanter:LabelFlytt(h).             
   END.
   ASSIGN 
   pd_width-p = MAX (pd_width-p,h:X + h:WIDTH-PIXELS + x-border)
   pd_height-p = MAX (pd_height-p,h:Y + h:HEIGHT-PIXELS + y-border).
   h = h:NEXT-SIBLING.
END.
ASSIGN 
ph_frame:SCROLLABLE    = no
   /*
ph_frame:WIDTH-PIXELS  = ph_frame:WIDTH-PIXELS  * x-mult 
ph_frame:HEIGHT-PIXELS = ph_frame:HEIGHT-PIXELS * y-mult 
*/
ph_frame:HIDDEN        = hidvar.

PROCEDURE flyttaX_UI :
    h:X = h:X * x-mult.     
    IF h:X + h:WIDTH-PIXELS >=  ph_frame:WIDTH-PIXELS - - x-border THEN  h:X =  ph_frame:WIDTH-PIXELS - (h:WIDTH-PIXELS + x-border + 3).
END PROCEDURE.
PROCEDURE flyttabaraY_UI :
    h:Y = h:Y * y-mult.     
    IF h:y + h:HEIGHT-PIXELS >=  ph_frame:HEIGHT-PIXELS - y-border THEN  h:Y =  ph_frame:HEIGHT-PIXELS - (h:HEIGHT-PIXELS + y-border + 3).
END PROCEDURE.
PROCEDURE flyttaXY_UI :
    h:X = h:X * x-mult.  
    IF h:X + h:WIDTH-PIXELS >=  ph_frame:WIDTH-PIXELS - - x-border THEN  h:X =  ph_frame:WIDTH-PIXELS - (h:WIDTH-PIXELS + x-border + 3). 
    h:Y = h:Y * y-mult.  
    IF h:y + h:HEIGHT-PIXELS >=  ph_frame:HEIGHT-PIXELS - y-border THEN  h:Y =  ph_frame:HEIGHT-PIXELS - (h:HEIGHT-PIXELS + y-border + 3).   
END PROCEDURE.
