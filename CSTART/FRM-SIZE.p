/*---------------------------------------------------------
  File: frm-size.p
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
DEFINE INPUT PARAMETER pd_height-p AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER pd_width-p  AS INTEGER NO-UNDO.
DEFINE OUTPUT PARAMETER x-mult AS DECIMAL NO-UNDO.
DEFINE OUTPUT PARAMETER y-mult AS DECIMAL NO-UNDO.

DEFINE VAR x-border AS INTEGER NO-UNDO.
DEFINE VAR y-border AS INTEGER NO-UNDO.
DEFINE VAR h AS HANDLE  NO-UNDO.

DEFINE VARIABLE hidvar AS LOGICAL NO-UNDO.
DEFINE VARIABLE fh AS HANDLE NO-UNDO.
{FINNSBEN.I}


hidvar = ph_frame:HIDDEN.

ASSIGN 
ph_frame:HIDDEN     = YES
ph_frame:SCROLLABLE = YES. 
IF pd_height-p > ph_frame:VIRTUAL-HEIGHT-PIXELS THEN ph_frame:VIRTUAL-HEIGHT-PIXELS = pd_height-p.
IF pd_width-p > ph_frame:VIRTUAL-WIDTH-PIXELS THEN  ph_frame:VIRTUAL-WIDTH-PIXELS = pd_width-p.
ASSIGN 
x-border = ph_frame:BORDER-LEFT-PIXELS + ph_frame:BORDER-RIGHT-PIXELS
y-border = ph_frame:BORDER-TOP-PIXELS + ph_frame:BORDER-BOTTOM-PIXELS
x-mult = (pd_width-p - x-border) / (ph_frame:WIDTH-PIXELS - x-border) 
y-mult = (pd_height-p - y-border) / (ph_frame:HEIGHT-PIXELS - y-border).
IF Guru.SharedVariable:globmaxforand = 0 OR Guru.SharedVariable:globmaxforand = 1 THEN Guru.SharedVariable:globmaxforand = y-mult.
ASSIGN 
ph_frame:WIDTH-PIXELS  = pd_width-p
ph_frame:HEIGHT-PIXELS = pd_height-p.
ASSIGN 
h = ph_frame:CURRENT-ITERATION /* The field-group */
h = h:FIRST-CHILD.             /* First true child */
DO WHILE VALID-HANDLE(h):
   fh = h:FRAME.
   
   IF h:TYPE EQ "FRAME":U THEN DO:     
      RUN VALUE(THIS-PROCEDURE:FILE-NAME)
               (INPUT h,INPUT h:HEIGHT-PIXELS * y-mult,INPUT h:WIDTH-PIXELS * x-mult, OUTPUT x-mult, OUTPUT y-mult).   
   END.
   ELSE DO:
      IF h:TYPE = "BUTTON" THEN DO:
         IF h:PRIVATE-DATA = "EJHB" THEN.
         ELSE IF h:HEIGHT = 1.92 AND h:WIDTH = 7.5 THEN DO:
            x-mult = x-mult. 
         END.
         ELSE IF h:HEIGHT = 1.92 AND h:WIDTH = 7.25 THEN DO:
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
         IF h:PRIVATE-DATA = "EJH" THEN DO: 
            h:WIDTH-PIXELS = h:WIDTH-PIXELS * x-mult NO-ERROR.
         END.
         ELSE IF h:PRIVATE-DATA = "EJB" THEN DO: 
            h:HEIGHT-PIXELS = h:HEIGHT-PIXELS * y-mult NO-ERROR.
         END.
         ELSE IF h:PRIVATE-DATA = "EJHB" THEN DO: 
         END.
         ELSE DO:   
            ASSIGN
            h:WIDTH-PIXELS = h:WIDTH-PIXELS * x-mult NO-ERROR.
            h:HEIGHT-PIXELS = h:HEIGHT-PIXELS * y-mult NO-ERROR.
         END.    
         IF finnsben = TRUE THEN RUN colwidth_UI. 
      END.    
      ELSE DO: 
         RUN flyttaXY_UI.
         IF h:PRIVATE-DATA = "EJH" THEN DO: 
            h:WIDTH-PIXELS = h:WIDTH-PIXELS * x-mult NO-ERROR.
         END.
         ELSE IF h:PRIVATE-DATA = "EJB" THEN DO: 
            h:HEIGHT-PIXELS = h:HEIGHT-PIXELS * y-mult NO-ERROR.
         END.
         ELSE IF h:PRIVATE-DATA = "EJHB" THEN DO: 
         END.
         ELSE DO:   
            ASSIGN
            h:WIDTH-PIXELS = h:WIDTH-PIXELS * x-mult NO-ERROR.
            h:HEIGHT-PIXELS = h:HEIGHT-PIXELS * y-mult NO-ERROR.
         END.    
      END.
      Guru.Konstanter:LabelFlytt(h).             
   END.
   ASSIGN 
   pd_width-p = MAX (pd_width-p,h:X + h:WIDTH-PIXELS + x-border)
   pd_height-p = MAX (pd_height-p,h:Y + h:HEIGHT-PIXELS + y-border).
   h = h:NEXT-SIBLING.
END.

ASSIGN
ph_frame:SCROLLABLE    = NO 
ph_frame:WIDTH-PIXELS  = pd_width-p
ph_frame:HEIGHT-PIXELS = pd_height-p
ph_frame:HIDDEN        = hidvar.


PROCEDURE flyttaX_UI :
    h:X = h:X * x-mult.     
END PROCEDURE.
PROCEDURE flyttabaraY_UI :
    h:Y = h:Y * y-mult.     
END PROCEDURE.
PROCEDURE flyttaXY_UI :
    h:X = h:X * x-mult.     
    h:Y = h:Y * y-mult.     
END PROCEDURE.