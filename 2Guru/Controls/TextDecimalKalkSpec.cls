 
 /*------------------------------------------------------------------------
    File        : TextDecimalKalkSpec
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : elprs
    Created     : Thu Mar 27 09:47:52 CET 2014
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Infragistics.Win.UltraWinEditors.UltraTextEditor.
USING System.Windows.Forms.*.


CLASS Controls.TextDecimalKalkSpec INHERITS Controls.TextDecimal:
    
   CONSTRUCTOR PUBLIC TextDecimalKalkSpec():
     SUPER ().
     
   END CONSTRUCTOR.   
   
   METHOD PROTECTED OVERRIDE VOID OnKeyPress(e AS KeyPressEventArgs):
      
      IF blockedKey THEN  e:Handled = TRUE.
   END METHOD.
   
   METHOD PROTECTED OVERRIDE VOID OnKeyDown(e AS KeyEventArgs):
      DEFINE VARIABLE keycodestr AS CHARACTER NO-UNDO.   
      DEFINE VARIABLE dublett AS LOGICAL NO-UNDO.   
      DEFINE VARIABLE tangent AS System.Windows.Forms.KeyEventArgs.
      blockedKey = FALSE.
      dublett = FALSE.
      keycodestr = e:KEYCODE:ToString().
         
      if e:Shift = TRUE OR e:Alt = TRUE THEN DO:
         blockedKey = TRUE.
         e:Handled = TRUE.
         RETURN.
      END.
      IF keycodestr = "TAB" AND  BlockaKeyTab = TRUE THEN DO:
         e:Handled = TRUE.
         RETURN.
      END.   
      IF SESSION:NUMERIC-FORMAT = "AMERICAN" THEN DO:
         tangent = NEW System.Windows.Forms.KeyEventArgs(Keys:OemPeriod).
      END.
      ELSE DO: 
         tangent = NEW System.Windows.Forms.KeyEventArgs(Keys:Oemcomma).
      END.
      
      if keycodestr < Keys:D0:ToString() OR keycodestr > Keys:D9:ToString() THEN DO:        
         if keycodestr < Keys:NumPad0:ToString() OR keycodestr > Keys:NumPad9:ToString() THEN DO:         
            IF keycodestr = Keys:DECIMAL:ToString() THEN blockedKey = FALSE.
            ELSE if keycodestr NE Keys:Back:TOSTRING() THEN DO:
               blockedKey = TRUE.
            END.                  
         END.      
      END.
      
      
      IF keycodestr = Keys:Oemcomma:ToString() OR keycodestr = Keys:OemPeriod:ToString() OR keycodestr = Keys:DECIMAL:ToString() THEN DO:         
         dublett = teckenFinns().        
         IF dublett = TRUE THEN  DO:            
            blockedKey = TRUE.
            e:Handled = TRUE.
            RETURN.
         END.
      END.
      
      IF SESSION:NUMERIC-FORMAT = "AMERICAN" AND tangent:KEYCODE:ToString() = KEYS:OemPeriod:ToString() AND tangent:KEYCODE:ToString() = keycodestr THEN DO:
         blockedKey = FALSE.
      END.   
      ELSE IF SESSION:NUMERIC-FORMAT = "EUROPEAN" AND tangent:KEYCODE:ToString() = KEYS:Oemcomma:ToString() AND tangent:KEYCODE:ToString() = keycodestr THEN DO:
         blockedKey = FALSE.
      END.  
      
      /*Anders Olsson Elpool i Umeå AB  26 mar 2014 17:43:12 
      SKA DETTA VARA MED ????
      IF  blockedKey = TRUE THEN DO:
         e:Handled = TRUE.
         RETURN.
      END. 
      */
         
   END METHOD.
   
END CLASS.