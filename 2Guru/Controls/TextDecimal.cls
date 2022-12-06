 
 /*------------------------------------------------------------------------
    File        : TextDecimal
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : elpkl
    Created     : Mon Nov 28 13:18:24 CET 2011
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Infragistics.Win.UltraWinEditors.UltraTextEditor.
USING System.Windows.Forms.*.

CLASS Controls.TextDecimal INHERITS UltraTextEditor:
   /*------------------------------------------------------------------------------
         Purpose:  																	  
         Notes:  																	  
   ------------------------------------------------------------------------------*/   
   DEFINE PRIVATE VARIABLE const AS Controls.contextDummy NO-UNDO.
   DEFINE PROPERTY blockedKey AS LOGICAL NO-UNDO INITIAL FALSE
   GET.  SET.
   DEFINE PROPERTY BlockaKeyTab AS LOGICAL NO-UNDO INITIAL FALSE
   GET.  SET.
   
   DEFINE PUBLIC VARIABLE KnappKontroll AS CHARACTER NO-UNDO.
   	
	CONSTRUCTOR PUBLIC TextDecimal (  ):
		SUPER ().
      const = NEW Controls.contextDummy().
      THIS-OBJECT:ContextMenu = const.
		
	END CONSTRUCTOR.
	
	/*
   METHOD PROTECTED OVERRIDE VOID OnKeyPress(e AS KeyPressEventArgs):
      /*IF blockedKey THEN  e:Handled = TRUE.*/
   END METHOD.
   */
  DEFINE PUBLIC EVENT Pil SIGNATURE VOID ().
   
   METHOD PROTECTED OVERRIDE VOID OnKeyDown(e AS KeyEventArgs):
      DEFINE VARIABLE keycodestr AS CHARACTER NO-UNDO.   
      DEFINE VARIABLE dublett AS LOGICAL NO-UNDO.   
      DEFINE VARIABLE tangent AS System.Windows.Forms.KeyEventArgs.
      blockedKey = FALSE.
      dublett = FALSE.

      keycodestr = e:KEYCODE:ToString().
      KnappKontroll = e:KEYCODE:ToString().

      /*IF keycodestr = "TAB" THEN TabKontroll = TRUE.*/
      if e:Shift = TRUE OR e:Alt = TRUE THEN DO:
         blockedKey = TRUE.
         e:SuppressKeyPress = TRUE.
         /*e:Handled = TRUE.*/
         RETURN.
      END.
      IF keycodestr = "TAB" AND BlockaKeyTab = TRUE THEN DO:
         e:SuppressKeyPress = TRUE.
         /*e:Handled = TRUE.*/
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
            e:SuppressKeyPress = TRUE.
            /*e:Handled = TRUE.*/
            RETURN.
         END.
      END.
      
      IF SESSION:NUMERIC-FORMAT = "AMERICAN" AND tangent:KEYCODE:ToString() = KEYS:OemPeriod:ToString() AND tangent:KEYCODE:ToString() = keycodestr THEN DO:
         blockedKey = FALSE.
      END.   
      ELSE IF SESSION:NUMERIC-FORMAT = "EUROPEAN" AND tangent:KEYCODE:ToString() = KEYS:Oemcomma:ToString() AND tangent:KEYCODE:ToString() = keycodestr THEN DO:
         blockedKey = FALSE.
      END.  
      
      IF keycodestr = "Down" OR keycodestr = "Up" THEN DO:
         Pil:Publish().
      END.
      
      /*Anders Olsson Elpool i Umeå AB  26 mar 2014 17:43:12 
      SKA DETTA VARA MED ????*/
      IF  blockedKey = TRUE THEN DO:
         e:SuppressKeyPress = TRUE.
         /*e:Handled = TRUE.*/
         RETURN.
      END. 
       
   END METHOD.
   
   METHOD PUBLIC VOID BlockaKeyTab(invar AS LOGICAL):
      BlockaKeyTab = invar. 
      
   END METHOD.
   
   METHOD PROTECTED LOGICAL teckenFinns():
      DEFINE VARIABLE slutvar  AS INTEGER   NO-UNDO.
      DEFINE VARIABLE rkollvar AS INTEGER   NO-UNDO.
      DEFINE VARIABLE feltxt AS CHARACTER NO-UNDO.
      feltxt = ",".
      IF INDEX(THIS-OBJECT:Text,feltxt) NE 0 THEN   RETURN TRUE. 
      feltxt = ".".
      IF INDEX(THIS-OBJECT:Text,feltxt) NE 0 THEN   RETURN TRUE. 
      RETURN FALSE.  
   END METHOD.
   
   
   METHOD PROTECTED LOGICAL teckenFinns(tangent AS System.Windows.Forms.KeyEventArgs):
      DEFINE VARIABLE slutvar  AS INTEGER   NO-UNDO.
      DEFINE VARIABLE rkollvar AS INTEGER   NO-UNDO.
      DEFINE VARIABLE feltxt AS CHARACTER NO-UNDO.
      IF tangent:KEYCODE = System.Windows.Forms.Keys:Oemcomma THEN 
         feltxt = ",".
      ELSE
         feltxt = ".".
        
      rkollvar = 1.
      slutvar = LENGTH(feltxt).      
      REPEAT: 
         IF INDEX(THIS-OBJECT:Text,SUBSTRING(feltxt,rkollvar,1)) NE 0 THEN 
         DO:            
            RETURN TRUE. 
         END.
         rkollvar = rkollvar + 1.
         IF rkollvar > slutvar THEN LEAVE.
      END.
      RETURN FALSE.  
   END METHOD.
   
   METHOD PUBLIC VOID Fokus(INPUT sender AS System.Object, INPUT e AS System.EventArgs):
      THIS-OBJECT:SelectAll().
   END METHOD.
   
	DESTRUCTOR PUBLIC TextDecimal ( ):

	END DESTRUCTOR.

END CLASS.