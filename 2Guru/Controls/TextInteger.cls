 
/*------------------------------------------------------------------------
   File        : TextInteger
   Purpose     : 
   Syntax      : 
   Description : 
   Author(s)   : elpkl
   Created     : Mon Nov 28 10:13:49 CET 2011
   Notes       : 
 ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Infragistics.Win.UltraWinEditors.UltraTextEditor.
USING System.Windows.Forms.*.

CLASS Controls.TextInteger INHERITS UltraTextEditor: 
   /*------------------------------------------------------------------------------
         Purpose:  																	  
         Notes:  					
            												  
   ------------------------------------------------------------------------------*/
   DEFINE PRIVATE VARIABLE const AS Controls.contextDummy NO-UNDO.
   DEFINE PROPERTY blockedKey AS LOGICAL NO-UNDO INITIAL FALSE
      GET. SET.
   DEFINE PUBLIC PROPERTY FORMDATATYP AS CHARACTER  NO-UNDO
      PUBLIC GET.
      PUBLIC SET.   
   CONSTRUCTOR PUBLIC TextInteger (  ):
		SUPER ().
      const = NEW Controls.contextDummy().
      THIS-OBJECT:ContextMenu = const.
		 
	END CONSTRUCTOR.   
   
   METHOD PROTECTED OVERRIDE VOID OnKeyPress(e AS KeyPressEventArgs):              
      IF blockedKey THEN            
         e:Handled = TRUE.
   END METHOD.
         
   METHOD PROTECTED OVERRIDE VOID OnKeyDown(e AS KeyEventArgs):
      DEFINE VARIABLE keycodestr AS CHARACTER NO-UNDO.                        
      blockedKey = FALSE.
      keycodestr = e:KEYCODE:ToString().
      
      if e:Shift = TRUE OR e:Alt = TRUE THEN 
      DO:
         blockedKey = TRUE.
         RETURN.
      END.
      if keycodestr < Keys:D0:ToString() OR keycodestr > Keys:D9:ToString() THEN 
      DO:        
         if keycodestr < Keys:NumPad0:ToString() OR keycodestr > Keys:NumPad9:ToString() THEN 
         DO:         
            if keycodestr NE Keys:Back:TOSTRING() THEN
               blockedKey = TRUE.               
         END.      
      END.
   END METHOD.
   
	DESTRUCTOR PUBLIC TextInteger ( ):

	END DESTRUCTOR.

END CLASS.