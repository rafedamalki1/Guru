 
 /*------------------------------------------------------------------------
    File        : TextLogical
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : elpkl
    Created     : Mon Nov 28 16:32:08 CET 2011
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Infragistics.Win.UltraWinEditors.UltraTextEditor.
USING System.Windows.Forms.*.


CLASS Controls.TextLogical INHERITS UltraTextEditor: 
   /*------------------------------------------------------------------------------
         Purpose:  																	  
         Notes:  																	  
   ------------------------------------------------------------------------------*/
   DEFINE PRIVATE VARIABLE const AS Controls.contextDummy NO-UNDO.
   DEFINE PROPERTY blockedKey AS LOGICAL NO-UNDO INITIAL FALSE
      GET.
      SET.
   DEFINE PUBLIC PROPERTY trueVar AS CHARACTER INITIAL "Ja" NO-UNDO  
      GET. 
      SET.
   DEFINE PUBLIC PROPERTY falseVar AS CHARACTER INITIAL "Nej" NO-UNDO  
      GET. 
      SET.
   	
	CONSTRUCTOR PUBLIC TextLogical (  ):
		SUPER ().
      const = NEW Controls.contextDummy().
      THIS-OBJECT:ContextMenu = const.
		THIS-OBJECT:TEXT = trueVar.
      THIS-OBJECT:MouseDown:Subscribe(THIS-OBJECT:OnMouseEnter).
/*      THIS-OBJECT:MouseClick:Subscribe(THIS-OBJECT:OnMouseClick).*/
      THIS-OBJECT:BackColor = System.Drawing.Color:White. 
      THIS-OBJECT:ReadOnly = TRUE.
/*      THIS-OBJECT:ValueChanged:Subscribe(THIS-OBJECT:Valuech).*/
	END CONSTRUCTOR.
   
   METHOD PRIVATE VOID OnMouseClick( INPUT sender AS System.Object, INPUT e AS System.Windows.Forms.MouseEventArgs ):
      fixaText().
      RETURN.

   END METHOD.
               
   METHOD PROTECTED  VOID OnMouseEnter( INPUT sender AS System.Object, INPUT e AS System.Windows.Forms.MouseEventArgs ):      
      fixaText().
      THIS-OBJECT:ENABLED = FALSE.
      THIS-OBJECT:ENABLED = TRUE.      
      RETURN.
   END METHOD.
   
   METHOD PROTECTED OVERRIDE VOID OnKeyPress(e AS KeyPressEventArgs):              
      IF blockedKey THEN            
         e:Handled = TRUE.         
   END METHOD.
   
   METHOD PROTECTED OVERRIDE VOID OnKeyDown(e AS KeyEventArgs):
      DEFINE VARIABLE keycodestr AS CHARACTER NO-UNDO.                        
      blockedKey = FALSE.
      keycodestr = e:KEYCODE:ToString().
      
      if e:Shift = true OR e:Alt = TRUE THEN 
      DO:
         blockedKey = TRUE.
         RETURN.
      END.
      if keycodestr = Keys:J:ToString() OR keycodestr = Keys:Y:ToString() OR keycodestr = Keys:S:ToString() THEN 
      DO:        
         THIS-OBJECT:TEXT = trueVar.
      END.
      ELSE IF keycodestr = Keys:N:ToString() OR keycodestr = Keys:F:ToString() THEN DO:
         THIS-OBJECT:TEXT = falseVar.
      END.
      blockedKey = TRUE.
   END METHOD.
   
   METHOD PRIVATE VOID fixaText():
      IF THIS-OBJECT:TEXT = trueVar THEN
         THIS-OBJECT:TEXT = falseVar.
      ELSE IF THIS-OBJECT:TEXT = falseVar THEN
            THIS-OBJECT:TEXT = trueVar.
         ELSE 
            THIS-OBJECT:TEXT = trueVar.
   END METHOD.
	DESTRUCTOR PUBLIC TextLogical ( ):

	END DESTRUCTOR.

END CLASS.