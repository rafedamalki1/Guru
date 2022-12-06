 
 /*------------------------------------------------------------------------
    File        : TextBase
    Purpose     : 
    Syntax      : 
    Description : basklass för textboxar
    Author(s)   : elpkl
    Created     : Thu Dec 01 15:58:03 CET 2011
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Infragistics.Win.UltraWinEditors.UltraTextEditor.
USING System.Windows.Forms.*.

CLASS Controls.TextBase INHERITS UltraTextEditor ABSTRACT:
   /*------------------------------------------------------------------------------
         Purpose: håller gemensamma saker för textboxar (ej TextDate då det är en annan klass)																	  
         Notes: 																	  
   ------------------------------------------------------------------------------*/
   DEFINE PRIVATE VARIABLE const AS Controls.contextDummy NO-UNDO.
   DEFINE PROPERTY blockedKey AS LOGICAL NO-UNDO INITIAL FALSE
   GET.
   SET.
     		
	CONSTRUCTOR PUBLIC TextBase (  ):
		SUPER ().
		ASSIGN
      const = NEW Controls.contextDummy()
      THIS-OBJECT:ContextMenu = const.
	END CONSTRUCTOR.   
   
   METHOD PROTECTED OVERRIDE VOID OnKeyPress(e AS KeyPressEventArgs):              
      IF blockedKey THEN            
         e:Handled = TRUE.
   END METHOD.
   
	DESTRUCTOR PUBLIC TextBase ( ):

	END DESTRUCTOR.

END CLASS.