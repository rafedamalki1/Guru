 
 /*------------------------------------------------------------------------
    File        : contextDummy
    Purpose     : 
    Syntax      : 
    Description : ?ppna kalender ist?llet f?r context menu vid h?gerklick i en TextDate

    Author(s)   : elpkl
    Created     : Tue Nov 29 15:25:52 CET 2011
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING System.Windows.Forms.ContextMenu.



CLASS contextDummy INHERITS ContextMenu: 
	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
   
   
   
      
   DEFINE PRIVATE PROPERTY datum AS LOGICAL INITIAL FALSE NO-UNDO  
      PRIVATE GET. 
      PRIVATE SET.
	CONSTRUCTOR PUBLIC contextDummy (  ):		
		SUPER ().            
	END CONSTRUCTOR.
   
     /*
   METHOD PROTECTED OVERRIDE VOID OnPopup(e AS System.Eventargs):      
   
   END METHOD.
   */
   
	DESTRUCTOR PUBLIC contextDummy ( ):
      
	END DESTRUCTOR.

END CLASS.