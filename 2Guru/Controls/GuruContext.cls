 
 /*------------------------------------------------------------------------
    File        : GuruContext
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : elpkl
    Created     : Mon Dec 05 12:37:32 CET 2011
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.


 
/*------------------------------------------------------------------------
   File        : contextDummy
   Purpose     : 
   Syntax      : 
   Description : ?ppna kalender ist?llet f?r context menu vid h?gerklick i en Controls.TextDate

   Author(s)   : elpkl
   Created     : Tue Nov 29 15:25:52 CET 2011
   Notes       : 
 ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING System.Windows.Forms.ContextMenu.



CLASS Controls.GuruContext INHERITS ContextMenu: 
   /*------------------------------------------------------------------------------
         Purpose:                                                      
         Notes:                                                     
   ------------------------------------------------------------------------------*/
   
   
   
      
   DEFINE PRIVATE PROPERTY datum AS LOGICAL INITIAL FALSE NO-UNDO  
      PRIVATE GET. 
      PRIVATE SET.
   CONSTRUCTOR PUBLIC GuruContext (  ):     
      SUPER ().      
      THIS-OBJECT:MenuItems:Add("ccc").      
   END CONSTRUCTOR.
   
   /*
 METHOD PROTECTED OVERRIDE VOID OnPopup(e AS System.Eventargs):      
   
 END METHOD.
 */
   
   DESTRUCTOR PUBLIC GuruContext ( ):
      
   END DESTRUCTOR.

END CLASS.