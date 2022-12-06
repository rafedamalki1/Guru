/*************************************************************/
/* Copyright (c) 1984-1997 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from Progress Software Corporation. */
/*************************************************************/

/*---------------------------------------------------------------------------
  pmenus.i
  Procedures for Editor Menus
----------------------------------------------------------------------------*/


PROCEDURE EditMenuDrop.
/*---------------------------------------------------------------------------
    Syntax     RUN EditMenuDrop ( INPUT p_Editor ) .
   
    Purpose    On the MENU-DROP event for the Edit Menu, set the enable/disable
               state of the Edit Menu selections. 
    
    Remarks    The p_Editor widget is presumed to be of :TYPE = "EDITOR".
    
    Return Values  NONE.
---------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p_Editor AS WIDGET-HANDLE NO-UNDO.
  
  &IF "{&WINDOW-SYSTEM}" BEGINS "MS-WIN" &THEN  
    /* Works well under MS-WINDOWS only. */
    DEFINE VAR Read_Only        AS LOGICAL NO-UNDO.
    DEFINE VAR Text_Is_Selected AS LOGICAL NO-UNDO.

    ASSIGN
        Read_Only        = p_Editor:READ-ONLY
        Text_Is_Selected = p_Editor:TEXT-SELECTED

        MENU-ITEM _Undo:SENSITIVE IN MENU mnu_Edit   = /* TRUE IF... */
                        ( p_Editor:EDIT-CAN-UNDO ) AND ( p_Editor:MODIFIED )
        
        MENU-ITEM _Cut:SENSITIVE IN MENU mnu_Edit   = /* TRUE IF... */
                        ( NOT Read_Only ) AND ( Text_Is_Selected )

        MENU-ITEM _Copy:SENSITIVE IN MENU mnu_Edit  = /* TRUE IF...*/
                        ( Text_Is_Selected  )
                            
        MENU-ITEM _Paste:SENSITIVE IN MENU mnu_Edit = /* TRUE IF... */
                        ( p_Editor:EDIT-CAN-PASTE ) AND ( NOT Read_Only )
                        
        MENU-ITEM _Insert_File:SENSITIVE IN MENU mnu_Edit = 
                        ( NOT Read_Only )
                        
        MENU-ITEM _Field_Selector:SENSITIVE IN MENU mnu_Edit = 
                        ( NOT Read_Only )
    . /* END ASSIGN. */

  &ENDIF
END PROCEDURE.


PROCEDURE BufferMenuDrop.
/*---------------------------------------------------------------------------
    Syntax     RUN BufferMenuDrop.
   
    Purpose    On the MENU-DROP event for the Buffer Menu, set the 
               enable/disable state of the Buffer Menu selections. 
    
    Remarks    
    
    Return Values  NONE.
---------------------------------------------------------------------------*/
  &IF "{&WINDOW-SYSTEM}" BEGINS "MS-WIN" &THEN  
  /* Works well under MS-WINDOWS only. */
  
  /* If more than one buffer open, enable Buffer Next and Prev. */
  RUN NumBuffers ( INPUT win_ProEdit, OUTPUT Buffers_Open ).
  ASSIGN
      MENU-ITEM _Next:SENSITIVE IN MENU mnu_Buffer = ( Buffers_Open > 1 )
      MENU-ITEM _Prev:SENSITIVE IN MENU mnu_Buffer = ( Buffers_Open > 1 )
  . /* END ASSIGN. */
  &ENDIF
      
END PROCEDURE.


PROCEDURE MenuInit .
/*---------------------------------------------------------------------------
    Syntax     RUN MenuInit . 

    Purpose    Initialize menu item accelerators and enable states.

    Remarks    

    Return Values  NONE.
---------------------------------------------------------------------------*/
  
&IF OPSYS <> "VMS" &THEN
  { adeedit/minitdbg.i }
&ENDIF

  &IF "{&WINDOW-SYSTEM}" = "TTY" &THEN
      /* TTY, so just return - no :accelerator or Options support. */
      RETURN.
  &ENDIF

  &IF "{&WINDOW-SYSTEM}" <> "TTY" &THEN
  DO:
      FIND FIRST Sys_Options.
      ASSIGN MENU-ITEM _Save_Settings_Exit:CHECKED IN MENU mnu_Options
                       = Sys_Options.Save_Settings
      . /* END ASSIGN. */
      RUN MenuAccelInit ( INPUT g_Editor_Cached_Accels ).
      
  END.  /* "MS-WINDOWS or OSF/Motif" */
  &ENDIF

END PROCEDURE.


PROCEDURE MenuAccelInit.
/*---------------------------------------------------------------------------
    Syntax     RUN MenuAccelInit( INPUT p_Accels ). 

    Purpose    Initialize/assign menu item accelerators.

    Parameters
        INPUT  p_Accels   Comma-delimited list of Menu Items and their
                          accelerators in the form:
                                
                                    "MenuLabel:Accelerator,..."
                                
                          The menu labels have all &, spaces, and ellipses
                          removed.
                          
    Remarks    Must be kept in perfect sync with adeedit/dmenus.i.
               Except for Tools Menu Accelerators, defined by the ADE
               Standards Tools Menu Include toolmenu.i.  Editor does not
               support Menu Accelerators on the Tools menu.
    Return Values  NONE.
---------------------------------------------------------------------------*/

DEFINE INPUT PARAMETER p_Accels AS CHAR NO-UNDO.

&IF "{&WINDOW-SYSTEM}" <> "TTY" &THEN

/*-----------   File Menu Accelerators --------------*/
ASSIGN
 MENU-ITEM _New:ACCELERATOR      IN MENU mnu_File = ENTRY( 2 , ENTRY( 1 , p_Accels ) , ":" )
 MENU-ITEM _Open:ACCELERATOR     IN MENU mnu_File = ENTRY( 2 , ENTRY( 2 , p_Accels ) , ":" )
 MENU-ITEM _Close:ACCELERATOR    IN MENU mnu_File = ENTRY( 2 , ENTRY( 3 , p_Accels ) , ":" )
 MENU-ITEM _New_PW:ACCELERATOR   IN MENU mnu_File = ENTRY( 2 , ENTRY( 4 , p_Accels ) , ":" )
 MENU-ITEM _Save:ACCELERATOR     IN MENU mnu_File = ENTRY( 2 , ENTRY( 5 , p_Accels ) , ":" )
 MENU-ITEM _Save_as:ACCELERATOR  IN MENU mnu_File = ENTRY( 2 , ENTRY( 6 , p_Accels ) , ":" )
 MENU-ITEM _Print:ACCELERATOR    IN MENU mnu_File = ENTRY( 2 , ENTRY( 7 , p_Accels ) , ":" )
 MENU-ITEM _Exit:ACCELERATOR     IN MENU mnu_File = ENTRY( 2 , ENTRY( 8 , p_Accels ) , ":" )
. /* END ASSIGN */

/*-----------   Edit Menu Accelerators --------------*/
ASSIGN
 MENU-ITEM _Undo:ACCELERATOR    IN MENU mnu_Edit = ENTRY( 2 , ENTRY( 9 , p_Accels ) , ":" )
 MENU-ITEM _Cut:ACCELERATOR     IN MENU mnu_Edit = ENTRY( 2 , ENTRY(10 , p_Accels ) , ":" )
 MENU-ITEM _Copy:ACCELERATOR    IN MENU mnu_Edit = ENTRY( 2 , ENTRY(11 , p_Accels ) , ":" )
 MENU-ITEM _Paste:ACCELERATOR   IN MENU mnu_Edit = ENTRY( 2 , ENTRY(12 , p_Accels ) , ":" )
 MENU-ITEM _Insert_File:ACCELERATOR     IN MENU mnu_Edit = ENTRY( 2 , ENTRY( 13 , p_Accels ) , ":" )
 MENU-ITEM _Field_Selector:ACCELERATOR  IN MENU mnu_Edit = ENTRY( 2 , ENTRY( 14, p_Accels ) , ":" )
.

/*-----------   Search Menu Accelerators --------------*/
ASSIGN 
 MENU-ITEM _Find:ACCELERATOR       IN MENU mnu_Search = ENTRY( 2 , ENTRY(15 , p_Accels ) , ":" )
 MENU-ITEM _Find_Next:ACCELERATOR  IN MENU mnu_Search = ENTRY( 2 , ENTRY(16 , p_Accels ) , ":" )
 MENU-ITEM _Find_Prev:ACCELERATOR  IN MENU mnu_Search = ENTRY( 2 , ENTRY(17 , p_Accels ) , ":" )
 MENU-ITEM _Replace:ACCELERATOR    IN MENU mnu_Search = ENTRY( 2 , ENTRY(18 , p_Accels ) , ":" )
 MENU-ITEM _Goto_Line:ACCELERATOR  IN MENU mnu_Search = ENTRY( 2 , ENTRY(19 , p_Accels ) , ":" )
.

/*-----------   Buffer Menu Accelerators --------------*/
ASSIGN
 MENU-ITEM _BuffList:ACCELERATOR IN MENU mnu_Buffer = ENTRY( 2 , ENTRY(20 , p_Accels ) , ":" )
 MENU-ITEM _Next:ACCELERATOR     IN MENU mnu_Buffer = ENTRY( 2 , ENTRY(21 , p_Accels ) , ":" )
 MENU-ITEM _Prev:ACCELERATOR     IN MENU mnu_Buffer = ENTRY( 2 , ENTRY(22 , p_Accels ) , ":" )
 MENU-ITEM _BufFont:ACCELERATOR  IN MENU mnu_Buffer = ENTRY( 2 , ENTRY(23 , p_Accels ) , ":" )
 MENU-ITEM _BufSettings:ACCELERATOR IN MENU mnu_Buffer = ENTRY( 2 , ENTRY( 24 , p_Accels ) , ":" )
.

/*-----------   Compile Menu Accelerators --------------*/
ASSIGN
 MENU-ITEM _Run:ACCELERATOR          IN MENU mnu_Compile = ENTRY( 2 , ENTRY( 25 , p_Accels ) , ":" )
 MENU-ITEM _Check_Syntax:ACCELERATOR IN MENU mnu_Compile = ENTRY( 2 , ENTRY( 26 , p_Accels ) , ":" )
&IF OPSYS <> "VMS" &THEN
 MENU-ITEM _Debug:ACCELERATOR        IN MENU mnu_Compile = ENTRY( 2 , ENTRY( 27 , p_Accels ) , ":" )
&ENDIF
 MENU-ITEM _Comp_Msgs:ACCELERATOR    IN MENU mnu_Compile = ENTRY( 2 , ENTRY( 28 , p_Accels ) , ":" )
.

/*-----------   Tools Menu Accelerators --------------*/
/* None - See Notes in header. */
  
/*-----------   Options Menu Accelerators --------------*/
ASSIGN 
 MENU-ITEM _Editor_Opts:ACCELERATOR IN MENU mnu_Options = ENTRY( 2 , ENTRY( 29 , p_Accels ) , ":" )
 MENU-ITEM _Menu_Accels:ACCELERATOR IN MENU mnu_Options = ENTRY( 2 , ENTRY( 30 , p_Accels ) , ":" )
 MENU-ITEM _DefFont:ACCELERATOR     IN MENU mnu_Options = ENTRY( 2 , ENTRY( 31 , p_Accels ) , ":" )
 MENU-ITEM _Save_Settings_Exit:ACCELERATOR IN MENU mnu_Options =
                                                          ENTRY( 2 , ENTRY( 32 , p_Accels ) , ":" )
.

/*-----------   Help Menu Accelerators ---------------*/
ASSIGN
 MENU-ITEM _Help_Topics:ACCELERATOR   IN MENU mnu_Help = ENTRY( 2 , ENTRY( 33 , p_Accels ) , ":" )
/* Keyboard option not in the GUI Help menu. This code never gets run in
TTY anyways, so I just commented it out and updated the sequences. - jep
 MENU-ITEM _Keyboard:ACCELERATOR      IN MENU mnu_Help = ENTRY( 2 , ENTRY( 36 , p_Accels ) , ":" )
*/
 MENU-ITEM _Menu_Messages:ACCELERATOR IN MENU mnu_Help = ENTRY( 2 , ENTRY( 34 , p_Accels ) , ":" )
 MENU-ITEM _Menu_Recent:ACCELERATOR   IN MENU mnu_Help = ENTRY( 2 , ENTRY( 35 , p_Accels ) , ":" )
 MENU-ITEM _About:ACCELERATOR         IN MENU mnu_Help = ENTRY( 2 , ENTRY( 36 , p_Accels ) , ":" )
.

&ENDIF

END PROCEDURE.
