/*************************************************************/
/* Copyright (c) 1984-1995 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from Progress Software Corporation. */
/*************************************************************/

/**************************************************************************
    Procedure:  _pwsave.p
    
    Purpose:    Execute Procedure Window File->Save command.

    Syntax :    RUN adecomm/_pwsave.p.

    Parameters:
    Description:
	1.  Test if the file is "untitled".
	2.  If untitled, execute the Save As Dialog, allowing
	    user to enter a file name to save.
	3.  Write contents of editor to disk.

    Notes  :
    Authors: John Palazzo
    Date   : January, 1994
**************************************************************************/

/* Procedure Window Global Defines. */
{ adecomm/_pwglob.i }

DEFINE VARIABLE pw_Window AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE pw_Editor AS WIDGET-HANDLE NO-UNDO.

DEFINE VARIABLE File_Name  AS CHARACTER NO-UNDO.
DEFINE VARIABLE SAVE_OK    AS LOGICAL   NO-UNDO. 
DEFINE VARIABLE Dlg_Answer AS LOGICAL   NO-UNDO.

DO ON STOP UNDO, LEAVE:
    /* Get widget handles of Procedure Window and its editor widget. */
    RUN adecomm/_pwgetwh.p ( INPUT SELF , OUTPUT pw_Window ).
    RUN adecomm/_pwgeteh.p ( INPUT pw_Window , OUTPUT pw_Editor ).
           
    /* If Untitled, open Save As dialog box. */
    IF pw_Editor:NAME BEGINS {&PW_Untitled}
    THEN DO:
        /* Save As routine takes care of actually saving the file. */
        RUN adecomm/_pwsavas.p.
        RETURN.
    END.
    RUN adecomm/_pwsavef.p ( INPUT pw_Editor , 
                           INPUT pw_Editor:NAME ,
                           OUTPUT SAVE_OK ).    
    ASSIGN pw_Window:TITLE = {&PW_Title_Leader} + pw_Editor:NAME.
END.
