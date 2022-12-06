/*************************************************************/
/* Copyright (c) 1984-1996 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from Progress Software Corporation. */
/*************************************************************/

/**************************************************************************
    Procedure:  _pwprint.p
    
    Purpose:    Execute Procedure Window File->Print command.

    Syntax :    RUN adecomm/_pwprint.p.

    Parameters:
    Description:
    Notes  :
    Authors: John Palazzo
    Date   : January, 1994
**************************************************************************/

/* Procedure Window Global Defines. */
{ adecomm/_pwglob.i }

DEFINE VARIABLE pw_Window AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE pw_Editor AS WIDGET-HANDLE NO-UNDO.

DEFINE VAR Return_Status AS LOGICAL NO-UNDO.
DEFINE VAR Printed       AS LOGICAL NO-UNDO.
DEFINE VAR vModified     AS LOGICAL NO-UNDO.    
DEFINE VAR Tmp_File      AS CHARACTER NO-UNDO.

_PRINT_BLOCK:
DO ON STOP UNDO _PRINT_BLOCK , RETRY _PRINT_BLOCK:
  /* Get widget handles of Procedure Window and its editor widget. */
  RUN adecomm/_pwgetwh.p ( INPUT SELF , OUTPUT pw_Window ).
  RUN adecomm/_pwgeteh.p ( INPUT pw_Window , OUTPUT pw_Editor ).

  IF NOT RETRY THEN
  DO:
    IF pw_Editor:EMPTY THEN
    DO:
      MESSAGE "Nothing in this procedure to print."
            VIEW-AS ALERT-BOX INFORMATION BUTTONS OK
            IN WINDOW pw_Window.
      RETURN.
    END.
    
    /* In Windows, we put up a dialog box. So we don't need the wait. */
    IF NOT SESSION:WINDOW-SYSTEM BEGINS "MS-WIN":U THEN
        RUN adecomm/_setcurs.p ( INPUT "WAIT" ).
    DO ON STOP UNDO, LEAVE:
       RUN adecomm/_tmpfile.p ( "p" , ".ped" , OUTPUT Tmp_File ).
    END.
    ASSIGN vModified         = pw_Editor:MODIFIED
           Return_Status     = pw_Editor:SAVE-FILE( Tmp_File )
           pw_Editor:MODIFIED = vModified
    . /* END ASSIGN */
    RUN adecomm/_osprint.p ( INPUT pw_Window,
                             INPUT Tmp_File,
                             INPUT pw_Editor:FONT,
                             INPUT 1,
                             INPUT 0,
                             INPUT 0,
                             OUTPUT Printed ) .
  
  END. /* NOT RETRY */

  OS-DELETE VALUE( Tmp_File ) .
  /* In Windows, we put up a dialog box. So we don't need the wait. */
  IF NOT SESSION:WINDOW-SYSTEM BEGINS "MS-WIN":U THEN
      RUN adecomm/_setcurs.p ( INPUT "" ).
  /* This is just to be sure the user winds up in the editor. */
  APPLY "ENTRY" TO pw_Editor.

END. /* DO ON STOP */

/* _pwprint.p - end of file */
