/*************************************************************/
/* Copyright (c) 1984-1997 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from Progress Software Corporation. */
/*************************************************************/

/**************************************************************************
    Procedure:  _pwsavef.p
    
    Purpose:    Low-level routine to save editor contents to disk file.

    Syntax :    RUN adecomm/_pwsavef.p ( INPUT p_Editor , 
                                       INPUT p_File_Selected ,
                                       OUTPUT p_Saved_File ) .

    Parameters:
    Description:
    Notes  :
    Authors: John Palazzo
    Date   : January, 1994
**************************************************************************/

/* Procedure Window Global Defines. */
{ adecomm/_pwglob.i }

DEFINE INPUT  PARAMETER p_Editor         AS WIDGET-HANDLE NO-UNDO.
DEFINE INPUT  PARAMETER p_File_Selected  AS CHARACTER     NO-UNDO.
DEFINE OUTPUT PARAMETER p_Saved_File     AS LOGICAL       NO-UNDO.

DEFINE VARIABLE pw_Window AS WIDGET-HANDLE NO-UNDO.

/* --- Begin SCM changes --- */
DEFINE VAR scm_ok       AS LOGICAL           NO-UNDO.
/* --- End SCM changes ----- */
  
DO ON STOP UNDO, LEAVE:
  /* Get widget handles of Procedure Window. */
  RUN adecomm/_pwgetwh.p ( INPUT p_Editor , OUTPUT pw_Window ).

  /* --- Begin SCM changes --- */
  RUN adecomm/_adeevnt.p 
      (INPUT  {&PW_NAME} , INPUT "Before-Save",
       INPUT STRING( p_Editor ), INPUT p_File_Selected , 
       OUTPUT scm_ok ).
  IF scm_ok = FALSE THEN
  DO:
      ASSIGN p_Saved_File = FALSE.  /* Cancel Save. */
      RETURN.
  END.
  /* --- End SCM changes ----- */

  ASSIGN p_Saved_File = p_Editor:SAVE-FILE( p_File_Selected ) NO-ERROR .

  IF ( p_Saved_File = FALSE )
  THEN DO:
      MESSAGE p_File_Selected SKIP
        "Cannot save to this file."  SKIP(1)
        "File is read-only or the path specified" SKIP
        "is invalid. Use a different filename."
        VIEW-AS ALERT-BOX WARNING BUTTONS OK IN WINDOW pw_Window.
  END.
  ELSE DO:
    /* Assign new file name to editor widget and update Window title. */
    ASSIGN p_Editor:NAME   = p_File_Selected
           pw_Window:TITLE = {&PW_Title_Leader} + p_Editor:NAME.
    
    /* Reset the EDIT-CAN-UNDO attribute. */
    ASSIGN p_Editor:EDIT-CAN-UNDO = FALSE.
   
    /* --- Begin SCM changes --- */
    RUN adecomm/_adeevnt.p 
        (INPUT  {&PW_NAME} , INPUT "Save",
         INPUT STRING( p_Editor ), INPUT p_Editor:NAME , 
         OUTPUT scm_ok ).
    /* --- End SCM changes ----- */
    
  END.      

END.
