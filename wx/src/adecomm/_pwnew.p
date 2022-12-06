/*************************************************************/
/* Copyright (c) 1984-1995 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from Progress Software Corporation. */
/*************************************************************/

/**************************************************************************
    Procedure:  _pwnew.p
    
    Purpose:    Execute Procedure Window File->New command.

    Syntax :    RUN adecomm/_pwnew.p.

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

DEFINE VARIABLE OK_Close     AS LOGICAL   NO-UNDO.
DEFINE VARIABLE Old_Name     AS CHARACTER NO-UNDO.
DEFINE VARIABLE New_Name     AS CHARACTER NO-UNDO.

/* --- Begin SCM changes --- */
DEFINE VAR scm_ok       AS LOGICAL           NO-UNDO.
/* --- End SCM changes ----- */

DO ON STOP UNDO, LEAVE:
    /* Get widget handles of Procedure Window and its editor widget. */
    RUN adecomm/_pwgetwh.p ( INPUT SELF , OUTPUT pw_Window ).
    RUN adecomm/_pwgeteh.p ( INPUT pw_Window , OUTPUT pw_Editor ).
    
    /* Because _pwclosf.p updates the ed:NAME, store off for later use. */
    ASSIGN Old_Name = pw_Editor:NAME
           New_Name = pw_Editor:NAME.
            
    RUN adecomm/_pwclosf.p ( INPUT pw_Window , INPUT pw_Editor ,
                             INPUT "New" ,
                             OUTPUT OK_Close ) .
    IF OK_Close <> TRUE THEN RETURN.

    /* --- Begin SCM changes --- */
    RUN adecomm/_adeevnt.p
        (INPUT  {&PW_NAME} , INPUT "Before-Close",
         INPUT  STRING( pw_Editor ) , INPUT pw_Editor:NAME ,
         OUTPUT scm_ok ).
    IF scm_ok = FALSE THEN RETURN.
    /* --- End SCM changes ----- */
    
    /* --- Begin SCM changes --- */
    RUN adecomm/_adeevnt.p 
        (INPUT  {&PW_NAME} , INPUT "Close",
         INPUT  STRING( pw_Editor ) , INPUT pw_Editor:NAME , 
         OUTPUT scm_ok ).
    /* --- End SCM changes ----- */

    /* If file was not Untitled, then get a new Untitled procedure name. */
    IF NOT Old_Name BEGINS {&PW_Untitled}
    THEN RUN adecomm/_pwgetun.p ( OUTPUT New_Name ).

    /* Clear buffer and rename to Untitled. */
    ASSIGN pw_Editor:SCREEN-VALUE = ""
           pw_Editor:NAME         = New_Name
           pw_Window:TITLE        = {&PW_Title_Leader} + New_Name.   
    
    APPLY "ENTRY" TO pw_Editor.
    
    /* --- Begin SCM changes --- */
    RUN adecomm/_adeevnt.p 
        (INPUT  {&PW_NAME} , INPUT "New",
         INPUT STRING( pw_Editor ), INPUT ? , 
         OUTPUT scm_ok ).
    /* --- End SCM changes ----- */

END.
