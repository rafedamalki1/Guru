/*************************************************************/
/* Copyright (c) 1984-1996 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from Progress Software Corporation. */
/*************************************************************/

/**************************************************************************
    Procedure:  _pwrun.p
    
    Purpose:    Execute Procedure Window commands
                  Compile->Run
                  Compile->Check Syntax

    Syntax :    RUN adecomm/_pwrun.p (INPUT p_Action).

    Parameters:
    Description:
    Notes  :
    Authors: John Palazzo
    Date   : July, 1995
**************************************************************************/

/* Procedure Window Global Defines. */
{ adecomm/_pwglob.i }

/* PROGRESS Preprocessor system message number. */
&SCOPED-DEFINE PP-4345      4345

DEFINE INPUT PARAMETER p_Action AS CHARACTER NO-UNDO.

DEFINE NEW GLOBAL SHARED VARIABLE wfRunning     AS CHARACTER NO-UNDO.
DEFINE NEW GLOBAL SHARED VARIABLE h_ade_tool    AS HANDLE    NO-UNDO.

DEFINE VARIABLE h_tool    AS HANDLE NO-UNDO.
DEFINE VARIABLE pw_Window AS HANDLE NO-UNDO.
DEFINE VARIABLE pw_Editor AS HANDLE NO-UNDO.
DEFINE VARIABLE h_menu    AS HANDLE NO-UNDO.

DEFINE VARIABLE Msgs_Output  AS CHARACTER     NO-UNDO.
DEFINE VARIABLE Comp_File    AS CHARACTER     NO-UNDO.
DEFINE VARIABLE Msgs_File    AS CHARACTER     NO-UNDO.
DEFINE VARIABLE Comp_Stopped AS LOGICAL INIT TRUE NO-UNDO.
DEFINE VARIABLE Error_Found  AS LOGICAL       NO-UNDO.
DEFINE VARIABLE Error_Msg    AS CHARACTER     NO-UNDO.
DEFINE VARIABLE Error_File   AS CHARACTER     NO-UNDO.
DEFINE VARIABLE Error_Row    AS INTEGER       NO-UNDO.
DEFINE VARIABLE Error_Col    AS INTEGER       NO-UNDO.
DEFINE VARIABLE Error_Offset AS INTEGER       NO-UNDO.
DEFINE VARIABLE Err_Num      AS INTEGER       NO-UNDO.
DEFINE VARIABLE Comp_Err     AS INTEGER       NO-UNDO.
DEFINE VARIABLE Cannot_Run   AS LOGICAL       NO-UNDO.
DEFINE VARIABLE Run_Msg      AS CHARACTER     NO-UNDO.
DEFINE VARIABLE app_handle   AS HANDLE        NO-UNDO.


DO ON STOP UNDO, LEAVE:
    /* Get widget handles of Procedure Window and its editor widget. */
    RUN adecomm/_pwgetwh.p ( INPUT SELF , OUTPUT pw_Window ).
    RUN adecomm/_pwgeteh.p  ( INPUT pw_Window , OUTPUT pw_Editor ).
    
    IF ( pw_Editor:EMPTY ) THEN
    DO:
        ASSIGN Run_Msg = REPLACE(LC(p_Action) , "-syntax" , "").
        MESSAGE "Nothing in this procedure to " + Run_Msg + "."
                VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
        RETURN.
    END.
    
    /* Put up the wait cursor. */
    RUN adecomm/_setcurs.p (INPUT "WAIT":U).
    
    /* Generate a temporary filename to write editor contents to. */
    RUN adecomm/_tmpfile.p ( "" , ".cmp" , OUTPUT Comp_File ).
        
    /* Compile the contents of the editor widget. */
    RUN adecomm/_pwcmpfl.p (INPUT  pw_Editor      /* Editor handle.   */ ,
                            INPUT  "CMP_NOERROR"  /* Messages Dest.   */ ,
                            INPUT  Comp_File      /* Compile file.    */ ,
                            INPUT  ""             /* Messages file.   */ ,
                            OUTPUT Comp_Stopped   /* Compile Stopped? */ ).

    /* Force out of the DO ON STOP and perform temp file deletions. */
    IF Comp_Stopped THEN STOP.

    ASSIGN Error_Found = COMPILER:ERROR.
    IF ( Error_Found = TRUE ) THEN
    DO:
        /* Assign these COMPILER widget attributes to vars to prevent their
           current values from being overwritten by a session compile during
           development.
        */
        ASSIGN Error_File   = COMPILER:FILENAME
               Error_Row    = COMPILER:ERROR-ROW
               Error_Col    = COMPILER:ERROR-COLUMN
               Error_Offset = COMPILER:FILE-OFFSET
               . /* END ASSIGN */      
        /* If the error occurs in an include file, the names here will not be
           the same.  When this happens, we cannot move to the line with the
           error, because the COMPILER widget is reporting the error values
           relative to the include file, not the file being compiled.  So,
           only move the cursor if the error occurred in the file being
           compiled.
        */
        IF ( Error_File = Comp_File ) THEN
        DO:
          &IF "{&WINDOW-SYSTEM}" BEGINS "MS-WIN" &THEN
           /* LARGE MSW editor does not support :CURSOR-OFFSET. */
           ASSIGN pw_Editor:CURSOR-LINE = Error_Row WHEN Error_Row <> 0
                  pw_Editor:CURSOR-CHAR = Error_Col WHEN Error_Col <> 0.
          &ELSE
           ASSIGN pw_Editor:CURSOR-OFFSET = Error_Offset
                                            WHEN Error_Offset <> 0.
          &ENDIF
        END.
    END.    
        
    /* Remove the wait cursor. */
    RUN adecomm/_setcurs.p (INPUT "":U).

    /* WARNING:
       DO NOT USE THE NO-ERROR OPTION ON ANY STATEMENT BETWEEN
       RUN adecomm/_pwcmpfl.p AND THIS CALL TO _errmsgs.p.
       PROCEDURE _errmsgs.p relies on the ERROR-STATUS HANDLE
       TO CORRECTLY REPORT ERRORS.
    */
    /* Display preprocessor and error messages, if any. */
    RUN adecomm/_errmsgs.p (INPUT pw_Window ,
                            INPUT Error_File ,
                            INPUT Comp_File ).
        
    IF Error_Found = FALSE THEN
    DO: /* No error. */
    CASE p_Action:

      WHEN "RUN":U THEN
      DO:
        RUN adecomm/_wfrun.p
            ( INPUT "A " + pw_Window:NAME + "," + STRING(pw_Window),
              OUTPUT Cannot_Run ).
        IF Cannot_Run = FALSE THEN
        DO ON STOP UNDO, RETRY:
            ASSIGN h_tool = h_ade_tool.
            IF NOT RETRY THEN
            DO:
              RUN disable_widgets IN h_tool NO-ERROR.
              ASSIGN pw_Window:VISIBLE = FALSE.
              RUN adecomm/_runcode.p
                  ( INPUT Comp_File ,
                    INPUT "_PAUSE":U /* Run Flags */ ,
                    INPUT ?     /* p_Stop_Widget */ ,
                    OUTPUT app_handle ) .
            END. 
            RUN enable_widgets IN h_tool NO-ERROR.
            ASSIGN wfRunning = "".
            ASSIGN pw_Window:VISIBLE = TRUE.
         END.
      END. /* WHEN "RUN":U */

      WHEN "CHECK-SYNTAX":U THEN
         MESSAGE "Syntax is correct."
             VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
    END CASE.
    END. /* Error_Found = FALSE */
END. /* DO ON STOP */

OS-DELETE VALUE ( Comp_File ).
/* If compile stopped, ensure wait cursor is removed. */
IF Comp_Stopped THEN
    RUN adecomm/_setcurs.p (INPUT "":U).

APPLY "ENTRY" TO pw_Editor.
