/*************************************************************/
/* Copyright (c) 1984-1997 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from Progress Software Corporation. */
/*************************************************************/

/**************************************************************************
    Procedure:  _pwrdfl.p
    
    Purpose:    Reads a specified file into a Procedure Window
                editor widget and updates the PW Title bar.
                Updates ED:NAME as well.

    Syntax :    
                RUN adecomm/_pwrdfl.p
                    ( INPUT  p_Editor , INPUT  p_File_Name ,
                      OUTPUT p_Read_OK ).

    Parameters:
    Description:
    Notes  :
    Authors: John Palazzo
    Date   : January, 1994
**************************************************************************/

/* Procedure Window Global Defines. */
{ adecomm/_pwglob.i }

DEFINE INPUT  PARAMETER p_Editor    AS WIDGET    NO-UNDO.
DEFINE INPUT  PARAMETER p_File_Name AS CHARACTER NO-UNDO.     
DEFINE OUTPUT PARAMETER p_Read_OK   AS LOGICAL   NO-UNDO.

DEFINE VARIABLE pw_Window    AS WIDGET    NO-UNDO.
DEFINE VARIABLE Dlg_Answer   AS LOGICAL   NO-UNDO.
DEFINE VARIABLE OK_Close     AS LOGICAL   NO-UNDO.
DEFINE VARIABLE In_Library   AS CHARACTER NO-UNDO.
DEFINE VARIABLE Open_Msg     AS CHARACTER NO-UNDO.

DO ON STOP UNDO, LEAVE:

    /* --- Begin SCM changes ----- */
    ASSIGN p_Read_OK = TRUE.
    RUN adecomm/_adeevnt.p 
        ( INPUT {&PW_NAME} ,
          INPUT "Before-Open", INPUT ?, INPUT p_File_Name ,
          OUTPUT p_Read_OK ).
    IF NOT p_Read_OK THEN RETURN.
    /* --- End SCM changes ----- */

    /* Get widget handle of Procedure Window. */
    RUN adecomm/_pwgetwh.p ( INPUT p_Editor , OUTPUT pw_Window ).

    /* Try to read file. */
    ASSIGN p_Read_OK = p_Editor:READ-FILE( p_File_Name ) NO-ERROR.
    IF (p_Read_OK = FALSE) OR (ERROR-STATUS:NUM-MESSAGES > 0)
    THEN DO:
      ASSIGN In_Library = LIBRARY( p_File_Name ).
      IF ( In_Library <> ? ) THEN
        /* 1. File in R-code Library. */
        ASSIGN Open_Msg = "File is in R-code Library " +
                           In_Library + ".":U .
      ELSE DO:
        /* 2. Path or Filename incorrect. */
        ASSIGN FILE-INFO:FILE-NAME = p_File_Name.
        IF FILE-INFO:FULL-PATHNAME = ? THEN
            ASSIGN Open_Msg = "The path or filename may be incorrect or " +
                              "the file may not exist.".
        /* 3. No read permissions. */
        ELSE IF INDEX(FILE-INFO:FILE-TYPE , "R":U) = 0 THEN
            ASSIGN Open_Msg = "You do not have read permission.".
        /* 4. File may be too large. */
        ELSE
            ASSIGN Open_Msg = "The file may be too large to open.".
      END.
      MESSAGE p_File_Name skip
              "Cannot open file." SKIP(1)
              Open_Msg
              VIEW-AS ALERT-BOX ERROR BUTTONS OK IN WINDOW pw_Window.
    END.

    IF p_Read_OK = FALSE THEN RETURN.
    /* Update information. */
    ASSIGN p_Editor:NAME   = p_File_Name
           pw_Window:TITLE = {&PW_Title_Leader} + p_File_Name .
END.
