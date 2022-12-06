/*************************************************************/
/* Copyright (c) 1984-1995 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from Progress Software Corporation. */
/*************************************************************/

/**************************************************************************
    Procedure:  _pwclosf.p
    
    Purpose:    Low-level routine to Close a Procedure Window file.

    Syntax :    RUN adecomm/_pwclosf.p ( INPUT p_Window , INPUT p_Editor ,
                                       INPUT p_Action ,
                                       OUTPUT p_OK_Close ) .

    Parameters:
        See DEFINE PARAMETER in code.
        
    Description:
        1. If unsaved changes, ask user to Save Changes: Yes-No-Cancel.
        2. If Untitled, open Save As dialog box.
        3. Finally, perform actual file save.

    Notes  :
    Authors: John Palazzo
    Date   : January, 1994
**************************************************************************/

/* Procedure Window Global Defines. */
{ adecomm/_pwglob.i }

DEFINE INPUT  PARAMETER p_Window   AS WIDGET-HANDLE NO-UNDO.
DEFINE INPUT  PARAMETER p_Editor   AS WIDGET-HANDLE NO-UNDO.
DEFINE INPUT  PARAMETER p_Action   AS CHARACTER     NO-UNDO.
DEFINE OUTPUT PARAMETER p_OK_Close AS LOGICAL       NO-UNDO.
    /* p_OK_Close = TRUE means its ok to continue operation.
                  = FALSE or ? means its not.
    */

DEFINE VARIABLE File_Name   AS CHARACTER NO-UNDO.
DEFINE VARIABLE l_OK        AS LOGICAL   NO-UNDO.

REPEAT ON STOP UNDO, RETRY:
        IF RETRY
        THEN DO:
            ASSIGN p_OK_Close = ?.    /* Cancel */
            RETURN.
        END.
        
        /* Bring PW to top. */
        ASSIGN l_OK = p_Window:MOVE-TO-TOP().
        
        /* Ask user to Save Changes: Yes-No-Cancel. Returns NO if unsaved
           changes or if user answered NO. */
        RUN adecomm/_pwfchg.p ( INPUT p_Editor , INPUT p_Action ,
                                OUTPUT p_OK_Close ). 
        
        IF p_OK_Close = ? THEN RETURN.  /* Cancel */   
        IF p_OK_Close = NO              /* NO - Don't Save. */
        THEN DO:
            ASSIGN p_OK_Close = TRUE. /* Means ok to contine and close file. */
            RETURN.
        END.
        
        IF p_OK_Close = YES /* Yes->Save. */
        THEN DO:
            /* If Untitled, open Save As dialog box. */
            IF p_Editor:NAME BEGINS {&PW_Untitled}
            THEN DO:
                RUN adecomm/_getfile.p ( INPUT p_Window , INPUT "Procedure" ,
                                         "Save As" , "Save As" , "SAVE", 
                                         INPUT-OUTPUT File_Name ,
                                         OUTPUT p_OK_Close ).       
                IF p_OK_Close = NO THEN STOP. /* Cancel. */
            END.
            ELSE
                ASSIGN File_Name = p_Editor:NAME.

            RUN adecomm/_pwsavef.p ( INPUT p_Editor , 
                                     INPUT File_Name ,
                                     OUTPUT p_OK_Close ).
        END.
        LEAVE.
END.
