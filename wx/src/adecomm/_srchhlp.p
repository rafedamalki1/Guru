/*************************************************************/
/* Copyright (c) 1984-1993 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from Progress Software Corporation. */
/*************************************************************/
/* $Id: _srchhlp.p,v 1.1 93/06/06 04:38:33 jep Exp $ */
/* $Log:	_srchhlp.p,v $
 * Revision 1.1  93/06/06  04:38:33  jep
 * Initial revision
 *  */

/***************************************************************************
    Procedure:      _srchhlp.p
    Purpose:        ADE Help API for Common Editor Search Routines.

    Run Syntax:     RUN adecomm/_srchhlp.p ( INPUT p_Help_On ) .

    Parameters:

          INPUT p_Help_On
                The SYSTEM-HELP <context-string> for which you want help.

                For example, the context string of the Find dialog
                box is Find_Dialog_Box.  This is converted to the
                appropriate SYSTEM-HELP <context-number>.

    Description:
    Notes:
***************************************************************************/

/* adecomm/_srchhlp.p */

DEFINE INPUT PARAMETER p_Help_On      AS CHARACTER NO-UNDO .

{ adecomm/commeng.i }

DEFINE VAR             vHelp_Context  AS INTEGER   NO-UNDO .

REPEAT ON STOP UNDO, LEAVE: /* proc-main */

    CASE p_Help_On :

        /*--------------- Search Help ---------------*/
        WHEN "Find_Dialog_Box"
    	    THEN vHelp_Context = {&Find_Dialog_Box} .

        WHEN "Replace_Dialog_Box"
    	    THEN vHelp_Context = {&Replace_Dialog_Box} .

        WHEN "Goto_Line_Dialog_Box"
            THEN vHelp_Context = {&Goto_Line_Dialog_Box} .

    END CASE .

    RUN adecomm/_adehelp.p
        ( INPUT "comm" , INPUT "CONTEXT" , INPUT vHelp_Context , INPUT ? ) .
    LEAVE.

END. /* proc-main */
