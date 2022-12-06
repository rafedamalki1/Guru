/*************************************************************/
/* Copyright (c) 1984-1995 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from Progress Software Corporation. */
/*************************************************************/

/**************************************************************************
    Procedure:  _pwfmod.p
    
    Purpose:    Returns TRUE if editor is a modified; FALSE if not.

    Syntax :    RUN adecomm/_pwfmod.p ( INPUT p_Editor , INPUT p_Title ,
                                      OUTPUT p_Modified ) .

    Parameters:
    Description:
    Notes  :
    Authors: John Palazzo
    Date   : January, 1994
**************************************************************************/

/* Procedure Window Global Defines. */
{ adecomm/_pwglob.i }

DEFINE INPUT  PARAMETER p_Editor   AS WIDGET-HANDLE      NO-UNDO.
DEFINE OUTPUT PARAMETER p_Modified AS LOGICAL INIT FALSE NO-UNDO.
    
DO:
    IF ( p_Editor:MODIFIED = NO ) OR 
       ( p_Editor:NAME BEGINS {&PW_Untitled} AND
         p_Editor:EMPTY ) 
    THEN p_Modified = FALSE.
    ELSE p_Modified = TRUE.

END.
