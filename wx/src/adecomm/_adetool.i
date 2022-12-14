/*************************************************************/
/* Copyright (c) 1984-1995 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from Progress Software Corporation. */
/*************************************************************/

/**************************************************************************
    Procedure:  _adetool.i
                                       
    Purpose:    Defines an internal procedure called 'ADEPersistent'. This
                should be included in any Persistent procedure which you do
                not want to be automatically deleted by any ADE tools (e.g.
                Editor, UIB). The program _runtool.p will check for the
                existence of ADEPersistent in a procedure and will delete
                it only if it doesn't exist.

    Syntax :    {adecomm/_adetool.i}

    Parameters: None
        
    Description:
    Notes  :
    Authors: Gerry Seidl
    Date   : 1/18/95
**************************************************************************/
PROCEDURE ADEPersistent:
    RETURN "OK".
END.
