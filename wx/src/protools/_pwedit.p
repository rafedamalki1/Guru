/*************************************************************/
/* Copyright (c) 1984-1995 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from Progress Software Corporation. */
/*************************************************************/

/**************************************************************************
    Procedure:  _pwedit.p

    Purpose:    ProTools front-end call to Procedure Window main procedure.

    Syntax :    RUN protools/_pwedit.p.

    Parameters:
    Description:
    Notes  :
                Parent ID passed is Null (""). This "parents" Procedure
                Windows created by ProTools to the ADE Desktop. When
                the ADE Desktop is told to exit, it notifies each
                of its Procedure Windows (Parent ID = Null), giving each
                a chance to let the user save work-in-progress.
    Authors: John Palazzo
    Date   : March, 1994
**************************************************************************/

RUN adecomm/_pwmain.p ("",   /* Parent ID [eg. UIB]. */
                       "",   /* FileList */
                       "").  /* p_Edit_command (Currently Unused) */
