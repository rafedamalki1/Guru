/*************************************************************/
/* Copyright (c) 1984-1995 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from Progress Software Corporation. */
/*************************************************************/

/**************************************************************************
    Procedure:  _fhidden.i

    Purpose:    Defines a 'static' variable which determines whether or
                not hidden tables are shown in the DD or Admin tool. The
                DD/Admin reports will use this value when they run.
                
    Syntax :    {prodict/_fhidden.i}

    Parameters: None

    Description:
    Notes  :
    Authors: Gerry Seidl
    Date   : 05/31/95
**************************************************************************/

DEFINE NEW GLOBAL SHARED VARIABLE fhidden AS LOGICAL NO-UNDO INITIAL NO.
