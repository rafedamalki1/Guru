/*************************************************************/
/* Copyright (c) 1984-1996 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from Progress Software Corporation. */
/*************************************************************/

/**************************************************************************
    Procedure:  _smoupg.p

    Purpose:    Checks to see if the UIB is running and if it is
                it launches adeuib/_smoupg.w.

    Parameters: None

    Description:
    Notes  :
    Authors: Gerry Seidl
    Date   : 9/12/96
**************************************************************************/

DEFINE VARIABLE level          AS INTEGER NO-UNDO INITIAL 1. 
DEFINE VARIABLE uib_is_running AS LOGICAL NO-UNDO INITIAL NO.

REPEAT WHILE PROGRAM-NAME(level) <> ?.
  IF PROGRAM-NAME(level) = "adeuib/_uibmain.p" THEN uib_is_running = TRUE.
  ASSIGN level = level + 1.
END.
IF NOT uib_is_running THEN DO:
  MESSAGE "The UIB is not running. You must start the UIB before running the SmartObject Upgrade Utility." VIEW-AS ALERT-BOX ERROR.
  RETURN.
END.
ELSE
  RUN adeuib/_smoupg.w PERSISTENT.

RETURN.
