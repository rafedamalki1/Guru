/*************************************************************/
/* Copyright (c) 1984-1998 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

{ prodict/dictvar.i }
{ prodict/user/uservar.i }

DEFINE VARIABLE i AS INTEGER NO-UNDO.
DEFINE VARIABLE j AS INTEGER NO-UNDO.
DEFINE VARIABLE l AS LOGICAL NO-UNDO.

ASSIGN
  j           = INDEX(user_filename,".")
  i	      = INDEX(user_filename," ")
  user_env[4] = SUBSTRING(user_filename,j + 1, i - j - 1,"character").
RUN prodict/gate/_gat_xlt.p (TRUE,drec_db,INPUT-OUTPUT user_env[4]).

ASSIGN
  user_env[1]   = SUBSTRING(user_filename,j + 1, i - j - 1,"character")
  user_env[2]   = SUBSTRING(user_filename,i + 1,-1,"character")
  user_env[3]   = SUBSTRING(user_filename,1,j - 1,"character")
  user_filename = "".

RUN prodict/ora/_ora_mak.p. /* bring over definitions for the indicated file */

RETURN.
