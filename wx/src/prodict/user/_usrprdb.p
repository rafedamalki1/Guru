/*************************************************************/
/* Copyright (c) 1984-1993 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/* create a new database */

{ prodict/user/uservar.i }

DEFINE VARIABLE newdb AS CHARACTER NO-UNDO.

RUN adecomm/_dbcreat.p (INPUT "", INPUT-OUTPUT newdb).
user_env[2] = newdb.

IF newdb = ? THEN user_path = "".

