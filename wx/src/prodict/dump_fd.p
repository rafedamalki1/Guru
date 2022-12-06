/*************************************************************/
/* Copyright (c) 1984-1993 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

DEFINE INPUT PARAMETER file-name    AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER fd-file-name AS CHARACTER NO-UNDO.

{ prodict/user/uservar.i NEW }
{ prodict/dictvar.i NEW }

FIND FIRST _Db WHERE _Db._Db-local.

ASSIGN
  user_dbname   = LDBNAME("DICTDB")
  user_dbtype   = DBTYPE("DICTDB")
  drec_db       = RECID(_Db)
  user_filename = file-name
  user_env[1]   = file-name
  user_env[2]   = fd-file-name.
DO TRANSACTION ON ERROR UNDO,LEAVE ON ENDKEY UNDO,LEAVE:
  RUN "prodict/dump/_dmpbulk.p".
END.
RETURN.
