/*************************************************************/
/* Copyright (c) 1984-1995 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/* gate_set - involved in CTOS-ISAM, C-ISAM, NetISAM, RMS db initialization */


{ prodict/dictvar.i }
{ prodict/user/uservar.i }
{ prodict/user/userhue.i }
{ prodict/user/userhdr.f }

run adecomm/_setcurs.p ("WAIT").

FIND _Db WHERE _Db._Db-name = user_env[2].
ASSIGN
  user_dbname   = user_env[2]
  user_dbtype   = (IF     user_env[7] = "b" THEN "CTOS-ISAM"
                  ELSE IF user_env[7] = "c" THEN "CISAM"
                  ELSE IF user_env[7] = "n" THEN "NETISAM"
                  ELSE                           "RMS")
  cache_dirty   = TRUE
  user_filename = ""
  drec_file     = ?
  drec_db       = RECID(_Db).

connect VALUE(user_dbname) -dt VALUE(user_dbtype) no-error.

if not connected(user_dbname)
 then do:
  message
    "The connect failed, however you can add" skip
    "table-definitons without being connected."
    view-as alert-box.
  end.

 else do:

  CREATE ALIAS "DICTDB"  FOR DATABASE VALUE(SDBNAME(user_dbname)) NO-ERROR.
  CREATE ALIAS "DICTDBG" FOR DATABASE VALUE(user_dbname) NO-ERROR.

  RUN "prodict/_dctsget.p".
  RUN "prodict/_dctsset.p" (user_dbname).

  end.
  
{ prodict/user/usercon.i }

run adecomm/_setcurs.p ("").

RETURN.
