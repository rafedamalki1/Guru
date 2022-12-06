/*************************************************************/
/* Copyright (c) 1984-1995 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/* _dctssub.p - called by _dctsget.p */
/*
{ prodict/dictvar.i }

FOR EACH _Db WHERE _Db._Db-type <> "PROGRESS":
  IF cache_db# = EXTENT(cache_db_s) THEN LEAVE.
  ASSIGN
    cache_db# = cache_db# + 1
    cache_db_s[cache_db#] = cache_db_s[cache_db# - 1] /* not SDBNAME! */
    cache_db_l[cache_db#] = _Db._Db-name
    cache_db_p[cache_db#] = PDBNAME(_Db._Db-name)
    cache_db_t[cache_db#] = CAPS(_Db._Db-type).
END.
RETURN.
*/
message "_dctssub.p is old and should no longer get used!" view-as alert-box.
