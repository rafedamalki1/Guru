/*************************************************************/
/* Copyright (c) 1984-1998 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

{ prodict/dictvar.i }
{ prodict/user/uservar.i }
{ prodict/ora/oravar.i }

DEFINE VARIABLE c AS CHARACTER NO-UNDO.

FIND _Db WHERE _Db._Db-name = ora_dbname NO-LOCK NO-ERROR.

IF NOT AVAILABLE _Db THEN DO TRANSACTION:
  CREATE _Db.
  ASSIGN
    _Db._Db-name    = ora_dbname
    _Db._Db-comm    = ""
    _Db._Db-type    = "ORACLE"
    _Db._Db-slave   = TRUE
    _Db._Db-Misc1[3] = ora_version
    _Db._Db-xl-name = ora_codepage.
END.

ASSIGN
  user_dbname = _Db._Db-name
  user_dbtype = _Db._Db-type
  drec_db     = RECID(_Db)
  c           = "add".

RUN prodict/ora/_ora_sys.p (drec_db,INPUT-OUTPUT c).

RETURN.
