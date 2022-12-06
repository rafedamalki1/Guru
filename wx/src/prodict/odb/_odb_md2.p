/*************************************************************/
/* Copyright (c) 1984-1996 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

{ prodict/dictvar.i }
{ prodict/user/uservar.i }
{ prodict/odb/odbvar.i }
{ prodict/odb/odb_ctl.i }	

DEFINE VARIABLE c AS CHARACTER NO-UNDO.

FIND DICTDB._Db WHERE DICTDB._Db._Db-name = odb_dbname NO-LOCK NO-ERROR.

IF NOT AVAILABLE DICTDB._Db THEN DO TRANSACTION:
  CREATE DICTDB._Db.
  ASSIGN
    DICTDB._Db._Db-name    = odb_dbname 
    DICTDB._Db._Db-addr    = odb_pdbname
    DICTDB._Db._Db-comm    = ""
    DICTDB._Db._Db-slave   = TRUE
    DICTDB._Db._Db-type    = {adecomm/ds_type.i
                        &direction = "etoi"
                        &from-type = "user_env[22]"
                        }.
    { prodict/gate/gat_cp1a.i
              &incpname = "odb_codepage" }
END.

ASSIGN
  user_dbname = DICTDB._Db._Db-name
  user_dbtype = DICTDB._Db._Db-type
  drec_db     = RECID(DICTDB._Db)
  c           = "add".

RUN prodict/odb/_odb_sys.p (drec_db,INPUT-OUTPUT c).

RETURN.
