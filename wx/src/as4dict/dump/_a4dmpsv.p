/*************************************************************/
/* Copyright (c) 1984-1995 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/* _A4dmpsv - Controlling jacket program for dumping sequence   */
/*            values from an AS/400 database.  This mirrors     */
/*            the dump process (in usermenu.i) which runs from  */
/*            the dataserver utilities menu.                    */
/*  Initial creation:  May 30, 1995 - Nhorn                     */

{as4dict/dictvar.i shared}
{as4dict/dump/dumpvar.i shared}

DEFINE VARIABLE save_db AS CHARACTER NO-UNDO.

/*  Find the correct database selected.  Preserve the one in user_dbname */
/*  Set up the recid for the correct database. */

FIND _db WHERE _db._Db-name = LDBNAME("as4dict").
  ASSIGN 
     save_db = user_dbname
     user_dbname = _Db._Db-name
     drec_db = RECID(_db).

user_env[9] = "k".
RUN "as4dict/dump/_usrdump.p".

if not user_cancel THEN
    RUN "as4dict/dump/_dmpseqs.p".

/* reset user cancel so user can come back in within same process */       
ASSIGN
   user_cancel = no
   user_dbname = save_db.

RETURN.