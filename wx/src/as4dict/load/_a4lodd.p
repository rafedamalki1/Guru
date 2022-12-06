/*************************************************************/
/* Copyright (c) 1984-1995 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/* _A4lodd -  Controlling jacket program for loading .d files   */
/*            to an AS/400 database.  This is the same load     */
/*            process (in usermenu.i) which runs from the       */
/*            dataserver utilities menu.                        */
/*  Initial creation:  May 18, 1995 - NEH                       */

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

user_path = "*N".
/* Refresh list of tables, if coming back in */
cache_dirty = yes.
user_env[1] = "s". 
RUN "prodict/gui/_guitget.p".

if user_path <> "" THEN DO ON ERROR UNDO, RETURN:
   user_env[9] = "f".
   RUN "prodict/user/_usrload.p".
END.

if user_path <> "" THEN 
   RUN "prodict/dump/_loddata.p".            

ASSIGN user_dbname = save_db.

RETURN.