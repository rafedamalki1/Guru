/*************************************************************/
/* Copyright (c) 1984-1995 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/* _A4dmpd -  Controlling jacket program for dumping .d files   */
/*            from an AS/400 database.  This mirrors the dump   */
/*            process (in usermenu.i) which runs from the       */
/*            dataserver utilities menu.                        */
/*  Initial creation:  May 4, 1995 - NEH                        */

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

/* Make sure there are tables to dump.  */

find first _File 
     where _File._Db-recid  = drec_db.

Find first _File WHERE NOT _File._File-Name BEGINS "P__" AND 
           NOT _File._File-Name BEGINS "_" AND 
           _File._File-Name <> "qcmd" NO-ERROR.

If NOT AVAILABLE (_File) THEN DO:
  Message "Schema has no tables to dump.  Run 'Synchronize
  Progress/400 Client' to update Progress schema files." 
           view-as ALERT-BOX ERROR buttons OK.
     RETURN.
  END.
     
ASSIGN 
   user_path = "*N".

/* Refresh list of tables, if coming back in */
cache_dirty = yes.
user_env[1] = "s". 

RUN "prodict/gui/_guitget.p".

if user_path <> "" THEN DO ON ERROR UNDO, RETURN:
   user_env[9] = "f".
   RUN "prodict/user/_usrdump.p".
END.

if user_path <> "" THEN 
   RUN "prodict/dump/_dmpdata.p".

/* Reset selected db */
ASSIGN user_dbname = save_db.            

RETURN.