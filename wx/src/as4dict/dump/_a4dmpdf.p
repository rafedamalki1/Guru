/*************************************************************/
/* Copyright (c) 1984-1997 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/* _A4dmpdf - Controlling jacket program for dumping .df files  */
/*            from an AS/400 database.  This mirrors the dump   */
/*            process (in usermenu.i) which runs from the       */
/*            dataserver utilities menu.                        */
/*  Initial creation:  May 1, 1995 - NEH                        */
/*          Modified:  10/9/97 DLM Changed assignment to        */
/*                     user_dbname 97-10-08-016                 */

{as4dict/dictvar.i shared}
{as4dict/dump/dumpvar.i shared}

DEFINE VARIABLE save_db AS CHARACTER NO-UNDO.

/*  Find the correct database selected.  Preserve the one in user_dbname */
  ASSIGN 
   save_db = user_dbname  
   user_dbname = PDBNAME("as4dict").

/* If we're coming back in, make sure the list of tables is refreshed. */
cache_dirty = yes.
user_env[1] = "a". 
RUN "as4dict/dump/_guitget.p".

if not user_cancel THEN DO:
    user_env[9] = "d".
    RUN "as4dict/dump/_usrdump.p".
END.

if not user_cancel THEN 
    RUN "as4dict/dump/_dmpsddl.p".

/* reset user cancel so user can come back in within same process */       
ASSIGN
   user_cancel = no
   user_dbname = save_db.

RETURN.
