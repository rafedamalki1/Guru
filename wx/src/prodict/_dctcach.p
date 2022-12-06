/*************************************************************/
/* Copyright (c) 1984-1995 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

{ prodict/dictvar.i }
{ prodict/user/uservar.i }
{ prodict/user/userhue.i }
{ prodict/user/userhdr.f }

/* LANGUAGE DEPENDENCIES START */ /*---------------------------------------*/
DEFINE VARIABLE new_lang AS CHARACTER EXTENT 1 NO-UNDO INITIAL [
  /* 1*/ "Reading Schema..."
].
/* LANGUAGE DEPENDENCIES END */ /*-----------------------------------------*/

/* Include hidden tables in list? */
DEFINE INPUT PARAMETER p_hidden AS LOGICAL NO-UNDO.

DEFINE VARIABLE c AS CHARACTER NO-UNDO.

ASSIGN
  cache_dirty = FALSE
  cache_file# = 0
  cache_file  = ""
  c           = user_hdr.  /* save it */

PAUSE 0 BEFORE-HIDE.  /* Added BEFORE-HIDE to prevent prior messages from 
                         being cleared (e.g., from _usrsget.p) in tty mode
                         (Nordhougen 07/25/95) */

{prodict/user/userhdr.i new_lang[1]}

/* For gui only, schema tables may be included in the cache for
   the table get program.
   In tty, the user must type in a schema table name.
*/
IF p_hidden
 THEN FOR EACH DICTDB._File
    WHERE DICTDB._File._Db-recid = drec_db
    BY DICTDB._File._File-name:
    ASSIGN
      cache_file# = cache_file# + 1
      cache_file[cache_file#] = DICTDB._File._File-name.
    END.
 ELSE FOR EACH DICTDB._File
    WHERE DICTDB._File._Db-recid = drec_db
    AND NOT DICTDB._File._Hidden
    BY DICTDB._File._File-name:
    ASSIGN
      cache_file# = cache_file# + 1
      cache_file[cache_file#] = DICTDB._File._File-name.
  END.

{prodict/user/userhdr.i c}  /* restore header line */

/* to refresh the db/file status line */
DISPLAY (IF user_filename = "ALL"  THEN "ALL"
    ELSE IF user_filename = "SOME" THEN "SOME"
    ELSE IF drec_file     = ?      THEN ""
    ELSE                                user_filename) 
                           @ user_filename WITH FRAME user_ftr.

RETURN.
