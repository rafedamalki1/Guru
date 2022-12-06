/*************************************************************/
/* Copyright (c) 1984-1995 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

{ as4dict/dictvar.i shared}
{ as4dict/dump/dumpvar.i shared }
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

/* temporary for testing - this definition belongs in dictvar.i */
/* DEFINE {1} SHARED VARIABLE file_num   AS INTEGER   INITIAL ?  NO-UNDO. */

ASSIGN
  cache_dirty = FALSE
  cache_file# = 0
  cache_file  = "".
PAUSE 0.
c = user_hdr.  /* save it */
{prodict/user/userhdr.i new_lang[1]}

/* For gui only, schema tables may be included in the cache for
   the table get program.
   In tty, the user must type in a schema table name.
*/
IF p_hidden THEN
  FOR EACH as4dict.p__File 
    BY as4dict.p__File._File-name:
    ASSIGN
      cache_file# = cache_file# + 1
      cache_file[cache_file#] = as4dict.p__File._File-name.
  END.
ELSE
  FOR EACH as4dict.p__File WHERE as4dict.p__file._Hidden <> "Y"
    BY as4dict.p__File._File-name:
    ASSIGN
      cache_file# = cache_file# + 1
      cache_file[cache_file#] = as4dict.p__File._File-name.
  END.

{prodict/user/userhdr.i c}  /* restore header line */

/* to refresh the db/file status line */
DISPLAY (IF user_filename = "ALL"  THEN "ALL"
  ELSE IF user_filename   = "SOME" THEN "SOME"
  ELSE IF file_num        = ? THEN ""
  ELSE user_filename) @ user_filename WITH FRAME user_ftr.

RETURN.
