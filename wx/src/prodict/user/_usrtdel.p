/*************************************************************/
/* Copyright (c) 1984-1999 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/**************************************************************
/* _usrtdel.p - delete files */

History
    laurief   04/09/98    Changed user_env[1] from "as" to "s" as part
                          of fix for BUG 97-07-22-029.

**************************************************************/

{ prodict/dictvar.i }
{ prodict/user/uservar.i }
{ prodict/user/userhue.i }
{ prodict/user/userhdr.f }

DEFINE VARIABLE answer  AS LOGICAL   INITIAL FALSE NO-UNDO.
DEFINE VARIABLE i       AS INTEGER                 NO-UNDO.
DEFINE VARIABLE msg-txt AS CHARACTER INITIAL     ? NO-UNDO.
DEFINE VARIABLE skipped AS LOGICAL   INITIAL FALSE NO-UNDO.

/* This program has a rather odd flow of control.  It is actually
executed four times.  The first time is to set up the execution string
and begin the transaction.  The second is to build the list of
deleteable files.  After allowing the user to select from a scrolling
list, the third time actual performs the deletion.  After that, the
transaction is allowed to end, and then it is called a fourth time to
display the "finished" message. */

/*--------------------------------------------------------------------------*/
/* INITIALIZATION phase */
IF user_env[9] = "" THEN DO:
  user_path = "*T,9=pre,_usrtdel,1=s,_usrtget,"
            + "9=mid,_usrtdel,*C,9=end,_usrtdel".
  RETURN.
END.

/*--------------------------------------------------------------------------*/
/* PRE phase */

IF user_env[9] = "pre" THEN _pre: DO:
  IF dict_rog THEN
    msg-txt = "The dictionary is in read-only mode - alterations not allowed.".
  ELSE
  DO FOR _File:
    FIND _File "_File".
    IF NOT CAN-DO(_Can-delete,USERID("DICTDB")) THEN msg-txt =
      "You do not have permission to delete table definitions.".
  END.
  IF msg-txt <> ? THEN LEAVE _pre.

  ASSIGN
    cache_dirty   = FALSE
    cache_file#   = 1
    cache_file    = ""
    cache_file[1] = "ALL".
  FOR EACH _File WHERE _Db-recid = drec_db AND NOT _Hidden
    BY _File._File-name:
    answer = FALSE.
    IF _Frozen
      OR CAN-FIND(FIRST _View-ref WHERE _View-ref._Ref-Table = _File._File-name)
      OR _Db-lang <> 0 THEN answer = TRUE.
    ELSE
    IF user_dbtype = "PROGRESS" THEN DO:
      FIND _Index WHERE RECID(_Index) = _File._Prime-Index NO-ERROR.
      IF AVAILABLE _Index AND NOT _Index._Active THEN skipped = TRUE.
    END.
    IF answer THEN DO:
      skipped = TRUE.
      NEXT.
    END.
    ASSIGN
      cache_file# = cache_file# + 1
      cache_file[cache_file#] = _File._File-name.
  END.
  IF cache_file# = 1 THEN cache_file# = 0.
  IF skipped THEN DO:
    MESSAGE "Note: SQL tables, Frozen tables, and tables with".
    MESSAGE "      no active indexes cannot be deleted here.".
  END.
  RETURN.
END.

/*--------------------------------------------------------------------------*/
/* MID phase */

IF user_env[9] = "mid" THEN _mid: DO:

  DO FOR _File i = 1 TO NUM-ENTRIES(user_env[1]) WHILE msg-txt = ?:
    FIND _File
      WHERE _File._Db-recid = drec_db
        AND _File._File-name = ENTRY(i,user_env[1]).
  END.
  IF msg-txt <> ? THEN LEAVE _mid.

  answer = FALSE.
  RUN "prodict/user/_usrdbox.p" (INPUT-OUTPUT answer,?,?,
    IF NUM-ENTRIES(user_env[1]) > 1
      THEN 'Are you sure that you want to remove the selected tables? "'
        + (IF LENGTH(user_env[1]) > 100
          THEN SUBSTRING(user_env[1],1,100) + "..." ELSE user_env[1]) + '".'
      ELSE 'Are you sure that you want to remove table "' + user_env[1] + '"?').

  
  IF NOT answer OR answer = ? THEN DO:
    MESSAGE "Nothing was deleted." view-as alert-box.
    user_path = "".
    LEAVE _mid.
  END.

  _killer:
  DO FOR _File TRANSACTION:
    DO i = 1 TO NUM-ENTRIES(user_env[1]): /* beginning delete... */
      FIND _File WHERE _File._Db-recid = drec_db
        AND _File._File-name = ENTRY(i,user_env[1]).
      HIDE MESSAGE NO-PAUSE.
      MESSAGE 'Removing the definition for "' + _File._File-name + '"...'.
      DO ON ERROR UNDO,LEAVE:
        { prodict/dump/loadkill.i }
      END.
      IF AVAILABLE _File THEN DO:
        MESSAGE 'The table "' + _File._File-name + '" was not deleted.'
                VIEW-AS ALERT-BOX ERROR BUTTONS OK.
        msg-txt= "UNDO performed - Delete operation ABORTED.".
        UNDO _killer,LEAVE _killer.
      END.
    END.
  END.

  IF msg-txt = ? THEN DO:
    HIDE MESSAGE NO-PAUSE.
    /* now reclaiming database space */
    MESSAGE (IF user_dbtype = "PROGRESS"
      THEN "Making database table space available for re-use"
      ELSE "Saving changes") + "...".
  END.
  ASSIGN
    cache_dirty = TRUE
    drec_file   = ?.

END.
/*--------------------------------------------------------------------------*/
/* END phase */

IF user_env[9] = "end" THEN _end: DO:
  MESSAGE "All tables marked for deletion have been removed.".
  ASSIGN
    user_filename = ""
    user_env[9]   = "".
  DISPLAY "" @ user_filename WITH FRAME user_ftr.
  RETURN.
END.

/*--------------------------------------------------------------------------*/

IF msg-txt <> ? THEN DO:
  MESSAGE msg-txt VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  user_path = "".
  RETURN.
END.

RETURN.


