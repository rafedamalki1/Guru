/*************************************************************/
/* Copyright (c) 1984-1998 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/* _usrsdel.p - delete database 

    Modified 06/03/98 DLM Removed the check to see if any schema present
                          and will just ask if user is sure.
  
*/

{ prodict/dictvar.i }
{ prodict/user/uservar.i }

&Global-define SKP &IF "{&WINDOW-SYSTEM}" = "OSF/Motif" &THEN SKIP &ELSE &ENDIF

DEFINE VARIABLE answer  AS LOGICAL INITIAL FALSE NO-UNDO.
DEFINE VARIABLE edbtyp  AS CHARACTER             NO-UNDO.
DEFINE VARIABLE msg-num AS INTEGER INITIAL 0     NO-UNDO.
DEFINE VARIABLE scrap   AS CHARACTER             NO-UNDO.

/* LANGUAGE DEPENDENCIES START */ /*----------------------------------------*/
DEFINE VARIABLE new_lang AS CHARACTER EXTENT 12 NO-UNDO INITIAL [
  /*  1*/ "You cannot delete a PROGRESS database through this program.",
  /*  2*/ "This is only for removing non-PROGRESS data definitions from the schema holder.",
  /*  3*/ "You cannot remove non-PROGRESS data definitions from a schema holder that still",
  /*  4*/ "contains table definitions.  Remove the table definitions first.",
  /*  5*/ "You do not have permission to delete data definitions from the schema holder definitions.",
  /*6,7*/ "Data definitions for database", "removed.",
  /*8,9*/ "Data definitions for database", "NOT removed.",
  /* 10*/ "The dictionary is in read-only mode - alterations not allowed.",
  /* 11*/ "Are you sure you want to remove the data definitions for the", 
  /* 12*/           "database called"
].
/* LANGUAGE DEPENDENCIES END */ /*------------------------------------------*/

assign
  edbtyp = { adecomm/ds_type.i
             &direction = "itoe"
             &from-type = "user_dbtype"
             }.

DO FOR DICTDB._File:
  FIND _File "_Db".
  IF NOT CAN-DO(_Can-delete,USERID("DICTDB")) THEN msg-num = 5.
  ELSE IF dict_rog THEN msg-num = 10. /* look but don't touch */
  IF msg-num > 0 THEN DO:
    MESSAGE new_lang[msg-num] VIEW-AS ALERT-BOX ERROR BUTTONS OK.
    user_path = "".
    RETURN.
  END.
END.

IF user_dbtype = "PROGRESS" THEN DO:
  MESSAGE new_lang[1] {&SKP} /* can't delete progress db */
      	  new_lang[2]
      	  VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  user_path = "".
  RETURN.
END.

DO:
  answer = FALSE.
  MESSAGE new_lang[11] SKIP /* r-u-sure */
      	  edbtyp + " " + new_lang[12] + ' "' + user_dbname + '"?' 
      	  VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE answer.
  IF answer THEN DO:
    RUN "adecomm/_setcurs.p" ("WAIT").
    IF CONNECTED(user_dbname) THEN DISCONNECT VALUE(user_dbname).
    DO TRANSACTION:
      FIND _Db WHERE RECID(_Db) = drec_db.
      FOR EACH _File OF _Db:
        { prodict/dump/loadkill.i }
      END.
      FOR EACH _SEQUENCE OF _Db:
        DELETE _Sequence.
      END.
      DELETE _Db.
    END.
    RUN "adecomm/_setcurs.p" ("").
    MESSAGE
      new_lang[6] '"' + user_dbname + '"' new_lang[7]
      VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
    ASSIGN
      drec_db     = ?
      user_dbname = ""
      cache_dirty = TRUE.
  END.
  ELSE DO:
     user_path = "".  /* If not deleted, don't ask user to select a db */
     MESSAGE
        new_lang[8] '"' + user_dbname + '"' new_lang[9]
      	VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
  END.
END.

RETURN.

