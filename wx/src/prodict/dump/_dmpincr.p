/*************************************************************/
/* Copyright (c) 1984-1998 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/* _dmpincr.p - phase 2 of incremental .df maker 

   DICTDB  is the current database
   DICTDB2 is the database chosen to compare against

   The aim is to produce a database like DICTDB.  So this .df file will be
   run against a database like DICTDB2 to create a database like DICTDB.
*/

/*
DICTDB  = new definitions
DICTDB2 = old definitions

for each file:
  match up filename
  match up indexnames
  match up fieldnames
  handle differences
end.

match up:
  find object of same name.
  if found, leave.
  otherwise, make note and continue until all matched.
  if none left over, assume deletes.
  otherwise, ask if renamed.
  return.
end.
*/
/*
in:       user_env[2] = Name of file to dump to.
          user_env[5] = "<internal defaults apply>" or "<target-code-page>"
    
changes:  user_env[19]

History:
    gfs         12/05/94    fixed problem with drop field if in pri indx
    hutegger    94/02/24    code-page - support and trailer-info added
    laurief     06/02/98    Added database names to screen display and 
                            combined the two separate frames into one that can
                            be viewed in three-d.

*/
/*h-*/

{ prodict/dictvar.i }
{ prodict/user/uservar.i }
{ prodict/user/userhue.i }

DEFINE VARIABLE ans        AS LOGICAL   INITIAL FALSE NO-UNDO.
DEFINE VARIABLE c        AS CHARACTER               NO-UNDO.
DEFINE VARIABLE db        AS CHARACTER               NO-UNDO.
DEFINE VARIABLE db2        AS CHARACTER               NO-UNDO.
DEFINE VARIABLE fil        AS CHARACTER               NO-UNDO.
DEFINE VARIABLE fil2        AS CHARACTER               NO-UNDO.
DEFINE VARIABLE fld        AS CHARACTER               NO-UNDO.
DEFINE VARIABLE fld2        AS CHARACTER               NO-UNDO.
DEFINE VARIABLE idx        AS CHARACTER               NO-UNDO.
DEFINE VARIABLE idx2        AS CHARACTER               NO-UNDO.
DEFINE VARIABLE seq        AS CHARACTER               NO-UNDO.
DEFINE VARIABLE seq2        AS CHARACTER               NO-UNDO.
DEFINE VARIABLE i        AS INTEGER                 NO-UNDO.
DEFINE VARIABLE j        AS INTEGER                 NO-UNDO.
DEFINE VARIABLE l        AS LOGICAL                 NO-UNDO.
DEFINE VARIABLE stopped AS LOGICAL   INITIAL TRUE  NO-UNDO.
DEFINE VARIABLE inpri   AS LOGICAL                 NO-UNDO.
DEFINE VARIABLE tmp_Field-name AS CHARACTER        NO-UNDO.

/* LANGUAGE DEPENDENCIES START */ /*----------------------------------------*/
DEFINE VARIABLE new_lang AS CHARACTER EXTENT 2 NO-UNDO INITIAL [
  /* 1*/ "(initializing)",
  /* 2*/ ? /* see below */
].

new_lang[2] = "The incremental definitions file will contain at least "
            + "one new definition of an unique index. "
            + "If PROGRESS finds duplicate values while creating the new "
            + "unique index, it will UNDO the entire transaction, causing "
            + "you to lose any other schema changes made.  Creating an "
            + "inactive index and then building it with ~"proutil -C "
            + "idxbuild~" will eliminate this problem.  Do you want to "
            + "create the definition for this index as inactive?".

FORM
  db      LABEL "Scanning"              COLON 10 FORMAT "x(27)"
  db2     LABEL "Working on"            COLON 50 FORMAT "x(27)" SKIP
  fil     LABEL "TABLE"                 COLON 10 FORMAT "x(27)"
  fil2    LABEL "TABLE"                     COLON 50 FORMAT "x(27)" SKIP
  fld     LABEL "FIELD"                    COLON 10 FORMAT "x(27)"
  fld2    LABEL "FIELD"                    COLON 50 FORMAT "x(27)" SKIP
  idx     LABEL "INDEX"                    COLON 10 FORMAT "x(27)"
  idx2    LABEL "INDEX"                    COLON 50 FORMAT "x(27)" SKIP
  seq     LABEL "SEQ"                   COLON 10 FORMAT "x(27)"
  seq2    LABEL "SEQ"                   COLON 50 FORMAT "x(27)" SKIP
  HEADER 
    " Press " +
    KBLABEL("STOP") + " to terminate the dump" FORMAT "x(50)" 
  WITH FRAME seeking 
  &IF "{&WINDOW-SYSTEM}" <> "TTY" &THEN VIEW-AS DIALOG-BOX THREE-D &ENDIF
  SIDE-LABELS WIDTH 80
  ROW 4 COLUMN 1 USE-TEXT.
COLOR DISPLAY MESSAGES fil fil2 fld fld2 idx idx2 seq seq2 WITH FRAME seeking.

/* LANGUAGE DEPENDENCIES END */ /*------------------------------------------*/

/* Definitions */ /*-------------------------------------------------------*/

DEFINE VARIABLE ddl  AS CHARACTER EXTENT 35 NO-UNDO.
DEFINE VARIABLE iact AS LOGICAL   INITIAL ? NO-UNDO.
DEFINE VARIABLE pri1 AS CHARACTER           NO-UNDO.
DEFINE VARIABLE pri2 AS CHARACTER           NO-UNDO.
DEFINE VARIABLE cnt  AS INTEGER             NO-UNDO.

DEFINE BUFFER database2 FOR DICTDB2._Db.
FIND FIRST database2 WHERE database2._Db-local.

DEFINE NEW SHARED STREAM ddl.

DEFINE WORKFILE missing NO-UNDO
  FIELD name AS CHARACTER INITIAL "".

DEFINE NEW SHARED WORKFILE table-list NO-UNDO
  FIELD t1-name AS CHARACTER INITIAL ""
  FIELD t2-name AS CHARACTER INITIAL ?.

DEFINE NEW SHARED WORKFILE field-list NO-UNDO
  FIELD f1-name AS CHARACTER INITIAL ""
  FIELD f2-name AS CHARACTER INITIAL ?.

DEFINE NEW SHARED WORKFILE seq-list NO-UNDO
  FIELD s1-name AS CHARACTER INITIAL ""
  FIELD s2-name AS CHARACTER INITIAL ?.

DEFINE WORKFILE index-list NO-UNDO
  FIELD i1-name AS CHARACTER INITIAL ""
  FIELD i1-comp AS CHARACTER INITIAL ""
  FIELD i2-name AS CHARACTER INITIAL ?
  FIELD i1-i2   AS LOGICAL.

DEFINE BUFFER index-alt FOR index-list.

DEFINE BUFFER old-field FOR DICTDB._Field.
DEFINE BUFFER new-field FOR DICTDB2._Field.

/* Internal Procedures */ /*------------------------------------------------*/

/* We don't delete indexes first because Progress doesn't let you delete
   the last index.  So if we are about to rename an index or add a new
   one, see if an index with this name is in the list to be deleted.
   If so, rename that one so we don't get a name conflict.  It will 
   be deleted later.
*/
PROCEDURE Check_Index_Conflict:
   DEFINE VAR tempname AS CHAR INITIAL "temp-we48z576-". /* meaningless goop */

   FIND FIRST index-alt WHERE NOT index-alt.i1-i2 AND /* to be deleted */
                    index-alt.i1-name = index-list.i1-name NO-ERROR. 
   IF AVAILABLE index-alt THEN DO:
      cnt = cnt + 1.
      tempname = tempname + STRING(cnt).
      PUT STREAM ddl UNFORMATTED
        'RENAME INDEX "' index-alt.i1-name
        '" TO "' tempname
        '" ON "' DICTDB._File._File-name '"' SKIP(1).
      index-alt.i1-name = tempname.
   END.
END.

PROCEDURE tmp-name:
   /* This procedure takes a field name and renames it to a unique
    * name so it can be deleted later. This is done in the instance
    * when a field has changed data-type or extent and is part of a
    * primary index. Since the index will not be deleted until later
    * on in the code. We will rename it and delete it later
    */
   DEFINE INPUT  PARAMETER fname   AS CHAR NO-UNDO.
   DEFINE OUTPUT PARAMETER newname AS CHAR NO-UNDO.

   DEFINE VARIABLE s AS INT NO-UNDO.

   DO s = 1 to 99:
     newname = "Z_" + substring(fname,1,28) + string(s,"99").
     FIND FIRST DICTDB.old-field WHERE DICTDB.old-field._Field-name = newname
          NO-ERROR.
     IF NOT AVAILABLE(DICTDB.old-field) THEN DO:
       FIND FIRST DICTDB2.new-field WHERE 
           DICTDB2.new-field._Field-name = newname
           NO-ERROR.
       IF NOT AVAILABLE(DICTDB2.new-field) THEN DO:
         FIND FIRST missing WHERE missing.name = newname NO-ERROR.
         IF NOT AVAILABLE(missing) THEN LEAVE. /* got it! */
       END.
     END.
   END. 
END PROCEDURE.   

PROCEDURE inprimary:
   /* Determines whether a field is part of a primary index */

   DEFINE INPUT PARAMETER rfield AS RECID          NO-UNDO. /*recid of _Field*/
   DEFINE OUTPUT PARAMETER prime AS LOG INITIAL no NO-UNDO. /*pri? y/n */
   
   FOR EACH DICTDB._index-field WHERE DICTDB._index-field._field-recid = rfield:
     FIND DICTDB._file WHERE recid(DICTDB._file) = 
         DICTDB._field._file-recid NO-ERROR.
     IF DICTDB._file._prime-index = DICTDB._index-field._index-recid THEN DO:
        prime = yes.
        LEAVE.
     END.
  END.
END PROCEDURE.

/* mainline code **********************************************************/

IF  user_env[5] = "" 
 OR user_env[5] = ?  THEN assign user_env[5] = "<internal defaults apply>". 

IF  user_env[5] = "<internal defaults apply>" 
 THEN OUTPUT STREAM ddl TO VALUE(user_env[2]) NO-ECHO NO-MAP
            NO-CONVERT.
 ELSE OUTPUT STREAM ddl TO VALUE(user_env[2]) NO-ECHO NO-MAP
            CONVERT SOURCE SESSION:CHARSET TARGET user_env[5].

SESSION:IMMEDIATE-DISPLAY = yes.
/* Display database name at the top of the respective column. */
FIND FIRST DICTDB._Db WHERE RECID(DICTDB._Db) = drec_db.
DISPLAY LDBNAME("DICTDB") @ db WITH FRAME seeking.
FIND FIRST DICTDB2._Db WHERE RECID(DICTDB2._Db) = RECID(database2).
DISPLAY LDBNAME("DICTDB2") @ db2 WITH FRAME seeking.

DISPLAY new_lang[1] @ fil  WITH FRAME seeking. /* initializing */
DISPLAY new_lang[1] @ fil2 WITH FRAME seeking. /* initializing */
run adecomm/_setcurs.p ("WAIT").

DO ON STOP UNDO, LEAVE:
  /* build missing file list for rename/delete determination */

  FOR EACH DICTDB2._File
    WHERE DICTDB2._File._Db-recid = RECID(database2)
      AND NOT DICTDB2._File._Hidden:
    FIND FIRST DICTDB._File
      WHERE DICTDB._File._Db-recid = drec_db
        AND DICTDB._File._File-name = DICTDB2._File._File-name
        AND NOT DICTDB._File._Hidden NO-ERROR.
    DISPLAY DICTDB2._File._File-name @ fil WITH FRAME seeking.
    IF AVAILABLE DICTDB._File THEN NEXT.
    CREATE missing.
    missing.name = DICTDB2._File._File-name.
  END.
  
  /* build list of new or renamed files */
  FOR EACH DICTDB._File
    WHERE DICTDB._File._Db-recid = drec_db
      AND NOT DICTDB._File._Hidden:
    FIND FIRST DICTDB2._File
      WHERE DICTDB2._File._Db-recid = RECID(database2)
        AND DICTDB2._File._File-name = DICTDB._File._File-name NO-ERROR.
    DISPLAY DICTDB._File._File-name @ fil WITH FRAME seeking.
    CREATE table-list.
    table-list.t1-name = DICTDB._File._File-name.
    IF AVAILABLE DICTDB2._File THEN
      table-list.t2-name = DICTDB._File._File-name.
  END.
  
  /* look for matches for renamed files with user input.  A prompt 
     is given for each file in DICTDB2 that's not in DICTDB but only when
     there is also a file in DICTDB that's not in DICTDB2.  The 2nd list
     is the potential values to rename to.
  */
  run adecomm/_setcurs.p ("").  /* while dmpisub is running */
  FOR EACH missing:
    DISPLAY missing.name @ fil WITH FRAME seeking.
    RUN "prodict/dump/_dmpisub.p"
      (INPUT "t",INPUT-OUTPUT missing.name,OUTPUT ans).
    IF missing.name = ? THEN DELETE missing.
    IF ans = ? THEN DO:
      HIDE FRAME seeking NO-PAUSE.
      user_path = "".
      RETURN.
    END.
  END.
  run adecomm/_setcurs.p ("WAIT").
  
  /* handle deleted files */
  ans = FALSE.
  FOR EACH missing:
    ans = TRUE.
    PUT STREAM ddl UNFORMATTED
      'DROP TABLE "' missing.name '"' SKIP.
    DISPLAY missing.name @ fil WITH FRAME seeking.
    DISPLAY missing.name @ fil2 WITH FRAME seeking.
    DELETE missing.
  END.
  IF ans THEN PUT STREAM ddl UNFORMATTED SKIP(1).
  
  /* handle renamed files */
  ans = FALSE.
  FOR EACH table-list
    WHERE table-list.t1-name <> table-list.t2-name
      AND table-list.t2-name <> ?:
    ans = TRUE.
    PUT STREAM ddl UNFORMATTED
      'RENAME TABLE "' table-list.t2-name
      '" TO "' table-list.t1-name '"' SKIP.
    DISPLAY table-list.t1-name @ fil WITH FRAME seeking.
    DISPLAY table-list.t1-name @ fil2 WITH FRAME seeking.
  END.
  IF ans THEN PUT STREAM ddl UNFORMATTED SKIP(1).
  
  /* dump newly created files */
  FOR EACH table-list WHERE table-list.t2-name = ?:
    FIND DICTDB._File WHERE DICTDB._File._Db-recid = drec_db AND
         DICTDB._File._File-name = table-list.t1-name.
    DISPLAY DICTDB._File._File-name @ fil WITH FRAME seeking.
    RUN "prodict/dump/_dmpdefs.p" ("t",RECID(DICTDB._File)).
    DISPLAY DICTDB._File._File-name @ fil2 WITH FRAME seeking.
    DELETE table-list.
  END.
  
  /* handle potentially altered files */
  FOR EACH table-list:
    DISPLAY table-list.t1-name @ fil "" @ fld "" @ idx WITH FRAME seeking.
    DISPLAY table-list.t1-name @ fil2 "" @ fld2 "" @ idx2 WITH FRAME seeking.
    FIND DICTDB._File WHERE DICTDB._File._Db-recid = drec_db
      AND DICTDB._File._File-name = table-list.t1-name.
    FIND DICTDB2._File WHERE DICTDB2._File._Db-recid = RECID(database2)
      AND DICTDB2._File._File-name = table-list.t2-name.
  
    /* clean out working storage */
    FOR EACH field-list:
      DELETE field-list.
    END.
    FOR EACH index-list:
      DELETE index-list.
    END.
  
    /* write out appropriate file definition changes */
    ASSIGN
      j      = 1
      ddl    = ""
      ddl[1] = 'UPDATE TABLE "' + DICTDB._File._File-name + '"'.
    RUN "prodict/_dctquot.p" (DICTDB._File._Can-Read,'"',OUTPUT c).
    IF DICTDB._File._Can-read <> DICTDB2._File._Can-read THEN ASSIGN
      j = j + 1
      ddl[j] = "  CAN-READ " + c.
    RUN "prodict/_dctquot.p" (DICTDB._File._Can-Write,'"',OUTPUT c).
    IF DICTDB._File._Can-write <> DICTDB2._File._Can-write THEN ASSIGN
      j = j + 1
      ddl[j] = "  CAN-WRITE " + c.
    RUN "prodict/_dctquot.p" (DICTDB._File._Can-Create,'"',OUTPUT c).
    IF DICTDB._File._Can-create <> DICTDB2._File._Can-create THEN ASSIGN
      j = j + 1
      ddl[j] = "  CAN-CREATE " + c.
    RUN "prodict/_dctquot.p" (DICTDB._File._Can-Delete,'"',OUTPUT c).
    IF DICTDB._File._Can-delete <> DICTDB2._File._Can-delete THEN ASSIGN
      j = j + 1
      ddl[j] = "  CAN-DELETE " + c.
    RUN "prodict/_dctquot.p" (DICTDB._File._Can-Dump,'"',OUTPUT c).
    IF DICTDB._File._Can-Dump <> DICTDB2._File._Can-Dump THEN ASSIGN
      j = j + 1
      ddl[j] = "  CAN-DUMP " + c.
    RUN "prodict/_dctquot.p" (DICTDB._File._Can-Load,'"',OUTPUT c).
    IF DICTDB._File._Can-Load <> DICTDB2._File._Can-Load THEN ASSIGN
      j = j + 1
      ddl[j] = "  CAN-LOAD " + c.
    RUN "prodict/_dctquot.p" (DICTDB._File._Desc,'"',OUTPUT c).
    IF DICTDB._File._Desc <> DICTDB2._File._Desc THEN ASSIGN
      j = j + 1
      ddl[j] = "  DESCRIPTION " + c.
    RUN "prodict/_dctquot.p" (DICTDB._File._File-label,'"',OUTPUT c).
    IF DICTDB._File._File-label <> DICTDB2._File._File-label THEN ASSIGN
      j = j + 1
      ddl[j] = "  LABEL " + c.
    RUN "prodict/_dctquot.p" (DICTDB._File._File-label-SA,'"',OUTPUT c).
    IF DICTDB._File._File-label-SA <> DICTDB2._File._File-label-SA THEN ASSIGN
      j = j + 1
      ddl[j] = "  LABEL-SA " + c.
    RUN "prodict/_dctquot.p" (DICTDB._File._Valexp,'"',OUTPUT c).
    IF DICTDB._File._Valexp <> DICTDB2._File._Valexp THEN ASSIGN
      j = j + 1
      ddl[j] = "  VALEXP " + c.
    RUN "prodict/_dctquot.p" (DICTDB._File._Valmsg,'"',OUTPUT c).
    IF DICTDB._File._Valmsg <> DICTDB2._File._Valmsg THEN ASSIGN
      j = j + 1
      ddl[j] = "  VALMSG " + c.
    RUN "prodict/_dctquot.p" (DICTDB._File._Valmsg-SA,'"',OUTPUT c).
    IF DICTDB._File._Valmsg-SA <> DICTDB2._File._Valmsg-SA THEN ASSIGN
      j = j + 1
      ddl[j] = "  VALMSG-SA " + c.
    RUN "prodict/_dctquot.p" (DICTDB._File._Dump-name,'"',OUTPUT c).
    IF DICTDB._File._Dump-name <> DICTDB2._File._Dump-name THEN ASSIGN
      j = j + 1
      ddl[j] = "  DUMP-NAME " + c.
    RUN "prodict/_dctquot.p" (DICTDB._File._Fil-misc2[6],'"',OUTPUT c).
    IF DICTDB._File._Fil-misc2[6] <> DICTDB2._File._Fil-misc2[6] THEN ASSIGN
      j = j + 1
      ddl[j] = "  FILE-MISC26 " + c.
    IF DICTDB._File._Frozen THEN ASSIGN
      j = j + 1
      ddl[j] = "  FROZEN".
  
    /* deal with file triggers */
    /* 1st, find ones to be deleted */
    FOR EACH DICTDB2._File-trig OF DICTDB2._File:
      FIND DICTDB._File-trig OF DICTDB._File
        WHERE DICTDB._File-trig._Event = DICTDB2._File-trig._Event NO-ERROR.
      IF NOT AVAILABLE DICTDB._File-trig THEN DO:
        RUN "prodict/_dctquot.p" (DICTDB2._File-trig._Event,'"',OUTPUT c).
        j = j + 1.
        ddl[j] = "  TABLE-TRIGGER " + c + " DELETE".
      END.
    END.
    /* now record updated or new ones */
    FOR EACH DICTDB._File-trig OF DICTDB._File:
      FIND DICTDB2._File-trig OF DICTDB2._File 
        WHERE DICTDB2._File-trig._Event = DICTDB._File-trig._Event NO-ERROR.
      IF AVAILABLE DICTDB2._File-trig AND 
        DICTDB2._File-trig._Override = DICTDB._File-trig._Override AND
        DICTDB2._File-trig._Proc-name = DICTDB._File-trig._Proc-name AND
        DICTDB2._File-trig._Trig-CRC = DICTDB._File-trig._Trig-CRC THEN
        NEXT.
        
      RUN "prodict/_dctquot.p" (DICTDB._File-trig._Event,'"',OUTPUT c).
      j = j + 1.
      ddl[j] = "  TABLE-TRIGGER " + c +
               (IF DICTDB._File-trig._Override THEN " OVERRIDE " 
                                               ELSE " NO-OVERRIDE ").
      RUN "prodict/_dctquot.p" (DICTDB._File-trig._Proc-name,'"',OUTPUT c).
      ddl[j] = ddl[j] + "PROCEDURE " + c + " CRC """ 
               + (IF DICTDB._File-trig._Trig-CRC = ? 
                  THEN "?" ELSE STRING(DICTDB._File-trig._Trig-CRC))
               + """".
    END.
  
    /* don't write out ddl[1] if j = 1 (i.e., we only have table header) */
    IF j > 1 THEN 
      DO i = 1 TO j + 1:
        IF ddl[i] = "" THEN  /* this puts an extra skip after the last one */
          PUT STREAM ddl UNFORMATTED SKIP(1).
        ELSE
          PUT STREAM ddl UNFORMATTED ddl[i] SKIP.
      END.
  
    /* build missing field list for rename/delete determination */
    FOR EACH DICTDB2._Field OF DICTDB2._File BY DICTDB2._Field._field-rpos:
      FIND FIRST DICTDB._Field OF DICTDB._File
        WHERE DICTDB._Field._Field-name = DICTDB2._Field._Field-name NO-ERROR.
      DISPLAY DICTDB2._Field._Field-name @ fld WITH FRAME seeking.
      IF AVAILABLE DICTDB._Field THEN NEXT.
      CREATE missing.
      missing.name = DICTDB2._Field._Field-name.
    END.
  
    /* build field rename list */
    FOR EACH DICTDB._Field OF DICTDB._File BY DICTDB._Field._field-rpos:
      FIND FIRST DICTDB2._Field OF DICTDB2._File
        WHERE DICTDB2._Field._Field-name = DICTDB._Field._Field-name NO-ERROR.
      DISPLAY DICTDB._Field._Field-name @ fld WITH FRAME seeking.
      CREATE field-list.
      field-list.f1-name = DICTDB._Field._Field-name.
      IF AVAILABLE DICTDB2._Field THEN
        field-list.f2-name = DICTDB._Field._Field-name.
    END.
  
    /* look for matches for renamed fields with user input.  A prompt 
       is given for each field in DICTDB2 that's not in DICTDB but only when
       there is also a field in DICTDB that's not in DICTDB2.  The 2nd list
       is the potential values to rename to.
    */
    run adecomm/_setcurs.p ("").
    user_env[19] = DICTDB._File._File-name. /* this is a hack */
    FOR EACH missing:
      DISPLAY missing.name @ fld WITH FRAME seeking.
      RUN "prodict/dump/_dmpisub.p"
        (INPUT "f",INPUT-OUTPUT missing.name,OUTPUT ans).
      IF missing.name = ? THEN DELETE missing.
      IF ans = ? THEN DO:
        HIDE FRAME seeking NO-PAUSE.
        user_path = "".
        RETURN.
      END.
    END.
    run adecomm/_setcurs.p ("WAIT").
  
    /* We use to handle deleted fields here but now it's done after
       index stuff.  See below.
     */
  
    /* handle renamed fields */
    ans = FALSE.
    FOR EACH field-list
      WHERE field-list.f1-name <> field-list.f2-name
        AND field-list.f2-name <> ?:
      ans = TRUE.
      DISPLAY field-list.f1-name @ fld2 WITH FRAME seeking.
      PUT STREAM ddl UNFORMATTED
        'RENAME FIELD "' field-list.f2-name
        '" OF "' DICTDB._File._File-name
        '" TO "' field-list.f1-name '"' SKIP.
    END.
    IF ans THEN PUT STREAM ddl UNFORMATTED SKIP(1).
  
    /* handle new or potentially altered fields */
    FOR EACH field-list:
      FIND FIRST DICTDB._Field OF DICTDB._File
        WHERE DICTDB._Field._Field-name = field-list.f1-name.
      FIND FIRST DICTDB2._Field OF DICTDB2._File
        WHERE DICTDB2._Field._Field-name = field-list.f2-name NO-ERROR.
      DISPLAY field-list.f1-name @ fld2 WITH FRAME seeking.
      l = AVAILABLE DICTDB2._Field.
      IF l AND (DICTDB._Field._Data-type <> DICTDB2._Field._Data-type
        OR      DICTDB._Field._Extent    <> DICTDB2._Field._Extent) THEN DO:
        /* If field is part of a primary index, we cannot simply drop it.
         * instead, we will rename it to something else, and delete it
         * later, after the indexes are processed.
         */
        RUN inprimary (INPUT RECID(DICTDB._Field), OUTPUT inpri).
        IF inpri THEN DO: /* field is part of primary index, don't DROP*/
          RUN tmp-name (INPUT DICTDB._Field._Field-name, OUTPUT tmp_Field-name).
          PUT STREAM ddl UNFORMATTED
            'RENAME FIELD "' DICTDB._Field._Field-name
            '" OF "' DICTDB._File._File-name
            '" TO "' tmp_Field-name '"' SKIP.
          CREATE missing. 
          ASSIGN missing.name = tmp_Field-name. /*record name to 'DROP' later*/
        END.
        ELSE /* is not in a primary index, we can DROP it now */
          PUT STREAM ddl UNFORMATTED
            'DROP FIELD "' DICTDB._Field._Field-name
            '" OF "' DICTDB._File._File-name '"' SKIP.
        RELEASE DICTDB2._Field.
        l = FALSE.
      END.
  
      /* If l is true we're updateing otherwise we're adding */
      ASSIGN
        ddl    = ""
        ddl[1] = (IF l THEN "UPDATE" ELSE "ADD")
               + ' FIELD "' + DICTDB._Field._Field-name
               + '" OF "' + DICTDB._File._File-name + '"'
               + (IF l THEN "" ELSE " AS " + DICTDB._Field._Data-type).
      RUN "prodict/_dctquot.p" (DICTDB._Field._Desc,'"',OUTPUT c).
      IF NOT l OR DICTDB._Field._Desc <> DICTDB2._Field._Desc THEN 
        ddl[2] = "  DESCRIPTION " + c.
      RUN "prodict/_dctquot.p" (DICTDB._Field._Format,'"',OUTPUT c).
      IF NOT l OR DICTDB._Field._Format <> DICTDB2._Field._Format THEN 
        ddl[3] = "  FORMAT " + c.
      RUN "prodict/_dctquot.p" (DICTDB._Field._Format-SA,'"',OUTPUT c).
      IF NOT l OR DICTDB._Field._Format-SA <> DICTDB2._Field._Format-SA THEN
        ddl[4] = "  FORMAT-SA " + c.
      RUN "prodict/_dctquot.p" (DICTDB._Field._Initial,'"',OUTPUT c).
      IF NOT l OR DICTDB._Field._Initial <> DICTDB2._Field._Initial THEN
        ddl[5] = "  INITIAL " + c.
      RUN "prodict/_dctquot.p" (DICTDB._Field._Initial-SA,'"',OUTPUT c).
      IF NOT l OR DICTDB._Field._Initial-SA <> DICTDB2._Field._Initial-SA THEN
        ddl[6] = "  INITIAL-SA " + c.
      RUN "prodict/_dctquot.p" (DICTDB._Field._Help,'"',OUTPUT c).
      IF NOT l OR DICTDB._Field._Help <> DICTDB2._Field._Help THEN
        ddl[7] = "  HELP " + c.
      RUN "prodict/_dctquot.p" (DICTDB._Field._Help-SA,'"',OUTPUT c).
      IF NOT l OR DICTDB._Field._Help-SA <> DICTDB2._Field._Help-SA THEN
        ddl[8] = "  HELP-SA " + c.
      RUN "prodict/_dctquot.p" (DICTDB._Field._Label,'"',OUTPUT c).
      IF NOT l OR DICTDB._Field._Label <> DICTDB2._Field._Label THEN
        ddl[9] = "  LABEL " + c.
      RUN "prodict/_dctquot.p" (DICTDB._Field._Label-SA,'"',OUTPUT c).
      IF NOT l OR DICTDB._Field._Label-SA <> DICTDB2._Field._Label-SA THEN
        ddl[10] = "  LABEL-SA " + c.
      RUN "prodict/_dctquot.p" (DICTDB._Field._Col-label,'"',OUTPUT c).
      IF NOT l OR DICTDB._Field._Col-label <> DICTDB2._Field._Col-label THEN
        ddl[11] = "  COLUMN-LABEL " + c.
      RUN "prodict/_dctquot.p" (DICTDB._Field._Col-label-SA,'"',OUTPUT c).
      IF NOT l OR 
               DICTDB._Field._Col-label-SA <> DICTDB2._Field._Col-label-SA THEN
        ddl[12] = "  COLUMN-LABEL-SA " + c.
      RUN "prodict/_dctquot.p" (DICTDB._Field._Can-Read,'"',OUTPUT c).
      IF NOT l OR DICTDB._Field._Can-read <> DICTDB2._Field._Can-read THEN
        ddl[13] = "  CAN-READ " + c.
      RUN "prodict/_dctquot.p" (DICTDB._Field._Can-Write,'"',OUTPUT c).
      IF NOT l OR DICTDB._Field._Can-write <> DICTDB2._Field._Can-write THEN
        ddl[14] = "  CAN-WRITE " + c.
      RUN "prodict/_dctquot.p" (DICTDB._Field._Valexp,'"',OUTPUT c).
      IF NOT l OR DICTDB._Field._Valexp <> DICTDB2._Field._Valexp THEN
        ddl[15] = "  VALEXP " + c.
      RUN "prodict/_dctquot.p" (DICTDB._Field._Valmsg,'"',OUTPUT c).
      IF NOT l OR DICTDB._Field._Valmsg <> DICTDB2._Field._Valmsg THEN
        ddl[16] = "  VALMSG " + c.
      RUN "prodict/_dctquot.p" (DICTDB._Field._Valmsg-SA,'"',OUTPUT c).
      IF NOT l OR DICTDB._Field._Valmsg-SA <> DICTDB2._Field._Valmsg-SA THEN
        ddl[17] = "  VALMSG-SA " + c.
      RUN "prodict/_dctquot.p" (DICTDB._Field._View-as,'"',OUTPUT c).
      IF NOT l OR DICTDB._Field._View-as <> DICTDB2._Field._View-as THEN
        ddl[18] = "  VIEW-AS " + c.
      IF NOT l OR DICTDB._Field._Extent <> DICTDB2._Field._Extent THEN
        ddl[19] = "  EXTENT " + STRING(DICTDB._Field._Extent).
      IF NOT l OR DICTDB._Field._Decimals <> DICTDB2._Field._Decimals THEN
        ddl[20] = "  DECIMALS " + (IF DICTDB._Field._Decimals = ? THEN "?"
                    ELSE STRING(DICTDB._Field._Decimals)).
      IF NOT l OR DICTDB._Field._Order <> DICTDB2._Field._Order THEN
        ddl[21] = "  ORDER " + STRING(DICTDB._Field._Order).
      IF NOT l OR DICTDB._Field._Mandatory <> DICTDB2._Field._Mandatory THEN
        ddl[22] = (IF DICTDB._Field._Mandatory
                    THEN "  MANDATORY" ELSE "  NULL-ALLOWED").
      IF NOT l OR DICTDB._Field._Fld-case <> DICTDB2._Field._Fld-case THEN
        ddl[23] = (IF DICTDB._Field._Fld-case
                    THEN "  CASE-SENSITIVE" ELSE "  NOT-CASE-SENSITIVE").
  
      /* deal with field triggers */
      /* 1st, find ones to be deleted if field is being updated */
      j = 23.
      IF l THEN
        FOR EACH DICTDB2._Field-trig OF DICTDB2._Field:
          FIND DICTDB._Field-trig OF DICTDB._Field
            WHERE DICTDB._Field-trig._Event = DICTDB2._Field-trig._Event 
                  NO-ERROR.
          IF NOT AVAILABLE DICTDB._Field-trig THEN DO:
            RUN "prodict/_dctquot.p" (DICTDB2._Field-trig._Event,'"',OUTPUT c).
            j = j + 1.
            ddl[j] = "  FIELD-TRIGGER " + c + " DELETE".
          END.
        END.
      /* now record updated or new ones */
      FOR EACH DICTDB._Field-trig OF DICTDB._Field:
        FIND DICTDB2._Field-trig OF DICTDB2._Field 
          WHERE DICTDB2._Field-trig._Event = DICTDB._Field-trig._Event NO-ERROR.
        IF AVAILABLE DICTDB2._Field-trig AND 
          DICTDB2._Field-trig._Override = DICTDB._Field-trig._Override AND
          DICTDB2._Field-trig._Proc-name = DICTDB._Field-trig._Proc-name AND
          DICTDB2._Field-trig._Trig-CRC = DICTDB._Field-trig._Trig-CRC THEN
          NEXT.
          
        RUN "prodict/_dctquot.p" (DICTDB._Field-trig._Event,'"',OUTPUT c).
        j = j + 1. 
        ddl[j] = "  FIELD-TRIGGER " + c +
                 (IF DICTDB._Field-trig._Override THEN " OVERRIDE " 
                                                  ELSE " NO-OVERRIDE ").
        RUN "prodict/_dctquot.p" (DICTDB._Field-trig._Proc-name,'"',OUTPUT c).
        ddl[j] = ddl[j] + "PROCEDURE " + c + " CRC """ 
                 + (IF DICTDB._Field-trig._Trig-CRC = ? 
                    THEN "?" ELSE STRING(DICTDB._Field-trig._Trig-CRC))
                 + """".
      END. 
  
      /* don't write out header or anything unless there's values to output */
      l = FALSE.
      DO i = 2 TO j WHILE NOT l:
        l = ddl[i] <> "".
      END.
      IF l THEN DO i = 1 TO j:
        /* if ddl[i] = "" this doesn't do anything */
        PUT STREAM ddl UNFORMATTED ddl[i] SKIP.  
      END.
      IF l THEN PUT STREAM ddl UNFORMATTED SKIP(1).
    END.         /* end FOR EACH field-list */  
  
    /* note that there is no user interface for resolving renamed
    indexes.  this is because we can completely match indexes by their
    component lists, which are invariant once the index is created.  */
    ASSIGN
      pri1   = ""
      pri2   = "".
  
    /* build index component match list */
    FOR EACH DICTDB2._Index OF DICTDB2._File:
      DISPLAY DICTDB2._Index._Index-name @ idx WITH FRAME seeking.
      c = STRING(DICTDB2._Index._Unique,"u/a")
        + (IF DICTDB2._Index._Wordidx = 1 THEN "w" ELSE "f").
      FOR EACH DICTDB2._Index-field OF DICTDB2._Index,
        DICTDB2._Field OF DICTDB2._Index-field:
        FIND FIRST field-list
          WHERE field-list.f2-name = DICTDB2._Field._Field-name NO-ERROR.
        c = c + ","
          + STRING(DICTDB2._Index-field._Ascending,"+/-")
          + STRING(DICTDB2._Index-field._Abbreviate,"y/n")
          + STRING(DICTDB2._Field._dtype)
          + (IF AVAILABLE field-list THEN field-list.f2-name ELSE "*").
      END.
      CREATE index-list.
      ASSIGN
        index-list.i1-name = DICTDB2._Index._Index-name
        index-list.i1-comp = c
        index-list.i1-i2   = FALSE.
      IF DICTDB2._File._Prime-Index = RECID(DICTDB2._Index) THEN pri2 = c.
    END.
    FOR EACH DICTDB._Index OF DICTDB._File:
      DISPLAY DICTDB._Index._Index-name @ idx WITH FRAME seeking.
      c = STRING(DICTDB._Index._Unique,"u/a")
        + (IF DICTDB._Index._Wordidx = 1 THEN "w" ELSE "f").
      FOR EACH DICTDB._Index-field OF DICTDB._Index,
        DICTDB._Field OF DICTDB._Index-field:
        c = c + ","
          + STRING(DICTDB._Index-field._Ascending,"+/-")
          + STRING(DICTDB._Index-field._Abbreviate,"y/n")
          + STRING(DICTDB._Field._dtype)
          + DICTDB._Field._Field-name.
      END.
      CREATE index-list.
      ASSIGN
        index-list.i1-name = DICTDB._Index._Index-name
        index-list.i1-comp = c
        index-list.i1-i2   = TRUE.
      IF DICTDB._File._Prime-Index = RECID(DICTDB._Index) THEN pri1 = c.
    END.
  
    /* find all unchanged or renamed indexes by comparing idx comp lists */
    FOR EACH index-list WHERE index-list.i1-i2:
      DISPLAY index-list.i2-name @ idx WITH FRAME seeking.
      FIND FIRST index-alt WHERE NOT index-alt.i1-i2
        AND index-list.i1-comp = index-alt.i1-comp NO-ERROR.
      IF NOT AVAILABLE index-alt THEN NEXT.
      index-list.i2-name = index-alt.i1-name.
      DELETE index-alt.
    END.

    /* Now all index-list records where i1-i2 is false represent
       indexes in db2 that will not be renamed, therefore they will
       be deleted.   
    */

    /* check deactivation on unchanged indexes */
    FOR EACH index-list WHERE index-list.i1-name = index-list.i2-name:
      FIND DICTDB._Index OF DICTDB._File
        WHERE DICTDB._Index._Index-name = index-list.i1-name.
      FIND DICTDB2._Index OF DICTDB2._File
        WHERE DICTDB2._Index._Index-name = index-list.i2-name.
      DISPLAY DICTDB._Index._Index-name @ idx2 WITH FRAME seeking.
      IF NOT DICTDB._Index._Active AND DICTDB2._Index._Active THEN
        PUT STREAM ddl UNFORMATTED
          'UPDATE INACTIVE INDEX "' DICTDB._Index._Index-name
          '" OF "' DICTDB._File._File-name '"' SKIP.
      DELETE index-list.
    END.
  
    /* handle renamed indexes */
    ans = FALSE.
    FOR EACH index-list WHERE index-list.i2-name <> ?:
      FIND DICTDB._Index OF DICTDB._File
        WHERE DICTDB._Index._Index-name = index-list.i1-name.
      FIND DICTDB2._Index OF DICTDB2._File
        WHERE DICTDB2._Index._Index-name = index-list.i2-name.
      ans = TRUE.
      DISPLAY index-list.i1-name @ idx2 WITH FRAME seeking.
      RUN Check_Index_Conflict.
      PUT STREAM ddl UNFORMATTED
        'RENAME INDEX "' index-list.i2-name
        '" TO "' index-list.i1-name
        '" ON "' DICTDB._File._File-name '"' SKIP.
      IF NOT DICTDB._Index._Active AND DICTDB2._Index._Active THEN
        PUT STREAM ddl UNFORMATTED
          'UPDATE INACTIVE INDEX "' DICTDB._Index._Index-name
          '" OF "' DICTDB._File._File-name '"' SKIP.
      DELETE index-list.
    END.
    IF ans THEN PUT STREAM ddl UNFORMATTED SKIP(1).
  
    /* check if unique indexes to be created as inactive */
    FOR EACH index-list WHERE index-list.i1-i2,
      EACH DICTDB._Index OF DICTDB._File
      WHERE DICTDB._Index._Index-name = index-list.i1-name
        AND DICTDB._Index._Unique AND DICTDB._Index._Active:
      iact = TRUE.
      RUN "prodict/user/_usrdbox.p" (INPUT-OUTPUT iact,?,?,new_lang[2]).
      iact = NOT iact.
      LEAVE. /* we only need to ask once */
    END.
  
    /* handle new indexes */
    FOR EACH index-list WHERE index-list.i1-i2,
        DICTDB._Index OF DICTDB._File
        WHERE DICTDB._Index._Index-name = index-list.i1-name.
      DISPLAY DICTDB._Index._Index-name @ idx2 WITH FRAME seeking.
      RUN Check_Index_Conflict.
      PUT STREAM ddl UNFORMATTED "ADD "
        'INDEX "' DICTDB._Index._Index-Name
        '" ON "' DICTDB._File._File-name '"' SKIP.
      IF DICTDB._Index._Unique THEN 
        PUT STREAM ddl UNFORMATTED "  UNIQUE" SKIP.
      IF NOT (DICTDB._Index._Active AND (IF iact = ? THEN TRUE ELSE iact)) THEN
        PUT STREAM ddl UNFORMATTED "  INACTIVE" SKIP.
      IF DICTDB._Index._Wordidx = 1 THEN 
        PUT STREAM ddl UNFORMATTED "  WORD" SKIP.
      FOR EACH DICTDB._Index-field OF _Index,DICTDB._Field OF _Index-field
        BREAK BY DICTDB._Index-field._Index-seq:
        PUT STREAM ddl UNFORMATTED
          '  INDEX-FIELD "' DICTDB._Field._Field-Name '" '
          TRIM(STRING(DICTDB._Index-field._Ascending,"A/DE")) "SCENDING"
          (IF DICTDB._Index-field._Abbreviate THEN " ABBREVIATED" ELSE "")
          (IF DICTDB._Index-field._Unsorted   THEN " UNSORTED"    ELSE "") SKIP.
      END.
      PUT STREAM ddl UNFORMATTED SKIP(1).
    END.
  
    /* set primary index */
    RELEASE DICTDB._Index.
    IF DICTDB._File._Prime-Index <> ? THEN
      FIND DICTDB._Index WHERE RECID(DICTDB._Index) = DICTDB._File._Prime-Index
        NO-ERROR.
    IF AVAILABLE DICTDB._Index AND pri1 <> pri2 THEN
      PUT STREAM ddl UNFORMATTED
        'UPDATE PRIMARY INDEX "' DICTDB._Index._Index-name
        '" ON "' DICTDB._File._File-name '"' SKIP(1).
  
    /* handle deleted indexes */
    ans = FALSE.
    FOR EACH index-list WHERE NOT index-list.i1-i2:
      ans = TRUE.
      DISPLAY index-list.i1-name @ idx2 WITH FRAME seeking.
      IF index-list.i1-name <> "default" THEN 
        PUT STREAM ddl UNFORMATTED
          'DROP INDEX "' index-list.i1-name
          '" ON "' DICTDB._File._File-name '"' SKIP.
      DELETE index-list.
    END.
    IF ans THEN PUT STREAM ddl UNFORMATTED SKIP(1).
  
    /* handle deleted fields.  
       Do this after index deletes since fields cannot be dropped when they 
       belong to a primary index.  This is not done for fields that were 
       dropped but replaced with another field (different data type or extent) 
       but with the same name.  This still has to be done above so we can add 
       the new field without conflict.
    */
    FIND FIRST missing NO-ERROR.
    ans = FALSE.
    FOR EACH missing:
      ans = TRUE.
      DISPLAY missing.name @ fld2 WITH FRAME seeking.
      PUT STREAM ddl UNFORMATTED
        'DROP FIELD "' missing.name
        '" OF "' DICTDB._File._File-name '"' SKIP.
      DELETE missing.
    END.
    IF ans THEN PUT STREAM ddl UNFORMATTED SKIP(1).
  
  
    DO ON ERROR UNDO,LEAVE ON ENDKEY UNDO,LEAVE:
      HIDE MESSAGE.
    END.
  END.  /* end FOR EACH potentially altered file */
  
  DISPLAY "" @ fil "" @ fld "" @ idx WITH FRAME seeking.
  DISPLAY "" @ fil2 "" @ fld2 "" @ idx2 WITH FRAME seeking.
  
  /* build missing sequence list for rename/delete determination */
  FOR EACH DICTDB2._Sequence
    WHERE DICTDB2._Sequence._Db-recid = RECID(database2):
    FIND FIRST DICTDB._Sequence
      WHERE DICTDB._Sequence._Db-recid = drec_db
        AND DICTDB._Sequence._Seq-name = DICTDB2._Sequence._Seq-name NO-ERROR.
    DISPLAY DICTDB2._Sequence._Seq-name @ seq WITH FRAME seeking.
    IF AVAILABLE DICTDB._Sequence THEN NEXT.
    CREATE missing.
    missing.name = DICTDB2._Sequence._Seq-name.
  END.
  
  /* build list of new or renamed sequences */
  FOR EACH DICTDB._Sequence
    WHERE DICTDB._Sequence._Db-recid = drec_db:
    FIND FIRST DICTDB2._Sequence
      WHERE DICTDB2._Sequence._Db-recid = RECID(database2)
        AND DICTDB2._Sequence._Seq-name = DICTDB._Sequence._Seq-name NO-ERROR.
    DISPLAY DICTDB._Sequence._Seq-name @ seq WITH FRAME seeking.
    CREATE seq-list.
    seq-list.s1-name = DICTDB._Sequence._Seq-name.
    IF AVAILABLE DICTDB2._Sequence THEN
      seq-list.s2-name = DICTDB._Sequence._Seq-name.
  END.
  
  /* look for matches for renamed sequences with user input.  A prompt 
     is given for each seq in DICTDB2 that's not in DICTDB but only when
     there is also a seq in DICTDB that's not in DICTDB2.  The 2nd list
     is the potential values to rename to.
  */
  run adecomm/_setcurs.p ("").
  FOR EACH missing:
    DISPLAY missing.name @ seq WITH FRAME seeking.
    RUN "prodict/dump/_dmpisub.p"
      (INPUT "s",INPUT-OUTPUT missing.name,OUTPUT ans).
    IF missing.name = ? THEN DELETE missing.
    IF ans = ? THEN DO:
      HIDE FRAME seeking NO-PAUSE.
      user_path = "".
      RETURN.
    END.
  END.
  run adecomm/_setcurs.p ("WAIT").
  
  /* handle deleted sequences */
  ans = FALSE.
  FOR EACH missing:
    ans = TRUE.
    PUT STREAM ddl UNFORMATTED
      'DROP SEQUENCE "' missing.name '"' SKIP.
    DISPLAY missing.name @ seq WITH FRAME seeking.
    DISPLAY missing.name @ seq2 WITH FRAME seeking.
    DELETE missing.
  END.
  IF ans THEN PUT STREAM ddl UNFORMATTED SKIP(1).
  
  /* handle renamed sequences */
  ans = FALSE.
  FOR EACH seq-list
    WHERE seq-list.s1-name <> seq-list.s2-name
      AND seq-list.s2-name <> ?:
    ans = TRUE.
    PUT STREAM ddl UNFORMATTED
      'RENAME SEQUENCE "' seq-list.s2-name
      '" TO "' seq-list.s1-name '"' SKIP.
    DISPLAY seq-list.s1-name @ seq WITH FRAME seeking.
    DISPLAY seq-list.s1-name @ seq2 WITH FRAME seeking.
  END.
  IF ans THEN PUT STREAM ddl UNFORMATTED SKIP(1).
  
  /* handle new or potentially altered sequences.  
     We can't use dumpdefs here like we do with files because it wasn't 
     made to handle individual sequences - it would dump them all.
     Some day!
  */
  FOR EACH seq-list:
    DISPLAY seq-list.s1-name @ seq WITH FRAME seeking.
  
    FIND DICTDB._Sequence WHERE DICTDB._Sequence._Db-recid = drec_db
      AND DICTDB._Sequence._Seq-name = seq-list.s1-name.
    FIND DICTDB2._Sequence WHERE DICTDB2._Sequence._Db-recid = RECID(database2)
      AND DICTDB2._Sequence._Seq-name = seq-list.s2-name NO-ERROR.
  
    DISPLAY seq-list.s1-name @ seq2 WITH FRAME seeking.
  
    /* If l is true we're updateing otherwise we're adding */
    l = AVAILABLE DICTDB2._Sequence.
  
    /* write out appropriate seq definition changes */
    ASSIGN
      j      = 1
      ddl    = ""
      ddl[1] = (IF l THEN "UPDATE" ELSE "ADD")
               + ' SEQUENCE "' + DICTDB._Sequence._Seq-name + '"'.
      IF NOT l OR 
               DICTDB._Sequence._Seq-init <> DICTDB2._Sequence._Seq-init THEN 
        ASSIGN
          j = j + 1
          ddl[j] = "  INITIAL " + (IF DICTDB._Sequence._Seq-init = ? THEN "?"
                   ELSE STRING(DICTDB._Sequence._Seq-init)).
      IF NOT l OR 
               DICTDB._Sequence._Seq-incr <> DICTDB2._Sequence._Seq-incr THEN 
        ASSIGN
          j = j + 1
          ddl[j] = "  INCREMENT " + (IF DICTDB._Sequence._Seq-incr = ? THEN "?" 
                   ELSE STRING(DICTDB._Sequence._Seq-incr)).
      IF NOT l OR 
               DICTDB._Sequence._Cycle-OK <> DICTDB2._Sequence._Cycle-OK THEN 
        ASSIGN
          j = j + 1
          ddl[j] = "  CYCLE-ON-LIMIT " + 
                   (IF DICTDB._Sequence._Cycle-OK THEN "yes" ELSE "no").
      IF NOT l OR DICTDB._Sequence._Seq-min <> DICTDB2._Sequence._Seq-min THEN 
        ASSIGN
          j = j + 1
          ddl[j] = "  MIN-VAL " + (IF DICTDB._Sequence._Seq-min = ? THEN "?" 
                   ELSE STRING(DICTDB._Sequence._Seq-min)).
      IF NOT l OR DICTDB._Sequence._Seq-max <> DICTDB2._Sequence._Seq-max THEN 
        ASSIGN
          j = j + 1
          ddl[j] = "  MAX-VAL " + (IF DICTDB._Sequence._Seq-max = ? THEN "?" 
                   ELSE STRING(DICTDB._Sequence._Seq-max)).
  
      /* don't write out ddl[1] if j = 1 (i.e., we only have seq header) */
      IF j > 1 THEN 
        DO i = 1 TO j + 1:
          IF ddl[i] = "" THEN  /* this puts an extra skip after the last one */
            PUT STREAM ddl UNFORMATTED SKIP(1).
          ELSE
            PUT STREAM ddl UNFORMATTED ddl[i] SKIP.
        END.
  
  END.  /* end FOR EACH new or potentially altered sequence */
  
  /* Sync up the auto-connect records.  If there's any auto-connect records
     in either database, drop all the ones in DICTDB2 and add what's in
     DICTDB.  Since there's no data associated with these, we can drop with
     a clear conscience.   This is easier than trying to compare the 
     differences.
  */
  FOR EACH DICTDB2._Db WHERE DICTDB2._Db._Db-name <> ? AND 
                             NOT DICTDB2._Db._Db-slave AND  /* not foreign db */
                             NOT DICTDB2._Db._Db-local:
     PUT STREAM ddl UNFORMATTED 
        'DROP DATABASE "' DICTDB2._Db._Db-name '"' SKIP(1).
  END.
  FOR EACH DICTDB._Db WHERE DICTDB._Db._Db-name <> ? AND 
                            NOT DICTDB._Db._Db-slave AND  /* not foreign db */
                            NOT DICTDB._Db._Db-local:
     PUT STREAM ddl UNFORMATTED 
        'ADD DATABASE "' DICTDB._Db._Db-name '" TYPE PROGRESS' SKIP.
     PUT STREAM ddl UNFORMATTED 'DBNAME "' DICTDB._Db._Db-addr '"' SKIP.
     IF DICTDB._Db._Db-comm <> "" THEN
        PUT STREAM ddl UNFORMATTED 'PARAMS "'  DICTDB._Db._Db-comm '"' SKIP.
     PUT STREAM ddl UNFORMATTED SKIP(1).
  END.


  {prodict/dump/dmptrail.i
    &entries      = " "
    &seek-stream  = "ddl"
    &stream       = "STREAM ddl "
    }  /* adds trailer with code-page-entrie to end of file */
    
  stopped = false.
END. /* on stop */

IF stopped THEN
   MESSAGE "Dump terminated."
                 VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
ELSE
   MESSAGE "Dump completed."
                 VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.

HIDE FRAME seeking NO-PAUSE.
SESSION:IMMEDIATE-DISPLAY = no.
run adecomm/_setcurs.p ("").
RETURN.





