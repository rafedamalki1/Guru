/*************************************************************/
/* Copyright (c) 1984-1997 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/* _dmpincr.p - phase 2 of incremental .df maker 

   as4dict  is the current database
   workfiles have been created for the old database to be coverted.

   The aim is to produce a database like as4dict.  So this .df file will be
   run against a olde version database to create a database like as4dict.
   Created 06/05/97 D. McMann
*/

/*
as4dict  = new definitions

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
    D. McMann   06/25/97    Modified to work with PROGRESS/400
        
*/

&GLOBAL-DEFINE WIN95-BTN YES

{ as4dict/dictvar.i SHARED }
{ as4dict/dump/dumpvar.i SHARED }
{ as4dict/dump/userpik.i }

define shared frame working.
{ as4dict/dump/as4dmpdf.f &FRAME = "working" }

DEFINE VARIABLE ans	AS LOGICAL   INITIAL FALSE NO-UNDO.
DEFINE VARIABLE c	AS CHARACTER               NO-UNDO.
DEFINE VARIABLE fil	AS CHARACTER               NO-UNDO.
DEFINE VARIABLE fld	AS CHARACTER               NO-UNDO.
DEFINE VARIABLE idx	AS CHARACTER               NO-UNDO.
DEFINE VARIABLE seq	AS CHARACTER               NO-UNDO.
DEFINE VARIABLE i	AS INTEGER                 NO-UNDO.
DEFINE VARIABLE j	AS INTEGER                 NO-UNDO.
DEFINE VARIABLE l	AS LOGICAL                 NO-UNDO.
DEFINE VARIABLE stopped AS LOGICAL   INITIAL TRUE  NO-UNDO.
DEFINE VARIABLE inpri   AS LOGICAL                 NO-UNDO.
DEFINE VARIABLE tmp_Field-name AS CHARACTER        NO-UNDO.

/* LANGUAGE DEPENDENCIES START */ /*----------------------------------------*/
DEFINE VARIABLE new_lang AS CHARACTER EXTENT 2 NO-UNDO INITIAL [
  /* 1*/ "(initializing)",
  /* 2*/ ? /* see below */
].

new_lang[2] = "The incremental definitions file will cause at least "
            + "one new unique index to be created. "
            + "If PROGRESS finds duplicate values while creating unique "
            + "indexes, it will UNDO the entire transaction, causing "
            + "you to lose any other schema changes just made. Do you want to "
            + "create indexes that are marked as unique for this file?".

FORM
  "Scanning:" SKIP
  fil LABEL "TABLE"    	COLON 9 FORMAT "x(27)" SKIP
  fld LABEL "FIELD"  	COLON 9 FORMAT "x(27)" SKIP
  idx LABEL "INDEX"  	COLON 9 FORMAT "x(27)" SKIP
  seq LABEL "SEQ" 	COLON 9 FORMAT "x(27)" SKIP
  HEADER 
    " Press " +
    KBLABEL("STOP") + " to terminate dump." format "x(38)" SKIP
  WITH FRAME seeking OVERLAY THREE-D
    SIDE-LABELS WIDTH 40 VIEW-AS DIALOG-BOX ROW 2 COLUMN 4 USE-TEXT.
COLOR DISPLAY MESSAGES fil fld idx seq WITH FRAME seeking.

FORM
  "Working on:" SKIP
  fil LABEL "TABLE"   	COLON 9 FORMAT "x(27)" SKIP
  fld LABEL "FIELD"  	COLON 9 FORMAT "x(27)" SKIP
  idx LABEL "INDEX"  	COLON 9 FORMAT "x(27)" SKIP
  seq LABEL "SEQ" 	COLON 9 FORMAT "x(27)" SKIP
  HEADER " " SKIP
  WITH FRAME wrking OVERLAY THREE-D
  SIDE-LABELS WIDTH 40 VIEW-AS DIALOG-BOX  ROW 2 COLUMN 44 USE-TEXT.
COLOR DISPLAY MESSAGES fil fld idx seq WITH FRAME wrking.

/* LANGUAGE DEPENDENCIES END */ /*------------------------------------------*/

/* Definitions */ /*-------------------------------------------------------*/

DEFINE VARIABLE ddl  AS CHARACTER EXTENT 35 NO-UNDO.
DEFINE VARIABLE iact AS LOGICAL   INITIAL ? NO-UNDO.
DEFINE VARIABLE pri1 AS CHARACTER           NO-UNDO.
DEFINE VARIABLE pri2 AS CHARACTER           NO-UNDO.
DEFINE VARIABLE cnt  AS INTEGER             NO-UNDO.


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

DEFINE BUFFER old-field FOR as4dict.p__Field.
DEFINE BUFFER new-field FOR wfld.

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
	'" ON "' as4dict.p__File._File-name '"' SKIP(1).
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
     FIND FIRST old-field WHERE as4dict.old-field._Field-name = newname
          NO-ERROR.
     IF NOT AVAILABLE(old-field) THEN DO:
       FIND FIRST new-field WHERE 
           new-field._Field-name = newname
           NO-ERROR.
       IF NOT AVAILABLE(new-field) THEN DO:
         FIND FIRST missing WHERE missing.name = newname NO-ERROR.
         IF NOT AVAILABLE(missing) THEN LEAVE. /* got it! */
       END.
     END.
   END. 
END PROCEDURE.   

PROCEDURE inprimary:
   /* Determines whether a field is part of a primary index */

   DEFINE INPUT PARAMETER rfield AS INTEGER        NO-UNDO. /*# of p__Field*/
   DEFINE INPUT PARAMETER rfldfil AS INTEGER       NO-UNDO.
   DEFINE OUTPUT PARAMETER prime AS LOG INITIAL no NO-UNDO. /*pri? y/n */
   
   FOR EACH as4dict.p__Idxfd WHERE as4dict.p__Idxfd._fld-number = rfield
                               AND as4dict.p__Idxfd._File-number = rfldfil:
     FIND as4dict.p__File WHERE as4dict.p__File._File-number = rfldfil NO-LOCK.                                as4dict.p__Field._file-number NO-ERROR.
     IF as4dict.p__File._prime-index = as4dict.p__Idxfd._idx-num THEN DO:
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
DISPLAY new_lang[1] @ fil WITH FRAME seeking. /* initializing */
DISPLAY new_lang[1] @ fil WITH FRAME wrking. /* initializing */
run adecomm/_setcurs.p ("WAIT").

DO ON STOP UNDO, LEAVE:
  /* build missing file list for rename/delete determination */
  FOR EACH wfil
    WHERE wfil._Hidden = "N" NO-LOCK:
    FIND FIRST as4dict.p__File WHERE as4dict.p__File._File-name = wfil._File-name
	                          AND as4dict.p__File._Hidden = "N" NO-LOCK NO-ERROR.
    DISPLAY wfil._File-name @ fil WITH FRAME seeking.
    IF AVAILABLE as4dict.p__File THEN NEXT.
    CREATE missing.
    missing.name = wfil._File-name.
  END.
  
  /* build list of new or renamed files */
  FOR EACH as4dict.p__File WHERE as4dict.p__File._Hidden = "N" NO-LOCK:
    FIND FIRST wfil WHERE wfil._File-name = as4dict.p__File._File-name NO-LOCK NO-ERROR.
    DISPLAY as4dict.p__File._File-name @ fil WITH FRAME seeking.
    CREATE table-list.
    table-list.t1-name = as4dict.p__File._File-name.
    IF AVAILABLE wfil THEN
      table-list.t2-name = wfil._File-name.
  END.
  
  /* look for matches for renamed files with user input.  A prompt 
     is given for each file that's not in as4dict but only when
     there is also a file in as4dict that's not in temp table.  The 2nd list
     is the potential values to rename to.
  */
  run adecomm/_setcurs.p ("").  /* while dmpisub is running */
  FOR EACH missing:
    DISPLAY missing.name @ fil WITH FRAME seeking.
    RUN "as4dict/dump/_dmpisub.p"
      (INPUT "t",INPUT-OUTPUT missing.name,OUTPUT ans).
    IF missing.name = ? THEN DELETE missing.
    IF ans = ? THEN DO:
      HIDE FRAME wrking NO-PAUSE.
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
    DISPLAY missing.name @ fil WITH FRAME wrking.
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
    DISPLAY table-list.t1-name @ fil WITH FRAME wrking.
  END.
  IF ans THEN PUT STREAM ddl UNFORMATTED SKIP(1).
  
  /* dump newly created files */
  FOR EACH table-list WHERE table-list.t2-name = ?:
    FIND as4dict.p__File WHERE as4dict.p__File._File-name = table-list.t1-name NO-LOCK.
    DISPLAY as4dict.p__File._File-name @ fil WITH FRAME seeking.
    RUN "as4dict/dump/_dmpdefs.p" ("t", as4dict.p__File._File-number).
    HIDE FRAME working.
    DISPLAY as4dict.p__File._File-name @ fil WITH FRAME wrking.
    DELETE table-list.
  END.
  
  /* handle potentially altered files */
  FOR EACH table-list:
    DISPLAY table-list.t1-name @ fil "" @ fld "" @ idx WITH FRAME seeking.
    DISPLAY table-list.t1-name @ fil "" @ fld "" @ idx WITH FRAME wrking.
    FIND as4dict.p__File WHERE as4dict.p__File._File-name = table-list.t1-name NO-LOCK.
    FIND wfil WHERE wfil._File-name = table-list.t2-name NO-LOCK.
  
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
      ddl[1] = 'UPDATE TABLE "' + as4dict.p__File._File-name + '"'.
    RUN "prodict/_dctquot.p" (as4dict.p__File._Can-Read,'"',OUTPUT c).
    IF as4dict.p__File._Can-read <> wfil._Can-read THEN ASSIGN
      j = j + 1
      ddl[j] = "  CAN-READ " + c.
    RUN "prodict/_dctquot.p" (as4dict.p__File._Can-Write,'"',OUTPUT c).
    IF as4dict.p__File._Can-write <> wfil._Can-write THEN ASSIGN
      j = j + 1
      ddl[j] = "  CAN-WRITE " + c.
    RUN "prodict/_dctquot.p" (as4dict.p__File._Can-Create,'"',OUTPUT c).
    IF as4dict.p__File._Can-create <> wfil._Can-create THEN ASSIGN
      j = j + 1
      ddl[j] = "  CAN-CREATE " + c.
    RUN "prodict/_dctquot.p" (as4dict.p__File._Can-Delete,'"',OUTPUT c).
    IF as4dict.p__File._Can-delete <> wfil._Can-delete THEN ASSIGN
      j = j + 1
      ddl[j] = "  CAN-DELETE " + c.
    RUN "prodict/_dctquot.p" (as4dict.p__File._Can-Dump,'"',OUTPUT c).
    IF as4dict.p__File._Can-Dump <> wfil._Can-Dump THEN ASSIGN
      j = j + 1
      ddl[j] = "  CAN-DUMP " + c.
    RUN "prodict/_dctquot.p" (as4dict.p__File._Can-Load,'"',OUTPUT c).
    IF as4dict.p__File._Can-Load <> wfil._Can-Load THEN ASSIGN
      j = j + 1
      ddl[j] = "  CAN-LOAD " + c.
    RUN "prodict/_dctquot.p" (as4dict.p__File._Desc,'"',OUTPUT c).
    IF as4dict.p__File._Desc <> wfil._Desc THEN ASSIGN
      j = j + 1
      ddl[j] = "  DESCRIPTION " + c.
    RUN "prodict/_dctquot.p" (as4dict.p__File._File-label,'"',OUTPUT c).
    IF as4dict.p__File._File-label <> wfil._File-label THEN ASSIGN
      j = j + 1
      ddl[j] = "  LABEL " + c.
    RUN "prodict/_dctquot.p" (as4dict.p__File._File-label-SA,'"',OUTPUT c).
    IF as4dict.p__File._File-label-SA <> wfil._File-label-SA THEN ASSIGN
      j = j + 1
      ddl[j] = "  LABEL-SA " + c.
    RUN "prodict/_dctquot.p" (as4dict.p__File._Valexp,'"',OUTPUT c).
    IF as4dict.p__File._Valexp <> wfil._Valexp THEN ASSIGN
      j = j + 1
      ddl[j] = "  VALEXP " + c.
    RUN "prodict/_dctquot.p" (as4dict.p__File._Valmsg,'"',OUTPUT c).
    IF as4dict.p__File._Valmsg <> wfil._Valmsg THEN ASSIGN
      j = j + 1
      ddl[j] = "  VALMSG " + c.
    RUN "prodict/_dctquot.p" (as4dict.p__File._Valmsg-SA,'"',OUTPUT c).
    IF as4dict.p__File._Valmsg-SA <> wfil._Valmsg-SA THEN ASSIGN
      j = j + 1
      ddl[j] = "  VALMSG-SA " + c.
    RUN "prodict/_dctquot.p" (as4dict.p__File._Dump-name,'"',OUTPUT c).
    IF as4dict.p__File._Dump-name <> wfil._Dump-name THEN ASSIGN
      j = j + 1
      ddl[j] = "  DUMP-NAME " + c.
    IF as4dict.p__File._Frozen = "Y" THEN ASSIGN
      j = j + 1
      ddl[j] = "  FROZEN".
  
    /* deal with file triggers */
    /* 1st, find ones to be deleted */
    FOR EACH wfit WHERE wfit._File-number =
                                        wfil._File-number NO-LOCK:
      FIND as4dict.p__Trgfl WHERE as4dict.p__Trgfl._File-number =
                                     as4dict.p__File._File-number
	                        AND as4dict.p__Trgfl._Event = wfit._Event NO-LOCK NO-ERROR.
      IF NOT AVAILABLE as4dict.p__Trgfl THEN DO:
	RUN "prodict/_dctquot.p" (wfit._Event,'"',OUTPUT c).
	j = j + 1.
	ddl[j] = "  TABLE-TRIGGER " + c + " DELETE".
      END.
    END.
    /* now record updated or new ones */
    FOR EACH as4dict.p__Trgfl WHERE as4dict.p__Trgfl._File-number =
                                                as4dict.p__File._File-number NO-LOCK:
      FIND wfit WHERE wfit._File-number =
                                                 wfil._File-number
	                        AND wfit._Event = as4dict.p__Trgfl._Event NO-LOCK NO-ERROR.
      IF AVAILABLE wfit AND 
   	  wfit._Override = as4dict.p__Trgfl._Override AND
	  wfit._Proc-name = as4dict.p__Trgfl._Proc-name AND
	  wfit._Trig-CRC = as4dict.p__Trgfl._Trig-CRC THEN
	  NEXT.
	
      RUN "prodict/_dctquot.p" (as4dict.p__Trgfl._Event,'"',OUTPUT c).
      j = j + 1.
      ddl[j] = "  TABLE-TRIGGER " + c +
	       (IF as4dict.p__Trgfl._Override = "Y" THEN " OVERRIDE " 
					       ELSE " NO-OVERRIDE ").
      RUN "prodict/_dctquot.p" (as4dict.p__Trgfl._Proc-name,'"',OUTPUT c).
      ddl[j] = ddl[j] + "PROCEDURE " + c + " CRC """ 
	       + (IF as4dict.p__Trgfl._Trig-CRC = ? 
		  THEN "?" ELSE STRING(as4dict.p__Trgfl._Trig-CRC))
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
    FOR EACH wfld WHERE wfld._File-number =
                                        wfil._File-number 
                                 AND wfld._fld-misc2[5] <> "A" NO-LOCK       
                                  BY wfld._fld-number:
      FIND FIRST as4dict.p__Field WHERE as4dict.p__Field._File-number =
                                            as4dict.p__File._File-number
	                              AND as4dict.p__Field._Field-name = wfld._Field-name NO-LOCK NO-ERROR.
      DISPLAY wfld._Field-name @ fld WITH FRAME seeking.
      IF AVAILABLE as4dict.p__Field THEN NEXT.
      CREATE missing.
      missing.name = wfld._Field-name.
    END.
  
    /* build field rename list */
    FOR EACH as4dict.p__Field WHERE as4dict.p__Field._File-number =
                                        as4dict.p__File._File-number
                                AND as4dict.p__Field._Fld-misc2[5] <> "A" NO-LOCK
                                 BY as4dict.p__Field._fld-number:
      FIND FIRST wfld WHERE wfld._File-number =
                                           wfil._File-number
	                              AND wfld._Field-name = 
	                                    as4dict.p__Field._Field-name NO-LOCK NO-ERROR.
      DISPLAY as4dict.p__Field._Field-name @ fld WITH FRAME seeking.
      CREATE field-list.
      field-list.f1-name = as4dict.p__Field._Field-name.
      IF AVAILABLE wfld THEN
	field-list.f2-name = wfld._Field-name.
    END.
  
    /* look for matches for renamed fields with user input.  A prompt 
       is given for each field that's not in as4dict but only when
       there is also a field in as4dict that's not in temp table.  The 2nd list
       is the potential values to rename to.
    */
    run adecomm/_setcurs.p ("").

    user_env[19] = as4dict.p__File._File-name. /* this is a hack */
    FOR EACH missing:
      DISPLAY missing.name @ fld WITH FRAME seeking.
      RUN "as4dict/dump/_dmpisub.p"
	(INPUT "f",INPUT-OUTPUT missing.name,OUTPUT ans).
      IF missing.name = ? THEN DELETE missing.
      IF ans = ? THEN DO:
	HIDE FRAME wrking NO-PAUSE.
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
      DISPLAY field-list.f1-name @ fld WITH FRAME wrking.
      PUT STREAM ddl UNFORMATTED
	'RENAME FIELD "' field-list.f2-name
	'" OF "' as4dict.p__File._File-name
	'" TO "' field-list.f1-name '"' SKIP.
    END.
    IF ans THEN PUT STREAM ddl UNFORMATTED SKIP(1).
  
    /* handle new or potentially altered fields */
    FOR EACH field-list:
      FIND FIRST as4dict.p__Field WHERE as4dict.p__Field._File-number =
                                           as4dict.p__File._File-number
	                             AND as4dict.p__Field._Field-name = 
	                                    field-list.f1-name NO-LOCK.
      FIND FIRST wfld WHERE wfld._File-number =
                                           wfil._File-number
	                              AND wfld._Field-name = 
	                                    field-list.f2-name NO-LOCK NO-ERROR.
      DISPLAY field-list.f1-name @ fld WITH FRAME wrking.
      l = AVAILABLE wfld.
      IF l AND (as4dict.p__Field._Data-type <> wfld._Data-type
	OR      as4dict.p__Field._Extent    <> wfld._Extent) THEN DO:
        /* If field is part of a primary index, we cannot simply drop it.
         * instead, we will rename it to something else, and delete it
         * later, after the indexes are processed.
         */
        RUN inprimary (INPUT as4dict.p__Field._fld-number, 
                       INPUT as4dict.p__Field._File-number,
                       OUTPUT inpri).
        IF inpri THEN DO: /* field is part of primary index, don't DROP*/
          RUN tmp-name (INPUT as4dict.p__Field._Field-name, OUTPUT tmp_Field-name).
          PUT STREAM ddl UNFORMATTED
	    'RENAME FIELD "' as4dict.p__Field._Field-name
	    '" OF "' as4dict.p__File._File-name
	    '" TO "' tmp_Field-name '"' SKIP.
          CREATE missing. 
          ASSIGN missing.name = tmp_Field-name. /*record name to 'DROP' later*/
        END.
        ELSE /* is not in a primary index, we can DROP it now */
	  PUT STREAM ddl UNFORMATTED
	    'DROP FIELD "' as4dict.p__Field._Field-name
	    '" OF "' as4dict.p__File._File-name '"' SKIP.
	RELEASE wfld.
	l = FALSE.
      END.
  
      /* If l is true we're updateing otherwise we're adding */
      ASSIGN
	ddl    = ""
	ddl[1] = (IF l THEN "UPDATE" ELSE "ADD")
	       + ' FIELD "' + as4dict.p__Field._Field-name
	       + '" OF "' + as4dict.p__File._File-name + '"'
	       + (IF l THEN "" ELSE " AS " + as4dict.p__Field._Data-type).
      RUN "prodict/_dctquot.p" (as4dict.p__Field._Desc,'"',OUTPUT c).    
      IF NOT l OR as4dict.p__Field._Desc <> wfld._Desc THEN  
           ddl[2] = (IF length(c) = 2 THEN "" ELSE "  DESCRIPTION " + c).	                  	  
      RUN "prodict/_dctquot.p" (as4dict.p__Field._Format,'"',OUTPUT c).
      IF NOT l OR as4dict.p__Field._Format <> wfld._Format THEN 
	   ddl[3] = (IF length(c) = 2 THEN "" ELSE "  FORMAT " + c).  
      RUN "prodict/_dctquot.p" (as4dict.p__Field._Format-SA,'"',OUTPUT c).
      IF NOT l OR as4dict.p__Field._Format-SA <> wfld._Format-SA THEN         
	   ddl[4] = (IF length(c) = 2 THEN "" ELSE ("  FORMAT-SA " + c)).	   	   
      RUN "prodict/_dctquot.p" (as4dict.p__Field._Initial,'"',OUTPUT c).
      IF NOT l OR as4dict.p__Field._Initial <> wfld._Initial THEN 
         ddl[5] = (IF length(c) = 2 THEN "" ELSE "  INITIAL " + c).         
      RUN "prodict/_dctquot.p" (as4dict.p__Field._Initial-SA,'"',OUTPUT c).
      IF NOT l OR as4dict.p__Field._Initial-SA <> wfld._Initial-SA THEN 
	   ddl[6] = (IF length(c) = 2 THEN "" ELSE "  INITIAL-SA " + c). 
      RUN "prodict/_dctquot.p" (as4dict.p__Field._Help,'"',OUTPUT c).
      IF NOT l OR as4dict.p__Field._Help <> wfld._Help THEN
	  ddl[7] = (IF length(c) = 2 THEN "" ELSE "  HELP " + c). 
      RUN "prodict/_dctquot.p" (as4dict.p__Field._Help-SA,'"',OUTPUT c).
      IF NOT l OR as4dict.p__Field._Help-SA <> wfld._Help-SA THEN
	ddl[8] = (IF length(c) = 2 THEN "" ELSE "  HELP-SA " + c).
      RUN "prodict/_dctquot.p" (as4dict.p__Field._Label,'"',OUTPUT c).
      IF NOT l OR as4dict.p__Field._Label <> wfld._Label THEN
	ddl[9] = (IF length(c) = 2 THEN "" ELSE "  LABEL " + c).
      RUN "prodict/_dctquot.p" (as4dict.p__Field._Label-SA,'"',OUTPUT c).
      IF NOT l OR as4dict.p__Field._Label-SA <> wfld._Label-SA THEN
	ddl[10] = (IF length(c) = 2 THEN "" ELSE "  LABEL-SA " + c).
      RUN "prodict/_dctquot.p" (as4dict.p__Field._Col-label,'"',OUTPUT c).
      IF NOT l OR as4dict.p__Field._Col-label <> wfld._Col-label THEN
	ddl[11] = (IF length(c) = 2 THEN "" ELSE "  COLUMN-LABEL " + c).
      RUN "prodict/_dctquot.p" (as4dict.p__Field._Col-label-SA,'"',OUTPUT c).
      IF NOT l OR 
      	 as4dict.p__Field._Col-label-SA <> wfld._Col-label-SA THEN
	ddl[12] = (IF length(c) = 2 THEN "" ELSE "  COLUMN-LABEL-SA " + c).
      RUN "prodict/_dctquot.p" (as4dict.p__Field._Can-Read,'"',OUTPUT c).
      IF NOT l OR as4dict.p__Field._Can-read <> wfld._Can-read THEN
	ddl[13] = (IF length(c) = 2 THEN "" ELSE "  CAN-READ " + c).
      RUN "prodict/_dctquot.p" (as4dict.p__Field._Can-Write,'"',OUTPUT c).
      IF NOT l OR as4dict.p__Field._Can-write <> wfld._Can-write THEN
	ddl[14] = (IF length(c) = 2 THEN "" ELSE "  CAN-WRITE " + c).
      RUN "prodict/_dctquot.p" (as4dict.p__Field._Valexp,'"',OUTPUT c).
      IF NOT l OR as4dict.p__Field._Valexp <> wfld._Valexp THEN
	ddl[15] = (IF length(c) = 2 THEN "" ELSE "  VALEXP " + c).
      RUN "prodict/_dctquot.p" (as4dict.p__Field._Valmsg,'"',OUTPUT c).
      IF NOT l OR as4dict.p__Field._Valmsg <> wfld._Valmsg THEN
	ddl[16] = (IF length(c) = 2 THEN "" ELSE "  VALMSG " + c).
      RUN "prodict/_dctquot.p" (as4dict.p__Field._Valmsg-SA,'"',OUTPUT c).
      IF NOT l OR as4dict.p__Field._Valmsg-SA <> wfld._Valmsg-SA THEN
	ddl[17] = (IF length(c) = 2 THEN "" ELSE "  VALMSG-SA " + c).
      RUN "prodict/_dctquot.p" (as4dict.p__Field._View-as,'"',OUTPUT c).
      IF NOT l OR as4dict.p__Field._View-as <> wfld._View-as THEN
	ddl[18] = (IF length(c) = 2 THEN "" ELSE "  VIEW-AS " + c).
      IF NOT l OR as4dict.p__Field._Extent <> wfld._Extent THEN
	ddl[19] = (IF length(c) = 2 THEN "" ELSE "  EXTENT " + STRING(as4dict.p__Field._Extent)).
      IF NOT l OR as4dict.p__Field._Decimals <> wfld._Decimals THEN
	ddl[20] = "  DECIMALS " + (IF as4dict.p__Field._Decimals = 0 THEN "0"
		    ELSE STRING(as4dict.p__Field._Decimals)).
      IF NOT l OR as4dict.p__Field._Order <> wfld._Order THEN
	  ddl[21] = "  ORDER " + STRING(as4dict.p__Field._Order).
      IF NOT l OR as4dict.p__Field._Mandatory <> wfld._Mandatory THEN
	  ddl[22] = (IF as4dict.p__Field._Mandatory = "Y"
		    THEN "  MANDATORY" ELSE "").
      IF NOT l OR as4dict.p__Field._Fld-Misc2[2] <> wfld._Fld-Misc2[2] THEN
	  ddl[23] = (IF as4dict.p__Field._Fld-Misc2[2] = "Y"
		    THEN "  NULL-CAPABLE" ELSE "").	    
      IF NOT l OR as4dict.p__Field._Fld-case <> wfld._Fld-case THEN
	  ddl[24] = (IF as4dict.p__Field._Fld-case = "Y" THEN "  CASE-SENSITIVE" ELSE "").
  
      /* deal with field triggers */
      /* 1st, find ones to be deleted if field is being updated */
      j = 23.
      IF l THEN
	FOR EACH wflt WHERE wflt._File-number =
	                                     wfld._File-number
	                             AND wflt._Fld-number =
	                                     wfld._Fld-number NO-LOCK:
	  FIND as4dict.p__Trgfd WHERE as4dict.p__Trgfd._File-number =
	                                     as4dict.p__Field._File-number
	                          AND as4dict.p__Trgfd._Fld-number =
	                                     as4dict.p__Field._Fld-number
	                          AND as4dict.p__Trgfd._Event = wflt._Event 
      	                          NO-LOCK NO-ERROR.
	  IF NOT AVAILABLE as4dict.p__Trgfd THEN DO:
	    RUN "prodict/_dctquot.p" (wflt._Event,'"',OUTPUT c).
	    j = j + 1.
	    ddl[j] = "  FIELD-TRIGGER " + c + " DELETE".
	  END.
	END.
      /* now record updated or new ones */
      FOR EACH as4dict.p__Trgfd WHERE as4dict.p__Trgfd._File-number =
	                                     as4dict.p__Field._File-number
	                           AND as4dict.p__Trgfd._Fld-number =
	                                     as4dict.p__Field._Fld-number NO-LOCK:
	FIND wflt WHERE wflt._File-number =
	                                     wfld._File-number
	                         AND wflt._Fld-number =
	                                     wfld._Fld-number 
	                         AND wflt._Event = as4dict.p__Trgfd._Event 
	                                     NO-LOCK NO-ERROR.
	IF AVAILABLE wflt AND 
	  wflt._Override = as4dict.p__Trgfd._Override AND
	  wflt._Proc-name = as4dict.p__Trgfd._Proc-name AND
	  wflt._Trig-CRC = as4dict.p__Trgfd._Trig-CRC THEN
	  NEXT.
	  
	RUN "prodict/_dctquot.p" (as4dict.p__Trgfd._Event,'"',OUTPUT c).
	j = j + 1. 
	ddl[j] = "  FIELD-TRIGGER " + c +
		 (IF as4dict.p__Trgfd._Override = "Y" THEN " OVERRIDE " 
						  ELSE " NO-OVERRIDE ").
	RUN "prodict/_dctquot.p" (as4dict.p__Trgfd._Proc-name,'"',OUTPUT c).
	ddl[j] = ddl[j] + "PROCEDURE " + c + " CRC """ 
		 + (IF as4dict.p__Trgfd._Trig-CRC = ? 
		    THEN "?" ELSE STRING(as4dict.p__Trgfd._Trig-CRC))
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
    END.	 /* end FOR EACH field-list */  
  
    /* note that there is no user interface for resolving renamed
    indexes.  this is because we can completely match indexes by their
    component lists, which are invariant once the index is created.  */
    ASSIGN
      pri1   = ""
      pri2   = "".
  
    /* build index component match list */
    FOR EACH widx WHERE widx._File-number =
                                        wfil._File-number NO-LOCK:
      DISPLAY widx._Index-name @ idx WITH FRAME seeking.
      c = STRING(widx._Unique,"u/a")
	+ (IF widx._Wordidx = 1 THEN "w" ELSE "f").
      FOR EACH wixf WHERE wixf._Idx-num =
                                          widx._Idx-num
                                   AND wixf._File-number =
                                          widx._File-number NO-LOCK:
	 FIND wfld WHERE wfld._File-number = 
	                                   widx._File-number 
	                            AND wfld._Fld-number =
	                                   wixf._Fld-number NO-LOCK.
	 FIND FIRST field-list
	    WHERE field-list.f2-name = wfld._Field-name NO-ERROR.
	 c = c + ","
	    + STRING(wixf._Ascending,"+/-")
	    + STRING(wixf._Abbreviate,"y/n")
	    + STRING(wfld._dtype)
	    + (IF AVAILABLE field-list THEN field-list.f2-name ELSE "*").
      END.
      CREATE index-list.
      ASSIGN
	index-list.i1-name = widx._Index-name
	index-list.i1-comp = c
	index-list.i1-i2   = FALSE.
      IF wfil._Prime-Index = widx._Idx-num THEN pri2 = c.
    END.
    FOR EACH as4dict.p__Index WHERE as4dict.p__Index._File-number =
                                       as4dict.p__File._File-number NO-LOCK:
      DISPLAY as4dict.p__Index._Index-name @ idx WITH FRAME seeking.
      c = STRING(as4dict.p__Index._Unique,"u/a")
	+ (IF as4dict.p__Index._Wordidx = 1 THEN "w" ELSE "f").
      FOR EACH as4dict.p__Idxfd WHERE as4dict.p__Idxfd._Idx-num =
                                          as4dict.p__Index._Idx-num
                                  AND as4dict.p__Idxfd._File-number =
                                          as4dict.p__Index._File-number NO-LOCK:
	  FIND as4dict.p__Field WHERE as4dict.p__Field._File-number =
	                                   as4dict.p__Index._File-number
	                           AND as4dict.p__Field._Fld-number =
	                                   as4dict.p__Idxfd._Fld-number NO-LOCK.
	c = c + ","
	  + STRING(as4dict.p__Idxfd._Ascending,"+/-")
	  + STRING(as4dict.p__Idxfd._Abbreviate,"y/n")
	  + STRING(as4dict.p__Field._dtype)
	  + as4dict.p__Field._Field-name.
      END.
      CREATE index-list.
      ASSIGN
	index-list.i1-name = as4dict.p__Index._Index-name
	index-list.i1-comp = c
	index-list.i1-i2   = TRUE.
      IF as4dict.p__File._Prime-Index = as4dict.p__Index._Idx-num THEN pri1 = c.
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
      FIND as4dict.p__Index WHERE as4dict.p__Index._File-number =
                                    as4dict.p__File._File-number
	                       AND as4dict.p__Index._Index-name = index-list.i1-name NO-LOCK.
      FIND widx WHERE widx._File-number =
                                    wfil._File-number
                               AND widx._Index-name = index-list.i2-name NO-LOCK.
      DISPLAY as4dict.p__Index._Index-name @ idx WITH FRAME wrking.
      IF as4dict.p__Index._Active = "N" AND widx._Active = "Y" THEN
	PUT STREAM ddl UNFORMATTED
	  'UPDATE INACTIVE INDEX "' as4dict.p__Index._Index-name
	  '" OF "' as4dict.p__File._File-name '"' SKIP.
      DELETE index-list.
    END.
  
    /* handle renamed indexes */
    ans = FALSE.
    FOR EACH index-list WHERE index-list.i2-name <> ?:
      FIND as4dict.p__Index WHERE as4dict.p__Index._File-number =
                                    as4dict.p__File._File-number
	                       AND as4dict.p__Index._Index-name = index-list.i1-name NO-LOCK.
      FIND widx WHERE widx._File-number =
                                      wfil._File-number
	                         AND widx._Index-name = index-list.i2-name NO-LOCK.
      ans = TRUE.
      DISPLAY index-list.i1-name @ idx WITH FRAME wrking.
      RUN Check_Index_Conflict.
      PUT STREAM ddl UNFORMATTED
	'RENAME INDEX "' index-list.i2-name
	'" TO "' index-list.i1-name
	'" ON "' as4dict.p__File._File-name '"' SKIP.
      IF as4dict.p__Index._Active = "N" AND widx._Active = "Y" THEN
	PUT STREAM ddl UNFORMATTED
	  'UPDATE INACTIVE INDEX "' as4dict.p__Index._Index-name
	  '" OF "' as4dict.p__File._File-name '"' SKIP.
      DELETE index-list.
    END.
    IF ans THEN PUT STREAM ddl UNFORMATTED SKIP(1).
  
    /* check if unique indexes to be created as inactive */
    FOR EACH index-list WHERE index-list.i1-i2,
      EACH as4dict.p__Index WHERE as4dict.p__Index._File-number =
                                     as4dict.p__File._File-number
                              AND as4dict.p__Index._Index-name = index-list.i1-name
                              AND as4dict.p__Index._Unique = "Y"
                              AND as4dict.p__Index._Active = "Y" NO-LOCK:
      iact = TRUE.
      RUN "prodict/user/_usrdbox.p" (INPUT-OUTPUT iact,?,?,new_lang[2]).
      LEAVE. /* we only need to ask once */
    END.
  
     
    /* handle new indexes */
    FOR EACH index-list WHERE index-list.i1-i2,
        EACH as4dict.p__Index WHERE as4dict.p__Index._File-number = as4dict.p__File._File-number
                                AND as4dict.p__Index._Index-name = index-list.i1-name NO-LOCK:
      IF as4dict.p__Index._Unique = "Y" AND iact = FALSE THEN.
      ELSE DO:  
        DISPLAY as4dict.p__Index._Index-name @ idx WITH FRAME wrking.
        RUN Check_Index_Conflict.
        PUT STREAM ddl UNFORMATTED "ADD "
	   'INDEX "' as4dict.p__Index._Index-Name
	   '" ON "' as4dict.p__File._File-name '"' SKIP.
        IF as4dict.p__Index._Unique = "Y" THEN 
	   PUT STREAM ddl UNFORMATTED "  UNIQUE" SKIP.
        IF as4dict.p__Index._Wordidx = 1 THEN 
	   PUT STREAM ddl UNFORMATTED "  WORD" SKIP.
        FOR EACH as4dict.p__Idxfd WHERE as4dict.p__Idxfd._Idx-num =
                                        as4dict.p__Index._Idx-num
                                    AND as4dict.p__Idxfd._File-number =
                                        as4dict.p__Index._File-number NO-LOCK
                               BREAK BY as4dict.p__Idxfd._Index-seq:

           FIND as4dict.p__Field WHERE as4dict.p__Field._File-number =
                                       as4dict.p__Index._File-number
                                   AND as4dict.p__Field._Fld-number =
                                       as4dict.p__Idxfd._Fld-number NO-LOCK.	                         
	    PUT STREAM ddl UNFORMATTED
	      '  INDEX-FIELD "' as4dict.p__Field._Field-Name '" '
	      (IF as4dict.p__Idxfd._Ascending = "Y" THEN "ASCENDING" ELSE "DESCENDING")
	      (IF as4dict.p__Idxfd._Abbreviate = "Y" THEN " ABBREVIATED" ELSE "")
	      (IF as4dict.p__Idxfd._Unsorted = "Y"  THEN " UNSORTED"    ELSE "") SKIP.
        END.
        PUT STREAM ddl UNFORMATTED SKIP(1).
      END.
    END.
  
    /* set primary index */
    RELEASE as4dict.p__Index.
    IF as4dict.p__File._Prime-Index <> ? THEN
      FIND as4dict.p__Index WHERE as4dict.p__Index._Idx-num = as4dict.p__File._Prime-Index
	 NO-LOCK NO-ERROR.
    IF AVAILABLE as4dict.p__Index AND pri1 <> pri2 THEN
      PUT STREAM ddl UNFORMATTED
	'UPDATE PRIMARY INDEX "' as4dict.p__Index._Index-name
	'" ON "' as4dict.p__File._File-name '"' SKIP(1).
  
    /* handle deleted indexes */
    ans = FALSE.
    FOR EACH index-list WHERE NOT index-list.i1-i2:
      ans = TRUE.
      DISPLAY index-list.i1-name @ idx WITH FRAME wrking.
      IF index-list.i1-name <> "default" THEN 
        PUT STREAM ddl UNFORMATTED
	  'DROP INDEX "' index-list.i1-name
	  '" ON "' as4dict.p__File._File-name '"' SKIP.
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
      DISPLAY missing.name @ fld WITH FRAME wrking.
      PUT STREAM ddl UNFORMATTED
	'DROP FIELD "' missing.name
	'" OF "' as4dict.p__File._File-name '"' SKIP.
      DELETE missing.
    END.
    IF ans THEN PUT STREAM ddl UNFORMATTED SKIP(1).
  
  
    DO ON ERROR UNDO,LEAVE ON ENDKEY UNDO,LEAVE:
      HIDE MESSAGE.
    END.
  END.  /* end FOR EACH potentially altered file */
  
  DISPLAY "" @ fil "" @ fld "" @ idx WITH FRAME seeking.
  DISPLAY "" @ fil "" @ fld "" @ idx WITH FRAME wrking.
  
  /* build missing sequence list for rename/delete determination */
  FOR EACH wseq NO-LOCK:
    FIND FIRST as4dict.p__Seq
      WHERE as4dict.p__Seq._Seq-name = wseq._Seq-name NO-LOCK NO-ERROR.
    DISPLAY wseq._Seq-name @ seq WITH FRAME seeking.
    IF AVAILABLE as4dict.p__Seq THEN NEXT.
    CREATE missing.
    missing.name = wseq._Seq-name.
  END.
  
  /* build list of new or renamed sequences */
  FOR EACH as4dict.p__Seq NO-LOCK:
    FIND FIRST wseq
      WHERE wseq._Seq-name = as4dict.p__Seq._Seq-name NO-LOCK NO-ERROR.
    DISPLAY as4dict.p__Seq._Seq-name @ seq WITH FRAME seeking.
    CREATE seq-list.
    seq-list.s1-name = as4dict.p__Seq._Seq-name.
    IF AVAILABLE wseq THEN
      seq-list.s2-name = as4dict.p__Seq._Seq-name.
  END.
  
  /* look for matches for renamed sequences with user input.  A prompt 
     is given for each seq that's not in as4dict but only when
     there is also a seq in as4dict that's not in temp table.  The 2nd list
     is the potential values to rename to.
  */
  run adecomm/_setcurs.p ("").
  FOR EACH missing:
    DISPLAY missing.name @ seq WITH FRAME seeking.
    RUN "as4dict/dump/_dmpisub.p"
      (INPUT "s",INPUT-OUTPUT missing.name,OUTPUT ans).
    IF missing.name = ? THEN DELETE missing.
    IF ans = ? THEN DO:
      HIDE FRAME wrking NO-PAUSE.
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
    DISPLAY missing.name @ seq WITH FRAME wrking.
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
    DISPLAY seq-list.s1-name @ seq WITH FRAME wrking.
  END.
  IF ans THEN PUT STREAM ddl UNFORMATTED SKIP(1).
  
  /* handle new or potentially altered sequences.  
     We can't use dumpdefs here like we do with files because it wasn't 
     made to handle individual sequences - it would dump them all.
     Some day!
  */
  FOR EACH seq-list:
    DISPLAY seq-list.s1-name @ seq WITH FRAME seeking.
  
    FIND as4dict.p__Seq WHERE as4dict.p__Seq._Seq-name = seq-list.s1-name NO-LOCK.
    FIND wseq WHERE wseq._Seq-name = seq-list.s2-name NO-LOCK NO-ERROR.
  
    DISPLAY seq-list.s1-name @ seq WITH FRAME wrking.
  
    /* If l is true we're updateing otherwise we're adding */
    l = AVAILABLE wseq.
  
    /* write out appropriate seq definition changes */
    ASSIGN
      j      = 1
      ddl    = ""
      ddl[1] = (IF l THEN "UPDATE" ELSE "ADD")
	       + ' SEQUENCE "' + as4dict.p__Seq._Seq-name + '"'.
      IF NOT l OR 
      	 as4dict.p__Seq._Seq-init <> wseq._Seq-init THEN 
	ASSIGN
	  j = j + 1
	  ddl[j] = "  INITIAL " + (IF as4dict.p__Seq._Seq-init = ? THEN "?"
		   ELSE STRING(as4dict.p__Seq._Seq-init)).
      IF NOT l OR 
      	 as4dict.p__Seq._Seq-incr <> wseq._Seq-incr THEN 
	ASSIGN
	  j = j + 1
	  ddl[j] = "  INCREMENT " + (IF as4dict.p__Seq._Seq-incr = ? THEN "?" 
		   ELSE STRING(as4dict.p__Seq._Seq-incr)).
      IF NOT l OR 
      	 as4dict.p__Seq._Cycle-OK <> wseq._Cycle-OK THEN 
	ASSIGN
	  j = j + 1
	  ddl[j] = "  CYCLE-ON-LIMIT " + 
		   (IF as4dict.p__Seq._Cycle-OK = "Y" THEN "yes" ELSE "no").
      IF NOT l OR as4dict.p__Seq._Seq-min <> wseq._Seq-min THEN 
	ASSIGN
	  j = j + 1
	  ddl[j] = "  MIN-VAL " + (IF as4dict.p__Seq._Seq-min = ? THEN "?" 
		   ELSE STRING(as4dict.p__Seq._Seq-min)).
      IF NOT l OR as4dict.p__Seq._Seq-max <> wseq._Seq-max THEN 
	ASSIGN
	  j = j + 1
	  ddl[j] = "  MAX-VAL " + (IF as4dict.p__Seq._Seq-max = ? THEN "?" 
		   ELSE STRING(as4dict.p__Seq._Seq-max)).
  
      /* don't write out ddl[1] if j = 1 (i.e., we only have seq header) */
      IF j > 1 THEN 
	DO i = 1 TO j + 1:
	  IF ddl[i] = "" THEN  /* this puts an extra skip after the last one */
	    PUT STREAM ddl UNFORMATTED SKIP(1).
	  ELSE
	    PUT STREAM ddl UNFORMATTED ddl[i] SKIP.
	END.
  
  END.  /* end FOR EACH new or potentially altered sequence */
  
 
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

HIDE FRAME wrking NO-PAUSE.
HIDE FRAME seeking NO-PAUSE.
SESSION:IMMEDIATE-DISPLAY = no.
run adecomm/_setcurs.p ("").
RETURN.





