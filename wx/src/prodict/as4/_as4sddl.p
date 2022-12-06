/*************************************************************/
/* Copyright (c) 1984-1994 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/* _as4sddl.p - AS/400 dump data definitions for V7 client */

{ prodict/dictvar.i }
{ prodict/user/uservar.i }

/*
in:  user_env[1] = containing comma-separated list of filenames in
		   current database, *only* if user_filename = "SOME".
     user_env[2] = Name of file to dump to.
     user_env[9] = "d" - dump defs 
			 - if user selected a specific table: translates into
			   "t" for dumpdefs.
			 - if user selected ALL: translates into "d","s"
			   and "t" for dumpdefs where "d" will output both
			   auto connect and collate/translate info.
		   "a" - auto connect 
		   "c" - collate/translate info
		   "s" - sequences.
*/
/* history

94/11/07    gfs         fixed HEADER and added code-page trailer support.
94/03/07    hutegger    changed "STOP to KBLABEL("STOP") in frame working.

*/


DEFINE VARIABLE Seqs AS CHARACTER NO-UNDO.
DEFINE VARIABLE Dbs  AS CHARACTER NO-UNDO.
DEFINE VARIABLE i    AS INTEGER   NO-UNDO. /* needed for dmptrail.i */

/* LANGUAGE DEPENDENCIES START */ /*----------------------------------------*/

FORM
  Dbs               LABEL "Database"  COLON 11 FORMAT "x(32)" SKIP
  _File._File-name  LABEL "Table"     COLON 11 FORMAT "x(32)" SKIP
/*Seqs              LABEL "Sequence"  COLON 11 FORMAT "x(32)" SKIP*/
  HEADER
    " Dumping Definitions.   Press " +
     KBLABEL("STOP") + " to terminate the dump process." format "x(70)"
  WITH FRAME working 
  ROW 4 CENTERED SIDE-LABELS ATTR-SPACE USE-TEXT.

COLOR DISPLAY MESSAGES
  _File._File-name Dbs /*Seqs*/
  WITH FRAME working.

IF TERMINAL <> "" THEN 
  DISPLAY
    ""   @ Dbs
    ""   @ _File._File-name
/*  ""   @ Seqs */
    WITH FRAME working.

/* LANGUAGE DEPENDENCIES END */ /*------------------------------------------*/

DEFINE VARIABLE stopped  AS LOGICAL NO-UNDO init true.
DEFINE VARIABLE file_len AS INTEGER NO-UNDO.
DEFINE NEW SHARED STREAM ddl.

IF TERMINAL <> "" THEN 
  run adecomm/_setcurs.p ("WAIT").
OUTPUT STREAM ddl TO VALUE(user_env[2]) NO-ECHO NO-MAP.
SESSION:IMMEDIATE-DISPLAY = yes.

DO ON STOP UNDO, LEAVE:
  IF user_env[9] = "a" THEN DO:
    IF TERMINAL <> "" THEN 
      DISPLAY "(Auto-Connect)" @ Dbs WITH FRAME working.
    RUN "prodict/dump/_dmpdefs.p" ("a",drec_db).
  END.
  ELSE IF user_env[9] = "c" THEN DO:
    IF TERMINAL <> "" THEN 
      DISPLAY user_dbname + " (Collate/Translate)" @ Dbs WITH FRAME working.
    RUN "prodict/dump/_dmpdefs.p" ("c",drec_db).
  END.
/*ELSE IF user_env[9] = "s" THEN DO:
    IF TERMINAL <> "" THEN 
      DISPLAY user_dbname @ Dbs 
	"ALL" @ Seqs 
	WITH FRAME working.
    RUN "prodict/dump/_dmpdefs.p" ("s",drec_db).
  END. */
  ELSE
    FOR EACH DICTDB._File
      WHERE _File._Db-recid = drec_db
       AND (
	 IF user_filename = "ALL" THEN
	   NOT DICTDB._File._Hidden
	 ELSE
	 IF user_filename = "SOME" THEN
	   CAN-DO(user_env[1],DICTDB._File._File-name)
	 ELSE
	   RECID(DICTDB._File) = drec_file)
      BREAK BY DICTDB._File._File-num:
  
      IF FIRST(DICTDB._File._File-num) AND user_filename = "ALL" THEN DO:
	IF TERMINAL <> "" THEN
	  DISPLAY "ALL" @ Dbs
	    WITH FRAME working.
     /* RUN "prodict/as4/_as4defs.p" ("d",drec_db). */
  
     /* IF TERMINAL <> "" THEN
	  DISPLAY user_dbname @ Dbs
	    "ALL" @ Seqs
	    WITH FRAME working.
	RUN "prodict/dump/_dmpdefs.p" ("s",drec_db). */
      END.
      ELSE
	IF TERMINAL <> "" THEN
	  DISPLAY user_dbname @ Dbs
	    WITH FRAME working.
  
      IF TERMINAL <> "" THEN 
	DISPLAY _File._File-name WITH FRAME working.
      RUN "prodict/as4/_as4defs.p" ("t",RECID(_File)).
    END. /* for each _file */

    { prodict/dump/dmptrail.i
          &entries     = " "
          &seek-stream = "ddl"
          &stream      = "stream ddl"
    } /* add trailer */
  stopped = false.
END.

file_len = SEEK(ddl).
OUTPUT STREAM ddl CLOSE.
SESSION:IMMEDIATE-DISPLAY = no.
IF TERMINAL <> "" THEN 
  run adecomm/_setcurs.p ("").

IF stopped THEN 
   MESSAGE "Dump terminated."
	   VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
ELSE DO:
  IF file_len < 3 THEN /* the file is empty - nothing to dump */
  DO:
    OS-DELETE VALUE(user_env[2]). 
    IF TERMINAL <> "" THEN 
      MESSAGE "There were no " +  
	      (IF user_env[9] = "a" THEN "auto-connect records" ELSE
	       IF user_env[9] = "s" THEN "sequence definitions" ELSE
				   "data definitions") + 
	      " to dump." SKIP
	      "The output file has been deleted."
	      VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
  END.
  ELSE 
    IF TERMINAL <> "" THEN 
      MESSAGE "Dump of definitions completed." 
	      VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
END.
IF TERMINAL <> "" THEN 
  HIDE FRAME working NO-PAUSE.

RETURN.


