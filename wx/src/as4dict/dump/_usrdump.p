/*************************************************************/
/* Copyright (c) 1984-1995 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/* userdump - user interface and defaults setup for dumping */
/*            As/400 definitions or data from the P__ files */
/*            which reside in the Progress/400 dictionary.  */
/*
IN:
  user_env[1] = "ALL" or comma-separated list of files
  user_env[9] = type of dump (e.g. .df or .d or bulk-load file)
		"4" = dump file definitions with AS/400 stuff
		"d" = dump definitions
		"f" = dump data file contents
		"s" = dump sequence definitions
		"k" = dump sequence values
		"u" = dump user file contents
		"v" = dump view file contents

OUT:
  user_env[1] = same as IN
  user_env[2] = physical file or directory name for some output
  user_env[3] = "MAP <name>" or "NO-MAP" OR ""
  user_env[4] = comma separated list of "y" (yes) or "n" (no) which
		corresponds to file list in user_env[1], indicating for each 
		whether triggers should be disabled when the dump is done.
		(only used for dump data file contents, "f").
  user_env[5] = "<source-code-page>,<target-code-page>" or "UNDEFINED"
                (only for .df- and .d-files!)  /*hutegger*/
  user_env[9] = same as IN

history:
      Nhorn   5/01/95   Initial Creation
         
*/
/*h-*/

{ as4dict/dictvar.i shared}
{ as4dict/dump/dumpvar.i shared}

DEFINE VARIABLE answer   AS LOGICAL    NO-UNDO.
DEFINE VARIABLE canned   AS LOGICAL    NO-UNDO INITIAL TRUE.
DEFINE VARIABLE base     AS CHARACTER  NO-UNDO.
DEFINE VARIABLE class    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE comma    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE dump-as  AS CHARACTER  NO-UNDO.
DEFINE VARIABLE err      AS CHARACTER  NO-UNDO.
DEFINE VARIABLE i        AS INTEGER    NO-UNDO.
DEFINE VARIABLE io-file  AS LOGICAL    NO-UNDO.
DEFINE VARIABLE io-mapc  AS CHARACTER  NO-UNDO.
DEFINE VARIABLE io-mapl  AS INTEGER    NO-UNDO INITIAL 0.
DEFINE VARIABLE io-title AS CHARACTER  NO-UNDO.
DEFINE VARIABLE is-all   AS LOGICAL    NO-UNDO.
DEFINE VARIABLE is-one   AS LOGICAL    NO-UNDO.
DEFINE VARIABLE is-some  AS LOGICAL    NO-UNDO.
DEFINE VARIABLE msg-num  AS INTEGER    NO-UNDO INITIAL 0.
DEFINE VARIABLE nodump   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE prefix   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE trash    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE dis_trig AS CHARACTER  NO-UNDO.

{prodict/misc/filesbtn.i}

FORM SKIP({&TFM_WID})
  user_env[2] {&STDPH_FILL} FORMAT "x(80)" COLON 12 VIEW-AS FILL-IN SIZE 40 BY 1
	 LABEL "Output File"
  btn_File                                        SKIP ({&VM_WIDG})
  user_env[5] {&STDPH_FILL} FORMAT "x(80)" COLON 12 VIEW-AS FILL-IN SIZE 40 BY 1
	 LABEL "Code Page"
  {prodict/user/userbtns.i}
  WITH FRAME write-def-file
  SIDE-LABELS ATTR-SPACE CENTERED 
  DEFAULT-BUTTON btn_OK CANCEL-BUTTON btn_Cancel
  VIEW-AS DIALOG-BOX TITLE " " + io-title + " ".

FORM SKIP({&TFM_WID})
  user_env[2] {&STDPH_FILL} FORMAT "x(80)" COLON 12 VIEW-AS FILL-IN SIZE 40 BY 1
	 LABEL "Output File"
  btn_File                                        SKIP ({&VM_WIDG})
  user_env[5] {&STDPH_FILL} FORMAT "x(80)" COLON 12 VIEW-AS FILL-IN SIZE 40 BY 1
	 LABEL "Code Page"
  {prodict/user/userbtns.i}
  WITH FRAME write-output-file
  SIDE-LABELS ATTR-SPACE CENTERED 
  DEFAULT-BUTTON btn_OK CANCEL-BUTTON btn_Cancel
  VIEW-AS DIALOG-BOX TITLE " " + io-title + " ".

&IF "{&WINDOW-SYSTEM}" <> "TTY" &THEN 
 
  &IF "{&WINDOW-SYSTEM}" = "OSF/Motif" &THEN  
    &GLOBAL-DEFINE RADBTNVOFF 1.2
    &GLOBAL-DEFINE RADBTNHOFF 13
    &GLOBAL-DEFINE FILLCH 55
  &ELSE
    &GLOBAL-DEFINE RADBTNVOFF 0.9
    &GLOBAL-DEFINE RADBTNHOFF 12
    &GLOBAL-DEFINE FILLCH 47
  &ENDIF

  FORM SKIP({&TFM_WID})
    "Output File:" VIEW-AS TEXT AT 2
    user_env[2] {&STDPH_FILL} FORMAT "x(80)" AT 2 VIEW-AS FILL-IN SIZE 40 BY 1
    btn_File                                        SKIP ({&VM_WIDG})
    "Character Mapping:" VIEW-AS TEXT AT 2
    io-mapl VIEW-AS RADIO-SET
	RADIO-BUTTONS "NO-MAP", 1, "Map", 2 AT 2    SKIP ({&VM_WIDG})
    "Code Page:" VIEW-AS TEXT AT 2
    user_env[5] {&STDPH_FILL} FORMAT "x(80)" AT 2 VIEW-AS FILL-IN SIZE 40 BY 1
   {prodict/user/userbtns.i}
    io-mapc {&STDPH_FILL} FORMAT "X(20)"
	AT ROW-OF io-mapl + {&RADBTNVOFF} COLUMN-OF io-mapl + {&RADBTNHOFF}
    WITH FRAME write-dump-file
    NO-LABELS NO-ATTR-SPACE CENTERED 
    DEFAULT-BUTTON btn_OK CANCEL-BUTTON btn_Cancel
    VIEW-AS DIALOG-BOX TITLE " " + io-title + " ".

  FORM SKIP({&TFM_WID})
    "Output Directory (if different from current directory):" VIEW-AS TEXT AT 2
    user_env[2] {&STDPH_FILL} FORMAT "x(80)" AT 2 VIEW-AS FILL-IN 
	SIZE {&FILLCH} BY 1                         SKIP({&VM_WIDG})
    "Character Mapping:" VIEW-AS TEXT AT 2
    io-mapl VIEW-AS RADIO-SET
	RADIO-BUTTONS "NO-MAP", 1, "Map", 2 AT 2    SKIP ({&VM_WIDG})
    "Code Page:" VIEW-AS TEXT AT 2
    user_env[5] {&STDPH_FILL} FORMAT "x(80)" AT 2 VIEW-AS FILL-IN SIZE 40 BY 1
    {prodict/user/userbtns.i}
    io-mapc {&STDPH_FILL} FORMAT "X(20)"
	AT ROW-OF io-mapl + {&RADBTNVOFF} COLUMN-OF io-mapl + {&RADBTNHOFF}
    WITH FRAME write-dump-dir
    NO-LABELS NO-ATTR-SPACE CENTERED 
    DEFAULT-BUTTON btn_OK CANCEL-BUTTON btn_Cancel
    VIEW-AS DIALOG-BOX TITLE " " + io-title + " ".
&ELSE       /* tty */
  FORM SKIP({&TFM_WID})
    user_env[2] {&STDPH_FILL} FORMAT "x(80)" COLON 19 VIEW-AS FILL-IN SIZE 40 BY 1
	 LABEL "Output File"
    btn_File                                         SKIP ({&VM_WIDG})
    io-mapc {&STDPH_FILL} LABEL "Character Mapping"
	FORMAT "x(20)" COLON 19
    "(Blank = Default Map; use" COLON 41
    "~"NO-MAP~" to turn off mapping)" COLON 41       SKIP ({&VM_WIDG})
    user_env[5] {&STDPH_FILL} FORMAT "x(80)" COLON 19 VIEW-AS FILL-IN SIZE 40 BY 1
	 LABEL "Code Page"
    {prodict/user/userbtns.i}
    WITH FRAME write-dump-file
    NO-LABELS SIDE-LABELS NO-ATTR-SPACE CENTERED 
    DEFAULT-BUTTON btn_OK CANCEL-BUTTON btn_Cancel
    VIEW-AS DIALOG-BOX TITLE " " + io-title + " ".

  FORM SKIP({&TFM_WID})
    user_env[2] {&STDPH_FILL} FORMAT "x(80)" COLON 19 VIEW-AS FILL-IN SIZE 50 BY 1
	 LABEL "Output Directory"                    SKIP ({&VM_WIDG})
    "(Blank = Current Directory)" VIEW-AS TEXT COLON 19 SKIP ({&VM_WIDG})
    io-mapc {&STDPH_FILL} LABEL "Character Mapping"
	FORMAT "x(20)" COLON 19
    "(Blank = Default Map; use" COLON 41
    "~"NO-MAP~" to turn off mapping)" COLON 41       SKIP ({&VM_WIDG})
    user_env[5] {&STDPH_FILL} FORMAT "x(80)" COLON 19 VIEW-AS FILL-IN SIZE 40 BY 1
	 LABEL "Code Page"
    {prodict/user/userbtns.i}
    WITH FRAME write-dump-dir
    NO-LABELS SIDE-LABELS NO-ATTR-SPACE CENTERED 
    DEFAULT-BUTTON btn_OK CANCEL-BUTTON btn_Cancel
    VIEW-AS DIALOG-BOX TITLE " " + io-title + " ".
&ENDIF


/*===============================Triggers=================================*/

&IF "{&WINDOW-SYSTEM}" <> "TTY" &THEN 
/*----- HELP -----*/
/*Note: may be overridden below for bulk loader stuff*/
on HELP of frame write-output-file
   or CHOOSE of btn_Help in frame write-output-file
   RUN "adecomm/_adehelp.p" (INPUT "as4d", INPUT "CONTEXT", 
			     INPUT {&AS4_Dump_Definitions_Dlg_Box},
			     INPUT ?).

on HELP of frame write-def-file 
   or CHOOSE of btn_Help in frame write-def-file
   RUN "adecomm/_adehelp.p" (INPUT "as4d", INPUT "CONTEXT", 
			     INPUT {&AS4_Dump_Definitions_Dlg_Box},
			     INPUT ?).

on HELP of frame write-dump-file 
   or CHOOSE of btn_Help in frame write-dump-file
   RUN "adecomm/_adehelp.p" (INPUT "as4d", INPUT "CONTEXT", 
			     INPUT {&Dump_Data_Contents_Dlg_Box},
			     INPUT ?).

on HELP of frame write-dump-dir
   or CHOOSE of btn_Help in frame write-dump-dir
   RUN "adecomm/_adehelp.p" (INPUT "as4d", INPUT "CONTEXT", 
			     INPUT {&Dump_Data_Contents_Some_Dlg_Box},
			     INPUT ?).
&ENDIF


/*----- ON GO or OK -----*/
ON GO OF FRAME write-output-file
DO:
  DEFINE VAR basename AS CHAR NO-UNDO.
  DEFINE VAR fil      AS CHAR NO-UNDO.
  DEFINE VAR prefix   AS CHAR NO-UNDO.

  fil = user_env[2]:SCREEN-VALUE IN FRAME write-output-file.
  
  RUN prodict/misc/osprefix.p
    ( INPUT  fil,
      OUTPUT prefix,
      OUTPUT basename
      ).
  if basename = fil then assign fil = "./" + fil.
  
  IF /*io-file AND*/ (SEARCH(fil) <> ?) THEN DO:
    answer = FALSE.
    MESSAGE "A file already exists with the name:" SKIP fil SKIP(1) "Overwrite?"
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE answer.
    IF NOT answer THEN 
    DO:
      APPLY "ENTRY" TO user_env[2] IN FRAME write-output-file.
      RETURN NO-APPLY.
    END.
  END.
END.

ON GO OF FRAME write-def-file
DO:
  DEFINE VAR basename AS CHAR NO-UNDO.
  DEFINE VAR fil      AS CHAR NO-UNDO.
  DEFINE VAR prefix   AS CHAR NO-UNDO.

  fil = user_env[2]:SCREEN-VALUE IN FRAME write-def-file.

  RUN prodict/misc/osprefix.p
    ( INPUT  fil,
      OUTPUT prefix,
      OUTPUT basename
      ).
  if basename = fil then assign fil = "./" + fil.
  
  IF /*io-file AND*/ (SEARCH(fil) <> ?) THEN DO:
    answer = FALSE.
    MESSAGE "A file already exists with the name:" SKIP fil SKIP(1) "Overwrite?"
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE answer.
    IF NOT answer THEN 
    DO:
      APPLY "ENTRY" TO user_env[2] IN FRAME write-def-file.
      RETURN NO-APPLY.
    END.
  END.
END.

ON GO OF FRAME write-dump-file
DO:
  DEFINE VAR basename AS CHAR NO-UNDO.
  DEFINE VAR fil      AS CHAR NO-UNDO.
  DEFINE VAR prefix   AS CHAR NO-UNDO.

  io-mapc = io-mapc:SCREEN-VALUE. /*To cover GUI case where not in UPDATE*/
  fil = user_env[2]:SCREEN-VALUE IN FRAME write-dump-file.

  RUN prodict/misc/osprefix.p
    ( INPUT  fil,
      OUTPUT prefix,
      OUTPUT basename
      ).
  if basename = fil then assign fil = "./" + fil.
  
  IF io-file AND (SEARCH(fil) <> ?) THEN DO:
    answer = FALSE.
    MESSAGE "A file already exists with the name:" SKIP fil SKIP(1) "Overwrite?"
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE answer.
    IF NOT answer THEN 
    DO:
      APPLY "ENTRY" TO user_env[2] IN FRAME write-dump-file.
      RETURN NO-APPLY.
    END.
  END.
END.

ON GO OF FRAME write-dump-dir
DO:
  DEFINE VAR dir   AS CHAR    NO-UNDO.
  DEFINE VAR exist AS LOGICAL NO-UNDO.
  DEFINE VAR fil   AS CHAR    NO-UNDO.
  DEFINE VAR i     AS INTEGER NO-UNDO.

  assign
    dir     = user_env[2]:SCREEN-VALUE IN FRAME write-dump-dir
    exist   = false
    io-mapc = io-mapc:SCREEN-VALUE. /*To cover GUI case where not in UPDATE*/

  if dir = "" then assign dir = "./".
  
  DO i = 1 to NUM-ENTRIES(user_env[1])
    WHILE NOT exist:
    ASSIGN fil = dir + ENTRY(i,user_env[1]) + ".d".
    IF (SEARCH(fil) <> ?) THEN ASSIGN exist = true.
  END.
    
  IF exist THEN DO:
    answer = FALSE.
    MESSAGE "One or more of the files already exist in this directory." SKIP 
            "Overwrite them?"
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE answer.
    IF NOT answer THEN 
    DO:
      APPLY "ENTRY" TO user_env[2] IN FRAME write-dump-dir.
      RETURN NO-APPLY.
    END.
  END.
END.

/*----- ON WINDOW-CLOSE -----*/
on WINDOW-CLOSE of frame write-output-file
   apply "END-ERROR" to frame write-output-file.
on WINDOW-CLOSE of frame write-dump-file
   apply "END-ERROR" to frame write-dump-file.
on WINDOW-CLOSE of frame write-dump-dir
   apply "END-ERROR" to frame write-dump-dir.

/*----- LEAVE of fill-ins: trim blanks the user may have typed in filenames---*/
ON LEAVE OF user_env[2] in frame write-output-file
   user_env[2]:screen-value in frame write-output-file = 
	TRIM(user_env[2]:screen-value in frame write-output-file).
ON LEAVE OF user_env[2] in frame write-dump-file
   user_env[2]:screen-value in frame write-dump-file = 
	TRIM(user_env[2]:screen-value in frame write-dump-file).
ON LEAVE OF user_env[2] in frame write-dump-dir
   user_env[2]:screen-value in frame write-dump-dir = 
	TRIM(user_env[2]:screen-value in frame write-dump-dir).
/* code-page-stuff    <hutegger> 94/02 */
  
{prodict/user/usrdump1.i
  &frame    = "write-dump-dir"
  &variable = "user_env[5]"
  }  /* checks if user_env[5] contains convertable code-page */
  
{prodict/user/usrdump1.i
  &frame    = "write-dump-file"
  &variable = "user_env[5]"
  }  /* checks if user_env[5] contains convertable code-page */
  
{prodict/user/usrdump1.i
  &frame    = "write-output-file"
  &variable = "user_env[5]"
  }  /* checks if user_env[5] contains convertable code-page */
  
  
  
/*----- HIT of FILE BUTTON -----*/
ON CHOOSE OF btn_File in frame write-output-file DO:
   RUN "prodict/misc/_filebtn.p"
       (INPUT user_env[2]:handle in frame write-output-file /*Fillin*/,
	INPUT "Find Output File"  /*Title*/,
	INPUT ""                 /*Filter*/,
	INPUT no                 /*Must exist*/).
END.
ON CHOOSE OF btn_File in frame write-dump-file DO:
   RUN "prodict/misc/_filebtn.p"
       (INPUT user_env[2]:handle in frame write-dump-file /*Fillin*/,
	INPUT "Find Output File"  /*Title*/,
	INPUT ""                 /*Filter*/,
	INPUT no                 /*Must exist*/).
END.

ON CHOOSE OF btn_File in frame write-def-file DO:
   RUN "prodict/misc/_filebtn.p"
       (INPUT user_env[2]:handle in frame write-def-file /*Fillin*/,
	INPUT "Find Output File"  /*Title*/,
	INPUT ""                 /*Filter*/,
	INPUT no                 /*Must exist*/).
END.

&IF "{&WINDOW-SYSTEM}" <> "TTY" &THEN  
ON VALUE-CHANGED OF io-mapl IN FRAME write-dump-file DO:
   DEFINE VARIABLE dummyl AS LOGICAL NO-UNDO.

   IF io-mapl:SCREEN-VALUE = "1" THEN DO:
	io-mapc:SENSITIVE = FALSE.
	io-mapc:SCREEN-VALUE = "".
   END.
   ELSE DO:
	io-mapc = "".
	io-mapc:SENSITIVE = TRUE.
	dummyl = io-mapc:MOVE-AFTER-TAB-ITEM(io-mapl:HANDLE).
   END.
END.
ON VALUE-CHANGED OF io-mapl IN FRAME write-dump-dir DO:
   DEFINE VARIABLE dummyl AS LOGICAL NO-UNDO.

   IF io-mapl:SCREEN-VALUE = "1" THEN DO:
	io-mapc:SENSITIVE = FALSE.
	io-mapc:SCREEN-VALUE = "".
   END.
   ELSE DO:
	io-mapc = "".
	io-mapc:SENSITIVE = TRUE.
	dummyl = io-mapc:MOVE-AFTER-TAB-ITEM(io-mapl:HANDLE).
   END.
END.
&ENDIF
/*============================Mainline code===============================*/
   
ASSIGN
&IF "{&WINDOW-SYSTEM}" = "TTY" &THEN 
  io-mapc  = "NO-MAP"
&ENDIF
  class    = SUBSTRING(user_env[9],1,1)
  io-file  = TRUE
  is-all   = (user_env[1] = "ALL")
  is-some  = (user_env[1] MATCHES "*,*")
  is-one   = NOT is-all AND NOT is-some
  prefix   = "".

/* Set default value for codepage gfs:94-04-28-043 */

IF SESSION:CHARSET = "UNDEFINED" THEN 
DO:
    FIND FIRST as4dict.p__db.
    ASSIGN user_env[5]  = p__db._db-xl-name. /* db codepage */
END.
ELSE IF SESSION:STREAM  = "UNDEFINED" THEN
    ASSIGN user_env[5]  = SESSION:CHARSET.
ELSE ASSIGN user_env[5] = SESSION:STREAM.
ASSIGN user_env[5]:SENSITIVE = true.
                                            
IF user_env[1] <> "" AND NOT is-all AND NOT is-some THEN DO FOR _File:
  FIND as4dict.p__File WHERE as4dict.p__file._File-name = user_env[1].
  dump-as = (IF as4dict.p__File._Dump-name = ? OR as4dict.p__File._Dump-name = ""
	    THEN SUBSTRING(as4dict.p__File._File-name,1,8)
	    ELSE as4dict.p__File._Dump-name).
END.

/*----------------------------------------------*/ /* DUMP FILE DEFINITIONS */
IF class = "d" OR class = "4" THEN DO FOR _File:
/*  Not needed  ------------------
  FIND _File "p__Db".
  IF NOT CAN-DO(_Can-read,USERID("as4dict")) THEN msg-num = 3.
  FIND _File "p__File".
  IF NOT CAN-DO(_Can-read,USERID("as4dict")) THEN msg-num = 3.
  FIND _File "p__Field".
  IF NOT CAN-DO(_Can-read,USERID("as4dict")) THEN msg-num = 3.
  FIND _File "p__Index".
  IF NOT CAN-DO(_Can-read,USERID("as4dict")) THEN msg-num = 3.
  -----------------------------------  */
  IF is-all OR is-some 
   then do:
    if PDBNAME(user_dbname) <> ?
     THEN RUN "prodict/misc/osprefix.p"
             ( INPUT PDBNAME(user_dbname),
               OUTPUT trash,
               OUTPUT dump-as
               ).
      else assign dump-as = user_dbname.
     end.          
  ASSIGN
    user_env[2] = (IF class = "4" THEN "_trgdefs.df"
		  ELSE prefix + dump-as + ".df")
    io-title    = (IF class = "4" THEN "Dump Database Triggers for "
		  ELSE "Dump Data Definitions for ")
		+ (IF is-all THEN "All Tables"
		  ELSE IF is-some THEN "Some Tables"
		  ELSE "Table" + ' "' + user_env[1] + '"').

END.

/*--------------------------------------------*/ /* DUMP DATA FILE CONTENTS */
ELSE IF class = "f" THEN DO FOR _File:

/* We don't need this since we already know what db type we have */
/*   nodump = "u".
  { prodict/dictgate.i &action=undumpload
    &dbtype=user_dbtype &dbrec=drec_db &output=nodump
  }      */

/*------------------- not needed ------
  FIND _File "p__File".
  IF NOT CAN-DO(_Can-read,USERID("as4dict")) THEN msg-num = 3.
--------------------------------------*/
  ASSIGN
    user_env[2] = prefix + (IF is-all OR is-some THEN "" ELSE dump-as + ".d")
    io-file     = NOT is-all AND NOT is-some
    io-title    = "Dump Data Contents for "
		+ (IF is-all THEN "All Tables"
		  ELSE IF is-some THEN "Some Tables"
		  ELSE "Table" + ' "' + user_env[1] + '"')
    base        = (IF is-one OR is-some THEN user_env[1] ELSE "")
    user_env[1] = ""
    comma       = ""
    i           = 1.

  /* If user had selected ALL, fill base array with list of files.  
     Otherwise, it is already set to file list.
  */
  IF is-all THEN FOR EACH as4dict.p__File
    WHERE p__File._File-number > 0
      AND p__File._Hidden <> "Y" 
      AND CAN-DO(p__File._Can-read,USERID(user_dbname))
      AND CAN-DO(p__File._Can-dump,USERID(user_dbname))
      AND (nodump = "" OR NOT CAN-DO(nodump,p__File._For-type))
    BY p__File._File-name:
    base = base + (IF base = "" THEN "" ELSE ",") + p__File._File-name.
  END.

  /* Run through the file list and cull out the ones that the user
     is not allowed to dump for some reason.
  */
  user_env[4] = "".
  DO i = 1 TO NUM-ENTRIES(base):
    err = ?.
    dis_trig = "y".
    FIND as4dict.p__File
      WHERE as4dict.p__File._File-name = ENTRY(i,base).
    IF NOT CAN-DO(as4dict.p__File._Can-read,USERID(user_dbname)) OR 
       NOT CAN-DO(as4dict.p__File._Can-dump,USERID(user_dbname)) THEN
      err = as4dict.p__File._File-name
	+ " will not be dumped due to insufficient privileges.".
    ELSE 
    IF nodump <> "" AND CAN-DO(nodump,as4dict.p__File._For-type) THEN
      err = "".
	/*SUBSTITUTE("&1 is a &2 &3 and cannot be dumped.",
	  p__File._File-name,user_dbtype,p__File._For-type).*/
    ELSE DO:
      {prodict/dump/dtrigchk.i &OK = answer}
      IF NOT answer THEN DO:
	 MESSAGE "You do not have privileges to dump table" as4dict.p__File._File-name 
		 SKIP
		 "with triggers disabled.  Do you want to dump this table" SKIP
		 "anyway even though the FIND trigger will fire?"
		 VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO-CANCEL UPDATE answer.
	 IF answer = no THEN
	    err = "".  /* don't do this table but no more messages */
	 ELSE IF answer = yes THEN DO:
	    err = ?.   /* include this table in dump */
	    dis_trig = "n".
	 END.
	 ELSE DO:    /* cancel the dump operation */
	    user_path = "".
	    user_cancel = yes.
	    RETURN.
	 END.
      END.
    END.
    IF err = ? THEN
      ASSIGN
	user_env[1] = user_env[1] + comma + as4dict.p__File._File-name
	user_env[4] = user_env[4] + comma + dis_trig
	comma       = ",".
    ELSE IF err <> "" THEN DO:
      answer = TRUE.
      IF NUM-ENTRIES(base) = 1 THEN DO:
         MESSAGE err VIEW-AS ALERT-BOX ERROR BUTTONS OK.
         user_path = "".
         user_cancel = yes.
         RETURN.
      END.
      ELSE DO:
         MESSAGE err SKIP "Do you want to continue?"
	    VIEW-AS ALERT-BOX ERROR BUTTONS YES-NO UPDATE answer.
         IF answer = FALSE THEN DO:
	    user_path = "".
	    user_cancel = yes.
	    return.
         END.
      END.
    END.
  END.  /* end of DO i = 2 TO NUM-ENTRIES(base) */

  /* subsequent removal of files changed from many to one, so reset ui stuff */
  IF (is-some OR is-all) AND NUM-ENTRIES(user_env[1]) = 1 THEN DO:
    FIND as4dict.p__File WHERE
	as4dict.p__File._File-name = user_env[1].
    ASSIGN
      is-some     = FALSE
      is-all      = FALSE
      is-one      = TRUE
      user_env[2] = prefix + (IF as4dict.p__File._Dump-name = ?
		    THEN as4dict.p__File._File-name ELSE as4dict.p__File._Dump-name)
		  + ".d"
      io-file     = TRUE
      io-title    = "Dump Data Contents for Table ~"" + user_env[1] + "~""
      base        = user_env[1].
  END.

END.

/*-----------------------------------------------------*/ /* DUMP SEQ DEFS */
ELSE IF class = "s" THEN DO FOR _File:
/* ------------------------------------------- 
   FIND _File "p__Seq".
  IF NOT CAN-DO(_Can-read,USERID("DICTDB")) THEN msg-num = 3.  
  IF user_dbtype <> "PROGRESS" THEN msg-num = 1.
  --------------------------------------------------*/
  ASSIGN
    user_env[2] = prefix + "_seqdefs.df"
    io-title    = "Dump Sequence Definitions".
END.

/*-----------------------------------------------------*/ /* DUMP SEQ VALS */
ELSE IF class = "k" THEN DO FOR _File:
/*-------------------------------------------- take out 
  FIND _File "p__Seq".
  IF NOT CAN-DO(_Can-read,USERID("DICTDB")) THEN msg-num = 3.
  IF user_dbtype <> "PROGRESS" THEN msg-num = 1.
----------------------------------------------------*/
  ASSIGN
    user_env[2] = prefix + "_seqvals.d"
    io-title    = "Dump Sequence Current Values".
END.
/*--------------------------------------------*/ /* DUMP USER FILE CONTENTS */
ELSE IF class = "u" THEN DO FOR _File:
/* --------------------------------------------
  FIND _File "p__User".
  IF NOT CAN-DO(_Can-read,USERID("DICTDB")) THEN msg-num = 3.
  IF user_dbtype <> "PROGRESS" THEN msg-num = 1.
------------------------------------------------*/
  ASSIGN
    user_env[2] = prefix + "_user.d"
    io-title    = "Dump User Table Contents".
END.
/*--------------------------------------------*/ /* DUMP VIEW FILE CONTENTS */
ELSE IF class = "v" THEN DO FOR _File:
/*---------------------------------------------
  FIND _File "p__View".
  IF NOT CAN-DO(_Can-read,USERID("DICTDB")) THEN msg-num = 3.
  IF user_dbtype <> "PROGRESS" THEN msg-num = 2.
-----------------------------------------------*/
  ASSIGN
    user_env[2] = prefix + "_view.d"
    io-title    = "Dump SQL Views".
END.


IF msg-num > 0 THEN DO:
  MESSAGE (
    IF      msg-num = 1 THEN
      "Cannot dump User information from a non-PROGRESS database."
    ELSE IF msg-num = 2 THEN
      "Cannot dump View information from a non-PROGRESS database."
    ELSE IF msg-num = 3 THEN
      "You do not have permission to use this option."
    ELSE IF msg-num = 4 THEN
      "You can only dump definitions in V5 format for PROGRESS databases."
    ELSE /*IF msg-num = 5 THEN*/
      "You can only create bulkload description files for PROGRESS databases.")
    VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  user_path = "".
  user_cancel = yes.
  RETURN.
END.

/* Adjust the graphical rectangle and the ok and cancel buttons */
IF class = "f" THEN DO:
   IF io-file THEN DO:
     {adecomm/okrun.i  
	&FRAME  = "FRAME write-dump-file" 
	&BOX    = "rect_Btns"
	&OK     = "btn_OK" 
	{&CAN_BTN}
	{&HLP_BTN}
     }
   END.
   ELSE DO:
     {adecomm/okrun.i  
	&FRAME  = "FRAME write-dump-dir" 
	&BOX    = "rect_Btns"
	&OK     = "btn_OK" 
	{&CAN_BTN}
	{&HLP_BTN}
     }
   END.
END.
ELSE DO:
  {adecomm/okrun.i  
     &FRAME  = "FRAME write-output-file" 
     &BOX    = "rect_Btns"
     &OK     = "btn_OK" 
     {&CAN_BTN}
     {&HLP_BTN}
  }
END.

IF    class = "5"  /* version 5 .df */
   or class = "k"  /* sequence def's */
   or class = "s"  /* sequence-values */
   THEN DO:
     {adecomm/okrun.i  
       &FRAME  = "FRAME write-def-file" 
       &BOX    = "rect_Btns"
       &OK     = "btn_OK" 
       {&CAN_BTN}
       {&HLP_BTN}
     }  
END.
 
PAUSE 0.
user_env[3] = "".

/* If name contains the .db, remove it */
IF INDEX(user_env[2],".db") > 0 THEN
	SUBSTRING(user_env[2],INDEX(user_env[2],".db"),3,"RAW") = "".

DO ON ERROR UNDO,RETRY ON ENDKEY UNDO,LEAVE:
   IF class = "k"  /* sequence def's */
   or class = "s"  /* sequence-values */
    THEN UPDATE user_env[2] 
	   btn_File
	   user_env[5]
	   btn_OK 
	   btn_Cancel
	   {&HLP_BTN_NAME}
	   WITH FRAME write-def-file.
  ELSE IF class <> "f" 
    THEN UPDATE user_env[2] 
	   btn_File
	   user_env[5] 
	   btn_OK 
	   btn_Cancel
	   {&HLP_BTN_NAME}
	   WITH FRAME write-output-file.
  ELSE DO:
    IF io-file THEN
       UPDATE user_env[2] 
	   btn_File        
	   &IF "{&WINDOW-SYSTEM}" <> "TTY" &THEN  
	    io-mapl
	   &ELSE
	    io-mapc 
	   &ENDIF
	   user_env[5] 
           btn_OK 
	   btn_Cancel
	   {&HLP_BTN_NAME}
	   WITH FRAME write-dump-file.
    ELSE
       UPDATE user_env[2] 
	   &IF "{&WINDOW-SYSTEM}" <> "TTY" &THEN  
	    io-mapl 
	   &ELSE
	    io-mapc 
	   &ENDIF
	   user_env[5] 
	   btn_OK 
	   btn_Cancel
	   {&HLP_BTN_NAME}
	   WITH FRAME write-dump-dir.
    
    &IF "{&WINDOW-SYSTEM}" <> "TTY" &THEN  
      CASE io-mapl:
	  WHEN 1 THEN user_env[3] = "NO-MAP".
	  WHEN 2 THEN user_env[3] = io-mapc.
      END CASE.
    &ELSE
      IF io-mapc <> "" THEN user_env[3] = io-mapc.
    &ENDIF
  END.
  IF NOT io-file THEN
    RUN "prodict/misc/ostodir.p" (INPUT-OUTPUT user_env[2]).

  { prodict/dictnext.i trash }
  canned = FALSE.
END.

HIDE MESSAGE NO-PAUSE.

HIDE FRAME write-output-file NO-PAUSE.
HIDE FRAME write-dump-file NO-PAUSE.
HIDE FRAME write-dump-dir NO-PAUSE.
IF canned THEN DO:
  user_path = "".
  user_cancel = yes.
END.
RETURN.

