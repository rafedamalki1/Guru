/*************************************************************/
/* Copyright (c) 1984-1995 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/* _ism_def.p - create/update/verify defs for NetISAM and C-ISAM keys/indexes */
/*
  user_env[2] = "add"
                add a new file definition
              = "chg"
                re-read C-ISAM or NetISAM def, user_filename contains file name
              = "vrf"
                re-read C-ISAM or NetISAM definition, compare
  user_env[7] = "c" for C-ISAM or "n" for NetISAM
*/

{ prodict/dictvar.i }
{ prodict/user/uservar.i }

DEFINE VARIABLE i           AS INTEGER     NO-UNDO.
DEFINE VARIABLE ism-file    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE l_drec_db   LIKE drec_db   NO-UNDO.
DEFINE VARIABLE l_user_path LIKE user_path NO-UNDO.
DEFINE VARIABLE old-user    LIKE user_env  NO-UNDO.
DEFINE VARIABLE pro-file    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE pru-file    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE canned      AS LOGICAL     NO-UNDO INITIAL TRUE.
DEFINE VARIABLE stat        AS LOGICAL     NO-UNDO.

DEFINE VARIABLE file_lbl AS CHARACTER  NO-UNDO.
DEFINE VARIABLE reclen   AS INTEGER    NO-UNDO LABEL "Record Size".
DEFINE VARIABLE dfchoice AS LOGICAL    NO-UNDO INITIAL YES
       VIEW-AS RADIO-SET VERTICAL
       RADIO-BUTTONS "Create Initial Table Definition",NO,
      	       	     "Load From Definition File (see below)",YES.

/* LANGUAGE DEPENDENCIES START */ /*----------------------------------------*/
DEFINE VARIABLE new_lang AS CHARACTER EXTENT 10 NO-UNDO INITIAL [
  /*  1*/   "Generation of .df file failed - Cannot find .df file",
  /*  2*/   "Unable to find file:",
  /*3,4*/   "Found Definition file:","Do you want to regenerate it?",
  /*  5*/   "File needed for .df generation not found:",
  /*  6*/   "Please specify a record size for this table.",
  /*  7*/   "That table name already exists in this database.",
  /*  8*/   "Please enter a name for this table."
].

FORM
  SKIP({&TFM_WID})
  pro-file FORMAT "x(32)" LABEL "PROGRESS Table Name"
      	       	     	       AT 2 
  SKIP({&VM_WIDG})

  file_lbl                     AT 2 NO-LABEL VIEW-AS TEXT 
      	       	     	       FORMAT "x(60)"    SKIP({&VM_WID})
  ism-file FORMAT "x(70)"      AT 2 NO-LABEL
  SKIP({&VM_WIDG})

  dfchoice NO-LABEL            AT 2 SPACE(4)
  reclen                       /* LABEL "Record Size" */  
  SKIP({&VM_WIDG})

  "Definition file:"           AT 2 VIEW-AS TEXT SKIP({&VM_WID})
      pru-file format "x(70)"  AT 2 NO-LABEL     SKIP({&VM_WID})
  "If you are on a UNIX machine with local data files, a Definition file"   
      	       	     	       AT 2 VIEW-AS TEXT SKIP
  "can be created automatically.  In that case leave this file name blank."  
      	       	     	       AT 2 VIEW-AS TEXT 

  {prodict/user/userbtns.i}
  WITH FRAME ism-add SIDE-LABELS ROW 1
  DEFAULT-BUTTON btn_OK CANCEL-BUTTON btn_Cancel
  VIEW-AS DIALOG-BOX 
  TITLE " " + (IF user_env[2] = "add" THEN "Adding" ELSE "Updating")
            + " " + (IF user_env[7] = "c" THEN "C-ISAM" ELSE "NetISAM")
            + " Table Definitions ".

/* LANGUAGE DEPENDENCIES END */ /*------------------------------------------*/


/*===============================Triggers=================================*/

/*-----WINDOW-CLOSE-----*/
ON WINDOW-CLOSE OF FRAME ism-add
   APPLY "END-ERROR" TO FRAME ism-add.


&IF "{&WINDOW-SYSTEM}" <> "TTY" &THEN 

/*----- HELP -----*/
on HELP of frame ism-add 
   or CHOOSE of btn_Help in frame ism-add
   RUN "adecomm/_adehelp.p" (INPUT "admn", INPUT "CONTEXT", 
			     INPUT {&Add_Table_Defs_Dlg_Box},
			     INPUT ?).

&ENDIF


/*----- GO or OK BUTTON -----*/
ON GO OF FRAME ism-add
DO:
  DEFINE VARIABLE fpfix     AS CHARACTER NO-UNDO INITIAL "".
  DEFINE VARIABLE fname     AS CHARACTER NO-UNDO INITIAL "".
  DEFINE VARIABLE df-file   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE idx-file  AS CHARACTER NO-UNDO.
  DEFINE VARIABLE defaulted AS LOGICAL   NO-UNDO INITIAL FALSE.
  DEFINE VARIABLE generate  AS LOGICAL   NO-UNDO INITIAL NO.

  RUN adecomm/_setcurs.p ("WAIT").

  ASSIGN
     ism-file = TRIM(INPUT FRAME ism-add ism-file)
     pro-file = TRIM(INPUT FRAME ism-add pro-file)
     pru-file = TRIM(INPUT FRAME ism-add pru-file).
/*   dfchoice = INPUT FRAME ism-add dfchoice 
     reclen   = INPUT FRAME ism-add reclen.
                   not needed anymore - dfchoice disabled */

  IF pro-file = ? OR pro-file = "" THEN DO:
    /* Default the table name to the isam file name if it was specified */
    IF dfchoice AND ism-file <> ? AND ism-file <> "" THEN DO:
      /* Break isam file name into path (prefix) and file name */
      RUN prodict/misc/osprefix.p (INPUT ism-file,  
				   OUTPUT fpfix,
				   OUTPUT fname).
      pro-file = fname.
      DISPLAY pro-file WITH FRAME ism-add.
    END.
    ELSE DO:
      MESSAGE new_lang[8]  /* please enter a name */
	      VIEW-AS ALERT-BOX ERROR BUTTONS OK.
      APPLY "ENTRY" TO pro-file IN FRAME ism-add.
      RETURN NO-APPLY.
    END.
  END.

  IF CAN-FIND(FIRST _File WHERE _File._Db-recid = l_drec_db  
      	       	     	  AND   _File._File-name = pro-file) THEN DO:
    MESSAGE new_lang[7]	/* table already exists */
      	    VIEW-AS ALERT-BOX ERROR BUTTONS OK.
    APPLY "ENTRY" TO pro-file IN FRAME ism-add.
    RETURN NO-APPLY.
  END.

  /* Default the isam file name to the Progress Table name.  In that
     case it is assumed to be in the current directory 
  */
  IF ism-file = ? OR ism-file = "" THEN DO:
    ism-file = pro-file.
    DISPLAY ism-file WITH FRAME ism-add.
  END.

  /* Make sure the isam file exists */
  IF pru-file = ? OR pru-file = " " THEN DO:  /* check only if no .df got entered */
    IF SEARCH(ism-file + ".dat") = ? THEN DO:
      MESSAGE new_lang[2] ism-file + ".dat" /* file not found */
              VIEW-AS ALERT-BOX ERROR BUTTONS OK.
      APPLY "ENTRY" TO ism-file IN FRAME ism-add.
      RETURN NO-APPLY.
    END.
  END.

  IF dfchoice THEN DO:
    /* Default the .df file name to the path of the isam file 
       followed by <progress table name>.df
    */
    IF pru-file = ? OR pru-file = " " THEN DO:
      if fpfix = "" then
	/* Break isam file name into path (prefix) and file name */
	RUN prodict/misc/osprefix.p (INPUT ism-file,  
				     OUTPUT fpfix,
				     OUTPUT fname).
      defaulted = TRUE.
      pru-file = fpfix + pro-file + ".df".
      DISPLAY pru-file WITH FRAME ism-add.
    END.

    /* See if the .df file is out there in the propath */
    df-file = SEARCH(pru-file).
    IF df-file = ? THEN DO:
      IF NOT defaulted THEN DO:
        MESSAGE new_lang[2] pru-file VIEW-AS ALERT-BOX ERROR BUTTONS OK.
        RETURN NO-APPLY.
      END.
      ELSE IF OPSYS = "UNIX" THEN 
	generate = YES. /* Couldn't find it so we'll generate a .df file */
      ELSE DO:
	/* Couldn't find and can't generate - so error */
	MESSAGE new_lang[2] pru-file VIEW-AS ALERT-BOX ERROR BUTTONS OK. 
	APPLY "ENTRY" TO pru-file IN FRAME ism-add.
        RETURN NO-APPLY.
      END.
    END.
    ELSE /* .df was found */
      IF defaulted AND OPSYS = "UNIX" THEN
	/* User didn't specify a file explicitly but we found a .df file
	   with the name we came up with as the default.  Ask the user
	   whether to use it or re-generate it (UNIX only).  If not UNIX,
	   we'll just use the .df that was found - no questions.
	*/
	MESSAGE new_lang[3] df-file SKIP /* overwrite? */
		new_lang[4] 
		VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE generate.
  
    IF generate THEN DO:
      idx-file = ism-file + STRING(user_env[7] = "c",".idx/.ind").
      IF SEARCH(idx-file) = ? THEN DO:
	MESSAGE new_lang[5] SKIP idx-file /* required file not found */
	     VIEW-AS ALERT-BOX ERROR BUTTONS OK.
	APPLY "ENTRY" TO ism-file IN FRAME ism-add.
	RETURN NO-APPLY.
      END.
      MESSAGE "Generating .df File".
      PAUSE 2 NO-MESSAGE.
      UNIX SILENT _proutil VALUE("-C")
	VALUE(IF user_env[7] = "c" THEN "cisamtodf" ELSE "netisamtodf")
	VALUE(ism-file) VALUE(pru-file) VALUE(pro-file).
      HIDE MESSAGE NO-PAUSE.
  
      /* If we still don't have a .df file we can't proceed */
      pru-file = SEARCH(pru-file).
      IF pru-file = ? THEN DO:
	MESSAGE new_lang[1] VIEW-AS ALERT-BOX ERROR BUTTONS OK.
	APPLY "ENTRY" TO pru-file IN FRAME ism-add.
	RETURN NO-APPLY.
      END.
    END. /* end if generate */
    ELSE
      pru-file = df-file.  /* set to value with full path included */
  END.  /* end if dfchoice */

/* not needed anymore - dfchoice disabled    
 *  ELSE DO:   
 *    IF reclen = 0 OR reclen = ? THEN DO:
 *      MESSAGE new_lang[6]  /* need rec size */
 *	      VIEW-AS ALERT-BOX ERROR BUTTONS OK.
 *	      APPLY "ENTRY" TO reclen IN FRAME ism-add.
 *      RETURN NO-APPLY.
 *    END.
 *  END.
 */

END.


/* not needed anymore - dfchoice disabled    
 * /*----- VALUE-CHANGED of RADIO-SET -----*/
 * ON VALUE-CHANGED OF dfchoice IN FRAME ism-add
 * DO:
 *    IF SELF:SCREEN-VALUE = "YES" THEN
 *       ASSIGN
 * 	   pru-file:SENSITIVE IN FRAME ism-add = yes
 * 	   reclen:SENSITIVE IN FRAME ism-add = no.
 *    ELSE
 *       ASSIGN
 * 	   pru-file:SENSITIVE IN FRAME ism-add = no
 * 	   reclen:SENSITIVE IN FRAME ism-add = yes.
 * END.
 */

/*============================Mainline code===============================*/

IF user_env[2] = "add" THEN DO:

  run adecomm/_setcurs.p ("").

  {adecomm/okrun.i  
    &FRAME  = "FRAME ism-add" 
    &BOX    = "rect_Btns"
    &OK     = "btn_OK" 
    {&CAN_BTN}
    {&HLP_BTN}
  }
  /* Assume user will use a .df file to start. Record length is only
     appropriate for the Create Default radio set choice.
  */
/* the second option is not implemented yet -> dfchoice disabled */  
  ASSIGN
    reclen:SENSITIVE IN FRAME ism-add   = NO
    dfchoice:SENSITIVE IN FRAME ism-add = NO.
END.
ASSIGN
  l_drec_db   = drec_db
  l_user_path = user_path
  file_lbl    = "Physical " 
              + (IF user_env[7] = "c" THEN "C-ISAM" ELSE "NetISAM")
      	      + " file name (without any extension):".
DISPLAY file_lbl dfchoice WITH FRAME ism-add.

DO TRANSACTION:
  /*
     add
  */
  IF user_env[2] = "add" THEN DO:
    DO ON ERROR UNDO, LEAVE ON ENDKEY UNDO, LEAVE:
      /* GO trigger processing will set the underlying variable values */
      ENABLE pro-file ism-file pru-file
      	     btn_OK btn_Cancel {&HLP_BTN_NAME}
        WITH FRAME ism-add.
      ASSIGN user_path = l_user_path.

/* not needed anymore - dfchoice disabled    
 *        stat        = reclen:MOVE-AFTER-TAB-ITEM(dfchoice:HANDLE IN FRAME ism-add) 
 *      	                IN FRAME ism-add.
 */

       WAIT-FOR CHOOSE OF btn_OK IN FRAME ism-add OR GO OF FRAME ism-add
      	 FOCUS pro-file IN FRAME ism-add.
      HIDE FRAME ism-add NO-PAUSE.

      IF dfchoice THEN DO:
        /* Create table def by loading a .df file */
	DO i = 1 to 19:
	  old-user[i] = user_env[i].        /* save current user_env */
	END.
    
	ASSIGN
	  cache_dirty = TRUE
	  user_env[2] = pru-file
	  user_env[4] = "n"
	  user_env[8] = user_dbname.
	RUN "prodict/dump/_lodsddl.p".
	HIDE MESSAGE.
	IF user_path = "*R":u THEN UNDO, RETRY. /* load failed */
	FIND _File WHERE _File._Db-recid  = l_drec_db AND 
      	       	     	 _File._File-name = pro-file.
	DO i = 1 to 19:
	  user_env[i] = old-user[i].
	END.
      END.  /* Create table def by loading a .df file */

/* not needed anymore - dfchoice disabled    
 *      ELSE DO:
 *      	/* Just create a default table definition */
 *	CREATE _File.
 *	ASSIGN
 *        _File._Db-recid = l_drec_db
 *	  _File._File-name = pro-file
 *	  _File._Dump-name = SUBSTR(pro-file,1,8)
 *	  _File._Can-create = "*"
 *	  _File._Can-read = "*"
 *	  _File._Can-write = "*"
 *	  _File._Can-delete = "*"
 *	  _File._For-name = ism-file
 *	  _File._For-size = reclen.
 *      END.
 */

      ASSIGN
	user_filename = pro-file
	drec_file     = RECID(_File)
	user_env[1]   = pro-file.

      canned = FALSE.
      run adecomm/_setcurs.p ("").

    END. /* DO ON ERROR UNDO, LEAVE */

  END. /* add */

  /*
     chg 
  */
  ELSE IF user_env[2] = "chg" THEN DO:
    FIND _File WHERE RECID(_File) = drec_file.
    canned = FALSE.
  END.

  /*
     vrf - not implemented yet
  */

END. /* transaction */


IF canned THEN user_path = "".

RETURN.

