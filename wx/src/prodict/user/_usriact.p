/*************************************************************/
/* Copyright (c) 1984-1998 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/* useriact - user interface for index activation/deactivation 
 *
 * HISTORY:
 * 
 * 04/09/98 by laurief  Added SIZE 80 BY 20 to FRAME "stuff" (BUG 98-04-09-046)
 * 07/13/94 by gfs      94-06-30-002
 *
 */

/*
user_env[1] holds the filename which owns the indexes to be activated.

user_env[9] = 'on'   to rebuild (thru proutil idxbuild) not yet supported.
            = 'off'  to deactivate the indexes.
            = 'note' to display the initial help message frame.
*/

{ prodict/dictvar.i }
{ prodict/user/uservar.i }
{ prodict/user/userhue.i }
{ prodict/user/userpik.i NEW }

DEFINE VARIABLE answer    AS LOGICAL               NO-UNDO.
DEFINE VARIABLE canned    AS LOGICAL INITIAL TRUE  NO-UNDO.
DEFINE VARIABLE c         AS CHARACTER             NO-UNDO.
DEFINE VARIABLE i         AS INTEGER               NO-UNDO.
DEFINE VARIABLE msg-num   AS INTEGER INITIAL 0     NO-UNDO.
DEFINE VARIABLE rebuild   AS LOGICAL               NO-UNDO.
DEFINE VARIABLE something AS LOGICAL INITIAL FALSE NO-UNDO.

/* LANGUAGE DEPENDENCIES START */ /*----------------------------------------*/
DEFINE VARIABLE new_lang AS CHARACTER EXTENT 9 NO-UNDO INITIAL [
  /* 1*/ "Select Indexes to Rebuild",
  /* 2*/ "Select Indexes to Deactivate",
  /* 3*/ "You can only rebuild PROGRESS database indexes.",
  /* 4*/ "There are no active indexes on this table.",
  /* 5*/ "Are you sure that you want to deactivate ALL indexes?",
  /* 6*/ "Are you sure that you want to deactivate one index?",
  /* 7*/ "Are you sure that you want to deactivate these indexes?",
  /* 8*/ "You do not have permission to use this option.",
  /* 9*/ "The dictionary is in read-only mode - alterations not allowed."
].
FORM
  _File._File-name   FORMAT "x(30)" LABEL "Table-name" 
  _Index._Index-name FORMAT "x(30)" LABEL "Index-name" 
  WITH FRAME offing
  SCREEN-LINES - 8 DOWN CENTERED USE-TEXT
  TITLE " The following indexes are being deactivated ".

FORM
  SKIP({&TFM_WID})
  "If you select 'ALL' tables, then all indexes for all tables will be":t74 AT 2
  SKIP
  "deactivated.":t74    	      	       	     	      	                AT 2
  SKIP({&VM_WIDG})
  "If you select a specific table, then you will be prompted for the":t74   AT 2
  SKIP
  "indexes to deactivate.":t74 AT 2
  SKIP({&VM_WIDG})
  "When indexes are deactivated, it means that they will no longer be":t74 AT 2
  SKIP
  "maintained by PROGRESS.  Creating or updating records in these tables":t74 AT 2
  SKIP
  "will be faster, so this is ideal for doing massive loads quickly.":t74   AT 2
  SKIP
  "However, you will not be able to perform a 'FIND' on records in a":t74   AT 2
  SKIP
  "table which has a deactivated Primary Index.":t74     	                AT 2
  SKIP({&VM_WIDG})
  "Use the Index Rebuild Utility to re-activate the indexes.  This is an":t74 AT 2
  SKIP
  "option of the 'proutil' (PROGRESS Utilities) command.":t74               AT 2
  {prodict/user/userbtns.i}
  WITH FRAME stuff SIZE 80 BY 20
  CENTERED NO-LABELS NO-ATTR-SPACE USE-TEXT 
  DEFAULT-BUTTON btn_OK CANCEL-BUTTON btn_Cancel
  VIEW-AS DIALOG-BOX TITLE "Index Deactivation".

/* LANGUAGE DEPENDENCIES END */ /*------------------------------------------*/

/*==============================Triggers================================*/

&IF "{&WINDOW-SYSTEM}" <> "TTY" &THEN 
/*----- HELP -----*/
on HELP of frame stuff
   or CHOOSE of btn_Help in frame stuff
   RUN "adecomm/_adehelp.p" (INPUT "admn", INPUT "CONTEXT", 
      	       	     	     INPUT {&Index_Deactivation_Dlg_Box},
      	       	     	     INPUT ?).
&ENDIF
/*----- WINDOW-CLOSE ------*/
ON WINDOW-CLOSE of frame stuff
   APPLY "END-ERROR" to frame stuff.

/*============================Mainline Code=============================*/
IF NOT CAN-FIND(FIRST DICTDB._File WHERE DICTDB._File._File-Number > 0) THEN DO:
  MESSAGE "There are no user tables in this database." VIEW-AS ALERT-BOX ERROR.
  user_path = "".
  RETURN.
END.

IF user_env[9] = "note" THEN DO:
  /* Adjust the graphical rectangle and the ok and cancel buttons */
    {prodict/user/userctr.i
        &FRAME = "FRAME stuff"
    }
    {adecomm/okrun.i  
        &FRAME  = "FRAME stuff" 
        &BOX    = "rect_Btns"
        &OK     = "btn_OK" 
        {&CAN_BTN}
        {&HLP_BTN}
    }
  PAUSE 0.
  DO ON ENDKEY UNDO, LEAVE:
    UPDATE btn_OK btn_Cancel {&HLP_BTN_NAME} WITH FRAME stuff.
    canned = FALSE.
  END.
  HIDE FRAME stuff NO-PAUSE.
  IF canned THEN
    user_path = "".
  RETURN.
END.

DO FOR _File:
  FIND _File "_Index".
  IF      user_dbtype <> "PROGRESS"               THEN msg-num = 3.
  ELSE IF NOT CAN-DO(_Can-write,USERID("DICTDB")) THEN msg-num = 8.
  ELSE IF dict_rog                                THEN msg-num = 9.
END.
IF msg-num > 0 THEN DO:
  MESSAGE new_lang[msg-num] VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  user_path = "".
  RETURN.
END.

ASSIGN
  rebuild    = user_env[1] = "on"
  pik_count  = 0
  pik_multi  = TRUE
  pik_wide   = FALSE
  pik_row    = 5
  pik_hide   = TRUE
  pik_column = 20
  pik_title  = new_lang[IF rebuild THEN 1 ELSE 2]. /* select to make/kill */

IF user_env[1] = "ALL" THEN 
_all: DO:
  answer = FALSE.
  MESSAGE new_lang[5] VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE answer.
  IF NOT answer THEN
    LEAVE _all. 

  run adecomm/_setcurs.p ("WAIT").
  FOR EACH _File
    WHERE _File._File-num > 0
      AND _File._Db-recid = drec_db
      AND NOT _File._dft-pk:
    FOR EACH _Index OF _File WHERE _Index._Active:
      _Index._Active = FALSE.
      IF RECID(_Index) = _File._Prime-Index THEN PAUSE 0.
      DISPLAY _File-name _Index-name WITH FRAME offing.
      IF FRAME-LINE(offing) = FRAME-DOWN(offing) THEN
        UP FRAME-LINE(offing) - 1 WITH FRAME offing.
      ELSE
        DOWN WITH FRAME offing.
    END.
  END.
  run adecomm/_setcurs.p ("").
  something = TRUE.
END.
ELSE _some: DO:
  FOR EACH _Index WHERE _Index._File-recid = drec_file
    AND _Index._Index-name <> "default"
    BY _Index._Index-name:
    IF rebuild = _Active THEN NEXT.
    ASSIGN
      pik_count = pik_count + 1
      pik_list[pik_count] = _Index._Index-name.
  END.

  IF pik_count = 0 THEN DO:
    MESSAGE new_lang[4] VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
    user_path = "".
    RETURN.
  END.

  &IF "{&WINDOW-SYSTEM}" = "TTY" &THEN
    RUN "prodict/user/_usrpick.p".
  &ELSE
    pik_help = {&Indexes_For_Deactivation_Dlg_Box}.
    RUN "prodict/gui/_guipick.p".
  &ENDIF
  IF pik_return = 0 THEN LEAVE _some.

  answer = FALSE.
  MESSAGE new_lang[IF pik_return = 1 THEN 6 ELSE 7]
      	  VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE answer. 
  IF NOT answer THEN LEAVE _some.

  run adecomm/_setcurs.p ("WAIT").
  DO i = 1 TO pik_return:
    FIND _Index WHERE _Index._File-recid = drec_file
      AND _Index._Index-name = pik_list[pik_chosen[i]].
    _Index._Active = FALSE.
    DISPLAY user_filename @ _File._File-name _Index-name WITH FRAME offing.
    IF FRAME-LINE(offing) = FRAME-DOWN(offing) THEN
      UP FRAME-LINE(offing) - 1 WITH FRAME offing.
    ELSE
      DOWN WITH FRAME offing.
  END.
  run adecomm/_setcurs.p ("").
  something = TRUE.
END.

IF something THEN 
   MESSAGE "Index deactivation complete."
      	   VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.

/* This used to give the user opportunity to hit "GET" to repeat the
   process.  It would set user_path = "1=a,_usrtget,_usriact".  This
   is not done anywhere else and I don't see the point in doing it here
   since the user could have selected multiple tables.  If we want to
   restore this add a question to the complete alert box.
*/
HIDE FRAME offing NO-PAUSE.
RETURN.