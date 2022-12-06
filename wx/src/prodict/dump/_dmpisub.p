/*************************************************************/
/* Copyright (c) 1984-1993 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/* _dmpisub.p - subroutine of _dmpincr.p */

{ prodict/dictvar.i }
{ prodict/user/uservar.i }
{ prodict/user/userhue.i }
{ prodict/user/userpik.i NEW }

DEFINE INPUT        PARAMETER which   AS CHARACTER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER missing AS CHARACTER NO-UNDO.
DEFINE OUTPUT       PARAMETER answer  AS LOGICAL   NO-UNDO.

DEFINE VARIABLE c     AS CHARACTER NO-UNDO.
DEFINE VARIABLE ix    AS INTEGER   NO-UNDO.
DEFINE VARIABLE name  AS CHARACTER NO-UNDO.
DEFINE VARIABLE fname AS CHARACTER NO-UNDO.
DEFINE VARIABLE type  AS CHARACTER NO-UNDO.

/* LANGUAGE DEPENDENCIES START */ /*----------------------------------------*/
DEFINE VARIABLE new_lang AS CHARACTER EXTENT 11 NO-UNDO INITIAL [
  /* 1*/ "<deleted>",
  /* 2*/ ? /* see below */ ,
  /* 3*/ "Does this mean you want to abort this program?",
  /* 4*/ "Table:",
  /* 5*/ "Field:",
  /* 6*/ "does not exist.",
  /* 7*/ "If it is renamed, please choose",
  /* 8*/ "the new name from the list of",
  /* 9*/ "unmatched names shown here.",
  /*10*/ "Otherwise, select ~"<deleted>~".",
  /*11*/ "Sequence:"
].

new_lang[2] = "You pressed [" + KBLABEL("END-ERROR") + "] or Cancel.".

FORM
  new_lang[4]  FORMAT "x(6)"
     name      FORMAT "x(32)" SKIP
  new_lang[6]  FORMAT "x(32)" SKIP(1)
  new_lang[7]  FORMAT "x(32)" SKIP
  new_lang[8]  FORMAT "x(32)" SKIP
  new_lang[9]  FORMAT "x(32)" SKIP
  new_lang[10] FORMAT "x(30)"
  WITH FRAME t-help NO-LABELS NO-ATTR-SPACE ROW 3 COLUMN 1 USE-TEXT.

FORM
  new_lang[4] FORMAT "x(6)"
    DICTDB._File._File-name   FORMAT "x(32)" SKIP
  new_lang[5] FORMAT "x(6)"
    DICTDB._Field._Field-name FORMAT "x(32)" SKIP
  new_lang[6]  	     	      FORMAT "x(32)" SKIP(1)
  new_lang[7]		      FORMAT "x(32)" SKIP
  new_lang[8]		      FORMAT "x(32)" SKIP
  new_lang[9]		      FORMAT "x(32)" SKIP
  new_lang[10]		      FORMAT "x(30)"
  WITH FRAME f-help NO-LABELS NO-ATTR-SPACE ROW 3 COLUMN 1 USE-TEXT.

FORM
  new_lang[11] FORMAT "x(9)"
     name      FORMAT "x(28)" SKIP
  new_lang[6]  FORMAT "x(32)" SKIP(1)
  new_lang[7]  FORMAT "x(32)" SKIP
  new_lang[8]  FORMAT "x(32)" SKIP
  new_lang[9]  FORMAT "x(32)" SKIP
  new_lang[10] FORMAT "x(30)"
  WITH FRAME s-help NO-LABELS NO-ATTR-SPACE ROW 3 COLUMN 1 USE-TEXT.

/* LANGUAGE DEPENDENCIES END */ /*------------------------------------------*/

/* start of definitions */ /*-----------------------------------------------*/
DEFINE SHARED WORKFILE table-list NO-UNDO
  FIELD t1-name AS CHARACTER INITIAL ""
  FIELD t2-name AS CHARACTER INITIAL ?.

DEFINE SHARED WORKFILE field-list NO-UNDO
  FIELD f1-name AS CHARACTER INITIAL ""
  FIELD f2-name AS CHARACTER INITIAL ?.

DEFINE SHARED WORKFILE seq-list NO-UNDO
  FIELD s1-name AS CHARACTER INITIAL ""
  FIELD s2-name AS CHARACTER INITIAL ?.

/* end of definitions */ /*-------------------------------------------------*/

ASSIGN
  answer      = FALSE
  pik_row     = 5
  pik_column  = 42
  pik_count   = 1
  pik_list[1] = new_lang[1]. /* <deleted> */

PAUSE 0.

IF which = "t" THEN _file: DO:
  FOR EACH table-list WHERE table-list.t2-name = ?:
    ASSIGN
      pik_count = pik_count + 1
      pik_list[pik_count] = table-list.t1-name.
  END.
  IF pik_count = 1 THEN LEAVE _file.
  &IF "{&WINDOW-SYSTEM}" = "TTY" &THEN
    DISPLAY
      new_lang[4] 
      missing @ name
      new_lang[6] new_lang[7] 
      new_lang[8] new_lang[9] 
      new_lang[10]
      WITH FRAME t-help.
    RUN "prodict/user/_usrpick.p".
  &ELSE
    pik_text = new_lang[4] + " " + missing.
    DO ix = 6 TO 10:
      pik_text = pik_text + "~n" + new_lang[ix].
    END.
    ASSIGN
      pik_text = pik_text + c
      pik_help = {&Resolve_Mismatched_Table_Dlg_Box}.
    RUN "prodict/gui/_guipick.p".
  &ENDIF

  IF pik_return = 0 THEN DO:
    MESSAGE new_lang[2] SKIP new_lang[3]
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE answer.
    IF answer THEN answer = ?.
    IF answer = ? THEN LEAVE _file.
  END.
  IF pik_chosen[1] = 1 THEN pik_first = ?.
  FIND FIRST table-list WHERE table-list.t1-name = pik_first NO-ERROR.
  IF AVAILABLE table-list THEN ASSIGN
    table-list.t2-name = missing
    missing            = ?.

END.

IF which = "f" THEN _field: DO:
  FOR EACH field-list WHERE field-list.f2-name = ?:
    ASSIGN
      pik_count = pik_count + 1
      pik_list[pik_count] = field-list.f1-name.
  END.
  IF pik_count = 1 THEN LEAVE _field.
  &IF "{&WINDOW-SYSTEM}" = "TTY" &THEN
    DISPLAY
      new_lang[4]
      user_env[19] @ DICTDB._File._File-name /* this is a hack */
      new_lang[5]
      missing @ DICTDB._Field._Field-name
      new_lang[6] new_lang[7] 
      new_lang[8] new_lang[9] 
      new_lang[10]
      WITH FRAME f-help.
    RUN "prodict/user/_usrpick.p".
  &ELSE	 
    pik_text = new_lang[4] + " " + user_env[19] + ", " + 
      	       new_lang[5] + " " + missing.
    DO ix = 6 TO 10:
      pik_text = pik_text + "~n" + new_lang[ix].
    END.
    ASSIGN
      pik_text = pik_text + c
      pik_help = {&Resolve_Mismatched_Field_Dlg_Box}.
    RUN "prodict/gui/_guipick.p".
  &ENDIF

  IF pik_return = 0 THEN DO:
    MESSAGE new_lang[2] SKIP new_lang[3]
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE answer.
    IF answer THEN answer = ?.
    IF answer = ? THEN LEAVE _field.
  END.
  IF pik_chosen[1] = 1 THEN pik_first = ?.
  FIND FIRST field-list WHERE field-list.f1-name = pik_first NO-ERROR.
  IF AVAILABLE field-list THEN ASSIGN
    field-list.f2-name = missing
    missing            = ?.
END.

IF which = "s" THEN _seq: DO:
  FOR EACH seq-list WHERE seq-list.s2-name = ?:
    ASSIGN
      pik_count = pik_count + 1
      pik_list[pik_count] = seq-list.s1-name.
  END.
  IF pik_count = 1 THEN LEAVE _seq.
  &IF "{&WINDOW-SYSTEM}" = "TTY" &THEN
    DISPLAY
      new_lang[11]
      missing @ name
      new_lang[6] new_lang[7] 
      new_lang[8] new_lang[9] 
      new_lang[10]
      WITH FRAME s-help.
    RUN "prodict/user/_usrpick.p".
  &ELSE
    pik_text = new_lang[4] + " " + missing.
    DO ix = 6 TO 10:
      pik_text = pik_text + "~n" + new_lang[ix].
    END.
    ASSIGN
      pik_text = pik_text + c
      pik_help = {&Resolve_Mismatched_Sequence_Dlg_Box}.
    RUN "prodict/gui/_guipick.p".
  &ENDIF

  IF pik_return = 0 THEN DO:
    MESSAGE new_lang[2] SKIP new_lang[3]
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE answer.
    IF answer THEN answer = ?.
    IF answer = ? THEN LEAVE _seq.
  END.
  IF pik_chosen[1] = 1 THEN pik_first = ?.
  FIND FIRST seq-list WHERE seq-list.s1-name = pik_first NO-ERROR.
  IF AVAILABLE seq-list THEN ASSIGN
    seq-list.s2-name = missing
    missing            = ?.
END.

HIDE FRAME t-help NO-PAUSE.
HIDE FRAME f-help NO-PAUSE.
HIDE FRAME s-help NO-PAUSE.
RETURN.


