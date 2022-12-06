/*************************************************************/
/* Copyright (c) 1984-1994 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/* Progress Lex Converter 7.1A->7.1B Version 1.11 */

/*
This program generates a COPY assignment for copying the contents of
one buffer to another.
*/

{ prodict/dictvar.i }
{ prodict/user/uservar.i }

DEFINE VARIABLE answer AS LOGICAL   NO-UNDO.
DEFINE VARIABLE canned AS LOGICAL   NO-UNDO INIT TRUE.
DEFINE VARIABLE fo     AS CHARACTER NO-UNDO.
{prodict/misc/filesbtn.i}

/* LANGUAGE DEPENDENCIES START */ /*----------------------------------------*/
DEFINE VARIABLE new_lang AS CHARACTER EXTENT 4 NO-UNDO INITIAL [
  /* 1*/ "You do not have permission to use this option.",
  /* 2*/ "A file named",
  /* 3*/ "already exists.",
  /* 4*/ "Overwrite it?"
].

FORM 
  SKIP({&TFM_WID})
  fo {&STDPH_FILL} FORMAT "x(80)" AT 2 VIEW-AS FILL-IN SIZE 40 BY 1
         LABEL "Output File"
  btn_File SKIP ({&VM_WIDG})
  {prodict/user/userbtns.i}
  WITH FRAME gencopys
  SIDE-LABELS CENTERED 
  DEFAULT-BUTTON btn_OK CANCEL-BUTTON btn_Cancel
  VIEW-AS DIALOG-BOX 
  TITLE " Generate ASSIGN for Copying ~"" + user_env[1] + "~" ".

/* LANGUAGE DEPENDENCIES END */ /*------------------------------------------*/

/*===============================Triggers=================================*/

&IF "{&WINDOW-SYSTEM}" <> "TTY" &THEN 
/*----- HELP -----*/
on HELP of frame gencopys
   or CHOOSE of btn_Help in frame gencopys
   RUN "adecomm/_adehelp.p" (INPUT "admn", INPUT "CONTEXT", 
      	       	     	     INPUT {&Generate_Assign_Dlg_Box},
      	       	     	     INPUT ?).
&ENDIF


ON GO OF FRAME gencopys
DO:
  DEFINE VAR fil AS CHAR NO-UNDO.

  fil = fo:SCREEN-VALUE IN FRAME gencopys.

  IF SEARCH(fil) <> ? THEN DO:
    answer = FALSE.
    MESSAGE new_lang[2] fil new_lang[3] SKIP new_lang[4]
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE answer.
    IF NOT answer THEN 
    DO:
      APPLY "ENTRY" TO fo IN FRAME gencopys.
      RETURN NO-APPLY.
    END.
  END.
END.

ON CHOOSE OF btn_File in frame gencopys DO:
   RUN "prodict/misc/_filebtn.p"
       (INPUT fo:handle in frame gencopys /*Fillin*/,
        INPUT "Find Output File"  /*Title*/,
        INPUT ""                 /*Filter*/,
        INPUT no                /*Must exist*/).
END.
ON LEAVE OF fo in frame gencopys
   fo:screen-value in frame gencopys = 
        TRIM(fo:screen-value in frame gencopys).

ON WINDOW-CLOSE OF FRAME gencopys
   APPLY "END-ERROR" TO FRAME gencopys.

/*============================Mainline code===============================*/

DO FOR DICTDB._File:
  FIND _File "_File".
  answer = CAN-DO(_Can-read,USERID("DICTDB")).
  FIND _File "_Field".
  answer = answer AND CAN-DO(_Can-read,USERID("DICTDB")).
  IF NOT answer THEN DO:
    MESSAGE new_lang[1] VIEW-AS ALERT-BOX ERROR BUTTONS OK.
    user_path = "".
    RETURN.
  END.
END.

PAUSE 0.
ASSIGN
  fo = LC(user_env[1]) + ".i".

/* Adjust the graphical rectangle and the ok and cancel buttons */
{adecomm/okrun.i  
    &FRAME  = "FRAME gencopys" 
    &BOX    = "rect_Btns"
    &OK     = "btn_OK" 
    {&CAN_BTN}
    {&HLP_BTN}
}

DO ON ERROR UNDO,RETRY ON ENDKEY UNDO,LEAVE:
  UPDATE fo btn_File btn_OK btn_Cancel {&HLP_BTN_NAME} WITH FRAME gencopys.
   
  /* Do it */
  user_env[1] = fo.
  RUN "prodict/misc/_wrkcgen.p".
  canned = FALSE.
END.

IF canned THEN 
  user_path = "".

HIDE FRAME gencopys NO-PAUSE.
RETURN.



