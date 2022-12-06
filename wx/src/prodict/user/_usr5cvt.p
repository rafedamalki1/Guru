/*************************************************************/
/* Copyright (c) 1984-1996 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/* Progress Lex Converter 7.1A->7.1B Version 1.11 */

/* user5cvt - convert V5 .df to V6 .df */

{ prodict/dictvar.i }
{ prodict/user/uservar.i }

DEFINE VARIABLE v5           AS CHARACTER FORMAT "x(40)" NO-UNDO.
DEFINE VARIABLE curr           AS CHARACTER FORMAT "x(40)" NO-UNDO.
DEFINE VARIABLE answer         AS LOGICAL                  NO-UNDO.
DEFINE VARIABLE canned  AS LOGICAL   INITIAL TRUE   NO-UNDO.
{prodict/misc/filesbtn.i &NAME = btn_File1}
{prodict/misc/filesbtn.i &NAME = btn_File2 &NOACC=yes}

/* LANGUAGE DEPENDENCIES START */ /*----------------------------------------*/
DEFINE VARIABLE new_lang AS CHARACTER EXTENT 4 NO-UNDO INITIAL [
  /* 1*/ "A file named",
  /* 2*/ "already exists.",
  /* 3*/ "Overwrite it?",
  /* 4*/ "Could not find source file."
].
&IF "{&WINDOW-SYSTEM}" begins "MS-Win" &THEN  
&GLOBAL-DEFINE LINEUP 22
&ELSE
&GLOBAL-DEFINE LINEUP 26
&ENDIF
FORM SKIP({&TFM_WID})
  v5 {&STDPH_FILL} FORMAT "x(80)" COLON {&LINEUP} 
        LABEL "Input V5 Definition File" 
        VIEW-AS FILL-IN SIZE 38 BY 1 
  btn_File1 SKIP({&VM_WIDG})
  curr {&STDPH_FILL} FORMAT "x(80)" COLON {&LINEUP}
        LABEL "Output Definition File" 
        VIEW-AS FILL-IN SIZE 38 BY 1
  btn_File2 SKIP
  {prodict/user/userbtns.i}
  WITH FRAME v5tocurr
  SIDE-LABELS ATTR-SPACE CENTERED 
  DEFAULT-BUTTON btn_OK CANCEL-BUTTON btn_Cancel
  VIEW-AS DIALOG-BOX TITLE "Convert V5 .df File into Current Format".

/* LANGUAGE DEPENDENCIES END */ /*------------------------------------------*/


/*===============================Triggers=================================*/

&IF "{&WINDOW-SYSTEM}" <> "TTY" &THEN 
/*----- HELP -----*/
on HELP of frame v5tocurr
   or CHOOSE of btn_Help in frame v5tocurr
   RUN "adecomm/_adehelp.p" (INPUT "admn", INPUT "CONTEXT", 
                                               INPUT {&Convert_df_File_Format_Dlg_Box},
                                               INPUT ?).
&ENDIF


ON GO OF FRAME v5tocurr
DO:
  DEFINE VAR fil AS CHAR NO-UNDO.

  IF SEARCH(v5:SCREEN-VALUE IN FRAME v5tocurr) = ? THEN DO:
    MESSAGE new_lang[4] VIEW-AS ALERT-BOX ERROR BUTTONS OK.
    RETURN NO-APPLY.
  END.
 
  fil = curr:SCREEN-VALUE IN FRAME v5tocurr.

  IF SEARCH(fil) <> ? THEN DO:
    answer = FALSE.
    MESSAGE new_lang[1] fil new_lang[2] SKIP new_lang[3]
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE answer.
    IF NOT answer THEN 
    DO:
      APPLY "ENTRY" TO curr IN FRAME v5tocurr.
      RETURN NO-APPLY.
    END.
  END.
END.

ON WINDOW-CLOSE OF FRAME v5tocurr
   APPLY "END-ERROR" TO FRAME v5tocurr.

/*----- HIT of FILE BUTTONS -----*/
ON CHOOSE OF btn_File1 in frame v5tocurr DO:
   RUN "prodict/misc/_filebtn.p"
       (INPUT v5:handle in frame v5tocurr /*Fillin*/,
        INPUT "Find Definition File"      /*Title*/,
        INPUT "*.df"                             /*Filter*/,
        INPUT yes                                /*Must exist*/).
END.
ON CHOOSE OF btn_File2 in frame v5tocurr DO:
   RUN "prodict/misc/_filebtn.p"
       (INPUT curr:handle in frame v5tocurr  /*Fillin*/,
        INPUT "Find Definition File"                /*Title*/,
        INPUT "*.df"                                /*Filter*/,
        INPUT yes                                   /*Must exist*/).
END.
ON LEAVE OF v5 in frame v5tocurr
   v5:screen-value in frame v5tocurr = 
        TRIM(v5:screen-value in frame v5tocurr).



/*============================Mainline code===============================*/

PAUSE 0.
/* Adjust the graphical rectangle and the ok and cancel buttons */
{adecomm/okrun.i  
    &FRAME  = "FRAME v5tocurr" 
    &BOX    = "rect_Btns"
    &OK     = "btn_OK" 
    {&CAN_BTN}
    {&HLP_BTN}
}

DO ON ERROR UNDO,RETRY ON ENDKEY UNDO,LEAVE:
  UPDATE v5 btn_File1 curr btn_File2
               btn_OK 
               btn_Cancel {&HLP_BTN_NAME}
               WITH FRAME v5tocurr.

  ASSIGN
    user_env[1] = v5
    user_env[2] = curr.
  { prodict/dictnext.i v5 }
  canned = FALSE.
END.

HIDE FRAME v5tocurr NO-PAUSE.
IF canned THEN
  user_path = "".
RETURN.
