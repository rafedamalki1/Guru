/*************************************************************/
/* Copyright (c) 1984-1996 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

{ prodict/user/uservar.i NEW }

DEFINE VARIABLE batch_mode       AS LOGICAL.
DEFINE VARIABLE db_exist         AS LOGICAL.
DEFINE VARIABLE output_file      AS CHARACTER.
DEFINE VARIABLE src_shname       AS CHARACTER.
DEFINE VARIABLE src_ldname       AS CHARACTER.
DEFINE VARIABLE trg_shdbname     AS CHARACTER.
DEFINE VARIABLE trg_ldname       AS CHARACTER.
DEFINE VARIABLE trg_shcpname     AS CHARACTER  INITIAL "s10migr".
DEFINE VARIABLE trg_shname       AS CHARACTER.
DEFINE VARIABLE make_copy        AS LOGICAL  INITIAL YES.
DEFINE VARIABLE j                AS INTEGER.

batch_mode = SESSION:BATCH-MODE.

FORM
  " "   SKIP
  "Name of Sybase Schema Holder Database:"
        TO 45 src_shname        FORMAT "x(32)"  NO-LABEL SKIP(1)
  "Logical name for Sybase Database:"
        TO 45 src_ldname        FORMAT "x(32)"  NO-LABEL SKIP(1)
  "Name of Sybase10 Schema Holder Database:"
        TO 45 trg_shdbname      FORMAT "x(32)"  NO-LABEL SKIP(1)
  "Logical name for Sybase10 Database:"
        TO 45 trg_ldname        FORMAT "x(32)"  NO-LABEL SKIP(1)
  "Create a copy of Sybase10 Schema Holder?"
        TO 45 make_copy         FORMAT "yes/no" NO-LABEL SKIP(1)
  "Name of Copy:"
        TO 45 trg_shcpname      FORMAT "x(32)"  NO-LABEL SKIP(1)
  {prodict/user/userbtns.i}
  WITH FRAME x ROW 4 CENTERED
      DEFAULT-BUTTON btn_OK CANCEL-BUTTON btn_Cancel
      VIEW-AS DIALOG-BOX TITLE "SYBASE to SYBASE10 Schema Migration".

ON WINDOW-CLOSE of FRAME x
    APPLY "END-ERROR" to FRAME x.

IF NOT batch_mode THEN DO:
{adecomm/okrun.i
    &FRAME = "FRAME x"
    &BOX   = "rect_Btns"
    &OK    = "btn_OK"
    {&CAN_BTN}
}
&IF "{&WINDOW-SYSTEM}" <> "TTY" &THEN
btn_Help:visible IN FRAME x = no.
&ENDIF
END.

main-blk:
DO ON ERROR UNDO main-blk, RETRY main-blk:

    DO WHILE LDBNAME (1) <> ?:
        DISCONNECT VALUE (LDBNAME (1)). 
    END.

/* 
 * If this is not batch mode then get our parameters from the screen.
 * Otherwise, get them from environment variables.
 */
    IF NOT batch_mode THEN DO:
        UPDATE src_shname
            src_ldname
            trg_shdbname
            trg_ldname
            make_copy
            trg_shcpname
            btn_OK btn_Cancel
            WITH FRAME x.
        END.
    ELSE DO:
        output_file = "s4tos10_output.tmp".
        OUTPUT TO VALUE(output_file). 
    END.

  CONNECT VALUE (src_shname) -ld srcsh -1 NO-ERROR.

  IF ERROR-STATUS:ERROR THEN DO:
    MESSAGE "Can't connect to Sybase Schema Holder Database " src_shname.
    DO j = 1 TO  ERROR-STATUS:NUM-MESSAGES:
        MESSAGE ERROR-STATUS:GET-MESSAGE(j).
    END.
    UNDO main-blk, RETRY main-blk.
  END.

  CREATE ALIAS holder FOR DATABASE srcsh.
  RUN "prodict/misc/_tstsh.p" (INPUT src_ldname, OUTPUT db_exist).
  DELETE ALIAS holder.
  IF NOT db_exist THEN DO:
    MESSAGE "Database " src_ldname " does not exist in schema holder " 
                   src_shname.
    UNDO main-blk, RETRY main-blk.
  END.

  IF (make_copy) THEN DO:
    IF trg_shcpname = ? THEN DO:
      MESSAGE "Name of copy for schema holder must be supplied".
      UNDO main-blk, RETRY main-blk.
    END.
    CREATE DATABASE  trg_shcpname FROM  trg_shdbname NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
      MESSAGE "Can't create " trg_shcpname.
        DO j = 1 TO  ERROR-STATUS:NUM-MESSAGES:
          MESSAGE ERROR-STATUS:GET-MESSAGE(j).
        END.
      UNDO main-blk, RETRY main-blk.
    END.
    trg_shname = trg_shcpname.
   END.
   ELSE
     trg_shname = trg_shdbname.

  CONNECT VALUE (trg_shname) -ld trgsh -1 NO-ERROR.

  IF ERROR-STATUS:ERROR THEN DO:
    MESSAGE "Can't connect to Sybase10 Schema Holder Database " trg_shname.
    DO j = 1 TO  ERROR-STATUS:NUM-MESSAGES:
        MESSAGE ERROR-STATUS:GET-MESSAGE(j).
    END.
    UNDO main-blk, RETRY main-blk.
  END.

  CREATE ALIAS holder FOR DATABASE trgsh.
  RUN "prodict/misc/_tstsh.p" (INPUT trg_ldname, OUTPUT db_exist).
  DELETE ALIAS holder.
  IF NOT db_exist THEN DO:
    MESSAGE "Database " trg_ldname " does not exist in schema holder " 
                   trg_shname.
    UNDO main-blk, RETRY main-blk.
  END.
  
/*
  ASSIGN
*/

  RUN prodict/odb/s4s10mgr.p  (src_ldname, trg_ldname).

   /* If this is batch mode, make sure we close the output file we 
   opened above.
   */
  DO WHILE LDBNAME (1) <> ?:
      DISCONNECT VALUE (LDBNAME (1)). 
  END.

  IF batch_mode THEN 
      OUTPUT CLOSE.

  MESSAGE  "See s4s10mgr.log for messages."
      VIEW-AS ALERT-BOX INFORMATION BUTTONS OK 
      TITLE "Migration Utility Completed". 
  END.

RETURN.



