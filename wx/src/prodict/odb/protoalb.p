/*************************************************************/
/* Copyright (c) 1984-1996 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

{ prodict/user/uservar.i NEW }
{ prodict/odb/odbvar.i NEW }

DEFINE VARIABLE cmd              AS CHARACTER.
DEFINE VARIABLE wait             AS CHARACTER.
DEFINE VARIABLE create_h         AS LOGICAL.
DEFINE VARIABLE db_exist         AS LOGICAL.
DEFINE VARIABLE tmp_str          AS CHARACTER. 
DEFINE VARIABLE batch_mode       AS LOGICAL.
DEFINE VARIABLE do_it_all        AS LOGICAL    INITIAL TRUE.
DEFINE VARIABLE output_file      AS CHARACTER.
DEFINE VARIABLE which_stages     AS INTEGER INITIAL 7.
DEFINE VARIABLE i                AS INTEGER.
DEFINE VARIABLE all_done         AS LOGICAL.
DEFINE VARIABLE compatible       AS LOGICAL INITIAL TRUE. 
DEFINE STREAM   strm.

batch_mode = SESSION:BATCH-MODE.

FORM
  " "   SKIP
  "Name of the Original PROGRESS Database:"
        TO 44 pro_dbname        FORMAT "x(32)"  NO-LABEL SKIP(1)
  "Enter name of Schema holder Database:"
        TO 44 osh_dbname        FORMAT "x(32)"  NO-LABEL SKIP({&VM_WID})
  "Enter Logical name for ALLBASE Database:"
        TO 44 odb_dbname        FORMAT "x(32)"  NO-LABEL SKIP({&VM_WID})
  "Enter Physical name for ALLBASE Database:"
        TO 44 odb_pdbname       FORMAT "x(32)"  NO-LABEL SKIP(1)
  "Enter the ALLBASE owner's login name:"
        TO 44 odb_username      FORMAT "x(32)"  NO-LABEL SKIP({&VM_WID})
  "Enter ALLBASE user's password:"
        TO 44 odb_password      FORMAT "x(32)"  NO-LABEL BLANK SKIP(1)
  "Dump and Load data:"
        TO 44 movedata                          NO-LABEL
/*  "Perform all operations:"
          TO 44 do_it_all                       NO-LABEL */
  "Add compatibility fields:"                  
	  TO 44 compatible 			NO-LABEL SKIP(1)
  {prodict/user/userbtns.i}
  WITH FRAME x ROW 4 CENTERED
    DEFAULT-BUTTON btn_OK CANCEL-BUTTON btn_Cancel
    VIEW-AS DIALOG-BOX TITLE "PROGRESS to SYBASE Conversion".

FORM
  " " SKIP
  which_stages VIEW-AS RADIO-SET
               RADIO-BUTTONS " Create SQL", 1,
                             " Dump Data", 2,
                             " Create Schema", 3,
                             " Create Objects   ", 4,
                             " Build Schema", 5,
                             " Fixup Schema", 6,
                             " Load Data", 7
               AT 4 NO-LABEL
   WITH FRAME Stages_Frame ROW 4 CENTERED
   TITLE "Stage to stop after".

FORM wait FORMAT "x" LABEL   "Creating Schema Holder - Please wait"   WITH FRAME
sh-wait ROW SCREEN-LINES - 2 COLUMN 1 NO-BOX.


ON WINDOW-CLOSE of FRAME x
   APPLY "END-ERROR" to FRAME x.

IF LDBNAME (1) <> ? THEN
  ASSIGN pro_dbname = LDBNAME (1).

IF NOT batch_mode THEN DO:
{adecomm/okrun.i  
    &FRAME  = "FRAME x" 
    &BOX    = "rect_Btns"
    &OK     = "btn_OK" 
    {&CAN_BTN}
}
&IF "{&WINDOW-SYSTEM}" <> "TTY" &THEN
btn_Help:visible IN FRAME x = no.
&ENDIF
END.

main-blk:
DO ON ERROR UNDO main-blk, RETRY main-blk:

  DO i = 1 to 7:
      stages[i] = FALSE.
  END. 

  all_done = FALSE. 

  DO WHILE LDBNAME (1) <> ?:
    DISCONNECT VALUE (LDBNAME (1)).
  END.


/* 
 * If this is not batch mode then get our parameters from the screen.
 * Otherwise, get them from environment variables.
 */
    IF NOT batch_mode THEN DO:
        UPDATE pro_dbname
            osh_dbname
            odb_dbname
            odb_pdbname
            odb_username
            odb_password
            movedata
            /* do_it_all */
            compatible
            btn_OK btn_Cancel
            WITH FRAME x.
        END.
    ELSE DO:
        IF OS-GETENV("PRODBNAME")   <> ? THEN 
            pro_dbname   = OS-GETENV("PRODBNAME").
        IF OS-GETENV("SHDBNAME")    <> ? THEN 
            osh_dbname   = OS-GETENV("SHDBNAME").
        IF OS-GETENV("ALBDBNAME")   <> ? THEN 
            odb_dbname   = OS-GETENV("ALBDBNAME").
        IF OS-GETENV("ALBPDBNAME")  <> ? THEN 
            odb_pdbname  = OS-GETENV("ALBPHYDBNAME").
        IF OS-GETENV("ALBUSERNAME") <> ? THEN 
            odb_username = OS-GETENV("ALBUSERNAME").
        IF OS-GETENV("ALBPASSWORD") <> ? THEN 
            odb_password = OS-GETENV("ALBPASSWORD").
        IF OS-GETENV("MOVEDATA")    <> ? THEN 
            tmp_str      = OS-GETENV("MOVEDATA").
        IF tmp_str BEGINS "Y" THEN movedata = TRUE. 
      
        IF OS-GETENV("ALBSTAGES")   <> ? THEN 
            which_stages = INTEGER(OS-GETENV("ALBSTAGES")). 

        IF OS-GETENV("ALBCOMPTABILE") <> ? THEN 
	    tmp_str      = OS-GETENV("ALBCOMPATIBLE").
        IF tmp_str BEGINS "Y" then compatible = TRUE.
        output_file = odb_dbname + "_output.tmp".
        OUTPUT TO VALUE(output_file). 
    END.


  IF odb_pdbname = "" THEN
      odb_pdbname = odb_dbname.

  IF not batch_mode and not do_it_all THEN 
      UPDATE which_stages WITH FRAME Stages_Frame.

  DO i = 1 to which_stages:
      stages[i] = TRUE. 
  END. 

  CONNECT VALUE (pro_dbname) -1 NO-ERROR.

  IF NOT CONNECTED (pro_dbname) THEN DO:
    MESSAGE "PROGRESS Database not found".
    UNDO main-blk, RETRY main-blk.
  END.

  CONNECT    VALUE (osh_dbname) -1 NO-ERROR.
  HIDE MESSAGE NO-PAUSE.

  IF CONNECTED (osh_dbname) THEN DO:
    create_h = FALSE.
    CREATE ALIAS holder FOR DATABASE VALUE (osh_dbname).
    RUN "prodict/misc/_tstsh.p" (INPUT odb_dbname, OUTPUT db_exist).
    IF db_exist THEN DO:
      MESSAGE "Database " odb_dbname " already exists in schema holder " 
                     osh_dbname.
      UNDO main-blk, RETRY main-blk.
    END. 
    DISCONNECT VALUE (osh_dbname).
  END. ELSE
    create_h = TRUE.

  ASSIGN
    user_dbname = odb_pdbname
    user_env[1] = "ALL"
    user_env[2] = osh_dbname 
    user_env[3]  = ""
    user_env[4]  = "n"
    user_env[5]  = ";"
    user_env[6]  = "n"
    user_env[7]  = "y"
    user_env[8]  = "y"
    user_env[9]  = "ALL"
    user_env[11] = "varchar" 
    user_env[12] = "date"
    user_env[13] = "integer"
    user_env[14] = "integer"
    user_env[15] = "decimal"
    user_env[16] = "decimal"
    user_env[17] = "integer"
    user_env[18] = "varchar"
    user_env[19] = "integer"
    user_env[20] = "##"
    user_env[21] = "y"  /* ALLBASE supports case-insensitive indx */
    user_env[22] = "ALLBASE"
    user_env[23] = "30"
    user_env[24] = "15"
    user_env[25] = "n"
    user_env[26] = odb_username.
 
  IF compatible THEN 
    ASSIGN user_env[27] = "y".	
  ELSE
    ASSIGN user_env[27] = "n".

  RUN "prodict/odb/_odb_md0.p".
   
 
  ASSIGN
	user_dbname = odb_dbname. 

  IF not stages[odb_create_sh] THEN LEAVE. 

  IF NOT batch_mode THEN 
      DISPLAY "" @ wait WITH FRAME sh-wait.

  CREATE DATABASE osh_dbname FROM "EMPTY".

  IF NOT batch_mode THEN 
      HIDE FRAME sh-wait NO-PAUSE.
  
  stages_complete[odb_create_sh] = TRUE.


  DISCONNECT VALUE (pro_dbname).

  IF not stages[odb_create_objects] THEN LEAVE. 

  RUN "prodict/odb/_odb_md1.p".

  IF not stages[odb_create_objects] or 
     not stages[odb_build_schema] or 
     not stages[odb_fixup_schema] THEN LEAVE. 

  /* See if we were successful. */

  IF NOT CONNECTED (odb_dbname) and NOT batch_mode THEN DO:
    cmd = "Error creating ALLBASE Database - Check logfile " + osh_dbname + ".log".
    MESSAGE cmd.
    UNDO main-blk, RETRY main-blk.
  END.

  DISCONNECT VALUE (osh_dbname).

  ASSIGN cmd = "c" + odb_dbname + ".p".

  OUTPUT STREAM strm TO VALUE (cmd) NO-MAP.

  PUT STREAM strm UNFORMATTED "CONNECT " osh_dbname " -1." SKIP.

  PUT STREAM strm UNFORMATTED "CONNECT " odb_pdbname " -ld ~"" odb_dbname
    "~" -dt ODBC -U ~"" odb_username "~" -P ~"" odb_password "~"." SKIP.

  OUTPUT STREAM strm CLOSE.

  RUN VALUE (cmd).

  IF not stages[odb_load_data] THEN LEAVE. 

  IF movedata THEN
    RUN "prodict/odb/_odb_md9.p".

  stages_complete[odb_load_data] = TRUE.

  DISCONNECT VALUE (odb_dbname).
  DISCONNECT VALUE (osh_dbname).

  IF NOT batch_mode THEN DO:
      IF OPSYS = "vms" THEN
        MESSAGE "Run progress/startup=" cmd " to connect to new ALLBASE database.".
      ELSE
        MESSAGE "Run pro -p " cmd " to connect to new ALLBASE database.".
  END.

/* If this is batch mode, make sure we close the output file we 
   opened above.
 */
IF batch_mode THEN 
    OUTPUT CLOSE.

all_done = TRUE.
END.
