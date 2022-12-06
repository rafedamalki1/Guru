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
DEFINE VARIABLE edbtype          AS CHARACTER. 
DEFINE VARIABLE l_txt1           AS CHARACTER. 
DEFINE VARIABLE l_txt2           AS CHARACTER. 
DEFINE VARIABLE l_txt3           AS CHARACTER. 
DEFINE VARIABLE l_txt4           AS CHARACTER. 
DEFINE VARIABLE l_txt5           AS CHARACTER. 
DEFINE VARIABLE l_txt6           AS CHARACTER. 
DEFINE VARIABLE l_txt7           AS CHARACTER. 
DEFINE VARIABLE l_txt8           AS CHARACTER. 
DEFINE VARIABLE l_txt9           AS CHARACTER. 
DEFINE VARIABLE l_txt10          AS CHARACTER. 
DEFINE VARIABLE tmp_str          AS CHARACTER. 
DEFINE VARIABLE batch_mode       AS LOGICAL.
DEFINE VARIABLE do_it_all        AS LOGICAL    INITIAL TRUE.
DEFINE VARIABLE output_file      AS CHARACTER.
DEFINE VARIABLE which_stages     AS INTEGER INITIAL 7.
DEFINE VARIABLE i                AS INTEGER.
DEFINE VARIABLE all_done         AS LOGICAL.
DEFINE VARIABLE compatible       AS LOGICAL INITIAL TRUE. 
DEFINE VARIABLE my_type          AS CHARACTER INITIAL "{&idbtyp}".
DEFINE VARIABLE run_time         AS INTEGER.
DEFINE STREAM   strm.

batch_mode = SESSION:BATCH-MODE.

FORM
  " "   SKIP
  l_txt1        FORMAT "x(43)"  AT 2
  pro_dbname    FORMAT "x(32)"  view-as fill-in size 32 by 1 SKIP({&VM_WID})
  l_txt10       FORMAT "x(43)"  AT 2  
  pro_conparms  FORMAT "x(256)" view-as fill-in size 32 by 1 SKIP({&VM_WID}) 
  l_txt2        FORMAT "x(43)"  AT 2
  osh_dbname    FORMAT "x(32)"  view-as fill-in size 32 by 1 SKIP({&VM_WIDG})
  l_txt3        FORMAT "x(43)"  AT 2
  odb_dbname    FORMAT "x(32)"  view-as fill-in size 32 by 1 SKIP({&VM_WID})
  l_txt4        FORMAT "x(43)"  AT 2
  odb_pdbname   FORMAT "x(32)"  view-as fill-in size 32 by 1 SKIP({&VM_WID})
  l_txt5        FORMAT "x(43)"  AT 2
  odb_codepage  FORMAT "x(32)"  view-as fill-in size 32 by 1 SKIP({&VM_WIDG}) 
  l_txt6        FORMAT "x(43)"  AT 2
  odb_username  FORMAT "x(32)"  view-as fill-in size 32 by 1 SKIP({&VM_WID})
  l_txt7        FORMAT "x(43)"  AT 2
  odb_password  FORMAT "x(32)"  BLANK 
                                view-as fill-in size 32 by 1 SKIP({&VM_WID})
  l_txt8        FORMAT "x(43)"  AT 2
  odb_conparms  FORMAT "x(256)" view-as fill-in size 32 by 1 SKIP
  l_txt9        FORMAT "x(43)"  AT 2
  movedata
/*  "Perform all operations:"
 *        TO 44 do_it_all                       NO-LABEL 
 *  "Add compatibility fields:"
 *          TO 44 compatible                         NO-LABEL SKIP(1) 
 */
  {prodict/user/userbtns.i}
  WITH FRAME x ROW 2 CENTERED no-labels 
    DEFAULT-BUTTON btn_OK CANCEL-BUTTON btn_Cancel
    VIEW-AS DIALOG-BOX TITLE "PROGRESS to " + edbtype + " Conversion".

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

assign
  edbtype  = {adecomm/ds_type.i
                &direction = "itoe"
                &from-type = """{&idbtyp}"""
                }
  l_txt1   = "Name of the Original PROGRESS Database:"
  l_txt10  = "Other connect parameters for PROGRESS Db:"
  l_txt2   = "Enter name of Schema holder Database:"
  l_txt3   = "Enter Logical name for " + edbtype + " Db:" 
  l_txt4   = "Enter Physical name for " + edbtype + " Db:"
  l_txt5   = "Progress/" + edbtype + " Code page:" 
  l_txt6   = "Enter the " + edbtype + " owner's login name:"
  l_txt7   = "Enter " + edbtype + " user's password:"
  l_txt8   = "Enter other connect parameters:"
  l_txt9   = "Dump and Load data:"
  .

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

{prodict/gate/gat_cpvl.i
  &frame    = "x"
  &variable = "odb_codepage"
  &adbtype  = "my_type"
  }  /* checks if codepage contains convertable code-page */

&IF "{&WINDOW-SYSTEM}" <> "TTY" &THEN
btn_Help:visible IN FRAME x = no.
&ENDIF
END.

main-blk:
DO ON ERROR UNDO main-blk, RETRY main-blk:

  all_done = FALSE. 

    IF OS-GETENV("PRODBNAME")   <> ? THEN 
        pro_dbname   = OS-GETENV("PRODBNAME").
    IF OS-GETENV("PROCONPARMS")   <> ? THEN 
        pro_conparms = OS-GETENV("PROCONPARMS").
    IF OS-GETENV("SHDBNAME")    <> ? THEN 
        osh_dbname   = OS-GETENV("SHDBNAME").
    IF OS-GETENV("{&idbtyppfx}DBNAME")   <> ? THEN 
        odb_dbname   = OS-GETENV("{&idbtyppfx}DBNAME").
    IF OS-GETENV("{&idbtyppfx}PHYDBNAME")  <> ? THEN 
        odb_pdbname  = OS-GETENV("{&idbtyppfx}PHYDBNAME").
    IF OS-GETENV("{&idbtyppfx}USERNAME") <> ? THEN 
        odb_username = OS-GETENV("{&idbtyppfx}USERNAME").
    IF OS-GETENV("{&idbtyppfx}PASSWORD") <> ? THEN 
        odb_password = OS-GETENV("{&idbtyppfx}PASSWORD").
    IF OS-GETENV("{&idbtyppfx}CONPARMS") <> ? THEN 
        odb_conparms = OS-GETENV("{&idbtyppfx}CONPARMS").
    IF OS-GETENV("MOVEDATA")    <> ? THEN 
        tmp_str      = OS-GETENV("MOVEDATA").
    IF tmp_str BEGINS "Y" THEN movedata = TRUE. 
      
    IF OS-GETENV("{&idbtyppfx}STAGES")   <> ? THEN 
        which_stages = INTEGER(OS-GETENV("{&idbtyppfx}STAGES")). 

    IF OS-GETENV("{&idbtyppfx}COMPATIBLE") <> ? THEN 
        tmp_str      = OS-GETENV("{&idbtyppfx}COMPATIBLE").
    IF tmp_str BEGINS "Y" then compatible = TRUE.
 
    IF OS-GETENV("{&idbtyppfx}CODEPAGE") <> ? THEN 
        odb_codepage = OS-GETENV("{&idbtyppfx}CODEPAGE").
    ELSE
        odb_codepage = "{&icodepage}".
    /* 
     * if this is not batch mode, allow override of environment variables.
     */
    IF NOT batch_mode THEN DO: 
        DISPLAY
            l_txt1 l_txt10 l_txt2 l_txt3
            l_txt4 l_txt5 l_txt6 
            l_txt7 l_txt8 l_txt9
            WITH FRAME x.
        UPDATE pro_dbname
            pro_conparms
            osh_dbname
            odb_dbname
            odb_pdbname
            odb_codepage
            odb_username
            odb_password
            odb_conparms
            movedata
            /* do_it_all */
            /* assume compatible */
            btn_OK btn_Cancel
            WITH FRAME x.
        END.
    ELSE DO:
        output_file = osh_dbname + ".log".
        OUTPUT STREAM logfile TO VALUE(output_file) 
            UNBUFFERED NO-ECHO NO-MAP KEEP-MESSAGES. 
        logfile_open = true. 
    END.

   /* the following routine does the actual work, it can return with
    * either an error or normal. In case of an error the on undo of
    * mainblock will catch it and retry
    */

    RUN prodict/odb/protoodb.p
      ( input pro_dbname,
        input pro_conparms, 
        input osh_dbname,
        input odb_dbname,
        input odb_pdbname,
        input odb_username,
        input odb_password,
        input odb_conparms,
        input odb_codepage,
        input movedata,
        input which_stages,
        input compatible,
        input edbtype,
        input "{&idbtyp}"
      ).

/* 
 * If this is batch mode, make sure we close the output file we 
 * opened above.
 */
IF batch_mode THEN 
    OUTPUT STREAM logfile CLOSE.

all_done = TRUE.
END.
