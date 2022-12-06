/*************************************************************/
/* Copyright (c) 1984-1998 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/
/* Modified D. McMann 12/19/97 Changed labels added toggle boxes
                               for loading sql and moving data.
            D. McMann 01/13/98 Added new env var ora_version.
*/            


{ prodict/user/uservar.i NEW }
{ prodict/ora/oravar.i NEW }

DEFINE VARIABLE cmd           AS CHARACTER.
DEFINE VARIABLE wait          AS CHARACTER.
DEFINE VARIABLE create_h      AS LOGICAL.
DEFINE VARIABLE db_exist      AS LOGICAL.
DEFINE VARIABLE batch_mode    AS LOGICAL INITIAL no. 
DEFINE VARIABLE output_file   AS CHARACTER.
DEFINE VARIABLE tmp_str       AS CHARACTER.
DEFINE VARIABLE run_time      AS INTEGER.
DEFINE VARIABLE err-rtn       AS LOGICAL INITIAL FALSE NO-UNDO.

DEFINE STREAM   strm.

batch_mode = SESSION:BATCH-MODE.

FORM
  " "   SKIP 
  pro_dbname   FORMAT "x(32)"  view-as fill-in size 32 by 1 
    LABEL "Original PROGRESS Database" colon 38 SKIP({&VM_WID}) 
  pro_conparms FORMAT "x(256)" view-as fill-in size 32 by 1 
    LABEL "Connect parameters for PROGRESS" colon 38 SKIP({&VM_WIDG})
  osh_dbname   FORMAT "x(32)"  view-as fill-in size 32 by 1 
    LABEL "Name of Schema holder Database" colon 38 SKIP({&VM_WID})
  ora_dbname   FORMAT "x(32)"  view-as fill-in size 32 by 1 
    LABEL "Logical name for ORACLE Database" colon 38 SKIP({&VM_WIDG})
  ora_version  FORMAT ">9" validate(input ora_version = 7 or ora_version = 8,
    "Oracle Version must be either 7 or 8") view-as fill-in size 23 by 1
    LABEL "What version of ORACLE" colon 38 SKIP ({&VM_WIDG})  
  ora_username FORMAT "x(32)"  view-as fill-in size 32 by 1 
    LABEL "ORACLE Owner's Username" colon 38 SKIP({&VM_WID})
  ora_password FORMAT "x(32)"  BLANK
        view-as fill-in size 32 by 1 
        LABEL "ORACLE User's Password" colon 38 SKIP({&VM_WIDG})
  ora_conparms FORMAT "x(256)" view-as fill-in size 32 by 1 
     LABEL "ORACLE connect parameters" colon 38 SKIP({&VM_WID})
  ora_codepage FORMAT "x(32)"  view-as fill-in size 32 by 1
     LABEL "Codepage for Schema Image" colon 38 SKIP({&VM_WIDG}) SPACE(5)
  compatible view-as toggle-box LABEL "Create Extended 4GL Objects"  
  loadsql view-as toggle-box label "Load SQL" 
  movedata view-as toggle-box label "Move Data" SKIP({&VM_WIDG})
             {prodict/user/userbtns.i}
  WITH FRAME x ROW 2 CENTERED SIDE-labels 
    DEFAULT-BUTTON btn_OK CANCEL-BUTTON btn_Cancel
    &IF "{&WINDOW-SYSTEM}" <> "TTY"
  &THEN VIEW-AS DIALOG-BOX &ENDIF
  TITLE "PROGRESS DB to ORACLE Conversion".

FORM
  wait FORMAT "x" LABEL
  "Creating tables - Please wait"
  WITH FRAME table-wait ROW SCREEN-LINES - 2 COLUMN 1 NO-BOX
  &IF "{&WINDOW-SYSTEM}" <> "TTY"
  &THEN VIEW-AS DIALOG-BOX &ENDIF.

ON WINDOW-CLOSE of FRAME x
   APPLY "END-ERROR" to FRAME x.
   
ON VALUE-CHANGED of loadsql IN FRAME x DO:
  IF SELF:screen-value = "yes" THEN 
     movedata:sensitive in frame x = YES.
  ELSE DO:
     movedata:screen-value in frame x = "no".
     movedata = false.
     movedata:sensitive in frame x = NO.
  END.   
END.  

IF LDBNAME ("DICTDB") <> ? THEN
  ASSIGN pro_dbname = LDBNAME ("DICTDB").

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

  run_time = TIME.

  IF OS-GETENV("PRODBNAME")   <> ? THEN
      pro_dbname   = OS-GETENV("PRODBNAME").
  IF OS-GETENV("PROCONPARMS")   <> ? THEN
        pro_conparms = OS-GETENV("PROCONPARMS").
  IF OS-GETENV("SHDBNAME")    <> ? THEN
      osh_dbname   = OS-GETENV("SHDBNAME").
  IF OS-GETENV("ORADBNAME")   <> ? THEN
      ora_dbname   = OS-GETENV("ORADBNAME").
  IF OS-GETENV("ORAVERSION")   <> ? THEN
      ora_version   = INTEGER(OS-GETENV("ORAVERSION")). 
  ELSE
      ora_version = 7.       
  IF OS-GETENV("ORAUSERNAME") <> ? THEN
      ora_username = OS-GETENV("ORAUSERNAME").
  IF OS-GETENV("ORAPASSWORD") <> ? THEN
      ora_password = OS-GETENV("ORAPASSWORD").
  IF OS-GETENV("ORACONPARMS") <> ? THEN
      ora_conparms = OS-GETENV("ORACONPARMS").
  IF OS-GETENV("ORACLE_SID") <> ? THEN
      ora_sid = OS-GETENV("ORACLE_SID").
  IF OS-GETENV("ORACODEPAGE") <> ? THEN
      ora_codepage = OS-GETENV("ORACODEPAGE").
  IF OS-GETENV("MOVEDATA")    <> ? THEN
      tmp_str      = OS-GETENV("MOVEDATA").
  IF tmp_str BEGINS "Y" THEN movedata = TRUE.

  IF OS-GETENV("COMPATIBLE") <> ? 
  THEN DO:
      tmp_str      = OS-GETENV("COMPATIBLE").
      IF tmp_str BEGINS "Y" then compatible = TRUE.
      ELSE compatible = FALSE.
   END. 

  IF OS-GETENV("LOADSQL") <> ? THEN DO:
    tmp_str      = OS-GETENV("LOADSQL").
    IF tmp_str BEGINS "Y" then loadsql = TRUE.
    ELSE loadsql = FALSE.
  END. 
  ELSE 
    loadsql = TRUE.
 
  if   pro_dbname   = ldbname("DICTDB")
   and pro_conparms = ""
   then assign pro_conparms = "<current working database>".
  /*
   * if this is not batch mode, allow override of environment variables.
   */
  
  IF NOT batch_mode THEN 
  _updtvar: 
   DO WHILE TRUE:
    UPDATE pro_dbname
      pro_conparms
      osh_dbname
      ora_dbname
      ora_version 
      ora_username
      ora_password
      ora_conparms
      ora_codepage
      compatible
      loadsql
      movedata
      btn_OK btn_Cancel
      WITH FRAME x.
      
    IF loadsql THEN DO:
      IF Osh_dbname = "" OR osh_dbname = ? THEN DO:
        IF "{&WINDOW-SYSTEM}" <> "TTY" THEN
           MESSAGE "Schema holder database Name is required."  VIEW-AS ALERT-BOX ERROR.
        ELSE
           MESSAGE "Schema holder Database Name is required." .   
        NEXT _updtvar.
      END.

      IF ora_dbname = "" OR ora_dbname = ? THEN DO:
        IF "{&WINDOW-SYSTEM}" <> "TTY" THEN
           MESSAGE "Oracle Database Name is required."  VIEW-AS ALERT-BOX ERROR.
        ELSE
           MESSAGE "Oracle Database Name is required." .   
        NEXT _updtvar.
      END.

      IF ora_username = "" OR ora_username = ? THEN DO:
        IF "{&WINDOW-SYSTEM}" <> "TTY" THEN
           MESSAGE "Oracle User Name is required."  VIEW-AS ALERT-BOX ERROR.
        ELSE
           MESSAGE "Oracle User Name is required." .   
        NEXT _updtvar.
      END.

      IF (ora_password = "" OR ora_password = ?) AND
         (INDEX(ora_username, "/") = 0 ) THEN DO:
        IF "{&WINDOW-SYSTEM}" <> "TTY" THEN
           MESSAGE "Oracle User Password is required."  VIEW-AS ALERT-BOX ERROR.
        ELSE
           MESSAGE "Oracle User Password is required." .   
        NEXT _updtvar.
      END.

      IF (ora_conparms = "" OR ora_conparms = ?) AND
         (ora_sid = "" OR ora_sid = ?) AND
         ((INDEX(ora_username, "@") = 0) OR (INDEX(ora_password, "@") = 0)) THEN DO:
        IF "{&WINDOW-SYSTEM}" <> "TTY" THEN
           MESSAGE "Oracle connect parameters are required."  VIEW-AS ALERT-BOX ERROR.
        ELSE
           MESSAGE "Oracle connect parameters are required or ORACLE_SID must be set." .   
        NEXT _updtvar.
      END.
   
    END.      
    LEAVE _updtvar.
  END.
  ELSE DO:
    IF osh_dbname <> "" AND osh_dbname <> ? THEN
        output_file = osh_dbname + ".log".
    ELSE
        output_file = "protoora.log". 
             
    OUTPUT STREAM logfile TO VALUE(output_file) NO-ECHO NO-MAP UNBUFFERED. 
    logfile_open = true. 
    IF pro_dbname = "" OR pro_dbname = ? THEN DO:
      PUT STREAM logfile UNFORMATTED "Progress Database name is required." SKIP.
      ASSIGN err-rtn = TRUE.
    END.
    IF loadsql THEN DO:
      IF Osh_dbname = "" OR osh_dbname = ? THEN DO:
         PUT STREAM logfile UNFORMATTED  "Schema holder Database Name is required." SKIP.   
         ASSIGN err-rtn = TRUE.        
      END.
      IF ora_dbname = "" OR ora_dbname = ? THEN DO:
         PUT STREAM logfile UNFORMATTED "Oracle Database Name is required." SKIP.   
         ASSIGN err-rtn = TRUE.
      END.
      IF ora_username = "" OR ora_username = ? THEN DO:
         PUT STREAM logfile UNFORMATTED "Oracle User Name is required." SKIP.   
         ASSIGN err-rtn = TRUE.
      END.
      IF (ora_password = "" OR ora_password = ?) AND
         (INDEX(ora_username, "@") = 0 ) THEN DO:
         PUT STREAM logfile UNFORMATTED "Oracle User Password is required." SKIP.   
         ASSIGN err-rtn = TRUE.
      END.
      IF (ora_conparms = "" OR ora_conparms = ?) AND
         (ora_sid = "" OR ora_sid = ? ) AND
         (INDEX(ora_username, "@") = 0 ) THEN DO:
         PUT STREAM logfile UNFORMATTED "Oracle connect parameters are required or ORACLE_SID must be set29." SKIP.   
         ASSIGN err-rtn = TRUE.
      END.
    END.  
    IF err-rtn THEN RETURN.
    
  END. 

  RUN prodict/ora/protoor1.p.
/*
 * If this is batch mode, make sure we close the output file we
 * opened above.
 */
IF logfile_open
 THEN OUTPUT STREAM logfile CLOSE.
 
IF CONNECTED (osh_dbname) THEN 
    DISCONNECT VALUE (osh_dbname).

END.
 
 
