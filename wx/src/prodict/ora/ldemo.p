/*************************************************************/
/* Copyright (c) 1984-1996 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/*--------------------------------------------------------------------

File: prodict/ora/ldemo.p

Description:
    ....
    
Input-Parameters:  
    none
                                                
Output-Parameters: 
    none
    
History:
    hutegger    95/05   added user_env initializations
    
--------------------------------------------------------------------*/
/*h-*/


{ prodict/ora/oravar.i NEW }
{ prodict/user/uservar.i NEW }

DEFINE VARIABLE cmd      AS CHARACTER.
DEFINE VARIABLE wait     AS CHARACTER.
DEFINE VARIABLE create_h AS LOGICAL.
DEFINE VARIABLE db_exist AS LOGICAL.
DEFINE STREAM   strm.

/*****************************************************************************/

OUTPUT TO "ldemo2.log".

ASSIGN  pro_dbname =    "demo"
	ora_dbname =    "oraodemo"
	osh_dbname =    "odemo"
	ora_username =  "odemo"
	ora_password =  "odemo"
	movedata = TRUE.

/*****************************************************************************/

FORM
  " "   SKIP
  "Name of the Original PROGRESS Database:"
	TO 50 pro_dbname        FORMAT "x(32)"  AT 42 NO-LABEL SKIP (2)
  "Enter name of Schema holder Database:"
	TO 50 osh_dbname        FORMAT "x(32)"  AT 42 NO-LABEL SKIP
  "Enter Logical name for ORACLE Database:"
	TO 50 ora_dbname        FORMAT "x(32)"  AT 42 NO-LABEL SKIP (2)
  "Enter the ORACLE owner's Username:"
	TO 50 ora_username      FORMAT "x(32)"  AT 42 NO-LABEL SKIP
  "Enter ORACLE user's password:"
	TO 50 ora_password      FORMAT "x(32)"  AT 42 NO-LABEL SKIP (2)
  "Dump and Load data:"
	TO 50 movedata                          AT 42 NO-LABEL SKIP (2)
  WITH FRAME x ROW 12 TITLE "PROGRESS to ORACLE Conversion" CENTERED.

FORM
  wait FORMAT "x" LABEL
  "Creating tables - Please wait"
  WITH FRAME table-wait ROW SCREEN-LINES - 2 COLUMN 1 NO-BOX.

IF LDBNAME (1) <> ? THEN
  ASSIGN pro_dbname = LDBNAME (1).

main-blk:
DO ON ERROR UNDO main-blk, RETRY main-blk:

  DO WHILE LDBNAME (1) <> ?:
    DISCONNECT VALUE (LDBNAME (1)).
  END.

  CONNECT    VALUE (pro_dbname) -1 NO-ERROR.

  IF NOT CONNECTED (pro_dbname) THEN DO:
    MESSAGE "PROGRESS Database not found".
    UNDO main-blk, RETRY main-blk.
  END.

  ASSIGN
    user_env[1]  = "ALL"
    user_env[2]  = osh_dbname
    user_env[3]  = ""
    user_env[4]  = "n"
    user_env[5]  = ";"
    user_env[6]  = "y"
    user_env[7]  = "n"
    user_env[8]  = "y"
    user_env[9]  = "ALL"
    user_env[11] = "char" 
    user_env[12] = "date"
    user_env[13] = "number"
    user_env[14] = "number"
    user_env[15] = "number"
    user_env[16] = "number"
    user_env[17] = "number"
    user_env[18] = "long"
    user_env[19] = "number"
    user_env[20] = "##"
    user_env[21] = "y"
    user_env[22] = "ORACLE"
    user_env[23] = "30"
    user_env[24] = "15"
    user_env[25] = "y"
    user_env[26] = ora_username
    user_env[27] = "y"
    user_env[31] = "-- ** ".

  CONNECT    VALUE (osh_dbname) -1 NO-ERROR.
  HIDE MESSAGE NO-PAUSE.

  IF CONNECTED (osh_dbname) THEN DO:
    create_h = FALSE.
    CREATE ALIAS holder FOR DATABASE VALUE (osh_dbname).
    RUN prodict/misc/_tstsh.i (INPUT ora_dbname, OUTPUT db_exist).
    IF db_exist THEN DO:
      MESSAGE "Database " ora_dbname " already exists in schema holder " osh_dbname.
      UNDO main-blk, RETRY main-blk.
    END. 
    DISCONNECT VALUE (osh_dbname).
  END. ELSE
  create_h = TRUE.

  RUN prodict/ora/_ora_md0.p.

  DISPLAY "" @ wait WITH FRAME table-wait.

  IF OPSYS = "unix" THEN DO:
    ASSIGN cmd =        "sqlplus " + ora_username + "/" + ora_password +
      " < " + osh_dbname + ".sql >ldemo1.log".

    UNIX SILENT VALUE (cmd).

    ASSIGN cmd =        "prodb " + osh_dbname + " empty >>ldemo1.log".

    IF create_h THEN
      UNIX SILENT VALUE (cmd).
  END. ELSE IF OPSYS = "vms" THEN DO:                           /* GUESS */
    ASSIGN cmd =        "sqlplus " + ora_username + "/" + ora_password +
                        " @" + osh_dbname + ".sql".

    VMS SILENT VALUE (cmd).

    ASSIGN cmd =        "progress/create " + osh_dbname + " empty".

    IF create_h THEN
      VMS SILENT VALUE (cmd).
  END. ELSE IF CAN-DO("msdos,win32",OPSYS) THEN DO:             /* GUESS */
    ASSIGN cmd =        "sqlplus " + ora_username + "/" + ora_password +
      " " + osh_dbname + ".sql".

    DOS SILENT VALUE (cmd).

    ASSIGN cmd =        "prodb " + osh_dbname + " empty".

    IF create_h THEN
      DOS SILENT VALUE (cmd).
  END. ELSE IF OPSYS = "os2" THEN DO:                           /* GUESS */
    ASSIGN cmd =        "sqlplus " + ora_username + "/" + ora_password +
      " " + osh_dbname + ".sql".

    OS2 SILENT VALUE (cmd).

    ASSIGN cmd =        "prodb " + osh_dbname + " empty".

    IF create_h THEN
      OS2 SILENT VALUE (cmd).
  END.

  HIDE FRAME table-wait NO-PAUSE.

  DISCONNECT VALUE (pro_dbname).

  RUN prodict/ora/_ora_md1.p.

  /* See if we were successful. */

  IF NOT CONNECTED (ora_dbname) THEN DO:
    cmd = "Error creating ORACLE Database - Check logfiles ldemo*.log".
    MESSAGE cmd.
    UNDO main-blk, RETRY main-blk.
  END.

  DISCONNECT VALUE (osh_dbname).

  ASSIGN cmd = "codemo.p".

  OUTPUT STREAM strm TO VALUE (cmd).

  PUT STREAM strm UNFORMATTED "CONNECT " osh_dbname " -1." SKIP.

  PUT STREAM strm UNFORMATTED "CONNECT " ora_dbname " -ld ~"" ora_dbname
    "~" -dt ORACLE -U ~"" ora_username "~" -P ~"" ora_password "~"." SKIP.

  OUTPUT STREAM strm CLOSE.

  RUN VALUE (cmd).

  IF movedata THEN
    RUN prodict/ora/_ora_md9.p.

  DISCONNECT VALUE (ora_dbname).
  DISCONNECT VALUE (osh_dbname).

  IF OPSYS = "vms" THEN
    MESSAGE "Run progress/startup=" cmd " to connect to new ORACLE database.".
  ELSE
    MESSAGE "Run pro -p " cmd " to connect to new ORACLE database.".

END.

