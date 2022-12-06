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
DEFINE VARIABLE tmp_str          AS CHARACTER. 
DEFINE VARIABLE batch_mode       AS LOGICAL.
DEFINE VARIABLE output_file      AS CHARACTER.
DEFINE VARIABLE proc-file	 AS CHARACTER.
DEFINE VARIABLE i                AS INTEGER.
DEFINE STREAM   strm.
DEFINE STREAM   proc-sql.

batch_mode = SESSION:BATCH-MODE.

main-blk:
DO ON ERROR UNDO main-blk:

  /* 
   * disconnect all databases that are currently connected...
   */
  DO WHILE LDBNAME (1) <> ?:
    DISCONNECT VALUE (LDBNAME (1)).
  END.

  ASSIGN 
      pro_dbname   = "psports"
      osh_dbname   = "holder"
      odb_dbname   = "{&iodbdbname}"
      odb_pdbname  = "sports"
      odb_conparms = "" 
      odb_username = "progress"
      odb_password = "progress"
      odb_codepage = "iso8859-1/iso_1"
      movedata     = TRUE
      proc-file    = "procsql.sql". 

  OS-DELETE VALUE(osh_dbname + ".log").

  IF batch_mode THEN DO:
      output_file = osh_dbname + ".log".
      OUTPUT STREAM logfile TO VALUE(output_file)
               UNBUFFERED NO-ECHO NO-MAP KEEP-MESSAGES APPEND.
      logfile_open = true.
  END.

  IF "{&iintdbtyp}" = "MSSQLSRV" 
  THEN DO: 
      IF OS-GETENV("{&idbtyppfx}CONPARMS") <> ? 
      THEN DO:
         odb_conparms = OS-GETENV("{&idbtyppfx}CONPARMS").
         odb_conparms = TRIM (odb_conparms).
         /* On NT, the environment variable may have quotes around it; 
                 if so, we strip the quotes here.
         */ 
         IF SUBSTRING (odb_conparms, 1, 1) = "~"" 
             THEN odb_conparms = 
                     SUBSTRING (odb_conparms, 2, LENGTH(odb_conparms) - 2). 
         END.
      ELSE DO:
        IF batch_mode 
        THEN DO:
          MESSAGE "Environment variable MSSCONPARMS has not been defined".
          MESSAGE "MS SQL Server name is unknown".
          OUTPUT STREAM logfile CLOSE.
          RETURN.
        END.
        ELSE DO:
          MESSAGE "Environment variable MSSCONPARMS has not been defined".
          MESSAGE "MS SQL Server name is unknown".
          RETURN.
        END.
      END.
    END.

  OS-DELETE VALUE(pro_dbname + ".bi").
  OS-DELETE VALUE(pro_dbname + ".db").
  OS-DELETE VALUE(pro_dbname + ".lg").

  CREATE DATABASE pro_dbname FROM "SPORTS".
  CONNECT VALUE (pro_dbname) -1 NO-ERROR.

  IF NOT CONNECTED (pro_dbname) THEN DO:
    MESSAGE "PROGRESS Database not found".
    UNDO main-blk.
  END.

  DO i = 1 to 7:
      stages[i] = TRUE.
  END.

  ASSIGN
    user_dbname = odb_pdbname
    user_env[1] = "ALL"
    user_env[2] = osh_dbname 
    user_env[3] = ""
    user_env[4] = "n"
    user_env[5] = "go"
    user_env[6] = "y"
    user_env[7] = "n"
    user_env[8] = "y"
    user_env[9] = "ALL"
    user_env[11] = "varchar"
    user_env[12] = "datetime"
    user_env[13] = "tinyint"
    user_env[14] = "integer"
    user_env[15] = "decimal(18,5)"
    user_env[16] = "decimal"
    user_env[17] = "integer"
    user_env[18] = "text"
    user_env[19] = "tinyint"
    user_env[20] = "##"
    user_env[21] = "y"
    user_env[22] = "{&iextdbtyp}"
    user_env[23] = "30"
    user_env[24] = "15"
    user_env[25] = "y"
    user_env[26] = odb_username
    user_env[27] = "y"
    user_env[28] = "30"
    user_env[29] = "24"
    user_env[30] = "n"
    user_env[31] = "-- ** ".

  OS-DELETE VALUE(osh_dbname + ".sql").
  OS-DELETE VALUE(osh_dbname + ".bi").
  OS-DELETE VALUE(osh_dbname + ".db").
  OS-DELETE VALUE(osh_dbname + ".lg").

  RUN "prodict/odb/_odb_md0.p".
 
  ASSIGN
	user_dbname = odb_dbname. 

  OS-DELETE VALUE(proc-file).
 
  OUTPUT STREAM proc-sql TO VALUE(proc-file).

  PUT STREAM proc-sql SKIP
	"if (select name from sysobjects " skip
        "  where name = 'pcust' and type = 'P' and " skip
        "        uid = (select uid from sysusers " skip
        "                      where suid = (select suid from master.dbo.syslogins " skip
        "                                           where UPPER(name) = UPPER('" odb_username "'))))" skip
        "  is not NULL" skip
        "     drop procedure pcust" skip
        "go" skip
	"create procedure pcust (@num int, @orders int out, @states int out) as" skip
	"    select customer.cust_num, customer.name, order_num " skip
        "           from customer, order_" skip
        "           where customer.cust_num = order_.cust_num and " skip
        "                 customer.cust_num > @num " skip
        "    select @orders = @@rowcount " skip 
        "    select cust_num, state.state, state_name " skip
        "           from customer, state " skip 
        "           where customer.state = state.state and " skip
        "                 customer.cust_num > @num" skip
        "    select @states = @@rowcount " skip
        "    return 0" skip
        "go" skip
        "if (select name from sysobjects " skip
        "  where name = '_BUFFER_pcust_orders' and type = 'V' and " skip
        "        uid = (select uid from sysusers " skip
        "                      where suid = (select suid from master.dbo.syslogi
ns " skip
        "                                           where UPPER(name) = UPPER('"
 odb_username "'))))" skip
        "  is not NULL" skip
        "     drop view _BUFFER_pcust_orders" skip
        "go" skip
        "create view _BUFFER_pcust_orders as " skip
        "       select customer.cust_num, customer.name, order_num " skip
        "              from customer, order_ " skip
        "              where customer.cust_num != customer.cust_num " skip
        "go " skip
        "if (select name from sysobjects " skip
        "  where name = '_BUFFER_pcust_states' and type = 'V' and " skip
        "        uid = (select uid from sysusers " skip
        "                      where suid = (select suid from master.dbo.syslogi
ns " skip
        "                                           where UPPER(name) = UPPER('"
 odb_username "'))))" skip
        "  is not NULL" skip
        "     drop view _BUFFER_pcust_states" skip
        "go" skip
        "create view _BUFFER_pcust_states as " skip
        "       select cust_num, state.state, state_name " skip
        "              from customer, state where cust_num != cust_num" skip
        "go" skip
	"exit" skip.

  OUTPUT STREAM proc-sql CLOSE.

  OS-APPEND VALUE(proc-file) VALUE(osh_dbname + ".sql"). 

  OS-DELETE VALUE(proc-file).

  CREATE DATABASE osh_dbname FROM "EMPTY".

  DISCONNECT VALUE (pro_dbname).

  RUN "prodict/odb/_odb_md1.p".

  IF batch_mode and NOT logfile_open THEN DO:
      OUTPUT TO VALUE(output_file) APPEND UNBUFFERED NO-ECHO NO-MAP 
                 KEEP-MESSAGES.
      logfile_open = true. 
  END.

  /* See if we were successful. */

  IF NOT CONNECTED (odb_dbname) and NOT batch_mode THEN DO:
    cmd = "Error creating {&iextdbtyp} Database - Check logfile " + osh_dbname + ".log".
    MESSAGE cmd.
    UNDO main-blk.
  END.

  DISCONNECT VALUE (osh_dbname).

  ASSIGN cmd = "c" + odb_dbname + ".p".

  OUTPUT STREAM strm TO VALUE (cmd) NO-MAP.

  PUT STREAM strm UNFORMATTED "CONNECT " osh_dbname " -1." SKIP.

  PUT STREAM strm UNFORMATTED "CONNECT " odb_pdbname " -ld ~"" odb_dbname
    "~" -dt {&iintdbtyp} -U ~"" odb_username "~" -P ~"" odb_password 
    "~" " odb_conparms " ." SKIP.

  OUTPUT STREAM strm CLOSE.

  RUN VALUE (cmd).

  RUN "prodict/odb/_odb_md9.p".

  DISCONNECT VALUE (odb_dbname).
  DISCONNECT VALUE (osh_dbname).

  IF OPSYS = "vms" THEN
    MESSAGE "Run progress/startup=" cmd " to connect to new {&iextdbtyp} database.".
  ELSE
    MESSAGE "Run pro -p " cmd " to connect to new {&iextdbtyp} database.".

/* If this is batch mode, make sure we close the output file we 
   opened above.
 */
IF batch_mode THEN 
    OUTPUT CLOSE.
END.
