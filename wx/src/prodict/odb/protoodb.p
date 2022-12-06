/*************************************************************/
/* Copyright (c) 1984-1996 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

define input parameter p_pro_dbname as character.
define input parameter p_pro_conparms as character.
define input parameter p_osh_dbname as character.
define input parameter p_odb_dbname as character.
define input parameter p_odb_pdbname as character.
define input parameter p_odb_username as character.
define input parameter p_odb_password as character.
define input parameter p_odb_conparms as character.
define input parameter p_odb_codepage as character.
define input parameter p_movedata as logical.
define input parameter p_which_stages as integer.
define input parameter p_compatible as logical.
define input parameter p_edbtype as character.
define input parameter p_idbtype as character.

{ prodict/user/uservar.i }
{ prodict/odb/odbvar.i }

DEFINE VARIABLE cmd              AS CHARACTER.
DEFINE VARIABLE wait             AS CHARACTER.
DEFINE VARIABLE create_h         AS LOGICAL.
DEFINE VARIABLE db_exist         AS LOGICAL.
DEFINE VARIABLE batch_mode       AS LOGICAL.
DEFINE VARIABLE output_file      AS CHARACTER.
DEFINE VARIABLE which_stages     AS INTEGER INITIAL 7.
DEFINE VARIABLE l_i                AS INTEGER.
DEFINE VARIABLE i_opened_log     AS LOGICAL.
DEFINE VARIABLE all_done         AS LOGICAL.
DEFINE VARIABLE compatible       AS LOGICAL INITIAL TRUE. 
DEFINE VARIABLE run_time         AS INTEGER. 

DEFINE STREAM   strm.

  assign
    batch_mode      = SESSION:BATCH-MODE
    all_done        = FALSE
    pro_dbname      = p_pro_dbname
    pro_conparms    = p_pro_conparms    
    osh_dbname      = p_osh_dbname
    odb_dbname      = p_odb_dbname
    odb_pdbname     = p_odb_pdbname
    odb_username    = p_odb_username
    odb_password    = p_odb_password
    odb_conparms    = p_odb_conparms
    movedata        = p_movedata
    which_stages    = p_which_stages
    compatible      = p_compatible
    odb_codepage    = p_odb_codepage
    run_time        = TIME
    stages          = FALSE.

  IF odb_pdbname = "" THEN
      odb_pdbname = odb_dbname.

  DO l_i = 1 to which_stages:
      stages[l_i] = TRUE. 
  END. 

  DO l_i = 1 to num-dbs:
    if  ldbname(l_i)   <> pro_dbname
     or pro_conparms <> ""
     then DISCONNECT VALUE (LDBNAME (l_i)).
  END.

  IF batch_mode and NOT logfile_open THEN DO:
      ASSIGN
        i_opened_log = true
        logfile_open = true
        output_file  = osh_dbname + ".log".
      OUTPUT STREAM logfile TO VALUE(output_file) APPEND UNBUFFERED NO-ECHO NO-MAP.
  END.
      
  IF batch_mode THEN DO:
      PUT STREAM logfile UNFORMATTED
          " "                                              skip
          "Progress to " + p_edbtype + " Log"              skip(2)
          "Original Progress Database           : " pro_dbname    skip
          "Other Progress db connect parameters : " pro_conparms  skip
          "Progress Schema Holder name          : " osh_dbname    skip 
          p_edbtype + " Logical Database       : " odb_dbname    skip
          p_edbtype + " Physical Database      : " odb_pdbname   skip
          "Other foreign db connect parameters  : " odb_conparms  skip
          p_edbtype + " Username               : " odb_username  skip
          "Code page                            : " odb_codepage  skip
          "Move the data?                       : " movedata      skip(2).


  END.

  CONNECT VALUE (pro_dbname) VALUE (pro_conparms) -1 NO-ERROR.

  IF ERROR-STATUS:ERROR OR NOT CONNECTED (pro_dbname) THEN DO:
    DO l_i = 1 TO  ERROR-STATUS:NUM-MESSAGES:
      IF batch_mode THEN
          PUT STREAM logfile UNFORMATTED ERROR-STATUS:GET-MESSAGE(l_i) skip.
      ELSE
          MESSAGE ERROR-STATUS:GET-MESSAGE(l_i).
    END.
    IF batch_mode THEN
      PUT STREAM logfile UNFORMATTED "Unable to connect to Progress database"
         skip.
    ELSE
      MESSAGE "Unable to connect to Progress database".
    UNDO, RETURN error.
  END.

  CONNECT    VALUE (osh_dbname) -1 NO-ERROR.

  IF NOT batch_mode THEN 
      HIDE MESSAGE NO-PAUSE.

  IF CONNECTED (osh_dbname) THEN DO:
    create_h = FALSE.
    CREATE ALIAS DICTDB2 FOR DATABASE VALUE (osh_dbname).
    RUN "prodict/misc/_tstsh.p" (INPUT odb_dbname, OUTPUT db_exist).
    IF db_exist THEN DO:
      IF batch_mode THEN 
          PUT STREAM logfile UNFORMATTED 
              "Database " odb_dbname " already exists in schema holder "
              osh_dbname skip.
      ELSE
          MESSAGE "Database " odb_dbname " already exists in schema holder " 
                     osh_dbname.

      UNDO, RETURN error.
    END. 
    DISCONNECT VALUE (osh_dbname).
  END. ELSE
    create_h = TRUE.

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
    user_env[22] = p_edbtype
    user_env[23] = "30"
    user_env[24] = "15"
    user_env[25] = "y"
    user_env[26] = odb_username
    user_env[28] = "30"
    user_env[29] = "24"
    user_env[31] = "-- ** ".
 
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

  IF create_h THEN DO: 
      IF batch_mode THEN DO:
          PUT STREAM logfile UNFORMATTED
              " " skip
              "-- ++ " skip
              "-- Creating empty schema holder " skip
              "-- -- " skip(2). 
      END.

      CREATE DATABASE osh_dbname FROM "EMPTY".
  END. 

  IF NOT batch_mode THEN 
      HIDE FRAME sh-wait NO-PAUSE.
  
  stages_complete[odb_create_sh] = TRUE.


  DISCONNECT VALUE (pro_dbname).

  IF not stages[odb_create_objects] THEN LEAVE. 

  RUN "prodict/odb/_odb_md1.p".

  IF batch_mode and NOT logfile_open THEN DO:
      OUTPUT TO VALUE(output_file) APPEND UNBUFFERED NO-ECHO NO-MAP.
      logfile_open = true. 
  END.

  IF not stages[odb_create_objects] or 
     not stages[odb_build_schema] or 
     not stages[odb_fixup_schema] THEN LEAVE. 

  /* See if we were successful. */

  IF NOT CONNECTED (odb_dbname) THEN DO:
    cmd = "Error creating " + p_edbtype 
        + " Database - Check logfile " + osh_dbname + ".log".
    IF batch_mode THEN 
        PUT UNFORMATTED cmd skip. 
    ELSE 
        MESSAGE cmd.

    UNDO, RETURN error.
  END.

  DISCONNECT VALUE (osh_dbname).

  ASSIGN cmd = "c" + odb_dbname + ".p".

  OUTPUT STREAM strm TO VALUE (cmd) NO-MAP.

  PUT STREAM strm UNFORMATTED "CONNECT " osh_dbname " -1." SKIP.

  PUT STREAM strm UNFORMATTED "CONNECT " odb_pdbname " -ld ~"" odb_dbname
    "~" -dt " p_idbtype " -U ~"" odb_username "~"".
  
  IF odb_password <> ? and odb_password <> "" THEN
      PUT STREAM strm UNFORMATTED " -P ~"" odb_password "~"".
  IF odb_conparms <> ? and odb_conparms <> "" THEN 
      PUT STREAM strm UNFORMATTED " " odb_conparms. 
 
  PUT STREAM strm UNFORMATTED  "."  SKIP.

  OUTPUT STREAM strm CLOSE.

  RUN VALUE (cmd).

  IF not stages[odb_load_data] THEN LEAVE. 

  IF movedata THEN DO: 
    IF batch_mode THEN 
        PUT UNFORMATTED
            "-- ++ " skip
            "-- Loading data into the " p_edbtype " database. " skip
            "-- -- " skip(2).

    RUN "prodict/odb/_odb_md9.p".
  END. 

  stages_complete[odb_load_data] = TRUE.

  DISCONNECT VALUE (odb_dbname).
  DISCONNECT VALUE (osh_dbname).

  IF NOT batch_mode THEN DO:
      IF OPSYS = "vms" THEN
        MESSAGE
         "Run progress/startup=" cmd 
         " to connect to new " p_edbtype 
         " database.".
      ELSE
        MESSAGE
         "Run pro -p " cmd 
         " to connect to new " p_edbtype 
         " database.".
      END.
  ELSE DO: 
      PUT UNFORMATTED
        " " skip
        "-- ++ " skip
        "-- {&icaller} Complete in " STRING(TIME - run_time,"HH:MM:SS") skip
        "-- -- " skip(2).

      IF OPSYS = "vms" THEN
        PUT UNFORMATTED
          "Run progress/startup=" cmd 
          " to connect to new "   p_edbtype 
          " database."              skip.
      ELSE
        PUT UNFORMATTED             skip 
          "Run pro -p " cmd 
          " to connect to new " p_edbtype 
          " database."              skip.
      END.

/* 
 * If this is batch mode, make sure we close the output file we 
 * opened above.
 */
IF i_opened_log THEN 
    OUTPUT STREAM logfile CLOSE.

all_done = TRUE.
