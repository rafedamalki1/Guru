/*************************************************************/
/* Copyright (c) 1984-1998 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/* Procedure prodict/ora/protoor1.p

    Modified 11/12/97 DLM Removed disconnect of Progress Database,
                          input parameters since shared variable were
                          available and added logic not to load sql. 
             01/13/98 DLM Added check for ORACLE version > 7 so that
                          longs will be assigned correctly.               
*/    

{ prodict/user/uservar.i }
{ prodict/ora/oravar.i }

DEFINE VARIABLE cmd           AS CHARACTER.
DEFINE VARIABLE wait          AS CHARACTER.
DEFINE VARIABLE create_h      AS LOGICAL.
DEFINE VARIABLE db_exist      AS LOGICAL.
DEFINE VARIABLE batch_mode    AS LOGICAL INITIAL no. 
DEFINE VARIABLE output_file   AS CHARACTER.
DEFINE VARIABLE tmp_str       AS CHARACTER.
DEFINE VARIABLE l_i           AS INTEGER.
DEFINE VARIABLE run_time      AS INTEGER.

DEFINE STREAM strm.

/*------------------------------------------------------------------*/

assign batch_mode    = SESSION:BATCH-MODE
       run_time      = TIME.

DO l_i = 1 to num-dbs:
  if  ldbname(l_i) <> pro_dbname or pro_conparms <> "<current working database>"
  then 
      DISCONNECT VALUE (LDBNAME (l_i)).
END.


IF batch_mode THEN DO:
   PUT STREAM logfile UNFORMATTED
       " " skip
       "Progress to Oracle Log" skip(2)
       "Original Progress Database:    " pro_dbname skip
       "Other Progress db connect parameters : " pro_conparms  skip
       "Oracle Logical Database:       " ora_dbname skip
       "Version of Oracle:             " ora_version skip
       "Progress Schema Holder name:   " osh_dbname skip
       "Oracle Username:               " ora_username skip
       "Compatible structure:          " compatible skip
       "Create objects in Oracle:      " loadsql skip
       "Moved data to Oracle:          " movedata skip(2).
END.

if not connected (pro_dbname) then do:
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
    ELSE DO:
      &IF "{&WINDOW-SYSTEM}" = "TTY" &THEN
          MESSAGE "Unable to connect to Progress database".
      &ELSE
          MESSAGE "Unable to connect to Progress database" 
            VIEW-AS ALERT-BOX ERROR.
      &ENDIF
    END.            
    UNDO, RETURN error.
  END.
END.

IF loadsql THEN DO:
 
  IF not batch_mode THEN
     HIDE MESSAGE NO-PAUSE.

  IF search (osh_dbname) <> ? THEN do:
    connect osh_dbname -1 no-error.
    if ERROR-STATUS:ERROR then do:
      IF not batch_mode THEN DO:
        &IF "{&WINDOW-SYSTEM}" = "TTY" &THEN 
            MESSAGE "Can't connect to Database " osh_dbname.
        &ELSE
            MESSAGE "Can't connect to Database " osh_dbname
              VIEW-AS ALERT-BOX ERROR.
        &ENDIF.
      END.       
      ELSE 
          PUT STREAM logfile UNFORMATTED 
               "Can't connect to Database " osh_dbname skip(2).
      UNDO, RETURN error.
    end.
  end.

  IF CONNECTED (osh_dbname) THEN DO:
    create_h = FALSE.
    CREATE ALIAS DICTDB2 FOR DATABASE VALUE (osh_dbname).
    RUN "prodict/misc/_tstsh.p" (INPUT ora_dbname, OUTPUT db_exist).
    IF db_exist THEN DO:
      IF not batch_mode THEN DO:
        &IF "{&WINDOW-SYSTEM}" = "TTY" &THEN 
           MESSAGE "Database " ora_dbname " already exists in schema holder " 
                  osh_dbname.
        &ELSE 
           MESSAGE "Database " ora_dbname " already exists in schema holder " 
      	               osh_dbname VIEW-AS ALERT-BOX ERROR.
        &ENDIF
      END. 	                  	           
      ELSE 
         PUT STREAM logfile UNFORMATTED 
            "Database " ora_dbname " already exists in schema holder " 
            osh_dbname skip(2).
         UNDO, RETURN error.
    END. 
    DISCONNECT VALUE (osh_dbname).
    
  END. 
  ELSE
    create_h = TRUE.

  IF ora_dbname = pro_dbname OR ora_dbname = osh_dbname THEN DO:
    IF batch_mode THEN 
       PUT STREAM logfile UNFORMATTED 
               "Database " ora_dbname 
               " must not be the same as schema holder or PROGRESS Database"
                skip(2).
    ELSE DO:
      &IF "{&WINDOW-SYSTEM}" = "TTY" &THEN 
           MESSAGE "Database " ora_dbname 
             " must not be the same as schema holder or PROGRESS Database".
      &ELSE
           MESSAGE "Database " ora_dbname 
             " must not be the same as schema holder or PROGRESS Database"
             VIEW-AS ALERT-BOX ERROR.
      &ENDIF
    END.             
    UNDO, RETURN error.
  END.
END.

IF ora_version > 7 THEN
  ASSIGN user_env[18] = "VARCHAR2".
ELSE
  ASSIGN user_env[18] = "long".
  

ASSIGN user_env[1]  = "ALL"
       user_env[2]  = osh_dbname
       user_env[3]  = ""
       user_env[4]  = "n"
       user_env[5]  = ";"
       user_env[6]  = "y"
       user_env[7]  = "y"
       user_env[8]  = "y"
       user_env[9]  = "ALL"
       user_env[11] = "char" 
       user_env[12] = "date"
       user_env[13] = "number"
       user_env[14] = "number"
       user_env[15] = "number"
       user_env[16] = "number"
       user_env[17] = "number"
       user_env[19] = "number"
       user_env[20] = "##"
       user_env[21] = "y"
       user_env[22] = "ORACLE"
       user_env[23] = "30"
       user_env[24] = "15"
       user_env[25] = "y"
       user_env[26] = ora_username
       user_env[28] = "30"
       user_env[29] = "26"
       user_env[31] = "-- ** ".
    
IF compatible THEN 
   ASSIGN user_env[27] = "y".
ELSE
   ASSIGN user_env[27] = "no".

    /* md0: creates SQL and .d-files */
RUN "prodict/ora/_ora_md0.p".

IF loadsql THEN DO:
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
    HIDE FRAME table-wait NO-PAUSE.

  /* md1: creates SI and ORACLE-Schema using send-sql.p
   *      pulls schema into SI
   *      beautifies SI by comparing it and matching it up with 
   *            the original progress-db
   */
  RUN "prodict/ora/_ora_md1.p".

  IF batch_mode and NOT logfile_open THEN DO:
    OUTPUT TO VALUE(output_file) APPEND UNBUFFERED NO-ECHO NO-MAP.
    logfile_open = true.
  END.

  /* See if we were successful. */

  IF NOT CONNECTED (ora_dbname) THEN DO:
    cmd = "Error creating ORACLE Database - Check logfile " + osh_dbname + 
      	  ".log".
    IF NOT Batch_mode THEN
      MESSAGE cmd.
    ELSE
      PUT STREAM logfile UNFORMATTED  cmd.  
    UNDO, RETURN error.
  END.

  ASSIGN cmd = "c" + ora_dbname + ".p".

  OUTPUT STREAM strm TO VALUE (cmd) NO-MAP.

  PUT STREAM strm UNFORMATTED "CONNECT " osh_dbname " -1." SKIP.

  IF ora_conparms <> ? THEN DO:
    IF INDEX(ora_conparms,"@") <> 0 THEN 
         ora_username = ora_username + ora_conparms. 
  END. 

  IF ora_password = "" or ora_password = ? THEN
     PUT STREAM strm UNFORMATTED "CONNECT " ora_dbname " -ld ~"" ora_dbname
        "~" -dt ORACLE -U ~"" ora_username "~"".
  ELSE 
     PUT STREAM strm UNFORMATTED "CONNECT " ora_dbname " -ld ~"" ora_dbname
        "~" -dt ORACLE -U ~"" ora_username "~" -P ~"" ora_password "~"".
 
  IF ora_conparms <> ? THEN DO:
    IF INDEX(ora_conparms,"@") = 0 THEN 
         PUT STREAM strm UNFORMATTED " " ora_conparms.
  END.

  PUT STREAM strm UNFORMATTED "." SKIP.

  OUTPUT STREAM strm CLOSE.

  IF movedata THEN 
  _mvdt:
  DO:
    IF NOT CONNECTED(ora_dbname) THEN DO:
      IF batch_mode THEN
        PUT STREAM logfile UNFORMATTED
          "Database" ora_dbname " not connected.  Unable to load data."  SKIP.
       ELSE
         MESSAGE "Database" ora_dbname " NOT connected.  Unable to load data." SKIP
           VIEW-AS ALERT-BOX.
       LEAVE _mvdt.    
    END.        
    IF batch_mode THEN
       PUT STREAM logfile UNFORMATTED
           "-- ++ " skip
           "-- Loading data into the Oracle database. " skip
           "-- -- " skip(2).

    RUN "prodict/ora/_ora_md9.p".

    DISCONNECT VALUE (ora_dbname).
    DISCONNECT VALUE (osh_dbname).
  END.  
end.
IF NOT batch_mode and loadsql THEN DO:
  &IF "{&WINDOW-SYSTEM}" = "TTY" &THEN 
      MESSAGE "Run pro -p " cmd " to connect to new Oracle database.".
  &ELSE
      MESSAGE "Run pro -p " cmd " to connect to new Oracle database."
            VIEW-AS ALERT-BOX INFORMATION.
  &ENDIF
END.
ELSE IF batch_mode THEN DO:

   IF NOT logfile_open THEN DO:
      OUTPUT STREAM logfile TO VALUE(output_file) APPEND UNBUFFERED NO-ECHO NO-MAP.
      logfile_open = true.
   END.

   PUT STREAM logfile UNFORMATTED
       " "           skip
       "-- ++ "      skip
       "-- PROTOORA Complete in " STRING(TIME - run_time,"HH:MM:SS")  skip
       "-- -- "      skip(2).
   if loadsql then
       PUT STREAM logfile UNFORMATTED
          " "                                   skip 
          "Run pro -p " cmd
          " to connect to new Oracle database." skip.
END.

/*------------------------------------------------------------------*/

