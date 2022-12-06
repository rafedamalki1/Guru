/*************************************************************/
/* Copyright (c) 1984-1995 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/*----------------------------------------------------------------------

File: dump/_dmpsylk.p 

Description:
    program to dump data in 'sylk' format

Input Parameters:
    user_env[ 1] = filename to export
    user_env[ 2] = 'where' clause
    user_env[ 3] = 'by' clause
    user_env[ 4] = output file name
    user_env[ 5] = number of fields
    user_env[ 6] = field list, comma delimited
    user_env[ 7] = ?, or filename to save generated code in
    user_env[ 9] = format name for message
    user_env[14] = "y" - disable triggers, "n" - do not disable triggers

History:
    95/07   hutegger    prepend <db>.<table> to field-names in order to
                        make generated program work with multiple-schema-
                        environement

----------------------------------------------------------------------*/
/*h-*/

{ prodict/dictvar.i }
{ prodict/user/uservar.i }

DEFINE NEW SHARED VARIABLE totrecs AS INTEGER NO-UNDO.
DEFINE VARIABLE i         AS INTEGER   NO-UNDO.
DEFINE VARIABLE dbfn      AS CHARACTER NO-UNDO.
DEFINE VARIABLE qual      AS CHARACTER NO-UNDO.
DEFINE VARIABLE sort      AS CHARACTER NO-UNDO.
DEFINE VARIABLE tmpfile   AS CHARACTER NO-UNDO.

/* LANGUAGE DEPENDENCIES START */ /*----------------------------------*/
DEFINE VARIABLE new_lang AS CHARACTER EXTENT 2 NO-UNDO INITIAL [
  /* 1*/ "records were dumped in SYLK format to",
  /* 2*/ ""
].

FORM
  HEADER 
    " Exporting.   Press " +
     KBLABEL("STOP") + " to terminate the export process." format "x(60)" skip(0.5)
   WITH FRAME exporting CENTERED ROW 5 USE-TEXT.

/* LANGUAGE DEPENDENCIES END */ /*------------------------------------*/

/*--------------------------------------------------------------------*/

ASSIGN
  dbfn    = user_dbname + "." + user_env[1]
  qual    = user_env[2]
  sort    = user_env[3]
  totrecs = 0.

IF user_env[7] = ?
 THEN RUN "adecomm/_tmpfile.p" (INPUT "", INPUT ".adm", OUTPUT tmpfile).
OUTPUT TO VALUE(IF user_env[7] = ?
                 THEN tmpfile
                 ELSE user_env[7]
               ) NO-MAP.
PUT UNFORMATTED
  "DEFINE SHARED VARIABLE totrecs AS INTEGER NO-UNDO." SKIP
  "OUTPUT TO ~"" user_env[4] "~" NO-MAP." SKIP.

IF user_env[14] = "y"
 THEN PUT UNFORMATTED "DISABLE TRIGGERS FOR DUMP OF " dbfn "." SKIP.

PUT UNFORMATTED
  "PUT UNFORMATTED ~"ID;PNP~" SKIP." SKIP
  "ASSIGN totrecs = 0."                     SKIP.

PUT UNFORMATTED
  "FOR EACH " SPACE(0) dbfn         SKIP
  "  " qual " " sort ":"            SKIP
  "  ASSIGN totrecs = totrecs + 1." SKIP.

FIND DICTDB._File
  WHERE DICTDB._File._Db-recid  = drec_db
  AND   DICTDB._File._File-name = user_env[1].

DO i = 1 TO NUM-ENTRIES(user_env[6]): /* count_chosen */
  FIND DICTDB._Field OF DICTDB._File
    WHERE DICTDB._Field._Field-name = ENTRY(i,user_env[6]).

  PUT UNFORMATTED
    "  PUT UNFORMATTED ~"C;Y~" totrecs ~";X" i ";K~" ".

  IF CAN-DO("character,date",DICTDB._Field._Data-type)
   THEN PUT UNFORMATTED 
      "~"~"~"~" " dbfn + "." + DICTDB._Field._Field-name " ~"~"~"~"".
   ELSE PUT UNFORMATTED 
                  dbfn + "." + DICTDB._Field._Field-name.

  PUT UNFORMATTED " SKIP."   SKIP.

  END.

PUT UNFORMATTED
  "  END."                      SKIP
  "PUT UNFORMATTED ~"E~" SKIP." SKIP
  "OUTPUT CLOSE."               SKIP
  "RETURN."                     SKIP.
OUTPUT CLOSE.

IF user_env[7] = ?
 THEN DO:
  SESSION:IMMEDIATE-DISPLAY = yes.
  VIEW FRAME exporting.
  run adecomm/_setcurs.p ("WAIT").
   
  DO ON STOP UNDO, LEAVE:
    RUN VALUE(tmpfile).
    END.

  HIDE FRAME exporting NO-PAUSE.
  SESSION:IMMEDIATE-DISPLAY = no.
  run adecomm/_setcurs.p ("").

  MESSAGE totrecs new_lang[1] user_env[4] 
      VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
  OS-DELETE VALUE(tmpfile).
  END.

RETURN.

/*--------------------------------------------------------------------*/
