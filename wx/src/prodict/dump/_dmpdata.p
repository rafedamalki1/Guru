/*************************************************************/
/* Copyright (c) 1984-1998 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/* _dmpdata.p */ /**** Data Dictionary dump contents module ****/ 

/*
input:
  user_env[1] = comma-sep list of filenames to dump
  user_env[2] = directory (if >1 file in user_env[1]) or filename to dump into
  user_env[3] = "MAP <name>" or "NO-MAP" OR ""
  user_env[4] = comma separated list of "y" (yes) or "n" (no) which
                      corresponds to file list in user_env[1], indicating for each,
                             whether triggers should be disabled when the dump is done.
  user_env[5] = "<internal defaults apply>" or "<target-code-page>"
  user_env[6] = "no-alert-boxes" or something else

History:
    mcmann      98/01/28    Removed view-as dialog box 97-11-24-002
    hutegger    95/01/24    single-files in multiple schemas
    hutegger    94/02/22    added code-page-stuff
    
*/
/*h-*/

{ prodict/dictvar.i }
{ prodict/user/uservar.i }

DEFINE NEW SHARED STREAM   dump.
DEFINE NEW SHARED VARIABLE recs AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE xpos AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE ypos AS INTEGER NO-UNDO.

DEFINE VARIABLE cerr        AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cntr        AS INTEGER   NO-UNDO.
DEFINE VARIABLE fil        AS CHARACTER NO-UNDO.
DEFINE VARIABLE i        AS INTEGER   NO-UNDO.
DEFINE VARIABLE loop        AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lots        AS LOGICAL   NO-UNDO.
DEFINE VARIABLE mdy        AS CHARACTER NO-UNDO.
DEFINE VARIABLE msg        AS CHARACTER NO-UNDO.
DEFINE VARIABLE stamp        AS CHARACTER NO-UNDO.
DEFINE VARIABLE useix        AS CHARACTER NO-UNDO.
DEFINE VARIABLE yy        AS INTEGER   NO-UNDO.
DEFINE VARIABLE stopped AS LOGICAL   NO-UNDO init true.

FORM
  DICTDB._File._File-name FORMAT "x(32)" LABEL "Table" AT 2 ATTR-SPACE
  SPACE(0) fil            FORMAT "x(32)" LABEL "Dump File"
  msg                     FORMAT "x(9)"  LABEL "Records" ATTR-SPACE SPACE(1)
  HEADER 
    " Dumping Data.   Press " + 
    KBLABEL("STOP") + " to terminate the dump process." format "x(66)" SKIP(1)
  WITH FRAME dumpdata NO-ATTR-SPACE USE-TEXT SCROLLABLE
  SCREEN-LINES - 8 DOWN ROW 2 CENTERED &IF "{&WINDOW-SYSTEM}" <> "TTY"
  &THEN THREE-D TITLE "Dump Table Contents" &ENDIF.

PAUSE 0.
SESSION:IMMEDIATE-DISPLAY = yes.
VIEW FRAME dumpdata.
run adecomm/_setcurs.p ("WAIT").

IF  user_env[5] = " "  
 OR user_env[5] = ?  THEN assign user_env[5] = "<internal defaults apply>".

RUN "prodict/_dctyear.p" (OUTPUT mdy,OUTPUT yy).

ASSIGN
  cntr = 0
  lots = INDEX(user_env[1],",") > 0
  loop = TRUE. /* use this to mark initial entry into loop */

PAUSE 5 BEFORE-HIDE.
DO ON STOP UNDO, LEAVE:
  DO FOR DICTDB._File WHILE ENTRY(1,user_env[1]) <> "" ON ERROR UNDO,NEXT:
    FIND DICTDB._File
      WHERE DICTDB._File._Db-recid = drec_db AND 
                  DICTDB._File._File-name = ENTRY(1,user_env[1]).
    user_env[1] = SUBSTRING(user_env[1]
                           ,LENGTH(ENTRY(1,user_env[1]),"character") + 2
                           ,-1
                           ,"character"
                           ).
  
    IF loop THEN .
    ELSE IF FRAME-LINE(dumpdata) = FRAME-DOWN(dumpdata) THEN
      UP FRAME-LINE(dumpdata) - 1 WITH FRAME dumpdata.
    ELSE
      DOWN 1 WITH FRAME dumpdata.
  
    ASSIGN
      fil  = user_env[2] 
           + ( IF lots
                 THEN ( IF DICTDB._File._Dump-name = ?
                          THEN DICTDB._File._File-name
                          ELSE DICTDB._File._Dump-name
                      ) + ".d"
                 ELSE ""
             )
      loop = FALSE
      recs = 0.
  
    DISPLAY DICTDB._File._File-name fil "Dumping" @ msg WITH FRAME dumpdata.
    COLOR DISPLAY MESSAGES DICTDB._File._File-name fil msg
      WITH FRAME dumpdata.

    ASSIGN
      xpos  = FRAME-COL(dumpdata) + 69
      ypos  = FRAME-ROW(dumpdata) + FRAME-LINE(dumpdata) + 5
      stamp = STRING(YEAR( TODAY),"9999") + "/"
            + STRING(MONTH(TODAY),"99"  ) + "/"
            + STRING(DAY(  TODAY),"99"  ) + "-"
            + STRING(TIME,"HH:MM:SS")
      cerr  = TRUE
      useix = " NO-LOCK".
  
    IF DICTDB._File._Prime-Index <> ? AND user_dbtype = "PROGRESS" THEN DO:
      FIND DICTDB._Index WHERE RECID(DICTDB._Index) = DICTDB._File._Prime-Index.
      IF NOT DICTDB._Index._Active THEN DO:
        FIND FIRST DICTDB._Index OF DICTDB._File WHERE DICTDB._Index._Active NO-ERROR.
        IF NOT AVAILABLE DICTDB._Index THEN DO:
          DISPLAY "Error!" @ msg WITH FRAME dumpdata.
          COLOR DISPLAY NORMAL DICTDB._File._File-name fil msg WITH FRAME dumpdata.
          MESSAGE
            "Cannot dump PROGRESS data when all indexes for a table inactive.".
          NEXT.
        END.
        useix = "USE-INDEX " + DICTDB._Index._Index-name + useix.
      END.
    END.
 
    DO ON ERROR UNDO,LEAVE:      /* code-page-stuf <hutegger> 94/02 */
      IF  user_env[3] = "" 
       OR user_env[3] = "NO-MAP" 
       THEN DO:
        IF  user_env[5] = "<internal defaults apply>" 
         THEN OUTPUT STREAM dump TO VALUE(fil) NO-ECHO NO-MAP
            NO-CONVERT.
         ELSE OUTPUT STREAM dump TO VALUE(fil) NO-ECHO NO-MAP
            CONVERT SOURCE SESSION:CHARSET TARGET user_env[5].
        END.
       ELSE DO:
        IF  user_env[5] = "<internal defaults apply>"  
         THEN OUTPUT STREAM dump TO VALUE(fil) NO-ECHO 
            MAP VALUE(SUBSTRING(user_env[3],5,-1,"character"))
            NO-CONVERT.
         ELSE OUTPUT STREAM dump TO VALUE(fil) NO-ECHO 
              MAP VALUE(SUBSTRING(user_env[3],5,-1,"character"))
            CONVERT SOURCE SESSION:CHARSET TARGET user_env[5].
        END.
      cerr = FALSE.
    END.
  
    IF cerr THEN DO:
      DISPLAY "Error!" @ msg WITH FRAME dumpdata.
      COLOR DISPLAY NORMAL DICTDB._File._File-name fil msg WITH FRAME dumpdata.
      NEXT.
    END.
  
    CREATE ALIAS "DICTDB2" FOR DATABASE VALUE(user_dbname).

    RUN "prodict/misc/_rundump.i" (INPUT ENTRY(1, user_env[4]))
                VALUE(_File._File-name) VALUE(useix).
    user_env[4] = SUBSTRING(user_env[4]
                           ,LENGTH(ENTRY(1,user_env[4]),"character") + 2
                           ,-1
                           ,"character"
                           ).
  

/*------------------ Trailer-INFO ------------------*/


  {prodict/dump/dmptrail.i
    &entries      = "PUT STREAM dump UNFORMATTED
                      ""filename=""   DICTDB._File._File-name SKIP
                      ""records=""    STRING(recs,""999999999"") SKIP
                      ""ldbname=""    LDBNAME(user_dbname) SKIP
                      ""timestamp=""  stamp SKIP
                      ""numformat=""  SUBSTRING(STRING(1,""9.""),2,1,""character"") SKIP
                      ""dateformat="" mdy STRING(- yy) SKIP.
                    IF user_env[3] = ""NO-MAP"" THEN
                      PUT STREAM dump UNFORMATTED ""map=NO-MAP"" SKIP.
                    ELSE
                    IF user_env[3] <> """" THEN
                      PUT STREAM dump UNFORMATTED ""map=MAP:"" 
                        SUBSTRING(user_env[3],4,-1,""character"") SKIP.
                    "  
    &seek-stream  = "dump"
    &stream       = "STREAM dump"
    }  /* adds trailer with code-page-entrie to end of file */
    
/*------------------ Trailer-INFO ------------------*/

    OUTPUT STREAM dump CLOSE.
  
    COLOR DISPLAY NORMAL DICTDB._File._File-name fil msg WITH FRAME dumpdata.
    DISPLAY DICTDB._File._File-name fil STRING(recs,">ZZZZZZZ9") @ msg
      WITH FRAME dumpdata.
    cntr = cntr + 1.
  
  END. /* for each DICTDB._File */

  stopped = false.
END.  /* on stop */

DO WHILE FRAME-LINE(dumpdata) < FRAME-DOWN(dumpdata):
  DOWN 1 WITH FRAME dumpdata.
  CLEAR FRAME dumpdata NO-PAUSE.
END.
run adecomm/_setcurs.p ("").

if user_env[6] = "no-alert-boxes"
then do:  /* output WITHOUT alert-box */

  IF stopped THEN
   MESSAGE "Dump terminated.".
  ELSE
   &IF "{&WINDOW-SYSTEM}" = "TTY" &THEN
      MESSAGE "Dump of database contents completed:" 
                    cntr "table(s) dumped successfully.".
      pause.
   &ELSE
      MESSAGE "Dump of database contents completed:" SKIP
              cntr "table(s) dumped successfully.".
   &ENDIF
  
end.      /* output WITHOUT alert-box */

else do:  /* output WITH alert-box */

  IF stopped THEN
   MESSAGE "Dump terminated."
                 VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
  ELSE
   &IF "{&WINDOW-SYSTEM}" = "TTY" &THEN
      MESSAGE "Dump of database contents completed:" 
                    cntr "table(s) dumped successfully.".
      pause.
   &ELSE
      MESSAGE "Dump of database contents completed:" SKIP
              cntr "table(s) dumped successfully."
              VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
   &ENDIF
  
end.     /* output WITH alert-box */

HIDE FRAME dumpdata NO-PAUSE.
SESSION:IMMEDIATE-DISPLAY = no.
RETURN.


