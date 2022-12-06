/*************************************************************/
/* Copyright (c) 1984-1998 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/*
NOTE:
  Do not change any text in this program.  _dctlrec.p must
  be able to parse the error log this program outputs.
*/
/*
History:

  laurief    6/9/98   Added user_env[6] to determine whether load errors
                      are displayed to screen.


   {1} - Name of the table to load
   {2} - tolerable load rate (%).  
   {3} - set size, status will be displayed after each set
   {4} - either table name or specific field names to load.
   {5} - expected # load records.  If {2} is either 0 or 100%, then
      	 this is ignored.
*/
   
/* Will be "y" or "n" to indicate whether to disable triggers or not */
DEFINE INPUT PARAMETER p_Disable AS CHARACTER NO-UNDO.

DEFINE SHARED STREAM   loaderr.
DEFINE SHARED VARIABLE errs    AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE recs    AS INTEGER. /*UNDO*/
DEFINE SHARED VARIABLE xpos    AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE ypos    AS INTEGER NO-UNDO.

DEFINE        VARIABLE errbyte AS INTEGER NO-UNDO.
DEFINE        VARIABLE errline AS INTEGER NO-UNDO.
DEFINE        VARIABLE nxtstop AS INTEGER NO-UNDO.
DEFINE        VARIABLE err%    AS INTEGER NO-UNDO.
DEFINE        VARIABLE ans999  AS LOGICAL NO-UNDO.
DEFINE        VARIABLE stopped AS LOGICAL NO-UNDO INIT FALSE.
DEFINE        VARIABLE j       AS INTEGER NO-UNDO.

DEFINE SHARED VARIABLE user_env    AS CHARACTER NO-UNDO EXTENT 35.

/* Need a little dialog instead of using PUT SCREEN for GUI because
   here, xpos and ypos will be based on fill-in height whereas frame is
   USE-TEXT so the rows won't line up. 
*/
&IF "{&WINDOW-SYSTEM}" <> "TTY" &THEN
  IF TERMINAL <> "" THEN DO:
    DEFINE STREAM run_load.
    OUTPUT STREAM run_load TO TERMINAL.
  
/*
    FORM 
      SKIP(.5)
      recs   format "ZZZZZZZ9" "Records" SPACE(1) 
      SKIP
      errs   format "ZZZZZZZ9" "Errors"  SPACE(1) 
      SKIP(.5)
      WITH FRAME run_load VIEW-AS DIALOG-BOX USE-TEXT NO-LABELS
	ROW 8 CENTERED TITLE "Loading".
*/
  END.
&ENDIF

ASSIGN
  recs = 0
  errs = 0
  err% = {2}.

IF p_Disable  = "y" THEN
  DISABLE TRIGGERS FOR LOAD OF DICTDB2.{1}.

/* Go through all load records.  When IMPORT hits the end of file or 
   ".", ENDKEY will be generated which will kick us out of this "top"
   loop.
*/
top:
DO WHILE TRUE TRANSACTION:

  nxtstop = recs + {3}.

  /* Go through set of {3} at a time */
  bottom:
  REPEAT FOR DICTDB2.{1}
    WHILE recs < nxtstop ON ENDKEY UNDO,LEAVE top:

    IF RETRY THEN DO:
      errs = errs + 1.
      DO j = 1 TO  ERROR-STATUS:NUM-MESSAGES:
          PUT STREAM loaderr UNFORMATTED
            ">> ERROR READING LINE #" errline 
            " (Offset=" errbyte  "): " ERROR-STATUS:GET-MESSAGE(j) SKIP.
      END.

      IF (err% = 0) OR
         (err% <> 100 AND errs > ({5} * {2}) / 100) THEN DO:
      	 PUT STREAM loaderr UNFORMATTED
      	    "** Tolerable load error rate is: {2}%." SKIP
      	    "** Loading table {1} is stopped after " errs " error(s)." SKIP.
      	 RETURN.
      END.
      NEXT.
    END.

    CREATE {1}.
    ASSIGN
      errbyte = SEEK(INPUT)
      errline = errline + 1
      recs    = recs + 1.
    IMPORT {4} NO-ERROR.

    IF ERROR-STATUS:ERROR THEN DO:
      IF TERMINAL <> "" AND user_env[6] = "s" THEN DO:
         DO j = 1 TO  ERROR-STATUS:NUM-MESSAGES:
       	    ans999 = yes.
            MESSAGE ERROR-STATUS:GET-MESSAGE(j) SKIP(1)
      	         "Press OK to continue or Cancel to stop processing."
      	       VIEW-AS ALERT-BOX ERROR BUTTONS OK-CANCEL UPDATE ans999.
            IF NOT ans999 THEN DO:
      	       stopped = TRUE.
      	       UNDO top, LEAVE top.
            END.
      	 END.
      END.
      UNDO bottom, RETRY bottom.
    END.   /* ERROR raised */

    VALIDATE {1} NO-ERROR.

    IF ERROR-STATUS:ERROR THEN DO:
      IF TERMINAL <> "" AND user_env[6] = "s" THEN DO:
         DO j = 1 TO  ERROR-STATUS:NUM-MESSAGES:
       	    ans999 = yes.
            MESSAGE ERROR-STATUS:GET-MESSAGE(j) SKIP(1)
      	         "Press OK to continue or Cancel to stop processing."
      	       VIEW-AS ALERT-BOX ERROR BUTTONS OK-CANCEL UPDATE ans999.
            IF NOT ans999 THEN DO:
      	       stopped = TRUE.
      	       UNDO top, LEAVE top.
            END.
      	 END.
      END.
      UNDO bottom, RETRY bottom.
    END.   /* ERROR raised */

  END. /* end bottom repeat */

/*
  IF TERMINAL <> "" THEN DO:
    &IF "{&WINDOW-SYSTEM}" = "TTY" &THEN
    IF xpos <> ? THEN DO:
      PUT SCREEN ROW ypos COLUMN xpos COLOR MESSAGES ATTR-SPACE
        STRING(recs,"ZZZZZZZ9").
      PUT SCREEN ROW ypos COLUMN xpos + 9 COLOR MESSAGES ATTR-SPACE
        STRING(errs,"ZZZZZZZ9").
    END.
    &ELSE
    IF xpos <> ? THEN
      DISPLAY STREAM run_load recs errs WITH FRAME run_load.
    &ENDIF
  END.
*/
END. /* top */

/*
&IF "{&WINDOW-SYSTEM}" <> "TTY" &THEN
  IF TERMINAL <> "" THEN DO:
    HIDE STREAM run_load FRAME run_load NO-PAUSE.
    OUTPUT STREAM run_load CLOSE.
  END.
&ENDIF
*/

IF stopped 
   THEN RETURN "stopped".
   ELSE RETURN.








