/*************************************************************/
/* Copyright (c) 1984-1997 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/* _rundump.i - Data Dictionary file dump module */
DEFINE SHARED STREAM   dump.
DEFINE SHARED VARIABLE recs AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE xpos AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE ypos AS INTEGER NO-UNDO.

/* Will be "y" or "n" */
DEFINE INPUT PARAMETER p_Disable AS CHARACTER NO-UNDO.

DEFINE VARIABLE i AS INTEGER NO-UNDO.

/* Need a little dialog instead of using PUT SCREEN for GUI because
   here, xpos and ypos will be based on fill-in height whereas frame is
   USE-TEXT so the rows won't line up. 
*/
&IF "{&WINDOW-SYSTEM}" <> "TTY" &THEN
  IF TERMINAL <> "" THEN DO:
    DEFINE STREAM run_dump.
    OUTPUT STREAM run_dump TO TERMINAL.
  
   /*
    FORM 
      SKIP(.5)
      recs   format "ZZZZZZZ9" "Records" SPACE(1) 
      SKIP(.5)
      WITH FRAME run_dump VIEW-AS DIALOG-BOX USE-TEXT NO-LABELS
	ROW 8 CENTERED TITLE "Dumping".
    */
  END.
&ENDIF
IF p_Disable  = "y" THEN
  DISABLE TRIGGERS FOR DUMP OF DICTDB2.{1}.
recs = 0.

/* We need to have for-each construct to be able to dump views of foreign
 * databases (DataServers) and we shoul duse for-each construct to take
 * atvantage of the prefatch feature.
 * 
 * old code:
 * 
REPEAT:
  i = recs.
  DO recs = i TO i + 99:
    FIND NEXT DICTDB2.{1} {2}.
    EXPORT STREAM dump DICTDB2.{1}.
  END.

/*
  IF TERMINAL <> "" THEN DO:
    &IF "{&WINDOW-SYSTEM}" = "TTY" &THEN
      PUT SCREEN ROW ypos COLUMN xpos COLOR MESSAGES ATTR-SPACE
	STRING(recs,"ZZZZZZZ9").
    &ELSE
      DISPLAY STREAM run_dump recs WITH FRAME run_dump.
    &ENDIF
  
  END.
*/
END.
 * 
 * end old code
 */

/* new code */
for each DICTDB2.{1} {2}:

  assign recs = recs + 1.
  export stream dump DICTDB2.{1}.
  
  if   terminal       <> ""
   and recs modulo 100 = 0
   then do:  /* */
/*
    &IF "{&WINDOW-SYSTEM}" = "TTY"
     &THEN
      put screen row ypos column xpos color messages attr-space
        string(recs,"zzzzzzz9").
     &ELSE
      display stream run_dump recs with frame run_dump.
    &ENDIF
*/    
    end.
  end.  /* for each DICTDB2.{1} {2} */
/* end new code */


/*
&IF "{&WINDOW-SYSTEM}" <> "TTY" &THEN
  IF TERMINAL <> "" THEN DO:
    HIDE STREAM run_dump FRAME run_dump NO-PAUSE.
    OUTPUT STREAM run_dump CLOSE.
  END.
&ENDIF
*/
RETURN.
