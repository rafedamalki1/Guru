/************************************************************************************
	PROCEDURE: dayname.p

	PURPOSE:   Returns the day of the week for a date provided

	SYNTAX:    "RUN samples/dayname.p"

	REMARKS:   This code will accept a date and return the day of the
		   week for that day.

	PARAMETERS:NONE

	AUTHORS:   Judy Rothermal
	DATE:      February 1993

	LAST INSPECTED:
	INSPECTED BY:

 ************************************************************************************/
 /* Copyright(c) PROGRESS SOFTWARE CORPORATION, 1993 - All Rights Reserved.	    */

 /*Code_Start*/
 
DEF VAR loop     AS INT                 NO-UNDO.
DEF VAR in-date  AS DATE                NO-UNDO.
DEF VAR day-week AS CHAR FORMAT "x(10)" NO-UNDO.
DEF VAR in-sample AS DATE EXTENT 10 NO-UNDO INIT
[01/01/1990,02/20/1922,12/07/1941,05/23/1934,11/22/1963,
 09/22/1965,07/04/1976,02/01/1980,08/01/1990,07/13/1992].
DEF VAR day-name  AS CHAR NO-UNDO
   INIT "Sunday,Monday,Tuesday,Wednesday,Thursday,Friday,Saturday".

DEF VAR samples AS CHAR VIEW-AS EDITOR SIZE 35 BY 10 SCROLLBAR-VERTICAL.

DEFINE BUTTON bQUIT LABEL "EXIT" AUTO-ENDKEY.
DEFINE BUTTON bOK LABEL "OK" AUTO-GO.

FORM samples NO-LABEL WITH FRAME samples_frm ROW 3 COL 2.
FORM 
   "Date" in-date
   SKIP
   "Day "  day-week FORMAT "X(12)" SKIP(1) 
  SPACE(2) bOK SPACE(2) bQUIT
   WITH FRAME frm-date NO-LABELS VIEW-AS DIALOG-BOX TITLE "Try One".
ASSIGN
   FRAME frm-date:ROW = 5
   FRAME frm-date:COLUMN = 40.
VIEW FRAME samples_frm.

REPEAT loop = 1 TO 10:
  ASSIGN
     day-week =
       ENTRY(WEEKDAY(in-sample[loop]),day-name).
  samples:SCREEN-VALUE = samples:SCREEN-VALUE + string(in-sample[loop]) + "   " + day-week + CHR(10).
  samples:CURSOR-OFFSET = SAMPLES:LENGTH.
END.

ENABLE samples WITH FRAME samples_frm.
ENABLE in-date bOK bQUIT WITH FRAME frm-date.

REPEAT:
   UPDATE in-date WITH FRAME frm-date.      
   DO WITH FRAME frm-date:
     day-week = ENTRY(WEEKDAY(INPUT in-date),day-name).
   END. /* DO WITH FRAME */
   DISPLAY day-week WITH FRAME frm-date.
  samples:SCREEN-VALUE = samples:SCREEN-VALUE + string(in-date) + "   " + day-week + CHR(10).
  samples:CURSOR-OFFSET = samples:LENGTH.
END.    /* ON RETURN */

RETURN.



