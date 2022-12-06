/*************************************************************/
/* Copyright (c) 1984-1997 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/*----------------------------------------------------------------------------

File: report.p

Description:
   Quick and dirty report to the screen with option to print.  This handles
   table, field, index, view and user reports.
 
Input Parameters:
   p_DbId    - Id of the _Db record for this database.
   p_Header  - Text string for the second line of the header.  For 
	       dictionary reports, it should look like
	       "Database: database name (type)           Table: xxxx"
	       with the Table part being optional.
   p_Title   - Title for the dialog box showing the report on the screen.
   p_Flags   - Text string for key to "flags" column, if there is one.
   p_Btns    - Character string indicating which optional buttons
	       should be visible.  These include "s" for switch files,
	       "o" for order fields by _Order or "a" to order fields
	       alphabetically.  
	       So this could be "", "s", "so", "sa", "o", OR "a".
   p_Func    - Function to call to display the report data.
   p_Tbl     - Optional table name for index and field reports. (i.e., may
	       be "".
   p_Help    - The help context for this report.

Outputs:
   Indicates actions that occurred within report:
   "s" if Switch file was chosen
   "o" if Order was active and the last order shown was by order#
   "a" if Order was active and the last order shown was alphabetical
   So this could be "", "s", "so", "sa", "o", OR "a".

Author: Laura Stern

Date Created: 10/02/92

HISTORY


	nordhougen  08/22/95    Initialized p_flags:width to its content's 
				length prior to the format calculation 
	hutegger    95/01/19    changed p_flags:format calculation to not
				ignore frame-width anymore
	gfs         07/22/94    Removed MUST-EXIST option from Files dialog.
	gfs         11/10/94    Fixed dimensions so that the dialog fits on
				Jpn WIN

----------------------------------------------------------------------------*/
&GLOBAL-DEFINE WIN95-BTN YES
/*
{C:\DELAD\pro9\guru\wx\src\adecomm/commeng.i}  /* Help contexts */
{C:\DELAD\pro9\guru\wx\src\adecomm/adestds.i}  /* Standard layout defines, colors etc. */

{C:\DELAD\pro9\guru\wx\src\adecomm/adeintl.i}  /* International support */
*/
/*
DEFINE INPUT PARAMETER p_DbId    AS RECID NO-UNDO.
*/
DEFINE INPUT PARAMETER p_Header  AS CHAR  NO-UNDO.
DEFINE INPUT PARAMETER p_Title   AS CHAR  NO-UNDO.
DEFINE INPUT PARAMETER p_Flags   AS CHAR  NO-UNDO.
DEFINE INPUT PARAMETER p_Btns    AS CHAR  NO-UNDO.
DEFINE INPUT PARAMETER p_Func    AS CHAR  NO-UNDO.
DEFINE INPUT PARAMETER p_Tbl     AS CHAR  NO-UNDO.
DEFINE INPUT PARAMETER p_Help    AS INT   NO-UNDO.
DEFINE NEW SHARED STREAM rpt.
DEFINE NEW SHARED STREAM instream.

DEFINE VARIABLE tmpfile     AS CHARACTER NO-UNDO.
DEFINE VARIABLE stat        AS LOGICAL   NO-UNDO.
DEFINE VARIABLE ret_val     AS CHARACTER NO-UNDO INIT "".
DEFINE VARIABLE show_order  AS LOGICAL   NO-UNDO. 
DEFINE VARIABLE show_switch AS LOGICAL   NO-UNDO. 
DEFINE VARIABLE a_or_o      AS CHAR      NO-UNDO INIT "". /* alpha or order# */
DEFINE VARIABLE wid         AS INTEGER   NO-UNDO.
DEFINE VARIABLE btn_col     AS INTEGER   NO-UNDO.
DEFINE VARIABLE btn_ht      AS INTEGER   NO-UNDO.
DEFINE VARIABLE hwidg       AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE inline      AS CHAR      NO-UNDO.

/* Frame variables */
DEFINE VARIABLE edit_widg  AS CHARACTER   NO-UNDO.

DEFINE BUTTON btn_OK    LABEL "OK"     {&STDPH_OKBTN} AUTO-GO.
DEFINE BUTTON btn_Print LABEL "&Print" {&STDPH_OKBTN}.

DEFINE BUTTON btn_Switch LABEL "&Switch Tables..." 
   &IF "{&WINDOW-SYSTEM}" <> "TTY" &THEN SIZE 22 BY 1.125 &ENDIF .
DEFINE BUTTON btn_Order  LABEL "&Change Field Order" 
   &IF "{&WINDOW-SYSTEM}" <> "TTY" &THEN SIZE 22 BY 1.125 &ENDIF .

&IF "{&WINDOW-SYSTEM}" <> "TTY" &THEN
   Define button    btn_Help  label "&Help" {&STDPH_OKBTN}.

   &global-define   HLP_BTN  &HELP = "btn_Help"
&ELSE
   &global-define   HLP_BTN  
&ENDIF

/* Variable used only in print options frame */
DEFINE VARIABLE dev  AS CHARACTER   NO-UNDO 
   VIEW-AS RADIO-SET
   RADIO-BUTTONS "Printer", "PRINTER", "File", "F"
   INIT "PRINTER".
DEFINE VARIABLE fil  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE app  AS LOGICAL     NO-UNDO.
DEFINE VARIABLE siz  AS INTEGER     NO-UNDO.

DEFINE BUTTON btn_Cancel LABEL "Cancel" {&STDPH_OKBTN} AUTO-ENDKEY.
DEFINE BUTTON btn_Files  LABEL "&Files..."
   &IF "{&WINDOW-SYSTEM}" <> "TTY" &THEN SIZE 12 by 1 &ENDIF .


/*=================================Forms================================*/

/* Form for editor widget containing the report and option buttons */
FORM
   edit_widg   VIEW-AS EDITOR SCROLLBAR-V
	       /* Make width one less than we want - we add 1 later */
	       &IF "{&WINDOW-SYSTEM}" = "TTY" &THEN
		  SIZE 77 BY 13 
	       &ELSEIF "{&WINDOW-SYSTEM}" BEGINS "MS-WIN" &THEN
		  INNER-CHARS 70 INNER-LINES 17 SCROLLBAR-H LARGE
	       &ELSE
		  INNER-CHARS 75 INNER-LINES 27 SCROLLBAR-H
	       &ENDIF
	       FONT 0  /* fixed font */
   SKIP
   p_Flags     VIEW-AS TEXT

   
   

   WITH FRAME report 
      DEFAULT-BUTTON btn_OK CANCEL-BUTTON btn_OK
      NO-LABELS VIEW-AS DIALOG-BOX SCROLLABLE.



/*==========================Internal Procedures==============================*/

/*-----------------------------------------------------------
   Run the procedure which displays the actual
   schema data to the current output device.
-----------------------------------------------------------*/
PROCEDURE Display_Schema_Info:

  

   IF p_Tbl = "" THEN
      RUN VALUE(p_Func) (INPUT p_DbId).
   ELSE IF NOT show_order THEN
      RUN VALUE(p_Func) (INPUT p_DbId, INPUT p_Tbl).
   ELSE
      RUN VALUE(p_Func) (INPUT p_DbId, INPUT p_Tbl, INPUT a_or_o).

  
END.


/*============================Trigger code===================================*/

/*----- ON CHOOSE of PRINT BUTTON -----*/

/*-----WINDOW-CLOSE-----*/
ON WINDOW-CLOSE OF FRAME report
   apply "END-ERROR" to frame report.


/*----- HELP -----*/
/*----- ON CHOOSE of SWITCH BUTTON -----*/


/*----- ON CHOOSE of ORDER BUTTON -----*/


/*=============================Mainline code================================*/

/* Run time layout for button area.  
   Do it for print frame here.  This way, it's only done once. Also 
   because, if you do it in the trigger the define for eff_frame_width
   becomes local to the trigger and it screws up second call to okrun.i.
*/

/* Fix up report frame second so that eff_frame_width is available for
   button calculation below.
*/

/* Adjust contents of frame so everything always fits (3/96 tsn) */
do with frame report:
  assign  
    edit_widg:width-pixel          =  frame report:width-pixel 
			          - frame report:border-left-pixel 
		 	          - frame report:border-right-pixel 
    p_Flags:format in frame report =  substitute("x(&1)", max(1,length(p_Flags,"column":U)))
    p_Flags:width-pixel            =  min(p_Flags:width-pixel
			                , frame report:width-pixel 
			                - frame report:border-left-pixel 
			                - frame report:border-right-pixel
			                - 10
			               )
    /* Center the flags string */
    p_Flags:x in frame report      = (frame report:width-p - p_Flags:width-p - 2) / 2
    .
end.

/* Set up action buttons based on input. */

/* Make a name for a temp table to use to store report so we can view it
   with an editor widget.  Display the data to the file.  
*/


/* Do the work */
 
 

/* Show the report in the editor widget. */
ASSIGN
   edit_widg:PFCOLOR IN FRAME report = 0
   edit_widg:READ-ONLY IN FRAME report = yes
   edit_widg:SENSITIVE IN FRAME report = yes /* need this due to a GUI bug */  
   FRAME report:TITLE = p_Title
   stat = edit_widg:READ-FILE(tmpfile) IN FRAME report.

/* display frame only if report could be read into the editor-widget (TH) */

IF stat
 THEN DO:
  display p_Flags with frame report.
  ENABLE edit_widg 
       btn_Switch when show_switch
       btn_Order  when show_order
       btn_OK 
       btn_Print 
       &IF "{&WINDOW-SYSTEM}" <> "TTY" &THEN
	  btn_Help
       &ENDIF
   WITH FRAME report.

  DO ON ERROR UNDO, LEAVE  ON ENDKEY UNDO, LEAVE:
    WAIT-FOR CHOOSE of btn_OK IN FRAME report OR
	    GO OF FRAME report
      FOCUS edit_widg.
  END.

END.

/* Close up shop. */
OS-DELETE VALUE(tmpfile).
HIDE FRAME report NO-PAUSE.
HIDE MESSAGE NO-PAUSE.

RETURN ret_val + a_or_o.

