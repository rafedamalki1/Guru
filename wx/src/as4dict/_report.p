/*************************************************************/
/* Copyright (c) 1984-1996 by Progress Software Corporation  */
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

        gfs     07/22/94    Removed MUST-EXIST option from Files dialog.
        DLM     01/02/95    Changed to work with PROGRESS/400 Data Dictionary
        DLM     05/08/96    Changed = MS-WINDOWS to BEGINS MS-WIN
----------------------------------------------------------------------------*/

{adecomm/commeng.i}  /* Help contexts */
{adecomm/adestds.i}  /* Standard layout defines, colors etc. */
{adecomm/adeintl.i}  /* International support */

DEFINE INPUT PARAMETER p_DbId    AS RECID NO-UNDO.
DEFINE INPUT PARAMETER p_Header  AS CHAR  NO-UNDO.
DEFINE INPUT PARAMETER p_Title   AS CHAR  NO-UNDO.
DEFINE INPUT PARAMETER p_Flags   AS CHAR  NO-UNDO.
DEFINE INPUT PARAMETER p_Btns    AS CHAR  NO-UNDO.
DEFINE INPUT PARAMETER p_Func    AS CHAR  NO-UNDO.
DEFINE INPUT PARAMETER p_Tbl     AS CHAR  NO-UNDO.
DEFINE INPUT PARAMETER p_Help    AS INT   NO-UNDO.

DEFINE NEW SHARED STREAM rpt.

DEFINE VARIABLE tmpfile     AS CHARACTER NO-UNDO.
DEFINE VARIABLE stat        AS LOGICAL   NO-UNDO.
DEFINE VARIABLE ret_val     AS CHARACTER NO-UNDO INIT "".
DEFINE VARIABLE show_order  AS LOGICAL   NO-UNDO. 
DEFINE VARIABLE show_switch AS LOGICAL   NO-UNDO. 
DEFINE VARIABLE a_or_o      AS CHAR      NO-UNDO INIT "". /* alpha or order# */
DEFINE VARIABLE wid         AS INTEGER   NO-UNDO.
DEFINE VARIABLE btn_col     AS INTEGER   NO-UNDO.
DEFINE VARIABLE btn_ht      AS INTEGER   NO-UNDO.
DEFINE VARIABLE hwidg 	    AS WIDGET-HANDLE NO-UNDO.

/* Frame variables */
DEFINE VARIABLE edit_widg  AS CHARACTER   NO-UNDO.

DEFINE BUTTON btn_OK    LABEL "OK"     {&STDPH_OKBTN} AUTO-GO.
DEFINE BUTTON btn_Print LABEL "&Print" {&STDPH_OKBTN}.

DEFINE BUTTON btn_Switch LABEL "&Switch Tables..." 
   &IF "{&WINDOW-SYSTEM}" <> "TTY" &THEN SIZE 20 BY 1 &ENDIF .
DEFINE BUTTON btn_Order  LABEL "&Change Field Order" 
   &IF "{&WINDOW-SYSTEM}" <> "TTY" &THEN SIZE 20 BY 1 &ENDIF .

&IF "{&WINDOW-SYSTEM}" <> "TTY" &THEN
   Define button    btn_Help  label "&Help" {&STDPH_OKBTN}.
   Define rectangle rect_Btns {&STDPH_OKBOX}. /* standard button rectangle */
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
      	          INNER-CHARS 75 INNER-LINES 21 SCROLLBAR-H LARGE
      	       &ELSE
      	          INNER-CHARS 75 INNER-LINES 27 SCROLLBAR-H
      	       &ENDIF
      	       FONT 0  /* fixed font */
   SKIP
   p_Flags     VIEW-AS TEXT 
   SKIP({&VM_WIDG})
   btn_Switch SPACE({&HM_BTN}) btn_Order

   {adecomm/okform.i
      &BOX    = rect_btns
      &STATUS = no
      &OK     = btn_OK
      &OTHER = "SPACE({&HM_DBTNG}) btn_Print"
      {&HLP_BTN}
   }

   WITH FRAME report 
      DEFAULT-BUTTON btn_OK CANCEL-BUTTON btn_OK
      NO-LABELS VIEW-AS DIALOG-BOX SCROLLABLE.

FORM 
   SKIP({&TFM_WID})
   
   dev LABEL "&Send Output to"           COLON 17  
   SKIP ({&VM_WID})

   fil NO-LABEL FORMAT "x(60)"           AT    19
       VIEW-AS FILL-IN NATIVE SIZE 30 BY 1         {&STDPH_FILL} SPACE(.3)
   btn_Files
   SKIP({&VM_WID})

   app LABEL "&Append to Existing File?" AT    19
       VIEW-AS TOGGLE-BOX 
   SKIP({&VM_WIDG})
   
   siz LABEL "&Page Length" FORMAT ">>9" COLON 17   {&STDPH_FILL}
   "(# Lines, 0 for Continuous)"                   	  

   {adecomm/okform.i
      &BOX    = rect_btns
      &STATUS = no
      &OK     = btn_OK
      &CANCEL = btn_Cancel
      {&HLP_BTN}
   }

   WITH frame prt_options 
      SIDE-LABELS 
      DEFAULT-BUTTON btn_Ok CANCEL-BUTTON btn_Cancel
      VIEW-AS DIALOG-BOX TITLE "Print Options".


/*==========================Internal Procedures==============================*/

/*-----------------------------------------------------------
   Run the procedure which displays the actual
   schema data to the current output device.
-----------------------------------------------------------*/
PROCEDURE Display_Schema_Info:

   run adecomm/_setcurs.p ("WAIT").

   IF p_Tbl = "" THEN
      RUN VALUE(p_Func) (INPUT p_DbId).
   ELSE IF NOT show_order THEN
      RUN VALUE(p_Func) (INPUT p_DbId, INPUT p_Tbl).
   ELSE
      RUN VALUE(p_Func) (INPUT p_DbId, INPUT p_Tbl, INPUT a_or_o).

   run adecomm/_setcurs.p ("").
END.


/*============================Trigger code===================================*/

/*----- ON CHOOSE of PRINT BUTTON -----*/
ON CHOOSE of btn_Print IN FRAME report
DO:
   /*===================Trigger section for Print===============*/

   /*----- HELP -----*/
   ON HELP of FRAME prt_options
      &IF "{&WINDOW-SYSTEM}" <> "TTY" &THEN
      	 OR choose of btn_Help in frame prt_options
      &ENDIF
      RUN "adecomm/_adehelp.p" (INPUT "comm", INPUT "CONTEXT", 
      	       	     	      	INPUT {&Print_Options},
      	       	     	        INPUT ?).


   /*-----WINDOW-CLOSE-----*/
   ON WINDOW-CLOSE OF FRAME prt_options
      apply "END-ERROR" to frame prt_options.


   /*----- OK or GO -----*/
   ON GO OF FRAME prt_OPTIONS  
   DO:
      ASSIGN 
	 dev
	 fil
	 app
	 siz.

      fil = TRIM(fil).
      IF dev <> "PRINTER" THEN DO:
       	 /* open the file to make sure we have permission */
       	 IF fil BEGINS "|" THEN
	    OUTPUT THROUGH VALUE(SUBSTRING(fil,2)) PAGE-SIZE VALUE(siz).
       	 ELSE IF app THEN
	    OUTPUT TO VALUE(fil) PAGE-SIZE VALUE(siz) APPEND.
       	 ELSE
	    OUTPUT TO VALUE(fil) PAGE-SIZE VALUE(siz).
       	 OUTPUT CLOSE.
      	 dev = fil.
      END.
   
      {as4dict/prtrpt.i
	    &Header = p_Header
      	    &Flags  = p_Flags
	    &dev    = dev
	    &app    = app
	    &siz    = siz
	    &Func   = Display_Schema_info} 
   END.

   /*----- VALUE-CHANGED of DEVICE RADIO SET -----*/
   ON VALUE-CHANGED OF dev IN FRAME prt_options
   DO:
      /* Default to continuous output for terminal, 60 for printer. */
      dev = SELF:SCREEN-VALUE.
      IF dev = "F" /* file */ THEN DO:
	 ASSIGN
      	    siz:SCREEN-VALUE IN FRAME prt_options = "0"
      	    &IF "{&WINDOW-SYSTEM}" <> "TTY" &THEN
	       fil:SENSITIVE IN FRAME prt_options = yes
	       btn_Files:SENSITIVE IN FRAME prt_options = yes
	       app:SENSITIVE IN FRAME prt_options = yes
      	    &ELSE
	       fil:VISIBLE IN FRAME prt_options = yes
	       btn_Files:VISIBLE IN FRAME prt_options = yes
	       app:VISIBLE IN FRAME prt_options = yes
      	    &ENDIF
      	    .
	 APPLY "ENTRY" TO fil IN FRAME prt_options.
      END.
      ELSE 
	 ASSIGN
      	    siz:SCREEN-VALUE IN FRAME prt_options = "60"
      	    &IF "{&WINDOW-SYSTEM}" <> "TTY" &THEN
	       fil:SENSITIVE IN FRAME prt_options = no
      	       fil:SCREEN-VALUE IN FRAME prt_options = ""
	       btn_Files:SENSITIVE IN FRAME prt_options = no
	       app:SENSITIVE IN FRAME prt_options = no
      	       app:SCREEN-VALUE IN FRAME prt_options = "no"
      	    &ELSE
	       fil:VISIBLE IN FRAME prt_options = no
	       btn_Files:VISIBLE IN FRAME prt_options = no
	       app:VISIBLE IN FRAME prt_options = no
      	    &ENDIF
      	    .
   END.

   /*----- HIT of FILE BUTTON -----*/
   on choose of btn_Files in FRAME prt_options
   do:
      Define var name       as char    NO-UNDO.
      Define var picked_one as logical NO-UNDO.
      Define var d_title    as char    NO-UNDO init "Report File".
   
      name = TRIM(fil:screen-value in FRAME prt_options).
      &IF "{&WINDOW-SYSTEM}" <> "TTY" &THEN
	 system-dialog GET-FILE 
		       name 
		       filters            "*" "*"
		       default-extension  ""
		       title 	       d_title 
		       /*must-exist*/
		       update          picked_one.
      &ELSE
	 run adecomm/_filecom.p
	     ( INPUT "" /* p_Filter */, 
	       INPUT "" /* p_Dir */ , 
	       INPUT "" /* p_Drive */ ,
	       INPUT no /* p_Save_As */ ,
	       INPUT d_title ,
               INPUT "" /* Dialog Options */ ,
	       INPUT-OUTPUT name,
	       OUTPUT picked_one ). 
      &ENDIF
   
      if picked_one then
	 fil:screen-value in FRAME prt_options = name.
   end.

   /*=======Mainline for Print processing=========*/

   /* Printer is default so adjust file and files button accordingly */
   ASSIGN
      app:SCREEN-VALUE IN FRAME prt_options = "no"
      fil:SCREEN-VALUE IN FRAME prt_options = ""
      &IF "{&WINDOW-SYSTEM}" <> "TTY" &THEN
	 fil:SENSITIVE IN FRAME prt_options = no
	 btn_Files:SENSITIVE IN FRAME prt_options = no
	 app:SENSITIVE IN FRAME prt_options = no
      &ELSE
	 fil:HIDDEN IN FRAME prt_options = yes
	 btn_Files:HIDDEN IN FRAME prt_options = yes
	 app:HIDDEN IN FRAME prt_options = yes
	 fil:SENSITIVE IN FRAME prt_options = yes
	 btn_Files:SENSITIVE IN FRAME prt_options = yes
	 app:SENSITIVE IN FRAME prt_options = yes
      &ENDIF
      .

   ASSIGN
      dev = "Printer"
      siz = 60.
   DISPLAY dev siz WITH FRAME prt_options.

   ENABLE
    	 dev
    	 siz
    	 btn_Ok 
    	 btn_Cancel
    	 &IF "{&WINDOW-SYSTEM}" <> "TTY" &THEN
            btn_Help
    	 &ENDIF
    	 WITH frame prt_options.

   /* Reset tab-order for things that will be enabled later. */
   ASSIGN
      stat = fil:MOVE-AFTER-TAB-ITEM(dev:HANDLE IN FRAME prt_options)
      stat = btn_Files:MOVE-AFTER-TAB-ITEM(fil:HANDLE IN FRAME prt_options)
      stat = app:MOVE-AFTER-TAB-ITEM(btn_Files:HANDLE IN FRAME prt_options).

   DO ON ERROR UNDO, LEAVE  ON ENDKEY UNDO, LEAVE:
      WAIT-FOR CHOOSE of btn_OK in FRAME prt_options OR 
      	 GO of frame prt_options FOCUS dev.
   END.
   HIDE FRAME prt_options.   
END.


/*-----WINDOW-CLOSE-----*/
ON WINDOW-CLOSE OF FRAME report
   apply "END-ERROR" to frame report.


/*----- HELP -----*/
ON HELP OF FRAME report 
   &IF "{&WINDOW-SYSTEM}" <> "TTY" &THEN
      OR choose of btn_Help in frame report
   &ENDIF
   RUN "adecomm/_adehelp.p" (INPUT "comm", INPUT "CONTEXT", 
      	       	     	     INPUT p_Help,
      	       	     	     INPUT ?).


/*----- ON CHOOSE of SWITCH BUTTON -----*/
ON CHOOSE of btn_Switch IN FRAME report
DO:
   ret_val = "s".
   APPLY "choose" to btn_OK IN FRAME report.
END.


/*----- ON CHOOSE of ORDER BUTTON -----*/
ON CHOOSE of btn_Order IN FRAME report
DO:
   /* This is for fields - redisplay with new order 
      (flip between alphabetical and by order#) 
   */
   a_or_o = (IF a_or_o = "o" THEN "a" ELSE "o").

   run adecomm/_setcurs.p ("WAIT").

   OS-DELETE VALUE(tmpfile).
   OUTPUT STREAM rpt TO VALUE(tmpfile). 
   RUN Display_Schema_info.
   OUTPUT STREAM rpt CLOSE. 

   stat = edit_widg:READ-FILE(tmpfile) IN FRAME report.

   run adecomm/_setcurs.p ("").
END.


/*=============================Mainline code================================*/

/* Run time layout for button area.  
   Do it for print frame here.  This way, it's only done once. Also 
   because, if you do it in the trigger the define for eff_frame_width
   becomes local to the trigger and it screws up second call to okrun.i.
*/
{adecomm/okrun.i  
   &FRAME = "frame prt_options" 
   &BOX   = "rect_Btns"
   &OK    = "btn_OK" 
   &CANCEL = "btn_Cancel"   /* to center for TTY mode */
   {&HLP_BTN}
}

/* Fix up report frame second so that eff_frame_width is available for
   button calculation below.
*/
{adecomm/okrun.i  
   &FRAME  = "frame report" 
   &BOX    = "rect_Btns"
   &OK     = "btn_OK"
   &CANCEL = "btn_Print"   /* to center for TTY mode */
   {&HLP_BTN}
}

/* Because okrun.i increases frame width to get a margin but in this
   case, we don't want the margin.
*/
edit_widg:WIDTH IN FRAME report = edit_widg:WIDTH IN FRAME report + 1.

/* Center the flags string */
IF p_Flags <> "" THEN
   ASSIGN
      wid = font-table:GET-TEXT-WIDTH(p_Flags)
      p_Flags:COLUMN IN FRAME report = (FRAME REPORT:WIDTH - wid - .5) / 2
      p_Flags:FORMAT IN FRAME report = SUBSTITUTE("x(&1)", LENGTH(p_Flags, "raw":U)).

/* Set up action buttons based on input. */
ASSIGN
   show_switch = (if INDEX(p_Btns, "s") = 0 then no else yes) 
   btn_Switch:hidden in frame report = NOT show_switch
   show_order = (IF INDEX(p_Btns, "o") > 0 OR INDEX(p_Btns, "a") > 0 
   	       	 THEN yes ELSE no)
   btn_Order:hidden in frame report = NOT show_order.

wid = 0.
IF show_switch THEN
   wid = wid + btn_Switch:WIDTH IN FRAME report.
IF show_order THEN
   ASSIGN
      a_or_o = (IF INDEX(p_BTNS, "o") > 0 THEN "o" ELSE "a")
      wid = wid + btn_Order:WIDTH IN FRAME report.
IF show_switch AND show_order THEN
   wid = wid + {&HM_BTN}.

IF show_order OR show_switch THEN
DO:
   btn_col = (eff_frame_width - wid) / 2.
   IF show_switch THEN
      ASSIGN
      	 btn_Switch:COLUMN IN FRAME report = btn_col
      	 btn_col = btn_col + btn_Switch:WIDTH IN FRAME report + {&HM_BTN}.
   IF show_order THEN
      ASSIGN
      	 btn_Order:COLUMN IN FRAME report = btn_col.
END.
ELSE DO:
   /* We have to move everything up to squish out the space for the
      buttons that aren't there!.
   */
   ASSIGN
      hwidg = btn_Switch:HANDLE in FRAME report
      btn_ht = btn_Switch:HEIGHT IN FRAME report /* + {&VM_WIDG}*/ .

   DO WHILE (hwidg NE ?):
      ASSIGN
      	 hwidg:ROW = hwidg:ROW - btn_ht
         hwidg     = hwidg:NEXT-SIBLING.
   END.

   ASSIGN
      FRAME report:SCROLLABLE = false
      &IF {&OKBOX} &THEN
      	 rect_Btns:ROW IN FRAME report = rect_Btns:ROW IN FRAME report - btn_ht
      &ELSE
      	 FRAME report:RULE-ROW = FRAME report:RULE-ROW - btn_ht
      &ENDIF
      FRAME report:HEIGHT = FRAME report:HEIGHT - btn_ht.
END.

/* Make a name for a temp table to use to store report so we can view it
   with an editor widget.  Display the data to the file.  
*/
run adecomm/_tmpfile.p (INPUT "", INPUT ".dct", OUTPUT tmpfile).

/* Do the work */
OUTPUT STREAM rpt TO VALUE(tmpfile). 
RUN Display_Schema_info.
OUTPUT STREAM rpt CLOSE. 

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

