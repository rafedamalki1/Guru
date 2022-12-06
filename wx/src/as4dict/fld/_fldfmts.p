/*************************************************************/
/* Copyright (c) 1984-1996 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/*----------------------------------------------------------------------------

File: _fldfmts.p

Description:
   Show example formats for the given data type and allow the user to
   choose one.

Input Parameters:
   p_Type - The current field data type (the numeric data type code).

Input/Output Parameters:
   p_Format - The format chosen or "" if user hit Cancel.

Author: Laura Stern

Date Created: 10/20/92 

----------------------------------------------------------------------------*/

{as4dict/dictvar.i shared}
{as4dict/uivar.i shared}
{adecomm/cbvar.i shared}
{as4dict/FLD/fldvar.i shared}


Define INPUT   	    PARAMETER p_Type   as integer  NO-UNDO.
Define INPUT-OUTPUT PARAMETER p_Format as char     NO-UNDO.

Define var ix        as integer       NO-UNDO.
Define var num       as integer       NO-UNDO.
Define var hlist     as widget-handle NO-UNDO. /* for convenience */
Define var fmt_list  as char  	      NO-UNDO.

Define var rule      as char  NO-UNDO.
Define var pre_rule  as char  NO-UNDO init "Valid format components are:".
Define var post_rule as char  NO-UNDO init "(Use Help for the full syntax.)".

&global-define NUM_CHAR_FMTS  7
Define var char_fmts as char extent {&NUM_CHAR_FMTS} NO-UNDO init
   [ "X(8)",
     "(999) 999-9999",
     "999-NNN-NNNN",
     "999-99-9999",
     "99999-9999",
     "!!",
     "A(10)"
   ].

Define var char_desc as char extent {&NUM_CHAR_FMTS} NO-UNDO init
   [ "Any 8 characters",
     "Phone #, all digits",
     "Phone #, 3 digits, 7 digits or letters",
     "Social Security Number",
     "Zip Code",
     "2 characters converted to upper case (e.g., State)",
     "Any 10 letters, no blanks"
   ].

&global-define NUM_INT_FMTS  5
Define var int_fmts as char extent {&NUM_INT_FMTS} NO-UNDO init
   [ "->,>>>,>>9",
     "99999",
     "99,999,999",
     "ZZZZZ9",
     "+>99"
   ].

Define var int_desc as char extent {&NUM_INT_FMTS} NO-UNDO init
   [ "Negative, 7 digit # with all but 1 leading 0 suppressed",
     "5 digit # (e.g., zip code)",
     "8 digit # with thousand separators",
     "6 digit #, may have leading zeroes",
     "Positive 3 digit # with at least 2 digits displayed"
   ].

&global-define NUM_DEC_FMTS  6
Define var dec_fmts as char extent {&NUM_DEC_FMTS} NO-UNDO init
   [ "->,>>>,>>9.99",
     "99,999",
     "$>,>>9.99",
     ">>,>9.99<<",
     "Total=+99,999.9",
     "+ZZZ,ZZ9.99"
   ].

Define var dec_desc as char extent {&NUM_DEC_FMTS} NO-UNDO init
   [ "Negative # with 2 decimal places, leading 0's suppressed",
     "5 digit # with thousand separators",
     "A dollar amount",
     "Floating decimal format",
     "5 digit total amount for report with 1 decimal place",
     "Positive # with leading 0's replaced with blanks"
   ].

&global-define NUM_LOG_FMTS  4
Define var log_fmts as char extent {&NUM_LOG_FMTS} NO-UNDO init
   [ "yes/no",
     "true/false",
     "male/female",
     "shipped/waiting"
   ].

/* No desc for logical - it's redundant */

&global-define NUM_DATE_FMTS  5
Define var date_fmts as char extent {&NUM_DATE_FMTS} NO-UNDO init
   [ "99/99/99",
     "99-99-99",
     "99/99/9999",
     "99-99-9999",
     "999999"
   ].

Define var date_desc as char extent {&NUM_DATE_FMTS} NO-UNDO init
   [ "2 digit year, slash separators",
     "2 digit year, dash separators",
     "4 digit year, slash separators",
     "4 digit year, dash separators",
     "2 digit year, no separators"
   ].

&global-define NUM_REC_FMTS  3
Define var rec_fmts as char extent {&NUM_REC_FMTS} NO-UNDO init
   [ ">>>>>>9",
     "ZZZZZ9",
     "******9"
   ].

Define var rec_desc as char extent {&NUM_REC_FMTS} NO-UNDO init
   [ "7 digit ID with leading 0's suppressed",
     "6 digit ID with leading 0's replaced with blanks",
     "7 digit ID with leading 0's replaced with asterisks"
   ].


/*--------------------------------Form-------------------------------------*/

form
   SKIP({&TFM_WID})
   "Valid format components are:"
      	 view-as TEXT         	       	     	at  2 SKIP
   rule  FORMAT "x(70)"  
      	 view-as TEXT         	       	     	at  5 SKIP({&VM_WIDG})

   "Some example formats:"
    	 view-as TEXT         	       	     	at  2 SKIP({&VM_WID})
   fmt_list  
      	 view-as SELECTION-LIST SINGLE 
      	 INNER-CHARS 73 INNER-LINES 7 FONT 0  	at  2 

   {adecomm/okform.i
      &BOX    = s_rect_btns
      &STATUS = no
      &OK     = s_btn_OK
      &CANCEL = s_btn_Cancel
      &HELP   = s_btn_Help}

   with frame fmt_examples 
      NO-LABELS
      DEFAULT-BUTTON s_btn_OK CANCEL-BUTTON s_btn_Cancel
      view-as DIALOG-BOX scrollable.


/*-----------------------------Triggers------------------------------------*/

/*----- HELP -----*/
on HELP of frame fmt_examples OR choose of s_btn_Help in frame fmt_examples
   RUN "adecomm/_adehelp.p" ("as4d", "CONTEXT", {&Format_Examples_Dlg_Box}, ?).


/*-----WINDOW-CLOSE-----*/
on window-close of frame fmt_examples
   apply "END-ERROR" to frame fmt_examples.


/*----- DBL-CLICK of FORMAT LIST -----*/
on default-action of fmt_list in frame fmt_examples
   apply "GO" to frame fmt_examples.


/*---------------------------Mainline code---------------------------------*/

/* Run time layout for button area. */
{adecomm/okrun.i  
   &FRAME = "frame fmt_examples" 
   &BOX   = "s_rect_Btns"
   &OK    = "s_btn_OK" 
   &HELP  = "s_btn_Help"
}

assign
   hlist = fmt_list:HANDLE in frame fmt_examples
   hlist:delimiter = "|".  /* have to pick something not in desc strings */

case p_Type:
   when {&DTYPE_CHARACTER} then
   do:
      do ix = 1 to {&NUM_CHAR_FMTS}:
   	 s_Res = hlist:add-last(STRING(char_fmts[ix],"x(17)") + char_desc[ix]).
      end.
      assign
   	 rule = "X N A ! 9 (n) <any fill characters you want>."
   	 frame fmt_examples:title = "Format Examples for Character Data Type".
   end.
   when {&DTYPE_INTEGER} then 
   do:
      do ix = 1 to {&NUM_INT_FMTS}:
   	 s_Res = hlist:add-last(STRING(int_fmts[ix],"x(17)") + int_desc[ix]).
      end.
      assign
      	 rule = "<leading string> ( ) + - , > < 9 Z * . DB CR DR " +
      	        "<trailing string>"
      	 frame fmt_examples:title = "Format Examples for Integer Data Type".
   end.
   when {&DTYPE_DECIMAL} then
   do:
      do ix = 1 to {&NUM_DEC_FMTS}:
      	 s_Res = hlist:add-last(STRING(dec_fmts[ix],"x(17)") + dec_desc[ix]).
      end.
      assign
      	 rule = "<leading string> ( ) + - , > < 9 Z * . DB CR DR " +
      	        "<trailing string>"
      	 frame fmt_examples:title = "Format Examples for Decimal Data Type".
   end.
   when {&DTYPE_LOGICAL} then
   do:
      do ix = 1 to {&NUM_LOG_FMTS}:
      	 s_Res = hlist:add-last(STRING(log_fmts[ix],"x(17)")).
      end.
      assign
      	 rule = "<any two strings>"
      	 frame fmt_examples:title = "Format Examples for Logical Data Type".
   end.
   when {&DTYPE_DATE} then
   do:
      do ix = 1 to {&NUM_DATE_FMTS}:
      	 s_Res = hlist:add-last(STRING(date_fmts[ix],"x(17)") + date_desc[ix]).
      end.
      assign
      	 rule = "9 / -"
      	 frame fmt_examples:title = "Format Examples for Date Data Type".
   end.
   when {&DTYPE_RECID} then
   do:
      do ix = 1 to {&NUM_REC_FMTS}:
   	 s_Res = hlist:add-last(STRING(rec_fmts[ix],"x(17)") + rec_desc[ix]).
      end.
      assign
   	 rule = "<leading string> ( ) > 9 Z * <trailing string>"
      	 frame fmt_examples:title = "Format Examples for RECID Data Type".
   end.
end.

/* If input format is non null and is in the list, make it select the list
   value - otherwise, select the first entry as default. 
*/
num = hlist:num-items.
if p_Format <> "" then
do:
   find_loop:
   do ix = 1 to num:
      if p_Format = TRIM(SUBSTR(hlist:entry(ix), 1, 17)) then 
      	 leave find_loop.
   end.
   if ix > num then ix = 1.  /* format not found */
end.
else 
   ix = 1.
fmt_list = hlist:entry(ix).  

display rule with frame fmt_examples.
do ON ERROR UNDO, LEAVE  ON ENDKEY UNDO, LEAVE:
   update fmt_list 
      	  s_btn_Ok 
      	  s_btn_Cancel
      	  s_btn_Help
       	  with frame fmt_examples.

   p_Format = TRIM(SUBSTR(fmt_list, 1, 17)). 
end.
