/*************************************************************/
/* Copyright (c) 1984-1997 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/*----------------------------------------------------------------------------

File: _seqprop.p

Description:
   Display the sequence properties for editing.

Author: Laura Stern

Date Created: 02/21/92 

----------------------------------------------------------------------------*/


&GLOBAL-DEFINE WIN95-BTN YES
{adedict/dictvar.i shared}
{adedict/uivar.i shared}
{adedict/SEQ/seqvar.i shared}
{adedict/capab.i}

Define var capab  AS CHAR NO-UNDO.

/*----------------------------Mainline code----------------------------------*/

/* Get the sequence type */
s_Seq_Type = s_DbCache_Type[s_DbCache_ix].  

find _File "_Sequence".
if NOT can-do(_File._Can-read, USERID("DICTDB")) then
do:
   message s_NoPrivMsg "see sequence definitions."
      view-as ALERT-BOX ERROR buttons Ok in window s_win_Browse.
   return.
end.

/* Get gateway capabilities */
run adedict/_capab.p (INPUT {&CAPAB_SEQ}, OUTPUT capab).


if LENGTH(capab) = 0 then
do:
   message "Sequences are not supported for this database type."
      view-as ALERT-BOX ERROR buttons OK.
   return.
end.


/* Don't want Cancel if moving to next seq - only when window opens */
if s_win_Seq = ? then
   s_btn_Close:label in frame seqprops = "Cancel".

/* Open the window if necessary */
run adedict/_openwin.p
   (INPUT   	  "Sequence Properties",
    INPUT   	  frame seqprops:HANDLE,
    INPUT         {&OBJ_SEQ},
    INPUT-OUTPUT  s_win_Seq).

/* Run time layout for button area. Since this is a shared frame we 
   have to avoid doing this code more than once.
*/
if frame seqprops:private-data <> "alive" then
do:
   /* okrun.i widens frame by 1 for margin */
   assign
      s_win_Seq:width = s_win_Seq:width + 1
      frame seqprops:private-data = "alive".

   {adecomm/okrun.i  
      &FRAME = "frame seqprops" 
      &BOX   = "s_rect_Btns"
      &OK    = "s_btn_OK" 
      &HELP  = "s_btn_Help"
   }
end.

/* Find the _Sequence record to edit. */
find b_Sequence where b_Sequence._Seq-Name = s_CurrSeq AND 
     b_Sequence._Db-recid = s_DbRecId.

/* Set the limit value to the upper or lower limit depending on if increment
   is positive or negative. */
if b_Sequence._Seq-Incr > 0 then
   assign
      s_Seq_Limit = b_Sequence._Seq-Max
      s_Seq_Limit:label in frame seqprops = "&Upper Limit".
else
   assign
      s_Seq_Limit = b_Sequence._Seq-Min
      s_Seq_Limit:label in frame seqprops = "&Lower Limit".

/* Set status line */
display "" @ s_Status with frame seqprops. /* clears from last time */

s_Seq_ReadOnly = (s_ReadOnly OR s_DB_ReadOnly).
if NOT s_Seq_ReadOnly then
   if NOT can-do(_File._Can-write, USERID("DICTDB")) then
   do:
      display s_NoPrivMsg + " modify sequence definitions." @ s_Status
      	 with frame seqprops.
      s_Seq_ReadOnly = true.
   end.

display b_Sequence._Seq-Name  b_Sequence._Seq-Init    b_Sequence._Seq-Incr
        s_Seq_Limit 	      b_Sequence._Cycle-Ok
        (IF INDEX(capab,{&CAPAB_OWNER})    = 0 
          then "n/a" else b_Sequence._Seq-misc[2]) @ b_Sequence._Seq-misc[2]
        (IF INDEX(capab,{&CAPAB_FOR_NAME}) = 0 
          then "n/a" else b_Sequence._Seq-misc[1]) @ b_Sequence._Seq-misc[1]
	IF INDEX(s_Seq_Type,"ORACLE") = 0 then "n/a" ELSE IF 
             b_Sequence._Seq-misc[8] = ? THEN "<Local-Db>" ELSE
             b_Sequence._Seq-misc[8] @ b_Sequence._Seq-misc[8]
        with frame seqprops.

/* Note: the order of enables will govern the TAB order. */
if s_Seq_ReadOnly then
do:
   disable all except
	  s_btn_Close
	  s_btn_Prev
	  s_btn_Next
	  s_btn_Help
	  with frame seqprops.
   enable s_btn_Close
	  s_btn_Prev
	  s_btn_Next
	  s_btn_Help
	  with frame seqprops.
   apply "entry" to s_btn_Close in frame seqprops.
end.
else do:
   enable b_Sequence._Seq-Name WHEN  INDEX(capab,{&CAPAB_RENAME}) <> 0
          b_Sequence._Seq-Init WHEN  INDEX(capab,{&CAPAB_MODIFY}) <> 0
          b_Sequence._Seq-Incr WHEN  INDEX(capab,{&CAPAB_MODIFY}) <> 0
	  s_Seq_Limit          WHEN  INDEX(capab,{&CAPAB_MODIFY}) <> 0
	  b_Sequence._Cycle-Ok WHEN  INDEX(capab,{&CAPAB_MODIFY}) <> 0
	  s_btn_OK             WHEN (INDEX(capab,{&CAPAB_MODIFY}) <> 0
	                         OR  INDEX(capab,{&CAPAB_RENAME}) <> 0)	      	  
	  s_btn_Save           WHEN (INDEX(capab,{&CAPAB_MODIFY}) <> 0
	                         OR  INDEX(capab,{&CAPAB_RENAME}) <> 0)	      	  
          s_btn_Close
      	  s_btn_Prev 	      
      	  s_btn_Next
      	  s_btn_Help
	  with frame seqprops.

   apply "entry" to b_Sequence._Seq-Name in frame seqprops.
end.







