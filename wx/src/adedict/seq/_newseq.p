/*************************************************************/
/* Copyright (c) 1984-1997 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/*----------------------------------------------------------------------------

File: _newseq.p

Description:
   Display and handle the add sequence dialog box and then add the sequence
   if the user presses OK.

Author: Laura Stern

Date Created: 02/20/92 

----------------------------------------------------------------------------*/


&GLOBAL-DEFINE WIN95-BTN YES
{adedict/dictvar.i shared}
{adedict/brwvar.i shared}
{adedict/menu.i shared}
{adedict/uivar.i shared}
{adedict/SEQ/seqvar.i shared}
{adedict/capab.i}


DEFINE VAR added     AS LOGICAL NO-UNDO INIT no.
Define var capab     AS CHAR    NO-UNDO.


/*-------------------------------Triggers------------------------------------*/

/* Triggers shared by create and edit triggers */
{adedict/SEQ/seqtrig.i &frame = "frame newseq"}


/*----- HIT of OK BUTTON -----*/
on choose of s_btn_OK in frame newseq
   s_OK_Hit = yes.
   /* The GO trigger will fire after this. */


/*----- HIT of OK or ADD (auto-go) -----*/
on GO of frame newseq 
do:
   run adedict/SEQ/_saveseq.p
      (b_Sequence._Seq-name:HANDLE in frame newseq,
       input frame newseq b_Sequence._Seq-Incr,
       input frame newseq s_Seq_Limit,
       b_Sequence._Seq-Init:HANDLE in frame newseq,
       input frame newseq b_Sequence._Cycle-Ok).

   if RETURN-VALUE = "error" then
   do:
      s_OK_Hit = no.
      return NO-APPLY.
   end.
   else added = yes.
end.


/*-----WINDOW-CLOSE-----*/
on window-close of frame newseq
   apply "END-ERROR" to frame newseq.


/*----- HELP -----*/
on HELP of frame newseq OR choose of s_btn_Help in frame newseq
   RUN "adecomm/_adehelp.p" ("dict", "CONTEXT", {&Create_Sequence_Dlg_Box}, ?).


/*----------------------------Mainline code----------------------------------*/

find _File "_Sequence".
if NOT can-do(_File._Can-create, USERID("DICTDB")) then
do:
   message s_NoPrivMsg "create sequences."
      view-as ALERT-BOX ERROR buttons Ok.
   return.
end.

/* Get gateway capabilities */
run adedict/_capab.p (INPUT {&CAPAB_SEQ}, OUTPUT capab).

if INDEX(capab, {&CAPAB_ADD}) = 0 then
do:
   message "You may not add a sequence definition for this database type."
      view-as ALERT-BOX ERROR buttons OK.
   return.
end.

/* what type of sequence */
s_Seq_Type = s_DbCache_Type[s_DbCache_ix].


/* Run time layout for button area.  Since this is a shared frame we have 
   to avoid doing this code more than once.
*/
if frame newseq:private-data <> "alive" then
do:
   frame newseq:private-data = "alive".

   {adecomm/okrun.i  
      &FRAME = "frame newseq" 
      &BOX   = "s_rect_Btns"
      &OK    = "s_btn_OK" 
      &HELP  = "s_btn_Help"
   }
end.

/* Erases value from the last time.  */
s_Status = "".
display s_Status with frame newseq.
s_btn_Done:label in frame newseq = "Cancel".

/* Note: the order of enables will govern the TAB order. */
enable b_Sequence._Seq-Name  b_Sequence._Seq-Init  b_Sequence._Seq-Incr
       s_Seq_Limit   	     b_Sequence._Cycle-Ok
       s_btn_OK
       s_btn_Add     	     
       s_btn_Done
       s_btn_Help
       with frame newseq.

/* Each add will be a subtransaction */
s_OK_Hit = no.
add_subtran:
repeat ON ERROR UNDO,LEAVE ON ENDKEY UNDO,LEAVE  ON STOP UNDO, LEAVE:
   /* Do this up top here, to be sure we committed the last create */
   if s_OK_Hit then leave add_subtran.

   if added AND s_btn_Done:label in frame newseq <> "Close" then
      s_btn_Done:label in frame newseq = "Close".

   create b_Sequence.

   assign
      s_Seq_Limit:label in frame newseq = "&Upper Limit"
      s_Seq_Limit = ?.

   display "" @ b_Sequence._Seq-Name  /* blank instead of ? */
      	   b_Sequence._Seq-Init  
      	   b_Sequence._Seq-Incr
           s_Seq_Limit           
      	   b_Sequence._Cycle-Ok 

	   (IF INDEX(capab,{&CAPAB_OWNER})    = 0 
             then "n/a" else b_Sequence._Seq-misc[2]) @ b_Sequence._Seq-misc[2]

	   (IF INDEX(capab,{&CAPAB_FOR_NAME}) = 0 
             then "n/a" else b_Sequence._Seq-misc[1]) @ b_Sequence._Seq-misc[1]
           IF INDEX(s_Seq_Type,"ORACLE") = 0 then "n/a" ELSE IF 
             b_Sequence._Seq-misc[8] = ? THEN "<Local-Db>" ELSE
             b_Sequence._Seq-misc[8] @ b_Sequence._Seq-misc[8]

      	   with frame newseq.

   wait-for choose of s_btn_OK, s_btn_Add in frame newseq OR
      	    GO of frame newseq
      	    FOCUS b_Sequence._Seq-Name in frame newseq.
end.

hide frame newseq.
return.




