/*************************************************************/
/* Copyright (c) 1984-1996 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/*----------------------------------------------------------------------------

File: seqtrig.i

Description:
   Triggers for add or modify Sequence.  This is in an include file so
   that we can have the same code that works on different frames, one for
   the add case and one for the modify case.

Argument:
   {&frame} - then name of the frame that we're working with.  This will be
      	      of the form "frame x".

Author: Laura Stern

Date Created: 02/21/92 

----------------------------------------------------------------------------*/


/*----- LEAVE of INCREMENT -----*/
on leave of b_Sequence._Seq-Incr in frame newseq,
      	    b_Sequence._Seq-Incr in frame seqprops
do:
   Define var incr as integer NO-UNDO.

   incr = input {&frame} b_Sequence._Seq-Incr.
   if incr = 0 then
   do:
      if NOT s_Adding then current-window = s_win_Seq.
      message "Increment can be negative or positive but not 0."
      	       view-as ALERT-BOX ERROR
      	       buttons OK.
      s_Valid = no.
      return NO-APPLY.
   end.

   if incr < 0 then 
   do:
      /* this check avoids flashing */
      if s_Seq_Limit:label in {&frame} <> "Lower Limit" then
      	 s_Seq_Limit:label in {&frame} = "&Lower Limit".
   end.
   else do: 
      if s_Seq_Limit:label in {&frame} <> "Upper Limit" then 
      	 s_Seq_Limit:label in {&Frame} = "&Upper Limit".
   end.
end.








