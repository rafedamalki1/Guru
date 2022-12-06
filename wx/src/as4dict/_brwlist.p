/*************************************************************/
/* Copyright (c) 1984-1995 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/*----------------------------------------------------------------------------

File: _brwlist.p

Description:
   Fill one of the selection lists shown in the browse window.

Input Parameter:
   p_Obj - The object type indicating which list to fill.
 
Author: Laura Stern

Date Created: 02/04/92 
            Modified to work with PROGRESS/400 Data Dictionary   D. McMann
----------------------------------------------------------------------------*/

{as4dict/dictvar.i shared}
{as4dict/brwvar.i shared}
{as4dict/uivar.i shared}


Define INPUT parameter p_Obj as integer NO-UNDO.


/*----------------------------------------------------------------------
   Do some common processing for all objects.

   Input Parameters:
      p_Obj    - The object type indicating which list to fill.
      p_List   - Handle of selection list for this object

   Input/Output Parameters:
      p_Cached - Cached flag

   Output Parameters:
      p_Curr   - Set to the selected value in the list (the first value).

----------------------------------------------------------------------*/
PROCEDURE Finish_Up:

Define INPUT   	     PARAMETER p_Obj  as integer     	 NO-UNDO.
Define INPUT   	     PARAMETER p_List as widget-handle 	 NO-UNDO.
Define INPUT-OUTPUT  PARAMETER p_Cached as logical    	 NO-UNDO.

Define var val as char NO-UNDO.

   if NOT p_Cached then  
   do:
      val = p_List:Entry(1).
      if val <> ? then  /* will be ? if list is empty */
      	 p_List:SCREEN-VALUE = val.
      apply "value-changed" to p_List.
      p_Cached = true.
   end.

   /* view and hide stuff in the browse window. */
   run as4dict/_brwadj.p (INPUT p_Obj, INPUT p_List:num-items).
end.   


/*---------------------------Mainline code-------------------------------*/

Define var access as logical NO-UNDO.

CURRENT-WINDOW = s_win_Browse.
s_DictState = {&STATE_OBJ_SELECTED}.

case p_Obj:
   when {&OBJ_TBL} then
   do:
      if NOT s_Tbls_Cached then
      do: 
      	 run as4dict/_tbllist.p
      	    (INPUT  s_lst_Tbls:HANDLE in frame browse,
      	     INPUT  s_Show_Hidden_Tbls,
      	     INPUT  s_DbRecId,
      	     INPUT  "",
      	     OUTPUT access).
      end.

      run Finish_Up (INPUT p_Obj,
      	       	     INPUT s_lst_Tbls:HANDLE in frame browse,
      	       	     INPUT-OUTPUT s_Tbls_Cached).
   end.
   
   when {&OBJ_SEQ} then
   do:
      if NOT s_Seqs_Cached then
      do:
      	 find _File "_Sequence".
      	 if NOT can-do(_File._Can-read, USERID("DICTDB")) then
      	    message s_NoPrivMsg "see any sequence information."
      	       view-as ALERT-BOX ERROR buttons OK.
      	 else
	    for each as4dict.p__Seq:
	       s_Res = s_lst_Seqs:add-last(as4dict.p__Seq._Seq-name) in frame browse.
	    end.
      end.
   
      run Finish_Up (INPUT p_Obj,
      	       	     INPUT s_lst_Seqs:HANDLE in frame browse,
      	       	     INPUT-OUTPUT s_Seqs_Cached).
   end.

   when {&OBJ_FLD} then
   do:
      if s_CurrTbl = "" then return.
   
      if NOT s_Flds_Cached then
      do:      
 
      	 run as4dict/_fldlist.p
      	    (INPUT   s_lst_Flds:HANDLE in frame browse,
      	     INPUT   s_Tblrecid,
      	     INPUT   (if s_Order_By = {&ORDER_ALPHA} then true else false),
      	     INPUT   "",
      	     INPUT   ?,
	     INPUT   no,
      	     INPUT   "",
      	     OUTPUT  access).
      end.
   
      run Finish_Up (INPUT p_Obj,
      	       	     INPUT s_lst_Flds:HANDLE in frame browse,
      	       	     INPUT-OUTPUT s_Flds_Cached).
   end.

   when {&OBJ_IDX} then
   do:
      if s_CurrTbl = "" then return.
   
      if NOT s_Idxs_Cached then
      do:              
    
      	 find _File "_Index".
      	 if NOT can-do(_File._Can-read, USERID("DICTDB")) then
      	    message s_NoPrivMsg "see any index information."
      	       view-as ALERT-BOX ERROR buttons OK.
      	 else
	    for each as4dict.p__Index where as4dict.p__Index._File-number = s_TblForNo:
	       s_Res = s_lst_Idxs:add-last(as4dict.p__Index._Index-name) in frame browse.
	    end.
      end.

      run Finish_Up (INPUT p_Obj,
      	       	     INPUT s_lst_Idxs:HANDLE in frame browse,
      	       	     INPUT-OUTPUT s_Idxs_Cached).
   end.
end. /* case */





