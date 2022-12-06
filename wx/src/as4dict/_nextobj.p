/*************************************************************/
/* Copyright (c) 1984-1995 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/*----------------------------------------------------------------------------

File: _nextobj.p

Description:
   Get the next or previous of an object and update the property window
   and possibly the browse window to show the new object.   

Input Parameter: 
   p_Obj  - The object type (e.g., OBJ_TBL)
   p_Next - True - Do Next;  False - do Previous
      
Author: Laura Stern

Date Created: 05/03/92      
           Modified for PROGRESS/400 Data Dictionary D. McMann

----------------------------------------------------------------------------*/

{as4dict/dictvar.i  shared}
{as4dict/uivar.i    shared}
{as4dict/brwvar.i shared}    
{adecomm/cbvar.i    shared}

{as4dict/TBL/tblvar.i shared}
{as4dict/SEQ/seqvar.i shared}
{as4dict/FLD/fldvar.i shared}
{as4dict/IDX/idxvar.i shared}


Define INPUT PARAMETER p_Obj  as integer NO-UNDO.
Define INPUT PARAMETER p_Next as logical NO-UNDO.

Define var nxttbl as char NO-UNDO.

case p_Obj:
   when {&OBJ_TBL} then	
   do:
      if p_Next then 
      do:
      	 {as4dict/TBL/nexttbl.i &Name = s_CurrTbl
      	       	     	      	&Next = nxttbl}
	 if nxttbl <> "" then
	 do:
	    run adecomm/_setcurs.p ("WAIT").
	    s_lst_Tbls:screen-value in frame browse = nxttbl.
      	    s_TblFill:screen-value in frame browse = nxttbl.
      	    run as4dict/_objsel.p (INPUT {&OBJ_TBL}).
	    run adecomm/_setcurs.p ("").
	 end.
	 else do:
      	    current-window = s_win_Tbl.
	    message "You are at the last table." 
      	       view-as alert-box information buttons ok.
      	 end.
      end.
      else do:
	 find as4dict.p__File where as4dict.p__File._File-number = s_TblForNo 
	                          AND as4dict.p__File._File-Name = s_CurrTbl.
      	 if s_Show_Hidden_Tbls then
            find first as4dict.p__File use-index p__Filel0
                where as4dict.p__File._file-name < s_CurrTbl NO-ERROR.
      	 else
	    find PREV as4dict.p__File use-index p__Filel0
	        where as4dict.p__File._file-name < s_CurrTbl 
	          AND as4dict.p__File._Hidden <> "Y" NO-ERROR.
      
	 if AVAILABLE as4dict.p__File then
	 do:
	    run adecomm/_setcurs.p ("WAIT").
	    s_lst_Tbls:screen-value in frame browse = as4dict.p__File._File-Name.
      	    s_TblFill:screen-value in frame browse = as4dict.p__File._File-Name.
      	    run as4dict/_objsel.p (INPUT {&OBJ_TBL}).
	    run adecomm/_setcurs.p ("").
	 end.
	 else do:
      	    current-window = s_win_Tbl.
	    message "You are at the first table."
	       view-as alert-box information buttons ok.
      	 end.
      end.
   end.

   when {&OBJ_SEQ} then
   do:	               
  
      if p_Next then
      do:
	 find FIRST as4dict.p__Seq where as4dict.p__Seq._Seq-Name > s_CurrSeq NO-ERROR.
      
	 if AVAILABLE as4dict.p__Seq then
	 do:
	    run adecomm/_setcurs.p ("WAIT").
	    s_lst_Seqs:screen-value in frame browse = as4dict.p__Seq._Seq-Name.
      	    s_SeqFill:screen-value in frame browse = as4dict.p__Seq._Seq-Name.
      	    run as4dict/_objsel.p (INPUT {&OBJ_SEQ}).
	    run adecomm/_setcurs.p ("").
	 end.
	 else do:
      	    current-window = s_win_Seq.
	    message "You are at the last sequence."
	       view-as alert-box information buttons ok.
      	 end.
      end.
      else do:
	 find as4dict.p__Seq    where as4dict.p__Seq._Seq-Name = s_CurrSeq.
	 find PREV as4dict.p__Seq  WHERE as4dict.p__Seq._Seq-name < s_CurrSeq NO-ERROR.
          
	 if AVAILABLE as4dict.p__Seq then
	 do:
	    run adecomm/_setcurs.p ("WAIT").
	    s_lst_Seqs:screen-value in frame browse = as4dict.p__Seq._Seq-Name.
      	    s_SeqFill:screen-value in frame browse = as4dict.p__Seq._Seq-Name.
      	    run as4dict/_objsel.p (INPUT {&OBJ_SEQ}).
	    run adecomm/_setcurs.p ("").
	 end.
	 else do:
      	    current-window = s_win_Seq.
	    message "You are at the first sequence."
	       view-as alert-box information buttons ok.
      	 end.
      end.
   end.

   when {&OBJ_FLD} then
   do:	 

/* Temporary tacky code because we don't have an index for field position */
      define var fldpos as integer NO-UNDO.
/* end tacky */

      if p_Next then do:
            find as4dict.p__File where as4dict.p__File._File-number = s_TblForNo.
            find as4dict.p__Field where as4dict.p__field._file-number = s_TBLForNo 
	         AND as4dict.p__Field._Field-Name = s_CurrFld.

            if s_Order_By = {&ORDER_ALPHA} THEN
	find NEXT as4dict.p__Field where as4dict.p__field._file-number = s_TBLForNo 
	              AND as4dict.p__Field._Fld-misc2[5] <> "A" 
	              use-index p__Field NO-ERROR.
            ELSE DO:                         
	 fldpos = as4dict.p__field._order.
	 find NEXT as4dict.p__Field USE-INDEX p__Fieldl0 
	                   where as4dict.p__field._file-number = s_TBLForNo
	                   AND as4dict.p__Field._order > fldpos 
	                   AND as4dict.p__Field._Fld-misc2[5] <> "A" NO-ERROR.
            END.
	  
            if AVAILABLE as4dict.p__Field then do:
                run adecomm/_setcurs.p ("WAIT").
                if s_Flds_Cached then do:
	   s_lst_Flds:screen-value in frame browse = as4dict.p__Field._Field-Name.
      	   s_FldFill:screen-value in frame browse = as4dict.p__Field._Field-Name.
                end.
                run as4dict/_objsel.p (INPUT {&OBJ_FLD}).
                run adecomm/_setcurs.p ("").
            end.
            else do:
      	 current-window = s_win_Fld.
	 message "You are at the last field."
	   view-as alert-box information buttons ok.
            end.
      end.
      else do:  /* FIND PREV */
            find as4dict.p__File where as4dict.p__File._File-number = s_TblForNo.
            find as4dict.p__Field where as4dict.p__field._file-number = s_TblForNo 
	        AND as4dict.p__Field._Field-Name = s_CurrFld.           
            if s_Order_By = {&ORDER_ALPHA} THEN
	find PREV as4dict.p__Field where as4dict.p__field._file-number = s_TblForNo
	                   AND as4dict.p__Field._Field-name < s_CurrFld
	                   AND as4dict.p__Field._Fld-misc2[5] <> "A"                                
	                  use-index p__Field NO-ERROR.
            else do:                                                 
	 fldpos = as4dict.p__field._order.  /*  tacky tacky */
	 find PREV as4dict.p__Field 
	          where as4dict.p__field._file-number = s_TblForNo
	          AND as4dict.p__Field._order < fldpos 
	          AND as4dict.p__Field._Fld-misc2[5] <> "A" 
	          USE-INDEX p__Fieldl0 NO-ERROR.
            end.
	                
            if AVAILABLE as4dict.p__Field then do:   
	 run adecomm/_setcurs.p ("WAIT").
      	 if s_Flds_Cached then do:
	       s_lst_Flds:screen-value in frame browse = as4dict.p__Field._Field-Name.
      	       s_FldFill:screen-value in frame browse = as4dict.p__Field._Field-Name.
      	 end.
	 run as4dict/_objsel.p (INPUT {&OBJ_FLD}).
	 run adecomm/_setcurs.p ("").
            end.
            else do:
      	current-window = s_win_Fld.
	message "You are at the first field."
	       view-as alert-box information buttons ok.
            end.
      end.
   end.

   when {&OBJ_IDX} then
   do:	 
      if p_Next then 
      do:
      	 find as4dict.p__File where as4dict.p__File._File-number = s_TblForNo.
	 find FIRST as4dict.p__Index where as4dict.p__Index._File-number = s_TBLForNo 
	      AND  as4dict.p__Index._Index-name > s_CurrIdx 
      	    NO-ERROR.
      
	 if AVAILABLE as4dict.p__Index then
	 do:	
	    run adecomm/_setcurs.p ("WAIT").
      	    if s_Idxs_Cached then
      	    do:
	       s_lst_Idxs:screen-value in frame browse = as4dict.p__Index._Index-Name.
      	       s_IdxFill:screen-value in frame browse = as4dict.p__Index._Index-Name.
      	    end.
	    run as4dict/_objsel.p (INPUT {&OBJ_IDX}).
	    run adecomm/_setcurs.p ("").
	 end.
	 else do:
      	    current-window = s_win_Idx.
	    message "You are at the last index."
	       view-as alert-box information buttons ok.
      	 end.
      end.
      else do:
      	 find as4dict.p__File where as4dict.p__File._File-number = s_TblForNo.
	 find as4dict.p__Index where as4dict.p__index._File-number = s_TBLForNo
	     AND as4dict.p__Index._Index-Name = s_CurrIdx.
	 find PREV as4dict.p__Index where as4dict.p__index._File-number = s_TBLForNo 
	         use-index p__Index NO-ERROR.  
       
	 if AVAILABLE as4dict.p__Index then
	 do:
	    run adecomm/_setcurs.p ("WAIT").
      	    if s_Idxs_Cached then
      	    do:
	       s_lst_Idxs:screen-value in frame browse = as4dict.p__Index._Index-Name.
      	       s_IdxFill:screen-value in frame browse = as4dict.p__Index._Index-Name.
      	    end.
	    run as4dict/_objsel.p (INPUT {&OBJ_IDX}).
	    run adecomm/_setcurs.p ("").
	 end.
	 else do:
      	    current-window = s_win_Idx.
	    message "You are at the first index."
	       view-as alert-box information buttons ok.
      	 end.
      end.
   end.
end.



