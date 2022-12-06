/*************************************************************/
/* Copyright (c) 1984-1998 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/*----------------------------------------------------------------------------

File: edittrig.i

Description:
   Here we have all the trigger definitions for the edit (properties)
   windows.
 
Author: Laura Stern

Date Created: 02/04/92 
     History: 11/16/98 Mario B. Added trigger on _Order to trap duplicate entry
----------------------------------------------------------------------------*/

/*======================Triggers for Browse Window===========================*/
/* I shouldn't need this - the trigger in brwtrig.i should take care of
   this.  I think this is a 4GL bug - FIX
*/

/*----- HELP anywhere -----*/
on HELP anywhere
   RUN "adecomm/_adehelp.p" ("dict", "TOPICS", {&Data_Dictionary_Window}, ?).


/*====================Triggers for Database Properties=======================*/

/*----- END-ERROR or OK BUTTON in DBPROPS FRAME -----*/
on END-ERROR of frame dbprops OR choose of s_btn_OK in frame dbprops
do:
   {adedict/delwin.i &Win = s_win_Db &Obj = {&OBJ_DB}}
   return NO-APPLY.  /* don't really do end-key processing */
end.

/*----- HELP in DBPROPS FRAME -----*/
on HELP of frame dbprops OR choose of s_btn_Help in frame dbprops
   RUN "adecomm/_adehelp.p" ("dict", "CONTEXT", 
      	       	     	     {&Database_Properties_Window}, ?).


/*======================Triggers for Table Properties=======================*/

/*----- END-ERROR OR CLOSE BUTTON of TBLPROPS FRAME -----*/
on END-ERROR of frame tblprops OR choose of s_btn_Close in frame tblprops
do:
   {adedict/delwin.i &Win = s_win_Tbl &Obj = {&OBJ_TBL}}
   return NO-APPLY.  /* otherwise it will undo everything! */
end.

/*----- HIT of SAVE BUTTON -----*/
on choose of s_btn_Save in frame tblprops
do:
   {adedict/forceval.i}  /* force leave trigger to fire */
   run adedict/TBL/_savetbl.p.
   IF (RETURN-VALUE <> "error" AND
       s_btn_Close:label in frame tblprops <> "Close") then
      s_btn_Close:label in frame tblprops = "Close".
end.


/*----- HIT of OK BUTTON or GO -----*/
on GO of frame tblprops
do:
   run adedict/TBL/_savetbl.p.
   if RETURN-VALUE = "error" then
      return NO-APPLY.

   {adedict/delwin.i &Win = s_win_Tbl &Obj = {&OBJ_TBL}}
end.


/*----- HIT of NEXT button -----*/
on choose of s_btn_Next in frame tblprops
do:
   {adedict/forceval.i}  /* force leave trigger to fire */
   run adedict/_nextobj.p ({&OBJ_TBL}, true).
end.


/*----- HIT of PREV button -----*/
on choose of s_btn_Prev in frame tblprops
do:
   {adedict/forceval.i}  /* force leave trigger to fire */
   run adedict/_nextobj.p ({&OBJ_TBL}, false).
end.


/*----- LEAVE of SAVE BUTTON -----*/
on leave of s_btn_Save in frame tblprops
   /* Since status is displayed only after save is hit */
   display "" @ s_Status with frame tblprops. /* clear status line */


/*----- HELP in TBLPROPS FRAME -----*/
on HELP of frame tblprops OR choose of s_btn_Help in frame tblprops 
   RUN "adecomm/_adehelp.p" ("dict", "CONTEXT", {&Table_Properties_Window}, ?).


/*----- LEAVE of NAME FIELD -----*/
on leave of b_File._File-Name 	 in frame newtbl,
            b_File._File-Name 	 in frame tblprops
do:
   Define var okay  as logical NO-UNDO.
   Define var name  as char    NO-UNDO.
   Define var dname as char    NO-UNDO.

   if s_Adding then
      display "" @ s_Status with frame newtbl.  /* clear status after add */

   run adedict/_leavnam.p (INPUT b_File._File-Name, INPUT s_win_Tbl,
      	       	     	   OUTPUT name, OUTPUT okay).
   if okay = ? then return.
   if NOT okay then do:
      s_Valid = no.
      return NO-APPLY.
   end.

   /* Make sure there isn't already an object with this name.  Since there is
      a unique index on Name/recid in _File, this would be caught by Progress
      eventually.  But let's check ourselves to give quicker feedback and to
      be consistent with the old dictionary.
   */
   if can-find (_File where _File._File-Name = name) then
   do:
      message "A table with this name already exists in this database."
      	 view-as ALERT-BOX ERROR
      	 buttons OK.
      ASSIGN s_Valid  = no
             s_OK_Hit = no.
      return NO-APPLY.
   end.

   /* Name is ok.  For Add, default the dump-name if it has no value yet. */
   if s_Adding then 
   do:
      dname = input frame newtbl b_File._Dump-name.
      if dname = "" OR dname = ? then
      do:
      	 b_File._Dump-name:screen-value in frame newtbl = LC(name). /* lower case */
      	 apply "leave" to b_File._Dump-name in frame newtbl.
      end.
   end.
end.


/*----- LEAVE of DUMP NAME -----*/
on leave of b_File._Dump-name in frame newtbl,
      	    b_File._Dump-name in frame tblprops
do:
   Define var dname as char NO-UNDO.
   Define var name  as char NO-UNDO.

   dname = TRIM(SELF:screen-value).

   /* If we're editing and name hasn't changed, then it's OK */
   if NOT s_Adding AND dname = b_File._Dump-name then
      return.

   if (dname = "?" OR dname = "") then
   do:
      /* Default to the name of the file, lower-cased if there is one. */
      if s_Adding then
      	 name = LC(input frame newtbl b_File._File-Name).
      else
      	 name = LC(input frame tblprops b_File._File-Name).

      if name = "" OR name = ? then return.
      
      SELF:screen-value = name.
      dname = name.
   end.

   /* Make sure the name is unique. */
   find first _File
      where _File._Dump-name = dname AND RECID(_File) <> RECID(b_File)
      NO-ERROR.
   if AVAILABLE _File then
   do:
      if NOT s_Adding then current-window = s_win_Tbl.
      message "This dump name is not unique within this database." 
      	 view-as ALERT-BOX ERROR
      	 buttons OK.
      s_Valid = no.
      return NO-APPLY.
   end.
end.


/*----- LEAVE of RECORD SIZE (_For-Size) -----*/
on leave of b_File._For-size in frame newtbl,
      	    b_File._For-size in frame tblprops
do:
   Define var recsize as integer NO-UNDO.

   recsize = INTEGER (SELF:screen-value).
   if recsize = ? OR recsize = 0 then
   do:
      if NOT s_Adding then current-window = s_win_Tbl.
      message "Record Size must be a positive integer."
      	 view-as ALERT-BOX ERROR
      	 buttons OK.
      s_Valid = no.
      return NO-APPLY.
   end.
end.


/*----- LEAVE of GATEWAY NAME (_For-name) -----*/
on leave of b_File._For-name in frame newtbl,
      	    b_File._For-name in frame tblprops
do:
   Define var gname as char NO-UNDO.

   gname = TRIM(SELF:screen-value).
   if gname = "?" OR gname = "" then
   do:
      if NOT s_Adding then current-window = s_win_Tbl.
      message "Data Server name is required."
      	 view-as ALERT-BOX ERROR buttons OK.
      s_Valid = no.
      return NO-APPLY.
   end.
end.


/*----- HIT of DATASERVER BUTTON -----*/ /**/
on choose of s_btn_Tbl_ds in frame tblprops
do:
   Define var widg    as widget-handle NO-UNDO.
   Define var no_name as logical       NO-UNDO.

   {adedict/forceval.i}  /* force leave trigger to fire */

   if s_Adding then
      /* Add is modal - current-window is already set */
      widg = b_File._File-name:HANDLE in frame newtbl.
   else do:
      widg = b_File._File-name:HANDLE in frame tblprops.
      current-window = s_win_Tbl.
   end.

   run adedict/_blnknam.p
      (INPUT widg, INPUT "table", OUTPUT no_name).
   if no_name then return NO-APPLY.

   RUN prodict/gate/_gat_row.p
     ( INPUT s_DbCache_Type[s_DbCache_ix],
       INPUT RECID(b_File)
     ).
   IF (NOT s_Adding AND RETURN-VALUE = "mod" AND
       s_btn_Close:label in frame tblprops <> "Close") then
      s_btn_Close:label in frame tblprops = "Close".
end.
/**/

/*----- HIT of VALIDATION BUTTON -----*/
on choose of s_btn_Tbl_Validation in frame newtbl,
      	     s_btn_Tbl_Validation in frame tblprops
do:
   Define var widg    as widget-handle NO-UNDO.
   Define var no_name as logical       NO-UNDO.

   {adedict/forceval.i}  /* force leave trigger to fire */

   if s_Adding then
      /* Add is modal - current-window is already set */
      widg = b_File._File-name:HANDLE in frame newtbl.
   else do:
      widg = b_File._File-name:HANDLE in frame tblprops.
      current-window = s_win_Tbl.
   end.

   run adedict/_blnknam.p
      (INPUT widg, INPUT "table", OUTPUT no_name).
   if no_name then return NO-APPLY.

   run adedict/TBL/_tblval.p.
   IF (NOT s_Adding AND RETURN-VALUE = "mod" AND
       s_btn_Close:label in frame tblprops <> "Close") then
      s_btn_Close:label in frame tblprops = "Close".
end.


/*----- HIT of TRIGGERS BUTTON -----*/
on choose of s_btn_Tbl_Triggers in frame newtbl,
      	     s_btn_Tbl_Triggers in frame tblprops
do:
   Define var widg    as widget-handle NO-UNDO.
   Define var no_name as logical       NO-UNDO.
   Define var junk    as logical       NO-UNDO.

   {adedict/forceval.i}  /* force leave trigger to fire */

   if s_Adding then
      /* Add is modal - current-window is already set */
      widg = b_File._File-name:HANDLE in frame newtbl.
   else do:
      widg = b_File._File-name:HANDLE in frame tblprops.
      current-window = s_win_Tbl.
   end.

   /* Check if name is blank and return if it is */
   run adedict/_blnknam.p
      (INPUT widg, INPUT "table", OUTPUT no_name).
   if no_name then return NO-APPLY.

   /*--- Save policy has been changed --------------------------------
      this is obsolete but leave in case we change our minds!

   /* Copy trigger values to work file in case we need to undo changes, if
      we haven't already done so for this table (only for editing). */
   if NOT s_Adding AND NOT s_TblTrigs_Stashed then
      run adedict/TBL/tblworkf.p (INPUT {&TRIGS_TO_WORK}, OUTPUT junk).
   ------------------------------------------------------------------*/
   
   run adedict/TRIG/_trigdlg.p (INPUT {&OBJ_TBL}, 
			       INPUT
"CREATE,DELETE,FIND,WRITE,REPLICATION-CREATE,REPLICATION-DELETE,REPLICATION-WRITE",
      	       	     	       INPUT widg,
      	       	     	       INPUT {&Table_Triggers_Dlg_Box}).
   IF (NOT s_Adding AND RETURN-VALUE = "mod" AND
       s_btn_Close:label in frame tblprops <> "Close") then
      s_btn_Close:label in frame tblprops = "Close".
end.


/*----- HIT of STRING ATTRIBUTES BUTTON -----*/
on choose of s_btn_Tbl_StringAttrs in frame newtbl,
      	     s_btn_Tbl_StringAttrs in frame tblprops
do:
   Define var widg    as widget-handle NO-UNDO.
   Define var no_name as logical       NO-UNDO.

   {adedict/forceval.i}  /* force leave trigger to fire */

   if s_Adding then
      /* Add is modal - current-window is already set */
      widg = b_File._File-name:HANDLE in frame newtbl.
   else do:
      widg = b_File._File-name:HANDLE in frame tblprops.
      current-window = s_win_Tbl.
   end.

   run adedict/_blnknam.p
      (INPUT widg, INPUT "table", OUTPUT no_name).
   if no_name then return NO-APPLY.

   run adedict/TBL/_tblsas.p.
   IF (NOT s_Adding AND RETURN-VALUE = "mod" AND
       s_btn_Close:label in frame tblprops <> "Close") then
      s_btn_Close:label in frame tblprops = "Close".
end.


/*==================Triggers for Field/Domain Properties======================*/

/*----- END-ERROR OR CLOSE BUTTON of FLDPROPS FRAME -----*/
on END-ERROR of frame fldprops OR choose of s_btn_Close in frame fldprops
do:
   {adedict/delwin.i &Win = s_win_Fld &Obj = {&OBJ_FLD}}
   return NO-APPLY.  /* otherwise it will undo everything! */
end.

/*----- HIT of SAVE BUTTON -----*/
on choose of s_btn_Save in frame fldprops
do:
   {adedict/forceval.i}  /* force leave trigger to fire */
   run adedict/FLD/_savefld.p.
   IF (RETURN-VALUE <> "error" AND
       s_btn_Close:label in frame fldprops <> "Close") then
      s_btn_Close:label in frame fldprops = "Close".
end.


/*----- HIT of OK BUTTON or GO -----*/
on GO of frame fldprops
do:
   {adedict/forceval.i}  /* force leave trigger to fire */
   run adedict/FLD/_savefld.p.
   if RETURN-VALUE = "error" then
      return NO-APPLY.

   {adedict/delwin.i &Win = s_win_Fld &Obj = {&OBJ_FLD}}
end.


/*----- HIT of NEXT button -----*/
on choose of s_btn_Next in frame fldprops
do:
   {adedict/forceval.i}  /* force leave trigger to fire */
   run adedict/_nextobj.p ({&OBJ_FLD}, true).
end.


/*----- HIT of PREV button -----*/
on choose of s_btn_Prev in frame fldprops
do:
   {adedict/forceval.i}  /* force leave trigger to fire */
   run adedict/_nextobj.p ({&OBJ_FLD}, false).
end.


/*----- LEAVE of FIELD SAVE BUTTON -----*/
on leave of s_btn_Save in frame fldprops
   display "" @ s_Status with frame fldprops. /* clear status line */


/*----- HELP in FLDPROPS FRAME -----*/
on HELP of frame fldprops OR choose of s_btn_Help in frame fldprops 
   RUN "adecomm/_adehelp.p" ("dict", "CONTEXT", {&Field_Properties_Window}, ?).


/*----- HIT of VALIDATION BUTTON (newfld/fldprops)-----*/
on choose of s_btn_Fld_Validation in frame newfld,
      	     s_btn_Fld_Validation in frame fldprops
do:
   Define var widg     as widget-handle NO-UNDO.
   Define var no_name  as logical       NO-UNDO.
   Define var readonly as logical       NO-UNDO.

   {adedict/forceval.i}  /* force leave trigger to fire */

   if s_Adding then
      /* Add is modal - current-window is already set */
      widg = b_Field._Field-name:HANDLE in frame newfld.
   else do:
      widg = b_Field._Field-name:HANDLE in frame fldprops.
      current-window = s_win_Fld.
   end.

   run adedict/_blnknam.p
      (INPUT widg, INPUT "field", OUTPUT no_name).
   if no_name then return NO-APPLY.

   run adedict/FLD/_fldval.p (INPUT s_Fld_ReadOnly).
   IF (NOT s_Adding AND RETURN-VALUE = "mod" AND
       s_btn_Close:label in frame fldprops <> "Close") then
      s_btn_Close:label in frame fldprops = "Close".
end.


/*----- HIT of VALIDATION BUTTON (domprops)-----*/
/*-----------------------------------------
on choose of s_btn_Fld_Validation in frame domprops
do:
   Define var no_name as logical NO-UNDO.

   {adedict/forceval.i}  /* force leave trigger to fire */
   current-window = s_win_Dom.

   run adedict/_blnknam.p
      (INPUT b_Field._Field-name:HANDLE in frame domprops,
       INPUT "domain", OUTPUT no_name).
   if no_name then return NO-APPLY.

   run adedict/FLD/_fldval.p (INPUT s_Dom_ReadOnly).
   IF (RETURN-VALUE = "mod" AND
       s_btn_Close:label in frame domprops <> "Close") then
      s_btn_Close:label in frame domprops = "Close".
end.
------------------------------------------*/


/*----- HIT of TRIGGERS BUTTON (newfld/fldprops) -----*/
on choose of s_btn_Fld_Triggers in frame newfld,
      	     s_btn_Fld_Triggers in frame fldprops
do:
   Define var widg    as widget-handle NO-UNDO.
   Define var no_name as logical       NO-UNDO.
   Define var junk    as logical       NO-UNDO.

   {adedict/forceval.i}  /* force leave trigger to fire */

   if s_Adding then
      /* Add is modal - current-window is already set */
      widg = b_Field._Field-name:HANDLE in frame newfld.
   else do:
      widg = b_Field._Field-name:HANDLE in frame fldprops.
      current-window = s_win_Fld.
   end.

   run adedict/_blnknam.p
      (INPUT widg, INPUT "field", OUTPUT no_name).
   if no_name then return NO-APPLY.
   
   /*--- Save policy has been changed --------------------------------
      this is obsolete but leave in case we change our minds!

   /* Copy trigger values to work file in case we need to undo changes, if
      we haven't already done so for this field (only for editing). */
   if NOT s_Adding AND NOT s_FldTrigs_Stashed then
      run adedict/FLD/fldworkf.p (INPUT {&TRIGS_TO_WORK}, OUTPUT junk).
   --------------------------------------------------------------------*/
   
   run adedict/TRIG/_trigdlg.p (INPUT {&OBJ_FLD}, 
			       INPUT "ASSIGN",
      	       	     	       INPUT widg,
      	       	     	       INPUT {&Field_Triggers_Dlg_Box}).
   IF (NOT s_Adding AND RETURN-VALUE = "mod" AND
       s_btn_Close:label in frame fldprops <> "Close") then
      s_btn_Close:label in frame fldprops = "Close".
end.


/*----- HIT of VIEW-AS BUTTON (fldprops)-----*/
on choose of s_btn_Fld_ViewAs in frame fldprops
do:
   Define var mod  as logical NO-UNDO.
   Define var temp as char    NO-UNDO.

   /* trigger for new field is in _newfld.p because dtype
      source is different */

   Define var no_name as logical NO-UNDO.

   {adedict/forceval.i}  /* force leave trigger to fire */
   current-window = s_win_Fld.

   run adedict/_blnknam.p
      (INPUT b_Field._Field-name:HANDLE in frame fldprops, 
       INPUT "field", OUTPUT no_name).
   if no_name then return NO-APPLY.

   /* Can't update field directly as in-out parm because if read-only
      this causes an error.
   */
   temp = b_Field._View-as.
   run adecomm/_viewas.p
      (INPUT s_Fld_ReadOnly, INPUT b_Field._dtype, INPUT s_Fld_ProType,
       INPUT-OUTPUT temp, OUTPUT mod).
   if mod then do:   
      b_Field._View-as = temp. 
      {adedict/setdirty.i &Dirty = "true"}.
      if (s_btn_Close:label in frame fldprops <> "Close") then
      	 s_btn_Close:label in frame fldprops = "Close".
   end.
end.


/*----- HIT of VIEW-AS BUTTON (domprops)-----*/


/*----- HIT of STRING ATTRIBUTES BUTTON (newfld/fldprops)-----*/
on choose of s_btn_Fld_StringAttrs in frame newfld,
      	     s_btn_Fld_StringAttrs in frame fldprops
do:
   Define var widg     as widget-handle NO-UNDO.
   Define var no_name  as logical       NO-UNDO.
   Define var readonly as logical       NO-UNDO.
   Define var mod      as logical      	NO-UNDO.

   {adedict/forceval.i}  /* force leave trigger to fire */

   if s_Adding then
      /* Add is modal - current-window is already set */
      widg = b_Field._Field-name:HANDLE in frame newfld.
   else do:
      widg = b_Field._Field-name:HANDLE in frame fldprops.
      current-window = s_win_Fld.
   end.

   run adedict/_blnknam.p
      (INPUT widg, INPUT "field", OUTPUT no_name).
   if no_name then return NO-APPLY.

   run adecomm/_fldsas.p (INPUT s_Fld_ReadOnly, 
      	       	     	 BUFFER b_Field, OUTPUT mod).
   if mod AND NOT s_Adding then do:
      {adedict/setdirty.i &Dirty = "true"}.
      if s_btn_Close:label in frame fldprops <> "Close" then
      	 s_btn_Close:label in frame fldprops = "Close".
   end.
end.


/*----- HIT of STRING ATTRIBUTES BUTTON (domprops)-----*/


/*----- HIT of GATEWAY BUTTON (newfld/fldprops)-----*/

on choose of s_btn_Fld_Gateway in frame newfld,
      	     s_btn_Fld_Gateway in frame fldprops
do:
   Define var widg     as widget-handle NO-UNDO.
   Define var no_name  as logical       NO-UNDO.
   Define var readonly as logical       NO-UNDO.

   {adedict/forceval.i}  /* force leave trigger to fire */

   if s_Adding then
      /* Add is modal - current-window is already set */
      widg = b_Field._Field-name:HANDLE in frame newfld.
   else do:
      widg = b_Field._Field-name:HANDLE in frame fldprops.
      current-window = s_win_Fld.
   end.

   run adedict/_blnknam.p
      (INPUT widg, INPUT "field", OUTPUT no_name).
   if no_name then return NO-APPLY.

   run adedict/FLD/_fldgate.p (INPUT s_Fld_ReadOnly).
   IF (NOT s_Adding AND RETURN-VALUE = "mod" AND
       s_btn_Close:label in frame fldprops <> "Close") then
      s_btn_Close:label in frame fldprops = "Close".
end.


/*----- HIT of GATEWAY BUTTON (domprops)-----*/
/*---------------------------------------------
on choose of s_btn_Fld_Gateway in frame domprops
do:
   Define var no_name as logical NO-UNDO.

   {adedict/forceval.i}  /* force leave trigger to fire */
   current-window = s_win_Dom.

   run adedict/_blnknam.p
      (INPUT b_Field._Field-name:HANDLE in frame domprops,
       INPUT "domain", OUTPUT no_name).
   if no_name then return NO-APPLY.

   run adedict/FLD/_fldgate.p(INPUT s_Dom_ReadOnly).
   IF (RETURN-VALUE = "mod" AND
       s_btn_Close:label in frame domprops <> "Close") then
      s_btn_Close:label in frame domprops = "Close".
end.
-----------------------------------------------*/


/*----- LEAVE of NAME FIELD -----*/
on leave of b_Field._Field-Name in frame newfld,
      	    b_Field._Field-Name in frame fldprops
      	    /* b_Field._Field-Name in frame domprops */
do:
   Define var okay   	as logical.
   Define var name   	as char.
   Define var record_id as recid.
   Define var win    	as widget-handle.

   if s_Adding then
      display "" @ s_Status with frame newfld.  /* clear status after add */

   win = (if s_CurrObj = {&OBJ_FLD} then s_win_Fld else s_win_Dom).
   run adedict/_leavnam.p (INPUT  b_Field._Field-Name, INPUT win, 
      	       	     	   OUTPUT name, OUTPUT okay).
   if okay = ? then return.
   if NOT okay then do:
      s_Valid = no.
      return NO-APPLY.
   end.

   if NOT s_Adding AND s_Fld_InView then
   do:
      message "This field is used in a view - you cannot rename it."
      	       view-as ALERT-BOX ERROR buttons Ok.
      s_Valid = no.
      return NO-APPLY.
   end.

   record_id = (if s_CurrObj = {&OBJ_FLD} then s_TblRecId else s_DomRecId).
   
   /* Make sure there isn't already an object with this name.  Since there is
      a unique index on Name/recid in _Field, this would be caught by Progress
      eventually.  But let's check ourselves to give quicker feedback and
      a better error message in the case of domains.
   */
   if can-find (_Field where _Field._File-recid = record_id AND
			     _Field._Field-Name = name) then
   do:
      if s_CurrObj = {&OBJ_FLD} then
	 message "A field with this name already exists in this table."
		  view-as ALERT-BOX ERROR buttons OK.
      else 
	 message "There is already a domain with this name."
		  view-as ALERT-BOX ERROR buttons OK.
      
      s_Valid = no.
      return NO-APPLY.
   end.
end.


/*---- COMBO BOX TRIGGERS for DATA TYPE -----*/
/* Forget about domains for now - deal with this in future. */
{adecomm/cbtdrop.i &Frame  = "frame fldprops"
		   &CBFill = "s_Fld_DType"
		   &CBList = "s_lst_Fld_DType"
		   &CBBtn  = "s_btn_Fld_DType"
		   &CBInit = """"}


/*----- CHANGE of DATA TYPE FIELD -----*/
on /* leave */ U1 of s_Fld_DType in frame fldprops, 
      	       	     s_lst_Fld_DType in frame fldprops
do:
   /* Set other field defaults based on the datatype chosen.  You CAN
      change data types for some gateways.  *See if it's really changed
      1st.  If user had changed format or initial value, for example,
      we don't want to clobber with defaults if we don't have to.
      (* - los 12/27/94)
   */
   if s_Fld_DType <> s_Fld_DType:screen-value in frame fldprops then
   do:
     {adedict/FLD/setdflts.i &Frame = "frame fldprops"} 
   end.
end.


/*----- LEAVE of FORMAT FIELD -----*/
on leave of b_Field._Format in frame fldprops
do:
   /* Set format to default if it's blank and fix up initial value
      if data type is logical based on the format. */
   run adedict/FLD/_dfltfmt.p   
      (INPUT b_Field._Format:HANDLE in frame fldprops,
       INPUT b_Field._Initial:HANDLE in frame fldprops,
       INPUT 0,
       INPUT false). 
end.

/*----- HIT of FORMAT EXAMPLES BUTTON -----*/
on choose of s_btn_Fld_Format in frame fldprops
 	     /* s_btn_Fld_Format in frame domprops */
do:
   Define var fmt as char NO-UNDO.

   /* If adding current window is already set since dlg is modal */
   if NOT s_Adding then current-window = s_win_Fld.

   /* Allow user to pick a different format from examples */
   fmt = input frame fldprops b_Field._Format.
   run adedict/FLD/_fldfmts.p (INPUT s_Fld_Typecode, INPUT-OUTPUT fmt).
   b_Field._Format:SCREEN-VALUE in frame fldprops = fmt.
end.

/*---------- LEAVE OF ORDER FIELD ---------*/
on leave of b_field._order in frame fldprops
DO:
      /* Avoid the test if the field hasn't changed */
      IF b_Field._Order = INT(b_Field._Order:SCREEN-VALUE IN FRAME fldprops)
         THEN LEAVE. 
      /* Is the new order number a duplicate?  Don't allow it.  */
      IF CAN-FIND(FIRST _Field WHERE
                        _Field._File-recid = s_TblRecId AND
                        _Field._Order =
			 INT(b_Field._Order:SCREEN-VALUE IN FRAME fldprops) AND
	                 _Field._Order <> b_Field._Order) THEN 
      DO:
	 MESSAGE "Order number " +
         TRIM(b_Field._Order:SCREEN-VALUE IN FRAME fldprops) "already exists." 
	 VIEW-AS ALERT-BOX ERROR BUTTONS OK.
	 /* set order number back to its current value */
	 b_Field._Order:SCREEN-VALUE IN FRAME fldprops = STRING(b_Field._Order).
        RETURN NO-APPLY.
      END.
END.


/*====================Triggers for Sequence Properties=======================*/

/* Some sequence triggers (Save button, GO from create, leave increment */
{adedict/SEQ/seqtrig.i &frame = "frame seqprops"}


/*----- END-ERROR OR CLOSE BUTTON of SEQPROPS FRAME -----*/
on END-ERROR of frame seqprops OR choose of s_btn_Close in frame seqprops
do:
   {adedict/delwin.i &Win = s_win_Seq &Obj = {&OBJ_SEQ}}
   return NO-APPLY.  /* otherwise everything will be undone! */
end.


/*----- HIT of OK BUTTON or GO -----*/
on GO of frame seqprops
do:
   run adedict/SEQ/_saveseq.p
      (b_Sequence._Seq-name:HANDLE in frame seqprops,
       input frame seqprops b_Sequence._Seq-Incr,
       input frame seqprops s_Seq_Limit,
       b_Sequence._Seq-Init:HANDLE in frame seqprops,
       input frame seqprops b_Sequence._Cycle-Ok).

   if RETURN-VALUE = "error" then
      return NO-APPLY.

   {adedict/delwin.i &Win = s_win_Seq &Obj = {&OBJ_SEQ}}
end.


/*----- HIT of SAVE BUTTON -----*/
on choose of s_btn_Save in frame seqprops
do:
   {adedict/forceval.i}  /* force leave trigger to fire */
   run adedict/SEQ/_saveseq.p
      (b_Sequence._Seq-name:HANDLE in frame seqprops,
       input frame seqprops b_Sequence._Seq-Incr,
       input frame seqprops s_Seq_Limit,
       b_Sequence._Seq-Init:HANDLE in frame seqprops,
       input frame seqprops b_Sequence._Cycle-Ok).
   IF (RETURN-VALUE <> "error" AND
       s_btn_Close:label in frame seqprops <> "Close") then
      s_btn_Close:label in frame seqprops = "Close".
end.


/*----- HIT of NEXT button -----*/
on choose of s_btn_Next in frame seqprops
do:
   {adedict/forceval.i}  /* force leave trigger to fire */
   run adedict/_nextobj.p ({&OBJ_SEQ}, true).
end.


/*----- HIT of PREV button -----*/
on choose of s_btn_Prev in frame seqprops
do:
   {adedict/forceval.i}  /* force leave trigger to fire */
   run adedict/_nextobj.p ({&OBJ_SEQ}, false).
end.


/*----- HELP in SEQPROPS FRAME -----*/
on HELP of frame seqprops OR choose of s_btn_Help in frame seqprops 
   RUN "adecomm/_adehelp.p" ("dict", "CONTEXT", 
      	       	     	     {&Sequence_Properties_Window}, ?).


/*----- LEAVE of NAME FIELD -----*/
on leave of b_Sequence._Seq-Name in frame newseq,
      	    b_Sequence._Seq-Name in frame seqprops
do:
   Define var okay as logical.
   Define var name as char.

   if s_Adding then
      display "" @ s_Status with frame newseq.  /* clear status after add */

   run adedict/_leavnam.p (INPUT  b_Sequence._Seq-Name, 
      	       	     	   INPUT  s_win_Seq, 
      	       	     	   OUTPUT name, OUTPUT okay).
   if okay = ? then return.
   if NOT okay then do:
      s_Valid = no.
      return NO-APPLY.
   end.

   /* Make sure there isn't already an object with this name.  Since there is
      a unique index on Name/recid in _File, this would be caught by Progress
      eventually.  But let's check ourselves to give quicker feedback and to
      be consistent with the old dictionary.
   */
   if can-find (_Sequence where _Sequence._Seq-Name = name) then
   do:
      message "A sequence with this name already exists in this database."
      	 view-as ALERT-BOX ERROR
      	 buttons OK.
      s_Valid = no.
      return NO-APPLY.
   end.
end.


/*----- LEAVE of SAVE BUTTON -----*/
on leave of s_btn_Save in frame seqprops
   display "" @ s_Status with frame seqprops. /* clear status line */


/*=====================Triggers for Index Properties======================*/

/*----- END-ERROR OR CLOSE BUTTON of IDXPROPS FRAME -----*/
on END-ERROR of frame idxprops OR choose of s_btn_Close in frame idxprops
do:
   {adedict/delwin.i &Win = s_win_Idx &Obj = {&OBJ_IDX}}
   return NO-APPLY.  /* otherwise everything will be undone! */
end.


/*----- HIT of SAVE BUTTON -----*/
on choose of s_btn_Save in frame idxprops
do:
   {adedict/forceval.i}  /* force leave trigger to fire */
   run adedict/IDX/_saveidx.p.
   IF (RETURN-VALUE <> "error" AND
       s_btn_Close:label in frame idxprops <> "Close") then
      s_btn_Close:label in frame idxprops = "Close".
end.


/*----- HIT of OK BUTTON or GO -----*/
on GO of frame idxprops
do:
   run adedict/IDX/_saveidx.p.
   if RETURN-VALUE = "error" then
      return NO-APPLY.

   {adedict/delwin.i &Win = s_win_Idx &Obj = {&OBJ_IDX}}
end.


/*----- LEAVE of SAVE BUTTON -----*/
on leave of s_btn_Save in frame idxprops
   display "" @ s_Status with frame idxprops. /* clear status line */


/*----- HIT of NEXT button -----*/
on choose of s_btn_Next in frame idxprops
do:
   {adedict/forceval.i}  /* force leave trigger to fire */
   run adedict/_nextobj.p ({&OBJ_IDX}, true).
end.


/*----- HIT of PREV button -----*/
on choose of s_btn_Prev in frame idxprops
do:
   {adedict/forceval.i}  /* force leave trigger to fire */
   run adedict/_nextobj.p ({&OBJ_IDX}, false).
end.


/*----- HELP in IDXPROPS FRAME -----*/
on HELP of frame idxprops OR choose of s_btn_Help in frame idxprops 
   RUN "adecomm/_adehelp.p" ("dict", "CONTEXT", {&Index_Properties_Window}, ?).


/*----- LEAVE of NAME FIELD -----*/
on leave of b_Index._Index-Name in frame newidx,
      	    b_Index._Index-Name in frame idxprops
do:
   Define var okay as logical.
   Define var name as char.
   Define var record_id as recid.

   if s_Adding then
      display "" @ s_Status with frame newidx.  /* clear status after add */

   run adedict/_leavnam.p (INPUT  b_Index._Index-Name,
      	       	     	   INPUT  s_win_Idx, 
      	       	     	   OUTPUT name, OUTPUT okay).
   if okay = ? then return.
   if NOT okay then do:
      s_Valid = no.
      return NO-APPLY.
   end.
   
   /* Make sure there isn't already an object with this name.  Since there is
      a unique index on Name/recid in _Index, this would be caught by Progress
      eventually.  But let's check ourselves to give quicker, clearer feedback.
   */
   if can-find (_Index where _Index._File-recid = s_TblRecId AND
			     _Index._Index-Name = name) then
   do:
      message "An index with this name already exists for this table."
	       view-as ALERT-BOX ERROR
	       buttons OK.
      
      s_Valid = no.
      return NO-APPLY.
   end.
end.



