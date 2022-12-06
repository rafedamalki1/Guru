/*************************************************************/
/* Copyright (c) 1984-1999 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/*----------------------------------------------------------------------------

File: menutrig.i

Description:
   Here we have all the trigger definitions for the selection of menu items.
 
Author: Laura Stern

Date Created: 02/17/92 
    Modified: 12/30/98 Mario B Call _guivget.p b4 _qviwrpt.p.  Bug 98-08-13-026

----------------------------------------------------------------------------*/


/*===========================Internal Procedures=========================*/

/*----------------------------------------------------------------
   See if the user made any changes in a property window 
   that he hasn't saved.  This (_changed.p) will ask if he wants 
   to save and do the save if he says yes. 

   Returns: "error" if an error occurs when the user tries to 
      	    save, otherwise, "".
---------------------------------------------------------------*/
Procedure Check_For_Changes:

   Define var err as logical NO-UNDO.

   err = no.
   if s_win_Tbl <> ? then
      run adedict/_changed.p (INPUT {&OBJ_TBL}, yes, OUTPUT err).
   if NOT err AND s_win_Seq <> ? then
      run adedict/_changed.p (INPUT {&OBJ_SEQ}, yes, OUTPUT err).
   /*-----
   if NOT err AND s_win_Dom <> ? then
      run adedict/_changed.p (INPUT {&OBJ_DOM}, yes, OUTPUT err).
   ----*/
   if NOT err AND s_win_Fld <> ? then
      run adedict/_changed.p (INPUT {&OBJ_FLD}, yes, OUTPUT err).
   if NOT err AND s_win_Idx <> ? then
      run adedict/_changed.p (INPUT {&OBJ_IDX}, yes, OUTPUT err).

   if err then 
      return "error".
   else
      return "".
End.


/*------------------------------------------------------------------------ 
   Do Exit processing.

------------------------------------------------------------------------*/
PROCEDURE Do_Exit:
   s_Trans = {&TRANS_ASK_AND_EXIT}.
   if s_DictState = {&STATE_NO_DB_SELECTED} then
   do:
      /* There's nothing to ask, since there can be no transaction
      	 so we're done */
      s_DictState = {&STATE_DONE}.
      apply "U1" to frame browse.
   end.
   else do:
      /* Check to see if user has made any changes in open property windows
	 that he hasn't saved.  If there are, and he saves now, and an error
	 occurs, don't continue with exit.
      */
      run Check_For_Changes.
      if RETURN-VALUE = "error" then return.

      /* Here, we have to ask commit or undo (if dict is dirty) and in
      	 any event, get out of the transaction loop. */
      apply "U2" to frame browse.
   end.
end.


/*=========================DATABASE menu=================================*/

/*----- CREATE DATABASE -----*/
On choose of MENU-ITEM mi_Crt_Database in MENU s_mnu_Database
do:
   if s_CurrObj <> {&OBJ_DB} then
      run Push_Obj_Button (INPUT  {&OBJ_DB}, 
      	       	     	   INPUT  s_icn_Dbs:HANDLE in frame browse,
      	       	     	   INPUT  false).
   run Choose_Create.
end.


/*----- CONNECT -----*/
On choose of MENU-ITEM mi_Connect in MENU s_mnu_Database
   run adedict/DB/_connect.p (INPUT ?).


/*----- DISCONNECT -----*/
On choose of MENU-ITEM mi_Disconnect in MENU s_mnu_Database
do:
   Define var answer as logical init yes NO-UNDO.

   /* If it's not dirty then ask "are you sure" here.  Otherwise
      dicttran will ask about committing and user can cancel 
      disconnect then. */
   if NOT s_DictDirty then
   do:
      current-window = s_win_Browse.
      message "Are you sure you want to disconnect database" s_CurrDb "?"
      	       view-as ALERT-BOX QUESTION
      	       buttons YES-NO
      	       update answer in window s_win_Browse.
   end.

   if answer then
   do:
      /* Make sure any changes are committed or undone before disconnecting */
      s_Trans = {&TRANS_ASK_AND_DO}.
      s_ActionProc = "adedict/DB/_dconn.p".
      apply "U2" to frame browse.
   end.
end.


/*----- EXIT ----- */
On choose of MENU-ITEM mi_Exit in MENU s_mnu_Database
   run Do_Exit.


/*----------------------------- REPORTS sub-menu---------------------------*/

Define var which_tbl as char init "t" NO-UNDO
   view-as RADIO-SET
   radio-buttons "Selected Table", "t", "ALL Tables", "a".

Form 
   SKIP(.5)
   "Report on:" at 4 view-as TEXT SKIP
   which_tbl    at 4 	          SKIP(1)
   s_btn_Ok     at 2 SPACE(2)
   s_btn_Cancel      SPACE(1)
   SKIP(.5)
   with frame tbl_select no-labels
      	      view-as DIALOG-BOX TITLE "Report Table Options".


/*----- DETAILED TABLE REPORT -----*/
On choose of MENU-ITEM mi_DetailedTbl in MENU s_mnu_Reports
do:
   current-window = s_win_Browse.
   if s_CurrTbl = "" then
   do:
      message "There are no tables in this database to look at."
      	  view-as ALERT-BOX ERROR buttons OK.
      return.
   end.

   run adecomm/_dtblrpt.p
      (INPUT s_DbRecId,
       INPUT (if s_DbCache_Pname[s_DbCache_ix] = ? 
      	       then s_CurrDb else s_DbCache_Pname[s_DbCache_ix]),
       INPUT s_DbCache_Type[s_DbCache_ix],
       INPUT (if s_Order_By = {&ORDER_ORDER#} then "o" else "a"),
       INPUT (if which_tbl = "a" then "ALL" else s_CurrTbl),
       INPUT yes).
end.


/*----- QUICK TABLE REPORT -----*/
On choose of MENU-ITEM mi_QuickTbl in MENU s_mnu_Reports
do:
   current-window = s_win_Browse.
   run adecomm/_qtblrpt.p
      (INPUT s_DbRecId,    
       INPUT (if s_DbCache_Pname[s_DbCache_ix] = ? 
      	       then s_CurrDb else s_DbCache_Pname[s_DbCache_ix]),
       INPUT s_DbCache_Type[s_DbCache_ix]).
end.


/*----- QUICK FIELD REPORT -----*/
On choose of MENU-ITEM mi_f_CurrTbl in MENU s_mnu_QuickFld
do:
   current-window = s_win_Browse.
   run adecomm/_qfldrpt.p
      (INPUT s_DbRecId,
       INPUT (if s_DbCache_Pname[s_DbCache_ix] = ? 
      	       then s_CurrDb else s_DbCache_Pname[s_DbCache_ix]),
       INPUT s_DbCache_Type[s_DbCache_ix],
       INPUT s_CurrTbl,
       INPUT (if s_Order_By = {&ORDER_ORDER#} then "o" else "a")).
end.

On choose of MENU-ITEM mi_f_AllTbls in MENU s_mnu_QuickFld
do:
   current-window = s_win_Browse.
   if s_CurrTbl = "" then
   do:
      message "There are no fields in this database to look at."
      	  view-as ALERT-BOX ERROR buttons OK.
      return.
   end.

   run adecomm/_qfldrpt.p
      (INPUT s_DbRecId,
       INPUT (if s_DbCache_Pname[s_DbCache_ix] = ? 
      	       then s_CurrDb else s_DbCache_Pname[s_DbCache_ix]),
       INPUT s_DbCache_Type[s_DbCache_ix],
       INPUT "ALL",
       INPUT (if s_Order_By = {&ORDER_ORDER#} then "o" else "a")).
end.

/*----- QUICK INDEX REPORT -----*/
On choose of MENU-ITEM mi_i_CurrTbl in MENU s_mnu_QuickIdx
do:
   current-window = s_win_Browse.
   run adecomm/_qidxrpt.p
      (INPUT s_DbRecId,
       INPUT (if s_DbCache_Pname[s_DbCache_ix] = ? 
      	       then s_CurrDb else s_DbCache_Pname[s_DbCache_ix]),
       INPUT s_DbCache_Type[s_DbCache_ix],
       INPUT s_CurrTbl,
       INPUT "").
end.

On choose of MENU-ITEM mi_i_AllTbls in MENU s_mnu_QuickIdx
do:
   current-window = s_win_Browse.
   if s_CurrTbl = "" then
   do:
      message "There are no indexes in this database to look at."
      	  view-as ALERT-BOX ERROR buttons OK.
      return.
   end.

   run adecomm/_qidxrpt.p
      (INPUT s_DbRecId,
       INPUT (if s_DbCache_Pname[s_DbCache_ix] = ? 
      	       then s_CurrDb else s_DbCache_Pname[s_DbCache_ix]),
       INPUT s_DbCache_Type[s_DbCache_ix],
       INPUT "ALL",
       INPUT "").
end.


/*----- QUICK VIEW REPORT -----*/
On choose of MENU-ITEM mi_QuickViw in MENU s_mnu_Reports
do:
   current-window = s_win_Browse.
   user_env = "a".  /* Means all is an allowable choice */
   run prodict/gui/_guivget.p.
   /* Get out if cancel is selected */
   IF user_env[1] = "" OR user_env[1] = ? THEN RETURN.
   run adecomm/_qviwrpt.p
      (INPUT s_DbRecId,    
       INPUT (if s_DbCache_Pname[s_DbCache_ix] = ? 
      	       then s_CurrDb else s_DbCache_Pname[s_DbCache_ix]),
       INPUT s_DbCache_Type[s_DbCache_ix],
       user_env[1]). 
end.


/*----- QUICK SEQUENCE REPORT -----*/
On choose of MENU-ITEM mi_QuickSeq in MENU s_mnu_Reports
do:
   current-window = s_win_Browse.
   run adecomm/_qseqrpt.p
      (INPUT s_DbRecId,    
       INPUT (if s_DbCache_Pname[s_DbCache_ix] = ? 
      	       then s_CurrDb else s_DbCache_Pname[s_DbCache_ix]),
       INPUT s_DbCache_Type[s_DbCache_ix]).
end.


/*----- TRIGGER REPORT -----*/
On choose of MENU-ITEM mi_Trigger in MENU s_mnu_Reports
do:
   current-window = s_win_Browse.
   run adecomm/_trigrpt.p
      (INPUT s_DbRecId,    
       INPUT (if s_DbCache_Pname[s_DbCache_ix] = ? 
      	       then s_CurrDb else s_DbCache_Pname[s_DbCache_ix]),
       INPUT s_DbCache_Type[s_DbCache_ix]).
end.


/*----- QUICK USER REPORT -----*/
On choose of MENU-ITEM mi_QuickUsr in MENU s_mnu_Reports
do:
   current-window = s_win_Browse.
   run adecomm/_qusrrpt.p
      (INPUT s_DbRecId,    
       INPUT (if s_DbCache_Pname[s_DbCache_ix] = ? 
      	       then s_CurrDb else s_DbCache_Pname[s_DbCache_ix]),
       INPUT s_DbCache_Type[s_DbCache_ix]).
end.


/*----- FILE RELATIONS REPORT -----*/
On choose of MENU-ITEM mi_r_CurrTbl in MENU s_mnu_TblRel
do:
   current-window = s_win_Browse.
   if s_CurrTbl = "" then
   do:
      message "There are no tables in this database to look at."
      	  view-as ALERT-BOX ERROR buttons OK.
      return.
   end.

   run adecomm/_trelrpt.p
      (INPUT s_DbRecId,
       INPUT (if s_DbCache_Pname[s_DbCache_ix] = ? 
      	       then s_CurrDb else s_DbCache_Pname[s_DbCache_ix]),
       INPUT s_DbCache_Type[s_DbCache_ix],
       INPUT s_CurrTbl,
       INPUT "").
end.

On choose of MENU-ITEM mi_r_AllTbls in MENU s_mnu_TblRel
do:
   current-window = s_win_Browse.
   if s_CurrTbl = "" then
   do:
      message "There are no tables in this database to look at."
      	  view-as ALERT-BOX ERROR buttons OK.
      return.
   end.

   run adecomm/_trelrpt.p
      (INPUT s_DbRecId,
       INPUT (if s_DbCache_Pname[s_DbCache_ix] = ? 
      	       then s_CurrDb else s_DbCache_Pname[s_DbCache_ix]),
       INPUT s_DbCache_Type[s_DbCache_ix],
       INPUT "ALL",
       INPUT "").
end.


/*=============================Edit menu=================================*/

/*----- UNDO ----- */
On choose of MENU-ITEM mi_Undo in MENU s_mnu_Edit
do:
   Define var answer as logical init yes NO-UNDO.

   current-window = s_win_Browse.
   message "Are you sure you want to undo your changes?"
      	    view-as ALERT-BOX QUESTION
      	    buttons YES-NO
      	    update answer in window s_win_Browse.

   if answer then
   do:
      /* If focus is in Db list, we want it to remain there instead
      	 of defaulting to fill-in.  So remember if list has the focus.
      	 focus should never be ? here (I don't think) but the
      	 GUI focus model is a bit wierd so sometimes it is.  
      */
      if focus <> ? then
      	 s_Dblst_Focus = (if s_lst_Dbs:handle in frame browse = 
      	       	       	  focus:handle then yes else no).

      s_Trans = {&TRANS_UNDO}.
      apply "U2" to frame browse.
   end.
end.


/*----- COMMIT ----- */
On choose of MENU-ITEM mi_Commit in MENU s_mnu_Edit
do:
   Define var answer as logical NO-UNDO init yes.

   /* Check to see if user has made any changes in open property windows
      that he hasn't saved.  If there are, and he saves now, and an error
      occurs, don't continue.  
   */
   run Check_For_Changes.
   if RETURN-VALUE = "error" then return.

   current-window = s_win_Browse.
   message "Commiting your changes may potentially take a long time." SKIP
      	   "Are you sure you want to commit your changes?"
      	    view-as ALERT-BOX QUESTION  buttons YES-NO  update answer
      	   in window s_win_Browse.

   if answer then
   do:
      /* If focus is in Db list, we want it to remain there instead
      	 of defaulting to fill-in.  So remember if list has the focus.
      	 focus should never be ? here (I don't think) but the
      	 GUI focus model is a bit wierd so sometimes it is.  
      */
      if focus <> ? then
      	 s_Dblst_Focus = (if s_lst_Dbs:handle in frame browse = 
      	       	     	  focus:handle then yes else no).

      s_Trans = {&TRANS_COMMIT}.
      apply "U2" to frame browse.
   end.
end.


/*----- DELETE ----- */
On choose of MENU-ITEM mi_Delete in MENU s_mnu_Edit
   run adedict/_delete.p (INPUT s_CurrObj).


/*----- PROPERTIES ----- */
On choose of MENU-ITEM mi_Properties in MENU s_mnu_Edit
   run Choose_Properties.


/*===========================Create menu=================================*/

/*----- TABLE -----*/
On choose of MENU-ITEM mi_Crt_Table in MENU s_mnu_Create
do:
   if s_CurrObj <> {&OBJ_TBL} then
      run Push_Obj_Button (INPUT {&OBJ_TBL}, 
      	       	     	   INPUT s_icn_Tbls:HANDLE in frame browse,
      	       	     	   INPUT  false).
   run Choose_Create.
end.


/*----- DOMAIN -----*/
/*---------------------
On choose of MENU-ITEM mi_Crt_Domain in MENU s_mnu_Create
end.
-----------------------*/


/*----- SEQUENCE -----*/
On choose of MENU-ITEM mi_Crt_Sequence in MENU s_mnu_Create
do:
   if s_CurrObj <> {&OBJ_SEQ} then
      run Push_Obj_Button (INPUT {&OBJ_SEQ}, 
      	       	     	   INPUT s_icn_Seqs:HANDLE in frame browse,
      	       	     	   INPUT  false).
   run Choose_Create.
end.


/*----- FIELD -----*/
On choose of MENU-ITEM mi_Crt_Field in MENU s_mnu_Create
do:
   if s_CurrObj <> {&OBJ_FLD} then
      run Push_Obj_Button (INPUT {&OBJ_FLD}, 
      	       	     	   INPUT s_icn_Flds:HANDLE in frame browse,
      	       	     	   INPUT  false).
   run Choose_Create.
end.


/*----- INDEX -----*/
On choose of MENU-ITEM mi_Crt_Index in MENU s_mnu_Create
do:
   if s_CurrObj <> {&OBJ_IDX} then
      run Push_Obj_Button (INPUT {&OBJ_IDX}, 
      	       	     	   INPUT s_icn_Idxs:HANDLE in frame browse,
      	       	     	   INPUT  false).
   run Choose_Create.
end.


/*=============================View menu=================================*/

/*----- SHOW HIDDEN TABLES ----- */
On value-changed of MENU-ITEM mi_Show_Hidden in MENU s_mnu_View
do:
   /* Toggle the flag */
   s_Show_Hidden_Tbls = NOT s_Show_Hidden_Tbls.
   fhidden = s_Show_Hidden_Tbls.
   
   /* redisplay the table list if it is currently visible */
   {adedict/uncache.i 
      &List   = "s_lst_Tbls"
      &Cached = "s_Tbls_Cached"
      &Curr   = "s_CurrTbl"}

   if s_Lvl1Obj = {&OBJ_TBL} then
   do:
      if s_CurrObj <> {&OBJ_TBL} then
      	 run adedict/_brwbtn.p (INPUT {&OBJ_TBL}, 
      	       	     	      	 INPUT s_icn_Tbls:HANDLE in frame browse, 
      	       	     	      	 INPUT false).   
      	 
      run adedict/_brwlist.p (INPUT {&OBJ_TBL}).
   end.
end.


/*----- ORDER FIELDS ----- */
On choose of MENU-ITEM mi_Order_Fields in MENU s_mnu_View
do:
   /* Change the menu label to indicate what the user will change to
      if he picks that option. 
   */
   if s_Order_By = {&ORDER_ALPHA} then
      assign
      	 s_Order_By = {&ORDER_ORDER#}
      	 SELF:label = "Order Fields Alphabetically".
   else
      assign
      	 s_Order_By = {&ORDER_ALPHA}
      	 SELF:label = "Order Fields by Order Number".

   /* redisplay the field list if it is currently visible */
   {adedict/uncache.i 
      &List   = "s_lst_Flds"
      &Cached = "s_Flds_Cached"
      &Curr   = "s_CurrFld"}

   if s_Lvl2Obj = {&OBJ_FLD} then
      run adedict/_brwlist.p (INPUT {&OBJ_FLD}).
end.


/*==========================Options menu=================================*/

/*----- RENUMBER FIELDS ----- */
On choose of MENU-ITEM mi_Field_Renumber in MENU s_mnu_Options
do:
   current-window = s_win_Browse. /* parents dialog to browse window */
   run adedict/FLD/_renum.p.
end.

/*----- RENAME FIELDS ----- */
On choose of MENU-ITEM mi_Field_Rename in MENU s_mnu_Options
do:
   current-window = s_win_Browse. /* parents dialog to browse window */
   run adedict/FLD/_renam.p.
end.

/*----- DATABASE MODE -----*/
On choose of MENU-ITEM mi_Mode_Db in MENU s_mnu_Options
   apply "mouse-select-down" to s_icn_Dbs in frame browse.

/*----- TABLES MODE -----*/
On choose of MENU-ITEM mi_Mode_Tbl in MENU s_mnu_Options
   apply "mouse-select-down" to s_icn_Tbls in frame browse.

/*----- DOMAIN MODE -----*/

/*----- SEQUENCES MODE -----*/
On choose of MENU-ITEM mi_Mode_Seq in MENU s_mnu_Options
   apply "mouse-select-down" to s_icn_Seqs in frame browse.

/*----- FIELD MODE -----*/
On choose of MENU-ITEM mi_Mode_Fld in MENU s_mnu_Options
   apply "mouse-select-down" to s_icn_Flds in frame browse.

/*----- INDEX MODE -----*/
On choose of MENU-ITEM mi_Mode_Idx in MENU s_mnu_Options
   apply "mouse-select-down" to s_icn_Idxs in frame browse.


/*===========================Help menu==================================*/

on choose of MENU-ITEM mi_Contents in MENU s_mnu_Help
   RUN "adecomm/_adehelp.p" ("dict", "TOPICS", ?, ?).

on choose of MENU-ITEM mi_messages in MENU s_mnu_Help
  RUN prohelp/_msgs.p.

on choose of MENU-ITEM mi_recent in MENU s_mnu_Help
  RUN prohelp/_rcntmsg.p.

on choose of MENU-ITEM mi_About in MENU s_mnu_Help
do:
   current-window = s_win_browse.
   run adecomm/_about.p ("Dictionary", "adeicon/dict%").
end.
