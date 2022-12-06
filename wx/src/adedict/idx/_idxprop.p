/*************************************************************/
/* Copyright (c) 1984-1997 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/*----------------------------------------------------------------------------

File: _idxprop.p

Description:
   Set up the index properties window so the user can view or modify the 
   information on an index.  Since this window is non-modal, we just do the
   set up here.  All triggers must be global.

Author: Laura Stern

Date Created: 04/29/92

Last modified on:

08/26/94 by gfs     Added Recid index support.

----------------------------------------------------------------------------*/
&GLOBAL-DEFINE WIN95-BTN YES

{adedict/dictvar.i shared}
{adedict/brwvar.i shared}
{adedict/menu.i shared}
{adedict/uivar.i shared}
{adedict/IDX/idxvar2.i shared}
{adedict/IDX/idxvar.i shared}

{adedict/capab.i}

Define var err 	     as logical NO-UNDO.
Define var capab     as char    NO-UNDO.
Define var frstfld   as char	NO-UNDO init "".
Define var lst_item  as char	NO-UNDO.
Define var name_mod  as logical NO-UNDO. /* name modifiable */
/*============================Mainline code==================================*/

find _File "_Index".
if NOT can-do(_File._Can-read, USERID("DICTDB")) then
do:
   message s_NoPrivMsg "see index definitions."
      view-as ALERT-BOX ERROR buttons Ok in window s_win_Browse.
   return.
end.
find _File "_Index-Field".
if NOT can-do(_File._Can-read, USERID("DICTDB")) then
do:
   message s_NoPrivMsg "see index definitions."
      view-as ALERT-BOX ERROR buttons Ok in window s_win_Browse.
   return.
end.

/* Don't want Cancel if moving to next index - only when window opens */
if s_win_Idx = ? then
   s_btn_Close:label in frame idxprops = "Cancel".

/* Open the window if necessary */
run adedict/_openwin.p
   (INPUT   	  "Index Properties",
    INPUT   	  frame idxprops:HANDLE,
    INPUT         {&OBJ_IDX},
    INPUT-OUTPUT  s_win_Idx).

/* Run time layout for button area. Since this is a shared frame we 
   have to avoid doing this code more than once.
*/
if frame idxprops:private-data <> "alive" then
do:
   /* okrun.i widens frame by 1 for margin */
   assign
      s_win_Idx:width = s_win_Idx:width + 1
      frame idxprops:private-data = "alive".

   {adecomm/okrun.i  
      &FRAME = "frame idxprops" 
      &BOX   = "s_rect_Btns"
      &OK    = "s_btn_OK" 
      &HELP  = "s_btn_Help"
   }

   /* So Return doesn't hit default button in editor widget */
   b_Index._Desc:RETURN-INSERT in frame idxprops = yes.
end.

/* First clear the select list in case it had stuff in it from the last time. */
s_lst_IdxFlds:LIST-ITEMS = "".

find _File where RECID(_File) = s_TblRecId.
find b_Index of _File where b_Index._Index-Name = s_CurrIdx.
if _File._Prime-Index = RECID(b_Index) then
   s_Idx_Primary = yes.
else
   s_Idx_Primary = no.

s_Idx_Word = (if b_Index._Wordidx = 0 OR b_Index._Wordidx = ? then no else yes).

find LAST _Index-Field of b_Index NO-ERROR.
if AVAILABLE _Index-Field then /* the default index has no fields */
   s_Idx_Abbrev = _Index-Field._Abbreviate.

IF s_dbCache_type[s_dbCache_ix] <> "PROGRESS" THEN DO: /* Foreign DB */
   ASSIGN ActRec:LABEL = "R&OWID".
   IF  b_Index._I-MISC2[1] begins "u"
    OR b_Index._I-MISC2[1]    =   "a"
    THEN ASSIGN
       ActRec:SENSITIVE = true
       ActRec           = false.
   ELSE IF b_Index._I-MISC2[1] begins "ru"
    OR     b_Index._I-MISC2[1]    =   "ra"
    THEN ASSIGN 
       ActRec:SENSITIVE = false
       ActRec           = true.
    ELSE ASSIGN
       ActRec:SENSITIVE = false
       ActRec           = false.
END.
ELSE ASSIGN ActRec:LABEL = "Ac&tive"
            ActRec       = b_Index._Active.
        
/* Set status line */
display "" @ s_Status ActRec with frame idxprops. /* clears from last time */

s_Idx_ReadOnly = (s_DB_ReadOnly OR s_ReadOnly).
if NOT s_Idx_ReadOnly then
do:
   if NOT can-do(_File._Can-write, USERID("DICTDB")) then
   do:
      display s_NoPrivMsg + " modify index definitions." @ s_Status
      	 with frame idxprops.
      s_Idx_ReadOnly = true.
   end.

   find _File where RECID(_File) = s_TblRecId.
   if _File._Frozen then
   do:
      s_Status:screen-value in frame idxprops =
	"Note: This file is frozen and cannot be modified.".
      s_Idx_ReadOnly = true.
   end.
end.

/* Setup field list and it's labels */
s_txt_List_Labels[1] = STRING(" ", "x(53)") + "A(sc)/".
s_txt_List_Labels[2] = STRING("Index Field", "x(33)") +
                       STRING("Data Type", "x(20)") +
                       "D(esc)".

/* Fill up the list of index fields */
for each b_idx-list.
   delete b_idx-list.
end.

for each _Index-Field of b_Index:
   find _Field where RECID(_Field) = _Index-Field._Field-recid.
   create b_idx-list.
   assign b_idx-list.fld-nam = _Field._Field-name
          b_idx-list.fld-typ = _field._Data-type
          b_idx-list.asc-desc = if _Index-Field._Ascending then "A"
                             else "D"
          b_idx-list.comp-seq = _Index-seq.
 /*
   lst_item = STRING(_Field._Field-Name, "x(33)") + 
      	      STRING(_Field._Data-type, "x(20)").

   lst_item = lst_item + (if _Index-Field._Ascending then " A" else " D").

   s_Res = s_lst_IdxFlds:add-last(lst_item) in frame idxprops.
*/
   if frstfld = "" then
      frstfld = lst_item.
end.
s_lst_IdxFlds:screen-value = frstfld.  /* set selection to the first fld */

open query q-idx-list for each b_idx-list no-lock.

display b_Index._Index-Name	 
    	b_Index._Desc
   	s_Idx_Primary
    	/*b_Index._Active*/ ActRec
    	b_Index._Unique
    	s_Idx_Word
   	s_Idx_Abbrev
/*      	s_txt_List_Labels[1]
        s_txt_List_Labels[2]
*/
       b-idx-list  
   with frame idxprops.

if s_Idx_ReadOnly then
do:
   disable all except
	 /*  s_lst_IdxFlds */
          b-idx-list
	  s_btn_Close 
	  s_btn_Prev
	  s_btn_Next
	  s_btn_Help
	  with frame idxprops.
   enable /* s_lst_IdxFlds */ 
          b-idx-list
	  s_btn_Close 
	  s_btn_Prev
	  s_btn_Next
	  s_btn_Help
	  with frame idxprops.
      apply "entry" to s_btn_Close in frame idxprops.

end.
else do:
   /* Get gateway capabilities */
   run adedict/_capab.p (INPUT {&CAPAB_IDX}, OUTPUT capab).

   /* Note: In Progress, you change the primary index by setting this one to
      be primary but you can't make a primary index be not-primary.  You
      can make an index inactive but not active - that is done via proutil. 
      In some gateways, making inactive and changing primary aren't allowed
      at all.

      Explicitly disable based on these conditions in case these were
      sensitive from the last index, and then conditionally enable (using
      ENABLE verb) below to make sure the TAB order comes out right.
   */
   if b_Index._Index-Name = "default" then
      assign
      	 b_Index._Index-Name:sensitive in frame idxprops = no
      	 name_mod = false.
   else
      name_mod = true.

   if s_Idx_Primary OR INDEX(capab, {&CAPAB_CHANGE_PRIMARY}) = 0 then
      s_Idx_Primary:sensitive in frame idxprops = no.

/*   if NOT b_Index._Active OR INDEX(capab, {&CAPAB_INACTIVATE}) = 0 then
      b_Index._Active:sensitive in frame idxprops = no.*/
      
   if (NOT b_Index._Active OR INDEX(capab, {&CAPAB_INACTIVATE}) = 0)
      AND 
        /* Hack to allow label w/mnemonic to equate on all systems (tomn 8/1/95) */
        &IF "{&WINDOW-SYSTEM}" begins "MS-WIN"
          &THEN ActRec:Label = "Ac&tive"
          &ELSE ActRec:Label = "Active"
        &ENDIF
      THEN ActRec:sensitive in frame idxprops = no.
   
   if INDEX(capab, {&CAPAB_CHANGE_UNIQ}) = 0 then
      b_Index._Unique:sensitive in frame idxprops = no.

   enable b_Index._Index-Name when name_mod
      	  b_Index._Desc
      	  s_Idx_Primary   when NOT s_Idx_Primary AND
      	       	     	       INDEX(capab, {&CAPAB_CHANGE_PRIMARY}) > 0
      	  b_Index._Unique when INDEX(capab, {&CAPAB_CHANGE_UNIQ}) > 0 
	 /* s_lst_IdxFlds */
          b-idx-list
      	  s_btn_OK
	  s_btn_Save
	  s_btn_Close
	  s_btn_Prev
	  s_btn_Next
      	  s_btn_Help
      with frame idxprops.

IF 
    /* Hack to allow label w/mnemonic to equate on all systems (tomn 8/1/95) */
    &IF "{&WINDOW-SYSTEM}" begins "MS-WIN"
      &THEN ActRec:Label = "Ac&tive"
      &ELSE ActRec:Label = "Active"
    &ENDIF
  AND b_Index._Active AND INDEX(capab, {&CAPAB_INACTIVATE}) > 0
  THEN enable ActRec with frame idxprops.

  if name_mod then apply "entry" to b_Index._Index-Name in frame idxprops.
              else apply "entry" to b_Index._Desc in frame idxprops.
  end.


return.




