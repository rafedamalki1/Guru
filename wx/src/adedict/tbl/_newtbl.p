/*************************************************************/
/* Copyright (c) 1984-1997 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/*----------------------------------------------------------------------------

File: _newtbl.p

Description:
   Display and handle the add table dialog box and then add the table
   if the user presses OK.

   Note: Currently only Progress and CTOSISAM files added through this 
   code.  All others are created via a gateway utility.  However, this is
   is set up to work for all gateways. 

Author: Laura Stern

Date Created: 03/13/92

History:

Modified on 12/07/94 by gfs - added support for ODBC
Modified on 07/08/94 by gfs - Bug 94-06-14-177 (again)
Modified on 07/07/94 by gfs - Bug 94-06-14-177

----------------------------------------------------------------------------*/
&GLOBAL-DEFINE WIN95-BTN YES
{adedict/dictvar.i shared}
{adedict/brwvar.i shared}
{adedict/menu.i shared}
{adedict/uivar.i shared}
{adecomm/cbvar.i shared}
{adedict/TBL/tblvar.i shared}
{adedict/capab.i}


DEFINE VARIABLE capab  AS CHARACTER            NO-UNDO.
DEFINE VARIABLE added  AS LOGICAL   INITIAL no NO-UNDO.
DEFINE VARIABLE isodbc AS LOGICAL   INITIAL no NO-UNDO.

/*===============================Triggers====================================*/

/*-----WINDOW-CLOSE-----*/
on window-close of frame newtbl
   apply "END-ERROR" to frame newtbl.


/*----- HIT of OK BUTTON -----*/
on choose of s_btn_OK in frame newtbl
   s_OK_Hit = yes.
   /* The GO trigger will fire after this. */


/*----- HIT of OK BUTTON or ADD BUTTON or GO -----*/
on GO of frame newtbl	/* or buttons - because they're auto-go */
do:
   Define var no_name  as logical NO-UNDO.
   Define var nxtname  as char    NO-UNDO.

   run adedict/_blnknam.p
      (INPUT b_File._File-name:HANDLE in frame newtbl,
       INPUT "table", OUTPUT no_name).
   if no_name then do:
      s_OK_Hit = no.  /* in case ok was hit */
      return NO-APPLY.
   end.

   do ON ERROR UNDO, LEAVE  ON STOP UNDO, LEAVE:
      run adecomm/_setcurs.p ("WAIT").
      
      IF NOT s_Show_Hidden_Tbls AND input frame newtbl b_File._Hidden THEN DO:
            ASSIGN MENU-ITEM mi_Show_Hidden:CHECKED in MENU s_mnu_View = TRUE.
            APPLY "VALUE-CHANGED" TO MENU-ITEM mi_Show_Hidden in MENU s_mnu_View.
      END.

      assign
	 b_File._DB-recid = s_DbRecId
         input frame newtbl b_File._File-name 
	 input frame newtbl b_File._Dump-Name
	 input frame newtbl b_File._Hidden
	 input frame newtbl b_File._For-Size
      	 input frame newtbl b_File._File-label
	 input frame newtbl b_File._Desc
         input frame newtbl b_File._Fil-misc2[6].
        

      if s_Tbl_Type:sensitive in frame newtbl then
      	 b_File._For-type = input frame newtbl s_Tbl_Type.
      if b_File._For-name:sensitive in frame newtbl then
      	 b_File._For-name = input frame newtbl b_File._For-name.
      
      /* For ODBC databases, the file's foreign type is "BUFFER"*/
      
      IF isodbc AND b_File._For-Name = ? AND b_File._For-Owner = ? THEN 
         ASSIGN 
            b_file._For-Type = "BUFFER" 
            b_File._For-Name = "NONAME".

      /* Add entry to tables list in alphabetical order */

      if s_Show_hidden_Tbls OR NOT b_File._Hidden then
      do:
      	 {adedict/TBL/nexttbl.i &Name = b_File._File-Name  
      	       	     	      	&Next = nxtname}
	 run adedict/_newobj.p
	    (INPUT s_lst_Tbls:HANDLE in frame browse,
	     INPUT b_File._File-name,
	     INPUT nxtname,
	     INPUT s_Tbls_Cached,
	     INPUT {&OBJ_TBL}).
      end.

      {adedict/setdirty.i &Dirty = "true"}.
      display "Table Created" @ s_Status with frame newtbl.
      added = yes.
      run adecomm/_setcurs.p ("").
      return.
   end.

   /* We only get here if an error occurred. Dialog box should remain
      on the screen so return NO-APPLY. */  
   run adecomm/_setcurs.p ("").
   s_OK_Hit = no.
   return NO-APPLY.  
end.


/*----- LEAVE of CREATE BUTTON -----*/
on LEAVE of s_btn_Add in frame newtbl
   display "" @ s_Status with frame newtbl. /* clear status line */


/*----- HELP -----*/
on HELP of frame newtbl OR choose of s_btn_Help in frame newtbl
   RUN "adecomm/_adehelp.p" ("dict", "CONTEXT", {&Create_Table_Dlg_Box}, ?).


/*============================Mainline code==================================*/

find _File "_File".
if NOT can-do(_File._Can-create, USERID("DICTDB")) then
do:
   message s_NoPrivMsg "create tables."
      view-as ALERT-BOX ERROR buttons Ok.
   return.
end.

/* Get gateway capabilities */
run adedict/_capab.p (INPUT {&CAPAB_TBL}, OUTPUT capab).

if INDEX(capab, {&CAPAB_ADD}) = 0 then
do:
   message "You may not add a table definition for this database type."
      view-as ALERT-BOX ERROR buttons OK.
   return.
end.

/* Get ODBC types in case this is an ODBC db */
odbtyp = { adecomm/ds_type.i
           &direction = "ODBC"
           &from-type = "odbtyp"}.
           
/* See if this db is an ODBC db */      
IF CAN-DO(odbtyp, s_DbCache_Type[s_DbCache_ix]) THEN ASSIGN isodbc = yes.

/* Make stuff appropriate for add visible, and other stuff invisible */
assign
   s_Tbl_IdxCnt:hidden in frame newtbl = yes.

/* Since this is a shared frame we have to avoid doing this code more
   than once.
*/
if frame newtbl:private-data <> "alive" then
do:
   frame newtbl:private-data = "alive".

   /* Run time layout for button area.  This defines eff_frame_width */
   {adecomm/okrun.i  
      &FRAME = "frame newtbl" 
      &BOX   = "s_rect_Btns"
      &OK    = "s_btn_OK" 
      &HELP  = "s_btn_Help"
   }
   
   /* So Return doesn't hit default button in editor widget. */
   b_File._Desc:RETURN-INSERT in frame newtbl = yes.
   
   /* runtime adjustment of "Optional" title band to the width of the frame */
   s_optional:width-chars in frame newtbl = eff_frame_width - ({&HFM_WID} * 2).
end.

/* Erase any status from the last time */
s_Status = "".
display s_Status with frame newtbl.
s_btn_Done:label in frame newtbl = "Cancel".

s_Tbl_Type = s_DbCache_Type[s_DbCache_ix].

/* Each add will be a subtransaction */
s_OK_Hit = no.
add_subtran:
repeat ON ERROR UNDO,LEAVE ON ENDKEY UNDO,LEAVE  ON STOP UNDO, LEAVE:
   /* Do this up top here, to be sure we committed the last create */
   if s_OK_Hit then leave add_subtran.

   if added AND s_btn_Done:label in frame newtbl <> "Close" then
      s_btn_Done:label in frame newtbl = "Close".

   create b_File.
/* moved enable statement to behind the create statement to avoid */
/* problems with buffer b_File <hutegger, 94/02/03> */
/* Note: the order of enables will govern the TAB order. */
/* FIX? what about owner and gateway name? Are they generated by gateway? */
   enable   
       b_File._File-Name
       b_File._Dump-Name
       b_File._Hidden
       s_Tbl_Type          when INDEX(capab, {&CAPAB_TBL_TYPE_ADD}) > 0
       b_File._File-label
       b_File._Desc
       b_File._Fil-misc2[6]
       b_File._For-Size	   when INDEX(capab, {&CAPAB_CHANGE_TBL_SIZE}) > 0
       b_file._For-Name	   when INDEX(capab, {&CAPAB_CHANGE_FOR_NAME}) > 0
       s_btn_Tbl_Triggers
       s_btn_Tbl_Validation
       s_btn_Tbl_StringAttrs
       s_btn_OK
       s_btn_Add
       s_btn_Done
       s_btn_Help
       with frame newtbl.

   /* Have to display all fields, so on 2nd or 3rd add, any entered values
      will be cleared. */
   display "" @ b_File._File-Name   /* display blank instead of ? */
      	   s_optional
      	   s_Tbl_Type
      	   "" @ b_File._Dump-Name
      	   b_File._Hidden
      	   b_File._Frozen
      	   b_File._File-label
      	   b_File._Desc
           b_File._Fil-misc2[6]
      	   b_File._For-Size when INDEX(capab, {&CAPAB_TBL_SIZE}) > 0

      	   (if INDEX(capab, {&CAPAB_FOR_NAME}) = 0 then
      	       "n/a"
      	    else
      	       b_File._For-Name
      	   ) @ b_File._For-Name

      	   (if INDEX(capab, {&CAPAB_OWNER}) = 0 then
      	       "n/a"
      	    else
      	       b_File._For-Owner
      	   ) @ b_File._For-Owner
 
          /* Display oracle dist. db status */
       
             IF INDEX(s_Tbl_Type,"ORACLE") = 0 then "n/a" ELSE IF 
             b_File._Fil-misc2[8] = ? THEN "<Local-Db>" ELSE
             b_File._Fil-misc2[8] @ b_File._Fil-misc2[8]
             
      	   with frame newtbl.

   wait-for choose of s_btn_OK, s_btn_Add in frame newtbl OR
      	    GO of frame newtbl
      	    FOCUS b_File._File-Name in frame newtbl.
end.

hide frame newtbl.

if s_OK_Hit then /* but not Create */
      apply "choose" to MENU-ITEM mi_Crt_Field in MENU s_mnu_Create.
return.
