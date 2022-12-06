/*************************************************************/
/* Copyright (c) 1984-1997 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/*----------------------------------------------------------------------------

File: _fldprop.p

Description:
   Set up the field properties window so the user can view or modify the 
   information on a field.  Since this window is non-modal, we just do the
   set up here.  All triggers must be global.

   All of this code is in an include file so that we can use it for fields
   and domains.

Author: Laura Stern

Date Created: 02/05/92 

----------------------------------------------------------------------------*/

&GLOBAL-DEFINE WIN95-BTN YES
{adedict/dictvar.i shared}
{adedict/uivar.i shared}
{adecomm/cbvar.i shared}
{adedict/FLD/fldvar.i shared}



/*----------------------------Mainline code----------------------------------*/

find _File "_Field".
if NOT can-do(_File._Can-read, USERID("DICTDB")) then
do:
   message s_NoPrivMsg "see field definitions."
      view-as ALERT-BOX ERROR buttons Ok in window s_win_Browse.
   return.
end.

/* Don't want Cancel if moving to next field - only when window opens */
if s_win_Fld = ? then
   s_btn_Close:label in frame fldprops = "Cancel".

/* Open the window if necessary */
run adedict/_openwin.p
   (INPUT   	  "Field Properties",
    INPUT   	  frame fldprops:HANDLE,
    INPUT         {&OBJ_FLD},
    INPUT-OUTPUT  s_win_Fld).

/* We haven't finished fiddling with frame yet so to set status line
   don't use display statement.
*/
s_Status:screen-value in frame fldprops = "". /* clears from last time */

s_Fld_ReadOnly = (s_ReadOnly OR s_DB_ReadOnly).
if NOT s_Fld_ReadOnly then
do:
   if NOT can-do(_File._Can-write, USERID("DICTDB")) then
   do:
      s_Status:screen-value in frame fldprops = 
      	 s_NoPrivMsg + " modify field definitions.".
      s_Fld_ReadOnly = true.
   end.
   else do:
      find _File where RECID(_File) = s_TblRecId.
      if _File._Frozen then
      do:
      	 s_Status:screen-value in frame fldprops =
      	    "Note: This table is frozen and cannot be modified.".
      	 s_Fld_ReadOnly = true.
      end.
   end.
end.

{adedict/FLD/fdprop.i &Frame    = "frame fldprops"
      	       	       &ReadOnly = "s_Fld_ReadOnly"}







