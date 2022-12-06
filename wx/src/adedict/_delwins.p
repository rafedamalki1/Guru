/*************************************************************/
/* Copyright (c) 1984-1993 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/*----------------------------------------------------------------------------

File: _delwins.p

Description:
   Delete any open edit windows.

Input Parameter:
   p_Db - Set to yes to include the DB window in the set of windows to
      	  close.
 
Author: Laura Stern

Date Created: 04/24/92 

----------------------------------------------------------------------------*/

{adedict/dictvar.i shared}

Define INPUT PARAMETER p_Db as logical NO-UNDO.

if p_Db then
   if s_win_Db <> ? then
   do:
      {adedict/delwin.i &Win = s_win_Db &Obj = {&OBJ_DB}}
   end.

if s_win_Tbl <> ? then
do:
   {adedict/delwin.i &Win = s_win_Tbl &Obj = {&OBJ_TBL}}
end.

if s_win_Seq <> ? then
do:
   {adedict/delwin.i &Win = s_win_Seq &Obj = {&OBJ_SEQ}}
end.

if s_win_Fld <> ? then
do:
   {adedict/delwin.i &Win = s_win_Fld &Obj = {&OBJ_FLD}}
end.

/*---------------------
if s_win_Dom <> ? then
do:
   {adedict/delwin.i &Win = s_win_Dom &Obj = {&OBJ_DOM}}
end.
---------------------*/

if s_win_Idx <> ? then
do:
   {adedict/delwin.i &Win = s_win_Idx &Obj = {&OBJ_IDX}}
end.



