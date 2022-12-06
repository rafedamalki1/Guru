/*************************************************************/
/* Copyright (c) 1984-1995 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/*----------------------------------------------------------------------------

File: _closwin.p

Description:
   Close the current edit window.
 
Author: Laura Stern

Date Created: 04/24/92 
           Modified to work with PROGRESS/400 Data Dictionary   D. McMann
----------------------------------------------------------------------------*/

{as4dict/dictvar.i shared}
{as4dict/uivar.i shared}
{adecomm/cbvar.i shared}

{as4dict/DB/dbvar.i shared}
{as4dict/TBL/tblvar.i shared}
{as4dict/SEQ/seqvar.i shared}
{as4dict/FLD/fldvar.i shared}
{as4dict/IDX/idxvar.i shared}

case (SELF):
   when s_win_Db then do:
      {as4dict/delwin.i &Win = s_win_Db &Obj = {&OBJ_DB}}
   end.
   when s_win_Tbl then do:
      {as4dict/delwin.i &Win = s_win_Tbl &Obj = {&OBJ_TBL}}
   end.
   when s_win_Seq then do:
      {as4dict/delwin.i &Win = s_win_Seq &Obj = {&OBJ_SEQ}}
   end.
   when s_win_Fld then do:
      {as4dict/delwin.i &Win = s_win_Fld &Obj = {&OBJ_FLD}}
   end.
   when s_win_Idx then do:
      {as4dict/delwin.i &Win = s_win_Idx &Obj = {&OBJ_IDX}}
   end.
end.

return ERROR.
