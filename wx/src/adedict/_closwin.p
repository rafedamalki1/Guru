/*************************************************************/
/* Copyright (c) 1984-1997 by Progress Software Corporation  */
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

----------------------------------------------------------------------------*/
&GLOBAL-DEFINE WIN95-BTN YES
{adedict/dictvar.i shared}
{adedict/uivar.i shared}
{adecomm/cbvar.i shared}

{adedict/DB/dbvar.i shared}
{adedict/TBL/tblvar.i shared}
{adedict/SEQ/seqvar.i shared}
{adedict/FLD/fldvar.i shared}
{adedict/IDX/idxvar2.i shared}
{adedict/IDX/idxvar.i shared}

case (SELF):
   when s_win_Db then do:
      {adedict/delwin.i &Win = s_win_Db &Obj = {&OBJ_DB}}
   end.
   when s_win_Tbl then do:
      {adedict/delwin.i &Win = s_win_Tbl &Obj = {&OBJ_TBL}}
   end.
   /*-------
   when s_win_Dom then do:
      {adedict/delwin.i &Win = s_win_Dom &Obj = {&OBJ_DOM}}
   end.
   ---------*/
   when s_win_Seq then do:
      {adedict/delwin.i &Win = s_win_Seq &Obj = {&OBJ_SEQ}}
   end.
   when s_win_Fld then do:
      {adedict/delwin.i &Win = s_win_Fld &Obj = {&OBJ_FLD}}
   end.
   when s_win_Idx then do:
      {adedict/delwin.i &Win = s_win_Idx &Obj = {&OBJ_IDX}}
   end.
end.

return ERROR.
