/*************************************************************/
/* Copyright (c) 1984-1995 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/*----------------------------------------------------------------------------

File: _btnup.p

Description:
   Toggle whichever one of the browse window "show object" buttons is pushed 
   in to be un-pushed.  This means bring the "up" button to the top of 
   the z order stack so that this is the one users see.

Author: Laura Stern

Date Created: 09/22/92 
           Modified to work with PROGRESS/400 Data Dictionary   D. McMann
----------------------------------------------------------------------------*/

{as4dict/dictvar.i shared}
{as4dict/brwvar.i shared}

case s_CurrObj:
   when {&OBJ_DB} then
      s_Res = s_Icn_Dbs:LOAD-IMAGE("adeicon/db-u").
   when {&OBJ_TBL} then
      s_Res = s_Icn_Tbls:LOAD-IMAGE("adeicon/table-u").
   when {&OBJ_SEQ} then
      s_Res = s_Icn_Seqs:LOAD-IMAGE("adeicon/seq-u").
   when {&OBJ_FLD} then
      s_Res = s_Icn_Flds:LOAD-IMAGE("adeicon/flds-u").
   when {&OBJ_IDX} then
      s_Res = s_Icn_Idxs:LOAD-IMAGE("adeicon/index-u").
end.


