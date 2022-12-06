/*************************************************************/
/* Copyright (c) 1984-1993 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/*----------------------------------------------------------------------------

File: _ptinlst.p

Description:
   Put the new field or existing field whose name or order # has changed
   into the browse select list in the correct position.

Input Parameters:
   p_Name  - The name of the new or modified field
   p_Order - The new or modified order# for this field.

Author: Laura Stern

Date Created: 12/01/92 

----------------------------------------------------------------------------*/

{adedict/dictvar.i shared}
{adedict/brwvar.i shared}


Define INPUT PARAMETER p_Name  as char 	  NO-UNDO.
Define INPUT PARAMETER p_Order as integer NO-UNDO.

Define var ins_name as char    NO-UNDO.


if s_Order_By = {&ORDER_ALPHA} then
   find FIRST _Field where _Field._File-recid = s_TblRecId AND
	     	      	   _Field._Field-Name > p_Name
      NO-ERROR.
else
   find FIRST _Field where _Field._File-recid = s_TblRecId AND
	     	      	    _Field._Order > p_Order
      NO-ERROR.

ins_name = (if AVAILABLE _Field then _Field._Field-name else "").
run adedict/_newobj.p 
   (INPUT s_lst_Flds:HANDLE in frame browse,
    INPUT p_Name,
    INPUT ins_name,
    INPUT s_Flds_Cached,
    INPUT {&OBJ_FLD}).
