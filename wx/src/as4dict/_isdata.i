/*************************************************************/
/* Copyright (c) 1984-1995 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/*----------------------------------------------------------------------------

File: _isdata.i

Description: 
   Check to see if there is any data in this table.  This will be compiled
   at run time when we know what the table name is.
 
Argument:
   &1  - The table to check.

Output Parameter:
   p_IsData - Flag - set to yes if there is data, no otherwise.

Author: Laura Stern

Date Created: 11/16/92 

----------------------------------------------------------------------------*/

Define OUTPUT parameter p_IsData as logical NO-UNDO.


p_IsData = CAN-FIND(FIRST {1}).
