/*************************************************************/
/* Copyright (c) 1984-1993 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/*----------------------------------------------------------------------------

File: dtrigchk.i

Description:   
   Check to see if there are any find triggers for this table and
   if so, see if this user has privileges to dump/load with triggers 
   disabled.

Input: The _File buffer has the _File record in it that we want to check.
 
Arguments:
   &OK - Set to true if no FIND trigger is found or user has privileges
      	 to dump load with triggers disabled.  Otherwise, set to false.

Author: Laura Stern

Date Created: 11/23/92 

----------------------------------------------------------------------------*/


find _File-trig of _File where _File-trig._Event = "FIND" NO-ERROR.
if AVAILABLE _File-trig then
   {&OK} = CAN-DO(_File._Can-Dump,USERID(user_dbname)).
else
   {&OK} = yes.

