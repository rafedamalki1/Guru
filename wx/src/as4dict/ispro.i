/*************************************************************/
/* Copyright (c) 1984-1995 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/*----------------------------------------------------------------------------

File: ispro.i

Description:
   Determine if the current database is a progress database.  This is 
   meant to be plugged into an IF statement - e.g., if {ispro.i} then...
 
Author: Laura Stern

Date Created: 03/05/92

----------------------------------------------------------------------------*/


(s_DbCache_Type[s_DbCache_ix] = "PROGRESS")
