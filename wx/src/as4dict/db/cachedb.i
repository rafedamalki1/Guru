/*************************************************************/
/* Copyright (c) 1984-1996 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/*----------------------------------------------------------------------------

File: cachedb.i

Description:
   Include file for code to store a new database into the database cache.

Arguments:
   &Lname   - Logical name to store
   &Pname   - Physical name to store
   &Holder  - Name of the schema holder
   &Type    - Database type.

Author: Laura Stern

Date Created: 03/03/92    
     Modified 06/19/96 added logical name for cache for logical name support

----------------------------------------------------------------------------*/


s_DbCache_Cnt = s_DbCache_Cnt + 1.   
s_DbCache_Lname[s_DbCache_Cnt]  = {&Lname}.
s_DbCache_Pname[s_DbCache_Cnt]  = {&Pname}.
s_DbCache_Holder[s_DbCache_Cnt] = {&Holder}.
s_DbCache_Type[s_DbCache_Cnt]   = {&Type}.
