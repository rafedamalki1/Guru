/*************************************************************/
/* Copyright (c) 1984-1993 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/*----------------------------------------------------------------------------

File: _istbl.i

Description: 
   Check to see if a table is committed to the database and therefore
   in the schema cache by trying to compile this .i which references
   the table.
 
Argument:
   &1  - Name of current database
   &2  - The table to check.

Author: Laura Stern

Date Created: 12/04/92 

----------------------------------------------------------------------------*/

/* We don't want any find trigger firing while we're doing this. */
disable triggers for dump of {1}.{2}.
find FIRST {1}.{2} NO-ERROR. 

