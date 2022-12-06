/*************************************************************/
/* Copyright (c) 1984-1996 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/*----------------------------------------------------------------------------

File: nexttbl.i

Description:
   Find the next table, alphabetically, in the current database.

Arguments:
   &Name - table name to get next for
   &Next - "next" variable - will get set with the name of the next
      	   table (or "" if there is no next)

Author: Laura Stern

Date Created: 07/22/93

----------------------------------------------------------------------------*/

if s_Show_Hidden_Tbls then
   find FIRST as4dict.p__File where as4dict.p__File._File-Name > {&Name}
      	     	          USE-INDEX  p__filel0 NO-ERROR.
else
   find FIRST as4dict.p__File where  as4dict.p__File._File-Name > {&Name} 
      	     	      	         and as4dict.p__File._Hidden <> "Y"
      	     	          USE-INDEX  p__filel0 NO-ERROR.

{&Next} = (if AVAILABLE as4dict.p__File then as4dict.p__File._File-name else "").

