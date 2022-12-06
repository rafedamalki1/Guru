/*************************************************************/
/* Copyright (c) 1984-1993 by Progress Software Corporation  */
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
   find FIRST _File where _File._Db-recid = s_DbRecId AND
       		     	  _File._File-Name > {&Name}
      	     	          NO-ERROR.
else
   find FIRST _File where _File._Db-recid = s_DbRecId AND
       		     	  _File._File-Name > {&Name} AND
      	     	      	  NOT _File._Hidden
      	     	          NO-ERROR.

{&Next} = (if AVAILABLE _File then _File._File-name else "").
