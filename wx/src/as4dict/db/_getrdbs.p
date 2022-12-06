/*************************************************************/
/* Copyright (c) 1984-1998 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/*----------------------------------------------------------------------------

File: _getrdbs.p

Description:   
   Get the list of foreign databases for which the current database is a
   schema holder looking through the _Db file.

   Add each one to the cache of databases making sure that it is not
   already there.

Author: Laura Stern

Date Created: 01/28/92 
    Modified: 01/1995 Modified to run with PROGRESS/400 Data Dictionary
              06/19/96 D. McMann changed during update to support logical
                       database name and still display dictionary library
              06/25/97 D. McMann 97-06-06-029 Logical Name
              06/03/98 D. McMann 98-04-03-003 Changed how &Holder name is assigned

----------------------------------------------------------------------------*/


{as4dict/dictvar.i shared}
{as4dict/brwvar.i shared}


Define var lname as char    NO-UNDO.
Define var i as integer no-undo.
lname = LDBNAME("DICTDB"). /* logical name for current database */

db_record:
do i = 1 to num-dbs:
  if dbtype(i) = "AS400" THEN DO:
    IF CONNECTED(LDBNAME(i)) THEN DO:
        IF s_lst_Dbs:LOOKUP(PDBNAME(i)) in frame browse = 0 then do:
       	 s_Res = s_lst_Dbs:add-last(PDBNAME(i)) in frame browse.

       	 /* Physical name will only be set if this database is connected.
       	    Otherwise, it will be ? */
       	 {as4dict/DB/cachedb.i &Lname  = LDBNAME(i)
      	       	     	         &Pname  = PDBNAME(i)
      	       	     	         &Holder = SDBNAME(i)
      	       	     	         &Type   = CAPS(DBTYPE(i))}
     	  END.
     END.
  END.
end.

return.

