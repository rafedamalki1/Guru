/*************************************************************/
/* Copyright (c) 1984-1995 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/*----------------------------------------------------------------------

File: _setid.p

Description:
   Set p_Id to the recid of the _File record for the current table (if 
   p_ObjType is TBL) or to the _File record that domains are associated
   with (if p_ObjType is DOM).

Input Parameter:
   p_ObjType - the type of object (DB or TBL or DOM)

Output Parameter:
   p_Id - The place to store the record Id 

Author: Laura Stern

Date Created: 02/07/92 

-----------------------------------------------------------------------*/

{adedict/dictvar.i shared}

Define input  parameter p_ObjType   as integer.
Define output parameter p_Id        as recid.

if p_ObjType = {&OBJ_DB}
 then do:
   FIND DICTDB._Db
     where DICTDB._Db._DB-Name = (if {adedict/ispro.i}
                                   then ? 
                                   else s_CurrDb
                                 ).
   p_Id = RECID(_Db).
  end.

 else do:  /* domain or table */

   if p_ObjType = {&OBJ_TBL}
    then find DICTDB._File
       where DICTDB._File._File-name = s_CurrTbl
       and   DICTDB._File._DB-recid  = s_DbRecId.
   /*-------------
    else
      /* FIX - Is domain a table in the database with the domains being
      	 it's fields or are the domain fields not associated with a table?
      	 Assume the former. */
      find DICTDB._File
        where DICTDB._File._File-name = "Domain"
      	and   DICTDB._File._DB-recid = s_DbRecId.
   -----------------*/

   p_Id = RECID(DICTDB._File).

  end.     /* domain or table */



