/*************************************************************/
/* Copyright (c) 1984-1997 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/*----------------------------------------------------------------------------


File: _setid.p

Description:
   Set p_Id to the recid of the _File record for the current table (if 
   p_ObjType is TBL) or to the _File record that domains are associated with
   (if p_ObjType is DOM).

Input Parameter:
   p_ObjType - the type of object (DB or TBL or DOM)

Output Parameter:
   p_Id - The place to store the record Id 

Author: Laura Stern

Date Created: 02/07/92 

     Modified to work with PROGRESS/400 Data Dictionary 
     
     06/26/97 D. McMann Changed for logical db name problem 97-06-06-029
----------------------------------------------------------------------------*/

{as4dict/dictvar.i shared}

Define input parameter p_ObjType as integer.
Define output parameter p_Id as recid.


if p_ObjType = {&OBJ_DB} then
do: 
   find _db where _db._db-name = Ldbname("as4dict") no-error.
   if available _db then assign p_Id = RECID(_db).
   else assign p_Id = ?.
end.
else do:     
    if p_ObjType = {&OBJ_TBL} then
      find as4dict.p__File where as4dict.p__File._File-name = s_CurrTbl
      	    NO-ERROR.      
      	       	       
/* Save the recid of p__file. Also, save the _for-number into a global to
    be used in lieu of s_TBLRecId.  */

   If available as4dict.p__file then 
       do: 
           p_Id = RECID(as4dict.p__File).
           s_TblForNo = as4dict.p__file._For-number.
       end.       
     
end.
