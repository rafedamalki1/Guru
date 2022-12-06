/*************************************************************/
/* Copyright (c) 1984-1995 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/*----------------------------------------------------------------------------

File: _getflst.p

Description:
   Fill a selection list with fields.  The fields will be ordered either
   either alphabetically or by order #.
   The database that these fields are stored in must be aliased to
   to DICTDB before this routine is called.

Input Parameters:
   p_List   - Handle of the selection list widget to add to.
	      p_List:private-data is a comma seperated list of items
	      NOT to be added. OR no items if the entire table-list is
	      to be added.

   p_Recid  - The recid of the table to which these fields belong.
   p_Alpha  - true if order should be alphabetical, false if we want to
      	      order by the _Order value of the field.
   p_Items  - List of fields to included in list.  If this parameter is
              blank, then just the field name is put in the list.  Currently
              the parameter can have the following values in any order:
              1 - just the field name - then is the default
              2 - tablename.fldname
              3 - dbname.tablename.fldname
              T - Name of table containing the field
              F - Format
   p_DType  - The data type to screen for.  If this is ?, then don't screen.
      	      Otherwise this must be either
      	       	  "character"
      	       	  "date"
      	       	  "decimal"
      	       	  "integer"
      	       	  "logical"
      	       	  "recid"
   p_ExpandExtent  - 0  field name only for extent field
                     1  extented field name changed to fieldName[1-N].
                     2  extented field names are expanded

  p_CallBack  - The name of the program to run for security of a "" to blow
              this callback off.


Output Parameters:
   p_Stat   - Set to true if list is retrieved (even if there were no fields
      	      this is successful).  Set to false, if user doesn't have access
      	      to fields.

Author: Laura Stern, Warren Bare

Date Created: 06/02/92 

----------------------------------------------------------------------------*/


Define INPUT  PARAMETER p_List         as widget-handle NO-UNDO.
Define INPUT  PARAMETER p_Recid        as recid     	 NO-UNDO.
Define INPUT  PARAMETER p_Alpha        as logical   	 NO-UNDO.
Define INPUT  PARAMETER p_Items        as character 	 NO-UNDO.
Define INPUT  PARAMETER p_DType        as character      NO-UNDO.
Define INPUT  PARAMETER p_ExpandExtent as integer   	 NO-UNDO.
Define INPUT  PARAMETER p_CallBack     as character   	 NO-UNDO.


Define OUTPUT PARAMETER p_Stat         as logical    	 NO-UNDO.


define buffer bField for as4dict.p__Field.

define var v_dbname  as char           NO-UNDO.
define var err       as logical        NO-UNDO.
define var widg      as widget-handle  NO-UNDO.
define var v_ItemCnt as integer	       NO-UNDO.
define var v_BldLine as char  	       NO-UNDO.
Define var lInclude  as logical        NO-UNDO.
define var i         as integer        no-undo.
define var fName     as character      no-undo.
define var sep       as character      no-undo.

 { as4dict/dictvar.i shared }
 
/************************* Inline code section ***************************/

find DICTDB._File "_Field" NO-LOCK.
if NOT can-do(DICTDB._File._Can-read, USERID("DICTDB")) then
do:
   message "You do not have permission to see any field information."
      view-as ALERT-BOX ERROR buttons OK.
   p_Stat = false.
   return.
end.

/* This is where we find other buffer information that may be needed */

FIND as4dict.p__File WHERE RECID(as4dict.p__File) = p_recid NO-ERROR.

FIND FIRST as4dict.p__DB. 
IF p_Items = "" THEN p_Items = "1".

v_dbname = as4dict.p__db._db-name.

widg = p_List:parent.  	/* gives me the group */
widg = widg:parent.  	/* gives me the frame */
run adecomm/_setcurs.p ("WAIT").

if p_Alpha then do:
   if p_DType = ? then
      for each bField where bField._File-number = as4dict.p__file._File-number 
                                    and bField._fld-misc2[5] <> "A" NO-LOCK
    	       	        by bField._Field-Name:
      	 {as4dict/fldlist.i}
      end.
   else
      for each bField where bField._File-number = as4dict.p__file._File-number
      	       	     	       	   AND bField._Data-type = p_DType 
      	       	     	       	   and bField._fld-misc2[5] <> "A" NO-LOCK
    	       	     	      	   by bField._Field-Name:
      	 {as4dict/fldlist.i}
      end.
end.
else do:
   if p_DType = ? then
      for each bField where bField._File-number = as4dict.p__File._File-number 
                                         and bField._fld-misc2[5] <> "A" NO-LOCK
   	       	     	      	   by bField._Order:
      	 {as4dict/fldlist.i}
      end.
   else
      for each bField where bField._File-number = as4dict.p__File._File-number 
      	       	     	       	   AND bField._Data-type = p_DType 
      	       	     	       	   and bField._fld-misc2[5] <> "A" NO-LOCK
   	       	     	      	   by bField._Order:
      	 {as4dict/fldlist.i}
      end.
end.

run adecomm/_setcurs.p ("").
p_Stat = true.
RETURN.


