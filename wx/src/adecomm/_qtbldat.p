/*************************************************************/
/* Copyright (c) 1984-1994 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/*----------------------------------------------------------------------------

File: _qtbldat.p

Description:
   Display _File information for the quick table report.  It will go to 
   the currently set output device (e.g., a file, the printer).
 
Input Parameters:
   p_DbId    - Id of the _Db record for this database.

Author: Tony Lavinio, Laura Stern

Date Created: 10/02/92

----------------------------------------------------------------------------*/

DEFINE INPUT PARAMETER p_DbId AS RECID NO-UNDO.

DEFINE SHARED STREAM rpt.

DEFINE VARIABLE flags   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE fldcnt  AS INTEGER    NO-UNDO INITIAL -1.
DEFINE VARIABLE odbtyp  AS CHARACTER  NO-UNDO. /* list of ODBC-types */


FORM
   _File._File-name  FORMAT "x(29)"  COLUMN-LABEL "Table!Name"
   _File._Dump-name  FORMAT "x(8)"   COLUMN-LABEL "Dump!Name"
   flags             FORMAT "x(5)"   COLUMN-LABEL "Table!Flags" 
   fldcnt            FORMAT ">>>>9"  COLUMN-LABEL "Field!Count"
   _File._numkey     FORMAT ">>>>9"  COLUMN-LABEL "Index!Count"
   _File._File-label FORMAT "x(19)"  COLUMN-LABEL "Table!Label"
   WITH FRAME shotable USE-TEXT STREAM-IO DOWN.

ASSIGN
  odbtyp      = {adecomm/ds_type.i
                  &direction = "ODBC"
                  &from-type = "flags"
                  }.
                  
FIND _Db WHERE RECID(_Db) = p_DbId.
FOR EACH _File WHERE _File._Db-recid = p_DbId AND NOT _File._Hidden:
   ASSIGN
      flags = (IF _File._Db-lang = 1 THEN "s" ELSE "")
      flags = (flags + IF _File._Frozen THEN "f" ELSE "").
    
   DISPLAY STREAM rpt
      _File._File-name
      _File._File-label
      flags
      /* Progress Db's have an extra hidden field that holds the table # 
      	 which gateway Db's don't have.
      */
      (IF _Db._Db-type = "PROGRESS"
       OR _Db._Db-type = "AS400" 
       OR CAN-DO(odbtyp,_Db._Db-type)
				    THEN _File._numfld - 1
      	       	     	      	    ELSE _File._numfld) @ fldcnt
      _File._numkey
      _File._Dump-name
      WITH FRAME shotable.
   DOWN STREAM rpt WITH FRAME shotable.
END.







