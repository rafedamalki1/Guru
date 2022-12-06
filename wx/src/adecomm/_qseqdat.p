/*************************************************************/
/* Copyright (c) 1984-1994 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/*----------------------------------------------------------------------------

File: _qseqdat.p

Description:
   Display _Sequence information for the quick sequence report.  It will go 
   to the currently set output device (e.g., a file, the printer).
 
Input Parameters:
   p_DbId - Id of the _Db record for this database.

Author: Tony Lavinio, Laura Stern

Date Created: 10/05/92

Modified on 06/14/94 by Gerry Seidl. Added NO-LOCKs to file accesses.
----------------------------------------------------------------------------*/

DEFINE INPUT PARAMETER p_DbId  AS RECID NO-UNDO.

DEFINE SHARED STREAM rpt.
DEFINE VAR max_min AS INTEGER NO-UNDO.

FORM
  _Sequence._Seq-Name  FORMAT "x(32)"  	    COLUMN-LABEL "Sequence Name"
  _Sequence._Seq-init  FORMAT "->>>>>>>>9"  COLUMN-LABEL "Initial!Value"
  _Sequence._Seq-incr  FORMAT "->>>>>>>>9"  COLUMN-LABEL "Increment"
  max_min              FORMAT "->>>>>>>>>9" COLUMN-LABEL "Max/Min!Value"
  _Sequence._Cycle-Ok  FORMAT "yes/no"	    COLUMN-LABEL "Cycle?"
  WITH FRAME shoseqs 
  DOWN USE-TEXT STREAM-IO.

FOR EACH _Sequence NO-LOCK WHERE _Sequence._Db-recid = p_DbId:
   max_min = (IF _Sequence._Seq-incr > 0 THEN _Sequence._Seq-max
      	       	     	      	       	 ELSE _Sequence._Seq-min).
   DISPLAY STREAM rpt
      _Sequence._Seq-Name 
      _Sequence._Seq-init 
      _Sequence._Seq-incr 
      max_min
      _Sequence._Cycle-Ok 
      WITH FRAME shoseqs.
  DOWN STREAM rpt WITH FRAME shoseqs.
END.

