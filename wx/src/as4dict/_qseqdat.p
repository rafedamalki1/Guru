/*************************************************************/
/* Copyright (c) 1984-1995 by Progress Software Corporation  */
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
Modified on 02/10/95 by Donna McMann changed to work with as400 meta schema files
----------------------------------------------------------------------------*/

DEFINE INPUT PARAMETER p_DbId  AS RECID NO-UNDO.

DEFINE SHARED STREAM rpt.
DEFINE VAR max_min AS INTEGER NO-UNDO.
DEFINE VAR cyc_ok  AS CHARACTER NO-UNDO.

FORM
  as4dict.p__Seq._Seq-Name  FORMAT "x(32)"  	 COLUMN-LABEL "Sequence Name"
  as4dict.p__Seq._Seq-init  FORMAT "->>>>>>>>9"  COLUMN-LABEL "Initial!Value"
  as4dict.p__Seq._Seq-incr  FORMAT "->>>>>>>>9"  COLUMN-LABEL "Increment"
  max_min              FORMAT "->>>>>>>>>9"      COLUMN-LABEL "Max/Min!Value"
  cyc_ok               FORMAT "x(3)"             COLUMN-LABEL "Cycle?"
  WITH FRAME shoseqs 
  DOWN USE-TEXT STREAM-IO.

FOR EACH as4dict.p__Seq NO-LOCK:
   max_min = (IF as4dict.p__Seq._Seq-incr > 0 THEN as4dict.p__Seq._Seq-max
      	       	     	      	       	 ELSE as4dict.p__Seq._Seq-min).   
   cyc_ok = (If as4dict.p__Seq._Cycle-OK = "Y" THEN "yes" ELSE "no").
      	       	     	      	       	 
   DISPLAY STREAM rpt
      as4dict.p__Seq._Seq-Name 
      as4dict.p__Seq._Seq-init 
      as4dict.p__Seq._Seq-incr 
      max_min
      cyc_ok 
      WITH FRAME shoseqs.
  DOWN STREAM rpt WITH FRAME shoseqs.
END.

