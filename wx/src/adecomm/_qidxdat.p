/*************************************************************/
/* Copyright (c) 1984-1995 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/*----------------------------------------------------------------------------

File: _qidxdat.p

Description:
   Display _Index information for the quick index report.  It will go to 
   the currently set output device (e.g., a file, the printer).
 
Input Parameters:
   p_DbId - Id of the _Db record for this database.
   p_Tbl  - The name of the table whose indexes we're showing or "ALL".

Author: Tony Lavinio, Laura Stern

Date Created: 10/05/92

Modified on 05/31/95 gfs Allow display of hidden tables (not meta-schema).
            06/14/94 gfs Added NO-LOCKs to file accesses.
----------------------------------------------------------------------------*/
{ prodict/fhidden.i }

DEFINE INPUT PARAMETER p_DbId AS RECID NO-UNDO.
DEFINE INPUT PARAMETER p_Tbl  AS CHAR  NO-UNDO.

DEFINE SHARED STREAM rpt.
DEFINE VARIABLE flags      AS CHARACTER   NO-UNDO.

FORM
   _File._File-name  LABEL "Table"
   SKIP
   WITH FRAME tblhdr NO-ATTR-SPACE USE-TEXT SIDE-LABELS STREAM-IO.

FORM
  flags                   FORMAT "x(5)"  COLUMN-LABEL "Flags"
  _Index._Index-name      FORMAT "x(32)" COLUMN-LABEL "Index Name"
  _Index._Num-comp        FORMAT ">>9"   COLUMN-LABEL "Cnt"
  _Index-field._Ascending FORMAT "+ /- " COLUMN-LABEL "Fi" SPACE(0)
  _Field._Field-name      FORMAT "x(31)" COLUMN-LABEL "eld Name"
  WITH FRAME shoindex 
  DOWN USE-TEXT STREAM-IO.

FORM
  SKIP(1) 
  SPACE(3) _File._File-name LABEL "Working on" FORMAT "x(32)" SPACE
  SKIP(1)
  WITH FRAME working_on SIDE-LABELS VIEW-AS DIALOG-BOX 
  TITLE "Generating Report".


/*----------------------------Mainline code--------------------------------*/

IF p_Tbl = "ALL" THEN
   SESSION:IMMEDIATE-DISPLAY = yes.

FOR EACH _File NO-LOCK WHERE _File._Db-recid = p_DbId AND
               	     (IF p_Tbl = "ALL" THEN (IF NOT fHidden THEN NOT _File._Hidden ELSE _File._File-Number > 0)
      	       	     	               ELSE _File._File-name = p_Tbl)
      	       BY _File._File-name:

   PAGE STREAM rpt.

   IF p_Tbl = "ALL" THEN
      DISPLAY _File._File-name WITH FRAME working_on.

   DISPLAY STREAM rpt _File._File-name WITH FRAME tblhdr.

   FOR EACH _Index OF _File NO-LOCK BREAK BY _Index._Index-name:
      FIND LAST _Index-field OF _Index NO-LOCK NO-ERROR.
      flags = 
      	 ( (IF _File._Prime-index = RECID(_Index) 
	       THEN "p" ELSE "")
	   + (IF _Unique   
	       THEN "u" ELSE "")
	   + (IF NOT _Active
	       THEN "i" ELSE "") 
	   + (IF _Index._Wordidx = 1
	       THEN "w" ELSE "") 
      	   + (IF AVAILABLE _Index-field AND _Index-field._Abbreviate
      	       THEN "a" ELSE "") ).

      DISPLAY STREAM rpt
      	  flags
	  _Index._Index-name
      	  _Index._Num-comp
      	  WITH FRAME shoindex.

      /* The default index has no fields! so this loop must be separate
      	 from the FOR EACH _Index loop or we'll get no output.
      */
      FOR EACH _Index-field OF _Index,
               _Field OF _Index-field NO-LOCK: 

	 DISPLAY STREAM rpt
	    _Index-field._Ascending
	    _Field._Field-name
	    WITH FRAME shoindex.
	DOWN STREAM rpt WITH FRAME shoindex.
      END.
      
     /* Put an extra line in between each index. */
     IF LAST-OF(_Index._Index-name) THEN 
        DOWN STREAM rpt 1 WITH FRAME shoindex.
   END.
END.

IF p_Tbl = "ALL" THEN
DO:
   HIDE FRAME working_on NO-PAUSE.
   SESSION:IMMEDIATE-DISPLAY = no.
END.
