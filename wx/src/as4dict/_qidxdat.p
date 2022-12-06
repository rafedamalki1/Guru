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

Modified on 06/14/94 by Gerry Seidl. Added NO-LOCKs to file accesses. 
Modified on 02/13/95 by D. McMann to work with Progress/400 data dictionary.
----------------------------------------------------------------------------*/

DEFINE INPUT PARAMETER p_DbId AS RECID NO-UNDO.
DEFINE INPUT PARAMETER p_Tbl  AS CHAR  NO-UNDO.

DEFINE SHARED STREAM rpt.
DEFINE VARIABLE flags      AS CHARACTER   NO-UNDO. 
DEFINE VARIABLE acedec     AS CHARACTER FORMAT "x(1)" NO-UNDO.

FORM
   as4dict.p__File._File-name  LABEL "Table"
   SKIP
   WITH FRAME tblhdr NO-ATTR-SPACE USE-TEXT SIDE-LABELS STREAM-IO.

FORM
  flags                   FORMAT "x(5)"  COLUMN-LABEL "Flags"
  as4dict.p__Index._Index-name      FORMAT "x(32)" COLUMN-LABEL "Index Name"
  as4dict.p__Index._Num-comp        FORMAT ">>9"   COLUMN-LABEL "Cnt"
  acedec       COLUMN-LABEL "Fi" SPACE(0)
  as4dict.p__Field._Field-name      FORMAT "x(31)" COLUMN-LABEL "eld Name"
  WITH FRAME shoindex 
  DOWN USE-TEXT STREAM-IO.

FORM
  SKIP(1) 
  SPACE(3) as4dict.p__File._File-name LABEL "Working on" FORMAT "x(32)" SPACE
  SKIP(1)
  WITH FRAME working_on SIDE-LABELS VIEW-AS DIALOG-BOX 
  TITLE "Generating Report".


/*----------------------------Mainline code--------------------------------*/

IF p_Tbl = "ALL" THEN
   SESSION:IMMEDIATE-DISPLAY = yes.

FOR EACH as4dict.p__File NO-LOCK WHERE (IF p_Tbl = "ALL" THEN 
                                                                                          as4dict.p__File._Hidden = "N"
      	       	     	                                     ELSE as4dict.p__File._File-name = p_Tbl)
      	       BY as4dict.p__File._File-name:

   IF p_Tbl = "ALL" THEN
      DISPLAY as4dict.p__File._File-name WITH FRAME working_on.

   DISPLAY STREAM rpt as4dict.p__File._File-name WITH FRAME tblhdr.

   FOR EACH as4dict.p__Index WHERE as4dict.p__Index._File-number =  
                                         as4dict.p__File._File-number NO-LOCK 
                              BREAK BY as4dict.p__Index._Index-name:
      FIND LAST as4dict.p__Idxfd WHERE as4dict.p__Idxfd._File-number =
                                               as4dict.p__Index._File-number 
             AND as4dict.p__Idxfd._Idx-num = as4dict.p__Index._Idx-num
             NO-LOCK NO-ERROR.
      flags = 
      	 ( (IF as4dict.p__File._Prime-index = as4dict.p__Index._Idx-num 
	       THEN "p" ELSE "")
	   + (IF as4dict.p__Index._Unique = "Y"   
	       THEN "u" ELSE "")
	   + (IF as4dict.p__Index._Active <> "Y"
	       THEN "i" ELSE "") 
	   + (IF as4dict.p__Index._Wordidx = 1
	       THEN "w" ELSE "") 
      	   + (IF AVAILABLE as4dict.p__Idxfd AND 
      	         as4dict.p__Idxfd._Abbreviate = "Y"
      	       THEN "a" ELSE "") ).

      DISPLAY STREAM rpt
      	  flags
	  as4dict.p__Index._Index-name
      	  as4dict.p__Index._Num-comp
      	  WITH FRAME shoindex.

      /* The default index has no fields! so this loop must be separate
      	 from the FOR EACH _Index loop or we'll get no output.
      */
      FOR EACH p__Idxfd where   as4dict.p__Idxfd._File-number = as4dict.p__Index._File-number
                                                    AND as4dict.p__Idxfd._Idx-num = as4dict.p__Index._Idx-num:
           IF as4dict.p__Idxfd._If-misc2[8] = "Y" THEN NEXT.                                        
           FOR EACH as4dict.p__Field where as4dict.p__Field._Fld-number = p__Idxfd._Fld-number  
                                   and as4dict.p__Field._File-number = p__Index._File-number NO-LOCK:
                 ASSIGN acedec = (IF as4dict.p__Idxfd._Ascending = "Y" THEN "+" ELSE "-").
                 DISPLAY STREAM rpt
                      acedec
	    as4dict.p__Field._Field-name
	    WITH FRAME shoindex.
	DOWN STREAM rpt WITH FRAME shoindex.     
             END.
      END.
      
     /* Put an extra line in between each index. */
     IF LAST-OF(p__Index._Index-name) THEN 
        DOWN STREAM rpt 1 WITH FRAME shoindex.
   END.
END.

IF p_Tbl = "ALL" THEN
DO:
   HIDE FRAME working_on NO-PAUSE.
   SESSION:IMMEDIATE-DISPLAY = no.
END.
