/*************************************************************/
/* Copyright (c) 1984-1995 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/*----------------------------------------------------------------------------

File: _qflddat.p

Description:
   Display _Field information for the quick field report.  It will go to 
   the currently set output device (e.g., a file, the printer).
 
Input Parameters:
   p_DbId  - Id of the _Db record for this database.
   p_Tbl   - The name of the table whose fields we're showing or "ALL"
   p_Order - "a" for alphabetical order or "o" for _Order order. 

Author: Tony Lavinio, Laura Stern

Date Created: 10/02/92

Modifed on 05/31/95 gfs Allow display of hidden tables (not meta-schema).
           06/14/94 gfs Added NO-LOCKs on file accesses.
----------------------------------------------------------------------------*/
DEFINE NEW GLOBAL SHARED VARIABLE fhidden AS LOGICAL NO-UNDO INITIAL NO.

DEFINE INPUT PARAMETER p_DbId  AS RECID NO-UNDO.
DEFINE INPUT PARAMETER p_Tbl   AS CHAR  NO-UNDO.
DEFINE var p_Order AS CHAR  NO-UNDO.



DEFINE BUFFER   bField FOR _Field.
DEFINE BUFFER   bFile  FOR _File.
DEFINE VARIABLE flags  AS  CHARACTER    NO-UNDO.

FORM
   bFile._File-name  LABEL "Table"
   SKIP
   WITH FRAME tblhdr USE-TEXT STREAM-IO SIDE-LABELS.      

FORM
   bField._Order      FORMAT ">>>>9" COLUMN-LABEL "Order"
   bField._Field-name FORMAT "x(31)" COLUMN-LABEL "Field Name"
   bField._Data-type  FORMAT "x(12)" COLUMN-LABEL "Data Type"
   flags              FORMAT "x(4)"  COLUMN-LABEL "Flags"
   bField._Format     FORMAT "x(19)" COLUMN-LABEL "Format"
   WITH FRAME shofield STREAM-IO
   DOWN USE-TEXT.

FORM
  SKIP(1) 
  SPACE(3) bFile._File-name LABEL "Working on" FORMAT "x(32)" SPACE
  SKIP(1)
  WITH FRAME working_on SIDE-LABELS VIEW-AS DIALOG-BOX 
  TITLE "Generating Report".


/*=========================Internal Procedures==========================*/

/*---------------------------------------
   Display the data for the record
   in the bField buffer.
---------------------------------------*/
PROCEDURE Display_Rec:
   DISPLAY 
      bField._Order
      bField._Field-name
  
      /* data type */
	/* ksu 94/02/24 SUBSTRING use default mode */
      SUBSTRING(bField._Data-type,1,4,"CHARACTER":u)
        + (IF bField._Data-type <> "Decimal" OR bField._Decimals = ? THEN ""
          ELSE "-" + STRING(bField._Decimals))
        + (IF bField._Extent = 0 THEN ""
          ELSE "[" + STRING(bField._Extent) + "]")
        @ bField._Data-type
  
      /* flags */
      (   (IF bField._Fld-case THEN "c" ELSE "")
        + (IF CAN-FIND(FIRST _Index-field OF bField)
          THEN "i" ELSE "")
        + (IF bField._Mandatory THEN "m" ELSE "")
        + (IF CAN-FIND(FIRST _View-ref
          WHERE _View-ref._Ref-Table = bFile._File-name
          AND _View-ref._Base-col = bField._Field-name)
          THEN "v" ELSE "")
      ) @ flags

      bField._Format
      WITH FRAME shofield.
   DOWN  WITH FRAME shofield.
END.


/*============================Mainline Code=============================*/

IF p_Tbl = "ALL" THEN
   SESSION:IMMEDIATE-DISPLAY = yes.

FOR EACH bFile NO-LOCK WHERE 
               	     (IF p_Tbl = "ALL" THEN (IF NOT fHidden THEN NOT bFile._Hidden ELSE bFile._File-Number > 0)
      	       	     	               ELSE bFile._File-name = p_Tbl)
      	       BY bFile._File-name:

   

   IF p_Tbl = "ALL" THEN
      DISPLAY bFile._File-name WITH FRAME working_on.

   DISPLAY  bFile._File-name WITH FRAME tblhdr.
   
   if INDEX(p_Order, "a") > 0 THEN
      FOR EACH bField OF bFile NO-LOCK USE-INDEX _Field-Name:
	 RUN Display_Rec. 
      END.
   ELSE
      FOR EACH bField OF bFile NO-LOCK USE-INDEX _Field-Position:
	 RUN Display_Rec.
      END.
END.


IF p_Tbl = "ALL" THEN
DO:
   HIDE FRAME working_on NO-PAUSE.
   SESSION:IMMEDIATE-DISPLAY = no.
END.


