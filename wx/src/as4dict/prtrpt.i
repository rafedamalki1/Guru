/*************************************************************/
/* Copyright (c) 1984-1995 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/*----------------------------------------------------------------------------

File: prtrpt.i

Description:
   Code to set up correct output device and headers for printing.

Arguments:
   &Header - page header string
   &Flags  - Flags string
   &dev    - output device
   &app    - append option
   &siz    - page size
   &Func   - Call to routine that's generates report.
 
Author: Laura Stern

Date Created: 10/12/92
     Modified: 02/10/95 D McMann changed report heading to Progress/400 Data Dictionary Report
----------------------------------------------------------------------------*/

DEFINE VARIABLE h_datetime AS CHARACTER   NO-UNDO.

/* Header with Page specification */
FORM HEADER
   h_datetime FORMAT "x(17)" SPACE(7) "Progress/400 Data Dictionary Report"
     "Page" AT 70 PAGE-NUMBER (rpt) FORMAT "ZZ9" SKIP
   {&Header} FORMAT "x(80)" SKIP(1)
   {&Flags}  FORMAT "x(80)" SKIP (1)
   WITH FRAME page_head_paged
   PAGE-TOP NO-LABELS NO-BOX NO-ATTR-SPACE NO-UNDERLINE USE-TEXT STREAM-IO.

/* Header for continuous output (no page #) */
FORM HEADER
   h_datetime FORMAT "x(17)" SPACE(7) "DB2/400 Report"
   SKIP
   {&Header} FORMAT "x(80)" SKIP(1)
   {&Flags}  FORMAT "x(80)" SKIP  (1)
   WITH FRAME page_head_cont
   PAGE-TOP NO-LABELS NO-BOX NO-ATTR-SPACE NO-UNDERLINE USE-TEXT STREAM-IO.

run adecomm/_setcurs.p ("WAIT").
DO ON STOP UNDO, LEAVE:
  IF {&dev} BEGINS "|" THEN
     OUTPUT STREAM rpt THROUGH VALUE(SUBSTRING({&dev},2)) 
	PAGE-SIZE VALUE({&siz}).
  ELSE IF {&app} THEN
     OUTPUT STREAM rpt TO VALUE({&dev}) PAGE-SIZE VALUE({&siz}) APPEND.
  ELSE
     OUTPUT STREAM rpt TO VALUE({&dev}) PAGE-SIZE VALUE({&siz}).
  
  h_datetime = STRING(TODAY) + " " + STRING(TIME,"HH:MM:SS").
  
  if {&siz} = 0 then
     VIEW STREAM rpt FRAME page_head_cont.
  else	 
     VIEW STREAM rpt FRAME page_head_paged.
  
  RUN {&Func}.
  
  OUTPUT STREAM rpt CLOSE.
END.
run adecomm/_setcurs.p ("").



