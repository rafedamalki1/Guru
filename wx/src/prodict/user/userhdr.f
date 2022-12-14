/*************************************************************/
/* Copyright (c) 1984-1997 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/


/* userhdr.f - user interface shared frame definitions */

DEFINE {1} SHARED FRAME user_hdr.

&IF "{&WINDOW-SYSTEM}" = "TTY" &THEN
  
  FORM
    user_hdr FORMAT "x(60)" 
    "Data Dictionary "
    WITH FRAME user_hdr 
    ROW SCREEN-LINES - 1 COLUMN 1 
    NO-BOX ATTR-SPACE NO-LABELS USE-TEXT
    COLOR DISPLAY VALUE(head-bg) PROMPT VALUE(head-bg).
&ENDIF

DEFINE {1} SHARED FRAME user_ftr.
FORM
  user_dbname   FORMAT "x(28)" LABEL "Database"
  user_filename FORMAT "x(34)" LABEL "Table"
  WITH FRAME user_ftr 
  ROW SCREEN-LINES COLUMN 1 &if "{&window-system}" = "ms-windows" &then width 95 &endif
  NO-BOX NO-ATTR-SPACE SIDE-LABELS USE-TEXT.

COLOR DISPLAY VALUE(head-fg) user_dbname user_filename WITH FRAME user_ftr.
