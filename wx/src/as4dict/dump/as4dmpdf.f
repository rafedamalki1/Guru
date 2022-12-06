/*************************************************************/
/* Copyright (c) 1984-1997 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/
/* Created for Progress/400 dump procedures - May 11, 1995   */

FORM
  as4dict.p__db._db-name FORMAT "x(32)":u LABEL "Database" COLON 11 SKIP
  as4dict.p__File._File-name FORMAT "x(32)":u LABEL "Table" COLON 11 SKIP
  as4dict.p__field._Field-name FORMAT "x(32)":u LABEL "Field" COLON 11 SKIP
  as4dict.p__Index._Index-name FORMAT "x(32)":u LABEL "Index" COLON 11 SKIP
  as4dict.p__Seq._Seq-name FORMAT "x(32)":u LABEL "Sequence" COLON 11 SKIP
  HEADER 
    " Dumping definitions.  Press " +
    KBLABEL("STOP") + " to terminate the dump process. " format "x(70)"
  WITH FRAME {&FRAME} OVERLAY THREE-D VIEW-AS DIALOG-BOX
  ROW 4 CENTERED SIDE-LABELS USE-TEXT.

COLOR DISPLAY MESSAGES as4dict.p__db._db-name
                       as4dict.p__File._File-name 
                       as4dict.p__Field._Field-name
                       as4dict.p__Index._Index-name
                       as4dict.p__Seq._Seq-name
                                  WITH FRAME {&FRAME}.                                  
