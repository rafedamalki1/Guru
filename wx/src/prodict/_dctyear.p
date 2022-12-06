/*************************************************************/
/* Copyright (c) 1984-1993 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/* _dctyear.p - returns -d <mdy> setting and -yy <nnnn> setting */

DEFINE OUTPUT PARAMETER mdy AS CHARACTER         NO-UNDO.
DEFINE OUTPUT PARAMETER yy  AS INTEGER INITIAL ? NO-UNDO.
DEFINE        VARIABLE  i   AS INTEGER           NO-UNDO.

DO i = 1900 TO 2000 WHILE yy = ?:
  IF LENGTH(STRING(DATE(1,1,i))) = 8 AND LENGTH(STRING(DATE(1,1,i + 99))) = 8
    THEN yy = i.
END.
DO i = 1000 TO 1900 WHILE yy = ?:
  IF LENGTH(STRING(DATE(1,1,i))) = 8 AND LENGTH(STRING(DATE(1,1,i + 99))) = 8
    THEN yy = i.
END.
DO i = 2000 TO 9900 WHILE yy = ?:
  IF LENGTH(STRING(DATE(1,1,i))) = 8 AND LENGTH(STRING(DATE(1,1,i + 99))) = 8
    THEN yy = i.
END.

ASSIGN
mdy = ENTRY(LOOKUP(STRING(DATE(2,1,1803)),
      "01/02/1803,01/1803/02,02/01/1803,02/1803/01,1803/01/02,1803/02/01"),
      "dmy,dym,mdy,myd,ydm,ymd").

RETURN.
