/*************************************************************/
/* Copyright (c) 1984-1996 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/* ostodir.p - takes its parameter and makes it look like a directory name */

DEFINE INPUT-OUTPUT PARAMETER dirname AS CHARACTER NO-UNDO.
DEFINE VARIABLE i AS INTEGER NO-UNDO.

IF dirname = "" AND OPSYS = "UNIX" THEN dirname = "./".

i = (IF CAN-DO("MSDOS,WIN32,OS2",OPSYS) THEN INDEX(dirname,"~\") ELSE 0).
DO WHILE i > 0:
  ASSIGN
    OVERLAY(dirname,i,1) = "/"
    i = INDEX(dirname,"~\").
END.

IF CAN-DO("MSDOS,WIN32,OS2,UNIX",OPSYS) AND dirname <> ""
  AND NOT dirname MATCHES "*/" THEN dirname = dirname + "/".

RETURN.
