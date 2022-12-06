/*************************************************************/
/* Copyright (c) 1984-1996 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/* this splits the filename prefix from the basename */

DEFINE INPUT  PARAMETER filename AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER fiprefix AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER basename AS CHARACTER NO-UNDO.
DEFINE VARIABLE i AS INTEGER NO-UNDO.
basename = filename.

IF CAN-DO("OS2,MSDOS,WIN32,UNIX,VMS",OPSYS) THEN DO:
  i = INDEX(basename,":").
  DO WHILE i > 0:
    ASSIGN
      basename = SUBSTRING(basename,i + 1)
      i        = INDEX(basename,":").
  END.
END.

IF CAN-DO("OS2,MSDOS,WIN32,UNIX",OPSYS) THEN DO:
  i = MAXIMUM(INDEX(basename,"~\"),INDEX(basename,"/")).
  DO WHILE i > 0:
    ASSIGN
      basename = SUBSTRING(basename,i + 1)
      i        = MAXIMUM(INDEX(basename,"~\"),INDEX(basename,"/")).
  END.
END.

IF OPSYS = "BTOS" THEN
  IF basename MATCHES "~{*~}*" THEN
    basename = SUBSTRING(basename,INDEX(basename,"~}") + 1).

IF CAN-DO("VMS,BTOS",OPSYS) THEN
  IF basename MATCHES "[*]*" THEN
    basename = SUBSTRING(basename,INDEX(basename,"]") + 1).

IF CAN-DO("VMS,BTOS",OPSYS) THEN
  IF basename MATCHES "<*>*" THEN
    basename = SUBSTRING(basename,INDEX(basename,">") + 1).

fiprefix = SUBSTRING(filename,1,LENGTH(filename) - LENGTH(basename)).
RETURN.
