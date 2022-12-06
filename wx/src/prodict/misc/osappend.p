/*************************************************************/
/* Copyright (c) 1984-1996 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/* This routine appends <appe> to the end of <base>. */

DEFINE INPUT PARAMETER appe AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER base AS CHARACTER NO-UNDO.

IF CAN-DO("MSDOS,WIN32",OPSYS) THEN
  DOS SILENT type VALUE(appe) >> VALUE(base).

ELSE IF OPSYS = "OS2" THEN
  OS2 SILENT type VALUE(appe) >> VALUE(base).

ELSE IF OPSYS = "UNIX" THEN
  UNIX SILENT cat VALUE(appe) >> VALUE(base).

ELSE IF OPSYS = "VMS" THEN
  VMS SILENT append VALUE(appe) VALUE(base).

ELSE IF OPSYS = "BTOS" THEN
  BTOS SILENT OS-APPEND VALUE(appe) VALUE(base).

ELSE MESSAGE "osappend.p: Unknown Operating System -" OPSYS.

RETURN.
