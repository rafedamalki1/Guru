/*************************************************************/
/* Copyright (c) 1984-1995 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/* _lodfini.p - finish up .df load process */

{ as4dict/dictvar.i }
{ as4dict/load/loaddefs.i }


DEFINE VARIABLE i AS INTEGER NO-UNDO.

FIND FIRST as4dict.p__File WHERE as4dict.p__File._Dump-name = ?
  AND NOT as4dict.p__File._File-name BEGINS "_" NO-ERROR.
IF AVAILABLE as4dict.p__File THEN DO TRANSACTION:
  HIDE MESSAGE NO-PAUSE.
  IF TERMINAL <> "" AND CURRENT-WINDOW:MESSAGE-AREA = yes THEN
     MESSAGE "Assigning Dump-names".
  RUN "as4dict/load/_lodname.p".
END.

IF frozencache <> "" THEN DO TRANSACTION:
  HIDE MESSAGE NO-PAUSE.
  IF TERMINAL <> "" AND CURRENT-WINDOW:MESSAGE-AREA = yes THEN
     MESSAGE "Marking FROZEN tables".
  DO i = 1 TO NUM-ENTRIES(frozencache):
    FIND as4dict.p__File WHERE RECID(as4dict.p__File) = INTEGER(ENTRY(i,frozencache)) NO-ERROR.
    IF AVAILABLE as4dict.p__File AND as4dict.p__File._Frozen = "N" THEN 
       assign as4dict.p__File._Frozen = "Y".
  END.
END.

HIDE MESSAGE NO-PAUSE.
RETURN.
