/*************************************************************/
/* Copyright (c) 1984-1993 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/* _dmpbulk.p - Make .fd file for _proutil -C bulkload */

{ prodict/dictvar.i }
{ prodict/user/uservar.i }

DEFINE STREAM bulk.

IF TERMINAL <> "" THEN 
  run adecomm/_setcurs.p ("WAIT").
OUTPUT STREAM bulk TO VALUE(user_env[2]) NO-MAP.

PUT STREAM bulk UNFORMATTED
  "# Database:  " PDBNAME("DICTDB") SKIP
  "# Date/Time: " STRING(YEAR(TODAY),"9999")
    "/" STRING(MONTH(TODAY),"99")
    "/" STRING(DAY(TODAY),"99")
    "-" STRING(TIME,"HH:MM:SS") SKIP.

FOR EACH _File
  WHERE _File._Db-recid = drec_db
   AND (
     IF user_filename = "ALL" THEN
       _File._File-number > 0
     ELSE
     IF user_filename = "SOME" THEN
       CAN-DO(user_env[1],_File._File-name)
     ELSE
       RECID(_File) = drec_file)
  BREAK BY _File._File-num:

  HIDE MESSAGE NO-PAUSE.
  IF TERMINAL <> "" THEN MESSAGE "Working on" _File._File-name.

  PUT STREAM bulk UNFORMATTED
    _File._File-name " "
    (IF _File._Dump-name = ? THEN _File._File-name ELSE _File._Dump-name) ".d "
    (IF _File._Dump-name = ? THEN _File._File-name ELSE _File._Dump-name) ".e"
    SKIP.

  FOR EACH _Field OF _File BY _Field._Order:
    IF _sys-field OR _Data-type = "recid" THEN NEXT.
    PUT STREAM bulk UNFORMATTED "  " _Field-name SKIP.
  END.

  IF NOT LAST(_File._File-num) THEN
    PUT STREAM bulk UNFORMATTED "." SKIP.

END. /* for each _file */

PUT STREAM bulk UNFORMATTED "# end-of-file" SKIP.
OUTPUT STREAM bulk CLOSE.
IF TERMINAL <> "" THEN 
  run adecomm/_setcurs.p ("").

IF TERMINAL <> "" THEN
  MESSAGE "Making of bulk load description file completed."
    VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.

RETURN.
