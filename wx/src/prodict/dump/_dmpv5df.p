/*************************************************************/
/* Copyright (c) 1984-1993 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/* dumpv5df - Data Dictionary dump definitions module */

{ prodict/dictvar.i }
{ prodict/user/uservar.i }

DEFINE STREAM chg.

run adecomm/_setcurs.p ("WAIT").
OUTPUT STREAM chg TO VALUE(user_env[2]) NO-ECHO NO-MAP.

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

  EXPORT STREAM chg "NEW-FILE   "
    _File-Name _Can-Create _Can-Read _Can-Write _Can-Delete
    SUBSTRING(_File._Desc,1,72)
    SUBSTRING(_File._Valexp,1,72)
    _Valmsg _Frozen _Hidden _Db-lang _Dump-name.

  FOR EACH _Field OF _File BY _Field-rpos:
    EXPORT STREAM chg "NEW-FIELD  "
      _Field-Name _Data-Type _Decimals _Format _Initial _Label _Mandatory
      _Order _Field._Can-Read _Field._Can-Write _Extent _Valexp _Valmsg
      _Help _Field._Desc _Col-label _Fld-case.
  END.
  PUT STREAM chg UNFORMATTED "." SKIP.

  FOR EACH _Index OF _File:
    /* dont print the default index */
    IF _File._dft-pk AND _File._Prime-Index = RECID(_Index) THEN NEXT.
    EXPORT STREAM chg "NEW-INDEX  " _Index-Name _Unique _active.
    FOR EACH _Index-Field OF _Index:
      FIND _Field OF _Index-Field.
      EXPORT STREAM chg "INDEX-FIELD"
        _Index-Seq _Field-Name _Ascending _Abbreviate.
    END.
    PUT STREAM chg UNFORMATTED "." SKIP.
  END.
  PUT STREAM chg UNFORMATTED "." SKIP.

  IF NOT _File._dft-pk THEN DO:
    FIND _Index WHERE _File._Prime-Index = RECID(_Index).
    EXPORT STREAM chg "PRIMARY    " _Index-Name.
  END.
  PUT STREAM chg UNFORMATTED "." SKIP.

END. /* for each file ... */

OUTPUT STREAM chg CLOSE.
run adecomm/_setcurs.p ("").
MESSAGE "Dump Completed." VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
RETURN.
