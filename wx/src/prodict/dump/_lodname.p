/*************************************************************/
/* Copyright (c) 1984-1995 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/* _lodname.p - assigns unique dump-names to file w/o them */

DEFINE VARIABLE nam  AS CHARACTER NO-UNDO.
DEFINE VARIABLE pass AS INTEGER   NO-UNDO.

/* when pulling oracle schema we could have two db's with the same
 * table-name (distributed dbs...). We result in one table named
 * <name> and the other named <name>-1. To make sure, their dump-names
 * are corresponding, we first try to assing the dump-names to the
 * tables with _fil-misc2[8] = ? (:= local-db). In the second-step we
 * assign dump-names to all the rest (:= distributed db's when ORACLE,
 * or do-never-loop for non-oracle-db's)
 */
FOR EACH _File
  WHERE   _Dump-name    =    ?
  AND NOT _File-name  BEGINS "_"
  AND     _Fil-misc2[8] =    ?:

    assign nam = _File-name.
    {prodict/dump/dumpname.i}
    assign _Dump-name = nam.

END.

FOR EACH _File
  WHERE   _Dump-name    =    ?
  AND NOT _File-name  BEGINS "_"
  BY      _Fil-misc2[8]:

    assign nam = _File-name.
    {prodict/dump/dumpname.i}
    assign _Dump-name = nam.

END.

RETURN.
