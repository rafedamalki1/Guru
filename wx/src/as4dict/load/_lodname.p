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

FOR EACH as4dict.p__File WHERE as4dict.p__File._Dump-name = ? :
                  

    assign nam = as4dict.p__File._File-name.
    {as4dict/load/dumpname.i}
    assign _Dump-name = nam.

END.

RETURN.
