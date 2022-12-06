/*************************************************************/
/* Copyright (c) 1984-1996 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

DEFINE INPUT PARAMETER odb-name     AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER pro-name     AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER pro-conparms AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER p_edbtype  AS CHARACTER NO-UNDO.


/* connect up the databases to work with ------------------------------------*/

DELETE ALIAS DICTDB2.

CONNECT VALUE(pro-name) -ld DICTDB2 VALUE (pro-conparms) -1.

RUN prodict/odb/_odb_fix.p (p_edbtype).

DISCONNECT DICTDB2.
RETURN.
