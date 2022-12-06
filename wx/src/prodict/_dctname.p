/*************************************************************/
/* Copyright (c) 1984-1993 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/* _dctname.p - returns TRUE if name is valid PROGRESS identifier */

DEFINE        VARIABLE  i    AS INTEGER   NO-UNDO.
DEFINE INPUT  PARAMETER name AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER okay AS LOGICAL   NO-UNDO.

okay = LENGTH(name) >= 1 AND LENGTH(name) <= 32
       AND SUBSTRING(name,1,1) >= "A"
       AND SUBSTRING(name,1,1) <= "Z"
       AND KEYWORD(name) = ?.
DO i = 2 TO LENGTH(name) WHILE okay:
  okay = INDEX("#$%&-_0123456789ETAONRISHDLFCMUGYPWBVKXJQZ",
         SUBSTRING(name,i,1)) > 0.
END.

RETURN.
