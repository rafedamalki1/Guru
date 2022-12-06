/*************************************************************/
/* Copyright (c) 1984-1993 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/* used by ???_dmp.p to dump structure and omit blank lines */

DO i = 1 TO 30:
 PUT STREAM ddl UNFORMATTED ddl[i] SKIP.
END.
