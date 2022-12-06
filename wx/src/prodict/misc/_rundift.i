/*************************************************************/
/* Copyright (c) 1984-1996 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

DEFINE SHARED VARIABLE totrecs AS INTEGER INITIAL 0 NO-UNDO.
DISABLE TRIGGERS FOR DUMP OF DICTDB2.{1}.
FOR EACH DICTDB2.{1} {2}:
 totrecs = totrecs + 1.
END.
