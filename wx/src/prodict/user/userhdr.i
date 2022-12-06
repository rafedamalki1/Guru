/*************************************************************/
/* Copyright (c) 1984-1993 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/


&IF "{&WINDOW-SYSTEM}" = "TTY" &THEN
  ASSIGN
    user_hdr = {1}.
  DISPLAY user_hdr WITH FRAME user_hdr.
&ENDIF
