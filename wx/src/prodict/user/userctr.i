/*************************************************************/
/* Copyright (c) 1984-1993 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/* userctr.i - dictionary user interface code to center frame--use this
   only when the frame is pretty large--TTY centering algorithm can put
   the frame just a hair below the menubar, which looks non-optimal.
   This code will move it down, so that the frame partially obscures
   the message line and status line--this looks better than being
   cheek-to-jowl with the menubar.  For shorter frames, the normal
   TTY centering algorithm works better. */

/**
** PARAMETERS:
** &FRAME    widget name of the frame to be centered
**/

&IF "{&WINDOW-SYSTEM}" = "TTY" &THEN
    {&FRAME}:ROW = 2 + (CURRENT-WINDOW:HEIGHT - {&FRAME}:HEIGHT) / 2.
&ENDIF

