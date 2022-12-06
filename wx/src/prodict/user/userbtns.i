/*************************************************************/
/* Copyright (c) 1984-1994 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/*----------------------------------------------------------------------------

File: userbtns.i

Description:   
   Adds an OK and Cancel button and the rectangle that underlies them,
   to a frame.  

Text-Parameters:
    &OTHER      other buttons for both TTY AND GUI frames
    &OTHERGUI   other buttons for GUI frames ONLY
    &OTHERTTY   other buttons for TTY frames ONLY
    
Author: Laura Stern

Date Created: 11/17/92 

Modified:
    hutegger    94-09-12    added text-parameters which can get omitted
    
----------------------------------------------------------------------------*/

&IF "{&WINDOW-SYSTEM}" <> "TTY" &THEN 
   {adecomm/okform.i
      &BOX    = rect_btns
      &STATUS = no
      &OK     = btn_OK
      &CANCEL = btn_Cancel
      &OTHER  = " {&OTHER}{&OTHERGUI}"
      &HELP   = btn_Help}
&ELSE
   {adecomm/okform.i
      &STATUS = no
      &OK     = btn_OK
      &CANCEL = btn_Cancel
      &OTHER  = " {&OTHER}{&OTHERTTY}"}
&ENDIF

