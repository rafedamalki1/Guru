/*************************************************************/
/* Copyright (c) 1984-1995 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/*----------------------------------------------------------------------------

File: forceval.i

Description:
   This is to enforce validation when the user hits a default button
   (other than an AUTO-GO button) by hitting RETURN or "presses" a button
   by using it's keyboard accelerator.  In these cases, Progress does
   not generate any LEAVE events, so we have to do it ourselves.

Author: Laura Stern

Date Created: 05/25/92 
----------------------------------------------------------------------------*/


s_Widget = focus:handle.
if s_Widget <> SELF:handle then 
do:
   s_Valid = yes.
   apply "LEAVE" to s_Widget.
   if NOT s_Valid then
      return NO-APPLY.
end.
