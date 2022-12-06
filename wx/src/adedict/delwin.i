/*************************************************************/
/* Copyright (c) 1984-1993 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/*----------------------------------------------------------------------------

File: delwin.i

Description:
   Close the given property window.
 
Argument
   &Win - handle of window to close.
   &Obj - the object # for this object

Author: Laura Stern

Date Created: 05/13/92 

----------------------------------------------------------------------------*/

if {&Win} <> ? then
do:
   assign
      s_x_Win[{&Obj}] = {&Win}:x
      s_y_Win[{&Obj}] = {&Win}:y.
   
   delete widget {&Win}.
   {&Win} = ?.
end.

