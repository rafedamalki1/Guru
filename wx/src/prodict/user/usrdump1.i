/*************************************************************/
/* Copyright (c) 1984-1994 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/* usrdump1.i   

function:
     on leave - trigger for field user_env[5] (code-page)
   
text-parameters:
    &frame          frame containing the field
    &variable       name of the field
    
included in:
  prodict/user/_usrdump.p    
  prodict/user/_usrincr.p    
    
history:
    hutegger    94/03/02    creation
    
*/
/*------------------ begin Trailer-INFO ------------------*/

ON LEAVE OF {&variable} in frame {&frame} do:    

  if {&variable}:screen-value in frame {&frame} = "?" 
   then assign {&variable}:screen-value in frame {&frame} = "<internal defaults apply>".

   else do:
    assign
      {&variable} = TRIM({&variable}:screen-value in frame {&frame})
      {&variable}:screen-value in frame {&frame} = {&variable}.
    if codepage-convert("a",{&variable},SESSION:CHARSET) = ? 
     then RETURN NO-APPLY. /* conversion not possible */
    end.      /* {&variable} <> ? */

   end.         /* leave of {&variable} in frame {&frame} */

/*--------------------------------------------------------*/

