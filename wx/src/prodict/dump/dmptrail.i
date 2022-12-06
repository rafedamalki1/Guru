/*************************************************************/
/* Copyright (c) 1984-1997 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/* dmptrail.i

function:
    appends a trailer with codepage-information 

preconditions:    
    needs the stream to be open, and all data to be output
    user_env[5] contains the value for the codepage-entry
        
text-parameters:
    &entries        ev. additional entries
    &seek-stream    "<stream-name>"        or "OUTPUT"
    &stream         "stream <stream-name>" or ""
    
included in:
  prodict/dump/_dmpdata.p    
  prodict/dump/_dmpsddl.p    
  prodict/dump/_dmpseqs.p    
  prodict/dump/_dmpuser.p    
  prodict/dump/_dmpview.p    
  prodict/dump/_dmpincr.p    
    
history:
    hutegger    94/03/02    creation
    
*/
/*------------------ begin Trailer-INFO ------------------*/

  PUT {&stream} UNFORMATTED "." SKIP.
  
  i = SEEK({&seek-stream}).
  
  PUT {&stream} UNFORMATTED "PSC" SKIP.
  
  {&entries}
  
  PUT {&stream} UNFORMATTED "codepage=" 
    ( if user_env[5] = "<internal defaults apply>"
       then "UNDEFINED"
       else user_env[5] 
     )                                          SKIP.
   
  PUT {&stream} UNFORMATTED
    "." SKIP
      STRING(i,"9999999999") SKIP. /* location of trailer */

/*------------------ end   Trailer-INFO ------------------*/

