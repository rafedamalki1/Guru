/*************************************************************/
/* Copyright (c) 1984-1994 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/*--------------------------------------------------------------------

File: prodict/gate/gat-trgm.i

Description:
    
    the variable trig-reass gets set in the delete-triggers (see:
    prodict/gate/gat-trig.i).
    
    If its value is true we display a message here.
            
Text-Parameters:
    none

    
Included in:
    odb/_odb_mak.p
    ora/_ora6mak.p
    ora/_ora7mak.p
    rdb/_rdb_mak.p
    syb/_syb_mak.p
    
Author: Tom Hutegger

History:
    hutegger    94/02/09    creation
    
--------------------------------------------------------------------*/        
/*h-*/

if trig-reass = true
 then do:   /* there are warnings or messages */
       
        &IF "{&WINDOW-SYSTEM}" = "TTY" 
         &THEN 
          message err-msg[6]. 
         &ELSE
          message err-msg[6] view-as alert-box warning buttons ok.
         &ENDIF         
  
  end.      /* there are warnings or messages */

/*------------------------------------------------------------------*/        
