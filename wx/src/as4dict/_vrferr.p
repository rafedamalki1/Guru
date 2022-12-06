/*************************************************************/
/* Copyright (c) 1984-1995 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/*----------------------------------------------------------------------------

File: _vrferr.p

Description:
   Output to file of errors received during running of as4)vrfy.p.
 
 Created 5/18/95  D. McMann
 
 ------------------------------------------------------------------------------- */
 
 { prodict/user/uservar.i }
 { as4dict/usersho.i }
 
  DEFINE VARIABLE i AS INTEGER NO-UNDO.         
  DEFINE VARIABLE ttl AS CHARACTER FORMAT "x(30)" INITIAL
        "Listing of anomalies found on" NO-UNDO.
  
 FORM HEADER 
        SPACE (5)  ttl  TODAY SKIP (1)
        WITH FRAME hlist CENTERED NO-BOX NO-LABEL.
  
  FORM
    sho_pages[i] SKIP
    WITH FRAME lst CENTERED DOWN STREAM-IO NO-BOX NO-LABEL.
    
 OUTPUT TO VALUE(user_env[8]).            
 VIEW FRAME hlist.           

 DO i = 1 TO sho_limit WITH FRAME lst:   
    DISPLAY sho_pages[i] FORMAT "x(65)".
    DOWN.
  END.           

  OUTPUT CLOSE.   
 
    
         
 