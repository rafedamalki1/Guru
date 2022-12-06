/*************************************************************/
/* Copyright (c) 1984-1995 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from Progress Software Corporation. */
/*************************************************************/

/* as4recon.p */

/* This file is run from _as4_sync.p when a code page change has occurred
   during synchronization.  It asks the user if they want to reconnect
   to the database that was disconnected.
   
   Created 01/24/95 D McMann
   
*/
  
DEFINE VARIABLE answer AS LOGICAL INITIAL FALSE NO-UNDO.

{ prodict/dictvar.i }
{ prodict/user/uservar.i }
{ prodict/user/userhue.i }
{ prodict/user/userhdr.f }

find _db where _db._Db-name = user_env[35] NO-ERROR.

IF AVAILABLE _db AND NOT CONNECTED(_Db._Db-name) THEN DO ON ERROR UNDO,LEAVE ON ENDKEY UNDO,LEAVE:
 
  MESSAGE _Db._Db-name "was disconnected because the " SKIP
          "code page changed during synchronization. " SKIP
          "Do you want to re-connected? "
          VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE answer.
  IF answer THEN DO:
    MESSAGE
      'Connecting to "' + _Db._Db-name  + '"'.
    run adecomm/_setcurs.p ("WAIT").
    CONNECT VALUE(_db._Db-name) -dt VALUE(_db._Db-type) VALUE(_db._Db-comm) NO-ERROR.

    PAUSE 1 NO-MESSAGE.  /* to avoid having the message flash to fast */  
    { prodict/user/usercon.i '' @ user_filename }
    RUN adecomm/_setcurs.p ("").
    HIDE MESSAGE NO-PAUSE.
  END.
  ELSE DO:   
    ASSIGN user_env[1] = "get".   
    RUN prodict/gui/_guisget.p.
  END.
END.
 
