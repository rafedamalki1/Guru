/*************************************************************/
/* Copyright (c) 1984-1996 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from Progress Software Corporation. */
/*************************************************************/
/* File:_as4_del.p

Description:
    This procedure deletes a DB2/400 DataServer Schema from the
    client.

History:
     Created  01/25/95  D. McMann
     Modified 03/19/96  D. McMann fixing bugs 95-12-21-026 and 96-01-31-025
                        added _usrsget to user_path when redo.
*/


/*===========================  Main Line Code  ===========================*/
{ prodict/dictvar.i }
{ prodict/user/uservar.i }
{ prodict/user/userhue.i }
{ prodict/user/userhdr.f }


SESSION:IMMEDIATE-DISPLAY = TRUE.

DEFINE VARIABLE answer AS LOGICAL INITIAL FALSE NO-UNDO.     
DEFINE VARIABLE dbhold  AS CHARACTER NO-UNDO.

FIND _db where _db._db-name = user_dbname.                         

IF _Db._Db-misc1[8] <> 7 THEN DO:
     MESSAGE "This utility can only be used against a V7 Server."
        VIEW-AS ALERT-BOX ERROR BUTTON OK.
      ASSIGN user_path = "".
 END.
        
ASSIGN dbhold = _Db._Db-name + "?".
IF user_env[1] =  "redo" THEN   answer = true.
 ELSE   MESSAGE "Are you sure you want to delete DB2/400 " SKIP
        "DataServer schema for"  dbhold SKIP
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE answer. 

/*  Database must be disconnect in order to delete _db record */
IF answer AND CONNECTED(user_dbname) THEN 
   DISCONNECT VALUE(_Db._Db-name).

     
IF answer THEN DO:   
  run adecomm/_setcurs.p ("WAIT").
  RUN as4dict/as4deldb.p.    
  run adecomm/_setcurs.p ("").  
END.
