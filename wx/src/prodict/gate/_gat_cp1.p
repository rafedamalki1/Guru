/*************************************************************/
/* Copyright (c) 1984-1995 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/*--------------------------------------------------------------------

File: prodict/gate/_gat_cp1.i

Description:
    
    assignes the new code-page to the _Db-record; and, to ensure
    the changes in the initial-values, assigns "" to them in every
    field 
            
Input-Parameters:
    none

Output-Parameters:
    none
    
Used/Modified Shared Objects:
    user_env[5]     code-page                            if new-value
    
Author: Tom Hutegger

History:
    hutegger    94/04/21    creation
    
--------------------------------------------------------------------*/        
/*h-*/

/*----------------------------  DEFINES  ---------------------------*/

{ prodict/dictvar.i }
{ prodict/user/uservar.i }

form
  " Applying the changes to the schema holder. " skip
  "                            Please wait ... "
  with overlay centered row 5
  frame msg.
  
/*---------------------------  TRIGGERS  ---------------------------*/

/*------------------------  INT.-PROCEDURES  -----------------------*/

/*---------------------------  MAIN-CODE  --------------------------*/

find _Db
  where _Db._Db-name = user_dbname
  no-error.
if not available _Db 
 then do:  /* */
  /* should actually never be */
  end.     /* */

 else do:  /* _db available */
  
  run adecomm/_setcurs.p ("WAIT").
  assign SESSION:IMMEDIATE-DISPLAY = yes.
  view frame msg.

  if ( _DB._Db-type = "SYB10" or _DB._Db-type = "MSSQLSRV")
    then do:
        { prodict/gate/gat_cp1a.i 
              &incpname = "user_env[5]" }
    end.
  else assign _Db._Db-xl-name = user_env[5].

  if  _Db._Db-xl-name =  ?
   or _Db._Db-xl-name = "?"
   then do:
    find first _File of _Db no-error.
    if available _File
     then find first _Field of _File no-error.
    if available _Field
     then assign _Field._Initial = _Field._Initial + "".
    end.
   else for each _File of _Db:
    for each _Field of _File:
      assign _Field._initial = _Field._initial + "".
      end.
    end.
    
  hide frame msg no-pause.
  run adecomm/_setcurs.p ("").
  
  end.     /* _db available */
      
/*------------------------------------------------------------------*/













