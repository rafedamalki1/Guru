/*************************************************************/
/* Copyright (c) 1984-1994 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/* _dmpuser.p   */

/*
in:  user_env[2] = Name of file to dump to.
     user_env[5] = "<internal defaults apply>" or "<target-code-page>"

History:
    hutegger    94/02/24    code-page - support and trailer-info added
    
*/
/*h-*/

{ prodict/user/uservar.i }

DEFINE VARIABLE i       AS INTEGER   NO-UNDO.

IF NOT CAN-FIND(FIRST DICTDB._User) THEN DO:
   MESSAGE "There are no user records to dump." SKIP
      	   "The output file has not been modified."
      	    VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
   RETURN.
END.

run adecomm/_setcurs.p ("WAIT").

IF  user_env[5] = ?
 OR user_env[5] = "" THEN assign user_env[5] = "<internal defaults apply>".

IF  user_env[5] = "<internal defaults apply>" 
 THEN OUTPUT TO VALUE(user_env[2]) NO-ECHO NO-MAP NO-CONVERT.
 ELSE OUTPUT TO VALUE(user_env[2]) NO-ECHO NO-MAP
            CONVERT SOURCE SESSION:CHARSET TARGET user_env[5].

FOR EACH DICTDB._User:
  EXPORT DICTDB._User.
END.

  {prodict/dump/dmptrail.i
    &entries      = " "
    &seek-stream  = "OUTPUT"
    &stream       = " "
    }  /* adds trailer with code-page-entrie to end of file */
    
OUTPUT CLOSE.
run adecomm/_setcurs.p ("").
MESSAGE "Dump of users completed." 
        VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
RETURN.
