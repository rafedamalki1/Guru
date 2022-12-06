/*************************************************************/
/* Copyright (c) 1984-1995 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/* _gat_con.p - connect to Oracle, Rdb or Sybase using -U & -P */
/*
in:  user_env[1] = userid
     user_env[2] = password

out: no environment variables changed
*/


{ prodict/dictvar.i }
{ prodict/user/uservar.i }
{ prodict/user/userhue.i }
{ prodict/user/userhdr.f }

DEFINE VARIABLE phynam AS CHARACTER NO-UNDO.
DEFINE VARIABLE c      AS CHARACTER NO-UNDO.

phynam = user_dbname.
{ prodict/dictgate.i &action=query &dbtype=user_dbname &dbrec=? &output=c }
IF ENTRY(5,c) MATCHES "*p*" THEN DO: /* physical name applies */
  FIND _Db WHERE RECID(_Db) = drec_db.
  phynam = (IF _Db-addr = "" OR _Db-addr = ? THEN user_dbname ELSE _Db-addr).
  /* Connect w' physical dbname if avail, otherwise assume logical dbname */
END.

IF user_env[1] <> "" OR NOT CONNECTED(user_dbname)
  THEN DO ON ERROR UNDO,LEAVE ON ENDKEY UNDO,LEAVE:
    MESSAGE
      'Connecting to "' + user_dbname + '"'
      (IF user_env[1] = "" THEN "" ELSE 'as user "' + user_env[1] + '"').
    run adecomm/_setcurs.p ("WAIT").
    IF CONNECTED(user_dbname) THEN DISCONNECT VALUE(user_dbname).
    CONNECT
      VALUE(phynam)
      -ld VALUE(user_dbname)
      VALUE(IF user_env[1] = "" THEN "" ELSE "-U " + user_env[1])
      VALUE(IF user_env[2] = "" THEN "" ELSE "-P " + user_env[2])
      -dt VALUE(user_dbtype) NO-ERROR.
  END.

{ prodict/user/usercon.i }

PAUSE 1 NO-MESSAGE.  /* to avoid having the message flash to fast */
run adecomm/_setcurs.p ("").
HIDE MESSAGE NO-PAUSE.

IF NOT CONNECTED(user_dbname)
 THEN DO:
  MESSAGE
    'Could not connect to "' + user_dbname + '"' 
    (IF user_env[1] = "" THEN "" ELSE 'as user "' + user_env[1] + '"') SKIP
    ERROR-STATUS:GET-MESSAGE(1)
    VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  user_path = "".
END.
ELSE
  CREATE ALIAS "DICTDBG" FOR DATABASE VALUE(user_dbname) NO-ERROR.

RETURN.




