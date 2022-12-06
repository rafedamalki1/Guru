/*************************************************************/
/* Copyright (c) 1984-1998 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/* _ora_md3 - returns list of Oracle OBJ$ in gate-work */

/*
This is used primarily for the Progress automated test
scripts, but it was reasoned that it is useful enough in
general to leave in the /prodict directory for future use.
*/

{ prodict/dictvar.i    }
{ prodict/ora/oravar.i }
{ prodict/user/uservar.i }

{prodict/gate/gatework.i
  &new        = " "
  &options    = "initial ""*"" "
  &SelVarType = "VARIABLE l"
  } /* Defines WORKFILE: gate-work */

DEFINE VARIABLE object-type AS CHARACTER NO-UNDO. 
DEFINE VARIABLE object-prog AS CHARACTER NO-UNDO. 

FOR EACH DICTDBG.oracle_objects NO-LOCK,
  EACH DICTDBG.oracle_users
    WHERE DICTDBG.oracle_users.user# = DICTDBG.oracle_objects.owner# NO-LOCK
    BY DICTDBG.oracle_users.name BY DICTDBG.oracle_objects.name:


  /*
  ** Skip objects that we didn't create and/or don't own.
  */ 

   IF (DICTDBG.oracle_objects.type <> 2 AND
       DICTDBG.oracle_objects.type <> 6 AND
       DICTDBG.oracle_objects.type <> 7 AND
       DICTDBG.oracle_objects.type <> 8 AND
       DICTDBG.oracle_objects.type <> 9) 
     THEN NEXT.
 
  IF DICTDBG.oracle_users.name <> CAPS(ora_username) THEN
      NEXT. 

  /* Skip sequence generators created for compatible oracle tables */

  IF DICTDBG.oracle_objects.name MATCHES "*_SEQ" THEN
    NEXT.

  IF DICTDBG.oracle_objects.type = 2 OR 
     DICTDBG.oracle_objects.type = 7 OR
     DICTDBG.oracle_objects.type = 8 OR
     DICTDBG.oracle_objects.type = 9 THEN
      object-type = "TABLE".
  ELSE 
      object-type = "SEQUENCE".

  object-prog = DICTDBG.oracle_objects.name.
  RUN prodict/gate/_gat_xlt.p (TRUE,drec_db,INPUT-OUTPUT object-prog).

  create gate-work. 

  ASSIGN
      gate-work.gate-type = object-type
      gate-work.gate-name = DICTDBG.oracle_objects.name
      gate-work.gate-qual = ?
      gate-work.gate-user = DICTDBG.oracle_users.name
      gate-work.gate-prog = object-prog. 

END.

RETURN.
