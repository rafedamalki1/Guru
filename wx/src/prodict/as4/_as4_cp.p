/****************************************************************/
/*  Copyright (c) 1984 - 1994 by Progress Software Corporation  */
/*                                                              */
/*  All rights reserved.  No part of this program or document   */
/*  may be reporduced in any form or by any means without       */
/*  permission in writing from PROGRESS Software Corporation.   */
/****************************************************************/

/* -----------------------------------------------------------------------------

File:  _as4_cp.p 

Description:
  This procedure verifies that _db._db-xl-name has been set for the
  AS400 schema just loaded.
  
Author:  Donna L. McMann

Date Created:  05/05/94

----------------------------------------------------------------------------- */

{ prodict/user/uservar.i }

DEFINE VARIABLE codepage      AS CHARACTER.


FIND FIRST _Db WHERE _Db._Db-name = user_dbname
                 AND _Db._db-type = "AS400" 
                 AND _Db._db-xl-name = ? 
                 EXCLUSIVE-LOCK NO-ERROR.
IF AVAILABLE _Db THEN DO:

  /* Set default code-page according to as4/_as4_sys.p */
  { prodict/dictgate.i 
    &action = query 
    &dbtype = _Db._db-type  
    &dbrec  = ? 
    &output = codepage 
    }
  assign _Db._db-xl-name = ENTRY(11,codepage).
  
  /* "Touch" each field of each file to cause the
     template records to be rebuilt using the DB
     codepage. */                               
     
  FOR EACH _File OF _Db:
    FOR EACH _Field OF _File:
      ASSIGN _Field._initial = _Field._initial + "".
    END.
  END.
  
  HIDE MESSAGE NO-PAUSE.
  MESSAGE "The Code-Page for Database" _db._db-name SKIP
          "has been set to default value of" _db._db-xl-name
          VIEW-AS ALERT-BOX INFORMATION BUTTON OK.
END.

RETURN.
