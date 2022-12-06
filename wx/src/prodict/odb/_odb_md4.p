/*************************************************************/
/* Copyright (c) 1984-1994 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

{ prodict/dictvar.i }
{ prodict/user/uservar.i }
{ prodict/odb/odbvar.i }  /* only temp */

DEFINE VARIABLE type_delimiter AS INTEGER NO-UNDO.
DEFINE VARIABLE name_delimiter AS INTEGER NO-UNDO.
DEFINE VARIABLE odb_type AS CHARACTER NO-UNDO.
DEFINE VARIABLE odb_name AS CHARACTER NO-UNDO.
DEFINE VARIABLE odb_owner AS CHARACTER NO-UNDO.

ASSIGN
  type_delimiter = INDEX(user_filename,".")
  odb_type       = SUBSTRING(user_filename,1,type_delimiter - 1)
  odb_owner      = SUBSTRING(user_filename,type_delimiter + 1)
  name_delimiter = INDEX(odb_owner,".")
  odb_name       = SUBSTR(odb_owner,name_delimiter + 1)
  odb_owner      = SUBSTR(odb_owner,1,name_delimiter - 1)
  user_env[4]    = odb_name.

RUN prodict/gate/_gat_xlt.p (TRUE,drec_db,INPUT-OUTPUT user_env[4]).

ASSIGN
  user_env[1]   = odb_name
  user_env[2]   = odb_type
  user_env[3]   = odb_owner
  user_env[5]   = ""
  user_filename = "".

RUN prodict/odb/_odb_mak.p. /* bring over definitions for the indicated file */

RETURN.
