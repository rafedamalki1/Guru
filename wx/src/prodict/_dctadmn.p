/*************************************************************/
/* Copyright (c) 1984-1993 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/*
To be a Security Administrator, you must have:

Write  permission on _File._Can-read
Write  permission on _File._Can-write
Write  permission on _File._Can-create
Write  permission on _File._Can-delete
Write  permission on _Field._Can-read
Write  permission on _Field._Can-write
Create permission on _User
Delete permission on _User
*/

DEFINE INPUT  PARAMETER usrn AS CHARACTER            NO-UNDO.
DEFINE OUTPUT PARAMETER okay AS LOGICAL INITIAL TRUE NO-UNDO.

/*check runtime privileges*/
FIND DICTDB._File "_File".
FIND DICTDB._Field "_Can-read" OF _File.
IF NOT CAN-DO(_Field._Can-write,usrn) THEN okay = FALSE.
FIND DICTDB._Field "_Can-write" OF _File.
IF NOT CAN-DO(_Field._Can-write,usrn) THEN okay = FALSE.
FIND DICTDB._Field "_Can-create" OF _File.
IF NOT CAN-DO(_Field._Can-write,usrn) THEN okay = FALSE.
FIND DICTDB._Field "_Can-delete" OF _File.
IF NOT CAN-DO(_Field._Can-write,usrn) THEN okay = FALSE.

FIND DICTDB._File "_Field".
FIND DICTDB._Field "_Can-read" OF _File.
IF NOT CAN-DO(_Field._Can-write,usrn) THEN okay = FALSE.
FIND DICTDB._Field "_Can-write" OF _File.
IF NOT CAN-DO(_Field._Can-write,usrn) THEN okay = FALSE.

FIND DICTDB._File "_User".
IF NOT CAN-DO(_File._Can-create,usrn)
  OR NOT CAN-DO(_File._Can-delete,usrn) THEN okay = FALSE.

/*
IF NOT okay THEN DO:
  MESSAGE "You must be a Security Administrator to execute this function.".
  PAUSE.
END.
*/
RETURN.
