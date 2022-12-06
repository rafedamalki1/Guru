/*************************************************************/
/* Copyright (c) 1984-1998 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/* _ora_cru - base users for oracle meta-schema 


*/

DEFINE INPUT PARAMETER dbkey AS RECID NO-UNDO.

FIND _File
  WHERE _File._Db-recid = dbkey
    AND _File._File-name = "oracle_users" NO-ERROR.
IF AVAILABLE _File THEN RETURN.

CREATE _File.
ASSIGN
  _File._Db-recid    = dbkey
  _File._File-name   = "oracle_users"
  _File._For-Type = "TABLE"
  _File._For-Name = "user$"
  _File._For-Owner = "sys"
  _File._Last-change = 2146431
  _File._Hidden      = TRUE.

CREATE _Field. /* file: user$ */
ASSIGN
  _Field._File-recid   = RECID(_File)
  _Field._Field-Name   = "USER#"
  _Field._Data-Type    = "integer"
  _Field._Initial      = ?
  _Field._Mandatory    = yes
  _Field._Format       = "->>>>>>>>>9"
  _Field._Order        = 10
  _Field._Fld-stdtype  = 8192
  _Field._Fld-stoff    = 1
  _Field._For-Name = "USER#"
  _Field._For-Type = "number".

CREATE _Field. /* file: user$ */
ASSIGN
  _Field._File-recid   = RECID(_File)
  _Field._Field-Name   = "NAME"
  _Field._Data-Type    = "character"
  _Field._Initial      = ?
  _Field._Mandatory    = yes
  _Field._Format       = "x(30)"
  _Field._Decimals     = 30
  _Field._Order        = 20
  _Field._Fld-stdtype  = 4096
  _Field._Fld-stoff    = 2
  _Field._For-Name = "NAME"
  _Field._For-Type = "char".

RETURN.
