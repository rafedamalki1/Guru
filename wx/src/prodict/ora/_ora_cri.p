/*************************************************************/
/* Copyright (c) 1984-1998 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/* _ora_cri - base indexes for oracle meta-schema 

   Modified:  DLM 12/29/97 Added check for Oracle Version
*/

DEFINE INPUT PARAMETER dbkey AS RECID NO-UNDO.

FIND _Db WHERE RECID(_Db) = dbkey NO-LOCK NO-ERROR.

FIND _File
  WHERE _File._Db-recid = dbkey
    AND _File._File-name = "oracle_indexes" NO-ERROR.
IF AVAILABLE _File THEN RETURN.

CREATE _File.
ASSIGN
  _File._Db-recid    = dbkey
  _File._File-name   = "oracle_indexes"
  _File._For-Type    = "TABLE"
  _File._For-Name    = "ind$"
  _File._For-Owner   = "sys"
  _File._Last-change = 2146431
  _File._Hidden      = TRUE.

CREATE _Field. /* file: ind$ */
ASSIGN
  _Field._File-recid   = RECID(_File)
  _Field._Field-Name   = "OBJ#"
  _Field._Data-Type    = "integer"
  _Field._Initial      = ?
  _Field._Mandatory    = yes
  _Field._Format       = "->>>>>>>>>9"
  _Field._Order        = 10
  _Field._Fld-stdtype  = 8192
  _Field._Fld-stoff    = 1
  _Field._For-Name = "OBJ#"
  _Field._For-Type = "number".

CREATE _Field. /* file: ind$ */
ASSIGN
  _Field._File-recid   = RECID(_File)
  _Field._Field-Name   = "BO#"
  _Field._Data-Type    = "integer"
  _Field._Initial      = ?
  _Field._Mandatory    = yes
  _Field._Format       = "->>>>>>>>>9"
  _Field._Order        = 20
  _Field._Fld-stdtype  = 8192
  _Field._Fld-stoff    = 2
  _Field._For-Name = "BO#"
  _Field._For-Type = "number".

CREATE _Field. /* file: ind$ */
ASSIGN
  _Field._File-recid   = RECID(_File)
  _Field._Field-Name   = "UNIQUE$"
  _Field._Data-Type    = "integer"
  _Field._Initial      = ?
  _Field._Mandatory    = yes
  _Field._Format       = "->>>>>>>>>9"
  _Field._Order        = 30
  _Field._Fld-stdtype  = 8192
  _Field._Fld-stoff    = 3
  _Field._For-Name = (IF _Db._Db-misc1[3] = 7 THEN "UNIQUE$"
                             ELSE "PROPERTY")
  _Field._For-Type = "number".

RETURN.
