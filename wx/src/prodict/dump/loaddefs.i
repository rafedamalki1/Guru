/*************************************************************/
/* Copyright (c) 1984-1993 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/* loaddefs.i - definitions for load .df file */

DEFINE {1} SHARED VARIABLE iarg AS CHARACTER NO-UNDO. /* usually = ilin[2] */
DEFINE {1} SHARED VARIABLE ikwd AS CHARACTER NO-UNDO. /* usually = ilin[1] */
DEFINE {1} SHARED VARIABLE imod AS CHARACTER NO-UNDO. /* add/mod/ren/del */
DEFINE {1} SHARED VARIABLE ipos	AS INTEGER   NO-UNDO. /* line# in file */
DEFINE {1} SHARED VARIABLE ilin AS CHARACTER EXTENT 256 NO-UNDO.

DEFINE {1} SHARED VARIABLE iprimary   AS LOGICAL   NO-UNDO. /* is prim idx */
DEFINE {1} SHARED VARIABLE irename    AS CHARACTER NO-UNDO. /* new name */
DEFINE {1} SHARED VARIABLE icomponent AS INTEGER   NO-UNDO. /* idx-fld seq # */

DEFINE {1} SHARED VARIABLE inoerror AS LOGICAL   NO-UNDO. /* no-error seen? */
DEFINE {1} SHARED VARIABLE ierror   AS INTEGER   NO-UNDO. /* error counter */


DEFINE {1} SHARED WORKFILE wdbs NO-UNDO LIKE _Db.
DEFINE {1} SHARED WORKFILE wfil NO-UNDO LIKE _File.
DEFINE {1} SHARED WORKFILE wfit NO-UNDO LIKE _File-trig.
DEFINE {1} SHARED WORKFILE wfld NO-UNDO LIKE _Field.
DEFINE {1} SHARED WORKFILE wflt NO-UNDO LIKE _Field-trig.
DEFINE {1} SHARED WORKFILE widx NO-UNDO LIKE _Index.
DEFINE {1} SHARED WORKFILE wixf NO-UNDO LIKE _Index-field.
DEFINE {1} SHARED WORKFILE wseq NO-UNDO LIKE _Sequence.

/* gate_xxx - for lookup of _for-type -> _fld-stdtype converions */
DEFINE {1} SHARED VARIABLE gate_dbtype AS CHARACTER NO-UNDO.
DEFINE {1} SHARED VARIABLE gate_proc   AS CHARACTER NO-UNDO. /* xxx_typ.p */

/* dblangcache - list of sql files - _File._Db-lang -> 1 */
DEFINE {1} SHARED VARIABLE dblangcache AS CHARACTER NO-UNDO.

/* kindexcache - list of index names deleted when fields deleted */
DEFINE {1} SHARED VARIABLE kindexcache AS CHARACTER NO-UNDO.

/* frozencache - list of files to be marked frozen */
DEFINE {1} SHARED VARIABLE frozencache AS CHARACTER NO-UNDO.

