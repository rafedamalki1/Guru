/*----------------------------------------------------------------------------

File: _gttblid.p

Description:
   This will return the id of a table.  DICTDB must be pointing to the
   correct database.

Input Parameters:
   p_DBID    - recid of the database this table is in. -- REQUIRED
   p_TblName - This is the name of the table you want to get an id for.

Output Parameters:
   p_TblID  - The recid of the _File record.

Author: Warren Bare

Date Created: 06/30/92 

----------------------------------------------------------------------------*/
/*Copyright (c) by PROGRESS SOFTWARE CORPORATION. 1992 - AllRights Reserved.*/


Define INPUT  PARAMETER p_DBID      as recid    NO-UNDO.
Define INPUT  PARAMETER p_TblName   as char     NO-UNDO.
Define OUTPUT PARAMETER p_TblID     as recid    NO-UNDO init ?.

Find dictdb._file WHERE _file._DB-recid = p_DBID AND
  _file-name = p_TblName NO-LOCK NO-ERROR.
if not available _file THEN
DO:
  /* This should not happen if you pass in a good p_dbid */
  IF NOT CAN-FIND(_db WHERE RECID(_db) = p_dbid) THEN
  MESSAGE SUBSTITUTE("_db record &1 does not exist in Database &2."
    ,p_dbid,ldbname("DICTDB"))
    view-as alert-box error buttons ok.

  MESSAGE SUBSTITUTE("Table &1 is no longer available.",p_TblName)
    view-as alert-box error buttons ok.
  RETURN.
END.
else
  p_TblID = RECID(_file).

RETURN.
