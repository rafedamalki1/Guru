/*************************************************************/
/* Copyright (c) 1984-1994 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/*----------------------------------------------------------------------------

File: qviwrpt.p

Description:
   Quick and dirty view report for both the GUI and character dictionary.

Input Parameters:
   p_DbId    - Id of the _Db record corresponding to the current database
   p_PName   - Physical name of the database
   p_DbType  - Database type (e.g., PROGRESS)
   p_View    - The name of the View to report on or "ALL".

Author: Tony Lavinio, Laura Stern

Date Created: 10/07/92

Modified on 06/14/94 by Gerry Seidl. Added NO-LOCKs to file accesses.
----------------------------------------------------------------------------*/

{adecomm/commeng.i}  /* Help contexts */

DEFINE INPUT PARAMETER p_DbId 	 AS RECID NO-UNDO.
DEFINE INPUT PARAMETER p_PName 	 AS CHAR  NO-UNDO.
DEFINE INPUT PARAMETER p_DbType  AS CHAR  NO-UNDO.
DEFINE INPUT PARAMETER p_View 	 AS CHAR  NO-UNDO.

DEFINE VAR header_str AS CHAR 	 NO-UNDO.
DEFINE VAR msg#       AS INTEGER NO-UNDO.

DEFINE VARIABLE msgs AS CHARACTER EXTENT 2 NO-UNDO INITIAL [
  /* 1*/ "There are no views in this database to look at.",
  /* 2*/ "You do not have permission to use this option."
].

DO FOR _File,_View:
  FIND _File "_View" NO-LOCK.
  FIND FIRST _View NO-LOCK NO-ERROR.
  msg# = (IF NOT AVAILABLE _View THEN 1 ELSE
          IF NOT CAN-DO(_File._Can-read,USERID("DICTDB")) THEN 2 ELSE 0).
  IF msg# > 0 THEN DO:
    MESSAGE msgs[msg#] VIEW-AS ALERT-BOX ERROR BUTTONS OK.
    RETURN.
  END.
END.

header_str = "Database: " + p_PName + " (" + p_DbType + ")".

RUN adecomm/_report.p 
   (INPUT p_DbId, 
    INPUT header_str,
    INPUT "View Report",
    INPUT "",
    INPUT "",
    INPUT "adecomm/_qviwdat.p",
    INPUT p_View,
    INPUT {&SQL_View_Report}).

/* _qviwrpt.p - end of file */

