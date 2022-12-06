/*************************************************************/
/* Copyright (c) 1984-1994 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/*----------------------------------------------------------------------------

File: trelrpt.p

Description:
   Invoke table relations report for both the GUI and Char dictionaries.
 
Input Parameters:
   p_DbId    - Id of the _Db record corresponding to the current database
   p_PName   - Physical name of the database
   p_DbType  - Database type (e.g., PROGRESS)
   p_TblName - Name of the table or "ALL".
   p_Btns    - Character string indicating which optional buttons
      	       should be visible.  These include "s" for switch files.
      	       So this could be "" OR "s"

Author: Tony Lavinio, Laura Stern

Date Created: 10/12/92

Modified on 06/14/94 by Gerry Seidl. Added NO-LOCKs to file accesses.
----------------------------------------------------------------------------*/

{adecomm/commeng.i}  /* Help contexts */

DEFINE INPUT PARAMETER p_DbId    AS RECID NO-UNDO.
DEFINE INPUT PARAMETER p_PName 	 AS CHAR  NO-UNDO.
DEFINE INPUT PARAMETER p_DbType  AS CHAR  NO-UNDO.
DEFINE INPUT PARAMETER p_TblName AS CHAR  NO-UNDO.
DEFINE INPUT PARAMETER p_Btns    AS CHAR  NO-UNDO.

DEFINE VAR header_str AS CHAR 	 NO-UNDO.
DEFINE VAR permit     AS LOGICAL NO-UNDO.

FIND _File "_File" NO-LOCK.
permit = CAN-DO(_Can-read,USERID("DICTDB")).
FIND _File "_Index" NO-LOCK.
permit = permit AND CAN-DO(_Can-read,USERID("DICTDB")).
FIND _File "_Field" NO-LOCK.
permit = permit AND CAN-DO(_Can-read,USERID("DICTDB")).
IF NOT permit THEN DO:
  MESSAGE "You do not have permission to use this option"
    VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  RETURN.
END.

header_str = "Database: " + p_PName + " (" + p_DbType + ")" +
      	     STRING("", "x(8)") + "Table: " + p_TblName.

RUN adecomm/_report.p 
   (INPUT p_DbId, 
    INPUT header_str,
    INPUT "Table Relations Report",
    INPUT "",
    INPUT p_Btns,
    INPUT "adecomm/_treldat.p",
    INPUT p_TblName,
    INPUT {&Table_Relations_Report}).

RETURN RETURN-VALUE.

/* _trelrpt.p - end of file */ 

