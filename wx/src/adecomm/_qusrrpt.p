/*************************************************************/
/* Copyright (c) 1984-1994 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/*----------------------------------------------------------------------------

File: qusrrpt.p

Description:
   Quick and dirty user report for both the GUI and character dictionaries.

Input Parameters:
   p_DbId    - Id of the _Db record corresponding to the current database
   p_PName   - Physical name of the database
   p_DbType  - Database type (e.g., PROGRESS)

Author: Tony Lavinio, Laura Stern

Date Created: 10/05/92

Modified on 06/14/94 by Gerry Seidl. Added NO-LOCKs to file accesses.
----------------------------------------------------------------------------*/

{adecomm/commeng.i}  /* Help contexts */

DEFINE INPUT PARAMETER p_DbId 	 AS RECID NO-UNDO.
DEFINE INPUT PARAMETER p_PName 	 AS CHAR  NO-UNDO.
DEFINE INPUT PARAMETER p_DbType  AS CHAR  NO-UNDO.

DEFINE VAR header_str AS CHAR NO-UNDO.

FIND _File "_User" NO-LOCK.
IF NOT CAN-DO(_File._Can-read,USERID("DICTDB")) THEN DO:
  MESSAGE "You do not have permission to use this option."
    VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  RETURN.
END.

FIND LAST _User NO-LOCK NO-ERROR.
IF NOT AVAILABLE _User THEN DO:
  MESSAGE "There are no users in this database to look at."
    VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  RETURN.
END.

header_str = "Database: " + p_PName + " (" + p_DbType + ")".
RUN adecomm/_report.p 
   (INPUT p_DbId, 
    INPUT header_str,
    INPUT "User Report",
    INPUT "",
    INPUT "",
    INPUT "adecomm/_qusrdat.p",
    INPUT "",
    INPUT {&User_Report}).

/* _qusrrpt.p - end of file */

