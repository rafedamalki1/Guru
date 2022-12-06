/*************************************************************/
/* Copyright (c) 1984-1995 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/*----------------------------------------------------------------------------

File: qtblrpt.p

Description:
   Quick and dirty table report for both the GUI and character dictionary.
 
Input Parameters:
   p_DbId   - Id of the _Db record corresponding to the current database
   p_PName  - Physical name of the database
   p_DbType - Database type (e.g., PROGRESS)

Author: Tony Lavinio, Laura Stern

Date Created: 10/05/92

----------------------------------------------------------------------------*/

{adecomm/commeng.i}  /* Help contexts */

DEFINE INPUT PARAMETER p_DbId 	 AS RECID NO-UNDO.
DEFINE INPUT PARAMETER p_PName 	 AS CHAR  NO-UNDO.
DEFINE INPUT PARAMETER p_DbType  AS CHAR  NO-UNDO.

DEFINE VAR header_str AS CHAR NO-UNDO.
DEFINE VAR flags      AS CHAR NO-UNDO.

FIND _File "_File".
IF NOT CAN-DO(_File._Can-read,USERID("DICTDB")) THEN DO:
  MESSAGE "You do not have permission to use this option."
    VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  RETURN.
END.

FIND LAST as4dict.p__File WHERE as4dict.p__File._Hidden = "N" NO-ERROR.
IF NOT AVAILABLE as4dict.p__File THEN DO:
  MESSAGE "There are no tables in this database to look at."
    VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  RETURN.
END.

header_str = "Database: " + p_PName + " (" + p_DbType + ")".   

/* COMMENT OUT FLAGS FOR NOW 
flags = "Flags: 'f' = frozen, 's' = a SQL table".  
*/
RUN as4dict/_report.p 
   (INPUT p_DbId, 
    INPUT header_str,
    INPUT "Quick Table Report",
    INPUT  "",
    INPUT "",
    INPUT "as4dict/_qtbldat.p",
    INPUT "",
    INPUT {&Quick_Table_Report}).

/* _qtblrpt.p - end of file */

