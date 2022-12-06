/*************************************************************/
/* Copyright (c) 1984-1994 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/*----------------------------------------------------------------------------

File: trigrpt.p

Description:
   Trigger report for both the GUI and character dictionary.
 
Input Parameters:
   p_DbId   - Id of the _Db record corresponding to the current database
   p_PName  - Physical name of the database
   p_DbType - Database type (e.g., PROGRESS)

Author: Laura Stern

Date Created: 11/19/92

Modified on 06/14/94 by Gerry Seidl. Added NO-LOCKs on file accesses.
----------------------------------------------------------------------------*/

{adecomm/commeng.i}  /* Help contexts */

DEFINE INPUT PARAMETER p_DbId 	 AS RECID NO-UNDO.
DEFINE INPUT PARAMETER p_PName 	 AS CHAR  NO-UNDO.
DEFINE INPUT PARAMETER p_DbType  AS CHAR  NO-UNDO.

DEFINE VAR header_str AS CHAR 	 NO-UNDO.
DEFINE VAR flags      AS CHAR 	 NO-UNDO.
DEFINE VAR can_see    AS LOGICAL NO-UNDO init yes.

FIND _File "_File" NO-LOCK.
IF NOT CAN-DO(_File._Can-read,USERID("DICTDB")) THEN can_see = no.
IF can_see THEN DO:
  FIND _File "_File-trig" NO-LOCK.
  IF NOT CAN-DO(_File._Can-read,USERID("DICTDB")) THEN can_see = no.
END.
IF can_see THEN DO:
  FIND _File "_Field-trig" NO-LOCK.
  IF NOT CAN-DO(_File._Can-read,USERID("DICTDB")) THEN can_see = no.
END.
IF NOT can_see THEN DO:
  MESSAGE "You do not have permission to use this option."
    VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  RETURN.
END.

FIND LAST _File-trig NO-LOCK NO-ERROR.
IF NOT AVAILABLE _File-trig THEN DO:
   FIND LAST _Field-trig NO-LOCK NO-ERROR.
   IF NOT AVAILABLE _Field-trig THEN DO:
      MESSAGE "There are no triggers in this database to look at."
      	VIEW-AS ALERT-BOX ERROR BUTTONS OK.
      RETURN.
   END.
END.

header_str = "Database: " + p_PName + " (" + p_DbType + ")".
flags = "Flags: * = overridable, 'm' = mismatched crc, 'nr' = no r-code found".
RUN adecomm/_report.p 
   (INPUT p_DbId, 
    INPUT header_str,
    INPUT "Trigger Report",
    INPUT flags,
    INPUT "",
    INPUT "adecomm/_trigdat.p",
    INPUT "",
    INPUT {&Trigger_Report}).

/* _trigrpt.p - end of file */

