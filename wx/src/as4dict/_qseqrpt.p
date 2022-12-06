/*************************************************************/
/* Copyright (c) 1984-1995 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/*----------------------------------------------------------------------------

File: qseqrpt.p

Description:
   Quick and dirty sequence report for both the GUI and character dictionaries.

Input Parameters:
   p_DbId    - Id of the _Db record corresponding to the current database
   p_PName   - Physical name of the database
   p_DbType  - Database type (e.g., PROGRESS)

Author: Tony Lavinio, Laura Stern

Date Created: 10/08/92

Modified on 06/14/94 by Gerry Seidl. Added NO-LOCKs to file accesses. 
Modified on 02/13/95 by D. McMann to work with Progress/400 data dictionary.
----------------------------------------------------------------------------*/

{adecomm/commeng.i}  /* Help contexts */

DEFINE INPUT PARAMETER p_DbId 	 AS RECID NO-UNDO.
DEFINE INPUT PARAMETER p_PName 	 AS CHAR  NO-UNDO.
DEFINE INPUT PARAMETER p_DbType  AS CHAR  NO-UNDO.

DEFINE VAR header_str AS CHAR NO-UNDO.

FIND _File "_Sequence" NO-LOCK.
IF NOT CAN-DO(_File._Can-read,USERID("DICTDB")) THEN DO:
  MESSAGE "You do not have permission to use this option."
    VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  RETURN.
END.

FIND LAST as4dict.p__Seq NO-LOCK NO-ERROR.
IF NOT AVAILABLE as4dict.p__Seq THEN DO:
  MESSAGE "There are no sequences in this database to look at."
    VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
  RETURN.
END.

header_str = "Database: " + p_PName + " (" + p_DbType + ")".
RUN as4dict/_report.p 
   (INPUT p_DbId, 
    INPUT header_str,
    INPUT "Sequence Report",
    INPUT "",
    INPUT "",
    INPUT "as4dict/_qseqdat.p",
    INPUT "",
    INPUT {&Sequence_Report}).

/* _qseqrpt.p - end of file */