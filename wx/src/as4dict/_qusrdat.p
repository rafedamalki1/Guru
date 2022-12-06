/*************************************************************/
/* Copyright (c) 1984-1995 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/*----------------------------------------------------------------------------

File: qusrdata.p

Description:
   Display _User information for the quick user report.  It will go to 
   the currently set output device (e.g., a file, the printer).
 
Input Parameters:
   p_DbId - Id of the _Db record for this database.

Author: Tony Lavinio, Laura Stern

Date Created: 10/05/92

Modified on 06/14/94 by Gerry Seidl. Added NO-LOCKs to file accesses. 
Modified on 02/16/95 by Donna McMann to work with Progress/400 Data Dictionary.
----------------------------------------------------------------------------*/

/* This isn't used because there is no db id in _User record
   but there should be!!! */
DEFINE INPUT PARAMETER p_DbId  AS RECID NO-UNDO.

DEFINE SHARED STREAM rpt.
DEFINE VAR pword AS LOGICAL NO-UNDO.

FORM
  p__User._Userid    FORMAT "x(8)"   LABEL "User ID"
  p__User._User-name FORMAT "x(40)"  LABEL "User Name"
  pword            FORMAT "yes/no" LABEL "Has Password?"
  WITH FRAME shousers 
  DOWN USE-TEXT STREAM-IO.

FOR EACH p__User NO-LOCK:
   DISPLAY STREAM rpt
      p__User._Userid
      p__User._User-name
      p__User._Password <> ENCODE("") @ pword
      WITH FRAME shousers.
  DOWN STREAM rpt WITH FRAME shousers.
END.

