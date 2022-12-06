/*************************************************************/
/* Copyright (c) 1984-1994 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/*----------------------------------------------------------------------------

File: _rptiqik.p

Description:
   Quick and dirty index report for character dictionary.
 
Author: Tony Lavinio, Laura Stern

Date Created: 10/05/92

----------------------------------------------------------------------------*/

{ prodict/dictvar.i }
{ prodict/user/uservar.i }

RUN adecomm/_qidxrpt.p
   (INPUT drec_db,
    INPUT (IF PDBNAME(user_dbname) = ? 
      	     THEN user_dbname ELSE PDBNAME(user_dbname)),
    INPUT user_dbtype,
    INPUT user_filename,
    INPUT "s").

IF RETURN-VALUE = "s" THEN 
   &IF "{&WINDOW-SYSTEM}" = "TTY" &THEN
   user_path = "1=a,_usrtget,_rptiqik".
   &ELSE
   user_path = "1=a,_guitget,_rptiqik".
   &ENDIF
