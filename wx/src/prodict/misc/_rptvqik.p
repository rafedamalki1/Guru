/*************************************************************/
/* Copyright (c) 1984-1994 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/*----------------------------------------------------------------------------

File: _rptvqik.p

Description:
   Quick and dirty view report for character dictionary.
 
Input: user_env[1] = View name or "ALL".

Author: Tony Lavinio, Laura Stern

Date Created: 10/07/92

----------------------------------------------------------------------------*/

{ prodict/dictvar.i }
{ prodict/user/uservar.i }

RUN adecomm/_qviwrpt.p
   (INPUT drec_db,    
    INPUT (IF PDBNAME(user_dbname) = ? 
      	     THEN user_dbname ELSE PDBNAME(user_dbname)),
    INPUT user_dbtype,
    INPUT user_env[1]).

