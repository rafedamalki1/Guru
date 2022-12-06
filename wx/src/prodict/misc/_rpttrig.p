/*************************************************************/
/* Copyright (c) 1984-1994 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/*----------------------------------------------------------------------------

File: _rpttrig.p

Description:
   Trigger report for character dictionary.
 
Author: Laura Stern

Date Created: 11/20/92

----------------------------------------------------------------------------*/

{ prodict/dictvar.i }
{ prodict/user/uservar.i }

RUN adecomm/_trigrpt.p
   (INPUT drec_db,    
    INPUT (IF PDBNAME(user_dbname) = ? 
      	     THEN user_dbname ELSE PDBNAME(user_dbname)),
    INPUT user_dbtype).

