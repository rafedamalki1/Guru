/*************************************************************/
/* Copyright (c) 1984-1995 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/*----------------------------------------------------------------------------

File: setalias.i

Description:   
   This procedure sets the alias as4dict which is used in the
   PROGRESS/400 Data Dictionary.  The alias allows the schema holder
   to have multi AS400 Schemas.
 
Author: Donna L. McMann

Date Created: 12/30/94

----------------------------------------------------------------------------*/

cr-al:
FOR EACH _Db NO-LOCK:
    IF _Db._Db-type = "AS400" THEN DO:
        /* default to the first as400 that is connected*/     
      IF CONNECTED(_Db._Db-name) THEN DO:      
           FIND _File OF _DB WHERE _File._File-name = "p__File" NO-LOCK NO-ERROR.
                 IF AVAILABLE _File THEN DO:
                    CREATE ALIAS as4dict FOR DATABASE VALUE(_Db._Db-name).
                     ASSIGN assgndb = TRUE.
                      LEAVE cr-al.
                 END.
            END.
      END.
END.

