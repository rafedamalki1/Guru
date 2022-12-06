/*************************************************************/
/* Copyright (c) 1984-1995 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/*--------------------------------------------------------------------

File: prodict/_dctalia.p

Description:
    
    Sets DICTDB alias to Database number p_db-num
            
Input-Parameters:
    p_db-num    number of db to set alias to

Output-Parameters:
    none
    
Used/Modified Shared Objects:
    none
    
Author: Tom Hutegger

History:
    hutegger    94/04/12    creation

                            
--------------------------------------------------------------------*/
/*h-*/

/*----------------------------  DEFINES  ---------------------------*/

define input parameter p_db-name    as character.

/*------------------------  INT.-PROCEDURES  -----------------------*/

/*---------------------------  MAIN-CODE  --------------------------*/

      find first DICTDB._Db
        where DICTDB._Db._Db-name = p_db-name
        no-error.

      if not available DICTDB._Db
       then create alias "DICTDB"
            for database VALUE(SDBNAME(p_db-name))
            no-error.

/*------------------------------------------------------------------*/
