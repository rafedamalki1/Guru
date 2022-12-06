/*************************************************************/
/* Copyright (c) 1984-1994 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/*--------------------------------------------------------------------

File: adedict/_dictfdb.p

Description:
    checks if the schmeaholder has also a PROGRESS-Schema in it
    
Input-Output Pramaeters:
    dbnum   gets increased by one if PROGRESS-Schemaholder contains no 
    PROGRESS-Schema
    
    
History:
    hutegger    94/06/13    creation
    
--------------------------------------------------------------------*/

DEFINE INPUT-OUTPUT PARAMETER dbnum as integer.

/*------------------------------------------------------------------*/

        find first DICTDB._db where DICTDB._db._db-name = ? no-error.         
        if available DICTDB._Db
         AND NOT can-find(first DICTDB._file of DICTDB._db             
                         where DICTDB._file._file-number > 0)  
          then assign dbnum = dbnum + 1.               


/*------------------------------------------------------------------*/
