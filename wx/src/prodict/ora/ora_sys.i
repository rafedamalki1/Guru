/*************************************************************/
/* Copyright (c) 1984-1996 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/*--------------------------------------------------------------------

File: prodict/ora/ora_sys.i

Description:
    This string contains a comma-seperated list of the names of all
    system-objects for ORACLE, that are non-queryable.
    For example: Stored-Procedures, Buffers, ...
    This file gets used also from the report-builder -therefore it
    needs to be in a format that complies with C *and* PRGORESS
    
Text-Parameters:  
   none
                                     
History:
    hutegger    95/08   creation
    
--------------------------------------------------------------------*/
/*h-*/

"PROC-TEXT-BUFFER,SEND-SQL-STATEMENT,CloseAllProcs"

/*------------------------------------------------------------------*/
