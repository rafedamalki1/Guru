/*************************************************************/
/* Copyright (c) 1984-1995 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/*--------------------------------------------------------------------

File: prodict/odb/odb_sys.i

Description:
    This string contains a comma-seperated list of the names of all
    system-objects for ODBC-based schemaholders, that are non-queryable.
    For example: Stored-Procedures, Buffers, ...
    This file gets used also from the report-builder -therefore it
    needs to be in a format that complies with C *and* PROGRESS
    
Text-Parameters:  
   none
                                     
History:
    hutegger    95/08   creation
    
--------------------------------------------------------------------*/
/*h-*/
"SQLTables,SQLTables_buffer,SQLColumns,SQLColumns_buffer,SQLStatistics,SQLStatistics_buffer,GetFieldIds,GetFieldIds_buffer,GetInfo,GetInfo_buffer,CloseAllProcs,SendInfo,PROC-TEXT-BUFFER,SEND-SQL-STATEMENT"

/*------------------------------------------------------------------*/
