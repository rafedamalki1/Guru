/*************************************************************/
/* Copyright (c) 1984-1996 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/*--------------------------------------------------------------------

File: adecomm/ds_type.i

Usage:
    <int-type> = {adecomm/ds_type.i
                     &direction = "itoe"
                     &from-type = "ixt-type"
                     }
    <ext-type> = {adecomm/ds_type.i
                     &direction = "etoi"
                     &from-type = "int-type"
                     }

Description:   
    Function to convert internal to external db-type
    the result is the converted db-type         OR
    supply the list of all internal or external db-types
            
call:
    
Text-Parameters:
    &direction      "etoi"  (external to internal)      OR
                    "etype" (list of external types)    OR
                    "itoe"  (internal to external)      OR
                    "itype" (list of internal types)    OR
                    "ODBC"  (list of internal ODBC-types)
    &from-type      db-type to be converted             OR
                    dummy-variable

Included in:
    adecomm/_dbconnx.p
    prodict/user/usermenu.i
        
Author: Tom Hutegger

History:
    pittman     96/06/05    added DB2-DRDA as new ODBC-based DS
    hutegger    95/11/16    added MSSQLSRV as new ODBC-based DS
    pittman     95/05/xx    added DB2      as new ODBC-based DS
    hutegger    94/07/25    extented with "itype" and "etype"
    hutegger    94/05/18    creation
    
--------------------------------------------------------------------*/ 
/*h-*/

/*------------------------------------------------------------------*/ 
/* NOTE: all lists have to be in the same order and all sets in sync! */
      
IF      "{&direction}" = "etoi"
 THEN ENTRY(LOOKUP({&from-type},
/* list of all external names */
    "AS/400,CTOS-ISAM,C-ISAM,NetISAM,ODBC,ORACLE,PROGRESS,SYBASE," +
    "SYBASE-10,Rdb,RMS,ALLBASE,DB2,DB2-DRDA,MS SQL Server"),
/* list of all internal names */
    "AS400,CTOSISAM,CISAM,NETISAM,ODBC,ORACLE,PROGRESS,SYBASE," +
    "SYB10,Rdb,RMS,ALLBASE,DB2,DB2-DRDA,MSSQLSRV")

ELSE IF "{&direction}" = "itoe"
 THEN ENTRY(LOOKUP({&from-type},
/* list of all internal names */
    "AS400,CTOSISAM,CISAM,NETISAM,ODBC,ORACLE,PROGRESS,SYBASE," +
    "SYB10,Rdb,RMS,ALLBASE,DB2,DB2-DRDA,MSSQLSRV"),
/* list of all external names */
    "AS/400,CTOS-ISAM,C-ISAM,NetISAM,ODBC,ORACLE,PROGRESS,SYBASE," +
    "SYBASE-10,Rdb,RMS,ALLBASE,DB2,DB2-DRDA,MS SQL Server")

ELSE IF "{&direction}" = "odbc"
 THEN /* list of all ODBC-DataServers (internal names) */
    "ODBC,SYB10,ALLBASE,DB2,DB2-DRDA,MSSQLSRV"

ELSE IF "{&direction}" = "itype"
 THEN /* list of all internal names */
    "AS400,CTOSISAM,CISAM,NETISAM,ODBC,ORACLE,PROGRESS,SYBASE," +
    "SYB10,Rdb,RMS,ALLBASE,DB2,DB2-DRDA,MSSQLSRV"

 ELSE /* list of all external names */
    "AS/400,CTOS-ISAM,C-ISAM,NetISAM,ODBC,ORACLE,PROGRESS,SYBASE," +
    "SYBASE-10,Rdb,RMS,ALLBASE,DB2,DB2-DRDA,MS SQL Server"

/*------------------------------------------------------------------*/
      

