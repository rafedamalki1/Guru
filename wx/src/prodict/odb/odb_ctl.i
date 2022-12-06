/*************************************************************/
/* Copyright (c) 1984-1997 by Progress Software Corporation  */
/*                                                           */
/* All rights reserved.  No part of this program or document */
/* may be  reproduced in  any form  or by  any means without */
/* permission in writing from PROGRESS Software Corporation. */
/*************************************************************/

/* odb_ctl.i - Odbc schema control settings */
/*
Note 'ODBALLOWED':
  This is a list of object types that the gateway supports.  Currently,
  "VIEW" "TABLE" "SYSTEM TABLE" and "PROCEDURE" are allowed.

Note 'SOBJECTS' 'POBJECTS':
  sobjects contains a list of Odbc objects as they described in the
  SQLTables API call. pobjects is their PROGRESS name.
*/

DEFINE VARIABLE datetime     	AS LOGICAL   NO-UNDO.
DEFINE VARIABLE odballowed   	AS CHARACTER NO-UNDO.
DEFINE VARIABLE pobjects     	AS CHARACTER NO-UNDO.
DEFINE VARIABLE sobjects     	AS CHARACTER NO-UNDO.
DEFINE VARIABLE odbc-dict-ver 	AS CHARACTER NO-UNDO INIT "1.000.000".

/* List of certified drivers. Warning is issued for non certified drivers. */
DEFINE VARIABLE odbc-certify-list AS CHARACTER NO-UNDO.

/* Each odbc-bug-list[N] variable includes a list of ODBC drivers which have */
/* the bug. "ALL" suggests that all the drivers has the bug. Each	     */
/* odbc-bug-excld[N] variable include a list of ODBC drivers which DON'T have*/
/* the bug (useful if you want to exclude specific drivers where ALL was     */
/* specified.								     */ 

/* BUG-1 is the inability to use Search Patterns in a Catalog function  */
/*       for qualifiers.						*/
/* BUG-2 is an occasional corrupt of prepared statements at the end of  */
/*       a transaction.							*/
/* BUG-3 is the inconsistency of the sort order of NULL values.		*/
/* BUG-4 is the description of all the components of an index in one row*/
/*       concatenating the fields: "fld1+fld2+fld3..."			*/
/* BUG-5 the support of only one statement per connection.		*/
/* BUG-6 Multiple connection can block each other.			*/
/* BUG-7 Quoting an identifier does not work.				*/ 
/* BUG-8 is the inability to use Search Patterns in a Catalog function  */
/*       for owners.		     				        */
/* BUG-9 is wrong type values returned from SQLColumnAttributes. To work*/
/*       around it, we skip runtime schema comparison.			*/
/* BUG-10 is not being able to handle WHERE 1 = 2 clauses.              */
/* BUG-11 NULL is sorted to be the lowest value. Turn BUG-3 off !!	*/
/* BUG-12 Do SELECT <key> not SELECT <flds> - More compatible but slow. */
/* BUG-13 Lock/transaction mode 1 support by driver:   			*/
/*            Exclusive-lock & Lock Upgrade.				*/
/*	      No shared-lock support,					*/
/*	      One connection.						*/
/*	      async mode.						*/
/*	      only scrolling cursors.					*/
/*	  Lock/transaction mode 0 by driver, which is the default:	*/
/*	      No lock control.						*/
/*	      Use the dbmgr isolation level for concurrency control.	*/
/*	      By default - One connection only.				*/
/*	      no async mode.						*/
/*	      might use non scrolling cursors.				*/
/* BUG-14 Does not follow the ODBC protocol for the LIKE where clause   */
/*	  operator. Wants the type of the value to match the type of    */
/* 	  the field instead of being specified as SQL_VARCHAR.		*/ 
/* BUG-15 The db has a "timestamp" field which get updated with each row*/
/* 	  update.							*/
/* BUG-16 We use SQLColumns, at runtime, to get schema info, instead of */
/*	  SQLColumnsAttributes - to prevent waiting on a lock.		*/
/*        To avoid a full table scan by not issuing SQL statement       */ 
/*        "select * from table where 1=2" for Access2 driver            */ 
/*        ODBCJT16.DLL per bug #95-09-01-043.                           */
/* BUG-17 UPPER function for the WHERE clause is not supported.		*/
/* BUG-18 This driver needs the hint system.				*/
/* BUG-19 This driver gives the wrong index uniqueness info.		*/
/* BUG-20 This Informix driver does not set the LOCK MODE to WAIT.	*/
/*	  We have to do it.						*/
/* BUG-21 The driver cannot handle FLD LIKE ? phrase. We put 	        */
/*	   FLD LIKE <literal> instead.					*/
/* BUG-22 Driver sends wrong data about indexes - don't build indexes   */
/*	  automatically.						*/
/* BUG-23 The driver has some problems in qualifying object in SQLTab().*/
/*        We will qualify, on the client side, the objects for schema   */
/*	  import.							*/
/* BUG-24 The driver has some problems with wild cards in SQLTables().  */
/*        We will qualify, on the client side, the objects for schema   */
/*	  import if wild cards are used. Note that BUG-23 implies       */
/*	  BUG-24.							*/
/* BUG-25 The driver supports WHERE char_fld = <value> even if <value>  */
/*	  is longer then what char_fld can hold. We would allow that    */
/*	  expression for those drivers.					*/
/* BUG-26 The driver has problems with re-using prepared statements -   */
/*        we don't reuse them.						*/
/* BUG-27 The driver has problems with join by sql db - do it by client.*/ 
/* BUG-28 Don't create OWNER.TABLE names, since "OWNER." is added by    */
/*        the Driver.                                                   */
/* BUG-29 The driver has proprietary ODBC extensions for BLOB/CLOB      */
/*  	  support. Specifically, the driver uses -98/-99 for these      */
/*        large objects. Progress will translate these to LongVarBinary */
/*        datatype.                                                     */
/* BUG-30 The driver supports setting transaction isolation level on	*/
/*  	  a statement basis. I.e., PODBC can change it within a         */
/*  	  transaction, not just before a transaction begins. Note that  */
/*        per-statement isolation is NOT ODBC V2.0 conformant, but is	*/
/*  	  very useful for Progress. 	    	    	    	    	*/
/* BUG-31 The driver supports the use of % and _ in LIKE string by      */
/*        using brackets around the characters.  Driver doesn't support */
/*        the escape clause.                                            */
/* BUG-32 Driver does not support the ESCAPE clause.  There is no known */
/*        mechanism to use % and _ characters in the LIKE string.       */    
/* BUG-33 Driver does NOT support ASYNC as PODBC prefers, so PODBC will */
/*        not issue ASYNC_ENABLE  call to driver. Crucial rule that	*/
/*        driver must follow is to support polling for call completion	*/
/*        by call with truncated parameter list (see dtserror.c).   	*/
/*        Thus, if this flag on, PODBC will NOT request driver to do    */
/*        ASYNC at Connect time. If flag off, PODBC will request ASYNC. */
/* BUG-34 Driver expects databases of the format dbname:owner.table     */
/*        as opposed to dbamae.owner.table.  If the flag is on, then    */
/*	  change the format of the full table name to use a :, not dot. */
/*        This is specifically for a problem with Informix databases.   */			 	
        

/* Modified by 	: Anders Luk						*/
/* Date	       	: 2/7/96						*/
/* Purpose	: Adding new 32-bit drivers for certification		*/
/*			Informix5 - Intersolv IVINF508.DLL		*/
/*			Dbase 5.5 - Intersolv IVDBF08.DLL		*/
/*			Access 7.0 - MS ODBCJT32.DLL			*/
/*			Paradox 7.0 - MS ODBCJT32.DLL ?			*/
/* Modified by 	: SLK 2/20/97						*/
/* Purpose	: Fix s/e 135 More than 4096 chars in single statement  */
 
DEFINE VARIABLE odbc-bug-list AS CHARACTER EXTENT 80 NO-UNDO.
DEFINE VARIABLE odbc-bug-excld AS CHARACTER EXTENT 80 NO-UNDO.
 
ASSIGN
odbc-certify-list = "PROGODBC.DLL,ODBCJT16.DLL,SIMBA.DLL,DB2CLIW.DLL,"
        + "PODBC_Sybase_Driver,PODBC_Allbase_Driver,PODBC_DB2_Driver,"
        + "QEINF04.DLL,QEINF05.DLL,QEINF06.DLL,QEPDX04.DLL,QEPDX05.DLL,"
        + "QEPDX06.DLL,QEPDX07.DLL,QEDBF03.DLL,QEDBF04.DLL,QEDBF05.DLL,"
        + "QEDBF06.DLL,QEDBF07.DLL,QEINF503.DLL,QEINF504.DLL,"
        + "QEINF505.DLL,QEINF506.DLL,QEINF507.DLL,PODBC_M/S_SQL_Server_Driver"
odbc-bug-list[1] = "ODBCJT32.DLL,ODBCJT16.DLL,SIMBA.DLL,QEDBF,QEPDX,QEGUP,"
	+ "IVDBF"
odbc-bug-list[2] = "SIMBA.DLL,QEDBF,QEPDX,IVDBF"
odbc-bug-list[3] = "ALL"
odbc-bug-list[4] = "QEDBF,QEPDX,IVDBF"
odbc-bug-list[5] = "PODBCSYB,PODBC_Sybase_Driver,PODBC_M/S_SQL_Server_Driver"
odbc-bug-list[6] = "PODBC_Sybase_Driver,PODBC_M/S_SQL_Server_Driver" 
odbc-bug-list[7] = "QEGUP,DB2CLIW.DLL,PODBC_M/S_SQL_Server_Driver,SQLSRV32.DLL,"
           + "DB2CLI.DLL,IVOR709.DLL"
odbc-bug-list[8] = "SIMBA.DLL,QEDBF,QEPDX,IVDBF"
odbc-bug-list[9] = "QEGUP"
odbc-bug-list[10] = "PODBC_Sybase_Driver,QEINF,QEINF5,PODBC_Allbase_Driver,"
           + "PODBC_M/S_SQL_Server_Driver,IVINF508.DLL,IVINF709.DLL"
odbc-bug-list[11] = "PODBC_Sybase_Driver,QEINF,QEINF5,IVINF508.DLL,IVINF709.DLL"
           + "PODBC_M/S_SQL_Server_Driver"
odbc-bug-list[12] = "PODBC_Sybase_Driver,PODBC_M/S_SQL_Server_Driver"
odbc-bug-list[13] = "PODBC_Sybase_Driver,PODBC_Allbase_Driver,DB2CLIW.DLL,"
           + "PODBC_DB2_Driver,PODBC_M/S_SQL_Server_Driver,SQLSRV32.DLL,"
           + "DB2CLI.DLL,IVOR709.DLL"
odbc-bug-list[14] = "PODBC_DB2_Driver,DB2CLIW.DLL,SQLSRV32.DLL,DB2CLI.DLL,"
           + "IVOR709.DLL"
. ASSIGN
odbc-bug-list[15] = "PODBC_Sybase_Driver,PODBC_M/S_SQL_Server_Driver"
odbc-bug-list[16] = "PODBC_Sybase_Driver,ODBCJT16.DLL,ODBCJT32.DLL"
odbc-bug-list[17] = "PODBC_Allbase_Driver,QEINF,QEINF5,IVINF508.DLL,"
           + "IVINF709.DLL"
odbc-bug-list[18] = "PODBC_Allbase_Driver"
odbc-bug-list[19] = ""
odbc-bug-list[20] = "QEINF,QEINF5,IVINF508.DLL,IVINF709.DLL"
odbc-bug-list[21] = ""
odbc-bug-list[22] = ""
odbc-bug-list[23] = "ALL"
odbc-bug-list[24] = ""
odbc-bug-list[25] = "PODBC_Sybase_Driver,PODBC_M/S_SQL_Server_Driver"
odbc-bug-list[26] = "QEINF,QEINF5,ODBCJT16.DLL,ODBCJT32.DLL,DB2CLIW.DLL,"
	  +  "IVINF508.DLL,IVINF709.DLL,DB2CLI.DLL"
odbc-bug-list[27] = "ODBCJT16.DLL,ODBCJT32.DLL"
odbc-bug-list[28] = ""
odbc-bug-list[29] = "DB2CLIW.DLL,,DB2CLI.DLL"
odbc-bug-list[30] = "PODBC_M/S_SQL_Server_Driver,PODBC_Sybase_Driver"
odbc-bug-list[31] = "PODBC_M/S_SQL_Server_Driver"
odbc-bug-list[32] = "ODBCJT16.DLL,ODBCJT32.DLL,IVDBF08.DLL"
odbc-bug-list[33] = "SQLSRV32.DLL,DB2CLIW.DLL,DB2CLI.DLL,IVOR709.DLL"
odbc-bug-list[34] = "IVINF508.DLL,IVINF709.DLL"
odbc-bug-excld[1] = "" 
odbc-bug-excld[2] = "" 
odbc-bug-excld[3] = "PODBC_Sybase_Driver,QEINF,QEINF5,PODBC_DB2_Driver,"
 + "DB2CLIW.DLL,PODBC_M/S_SQL_Server_Driver,IVINF508.DLL,IVINF709.DLL,"
 + "DB2CLI.DLL" 
odbc-bug-excld[4] = "" 
odbc-bug-excld[5] = "" 
odbc-bug-excld[6] = "" 
odbc-bug-excld[7] = ""
odbc-bug-excld[8] = ""
odbc-bug-excld[9] = ""
odbc-bug-excld[10] = ""
odbc-bug-excld[11] = ""
odbc-bug-excld[12] = ""
odbc-bug-excld[13] = ""
odbc-bug-excld[14] = ""
odbc-bug-excld[15] = ""
odbc-bug-excld[16] = ""
odbc-bug-excld[17] = ""
odbc-bug-excld[18] = ""
odbc-bug-excld[19] = ""
odbc-bug-excld[20] = ""
odbc-bug-excld[21] = ""
odbc-bug-excld[22] = ""
odbc-bug-excld[23] = "PODBC_Sybase_Driver,PODBC_DB2_Driver,"
           + "PODBC_M/S_SQL_Server_Driver"
odbc-bug-excld[24] = ""
odbc-bug-excld[25] = ""
odbc-bug-excld[26] = ""
odbc-bug-excld[27] = ""
odbc-bug-excld[28] = ""
odbc-bug-excld[29] = ""
odbc-bug-excld[30] = ""
odbc-bug-excld[31] = ""
odbc-bug-excld[32] = ""
odbc-bug-excld[33] = ""
odbc-bug-excld[34] = ""

pobjects = "TABLE,STABLE,VIEW,LOG,PROCEDURE,RULE,DEFAULT,TRIGGER,BUFFER,ALIAS,SYNONYM,SEQUENCE"
sobjects = "TABLE,SYSTEM TABLE,VIEW,LOG,PROCEDURE,RULE,DEFAULT,TRIGGER,BUFFER,ALIAS,SYNONYM,SEQUENCE"
odballowed = "TABLE,SYSTEM TABLE,VIEW,ALIAS,SYNONYM,PROCEDURE,BUFFER,SEQUENCE".



