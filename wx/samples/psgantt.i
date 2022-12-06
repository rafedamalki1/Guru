/************************************************************************************
	PROCEDURE: psgantt.i

	PURPOSE:   Workfile needed for the gsgantt.p 

	SYNTAX:    "{samples/psgantt.i}"

	REMARKS:   

        PARAMETERS:NONE

	AUTHORS:   Progress Consulting
	DATE:      March 1993

	LAST INSPECTED:
	INSPECTED BY:

 ************************************************************************************/
 /* Copyright(c) PROGRESS SOFTWARE CORPORATION, 1993 - All Rights Reserved.	    */

/*Code_Start*/

/* psgantt.i - Workfile needed for the psgantt.p routine */

define {1} shared workfile gantt
       field cur-start  as date
       field cur-finish as date
       field lst-start  as date
       field lst-finish as date
       field grp-name   as char
       field grp-label  as char
       field grp-sort   as char.
