/************************************************************************************
	PROCEDURE: addweek.dem

	PURPOSE:   Calculate the postscript x coordinate of a date

	SYNTAX:    "{samples/psganttc.i}"

	REMARKS:  

        PARAMETERS:NONE

	AUTHORS:   Progress Consulting
	DATE:      March 1993

	LAST INSPECTED:
	INSPECTED BY:

 ************************************************************************************/
 /* Copyright(c) PROGRESS SOFTWARE CORPORATION, 1993 - All Rights Reserved.	    */

/*Code_Start*/

/* GANTT Chart Generation Interface */
/* psganttc.i - calculate the postscript x coordinate of a date */

(max(250, min(750, 250 + 500 *(({1}) - leftdate) / date-range)))
