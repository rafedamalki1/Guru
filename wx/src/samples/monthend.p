/************************************************************************************
	PROCEDURE: monthend.p

	PURPOSE:   Calculates the last day of the month

	SYNTAX:    RUN samples/monthend.p (INPUT in, OUTPUT out).

	REMARKS:   This code calculates the last day of the month
		 

	PARAMETERS:
            INPUT:  date
            OUTPUT: date of the last day of the month

	AUTHORS:   Judy Rothermal
	DATE:      February 1993

	LAST INSPECTED:
	INSPECTED BY:

 ************************************************************************************/
 /* Copyright(c) PROGRESS SOFTWARE CORPORATION, 1993 - All Rights Reserved.	    */

 /*Code_Start*/
 
DEF INPUT  PARAMETER in-date  AS DATE NO-UNDO.
DEF OUTPUT PARAMETER out-date AS DATE NO-UNDO.

out-date = 
        ((DATE(MONTH(in-date),28,YEAR(in-date)) + 4) - 
          DAY(DATE(MONTH(in-date),28,YEAR(in-date)) + 4)).



