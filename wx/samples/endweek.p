/************************************************************************************
	PROCEDURE: endweek.p

	PURPOSE:   Calculates last day of the week 

	SYNTAX:    RUN samples/endweek.p (INPUT in, OUTPUT out).

	REMARKS:   This code calculates the date of the LAST day for a 
		   given week.

	PARAMETERS:
            INPUT:  A week-number (For example 199042)
            OUTPUT: The date of day 7 in input week (10/15/90 using 199042 as input)

	AUTHORS:   Judy Rothermal
	DATE:      February 1993

	LAST INSPECTED:
	INSPECTED BY:

 ************************************************************************************/
 /* Copyright(c) PROGRESS SOFTWARE CORPORATION, 1993 - All Rights Reserved.	    */

 /*Code_Start*/
 
/* Assumptions:                                                     */
/* 1. Weeks start on MONDAYS                                        */
/* 2. If January 1st falls on Friday, Saturday, Sunday or Monday    */
/*    then week 1 for this year will start on the first Monday      */
/*    the same year. If not, week 1 will start on the last Monday   */
/*    previous year.                                                */
/*    (In other words: At least 4 of the seven days of week 1 for   */
/*     a given year must fall into this year)                       */


DEFINE INPUT  PARAMETER inweek AS INT.   /* Input week , eg 199042   */
DEFINE OUTPUT PARAMETER ldate  AS DATE.  /* Output date, eg 10/15/90 */

RUN samples/begweek.p (inweek , OUTPUT ldate).
ldate = ldate + 6.




