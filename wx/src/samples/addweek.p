/************************************************************************************
	PROCEDURE: addweek.p

	PURPOSE:   Add/Subtract a number of weeks to a year/week 

	SYNTAX:    "RUN samples/addweek.p (INPUT in, OUPUT out)."

	REMARKS:   This code adds or subtract a given number of weeks to
                   a given Year/Wk combination

	PARAMETERS:
          INPUT:  A week-number (For example 199042)                    
          INPUT:  A number of weeks to be added (subtracted if negative) 
          OUTPUT: The resulting week 

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
/*     a given year must fall into this year).                      */


DEFINE INPUT  PARAMETER inweek  AS INT.   /* Input week , eg 199042    */
DEFINE INPUT  PARAMETER numweek AS INT.   /* Number of weeks to be     */
		      		          /* added (or subtracted if   */
				          /* numweek is negative)      */
DEFINE OUTPUT PARAMETER newweek AS INT.   /* Output week, eg 199052    */
DEFINE VARIABLE wdate AS DATE.            /* Work date */

/* Calculate first day of inweek */
RUN samples/begweek.p (INPUT inweek , OUTPUT wdate).    
wdate = wdate + 7 * numweek.
RUN samples/weeknum.p (wdate , OUTPUT newweek).






