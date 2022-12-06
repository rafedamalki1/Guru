/************************************************************************************
	PROCEDURE: monthnam.p

	PURPOSE:   Returns the name of the month

	SYNTAX:    RUN samples/monthnam.p (INPUT in, OUTPUT out).

	REMARKS:   This code looks up in a comma-delimited list for the 
		   month name 

	PARAMETERS:
            INPUT:  month
            OUTPUT: month in words

	AUTHORS:   Judy Rothermal
	DATE:      February 1993

	LAST INSPECTED:
	INSPECTED BY:

 ************************************************************************************/
 /* Copyright(c) PROGRESS SOFTWARE CORPORATION, 1993 - All Rights Reserved.	    */

 /*Code_Start*/
 
DEF INPUT  PARAMETER in-month   AS INT  NO-UNDO.
DEF OUTPUT PARAMETER month-name AS CHAR NO-UNDO.

month-name = 
ENTRY(in-month,"January,February,March,April,May,June,July,August," +
  "September,October,November,December").


