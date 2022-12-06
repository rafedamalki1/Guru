/************************************************************************************
	PROCEDURE: worddate.p

	PURPOSE:   Expands a date into mmmm dddth, yyyy format

	SYNTAX:    RUN samples/worddate.p (INPUT in, OUTPUT out).

	REMARKS:   This code expands a date from DATE format to CHARACTER
		   format in "mmmm dddth, yyyy" format

	PARAMETERS:
            INPUT:  date
            OUTPUT: character string

	AUTHORS:   Judy Rothermal
	DATE:      February 1993

	LAST INSPECTED:
	INSPECTED BY:

 ************************************************************************************/
 /* Copyright(c) PROGRESS SOFTWARE CORPORATION, 1993 - All Rights Reserved.	    */
/*Code_Start*/

DEF INPUT  PARAMETER in-date   AS DATE                NO-UNDO.
DEF OUTPUT PARAMETER word-date AS CHAR FORMAT "X(30)" NO-UNDO.

word-date =
   ENTRY(MONTH(in-date),"January,February,March,April,May,June," +
            "July,August,September,October,November,December")   + " " +
    STRING(DAY(in-date),
    IF DAY(in-date) < 10 THEN "9" ELSE "99") +       
SUBSTR("stndrdthththththththththththththththththstndrdthththththththst",
  DAY(in-date) * 2 - 1,2)
       + ", " + STRING(YEAR(in-date),"9999").








