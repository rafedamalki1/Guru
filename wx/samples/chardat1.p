/************************************************************************************
	PROCEDURE: chardat1.p

	PURPOSE:   Converts a character date to DATE format

	SYNTAX:    "RUN samples/chardat1.p (INPUT in, OUTPUT out)".

	REMARKS:   This code will convert a character date to the DATE format.

	PARAMETERS:
              INPUT  in-date  - Character format to be converted
              OUTPUT out-date - DATE format variable

	AUTHORS:   Progress Software Consulting
	DATE:      February 1993

	LAST INSPECTED:
	INSPECTED BY:

 ************************************************************************************/
 /* Copyright(c) PROGRESS SOFTWARE CORPORATION, 1993 - All Rights Reserved.	    */
 
/*Code_Start*/

DEF INPUT  PARAMETER in-date  AS CHAR FORMAT "x(10)" NO-UNDO.
DEF OUTPUT PARAMETER out-date AS DATE NO-UNDO.
   
out-date = 
   DATE( 
     INTEGER(SUBSTR(in-date , 
       INDEX(STRING(DATE(2,1,1903)) , "02") ,2)),
     INTEGER(SUBSTR(in-date , 
       INDEX(STRING(DATE(2,1,1903)) , "01") ,2)),
     INTEGER(SUBSTR(in-date , 
       INDEX(STRING(DATE(2,1,1903)) , "03") ,2)) + 1900).




