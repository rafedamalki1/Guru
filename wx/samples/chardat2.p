/************************************************************************************
	PROCEDURE: chardat2.p

	PURPOSE:   Converts character date to DATE format regardless if
                      date if 99/99/99 or 99/99/9999 format

	SYNTAX:    "RUN samples/chardat2.p (INPUT in, OUTPUT out)".

	REMARKS:   This code will convert a date in a character field to DATE format
		   regardless of the DATE format being 99/99/99 or 99/99/9999.

	PARAMETERS:
              INPUT  in-date  - Character field to be converted
              OUTPUT out-date - Date formatted variable

	AUTHORS:   George Kassabgi
	DATE:      February 1993

	LAST INSPECTED:
	INSPECTED BY:

 ************************************************************************************/
 /* Copyright(c) PROGRESS SOFTWARE CORPORATION, 1993 - All Rights Reserved.	    */
 
/*Code_Start*/


DEF INPUT  PARAMETER in-date  AS CHAR FORMAT "x(10)" NO-UNDO.
DEF OUTPUT PARAMETER out-date AS DATE                NO-UNDO.

out-date = 
   DATE( INTEGER(SUBSTR(in-date,1,2)),
         INTEGER(SUBSTR(in-date,4,2)),
         IF INTEGER(SUBSTR(in-date,7)) < 100
            THEN INTEGER(SUBSTR(in-date,7)) + 1900
         ELSE INTEGER(SUBSTR(in-date,7))).



