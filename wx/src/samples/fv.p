/************************************************************************************
	PROCEDURE: fv.p

	PURPOSE:   Future Value

	SYNTAX:    "RUN samples/fv.p (INPUT pmt, INPUT int, INPUT life,
                                      OUTPUT result)"

	REMARKS:   This code handles menu options and selections within the selection
		   lists for topics and procedures.
		   Internal procedure on_off enables and disables widgets to handle
		   execution of modal subroutines.

	PARAMETERS:NONE, See loader.p for proclib.ini startup options.

	AUTHORS:   Progress Consulting
	DATE:      February 1993

	LAST INSPECTED:
	INSPECTED BY:

 ************************************************************************************/
 /* Copyright(c) PROGRESS SOFTWARE CORPORATION, 1993 - All Rights Reserved.	    */
 
/*Code_Start*/

/* fv.p            ******   Future Value Function  ******

   This function returns the future value of an annuity based on the
   payment amount, interest rate, and life (# of compounding periods).

     The formula used is as follows:

        FV = PAYMENT * (1 + INTEREST)^(LIFE) - 1
                      ---------------------------
                                INTEREST
*/

DEFINE INPUT  PARAMETER payment  AS DECIMAL DECIMALS 10.
DEFINE INPUT  PARAMETER interest AS DECIMAL DECIMALS 10.
DEFINE INPUT  PARAMETER life     AS INTEGER.
DEFINE OUTPUT PARAMETER result   AS DECIMAL DECIMALS 10.

result = payment * ((exp(1 + interest, life) - 1) / interest).

