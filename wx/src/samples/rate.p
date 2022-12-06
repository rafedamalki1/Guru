/************************************************************************************
	PROCEDURE: rate.p

	PURPOSE:   Rate calculations

	SYNTAX:    "RUN samples/rate.p"

	REMARKS:   

	PARAMETERS:NONE, See loader.p for proclib.ini startup options.

	AUTHORS:   Progress Consulting
	DATE:      February 1993

	LAST INSPECTED:
	INSPECTED BY:

 ************************************************************************************/
 /* Copyright(c) PROGRESS SOFTWARE CORPORATION, 1993 - All Rights Reserved.	    */
 
/*Code_Start*/


/* rate.p        **** Periodic Interest Rate Function ***

   This function returns the periodic interest rate required to return the
   future value (FUTURE) based on the present value (PRESENT) and the TERM
   (number of compounding periods).

*/

DEFINE INPUT  PARAMETER present_value AS DECIMAL DECIMALS 10.
DEFINE INPUT  PARAMETER future_value  AS DECIMAL DECIMALS 10.
DEFINE INPUT  PARAMETER term_         AS INTEGER.

DEFINE OUTPUT PARAMETER rate          AS DECIMAL DECIMALS 10.

rate = EXP( (future_value / present_value), (1 / term_) ) - 1.


