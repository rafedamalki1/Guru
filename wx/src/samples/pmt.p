/************************************************************************************
	PROCEDURE: pmt.p

	PURPOSE:   Calculates payment amount for paying off loan

	SYNTAX:    "RUN samples/pmt.p"

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


/* pmt.p

   This function returns the amount of payment needed to pay off the
   principal based of the life (# of periods) and interest rate.

   The formula used to determine payment is as follows:

       PMT =              PRINCIPAL * INTEREST
			--------------------------
			1 - (1 + INTEREST)^(-LIFE)
*/
/* Code Start */
DEFINE INPUT  PARAMETER principal AS DECIMAL DECIMALS 10.
DEFINE INPUT  PARAMETER interest  AS DECIMAL DECIMALS 10.
DEFINE INPUT  PARAMETER life      AS INTEGER.
DEFINE OUTPUT PARAMETER pmt       AS DECIMAL DECIMALS 10.

pmt = (principal * interest) / (1 - exp((interest + 1),(life * -1))).


