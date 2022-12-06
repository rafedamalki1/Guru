/************************************************************************************
	PROCEDURE: pv.p

	PURPOSE:   Present Value program

	SYNTAX:    "RUN samples/pv.p"

	REMARKS:   

	PARAMETERS:NONE, See loader.p for proclib.ini startup options.

	AUTHORS:   Progress Consulting
	DATE:      February 1993

	LAST INSPECTED:
	INSPECTED BY:

 ************************************************************************************/
 /* Copyright(c) PROGRESS SOFTWARE CORPORATION, 1993 - All Rights Reserved.	    */
 

/*Code_Start*/

/* pv.p            ******   Present Value Function  ******

   This function determines the present value of an annuity based on
   payment amount, interest rate, and life (number of compounding periods)

  The formula used is as follows:
                        1 - (1 + INTEREST) ^ (- LIFE)
       PV =   PAYMENT * -----------------------------
                                  INTEREST
*/

DEFINE INPUT PARAMETER payment  AS DECIMAL DECIMALS 10.
DEFINE INPUT PARAMETER interest AS DECIMAL DECIMALS 10.
DEFINE INPUT PARAMETER life     AS INTEGER.

DEFINE OUTPUT PARAMETER present_value AS DECIMAL DECIMALS 10.

present_value = payment * ( 1 - EXP((1 + interest),(life * (-1))) /
                interest).


