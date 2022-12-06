/************************************************************************************
	PROCEDURE: newbase.p

	PURPOSE:   Converts a base 10 number to a different base

	SYNTAX:    RUN samples/newbase.p (INPUT in, OUTPUT out).

	REMARKS:   This code converts a base 10 number to a given base.
	
	PARAMETERS:
            INPUT:  base 10 number
            INPUT:  base to be converted to
            OUTPUT: converted number

	AUTHORS:   Judy Rothermal
	DATE:      February 1993

	LAST INSPECTED:
	INSPECTED BY:

 ************************************************************************************/
 /* Copyright(c) PROGRESS SOFTWARE CORPORATION, 1993 - All Rights Reserved.	    */

/*Code_Start*/


DEFINE INPUT  PARAMETER  dnumber AS INTEGER   NO-UNDO.
DEFINE INPUT  PARAMETER  newbase AS INTEGER   NO-UNDO.
DEFINE OUTPUT PARAMETE   nstring AS CHARACTER NO-UNDO.
DEFINE VARIABLE  r       AS INTEGER   NO-UNDO.
DEFINE VARIABLE  s       AS LOGICAL   NO-UNDO.

/* Take the remainder to find the value of the current position. */
/* If the result is less than ten, return a digit (0..9). */
/* Otherwise, return a letter (A..Z). */

IF newbase < 2 OR newbase > 36 OR 
   newbase = ? OR dnumber = ? THEN
   nstring = ?.
ELSE DO:
   ASSIGN
       nstring = ""
       s       =  dnumber < 0
       dnumber = (IF s THEN - dnumber ELSE dnumber).

   DO WHILE dnumber > 0:
      ASSIGN
         r       = dnumber MODULO newbase
         nstring = CHR(r + IF r < 10 THEN 48 ELSE 55) + nstring
         dnumber = TRUNCATE(dnumber /  newbase,0).
   END.
   IF s THEN nstring = "-" + nstring.
   IF nstring = "" THEN nstring = "0".
END.


