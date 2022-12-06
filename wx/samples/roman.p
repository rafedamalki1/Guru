/************************************************************************************
	PROCEDURE: roman.p

	PURPOSE:   Converts an Arabic number to Roman Numerals

	SYNTAX:    RUN samples/roman.p (INPUT in, OUTPUT out).

	REMARKS:   This code converts an arabic number to roman numerals.

	PARAMETERS:
            INPUT:  arabic number  - integer
            OUTPUT: roman numerals - character string

	AUTHORS:   Judy Rothermal
	DATE:      February 1993

	LAST INSPECTED:
	INSPECTED BY:

 ************************************************************************************/
 /* Copyright(c) PROGRESS SOFTWARE CORPORATION, 1993 - All Rights Reserved.	    */
 
/*Code_Start*/
/* M=1000, D=500, C=100, L=50, X=10, V=5, I=1 */

DEFINE INPUT  PARAMETER arabic AS INTEGER   NO-UNDO.
DEFINE OUTPUT PARAMETER roman  AS CHARACTER NO-UNDO.

DEFINE VARIABLE  i      AS INTEGER   NO-UNDO.
DEFINE VARIABLE  j      AS INTEGER   NO-UNDO.
DEFINE VARIABLE  k      AS INTEGER   NO-UNDO.

ASSIGN
   roman = "".
IF arabic < 1 OR arabic > 3999 OR arabic = ? THEN roman = ?.
ELSE 
DO j = 1 TO 4:
   k = EXP(10,4 - j).
   IF arabic >= 9 * k THEN 
      ASSIGN
         roman  = roman + ENTRY(j,",CM,XC,IX")
         arabic = arabic - 9 * k.
   IF arabic >= 5 * k THEN 
      ASSIGN
         roman  = roman + ENTRY(j,",D,L,V")
         arabic = arabic - 5 * k.
   IF arabic >= 4 * k THEN 
      ASSIGN
         roman  = roman + ENTRY(j,",CD,XL,IV")
         arabic = arabic - 4 * k.
   IF arabic >= k THEN 
      ASSIGN
         i      = TRUNCATE(arabic / k,0)
         roman  = roman + FILL(ENTRY(j,"M,C,X,I"),i)
         arabic = arabic - i * k.
END.



