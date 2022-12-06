/************************************************************************************
	PROCEDURE: flip.p

	PURPOSE:   Flips words in front to the end

	SYNTAX:    RUN samples/flip.p (INPUT in, OUTPUT out).

	REMARKS:   This code will move the first words to the end based on
                   the position of a semi-colon, comma, or the first space.

	PARAMETERS:
            INPUT:  character value to be flipped
            OUTPUT: Flipped value

	AUTHORS:   Judy Rothermal
	DATE:      February 1993

	LAST INSPECTED:
	INSPECTED BY:

 ************************************************************************************/
 /* Copyright(c) PROGRESS SOFTWARE CORPORATION, 1993 - All Rights Reserved.	    */
 
/*Code_Start*/
 
/*
This is a summary of the rules that "flip.i" uses to flip names.

When this routine is called, the first word in the name is moved
to the end of the name.  This works in the following way:

  The computer first searches for a semicolon ";".
  If present, the name is flipped at that point.
  If not, but a comma "," is, then the name is flipped there.
  Otherwise, it flips at the first space in the name.

Here are some examples:

  Wright, Steven         flips to    Steven Wright
  Wright Jr., Richard    flips to    Richard Wright Jr.
  Wright Steven          flips to    Steven Wright
  Wright Jr. Richard     flips to    Jr. Richard Wright (oops)

A good use for the semicolon is in the case of a corporation, when the
company name begins with a "The" but you do not want to have to find it by
typing "The ...," as in the following example:

  Greasy Spoon, Inc.; The    flips to    The Greasy Spoon, Inc.

The flipping feature allows "Doe, John" to be printed as "John Doe" on any
correspondance.  It is recommended that an additional variable be added to
the client record which indicates whether or not the name should in fact be
flipped.
*/

DEF VAR i  AS INT NO-UNDO.

DEF INPUT  PARAMETER in-name  AS CHAR FORMAT "x(30)" NO-UNDO.
DEF OUTPUT PARAMETER out-name AS CHAR FORMAT "x(30)" NO-UNDO.

  ASSIGN
    i   = INDEX(in-name,";")
    i   = (IF i > 0 THEN i ELSE INDEX(in-name,","))
    i   = (IF i > 0 THEN i ELSE INDEX(in-name," "))
    out-name = (IF i < 2 THEN in-name
           ELSE SUBSTR(in-name,i + 1) + " " + SUBSTR(in-name,1,i - 1)).

  /* peel off extra characters from name start and end */
  DO WHILE INDEX(" ,;",SUBSTR(out-name,1,1)) > 0 AND LENGTH(out-name) > 1:
    out-name = SUBSTR(out-name,2).
  END.
  DO WHILE INDEX(" ,;",SUBSTR(out-name,LENGTH(out-name),1)) > 1 AND 
     LENGTH(out-name) > 1:
    out-name = SUBSTR(out-name,1,LENGTH(out-name) - 1).
  END.




