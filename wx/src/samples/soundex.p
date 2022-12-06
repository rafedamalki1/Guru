/************************************************************************************
	PROCEDURE: soundex.p

	PURPOSE:   Converts a character string to a code to better
                   search on character strings

	SYNTAX:    RUN samples/soundex.p (INPUT in, OUTPUT out).

	REMARKS:   This code converts a character string so that phonetic
		   searches can be done.

	PARAMETERS:
            INPUT:  character string
            OUTPUT: phonetic code

	AUTHORS:   Judy Rothermal
	DATE:      February 1993

	LAST INSPECTED:
	INSPECTED BY:

 ************************************************************************************/
 /* Copyright(c) PROGRESS SOFTWARE CORPORATION, 1993 - All Rights Reserved.	    */

/*Code_Start*/

/*
The soundex code is a useful function for finding names phonetically.
The best use of this would be to have a file with an non-unique indexed
field called Soundex, and to place into this field the soundex code for
the name.  Then, when the user inputs a name, convert that input to a
soundex code, and do the find on that.  This will narrow down the
choices considerably, and the user can then choose from the resulting
list.
Note that better algorithms exist for phonetic matching then soundex,
(for example, 'PH' and 'F' should be equivalent, 'KN' at the beginning
of words should match 'N', etc.) but soundex is popular both for its
speed and simplicity, and also the fact that it works well most of the
time.
*/

DEFINE INPUT  PARAMETER name AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER code AS CHARACTER NO-UNDO.

DEFINE VARIABLE e AS INTEGER   NO-UNDO.
DEFINE VARIABLE i AS INTEGER   NO-UNDO.
DEFINE VARIABLE k AS CHARACTER NO-UNDO.
DEFINE VARIABLE l AS CHARACTER NO-UNDO.

ASSIGN  
  l    = ""
  name = CAPS(name)
  code = SUBSTRING(name,1,1).
DO i = 2 TO LENGTH(name):
  e = ASC(SUBSTRING(name,i,1)) - 64.
  IF e >= 1 AND e <= 26 THEN DO:
    k = SUBSTRING("01230120022455012623010202",e,1).
    IF k <> l AND k <> "0" THEN code = code + k.
    IF LENGTH(code) > 3 THEN LEAVE.
  END.
  l = k.
END.
code = SUBSTRING(code + "000",1,4).
RETURN.



