/************************************************************************************
	PROCEDURE: wordnum.p

	PURPOSE:   Converts a decimal value into words

	SYNTAX:    RUN samples/wordnum.p (INPUT in, INPUT offset, OUTPUT out).

	REMARKS:   This code converts a decimal value into words
		 

	PARAMETERS:
            INPUT:  string conversion of a decimal number
            INPUT:  offset into string
            INPUT-OUTPUT: character string of the number in words

	AUTHORS:   Judy Rothermal
	DATE:      February 1993

	LAST INSPECTED:
	INSPECTED BY:

 ************************************************************************************/
 /* Copyright(c) PROGRESS SOFTWARE CORPORATION, 1993 - All Rights Reserved.	    */

/*Code_Start*/

DEF INPUT        PARAMETER word-num AS CHAR NO-UNDO.
DEF INPUT        PARAMETER offset   AS INT  NO-UNDO.
DEF INPUT-OUTPUT PARAMETER words    AS CHAR NO-UNDO.

DEFINE VARIABLE word-lst AS CHARACTER NO-UNDO INITIAL
  "One,Two,Three,Four,Five,Six,Seven,Eight,Nine".
DEFINE VARIABLE word-ten AS CHARACTER NO-UNDO.

words = (IF words = "" THEN "" ELSE words) +
      (IF INTEGER(SUBSTR(word-num,offset,1)) = 0 THEN "" ELSE (
        ENTRY(INTEGER(SUBSTR(word-num,offset,1)),word-lst) + " Hundred")).
IF INTEGER(SUBSTR(word-num,offset + 1,2)) = 0 THEN
  word-ten = "".
ELSE 
IF INTEGER(SUBSTR(word-num,offset + 1,1)) = 0 THEN
  word-ten = ENTRY(INTEGER(SUBSTR(word-num,offset + 2,1)),word-lst).
ELSE 
IF SUBSTR(word-num,offset + 1,1) = "1" THEN
  word-ten = ENTRY(INTEGER(SUBSTR(word-num,offset + 2,1)) + 1,
             "Ten,Eleven,Twelve,Thirteen,Fourteen,Fifteen,Sixteen,"
               + "Seventeen,Eighteen,Nineteen").
ELSE
  word-ten = ENTRY(INTEGER(SUBSTR(word-num,offset + 1,1)) - 1,
               "Twenty,Thirty,Forty,Fifty,Sixty,Seventy,Eighty,Ninety")
          + (IF INTEGER(SUBSTR(word-num,offset + 2,1)) = 0 THEN "" ELSE
            ("-" + ENTRY(INTEGER(SUBSTR(word-num,offset + 2,1)),word-lst))).
words = (IF words = "" THEN "" ELSE (words + " ")) + word-ten.



