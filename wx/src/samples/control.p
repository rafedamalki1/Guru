/*
UNLOAD-HEADER   "Programming Utilities"
UNLOAD-LIBRARY  "tools"
UNLOAD-FILENAME "control.p"
UNLOAD-CATEGORY ""
UNLOAD-DESCRIPT "Converts control codes into character string."
UNLOAD-CHILDREN ""
*/
/* 	program :       control.p
	date    :       March 5, 1987
	author  :       Steven J. Feinstein
			Product Services

This is a conversion utility to put control codes into a character string.

If you are distributing an application that is going to be used with
different printers, it is impossible to maintain all the different codes
for all the different font attributes for all the different printers.

It would be advisable to write a maintenance program to enter the proper
codes into a system control file.  Unfortunately, you can not just use
a normal UPDATE to enter the codes.
PROGRESS would interpret the sequence "~033[C" as the character string:
	 [TILDE][ZERO][THREE][THREE][LEFT-BRACKET][CAPITAL C]
instead of the proper code sequence of:
		[ESCAPE][LEFT-BRACKET][CAPITAL C]

This utility can be used as a front-end for an application that maintains
a list of printers and their corresponding control codes for controlling
different features of the printer.

Many printer control code sequences contain characters that can be represented
by their octal equivalent.  To distinguish these octal codes, you must precede
the three octal digits by an escape character.  If you are using DOS, the
escape character is the [TILDE] "~".  If you are using UNIX, the escape
character is either the [TILDE] "~" or a [BACKSLASH] "\".

USAGE:

    The way to use this program is to enter the control code sequence
    as a string of normal characters.

    For example, let's say that the proper control code sequence for
    starting ITALIC printing is: ESC G 3.
    The proper input for this program would be:
	~033G3 (for DOS machines) or \033 (for UNIX machines)
    where ~033 or \033 is the octal equivalent for [ESCAPE] (ASCII 27).

    The control code sequence will be displayed and ask for confirmation
    before leaving the routine.  The variable OUTFIELD will contain the
    proper control code sequence.

    To enter control codes use the carat character (^) to precede the
    control code that you want.  (^Y or ^y for [CONTROL-Y])

    N.B. If you need to use the carat character within a control code
	 sequence, like ESC ^, then use the octal equivalent code for
	 the carat (\136 or ~136).  Therefore ESC ^ becomes \033\136.

SPECIAL NOTE:

    PROGRESS does not recognize the NULL character.  Internally, PROGRESS
    uses the NULL character to terminate a character string.  Because of this,
    you can not store a NULL character in a field or variable or even send
    the NULL character to any device.


IMPORTANT VARIABLES:

    INPUT:  infield     chr x(40)   The input field
   OUTPUT:  outfield    chr x(40)   The result variable
	    dispstr     chr x(65)   The display sequence for the screen

METHOD OF MADNESS:

    Check one character at a time and validate the expression as you go.
    If, for example, you find a carat (^) then make sure another character
    follows it.  Then make sure it is a valid control chracter.  If you
    found a backslash or tilde, indicating an octal representation of the
    code is following, then make sure there are three (3) more characters
    following and then make sure they are all valid octal digits (0 thru 7).
    After validating the character, convert it to the proper ASCII equivalent
    and append it to OUTFIELD.  At the same time, build the string for
    displaying purposes.

DEBUGGING HELP:

    There are two ways to make sure the proper control codes were entered
    into OUTFIELD.  You can display outfield to the screen by using
			PUT SCREEN outfield.
    Or you can display outfield to a file and then use OD (if you are on
    unix) or NORTON UTILITIES (if you are on DOS) to check out the
    control codes.
*/

DEFINE VARIABLE infield    AS CHARACTER FORMAT "x(40)".
DEFINE VARIABLE chkchr     AS CHARACTER FORMAT "x(1)".
DEFINE VARIABLE outfield   AS CHARACTER FORMAT "x(40)".
DEFINE VARIABLE dispstr    AS CHARACTER FORMAT "x(65)".
DEFINE VARIABLE val        AS INTEGER.
DEFINE VARIABLE octnum     AS CHARACTER FORMAT "x(3)".
DEFINE VARIABLE okay       AS LOGICAL INITIAL FALSE.
DEFINE VARIABLE tmpval     AS INTEGER.
DEFINE VARIABLE i          AS INTEGER.
DEFINE VARIABLE j          AS INTEGER.
DEFINE VARIABLE in-row     AS INTEGER INITIAL 2.
DEFINE VARIABLE out-row    AS INTEGER INITIAL 7.
DEFINE VARIABLE err-row    AS INTEGER INITIAL 5.  /* in-row + 3 */
DEFINE VARIABLE err-ptr    AS CHARACTER FORMAT "x(1)" INITIAL "^".
DEFINE VARIABLE err-msg    AS CHARACTER FORMAT "x(5)" INITIAL "ERROR".
DEFINE VARIABLE err-disp   AS CHARACTER FORMAT "x(40)".
DEFINE VARIABLE err2-disp  AS CHARACTER FORMAT "x(44)".
DEFINE VARIABLE err-offset AS INTEGER INITIAL 18.
DEFINE VARIABLE more-2-do  AS LOGICAL INITIAL TRUE.

FORM err-disp
     WITH FRAME err-frame NO-LABEL NO-BOX
	  ROW err-row CENTERED.

FORM err-msg
     WITH FRAME err2-frame NO-LABEL NO-BOX
	  ROW err-row + 1 COL err-offset + i + j.

OUTERLOOP:
REPEAT WHILE more-2-do:

  MAINLOOP:
  DO WHILE NOT okay ON ERROR UNDO, LEAVE:

      UPDATE infield LABEL "      ENTER CONTROL CODE SEQUENCE"
	 WITH FRAME in-str NO-BOX CENTERED ROW in-row.

      outfield = "".
      dispstr  = "".
      err-disp = "".

      COLOR DISPLAY MESSAGES err-msg WITH FRAME err2-frame.

      CHK-INPUT:
      DO i = 1 TO LENGTH(infield):
	  chkchr = SUBSTRING(infield,i,1).
	  IF chkchr = "^" THEN DO:   /* check for control code */
	      IF i = LENGTH(infield) THEN DO:
		  err-disp = FILL(" ",i - 1) + err-ptr.
		  DISPLAY err-disp WITH FRAME err-frame.
		  DISPLAY err-msg WITH FRAME err2-frame.
		  MESSAGE "ERROR -- INCORRECT LENGTH FOR CONTROL CODE".
		  PAUSE MESSAGE "PRESS SPACE BAR TO CONTINUE".
		  HIDE FRAME err-frame.
		  HIDE FRAME err2-frame.
		  NEXT MAINLOOP.
	      END. /* if i = length(infield) */
	      i = i + 1.
	      chkchr = SUBSTRING(infield,i,1).
	      val = ASC(chkchr).

	      /*
		 make sure you have a valid control character:
		 CONTROL-A thru CONTROL-_ or CONTROL-a thru CONTROL-(tilde)
		 ASCII 65  thru ASCII 95  or ASCII 97  thru ASCII 126
	      */

	      IF val >= 65 AND val <= 95 THEN
		  outfield = outfield + CHR(val - 64).
	      ELSE
	      IF val >= 97 AND val <= 126 THEN
		  outfield = outfield + CHR(val - 96).
	      ELSE DO:
		  err-disp = FILL(" ",i - 1) + err-ptr.
		  DISPLAY err-disp WITH FRAME err-frame.
		  DISPLAY err-msg WITH FRAME err2-frame.
		  MESSAGE "ERROR -- NOT A VALID CONTROL CHARACTER".
		  PAUSE MESSAGE "PRESS SPACE BAR TO CONTINUE".
		  HIDE FRAME err-frame.
		  HIDE FRAME err2-frame.
		  NEXT MAINLOOP.
	      END.
	      dispstr = dispstr + "[CONTROL-" + CAPS(CHR(val)) + "] ".
	  END.  /* if chkchr = "^" */

	  ELSE         /* check for an octal code */
	  IF chkchr = "\\" OR chkchr = "~~" THEN DO:
	      /*
		Two (2) backslashes so PROGRESS will look for one (1)
		  (same with the tilde character)
	      */
	      IF i + 3 > LENGTH(infield) THEN DO:
		  err-disp = FILL(" ",i - 1) + err-ptr.
		  DISPLAY err-disp WITH FRAME err-frame.
		  DISPLAY err-msg WITH FRAME err2-frame.
		  MESSAGE "ERROR -- INCORRECT LENGTH FOR OCTAL CODE".
		  PAUSE MESSAGE "PRESS SPACE BAR TO CONTINUE".
		  HIDE FRAME err-frame.
		  HIDE FRAME err2-frame.
		  NEXT MAINLOOP.
	      END. /* if i + 3 > length(infield) */
	      val = 0.
	      DO j = 1 TO 3:
		  tmpval = ASC(SUBSTRING(infield,i + j,1)).
		  /* VALID OCTAL DIGITS ARE 0 thru 7 (ASCII 48 thru ASCII 55)*/
		  IF tmpval < 48 OR tmpval > 55 THEN DO:
		      err-disp = FILL(" ",i + j - 1) + err-ptr.
		      DISPLAY err-disp WITH FRAME err-frame.
		      DISPLAY err-msg WITH FRAME err2-frame.
		      MESSAGE "ERROR -- NOT A VALID OCTAL DIGIT".
		      PAUSE MESSAGE "PRESS SPACE BAR TO CONTINUE".
		      HIDE FRAME err-frame.
		      HIDE FRAME err2-frame.
		      NEXT MAINLOOP.
		  END.  /* if tmpval < 48 or tmpval > 55 */

		  val = val * 8 + (tmpval - 48).  /* octal conversion */

	      END. /* do j = 1 to 3 */
	      j = 0.  /* reset j for frame err2-frame. */
	      outfield = outfield + CHR(val).
	      octnum = SUBSTRING(infield,i + 1, 3).
	      i = i + 3.
	      IF val = 0 THEN DO:
		   err-disp = FILL(" ",i - 3) + err-ptr + err-ptr + err-ptr.
		   DISPLAY err-disp WITH FRAME err-frame.
		   DISPLAY err-msg WITH FRAME err2-frame.
		   MESSAGE "ERROR -- PROGRESS DOES NOT RECOGNIZE THE NULL CHAR".
		   PAUSE MESSAGE "PRESS SPACE BAR TO CONTINUE".
		   HIDE FRAME err-frame.
		   HIDE FRAME err2-frame.
		   NEXT MAINLOOP.
	      END.  /* if val = 0 (NULL CHAR)  */

	      IF val = 27 THEN
		 dispstr = dispstr + "[ESCAPE] ".
	      ELSE
	      IF val = 32 THEN
		 dispstr = dispstr + "[BLANK] ".
	      ELSE
	      IF val <= 31 OR val >= 127 THEN
		 dispstr = dispstr + "[NON-PRINTABLE CHAR(" + octnum + ")] ".
	      ELSE
		 dispstr = dispstr + CHR(val) + " ".
	  END.  /* if chkchr = "\\" or chkchr = "~~" */
	  ELSE DO:  /* character is an ordinary character */
	      outfield = outfield + chkchr.
	      IF chkchr = " " THEN
		  dispstr = dispstr + "[BLANK] ".
	      ELSE
		  dispstr = dispstr + chkchr + " ".
	  END.
      END. /* CHK-INPUT */
      DISPLAY dispstr LABEL "                 FINISHED CONTROL CODE SEQUENCE"
	  WITH FRAME out-str NO-BOX CENTERED ROW out-row.
      okay = TRUE.
      MESSAGE "IS THIS THE CONTROL CODE SEQUENCE YOU ENTERED ?" UPDATE okay.
      IF NOT okay THEN DO:
	  outfield = "".
	  dispstr = "".
	  HIDE FRAME in-str.
	  HIDE FRAME out-str.
      END.
  END. /* MAINLOOP */

  /*
    At this point the variable OUTFIELD contains the proper
    control code sequence that was entered.

    It can be assigned to the field in the database that holds the
    printer control code sequence.
  */

  HIDE MESSAGE.
  more-2-do = FALSE.
  MESSAGE "WOULD YOU LIKE TO ENTER ANOTHER CONTROL CODE SEQUENCE?"
       UPDATE more-2-do.
  okay = FALSE.
  outfield = "".
  dispstr = "".
  HIDE FRAME in-str.
  HIDE FRAME out-str.
END. /* OUTERLOOP */
