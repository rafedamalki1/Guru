/************************************************************************************

        PROCEDURE: bigcalc.p



        PURPOSE:   Program for big calculator



        SYNTAX:    "RUN samples/bigcalc.p".



        REMARKS:   This code creates and processes keystrokes for a 

                   calculator.



        PARAMETERS:NONE



        AUTHORS:   Judy Rothermal

        DATE:      February 1993



        LAST INSPECTED:

        INSPECTED BY:



 ************************************************************************************/

 /* Copyright(c) PROGRESS SOFTWARE CORPORATION, 1993 - All Rights Reserved.            */



/*Code_Start*/



DEFINE VARIABLE cannot-do AS LOGICAL INITIAL TRUE NO-UNDO.

DEFINE VARIABLE frame-ok  AS LOGICAL              NO-UNDO.



DEFINE VARIABLE x         AS DECIMAL FORMAT "->>>>>>>>9.9999" INITIAL 0 

                                                  NO-UNDO.

DEFINE VARIABLE y         LIKE x                  NO-UNDO.



DEFINE VARIABLE whole     AS INTEGER INITIAL 0    NO-UNDO.

DEFINE VARIABLE frac      AS DECIMAL INITIAL 0 FORMAT "9.99" NO-UNDO.

DEFINE VARIABLE fact      AS DECIMAL INITIAL 1    NO-UNDO.

DEFINE VARIABLE w_or_f    AS LOGICAL INITIAL TRUE NO-UNDO.

DEFINE VARIABLE x_or_y    AS LOGICAL INITIAL TRUE NO-UNDO.

DEFINE VARIABLE operator  AS INTEGER INITIAL 0    NO-UNDO.

DEFINE VARIABLE pnt       AS CHARACTER            NO-UNDO.

DEFINE BUTTON   btn_exit  LABEL " " SIZE 2 BY 1.



FORM SPACE(22)

  WITH 15 DOWN ROW 1 COLUMN 28 WIDTH 35 OVERLAY FRAME outline.

FORM x
  WITH ROW 2 COLUMN 35 OVERLAY no-label FRAME solve.



FORM " 1 " WITH ROW 5  COLUMN 30 OVERLAY FRAME a1.

FORM " 2 " WITH ROW 5  COLUMN 37 OVERLAY FRAME a2.

FORM " 3 " WITH ROW 5  COLUMN 44 OVERLAY FRAME a3.

FORM " / " WITH ROW 5  COLUMN 51 OVERLAY FRAME b1.

FORM " 4 " WITH ROW 8  COLUMN 30 OVERLAY FRAME a4.

FORM " 5 " WITH ROW 8  COLUMN 37 OVERLAY FRAME a5.

FORM " 6 " WITH ROW 8  COLUMN 44 OVERLAY FRAME a6.

FORM " * " WITH ROW 8  COLUMN 51 OVERLAY FRAME b2.

FORM " 7 " WITH ROW 11 COLUMN 30 OVERLAY FRAME a7.

FORM " 8 " WITH ROW 11 COLUMN 37 OVERLAY FRAME a8.

FORM " 9 " WITH ROW 11 COLUMN 44 OVERLAY FRAME a9.

FORM " - " WITH ROW 11 COLUMN 51 OVERLAY FRAME b3.

FORM " 0 " WITH ROW 14 COLUMN 30 OVERLAY FRAME a0.

FORM " . " WITH ROW 14 COLUMN 37 OVERLAY FRAME b4.

FORM " = " WITH ROW 14 COLUMN 44 OVERLAY FRAME b5.

FORM " + " WITH ROW 14 COLUMN 51 OVERLAY FRAME b6.



pnt = SUBSTR(STRING(1.1),2,1).



PAUSE 0 BEFORE-HIDE.

/*-------------------------------------------------------------*/



FORM btn_exit AT ROW 1 COLUMN 1 WITH FRAME outline.

ENABLE btn_exit WITH FRAME outline.

APPLY "ENTRY" TO btn_exit IN FRAME outline.



VIEW FRAME outline. 

VIEW FRAME btn.

DISPLAY x WITH FRAME solve.

VIEW FRAME a1. VIEW FRAME a2. VIEW FRAME a3. VIEW FRAME b1.

VIEW FRAME a4. VIEW FRAME a5. VIEW FRAME a6. VIEW FRAME b2.

VIEW FRAME a7. VIEW FRAME a8. VIEW FRAME a9. VIEW FRAME b3.

VIEW FRAME a0. VIEW FRAME b4. VIEW FRAME b5. VIEW FRAME b6.



DO ON ERROR UNDO,LEAVE:

  ASSIGN

     x = DECIMAL(FRAME-VALUE)

     cannot-do = FALSE.

END.

HIDE MESSAGE NO-PAUSE.



frame-ok = FRAME solve:MOVE-TO-TOP() = yes.

DISPLAY x WITH FRAME solve.



REPEAT ON ENDKEY UNDO, LEAVE:

  READKEY.

  

  IF KEYFUNCTION(LASTKEY) = "END-ERROR" OR

     KEYLABEL(LASTKEY) = "ALT-F4" THEN LEAVE.

     

  IF INDEX("+-*/=",CHR(LASTKEY)) > 0 THEN DO:

    IF NOT x_or_y OR LASTKEY = KEYCODE("=") THEN DO:

      IF      operator = KEYCODE("+") THEN x = x + y.

      ELSE IF operator = KEYCODE("-") THEN x = x - y.

      ELSE IF operator = KEYCODE("*") THEN x = x * y.

      ELSE IF operator = KEYCODE("/") THEN x = x / y.

      DISPLAY x WITH FRAME solve.

      IF LASTKEY = KEYCODE("=") THEN DO:

        PAUSE.

        LEAVE.

      END. /* lastkey = "=" */

    END.   /* NOT x_or_y OR lastkey = "=" */

    ASSIGN

       operator = LASTKEY

       whole    = 0

       fact     = 1

       frac     = 0

       w_or_f   = TRUE

       x_or_y   = FALSE.

  END. /* IF DO */

  ELSE

  IF INDEX("0123456789" + pnt , CHR(LASTKEY)) > 0 THEN DO:

    IF LASTKEY <> KEYCODE(pnt) AND  w_or_f THEN

      whole = whole * 10 + INTEGER(CHR(LASTKEY)).

    ELSE

    IF LASTKEY <> KEYCODE(pnt) THEN  /* entering fractional portion */

       ASSIGN

          fact = fact * 0.1

          frac = frac + INTEGER(CHR(LASTKEY)) * fact.

    ELSE

      w_or_f = FALSE.



    IF x_or_y THEN DO:

      x = whole + frac.

      DISPLAY x WITH FRAME solve.

    END. /* IF DO*/

    ELSE DO:

      y = whole + frac.

      DISPLAY y @ x WITH FRAME solve.

    END.

  END. /* end of '0'-'9' or '.' block */



END.

/*
HIDE FRAME outline. HIDE FRAME solve.
*/

HIDE FRAME a1. HIDE FRAME a2. HIDE FRAME a3. HIDE FRAME b1.

HIDE FRAME a4. HIDE FRAME a5. HIDE FRAME a6. HIDE FRAME b2.

HIDE FRAME a7. HIDE FRAME a8. HIDE FRAME a9. HIDE FRAME b3.

HIDE FRAME a0. HIDE FRAME b4. HIDE FRAME b5. HIDE FRAME b6.

HIDE FRAME solve. HIDE FRAME outline.

RETURN.













