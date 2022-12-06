/************************************************************************************

        PROCEDURE: amort.p



        PURPOSE:   Main program shows an amortization schedule



        SYNTAX:    "RUN amort.p"



        REMARKS:   



        PARAMETERS:NONE, See loader.p for proclib.ini startup options.



        AUTHORS:   Progress Software

        DATE:      March 1993



        LAST INSPECTED:

        INSPECTED BY:



 ************************************************************************************/

 /* Copyright(c) PROGRESS SOFTWARE CORPORATION, 1993 - All Rights Reserved.            */

 



/*Code_Start*/



/* Loan Amortization Calculation */



DEFINE VARIABLE bpb  AS DECIMAL           DECIMALS 2 NO-UNDO.

DEFINE VARIABLE dat  AS DATE    INITIAL TODAY        NO-UNDO.

DEFINE VARIABLE i    AS INTEGER                      NO-UNDO.

DEFINE VARIABLE incr AS INTEGER INITIAL 1            NO-UNDO

  VIEW-AS RADIO-SET HORIZONTAL

/*71C* ERROR: VIEW-AS RADIO-SET syntax has changed */

    RADIO-BUTTONS "Monthly", 1,

                  "Bi-Monthly", 2,

                  "Quarterly", 3,

                  "Yearly", 4.

DEFINE VARIABLE intr AS DECIMAL INITIAL 0 DECIMALS 2 NO-UNDO.

DEFINE VARIABLE inty AS DECIMAL           DECIMALS 10 NO-UNDO.

DEFINE VARIABLE j    AS INTEGER                      NO-UNDO.

DEFINE VARIABLE jd   AS INTEGER                      NO-UNDO.

DEFINE VARIABLE jm   AS INTEGER                      NO-UNDO.

DEFINE VARIABLE k    AS CHARACTER                    NO-UNDO.

DEFINE VARIABLE msg  AS CHARACTER EXTENT 16          NO-UNDO.

DEFINE VARIABLE paid AS DECIMAL           DECIMALS 2 NO-UNDO.

DEFINE VARIABLE pmt  AS DECIMAL           DECIMALS 2 NO-UNDO.

DEFINE VARIABLE pri  AS DECIMAL INITIAL 0 DECIMALS 2 NO-UNDO.

DEFINE VARIABLE rpb  AS DECIMAL           DECIMALS 2 NO-UNDO.

DEFINE VARIABLE tday AS INTEGER                      NO-UNDO.

DEFINE VARIABLE trm  AS INTEGER INITIAL 0            NO-UNDO

   VIEW-AS SLIDER MAX-VALUE 100 MIN-VALUE 1 HORIZONTAL.

DEFINE VARIABLE trm-ch AS INTEGER INITIAL 0 FORMAT ">>9" NO-UNDO.

DEFINE VARIABLE ttl1 AS CHARACTER                    NO-UNDO.

DEFINE VARIABLE ttl2 AS CHARACTER                    NO-UNDO.

DEFINE VARIABLE ttl3 AS CHARACTER                    NO-UNDO.

DEFINE VARIABLE ydat AS DATE                         NO-UNDO.

DEFINE VARIABLE yint AS DECIMAL EXTENT 99 DECIMALS 2 NO-UNDO.

DEFINE VARIABLE ypri AS DECIMAL EXTENT 99 DECIMALS 2 NO-UNDO.



DEFINE BUTTON btn-OK   LABEL "OK"   AUTO-GO.

DEFINE BUTTON btn-EXIT LABEL "EXIT" AUTO-ENDKEY.



FORM SKIP(1)

  " Principal Amount     " pri  AT 25 FORMAT "->>>,>>>.99" SKIP

  " Annual Interest Rate " intr AT 25 FORMAT ">>>.99%" SKIP

  " Number of Payments "   trm  AT 25  NO-LABEL

  SKIP(1)

  " Method of Payment    " incr AT 25 NO-LABEL

  SKIP(1)

  " Starting Date        " dat  AT 25 FORMAT "99/99/9999" SKIP

  " Amortization Title   " ttl1 AT 25 FORMAT "x(40)" SPACE(1) SKIP

  ttl2 AT 25 FORMAT "x(40)" SKIP(1)

  btn-OK AT 20 SPACE(5) btn-EXIT

  WITH FRAME loan1 TITLE "Amortization Table Calculator" 

  NO-LABELS OVERLAY CENTERED ROW 2 VIEW-AS DIALOG-BOX.



ON CHOOSE OF btn-OK IN FRAME loan1 DO:

   IF INPUT pri = 0 THEN DO:
      MESSAGE "Principal amount cannot be 0. Please re-enter."
        VIEW-AS ALERT-BOX WARNING BUTTONS OK.
      APPLY "ENTRY" TO pri IN FRAME loan1.
      RETURN NO-APPLY.
   END. /* invalid principal */
   ELSE
   IF INPUT intr = 0 THEN DO:
      MESSAGE "Interest rate cannot be 0.  Please re-enter."
         VIEW-AS ALERT-BOX WARNING BUTTONS OK.
      APPLY "ENTRY" TO intr IN FRAME loan1.
      RETURN NO-APPLY.
   END. /* invalid interest */
   ELSE
   IF INPUT dat = ? OR INPUT dat = "" THEN DO:
      MESSAGE "Invalid date. Please re-enter." VIEW-AS ALERT-BOX
          WARNING BUTTONS OK.
      APPLY "ENTRY" TO dat IN FRAME loan1.
      RETURN NO-APPLY.
   END. /* invalid date */

   HIDE FRAME loan1.

   RUN calc. 

   ENABLE pri intr trm incr dat ttl1 ttl2 btn-OK btn-exit

        WITH FRAME loan1.

   APPLY "ENTRY" TO pri IN FRAME loan1.

END.
PAUSE BEFORE-HIDE.
ENABLE pri intr trm incr dat ttl1 ttl2 btn-OK btn-exit
     WITH FRAME loan1.

dat = ?.
WAIT-FOR CHOOSE OF btn-EXIT IN FRAME loan1.

PAUSE 0 BEFORE-HIDE.

PROCEDURE calc.

/*

Monthly payment = Principal * Interest / (1-(1/( (1 + Interest) ^ Term )))

*/

  DO WITH FRAME loan1:  

    ASSIGN

       dat

       incr

       intr

       pri

       trm

       ttl1

       ttl2.

  END.

  ASSIGN

    trm-ch = trm

    ydat = dat

    tday = DAY(dat)

    inty = intr / 1200 * incr  

    bpb  = pri

    pmt  = pri * inty / (1 - 1 / EXP(inty + 1,trm-ch))

    ttl3 = "LOAN AMORTIZATION".



FORM HEADER

  ttl3 FORMAT "x(44)" 

  TODAY FORMAT "99/99/9999" 

  STRING(TIME,"HH:MM AM") 

  SKIP

  "Each Payment ="  pmt  TO 34   FORMAT "->>>,>>>.99" SPACE(2) 

  ttl1 FORMAT "x(40)"  SKIP

  "Annual Total =" TO 15 pmt * 12 / incr  TO 34 

       FORMAT "->>>,>>>.99"  SPACE(2) 

  ttl2 FORMAT "x(40)"   SKIP

  "   Principal =" TO 15 pri    TO 34      

       FORMAT "->>>,>>>.99"  

     SKIP

  "Number of Payments =    " trm-ch   FORMAT ">>9" 

  "     Interest Rate =" STRING(intr,">>9.99%")  SKIP(1)

  WITH FRAME hdr60 PAGE-TOP NO-BOX.

  

VIEW FRAME hdr60.

REPEAT i = 1 TO trm-ch:

  ASSIGN

    paid = bpb * inty

    rpb  = bpb - pmt + paid.

  DISPLAY

    i          FORMAT ">>9"         LABEL "Pmt#"           

               VIEW-AS TEXT

    dat        FORMAT "99/99/9999"  LABEL "   Date"        

               VIEW-AS TEXT

    bpb        FORMAT "-zzz,zzz.99" LABEL "Beg Prin Bal"   

               VIEW-AS TEXT

    paid       FORMAT "-zzz,zzz.99" LABEL "Interest Paid"  

               VIEW-AS TEXT

    pmt - paid FORMAT "-zzz,zzz.99" LABEL "Principal Paid" 

               VIEW-AS TEXT

    rpb        FORMAT "-zzz,zzz.99" LABEL "Rem Prin Bal"   

               VIEW-AS TEXT

    WITH NO-BOX IN WINDOW CURRENT-WINDOW.

  ASSIGN

    j       = YEAR(dat) - YEAR(ydat) + 1

    yint[j] = yint[j] + paid

    ypri[j] = ypri[j] + pmt - paid

    bpb     = rpb

    j       = MONTH(dat) + incr - 1

    jd      = tday

    jm      = (j MOD 12) + 1.

  IF jm = 2 AND jd > 28 THEN jd = 28. /* ignore leap year. (who cares?) */

  IF CAN-DO("4,6,9,11",STRING(jm)) AND jd > 30 THEN jd = 30.

  dat = DATE(jm,jd,YEAR(dat) + INTEGER(TRUNCATE(j / 12,0))).

END.

PAUSE.

ttl3 = "LOAN AMORTIZATION INTEREST TOTALS BY YEAR".

REPEAT i = 1 TO 1 + YEAR(dat) - YEAR(ydat):

  DISPLAY

    "Calendar " + STRING(YEAR(ydat) + i - 1,"9999")

            FORMAT "x(14)"          LABEL "Period Covered"

    yint[i] FORMAT "->>>,>>>.99" LABEL " Interest Paid" (TOTAL)

    ypri[i] FORMAT "->>>,>>>.99" LABEL "Principal Paid" (TOTAL)

    WITH NO-BOX IN WINDOW CURRENT-WINDOW.

END.

PAUSE.

HIDE ALL.

END PROCEDURE.



RETURN.

































