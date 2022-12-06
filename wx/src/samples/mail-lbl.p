/************************************************************************************

	PROCEDURE: mail-lbl.p



	PURPOSE:   Prints mailing labels



	SYNTAX:    RUN samples/mail-lbl.p



	REMARKS:   This code prints mailing labels for each customer.

		   



	PARAMETERS: NONE



	AUTHORS:   Judy Rothermal

	DATE:      February 1993



	LAST INSPECTED:

	INSPECTED BY:



 ************************************************************************************/

 /* Copyright(c) PROGRESS SOFTWARE CORPORATION, 1993 - All Rights Reserved.	    */



  /*Code_Start*/

 

/* mail-lbl.p  -  Print mailing labels 1, 2, or 3 up */



DEFINE VARIABLE num-up AS INTEGER INITIAL 3. /* change to 1, 2, or 3 as */

                             /* desired and change the followng extents */

DEFINE VARIABLE line1 AS CHARACTER EXTENT 3 FORMAT "x(25)".

DEFINE VARIABLE line2 AS CHARACTER EXTENT 3 FORMAT "x(25)".

DEFINE VARIABLE line3 AS CHARACTER EXTENT 3 FORMAT "x(25)".

DEFINE VARIABLE line4 AS CHARACTER EXTENT 3 FORMAT "x(25)".

DEFINE VARIABLE no-more AS LOGICAL INITIAL FALSE.

DEFINE VARIABLE i AS INTEGER.

DEFINE VARIABLE j AS INTEGER.



/* Put the line

        OUTPUT TO PRINTER PAGE-SIZE 99999.

   here to send labels to the printer. */

PAUSE BEFORE-HIDE.

cust-loop:

REPEAT FOR customer

    PRESELECT EACH customer NO-LOCK BY postal-code:

     /* omit preselect if no sort needed */



    /* 

       format labels with 1 blank line,

                          4 address lines, 

                          1 blank line 

    */

    FORM SKIP(1) line1 SKIP line2 SKIP line3 SKIP line4 SKIP(1)

         WITH NO-BOX NO-LABELS NO-ATTR-SPACE 3 DOWN

         CENTERED.



    label-set:

    REPEAT i = 1 TO num-up:

        FIND NEXT customer NO-ERROR.

        IF NOT AVAILABLE customer THEN DO:

            no-more = TRUE.

            /* blank out lines not filled with new customers */

            DO j = i TO num-up:

              ASSIGN

                line1[j] = ""

                line2[j] = ""

                line3[j] = ""

                line4[j] = "".

            END.

            LEAVE label-set.

        END. /* not avail customer */

        ASSIGN

           line1[i] = name

           line2[i] = address.

        IF address2 = "" THEN

          ASSIGN

            line3[i] = city + ", " + st + "  " + 

                       STRING(postal-code,"99999")

            line4[i] = "".

        ELSE 

          ASSIGN

            line3[i] = address2

            line4[i] = city + ", " + st + "  " + STRING(postal-code,"99999").

    END. /* label-set */

    IF i > 1 THEN

       DISPLAY 

           line1 VIEW-AS TEXT

           line2 VIEW-AS TEXT

           line3 VIEW-AS TEXT

           line4 VIEW-AS TEXT.

    

    IF no-more THEN LEAVE cust-loop.

END. /* cust-loop */







