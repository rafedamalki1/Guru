/************************************************************************************

        PROCEDURE: pspie.p



        PURPOSE:   Pie Chart generator



        SYNTAX:    "RUN samples/pspie.p"



        REMARKS:  



        PARAMETERS:NONE



        AUTHORS:   Progress Consulting

        DATE:      March 1993



        LAST INSPECTED:

        INSPECTED BY:



 ************************************************************************************/

 /* Copyright(c) PROGRESS SOFTWARE CORPORATION, 1993 - All Rights Reserved.            */



/*Code_Start*/



/* pspie.p - Pie chart generator */



{samples/psinit.i}



DEFINE VARIABLE maxval AS INTEGER NO-UNDO.

DEFINE VARIABLE maxscale AS INTEGER NO-UNDO.

DEFINE VARIABLE minval AS INTEGER NO-UNDO.

DEFINE VARIABLE sumval AS INTEGER INITIAL 0 NO-UNDO.



DEFINE VARIABLE t-file AS CHARACTER FORMAT "x(12)" NO-UNDO.  /* Temp file name variable */

DEFINE VARIABLE i AS INTEGER.



/*-------------------------------------------------------------------------*/

PAUSE 0.        /* Suspend default PROGRESS processing */



                /* Calculate maximum data value */

FOR EACH datapoints i = 1 TO numrecs:

   ASSIGN

    maxval = MAXIMUM(dataval, maxval)

    minval = MINIMUM(dataval, minval)

    sumval = sumval + dataval.

END.



/*-------------------------------------------------------------------------*/

HIDE MESSAGE NO-PAUSE.

DISPLAY "Creating output file " + graph-file + "..." FORMAT "x(50)"

        WITH FRAME msgs VIEW-AS DIALOG-BOX.

PAUSE 2 NO-MESSAGE.

t-file = "psbeg.ps".

OS-COPY VALUE(t-file) VALUE(graph-file).



t-file = "psprttxt.ps".

OS-APPEND VALUE(t-file) VALUE(graph-file).



t-file = "psctrtxt.ps".

OS-APPEND VALUE(t-file) VALUE(graph-file).



t-file = "pspieslc.ps".

OS-APPEND VALUE(t-file) VALUE(graph-file).



/*-------------------------------------------------------------------------*/

HIDE MESSAGE NO-PAUSE.

DISPLAY "Generating texts - header, footer etc..." FORMAT "x(50)"

        WITH FRAME msgs VIEW-AS DIALOG-BOX.

PAUSE 2 NO-MESSAGE.



{samples/psgettxt.i}



/*-------------------------------------------------------------------------*/

HIDE MESSAGE NO-PAUSE.

DISPLAY "Generating pie-chart data..." FORMAT "x(50)"

        WITH FRAME msgs VIEW-AS DIALOG-BOX.

PAUSE 2 NO-MESSAGE.



OUTPUT TO VALUE (graph-file) APPEND.



PUT "      % Line graph data. " SKIP(1).



PUT "295 360 translate" SKIP(1).



start-pos = 0.



FOR EACH datapoints i = 1 TO numrecs:

   ASSIGN

    start-pos = end-pos

    slice-pct = (dataval * 100) / sumval

    end-pos   = IF (i = numrecs) THEN 360

                ELSE end-pos + (dataval * 360) / sumval.



    IF prt-pct = "b" THEN

        PUT "(" + datalabel + " (" + STRING (dataval) + ", " 

                + STRING (TRUNCATE (slice-pct, 2)) + "%))" FORMAT "x(30)".

    ELSE IF prt-pct = "d" THEN

        PUT "(" + datalabel + " (" + STRING (dataval) + "))" FORMAT "x(25)".

    ELSE IF prt-pct = "p" THEN

        PUT "(" + datalabel + " (" + STRING (TRUNCATE (slice-pct, 2)) + "%))"

            FORMAT "x(25)".

    PUT

        start-pos

        end-pos

        datagray

        circ-radius

        label-size

        " DrawSlice"

        SKIP(1).

END.



OUTPUT CLOSE.



/*-------------------------------------------------------------------------*/

t-file = "psend.ps".

OS-APPEND VALUE(t-file) VALUE(graph-file).



DEF VAR show-it AS CHAR VIEW-AS EDITOR SIZE 70 BY 14 SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL.

DEF BUTTON bOK LABEL "OK".

DEF VAR ok_stat AS LOGICAL NO-UNDO.



HIDE FRAME msgs NO-PAUSE.



DISPLAY show-it bOK WITH FRAME show_frame

                    VIEW-AS DIALOG-BOX NO-LABELS.

HIDE MESSAGE NO-PAUSE.

ASSIGN
ok_stat = show-it:READ-FILE(graph-file)
show-it:SENSITIVE = YES
bOK:SENSITIVE = YES.
IF "{&WINDOW-SYSTEM}" <> "TTY" THEN
show-it:FONT = 2.   /* USE non-proportional FONT */

WAIT-FOR CHOOSE OF bOK IN FRAME show_frame.

MESSAGE "Program complete. Postscript code is in the file " + graph-file + "."

            VIEW-AS ALERT-BOX MESSAGE

        BUTTONS OK.





