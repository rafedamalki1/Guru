/************************************************************************************

        PROCEDURE: pshist.p



        PURPOSE:   Histogram generator



        SYNTAX:    "RUN samples/pshist.p"



        REMARKS:   



        PARAMETERS:NONE



        AUTHORS:   Progress Consulting

        DATE:      March 1993



        LAST INSPECTED:

        INSPECTED BY:



 ************************************************************************************/

 /* Copyright(c) PROGRESS SOFTWARE CORPORATION, 1993 - All Rights Reserved.            */



/*Code_Start*/



/* pshist.p - histogram generator */



{samples/psinit.i}



DEFINE VARIABLE maxval AS INTEGER NO-UNDO.

DEFINE VARIABLE maxscale AS INTEGER NO-UNDO.

DEFINE VARIABLE minval AS INTEGER NO-UNDO.

DEFINE VARIABLE minscale AS INTEGER NO-UNDO.



DEFINE VARIABLE t-file AS CHARACTER FORMAT "x(12)" NO-UNDO.  /* Temp file name variable */



DEFINE VARIABLE hpts  AS INTEGER NO-UNDO.

DEFINE VARIABLE hgap AS INTEGER NO-UNDO.

DEFINE VARIABLE vpts AS INTEGER NO-UNDO.

DEFINE VARIABLE vscale AS INTEGER INITIAL 1 NO-UNDO.

DEFINE VARIABLE vgrade AS INTEGER NO-UNDO.



DEFINE VARIABLE i AS INTEGER.

DEFINE VARIABLE left AS INTEGER.

DEFINE VARIABLE right AS INTEGER.

DEFINE VARIABLE upper AS INTEGER.



/*-------------------------------------------------------------------------*/

PAUSE 0.        /* Suspend default PROGRESS processing */



IF autoadj THEN

   ASSIGN

    hpts = pagewidth / (numrecs * 2 + 1)

    hgap = hpts.

ELSE DO:

    FORM

    hpts LABEL "Width of a data bar" COLON 35 SKIP(0)

           hgap LABEL "Space between two data bars" COLON 35

        WITH FRAME h-adj SIDE-LABELS NO-BOX ROW 5.



    DO WHILE TRUE:

        UPDATE hpts hgap WITH FRAME h-adj.

        pagewidth = (hpts * numrecs) + hgap * (numrecs - 1).

        IF (pagewidth <= (612 - left-margin)) THEN

            LEAVE.

        ELSE

            MESSAGE "Graph too wide for page. Reenter data.".

    END.

    HIDE FRAME h-adj NO-PAUSE.

END.



/*-------------------------------------------------------------------------*/

                /* Calculate maximum data value */

FOR EACH datapoints i = 1 TO numrecs:

   ASSIGN

    maxval = MAXIMUM(dataval, maxval)

    minval = MINIMUM(dataval, minval).

END.



DEFINE VARIABLE tnum AS INTEGER.

DEFINE VARIABLE dnum AS INTEGER.

DEFINE VARIABLE snum AS INTEGER.

DEFINE VARIABLE scal AS INTEGER.

DEFINE VARIABLE newnum AS INTEGER.





IF (minval < 0) THEN DO:

    DEFINE VARIABLE tval AS INTEGER.

    DEFINE VARIABLE toppts AS INTEGER.

    DEFINE VARIABLE botpts AS INTEGER.



    tval = MAXIMUM (maxval, -1 * minval).

    {samples/psscale.i tval tval vgrade}



    toppts = TRUNCATE (maxval / vgrade, 0).

    IF (maxval MODULO vgrade) > 0 THEN

        toppts = toppts + 1.

    ASSIGN

       maxval = toppts * vgrade

       botpts = TRUNCATE (minval / vgrade, 0).

    IF (minval MODULO vgrade) > 0 THEN

        botpts = botpts - 1.

    ASSIGN 

       minval     = botpts * vgrade

       tval       = maxval - minval

       lower      = -576 * (minval / tval) + 100

       pageheight = 676 - lower.

END.

ELSE DO:

    {samples/psscale.i maxval maxval vgrade}

END.



IF (maxval > pageheight) THEN

    vscale = (maxval / pageheight).



vpts  = (vscale * pageheight) / maxval.



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



t-file = "psboxdef.ps".

OS-APPEND VALUE(t-file) VALUE(graph-file).



/*-------------------------------------------------------------------------*/

HIDE MESSAGE NO-PAUSE.

DISPLAY "Generating texts - header, footer etc..." FORMAT "x(50)"

        WITH FRAME msgs.

PAUSE 2 NO-MESSAGE.

{samples/psgentxt.i}



/*-------------------------------------------------------------------------*/

HIDE MESSAGE NO-PAUSE.

DISPLAY "Generating histogram data..." FORMAT "x(50)"

        WITH FRAME msgs.

PAUSE 2 NO-MESSAGE.

OUTPUT TO VALUE (graph-file) APPEND.



PUT "      % Histogram data. " SKIP(1).



left = IF autoadj THEN left-margin ELSE (612 - pagewidth) / 2.



FOR EACH datapoints i = 1 TO numrecs WITH NO-LABELS:

    ASSIGN

       right = left + hpts

       upper = lower + (dataval / vscale) * vpts.

    PUT hpts ((dataval / vscale) * vpts) left lower " vbox" SKIP.

    IF datagyn THEN

        PUT datagray " setgray fill 0 setgray" SKIP(1).

    ELSE

        PUT "    stroke" SKIP(1).



    IF (dataval >= 0) THEN

    DO:

    PUT hpts " (" + STRING(dataval,"->>,>>9") + ") " FORMAT "x(15)"

        left (upper + 7)

        " PrtCenterText" SKIP (1).



    PUT hpts " (" + datalabel + ") " FORMAT "x(12)" left (lower - 12)

        " PrtCenterText" SKIP (1).

    END.

    ELSE DO:

    PUT hpts " (" + STRING(dataval,"->>,>>9") + ") " FORMAT "x(15)"

        left (upper - 12)

        " PrtCenterText" SKIP (1).



    PUT hpts " (" + datalabel + ") " FORMAT "x(12)" left (lower + 7)

        " PrtCenterText" SKIP (1).

    END.



    left = left + hpts + hgap.

END.



OUTPUT CLOSE.



/*-------------------------------------------------------------------------*/

HIDE MESSAGE NO-PAUSE.

DISPLAY "Histogram scales data..." FORMAT "x(50)"

        WITH FRAME msgs.

                /* Draw scale and coordinates */

{samples/psprtcrd.i}



/*-------------------------------------------------------------------------*/

t-file = "psend.ps".

OS-APPEND VALUE(t-file) VALUE(graph-file).

PAUSE 2 NO-MESSAGE.



DEF VAR show-it AS CHAR VIEW-AS EDITOR SIZE 70 BY 14

                        SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL.

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
IF "{&WINDOW-SYSTEM}" <> "TTY"  THEN
show-it:FONT = 2.   /* USE non-proportional FONT */

WAIT-FOR CHOOSE OF bOK IN FRAME show_frame.

MESSAGE "Program complete. Postscript code is in the file " 

         + graph-file + "."

            VIEW-AS ALERT-BOX MESSAGE BUTTONS OK.







