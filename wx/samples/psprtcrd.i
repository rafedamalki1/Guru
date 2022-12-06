/************************************************************************************
	PROCEDURE: psprtcrd.i

	PURPOSE:   

	SYNTAX:    "{samples/psprtcrd.i}"

	REMARKS:   

        PARAMETERS:NONE

	AUTHORS:   Progress Consulting
	DATE:      March 1993

	LAST INSPECTED:
	INSPECTED BY:

 ************************************************************************************/
 /* Copyright(c) PROGRESS SOFTWARE CORPORATION, 1993 - All Rights Reserved.	    */

/*Code_Start*/
/* PROGRESS PostScript Interface */
/* psprtcrd.i */

OUTPUT TO VALUE(graph-file) APPEND.

PUT "       % Histogram labels." SKIP(1).

		/* Draw a horizontal line */
PUT " .8 setgray newpath 20 " lower " moveto "
      left lower " lineto stroke" SKIP(1).

PUT " newpath " (left-margin / 2) ((minval * vpts) / vscale + lower)
    " moveto " (left-margin / 2) (5 + lower + (vpts * maxval) / vscale)
    " lineto stroke" SKIP(1).


DEFINE VARIABLE tn AS INTEGER.

tn = minval.
DO WHILE tn <= maxval:
    PUT ".8 setgray newpath " (left-margin / 2 - 5)
	 ((tn * vpts) / vscale + lower)
	" moveto 10 0 rlineto stroke" SKIP.

    IF (tn <> 0) THEN
    DO:
	PUT "0 setgray " (left-margin / 2 + 5)
	((tn * vpts) / vscale + lower + 5)
	" moveto (" + STRING (tn, "->>,>>9") + ") show " FORMAT "x(35)" SKIP(1).
    END.
    tn = tn + vgrade.
END.

OUTPUT CLOSE.
