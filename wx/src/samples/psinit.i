/************************************************************************************
	PROCEDURE: psinit.i

	PURPOSE:   Variable definitions

	SYNTAX:    "{samples/psinit.i}"

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
/* psinit.i */

/*------------------------  Data related definitions -------------------*/

DEFINE {1} SHARED WORKFILE datapoints NO-UNDO
   FIELD datalabel AS CHARACTER
   FIELD dataval AS INTEGER
   FIELD datagyn AS LOGICAL INITIAL TRUE
   FIELD datagray AS DECIMAL DECIMALS 2.

DEFINE {1} SHARED VARIABLE numrecs AS INTEGER NO-UNDO.

DEFINE {1} SHARED VARIABLE graph-file AS CHARACTER FORMAT "x(60)" NO-UNDO. /* output file */
DEFINE {1} SHARED VARIABLE autoadj AS LOGICAL NO-UNDO.

DEFINE {1} SHARED VARIABLE left-margin AS INTEGER NO-UNDO.
DEFINE {1} SHARED VARIABLE pagewidth AS INTEGER NO-UNDO.
DEFINE {1} SHARED VARIABLE pageheight AS INTEGER NO-UNDO.
DEFINE {1} SHARED VARIABLE lower AS INTEGER.

/*--------------------   Text related definitions -------------------------*/
DEFINE {1} SHARED WORKFILE textline NO-UNDO
    FIELD textstr  AS CHARACTER FORMAT "x(60)"               /* Text */
    FIELD textfont AS CHARACTER FORMAT "x(30)"               /* Font */
    FIELD textsize AS INTEGER               /* Size */
    FIELD textctr  AS LOGICAL           /* Centered? */
    FIELD textxpos AS INTEGER               /* x-coordinate */
    FIELD textypos AS INTEGER.              /* y-coordinate */

DEFINE {1} SHARED VARIABLE label-font AS CHARACTER FORMAT "x(30)" NO-UNDO. /* Label font */
DEFINE {1} SHARED VARIABLE label-size AS INTEGER NO-UNDO.      /* Label font size */

DEFINE {1} SHARED VARIABLE num-lines AS INTEGER NO-UNDO.

/*-------------------   Special definitions for piecharts ----------------*/
DEFINE {1} SHARED VARIABLE circ-radius AS INTEGER INITIAL 160 NO-UNDO.
DEFINE {1} SHARED VARIABLE start-pos   AS INTEGER INITIAL 0 NO-UNDO.
DEFINE {1} SHARED VARIABLE end-pos     AS INTEGER INITIAL 0 NO-UNDO.
DEFINE {1} SHARED VARIABLE slice-pct   AS DECIMAL NO-UNDO.
DEFINE {1} SHARED VARIABLE prt-pct     AS CHARACTER FORMAT "x" INITIAL "b" NO-UNDO.
			/* d = print just data beside the pie
			   p = print percent
			   b = print both data and percent
			*/

/*---------------------------------------------------------------------*/
