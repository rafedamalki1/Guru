/************************************************************************************
	PROCEDURE: psscale.i

	PURPOSE:   

	SYNTAX:    "{samples/psscale.i}"

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
/* psscale.i */

tnum = {1}.

dnum = tnum.

DO WHILE dnum > 0:
    dnum = TRUNCATE (dnum / 10, 0).
    snum = snum + 1.
END.

scal = EXP (10, snum - 1).
newnum = TRUNCATE (tnum / scal + 1, 0) * scal.
{2} = newnum.
{3} = scal.
