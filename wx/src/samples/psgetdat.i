/************************************************************************************
	PROCEDURE: psgetdat.i

	PURPOSE:   Getting data interactively from the user

	SYNTAX:    "{samples/psgetdat.i}"

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
/* psgetdat.i */

	/* Getting data interactively from the user */

REPEAT WITH FRAME i-frame 10 DOWN:
    CREATE datapoints.
    UPDATE datalabel dataval datagyn.
    IF datagyn THEN
	UPDATE datagray.
    numrecs = numrecs + 1.

    DOWN WITH FRAME i-frame.
END.

HIDE FRAME i-frame NO-PAUSE.

	/* Getting data from the customer file */
/*
numrecs = 0.
For each customer where curr-bal > 4999 and curr-bal < 10001:
    numrecs = numrecs + 1.
    create datapoints.
    datalabel = sales-rep + "-" + substring(name, 1, 3).
    dataval = curr-bal.
    datagyn = yes.
    if (numrecs mod 2) eq 0 then
	datagray = 0.8.
    else
	datagray = 0.3.
    if (numrecs >= 10) then
	leave.
end.
*/

graph-file = "graph.out".
left-margin = 72.
pagewidth = 612 - 72.
lower = 100.
pageheight = 576 - (lower - 100).
autoadj = TRUE.

