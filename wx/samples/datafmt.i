/************************************************************************************
	PROCEDURE: datafmt.i

	PURPOSE:   Update section of the FORMAT PHRASES procedure

	SYNTAX:    {samples/datafmt.i &field = ""}

	REMARKS:   This code handles enabling the appropriate fields and displaying
		   the formatting field.
	
	PARAMETERS: &field

	AUTHORS:   Judy Rothermal
	DATE:      February 1993

	LAST INSPECTED:
	INSPECTED BY:

 ************************************************************************************/
 /* Copyright(c) PROGRESS SOFTWARE CORPORATION, 1993 - All Rights Reserved.	    */
/*Code_Start*/
 
DO:
   ENABLE bOK bQUIT WITH FRAME frm-{&field}.
   REPEAT:
      UPDATE {&field} fmt WITH FRAME frm-{&field}.    
      DISPLAY STRING({&field}, fmt) FORMAT "x(25)" 
         @ result WITH FRAME frm-{&field}.
   END.
   HIDE FRAME frm-{&field}.
END.


