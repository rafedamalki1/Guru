/************************************************************************************
	PROCEDURE: selval.p

	PURPOSE:   Selection List Display and Selection

	SYNTAX:    "RUN samples/selval.p"

	REMARKS:   Enter a database, file, field to choose from.  The
                   file is read and the field values displayed for 
                   selection.

	PARAMETERS: 

	AUTHORS:   Progress Consulting
	DATE:      February 1993

	LAST INSPECTED:
	INSPECTED BY:

 ************************************************************************************/
 /* Copyright(c) PROGRESS SOFTWARE CORPORATION, 1993 - All Rights Reserved.	    */

/*Code_Start*/

DEFINE SHARED VARIABLE s_carrier LIKE order.carrier NO-UNDO.

DEFINE VARIABLE carrier-list AS CHARACTER VIEW-AS SELECTION-LIST
                             SIZE 30 BY 7.
DEFINE VARIABLE ok-flag      AS LOGICAL.
s_carrier = "".
FORM
   carrier-list
   HELP "Press GO to select carrier."
   WITH FRAME frm_carrier NO-LABEL OVERLAY ROW 5 COLUMN 40
   TITLE " Carriers " VIEW-AS DIALOG-BOX.

FOR EACH order NO-LOCK BREAK BY order.carrier: 
   IF FIRST-OF(order.carrier) AND order.carrier <> "" THEN DO:
      ok-flag = carrier-list:ADD-LAST(order.carrier)
         IN FRAME frm_carrier.
      IF NOT ok-flag THEN UNDO, RETURN.
   END. /* first-of */
END.    /* FOR EACH */

ENABLE carrier-list WITH FRAME frm_carrier.
APPLY "CHOOSE" TO carrier-list IN FRAME frm_carrier.
ON MOUSE-SELECT-DBLCLICK OF carrier-list
   APPLY "GO" TO carrier-list IN FRAME frm_carrier.
UPDATE carrier-list WITH FRAME frm_carrier. 
s_carrier = carrier-list:screen-value.
HIDE FRAME frm_carrier.













