/************************************************************************************
	PROCEDURE: psval.p

	PURPOSE:   Browser Widget

	SYNTAX:    "RUN samples/psval.p"

	REMARKS:   Program was created using the User Interface Builder.
                   A BROWSER widget was used with the QUERY statements
                   to show all current customers in the SPORTS database
                   and have the user select one.

	PARAMETERS: 

	AUTHORS:   PROGRESS Consulting
	DATE:      February 1993

	LAST INSPECTED:
	INSPECTED BY:

 ************************************************************************************/
 /* Copyright(c) PROGRESS SOFTWARE CORPORATION, 1993 - All Rights Reserved.	    */
/*Code_Start*/
 
&ANALYZE-SUSPEND _VERSION-NUMBER 7.1A001

DEFINE SHARED VAR s_cust_num     LIKE customer.cust-num NO-UNDO.

&ANALYZE-RESUME


/* Definitions of the field level widgets                               */
/* Definitions of the queries                                           */
DEFINE NEW SHARED VAR s_UIB_TblList  AS CHAR               NO-UNDO.
DEFINE NEW SHARED VAR s_UIB_FldList  AS CHAR               NO-UNDO.
DEFINE NEW SHARED VAR s_UIB_OrdList  AS CHAR               NO-UNDO.
DEFINE NEW SHARED VAR s_UIB_JoinTo   AS INTEGER  EXTENT 20 NO-UNDO.
DEFINE NEW SHARED VAR s_UIB_JoinCode AS CHAR     EXTENT 20 NO-UNDO.
DEFINE NEW SHARED VAR s_UIB_Where    AS CHAR     EXTENT 20 NO-UNDO.
DEFINE NEW SHARED VAR s_UIB_Query    AS CHAR               NO-UNDO.
DEFINE NEW SHARED VAR s_UIB_Browse   AS CHAR               NO-UNDO.
DEFINE NEW SHARED BUFFER Customer FOR Customer.
DEFINE NEW SHARED QUERY cust-num-browse FOR Customer SCROLLING.

/* Definitions of the frame widgets                                     */
 
/* Definitions of the browser frames                                    */
DEFINE BROWSE cust-num-browse QUERY cust-num-browse DISPLAY 
       Cust-Num Name
    WITH OVERLAY  NO-LABELS
        AT COL 10 ROW 7.41  SIZE 40 BY 7 
        BGCOLOR ? FGCOLOR ?.

DEFINE FRAME frm_browse
   cust-num-browse
   WITH OVERLAY NO-LABELS AT COL 10 ROW 7.41 SIZE-CHAR 40 BY 8
   VIEW-AS DIALOG-BOX TITLE "Customers".
   

/* Definitions of the browser widgets                                   */
&ANALYZE-SUSPEND _BROWSER-BLOCK cust-num-browse
OPEN QUERY cust-num-browse FOR EACH Customer.  /* _BROWSER-BLOCK-END */

ON MOUSE-MENU-DOWN OF BROWSE cust-num-browse DO:
  ASSIGN
          s_UIB_TblList     = "Customer"
          s_UIB_FldList     = "Cust-Num Name"
          s_UIB_OrdList     = ""
          s_UIB_Query       = 
               "DEFINE SHARED QUERY cust-num-browse FOR Customer SCROLLING.".

   RUN adeuib/rtqryedt.p ("cust-num-browse").
END.
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK GO cust-num-browse

ON MOUSE-SELECT-DBLCLICK, GO OF BROWSE cust-num-browse DO:
  s_cust_num = customer.cust-num.
  APPLY "GO" TO cust-num-browse IN FRAME frm_browse.
END.
ON ENDKEY OF BROWSE cust-num-browse DO:
   HIDE BROWSE cust-num-browse NO-PAUSE.
   RETURN.
END.
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
UPDATE cust-num-browse WITH FRAME frm_browse.
HIDE BROWSE cust-num-browse NO-PAUSE.
RETURN.










