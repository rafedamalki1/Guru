&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          sports           PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------
  File: e-ocx1.w

  Description: Reads and updates Customer records using the PROGRESS OCX 
               spin control, CSSpin.

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE result          AS LOGICAL.
DEFINE VARIABLE max-records     AS INTEGER.
DEFINE VARIABLE chCSSpin        AS COM-HANDLE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Customer

/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define FIELDS-IN-QUERY-Dialog-Frame Customer.Name ~
Customer.Credit-Limit Customer.Address Customer.Terms Customer.Address2 ~
Customer.Discount Customer.City Customer.Sales-Rep Customer.State ~
Customer.Contact Customer.Country Customer.Phone Customer.Postal-Code ~
Customer.Comments Customer.Cust-Num Customer.Balance 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Dialog-Frame Customer.Name ~
Customer.Credit-Limit Customer.Address Customer.Terms Customer.Address2 ~
Customer.Discount Customer.City Customer.Sales-Rep Customer.State ~
Customer.Contact Customer.Country Customer.Phone Customer.Postal-Code ~
Customer.Comments Customer.Cust-Num Customer.Balance 
&Scoped-define ENABLED-TABLES-IN-QUERY-Dialog-Frame Customer
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Dialog-Frame Customer

&Scoped-define FIELD-PAIRS-IN-QUERY-Dialog-Frame~
 ~{&FP1}Name ~{&FP2}Name ~{&FP3}~
 ~{&FP1}Credit-Limit ~{&FP2}Credit-Limit ~{&FP3}~
 ~{&FP1}Address ~{&FP2}Address ~{&FP3}~
 ~{&FP1}Terms ~{&FP2}Terms ~{&FP3}~
 ~{&FP1}Address2 ~{&FP2}Address2 ~{&FP3}~
 ~{&FP1}Discount ~{&FP2}Discount ~{&FP3}~
 ~{&FP1}City ~{&FP2}City ~{&FP3}~
 ~{&FP1}Sales-Rep ~{&FP2}Sales-Rep ~{&FP3}~
 ~{&FP1}State ~{&FP2}State ~{&FP3}~
 ~{&FP1}Contact ~{&FP2}Contact ~{&FP3}~
 ~{&FP1}Country ~{&FP2}Country ~{&FP3}~
 ~{&FP1}Phone ~{&FP2}Phone ~{&FP3}~
 ~{&FP1}Postal-Code ~{&FP2}Postal-Code ~{&FP3}~
 ~{&FP1}Comments ~{&FP2}Comments ~{&FP3}~
 ~{&FP1}Cust-Num ~{&FP2}Cust-Num ~{&FP3}~
 ~{&FP1}Balance ~{&FP2}Balance ~{&FP3}
&Scoped-define SELF-NAME Dialog-Frame
&Scoped-define OPEN-QUERY-Dialog-Frame OPEN QUERY Dialog-Frame PRESELECT EACH sports.Customer NO-LOCK.
&Scoped-define TABLES-IN-QUERY-Dialog-Frame Customer
&Scoped-define FIRST-TABLE-IN-QUERY-Dialog-Frame Customer


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS Customer.Name Customer.Credit-Limit ~
Customer.Address Customer.Terms Customer.Address2 Customer.Discount ~
Customer.City Customer.Sales-Rep Customer.State Customer.Contact ~
Customer.Country Customer.Phone Customer.Postal-Code Customer.Comments ~
Customer.Cust-Num Customer.Balance 
&Scoped-define FIELD-PAIRS~
 ~{&FP1}Name ~{&FP2}Name ~{&FP3}~
 ~{&FP1}Credit-Limit ~{&FP2}Credit-Limit ~{&FP3}~
 ~{&FP1}Address ~{&FP2}Address ~{&FP3}~
 ~{&FP1}Terms ~{&FP2}Terms ~{&FP3}~
 ~{&FP1}Address2 ~{&FP2}Address2 ~{&FP3}~
 ~{&FP1}Discount ~{&FP2}Discount ~{&FP3}~
 ~{&FP1}City ~{&FP2}City ~{&FP3}~
 ~{&FP1}Sales-Rep ~{&FP2}Sales-Rep ~{&FP3}~
 ~{&FP1}State ~{&FP2}State ~{&FP3}~
 ~{&FP1}Contact ~{&FP2}Contact ~{&FP3}~
 ~{&FP1}Country ~{&FP2}Country ~{&FP3}~
 ~{&FP1}Phone ~{&FP2}Phone ~{&FP3}~
 ~{&FP1}Postal-Code ~{&FP2}Postal-Code ~{&FP3}~
 ~{&FP1}Comments ~{&FP2}Comments ~{&FP3}~
 ~{&FP1}Cust-Num ~{&FP2}Cust-Num ~{&FP3}~
 ~{&FP1}Balance ~{&FP2}Balance ~{&FP3}
&Scoped-define ENABLED-TABLES Customer
&Scoped-define FIRST-ENABLED-TABLE Customer
&Scoped-Define ENABLED-OBJECTS Btn_Update Btn_Cancel RECT-1 Record_Count 
&Scoped-Define DISPLAYED-FIELDS Customer.Name Customer.Credit-Limit ~
Customer.Address Customer.Terms Customer.Address2 Customer.Discount ~
Customer.City Customer.Sales-Rep Customer.State Customer.Contact ~
Customer.Country Customer.Phone Customer.Postal-Code Customer.Comments ~
Customer.Cust-Num Customer.Balance 
&Scoped-Define DISPLAYED-OBJECTS Record_Count 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE custSpin AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chcustSpin AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Cancel" 
     SIZE 12 BY 1.1 TOOLTIP "Close Customer Information"
     BGCOLOR 8 .

DEFINE BUTTON Btn_Update 
     LABEL "Update" 
     SIZE 12 BY 1.1 TOOLTIP "Update Customer Information"
     BGCOLOR 8 .

DEFINE VARIABLE Record_Count AS INTEGER FORMAT "9999":U INITIAL 0 
     LABEL "Record Number" 
      VIEW-AS TEXT 
     SIZE 5.8 BY .62 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL 
     SIZE 24 BY .95.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Dialog-Frame FOR 
      Customer SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     Customer.Name AT ROW 2.43 COL 13.4 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 21.2 BY .95 TOOLTIP "Enter customer name"
     Customer.Credit-Limit AT ROW 2.43 COL 47.2 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12.2 BY .95 TOOLTIP "Enter new credit limit"
     Customer.Address AT ROW 3.43 COL 13.4 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 21.2 BY .95 TOOLTIP "Enter first line of address"
     Customer.Terms AT ROW 3.43 COL 47.2 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 21.2 BY .95 TOOLTIP "Enter terms of payment"
     Customer.Address2 AT ROW 4.43 COL 13.4 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 21.2 BY .95 TOOLTIP "Enter second line of address"
     Customer.Discount AT ROW 4.43 COL 47.2 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 5.6 BY .95 TOOLTIP "Enter standard discount for this customer"
     Customer.City AT ROW 5.43 COL 13.4 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 15.8 BY .95 TOOLTIP "Enter customer city"
     Customer.Sales-Rep AT ROW 5.43 COL 47.2 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 7.8 BY .95 TOOLTIP "Enter sales representative code"
     Customer.State AT ROW 6.43 COL 13.4 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 21.2 BY .95 TOOLTIP "Enter customer state"
     Customer.Contact AT ROW 6.43 COL 47.2 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 21.2 BY .95 TOOLTIP "Enter name of customer contact person"
     Customer.Country AT ROW 7.43 COL 13.4 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 21.2 BY .95 TOOLTIP "Enter customer country"
     Customer.Phone AT ROW 7.43 COL 47.2 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 21.2 BY .95 TOOLTIP "Enter customer contact phone number"
     Customer.Postal-Code AT ROW 8.43 COL 13.4 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.8 BY .95 TOOLTIP "Enter zip or other national postal code"
     Customer.Comments AT ROW 9.67 COL 5
          VIEW-AS FILL-IN 
          SIZE 61.2 BY .95 TOOLTIP "Enter any notes on this customer"
     Btn_Update AT ROW 11.19 COL 2.8
     Btn_Cancel AT ROW 12.43 COL 2.8
     Customer.Cust-Num AT ROW 1.43 COL 13.4 COLON-ALIGNED
           VIEW-AS TEXT 
          SIZE 6.6 BY .62
     Customer.Balance AT ROW 1.43 COL 47.2 COLON-ALIGNED
           VIEW-AS TEXT 
          SIZE 15.6 BY .62
     Record_Count AT ROW 13.86 COL 49.2 COLON-ALIGNED
     RECT-1 AT ROW 13.62 COL 34
     SPACE(20.32) SKIP(0.83)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Customer Information"
         CANCEL-BUTTON Btn_Cancel.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Allow: Basic,Browse,DB-Fields,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS


/* ***************  Runtime Attributes and UIB Settings  ************** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
                                                                        */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE.

/* SETTINGS FOR FILL-IN Customer.Comments IN FRAME Dialog-Frame
   ALIGN-L                                                              */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX Dialog-Frame
/* Query rebuild information for DIALOG-BOX Dialog-Frame
     _START_FREEFORM
OPEN QUERY Dialog-Frame PRESELECT EACH sports.Customer NO-LOCK.
     _END_FREEFORM
     _Options          = "NO-LOCK"
     _Query            is OPENED
*/  /* DIALOG-BOX Dialog-Frame */
&ANALYZE-RESUME

 



/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME custSpin ASSIGN
       FRAME        = FRAME Dialog-Frame:HANDLE
       ROW          = 11.19
       COLUMN       = 15.4
       HEIGHT       = 2.33
       WIDTH        = 61.2
       HIDDEN       = no
       SENSITIVE    = yes.
      custSpin:NAME = "custSpin":U .
/* custSpin OCXINFO:CREATE-CONTROL from: {EAF26C8F-9586-101B-9306-0020AF234C9D} type: CSSpin */
      custSpin:MOVE-AFTER(Btn_Update:HANDLE IN FRAME Dialog-Frame).

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Customer Information */
DO:
 APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Update
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Update Dialog-Frame
ON CHOOSE OF Btn_Update IN FRAME Dialog-Frame /* Update */
DO:
    GET CURRENT Dialog-Frame EXCLUSIVE-LOCK NO-WAIT.
    IF LOCKED(Customer) THEN DO:
        MESSAGE "This record is locked by another user." SKIP(1)
                "Please try again, later."
        VIEW-AS ALERT-BOX INFORMATION.
        RETURN NO-APPLY.
    END.
    IF CURRENT-CHANGED(Customer) THEN Update-Conflict: DO:
        MESSAGE "Another user has modified this Customer." SKIP(1)
                "Would you like to:" SKIP(2)
                "       Update this Customer, anyway, with your new values?  (YES)" SKIP(1)
                "       Retrieve the values modified by the other user?      (NO)"  SKIP(1)
                "       Leave your values in place, but cancel the update?   (CANCEL)"
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO-CANCEL TITLE "Update Conflict" UPDATE update-force AS LOGICAL.
        CASE update-force:
            WHEN TRUE THEN DO:      /* YES */
                ASSIGN Customer.
            END.
            WHEN FALSE THEN DO:     /* NO */
                RUN displayCustomer.
            END.
            OTHERWISE DO:           /* CANCEL */
                LEAVE Update-Conflict.
            END.
        END CASE.
    END. /* Update-Conflict */
    ELSE Update-OK: DO:
        MESSAGE 
            "Update Customer in record number" STRING(Record_Count) + "?"
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE update-record AS LOGICAL.
        IF update-record THEN DO:
            ASSIGN Customer.
            MESSAGE 
                "Customer in record number" STRING(Record_Count) "updated." 
            VIEW-AS ALERT-BOX INFORMATION.
        END.
        ELSE DO:
            MESSAGE 
                "Customer in record number" STRING(Record_Count) "NOT updated." 
            VIEW-AS ALERT-BOX INFORMATION.
        END.
    END. /* Update-OK */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME custSpin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL custSpin Dialog-Frame
ON HELP OF custSpin /* CSSpin */
DO:
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL custSpin Dialog-Frame
PROCEDURE custSpin.CSSpin.SPINDOWN .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters Required for this OCX:  
   None
  Notes:       
------------------------------------------------------------------------------*/

    GET PREV Dialog-Frame.
    IF NOT AVAILABLE Customer THEN DO:
        chCSSpin:Value = max-records.
        GET LAST Dialog-Frame.
    END.
    RUN displayCustomer.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL custSpin Dialog-Frame
PROCEDURE custSpin.CSSpin.SPINUP .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters Required for this OCX:  
   None
  Notes:       
------------------------------------------------------------------------------*/

    GET NEXT Dialog-Frame.
    IF NOT AVAILABLE Customer THEN DO:
        /* Control is incremented to last Customer + 1 */
        max-records = chCSSpin:Value - 1.
        chCSSpin:Value = 1.
        GET FIRST Dialog-Frame.
    END.
    RUN displayCustomer.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
    RUN enable_UI.
    
    /* Set max-records from open query in enable_UI. */

    max-records = NUM-RESULTS("Dialog-Frame").
     
    WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE control_load Dialog-Frame _CONTROL-LOAD
PROCEDURE control_load :
/*------------------------------------------------------------------------------
  Purpose:     Load the OCXs    
  Parameters:  <none>
  Notes:       Here we load, initialize and make visible the 
               OCXs in the interface.                        
------------------------------------------------------------------------------*/

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN
DEFINE VARIABLE UIB_S    AS LOGICAL    NO-UNDO.
DEFINE VARIABLE OCXFile  AS CHARACTER  NO-UNDO.

OCXFile = SEARCH( "e-ocx1.wrx":U ).

IF OCXFile <> ? THEN DO:
  ASSIGN
    chcustSpin = custSpin:COM-HANDLE
    UIB_S = chcustSpin:LoadControls( OCXFile, "custSpin":U)
  .
  RUN initialize-controls IN THIS-PROCEDURE NO-ERROR.
END.
ELSE MESSAGE "The file, e-ocx1.wrx, could not be found." skip
             "The controls cannot be loaded."
             VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dialog-Frame _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  HIDE FRAME Dialog-Frame.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE displayCustomer Dialog-Frame 
PROCEDURE displayCustomer :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    Record_Count = chCSSpin:Value.
    DISPLAY sports.Customer.name sports.Customer.Address 
        sports.Customer.Address2 sports.Customer.Balance 
        sports.Customer.City sports.Customer.Comments 
        sports.Customer.Contact sports.Customer.Country 
        sports.Customer.Credit-Limit sports.Customer.Cust-Num 
        sports.Customer.Discount sports.Customer.Postal-Code 
        sports.Customer.Sales-Rep sports.Customer.Terms 
        sports.Customer.state sports.Customer.phone 
        Record_Count
    WITH FRAME Dialog-Frame.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dialog-Frame _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  RUN control_load.

  {&OPEN-QUERY-Dialog-Frame}
  GET FIRST Dialog-Frame.
  DISPLAY Record_Count 
      WITH FRAME Dialog-Frame.
  IF AVAILABLE Customer THEN 
    DISPLAY Customer.Name Customer.Credit-Limit Customer.Address Customer.Terms 
          Customer.Address2 Customer.Discount Customer.City Customer.Sales-Rep 
          Customer.State Customer.Contact Customer.Country Customer.Phone 
          Customer.Postal-Code Customer.Comments Customer.Cust-Num 
          Customer.Balance 
      WITH FRAME Dialog-Frame.
  ENABLE Customer.Name Customer.Credit-Limit Customer.Address Customer.Terms 
         Customer.Address2 Customer.Discount Customer.City Customer.Sales-Rep 
         Customer.State Customer.Contact Customer.Country Customer.Phone 
         Customer.Postal-Code Customer.Comments Btn_Update Btn_Cancel RECT-1 
         Customer.Cust-Num Customer.Balance Record_Count 
      WITH FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initialize-controls Dialog-Frame 
PROCEDURE initialize-controls :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    chCSSpin = chcustSpin:CSSpin.
    
    /* Must initialize Record_Count from the initial */
    /* spin control value after control is loaded.   */
    
    Record_Count = chCSSpin:Value.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


