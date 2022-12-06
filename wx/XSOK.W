&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          sports           PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
  DEFINE VARIABLE curr-record AS RECID.
  DEFINE VARIABLE initial-str AS CHARACTER.

DEFINE VARIABLE credit-left AS DECIMAL LABEL "Credit Left".
DEFINE VARIABLE method-return AS LOGICAL.
DEFINE VARIABLE wh AS WIDGET-HANDLE.
DEFINE VARIABLE olabel AS CHARACTER.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F1
&Scoped-define BROWSE-NAME B1

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Customer

/* Definitions for BROWSE B1                                            */
&Scoped-define FIELDS-IN-QUERY-B1 Customer.Credit-Limit Customer.Cust-Num ~
Customer.Name Customer.Balance 
&Scoped-define ENABLED-FIELDS-IN-QUERY-B1 Customer.Credit-Limit ~
Customer.Name 
&Scoped-define FIELD-PAIRS-IN-QUERY-B1~
 ~{&FP1}Credit-Limit ~{&FP2}Credit-Limit ~{&FP3}~
 ~{&FP1}Name ~{&FP2}Name ~{&FP3}
&Scoped-define ENABLED-TABLES-IN-QUERY-B1 Customer
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-B1 Customer
&Scoped-define OPEN-QUERY-B1 OPEN QUERY B1 FOR EACH Customer NO-LOCK.
&Scoped-define TABLES-IN-QUERY-B1 Customer
&Scoped-define FIRST-TABLE-IN-QUERY-B1 Customer


/* Definitions for FRAME F1                                             */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F1 ~
    ~{&OPEN-QUERY-B1}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS B1 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY B1 FOR 
      Customer SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE B1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS B1 C-Win _STRUCTURED
  QUERY B1 DISPLAY
      Customer.Credit-Limit
      Customer.Cust-Num
      Customer.Name
      Customer.Balance
  ENABLE
      Customer.Credit-Limit
      Customer.Name
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 104 BY 9.52.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F1
     B1 AT ROW 2.19 COL 6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 120.4 BY 16.62.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "<insert window title>"
         HEIGHT             = 16.62
         WIDTH              = 120.4
         MAX-HEIGHT         = 16.62
         MAX-WIDTH          = 120.4
         VIRTUAL-HEIGHT     = 16.62
         VIRTUAL-WIDTH      = 120.4
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME


/* ***************  Runtime Attributes and UIB Settings  ************** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F1
                                                                        */
/* BROWSE-TAB B1 1 F1 */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE B1
/* Query rebuild information for BROWSE B1
     _TblList          = "sports.Customer"
     _FldNameList[1]   > sports.Customer.Credit-Limit
"Credit-Limit" ? ? "decimal" ? ? ? ? ? ? yes ?
     _FldNameList[2]   = sports.Customer.Cust-Num
     _FldNameList[3]   > sports.Customer.Name
"Name" ? ? "character" ? ? ? ? ? ? yes ?
     _FldNameList[4]   = sports.Customer.Balance
     _Query            is OPENED
*/  /* BROWSE B1 */
&ANALYZE-RESUME

 




/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* <insert window title> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* <insert window title> */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME B1
&Scoped-define SELF-NAME B1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B1 C-Win
ON ANY-PRINTABLE OF B1 IN FRAME F1 /* Browse 1 */
DO:
   ASSIGN curr-record = RECID(customer)
     initial-str = initial-str + LAST-EVENT:LABEL.     
     match-string:
     DO ON ERROR UNDO match-string, LEAVE match-string: 
        IF RETRY THEN initial-str = LAST-EVENT:LABEL.               
        message "initial-str: "initial-str.       
        FIND NEXT customer WHERE customer.name BEGINS initial-str 
        USE-INDEX name NO-LOCK NO-ERROR.                
        IF AVAILABLE customer THEN curr-record = RECID(customer).
        ELSE IF RETRY THEN  DO:
           initial-str = "".
           BELL.
        END.
        ELSE UNDO match-string, RETRY match-string.         
     END.
     REPOSITION B1 TO RECID curr-record.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B1 C-Win
ON END-SEARCH OF B1 IN FRAME F1 /* Browse 1 */
DO:
   wh:LABEL = olabel.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B1 C-Win
ON START-SEARCH OF B1 IN FRAME F1 /* Browse 1 */
DO:
   wh = b1:CURRENT-COLUMN.     
    IF wh:LABEL = "name" THEN DO: 
        olabel = wh:label.
        wh:label = "SEARCHING...".     
        MESSAGE "Incremental Search?" VIEW-AS ALERT-BOX QUESTION 
            BUTTONS YES-NO-CANCEL TITLE "Search Mode" 
            UPDATE answ AS LOGICAL.
        CASE answ:
            WHEN TRUE THEN DO:               
                RUN inc-src.
                RETURN. 
            END.
            WHEN FALSE THEN DO:
                MESSAGE "Search mode defaults to the first character."
                VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
                RETURN NO-APPLY.
            END.
            OTHERWISE APPLY "END-SEARCH" TO SELF.
        END CASE.   
    END.
    ELSE DO:
       olabel = wh:LABEL.
       wh:LABEL = "SEARCHING...".
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B1 C-Win
ON VALUE-CHANGED OF B1 IN FRAME F1 /* Browse 1 */
DO:
    message "vchg".
        initial-str = "".

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win _DEFAULT-ENABLE
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
  ENABLE B1 
      WITH FRAME F1 IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-F1}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE inc-src C-Win 
PROCEDURE inc-src :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*   APPLY "START-SEARCH" TO BROWSE b1.
*/
  ON ANY-PRINTABLE OF b1 IN FRAME f1
  DO:    
     ASSIGN curr-record = RECID(customer)
     initial-str = initial-str + LAST-EVENT:LABEL.   
     match-string:
     DO ON ERROR UNDO match-string, LEAVE match-string: 
        IF RETRY THEN initial-str = LAST-EVENT:LABEL.               
        message "initial-str: "initial-str.       
        FIND NEXT customer WHERE customer.name BEGINS initial-str 
        USE-INDEX name NO-LOCK NO-ERROR.                
        IF AVAILABLE customer THEN curr-record = RECID(customer).
        ELSE IF RETRY THEN  DO:
           initial-str = "".
           BELL.
        END.
        ELSE UNDO match-string, RETRY match-string.         
     END.
 
     REPOSITION B1 TO RECID curr-record.
  END. /* of anyprintable */

 
    ON VALUE-CHANGED OF b1 IN FRAME f1
    DO: 
        message "vchg".
        initial-str = "".
    END.
 /*    
    WAIT-FOR "END-SEARCH" OF BROWSE b1.
    APPLY "ENTRY" TO name IN BROWSE b1.
    */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


