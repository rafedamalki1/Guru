&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v7r11 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME DIALOG-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS DIALOG-1 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 04/23/97 - 10:50 am

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{ALLDEF.I}

DEFINE SHARED VARIABLE meter AS INTEGER NO-UNDO.
DEFINE VARIABLE var1 AS INTEGER NO-UNDO.
DEFINE VARIABLE var2 AS INTEGER NO-UNDO.
DEFINE VARIABLE var3 AS INTEGER NO-UNDO.
DEFINE VARIABLE var4 AS INTEGER NO-UNDO.
DEFINE VARIABLE var5 AS INTEGER NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DIALOG-1

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS SEl_1 SEL_2 SEL_3 SEL_4 SEL_5 BTN_OK 
&Scoped-Define DISPLAYED-OBJECTS SEl_1 SEL_2 SEL_3 SEL_4 SEL_5 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_OK AUTO-END-KEY 
     LABEL "Ok" 
     SIZE 14 BY 1
     BGCOLOR 8 .

DEFINE VARIABLE SEl_1 AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     LIST-ITEMS "0","10000","20000","30000","40000","50000","60000","70000","80000","90000" 
     SIZE 8 BY 7 NO-UNDO.

DEFINE VARIABLE SEL_2 AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     LIST-ITEMS "0","1000","2000","3000","4000","5000","6000","7000","8000","9000" 
     SIZE 8 BY 7 NO-UNDO.

DEFINE VARIABLE SEL_3 AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     LIST-ITEMS "0","100","200","300","400","500","600","700","800","900" 
     SIZE 8 BY 7 NO-UNDO.

DEFINE VARIABLE SEL_4 AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     LIST-ITEMS "0","10","20","30","40","50","60","70","80","90" 
     SIZE 8 BY 7 NO-UNDO.

DEFINE VARIABLE SEL_5 AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     LIST-ITEMS "0","1","2","3","4","5","6","7","8","9" 
     SIZE 8 BY 7 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-1
     SEl_1 AT ROW 2.04 COL 4.13 NO-LABEL
     SEL_2 AT ROW 2.04 COL 14.13 NO-LABEL
     SEL_3 AT ROW 2.04 COL 24.13 NO-LABEL
     SEL_4 AT ROW 2.04 COL 34.13 NO-LABEL
     SEL_5 AT ROW 2.04 COL 44.13 NO-LABEL
     BTN_OK AT ROW 9.75 COL 40
     SPACE(0.87) SKIP(0.32)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Antal meter".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Dialog-Box
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX DIALOG-1
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME DIALOG-1:SCROLLABLE       = FALSE
       FRAME DIALOG-1:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME BTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK DIALOG-1
ON CHOOSE OF BTN_OK IN FRAME DIALOG-1 /* Ok */
DO:
   meter = var1 + var2 + var3 + var4 + var5.
   {muswait.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME SEl_1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL SEl_1 DIALOG-1
ON VALUE-CHANGED OF SEl_1 IN FRAME DIALOG-1
DO:
   SEL_1 = INPUT SEL_1.
   IF SEL_1 = "0" THEN var1 = 0.
   ELSE IF SEL_1 = "10000" THEN var1 = 10000.
   ELSE IF SEL_1 = "20000" THEN var1 = 20000.
   ELSE IF SEL_1 = "30000" THEN var1 = 30000.
   ELSE IF SEL_1 = "40000" THEN var1 = 40000.
   ELSE IF SEL_1 = "50000" THEN var1 = 50000.
   ELSE IF SEL_1 = "60000" THEN var1 = 60000.
   ELSE IF SEL_1 = "70000" THEN var1 = 70000.
   ELSE IF SEL_1 = "80000" THEN var1 = 80000.
   ELSE var1 = 90000.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME SEL_2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL SEL_2 DIALOG-1
ON VALUE-CHANGED OF SEL_2 IN FRAME DIALOG-1
DO:
   SEL_2 = INPUT SEL_2.
   IF SEL_2 = "0" THEN var2 = 0.
   ELSE IF SEL_2 = "1000" THEN var2 = 1000.
   ELSE IF SEL_2 = "2000" THEN var2 = 2000.
   ELSE IF SEL_2 = "3000" THEN var2 = 3000.
   ELSE IF SEL_2 = "4000" THEN var2 = 4000.
   ELSE IF SEL_2 = "5000" THEN var2 = 5000.
   ELSE IF SEL_2 = "6000" THEN var2 = 6000.
   ELSE IF SEL_2 = "7000" THEN var2 = 7000.
   ELSE IF SEL_2 = "8000" THEN var2 = 8000.
   ELSE var2 = 9000.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME SEL_3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL SEL_3 DIALOG-1
ON VALUE-CHANGED OF SEL_3 IN FRAME DIALOG-1
DO:
   SEL_3 = INPUT SEL_3.
   IF SEL_3 = "0" THEN var3 = 0.
   ELSE IF SEL_3 = "100" THEN var3 = 100.
   ELSE IF SEL_3 = "200" THEN var3 = 200.
   ELSE IF SEL_3 = "300" THEN var3 = 300.
   ELSE IF SEL_3 = "400" THEN var3 = 400.
   ELSE IF SEL_3 = "500" THEN var3 = 500.
   ELSE IF SEL_3 = "600" THEN var3 = 600.
   ELSE IF SEL_3 = "700" THEN var3 = 700.
   ELSE IF SEL_3 = "800" THEN var3 = 800.
   ELSE var3 = 900.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME SEL_4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL SEL_4 DIALOG-1
ON VALUE-CHANGED OF SEL_4 IN FRAME DIALOG-1
DO:
   SEL_4 = INPUT SEL_4.
   IF SEL_4 = "0" THEN var4 = 0.
   ELSE IF SEL_4 = "10" THEN var4 = 10.
   ELSE IF SEL_4 = "20" THEN var4 = 20.
   ELSE IF SEL_4 = "30" THEN var4 = 30.
   ELSE IF SEL_4 = "40" THEN var4 = 40.
   ELSE IF SEL_4 = "50" THEN var4 = 50.
   ELSE IF SEL_4 = "60" THEN var4 = 60.
   ELSE IF SEL_4 = "70" THEN var4 = 70.
   ELSE IF SEL_4 = "80" THEN var4 = 80.
   ELSE var4 = 90.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME SEL_5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL SEL_5 DIALOG-1
ON VALUE-CHANGED OF SEL_5 IN FRAME DIALOG-1
DO:
   SEL_5 = INPUT SEL_5.
   IF SEL_5 = "0" THEN var5 = 0.
   ELSE IF SEL_5 = "1" THEN var5 = 1.
   ELSE IF SEL_5 = "2" THEN var5 = 2.
   ELSE IF SEL_5 = "3" THEN var5 = 3.
   ELSE IF SEL_5 = "4" THEN var5 = 4.
   ELSE IF SEL_5 = "5" THEN var5 = 5.
   ELSE IF SEL_5 = "6" THEN var5 = 6.
   ELSE IF SEL_5 = "7" THEN var5 = 7.
   ELSE IF SEL_5 = "8" THEN var5 = 8.
   ELSE var5 = 9.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK DIALOG-1 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.

/* Add Trigger to equate WINDOW-CLOSE to END-ERROR                      */
ON WINDOW-CLOSE OF FRAME {&FRAME-NAME} APPLY "END-ERROR":U TO SELF.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
   {DIA_M_START.I}
   ASSIGN 
   SEL_1 = "0"
   SEL_2 = "0"
   SEL_3 = "0"
   SEL_4 = "0"
   SEL_5 = "0".
   RUN enable_UI.       
   {FRMSIZED.I} 
   {musarrow.i}
   {DIA_M_SLUT.I}
   WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI DIALOG-1  _DEFAULT-DISABLE
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
  HIDE FRAME DIALOG-1.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI DIALOG-1  _DEFAULT-ENABLE
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
  DISPLAY SEl_1 SEL_2 SEL_3 SEL_4 SEL_5 
      WITH FRAME DIALOG-1.
  ENABLE SEl_1 SEL_2 SEL_3 SEL_4 SEL_5 BTN_OK 
      WITH FRAME DIALOG-1.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-1}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

