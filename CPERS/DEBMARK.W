&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v7r11 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
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

  Created: 95/06/28 - 12:36 pm

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{ALLDEF.I}

&Scoped-define NEW  
&Scoped-define SHARED SHARED
{DEBPRITAB.I} 
DEFINE SHARED VARIABLE timkostrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.

DEFINE VARIABLE pris1 AS LOGICAL NO-UNDO.    
DEFINE VARIABLE pris2 AS LOGICAL NO-UNDO.   
DEFINE VARIABLE pris3 AS LOGICAL NO-UNDO.   
DEFINE VARIABLE pris4 AS LOGICAL NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DIALOG-1

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES debpris

/* Definitions for DIALOG-BOX DIALOG-1                                  */
&Scoped-define QUERY-STRING-DIALOG-1 FOR EACH debpris NO-LOCK
&Scoped-define OPEN-QUERY-DIALOG-1 OPEN QUERY DIALOG-1 FOR EACH debpris NO-LOCK.
&Scoped-define TABLES-IN-QUERY-DIALOG-1 debpris
&Scoped-define FIRST-TABLE-IN-QUERY-DIALOG-1 debpris


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS debpris.PRISA debpris.PRISA-2 debpris.PRISA-3 ~
debpris.PRISA-4 
&Scoped-define ENABLED-TABLES debpris
&Scoped-define FIRST-ENABLED-TABLE debpris
&Scoped-Define ENABLED-OBJECTS BTN_OK BTN_AVB 
&Scoped-Define DISPLAYED-FIELDS debpris.PERSONALKOD debpris.EFTERNAMN ~
debpris.PRISA debpris.PRISA-2 debpris.PRISA-3 debpris.PRISA-4 
&Scoped-define DISPLAYED-TABLES debpris
&Scoped-define FIRST-DISPLAYED-TABLE debpris
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-PER FILL-IN-ENAMN FILL-IN-DEB-1 ~
FILL-IN-DEB-2 FILL-IN-DEB-3 FILL-IN-DEB-4 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB AUTO-END-KEY 
     LABEL "Avbryt":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_OK AUTO-GO 
     LABEL "Ok":L 
     SIZE 14 BY 1.

DEFINE {&NEW} SHARED VARIABLE FILL-IN-DEB-1 AS CHARACTER FORMAT "X(9)":U INITIAL ? 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE FILL-IN-DEB-2 AS CHARACTER FORMAT "X(9)":U INITIAL ? 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE FILL-IN-DEB-3 AS CHARACTER FORMAT "X(9)":U INITIAL ? 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE FILL-IN-DEB-4 AS CHARACTER FORMAT "X(9)":U INITIAL ? 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE FILL-IN-ENAMN AS CHARACTER FORMAT "X(10)":U INITIAL ? 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE FILL-IN-PER AS CHARACTER FORMAT "X(5)":U INITIAL ? 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY DIALOG-1 FOR 
      debpris SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-1
     FILL-IN-PER AT ROW 2.04 COL 2.13 NO-LABEL
     FILL-IN-ENAMN AT ROW 2.04 COL 8.13 COLON-ALIGNED NO-LABEL
     FILL-IN-DEB-1 AT ROW 2.04 COL 24.63 COLON-ALIGNED NO-LABEL
     FILL-IN-DEB-2 AT ROW 2.04 COL 36.63 COLON-ALIGNED NO-LABEL
     FILL-IN-DEB-3 AT ROW 2.04 COL 48.63 COLON-ALIGNED NO-LABEL
     FILL-IN-DEB-4 AT ROW 2.04 COL 60.63 COLON-ALIGNED NO-LABEL
     debpris.PERSONALKOD AT ROW 3.54 COL 2.13 NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     debpris.EFTERNAMN AT ROW 3.54 COL 7.63 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
     debpris.PRISA AT ROW 3.54 COL 24.63 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 10 BY 1
     debpris.PRISA-2 AT ROW 3.54 COL 36.63 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 10 BY 1
     debpris.PRISA-3 AT ROW 3.54 COL 48.63 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 10 BY 1
     debpris.PRISA-4 AT ROW 3.54 COL 60.63 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 10 BY 1
     BTN_OK AT ROW 5.08 COL 43.63
     BTN_AVB AT ROW 5.08 COL 58.63
     SPACE(0.61) SKIP(0.49)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Ändrig av priser":L.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: 
   Temp-Tables and Buffers:
      TABLE: ? T "?" NO-UNDO temp-db debpris
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX DIALOG-1
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME DIALOG-1:SCROLLABLE       = FALSE
       FRAME DIALOG-1:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN debpris.EFTERNAMN IN FRAME DIALOG-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-DEB-1 IN FRAME DIALOG-1
   SHARED NO-ENABLE                                                     */
/* SETTINGS FOR FILL-IN FILL-IN-DEB-2 IN FRAME DIALOG-1
   SHARED NO-ENABLE                                                     */
/* SETTINGS FOR FILL-IN FILL-IN-DEB-3 IN FRAME DIALOG-1
   SHARED NO-ENABLE                                                     */
/* SETTINGS FOR FILL-IN FILL-IN-DEB-4 IN FRAME DIALOG-1
   SHARED NO-ENABLE                                                     */
/* SETTINGS FOR FILL-IN FILL-IN-ENAMN IN FRAME DIALOG-1
   SHARED NO-ENABLE                                                     */
/* SETTINGS FOR FILL-IN FILL-IN-PER IN FRAME DIALOG-1
   SHARED NO-ENABLE ALIGN-L                                             */
/* SETTINGS FOR FILL-IN debpris.PERSONALKOD IN FRAME DIALOG-1
   NO-ENABLE ALIGN-L                                                    */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX DIALOG-1
/* Query rebuild information for DIALOG-BOX DIALOG-1
     _TblList          = "Temp-Tables.debpris"
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX DIALOG-1 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME DIALOG-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON END-ERROR OF FRAME DIALOG-1 /* Ändrig av priser */
DO:
   musz = TRUE.
   RETURN. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON ENDKEY OF FRAME DIALOG-1 /* Ändrig av priser */
DO:
   musz = TRUE.
   RETURN. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB DIALOG-1
ON CHOOSE OF BTN_AVB IN FRAME DIALOG-1 /* Avbryt */
DO:
   musz = TRUE.
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK DIALOG-1
ON CHOOSE OF BTN_OK IN FRAME DIALOG-1 /* Ok */
DO:
   ASSIGN debpris.PRISA = INPUT debpris.PRISA
   debpris.PRISA-2 = INPUT debpris.PRISA-2
   debpris.PRISA-3 = INPUT debpris.PRISA-3
   debpris.PRISA-4 = INPUT debpris.PRISA-4.
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
   FIND debpris WHERE RECID(debpris) = timkostrec EXCLUSIVE-LOCK NO-ERROR.   
   RUN enable_UI.       
   {FRMSIZED.I}
   IF FILL-IN-DEB-1 = "" THEN debpris.PRISA:HIDDEN = TRUE.  
   IF FILL-IN-DEB-2 = "" THEN debpris.PRISA-2:HIDDEN = TRUE.  
   IF FILL-IN-DEB-3 = "" THEN debpris.PRISA-3:HIDDEN = TRUE.  
   IF FILL-IN-DEB-4 = "" THEN debpris.PRISA-4:HIDDEN = TRUE.  
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
  DISPLAY FILL-IN-PER FILL-IN-ENAMN FILL-IN-DEB-1 FILL-IN-DEB-2 FILL-IN-DEB-3 
          FILL-IN-DEB-4 
      WITH FRAME DIALOG-1.
  IF AVAILABLE debpris THEN 
    DISPLAY debpris.PERSONALKOD debpris.EFTERNAMN debpris.PRISA debpris.PRISA-2 
          debpris.PRISA-3 debpris.PRISA-4 
      WITH FRAME DIALOG-1.
  ENABLE debpris.PRISA debpris.PRISA-2 debpris.PRISA-3 debpris.PRISA-4 BTN_OK 
         BTN_AVB 
      WITH FRAME DIALOG-1.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-1}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

