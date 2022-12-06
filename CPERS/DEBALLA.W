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

  Created: 95/06/28 - 12:36 pm

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{ALLDEF.I}
{GLOBVAR2DEL1.I}
&Scoped-define NEW 
&Scoped-define SHARED SHARED
{DEBPRITAB.I}  
{AUTOMTEMP.I}

{SOKDEF.I}
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

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DIALOG-1

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS FILL-IN-PRIS-1 FILL-IN-PRIS-2 FILL-IN-PRIS-3 ~
FILL-IN-PRIS-4 BTN_OK BTN_AVB 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-DEB-1 FILL-IN-DEB-2 FILL-IN-DEB-3 ~
FILL-IN-DEB-4 FILL-IN-PRIS-1 FILL-IN-PRIS-2 FILL-IN-PRIS-3 FILL-IN-PRIS-4 

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

DEFINE VARIABLE FILL-IN-PRIS-1 AS DECIMAL FORMAT "->>>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-PRIS-2 AS DECIMAL FORMAT "->>>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-PRIS-3 AS DECIMAL FORMAT "->>>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-PRIS-4 AS DECIMAL FORMAT "->>>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-1
     FILL-IN-DEB-1 AT ROW 1.75 COL 4.5 COLON-ALIGNED NO-LABEL
     FILL-IN-DEB-2 AT ROW 1.75 COL 16.5 COLON-ALIGNED NO-LABEL
     FILL-IN-DEB-3 AT ROW 1.75 COL 28.5 COLON-ALIGNED NO-LABEL
     FILL-IN-DEB-4 AT ROW 1.75 COL 40.5 COLON-ALIGNED NO-LABEL
     FILL-IN-PRIS-1 AT ROW 3.25 COL 4.5 COLON-ALIGNED NO-LABEL
     FILL-IN-PRIS-2 AT ROW 3.25 COL 16.5 COLON-ALIGNED NO-LABEL
     FILL-IN-PRIS-3 AT ROW 3.25 COL 28.5 COLON-ALIGNED NO-LABEL
     FILL-IN-PRIS-4 AT ROW 3.25 COL 40.5 COLON-ALIGNED NO-LABEL
     BTN_OK AT ROW 4.58 COL 23.5
     BTN_AVB AT ROW 4.58 COL 38.5
     SPACE(1.49) SKIP(0.41)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Ändrig av priser för samtliga":L.


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

/* SETTINGS FOR FILL-IN FILL-IN-DEB-1 IN FRAME DIALOG-1
   SHARED NO-ENABLE                                                     */
/* SETTINGS FOR FILL-IN FILL-IN-DEB-2 IN FRAME DIALOG-1
   SHARED NO-ENABLE                                                     */
/* SETTINGS FOR FILL-IN FILL-IN-DEB-3 IN FRAME DIALOG-1
   SHARED NO-ENABLE                                                     */
/* SETTINGS FOR FILL-IN FILL-IN-DEB-4 IN FRAME DIALOG-1
   SHARED NO-ENABLE                                                     */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX DIALOG-1
/* Query rebuild information for DIALOG-BOX DIALOG-1
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX DIALOG-1 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB DIALOG-1
ON CHOOSE OF BTN_AVB IN FRAME DIALOG-1 /* Avbryt */
DO:
   
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK DIALOG-1
ON CHOOSE OF BTN_OK IN FRAME DIALOG-1 /* Ok */
DO:
   ASSIGN
   FILL-IN-PRIS-1 = INPUT FILL-IN-PRIS-1
   FILL-IN-PRIS-2 = INPUT FILL-IN-PRIS-2
   FILL-IN-PRIS-3 = INPUT FILL-IN-PRIS-3 
   FILL-IN-PRIS-4 = INPUT FILL-IN-PRIS-4.
  
   MESSAGE "Vill du att alla ska ha " LC(Guru.Konstanter:gdebk) " "
   FILL-IN-DEB-1 "="  FILL-IN-PRIS-1 " kr ?"
   VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE val1 AS LOGICAL.
   CASE val1:
      WHEN TRUE THEN DO:
         pris1 = TRUE.          
      END.
      WHEN FALSE THEN DO:
         pris1 = FALSE. 
      END.       
   END CASE. 
   MESSAGE "Vill du att alla ska ha " LC(Guru.Konstanter:gdebk) " "
   FILL-IN-DEB-2 "="  FILL-IN-PRIS-2 "kr ?"
   VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE val2 AS LOGICAL.
   CASE val2:
      WHEN TRUE THEN DO:
         pris2 = TRUE.          
      END.
      WHEN FALSE THEN DO:
         pris2 = FALSE. 
      END.       
   END CASE.
   MESSAGE "Vill du att alla ska ha " LC(Guru.Konstanter:gdebk) " "
   FILL-IN-DEB-3 "="  FILL-IN-PRIS-3 "kr ?"
   VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE val3 AS LOGICAL.
   CASE val3:
      WHEN TRUE THEN DO:
         pris3 = TRUE.          
      END.
      WHEN FALSE THEN DO:
         pris3 = FALSE. 
      END.       
   END CASE. 
   MESSAGE "Vill du att alla ska ha " LC(Guru.Konstanter:gdebk) " "
   FILL-IN-DEB-4 "="  FILL-IN-PRIS-4 "kr ?"
   VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE val4 AS LOGICAL.
   CASE val4:
      WHEN TRUE THEN DO:
         pris4 = TRUE.          
      END.
      WHEN FALSE THEN DO:
         pris4 = FALSE. 
      END.       
   END CASE. 
   FOR EACH valsoktemp:
      DELETE valsoktemp.
   END.   
   CREATE valsoktemp.
   ASSIGN
   valsoktemp.SOKCHAR[1] = FILL-IN-DEB-1
   valsoktemp.SOKCHAR[2] = FILL-IN-DEB-2
   valsoktemp.SOKCHAR[3] = FILL-IN-DEB-3
   valsoktemp.SOKCHAR[4] = FILL-IN-DEB-4.
   IF pris1 = TRUE THEN DO:
      ASSIGN valsoktemp.SOKINT[1] = FILL-IN-PRIS-1.
   END.
   ELSE valsoktemp.SOKCHAR[1] = "XXX". 
   IF pris2 = TRUE THEN DO:
      ASSIGN valsoktemp.SOKINT[2] = FILL-IN-PRIS-2.
   END.
   ELSE valsoktemp.SOKCHAR[2] = "XXX".
   IF pris3 = TRUE THEN DO:   
      ASSIGN valsoktemp.SOKINT[3] = FILL-IN-PRIS-3.
   END.
   ELSE valsoktemp.SOKCHAR[3] = "XXX".
   IF pris4 = TRUE THEN DO:   
      ASSIGN valsoktemp.SOKINT[4] = FILL-IN-PRIS-4.
   END.
   ELSE valsoktemp.SOKCHAR[4] = "XXX".              
   ASSIGN
   valsoktemp.SOKINT[1] = FILL-IN-PRIS-1      
   valsoktemp.SOKINT[2] = FILL-IN-PRIS-2
   valsoktemp.SOKINT[3] = FILL-IN-PRIS-3
   valsoktemp.SOKINT[4] = FILL-IN-PRIS-4.
   IF Guru.Konstanter:appcon THEN DO:                           
      RUN FLRDEB.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT 2,INPUT TABLE valsoktemp,INPUT-OUTPUT TABLE debpris).
   END.
   ELSE DO:
      RUN FLRDEB.P 
      (INPUT 2,INPUT TABLE valsoktemp,INPUT-OUTPUT TABLE debpris).
   END.   
   {musarrow.i} 
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

   FIND FIRST automregtemp WHERE automregtemp.PRISTYP = FILL-IN-DEB-1
   USE-INDEX PRISTYP NO-LOCK NO-ERROR.
   IF NOT AVAILABLE automregtemp THEN musz = musz.
   ELSE
   FILL-IN-PRIS-1 = automregtemp.PRISA.
   FIND FIRST automregtemp WHERE automregtemp.PRISTYP = FILL-IN-DEB-2
   USE-INDEX PRISTYP NO-LOCK NO-ERROR.
   IF NOT AVAILABLE automregtemp THEN musz = musz.
   ELSE
   FILL-IN-PRIS-2 = automregtemp.PRISA.
   FIND FIRST automregtemp WHERE automregtemp.PRISTYP = FILL-IN-DEB-3
   USE-INDEX PRISTYP NO-LOCK NO-ERROR.
   IF NOT AVAILABLE automregtemp THEN musz = musz.
   ELSE
   FILL-IN-PRIS-3 = automregtemp.PRISA.
   FIND FIRST automregtemp WHERE automregtemp.PRISTYP = FILL-IN-DEB-4
   USE-INDEX PRISTYP NO-LOCK NO-ERROR.
   IF NOT AVAILABLE automregtemp THEN musz = musz.
   ELSE
   FILL-IN-PRIS-4 = automregtemp.PRISA.
     
   RUN enable_UI.       
   {FRMSIZED.I}
   IF FILL-IN-DEB-1 = "" THEN FILL-IN-PRIS-1:HIDDEN = TRUE.  
   IF FILL-IN-DEB-2 = "" THEN FILL-IN-PRIS-2:HIDDEN = TRUE.  
   IF FILL-IN-DEB-3 = "" THEN FILL-IN-PRIS-3:HIDDEN = TRUE.  
   IF FILL-IN-DEB-4 = "" THEN FILL-IN-PRIS-4:HIDDEN = TRUE.
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
  DISPLAY FILL-IN-DEB-1 FILL-IN-DEB-2 FILL-IN-DEB-3 FILL-IN-DEB-4 FILL-IN-PRIS-1 
          FILL-IN-PRIS-2 FILL-IN-PRIS-3 FILL-IN-PRIS-4 
      WITH FRAME DIALOG-1.
  ENABLE FILL-IN-PRIS-1 FILL-IN-PRIS-2 FILL-IN-PRIS-3 FILL-IN-PRIS-4 BTN_OK 
         BTN_AVB 
      WITH FRAME DIALOG-1.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-1}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

