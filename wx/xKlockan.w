&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
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

  Created: 95/09/28 -  1:51 pm

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
DEFINE SHARED VARIABLE klocka LIKE TIDREGITAB.START NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DIALOG-1

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-41 TOG_12 TOG_11 TOG_1 BTN_00 TOG_10 ~
TOG_2 BTN_15 RAD_FMEM TOG_9 TOG_3 BTN_30 TOG_8 TOG_4 BTN_45 TOG_7 TOG_5 ~
TOG_6 BTN_KLAR 
&Scoped-Define DISPLAYED-OBJECTS TOG_12 TOG_11 TOG_1 TOG_10 TOG_2 RAD_FMEM ~
TOG_9 TOG_3 TOG_8 TOG_4 TOG_7 TOG_5 TOG_6 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_00 
     LABEL "00":L 
     SIZE 3 BY 1.

DEFINE BUTTON BTN_15 
     LABEL "15":L 
     SIZE 3 BY 1.

DEFINE BUTTON BTN_30 
     LABEL "30":L 
     SIZE 3 BY 1.

DEFINE BUTTON BTN_45 
     LABEL "45":L 
     SIZE 3 BY 1.

DEFINE BUTTON BTN_KLAR AUTO-GO 
     LABEL "Klar":L 
     SIZE 9 BY 1.45.

DEFINE VARIABLE RAD_FMEM AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Förmiddag", 1,
"Eftermiddag", 2
     SIZE 13.38 BY 1.95
     BGCOLOR 8  NO-UNDO.

DEFINE RECTANGLE RECT-41
     EDGE-PIXELS 4 GRAPHIC-EDGE  
     SIZE 58 BY 10.68
     BGCOLOR 8 .

DEFINE VARIABLE TOG_1 AS LOGICAL INITIAL no 
     LABEL "1" 
     VIEW-AS TOGGLE-BOX
     SIZE 4.25 BY .91 NO-UNDO.

DEFINE VARIABLE TOG_10 AS LOGICAL INITIAL no 
     LABEL "10" 
     VIEW-AS TOGGLE-BOX
     SIZE 5.13 BY .91 NO-UNDO.

DEFINE VARIABLE TOG_11 AS LOGICAL INITIAL no 
     LABEL "11" 
     VIEW-AS TOGGLE-BOX
     SIZE 4.25 BY .91 NO-UNDO.

DEFINE VARIABLE TOG_12 AS LOGICAL INITIAL no 
     LABEL "12" 
     VIEW-AS TOGGLE-BOX
     SIZE 4.25 BY .91 NO-UNDO.

DEFINE VARIABLE TOG_2 AS LOGICAL INITIAL no 
     LABEL "2" 
     VIEW-AS TOGGLE-BOX
     SIZE 4.25 BY .91 NO-UNDO.

DEFINE VARIABLE TOG_3 AS LOGICAL INITIAL no 
     LABEL "3" 
     VIEW-AS TOGGLE-BOX
     SIZE 4.25 BY .91 NO-UNDO.

DEFINE VARIABLE TOG_4 AS LOGICAL INITIAL no 
     LABEL "4" 
     VIEW-AS TOGGLE-BOX
     SIZE 4.25 BY .91 NO-UNDO.

DEFINE VARIABLE TOG_5 AS LOGICAL INITIAL no 
     LABEL "5" 
     VIEW-AS TOGGLE-BOX
     SIZE 4.25 BY .91 NO-UNDO.

DEFINE VARIABLE TOG_6 AS LOGICAL INITIAL no 
     LABEL "6" 
     VIEW-AS TOGGLE-BOX
     SIZE 4.25 BY .91 NO-UNDO.

DEFINE VARIABLE TOG_7 AS LOGICAL INITIAL no 
     LABEL "7" 
     VIEW-AS TOGGLE-BOX
     SIZE 4.25 BY .91 NO-UNDO.

DEFINE VARIABLE TOG_8 AS LOGICAL INITIAL no 
     LABEL "8" 
     VIEW-AS TOGGLE-BOX
     SIZE 4.25 BY .91 NO-UNDO.

DEFINE VARIABLE TOG_9 AS LOGICAL INITIAL no 
     LABEL "9" 
     VIEW-AS TOGGLE-BOX
     SIZE 4.25 BY .91 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-1
     TOG_12 AT ROW 3.14 COL 14.63
     TOG_11 AT ROW 3.55 COL 10.38
     TOG_1 AT ROW 3.55 COL 19.13
     BTN_00 AT ROW 3.82 COL 32.38
     TOG_10 AT ROW 4.64 COL 6.38
     TOG_2 AT ROW 4.64 COL 23.25
     BTN_15 AT ROW 5.32 COL 32.38
     RAD_FMEM AT ROW 5.73 COL 9.75 NO-LABEL
     TOG_9 AT ROW 6.09 COL 4.88
     TOG_3 AT ROW 6.09 COL 24.25
     BTN_30 AT ROW 6.82 COL 32.38
     TOG_8 AT ROW 7.59 COL 6.38
     TOG_4 AT ROW 7.59 COL 23.25
     BTN_45 AT ROW 8.32 COL 32.38
     TOG_7 AT ROW 8.59 COL 10.38
     TOG_5 AT ROW 8.59 COL 19.13
     TOG_6 AT ROW 9.09 COL 14.63
     BTN_KLAR AT ROW 9.82 COL 32.38
     RECT-41 AT ROW 1.14 COL 1.25
     "MINUTER" VIEW-AS TEXT
          SIZE 14 BY 1.5 AT ROW 1.55 COL 27.63
          FONT 17
     "TIMMAR" VIEW-AS TEXT
          SIZE 12.5 BY 1.5 AT ROW 1.59 COL 11.25
          FONT 17
     SPACE(35.99) SKIP(8.73)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Klocka".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS


/* ***************  Runtime Attributes and UIB Settings  ************** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX DIALOG-1
                                                                        */
ASSIGN 
       FRAME DIALOG-1:SCROLLABLE       = FALSE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 




/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME BTN_00
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_00 DIALOG-1
ON CHOOSE OF BTN_00 IN FRAME DIALOG-1 /* 00 */
DO:
   klocka = TRUNCATE(klocka,0) + 00.00.             
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_15
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_15 DIALOG-1
ON CHOOSE OF BTN_15 IN FRAME DIALOG-1 /* 15 */
DO:
   klocka = TRUNCATE(klocka,0) + 00.15.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_30
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_30 DIALOG-1
ON CHOOSE OF BTN_30 IN FRAME DIALOG-1 /* 30 */
DO:
   klocka = TRUNCATE(klocka,0) + 00.30.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_45
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_45 DIALOG-1
ON CHOOSE OF BTN_45 IN FRAME DIALOG-1 /* 45 */
DO:
   klocka = TRUNCATE(klocka,0) + 00.45.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_KLAR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_KLAR DIALOG-1
ON CHOOSE OF BTN_KLAR IN FRAME DIALOG-1 /* Klar */
DO:
   ASSIGN
   TOG_1 = INPUT TOG_1 
   TOG_2 = INPUT TOG_2 
   TOG_3 = INPUT TOG_3
   TOG_4 = INPUT TOG_4 
   TOG_5 = INPUT TOG_5 
   TOG_6 = INPUT TOG_6
   TOG_7 = INPUT TOG_7 
   TOG_8 = INPUT TOG_8 
   TOG_9 = INPUT TOG_9
   TOG_10 = INPUT TOG_10 
   TOG_11 = INPUT TOG_11 
   TOG_12 = INPUT TOG_12.   
   IF TOG_1 = TRUE THEN DO:
      IF RAD_FMEM = 1 THEN DO:
         klocka = (klocka - TRUNCATE(klocka,0)) + 01.             
      END.
      IF RAD_FMEM = 2 THEN DO:
         klocka = (klocka - TRUNCATE(klocka,0)) + 13.             
      END.  
   END.
   ELSE IF TOG_2 = TRUE THEN DO: 
      IF RAD_FMEM = 1 THEN DO:
         klocka = (klocka - TRUNCATE(klocka,0)) + 2.             
      END.
      IF RAD_FMEM = 2 THEN DO:
         klocka = (klocka - TRUNCATE(klocka,0)) + 14.             
      END.  
   END.
   ELSE IF TOG_3 = TRUE THEN DO: 
      IF RAD_FMEM = 1 THEN DO:
         klocka = (klocka - TRUNCATE(klocka,0)) + 3.             
      END.
      IF RAD_FMEM = 2 THEN DO:
         klocka = (klocka - TRUNCATE(klocka,0)) + 15.             
      END.  
   END.
   ELSE IF TOG_4 = TRUE THEN DO:
      IF RAD_FMEM = 1 THEN DO:
         klocka = (klocka - TRUNCATE(klocka,0)) + 4.             
      END.
      IF RAD_FMEM = 2 THEN DO:
         klocka = (klocka - TRUNCATE(klocka,0)) + 16.             
      END.  
   END. 
   ELSE IF TOG_5 = TRUE THEN DO:
      IF RAD_FMEM = 1 THEN DO:
         klocka = (klocka - TRUNCATE(klocka,0)) + 5.             
      END.
      IF RAD_FMEM = 2 THEN DO:
         klocka = (klocka - TRUNCATE(klocka,0)) + 17.             
      END.  
   END. 
   ELSE IF TOG_6 = TRUE THEN DO:
      IF RAD_FMEM = 1 THEN DO:
         klocka = (klocka - TRUNCATE(klocka,0)) + 6.             
      END.
      IF RAD_FMEM = 2 THEN DO:
         klocka = (klocka - TRUNCATE(klocka,0)) + 18.             
      END.  
   END. 
   ELSE IF TOG_7 = TRUE THEN DO:
      IF RAD_FMEM = 1 THEN DO:
         klocka = (klocka - TRUNCATE(klocka,0)) + 7.             
      END.
      IF RAD_FMEM = 2 THEN DO:
         klocka = (klocka - TRUNCATE(klocka,0)) + 19.             
      END.  
   END. 
   ELSE IF TOG_8 = TRUE THEN DO:
      IF RAD_FMEM = 1 THEN DO:
         klocka = (klocka - TRUNCATE(klocka,0)) + 8.             
      END.
      IF RAD_FMEM = 2 THEN DO:
         klocka = (klocka - TRUNCATE(klocka,0)) + 20.             
      END.  
   END. 
   ELSE IF TOG_9 = TRUE THEN DO:
      IF RAD_FMEM = 1 THEN DO:
         klocka = (klocka - TRUNCATE(klocka,0)) + 9.             
      END.
      IF RAD_FMEM = 2 THEN DO:
         klocka = (klocka - TRUNCATE(klocka,0)) + 21.             
      END.  
   END. 
   ELSE IF TOG_10 = TRUE THEN DO:
      IF RAD_FMEM = 1 THEN DO:
         klocka = (klocka - TRUNCATE(klocka,0)) + 10.             
      END.
      IF RAD_FMEM = 2 THEN DO:
         klocka = (klocka - TRUNCATE(klocka,0)) + 22.             
      END.  
   END. 
   ELSE IF TOG_11 = TRUE THEN DO:
      IF RAD_FMEM = 1 THEN DO:
         klocka = (klocka - TRUNCATE(klocka,0)) + 11.             
      END.
      IF RAD_FMEM = 2 THEN DO:
         klocka = (klocka - TRUNCATE(klocka,0)) + 23.             
      END.  
   END. 
   ELSE IF TOG_12 = TRUE THEN DO: 
      IF RAD_FMEM = 1 THEN DO:
         klocka = (klocka - TRUNCATE(klocka,0)) + 00.             
      END.
      IF RAD_FMEM = 2 THEN DO:
         klocka = (klocka - TRUNCATE(klocka,0)) + 12.             
      END.  
   END.   
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RAD_FMEM
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RAD_FMEM DIALOG-1
ON VALUE-CHANGED OF RAD_FMEM IN FRAME DIALOG-1
DO:
   RAD_FMEM = INPUT RAD_FMEM. 
   IF RAD_FMEM = 1 THEN DO:
      IF klocka >= 12.00 THEN DO:
         klocka = klocka - 12.00.
      END.
   END.      
   IF RAD_FMEM = 2 THEN DO:
      IF klocka < 12.00 THEN DO:
         klocka = klocka + 12.00.
      END.
   END.      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_12
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_12 DIALOG-1
ON VALUE-CHANGED OF TOG_12 IN FRAME DIALOG-1 /* 12 */
DO:
   RUN tog_UI.
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
   RAD_FMEM = 1.
   IF klocka >= 12.00 THEN RAD_FMEM = 2.
   ASSIGN
   TOG_1 = FALSE TOG_2 = FALSE TOG_3 = FALSE
   TOG_4 = FALSE TOG_5 = FALSE TOG_6 = FALSE
   TOG_7 = FALSE TOG_8 = FALSE TOG_9 = FALSE
   TOG_10 = FALSE TOG_11 = FALSE TOG_12 = FALSE.
   IF TRUNCATE(klocka,0) = 1 OR TRUNCATE(klocka,0) = 13 THEN TOG_1 = TRUE.
   ELSE IF TRUNCATE(klocka,0) = 2 OR TRUNCATE(klocka,0) = 14 THEN TOG_2 = TRUE.
   ELSE IF TRUNCATE(klocka,0) = 3 OR TRUNCATE(klocka,0) = 15 THEN TOG_3 = TRUE.
   ELSE IF TRUNCATE(klocka,0) = 4 OR TRUNCATE(klocka,0) = 16 THEN TOG_4 = TRUE.
   ELSE IF TRUNCATE(klocka,0) = 5 OR TRUNCATE(klocka,0) = 17 THEN TOG_5 = TRUE.
   ELSE IF TRUNCATE(klocka,0) = 6 OR TRUNCATE(klocka,0) = 18 THEN TOG_6 = TRUE.
   ELSE IF TRUNCATE(klocka,0) = 7 OR TRUNCATE(klocka,0) = 19 THEN TOG_7 = TRUE.
   ELSE IF TRUNCATE(klocka,0) = 8 OR TRUNCATE(klocka,0) = 20 THEN TOG_8 = TRUE.
   ELSE IF TRUNCATE(klocka,0) = 9 OR TRUNCATE(klocka,0) = 21 THEN TOG_9 = TRUE.
   ELSE IF TRUNCATE(klocka,0) = 10 OR TRUNCATE(klocka,0) = 22 THEN TOG_10 = TRUE.
   ELSE IF TRUNCATE(klocka,0) = 11 OR TRUNCATE(klocka,0) = 23 THEN TOG_11 = TRUE.
   ELSE IF TRUNCATE(klocka,0) = 12 OR TRUNCATE(klocka,0) = 24 THEN TOG_12 = TRUE.   
   RUN enable_UI.
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI DIALOG-1 _DEFAULT-DISABLE
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI DIALOG-1 _DEFAULT-ENABLE
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
  DISPLAY TOG_12 TOG_11 TOG_1 TOG_10 TOG_2 RAD_FMEM TOG_9 TOG_3 TOG_8 TOG_4 
          TOG_7 TOG_5 TOG_6 
      WITH FRAME DIALOG-1.
  ENABLE RECT-41 TOG_12 TOG_11 TOG_1 BTN_00 TOG_10 TOG_2 BTN_15 RAD_FMEM TOG_9 
         TOG_3 BTN_30 TOG_8 TOG_4 BTN_45 TOG_7 TOG_5 TOG_6 BTN_KLAR 
      WITH FRAME DIALOG-1.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-1}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


