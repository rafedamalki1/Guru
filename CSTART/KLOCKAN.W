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
{ALLDEF.I}

&Scoped-define NEW
/*
DEFINE SHARED VARIABLE klocka LIKE TIDREGITAB.START NO-UNDO.
*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DIALOG-1

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BTN_12 BTN_11 BTN_1 BTN_00 BTN_10 BTN_2 ~
RAD_FMEM BTN_15 BTN_9 BTN_3 BTN_30 BTN_8 BTN_4 BTN_45 BTN_7 BTN_5 BTN_6 ~
BTN_KLAR 
&Scoped-Define DISPLAYED-OBJECTS RAD_FMEM klocka 

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

DEFINE BUTTON BTN_1 
     LABEL "1":L 
     SIZE 3 BY 1.

DEFINE BUTTON BTN_10 
     LABEL "10":L 
     SIZE 3 BY 1.

DEFINE BUTTON BTN_11 
     LABEL "11":L 
     SIZE 3 BY 1.

DEFINE BUTTON BTN_12 
     LABEL "12":L 
     SIZE 3 BY 1.

DEFINE BUTTON BTN_15 
     LABEL "15":L 
     SIZE 3 BY 1.

DEFINE BUTTON BTN_2 
     LABEL "2":L 
     SIZE 3 BY 1.

DEFINE BUTTON BTN_3 
     LABEL "3":L 
     SIZE 3 BY 1.

DEFINE BUTTON BTN_30 
     LABEL "30":L 
     SIZE 3 BY 1.

DEFINE BUTTON BTN_4 
     LABEL "4":L 
     SIZE 3 BY 1.

DEFINE BUTTON BTN_45 
     LABEL "45":L 
     SIZE 3 BY 1.

DEFINE BUTTON BTN_5 
     LABEL "5":L 
     SIZE 3 BY 1.

DEFINE BUTTON BTN_6 
     LABEL "6":L 
     SIZE 3 BY 1.

DEFINE BUTTON BTN_7 
     LABEL "7":L 
     SIZE 3 BY 1.

DEFINE BUTTON BTN_8 
     LABEL "8":L 
     SIZE 3 BY 1.

DEFINE BUTTON BTN_9 
     LABEL "9":L 
     SIZE 3 BY 1.

DEFINE BUTTON BTN_KLAR AUTO-GO 
     LABEL "Ok":L 
     SIZE 14 BY 1.

DEFINE {&NEW} SHARED VARIABLE klocka AS DECIMAL FORMAT ">9.99" INITIAL ? 
     VIEW-AS FILL-IN 
     SIZE 6 BY .96
     FGCOLOR 12  NO-UNDO.

DEFINE VARIABLE RAD_FMEM AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Förmiddag", 1,
"Eftermiddag", 2
     SIZE 13.5 BY 2.5 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-1
     BTN_12 AT ROW 3.5 COL 12.75
     BTN_11 AT ROW 4 COL 8.25
     BTN_1 AT ROW 4 COL 17.25
     BTN_00 AT ROW 4 COL 29.25
     BTN_10 AT ROW 5 COL 4.25
     BTN_2 AT ROW 5 COL 21.25
     RAD_FMEM AT ROW 5.5 COL 7.75 NO-LABEL
     BTN_15 AT ROW 5.5 COL 29.25
     BTN_9 AT ROW 6.5 COL 2.75
     BTN_3 AT ROW 6.5 COL 22.25
     BTN_30 AT ROW 7 COL 29.25
     BTN_8 AT ROW 8 COL 4.25
     BTN_4 AT ROW 8 COL 21.25
     BTN_45 AT ROW 8.5 COL 29.25
     BTN_7 AT ROW 9 COL 8.25
     BTN_5 AT ROW 9 COL 17.25
     BTN_6 AT ROW 9.5 COL 12.75
     BTN_KLAR AT ROW 10.63 COL 26.5
     klocka AT ROW 10.67 COL 9.38 COLON-ALIGNED NO-LABEL
     "Minuter" VIEW-AS TEXT
          SIZE 14 BY 1.5 AT ROW 1.38 COL 24.5
          FONT 17
     "Timmar" VIEW-AS TEXT
          SIZE 12.5 BY 1.5 AT ROW 1.42 COL 9.38
          FONT 17
     SPACE(19.24) SKIP(8.99)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Klocka".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX DIALOG-1
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME DIALOG-1:SCROLLABLE       = FALSE
       FRAME DIALOG-1:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN klocka IN FRAME DIALOG-1
   SHARED NO-ENABLE                                                     */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME BTN_00
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_00 DIALOG-1
ON CHOOSE OF BTN_00 IN FRAME DIALOG-1 /* 00 */
DO:
   klocka = TRUNCATE(klocka,0) + 00.00.
   DISPLAY klocka WITH FRAME {&FRAME-NAME}.              
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_1 DIALOG-1
ON CHOOSE OF BTN_1 IN FRAME DIALOG-1 /* 1 */
DO:     
   IF RAD_FMEM = 1 THEN DO:
      klocka = (klocka - TRUNCATE(klocka,0)) + 01.             
   END.
   IF RAD_FMEM = 2 THEN DO:
      klocka = (klocka - TRUNCATE(klocka,0)) + 13.             
   END.
   DISPLAY klocka WITH FRAME {&FRAME-NAME}.     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_10
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_10 DIALOG-1
ON CHOOSE OF BTN_10 IN FRAME DIALOG-1 /* 10 */
DO:
   IF RAD_FMEM = 1 THEN DO:
      klocka = (klocka - TRUNCATE(klocka,0)) + 10.             
   END.
   IF RAD_FMEM = 2 THEN DO:
      klocka = (klocka - TRUNCATE(klocka,0)) + 22.             
   END.   
   DISPLAY klocka WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_11
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_11 DIALOG-1
ON CHOOSE OF BTN_11 IN FRAME DIALOG-1 /* 11 */
DO:
   IF RAD_FMEM = 1 THEN DO:
      klocka = (klocka - TRUNCATE(klocka,0)) + 11.             
   END.
   IF RAD_FMEM = 2 THEN DO:
      klocka = (klocka - TRUNCATE(klocka,0)) + 23.             
   END.   
   DISPLAY klocka WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_12
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_12 DIALOG-1
ON CHOOSE OF BTN_12 IN FRAME DIALOG-1 /* 12 */
DO:
   IF RAD_FMEM = 1 THEN DO:
      klocka = (klocka - TRUNCATE(klocka,0)) + 00.             
   END.
   IF RAD_FMEM = 2 THEN DO:
      klocka = (klocka - TRUNCATE(klocka,0)) + 12.             
   END.   
   DISPLAY klocka WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_15
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_15 DIALOG-1
ON CHOOSE OF BTN_15 IN FRAME DIALOG-1 /* 15 */
DO:
   klocka = TRUNCATE(klocka,0) + 00.15.
   DISPLAY klocka WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_2 DIALOG-1
ON CHOOSE OF BTN_2 IN FRAME DIALOG-1 /* 2 */
DO:
   IF RAD_FMEM = 1 THEN DO:
      klocka = (klocka - TRUNCATE(klocka,0)) + 02.             
   END.
   IF RAD_FMEM = 2 THEN DO:
      klocka = (klocka - TRUNCATE(klocka,0)) + 14.             
   END.   
   DISPLAY klocka WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_3 DIALOG-1
ON CHOOSE OF BTN_3 IN FRAME DIALOG-1 /* 3 */
DO:
   IF RAD_FMEM = 1 THEN DO:
      klocka = (klocka - TRUNCATE(klocka,0)) + 03.             
   END.
   IF RAD_FMEM = 2 THEN DO:
      klocka = (klocka - TRUNCATE(klocka,0)) + 15.             
   END.   
   DISPLAY klocka WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_30
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_30 DIALOG-1
ON CHOOSE OF BTN_30 IN FRAME DIALOG-1 /* 30 */
DO:
   klocka = TRUNCATE(klocka,0) + 00.30.
   DISPLAY klocka WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_4 DIALOG-1
ON CHOOSE OF BTN_4 IN FRAME DIALOG-1 /* 4 */
DO:
   IF RAD_FMEM = 1 THEN DO:
      klocka = (klocka - TRUNCATE(klocka,0)) + 04.             
   END.
   IF RAD_FMEM = 2 THEN DO:
      klocka = (klocka - TRUNCATE(klocka,0)) + 16.             
   END.
   DISPLAY klocka WITH FRAME {&FRAME-NAME}.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_45
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_45 DIALOG-1
ON CHOOSE OF BTN_45 IN FRAME DIALOG-1 /* 45 */
DO:
   klocka = TRUNCATE(klocka,0) + 00.45.
   DISPLAY klocka WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_5 DIALOG-1
ON CHOOSE OF BTN_5 IN FRAME DIALOG-1 /* 5 */
DO:
   IF RAD_FMEM = 1 THEN DO:
      klocka = (klocka - TRUNCATE(klocka,0)) + 05.             
   END.
   IF RAD_FMEM = 2 THEN DO:
      klocka = (klocka - TRUNCATE(klocka,0)) + 17.             
   END.   
   DISPLAY klocka WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_6
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_6 DIALOG-1
ON CHOOSE OF BTN_6 IN FRAME DIALOG-1 /* 6 */
DO:
   IF RAD_FMEM = 1 THEN DO:
      klocka = (klocka - TRUNCATE(klocka,0)) + 06.             
   END.
   IF RAD_FMEM = 2 THEN DO:
      klocka = (klocka - TRUNCATE(klocka,0)) + 18.             
   END.   
   DISPLAY klocka WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_7
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_7 DIALOG-1
ON CHOOSE OF BTN_7 IN FRAME DIALOG-1 /* 7 */
DO:
   IF RAD_FMEM = 1 THEN DO:
      klocka = (klocka - TRUNCATE(klocka,0)) + 07.             
   END.
   IF RAD_FMEM = 2 THEN DO:
      klocka = (klocka - TRUNCATE(klocka,0)) + 19.             
   END.   
   DISPLAY klocka WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_8
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_8 DIALOG-1
ON CHOOSE OF BTN_8 IN FRAME DIALOG-1 /* 8 */
DO:
   IF RAD_FMEM = 1 THEN DO:
      klocka = (klocka - TRUNCATE(klocka,0)) + 08.             
   END.
   IF RAD_FMEM = 2 THEN DO:
      klocka = (klocka - TRUNCATE(klocka,0)) + 20.             
   END.   
   DISPLAY klocka WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_9
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_9 DIALOG-1
ON CHOOSE OF BTN_9 IN FRAME DIALOG-1 /* 9 */
DO:
   IF RAD_FMEM = 1 THEN DO:
      klocka = (klocka - TRUNCATE(klocka,0)) + 09.             
   END.
   IF RAD_FMEM = 2 THEN DO:
      klocka = (klocka - TRUNCATE(klocka,0)) + 21.             
   END.   
   DISPLAY klocka WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_KLAR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_KLAR DIALOG-1
ON CHOOSE OF BTN_KLAR IN FRAME DIALOG-1 /* Ok */
DO:
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
   RUN label_UI.      
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
   RAD_FMEM = 1.
   IF klocka >= 12.00 THEN RAD_FMEM = 2.
   RUN label_UI.
   RUN enable_UI.       
   {FRMSIZED.I}
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
  DISPLAY RAD_FMEM klocka 
      WITH FRAME DIALOG-1.
  ENABLE BTN_12 BTN_11 BTN_1 BTN_00 BTN_10 BTN_2 RAD_FMEM BTN_15 BTN_9 BTN_3 
         BTN_30 BTN_8 BTN_4 BTN_45 BTN_7 BTN_5 BTN_6 BTN_KLAR 
      WITH FRAME DIALOG-1.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-1}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE label_UI DIALOG-1 
PROCEDURE label_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/                   
   {KLOCKAN.I}           
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

