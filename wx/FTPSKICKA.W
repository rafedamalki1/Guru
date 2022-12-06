&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
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
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
DEFINE TEMP-TABLE felmeddtemp 
  FIELD FELMEDD AS CHARACTER
  FIELD VAL AS INTEGER.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS FILL-IN-maskin FILL-IN-ANV FILL-IN-LOSEN ~
FILL-IN-FLYTTA FILL-IN-SPARAS BTN_SKICKA BTN_HAMT Btn_AVB 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-maskin FILL-IN-ANV FILL-IN-LOSEN ~
FILL-IN-FLYTTA FILL-IN-SPARAS 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_AVB AUTO-END-KEY 
     LABEL "Avsluta" 
     SIZE 15 BY 1.13
     BGCOLOR 8 .

DEFINE BUTTON BTN_HAMT 
     LABEL "Hämta" 
     SIZE 15 BY 1.13.

DEFINE BUTTON BTN_SKICKA 
     LABEL "Skicka" 
     SIZE 15 BY 1.13.

DEFINE VARIABLE FILL-IN-ANV AS CHARACTER FORMAT "X(256)":U 
     LABEL "Användare" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-FLYTTA AS CHARACTER FORMAT "X(256)":U 
     LABEL "Fil som skall flyttas" 
     VIEW-AS FILL-IN 
     SIZE 44 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-LOSEN AS CHARACTER FORMAT "X(256)":U 
     LABEL "Lösen" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-maskin AS CHARACTER FORMAT "X(256)":U 
     LABEL "Ftp ip" 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-SPARAS AS CHARACTER FORMAT "X(256)":U 
     LABEL "Fil som skall sparas" 
     VIEW-AS FILL-IN 
     SIZE 43.5 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     FILL-IN-maskin AT ROW 2 COL 22 COLON-ALIGNED
     FILL-IN-ANV AT ROW 3.71 COL 22 COLON-ALIGNED
     FILL-IN-LOSEN AT ROW 5.42 COL 22 COLON-ALIGNED
     FILL-IN-FLYTTA AT ROW 7.13 COL 22 COLON-ALIGNED
     FILL-IN-SPARAS AT ROW 8.75 COL 22 COLON-ALIGNED
     BTN_SKICKA AT ROW 13 COL 10.5
     BTN_HAMT AT ROW 13 COL 44
     Btn_AVB AT ROW 18.25 COL 27
     SPACE(28.37) SKIP(0.49)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Ftp"
         CANCEL-BUTTON Btn_AVB.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Dialog-Box
   Allow: Basic,Browse,DB-Fields,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
                                                                        */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Ftp */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_HAMT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_HAMT Dialog-Frame
ON CHOOSE OF BTN_HAMT IN FRAME Dialog-Frame /* Hämta */
DO:
   RUN ftp_UI (INPUT FALSE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_SKICKA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_SKICKA Dialog-Frame
ON CHOOSE OF BTN_SKICKA IN FRAME Dialog-Frame /* Skicka */
DO:
   RUN ftp_UI (INPUT TRUE).
END.

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
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dialog-Frame  _DEFAULT-DISABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dialog-Frame  _DEFAULT-ENABLE
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
  DISPLAY FILL-IN-maskin FILL-IN-ANV FILL-IN-LOSEN FILL-IN-FLYTTA FILL-IN-SPARAS 
      WITH FRAME Dialog-Frame.
  ENABLE FILL-IN-maskin FILL-IN-ANV FILL-IN-LOSEN FILL-IN-FLYTTA FILL-IN-SPARAS 
         BTN_SKICKA BTN_HAMT Btn_AVB 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ftp_UI Dialog-Frame 
PROCEDURE ftp_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER skicka AS LOGICAL NO-UNDO.
   ASSIGN
   FILL-IN-ANV = INPUT FRAME {&FRAME-NAME} FILL-IN-ANV
   FILL-IN-FLYTTA = INPUT FILL-IN-FLYTTA 
   FILL-IN-LOSEN  = INPUT FILL-IN-LOSEN  
   FILL-IN-maskin = INPUT FILL-IN-maskin 
   FILL-IN-SPARAS = INPUT FILL-IN-SPARAS .
   RUN FTPFILE.P (INPUT FILL-IN-ANV, INPUT FILL-IN-LOSEN, INPUT skicka, INPUT 1,
                     INPUT FILL-IN-FLYTTA, INPUT FILL-IN-SPARAS,
                     INPUT FILL-IN-maskin, OUTPUT TABLE felmeddtemp).
   FIND FIRST felmeddtemp NO-LOCK NO-ERROR.
   IF felmeddtemp.FELMEDD BEGINS "Fil skickad..." THEN DO:
      MESSAGE felmeddtemp.FELMEDD VIEW-AS ALERT-BOX TITLE "Meddelande".
      DELETE felmeddtemp.      
   END.
   ELSE DO:
      FOR EACH felmeddtemp:         
         MESSAGE felmeddtemp.FELMEDD VIEW-AS ALERT-BOX TITLE "Meddelande".
         DELETE felmeddtemp.      
      END.         
   END.      
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

