&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
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
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{ALLDEF.I}

DEFINE NEW SHARED VARIABLE fnytid AS DECIMAL FORMAT "-99.99" NO-UNDO.       

DEFINE SHARED VARIABLE nytid AS DECIMAL FORMAT "99.99" NO-UNDO.
DEFINE SHARED VARIABLE sekunder AS INTEGER FORMAT "-9999999" NO-UNDO.

DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.  
DEFINE SHARED VARIABLE regdatum AS DATE NO-UNDO.
DEFINE SHARED VARIABLE vkdatum AS DATE NO-UNDO.
DEFINE VARIABLE pflex AS INTEGER FORMAT "-9999999" NO-UNDO.
DEFINE VARIABLE perflex AS INTEGER FORMAT "-9999999" NO-UNDO.
DEFINE VARIABLE pover AS INTEGER FORMAT "-9999999" NO-UNDO.
DEFINE VARIABLE ptot AS INTEGER FORMAT "-9999999" NO-UNDO.
DEFINE VARIABLE personal AS CHARACTER NO-UNDO.
DEFINE VARIABLE sdatum AS DATE NO-UNDO.
DEFINE VARIABLE totsal AS DECIMAL FORMAT "99.99" NO-UNDO.
DEFINE VARIABLE diff AS DECIMAL FORMAT "99.99" NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BTN_NVE FILL-IN-TOMDAT BTN_FVE FILL-IN-LOSEN ~
BTN_OK BTN_AVS 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-TOMDAT FILL-IN-LOSEN 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVS AUTO-END-KEY 
     LABEL "Avbryt" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_FVE 
     LABEL "-" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_NVE 
     LABEL "+" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_OK 
     LABEL "Ok" 
     SIZE 14 BY 1.

DEFINE VARIABLE FILL-IN-LOSEN AS CHARACTER FORMAT "X(256)":U 
     LABEL "Lösen" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-TOMDAT AS DATE FORMAT "99/99/99":U 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     BTN_NVE AT ROW 4.29 COL 19.63
     FILL-IN-TOMDAT AT ROW 4.67 COL 8.38 NO-LABEL
     BTN_FVE AT ROW 5.29 COL 19.63
     FILL-IN-LOSEN AT ROW 6.33 COL 6.38 COLON-ALIGNED DEBLANK 
     BTN_OK AT ROW 8 COL 8.5
     BTN_AVS AT ROW 8 COL 23.5
     "registreringar till och med datum :" VIEW-AS TEXT
          SIZE 36 BY 1 AT ROW 2.92 COL 1.5
          FONT 17
     "Flexkörningen  skall gälla alla" VIEW-AS TEXT
          SIZE 31.5 BY 1 AT ROW 1.75 COL 1.5
          FONT 17
     SPACE(4.87) SKIP(6.32)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Flexkörning".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Allow: Basic,Browse,DB-Fields,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-TOMDAT IN FRAME Dialog-Frame
   ALIGN-L                                                              */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Flexkörning */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVS Dialog-Frame
ON CHOOSE OF BTN_AVS IN FRAME Dialog-Frame /* Avbryt */
DO:
   musz = TRUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_FVE
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_FVE Dialog-Frame
ON CHOOSE OF BTN_FVE IN FRAME Dialog-Frame /* - */
DO: 
   ASSIGN
   FILL-IN-TOMDAT = INPUT FILL-IN-TOMDAT.   
   FILL-IN-TOMDAT = FILL-IN-TOMDAT - 1.      
   DISPLAY FILL-IN-TOMDAT WITH FRAME {&FRAME-NAME}. 
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NVE
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NVE Dialog-Frame
ON CHOOSE OF BTN_NVE IN FRAME Dialog-Frame /* + */
DO:   
   ASSIGN
   FILL-IN-TOMDAT = INPUT FILL-IN-TOMDAT.   
   FILL-IN-TOMDAT = FILL-IN-TOMDAT + 1.        
   DISPLAY FILL-IN-TOMDAT WITH FRAME {&FRAME-NAME}.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK Dialog-Frame
ON CHOOSE OF BTN_OK IN FRAME Dialog-Frame /* Ok */
DO:
   musz = FALSE.                
   ASSIGN
   FILL-IN-TOMDAT = INPUT FILL-IN-TOMDAT.
   FILL-IN-LOSEN = INPUT FILL-IN-LOSEN.
   vkdatum = FILL-IN-TOMDAT.
   musz = FALSE.
   IF FILL-IN-LOSEN NE "GURUFLEX" THEN musz = TRUE.
   IF musz = TRUE THEN DO:
      musz = FALSE.
      MESSAGE "Felaktigt lösenord" VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN-LOSEN IN FRAME {&FRAME-NAME}.
      APPLY "ENDKEY" TO BTN_OK IN FRAME {&FRAME-NAME}.
   END.       
   ELSE DO:
      {muswait.i}   
      IF Guru.Konstanter:appcon THEN DO:                                 
         RUN FLXMAN.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT (INPUT vkdatum).       
         RUN EJKSALDO.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT.
         RUN EJKSISTA.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT.
      END.
      ELSE DO:
         RUN FLXMAN.P (INPUT vkdatum).     
         RUN EJKSALDO.P.
         RUN EJKSISTA.P.
      END.     
      
      {musarrow.i} 
      MESSAGE "Flexkörningen är färdig." VIEW-AS ALERT-BOX. 
      APPLY "GO" TO BTN_AVS IN FRAME {&FRAME-NAME}.   
   END.      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-LOSEN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-LOSEN Dialog-Frame
ON LEAVE OF FILL-IN-LOSEN IN FRAME Dialog-Frame /* Lösen */
DO:
   FILL-IN-LOSEN = INPUT FILL-IN-LOSEN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-TOMDAT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-TOMDAT Dialog-Frame
ON LEAVE OF FILL-IN-TOMDAT IN FRAME Dialog-Frame
DO:
   ASSIGN
   FILL-IN-TOMDAT = INPUT FILL-IN-TOMDAT.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-TOMDAT Dialog-Frame
ON MOUSE-MENU-CLICK OF FILL-IN-TOMDAT IN FRAME Dialog-Frame
DO:
   ASSIGN
   FILL-IN-TOMDAT = INPUT FILL-IN-TOMDAT
   Guru.GlobalaVariabler:regdatum = INPUT FILL-IN-TOMDAT.
   RUN AlmanBtn.w.
   FILL-IN-TOMDAT = Guru.GlobalaVariabler:regdatum.
   DISPLAY FILL-IN-TOMDAT WITH FRAME {&FRAME-NAME}.
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
   {DIA_M_START.I}
   {musarrow.i}
   FILL-IN-TOMDAT = TODAY - 1.
   RUN enable_UI.       
   {FRMSIZED.I}
   {DIA_M_SLUT.I}
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
  DISPLAY FILL-IN-TOMDAT FILL-IN-LOSEN 
      WITH FRAME Dialog-Frame.
  ENABLE BTN_NVE FILL-IN-TOMDAT BTN_FVE FILL-IN-LOSEN BTN_OK BTN_AVS 
      WITH FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

