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
DEFINE INPUT PARAMETER rubrik AS CHARACTER NO-UNDO.
/* Local Variable Definitions ---                                       */
{ALLDEF.I}
&Scoped-define NEW
{GLOBVAR2DEL1.I}
{HOPALLA.I}
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS FILL-IN-TILL FILL-IN-RUBRIK BTN_OK BTN_AVB 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-TILL FILL-IN-RUBRIK 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB AUTO-END-KEY 
     LABEL "Nej" 
     SIZE 14 BY 1
     BGCOLOR 8 .

DEFINE BUTTON BTN_OK AUTO-GO 
     LABEL "Ok" 
     SIZE 14 BY 1
     BGCOLOR 8 .

DEFINE VARIABLE FILL-IN-RUBRIK AS CHARACTER FORMAT "X(256)":U 
     LABEL "Ämne" 
     VIEW-AS FILL-IN 
     SIZE 70.5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-TILL AS CHARACTER FORMAT "X(256)":U INITIAL "elnat@elektroskandia.se" 
     LABEL "Till" 
     VIEW-AS FILL-IN 
     SIZE 70.5 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     FILL-IN-TILL AT ROW 3.25 COL 3.5 WIDGET-ID 32
     FILL-IN-RUBRIK AT ROW 6.5 COL 3.5 WIDGET-ID 34
     BTN_OK AT ROW 11.75 COL 49
     BTN_AVB AT ROW 11.75 COL 64
     "Välj filer som du vill bifoga i ditt E-post program efter du har tryckt OK!" VIEW-AS TEXT
          SIZE 76 BY 2.5 AT ROW 8 COL 3 WIDGET-ID 38
          FONT 17
     "Vill du skicka epost till Elektroskandia?" VIEW-AS TEXT
          SIZE 47 BY 1.17 AT ROW 1.25 COL 2
          FONT 17
     SPACE(32.37) SKIP(11.20)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Välj vad som skall visas"
         DEFAULT-BUTTON BTN_OK CANCEL-BUTTON BTN_AVB.


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
   NOT-VISIBLE FRAME-NAME                                               */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-RUBRIK IN FRAME Dialog-Frame
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FILL-IN-TILL IN FRAME Dialog-Frame
   ALIGN-L                                                              */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Välj vad som skall visas */
DO:
   musz = TRUE.
   APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB Dialog-Frame
ON CHOOSE OF BTN_AVB IN FRAME Dialog-Frame /* Nej */
DO:
   musz = TRUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK Dialog-Frame
ON CHOOSE OF BTN_OK IN FRAME Dialog-Frame /* Ok */
DO:
   ASSIGN
   FILL-IN-RUBRIK = INPUT FILL-IN-RUBRIK
   FILL-IN-TILL = INPUT FILL-IN-TILL.
   musz = FALSE.
   RUN EPOSTKOLL.P (INPUT FILL-IN-TILL,OUTPUT musz).   
   IF musz = FALSE THEN DO:
      APPLY "ENTRY" TO FILL-IN-TILL.
      RETURN NO-APPLY.                 
   END.      
   musz = FALSE.
   
   RUN SPECIALMAPITO.P (INPUT FILL-IN-TILL, INPUT FILL-IN-RUBRIK, INPUT "", INPUT "").
   
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
   FILL-IN-RUBRIK = rubrik. 
   
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
  DISPLAY FILL-IN-TILL FILL-IN-RUBRIK 
      WITH FRAME Dialog-Frame.
  ENABLE FILL-IN-TILL FILL-IN-RUBRIK BTN_OK BTN_AVB 
      WITH FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

