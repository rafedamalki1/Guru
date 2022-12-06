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
DEFINE INPUT PARAMETER vad AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER ilkod AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER projsnabb AS INTEGER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER togpris AS LOGICAL NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER togtot AS LOGICAL NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER togupp AS LOGICAL NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER toggbygg AS LOGICAL NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER togskylt AS LOGICAL NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER togdelaupp AS LOGICAL NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER delamed AS INTEGER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER visastolp AS LOGICAL NO-UNDO.
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
&Scoped-Define ENABLED-OBJECTS TOG_PRIS TOG_TMS TOG_MS TOG_Bygg ~
FILL-IN-DELAMED TOG_DELAUPP TOG_SKYLT TOG_STOLP BTN_OK BTN_AVB 
&Scoped-Define DISPLAYED-OBJECTS TOG_PRIS TOG_TMS TOG_MS TOG_Bygg 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB AUTO-END-KEY 
     LABEL "Avbryt" 
     SIZE 14 BY 1
     BGCOLOR 8 .

DEFINE BUTTON BTN_OK AUTO-GO 
     LABEL "Ok" 
     SIZE 14 BY 1
     BGCOLOR 8 .

DEFINE VARIABLE FILL-IN-DELAMED AS INTEGER FORMAT ">>>9":U INITIAL 0 
     LABEL "Dela med" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1 NO-UNDO.

DEFINE VARIABLE TOG_Bygg AS LOGICAL INITIAL no 
     LABEL "Byggprotokoll" 
     VIEW-AS TOGGLE-BOX
     SIZE 33 BY .88 NO-UNDO.

DEFINE VARIABLE TOG_DELAUPP AS LOGICAL INITIAL no 
     LABEL "Dela upp mtrlantal" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .88 NO-UNDO.

DEFINE VARIABLE TOG_MS AS LOGICAL INITIAL no 
     LABEL "Materielsammanställning per upplag" 
     VIEW-AS TOGGLE-BOX
     SIZE 33 BY .88 NO-UNDO.

DEFINE VARIABLE TOG_PRIS AS LOGICAL INITIAL no 
     LABEL "Visa med pris" 
     VIEW-AS TOGGLE-BOX
     SIZE 34.5 BY .88 NO-UNDO.

DEFINE VARIABLE TOG_SKYLT AS LOGICAL INITIAL no 
     LABEL "Skyltmall" 
     VIEW-AS TOGGLE-BOX
     SIZE 33 BY .88 NO-UNDO.

DEFINE VARIABLE TOG_STOLP AS LOGICAL INITIAL no 
     LABEL "Visa stolpprotokoll" 
     VIEW-AS TOGGLE-BOX
     SIZE 33 BY .88 NO-UNDO.

DEFINE VARIABLE TOG_TMS AS LOGICAL INITIAL no 
     LABEL "Total Materielsammanställning" 
     VIEW-AS TOGGLE-BOX
     SIZE 33 BY .88 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     TOG_PRIS AT ROW 2.67 COL 1.5
     TOG_TMS AT ROW 3.92 COL 1.5
     TOG_MS AT ROW 5.17 COL 1.5
     TOG_Bygg AT ROW 6.42 COL 1.5 WIDGET-ID 28
     FILL-IN-DELAMED AT ROW 7.54 COL 31.25 COLON-ALIGNED WIDGET-ID 24
     TOG_DELAUPP AT ROW 7.67 COL 1.5 WIDGET-ID 2
     TOG_SKYLT AT ROW 8.92 COL 1.5 WIDGET-ID 4
     TOG_STOLP AT ROW 10.21 COL 1.5 WIDGET-ID 26
     BTN_OK AT ROW 11.63 COL 20.75
     BTN_AVB AT ROW 11.63 COL 35.75
     "Välj för visning:" VIEW-AS TEXT
          SIZE 31.5 BY 1.17 AT ROW 1.25 COL 2
          FONT 17
     SPACE(17.49) SKIP(11.20)
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

/* SETTINGS FOR FILL-IN FILL-IN-DELAMED IN FRAME Dialog-Frame
   NO-DISPLAY                                                           */
ASSIGN 
       FILL-IN-DELAMED:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR TOGGLE-BOX TOG_DELAUPP IN FRAME Dialog-Frame
   NO-DISPLAY                                                           */
ASSIGN 
       TOG_DELAUPP:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR TOGGLE-BOX TOG_SKYLT IN FRAME Dialog-Frame
   NO-DISPLAY                                                           */
ASSIGN 
       TOG_SKYLT:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR TOGGLE-BOX TOG_STOLP IN FRAME Dialog-Frame
   NO-DISPLAY                                                           */
ASSIGN 
       TOG_STOLP:HIDDEN IN FRAME Dialog-Frame           = TRUE.

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
ON CHOOSE OF BTN_AVB IN FRAME Dialog-Frame /* Avbryt */
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
   toggbygg = INPUT TOG_Bygg
   togpris = INPUT TOG_PRIS
   togtot = INPUT TOG_TMS
   togupp = INPUT TOG_MS.
   togskylt = INPUT TOG_SKYLT.
   togdelaupp = INPUT TOG_DELAUPP.
   visastolp = INPUT TOG_STOLP.
   delamed = INPUT FILL-IN-DELAMED.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-DELAMED
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-DELAMED Dialog-Frame
ON LEAVE OF FILL-IN-DELAMED IN FRAME Dialog-Frame /* Dela med */
DO:  
   
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_Bygg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_Bygg Dialog-Frame
ON VALUE-CHANGED OF TOG_Bygg IN FRAME Dialog-Frame /* Byggprotokoll */
DO:
  /*
   TOG_Bygg = INPUT TOG_Bygg.
   IF projsnabb = 1 THEN DO:
      IF TOG_Bygg = TRUE THEN DO:
         TOG_TMS = FALSE.
         TOG_MS = FALSE.
      END.   
      ELSE DO:
         TOG_TMS = TRUE.
      END.    
     DISPLAY TOG_TMS TOG_MS TOG_Bygg 
         WITH FRAME Dialog-Frame. 
   END.
   */ 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_MS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_MS Dialog-Frame
ON VALUE-CHANGED OF TOG_MS IN FRAME Dialog-Frame /* Materielsammanställning per upplag */
DO:
/*
   TOG_MS = INPUT TOG_MS.
   IF projsnabb = 1 THEN DO:
      IF TOG_MS = TRUE THEN DO:
         TOG_Bygg = FALSE.
      END.   
      ELSE DO:
         IF TOG_TMS = FALSE THEN TOG_Bygg = TRUE.
      END. 
      DISPLAY  TOG_TMS TOG_MS TOG_Bygg 
         WITH FRAME Dialog-Frame.
   END.
   */      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_TMS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_TMS Dialog-Frame
ON VALUE-CHANGED OF TOG_TMS IN FRAME Dialog-Frame /* Total Materielsammanställning */
DO:
   /*
   TOG_TMS = INPUT TOG_TMS.
   IF projsnabb = 1 THEN DO:
      IF TOG_TMS = TRUE THEN DO:
         TOG_Bygg = FALSE.
      END.   
      ELSE DO:
         IF TOG_MS = FALSE THEN TOG_Bygg = TRUE.
      END. 
      DISPLAY TOG_TMS TOG_MS TOG_Bygg 
         WITH FRAME Dialog-Frame.
   END.
   */      
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
   TOG_Bygg = toggbygg.  
   TOG_TMS = togtot.
   TOG_PRIS = togpris.
   TOG_MS = togupp.
   TOG_SKYLT = togskylt.
   TOG_DELAUPP = togdelaupp.
   TOG_STOLP = visastolp.   
   RUN enable_UI.       
   {FRMSIZED.I}   
   TOG_SKYLT:HIDDEN IN FRAME {&FRAME-NAME}           = TRUE.   
   TOG_DELAUPP:HIDDEN IN FRAME {&FRAME-NAME}           = TRUE.
   FILL-IN-DELAMED:HIDDEN IN FRAME {&FRAME-NAME}           = TRUE.   
   IF vad = 2 OR vad = 12 THEN TOG_STOLP:HIDDEN IN FRAME {&FRAME-NAME}           = FALSE.
   ELSE TOG_STOLP:HIDDEN IN FRAME {&FRAME-NAME}           = TRUE.
   IF Guru.Konstanter:globforetag = "gran" THEN DO:
      IF ilkod BEGINS  "3" THEN DO:
         TOG_DELAUPP = TRUE.             
         TOG_DELAUPP:HIDDEN IN FRAME {&FRAME-NAME}           = FALSE.
         FILL-IN-DELAMED = 500.
         FILL-IN-DELAMED:HIDDEN IN FRAME {&FRAME-NAME}           = FALSE.
         DISPLAY TOG_DELAUPP FILL-IN-DELAMED WITH FRAME {&FRAME-NAME}.
      END. 
      ELSE TOG_DELAUPP = FALSE.  
   END.  
   IF Guru.Konstanter:mtrlsekvar[6]  = TRUE THEN DO:
      TOG_PRIS = FALSE.
      DISABLE TOG_PRIS WITH FRAME Dialog-Frame.
      DISPLAY TOG_PRIS WITH FRAME Dialog-Frame.
   END.   
   IF projsnabb = 2 THEN TOG_Bygg:HIDDEN = TRUE.      
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
  DISPLAY TOG_PRIS TOG_TMS TOG_MS TOG_Bygg 
      WITH FRAME Dialog-Frame.
  ENABLE TOG_PRIS TOG_TMS TOG_MS TOG_Bygg FILL-IN-DELAMED TOG_DELAUPP TOG_SKYLT 
         TOG_STOLP BTN_OK BTN_AVB 
      WITH FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

