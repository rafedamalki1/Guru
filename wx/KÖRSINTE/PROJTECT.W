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
DEFINE INPUT PARAMETER faonr AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER fdelnr AS INTEGER  NO-UNDO.
DEFINE INPUT PARAMETER vama AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER mall AS INTEGER  NO-UNDO.
DEFINE OUTPUT PARAMETER ponr AS CHARACTER FORMAT "X(12)" NO-UNDO.
DEFINE VARIABLE kontaktvar AS CHARACTER NO-UNDO.
/* Local Variable Definitions ---                                       */
{ALLDEF.I}
{GLOBVAR2DEL1.I}
/*{EGENBEN.I}*/
DEFINE VARIABLE edataapph AS HANDLE NO-UNDO.
DEFINE VARIABLE nyttaoapph AS HANDLE NO-UNDO.
{EXTRADATA.I}
&Scoped-define NEW  
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS FILL-IN-PROJEKT FILL-IN-LEDLITT ~
FILL-IN-PNAMN FILL-IN-PLED FILL-IN-KONC FILL-IN-DOMSAGA BTN_OK BTN_AVS 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-PROJEKT FILL-IN-LEDLITT ~
FILL-IN-PNAMN FILL-IN-PLED FILL-IN-KONC FILL-IN-DOMSAGA 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVS 
     LABEL "Avbryt" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_OK 
     LABEL "Ok" 
     SIZE 14 BY 1.

DEFINE VARIABLE FILL-IN-DOMSAGA AS CHARACTER FORMAT "X(256)":U 
     LABEL "Domsaga" 
     VIEW-AS FILL-IN 
     SIZE 26.88 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-KONC AS CHARACTER FORMAT "X(256)":U 
     LABEL "Konc" 
     VIEW-AS FILL-IN 
     SIZE 26.88 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-LEDLITT AS CHARACTER FORMAT "X(256)":U 
     LABEL "Ledningslittera" 
     VIEW-AS FILL-IN 
     SIZE 26.88 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-PLED AS CHARACTER FORMAT "X(256)":U 
     LABEL "Projektledare" 
     VIEW-AS FILL-IN 
     SIZE 27.38 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-PNAMN AS CHARACTER FORMAT "X(256)":U 
     LABEL "Projektnamn" 
     VIEW-AS FILL-IN 
     SIZE 48.38 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-PROJEKT AS CHARACTER FORMAT "X(18)":U 
     LABEL "ProjektID" 
     VIEW-AS FILL-IN 
     SIZE 21.5 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     FILL-IN-PROJEKT AT ROW 1.58 COL 16.63 COLON-ALIGNED
     FILL-IN-LEDLITT AT ROW 2.67 COL 16.63 COLON-ALIGNED
     FILL-IN-PNAMN AT ROW 3.75 COL 16.63 COLON-ALIGNED
     FILL-IN-PLED AT ROW 4.83 COL 16.63 COLON-ALIGNED
     FILL-IN-KONC AT ROW 5.92 COL 16.63 COLON-ALIGNED
     FILL-IN-DOMSAGA AT ROW 7 COL 16.63 COLON-ALIGNED
     BTN_OK AT ROW 8.88 COL 37.38
     BTN_AVS AT ROW 8.88 COL 52.38
     SPACE(1.11) SKIP(0.49)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Nätägarens projektnummer".


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

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON END-ERROR OF FRAME Dialog-Frame /* Nätägarens projektnummer */
DO:
   musz = TRUE.
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON ENDKEY OF FRAME Dialog-Frame /* Nätägarens projektnummer */
DO:
   APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVS Dialog-Frame
ON CHOOSE OF BTN_AVS IN FRAME Dialog-Frame /* Avbryt */
DO:
   
   APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVS Dialog-Frame
ON GO OF BTN_AVS IN FRAME Dialog-Frame /* Avbryt */
DO:
  IF VALID-HANDLE(edataapph) THEN DELETE PROCEDURE edataapph. 
  IF VALID-HANDLE(nyttaoapph) THEN DO:
      RUN borthandle_UI IN nyttaoapph.
      DELETE PROCEDURE nyttaoapph.
      nyttaoapph = ?.
   END. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK Dialog-Frame
ON CHOOSE OF BTN_OK IN FRAME Dialog-Frame /* Ok */
DO:
   musz = FALSE.                
   ASSIGN
   FILL-IN-PROJEKT = INPUT FILL-IN-PROJEKT
   FILL-IN-LEDLITT = INPUT FILL-IN-LEDLITT
   FILL-IN-PNAMN = INPUT FILL-IN-PNAMN
   FILL-IN-PLED = INPUT FILL-IN-PLED
   FILL-IN-KONC = INPUT FILL-IN-KONC
   FILL-IN-DOMSAGA = INPUT FILL-IN-DOMSAGA.

    
   ponr = FILL-IN-PROJEKT.   
   IF FILL-IN-LEDLITT = "" AND FILL-IN-PNAMN = "" AND FILL-IN-PLED = "" AND FILL-IN-KONC = "" AND FILL-IN-DOMSAGA = "" THEN .
   ELSE IF FILL-IN-PROJEKT = "" THEN.
   ELSE DO:   
      EMPTY TEMP-TABLE inextradatatemp NO-ERROR.
      CREATE inextradatatemp.          
      ASSIGN
      inextradatatemp.PROGRAM = "NATPROJ"                   
      inextradatatemp.HUVUDCH = FILL-IN-PROJEKT
      inextradatatemp.SOKCHAR[1] = FILL-IN-LEDLITT
      inextradatatemp.SOKCHAR[2] = FILL-IN-PNAMN
      inextradatatemp.SOKCHAR[3] = FILL-IN-PLED
      inextradatatemp.SOKCHAR[4] = FILL-IN-KONC
      inextradatatemp.SOKCHAR[5] = FILL-IN-DOMSAGA.
      RUN extraspar_UI IN edataapph (INPUT TABLE inextradatatemp). 
   END.
   APPLY "GO" TO BTN_OK IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK Dialog-Frame
ON GO OF BTN_OK IN FRAME Dialog-Frame /* Ok */
DO:
   IF VALID-HANDLE(edataapph) THEN DELETE PROCEDURE edataapph. 
   IF VALID-HANDLE(nyttaoapph) THEN DO:
      RUN borthandle_UI IN nyttaoapph.
      DELETE PROCEDURE nyttaoapph.
      nyttaoapph = ?.
   END. 
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-DOMSAGA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-DOMSAGA Dialog-Frame
ON LEAVE OF FILL-IN-DOMSAGA IN FRAME Dialog-Frame /* Domsaga */
DO:
   ASSIGN
   FILL-IN-DOMSAGA = INPUT FILL-IN-DOMSAGA.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-KONC
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-KONC Dialog-Frame
ON LEAVE OF FILL-IN-KONC IN FRAME Dialog-Frame /* Konc */
DO:
   ASSIGN
   FILL-IN-KONC = INPUT FILL-IN-KONC.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-LEDLITT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-LEDLITT Dialog-Frame
ON LEAVE OF FILL-IN-LEDLITT IN FRAME Dialog-Frame /* Ledningslittera */
DO:
   ASSIGN
   FILL-IN-LEDLITT = INPUT FILL-IN-LEDLITT.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-PLED
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-PLED Dialog-Frame
ON LEAVE OF FILL-IN-PLED IN FRAME Dialog-Frame /* Projektledare */
DO:
  ASSIGN
   FILL-IN-PLED = INPUT FILL-IN-PLED.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-PNAMN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-PNAMN Dialog-Frame
ON LEAVE OF FILL-IN-PNAMN IN FRAME Dialog-Frame /* Projektnamn */
DO:
  ASSIGN
   FILL-IN-PNAMN = INPUT FILL-IN-PNAMN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-PROJEKT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-PROJEKT Dialog-Frame
ON LEAVE OF FILL-IN-PROJEKT IN FRAME Dialog-Frame /* ProjektID */
DO:
   ASSIGN
   FILL-IN-PROJEKT = INPUT FILL-IN-PROJEKT.
   EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
   CREATE inextradatatemp.          
   ASSIGN
   inextradatatemp.PROGRAM = "NATPROJ"                   
   inextradatatemp.HUVUDCH = FILL-IN-PROJEKT.                    
   RUN etabhamt_UI IN edataapph (INPUT TABLE inextradatatemp, OUTPUT TABLE extradatatemp). 
   FIND FIRST extradatatemp NO-LOCK NO-ERROR.
   IF AVAILABLE extradatatemp THEN DO:      
      ASSIGN      
      FILL-IN-LEDLITT = extradatatemp.SOKCHAR[1] 
      FILL-IN-PNAMN = extradatatemp.SOKCHAR[2] 
      FILL-IN-PLED = extradatatemp.SOKCHAR[3]
      FILL-IN-KONC = extradatatemp.SOKCHAR[4]
      FILL-IN-DOMSAGA = extradatatemp.SOKCHAR[5].
   END.   
   DISPLAY FILL-IN-LEDLITT FILL-IN-PNAMN FILL-IN-PLED FILL-IN-KONC FILL-IN-DOMSAGA WITH  FRAME {&FRAME-NAME}.
   IF vama = "vard" AND mall = 6 THEN DO:
      ASSIGN
      FILL-IN-KONC:HIDDEN  IN FRAME {&FRAME-NAME} = FALSE 
      FILL-IN-DOMSAGA:HIDDEN = FALSE.
   END.
   ELSE DO:
      ASSIGN
      FILL-IN-KONC:HIDDEN = TRUE
      FILL-IN-DOMSAGA:HIDDEN = TRUE.
   END.
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
   {ALLSTARTDYN.I}
   IF faonr NE "" THEN DO:   
      RUN projextra3 IN nyttaoapph (INPUT faonr, INPUT fdelnr, OUTPUT ponr,OUTPUT kontaktvar).   
      IF ponr NE "" THEN DO:
         FILL-IN-PROJEKT = ponr.
         EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
         CREATE inextradatatemp.          
         ASSIGN
         inextradatatemp.PROGRAM = "NATPROJ"                   
         inextradatatemp.HUVUDCH = FILL-IN-PROJEKT.                    
         RUN etabhamt_UI IN edataapph (INPUT TABLE inextradatatemp, OUTPUT TABLE extradatatemp). 
         FIND FIRST extradatatemp NO-LOCK NO-ERROR.
         IF AVAILABLE extradatatemp THEN DO:      
            ASSIGN      
            FILL-IN-LEDLITT = extradatatemp.SOKCHAR[1] 
            FILL-IN-PNAMN = extradatatemp.SOKCHAR[2] 
            FILL-IN-PLED = extradatatemp.SOKCHAR[3] 
            FILL-IN-KONC = extradatatemp.SOKCHAR[4]
            FILL-IN-DOMSAGA = extradatatemp.SOKCHAR[5].     
         END.   
      END.
   END.
   
   {musarrow.i}
   RUN enable_UI.       
   IF vama = "vard" AND mall = 6 THEN DO:
      ASSIGN
      FILL-IN-KONC:HIDDEN  IN FRAME {&FRAME-NAME} = FALSE 
      FILL-IN-DOMSAGA:HIDDEN = FALSE.
   END.
   ELSE DO:
      ASSIGN
      FILL-IN-KONC:HIDDEN = TRUE
      FILL-IN-DOMSAGA:HIDDEN = TRUE.
   END.
   {FRMSIZED.I}
   {DIA_M_SLUT.I}
   WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE allstartbrw_UI Dialog-Frame 
PROCEDURE allstartbrw_UI :
/* -----------------------------------------------------------
  Purpose: Changing screen-value for combo-box CMB_OMR     
  Parameters:  Input = Screen-value for CMB_FOR
  Notes:       
-------------------------------------------------------------*/       
   IF Guru.Konstanter:appcon THEN DO:
      RUN NYTTAOAPP.P PERSISTENT SET nyttaoapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
      RUN EXTRADATAHMT.P PERSISTENT SET edataapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT.                  
   END.
   ELSE DO:
      RUN NYTTAOAPP.P PERSISTENT SET nyttaoapph.
      RUN EXTRADATAHMT.P PERSISTENT SET edataapph.      
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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
  DISPLAY FILL-IN-PROJEKT FILL-IN-LEDLITT FILL-IN-PNAMN FILL-IN-PLED 
          FILL-IN-KONC FILL-IN-DOMSAGA 
      WITH FRAME Dialog-Frame.
  ENABLE FILL-IN-PROJEKT FILL-IN-LEDLITT FILL-IN-PNAMN FILL-IN-PLED 
         FILL-IN-KONC FILL-IN-DOMSAGA BTN_OK BTN_AVS 
      WITH FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

