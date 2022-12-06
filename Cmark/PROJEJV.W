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
FILL-IN-LEDAGARE FILL-IN-AGORG FILL-IN-AGFAST FILL-IN-AGKOMMUN ~
FILL-IN-AGLAN BTN_OK BTN_AVS 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-PROJEKT FILL-IN-LEDLITT ~
FILL-IN-LEDAGARE FILL-IN-AGORG FILL-IN-AGFAST FILL-IN-AGKOMMUN ~
FILL-IN-AGLAN 

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

DEFINE VARIABLE FILL-IN-AGFAST AS CHARACTER FORMAT "X(256)":U 
     LABEL "Ägande fastighet" 
     VIEW-AS FILL-IN 
     SIZE 41.5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-AGKOMMUN AS CHARACTER FORMAT "X(256)":U 
     LABEL "Kommun" 
     VIEW-AS FILL-IN 
     SIZE 27.38 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-AGLAN AS CHARACTER FORMAT "X(256)":U 
     LABEL "Län" 
     VIEW-AS FILL-IN 
     SIZE 26.88 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-AGORG AS CHARACTER FORMAT "X(256)":U 
     LABEL "Ledningsägare orgnr" 
     VIEW-AS FILL-IN 
     SIZE 26.88 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-LEDAGARE AS CHARACTER FORMAT "X(256)":U 
     LABEL "Ledningsägare" 
     VIEW-AS FILL-IN 
     SIZE 26.88 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-LEDLITT AS CHARACTER FORMAT "X(256)":U 
     LABEL "Ledningslittera" 
     VIEW-AS FILL-IN 
     SIZE 26.88 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-PROJEKT AS CHARACTER FORMAT "X(18)":U 
     LABEL "Nätägares projektnummer" 
     VIEW-AS FILL-IN 
     SIZE 21.5 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     FILL-IN-PROJEKT AT ROW 1.58 COL 24 COLON-ALIGNED
     FILL-IN-LEDLITT AT ROW 2.67 COL 24 COLON-ALIGNED
     FILL-IN-LEDAGARE AT ROW 3.75 COL 24 COLON-ALIGNED
     FILL-IN-AGORG AT ROW 4.83 COL 24 COLON-ALIGNED
     FILL-IN-AGFAST AT ROW 5.92 COL 24 COLON-ALIGNED
     FILL-IN-AGKOMMUN AT ROW 7 COL 24 COLON-ALIGNED
     FILL-IN-AGLAN AT ROW 8.17 COL 24 COLON-ALIGNED
     BTN_OK AT ROW 9.83 COL 46.38
     BTN_AVS AT ROW 9.83 COL 61.38
     SPACE(1.36) SKIP(0.12)
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
   FILL-IN-LEDAGARE = INPUT FILL-IN-LEDAGARE
   FILL-IN-AGFAST = INPUT FILL-IN-AGFAST
   FILL-IN-AGKOMMUN = INPUT FILL-IN-AGKOMMUN
   FILL-IN-AGLAN = INPUT FILL-IN-AGLAN
   FILL-IN-AGORG = INPUT FILL-IN-AGORG.   

    
   ponr = FILL-IN-PROJEKT.   
   IF FILL-IN-LEDLITT = "" AND FILL-IN-LEDAGARE = "" AND FILL-IN-AGFAST = "" AND FILL-IN-AGKOMMUN = "" AND FILL-IN-AGLAN = "" AND FILL-IN-AGORG = "" THEN .
   ELSE IF FILL-IN-PROJEKT = "" THEN.
   ELSE DO:   
      EMPTY TEMP-TABLE inextradatatemp NO-ERROR.
      CREATE inextradatatemp.          
      ASSIGN
      inextradatatemp.PROGRAM = "NATPROJ"                   
      inextradatatemp.HUVUDCH = FILL-IN-PROJEKT
      inextradatatemp.SOKCHAR[1] = FILL-IN-LEDLITT 
      /*inextradatatemp.SOKCHAR[2] = FILL-IN-PNAMN 
      inextradatatemp.SOKCHAR[3] = FILL-IN-PLED  */
      inextradatatemp.SOKCHAR[6] = FILL-IN-LEDAGARE
      inextradatatemp.SOKCHAR[7] = FILL-IN-AGFAST
      inextradatatemp.SOKCHAR[8] = FILL-IN-AGKOMMUN
      inextradatatemp.SOKCHAR[9] = FILL-IN-AGLAN
      inextradatatemp.SOKCHAR[10] = FILL-IN-AGORG.      
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


&Scoped-define SELF-NAME FILL-IN-AGFAST
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-AGFAST Dialog-Frame
ON LEAVE OF FILL-IN-AGFAST IN FRAME Dialog-Frame /* Ägande fastighet */
DO:
  ASSIGN
   FILL-IN-AGFAST = INPUT FILL-IN-AGFAST.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-AGKOMMUN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-AGKOMMUN Dialog-Frame
ON LEAVE OF FILL-IN-AGKOMMUN IN FRAME Dialog-Frame /* Kommun */
DO:
  ASSIGN
   FILL-IN-AGKOMMUN = INPUT FILL-IN-AGKOMMUN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-AGLAN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-AGLAN Dialog-Frame
ON LEAVE OF FILL-IN-AGLAN IN FRAME Dialog-Frame /* Län */
DO:
   ASSIGN
   FILL-IN-AGLAN = INPUT FILL-IN-AGLAN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-AGORG
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-AGORG Dialog-Frame
ON LEAVE OF FILL-IN-AGORG IN FRAME Dialog-Frame /* Ledningsägare orgnr */
DO:
   ASSIGN
   FILL-IN-AGORG = INPUT FILL-IN-AGORG.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-LEDAGARE
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-LEDAGARE Dialog-Frame
ON LEAVE OF FILL-IN-LEDAGARE IN FRAME Dialog-Frame /* Ledningsägare */
DO:
   ASSIGN
   FILL-IN-LEDAGARE = INPUT FILL-IN-LEDAGARE.
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


&Scoped-define SELF-NAME FILL-IN-PROJEKT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-PROJEKT Dialog-Frame
ON LEAVE OF FILL-IN-PROJEKT IN FRAME Dialog-Frame /* Nätägares projektnummer */
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
      /*FILL-IN-PNAMN = extradatatemp.SOKCHAR[2]
      FILL-IN-PLED  = extradatatemp.SOKCHAR[3]*/
      FILL-IN-LEDAGARE = extradatatemp.SOKCHAR[6] 
      FILL-IN-AGFAST = extradatatemp.SOKCHAR[7] 
      FILL-IN-AGKOMMUN = extradatatemp.SOKCHAR[8]
      FILL-IN-AGLAN = extradatatemp.SOKCHAR[9]
      FILL-IN-AGORG = extradatatemp.SOKCHAR[10].
      
   END.   
   DISPLAY FILL-IN-LEDAGARE FILL-IN-AGFAST FILL-IN-AGKOMMUN FILL-IN-AGLAN FILL-IN-AGORG FILL-IN-LEDLITT /*FILL-IN-PNAMN FILL-IN-PLED*/ WITH  FRAME {&FRAME-NAME}.
   IF vama = "vard" AND ( mall = 3 OR mall = 4) THEN DO:
      ASSIGN
      FILL-IN-LEDAGARE:HIDDEN  IN FRAME {&FRAME-NAME} = FALSE 
      FILL-IN-AGFAST:HIDDEN = FALSE.
      FILL-IN-AGKOMMUN:HIDDEN = FALSE.
      FILL-IN-AGLAN:HIDDEN = FALSE.
      FILL-IN-AGORG:HIDDEN = FALSE.
      FILL-IN-LEDLITT:HIDDEN = TRUE.
      /*FILL-IN-PNAMN:HIDDEN = TRUE.
      FILL-IN-PLED:HIDDEN = TRUE.*/
   END.
   ELSE DO:
      ASSIGN
      FILL-IN-LEDLITT:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
      /*FILL-IN-PNAMN:HIDDEN = FALSE.
      FILL-IN-PLED:HIDDEN = FALSE.*/
      FILL-IN-LEDAGARE:HIDDEN  = TRUE. 
      FILL-IN-AGFAST:HIDDEN = TRUE.
      FILL-IN-AGKOMMUN:HIDDEN = TRUE.
      FILL-IN-AGLAN:HIDDEN = TRUE.
      FILL-IN-AGORG:HIDDEN = TRUE.
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
            /*FILL-IN-PNAMN = extradatatemp.SOKCHAR[2] 
            FILL-IN-PLED = extradatatemp.SOKCHAR[3]*/
            FILL-IN-LEDAGARE = extradatatemp.SOKCHAR[6] 
            FILL-IN-AGFAST = extradatatemp.SOKCHAR[7] 
            FILL-IN-AGKOMMUN = extradatatemp.SOKCHAR[8] 
            FILL-IN-AGLAN = extradatatemp.SOKCHAR[9]
            FILL-IN-AGORG = extradatatemp.SOKCHAR[10].            
         END.   
      END.
   END.
   
   {musarrow.i}
   RUN enable_UI.       
   IF vama = "vard" AND ( mall = 3 OR mall = 4) THEN DO:
      ASSIGN
      FILL-IN-LEDAGARE:HIDDEN  IN FRAME {&FRAME-NAME} = FALSE 
      FILL-IN-AGFAST:HIDDEN = FALSE.
      FILL-IN-AGKOMMUN:HIDDEN = FALSE.
      FILL-IN-AGLAN:HIDDEN = FALSE.
      FILL-IN-AGORG:HIDDEN = FALSE.
      FILL-IN-LEDLITT:HIDDEN = TRUE.
      /*FILL-IN-PNAMN:HIDDEN = TRUE.
      FILL-IN-PLED:HIDDEN = TRUE.*/
   END.
   ELSE DO:
      ASSIGN
      FILL-IN-LEDLITT:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
      /*FILL-IN-PNAMN:HIDDEN = FALSE.
      FILL-IN-PLED:HIDDEN = FALSE.*/
      FILL-IN-LEDAGARE:HIDDEN  = TRUE. 
      FILL-IN-AGFAST:HIDDEN = TRUE.
      FILL-IN-AGKOMMUN:HIDDEN = TRUE.
      FILL-IN-AGLAN:HIDDEN = TRUE.
      FILL-IN-AGORG:HIDDEN = TRUE.
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
  DISPLAY FILL-IN-PROJEKT FILL-IN-LEDLITT FILL-IN-LEDAGARE FILL-IN-AGORG 
          FILL-IN-AGFAST FILL-IN-AGKOMMUN FILL-IN-AGLAN 
      WITH FRAME Dialog-Frame.
  ENABLE FILL-IN-PROJEKT FILL-IN-LEDLITT FILL-IN-LEDAGARE FILL-IN-AGORG 
         FILL-IN-AGFAST FILL-IN-AGKOMMUN FILL-IN-AGLAN BTN_OK BTN_AVS 
      WITH FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

