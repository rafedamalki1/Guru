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
{ALLDEF.I}
&Scoped-define NEW 
{GLOBVAR2DEL1.I}
&Scoped-define SHARED SHARED
{ANNAMN.I}
{BERANN.I} 
{AVDTEMP.I}
{AVDELNINGTEMP.I}
{OMRTEMPW.I}

DEFINE SHARED VARIABLE valford AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE vallinje AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE valnat AS CHARACTER NO-UNDO. 
DEFINE SHARED VARIABLE siff AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.   
DEFINE SHARED VARIABLE andrakod AS CHARACTER NO-UNDO. 
DEFINE SHARED VARIABLE id AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE nyttidnr AS CHARACTER.
DEFINE VARIABLE sparnr AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE fordrowid AS ROWID NO-UNDO.      
DEFINE SHARED VARIABLE linjerowid AS ROWID NO-UNDO.
DEFINE SHARED VARIABLE natrowid AS ROWID NO-UNDO.
DEFINE SHARED VARIABLE andra AS LOGICAL NO-UNDO.
DEFINE VARIABLE nyannapph AS HANDLE NO-UNDO.
DEFINE VARIABLE felmedd AS CHARACTER NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE valomr AS CHARACTER NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS CMB_JURP CMB_AVD CMB_OMR FILL-IN-NR ~
FILL-IN-NAMN BTN_OK BTN_AVB FILL-IN-TEXT 
&Scoped-Define DISPLAYED-OBJECTS CMB_JURP CMB_AVD CMB_OMR FILL-IN-NR ~
FILL-IN-NAMN FILL-IN-TEXT 

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

DEFINE BUTTON BTN_OK 
     LABEL "Ok" 
     SIZE 14 BY 1.

DEFINE VARIABLE CMB_AVD AS CHARACTER FORMAT "X(256)":U INITIAL ? 
     LABEL "Avdelning" 
     VIEW-AS COMBO-BOX INNER-LINES 15
     DROP-DOWN-LIST
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_JURP AS CHARACTER FORMAT "X(256)":U INITIAL ? 
     LABEL "Juridisp" 
     VIEW-AS COMBO-BOX INNER-LINES 15
     DROP-DOWN-LIST
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_OMR AS CHARACTER FORMAT "X(256)":U INITIAL ? 
     LABEL "Område" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-NAMN AS CHARACTER FORMAT "X(30)":U 
     LABEL "Namn" 
     VIEW-AS FILL-IN 
     SIZE 31 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-NR AS CHARACTER FORMAT "X(10)":U 
     LABEL "Nr" 
     VIEW-AS FILL-IN 
     SIZE 11.63 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-TEXT AS CHARACTER FORMAT "X(256)":U INITIAL "Välj område:" 
      VIEW-AS TEXT 
     SIZE 14 BY .63 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     CMB_JURP AT ROW 2.42 COL 2.5
     CMB_AVD AT ROW 3.71 COL 1.5
     CMB_OMR AT ROW 5 COL 10.5 COLON-ALIGNED
     FILL-IN-NR AT ROW 6.29 COL 10.5 COLON-ALIGNED
     FILL-IN-NAMN AT ROW 7.67 COL 10.5 COLON-ALIGNED
     BTN_OK AT ROW 9.25 COL 14.5
     BTN_AVB AT ROW 9.25 COL 29.5
     FILL-IN-TEXT AT ROW 1.42 COL 12.75 COLON-ALIGNED NO-LABEL
     SPACE(16.12) SKIP(8.65)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Anläggningar".


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
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR COMBO-BOX CMB_AVD IN FRAME Dialog-Frame
   ALIGN-L                                                              */
/* SETTINGS FOR COMBO-BOX CMB_JURP IN FRAME Dialog-Frame
   ALIGN-L                                                              */
ASSIGN 
       FILL-IN-TEXT:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Anläggningar */
DO:
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
   FILL-IN-NR = INPUT FILL-IN-NR
   FILL-IN-NAMN = INPUT FILL-IN-NAMN.
   IF FILL-IN-NR = "" THEN DO:
      MESSAGE FILL-IN-NR:LABEL " får inte vara blank!" VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
   IF siff = 1 THEN DO:
      CMB_OMR = INPUT CMB_OMR.            
      valomr = "ALLA".
      IF CMB_OMR = "Alla" THEN DO:
         MESSAGE "Du har inte valt något " LC(Guru.Konstanter:gomrk) ". Är det OK?"
         VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE val AS LOGICAL.
         IF val = FALSE THEN DO:
            RETURN NO-APPLY.  
         END.
      END.
      ELSE DO:
         FIND FIRST omrtemp WHERE omrtemp.NAMN = CMB_OMR NO-LOCK NO-ERROR.
         IF AVAILABLE omrtemp THEN DO:
            valomr = omrtemp.OMRADE.
         END.
      END.
   END.
   felmedd = "".
   FIND FIRST annamntemp NO-LOCK NO-ERROR.
   RUN btnok_UI IN nyannapph (INPUT andra,INPUT siff,INPUT FILL-IN-NR,INPUT FILL-IN-NAMN,
                              INPUT valford,INPUT vallinje,INPUT sparnr,INPUT andrakod,INPUT valomr,
                              OUTPUT felmedd).
   IF felmedd NE "" THEN DO:
      MESSAGE felmedd VIEW-AS ALERT-BOX TITLE "Meddelande".
      felmedd = "".
      APPLY "ENTRY" TO FILL-IN-NR IN FRAME {&FRAME-NAME}.
   END.
   ELSE DO:
      andrakod = FILL-IN-NR.
      APPLY "GO" TO FRAME {&FRAME-NAME}.          
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK Dialog-Frame
ON GO OF BTN_OK IN FRAME Dialog-Frame /* Ok */
DO:
  musz = FALSE.
   IF VALID-HANDLE(nyannapph) THEN DELETE PROCEDURE nyannapph.
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_AVD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_AVD Dialog-Frame
ON VALUE-CHANGED OF CMB_AVD IN FRAME Dialog-Frame /* Avdelning */
DO:
     
   CMB_AVD = INPUT CMB_AVD.         
   {CMB_AVD1.I}   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_JURP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_JURP Dialog-Frame
ON VALUE-CHANGED OF CMB_JURP IN FRAME Dialog-Frame /* Juridisp */
DO:
     
   CMB_JURP = INPUT CMB_JURP.         
   {CMB_JURP.I}   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_OMR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_OMR Dialog-Frame
ON VALUE-CHANGED OF CMB_OMR IN FRAME Dialog-Frame /* Område */
DO:
   CMB_OMR = INPUT CMB_OMR.            
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
   {muswait.i}
   {ALLSTARTDYN.I}
   FIND FIRST annamntemp NO-LOCK NO-ERROR.
   IF andra = FALSE THEN DO:
      IF id = FALSE THEN DO:
         IF siff = 1 THEN 
         {&WINDOW-NAME}:TITLE = "Ny " + annamntemp.TAB1.
         ELSE IF siff = 2 THEN 
         {&WINDOW-NAME}:TITLE = "Ny " + annamntemp.TAB2.
         ELSE
         {&WINDOW-NAME}:TITLE = "Ny " + annamntemp.TAB3.
      END.
      ELSE DO: 
         IF siff = 1 THEN 
         {&WINDOW-NAME}:TITLE = "Ny " + annamntemp.TAB1.
         ELSE IF siff = 2 THEN 
         {&WINDOW-NAME}:TITLE = "Ny " + annamntemp.TAB2.
         ELSE
         {&WINDOW-NAME}:TITLE = "Ny " + annamntemp.TAB3.
         ASSIGN
         FILL-IN-NR = nyttidnr.
      END.     
   END.
   ELSE DO:       
      IF siff = 1 THEN DO:
         {&WINDOW-NAME}:TITLE = "Ändring av " + annamntemp.TAB1. 
         FIND FIRST berfortemp WHERE berfortemp.FORNR = andrakod NO-LOCK NO-ERROR.
         ASSIGN
         FILL-IN-NR = berfortemp.FORNR
         FILL-IN-NAMN = berfortemp.NAMN.
      END.   
      ELSE IF siff = 2 THEN DO:
         {&WINDOW-NAME}:TITLE = "Ändring av " + annamntemp.TAB2. 
         FIND FIRST berlinjetemp WHERE berlinjetemp.FORNR = valford AND
         berlinjetemp.LINNR = andrakod NO-LOCK NO-ERROR.
         ASSIGN
         FILL-IN-NR = berlinjetemp.LINNR
         FILL-IN-NAMN = berlinjetemp.NAMN.
      END.   
      ELSE DO:
         {&WINDOW-NAME}:TITLE = "Ändring av " + annamntemp.TAB3.
         FIND FIRST bernattemp WHERE bernattemp.FORNR = valford AND
         bernattemp.LINNR = vallinje AND bernattemp.NATNR = andrakod NO-LOCK NO-ERROR.
         ASSIGN
         FILL-IN-NR = bernattemp.NATNR
         FILL-IN-NAMN = bernattemp.NAMN.
      END.               
      ASSIGN
      sparnr = FILL-IN-NR.
   END. 
   RUN enable_UI.   
   IF siff = 1 THEN DO:
      status-ok = CMB_JURP:ADD-FIRST("Alla").
      FOR EACH jurperstemp:
         status-ok = CMB_JURP:ADD-LAST(jurperstemp.NAMN).
      END.
      CMB_JURP:SCREEN-VALUE = "Alla".  
      status-ok = CMB_AVD:ADD-FIRST("Alla").
      FOR EACH avdtemp:
         status-ok = CMB_AVD:ADD-LAST(avdtemp.AVDELNINGNAMN).
      END.
      CMB_AVD:SCREEN-VALUE = "Alla".  
      ASSIGN 
      status-ok = CMB_OMR:ADD-FIRST("Alla").   
      CMB_OMR:SCREEN-VALUE = "Alla".
      FOR EACH omrtemp:
         status-ok = CMB_OMR:ADD-LAST(omrtemp.NAMN).
      END.
      IF andra = FALSE THEN DO:
         FIND FIRST omrtemp WHERE omrtemp.OMRADE = Guru.Konstanter:globomr 
         USE-INDEX OMR NO-LOCK NO-ERROR.
      END.
      ELSE DO:
         FIND FIRST omrtemp WHERE omrtemp.OMRADE = berfortemp.OMRADE 
         USE-INDEX OMR NO-LOCK NO-ERROR.
      END.
      IF AVAILABLE omrtemp THEN DO:
         ASSIGN CMB_OMR:SCREEN-VALUE = omrtemp.NAMN.       
      END.
      ASSIGN
      FILL-IN-TEXT = "Välj " + LC(Guru.Konstanter:gomrk) + ":"
      CMB_JURP:LABEL = Guru.Konstanter:gjuk
      CMB_AVD:LABEL= Guru.Konstanter:gavdk
      CMB_OMR:LABEL = Guru.Konstanter:gomrk.
      IF Guru.Konstanter:globforetag = "sund" OR Guru.Konstanter:globforetag = "SNAT"  THEN DO:
         CMB_JURP:HIDDEN = TRUE.   
      END.
   END.
   ELSE DO:
      ASSIGN
      CMB_JURP:HIDDEN = TRUE
      CMB_OMR:HIDDEN = TRUE
      CMB_AVD:HIDDEN = TRUE
      FILL-IN-TEXT:HIDDEN = TRUE.
   END.
   {FRMSIZED.I}        
   {musarrow.i}
   {DIA_M_SLUT.I}
   
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE allstartbrw_UI Dialog-Frame 
PROCEDURE allstartbrw_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   IF Guru.Konstanter:appcon THEN DO:
      RUN NYANNAPPJ.P PERSISTENT SET nyannapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN NYANNAPPJ.P PERSISTENT SET nyannapph.
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
  DISPLAY CMB_JURP CMB_AVD CMB_OMR FILL-IN-NR FILL-IN-NAMN FILL-IN-TEXT 
      WITH FRAME Dialog-Frame.
  ENABLE CMB_JURP CMB_AVD CMB_OMR FILL-IN-NR FILL-IN-NAMN BTN_OK BTN_AVB 
         FILL-IN-TEXT 
      WITH FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

