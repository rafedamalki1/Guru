&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
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

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEFINE INPUT PARAMETER andra AS LOGICAL NO-UNDO.

/* Local Variable Definitions ---                                       */
{ALLDEF.I}


DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE valaonr AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE valomrade AS CHARACTER NO-UNDO.
DEFINE VARIABLE eget AS LOGICAL NO-UNDO.
DEFINE VARIABLE eavar AS DECIMAL NO-UNDO.
DEFINE VARIABLE arbvar AS DECIMAL NO-UNDO.
DEFINE VARIABLE mskvar AS DECIMAL NO-UNDO.
DEFINE VARIABLE kataar AS INTEGER NO-UNDO.
DEFINE VARIABLE extrkodapph AS HANDLE NO-UNDO.
DEFINE VARIABLE extrbefbapph AS HANDLE NO-UNDO.
DEFINE VARIABLE emask3 AS DECIMAL NO-UNDO.
DEFINE VARIABLE berpris AS DECIMAL NO-UNDO.
DEFINE VARIABLE KalkClasserStart AS HANDLE NO-UNDO.
{KALKBEFBTEMP.I}  
&SCOPED-DEFINE NEW

{GLOBVAR2DEL1.I}
/*{EGENBEN.I}*/
&SCOPED-DEFINE SHARED SHARED

/*{KALKALLTEMP.I}
{FASTKALKTEMP.I}*/
{EBRPRISTEMP.I}
{KALKTEMP2.I}
/*DEFINE SHARED TEMP-TABLE extravaldfasttemp NO-UNDO LIKE valdfasttemp.*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 RECT-2 RECT-4 FILL-IN-ENHET ~
FILL-IN-BEN FILL-IN-MATERIEL FILL-IN-OVRIGT BTN_OK BTN_AVB FILL-IN-LOPNR ~
FILL-IN-L1 FILL-IN-L2 FILL-IN-L3 FILL-IN-L4 FILL-IN-L5 FILL-IN-L6 ~
FILL-IN-L7 FILL-IN-L8 FILL-IN-L9 FILL-IN-L10 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-ENHET FILL-IN-BEN FILL-IN-MATERIEL ~
FILL-IN-OVRIGT FILL-IN-ENTRP FILL-IN-LOPNR FILL-IN-L1 FILL-IN-L2 FILL-IN-L3 ~
FILL-IN-L4 FILL-IN-L5 FILL-IN-L6 FILL-IN-L7 FILL-IN-L8 FILL-IN-L9 ~
FILL-IN-L10 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB 
     LABEL "Avbryt" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_OK 
     LABEL "Ok" 
     SIZE 14 BY 1.

DEFINE VARIABLE FILL-IN-ARBETE AS DECIMAL FORMAT "->>>>>>9.99":U INITIAL 0 
     LABEL "Arbete" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-BEN AS CHARACTER FORMAT "X(40)":U 
     LABEL "Benämning" 
     VIEW-AS FILL-IN 
     SIZE 46.13 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-EA AS DECIMAL FORMAT ">>>9.99":U INITIAL 0 
     LABEL "EA" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-ENHET AS CHARACTER FORMAT "X(3)":U 
     LABEL "Enhet" 
     VIEW-AS FILL-IN 
     SIZE 4.75 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-ENTRP AS DECIMAL FORMAT "->>>>>>9.99":U INITIAL 0 
     LABEL "Entreprenör" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-F1 AS DECIMAL FORMAT ">>>9.99":U INITIAL 0 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-F10 AS DECIMAL FORMAT ">>>9.99":U INITIAL 0 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-F2 AS DECIMAL FORMAT ">>>9.99":U INITIAL 0 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-F3 AS DECIMAL FORMAT ">>>9.99":U INITIAL 0 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-F4 AS DECIMAL FORMAT ">>>9.99":U INITIAL 0 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-F5 AS DECIMAL FORMAT ">>>9.99":U INITIAL 0 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-F6 AS DECIMAL FORMAT ">>>9.99":U INITIAL 0 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-F7 AS DECIMAL FORMAT ">>>9.99":U INITIAL 0 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-F8 AS DECIMAL FORMAT ">>>9.99":U INITIAL 0 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-F9 AS DECIMAL FORMAT ">>>9.99":U INITIAL 0 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-L1 AS CHARACTER FORMAT "X(20)":U 
      VIEW-AS TEXT 
     SIZE 21 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-L10 AS CHARACTER FORMAT "X(20)":U 
      VIEW-AS TEXT 
     SIZE 21 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-L2 AS CHARACTER FORMAT "X(20)":U 
      VIEW-AS TEXT 
     SIZE 21 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-L3 AS CHARACTER FORMAT "X(20)":U 
      VIEW-AS TEXT 
     SIZE 21 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-L4 AS CHARACTER FORMAT "X(20)":U 
      VIEW-AS TEXT 
     SIZE 21 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-L5 AS CHARACTER FORMAT "X(20)":U 
      VIEW-AS TEXT 
     SIZE 21 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-L6 AS CHARACTER FORMAT "X(20)":U 
      VIEW-AS TEXT 
     SIZE 21 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-L7 AS CHARACTER FORMAT "X(20)":U 
      VIEW-AS TEXT 
     SIZE 21 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-L8 AS CHARACTER FORMAT "X(20)":U 
      VIEW-AS TEXT 
     SIZE 21 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-L9 AS CHARACTER FORMAT "X(20)":U 
      VIEW-AS TEXT 
     SIZE 21 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-LOPNR AS INTEGER FORMAT ">>>":U INITIAL 0 
     LABEL "Löpnr" 
      VIEW-AS TEXT 
     SIZE 4.5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-MASKINKOST AS DECIMAL FORMAT "->>>>>>9.99":U INITIAL 0 
     LABEL "Maskin" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-MATERIEL AS DECIMAL FORMAT "->>>>>>9.99":U INITIAL 0 
     LABEL "Materiel" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-OVRIGT AS DECIMAL FORMAT "->>>>>>9.99":U INITIAL 0 
     LABEL "Övrigt" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-P1 AS DECIMAL FORMAT ">>>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-P10 AS DECIMAL FORMAT ">>>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-P2 AS DECIMAL FORMAT ">>>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-P3 AS DECIMAL FORMAT ">>>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-P4 AS DECIMAL FORMAT ">>>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-P5 AS DECIMAL FORMAT ">>>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-P6 AS DECIMAL FORMAT ">>>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-P7 AS DECIMAL FORMAT ">>>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-P8 AS DECIMAL FORMAT ">>>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-P9 AS DECIMAL FORMAT ">>>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-SUMMA AS DECIMAL FORMAT "->>>>>>9.99":U INITIAL 0 
     LABEL "Summa" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 96.63 BY 11.08
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 96.63 BY 6.38
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 96.63 BY 4.17
     BGCOLOR 8 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     FILL-IN-ENHET AT ROW 2.33 COL 32.13 COLON-ALIGNED
     FILL-IN-BEN AT ROW 3.67 COL 15 COLON-ALIGNED
     FILL-IN-F1 AT ROW 7.04 COL 25 COLON-ALIGNED
     FILL-IN-P1 AT ROW 7.04 COL 36 COLON-ALIGNED NO-LABEL
     FILL-IN-F2 AT ROW 7.04 COL 73.25 COLON-ALIGNED
     FILL-IN-P2 AT ROW 7.04 COL 84.13 COLON-ALIGNED NO-LABEL
     FILL-IN-F3 AT ROW 8.54 COL 25 COLON-ALIGNED
     FILL-IN-P3 AT ROW 8.54 COL 36 COLON-ALIGNED NO-LABEL
     FILL-IN-F4 AT ROW 8.54 COL 73.25 COLON-ALIGNED
     FILL-IN-P4 AT ROW 8.54 COL 84.13 COLON-ALIGNED NO-LABEL
     FILL-IN-F5 AT ROW 10 COL 25 COLON-ALIGNED
     FILL-IN-P5 AT ROW 10 COL 36 COLON-ALIGNED NO-LABEL
     FILL-IN-F6 AT ROW 10 COL 73.25 COLON-ALIGNED
     FILL-IN-P6 AT ROW 10 COL 84.13 COLON-ALIGNED NO-LABEL
     FILL-IN-F7 AT ROW 11.54 COL 25 COLON-ALIGNED
     FILL-IN-P7 AT ROW 11.54 COL 36 COLON-ALIGNED NO-LABEL
     FILL-IN-F8 AT ROW 11.54 COL 73.25 COLON-ALIGNED
     FILL-IN-P8 AT ROW 11.54 COL 84.13 COLON-ALIGNED NO-LABEL
     FILL-IN-F9 AT ROW 13.04 COL 25 COLON-ALIGNED
     FILL-IN-P9 AT ROW 13.04 COL 36 COLON-ALIGNED NO-LABEL
     FILL-IN-F10 AT ROW 13.04 COL 73.25 COLON-ALIGNED
     FILL-IN-P10 AT ROW 13.04 COL 84.13 COLON-ALIGNED NO-LABEL
     FILL-IN-EA AT ROW 14.54 COL 25 COLON-ALIGNED
     FILL-IN-ARBETE AT ROW 17.75 COL 11.88 COLON-ALIGNED
     FILL-IN-MATERIEL AT ROW 17.75 COL 50.13 COLON-ALIGNED
     FILL-IN-MASKINKOST AT ROW 19.25 COL 11.88 COLON-ALIGNED
     FILL-IN-OVRIGT AT ROW 19.25 COL 50.13 COLON-ALIGNED
     FILL-IN-SUMMA AT ROW 19.25 COL 79.75 COLON-ALIGNED
     FILL-IN-ENTRP AT ROW 20.75 COL 50.13 COLON-ALIGNED
     BTN_OK AT ROW 23.33 COL 68.63
     BTN_AVB AT ROW 23.33 COL 83.63
     FILL-IN-LOPNR AT ROW 2.33 COL 15.13 COLON-ALIGNED
     FILL-IN-L1 AT ROW 7.04 COL 2.13 COLON-ALIGNED NO-LABEL
     FILL-IN-L2 AT ROW 7.04 COL 50.38 COLON-ALIGNED NO-LABEL
     FILL-IN-L3 AT ROW 8.54 COL 2.13 COLON-ALIGNED NO-LABEL
     FILL-IN-L4 AT ROW 8.54 COL 50.38 COLON-ALIGNED NO-LABEL
     FILL-IN-L5 AT ROW 10 COL 2.13 COLON-ALIGNED NO-LABEL
     FILL-IN-L6 AT ROW 10 COL 50.38 COLON-ALIGNED NO-LABEL
     FILL-IN-L7 AT ROW 11.54 COL 2.13 COLON-ALIGNED NO-LABEL
     FILL-IN-L8 AT ROW 11.54 COL 50.38 COLON-ALIGNED NO-LABEL
     FILL-IN-L9 AT ROW 13.04 COL 2 COLON-ALIGNED NO-LABEL
     FILL-IN-L10 AT ROW 13.04 COL 50.38 COLON-ALIGNED NO-LABEL
     "Timmar     Pris" VIEW-AS TEXT
          SIZE 21 BY .83 AT ROW 6.17 COL 75.38
     "Kostnad:" VIEW-AS TEXT
          SIZE 10.5 BY .92 AT ROW 16.63 COL 1.5
          FONT 17
     "Timmar / pris:" VIEW-AS TEXT
          SIZE 16.38 BY 1.13 AT ROW 5.63 COL 1.5
          FONT 17
     "Information:" VIEW-AS TEXT
          SIZE 14.13 BY .96 AT ROW 1.17 COL 1.5
          FONT 17
     "Timmar     Pris" VIEW-AS TEXT
          SIZE 21 BY .83 AT ROW 6.17 COL 27
     RECT-1 AT ROW 5.17 COL 1
     RECT-2 AT ROW 16.25 COL 1
     RECT-4 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 97.38 BY 23.63.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "<insert window title>"
         HEIGHT             = 23.75
         WIDTH              = 97.38
         MAX-HEIGHT         = 24.25
         MAX-WIDTH          = 97.38
         VIRTUAL-HEIGHT     = 24.25
         VIRTUAL-WIDTH      = 97.38
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME DEFAULT-FRAME
                                                                        */
/* SETTINGS FOR FILL-IN FILL-IN-ARBETE IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE                                                 */
/* SETTINGS FOR FILL-IN FILL-IN-EA IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE                                                 */
/* SETTINGS FOR FILL-IN FILL-IN-ENTRP IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       FILL-IN-ENTRP:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-F1 IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN-F1:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-F10 IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN-F10:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-F2 IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN-F2:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-F3 IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN-F3:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-F4 IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN-F4:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-F5 IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN-F5:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-F6 IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN-F6:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-F7 IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN-F7:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-F8 IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN-F8:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-F9 IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN-F9:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

ASSIGN 
       FILL-IN-L1:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

ASSIGN 
       FILL-IN-L10:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

ASSIGN 
       FILL-IN-L2:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

ASSIGN 
       FILL-IN-L3:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

ASSIGN 
       FILL-IN-L4:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

ASSIGN 
       FILL-IN-L5:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

ASSIGN 
       FILL-IN-L6:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

ASSIGN 
       FILL-IN-L7:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

ASSIGN 
       FILL-IN-L8:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

ASSIGN 
       FILL-IN-L9:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-MASKINKOST IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE                                                 */
/* SETTINGS FOR FILL-IN FILL-IN-P1 IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN-P1:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-P10 IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN-P10:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-P2 IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN-P2:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-P3 IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN-P3:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-P4 IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN-P4:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-P5 IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN-P5:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-P6 IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN-P6:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-P7 IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN-P7:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-P8 IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN-P8:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-P9 IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN-P9:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-SUMMA IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE                                                 */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* <insert window title> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
   musz = TRUE.
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* <insert window title> */
DO:
  /* This event will close the window and terminate the procedure.  */
  musz = TRUE.
  IF VALID-HANDLE(KalkClasserStart) THEN DELETE PROCEDURE KalkClasserStart NO-ERROR.
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB C-Win
ON CHOOSE OF BTN_AVB IN FRAME DEFAULT-FRAME /* Avbryt */
DO:
   musz = TRUE.
   APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK C-Win
ON CHOOSE OF BTN_OK IN FRAME DEFAULT-FRAME /* Ok */
DO:
   RUN btnok_UI.
   musz = FALSE.
   APPLY "CLOSE":U TO THIS-PROCEDURE.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
DO:
   {BORTBRWPROC.I}
   IF VALID-HANDLE(extrkodapph) THEN DELETE PROCEDURE extrkodapph.
   IF VALID-HANDLE(extrbefbapph) THEN DELETE PROCEDURE extrbefbapph.
   RUN disable_UI.
END.
   

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK: 
      
   {WIN_M_START.I}
   {muswait.i}
   {ALLSTARTDYN.I}
   FILL-IN-LOPNR:FORMAT =  Guru.Konstanter:varforetypchar[6].
   /*FIND FIRST extrafastkalktemp NO-LOCK NO-ERROR.
   FIND FIRST extravaldfasttemp NO-LOCK NO-ERROR.*/
   FIND FIRST ekalk_temp  NO-LOCK NO-ERROR.
   EMPTY TEMP-TABLE ebrpristemp NO-ERROR. 
   RUN hmtebr_UI IN KalkClasserStart (INPUT valaonr,INPUT valomrade,OUTPUT TABLE ebrpristemp,OUTPUT TABLE kalkbefbtemp).
   FIND FIRST ebrpristemp WHERE NO-LOCK NO-ERROR.
   emask3 = ebrpristemp.MASK3.
   kataar = ebrpristemp.ARTAL.
   
  
  
   IF andra = FALSE THEN DO:      
      /*IF extravaldfasttemp.EGETPRIS = TRUE THEN DO:
         FIND FIRST kalkbefbtemp WHERE kalkbefbtemp.KALKNR = extravaldfasttemp.KALKNR AND 
            kalkbefbtemp.ARBKOD = "" AND kalkbefbtemp.LOPNR = 0 USE-INDEX OMRADE NO-LOCK NO-ERROR.
         eget = TRUE.
      END.   
      ELSE DO:*/
         FIND FIRST kalkbefbtemp WHERE kalkbefbtemp.BERNR = 0 AND 
         kalkbefbtemp.KATAR = kataar USE-INDEX ARTAL NO-LOCK NO-ERROR.        
         eget = FALSE.
      /*END.      */
      {&WINDOW-NAME}:TITLE = "Upplägg av EGEN arbetskod och löpnr.".         
      FILL-IN-LOPNR = ekalk_temp.LOPNR.                
   END.
   ELSE DO:      
      
      FIND FIRST kalkbefbtemp WHERE kalkbefbtemp.OMRADE = valomrade AND
      kalkbefbtemp.BERNR = INTEGER(valaonr) AND kalkbefbtemp.ARBKOD = ekalk_temp.ARBKOD AND
      kalkbefbtemp.LOPNR = ekalk_temp.LOPNR USE-INDEX OMRADE EXCLUSIVE-LOCK NO-ERROR.
      {&WINDOW-NAME}:TITLE = "Ändring av EGEN arbetskod och löpnr.".      
      eget = TRUE.      
           
   END.   
   RUN enable_UI.   
   {FRMSIZE.I}   
   ENABLE FILL-IN-ENTRP WITH FRAME {&FRAME-NAME}.
   IF Guru.Konstanter:globforetag = "GRAN" OR Guru.Konstanter:globforetag = "NAEK"  OR Guru.Konstanter:globforetag = "CPOMA" THEN FILL-IN-ENTRP:HIDDEN = TRUE.  
   RUN gomma_UI.
   IF andra = TRUE THEN DO:
      RUN andra_UI.
      RUN rakna_UI.
   END.   
   ON "LEAVE" OF FILL-IN-F1, FILL-IN-F2, FILL-IN-F3, FILL-IN-F4, FILL-IN-F5, FILL-IN-F6, FILL-IN-F7,
      FILL-IN-F8, FILL-IN-F9, FILL-IN-F10, FILL-IN-P1, FILL-IN-P2, FILL-IN-P3, FILL-IN-P4, FILL-IN-P5,
      FILL-IN-P6, FILL-IN-P7, FILL-IN-P8, FILL-IN-P9, FILL-IN-P10, FILL-IN-MATERIEL, FILL-IN-OVRIGT, FILL-IN-ENTRP 
      PERSISTENT RUN rakna_UI IN THIS-PROCEDURE. 
   /*KALKmatris*/
   ASSIGN
   FILL-IN-F10:HIDDEN = TRUE 
   FILL-IN-P10:HIDDEN = TRUE. 
   {musarrow.i}
   {WIN_M_SLUT.I}
   IF NOT THIS-PROCEDURE:PERSISTENT THEN
   WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE allstartbrw_UI C-Win 
PROCEDURE allstartbrw_UI :
/* -----------------------------------------------------------
  Purpose:    
  Parameters: 
  Notes:       
-------------------------------------------------------------*/ 
   IF NOT VALID-HANDLE(KalkClasserStart) THEN RUN Modules\Kalkyl\KalkClasserStart.P PERSISTENT SET KalkClasserStart.
   IF Guru.Konstanter:appcon THEN DO:
      RUN EXTRKODAPP.P PERSISTENT SET extrkodapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN EXTRKODAPP.P PERSISTENT SET extrkodapph.
   END. 
   IF Guru.Konstanter:appcon THEN DO:
      RUN EXTRKODBAPP.P PERSISTENT SET extrbefbapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN EXTRKODBAPP.P PERSISTENT SET extrbefbapph.
   END. 
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE andra_UI C-Win 
PROCEDURE andra_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/ 
   ASSIGN    
   FILL-IN-LOPNR = ekalk_temp.LOPNR
   FILL-IN-BEN = ekalk_temp.BENAMNING
   FILL-IN-ENHET = ekalk_temp.ENHET  
   FILL-IN-F1 = kalkbefbtemp.F1
   FILL-IN-F2 = kalkbefbtemp.F2 
   FILL-IN-F3 = kalkbefbtemp.F3
   FILL-IN-F4 = kalkbefbtemp.F4 
   FILL-IN-F5 = kalkbefbtemp.F5 
   FILL-IN-F6 = kalkbefbtemp.F6 
   FILL-IN-F7 = kalkbefbtemp.F7 
   FILL-IN-F8 = kalkbefbtemp.F8 
   FILL-IN-F9 = kalkbefbtemp.F9 
      /*KALKmatris
   FILL-IN-F10 = kalkbefbtemp.F10              
   */
   FILL-IN-MATERIEL = kalkbefbtemp.MATERIEL    
   FILL-IN-OVRIGT = kalkbefbtemp.OVRIGT  
   FILL-IN-ENTRP = kalkbefbtemp.ENTRP.
   DISPLAY FILL-IN-ENTRP WITH FRAME {&FRAME-NAME}.
   DISPLAY FILL-IN-LOPNR WITH FRAME {&FRAME-NAME}.
   DISPLAY FILL-IN-BEN WITH FRAME {&FRAME-NAME}.
   DISPLAY FILL-IN-ENHET WITH FRAME {&FRAME-NAME}. 
   IF Guru.Konstanter:globforetag = "GRAN"  OR Guru.Konstanter:globforetag = "NAEK"  OR Guru.Konstanter:globforetag = "CPOMA" THEN FILL-IN-ENTRP:HIDDEN = TRUE.
   IF kalkbefbtemp.BEF1 NE "" THEN DISPLAY FILL-IN-F1 WITH FRAME {&FRAME-NAME}. 
   IF kalkbefbtemp.BEF2 NE "" THEN DISPLAY FILL-IN-F2 WITH FRAME {&FRAME-NAME}.
   IF kalkbefbtemp.BEF3 NE "" THEN DISPLAY FILL-IN-F3 WITH FRAME {&FRAME-NAME}.
   IF kalkbefbtemp.BEF4 NE "" THEN DISPLAY FILL-IN-F4 WITH FRAME {&FRAME-NAME}.
   IF kalkbefbtemp.BEF5 NE "" THEN DISPLAY FILL-IN-F5 WITH FRAME {&FRAME-NAME}.
   IF kalkbefbtemp.BEF6 NE "" THEN DISPLAY FILL-IN-F6 WITH FRAME {&FRAME-NAME}.
   IF kalkbefbtemp.BEF7 NE "" THEN DISPLAY FILL-IN-F7 WITH FRAME {&FRAME-NAME}.
   IF kalkbefbtemp.BEF8 NE "" THEN DISPLAY FILL-IN-F8 WITH FRAME {&FRAME-NAME}.
   IF kalkbefbtemp.BEF9 NE "" THEN DISPLAY FILL-IN-F9 WITH FRAME {&FRAME-NAME}.
   /*KALKmatris
   IF kalkbefbtemp.BEF10 NE "" THEN DISPLAY FILL-IN-F10 WITH FRAME {&FRAME-NAME}.
   */
   DISPLAY FILL-IN-MATERIEL WITH FRAME {&FRAME-NAME}.
   DISPLAY FILL-IN-OVRIGT WITH FRAME {&FRAME-NAME}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnok_UI C-Win 
PROCEDURE btnok_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   ASSIGN
   FILL-IN-ENHET = INPUT FRAME {&FRAME-NAME} FILL-IN-ENHET
   FILL-IN-BEN = INPUT FILL-IN-BEN
   FILL-IN-F1 = INPUT FILL-IN-F1
   FILL-IN-F2 = INPUT FILL-IN-F2
   FILL-IN-F3 = INPUT FILL-IN-F3
   FILL-IN-F4 = INPUT FILL-IN-F4
   FILL-IN-F5 = INPUT FILL-IN-F5
   FILL-IN-F6 = INPUT FILL-IN-F6
   FILL-IN-F7 = INPUT FILL-IN-F7
   FILL-IN-F8 = INPUT FILL-IN-F8
   FILL-IN-F9 = INPUT FILL-IN-F9
   /*KALKmatris
   FILL-IN-F10 = INPUT FILL-IN-F10
   */
   FILL-IN-P1 = INPUT FILL-IN-P1
   FILL-IN-P2 = INPUT FILL-IN-P2
   FILL-IN-P3 = INPUT FILL-IN-P3
   FILL-IN-P4 = INPUT FILL-IN-P4
   FILL-IN-P5 = INPUT FILL-IN-P5
   FILL-IN-P6 = INPUT FILL-IN-P6
   FILL-IN-P7 = INPUT FILL-IN-P7
   FILL-IN-P8 = INPUT FILL-IN-P8
   FILL-IN-P9 = INPUT FILL-IN-P9
   /*KALKmatris
   FILL-IN-P10 = INPUT FILL-IN-P10
   */
   FILL-IN-EA = INPUT FILL-IN-EA
   FILL-IN-ARBETE = INPUT FILL-IN-ARBETE
   FILL-IN-MATERIEL = INPUT FILL-IN-MATERIEL
   FILL-IN-MASKINKOST = INPUT FILL-IN-MASKINKOST
   FILL-IN-OVRIGT = INPUT FILL-IN-OVRIGT   
   FILL-IN-ENTRP = INPUT FILL-IN-ENTRP. 
   
   ASSIGN
   ekalk_temp.BENAMNING = FILL-IN-BEN                
   ekalk_temp.ENHET = FILL-IN-ENHET
   kalkbefbtemp.F1 = FILL-IN-F1
   kalkbefbtemp.F2 = FILL-IN-F2
   kalkbefbtemp.F3 = FILL-IN-F3
   kalkbefbtemp.F4 = FILL-IN-F4
   kalkbefbtemp.F5 = FILL-IN-F5
   kalkbefbtemp.F6 = FILL-IN-F6
   kalkbefbtemp.F7 = FILL-IN-F7
   kalkbefbtemp.F8 = FILL-IN-F8
   kalkbefbtemp.F9 = FILL-IN-F9
   /*KALKmatris
   kalkbefbtemp.F10 = FILL-IN-F10
   */
   kalkbefbtemp.EA = FILL-IN-EA
   kalkbefbtemp.ARBETE = FILL-IN-ARBETE
   kalkbefbtemp.MATERIEL = FILL-IN-MATERIEL
   kalkbefbtemp.MASKINKOST = FILL-IN-MASKINKOST
   kalkbefbtemp.OVRIGT = FILL-IN-OVRIGT      
   kalkbefbtemp.ENTRP = FILL-IN-ENTRP.   
   EMPTY TEMP-TABLE extrakalkbefbtemp NO-ERROR. 
   CREATE extrakalkbefbtemp.
   BUFFER-COPY kalkbefbtemp TO extrakalkbefbtemp.
   ASSIGN
   extrakalkbefbtemp.BERNR = INTEGER(valaonr)
   extrakalkbefbtemp.OMRADE = valomrade
   extrakalkbefbtemp.NUM = ekalk_temp.NUM
   extrakalkbefbtemp.ARBKOD = "EGEN"
   extrakalkbefbtemp.LOPNR = FILL-IN-LOPNR
   extrakalkbefbtemp.PRIS1 = FILL-IN-P1
   extrakalkbefbtemp.PRIS2 = FILL-IN-P2
   extrakalkbefbtemp.PRIS3 = FILL-IN-P3
   extrakalkbefbtemp.PRIS4 = FILL-IN-P4
   extrakalkbefbtemp.PRIS5 = FILL-IN-P5
   extrakalkbefbtemp.PRIS6 = FILL-IN-P6
   extrakalkbefbtemp.PRIS7 = FILL-IN-P7
   extrakalkbefbtemp.PRIS8 = FILL-IN-P8
   extrakalkbefbtemp.PRIS9 = FILL-IN-P9.
      /*
   extrakalkbefbtemp.PRIS10 = FILL-IN-P10.
   */
   
   /*EMPTY TEMP-TABLE ekalk_temp NO-ERROR. 
   CREATE ekalk_temp.
   BUFFER-COPY kalk_temp TO ekalk_temp.*/
   
   IF ekalk_temp.SID = 0 THEN RUN btnokegenkod_UI IN extrbefbapph (INPUT andra,INPUT valaonr,INPUT valomrade,INPUT-OUTPUT TABLE ekalk_temp, INPUT TABLE extrakalkbefbtemp).
   ELSE RUN btnokegenkodsid_UI IN extrbefbapph (INPUT andra,INPUT valaonr,INPUT valomrade,INPUT-OUTPUT TABLE ekalk_temp, INPUT TABLE extrakalkbefbtemp).
   FIND FIRST ekalk_temp NO-LOCK NO-ERROR.   
   FIND FIRST kalkbefbtemp WHERE kalkbefbtemp.OMRADE = valomrade AND
   kalkbefbtemp.BERNR = INTEGER(valaonr) AND kalkbefbtemp.ARBKOD = ekalk_temp.ARBKOD AND
   kalkbefbtemp.LOPNR = ekalk_temp.LOPNR USE-INDEX OMRADE EXCLUSIVE-LOCK NO-ERROR.
   IF NOT AVAILABLE kalkbefbtemp THEN CREATE kalkbefbtemp.
   BUFFER-COPY extrakalkbefbtemp TO kalkbefbtemp.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
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
  DISPLAY FILL-IN-ENHET FILL-IN-BEN FILL-IN-MATERIEL FILL-IN-OVRIGT 
          FILL-IN-ENTRP FILL-IN-LOPNR FILL-IN-L1 FILL-IN-L2 FILL-IN-L3 
          FILL-IN-L4 FILL-IN-L5 FILL-IN-L6 FILL-IN-L7 FILL-IN-L8 FILL-IN-L9 
          FILL-IN-L10 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-1 RECT-2 RECT-4 FILL-IN-ENHET FILL-IN-BEN FILL-IN-MATERIEL 
         FILL-IN-OVRIGT BTN_OK BTN_AVB FILL-IN-LOPNR FILL-IN-L1 FILL-IN-L2 
         FILL-IN-L3 FILL-IN-L4 FILL-IN-L5 FILL-IN-L6 FILL-IN-L7 FILL-IN-L8 
         FILL-IN-L9 FILL-IN-L10 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE gomma_UI C-Win 
PROCEDURE gomma_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
         
   IF kalkbefbtemp.BEF1 NE "" THEN DO:           
      ASSIGN      
      FILL-IN-L1:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      FILL-IN-L1 = kalkbefbtemp.BEF1.
      ENABLE FILL-IN-F1 WITH FRAME {&FRAME-NAME}. 
      ENABLE FILL-IN-P1 WITH FRAME {&FRAME-NAME}.    
      DISPLAY FILL-IN-F1 WITH FRAME {&FRAME-NAME}.
      DISPLAY FILL-IN-L1 WITH FRAME {&FRAME-NAME}.      
      IF eget = TRUE THEN DO:
         FILL-IN-P1 = kalkbefbtemp.PRIS1.
      END.
      ELSE DO:
         IF kalkbefbtemp.EBR1 = 1 THEN FILL-IN-P1 = ebrpristemp.MONT.
         ELSE IF kalkbefbtemp.EBR1 = 2 THEN FILL-IN-P1 = ebrpristemp.MASK1.
         ELSE FILL-IN-P1 = ebrpristemp.MASK2.
      END.
      DISPLAY FILL-IN-P1 WITH FRAME {&FRAME-NAME}.
   END.
   IF kalkbefbtemp.BEF2 NE "" THEN DO:
      ASSIGN      
      FILL-IN-L2:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      FILL-IN-L2 = kalkbefbtemp.BEF2.
      ENABLE FILL-IN-F2 WITH FRAME {&FRAME-NAME}.      
      ENABLE FILL-IN-P2 WITH FRAME {&FRAME-NAME}.
      DISPLAY FILL-IN-F2 WITH FRAME {&FRAME-NAME}.
      DISPLAY FILL-IN-L2 WITH FRAME {&FRAME-NAME}.
      IF eget = TRUE THEN DO:
         FILL-IN-P2 = kalkbefbtemp.PRIS2.
      END.
      ELSE DO:
         IF kalkbefbtemp.EBR2 = 1 THEN FILL-IN-P2 = ebrpristemp.MONT.
         ELSE IF kalkbefbtemp.EBR2 = 2 THEN FILL-IN-P2 = ebrpristemp.MASK1.
         ELSE FILL-IN-P2 = ebrpristemp.MASK2.
      END.
      DISPLAY FILL-IN-P2 WITH FRAME {&FRAME-NAME}.
   END.
   IF kalkbefbtemp.BEF3 NE "" THEN DO:
      ASSIGN      
      FILL-IN-L3:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      FILL-IN-L3 = kalkbefbtemp.BEF3.
      ENABLE FILL-IN-F3 WITH FRAME {&FRAME-NAME}.    
      ENABLE FILL-IN-P3 WITH FRAME {&FRAME-NAME}.
      DISPLAY FILL-IN-F3 WITH FRAME {&FRAME-NAME}.
      DISPLAY FILL-IN-L3 WITH FRAME {&FRAME-NAME}.
      IF eget = TRUE THEN DO:
         FILL-IN-P3 = kalkbefbtemp.PRIS3.
      END.
      ELSE DO:
         IF kalkbefbtemp.EBR3 = 1 THEN FILL-IN-P3 = ebrpristemp.MONT.
         ELSE IF kalkbefbtemp.EBR3 = 2 THEN FILL-IN-P3 = ebrpristemp.MASK1.
         ELSE FILL-IN-P3 = ebrpristemp.MASK2.
      END.
      DISPLAY FILL-IN-P3 WITH FRAME {&FRAME-NAME}.
   END.
   IF kalkbefbtemp.BEF4 NE "" THEN DO:
      ASSIGN      
      FILL-IN-L4:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      FILL-IN-L4 = kalkbefbtemp.BEF4.
      ENABLE FILL-IN-F4 WITH FRAME {&FRAME-NAME}.  
      ENABLE FILL-IN-P4 WITH FRAME {&FRAME-NAME}.
      DISPLAY FILL-IN-F4 WITH FRAME {&FRAME-NAME}.
      DISPLAY FILL-IN-L4 WITH FRAME {&FRAME-NAME}.
      IF eget = TRUE THEN DO:
         FILL-IN-P4 = kalkbefbtemp.PRIS4.
      END.
      ELSE DO:
         IF kalkbefbtemp.EBR4 = 1 THEN FILL-IN-P4 = ebrpristemp.MONT.
         ELSE IF kalkbefbtemp.EBR4 = 2 THEN FILL-IN-P4 = ebrpristemp.MASK1.
         ELSE FILL-IN-P4 = ebrpristemp.MASK2.
      END.
      DISPLAY FILL-IN-P4 WITH FRAME {&FRAME-NAME}.
   END.
   IF kalkbefbtemp.BEF5 NE "" THEN DO:
      ASSIGN      
      FILL-IN-L5:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      FILL-IN-L5 = kalkbefbtemp.BEF5.
      ENABLE FILL-IN-F5 WITH FRAME {&FRAME-NAME}.  
      ENABLE FILL-IN-P5 WITH FRAME {&FRAME-NAME}.
      DISPLAY FILL-IN-F5 WITH FRAME {&FRAME-NAME}.
      DISPLAY FILL-IN-L5 WITH FRAME {&FRAME-NAME}.
      IF eget = TRUE THEN DO:
         FILL-IN-P5 = kalkbefbtemp.PRIS5.
      END.
      ELSE DO:
         IF kalkbefbtemp.EBR5 = 1 THEN FILL-IN-P5 = ebrpristemp.MONT.
         ELSE IF kalkbefbtemp.EBR5 = 2 THEN FILL-IN-P5 = ebrpristemp.MASK1.
         ELSE FILL-IN-P5 = ebrpristemp.MASK2.
      END.
      DISPLAY FILL-IN-P5 WITH FRAME {&FRAME-NAME}.
   END.
   IF kalkbefbtemp.BEF6 NE "" THEN DO:
      ASSIGN      
      FILL-IN-L6:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      FILL-IN-L6 = kalkbefbtemp.BEF6.
      ENABLE FILL-IN-F6 WITH FRAME {&FRAME-NAME}.    
      ENABLE FILL-IN-P6 WITH FRAME {&FRAME-NAME}.
      DISPLAY FILL-IN-F6 WITH FRAME {&FRAME-NAME}.
      DISPLAY FILL-IN-L6 WITH FRAME {&FRAME-NAME}.
      IF eget = TRUE THEN DO:
         FILL-IN-P6 = kalkbefbtemp.PRIS6.
      END.
      ELSE DO:
         IF kalkbefbtemp.EBR6 = 1 THEN FILL-IN-P6 = ebrpristemp.MONT.
         ELSE IF kalkbefbtemp.EBR6 = 2 THEN FILL-IN-P6 = ebrpristemp.MASK1.
         ELSE FILL-IN-P6 = ebrpristemp.MASK2.
      END.
      DISPLAY FILL-IN-P6 WITH FRAME {&FRAME-NAME}.
   END.
   IF kalkbefbtemp.BEF7 NE "" THEN DO:
      ASSIGN      
      FILL-IN-L7:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      FILL-IN-L7 = kalkbefbtemp.BEF7.
      ENABLE FILL-IN-F7 WITH FRAME {&FRAME-NAME}. 
      ENABLE FILL-IN-P7 WITH FRAME {&FRAME-NAME}.     
      DISPLAY FILL-IN-F7 WITH FRAME {&FRAME-NAME}.
      DISPLAY FILL-IN-L7 WITH FRAME {&FRAME-NAME}.
      IF eget = TRUE THEN DO:
         FILL-IN-P7 = kalkbefbtemp.PRIS7.
      END.
      ELSE DO:
         IF kalkbefbtemp.EBR7 = 1 THEN FILL-IN-P7 = ebrpristemp.MONT.
         ELSE IF kalkbefbtemp.EBR7 = 2 THEN FILL-IN-P7 = ebrpristemp.MASK1.
         ELSE IF kalkbefbtemp.EBR7 = 3 THEN  FILL-IN-P7 = ebrpristemp.MASK2.
         ELSE IF kalkbefbtemp.EBR7 = 4 THEN  FILL-IN-P7 = emask3.
      END.
      DISPLAY FILL-IN-P7 WITH FRAME {&FRAME-NAME}.
   END.
   IF kalkbefbtemp.BEF8 NE "" THEN DO:
      ASSIGN      
      FILL-IN-L8:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      FILL-IN-L8 = kalkbefbtemp.BEF8.
      ENABLE FILL-IN-F8 WITH FRAME {&FRAME-NAME}.    
      ENABLE FILL-IN-P8 WITH FRAME {&FRAME-NAME}.
      DISPLAY FILL-IN-F8 WITH FRAME {&FRAME-NAME}.
      DISPLAY FILL-IN-L8 WITH FRAME {&FRAME-NAME}.
      IF eget = TRUE THEN DO:
         FILL-IN-P8 = kalkbefbtemp.PRIS8.
      END.
      ELSE DO:
         IF kalkbefbtemp.EBR8 = 1 THEN FILL-IN-P8 = ebrpristemp.MONT.
         ELSE IF kalkbefbtemp.EBR8 = 2 THEN FILL-IN-P8 = ebrpristemp.MASK1.
         ELSE FILL-IN-P8 = ebrpristemp.MASK2.
      END.
      DISPLAY FILL-IN-P8 WITH FRAME {&FRAME-NAME}.
   END.
   IF kalkbefbtemp.BEF9 NE "" THEN DO:
      ASSIGN      
      FILL-IN-L9:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      FILL-IN-L9 = kalkbefbtemp.BEF9.
      ENABLE FILL-IN-F9 WITH FRAME {&FRAME-NAME}.   
      ENABLE FILL-IN-P9 WITH FRAME {&FRAME-NAME}.   
      DISPLAY FILL-IN-F9 WITH FRAME {&FRAME-NAME}.
      DISPLAY FILL-IN-L9 WITH FRAME {&FRAME-NAME}.
      IF eget = TRUE THEN DO:
         FILL-IN-P9 = kalkbefbtemp.PRIS9.
      END.
      ELSE DO:
         IF kalkbefbtemp.EBR9 = 1 THEN FILL-IN-P9 = ebrpristemp.MONT.
         ELSE IF kalkbefbtemp.EBR9 = 2 THEN FILL-IN-P9 = ebrpristemp.MASK1.
         ELSE FILL-IN-P9 = ebrpristemp.MASK2.
      END.
      DISPLAY FILL-IN-P9 WITH FRAME {&FRAME-NAME}.
   END.
   /*KALKmatris
   IF kalkbefbtemp.BEF10 NE "" THEN DO:
      ASSIGN     
      FILL-IN-L10:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      FILL-IN-L10 = kalkbefbtemp.BEF10.
      ENABLE FILL-IN-F10 WITH FRAME {&FRAME-NAME}.  
      ENABLE FILL-IN-P10 WITH FRAME {&FRAME-NAME}.   
      DISPLAY FILL-IN-F10 WITH FRAME {&FRAME-NAME}.
      DISPLAY FILL-IN-L10 WITH FRAME {&FRAME-NAME}.
      IF eget = TRUE THEN DO:
         FILL-IN-P10 = kalkbefbtemp.PRIS10.
      END.
      ELSE DO:
         IF kalkbefbtemp.EBR10 = 1 THEN FILL-IN-P10 = ebrpristemp.MONT.
         ELSE IF kalkbefbtemp.EBR10 = 2 THEN FILL-IN-P10 = ebrpristemp.MASK1.
         ELSE FILL-IN-P10 = ebrpristemp.MASK2.
      END.
      DISPLAY FILL-IN-P10 WITH FRAME {&FRAME-NAME}.
   END.
   */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE gomma_UI C-Win 
PROCEDURE gommaest_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE VARIABLE gomvar AS LOGICAL NO-UNDO.
   IF Guru.Konstanter:globforetag = "GRAN" OR Guru.Konstanter:globforetag = "NAEK" OR Guru.Konstanter:globforetag = "ELPA"  OR Guru.Konstanter:globforetag = "CPOMA" THEN.
   ELSE RETURN.
   IF FILL-IN-F3 + FILL-IN-F4 + FILL-IN-F5 + FILL-IN-F6 + FILL-IN-F7 + FILL-IN-F8 + FILL-IN-F9 = 0 THEN DO:
      RUN gomma_UI.
      RETURN.
   END.   
    
   IF FILL-IN-F3 NE 0 THEN gomvar = FALSE.
   ELSE gomvar = TRUE.
   ASSIGN      
   FILL-IN-L3:HIDDEN IN FRAME {&FRAME-NAME} = gomvar
   FILL-IN-P3:HIDDEN IN FRAME {&FRAME-NAME} = gomvar
   FILL-IN-F3:HIDDEN IN FRAME {&FRAME-NAME} = gomvar.
   
   IF FILL-IN-F4 NE 0 THEN gomvar = FALSE.
   ELSE gomvar = TRUE.
   ASSIGN      
   FILL-IN-L4:HIDDEN IN FRAME {&FRAME-NAME} = gomvar
   FILL-IN-P4:HIDDEN IN FRAME {&FRAME-NAME} = gomvar
   FILL-IN-F4:HIDDEN IN FRAME {&FRAME-NAME} = gomvar.
     
   IF FILL-IN-F5 NE 0 THEN gomvar = FALSE.
   ELSE gomvar = TRUE.
   ASSIGN      
   FILL-IN-L5:HIDDEN IN FRAME {&FRAME-NAME} = gomvar
   FILL-IN-P5:HIDDEN IN FRAME {&FRAME-NAME} = gomvar
   FILL-IN-F5:HIDDEN IN FRAME {&FRAME-NAME} = gomvar.
     
   IF FILL-IN-F6 NE 0 THEN gomvar = FALSE.
   ELSE gomvar = TRUE.
   ASSIGN      
   FILL-IN-L6:HIDDEN IN FRAME {&FRAME-NAME} = gomvar
   FILL-IN-P6:HIDDEN IN FRAME {&FRAME-NAME} = gomvar
   FILL-IN-F6:HIDDEN IN FRAME {&FRAME-NAME} = gomvar.      
   IF FILL-IN-F7 NE 0 THEN gomvar = FALSE.
   ELSE gomvar = TRUE.
   ASSIGN      
   FILL-IN-L7:HIDDEN IN FRAME {&FRAME-NAME} = gomvar
   FILL-IN-P7:HIDDEN IN FRAME {&FRAME-NAME} = gomvar
   FILL-IN-F7:HIDDEN IN FRAME {&FRAME-NAME} = gomvar.
  
   IF FILL-IN-F8 NE 0 THEN gomvar = FALSE.
   ELSE gomvar = TRUE.
   ASSIGN      
   FILL-IN-L8:HIDDEN IN FRAME {&FRAME-NAME} = gomvar
   FILL-IN-P8:HIDDEN IN FRAME {&FRAME-NAME} = gomvar
   FILL-IN-F8:HIDDEN IN FRAME {&FRAME-NAME} = gomvar.
     
   IF FILL-IN-F9 NE 0 THEN gomvar = FALSE.
   ELSE gomvar = TRUE.
   ASSIGN      
   FILL-IN-L9:HIDDEN IN FRAME {&FRAME-NAME} = gomvar
   FILL-IN-P9:HIDDEN IN FRAME {&FRAME-NAME} = gomvar
   FILL-IN-F9:HIDDEN IN FRAME {&FRAME-NAME} = gomvar.
  
   /*KALKmatris
   IF FILL-IN-F10 NE 0 THEN gomvar = FALSE.
   ELSE gomvar = TRUE.
      ASSIGN      
      FILL-IN-L10:HIDDEN IN FRAME {&FRAME-NAME} = gomvar
      FILL-IN-P10:HIDDEN IN FRAME {&FRAME-NAME} = gomvar
      FILL-IN-F10:HIDDEN IN FRAME {&FRAME-NAME} = gomvar.
   
   */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE rakna_UI C-Win 
PROCEDURE rakna_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   ASSIGN  
   FILL-IN-F1 = INPUT FRAME {&FRAME-NAME} FILL-IN-F1
   FILL-IN-F2 = INPUT FRAME {&FRAME-NAME} FILL-IN-F2
   FILL-IN-F3 = INPUT FRAME {&FRAME-NAME} FILL-IN-F3
   FILL-IN-F4 = INPUT FRAME {&FRAME-NAME} FILL-IN-F4
   FILL-IN-F5 = INPUT FRAME {&FRAME-NAME} FILL-IN-F5
   FILL-IN-F6 = INPUT FRAME {&FRAME-NAME} FILL-IN-F6
   FILL-IN-F7 = INPUT FRAME {&FRAME-NAME} FILL-IN-F7
   FILL-IN-F8 = INPUT FRAME {&FRAME-NAME} FILL-IN-F8
   FILL-IN-F9 = INPUT FRAME {&FRAME-NAME} FILL-IN-F9
   /*KALKmatris
   FILL-IN-F10 = INPUT FRAME {&FRAME-NAME} FILL-IN-F10
   */
   FILL-IN-P1 = INPUT FRAME {&FRAME-NAME} FILL-IN-P1
   FILL-IN-P2 = INPUT FRAME {&FRAME-NAME} FILL-IN-P2
   FILL-IN-P3 = INPUT FRAME {&FRAME-NAME} FILL-IN-P3
   FILL-IN-P4 = INPUT FRAME {&FRAME-NAME} FILL-IN-P4
   FILL-IN-P5 = INPUT FRAME {&FRAME-NAME} FILL-IN-P5
   FILL-IN-P6 = INPUT FRAME {&FRAME-NAME} FILL-IN-P6
   FILL-IN-P7 = INPUT FRAME {&FRAME-NAME} FILL-IN-P7
   FILL-IN-P8 = INPUT FRAME {&FRAME-NAME} FILL-IN-P8
   FILL-IN-P9 = INPUT FRAME {&FRAME-NAME} FILL-IN-P9
      /*KALKmatris
   FILL-IN-P10 = INPUT FRAME {&FRAME-NAME} FILL-IN-P10      
   */
   FILL-IN-MATERIEL = INPUT FRAME {&FRAME-NAME} FILL-IN-MATERIEL   
   FILL-IN-OVRIGT = INPUT FRAME {&FRAME-NAME} FILL-IN-OVRIGT 
   FILL-IN-ENTRP = INPUT FRAME {&FRAME-NAME} FILL-IN-ENTRP
   eavar = 0
   arbvar = 0
   mskvar = 0.
   IF kalkbefbtemp.BEF1 NE "" THEN DO:   
      IF kalkbefbtemp.PERSMASK1 = TRUE THEN DO:
         arbvar = arbvar + (FILL-IN-F1 * FILL-IN-P1).
         IF kalkbefbtemp.EA1 = TRUE THEN 
         eavar = eavar + FILL-IN-F1.
      END.
      ELSE DO:
         mskvar = mskvar + (FILL-IN-F1 * FILL-IN-P1).
         IF kalkbefbtemp.EA1 = TRUE THEN
         eavar = eavar + ((FILL-IN-F1 * FILL-IN-P1) / ebrpristemp.RORLIG).
      END.   
   END.   
   IF kalkbefbtemp.BEF2 NE "" THEN DO:
      IF kalkbefbtemp.PERSMASK2 = TRUE THEN DO:
         arbvar = arbvar + (FILL-IN-F2 * FILL-IN-P2).
         IF kalkbefbtemp.EA2 = TRUE THEN
         eavar = eavar + FILL-IN-F2.
      END.
      ELSE DO:
         mskvar = mskvar + (FILL-IN-F2 * FILL-IN-P2).
         IF kalkbefbtemp.EA2 = TRUE THEN
         eavar = eavar + ((FILL-IN-F2 * FILL-IN-P2) / ebrpristemp.RORLIG).
      END.        
   END.   
   IF kalkbefbtemp.BEF3 NE "" THEN DO:
      IF kalkbefbtemp.PERSMASK3 = TRUE THEN DO:
         arbvar = arbvar + (FILL-IN-F3 * FILL-IN-P3).
         IF kalkbefbtemp.EA3 = TRUE THEN
         eavar = eavar + FILL-IN-F3.
      END.
      ELSE DO:
         mskvar = mskvar + (FILL-IN-F3 * FILL-IN-P3).
         IF kalkbefbtemp.EA3 = TRUE THEN
         eavar = eavar + ((FILL-IN-F3 * FILL-IN-P3) / ebrpristemp.RORLIG).
      END.   
   END.   
   IF kalkbefbtemp.BEF4 NE "" THEN DO:
      IF kalkbefbtemp.PERSMASK4 = TRUE THEN DO:
         arbvar = arbvar + (FILL-IN-F4 * FILL-IN-P4).
         IF kalkbefbtemp.EA4 = TRUE THEN
         eavar = eavar + FILL-IN-F4.
      END.
      ELSE DO:
         mskvar = mskvar + (FILL-IN-F4 * FILL-IN-P4).
         IF kalkbefbtemp.EA4 = TRUE THEN
         eavar = eavar + ((FILL-IN-F4 * FILL-IN-P4) / ebrpristemp.RORLIG).
      END.
   END.   
   IF kalkbefbtemp.BEF5 NE "" THEN DO:
      IF kalkbefbtemp.PERSMASK5 = TRUE THEN DO:
         arbvar = arbvar + (FILL-IN-F5 * FILL-IN-P5).
         IF kalkbefbtemp.EA5 = TRUE THEN
         eavar = eavar + FILL-IN-F5.
      END.
      ELSE DO:
         mskvar = mskvar + (FILL-IN-F5 * FILL-IN-P5).
         IF kalkbefbtemp.EA5 = TRUE THEN
         eavar = eavar + ((FILL-IN-F5 * FILL-IN-P5) / ebrpristemp.RORLIG).
      END.
   END.   
   IF kalkbefbtemp.BEF6 NE "" THEN DO:
      IF kalkbefbtemp.PERSMASK6 = TRUE THEN DO:
         arbvar = arbvar + (FILL-IN-F6 * FILL-IN-P6).
         IF kalkbefbtemp.EA6 = TRUE THEN
         eavar = eavar + FILL-IN-F6.
      END.
      ELSE DO:
         mskvar = mskvar + (FILL-IN-F6 * FILL-IN-P6).
         IF kalkbefbtemp.EA6 = TRUE THEN
         eavar = eavar + ((FILL-IN-F6 * FILL-IN-P6) / ebrpristemp.RORLIG).
      END.
   END.   
   IF kalkbefbtemp.BEF7 NE "" THEN DO:
      IF kalkbefbtemp.PERSMASK7 = TRUE THEN DO:
         arbvar = arbvar + (FILL-IN-F7 * FILL-IN-P7).
         IF kalkbefbtemp.EA7 = TRUE THEN
         eavar = eavar + FILL-IN-F7.
      END.
      ELSE DO:
         mskvar = mskvar + (FILL-IN-F7 * FILL-IN-P7).
         IF kalkbefbtemp.EA7 = TRUE THEN
         eavar = eavar + ((FILL-IN-F7 * FILL-IN-P7) / ebrpristemp.RORLIG).
      END.
   END. 
   IF kalkbefbtemp.BEF8 NE "" THEN DO:
      IF kalkbefbtemp.PERSMASK8 = TRUE THEN DO:
         arbvar = arbvar + (FILL-IN-F8 * FILL-IN-P8).
         IF kalkbefbtemp.EA8 = TRUE THEN
         eavar = eavar + FILL-IN-F8.
      END.
      ELSE DO:
         mskvar = mskvar + (FILL-IN-F8 * FILL-IN-P8).
         IF kalkbefbtemp.EA8 = TRUE THEN
         eavar = eavar + ((FILL-IN-F8 * FILL-IN-P8) / ebrpristemp.RORLIG).
      END.
   END.
   IF kalkbefbtemp.BEF9 NE "" THEN DO:
      IF kalkbefbtemp.PERSMASK9 = TRUE THEN DO:
         arbvar = arbvar + (FILL-IN-F9 * FILL-IN-P9).
         IF kalkbefbtemp.EA9 = TRUE THEN
         eavar = eavar + FILL-IN-F9.
      END.
      ELSE DO:
         mskvar = mskvar + (FILL-IN-F9 * FILL-IN-P9).
         IF kalkbefbtemp.EA9 = TRUE THEN
         eavar = eavar + ((FILL-IN-F9 * FILL-IN-P9) / ebrpristemp.RORLIG).
      END.
   END.   
   /*KALKmatris
   IF kalkbefbtemp.BEF10 NE "" THEN DO:
      IF kalkbefbtemp.PERSMASK10 = TRUE THEN DO:
         arbvar = arbvar + (FILL-IN-F10 * FILL-IN-P10).
         IF kalkbefbtemp.EA10 = TRUE THEN
         eavar = eavar + FILL-IN-F10.
      END.
      ELSE DO:
         mskvar = mskvar + (FILL-IN-F10 * FILL-IN-P10).
         IF kalkbefbtemp.EA10 = TRUE THEN
         eavar = eavar + ((FILL-IN-F10 * FILL-IN-P10) / ebrpristemp.RORLIG).
      END.
   END.  
   */
   ASSIGN
   FILL-IN-EA = eavar
   FILL-IN-ARBETE = arbvar
   FILL-IN-MASKINKOST = mskvar
   FILL-IN-SUMMA = arbvar + mskvar + FILL-IN-MATERIEL + FILL-IN-OVRIGT + FILL-IN-ENTRP.
  
   DISPLAY FILL-IN-EA WITH FRAME {&FRAME-NAME}.
   DISPLAY FILL-IN-ARBETE WITH FRAME {&FRAME-NAME}.
   DISPLAY FILL-IN-MASKINKOST WITH FRAME {&FRAME-NAME}.
   DISPLAY FILL-IN-SUMMA WITH FRAME {&FRAME-NAME}.
   RUN gommaest_UI.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

