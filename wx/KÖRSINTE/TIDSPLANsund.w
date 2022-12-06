&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
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
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{ALLDEF.I}
{EXTRADATA.I}
&Scoped-define NEW 
{GLOBVAR2DEL1.I}

&Scoped-define SHARED SHARED 
{DIRDEF.I}
&Scoped-define NEW NEW
&Scoped-define SHARED SHARED 
{AVTAONRTEMP.I}
{TIDSPLAN.I}
DEFINE NEW SHARED VARIABLE brwproch AS HANDLE NO-UNDO.
DEFINE NEW SHARED VARIABLE nyttaoapph AS HANDLE NO-UNDO.                      /*NYTTAOAPP.P*/
DEFINE NEW SHARED VARIABLE grundmappvar AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE vartpro AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE SHARED VARIABLE regdatum AS DATE NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.

DEFINE VARIABLE svar AS LOGICAL NO-UNDO.
DEFINE VARIABLE ingenkod AS LOGICAL NO-UNDO.
DEFINE VARIABLE dynbrwh AS HANDLE NO-UNDO.
DEFINE VARIABLE tidsplandynh AS HANDLE NO-UNDO.
DEFINE VARIABLE dynbrwproch AS HANDLE NO-UNDO.
DEFINE VARIABLE exdatahmth AS HANDLE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 RECT-3 RECT-4 FILL-IN-AVSTARTD ~
FILL-IN-AVSLUTD CMB_PERS CMB_ENTRE CMB_PROJL BTN_RENSA BTN_SPARA BTN_OK ~
BTN_AVB FILL-IN-BREDBAND FILL-IN-EL FILL-IN_PROJEKT FILL-IN-PAB FILL-IN-PAE ~
FILL-IN_DATUM FILL-IN-TFB FILL-IN-TAE 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-AVSTARTD FILL-IN-OCH ~
FILL-IN-AVSLUTD CMB_PERS CMB_ENTRE CMB_PROJL FILL-IN-BREDBAND FILL-IN-EL ~
FILL-IN_PROJEKT FILL-IN-PAB FILL-IN-PAE FILL-IN_DATUM FILL-IN-TFB ~
FILL-IN-TAE 

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
     LABEL "OK" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_RENSA 
     LABEL "Rensa tid" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_SPARA 
     LABEL "Snabbspara" 
     SIZE 14 BY 1.

DEFINE VARIABLE CMB_ENTRE AS CHARACTER FORMAT "X(256)":U 
     LABEL "Entreprenör" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_PERS AS CHARACTER FORMAT "X(256)":U 
     LABEL "Personal" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_PROJL AS CHARACTER FORMAT "X(256)":U 
     LABEL "Projektledare" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-AVSLUTD AS DATE FORMAT "99/99/99":U 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1 TOOLTIP "Avslutade mellan angivna datum" NO-UNDO.

DEFINE VARIABLE FILL-IN-AVSTARTD AS DATE FORMAT "99/99/99":U 
     LABEL "Visa veckor mellan" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1 TOOLTIP "Avslutade mellan angivna datum" NO-UNDO.

DEFINE VARIABLE FILL-IN-BREDBAND AS CHARACTER FORMAT "X(256)":U INITIAL "Bredband:" 
      VIEW-AS TEXT 
     SIZE 9.5 BY .63 NO-UNDO.

DEFINE VARIABLE FILL-IN-EL AS CHARACTER FORMAT "X(256)":U INITIAL "El:" 
      VIEW-AS TEXT 
     SIZE 5 BY .63 NO-UNDO.

DEFINE VARIABLE FILL-IN-OCH AS CHARACTER FORMAT "X(3)":U 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-PAB AS CHARACTER FORMAT "X(256)":U 
     LABEL "Planerad aktivitet" 
      VIEW-AS TEXT 
     SIZE 13.5 BY .63
     BGCOLOR 12  NO-UNDO.

DEFINE VARIABLE FILL-IN-PAE AS CHARACTER FORMAT "X(256)":U 
     LABEL "Planerad aktivitet" 
      VIEW-AS TEXT 
     SIZE 13.5 BY .63
     BGCOLOR 9  NO-UNDO.

DEFINE VARIABLE FILL-IN-TAE AS CHARACTER FORMAT "X(256)":U 
     LABEL "Tillfällig aktivitet" 
      VIEW-AS TEXT 
     SIZE 13.5 BY .63
     BGCOLOR 14  NO-UNDO.

DEFINE VARIABLE FILL-IN-TFB AS CHARACTER FORMAT "X(256)":U 
     LABEL "Tillfällig aktivitet" 
      VIEW-AS TEXT 
     SIZE 13.5 BY .63
     BGCOLOR 2  NO-UNDO.

DEFINE VARIABLE FILL-IN_DATUM AS DATE FORMAT "9999/99/99":U 
     LABEL "Datum" 
      VIEW-AS TEXT 
     SIZE 15.88 BY .63 NO-UNDO.

DEFINE VARIABLE FILL-IN_PROJEKT AS CHARACTER FORMAT "X(256)":U 
     LABEL "Projekt" 
      VIEW-AS TEXT 
     SIZE 27.13 BY .63 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 37.5 BY 2.5.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 45.5 BY 2.5.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 41 BY 2.5.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     FILL-IN-AVSTARTD AT ROW 1.5 COL 19.25 COLON-ALIGNED
     FILL-IN-OCH AT ROW 1.5 COL 29.63 COLON-ALIGNED NO-LABEL
     FILL-IN-AVSLUTD AT ROW 1.5 COL 33.63 COLON-ALIGNED NO-LABEL
     CMB_PERS AT ROW 1.5 COL 52.38 COLON-ALIGNED
     CMB_ENTRE AT ROW 1.5 COL 73.13 COLON-ALIGNED
     CMB_PROJL AT ROW 1.5 COL 95.38 COLON-ALIGNED
     BTN_RENSA AT ROW 28.17 COL 66.5
     BTN_SPARA AT ROW 28.17 COL 81.5
     BTN_OK AT ROW 28.17 COL 96.5
     BTN_AVB AT ROW 28.17 COL 111.5
     FILL-IN-BREDBAND AT ROW 25.42 COL 37.75 COLON-ALIGNED NO-LABEL
     FILL-IN-EL AT ROW 25.42 COL 83.38 COLON-ALIGNED NO-LABEL
     FILL-IN_PROJEKT AT ROW 25.46 COL 8.88 COLON-ALIGNED
     FILL-IN-PAB AT ROW 25.58 COL 67.63 COLON-ALIGNED
     FILL-IN-PAE AT ROW 25.58 COL 108.75 COLON-ALIGNED
     FILL-IN_DATUM AT ROW 26.71 COL 8.88 COLON-ALIGNED
     FILL-IN-TFB AT ROW 26.71 COL 67.63 COLON-ALIGNED
     FILL-IN-TAE AT ROW 26.71 COL 108.75 COLON-ALIGNED
     RECT-1 AT ROW 25.25 COL 1.5
     RECT-3 AT ROW 25.25 COL 39
     RECT-4 AT ROW 25.25 COL 84.5
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 124.75 BY 28.38.


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
         TITLE              = "Guru-Projekthuvud"
         HEIGHT             = 28.42
         WIDTH              = 124.88
         MAX-HEIGHT         = 28.42
         MAX-WIDTH          = 124.88
         VIRTUAL-HEIGHT     = 28.42
         VIRTUAL-WIDTH      = 124.88
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
ASSIGN 
       FILL-IN-AVSLUTD:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

ASSIGN 
       FILL-IN-AVSTARTD:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-OCH IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       FILL-IN-OCH:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

ASSIGN 
       FILL-IN-PAB:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       FILL-IN-PAE:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       FILL-IN-TAE:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       FILL-IN-TFB:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       FILL-IN_DATUM:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       FILL-IN_PROJEKT:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Guru-Projekthuvud */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Guru-Projekthuvud */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB C-Win
ON CHOOSE OF BTN_AVB IN FRAME DEFAULT-FRAME /* Avbryt */
DO:
  APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK C-Win
ON CHOOSE OF BTN_OK IN FRAME DEFAULT-FRAME /* OK */
DO:
   EMPTY TEMP-TABLE extidsplantemp NO-ERROR. 
   FOR EACH tidsplantemp WHERE tidsplantemp.ANDRAD = TRUE NO-LOCK:
      CREATE extidsplantemp.
      tidsplantemp.ANDRAD = FALSE.
      BUFFER-COPY tidsplantemp TO extidsplantemp. 
      EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
      CREATE inextradatatemp.          
      ASSIGN
      inextradatatemp.PROGRAM = "AOREF"                   
      inextradatatemp.HUVUDCH = extidsplantemp.AONR            
      inextradatatemp.HUVUDINT = extidsplantemp.DELNR
      inextradatatemp.SOKCHAR[2] = extidsplantemp.PERSONAL
      inextradatatemp.SOKCHAR[3] = extidsplantemp.ENTREPRENOR
      inextradatatemp.SOKCHAR[4] = extidsplantemp.PROJEKTLEDARE
      inextradatatemp.SOKINT[1] = extidsplantemp.FARGNR.
      RUN extraspar_UI IN exdatahmth (INPUT TABLE inextradatatemp).
   END.
   RUN tidsplanspar_UI IN nyttaoapph (INPUT Guru.Konstanter:globanv,INPUT TABLE extidsplantemp).
   APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_RENSA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_RENSA C-Win
ON CHOOSE OF BTN_RENSA IN FRAME DEFAULT-FRAME /* Rensa tid */
DO:
  RUN rensa_UI IN tidsplandynh.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_SPARA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_SPARA C-Win
ON CHOOSE OF BTN_SPARA IN FRAME DEFAULT-FRAME /* Snabbspara */
DO:
   EMPTY TEMP-TABLE extidsplantemp NO-ERROR. 
   FOR EACH tidsplantemp WHERE tidsplantemp.ANDRAD = TRUE NO-LOCK:
      CREATE extidsplantemp.
      tidsplantemp.ANDRAD = FALSE.
      BUFFER-COPY tidsplantemp TO extidsplantemp.     
      EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
      CREATE inextradatatemp.          
      ASSIGN
      inextradatatemp.PROGRAM = "AOREF"                   
      inextradatatemp.HUVUDCH = extidsplantemp.AONR            
      inextradatatemp.HUVUDINT = extidsplantemp.DELNR
      inextradatatemp.SOKCHAR[2] = extidsplantemp.PERSONAL
      inextradatatemp.SOKCHAR[3] = extidsplantemp.ENTREPRENOR
      inextradatatemp.SOKCHAR[4] = extidsplantemp.PROJEKTLEDARE
      inextradatatemp.SOKINT[1] = extidsplantemp.FARGNR.
      RUN extraspar_UI IN exdatahmth (INPUT TABLE inextradatatemp).
   END.
   RUN tidsplanspar_UI IN nyttaoapph (INPUT Guru.Konstanter:globanv,INPUT TABLE extidsplantemp).
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_ENTRE
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_ENTRE C-Win
ON VALUE-CHANGED OF CMB_ENTRE IN FRAME DEFAULT-FRAME /* Entreprenör */
DO:
  RUN entre_UI IN tidsplandynh (INPUT CMB_ENTRE:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_PERS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_PERS C-Win
ON VALUE-CHANGED OF CMB_PERS IN FRAME DEFAULT-FRAME /* Personal */
DO:
  RUN pers_UI IN tidsplandynh (INPUT CMB_PERS:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_PROJL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_PROJL C-Win
ON VALUE-CHANGED OF CMB_PROJL IN FRAME DEFAULT-FRAME /* Projektledare */
DO:
   RUN proj_UI IN tidsplandynh (INPUT CMB_PROJL:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-AVSLUTD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-AVSLUTD C-Win
ON LEAVE OF FILL-IN-AVSLUTD IN FRAME DEFAULT-FRAME
DO:
   FILL-IN-AVSLUTD = INPUT FILL-IN-AVSLUTD.
   IF FILL-IN-AVSLUTD < FILL-IN-AVSTARTD THEN DO:
      FILL-IN-AVSTARTD = FILL-IN-AVSLUTD.
      DISPLAY FILL-IN-AVSTARTD WITH FRAME {&FRAME-NAME}.
   END.      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-AVSLUTD C-Win
ON MOUSE-MENU-CLICK OF FILL-IN-AVSLUTD IN FRAME DEFAULT-FRAME
DO:
   ASSIGN
   FILL-IN-AVSLUTD = INPUT FILL-IN-AVSLUTD
   Guru.GlobalaVariabler:regdatum = INPUT FILL-IN-AVSLUTD.
   RUN AlmanBtn.w.
   FILL-IN-AVSLUTD = Guru.GlobalaVariabler:regdatum.
   DISPLAY FILL-IN-AVSLUTD WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-AVSTARTD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-AVSTARTD C-Win
ON LEAVE OF FILL-IN-AVSTARTD IN FRAME DEFAULT-FRAME /* Visa veckor mellan */
DO:
   FILL-IN-AVSTARTD = INPUT FILL-IN-AVSTARTD.
   IF FILL-IN-AVSLUTD < FILL-IN-AVSTARTD THEN DO:
      FILL-IN-AVSLUTD = FILL-IN-AVSTARTD.
      DISPLAY FILL-IN-AVSLUTD WITH FRAME {&FRAME-NAME}.
   END.      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-AVSTARTD C-Win
ON MOUSE-MENU-CLICK OF FILL-IN-AVSTARTD IN FRAME DEFAULT-FRAME /* Visa veckor mellan */
DO:
  ASSIGN
   FILL-IN-AVSTARTD = INPUT FILL-IN-AVSTARTD
   Guru.GlobalaVariabler:regdatum = INPUT FILL-IN-AVSTARTD.
   RUN AlmanBtn.w.
   FILL-IN-AVSTARTD = Guru.GlobalaVariabler:regdatum.
   DISPLAY FILL-IN-AVSTARTD WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-PAB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-PAB C-Win
ON ENTRY OF FILL-IN-PAB IN FRAME DEFAULT-FRAME /* Planerad aktivitet */
DO:
  RUN colvalue_UI IN tidsplandynh (INPUT FILL-IN-PAB:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-PAE
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-PAE C-Win
ON ENTRY OF FILL-IN-PAE IN FRAME DEFAULT-FRAME /* Planerad aktivitet */
DO:
  RUN colvalue_UI IN tidsplandynh (INPUT FILL-IN-PAE:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-TAE
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-TAE C-Win
ON ENTRY OF FILL-IN-TAE IN FRAME DEFAULT-FRAME /* Tillfällig aktivitet */
DO:
  RUN colvalue_UI IN tidsplandynh (INPUT FILL-IN-TAE:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-TFB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-TFB C-Win
ON ENTRY OF FILL-IN-TFB IN FRAME DEFAULT-FRAME /* Tillfällig aktivitet */
DO:
  RUN colvalue_UI IN tidsplandynh (INPUT FILL-IN-TFB:HANDLE).
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
   IF VALID-HANDLE(brwproch) THEN DELETE PROCEDURE brwproch NO-ERROR.
   IF VALID-HANDLE(dynbrwproch) THEN DELETE PROCEDURE dynbrwproch NO-ERROR.
   IF VALID-HANDLE(nyttaoapph) THEN DO:
      RUN borthandle_UI IN nyttaoapph.
      DELETE PROCEDURE nyttaoapph NO-ERROR.
      nyttaoapph = ?.
   END.  
   IF VALID-HANDLE(exdatahmth) THEN DELETE PROCEDURE exdatahmth NO-ERROR.
   RUN releaseh_UI IN tidsplandynh.
   IF VALID-HANDLE(tidsplandynh) THEN DELETE PROCEDURE tidsplandynh NO-ERROR.  
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
   ASSIGN
   FILL-IN_DATUM = TODAY
   FILL-IN_PROJEKT = globforetag
   ingenkod = FALSE.
   
   RUN arbartout_UI IN nyttaoapph (OUTPUT TABLE arbarttemp).
   DEFINE VARIABLE num AS INTEGER NO-UNDO.
   num = 1.
   FOR EACH arbarttemp NO-LOCK:
      FOR EACH valdaao WHERE valdaao.ARBARTKOD = arbarttemp.ARBARTKOD AND 
         valdaao.DELNR = 0 AND valdaao.AONRAVDATUM = 01/01/91 NO-LOCK:
         RUN laddaaotid IN nyttaoapph (INPUT valdaao.AONR,INPUT valdaao.DELNR,OUTPUT TABLE aotidslagtemp).
         FOR EACH aotidslagtemp WHERE aotidslagtemp.AONR = valdaao.AONR AND aotidslagtemp.DELNR = valdaao.DELNR
         AND aotidslagtemp.IDTLAG = "TIDPLAN" NO-LOCK:
            ASSIGN
            ingenkod = TRUE
            svar = FALSE.
            CREATE tidsplantemp.
            ASSIGN 
            tidsplantemp.AONR          = valdaao.AONR
            tidsplantemp.DELNR         = valdaao.DELNR
            tidsplantemp.ARBART        = arbarttemp.ARBBENAMNING
            tidsplantemp.ARBARTKOD     = arbarttemp.ARBARTKOD
            tidsplantemp.ORT           = valdaao.ORT
            tidsplantemp.IDTIDLAG      = aotidslagtemp.IDTIDLAG
            tidsplantemp.TIDLAGE       = aotidslagtemp.TIDLAGE
            tidsplantemp.STARTDAT      = aotidslagtemp.DAT1
            tidsplantemp.SLUTDAT       = aotidslagtemp.DAT2
            tidsplantemp.ANDRAD        = FALSE.
            RUN finnsplan IN nyttaoapph (INPUT valdaao.PLANNR, INPUT valdaao.ARTAL, OUTPUT svar).
            IF svar = FALSE THEN DO:
               RUN finnsplanao IN nyttaoapph (INPUT valdaao.AONR, INPUT valdaao.DELNR, OUTPUT svar).
            END.
            IF svar = TRUE THEN DO:
               tidsplantemp.PLAN = TRUE.
            END.
            ELSE DO:
               tidsplantemp.PLAN = FALSE.
            END.
            num = num + 1.
         END.           
      END.
   END.
   FILL-IN-OCH = "och".
   FIND FIRST tidsplantemp WHERE tidsplantemp.STARTDAT NE ? USE-INDEX DATUM NO-LOCK NO-ERROR.
   IF AVAILABLE tidsplantemp THEN DO:
      FILL-IN-AVSTARTD = tidsplantemp.STARTDAT.
   END.
   ELSE DO:
      FILL-IN-AVSTARTD = TODAY.
   END.
   FIND LAST tidsplantemp WHERE tidsplantemp.SLUTDAT NE ? AND tidsplantemp.SLUTDAT < 01/01/2099 USE-INDEX DATUM NO-LOCK NO-ERROR.
   IF AVAILABLE tidsplantemp THEN DO:
     FILL-IN-AVSLUTD = tidsplantemp.SLUTDAT.
   END.
   ELSE DO:
      FILL-IN-AVSLUTD = FILL-IN-AVSTARTD + 14.
   END.
   FOR EACH tidsplantemp:
      EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
      CREATE inextradatatemp.          
      ASSIGN
      inextradatatemp.PROGRAM = "AOREF"                   
      inextradatatemp.HUVUDCH = tidsplantemp.AONR.                    
      inextradatatemp.HUVUDINT = tidsplantemp.DELNR.                    
      RUN etabhamt_UI IN exdatahmth (INPUT TABLE inextradatatemp, OUTPUT TABLE extradatatemp APPEND). 
   END.  
   FOR EACH tidsplantemp:
      FIND FIRST extradatatemp WHERE extradatatemp.HUVUDCH = tidsplantemp.AONR AND 
      extradatatemp.HUVUDINT = tidsplantemp.DELNR NO-LOCK NO-ERROR.
      IF AVAILABLE extradatatemp THEN DO:      
         ASSIGN
         tidsplantemp.PERSONAL = extradatatemp.SOKCHAR[2]
         tidsplantemp.ENTREPRENOR = extradatatemp.SOKCHAR[3]
         tidsplantemp.PROJEKTLEDARE = extradatatemp.SOKCHAR[4]
         tidsplantemp.FARGNR = extradatatemp.SOKINT[1].     
      END.   
   END.
   IF ingenkod = FALSE THEN DO:
      MESSAGE Guru.Konstanter:gaok + " har inget " + LC(Guru.Konstanter:gtidlk) + " upplagt. " + CHR(10) + "Välj " + LC(Guru.Konstanter:gtidlk) + " ""TIDPLAN"" för varje " + Guru.Konstanter:gaok + " ni vill visa i tidplan." VIEW-AS ALERT-BOX.      .
   END.
   RUN createbrw_UI IN tidsplandynh.
   RUN enable_UI.
   ASSIGN
   CMB_PERS:HIDDEN = TRUE
   CMB_ENTRE:HIDDEN = TRUE
   CMB_PROJL:HIDDEN = TRUE.
   {FRMSIZE.I}  
   {WIN_M_SLUT.I}
   
   IF NOT THIS-PROCEDURE:PERSISTENT THEN WAIT-FOR CLOSE OF THIS-PROCEDURE.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE allstartbrw_UI C-Win 
PROCEDURE allstartbrw_UI :
/* -----------------------------------------------------------
  Purpose: Changing screen-value for combo-box CMB_OMR     
  Parameters:  Input = Screen-value for CMB_FOR
  Notes:       
-------------------------------------------------------------*/    
   CREATE BROWSE dynbrwh.
   RUN TIDSPLANDYNSUND.P PERSISTENT SET tidsplandynh
      (INPUT dynbrwh,INPUT FRAME DEFAULT-FRAME:HANDLE,INPUT FILL-IN-AVSTARTD:HANDLE,INPUT FILL-IN-AVSLUTD:HANDLE).    
   IF Guru.Konstanter:appcon THEN DO:
      RUN NYTTAOAPP.P PERSISTENT SET nyttaoapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
      RUN EXTRADATAHMT.P PERSISTENT SET exdatahmth ON Guru.Konstanter:apphand TRANSACTION DISTINCT.                  
   END.
   ELSE DO:
      RUN NYTTAOAPP.P PERSISTENT SET nyttaoapph.
      RUN EXTRADATAHMT.P PERSISTENT SET exdatahmth.
   END.

   

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
  DISPLAY FILL-IN-AVSTARTD FILL-IN-OCH FILL-IN-AVSLUTD CMB_PERS CMB_ENTRE 
          CMB_PROJL FILL-IN-BREDBAND FILL-IN-EL FILL-IN_PROJEKT FILL-IN-PAB 
          FILL-IN-PAE FILL-IN_DATUM FILL-IN-TFB FILL-IN-TAE 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-1 RECT-3 RECT-4 FILL-IN-AVSTARTD FILL-IN-AVSLUTD CMB_PERS 
         CMB_ENTRE CMB_PROJL BTN_RENSA BTN_SPARA BTN_OK BTN_AVB 
         FILL-IN-BREDBAND FILL-IN-EL FILL-IN_PROJEKT FILL-IN-PAB FILL-IN-PAE 
         FILL-IN_DATUM FILL-IN-TFB FILL-IN-TAE 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

