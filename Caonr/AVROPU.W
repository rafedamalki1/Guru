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
DEFINE INPUT PARAMETER aonrvar AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER delnrvar AS INTEGER NO-UNDO.
/* Local Variable Definitions ---                                       */
{ALLDEF.I}

&SCOPED-DEFINE NEW 
{GLOBVAR2DEL1.I}

{AVROPTEMP.I}
{LEVTEMP.I}
{ANVPERS.I}
&Scoped-define SHARED SHARED
{DIRDEF.I}
{AONRDEF.I}
DEFINE SHARED VARIABLE aonrrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE aonrrec2 AS RECID NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE regdatum AS DATE NO-UNDO.
DEFINE SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.

DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE aosok AS CHARACTER FORMAT "X(10)" NO-UNDO.
DEFINE VARIABLE ortssok AS CHARACTER NO-UNDO.
DEFINE VARIABLE prisvar AS INTEGER NO-UNDO.
DEFINE VARIABLE avropapph AS HANDLE NO-UNDO.

DEFINE NEW SHARED TEMP-TABLE vis_temp
   FIELD AONR AS CHARACTER
   FIELD DELNR AS INTEGER
   FIELD BENAMNING AS CHARACTER
   FIELD LEVNAMN AS CHARACTER
   FIELD DATUM AS DATE
   FIELD KLAR AS DATE
   FIELD BKONTAKT AS CHARACTER
   FIELD BTEL AS CHARACTER
   FIELD BMOBIL AS CHARACTER
   FIELD EKONTAKT AS CHARACTER
   FIELD ETEL AS CHARACTER
   FIELD EMOBIL AS CHARACTER
   FIELD PRIS AS INTEGER.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-57 RECT-58 RECT-59 RECT-60 RECT-61 ~
RECT-62 CMB_LEV BTN_VISA BTN_SKRIV TOG_FAST TOG_TIM TOG_P2 FILL-IN-BESTKONT ~
FILL-IN-TEL1 FILL-IN-MOB1 FILL-IN-ENTRKONT FILL-IN-TEL2 FILL-IN-MOB2 ~
FILL-IN-DATUM2 BTN_OK BTN_AVB FILL-IN-AONR FILL-IN-DELNR FILL-IN-DATUM ~
FILL-IN-ENTR 
&Scoped-Define DISPLAYED-OBJECTS CMB_LEV TOG_FAST TOG_TIM TOG_P2 ~
FILL-IN-BESTKONT FILL-IN-TEL1 FILL-IN-MOB1 FILL-IN-ENTRKONT FILL-IN-TEL2 ~
FILL-IN-MOB2 FILL-IN-DATUM2 FILL-IN-AONR FILL-IN-DELNR FILL-IN-DATUM ~
FILL-IN-ENTR 

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
     SIZE 14 BY 1
     BGCOLOR 8 .

DEFINE BUTTON BTN_OK 
     LABEL "Ok" 
     SIZE 14 BY 1
     BGCOLOR 8 .

DEFINE BUTTON BTN_SKRIV 
     LABEL "Skriv ut" 
     SIZE 14 BY 1
     FGCOLOR 1 .

DEFINE BUTTON BTN_VISA 
     LABEL "Visa" 
     SIZE 14 BY 1
     FGCOLOR 1 .

DEFINE VARIABLE CMB_LEV AS CHARACTER FORMAT "X(30)":U 
     LABEL "Entreprenör" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "0" 
     DROP-DOWN-LIST
     SIZE 18.25 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-AONR AS CHARACTER FORMAT "X(6)":U 
     LABEL "Aonr" 
      VIEW-AS TEXT 
     SIZE 7.25 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-BESTKONT AS CHARACTER FORMAT "X(25)":U 
     LABEL "Beställarens ombud/kontaktman" 
     VIEW-AS FILL-IN 
     SIZE 26.5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-DATUM AS DATE FORMAT "9999/99/99":U 
      VIEW-AS TEXT 
     SIZE 13.5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-DATUM2 AS DATE FORMAT "99/99/99":U 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-DELNR AS INTEGER FORMAT ">99":U INITIAL 0 
     LABEL "Delnr" 
      VIEW-AS TEXT 
     SIZE 7.5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-ENTR AS CHARACTER FORMAT "X(35)":U 
      VIEW-AS TEXT 
     SIZE 30.88 BY 1.21 NO-UNDO.

DEFINE VARIABLE FILL-IN-ENTRKONT AS CHARACTER FORMAT "X(25)":U 
     LABEL "Entreprenörens ombud/kontaktman" 
     VIEW-AS FILL-IN 
     SIZE 26.5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-MOB1 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Mobil" 
     VIEW-AS FILL-IN 
     SIZE 16.5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-MOB2 AS CHARACTER FORMAT "X(15)":U 
     LABEL "Mobil" 
     VIEW-AS FILL-IN 
     SIZE 16.5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-TEL1 AS CHARACTER FORMAT "X(15)":U 
     LABEL "Tele" 
     VIEW-AS FILL-IN 
     SIZE 16.5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-TEL2 AS CHARACTER FORMAT "X(15)":U 
     LABEL "Tele" 
     VIEW-AS FILL-IN 
     SIZE 16.5 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-57
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 88 BY 2.

DEFINE RECTANGLE RECT-58
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 88 BY 5.

DEFINE RECTANGLE RECT-59
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 88 BY 5.

DEFINE RECTANGLE RECT-60
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 88 BY 4.

DEFINE RECTANGLE RECT-61
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 88 BY 6.

DEFINE RECTANGLE RECT-62
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 88 BY 4.

DEFINE VARIABLE TOG_FAST AS LOGICAL INITIAL no 
     LABEL "Fastpris" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.13 BY 1 NO-UNDO.

DEFINE VARIABLE TOG_P2 AS LOGICAL INITIAL no 
     LABEL "P2" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.13 BY 1 NO-UNDO.

DEFINE VARIABLE TOG_TIM AS LOGICAL INITIAL no 
     LABEL "Timpris" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.13 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     CMB_LEV AT ROW 1.75 COL 13.5 COLON-ALIGNED
     BTN_VISA AT ROW 8 COL 90
     BTN_SKRIV AT ROW 9.08 COL 90
     TOG_FAST AT ROW 15.63 COL 2
     TOG_TIM AT ROW 15.63 COL 20
     TOG_P2 AT ROW 15.63 COL 38
     FILL-IN-BESTKONT AT ROW 18.17 COL 33.5 COLON-ALIGNED
     FILL-IN-TEL1 AT ROW 19.17 COL 33.5 COLON-ALIGNED
     FILL-IN-MOB1 AT ROW 19.17 COL 60 COLON-ALIGNED
     FILL-IN-ENTRKONT AT ROW 20.67 COL 33.5 COLON-ALIGNED
     FILL-IN-TEL2 AT ROW 21.67 COL 33.5 COLON-ALIGNED
     FILL-IN-MOB2 AT ROW 21.67 COL 60 COLON-ALIGNED
     FILL-IN-DATUM2 AT ROW 25.67 COL 26.38 COLON-ALIGNED NO-LABEL
     BTN_OK AT ROW 27.25 COL 75
     BTN_AVB AT ROW 27.25 COL 90
     FILL-IN-AONR AT ROW 4.83 COL 8 COLON-ALIGNED
     FILL-IN-DELNR AT ROW 4.83 COL 22 COLON-ALIGNED
     FILL-IN-DATUM AT ROW 5.75 COL 59 COLON-ALIGNED NO-LABEL
     FILL-IN-ENTR AT ROW 10.63 COL 55.5 COLON-ALIGNED NO-LABEL
     "avseende schakt för eldistribution, signal- och optoledningar." VIEW-AS TEXT
          SIZE 68.63 BY 1 AT ROW 6.83 COL 2
     "Betalningsvillkor och ersättning följer gällande ramavtal 2000." VIEW-AS TEXT
          SIZE 63.25 BY 1 AT ROW 14.63 COL 2
     "Ombud" VIEW-AS TEXT
          SIZE 11.13 BY 1 AT ROW 17.17 COL 2
          FONT 17
     "och Sundsvall Energi Elnät AB." VIEW-AS TEXT
          SIZE 30.63 BY 1 AT ROW 11.58 COL 2
     "Omfattning" VIEW-AS TEXT
          SIZE 17.88 BY 1 AT ROW 8.58 COL 2
          FONT 17
     "Ekonomi" VIEW-AS TEXT
          SIZE 17.13 BY 1 AT ROW 13.63 COL 2
          FONT 17
     "beredningshandlingar samt gällande ramavtal 2000 mellan" VIEW-AS TEXT
          SIZE 55.38 BY 1 AT ROW 10.58 COL 2
     "Beställning/Avrop" VIEW-AS TEXT
          SIZE 20.13 BY 1.13 AT ROW 3.33 COL 2
          FONT 17
     "Omfattning och utförande av entreprenaden följer gällande (bifogade)" VIEW-AS TEXT
          SIZE 68.13 BY 1 AT ROW 9.58 COL 2
     "eller överlämnande senast" VIEW-AS TEXT
          SIZE 25.25 BY 1 AT ROW 25.67 COL 2.5
     "Tider" VIEW-AS TEXT
          SIZE 8 BY 1 AT ROW 23.67 COL 2.5
          FONT 17
     "Entreprenaden skall vara klar för slutbesiktning/annat godkännande" VIEW-AS TEXT
          SIZE 66.25 BY 1 AT ROW 24.67 COL 2.5
     "Under åberopande av vårt mellanvarande ramavtal 2000 datum" VIEW-AS TEXT
          SIZE 58.38 BY 1 AT ROW 5.83 COL 2
     RECT-57 AT ROW 1 COL 1
     RECT-58 AT ROW 8 COL 1
     RECT-59 AT ROW 3 COL 1
     RECT-60 AT ROW 13 COL 1
     RECT-61 AT ROW 17 COL 1
     RECT-62 AT ROW 23 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 104.13 BY 27.33.


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
         TITLE              = "Beställning/Avrop"
         HEIGHT             = 27.33
         WIDTH              = 104.13
         MAX-HEIGHT         = 28.42
         MAX-WIDTH          = 106
         VIRTUAL-HEIGHT     = 28.42
         VIRTUAL-WIDTH      = 106
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
       CMB_LEV:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Beställning/Avrop */
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
ON WINDOW-CLOSE OF C-Win /* Beställning/Avrop */
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
   MESSAGE "OBS! Vill du spara dina ändringar?"
   VIEW-AS ALERT-BOX
   QUESTION BUTTONS YES-NO-CANCEL TITLE "Spara ändringar?" UPDATE svar AS LOGICAL.         
   IF svar THEN DO:      
      APPLY "CHOOSE" TO BTN_OK IN FRAME {&FRAME-NAME}.
   END.
   ELSE IF NOT svar THEN DO:       
      APPLY "CLOSE":U TO THIS-PROCEDURE.   
   END.                    
   ELSE DO:
      musz = musz.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK C-Win
ON CHOOSE OF BTN_OK IN FRAME DEFAULT-FRAME /* Ok */
DO:  
   IF INPUT TOG_FAST = TRUE THEN prisvar = 1.
   ELSE IF TOG_TIM = TRUE THEN prisvar = 2.
   ELSE IF TOG_P2 = TRUE THEN prisvar = 3.
   ELSE DO:
      MESSAGE "Ni har inte gjort något val under Ekonomi."
      VIEW-AS ALERT-BOX TITLE "Meddelande".
      RETURN NO-APPLY.
   END.
   FIND FIRST avroptemp WHERE avroptemp.AONR = aonrvar AND
   avroptemp.DELNR = delnrvar USE-INDEX AONR NO-LOCK NO-ERROR.
   IF NOT AVAILABLE avroptemp THEN DO:
      CREATE avroptemp.
      ASSIGN 
      avroptemp.AONR = aonrvar
      avroptemp.DELNR = delnrvar.
   END.
   FIND FIRST levtemp WHERE levtemp.LEVNAMN = INPUT CMB_LEV
   NO-LOCK NO-ERROR.
   IF AVAILABLE levtemp THEN DO:
      ASSIGN                                  
      avroptemp.DATUM = INPUT FILL-IN-DATUM       
      avroptemp.KLAR = INPUT FILL-IN-DATUM2       
      avroptemp.LEVKOD = levtemp.LEVKOD        
      avroptemp.BKONTAKT = INPUT FILL-IN-BESTKONT 
      avroptemp.BTEL = INPUT FILL-IN-TEL1         
      avroptemp.BMOB = INPUT FILL-IN-MOB1         
      avroptemp.EKONTAKT = INPUT FILL-IN-ENTRKONT 
      avroptemp.ETEL = INPUT FILL-IN-TEL2         
      avroptemp.EMOB = INPUT FILL-IN-MOB2         
      avroptemp.PRIS = prisvar.  
      RUN btnok IN avropapph (INPUT aonrvar,INPUT delnrvar,
                              INPUT TABLE avroptemp).                                                     
   END.
   APPLY "CLOSE":U TO THIS-PROCEDURE.
   {muswait.i}      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_SKRIV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_SKRIV C-Win
ON CHOOSE OF BTN_SKRIV IN FRAME DEFAULT-FRAME /* Skriv ut */
DO: 
   RUN SKRIVVAL.W (INPUT FALSE).       
   {musarrow.i}  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_SKRIV C-Win
ON MOUSE-MENU-CLICK OF BTN_SKRIV IN FRAME DEFAULT-FRAME /* Skriv ut */
DO:
   RUN SIDLANGD.W.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_VISA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_VISA C-Win
ON CHOOSE OF BTN_VISA IN FRAME DEFAULT-FRAME /* Visa */
DO:
   {muswait.i} 
   EMPTY TEMP-TABLE vis_temp NO-ERROR.    
   IF INPUT TOG_FAST = TRUE THEN prisvar = 1.
   ELSE IF TOG_TIM = TRUE THEN prisvar = 2.
   ELSE IF TOG_P2 = TRUE THEN prisvar = 3.
   ELSE DO:
      MESSAGE "Ni har inte gjort något val under Ekonomi."
      VIEW-AS ALERT-BOX TITLE "Meddelande".
      RETURN NO-APPLY.
   END.
   CREATE vis_temp.
   ASSIGN
   vis_temp.AONR = uppaonrtemp.AONR
   vis_temp.DELNR = uppaonrtemp.DELNR
   vis_temp.BENAMNING = uppaonrtemp.ORT
   vis_temp.DATUM = INPUT FILL-IN-DATUM
   vis_temp.KLAR = INPUT FILL-IN-DATUM2
   vis_temp.LEVNAMN = INPUT CMB_LEV
   vis_temp.BKONTAKT = INPUT FILL-IN-BESTKONT
   vis_temp.BTEL = INPUT FILL-IN-TEL1
   vis_temp.BMOB = INPUT FILL-IN-MOB1
   vis_temp.EKONTAKT = INPUT FILL-IN-ENTRKONT
   vis_temp.ETEL = INPUT FILL-IN-TEL2
   vis_temp.EMOB = INPUT FILL-IN-MOB2
   vis_temp.PRIS = prisvar.
   IF musz = TRUE THEN musz = FALSE.   
   ASSIGN    
   skrivut = FALSE.   
   {AVBGOM.I}
   RUN AVROPEXC.P.
   {AVBFRAM.I} 
   {musarrow.i}  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_LEV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_LEV C-Win
ON VALUE-CHANGED OF CMB_LEV IN FRAME DEFAULT-FRAME /* Entreprenör */
DO:
   {muswait.i}       
   ASSIGN                          
   CMB_LEV = INPUT CMB_LEV
   FILL-IN-ENTR = CMB_LEV.
   DISPLAY FILL-IN-ENTR WITH FRAME {&FRAME-NAME}.   
   {musarrow.i}     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-DATUM
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-DATUM C-Win
ON MOUSE-MENU-CLICK OF FILL-IN-DATUM IN FRAME DEFAULT-FRAME
DO:
   ASSIGN  
   Guru.GlobalaVariabler:regdatum = TODAY.
   RUN AlmanBtn.w.
   FILL-IN-DATUM = Guru.GlobalaVariabler:regdatum.
   DISPLAY FILL-IN-DATUM WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-DATUM2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-DATUM2 C-Win
ON MOUSE-MENU-CLICK OF FILL-IN-DATUM2 IN FRAME DEFAULT-FRAME
DO:
   ASSIGN  
   Guru.GlobalaVariabler:regdatum = TODAY.
   RUN AlmanBtn.w.
   FILL-IN-DATUM2 = Guru.GlobalaVariabler:regdatum.
   DISPLAY FILL-IN-DATUM2 WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_FAST
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_FAST C-Win
ON VALUE-CHANGED OF TOG_FAST IN FRAME DEFAULT-FRAME /* Fastpris */
DO:
   TOG_FAST = INPUT TOG_FAST.
   IF TOG_FAST = TRUE THEN DO:
      ASSIGN
      TOG_TIM = FALSE
      TOG_P2 = FALSE.
      DISPLAY TOG_TIM TOG_P2 WITH FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_P2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_P2 C-Win
ON VALUE-CHANGED OF TOG_P2 IN FRAME DEFAULT-FRAME /* P2 */
DO:
   TOG_P2 = INPUT TOG_P2.
   IF TOG_P2 = TRUE THEN DO:
      ASSIGN
      TOG_TIM = FALSE
      TOG_FAST = FALSE.
      DISPLAY TOG_TIM TOG_FAST WITH FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_TIM
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_TIM C-Win
ON VALUE-CHANGED OF TOG_TIM IN FRAME DEFAULT-FRAME /* Timpris */
DO:
   TOG_TIM = INPUT TOG_TIM.
   IF TOG_TIM = TRUE THEN DO:
      ASSIGN
      TOG_P2 = FALSE
      TOG_FAST = FALSE.
      DISPLAY TOG_P2 TOG_FAST WITH FRAME {&FRAME-NAME}.
   END. 
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
   IF VALID-HANDLE(avropapph) THEN DELETE PROCEDURE avropapph.
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
   RUN ladda IN avropapph (OUTPUT TABLE avroptemp,OUTPUT TABLE levtemp,
                           OUTPUT TABLE anvandartemp,OUTPUT TABLE personaltemp).
   FILL-IN-DELNR:LABEL = Guru.Konstanter:gdelnrk.
   status-ok = CMB_LEV:DELETE("0").       
   FOR EACH levtemp WHERE  
   levtemp.LEVKOD NE "0" AND levtemp.LEVKOD NE "99" 
   AND levtemp.BORTTAG = FALSE USE-INDEX LEV NO-LOCK:      
      ASSIGN
      status-ok = CMB_LEV:ADD-LAST(levtemp.LEVNAMN)IN FRAME {&FRAME-NAME}
      CMB_LEV:SCREEN-VALUE = levtemp.LEVNAMN.
   END.             
   ASSIGN
   CMB_LEV = INPUT CMB_LEV
   TOG_FAST = FALSE
   TOG_TIM = FALSE
   TOG_P2 = FALSE.
   FIND FIRST uppaonrtemp WHERE uppaonrtemp.AONR = aonrvar AND 
   uppaonrtemp.DELNR = delnrvar NO-LOCK NO-ERROR.
   FIND FIRST avroptemp WHERE avroptemp.AONR = aonrvar AND avroptemp.DELNR = delnrvar USE-INDEX AONR NO-LOCK NO-ERROR.
   IF AVAILABLE avroptemp THEN DO:
      FIND FIRST levtemp WHERE levtemp.LEVKOD = avroptemp.LEVKOD NO-LOCK NO-ERROR.
      ASSIGN
      CMB_LEV:SCREEN-VALUE = levtemp.LEVNAMN
      CMB_LEV = INPUT CMB_LEV
      FILL-IN-ENTR = levtemp.LEVNAMN      
      FILL-IN-DATUM = avroptemp.DATUM
      FILL-IN-DATUM2 = avroptemp.KLAR
      FILL-IN-BESTKONT = avroptemp.BKONTAKT
      FILL-IN-TEL1 = avroptemp.BTEL
      FILL-IN-MOB1 = avroptemp.BMOBIL
      FILL-IN-ENTRKONT = avroptemp.EKONTAKT
      FILL-IN-TEL2 = avroptemp.ETELE
      FILL-IN-MOB2 = avroptemp.EMOBIL.
      IF avroptemp.PRIS = 1 THEN TOG_FAST = TRUE.
      ELSE IF avroptemp.PRIS = 2 THEN TOG_TIM = TRUE.
      ELSE TOG_P2 = TRUE.
   END.
   ELSE DO:  
      FIND FIRST anvandartemp WHERE anvandartemp.ANVANDARE = Guru.Konstanter:globanv
      NO-LOCK NO-ERROR.
      FIND FIRST personaltemp WHERE personaltemp.PERSONALKOD = anvandartemp.PERSONALKOD
      USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.    
      IF AVAILABLE personaltemp THEN DO:
         ASSIGN
         FILL-IN-BESTKONT = personaltemp.FORNAMN + " " + personaltemp.EFTERNAMN
         FILL-IN-TEL1 = personaltemp.TELEFON
         FILL-IN-MOB1 = personaltemp.MOBILTEL.
      END.
      ASSIGN
      /*FAST DATUM SOM KAN KOMMA ATT ÄNDRAS*/   
      FILL-IN-DATUM = 06/01/00
      TOG_FAST = TRUE      
      FILL-IN-DATUM2 = TODAY
      FILL-IN-ENTR = CMB_LEV.
   END.
   ASSIGN
   FILL-IN-AONR = uppaonrtemp.AONR
   FILL-IN-DELNR = uppaonrtemp.DELNR
   FILL-IN-AONR:LABEL = Guru.Konstanter:gaok.
   &Scoped-define FORMATNAMN FILL-IN-AONR   
   {AOFORMAT3.I}
   &Scoped-define FORMATNAMN FILL-IN-DELNR   
   {DELNRFORMAT.I}
   RUN enable_UI.   
   {FRMSIZE.I}  
   BTN_SKRIV:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
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
  Purpose: Changing screen-value for combo-box CMB_OMR     
  Parameters:  Input = Screen-value for CMB_FOR
  Notes:       
-------------------------------------------------------------*/    
   IF Guru.Konstanter:appcon THEN DO:
      RUN AVROPAPP.P PERSISTENT SET avropapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN AVROPAPP.P PERSISTENT SET avropapph.
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
  DISPLAY CMB_LEV TOG_FAST TOG_TIM TOG_P2 FILL-IN-BESTKONT FILL-IN-TEL1 
          FILL-IN-MOB1 FILL-IN-ENTRKONT FILL-IN-TEL2 FILL-IN-MOB2 FILL-IN-DATUM2 
          FILL-IN-AONR FILL-IN-DELNR FILL-IN-DATUM FILL-IN-ENTR 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-57 RECT-58 RECT-59 RECT-60 RECT-61 RECT-62 CMB_LEV BTN_VISA 
         BTN_SKRIV TOG_FAST TOG_TIM TOG_P2 FILL-IN-BESTKONT FILL-IN-TEL1 
         FILL-IN-MOB1 FILL-IN-ENTRKONT FILL-IN-TEL2 FILL-IN-MOB2 FILL-IN-DATUM2 
         BTN_OK BTN_AVB FILL-IN-AONR FILL-IN-DELNR FILL-IN-DATUM FILL-IN-ENTR 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

