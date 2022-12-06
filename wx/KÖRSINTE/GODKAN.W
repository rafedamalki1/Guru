&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
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

/* Local Variable Definitions ---                                       */
{ALLDEF.I}
&Scoped-define NEW
&Scoped-define SHARED SHARED
{TIDPERS.I}
{PHMT.I}
{GODTEMP.I}
/*{EGENBEN.I}*/
{GLOBVAR2DEL1.I}
{REGVAR.I}
{SOKDEF.I}
{OMRTEMPW.I}
DEFINE SHARED VARIABLE vaxla AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE vart AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE SHARED VARIABLE tidtabrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE tidtabrec2 AS RECID NO-UNDO.
DEFINE SHARED VARIABLE persrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE persrec2 AS RECID NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE antal_valda AS INTEGER NO-UNDO.
DEFINE VARIABLE antal_raknare AS INTEGER NO-UNDO.
/*FÖR TIDSEDEL*/
DEFINE NEW SHARED VARIABLE RAD_TIDSVAL AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Alla tidsedlar för perioden", 1,
"Godkända", 2,
"Ej godkända", 3
     SIZE 31.5 BY 2
     BGCOLOR 8  NO-UNDO.       
DEFINE NEW SHARED VARIABLE TOGGLE-MONTH AS LOGICAL INITIAL ? 
     LABEL "Två tidsedlar vid månadasskifte":L 
     VIEW-AS TOGGLE-BOX
     SIZE 34.5 BY 1 NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BRW_TID-2

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES godmarkpers

/* Definitions for BROWSE BRW_TID-2                                     */
&Scoped-define FIELDS-IN-QUERY-BRW_TID-2 godmarkpers.PERSONALKOD ~
godmarkpers.FORNAMN godmarkpers.EFTERNAMN godmarkpers.DATUM 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_TID-2 godmarkpers.PERSONALKOD 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_TID-2 godmarkpers
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_TID-2 godmarkpers
&Scoped-define QUERY-STRING-BRW_TID-2 FOR EACH godmarkpers NO-LOCK
&Scoped-define OPEN-QUERY-BRW_TID-2 OPEN QUERY BRW_TID-2 FOR EACH godmarkpers NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_TID-2 godmarkpers
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_TID-2 godmarkpers


/* Definitions for FRAME DEFAULT-FRAME                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS FBTN_VISA BTN_GODA BTN_GODM BTN_AVB 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-REGIS 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB AUTO-END-KEY 
     LABEL "Avsluta":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_GODA 
     LABEL "Alla i bilden":L 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_GODM 
     LABEL "Markerade i bilden":L 
     SIZE 12 BY 1.

DEFINE BUTTON FBTN_VISA 
     LABEL "Visa tidsedel":L 
     SIZE 14 BY 1.

DEFINE VARIABLE FILL-IN-AR AS INTEGER FORMAT "9999":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 5.5 BY .88 NO-UNDO.

DEFINE VARIABLE FILL-IN-MANAD AS CHARACTER FORMAT "X(15)":U INITIAL "0" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .88 NO-UNDO.

DEFINE VARIABLE FILL-IN-REGIS AS CHARACTER FORMAT "X(17)":U 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN_EFTERNAMN AS CHARACTER FORMAT "X(13)" 
     VIEW-AS FILL-IN 
     SIZE 13.5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN_FORNAMN AS CHARACTER FORMAT "X(14)" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN_NAMN AS CHARACTER FORMAT "X(16)" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE RAD_ALLVAL AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Ansvarig tidredovisare", 1,
"Godkänner tidsedel", 6,
"Område", 2,
"Alla", 3,
"Enhet/Sign", 4,
"Markerade", 5
     SIZE 76.5 BY 1.17 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_TID-2 FOR 
      godmarkpers SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_TID-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_TID-2 C-Win _STRUCTURED
  QUERY BRW_TID-2 NO-LOCK DISPLAY
      godmarkpers.PERSONALKOD COLUMN-LABEL "Enhet/Sign" FORMAT "X(5)":U
      godmarkpers.FORNAMN COLUMN-LABEL "Förnamn" FORMAT "X(15)":U
      godmarkpers.EFTERNAMN COLUMN-LABEL "Efternamn" FORMAT "X(25)":U
      godmarkpers.DATUM COLUMN-LABEL "Godkänd till" FORMAT "99/99/99":U
  ENABLE
      godmarkpers.PERSONALKOD
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS MULTIPLE SIZE 67 BY 17.5.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     FILL-IN-REGIS AT ROW 3.08 COL 4.5 COLON-ALIGNED NO-LABEL
     FILL-IN_NAMN AT ROW 3.08 COL 24 COLON-ALIGNED HELP
          "ANGE ORAGNISTIONENSBENÄMNING" NO-LABEL
     FILL-IN_FORNAMN AT ROW 3.08 COL 24 COLON-ALIGNED NO-LABEL
     FILL-IN_EFTERNAMN AT ROW 3.08 COL 45.5 COLON-ALIGNED NO-LABEL
     FILL-IN-AR AT ROW 4.71 COL 33.25 COLON-ALIGNED NO-LABEL
     FILL-IN-MANAD AT ROW 4.71 COL 42.5 COLON-ALIGNED NO-LABEL
     BRW_TID-2 AT ROW 6.46 COL 1.5
     FBTN_VISA AT ROW 8 COL 71
     RAD_ALLVAL AT ROW 12.96 COL 1 NO-LABEL
     BTN_GODA AT ROW 25.67 COL 11.5
     BTN_GODM AT ROW 25.67 COL 38.38
     BTN_AVB AT ROW 25.67 COL 71
     "Godkänna tidsedlar för:" VIEW-AS TEXT
          SIZE 43.88 BY 1.33 AT ROW 1.5 COL 1.5
          FONT 17
     "Godkänna:" VIEW-AS TEXT
          SIZE 15 BY .67 AT ROW 24.46 COL 1.5
     "Det finns inga tidsedlar att godkänna" VIEW-AS TEXT
          SIZE 57 BY 3.5 AT ROW 9.71 COL 7
          FONT 17
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 84.88 BY 26.08.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Temp-Tables and Buffers:
      TABLE: ? T "?" NO-UNDO temp-db godmarkpers
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Godkänna tidsedlar"
         HEIGHT             = 25.88
         WIDTH              = 84.5
         MAX-HEIGHT         = 28.42
         MAX-WIDTH          = 90.25
         VIRTUAL-HEIGHT     = 28.42
         VIRTUAL-WIDTH      = 90.25
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
/* BROWSE-TAB BRW_TID-2 FILL-IN-MANAD DEFAULT-FRAME */
/* SETTINGS FOR BROWSE BRW_TID-2 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       BRW_TID-2:HIDDEN  IN FRAME DEFAULT-FRAME                = TRUE
       BRW_TID-2:MAX-DATA-GUESS IN FRAME DEFAULT-FRAME         = 300.

/* SETTINGS FOR FILL-IN FILL-IN-AR IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN-AR:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-MANAD IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN-MANAD:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-REGIS IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN_EFTERNAMN IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN_EFTERNAMN:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_FORNAMN IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN_FORNAMN:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_NAMN IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN_NAMN:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR RADIO-SET RAD_ALLVAL IN FRAME DEFAULT-FRAME
   NO-DISPLAY SHARED NO-ENABLE                                          */
ASSIGN 
       RAD_ALLVAL:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_TID-2
/* Query rebuild information for BROWSE BRW_TID-2
     _TblList          = "Temp-Tables.godmarkpers"
     _Options          = "NO-LOCK "
     _FldNameList[1]   > Temp-Tables.godmarkpers.PERSONALKOD
"godmarkpers.PERSONALKOD" "Enhet/Sign" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.godmarkpers.FORNAMN
"godmarkpers.FORNAMN" "Förnamn" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.godmarkpers.EFTERNAMN
"godmarkpers.EFTERNAMN" "Efternamn" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[4]   > Temp-Tables.godmarkpers.DATUM
"godmarkpers.DATUM" "Godkänd till" ? "date" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_TID-2 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Godkänna tidsedlar */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Godkänna tidsedlar */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB C-Win
ON CHOOSE OF BTN_AVB IN FRAME DEFAULT-FRAME /* Avsluta */
DO:   
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_GODA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_GODA C-Win
ON CHOOSE OF BTN_GODA IN FRAME DEFAULT-FRAME /* Alla i bilden */
DO:
   {muswait.i}
   {SOKSTART.I}
   ASSIGN
   soktemp.SOKVAL = 16.
   {SOKANROP.I}
   IF soktemp.SOKLOG[1] = TRUE THEN DO:
      MESSAGE "Det går inte att godkänna tidsedlar nu." Skip
      "För det pågår en ekonomi- och lönesammanställning."
      VIEW-AS ALERT-BOX.
      status-mus2 = SESSION:SET-WAIT-STATE("").
      RETURN NO-APPLY.   
   END.
   MESSAGE "Vill du verkligen godkänna alla i bilden ?" 
   VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE val1 AS LOGICAL.
   CASE val1:
      WHEN TRUE THEN DO:
         musz = musz.          
      END.
      WHEN FALSE THEN DO:
         RETURN NO-APPLY.
      END.
   END CASE.   
   IF NOT AVAILABLE godmarkpers THEN FIND FIRST godmarkpers USE-INDEX PERSONALKOD NO-LOCK NO-ERROR. 
   ASSIGN 
   tidtabrec = RECID(godmarkpers).  
   regdatum = TODAY.
   RUN REGVEC.P.
   IF MONTH(regdatum) = regmnr AND YEAR(regdatum) = regar THEN DO:
      regdatum = TODAY - 1.
      {AVBGOMD.I}
      RUN GODDATUM.W.
      {AVBFRAMD.I}       
   END.
   ELSE DO:         
      IF regmnr = 12 THEN regdatum = DATE(12,31,regar). 
      ELSE regdatum = DATE((regmnr + 1),01,regar) - 1.
      {AVBGOMD.I}
      RUN GODDATUM.W.
      {AVBFRAMD.I}               
   END.
   IF musz = TRUE THEN DO:
      musz = FALSE.
   END. 
   ELSE DO:   
      EMPTY TEMP-TABLE appmarkpers NO-ERROR. 
      FOR EACH godmarkpers USE-INDEX PERSONALKOD:                                      
         IF godmarkpers.DATUM <= regdatum OR godmarkpers.DATUM = ? THEN DO:
            CREATE appmarkpers.
            ASSIGN appmarkpers.PERSONALKOD = godmarkpers.PERSONALKOD
            appmarkpers.DATUM = regdatum
            appmarkpers.VECKONUMMER = godmarkpers.VECKONUMMER.
         END.
         ASSIGN 
         godmarkpers.DATUM = regdatum
         godmarkpers.GODKAND = "G" + STRING(regvnr, "999").
      END.      
      {GODKANIN.I}                              
      EMPTY TEMP-TABLE appmarkpers NO-ERROR. 
      RUN refreshbrw_UI IN brwproc[1].
   END.            
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_GODM
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_GODM C-Win
ON CHOOSE OF BTN_GODM IN FRAME DEFAULT-FRAME /* Markerade i bilden */
DO:   
   {muswait.i}
   {SOKSTART.I}
   ASSIGN
   soktemp.SOKVAL = 16.
   {SOKANROP.I}
   IF soktemp.SOKLOG[1] = TRUE THEN DO:
      MESSAGE "Det går inte att godkänna tidsedlar nu." Skip
      "För det pågår en ekonomi- och lönesammanställning."
      VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.      
   END.
   RUN godmt2_UI.
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_VISA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_VISA C-Win
ON CHOOSE OF FBTN_VISA IN FRAME DEFAULT-FRAME /* Visa tidsedel */
DO:
   {muswait.i}
   ASSIGN    
   persrec = godmarkpers.TIDPERSREC
   vaxla = TRUE
   RAD_TIDSVAL = 1.                
   ASSIGN
   bdatum = DATE(godmarkpers.MANADNR,01,regar).      
   IF godmarkpers.MANADNR = 12 THEN avdatum = DATE(12,31,godmarkpers.AR).
   ELSE avdatum = DATE(godmarkpers.MANADNR + 1,01,godmarkpers.AR) - 1.         
   {AVBGOM.I}
   {AMERICANEUROPEAN.I}
   RUN TIDSEDL.W.
   {EUROPEANAMERICAN.I}
   {AVBFRAM.I}   
   vaxla = FALSE.      
   {musarrow.i}   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_VISA C-Win
ON GO OF FBTN_VISA IN FRAME DEFAULT-FRAME /* Visa tidsedel */
DO:
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_TID-2
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE DO:
   {BORTBRWPROC.I}
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
   EMPTY TEMP-TABLE godmarkpers NO-ERROR. 
   FOR EACH tidpers USE-INDEX PERSONALKOD:
      CREATE godmarkpers.
      ASSIGN
      godmarkpers.EFTERNAMN = tidpers.EFTERNAMN
      godmarkpers.FORNAMN = tidpers.FORNAMN
      godmarkpers.PERSONALKOD = tidpers.PERSONALKOD
      godmarkpers.GODKAND = "SAKNAR TID"    
      godmarkpers.TIDPERSREC = tidpers.TIDPERSREC
      godmarkpers.VECKONUMMER = regvnr
      godmarkpers.AR = regar
      godmarkpers.MANADNR = regmnr.
   END.      
   IF Guru.Konstanter:appcon THEN DO:                           
      RUN GODAPP.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT 4,INPUT Guru.Konstanter:globanv,INPUT "",
       INPUT-OUTPUT regar,INPUT-OUTPUT regmnr,INPUT-OUTPUT regdatum,
       INPUT TABLE tidpers, INPUT-OUTPUT TABLE godmarkpers).
   END.
   ELSE DO:
      RUN GODAPP.P 
      (INPUT 4,INPUT Guru.Konstanter:globanv,INPUT "",
       INPUT-OUTPUT regar,INPUT-OUTPUT regmnr,INPUT-OUTPUT regdatum,
       INPUT TABLE tidpers, INPUT-OUTPUT TABLE godmarkpers).
   END.
   FIND FIRST tidpers USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.
   IF NOT AVAILABLE tidpers THEN LEAVE MAIN-BLOCK.
   FIND FIRST personaltemp WHERE personaltemp.PERSONALKOD = tidpers.PERSONALKOD
   NO-LOCK NO-ERROR.
   IF RAD_ALLVAL = 1 THEN DO:
      ASSIGN
      FILL-IN-REGIS = "Ansvarig:".
      FIND FIRST ansvarigtemp WHERE ansvarigtemp.PERSONALKOD = SUBSTRING(tidpers.ANSVARIGTIDR,1,5) NO-LOCK NO-ERROR.
      ASSIGN
      FILL-IN_EFTERNAMN = ansvarigtemp.EFTERNAMN
      FILL-IN_FORNAMN = ansvarigtemp.FORNAMN
      FILL-IN_EFTERNAMN:HIDDEN = FALSE
      FILL-IN_FORNAMN:HIDDEN = FALSE.
      DISPLAY FILL-IN_EFTERNAMN FILL-IN_FORNAMN WITH FRAME {&FRAME-NAME}.
   END.
   IF RAD_ALLVAL = 2 THEN DO:
      FILL-IN-REGIS = Guru.Konstanter:gomrk + ":".
      FIND FIRST omrtemp WHERE omrtemp.OMRADE = tidpers.OMRADE USE-INDEX OMR NO-LOCK NO-ERROR.
      ASSIGN
      FILL-IN_NAMN:HIDDEN = FALSE
      FILL-IN_NAMN = omrtemp.NAMN.
      DISPLAY FILL-IN_NAMN WITH FRAME {&FRAME-NAME}.
   END.
   IF RAD_ALLVAL = 3 THEN DO:
      FILL-IN-REGIS = "Alla".
   END.
    IF RAD_ALLVAL = 4 THEN DO:
      FILL-IN-REGIS = "".
   END.
   IF RAD_ALLVAL = 5 THEN FILL-IN-REGIS = "Markerade enheter".
   IF RAD_ALLVAL = 6 THEN DO:
      FILL-IN-REGIS = "Godkänner tidsedel:".
      FIND FIRST godkannartemp WHERE godkannartemp.PERSONALKOD = personaltemp.TIDSGODK NO-LOCK NO-ERROR.
      ASSIGN
      FILL-IN_EFTERNAMN = godkannartemp.EFTERNAMN
      FILL-IN_FORNAMN = godkannartemp.FORNAMN
      FILL-IN_EFTERNAMN:HIDDEN = FALSE
      FILL-IN_FORNAMN:HIDDEN = FALSE.
      DISPLAY FILL-IN_EFTERNAMN FILL-IN_FORNAMN WITH FRAME {&FRAME-NAME}.
   END.
   ASSIGN
   FILL-IN-AR = regar 
   FILL-IN-MANAD = regmannamn.    
   RUN enable_UI.   
   {FRMSIZE.I}  
   FIND FIRST godmarkpers USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.
   IF NOT AVAILABLE godmarkpers THEN DO:
      ASSIGN
      BRW_TID-2:HIDDEN = TRUE.
      DISABLE BTN_GODA BTN_GODM WITH FRAME {&FRAME-NAME}.
   END.
   ELSE DO:    
      RUN openbdyn_UI IN brwproc[1] (INPUT "").
      ENABLE BRW_TID-2 WITH FRAME {&FRAME-NAME}.
      BRW_TID-2:HIDDEN = FALSE.        
   END.   
   DISPLAY FILL-IN-AR FILL-IN-MANAD WITH FRAME {&FRAME-NAME}.    
   status-ok = RAD_ALLVAL:DELETE("Ansvarig tidredovisare").
   status-ok = RAD_ALLVAL:DELETE("Godkänner tidsedel").
   status-ok = RAD_ALLVAL:DELETE("Område").
   status-ok = RAD_ALLVAL:DELETE("Alla").
   status-ok = RAD_ALLVAL:DELETE("Enhet/Sign").
   status-ok = RAD_ALLVAL:DELETE("Markerade").
   RAD_ALLVAL:ADD-LAST("Ansvarig tidredovisare", 1).
   RAD_ALLVAL:ADD-LAST("Godkänner tidsedel", 2).
   RAD_ALLVAL:ADD-LAST(Guru.Konstanter:gomrk, 3).
   RAD_ALLVAL:ADD-LAST("Alla", 4).
   RAD_ALLVAL:ADD-LAST("Enhet/Sign", 5).
   RAD_ALLVAL:ADD-LAST("Markerade", 6).   
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
   godmarkpers.PERSONALKOD:READ-ONLY IN BROWSE BRW_TID-2 = TRUE.
   RUN DYNBRW.P PERSISTENT SET brwproc[1]
      (INPUT BRW_TID-2:HANDLE IN FRAME {&FRAME-NAME}).    
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
  DISPLAY FILL-IN-REGIS 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE FBTN_VISA BTN_GODA BTN_GODM BTN_AVB 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE godmt2_UI C-Win 
PROCEDURE godmt2_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   {muswait.i}    
   antal_valda = BRW_TID-2:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME}.
   IF antal_valda = 0 THEN DO:
      MESSAGE "Du måste välja någon." VIEW-AS ALERT-BOX.
   END.
   ELSE DO:        
      ASSIGN
      antal_raknare = 1
      regdatum = TODAY.      
      RUN REGVEC.P.  
        
      IF MONTH(regdatum) = regmnr AND YEAR(regdatum) = regar THEN DO:
         regdatum = TODAY - 1.
         {AVBGOMD.I}
         RUN GODDATUM.W.
         {AVBFRAMD.I}
         IF musz = TRUE THEN DO:
            musz = FALSE.
            RETURN.
         END.
      END. 
      ELSE DO:         
         IF regmnr = 12 THEN regdatum = DATE(12,31,regar). 
         ELSE regdatum = DATE((regmnr + 1),01,regar) - 1.
         {AVBGOMD.I}
         RUN GODDATUM.W.
         {AVBFRAMD.I}         
         IF musz = TRUE THEN DO:
            musz = FALSE.
            RETURN.
         END.         
      END.
      EMPTY TEMP-TABLE appmarkpers NO-ERROR. 
      
      DO WHILE antal_raknare LE antal_valda :
         status-ok = BRW_TID-2:FETCH-SELECTED-ROW(antal_raknare).
         tidtabrec = RECID(godmarkpers).
         IF godmarkpers.DATUM <= regdatum OR godmarkpers.DATUM = ? THEN DO:
            CREATE appmarkpers.
            ASSIGN appmarkpers.PERSONALKOD = godmarkpers.PERSONALKOD
            appmarkpers.DATUM = regdatum
            appmarkpers.VECKONUMMER = godmarkpers.VECKONUMMER.
         END.
         ASSIGN 
         godmarkpers.DATUM = regdatum
         godmarkpers.GODKAND = "G" + STRING(regvnr, "999").
         antal_raknare = antal_raknare + 1. 
      END.
      {GODKANIN.I}                              
      EMPTY TEMP-TABLE appmarkpers NO-ERROR. 
      RUN refreshbrw_UI IN brwproc[1].
   END.
   {musarrow.i}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

