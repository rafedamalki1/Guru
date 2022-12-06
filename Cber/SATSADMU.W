&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
&Scoped-define WINDOW-NAME WINDOW-2


/* Temp-Table and Buffer definitions                                    */



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS WINDOW-2 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 06/12/97 -  3:16 pm

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
{GLOBVAR2DEL1.I}

&Scoped-define NEW NEW
&Scoped-define SHARED SHARED
{HUVLEVTEMP.I}
{LEVTEMP.I}
{MTRLTEMP.I}
{KONSTRMTRL.I}
{BBENAMNTEMP.I}
{DEFSOK.I} 
{SOKDEF.I}
DEFINE NEW SHARED VARIABLE ny AS LOGICAL NO-UNDO. 
DEFINE NEW SHARED VARIABLE skap_row AS ROWID NO-UNDO.
DEFINE NEW SHARED VARIABLE skapben AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE skapkod AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE skapenr AS CHARACTER NO-UNDO.       
DEFINE NEW SHARED VARIABLE skapenh AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE skappris AS DECIMAL NO-UNDO.
DEFINE SHARED VARIABLE vald_kundlev AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE vald_lev AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.  
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO. 
DEFINE SHARED VARIABLE vartpro AS CHARACTER FORMAT "X(3)" NO-UNDO.  
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE antal_valda AS INTEGER NO-UNDO.
DEFINE VARIABLE antal_raknare AS INTEGER NO-UNDO. 
DEFINE NEW SHARED VARIABLE firstkod AS CHARACTER NO-UNDO. 
DEFINE VARIABLE aosok AS CHARACTER FORMAT "X(40)" NO-UNDO.
DEFINE VARIABLE posok AS CHARACTER FORMAT "X(11)" NO-UNDO.
DEFINE VARIABLE satsadmapph AS HANDLE NO-UNDO.
DEFINE VARIABLE laddaproch AS HANDLE NO-UNDO.
DEFINE VARIABLE felmedd AS CHARACTER NO-UNDO.
DEFINE VARIABLE val AS LOGICAL NO-UNDO.
DEFINE VARIABLE satsvar AS LOGICAL NO-UNDO.
DEFINE VARIABLE svar AS LOGICAL.
DEFINE VARIABLE mtrlhmtapph AS HANDLE NO-UNDO.
DEFINE NEW SHARED TEMP-TABLE sats_mtrl
   FIELD KOD AS CHARACTER
   FIELD ENR AS CHARACTER 
   FIELD BENAMNING AS CHARACTER LABEL "Benämning"
   FIELD ENHET AS CHARACTER LABEL "Enhet" 
   FIELD ANTAL AS INTEGER LABEL "Antal"
   FIELD PRIS AS DECIMAL LABEL "Pris"
   FIELD LEVKOD AS CHARACTER.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE WINDOW
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-B
&Scoped-define BROWSE-NAME BRW_MTRL

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES satstemp

/* Definitions for BROWSE BRW_MTRL                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_MTRL satstemp.KOD satstemp.BENAMNING ~
satstemp.ENR 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_MTRL 
&Scoped-define QUERY-STRING-BRW_MTRL FOR EACH satstemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_MTRL OPEN QUERY BRW_MTRL FOR EACH satstemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_MTRL satstemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_MTRL satstemp


/* Definitions for FRAME FRAME-B                                        */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-9 IMAGE-6 CMB_LEV BRW_MTRL FBTN_VISA ~
FBTN_SKRIV BTN_NY BTN_ANDRA BTN_BORT FILL-IN-KOD FILL-IN-BEN RAD_SOK ~
BTN_AVB FILL-IN-SOKALT 
&Scoped-Define DISPLAYED-OBJECTS CMB_LEV FILL-IN-KOD FILL-IN-BEN RAD_SOK ~
FILL-IN-SOKALT 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR WINDOW-2 AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_ANDRA 
     LABEL "Ändra" 
     SIZE 12 BY 1
     FGCOLOR 1 .

DEFINE BUTTON BTN_AVB AUTO-END-KEY 
     LABEL "Avsluta":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_BORT 
     LABEL "Ta bort" 
     SIZE 12 BY 1
     FGCOLOR 1 .

DEFINE BUTTON BTN_NY 
     LABEL "Ny" 
     SIZE 12 BY 1
     FGCOLOR 1 .

DEFINE BUTTON FBTN_SKRIV 
     LABEL "Skriv ut":L 
     SIZE 14 BY 1.

DEFINE BUTTON FBTN_VISA 
     LABEL "Visa":L 
     SIZE 14 BY 1.

DEFINE VARIABLE CMB_LEV AS CHARACTER FORMAT "X(20)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "0" 
     DROP-DOWN-LIST
     SIZE 22 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-BEN AS CHARACTER FORMAT "X(30)":U 
     LABEL "Benämning" 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-KOD AS CHARACTER FORMAT "X(11)":U 
     LABEL "Kod" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-SOKALT AS CHARACTER FORMAT "X(256)":U INITIAL "Sökalternativ:" 
      VIEW-AS TEXT 
     SIZE 14 BY .83 NO-UNDO.

DEFINE IMAGE IMAGE-6
     FILENAME "BILDER\sokpa.gif":U CONVERT-3D-COLORS
     SIZE 8 BY .83.

DEFINE VARIABLE RAD_SOK AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Början", 1,
"Någonstans", 2,
"Slutet", 3
     SIZE 38 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 54.5 BY 4.25
     BGCOLOR 8 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_MTRL FOR 
      satstemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_MTRL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_MTRL WINDOW-2 _STRUCTURED
  QUERY BRW_MTRL NO-LOCK DISPLAY
      satstemp.KOD FORMAT "X(11)":U
      satstemp.BENAMNING FORMAT "x(256)":U WIDTH 35
      satstemp.ENR FORMAT "X(11)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SIZE 54.5 BY 20
         TITLE "Upplagda satser".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-B
     CMB_LEV AT ROW 1.92 COL 1.5 NO-LABEL
     BRW_MTRL AT ROW 3 COL 1.5
     FBTN_VISA AT ROW 8 COL 57.38
     FBTN_SKRIV AT ROW 9.08 COL 57.38
     BTN_NY AT ROW 23.5 COL 7.25
     BTN_ANDRA AT ROW 23.5 COL 22.25
     BTN_BORT AT ROW 23.5 COL 37.25
     FILL-IN-KOD AT ROW 25.04 COL 26.88 COLON-ALIGNED
     FILL-IN-BEN AT ROW 26.29 COL 26.88 COLON-ALIGNED
     RAD_SOK AT ROW 27.71 COL 17.38 NO-LABEL
     BTN_AVB AT ROW 28.08 COL 57.38
     FILL-IN-SOKALT AT ROW 27.88 COL 2.63 NO-LABEL
     RECT-9 AT ROW 24.83 COL 1.5
     IMAGE-6 AT ROW 25.04 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 71.5 BY 28.42.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: WINDOW
   Temp-Tables and Buffers:
      TABLE: satstemp T "?" NO-UNDO temp-db satstemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW WINDOW-2 ASSIGN
         HIDDEN             = YES
         TITLE              = "Administration satser"
         HEIGHT             = 28.42
         WIDTH              = 71.63
         MAX-HEIGHT         = 28.42
         MAX-WIDTH          = 71.63
         VIRTUAL-HEIGHT     = 28.42
         VIRTUAL-WIDTH      = 71.63
         RESIZE             = yes
         SCROLL-BARS        = yes
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
/* SETTINGS FOR WINDOW WINDOW-2
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME FRAME-B
   FRAME-NAME                                                           */
/* BROWSE-TAB BRW_MTRL CMB_LEV FRAME-B */
ASSIGN 
       BRW_MTRL:MAX-DATA-GUESS IN FRAME FRAME-B         = 10000
       BRW_MTRL:ALLOW-COLUMN-SEARCHING IN FRAME FRAME-B = TRUE.

/* SETTINGS FOR COMBO-BOX CMB_LEV IN FRAME FRAME-B
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FILL-IN-SOKALT IN FRAME FRAME-B
   ALIGN-L                                                              */
ASSIGN 
       FILL-IN-SOKALT:READ-ONLY IN FRAME FRAME-B        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-2)
THEN WINDOW-2:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_MTRL
/* Query rebuild information for BROWSE BRW_MTRL
     _TblList          = "Temp-Tables.satstemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   = Temp-Tables.satstemp.KOD
     _FldNameList[2]   > Temp-Tables.satstemp.BENAMNING
"satstemp.BENAMNING" ? "x(256)" "character" ? ? ? ? ? ? no ? no no "35" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   = Temp-Tables.satstemp.ENR
     _Query            is NOT OPENED
*/  /* BROWSE BRW_MTRL */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME BTN_ANDRA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_ANDRA WINDOW-2
ON CHOOSE OF BTN_ANDRA IN FRAME FRAME-B /* Ändra */
DO:
   antal_valda = BRW_MTRL:NUM-SELECTED-ROWS.
   IF antal_valda > 0 THEN DO: 
      {muswait.i}     
      ASSIGN      
      ny = FALSE
      status-ok = BRW_MTRL:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}.
      skap_row = satstemp.SATSROW.
      {AVBGOM.I}
      RUN SATSANDU.W.
      {AVBFRAM.I} 
      IF musz = FALSE THEN DO:
         RUN satshmten_UI IN satsadmapph (INPUT skap_row,OUTPUT TABLE satstemp APPEND).
         RUN setcolsortvar_UI IN brwproc[1] (INPUT " WHERE satstemp.LEVKOD = '" + vald_lev + "' AND satstemp.SATS = TRUE").
         RUN openbdynspec_UI IN brwproc[1].                                         
         FIND FIRST satstemp WHERE satstemp.SATSROW = skap_row NO-LOCK NO-ERROR.
         IF AVAILABLE satstemp THEN DO:
            RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(satstemp)).
            RUN lastselectdyn_UI IN brwproc[1].
         END.
         APPLY "VALUE-CHANGED" TO BRW_MTRL IN FRAME {&FRAME-NAME}. 
      END.
      {musarrow.i}                                            
   END.
   ELSE DO:
      MESSAGE "Du har inte valt någon sats som ska ändras."
      VIEW-AS ALERT-BOX TITLE "Meddelande".
   END. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB WINDOW-2
ON CHOOSE OF BTN_AVB IN FRAME FRAME-B /* Avsluta */
DO:    
   RETURN.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_BORT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_BORT WINDOW-2
ON CHOOSE OF BTN_BORT IN FRAME FRAME-B /* Ta bort */
DO:
   antal_valda = BRW_MTRL:NUM-SELECTED-ROWS.
   IF antal_valda > 0 THEN DO:      
      {muswait.i} 
      status-ok = BRW_MTRL:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}. 
      ASSIGN
      skap_row = satstemp.SATSROW
      skapkod = satstemp.KOD.
      MESSAGE "Vill du ta bort - " + satstemp.KOD satstemp.BENAMNING  + "."
      VIEW-AS ALERT-BOX
      QUESTION BUTTONS YES-NO TITLE "Ta bort" UPDATE svar AS LOGICAL.         
      IF svar THEN DO: 
         RUN bortsats_UI IN satsadmapph(INPUT skap_row,INPUT skapkod,
                                        INPUT vald_lev).
         FOR EACH satstemp WHERE satstemp.KOD = skapkod AND 
         satstemp.LEVKOD = vald_lev AND satstemp.SATS = FALSE:          
            DELETE satstemp.
         END.
         FIND FIRST satstemp WHERE satstemp.SATSROW = skap_row NO-LOCK NO-ERROR.
         IF AVAILABLE satstemp THEN DO:
            DELETE satstemp.
         END.
         RUN selnextprevrow_UI IN brwproc[1].
         RUN setcolsortvar_UI IN brwproc[1] (INPUT " WHERE satstemp.LEVKOD = '" + vald_lev + "' AND satstemp.SATS = TRUE").
         RUN openbdynspec_UI IN brwproc[1].                                         
         RUN lastselectdyn_UI IN brwproc[1].   
         APPLY "VALUE-CHANGED" TO BRW_MTRL IN FRAME {&FRAME-NAME}. 
      END.        
      {musarrow.i}     
   END.
   ELSE DO:
      MESSAGE "Du har inte valt någon sats som ska tas bort."
      VIEW-AS ALERT-BOX TITLE "Meddelande".
   END. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NY
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NY WINDOW-2
ON CHOOSE OF BTN_NY IN FRAME FRAME-B /* Ny */
DO: 
   {muswait.i} 
   ny = TRUE.   
   {AVBGOM.I}       
   RUN SATSANDU.W.
   {AVBFRAM.I}
   IF musz = FALSE THEN DO:
      RUN satshmten_UI IN satsadmapph (INPUT skap_row,OUTPUT TABLE satstemp APPEND).
      RUN setcolsortvar_UI IN brwproc[1] (INPUT " WHERE satstemp.LEVKOD = '" + vald_lev + "' AND satstemp.SATS = TRUE").
      RUN openbdynspec_UI IN brwproc[1].                                         
      FIND FIRST satstemp WHERE satstemp.SATSROW = skap_row NO-LOCK NO-ERROR.
      IF AVAILABLE satstemp THEN DO:
         skapkod = satstemp.KOD.
         EMPTY TEMP-TABLE sats_mtrl NO-ERROR. 
         RUN skapsats_UI IN satsadmapph (INPUT skapkod,INPUT vald_lev,
                                      OUTPUT TABLE sats_mtrl).
      END.  
      FIND FIRST satstemp WHERE satstemp.SATSROW = skap_row NO-LOCK NO-ERROR.
      IF AVAILABLE satstemp THEN DO:
         RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(satstemp)).
         RUN lastselectdyn_UI IN brwproc[1].
      END.
      APPLY "VALUE-CHANGED" TO BRW_MTRL IN FRAME {&FRAME-NAME}. 
   END.
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_LEV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_LEV WINDOW-2
ON VALUE-CHANGED OF CMB_LEV IN FRAME FRAME-B
DO:
   {muswait.i}              
   ASSIGN
   CMB_LEV = INPUT CMB_LEV.
   FIND FIRST levtemp WHERE levtemp.LEVNAMN = CMB_LEV 
   USE-INDEX LEV NO-LOCK NO-ERROR.
   IF AVAILABLE levtemp THEN DO:
      vald_lev = levtemp.LEVKOD.
      RUN setorgtitle_UI IN brwproc[1] (INPUT "Satslista " + vald_lev).
      RUN setcolsortvar_UI IN brwproc[1] (INPUT " WHERE satstemp.LEVKOD = '" + vald_lev + "' AND satstemp.SATS = TRUE").      
      RUN openbdynspec_UI IN brwproc[1].
   END.                
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_SKRIV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_SKRIV WINDOW-2
ON CHOOSE OF FBTN_SKRIV IN FRAME FRAME-B /* Skriv ut */
DO:    
   RUN SKRIVVAL.W (INPUT FALSE).       
   IF musz = TRUE THEN musz = FALSE. 
   ELSE DO:
      {muswait.i}   
      ASSIGN 
      skrivut = TRUE
      status-ok = BRW_MTRL:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}.
      RUN skapa_UI.
      {AVBGOM.I}
      RUN VISSATSU.W.
      {AVBFRAM.I}
   END.     
   {musarrow.i}  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_SKRIV WINDOW-2
ON MOUSE-MENU-CLICK OF FBTN_SKRIV IN FRAME FRAME-B /* Skriv ut */
DO:
   RUN SIDLANGD.W.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_VISA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_VISA WINDOW-2
ON CHOOSE OF FBTN_VISA IN FRAME FRAME-B /* Visa */
DO:    
   {muswait.i}     
   IF musz = TRUE THEN musz = FALSE.   
   ASSIGN    
   skrivut = FALSE
   status-ok = BRW_MTRL:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}.  
   RUN skapa_UI.
   {AVBGOM.I}
   RUN VISSATSU.W.
   {AVBFRAM.I} 
   {musarrow.i}  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-BEN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-BEN WINDOW-2
ON ANY-KEY OF FILL-IN-BEN IN FRAME FRAME-B /* Benämning */
DO:
   {TRYCKS.I}
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN-BEN IN FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-BEN WINDOW-2
ON MOUSE-SELECT-DBLCLICK OF FILL-IN-BEN IN FRAME FRAME-B /* Benämning */
DO:
   {muswait.i}
   ASSIGN
   RAD_SOK = INPUT RAD_SOK
   FILL-IN-BEN = INPUT FILL-IN-BEN.
   IF FILL-IN-BEN = "" THEN DO:
      MESSAGE "Sökbegreppet kan inte vara blankt." VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN-BEN IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
   END.
   stjarnvar = INDEX(FILL-IN-BEN,"*",1).
   IF stjarnvar NE 0 THEN DO:
      MESSAGE "Ni behöver inte använda tecknet * vid sökningen" VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
   IF RAD_SOK = 1 THEN DO:
      ASSIGN
      aosok = FILL-IN-BEN.
      begvar = TRUE.
   END.
   ELSE IF RAD_SOK = 2 THEN DO:
      ASSIGN
      aosok = "*" + FILL-IN-BEN + "*"
      begvar = FALSE.      
   END.
   ELSE IF RAD_SOK = 3 THEN DO:
      ASSIGN
      aosok = "*" + FILL-IN-BEN.
      begvar = FALSE.
   END.   
   RUN initsok_UI (INPUT 1,INPUT aosok).
   {musarrow.i}  
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-KOD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-KOD WINDOW-2
ON ANY-KEY OF FILL-IN-KOD IN FRAME FRAME-B /* Kod */
DO:
   {TRYCKS.I}
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN-KOD IN FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-KOD WINDOW-2
ON MOUSE-SELECT-DBLCLICK OF FILL-IN-KOD IN FRAME FRAME-B /* Kod */
DO:
   {muswait.i}
   {ENRHMT3.I}
   RUN initsok_UI (INPUT 2,INPUT posok).
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_MTRL
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK WINDOW-2 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE
DO:
   {BORTBRWPROC.I}
   IF VALID-HANDLE(mtrlhmtapph) THEN DELETE PROCEDURE mtrlhmtapph.
   IF VALID-HANDLE(laddaproch) THEN DELETE PROCEDURE laddaproch.
   IF VALID-HANDLE(satsadmapph) THEN DELETE PROCEDURE satsadmapph.
   RUN disable_UI.
END.
   

/* These events will close the window and terminate the procedure.      */
/* (NOTE: this will override any user-defined triggers previously       */
/*  defined on the window.)                                             */
ON WINDOW-CLOSE OF {&WINDOW-NAME} DO:
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.
ON ENDKEY, END-ERROR OF {&WINDOW-NAME} ANYWHERE DO:
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
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
   status-ok = CMB_LEV:DELETE("0"). 
   FIND FIRST huvlevtemp WHERE huvlevtemp.DEP-NR = 999 NO-LOCK NO-ERROR.
   IF AVAILABLE huvlevtemp THEN DO:  
      vald_kundlev = huvlevtemp.LEVKOD.
   END.                              
   ELSE DO:
      FIND FIRST levtemp WHERE levtemp.LEVKOD NE "0" 
      AND levtemp.LEVKOD NE "99" AND levtemp.BORTTAG = FALSE NO-LOCK NO-ERROR.
      vald_kundlev = levtemp.LEVKOD.
   END.   
   status-ok = CMB_LEV:DELETE("0"). 
   FIND FIRST levtemp WHERE levtemp.LEVKOD = vald_kundlev
   USE-INDEX LEV NO-LOCK NO-ERROR. 
   ASSIGN
   status-ok = CMB_LEV:ADD-LAST(levtemp.LEVNAMN)IN FRAME {&FRAME-NAME}
   CMB_LEV:SCREEN-VALUE = levtemp.LEVNAMN.   
   FOR EACH levtemp WHERE levtemp.LEVKOD NE vald_kundlev AND 
   levtemp.LEVKOD NE "0" AND levtemp.LEVKOD NE "99" AND levtemp.BORTTAG = FALSE USE-INDEX LEV NO-LOCK:      
      status-ok = CMB_LEV:ADD-LAST(levtemp.LEVNAMN)IN FRAME {&FRAME-NAME}.              
   END.
   RUN enable_UI.   
   {FRMSIZE.I}    
   ASSIGN            
   vald_lev = vald_kundlev
   satsvar = TRUE.
   APPLY "VALUE-CHANGED" TO CMB_LEV IN FRAME {&FRAME-NAME}.  
   {musarrow.i}
   {WIN_M_SLUT.I}
   IF NOT THIS-PROCEDURE:PERSISTENT THEN
   WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE allstartbrw_UI WINDOW-2 
PROCEDURE allstartbrw_UI :
/* -----------------------------------------------------------
  Purpose:      
  Parameters:  
  Notes:       
-------------------------------------------------------------*/      
   RUN DYNBRW.P PERSISTENT SET brwproc[1]
      (INPUT BRW_MTRL:HANDLE IN FRAME {&FRAME-NAME}).
/*    RUN settitlenum_UI IN brwproc[1] (INPUT TRUE). */
   RUN StartSokEnrLev_UI IN brwproc[1] (INPUT THIS-PROCEDURE).
   IF Guru.Konstanter:appcon THEN DO:
      RUN SATSADMAPP.P PERSISTENT SET satsadmapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN SATSADMAPP.P PERSISTENT SET satsadmapph.
   END.  
   IF Guru.Konstanter:appcon THEN DO:
      RUN MTRLHMT.P PERSISTENT SET mtrlhmtapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN MTRLHMT.P PERSISTENT SET mtrlhmtapph.
   END.  

   tthandle = TEMP-TABLE levtemp:HANDLE.
   IF Guru.Konstanter:appcon THEN DO:
      RUN DYNLADDATEMP.P PERSISTENT SET laddaproch ON Guru.Konstanter:apphand TRANSACTION DISTINCT
         (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "LEVERANTOR", INPUT " WHERE BORTTAG = FALSE ").
   END.
   ELSE DO:
      RUN DYNLADDATEMP.P PERSISTENT SET laddaproch
         (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "LEVERANTOR", INPUT " WHERE BORTTAG = FALSE ").
   END.
   tthandle = TEMP-TABLE huvlevtemp:HANDLE.
   RUN laddatemp_UI IN laddaproch (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "HUVUDLEV", INPUT "").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI WINDOW-2  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-2)
  THEN DELETE WIDGET WINDOW-2.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI WINDOW-2  _DEFAULT-ENABLE
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
  DISPLAY CMB_LEV FILL-IN-KOD FILL-IN-BEN RAD_SOK FILL-IN-SOKALT 
      WITH FRAME FRAME-B IN WINDOW WINDOW-2.
  ENABLE RECT-9 IMAGE-6 CMB_LEV BRW_MTRL FBTN_VISA FBTN_SKRIV BTN_NY BTN_ANDRA 
         BTN_BORT FILL-IN-KOD FILL-IN-BEN RAD_SOK BTN_AVB FILL-IN-SOKALT 
      WITH FRAME FRAME-B IN WINDOW WINDOW-2.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-B}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE infoES_UI WINDOW-2 
PROCEDURE infoES_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/      
   DEFINE INPUT  PARAMETER valenr AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER brwname AS CHARACTER NO-UNDO.
   DEFINE VARIABLE valbrw AS INTEGER NO-UNDO.  
   DEFINE VARIABLE levnamnvar AS CHARACTER NO-UNDO.
   IF brwname = "BRW_MTRL" THEN valbrw = 1.  
    
   IF valbrw = 1 THEN DO:
      status-ok = BRW_MTRL:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}  NO-ERROR.
      IF status-ok THEN DO:
         valenr = satstemp.ENR.
         FIND FIRST levtemp WHERE levtemp.LEVKOD = satstemp.LEVKOD
         NO-LOCK NO-ERROR.      
      END.
   END.
   IF status-ok THEN DO:
      levnamnvar = levtemp.LEVNAMN.
      {LEVLANK.I}      
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initsok_UI WINDOW-2 
PROCEDURE initsok_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/           
   DEFINE INPUT  PARAMETER vad AS INTEGER    NO-UNDO.
   DEFINE INPUT PARAMETER sokpa AS CHARACTER NO-UNDO.
   DEFINE VARIABLE orgfraga AS CHARACTER NO-UNDO.
   IF vad = 1 THEN DO:
      orgfraga = " WHERE SATS = TRUE AND LEVKOD = '" + STRING(vald_lev) + "' USE-INDEX KOD ".
      tth = TEMP-TABLE satstemp:HANDLE.
      EMPTY TEMP-TABLE valsoktemp NO-ERROR. 
      CREATE valsoktemp.
      ASSIGN 
      valsoktemp.SOKCHAR[1] = "SATS"     /*Skarp tabell*/
      valsoktemp.SOKCHAR[2] = orgfraga   /*Öppningsquery*/
      valsoktemp.SOKCHAR[3] = "BENAMNING" /*sökfält*/
      valsoktemp.SOKCHAR[4] = "SATSROW"  /*temptabells faltnamn för rowid*/
      valsoktemp.SOKCHAR[5] = sokpa.      /*sök på*/       
      RUN sokhmt_UI IN  brwproc[1] (INPUT TABLE valsoktemp).  
   END.
   IF vad = 2 THEN DO:
     orgfraga = " WHERE SATS = TRUE AND LEVKOD = '" + STRING(vald_lev) + "' USE-INDEX KOD ".
     tth = TEMP-TABLE satstemp:HANDLE.
     EMPTY TEMP-TABLE valsoktemp NO-ERROR. 
     CREATE valsoktemp.
     ASSIGN 
     valsoktemp.SOKCHAR[1] = "SATS"     /*Skarp tabell*/
     valsoktemp.SOKCHAR[2] = orgfraga   /*Öppningsquery*/
     valsoktemp.SOKCHAR[3] = "KOD" /*sökfält*/
     valsoktemp.SOKCHAR[4] = "SATSROW"  /*temptabells faltnamn för rowid*/
     valsoktemp.SOKCHAR[5] = sokpa.      /*sök på*/       
     RUN sokhmt_UI IN  brwproc[1] (INPUT TABLE valsoktemp).  
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE skapa_UI WINDOW-2 
PROCEDURE skapa_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/    
   EMPTY TEMP-TABLE sats_mtrl NO-ERROR.    
   ASSIGN
   skapben = satstemp.BENAMNING       
   skapkod = satstemp.KOD
   skapenr = satstemp.ENR      
   skapenh = satstemp.ENHET 
   skappris = satstemp.PRIS.
   RUN skapsats_UI IN satsadmapph (INPUT skapkod,INPUT vald_lev,
                                   OUTPUT TABLE sats_mtrl).

                    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

