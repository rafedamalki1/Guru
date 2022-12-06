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
{GLOBVAR2DEL1.I}
{REGVAR.I}

&Scoped-define SHARED SHARED
{OMRTEMPW.I}
{AVTPLANTEMP.I}
&Scoped-define NEW NEW
DEFINE NEW SHARED VARIABLE kontrec AS RECID NO-UNDO.
DEFINE NEW SHARED VARIABLE valomr AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE kto AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE franar AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.   

DEFINE VARIABLE ortssok AS CHARACTER NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE my1hand AS WIDGET-HANDL NO-UNDO.
DEFINE VARIABLE antal_valda AS INTEGER NO-UNDO.
DEFINE VARIABLE antal_raknare AS INTEGER NO-UNDO. 
DEFINE VARIABLE antal_valda2 AS INTEGER NO-UNDO.
DEFINE VARIABLE antal_raknare2 AS INTEGER NO-UNDO.
DEFINE VARIABLE buff AS CHARACTER NO-UNDO. 
DEFINE VARIABLE uppar AS INTEGER NO-UNDO. 
DEFINE VARIABLE slutar AS INTEGER NO-UNDO.
DEFINE VARIABLE raknare AS INTEGER NO-UNDO.
DEFINE VARIABLE stat AS LOGICAL NO-UNDO. 
DEFINE VARIABLE tillrec AS RECID NO-UNDO.

DEFINE NEW SHARED TEMP-TABLE kontkod   
   FIELD KONTO AS CHARACTER
   FIELD KONTONR AS CHARACTER 
   FIELD BENAMNING AS CHARACTER
   INDEX KNR IS PRIMARY KONTONR ASCENDING.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BRW_KONTO

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES kontkod omrtemp

/* Definitions for BROWSE BRW_KONTO                                     */
&Scoped-define FIELDS-IN-QUERY-BRW_KONTO kontkod.KONTONR kontkod.BENAMNING 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_KONTO 
&Scoped-define QUERY-STRING-BRW_KONTO FOR EACH kontkod NO-LOCK
&Scoped-define OPEN-QUERY-BRW_KONTO OPEN QUERY BRW_KONTO FOR EACH kontkod NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_KONTO kontkod
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_KONTO kontkod


/* Definitions for BROWSE BRW_OMR                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_OMR omrtemp.OMRADE omrtemp.NAMN 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_OMR 
&Scoped-define QUERY-STRING-BRW_OMR FOR EACH omrtemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_OMR OPEN QUERY BRW_OMR FOR EACH omrtemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_OMR omrtemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_OMR omrtemp


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-BRW_KONTO}~
    ~{&OPEN-QUERY-BRW_OMR}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-21 CMB_ARTAL BRW_KONTO BRW_OMR ~
FBTN_VISA RAD_KONTO FBTN_SKRIV BTN_BUD FILL-IN-KOD FILL-IN-TEXT BTN_AVB 
&Scoped-Define DISPLAYED-OBJECTS CMB_ARTAL FILL-IN-VALJ RAD_KONTO ~
FILL-IN-KOD FILL-IN-TEXT 

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

DEFINE BUTTON BTN_BUD 
     LABEL "Tilldela" 
     SIZE 12 BY 1.

DEFINE BUTTON FBTN_SKRIV 
     LABEL "Skriv ut" 
     SIZE 14 BY 1.

DEFINE BUTTON FBTN_VISA 
     LABEL "Visa" 
     SIZE 14 BY 1.

DEFINE VARIABLE CMB_ARTAL AS INTEGER FORMAT "9999":U INITIAL 0 
     LABEL "Årtal" 
     VIEW-AS COMBO-BOX INNER-LINES 7
     LIST-ITEMS "0" 
     DROP-DOWN-LIST
     SIZE 12.25 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-KOD AS CHARACTER FORMAT "x(4)" 
     LABEL "Kod" 
     VIEW-AS FILL-IN 
     SIZE 6.5 BY .83.

DEFINE VARIABLE FILL-IN-TEXT AS CHARACTER FORMAT "x(256)" 
     LABEL "Benämning" 
     VIEW-AS FILL-IN 
     SIZE 20.75 BY .83.

DEFINE VARIABLE FILL-IN-VALJ AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 25.25 BY 1.21
     FONT 17 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE RAD_KONTO AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "", 1
     SIZE 12.88 BY 9.63 NO-UNDO.

DEFINE RECTANGLE RECT-21
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 35 BY 2.42
     BGCOLOR 8 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_KONTO FOR 
      kontkod SCROLLING.

DEFINE QUERY BRW_OMR FOR 
      omrtemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_KONTO
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_KONTO C-Win _STRUCTURED
  QUERY BRW_KONTO NO-LOCK DISPLAY
      kontkod.KONTONR COLUMN-LABEL "Kod" FORMAT "x(6)":U
      kontkod.BENAMNING COLUMN-LABEL "Benämning" FORMAT "X(25)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH MULTIPLE SIZE 35 BY 11.04.

DEFINE BROWSE BRW_OMR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_OMR C-Win _STRUCTURED
  QUERY BRW_OMR NO-LOCK DISPLAY
      omrtemp.OMRADE COLUMN-LABEL "Område" FORMAT "X(6)":U
      omrtemp.NAMN COLUMN-LABEL "Benämning" FORMAT "X(256)":U WIDTH 30
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SIZE 41 BY 11.04.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     CMB_ARTAL AT ROW 3.04 COL 3.38
     FILL-IN-VALJ AT ROW 5.29 COL 1.5 NO-LABEL
     BRW_KONTO AT ROW 6.63 COL 57.63
     BRW_OMR AT ROW 6.71 COL 1.5
     FBTN_VISA AT ROW 6.79 COL 93.5
     RAD_KONTO AT ROW 7.13 COL 43.38 NO-LABEL
     FBTN_SKRIV AT ROW 8 COL 93.5
     BTN_BUD AT ROW 17.92 COL 70.13
     FILL-IN-KOD AT ROW 19.46 COL 69.5 COLON-ALIGNED
     FILL-IN-TEXT AT ROW 20.54 COL 69.5 COLON-ALIGNED
     BTN_AVB AT ROW 20.71 COL 93.5
     "Välj det år som skall budgeteras:" VIEW-AS TEXT
          SIZE 40 BY 1 AT ROW 1.54 COL 1.5
          FONT 17
     "Sök på:" VIEW-AS TEXT
          SIZE 7.25 BY .83 AT ROW 19.58 COL 58.63
     "Välj kod" VIEW-AS TEXT
          SIZE 14.5 BY 1.21 AT ROW 5.29 COL 67.13
          FONT 17
     RECT-21 AT ROW 19.29 COL 57.63
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 107.63 BY 22.17.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Temp-Tables and Buffers:
      TABLE: omrtemp T "?" NO-UNDO temp-db omrtemp
      TABLE: ? T "?" NO-UNDO temp-db kontkod
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "BUDGET - Välj område / kontodel /konto"
         HEIGHT             = 22.17
         WIDTH              = 107.38
         MAX-HEIGHT         = 25.13
         MAX-WIDTH          = 110.63
         VIRTUAL-HEIGHT     = 25.13
         VIRTUAL-WIDTH      = 110.63
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
/* BROWSE-TAB BRW_KONTO FILL-IN-VALJ DEFAULT-FRAME */
/* BROWSE-TAB BRW_OMR BRW_KONTO DEFAULT-FRAME */
/* SETTINGS FOR COMBO-BOX CMB_ARTAL IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FILL-IN-VALJ IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR RADIO-SET RAD_KONTO IN FRAME DEFAULT-FRAME
   SHARED                                                               */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_KONTO
/* Query rebuild information for BROWSE BRW_KONTO
     _TblList          = "Temp-Tables.kontkod"
     _Options          = "NO-LOCK "
     _FldNameList[1]   > Temp-Tables.kontkod.KONTONR
"kontkod.KONTONR" "Kod" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.kontkod.BENAMNING
"kontkod.BENAMNING" "Benämning" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is OPENED
*/  /* BROWSE BRW_KONTO */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_OMR
/* Query rebuild information for BROWSE BRW_OMR
     _TblList          = "Temp-Tables.omrtemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.omrtemp.OMRADE
"omrtemp.OMRADE" "Område" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.omrtemp.NAMN
"omrtemp.NAMN" "Benämning" "X(256)" "character" ? ? ? ? ? ? no "ANGE ORAGNISTIONENSBENÄMNING" no no "30" yes no no "U" "" ""
     _Query            is OPENED
*/  /* BROWSE BRW_OMR */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* BUDGET - Välj område / kontodel /konto */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* BUDGET - Välj område / kontodel /konto */
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
   musz = TRUE.
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_BUD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_BUD C-Win
ON CHOOSE OF BTN_BUD IN FRAME DEFAULT-FRAME /* Tilldela */
DO: 
   {muswait.i} 
   status-ok = BRW_OMR:SELECT-FOCUSED-ROW().
   valomr = omrtemp.OMRADE.
   antal_valda2 = BRW_KONTO:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME}.      
   antal_raknare2 = 1.
   DO WHILE antal_raknare2 LE antal_valda2 :
      status-ok = BRW_KONTO:FETCH-SELECTED-ROW(antal_raknare2). 
      kontrec = RECID(kontkod).
      
      RUN BUDTILL.W.          
      
      antal_raknare2 = antal_raknare2 + 1.
   END.     
   {musarrow.i}        
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_ARTAL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_ARTAL C-Win
ON LEAVE OF CMB_ARTAL IN FRAME DEFAULT-FRAME /* Årtal */
DO:                            
   ASSIGN
   CMB_ARTAL = INPUT CMB_ARTAL
   franar = CMB_ARTAL.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_ARTAL C-Win
ON VALUE-CHANGED OF CMB_ARTAL IN FRAME DEFAULT-FRAME /* Årtal */
DO:                           
   ASSIGN
   CMB_ARTAL = INPUT CMB_ARTAL
   franar = CMB_ARTAL.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_SKRIV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_SKRIV C-Win
ON CHOOSE OF FBTN_SKRIV IN FRAME DEFAULT-FRAME /* Skriv ut */
DO:        
   {muswait.i}   
   status-ok = BRW_OMR:SELECT-FOCUSED-ROW().
   valomr = omrtemp.OMRADE.
   ASSIGN 
   skrivut = TRUE.
   RUN SKRIVVAL.W (INPUT FALSE).   
   IF musz = TRUE THEN DO:
       musz = FALSE.
       skrivut = FALSE.  
   END.                 
   IF skrivut = FALSE THEN DO:
      skrivut = skrivut.
   END.   
   ELSE DO:  
      {AVBGOM.I}
      RUN VISBUD.W.
      {AVBFRAM.I} 
      musz = FALSE.
   END. 
   {musarrow.i}    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_SKRIV C-Win
ON MOUSE-MENU-CLICK OF FBTN_SKRIV IN FRAME DEFAULT-FRAME /* Skriv ut */
DO:
   RUN SIDLANGD.W.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_VISA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_VISA C-Win
ON CHOOSE OF FBTN_VISA IN FRAME DEFAULT-FRAME /* Visa */
DO: 
   {muswait.i}       
   status-ok = BRW_OMR:SELECT-FOCUSED-ROW().
   valomr = omrtemp.OMRADE.
   ASSIGN 
   skrivut = FALSE. 
   {AVBGOM.I}
   RUN VISBUD.W.
   {AVBFRAM.I}
   musz = FALSE.
   {musarrow.i}        
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-KOD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-KOD C-Win
ON ANY-KEY OF FILL-IN-KOD IN FRAME DEFAULT-FRAME /* Kod */
DO:
   {TRYCKS.I}
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN-KOD IN FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-KOD C-Win
ON MOUSE-SELECT-DBLCLICK OF FILL-IN-KOD IN FRAME DEFAULT-FRAME /* Kod */
DO:
   DEFINE VARIABLE posok AS CHARACTER NO-UNDO.
   FILL-IN-KOD = INPUT FILL-IN-KOD.
   /*status-ok = BRW_KONTO:SELECT-FOCUSED-ROW().
   APPLY "VALUE-CHANGED" TO BRW_KONTO.*/
   IF AVAILABLE kontkod THEN tillrec = RECID(kontkod).
   IF FILL-IN-KOD = '' THEN DO:
      MESSAGE "Sökbegreppet kan inte vara blankt." VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN-KOD IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
   END.      
   posok = '*' + FILL-IN-KOD + '*'.        
   FIND kontkod WHERE RECID(kontkod) = tillrec NO-LOCK NO-ERROR.
   FIND NEXT kontkod WHERE kontkod.KONTONR MATCHES posok       
   USE-INDEX KNR NO-LOCK NO-ERROR. 
   IF NOT AVAILABLE kontkod THEN DO:      
      FIND FIRST kontkod WHERE kontkod.KONTONR MATCHES posok       
      USE-INDEX KNR NO-LOCK NO-ERROR.   
   END.   
   IF NOT AVAILABLE kontkod THEN DO:
      MESSAGE "Det finns inget på sökbegreppet." VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN-KOD IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY. 
   END.     
   IF AVAILABLE kontkod THEN DO:
      RUN repo_UI (INPUT 1, INPUT RECID(kontkod)).
      status-ok = BRW_KONTO:SELECT-FOCUSED-ROW().
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-TEXT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-TEXT C-Win
ON ANY-KEY OF FILL-IN-TEXT IN FRAME DEFAULT-FRAME /* Benämning */
DO:
   {TRYCKS.I}
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN-TEXT IN FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-TEXT C-Win
ON MOUSE-SELECT-DBLCLICK OF FILL-IN-TEXT IN FRAME DEFAULT-FRAME /* Benämning */
DO:
   DEFINE VARIABLE posok AS CHARACTER NO-UNDO.   
   FILL-IN-TEXT = INPUT FILL-IN-TEXT.
   /*status-ok = BRW_KONTO:SELECT-FOCUSED-ROW().
   APPLY "VALUE-CHANGED" TO BRW_KONTO.*/
   IF AVAILABLE kontkod THEN tillrec = RECID(kontkod).
   IF FILL-IN-TEXT = '' THEN DO:
      MESSAGE "Sökbegreppet kan inte vara blankt." VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN-TEXT IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
   END.      
   posok = '*' + FILL-IN-TEXT + '*'.
   FIND kontkod WHERE RECID(kontkod) = tillrec NO-LOCK NO-ERROR.
   FIND NEXT kontkod WHERE kontkod.BENAMNING MATCHES posok       
   USE-INDEX KNR NO-LOCK NO-ERROR.
   IF NOT AVAILABLE kontkod THEN DO:      
      FIND FIRST kontkod WHERE kontkod.BENAMNING MATCHES posok       
      USE-INDEX KNR NO-LOCK NO-ERROR.     
      IF NOT AVAILABLE kontkod THEN DO:      
         MESSAGE "Det finns inget på sökbegreppet." VIEW-AS ALERT-BOX.
         APPLY "ENTRY" TO FILL-IN-TEXT IN FRAME {&FRAME-NAME}.
         RETURN NO-APPLY.
      END.
   END.   
   IF AVAILABLE kontkod THEN DO:
      RUN repo_UI (INPUT 1, INPUT RECID(kontkod)).
      status-ok = BRW_KONTO:SELECT-FOCUSED-ROW().
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RAD_KONTO
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RAD_KONTO C-Win
ON ENTRY OF RAD_KONTO IN FRAME DEFAULT-FRAME
DO:      
  /*RETURN. */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RAD_KONTO C-Win
ON VALUE-CHANGED OF RAD_KONTO IN FRAME DEFAULT-FRAME
DO:  
   ASSIGN 
   RAD_KONTO = INPUT RAD_KONTO.
   IF RAD_KONTO = 1 THEN kto = "K1".
   IF RAD_KONTO = 2 THEN kto = "K2".
   IF RAD_KONTO = 3 THEN kto = "K3".
   IF RAD_KONTO = 4 THEN kto = "K4".
   IF RAD_KONTO = 5 THEN kto = "K5".               
   RUN tillagg_UI.  
      /*
   kto = "K2".
   IF RAD_KONTO NE 2 THEN DO:
      MESSAGE "Endast kontogren 2 går att välja"
      VIEW-AS ALERT-BOX TITLE "Meddelande".
      RAD_KONTO = 2.
      DISPLAY RAD_KONTO WITH FRAME {&FRAME-NAME}.
   END.   
   */
   /*
   buff = " ".   
   IF RAD_KONTO = 1 THEN kto = "K1".
   IF RAD_KONTO = 2 THEN kto = "K2".
   IF RAD_KONTO = 3 THEN kto = "K3".
   IF RAD_KONTO = 4 THEN kto = "K4".
   IF RAD_KONTO = 5 THEN kto = "K5".               
   
   BRW_KONTO:HIDDEN=FALSE.      
   OPEN QUERY kontq FOR EACH KONTO WHERE KONTO.KONTO = kto 
   USE-INDEX KONTO NO-LOCK.
   GET FIRST kontq NO-LOCK.   
   DO WHILE AVAILABLE(KONTO):
     IF kto = "K1" THEN DO: 
         FIND FIRST AONRKONTKOD WHERE AONRKONTKOD.K1 =  KONTO.KONTONR NO-LOCK NO-ERROR.
         IF AVAILABLE AONRKONTKOD THEN DO:
            CREATE kontkod.   
            ASSIGN
            kontkod.KONTO = "K1"
            kontkod.KONTONR = KONTO.KONTONR
            kontkod.BENAMNING = KONTO.BENAMNING.         
         END.       
      END.   
      ELSE IF kto = "K2" THEN DO: 
         FIND FIRST AONRKONTKOD WHERE AONRKONTKOD.K2 =  KONTO.KONTONR NO-LOCK NO-ERROR.
         IF AVAILABLE AONRKONTKOD THEN DO:
            CREATE kontkod.   
            ASSIGN
            kontkod.KONTO = "K2"
            kontkod.KONTONR = KONTO.KONTONR
            kontkod.BENAMNING = KONTO.BENAMNING.      
         END.  
      END.   
      ELSE IF kto = "K3" THEN DO: 
         FIND FIRST AONRKONTKOD WHERE AONRKONTKOD.K3 =  KONTO.KONTONR NO-LOCK NO-ERROR.
         IF AVAILABLE AONRKONTKOD THEN DO:
            CREATE kontkod.   
            ASSIGN              
            kontkod.KONTO = "K3"
            kontkod.KONTONR = KONTO.KONTONR
            kontkod.BENAMNING = KONTO.BENAMNING.    
         END.  
      END.   
      ELSE IF kto = "K4" THEN DO: 
         FIND FIRST AONRKONTKOD WHERE AONRKONTKOD.K4 =  KONTO.KONTONR NO-LOCK NO-ERROR.
         IF AVAILABLE AONRKONTKOD THEN DO:
            CREATE kontkod.   
            ASSIGN              
            kontkod.KONTO = "K4"
            kontkod.KONTONR = KONTO.KONTONR
            kontkod.BENAMNING = KONTO.BENAMNING.                                                           
         END.                                                                          
      END.   
      ELSE IF kto = "K5" THEN DO: 
         FIND FIRST AONRKONTKOD WHERE AONRKONTKOD.K5 =  KONTO.KONTONR NO-LOCK NO-ERROR.
         IF AVAILABLE AONRKONTKOD THEN DO:
            CREATE kontkod.   
            ASSIGN              
            kontkod.KONTO = "K5"
            kontkod.KONTONR = KONTO.KONTONR
            kontkod.BENAMNING = KONTO.BENAMNING.      
         END.  
      END.   
      GET NEXT kontq NO-LOCK.      
   END.
   CLOSE QUERY kontq.                     
   OPEN QUERY BRW_KONTO FOR EACH kontkod USE-INDEX KNR NO-LOCK.
   */  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_KONTO
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:   
   {WIN_M_START.I}
   {muswait.i}   

 /*    FIND FIRST KBENAMNING USE-INDEX KBEN NO-LOCK NO-ERROR. */
   IF Guru.Konstanter:appcon THEN DO:                           
      RUN KBENHMT.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (OUTPUT TABLE kbenamntemp).
   END.
   ELSE DO:
      RUN KBENHMT.P 
      (OUTPUT TABLE kbenamntemp).                  
   END.
   FIND FIRST kbenamntemp USE-INDEX KBEN NO-LOCK NO-ERROR.
   IF AVAILABLE kbenamntemp THEN DO:
      stat = rad_konto:DELETE("").
      IF kbenamntemp.K1 NE "" THEN DO:
         RAD_KONTO = 1.
         stat = rad_konto:ADD-LAST(CAPS(SUBSTRING(kbenamntemp.K1,1,1)) + LC(SUBSTRING(kbenamntemp.K1,2)),1).
      END.
      IF kbenamntemp.K2 NE "" THEN DO:
         RAD_KONTO = 2.
         stat = rad_konto:ADD-LAST(CAPS(SUBSTRING(kbenamntemp.K2,1,1)) + LC(SUBSTRING(kbenamntemp.K2,2)),2).
      END.
      IF kbenamntemp.K3 NE "" THEN DO:
        stat = rad_konto:ADD-LAST(CAPS(SUBSTRING(kbenamntemp.K3,1,1)) + LC(SUBSTRING(kbenamntemp.K3,2)),3).
      END.
      IF kbenamntemp.K4 NE "" THEN DO:
         stat = rad_konto:ADD-LAST(CAPS(SUBSTRING(kbenamntemp.K4,1,1)) + LC(SUBSTRING(kbenamntemp.K4,2)),4).
      END.
      IF kbenamntemp.K5 NE "" THEN DO:  
         stat = rad_konto:ADD-LAST(CAPS(SUBSTRING(kbenamntemp.K5,1,1)) + LC(SUBSTRING(kbenamntemp.K5,2)),5). 
      END.
   END.
   ASSIGN
   status-ok = CMB_ARTAL:DELETE("0"). 
   /*LADDAR ÅR I CMB_ARTAL*/  
   uppar = franar.
   slutar = YEAR(TODAY) + 4.
   status-ok = CMB_ARTAL:ADD-LAST(STRING(uppar,"9999")).             
   DO WHILE uppar < slutar: 
       uppar = uppar + 1.             
       status-ok = CMB_ARTAL:ADD-LAST(STRING(uppar,"9999")).    
   END.  
   ASSIGN
   CMB_ARTAL:SCREEN-VALUE = STRING(franar,"9999").       
   ASSIGN
   CMB_ARTAL = INPUT CMB_ARTAL.      
   IF RAD_KONTO = 1 THEN kto = "K1".
   IF RAD_KONTO = 2 THEN kto = "K2".
   IF RAD_KONTO = 3 THEN kto = "K3".
   IF RAD_KONTO = 4 THEN kto = "K4".
   IF RAD_KONTO = 5 THEN kto = "K5".
  
   RUN enable_UI.   
   {FRMSIZE.I}     
   EMPTY TEMP-TABLE kontkod NO-ERROR.    
   omrtemp.OMRADE:LABEL IN BROWSE BRW_OMR = Guru.Konstanter:gomrk.
   FILL-IN-VALJ = "Välj " + LC(Guru.Konstanter:gomrk).   
   ENABLE BRW_OMR WITH FRAME {&FRAME-NAME}.  
   OPEN QUERY BRW_OMR FOR EACH omrtemp USE-INDEX OMR NO-LOCK .
/*    FOR EACH omrtemp WHERE omrtemp.ELVOMRKOD = 0 USE-INDEX OMR NO-LOCK . */
   RUN tillagg_UI.  
   {musarrow.i}     
   {WIN_M_SLUT.I}
   IF NOT THIS-PROCEDURE:PERSISTENT THEN
   WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

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
  DISPLAY CMB_ARTAL FILL-IN-VALJ RAD_KONTO FILL-IN-KOD FILL-IN-TEXT 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-21 CMB_ARTAL BRW_KONTO BRW_OMR FBTN_VISA RAD_KONTO FBTN_SKRIV 
         BTN_BUD FILL-IN-KOD FILL-IN-TEXT BTN_AVB 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE repo_UI C-Win 
PROCEDURE repo_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/   
   DEFINE INPUT PARAMETER brwvar AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER browrec AS RECID NO-UNDO.
   IF brwvar = 1 THEN DO:
      &Scoped-define BROWSE-NAME BRW_KONTO
      {&BROWSE-NAME}:SET-REPOSITIONED-ROW(35,"ALWAYS") IN FRAME {&FRAME-NAME}.
      REPOSITION {&BROWSE-NAME} TO RECID browrec NO-ERROR.  
   END.
   IF brwvar = 2 THEN DO:
      &Scoped-define BROWSE-NAME BRW_OMR
      {&BROWSE-NAME}:SET-REPOSITIONED-ROW(35,"ALWAYS") IN FRAME {&FRAME-NAME}.
      REPOSITION {&BROWSE-NAME} TO RECID browrec NO-ERROR.  
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tillagg_UI C-Win 
PROCEDURE tillagg_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   buff = " ".   
   IF RAD_KONTO = 1 THEN kto = "K1".
   IF RAD_KONTO = 2 THEN kto = "K2".
   IF RAD_KONTO = 3 THEN kto = "K3".
   IF RAD_KONTO = 4 THEN kto = "K4".
   IF RAD_KONTO = 5 THEN kto = "K5".

   IF Guru.Konstanter:appcon THEN DO:                           
      RUN KONTHMT.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT kto,OUTPUT TABLE kontkod).
   END.
   ELSE DO:
      RUN KONTHMT.P 
      (INPUT kto,OUTPUT TABLE kontkod).        
   END.
   
/*                                                                                     */
/*    OPEN QUERY kontq FOR EACH KONTO WHERE KONTO.KONTO = kto USE-INDEX KONTO NO-LOCK. */
/*    GET FIRST kontq NO-LOCK.                                                         */
/*    DO WHILE AVAILABLE(KONTO):                                                       */
/*       CREATE kontkod.                                                               */
/*       ASSIGN                                                                        */
/*       kontkod.KONTO = kto                                                           */
/*       kontkod.KONTONR = KONTO.KONTONR                                               */
/*       kontkod.BENAMNING = KONTO.BENAMNING.                                          */
/*       /*                                                                            */
/*       IF kto = "K1" THEN DO:                                                        */
/*          CREATE kontkod.                                                            */
/*          ASSIGN                                                                     */
/*          kontkod.KONTO = "K1"                                                       */
/*          kontkod.KONTONR = KONTO.KONTONR                                            */
/*          kontkod.BENAMNING = KONTO.BENAMNING.                                       */
/*       END.                                                                          */
/*       ELSE IF kto = "K2" THEN DO:                                                   */
/*          CREATE kontkod.                                                            */
/*          ASSIGN                                                                     */
/*          kontkod.KONTO = "K2"                                                       */
/*          kontkod.KONTONR = KONTO.KONTONR                                            */
/*          kontkod.BENAMNING = KONTO.BENAMNING.                                       */
/*       END.                                                                          */
/*       ELSE IF kto = "K3" THEN DO:                                                   */
/*          CREATE kontkod.                                                            */
/*          ASSIGN                                                                     */
/*          kontkod.KONTO = "K3"                                                       */
/*          kontkod.KONTONR = KONTO.KONTONR                                            */
/*          kontkod.BENAMNING = KONTO.BENAMNING.                                       */
/*       END.                                                                          */
/*       ELSE IF kto = "K4" THEN DO:                                                   */
/*          CREATE kontkod.                                                            */
/*          ASSIGN                                                                     */
/*          kontkod.KONTO = "K4"                                                       */
/*          kontkod.KONTONR = KONTO.KONTONR                                            */
/*          kontkod.BENAMNING = KONTO.BENAMNING.                                       */
/*       END.                                                                          */
/*       ELSE IF kto = "K5" THEN DO:                                                   */
/*          CREATE kontkod.                                                            */
/*          ASSIGN                                                                     */
/*          kontkod.KONTO = "K5"                                                       */
/*          kontkod.KONTONR = KONTO.KONTONR                                            */
/*          kontkod.BENAMNING = KONTO.BENAMNING.                                       */
/*       END.                                                                          */
/*       */                                                                            */
/*       GET NEXT kontq NO-LOCK.                                                       */
/*    END.                                                                             */
/*    CLOSE QUERY kontq.                                                               */
   OPEN QUERY BRW_KONTO FOR EACH kontkod USE-INDEX KNR NO-LOCK.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

