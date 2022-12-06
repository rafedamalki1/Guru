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
 DEFINE VARIABLE persproch AS HANDLE NO-UNDO.     /* PERSONALAPP.P */
   {JURPERST.I}
{ALLDEF.I}
{OMRTEMPW.I}
&Scoped-define NEW 
{GLOBVAR2DEL1.I}
{REGVAR.I}

&Scoped-define SHARED SHARED
{AVTPLANTEMP.I}

{ANSVPLANTEMP.I}
&Scoped-define NEW NEW 
DEFINE SHARED VARIABLE  visvalvar AS INTEGER NO-UNDO.   /* 1= progres vis 2 = excel 3 = IE 4 = pdf*/
DEFINE SHARED VARIABLE franar AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE tillar AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE period AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE vallista AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.  
DEFINE NEW SHARED VARIABLE tillrec AS RECID NO-UNDO. 
DEFINE SHARED VARIABLE omr AS LOGICAL NO-UNDO.  
DEFINE NEW SHARED VARIABLE kto AS CHARACTER NO-UNDO.
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
DEFINE VARIABLE RAKNARE AS INTEGER NO-UNDO.
DEFINE VARIABLE stat AS LOGICAL NO-UNDO. 
DEFINE VARIABLE checkvar AS LOGICAL NO-UNDO.
DEFINE VARIABLE franarorg AS INTEGER NO-UNDO.

DEFINE NEW SHARED TEMP-TABLE omrkonto    
   FIELD OMRADE AS CHARACTER
   FIELD NAMN AS CHARACTER
   FIELD OMREC AS RECID  
   INDEX OMRADE IS PRIMARY OMRADE ASCENDING. 
   
DEFINE NEW SHARED TEMP-TABLE kkod
   FIELD KONTO AS CHARACTER
   FIELD KONTONR AS CHARACTER
   FIELD BENAMNING AS CHARACTER
   FIELD INDEL AS INTEGER
   FIELD KREC AS RECID
   INDEX KNR IS PRIMARY KONTONR ASCENDING.
   
   
DEFINE NEW SHARED TEMP-TABLE okod
   FIELD KONTO AS CHARACTER
   FIELD KONTONR AS CHARACTER
   FIELD BENAMNING AS CHARACTER
   FIELD INDEL AS INTEGER
   FIELD KREC AS RECID
   INDEX KNR IS PRIMARY KONTONR ASCENDING.    
  
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
&Scoped-Define ENABLED-OBJECTS RECT-21 RAD_PERIOD CMB_ARTAL CMB_FRAN ~
CMB_TILL CMB_ANSV TOG_ALLA BRW_OMR BRW_KONTO RAD_KONTO FBTN_SKRIV FBTN_VISA ~
FBTN_EX FILL-IN-KOD FILL-IN-TEXT BTN_AVB 
&Scoped-Define DISPLAYED-OBJECTS RAD_PERIOD CMB_ARTAL CMB_FRAN CMB_TILL ~
CMB_ANSV FILL-IN-VALJ FILL-IN-VKOD TOG_ALLA RAD_KONTO FILL-IN-KOD ~
FILL-IN-TEXT 

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

DEFINE BUTTON FBTN_EX 
     LABEL "Visa I EXCEL" 
     SIZE 14 BY 1.

DEFINE BUTTON FBTN_SKRIV 
     LABEL "Skriv ut" 
     SIZE 14 BY 1.

DEFINE BUTTON FBTN_VISA 
     LABEL "Visa" 
     SIZE 14 BY 1.

DEFINE VARIABLE CMB_ANSV AS CHARACTER FORMAT "X(256)":U 
     LABEL "Ansvarig" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 22 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_ARTAL AS INTEGER FORMAT "9999":U INITIAL 0 
     LABEL "Årtal" 
     VIEW-AS COMBO-BOX INNER-LINES 7
     LIST-ITEMS "0" 
     DROP-DOWN-LIST
     SIZE 12.25 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_FRAN AS INTEGER FORMAT "9999":U INITIAL 0 
     LABEL "Från" 
     VIEW-AS COMBO-BOX INNER-LINES 7
     LIST-ITEMS "0" 
     DROP-DOWN-LIST
     SIZE 12.25 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_TILL AS INTEGER FORMAT "9999":U INITIAL 0 
     LABEL "till" 
     VIEW-AS COMBO-BOX INNER-LINES 7
     LIST-ITEMS "0" 
     DROP-DOWN-LIST
     SIZE 12.25 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-KOD AS CHARACTER FORMAT "x(4)" 
     LABEL "Kod" 
     VIEW-AS FILL-IN 
     SIZE 6.5 BY .83.

DEFINE VARIABLE FILL-IN-TEXT AS CHARACTER FORMAT "x(20)" 
     LABEL "Benämning" 
     VIEW-AS FILL-IN 
     SIZE 12.5 BY .83.

DEFINE VARIABLE FILL-IN-VALJ AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 27 BY 1.21
     FONT 17 NO-UNDO.

DEFINE VARIABLE FILL-IN-VKOD AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 20.38 BY 1.21
     FONT 17 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE RAD_KONTO AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "", 1
     SIZE 12.88 BY 9.63 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE RAD_PERIOD AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Visning per år", 1,
"Visning period", 2
     SIZE 18.75 BY 2.5 NO-UNDO.

DEFINE RECTANGLE RECT-21
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 35 BY 2.42
     BGCOLOR 8 .

DEFINE VARIABLE TOG_ALLA AS LOGICAL INITIAL no 
     LABEL "Alla konton" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.88 BY 1 NO-UNDO.

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
      omrtemp.NAMN COLUMN-LABEL "Benämning" FORMAT "X(25)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH MULTIPLE SIZE 34.5 BY 11.04.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     RAD_PERIOD AT ROW 1.46 COL 1.5 NO-LABEL
     CMB_ARTAL AT ROW 1.54 COL 21.25
     CMB_FRAN AT ROW 2.92 COL 22.38
     CMB_TILL AT ROW 2.92 COL 40.88
     CMB_ANSV AT ROW 4.21 COL 26.38 COLON-ALIGNED
     FILL-IN-VALJ AT ROW 5.54 COL 1.5 NO-LABEL
     FILL-IN-VKOD AT ROW 5.54 COL 49.13 COLON-ALIGNED NO-LABEL
     TOG_ALLA AT ROW 5.96 COL 72.63
     BRW_OMR AT ROW 7.04 COL 1.5
     BRW_KONTO AT ROW 7.21 COL 51.38
     RAD_KONTO AT ROW 7.71 COL 36.75 NO-LABEL
     FBTN_SKRIV AT ROW 8 COL 88.25
     FBTN_VISA AT ROW 9.08 COL 88.25
     FBTN_EX AT ROW 10.21 COL 88.25
     FILL-IN-KOD AT ROW 19 COL 64.25 COLON-ALIGNED
     FILL-IN-TEXT AT ROW 19.96 COL 64.25 COLON-ALIGNED
     BTN_AVB AT ROW 20.13 COL 88.25
     "Sök på:" VIEW-AS TEXT
          SIZE 8.5 BY .83 AT ROW 19 COL 52.88
     RECT-21 AT ROW 18.71 COL 51.38
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 102.25 BY 20.54.


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
         TITLE              = "Välj område / kontodel /konto"
         HEIGHT             = 20.46
         WIDTH              = 102.38
         MAX-HEIGHT         = 25.13
         MAX-WIDTH          = 105
         VIRTUAL-HEIGHT     = 25.13
         VIRTUAL-WIDTH      = 105
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
/* BROWSE-TAB BRW_OMR TOG_ALLA DEFAULT-FRAME */
/* BROWSE-TAB BRW_KONTO BRW_OMR DEFAULT-FRAME */
/* SETTINGS FOR COMBO-BOX CMB_ARTAL IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
/* SETTINGS FOR COMBO-BOX CMB_FRAN IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
/* SETTINGS FOR COMBO-BOX CMB_TILL IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FILL-IN-VALJ IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN FILL-IN-VKOD IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR RADIO-SET RAD_KONTO IN FRAME DEFAULT-FRAME
   SHARED                                                               */
/* SETTINGS FOR RADIO-SET RAD_PERIOD IN FRAME DEFAULT-FRAME
   SHARED                                                               */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_KONTO
/* Query rebuild information for BROWSE BRW_KONTO
     _TblList          = "Temp-Tables.kontkod"
     _Options          = "NO-LOCK"
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
"omrtemp.NAMN" "Benämning" "X(25)" "character" ? ? ? ? ? ? no "ANGE ORGANISATIONSBENÄMNING" no no ? yes no no "U" "" ""
     _Query            is OPENED
*/  /* BROWSE BRW_OMR */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Välj område / kontodel /konto */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Välj område / kontodel /konto */
DO:
  /* This event will close the window and terminate the procedure.  */
   IF VALID-HANDLE(persproch) THEN DELETE PROCEDURE persproch NO-ERROR.
   APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB C-Win
ON CHOOSE OF BTN_AVB IN FRAME DEFAULT-FRAME /* Avsluta */
DO:

   franar = franarorg.
   musz = TRUE.
   RETURN.
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


&Scoped-define SELF-NAME CMB_FRAN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_FRAN C-Win
ON LEAVE OF CMB_FRAN IN FRAME DEFAULT-FRAME /* Från */
DO:                            
   ASSIGN
   CMB_FRAN = INPUT CMB_FRAN
   franar = CMB_FRAN.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_FRAN C-Win
ON VALUE-CHANGED OF CMB_FRAN IN FRAME DEFAULT-FRAME /* Från */
DO:                           
   ASSIGN
   CMB_FRAN = INPUT CMB_FRAN
   franar = CMB_FRAN.      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_TILL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_TILL C-Win
ON LEAVE OF CMB_TILL IN FRAME DEFAULT-FRAME /* till */
DO:                            
   ASSIGN
   CMB_TILL = INPUT CMB_TILL
   tillar = CMB_TILL.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_TILL C-Win
ON VALUE-CHANGED OF CMB_TILL IN FRAME DEFAULT-FRAME /* till */
DO:                           
   ASSIGN
   CMB_TILL = INPUT CMB_TILL
   tillar = CMB_TILL.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_EX
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_EX C-Win
ON CHOOSE OF FBTN_EX IN FRAME DEFAULT-FRAME /* Visa I EXCEL */
DO: 
   {muswait.i}
   visvalvar = 2.
   RUN check_UI.   
   IF checkvar = FALSE THEN DO:
      EMPTY TEMP-TABLE omrkonto NO-ERROR. 
      EMPTY TEMP-TABLE kkod  NO-ERROR.       
      RUN omr_UI.
      RUN valda_UI.   
      ASSIGN 
      skrivut = FALSE.
      RUN ut_UI.
   END.
   musz = FALSE.
   {musarrow.i}        
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_SKRIV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_SKRIV C-Win
ON CHOOSE OF FBTN_SKRIV IN FRAME DEFAULT-FRAME /* Skriv ut */
DO:        
   {muswait.i} 
   visvalvar = 1.
   RUN check_UI.
   IF checkvar = FALSE THEN DO:
      EMPTY TEMP-TABLE omrkonto NO-ERROR. 
      EMPTY TEMP-TABLE kkod NO-ERROR.       
      RUN omr_UI.
      RUN valda_UI.
      ASSIGN 
      skrivut = TRUE.
      RUN SKRIVVAL.W (INPUT TRUE).   
      IF musz = TRUE THEN DO:
          musz = FALSE.
          skrivut = FALSE.  
      END.                 
      IF skrivut = FALSE THEN DO:
         skrivut = skrivut.
      END.   
      ELSE DO:  
         RUN ut_UI.                 
      END.
   END. 
   musz = FALSE.
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
   visvalvar = 1.
   RUN check_UI.   
   IF checkvar = FALSE THEN DO:
      EMPTY TEMP-TABLE omrkonto NO-ERROR. 
      EMPTY TEMP-TABLE kkod  NO-ERROR.       
      RUN omr_UI.
      RUN valda_UI.   
      ASSIGN 
      skrivut = FALSE.
      RUN ut_UI.
   END.
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
      RUN repo_UI (INPUT RECID(kontkod)).
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
      RUN repo_UI (INPUT RECID(kontkod)).
      status-ok = BRW_KONTO:SELECT-FOCUSED-ROW().
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RAD_KONTO
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RAD_KONTO C-Win
ON ENTRY OF RAD_KONTO IN FRAME DEFAULT-FRAME
DO:      
  /*IF Guru.Konstanter:globforetag = "GRAN" THEN ASSIGN RAD_KONTO = 2. 
  ELSE ASSIGN RAD_KONTO = 1.  */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RAD_KONTO C-Win
ON VALUE-CHANGED OF RAD_KONTO IN FRAME DEFAULT-FRAME
DO:  
   ASSIGN 
   RAD_KONTO = INPUT RAD_KONTO.
   /*kto = "K2".
 *    IF RAD_KONTO NE 2 THEN DO:
 *       MESSAGE "Endast kontogren 2 går att välja."
 *       VIEW-AS ALERT-BOX TITLE "Meddelande".
 *       RAD_KONTO = 2.
 *       DISPLAY RAD_KONTO WITH FRAME {&FRAME-NAME}.
 *    END.*/   
         
   IF RAD_KONTO = 1 THEN kto = "K1".
   IF RAD_KONTO = 2 THEN kto = "K2".
   IF RAD_KONTO = 3 THEN kto = "K3".
   IF RAD_KONTO = 4 THEN kto = "K4".
   IF RAD_KONTO = 5 THEN kto = "K5".               
   RUN tillagg_UI.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RAD_PERIOD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RAD_PERIOD C-Win
ON VALUE-CHANGED OF RAD_PERIOD IN FRAME DEFAULT-FRAME
DO:
   RAD_PERIOD = INPUT RAD_PERIOD.   
   period = RAD_PERIOD.
   IF RAD_PERIOD = 1 THEN DO:
      franar = CMB_ARTAL.
   END.
   ELSE DO:
      ASSIGN
      franar = CMB_FRAN
      tillar = CMB_TILL.   
   END.   
   RUN hide_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_ALLA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_ALLA C-Win
ON VALUE-CHANGED OF TOG_ALLA IN FRAME DEFAULT-FRAME /* Alla konton */
DO:    
   TOG_ALLA = INPUT TOG_ALLA.
   IF TOG_ALLA = TRUE THEN DO:
      BRW_KONTO:HIDDEN = TRUE.    
      FILL-IN-VKOD:HIDDEN = TRUE.  
      FILL-IN-KOD:HIDDEN = TRUE.
      FILL-IN-TEXT:HIDDEN = TRUE.
   END.
   ELSE DO:
       BRW_KONTO:HIDDEN = FALSE.    
       FILL-IN-VKOD:HIDDEN = FALSE.
       FILL-IN-KOD:HIDDEN = FALSE.
       FILL-IN-TEXT:HIDDEN = FALSE.
   END.   
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
   {ALLSTARTDYN.I}      
/*    /*Ladda områden*/ */
   {OMRHMT.I}
   
   FOR EACH omrtemp:
      FIND FIRST judavdtemp WHERE judavdtemp.AVDELNINGNR = omrtemp.AVDELNINGNR NO-LOCK NO-ERROR.
      IF NOT AVAILABLE judavdtemp THEN DO:
         DELETE omrtemp.
      END.      
   END.
   ASSIGN FILL-IN-VKOD = "Välj kod".
   IF Guru.Konstanter:appcon THEN DO:
      RUN KBENHMT.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
      (OUTPUT TABLE kbenamntemp).
   END.
   ELSE DO:
      RUN KBENHMT.P 
      (OUTPUT TABLE kbenamntemp).
   END.
   FIND FIRST kbenamntemp USE-INDEX KBEN NO-LOCK NO-ERROR.
   stat = rad_konto:DELETE("").
   RAD_KONTO = 1.
   IF kbenamntemp.K1 NE "" THEN stat = rad_konto:ADD-LAST(CAPS(SUBSTRING(kbenamntemp.K1,1,1)) + LC(SUBSTRING(kbenamntemp.K1,2)),1).
   IF kbenamntemp.K2 NE "" THEN DO:
      RAD_KONTO = 2. 
      stat = rad_konto:ADD-LAST(CAPS(SUBSTRING(kbenamntemp.K2,1,1)) + LC(SUBSTRING(kbenamntemp.K2,2)),2).
   END.
   IF kbenamntemp.K3 NE "" THEN stat = rad_konto:ADD-LAST(CAPS(SUBSTRING(kbenamntemp.K3,1,1)) + LC(SUBSTRING(kbenamntemp.K3,2)),3).
   IF kbenamntemp.K4 NE "" THEN DO:
      stat = rad_konto:ADD-LAST(CAPS(SUBSTRING(kbenamntemp.K4,1,1)) + LC(SUBSTRING(kbenamntemp.K4,2)),4).
   END.
   IF kbenamntemp.K5 NE "" THEN DO:  
      stat = rad_konto:ADD-LAST(CAPS(SUBSTRING(kbenamntemp.K5,1,1)) + LC(SUBSTRING(kbenamntemp.K5,2)),5). 
   END.  
   ASSIGN
   status-ok = CMB_ARTAL:DELETE("0")
   status-ok = CMB_FRAN:DELETE("0")
   status-ok = CMB_TILL:DELETE("0").   
   /*LADDAR ÅR I CMB_ARTAL*/  
   franarorg = franar.
   uppar = franar.
   slutar = YEAR(TODAY) + 4.
   status-ok = CMB_ARTAL:ADD-LAST(STRING(uppar,"9999")).             
   DO WHILE uppar < slutar: 
       uppar = uppar + 1.             
       status-ok = CMB_ARTAL:ADD-LAST(STRING(uppar,"9999")).    
   END.  
   ASSIGN
   CMB_ARTAL:SCREEN-VALUE = STRING(franar,"9999").   

   /*LADDAR ÅR I CMB_FRAN*/  
   uppar = franar.
   slutar = YEAR(TODAY) + 4.
   status-ok = CMB_FRAN:ADD-LAST(STRING(uppar,"9999")).             
   DO WHILE uppar < slutar: 
       uppar = uppar + 1.             
       status-ok = CMB_FRAN:ADD-LAST(STRING(uppar,"9999")).    
   END.  
   ASSIGN
   CMB_FRAN:SCREEN-VALUE = STRING(franar,"9999").
   
    /*LADDAR ÅR I CMB_TILL*/  
   uppar = franar.
   slutar = YEAR(TODAY) + 9.
   status-ok = CMB_TILL:ADD-LAST(STRING(uppar,"9999")).             
   DO WHILE uppar < slutar: 
       uppar = uppar + 1.             
       status-ok = CMB_TILL:ADD-LAST(STRING(uppar,"9999")).    
   END.  
   ASSIGN
   CMB_TILL:SCREEN-VALUE = STRING(tillar,"9999").
   
   ASSIGN
   CMB_ARTAL = INPUT CMB_ARTAL
   CMB_FRAN = INPUT CMB_FRAN
   CMB_TILL = INPUT CMB_TILL   
   RAD_PERIOD = period  
   TOG_ALLA = FALSE.   
                   
   IF RAD_KONTO = 1 THEN kto = "K1". 
   IF RAD_KONTO = 2 THEN kto = "K2".
   status-ok = CMB_ANSV:ADD-FIRST("Alla").
   FOR EACH ansvplantemp USE-INDEX NAMN NO-LOCK:
      status-ok = CMB_ANSV:ADD-LAST(ansvplantemp.NAMN).
   END.
   CMB_ANSV:SCREEN-VALUE = "Alla".
   CMB_ANSV:LABEL = Guru.Konstanter:garbal.
   RUN enable_UI.       
             
   RUN hide_UI.
   EMPTY TEMP-TABLE kontkod NO-ERROR.    
   omrtemp.OMRADE:LABEL IN BROWSE BRW_OMR = Guru.Konstanter:gomrk.
   C-WIN:TITLE = "Välj " + LC(Guru.Konstanter:gomrk) + "/kontodel/konto".
   FILL-IN-VALJ = "Välj " + LC(Guru.Konstanter:gomrk).  
   ENABLE BRW_OMR WITH FRAME {&FRAME-NAME}.  
   OPEN QUERY BRW_OMR FOR EACH omrtemp USE-INDEX OMR NO-LOCK .
   RUN tillagg_UI.
   {FRMSIZE.I}
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
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   RUN DYNBRW.P PERSISTENT SET brwproc[{&LEFT-BROWSE}]
      (INPUT BRW_KONTO:HANDLE IN FRAME {&FRAME-NAME}).  
   RUN DYNBRW.P PERSISTENT SET brwproc[{&RIGHT-BROWSE}]
      (INPUT BRW_OMR:HANDLE IN FRAME {&FRAME-NAME}).  
   IF Guru.Konstanter:appcon THEN DO:
      RUN PERSONALAPP.P PERSISTENT SET persproch ON Guru.Konstanter:apphand TRANSACTION DISTINCT.   
      
   END.
   ELSE DO:
      RUN PERSONALAPP.P PERSISTENT SET persproch.  
   END.
   RUN jurp_UI IN persproch (INPUT Guru.Konstanter:globanv,OUTPUT TABLE jurperstemp,OUTPUT TABLE judavdtemp).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE check_UI C-Win 
PROCEDURE check_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   checkvar = FALSE.
   IF BRW_OMR:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME} = 0 THEN DO: 
      checkvar = TRUE.
      MESSAGE "Inget " + LC(Guru.Konstanter:gomrk) + " är markerat." VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO BRW_OMR IN FRAME {&FRAME-NAME}.
      RETURN. 
   END.  
   ELSE DO:              
      IF TOG_ALLA = FALSE THEN DO:
         IF BRW_KONTO:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME} = 0 THEN DO:
            checkvar = TRUE. 
            MESSAGE "Inget konto är markerat." VIEW-AS ALERT-BOX.
            APPLY "ENTRY" TO BRW_KONTO IN FRAME {&FRAME-NAME}.
            RETURN. 
         END.
      END.    
   END.   
   IF RAD_PERIOD = 2 THEN DO:
      IF CMB_FRAN > CMB_TILL THEN DO:
         checkvar = TRUE.
         MESSAGE "Årtalet 'Från' får inte vara större än årtalet 'Till'." 
         VIEW-AS ALERT-BOX TITLE "Meddelnade".
         APPLY "ENTRY" TO CMB_FRAN IN FRAME {&FRAME-NAME}.
         RETURN.
      END. 
      IF CMB_TILL =  CMB_FRAN THEN DO:
         checkvar = TRUE.
         MESSAGE "Årtalet 'Från' är lika med årtalet 'Till'. Om det är ditt val, välj visning per år." 
         VIEW-AS ALERT-BOX TITLE "Meddelnade".
         APPLY "ENTRY" TO CMB_TILL IN FRAME {&FRAME-NAME}.
         RETURN.
      END.  
      IF (CMB_TILL - CMB_FRAN) GE 5 THEN DO:
         checkvar = TRUE.
         MESSAGE "Du kan max välja 5 år som skall visas." 
         VIEW-AS ALERT-BOX TITLE "Meddelande".
         APPLY "ENTRY" TO CMB_TILL IN FRAME {&FRAME-NAME}.
         RETURN.
      END.
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
  DISPLAY RAD_PERIOD CMB_ARTAL CMB_FRAN CMB_TILL CMB_ANSV FILL-IN-VALJ 
          FILL-IN-VKOD TOG_ALLA RAD_KONTO FILL-IN-KOD FILL-IN-TEXT 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-21 RAD_PERIOD CMB_ARTAL CMB_FRAN CMB_TILL CMB_ANSV TOG_ALLA 
         BRW_OMR BRW_KONTO RAD_KONTO FBTN_SKRIV FBTN_VISA FBTN_EX FILL-IN-KOD 
         FILL-IN-TEXT BTN_AVB 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE hide_UI C-Win 
PROCEDURE hide_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   IF RAD_PERIOD = 1 THEN DO:                  
      CMB_ARTAL:HIDDEN IN FRAME {&FRAME-NAME} = FALSE. 
      CMB_FRAN:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
      CMB_TILL:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.    
   END.
   ELSE DO:                                                   
      CMB_ARTAL:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
      CMB_FRAN:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
      CMB_TILL:HIDDEN IN FRAME {&FRAME-NAME} = FALSE. 
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE omr_UI C-Win 
PROCEDURE omr_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   antal_valda2 = BRW_OMR:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME}.   
   antal_raknare2 = 1.
   DO WHILE antal_raknare2 LE antal_valda2 :
      status-ok = BRW_OMR:FETCH-SELECTED-ROW(antal_raknare2).
      CREATE omrkonto.
      ASSIGN 
      omrkonto.OMREC = RECID(omrtemp)
      omrkonto.OMRADE = omrtemp.OMRADE
      omrkonto.NAMN = omrtemp.NAMN        
      antal_raknare2 = antal_raknare2 + 1.
   END.
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
   DEFINE INPUT PARAMETER browrec AS RECID NO-UNDO.
   {&BROWSE-NAME}:SET-REPOSITIONED-ROW(35,"ALWAYS") IN FRAME {&FRAME-NAME}.
   REPOSITION {&BROWSE-NAME} TO RECID browrec NO-ERROR.  
   
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
   IF RAD_KONTO = 1 THEN kto = "K1".
   IF RAD_KONTO = 2 THEN kto = "K2".
   IF RAD_KONTO = 3 THEN kto = "K3".
   IF RAD_KONTO = 4 THEN kto = "K4".
   IF RAD_KONTO = 5 THEN kto = "K5".
   IF Guru.Konstanter:appcon THEN DO:
      RUN KONTVALAPP.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
      (INPUT kto,OUTPUT TABLE kontkod).
   END.
   ELSE DO:
      RUN KONTVALAPP.P 
      (INPUT kto,OUTPUT TABLE kontkod).
   END.   
/*    IF Guru.Konstanter:appcon THEN DO:                                  */
/*       RUN KONTVALAPP.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT */
/*       (INPUT kto,OUTPUT TABLE kontkod APPEND).         */
/*    END.                                                */
/*    ELSE DO:                                            */
/*       RUN KONTVALAPP.P                                 */
/*       (INPUT kto,OUTPUT TABLE kontkod APPEND).         */
/*    END.                                                */

   OPEN QUERY BRW_KONTO FOR EACH kontkod USE-INDEX KNR NO-LOCK.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ut_UI C-Win 
PROCEDURE ut_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE VARIABLE pansv AS CHARACTER NO-UNDO.
   CMB_ANSV = INPUT FRAME {&FRAME-NAME} CMB_ANSV .
   FIND FIRST ansvplantemp WHERE ansvplantemp.NAMN = CMB_ANSV NO-ERROR.
   IF AVAILABLE ansvplantemp THEN pansv = ansvplantemp.PERSONALKOD.
   ELSE pansv = "Alla".
   IF vallista = 1 THEN DO:
      {AVBGOM.I}
      RUN PKONTAUTF.W (INPUT pansv).
      {AVBFRAM.I}
   END.   
   ELSE DO:
      IF period = 1 THEN tillar = franar.
      {AVBGOM.I}
      RUN BUDAUTF.W (INPUT pansv).
      {AVBFRAM.I}
   END.
  
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valda_UI C-Win 
PROCEDURE valda_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   IF TOG_ALLA = FALSE THEN DO:
      antal_valda2 = BRW_KONTO:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME}.      
      antal_raknare2 = 1.
      DO WHILE antal_raknare2 LE antal_valda2 :
         status-ok = BRW_KONTO:FETCH-SELECTED-ROW(antal_raknare2).         
         CREATE kkod.
         ASSIGN       
         kkod.KONTO = kontkod.KONTO
         kkod.KONTONR = kontkod.KONTONR
         kkod.BENAMNING = kontkod.BENAMNING   
         antal_raknare2 = antal_raknare2 + 1.
      END.        
   END.
   ELSE DO:
      FOR EACH kontkod:
         CREATE kkod.
         ASSIGN       
         kkod.KONTO = kontkod.KONTO
         kkod.KONTONR = kontkod.KONTONR
         kkod.BENAMNING = kontkod.BENAMNING.
      END.
   END.      
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

