&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
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
&Scoped-define NEW
{GLOBVAR2DEL1.I}

{OMRTEMPW.I}
DEFINE TEMP-TABLE omrtemp3 NO-UNDO LIKE omrtemp.
DEFINE SHARED VARIABLE omr AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE aonrrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE skrivut AS LOGICAL NO-UNDO. 
DEFINE SHARED VARIABLE regdatum AS DATE NO-UNDO.
DEFINE SHARED VARIABLE bdatum AS DATE NO-UNDO.
DEFINE SHARED VARIABLE avdatum AS DATE NO-UNDO.
DEFINE SHARED VARIABLE utomr AS CHARACTER NO-UNDO. 
DEFINE SHARED VARIABLE SEL_UPP AS CHARACTER INITIAL ? 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     LIST-ITEMS "Arbetsorder-tid-övertid-traktamenten",
                "Tid-övertid-traktamenten / område", 
                "Det.lista Tid-övertid-trakt./område",
                "Arbetsorder-kostnadsreg.-kalkyl",
                "Arbetsorder-personal",
                "Arbetstidens fördelning",
                "Debiteringsgrad-område",
                "Personal-lönetillägg-personal",
                "Område-interna intäkter/kostnader"
                
     SIZE 39.5 BY 9.5
       NO-UNDO.
DEFINE VARIABLE perurvalapph AS HANDLE NO-UNDO.
DEFINE VARIABLE aosok AS CHARACTER FORMAT "X(8)" NO-UNDO. 
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE my1hand AS WIDGET-HANDL NO-UNDO. 
DEFINE VARIABLE uppar AS INTEGER NO-UNDO.
DEFINE VARIABLE antal_valda AS INTEGER NO-UNDO. 
DEFINE VARIABLE antal_raknare AS INTEGER NO-UNDO.
DEFINE VARIABLE fastvrec AS RECID NO-UNDO.

DEFINE SHARED TEMP-TABLE omrtemp2 NO-UNDO LIKE omrtemp.

DEFINE TEMP-TABLE omftemp    
   FIELD OMRADE AS CHARACTER
   FIELD NAMN AS CHARACTER
   FIELD OMREC AS RECID  
   INDEX OMRADE IS PRIMARY OMRADE ASCENDING.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BRW_OMR

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES omrtemp3 omrtemp2

/* Definitions for BROWSE BRW_OMR                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_OMR omrtemp3.OMRADE omrtemp3.NAMN 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_OMR 
&Scoped-define QUERY-STRING-BRW_OMR FOR EACH omrtemp3 NO-LOCK
&Scoped-define OPEN-QUERY-BRW_OMR OPEN QUERY BRW_OMR FOR EACH omrtemp3 NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_OMR omrtemp3
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_OMR omrtemp3


/* Definitions for BROWSE BRW_VOMR                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_VOMR omrtemp2.OMRADE omrtemp2.NAMN 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_VOMR 
&Scoped-define QUERY-STRING-BRW_VOMR FOR EACH omrtemp2 NO-LOCK
&Scoped-define OPEN-QUERY-BRW_VOMR OPEN QUERY BRW_VOMR FOR EACH omrtemp2 NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_VOMR omrtemp2
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_VOMR omrtemp2


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-BRW_OMR}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RAD_PERIOD CMB_ARTAL BTN_NVE BTN_NVE-2 ~
FILL-IN-STARTDAT FILL-IN-STOPPDAT BTN_FVE BTN_FVE-2 TOG_ALLA BRW_OMR ~
BRW_VOMR FBTN_VISA RAD_TYP BTN_OVER FILL-IN-AR BTN_BACK BTN_AVB 
&Scoped-Define DISPLAYED-OBJECTS RAD_PERIOD CMB_ARTAL FILL-IN-STARTDAT ~
FILL-IN-STOPPDAT TOG_ALLA RAD_TYP FILL-IN-AR 

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

DEFINE BUTTON BTN_BACK 
     IMAGE-UP FILE "BILDER\prev-u":U
     LABEL "":L 
     SIZE 4 BY 1.21.

DEFINE BUTTON BTN_FVE 
     LABEL "-" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_FVE-2 
     LABEL "-" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_NVE 
     LABEL "+" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_NVE-2 
     LABEL "+" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_OVER 
     IMAGE-UP FILE "BILDER\next-u":U
     LABEL "":L 
     SIZE 4 BY 1.21.

DEFINE BUTTON FBTN_VISA 
     LABEL "Visa" 
     SIZE 14 BY 1.

DEFINE VARIABLE CMB_ARTAL AS INTEGER FORMAT "9999":U INITIAL 0 
     LABEL "Årtal" 
     VIEW-AS COMBO-BOX INNER-LINES 7
     LIST-ITEMS "0" 
     DROP-DOWN-LIST
     SIZE 12.25 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-AR AS INTEGER FORMAT "9999":U INITIAL ? 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE FILL-IN-STARTDAT AS DATE FORMAT "99/99/99":U 
     LABEL "Från" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE FILL-IN-STOPPDAT AS DATE FORMAT "99/99/99":U 
     LABEL "Till" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE RAD_PERIOD AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Visning per år", 1,
"Visning per period", 2
     SIZE 21.5 BY 2.75 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE RAD_TYP AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Intäkter", 1,
"Kostnader", 2
     SIZE 12 BY 3.08 NO-UNDO.

DEFINE VARIABLE TOG_ALLA AS LOGICAL INITIAL no 
     LABEL "Alla" 
     VIEW-AS TOGGLE-BOX
     SIZE 15.5 BY .88 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_OMR FOR 
      omrtemp3 SCROLLING.

DEFINE QUERY BRW_VOMR FOR 
      omrtemp2 SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_OMR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_OMR C-Win _STRUCTURED
  QUERY BRW_OMR NO-LOCK DISPLAY
      omrtemp3.OMRADE COLUMN-LABEL "Område" FORMAT "X(6)":U
      omrtemp3.NAMN COLUMN-LABEL "Namn" FORMAT "X(20)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 33.5 BY 11.75
         TITLE "Vallista områden".

DEFINE BROWSE BRW_VOMR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_VOMR C-Win _STRUCTURED
  QUERY BRW_VOMR DISPLAY
      omrtemp2.OMRADE COLUMN-LABEL "Område" FORMAT "X(6)":U
      omrtemp2.NAMN COLUMN-LABEL "Namn" FORMAT "X(20)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 33.5 BY 11.75
         TITLE "Valda områden".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     RAD_PERIOD AT ROW 2 COL 1.5 NO-LABEL
     CMB_ARTAL AT ROW 2.08 COL 37.38
     BTN_NVE AT ROW 3.17 COL 41.13
     BTN_NVE-2 AT ROW 3.17 COL 59.88
     FILL-IN-STARTDAT AT ROW 3.63 COL 29.5 COLON-ALIGNED
     FILL-IN-STOPPDAT AT ROW 3.63 COL 48.25 COLON-ALIGNED
     BTN_FVE AT ROW 4 COL 41.13
     BTN_FVE-2 AT ROW 4 COL 59.88
     TOG_ALLA AT ROW 5.13 COL 1.5
     BRW_OMR AT ROW 6.75 COL 1.5
     BRW_VOMR AT ROW 6.75 COL 39.88
     FBTN_VISA AT ROW 8 COL 75.5
     RAD_TYP AT ROW 9.5 COL 51 NO-LABEL
     BTN_OVER AT ROW 11.17 COL 35.63
     FILL-IN-AR AT ROW 13.75 COL 50 COLON-ALIGNED NO-LABEL
     BTN_BACK AT ROW 14.67 COL 35.63
     BTN_AVB AT ROW 17.5 COL 75.5
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 89.63 BY 18.04.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Temp-Tables and Buffers:
      TABLE: omrtemp T "?" NO-UNDO temp-db omrtemp
      TABLE: omrtemp2 T "?" NO-UNDO temp-db omrtemp2
      TABLE: omrtemp3 T "?" NO-UNDO temp-db omrtemp3
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "<insert window title>"
         HEIGHT             = 17.96
         WIDTH              = 89.88
         MAX-HEIGHT         = 20.79
         MAX-WIDTH          = 100
         VIRTUAL-HEIGHT     = 20.79
         VIRTUAL-WIDTH      = 100
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
/* BROWSE-TAB BRW_VOMR BRW_OMR DEFAULT-FRAME */
ASSIGN 
       BRW_OMR:ALLOW-COLUMN-SEARCHING IN FRAME DEFAULT-FRAME = TRUE.

ASSIGN 
       BRW_VOMR:ALLOW-COLUMN-SEARCHING IN FRAME DEFAULT-FRAME = TRUE.

ASSIGN 
       BTN_FVE:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

ASSIGN 
       BTN_FVE-2:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

ASSIGN 
       BTN_NVE:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

ASSIGN 
       BTN_NVE-2:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR COMBO-BOX CMB_ARTAL IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
ASSIGN 
       FILL-IN-AR:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-STARTDAT IN FRAME DEFAULT-FRAME
   SHARED                                                               */
ASSIGN 
       FILL-IN-STARTDAT:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-STOPPDAT IN FRAME DEFAULT-FRAME
   SHARED                                                               */
ASSIGN 
       FILL-IN-STOPPDAT:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR RADIO-SET RAD_PERIOD IN FRAME DEFAULT-FRAME
   SHARED                                                               */
/* SETTINGS FOR RADIO-SET RAD_TYP IN FRAME DEFAULT-FRAME
   SHARED                                                               */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_OMR
/* Query rebuild information for BROWSE BRW_OMR
     _TblList          = "Temp-Tables.omrtemp3"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.omrtemp3.OMRADE
"OMRADE" "Område" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.omrtemp3.NAMN
"NAMN" "Namn" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is OPENED
*/  /* BROWSE BRW_OMR */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_VOMR
/* Query rebuild information for BROWSE BRW_VOMR
     _TblList          = "Temp-Tables.omrtemp2"
     _FldNameList[1]   > Temp-Tables.omrtemp2.OMRADE
"omrtemp2.OMRADE" "Område" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.omrtemp2.NAMN
"omrtemp2.NAMN" "Namn" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_VOMR */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* <insert window title> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* <insert window title> */
DO:
  /* This event will close the window and terminate the procedure.  */
  IF VALID-HANDLE(perurvalapph) THEN DELETE PROCEDURE perurvalapph.
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB C-Win
ON CHOOSE OF BTN_AVB IN FRAME DEFAULT-FRAME /* Avsluta */
DO:
   APPLY "END-ERROR":U TO SELF.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_FVE
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_FVE C-Win
ON CHOOSE OF BTN_FVE IN FRAME DEFAULT-FRAME /* - */
DO: 
   ASSIGN
   FILL-IN-STARTDAT = INPUT FILL-IN-STARTDAT.   
   FILL-IN-STARTDAT = FILL-IN-STARTDAT - 1.      
   DISPLAY FILL-IN-STARTDAT WITH FRAME {&FRAME-NAME}.     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_FVE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_FVE-2 C-Win
ON CHOOSE OF BTN_FVE-2 IN FRAME DEFAULT-FRAME /* - */
DO: 
   ASSIGN
   FILL-IN-STOPPDAT = INPUT FILL-IN-STOPPDAT.   
   FILL-IN-STOPPDAT = FILL-IN-STOPPDAT - 1.      
   DISPLAY FILL-IN-STOPPDAT WITH FRAME {&FRAME-NAME}.     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NVE
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NVE C-Win
ON CHOOSE OF BTN_NVE IN FRAME DEFAULT-FRAME /* + */
DO:   
   ASSIGN
   FILL-IN-STARTDAT = INPUT FILL-IN-STARTDAT.   
   FILL-IN-STARTDAT = FILL-IN-STARTDAT + 1.        
   DISPLAY FILL-IN-STARTDAT WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NVE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NVE-2 C-Win
ON CHOOSE OF BTN_NVE-2 IN FRAME DEFAULT-FRAME /* + */
DO:   
   ASSIGN
   FILL-IN-STOPPDAT = INPUT FILL-IN-STOPPDAT.   
   FILL-IN-STOPPDAT = FILL-IN-STOPPDAT + 1.        
   DISPLAY FILL-IN-STOPPDAT WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_ARTAL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_ARTAL C-Win
ON LEAVE OF CMB_ARTAL IN FRAME DEFAULT-FRAME /* Årtal */
DO:                            
   ASSIGN
   CMB_ARTAL = INPUT CMB_ARTAL
   FILL-IN-AR = INPUT CMB_ARTAL.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_ARTAL C-Win
ON VALUE-CHANGED OF CMB_ARTAL IN FRAME DEFAULT-FRAME /* Årtal */
DO:                           
   ASSIGN
   CMB_ARTAL = INPUT CMB_ARTAL
   FILL-IN-AR = INPUT CMB_ARTAL.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_VISA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_VISA C-Win
ON CHOOSE OF FBTN_VISA IN FRAME DEFAULT-FRAME /* Visa */
DO:   
   IF TOG_ALLA = FALSE THEN DO:
      FIND FIRST omrtemp2 NO-ERROR.
      IF NOT AVAILABLE omrtemp2 THEN DO:
         MESSAGE "Välj ett " + LC(Guru.Konstanter:gomrl) + "! "VIEW-AS ALERT-BOX.
         RETURN NO-APPLY.
      END.   
   END.
   IF TOG_ALLA = TRUE THEN DO:
      FOR EACH omrtemp3:      
         CREATE omrtemp2.
         ASSIGN     
         omrtemp2.OMRADE = omrtemp3.OMRADE
         omrtemp2.NAMN = omrtemp3.NAMN.
      END.
   END.
   IF TOG_ALLA = TRUE THEN utomr = "ALLA".
   ELSE DO:
      FIND FIRST omrtemp2 NO-ERROR.
      utomr = omrtemp2.OMRADE.
   END.   
   IF RAD_PERIOD = 1 THEN
      bdatum = DATE(01,01,FILL-IN-AR).
   ELSE DO:
      ASSIGN
      bdatum = FILL-IN-STARTDAT
      avdatum = FILL-IN-STOPPDAT.
   END.    
   skrivut = FALSE.  
   APPLY "GO" TO FBTN_VISA IN FRAME {&FRAME-NAME}.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_VISA C-Win
ON GO OF FBTN_VISA IN FRAME DEFAULT-FRAME /* Visa */
DO:
   {muswait.i}
   RUN PROCUTFU.W.          
   {musarrow.i}
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-AR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-AR C-Win
ON LEAVE OF FILL-IN-AR IN FRAME DEFAULT-FRAME
DO:
   FILL-IN-AR = INPUT FILL-IN-AR.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-STARTDAT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-STARTDAT C-Win
ON LEAVE OF FILL-IN-STARTDAT IN FRAME DEFAULT-FRAME /* Från */
DO:
  FILL-IN-STARTDAT = INPUT FILL-IN-STARTDAT.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-STARTDAT C-Win
ON MOUSE-MENU-CLICK OF FILL-IN-STARTDAT IN FRAME DEFAULT-FRAME /* Från */
DO:
   ASSIGN
   FILL-IN-STARTDAT = INPUT FILL-IN-STARTDAT
   Guru.GlobalaVariabler:regdatum = INPUT FILL-IN-STARTDAT.
   RUN AlmanBtn.w.
   FILL-IN-STARTDAT = Guru.GlobalaVariabler:regdatum.
   DISPLAY FILL-IN-STARTDAT WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-STOPPDAT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-STOPPDAT C-Win
ON LEAVE OF FILL-IN-STOPPDAT IN FRAME DEFAULT-FRAME /* Till */
DO:
  FILL-IN-STOPPDAT = INPUT FILL-IN-STOPPDAT.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-STOPPDAT C-Win
ON MOUSE-MENU-CLICK OF FILL-IN-STOPPDAT IN FRAME DEFAULT-FRAME /* Till */
DO:
   ASSIGN
   FILL-IN-STOPPDAT = INPUT FILL-IN-STOPPDAT
   Guru.GlobalaVariabler:regdatum = INPUT FILL-IN-STOPPDAT.
   RUN AlmanBtn.w.
   FILL-IN-STOPPDAT = Guru.GlobalaVariabler:regdatum.
   DISPLAY FILL-IN-STOPPDAT WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RAD_PERIOD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RAD_PERIOD C-Win
ON VALUE-CHANGED OF RAD_PERIOD IN FRAME DEFAULT-FRAME
DO:
   RAD_PERIOD = INPUT RAD_PERIOD.
   RUN hide_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RAD_TYP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RAD_TYP C-Win
ON VALUE-CHANGED OF RAD_TYP IN FRAME DEFAULT-FRAME
DO:
   RAD_TYP = INPUT RAD_TYP.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_ALLA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_ALLA C-Win
ON VALUE-CHANGED OF TOG_ALLA IN FRAME DEFAULT-FRAME /* Alla */
DO:
  TOG_ALLA = INPUT TOG_ALLA.
  IF TOG_ALLA = TRUE THEN DO:
     BRW_OMR:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
     BRW_VOMR:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
     BTN_OVER:HIDDEN IN FRAME {&FRAME-NAME} = TRUE. 
     BTN_BACK:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
  END.
  ELSE DO:
     BRW_OMR:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
     BRW_VOMR:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
     BTN_OVER:HIDDEN IN FRAME {&FRAME-NAME} = FALSE. 
     BTN_BACK:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
  END.
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_OMR
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
   IF VALID-HANDLE(perurvalapph) THEN DELETE PROCEDURE perurvalapph.
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
   EMPTY TEMP-TABLE omrtemp2 NO-ERROR.    
   EMPTY TEMP-TABLE omrtemp3 NO-ERROR. 
   RUN omr_UI IN perurvalapph (OUTPUT TABLE omrtemp3).  
   IF RAD_PERIOD = 1 THEN DO:
      ASSIGN
      FILL-IN-STARTDAT = DATE(01,01,YEAR(TODAY))
      FILL-IN-STOPPDAT = TODAY.
   END.
   RUN enable_UI.       
   
   FILL-IN-AR:HIDDEN = TRUE.
   RUN hide_UI.
   ASSIGN C-win:TITLE = "Lista " + SEL_UPP + ". Välj " + LC(Guru.Konstanter:gomrl) + ".".
   {FRMSIZE.I}
   {musarrow.i}
   {ARTALBORT.I}
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
   RUN DYNBRW.P PERSISTENT SET brwproc[{&LEFT-BROWSE}]
      (INPUT BRW_OMR:HANDLE IN FRAME {&FRAME-NAME}).  
   RUN DYNBRW.P PERSISTENT SET brwproc[{&RIGHT-BROWSE}]
      (INPUT BRW_VOMR:HANDLE IN FRAME {&FRAME-NAME}).  
   
   RUN DYNARROW.P PERSISTENT SET brwproc[{&ARROWS}]
      (INPUT BRW_OMR:HANDLE, INPUT BRW_VOMR:HANDLE ,
       INPUT BTN_OVER:HANDLE, INPUT ?,
       INPUT ?, INPUT BTN_BACK:HANDLE).
   IF Guru.Konstanter:appcon THEN DO:
      RUN PERURVALAPP.P PERSISTENT SET perurvalapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN PERURVALAPP.P PERSISTENT SET perurvalapph.
   END.
   RUN anvglob_UI IN perurvalapph (INPUT Guru.Konstanter:globanv).    
   
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
  DISPLAY RAD_PERIOD CMB_ARTAL FILL-IN-STARTDAT FILL-IN-STOPPDAT TOG_ALLA 
          RAD_TYP FILL-IN-AR 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RAD_PERIOD CMB_ARTAL BTN_NVE BTN_NVE-2 FILL-IN-STARTDAT 
         FILL-IN-STOPPDAT BTN_FVE BTN_FVE-2 TOG_ALLA BRW_OMR BRW_VOMR FBTN_VISA 
         RAD_TYP BTN_OVER FILL-IN-AR BTN_BACK BTN_AVB 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE hide_UI C-Win 
PROCEDURE hide_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/ 
   IF omr = TRUE THEN DO:
      RAD_TYP:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
   END.
   ELSE DO:
      RAD_TYP:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
   END.   
   IF RAD_PERIOD = 1 THEN DO:            
      ASSIGN
      FILL-IN-STARTDAT:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      FILL-IN-STOPPDAT:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      BTN_FVE:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      BTN_FVE-2:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      BTN_NVE:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      BTN_NVE-2:HIDDEN IN FRAME {&FRAME-NAME} = TRUE      
      CMB_ARTAL:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.     
   END.
   ELSE DO:                                             
      ASSIGN
      FILL-IN-STARTDAT:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      FILL-IN-STOPPDAT:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      BTN_FVE:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      BTN_FVE-2:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      BTN_NVE:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      BTN_NVE-2:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      CMB_ARTAL:HIDDEN IN FRAME {&FRAME-NAME} = FALSE. 
   END.  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valdafast_UI C-Win 
PROCEDURE valdafast_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   OPEN QUERY BRW_VOMR FOR EACH omrtemp2 NO-LOCK.
   FIND FIRST omrtemp2 NO-LOCK NO-ERROR.
   musz = TRUE.   
   IF AVAILABLE omrtemp2 THEN DO:
      musz = FALSE.      
   END.     
   IF musz = TRUE THEN DO:
      musz = FALSE.
      BRW_VOMR:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
   END. 
   ELSE DO:
      ENABLE BRW_VOMR WITH FRAME {&FRAME-NAME}.
      BRW_VOMR:HIDDEN = FALSE.
   END. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

