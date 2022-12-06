&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
&Scoped-define WINDOW-NAME WINDOW-1



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS WINDOW-1 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 95/05/02 -  1:43 pm

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
{GLOBVAR2DEL1.I}
&Scoped-define NEW 
&Scoped-define SHARED SHARED
{BESTKUNDALLT.I}
&Scoped-define NEW NEW
{AVDELNINGTEMPT.I}
&Scoped-define NEW 
DEFINE NEW SHARED VARIABLE bestapph AS HANDLE NO-UNDO.
DEFINE NEW SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.   
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE str AS CHARACTER FORMAT "X(80)" NO-UNDO.
DEFINE VARIABLE vemanvandare AS CHARACTER NO-UNDO.
DEFINE VARIABLE valford AS CHARACTER NO-UNDO.
{TIDUTTTNEW.I}   



/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE WINDOW
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-A
&Scoped-define BROWSE-NAME BRW_BEST

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES bestkundallt bestkundextra

/* Definitions for BROWSE BRW_BEST                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_BEST bestkundallt.VIBESTID ~
bestkundallt.BESTNAMN 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_BEST bestkundallt.VIBESTID 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_BEST bestkundallt
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_BEST bestkundallt
&Scoped-define QUERY-STRING-BRW_BEST FOR EACH bestkundallt NO-LOCK ~
    BY bestkundallt.VIBESTID
&Scoped-define OPEN-QUERY-BRW_BEST OPEN QUERY BRW_BEST FOR EACH bestkundallt NO-LOCK ~
    BY bestkundallt.VIBESTID.
&Scoped-define TABLES-IN-QUERY-BRW_BEST bestkundallt
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_BEST bestkundallt


/* Definitions for BROWSE BRW_BESTAVD                                   */
&Scoped-define FIELDS-IN-QUERY-BRW_BESTAVD bestkundextra.VIBESTID ~
bestkundextra.BESTNAMN 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_BESTAVD bestkundextra.VIBESTID 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_BESTAVD bestkundextra
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_BESTAVD bestkundextra
&Scoped-define QUERY-STRING-BRW_BESTAVD FOR EACH bestkundextra NO-LOCK
&Scoped-define OPEN-QUERY-BRW_BESTAVD OPEN QUERY BRW_BESTAVD FOR EACH bestkundextra NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_BESTAVD bestkundextra
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_BESTAVD bestkundextra


/* Definitions for FRAME FRAME-A                                        */
&Scoped-define OPEN-BROWSERS-IN-QUERY-FRAME-A ~
    ~{&OPEN-QUERY-BRW_BEST}~
    ~{&OPEN-QUERY-BRW_BESTAVD}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-45 CMB_BESTAVD BRW_BESTAVD BRW_BEST ~
FBTN_VPER FBTN_SKR BTN_NY BTN_AND BTN_BORT BTN_VISAO BTN_SKR-2 ~
FILL-IN_BESTID FILL-IN_BESTID-2 FILL-IN_ORT FILL-IN_ORT-2 BTN_AVB 
&Scoped-Define DISPLAYED-OBJECTS CMB_BESTAVD FILL-IN_BESTID ~
FILL-IN_BESTID-2 FILL-IN_ORT FILL-IN_ORT-2 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR WINDOW-1 AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AND 
     LABEL "Ändra":L 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_AVB 
     LABEL "Avsluta":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_BORT 
     LABEL "Ta bort":L 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_NY 
     LABEL "Ny":L 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_SKR-2 
     LABEL "Skriv ut" 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_VISAO 
     LABEL "Visa":L 
     SIZE 12 BY 1.

DEFINE BUTTON FBTN_SKR 
     LABEL "Skriv ut alla":L 
     SIZE 14 BY 1.

DEFINE BUTTON FBTN_VPER 
     LABEL "Visa alla":L 
     SIZE 14 BY 1.

DEFINE VARIABLE CMB_BESTAVD AS CHARACTER FORMAT "X(256)":U 
     LABEL "Avdelning" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Alla","Ej kopplad","Allmän"
     DROP-DOWN-LIST
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN_BESTID AS CHARACTER FORMAT "X(17)" 
     LABEL "Best-id" 
     VIEW-AS FILL-IN 
     SIZE 18 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN_BESTID-2 AS CHARACTER FORMAT "X(17)" 
     LABEL "Best-id" 
     VIEW-AS FILL-IN 
     SIZE 18 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN_ORT AS CHARACTER FORMAT "x(40)" 
     LABEL "Namn" 
     VIEW-AS FILL-IN 
     SIZE 18 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN_ORT-2 AS CHARACTER FORMAT "x(40)" 
     LABEL "Namn" 
     VIEW-AS FILL-IN 
     SIZE 18 BY .83 NO-UNDO.

DEFINE RECTANGLE RECT-45
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 64 BY 1.21.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_BEST FOR 
      bestkundallt SCROLLING.

DEFINE QUERY BRW_BESTAVD FOR 
      bestkundextra SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_BEST
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_BEST WINDOW-1 _STRUCTURED
  QUERY BRW_BEST NO-LOCK DISPLAY
      bestkundallt.VIBESTID FORMAT "X(10)":U
      bestkundallt.BESTNAMN COLUMN-LABEL "Namn" FORMAT "x(256)":U
            WIDTH 45
  ENABLE
      bestkundallt.VIBESTID
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 64 BY 19.25
         TITLE "Beställare/Kund".

DEFINE BROWSE BRW_BESTAVD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_BESTAVD WINDOW-1 _STRUCTURED
  QUERY BRW_BESTAVD NO-LOCK DISPLAY
      bestkundextra.VIBESTID FORMAT "X(10)":U
      bestkundextra.BESTNAMN COLUMN-LABEL "Namn" FORMAT "x(256)":U
            WIDTH 45
  ENABLE
      bestkundextra.VIBESTID
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 64 BY 19.25
         TITLE "Beställare/Kund".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     CMB_BESTAVD AT ROW 1.75 COL 24.5 COLON-ALIGNED
     BRW_BESTAVD AT ROW 3.25 COL 1.5
     BRW_BEST AT ROW 3.25 COL 1.5
     FBTN_VPER AT ROW 6.5 COL 69
     FBTN_SKR AT ROW 7.58 COL 69
     BTN_NY AT ROW 23 COL 2
     BTN_AND AT ROW 23 COL 14.63
     BTN_BORT AT ROW 23 COL 27.25
     BTN_VISAO AT ROW 23 COL 39.88
     BTN_SKR-2 AT ROW 23 COL 52.63
     FILL-IN_BESTID AT ROW 25.25 COL 13 COLON-ALIGNED
     FILL-IN_BESTID-2 AT ROW 25.25 COL 13 COLON-ALIGNED
     FILL-IN_ORT AT ROW 25.25 COL 43.38 COLON-ALIGNED
     FILL-IN_ORT-2 AT ROW 25.25 COL 43.38 COLON-ALIGNED
     BTN_AVB AT ROW 25.29 COL 69
     "Sök på:" VIEW-AS TEXT
          SIZE 7.5 BY .83 AT ROW 25.25 COL 2.5
     RECT-45 AT ROW 25.08 COL 1.5
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 84.25 BY 25.88.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: WINDOW
   Temp-Tables and Buffers:
      TABLE: bestkundallt T "?" NO-UNDO temp-db bestkundallt
      TABLE: bestkundextra T "?" NO-UNDO temp-db bestkundextra
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW WINDOW-1 ASSIGN
         HIDDEN             = YES
         TITLE              = "Beställare/Kund"
         HEIGHT             = 26
         WIDTH              = 84.5
         MAX-HEIGHT         = 28.13
         MAX-WIDTH          = 94.63
         VIRTUAL-HEIGHT     = 28.13
         VIRTUAL-WIDTH      = 94.63
         RESIZE             = yes
         SCROLL-BARS        = yes
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW WINDOW-1
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME FRAME-A
                                                                        */
/* BROWSE-TAB BRW_BESTAVD CMB_BESTAVD FRAME-A */
/* BROWSE-TAB BRW_BEST BRW_BESTAVD FRAME-A */
ASSIGN 
       BRW_BESTAVD:HIDDEN  IN FRAME FRAME-A                = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-1)
THEN WINDOW-1:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_BEST
/* Query rebuild information for BROWSE BRW_BEST
     _TblList          = "Temp-Tables.bestkundallt"
     _Options          = "NO-LOCK"
     _OrdList          = "RT9.bestkundallt.VIBESTID|yes"
     _FldNameList[1]   > Temp-Tables.bestkundallt.VIBESTID
"bestkundallt.VIBESTID" ? "X(10)" "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.bestkundallt.BESTNAMN
"bestkundallt.BESTNAMN" "Namn" "x(256)" "character" ? ? ? ? ? ? no ? no no "45" yes no no "U" "" ""
     _Query            is OPENED
*/  /* BROWSE BRW_BEST */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_BESTAVD
/* Query rebuild information for BROWSE BRW_BESTAVD
     _TblList          = "Temp-Tables.bestkundextra"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.bestkundextra.VIBESTID
"bestkundextra.VIBESTID" ? "X(10)" "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.bestkundextra.BESTNAMN
"bestkundextra.BESTNAMN" "Namn" "x(256)" "character" ? ? ? ? ? ? no ? no no "45" yes no no "U" "" ""
     _Query            is OPENED
*/  /* BROWSE BRW_BESTAVD */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME BRW_BEST
&Scoped-define SELF-NAME BRW_BEST
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_BEST WINDOW-1
ON LEFT-MOUSE-DBLCLICK OF BRW_BEST IN FRAME FRAME-A /* Beställare/Kund */
DO:
   RUN andra_UI.
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_BEST WINDOW-1
ON VALUE-CHANGED OF BRW_BEST IN FRAME FRAME-A /* Beställare/Kund */
DO:
  status-ok = BRW_BEST:SELECT-FOCUSED-ROW() NO-ERROR.
  /*FIND FIRST bestkundextra WHERE bestkundextra.BESTID = bestkundallt.BESTID NO-LOCK NO-ERROR.     */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_BESTAVD
&Scoped-define SELF-NAME BRW_BESTAVD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_BESTAVD WINDOW-1
ON LEFT-MOUSE-DBLCLICK OF BRW_BESTAVD IN FRAME FRAME-A /* Beställare/Kund */
DO:
   RUN andra_UI.
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_BESTAVD WINDOW-1
ON VALUE-CHANGED OF BRW_BESTAVD IN FRAME FRAME-A /* Beställare/Kund */
DO:
   status-ok = BRW_BESTAVD:SELECT-FOCUSED-ROW() NO-ERROR.
   FIND FIRST bestkundallt WHERE bestkundallt.BESTID = bestkundextra.BESTID NO-LOCK NO-ERROR.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AND
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AND WINDOW-1
ON CHOOSE OF BTN_AND IN FRAME FRAME-A /* Ändra */
DO:     
   RUN andra_UI.
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB WINDOW-1
ON CHOOSE OF BTN_AVB IN FRAME FRAME-A /* Avsluta */
DO:
  APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_BORT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_BORT WINDOW-1
ON CHOOSE OF BTN_BORT IN FRAME FRAME-A /* Ta bort */
DO:            
   RUN bort_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NY
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NY WINDOW-1
ON CHOOSE OF BTN_NY IN FRAME FRAME-A /* Ny */
DO:
  RUN ny_UI. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_SKR-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_SKR-2 WINDOW-1
ON CHOOSE OF BTN_SKR-2 IN FRAME FRAME-A /* Skriv ut */
DO:       
   skrivut = TRUE.
   RUN skriv_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_SKR-2 WINDOW-1
ON MOUSE-MENU-CLICK OF BTN_SKR-2 IN FRAME FRAME-A /* Skriv ut */
DO:
   RUN SIDLANGD.W.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_VISAO
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_VISAO WINDOW-1
ON CHOOSE OF BTN_VISAO IN FRAME FRAME-A /* Visa */
DO:
   RUN visafak_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_BESTAVD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_BESTAVD WINDOW-1
ON VALUE-CHANGED OF CMB_BESTAVD IN FRAME FRAME-A /* Avdelning */
DO:
   CMB_BESTAVD = INPUT CMB_BESTAVD.  
   IF CMB_BESTAVD = "Alla" THEN DO:
    
      ASSIGN
      FILL-IN_BESTID-2:HIDDEN = TRUE
      FILL-IN_ORT-2:HIDDEN = TRUE
      FILL-IN_BESTID:HIDDEN = FALSE
      FILL-IN_ORT:HIDDEN = FALSE
      BRW_BESTAVD:HIDDEN = TRUE
      BRW_BEST:HIDDEN = FALSE.
   END. 
   ELSE DO:
      ASSIGN
      FILL-IN_BESTID:HIDDEN = TRUE
      FILL-IN_ORT:HIDDEN = TRUE
      FILL-IN_BESTID-2:HIDDEN = FALSE
      FILL-IN_ORT-2:HIDDEN = FALSE
      BRW_BESTAVD:HIDDEN = FALSE
      BRW_BEST:HIDDEN = TRUE.
      IF CMB_BESTAVD = "ALLMÄN" THEN DO:
         valford = 'bestkundextra.AVDELNING = "' + CMB_BESTAVD + '"'.
      END.
      ELSE IF CMB_BESTAVD = "ej kopplad" THEN DO:
         valford = 'bestkundextra.AVDELNING = " "'.
      END.      
      ELSE DO:
         FIND FIRST avdelningtemp WHERE avdelningtemp.AVDELNINGNAMN = CMB_BESTAVD NO-LOCK NO-ERROR.
         valford = 'bestkundextra.AVDELNING = "'  + STRING(avdelningtemp.AVDELNINGNR) + '"'.
      END.
      RUN setcolsortvar_UI IN brwproc[2] (INPUT valford).
      RUN openbdynspec_UI IN brwproc[2]. 
      APPLY "VALUE-CHANGED" TO BRW_BESTAVD.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_SKR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_SKR WINDOW-1
ON CHOOSE OF FBTN_SKR IN FRAME FRAME-A /* Skriv ut alla */
DO:    
   {muswait.i}
   skrivut = TRUE.  
   RUN SKRIVVAL.W (INPUT FALSE).
   IF musz = TRUE THEN DO:
      musz = FALSE.
      skrivut = FALSE.  
   END.
   ELSE DO:
      RUN ut_UI. 
   END.  
   skrivut = FALSE.
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_SKR WINDOW-1
ON MOUSE-MENU-CLICK OF FBTN_SKR IN FRAME FRAME-A /* Skriv ut alla */
DO:
   RUN SIDLANGD.W.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_VPER
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_VPER WINDOW-1
ON CHOOSE OF FBTN_VPER IN FRAME FRAME-A /* Visa alla */
DO:
   skrivut = FALSE.  
   RUN ut_UI.  
   {musarrow.i}   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_BESTID
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_BESTID WINDOW-1
ON LEAVE OF FILL-IN_BESTID IN FRAME FRAME-A /* Best-id */
DO:
   FILL-IN_BESTID = INPUT FILL-IN_BESTID.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_BESTID-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_BESTID-2 WINDOW-1
ON LEAVE OF FILL-IN_BESTID-2 IN FRAME FRAME-A /* Best-id */
DO:
   FILL-IN_BESTID-2 = INPUT FILL-IN_BESTID-2.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_ORT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_ORT WINDOW-1
ON LEAVE OF FILL-IN_ORT IN FRAME FRAME-A /* Namn */
DO:
   FILL-IN_ORT = INPUT FILL-IN_ORT.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_ORT-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_ORT-2 WINDOW-1
ON LEAVE OF FILL-IN_ORT-2 IN FRAME FRAME-A /* Namn */
DO:
   FILL-IN_ORT-2 = INPUT FILL-IN_ORT-2.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_BEST
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK WINDOW-1 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
DO:
   IF VALID-HANDLE(bestapph) THEN DELETE PROCEDURE bestapph.
   {BORTBRWPROC.I}
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
   EMPTY TEMP-TABLE avdelningtemp NO-ERROR.    
   IF Guru.Konstanter:appcon THEN DO:              
      RUN AVDHMT.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT (INPUT-OUTPUT TABLE avdelningtemp).            
   END.
   ELSE DO:      
      RUN AVDHMT.P (INPUT-OUTPUT TABLE avdelningtemp).
   END.
   ASSIGN
   FILL-IN_BESTID-2:LABEL = Guru.Konstanter:gbestk
   FILL-IN_BESTID:LABEL = Guru.Konstanter:gbestk
   /*FILL-IN-TEXT = Guru.Konstanter:gbestk + ":"*/
   BRW_BEST:TITLE = Guru.Konstanter:gbestk
   {&WINDOW-NAME}:TITLE = Guru.Konstanter:gbestl
   bestkundallt.VIBESTID:LABEL IN BROWSE BRW_BEST = Guru.Konstanter:gbestk.
   bestkundextra.VIBESTID:LABEL IN BROWSE BRW_BESTAVD = Guru.Konstanter:gbestk.
   
   EMPTY TEMP-TABLE bestkundallt NO-ERROR. 
  
   FIND FIRST bestkundallt NO-LOCK NO-ERROR.
   IF NOT AVAILABLE bestkundallt THEN DO:    
      bestvad = 1.
      {BESTHMT.I}
   END.
   

   
   RUN enable_UI.   
   FOR EACH avdelningtemp:
      status-ok = CMB_BESTAVD:ADD-LAST(avdelningtemp.AVDELNINGNAMN).
   END.
   IF CMB_BESTAVD:NUM-ITEMS > 18 THEN CMB_BESTAVD:INNER-LINES = 18.
   ELSE CMB_BESTAVD:INNER-LINES = CMB_BESTAVD:NUM-ITEMS.
   CMB_BESTAVD:SCREEN-VALUE = "Alla".
   CMB_BESTAVD = "Alla".
   CMB_BESTAVD:LABEL = Guru.Konstanter:gavdl.
   /*RUN besthmt_UI IN bestapph (OUTPUT TABLE bestkundallt).             */
   
   RUN openbdyn_UI IN brwproc[1] (INPUT "").
   ASSIGN
   FILL-IN_BESTID-2:HIDDEN = TRUE
   FILL-IN_ORT-2:HIDDEN = TRUE
   FILL-IN_BESTID:HIDDEN = FALSE
   FILL-IN_ORT:HIDDEN = FALSE
   BRW_BESTAVD:HIDDEN = TRUE.
   {FRMSIZE.I}     
   {musarrow.i}
   {WIN_M_SLUT.I}
   IF NOT THIS-PROCEDURE:PERSISTENT THEN
   WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE allstartbrw_UI WINDOW-1 
PROCEDURE allstartbrw_UI :
/* -----------------------------------------------------------
  Purpose: Changing screen-value for combo-box CMB_OMR     
  Parameters:  Input = Screen-value for CMB_FOR
  Notes:       
-------------------------------------------------------------*/    
   ASSIGN
   bestkundallt.VIBESTID:READ-ONLY IN BROWSE BRW_BEST = TRUE.
   RUN DYNBRW.P PERSISTENT SET brwproc[1]
      (INPUT BRW_BEST:HANDLE IN FRAME {&FRAME-NAME}).         
   ASSIGN
   bestkundextra.VIBESTID:READ-ONLY IN BROWSE BRW_BESTAVD = TRUE.
   RUN DYNBRW.P PERSISTENT SET brwproc[2]
      (INPUT BRW_BESTAVD:HANDLE IN FRAME {&FRAME-NAME}).         
   RUN addfillin_UI IN brwproc[1] (INPUT FILL-IN_BESTID:HANDLE, INPUT "VIBESTID").
   RUN addfillin_UI IN brwproc[1] (INPUT FILL-IN_ORT:HANDLE, INPUT "BESTNAMN").
   RUN addfillin_UI IN brwproc[2] (INPUT FILL-IN_BESTID-2:HANDLE, INPUT "VIBESTID").
   RUN addfillin_UI IN brwproc[2] (INPUT FILL-IN_ORT-2:HANDLE, INPUT "BESTNAMN").
   IF Guru.Konstanter:appcon THEN DO:
      IF Guru.Konstanter:varforetypval[27] = 0 THEN RUN BESTKUNDAPP.P PERSISTENT SET bestapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      IF Guru.Konstanter:varforetypval[27] = 0 THEN RUN BESTKUNDAPP.P PERSISTENT SET bestapph.
   END.   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE andra_UI WINDOW-1 
PROCEDURE andra_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   {muswait.i} 
   /*status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.*/
   IF NOT AVAILABLE bestkundallt THEN DO:
      RETURN NO-APPLY.
   END.
   
   vemanvandare = bestkundallt.BESTID.   
   RUN BESTNYU.W (INPUT-OUTPUT vemanvandare).     
   IF musz = FALSE THEN DO: 
      DELETE bestkundallt.
      RUN hamtaen_UI IN bestapph (INPUT vemanvandare,OUTPUT TABLE bestkundallt APPEND).                         
      FIND FIRST bestkundextra WHERE bestkundextra.BESTID = vemanvandare NO-ERROR.
      IF AVAILABLE bestkundextra THEN DO:
         DELETE bestkundextra.
      END.
      RUN hamtaenextra_UI IN bestapph (INPUT vemanvandare,OUTPUT TABLE bestkundextra APPEND).
      FIND FIRST bestkundallt WHERE bestkundallt.BESTID = vemanvandare
      NO-LOCK NO-ERROR.   
      IF AVAILABLE bestkundallt THEN DO:
         RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(bestkundallt)).              
         RUN openbdyn_UI IN brwproc[1] (INPUT "").
         RUN lastselectdyn_UI IN brwproc[1].              
      END.
      IF CMB_BESTAVD = "Alla" THEN musz = musz.
      ELSE DO:
         FIND FIRST bestkundextra WHERE bestkundextra.BESTID = vemanvandare
         NO-LOCK NO-ERROR.   
         IF AVAILABLE bestkundextra THEN DO:
            RUN setlastrowid_UI IN brwproc[2] (INPUT ROWID(bestkundextra)).              
            IF CMB_BESTAVD = "ALLMÄN" THEN DO:
               valford = 'bestkundextra.AVDELNING = "' + CMB_BESTAVD + '"'.
            END.
            ELSE IF CMB_BESTAVD = "ej kopplad" THEN DO:
               valford = 'bestkundextra.AVDELNING = " "'.
            END.
            ELSE DO:
               FIND FIRST avdelningtemp WHERE avdelningtemp.AVDELNINGNAMN = CMB_BESTAVD NO-LOCK NO-ERROR.
               valford = 'bestkundextra.AVDELNING = "'  + STRING(avdelningtemp.AVDELNINGNR) + '"'.
            END.
            RUN setcolsortvar_UI IN brwproc[2] (INPUT valford).
            RUN openbdynspec_UI IN brwproc[2].             
            RUN lastselectdyn_UI IN brwproc[2].              
         END.
      END.
      
   END.
      
   musz = FALSE.
   
   {musarrow.i}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE bort_UI WINDOW-1 
PROCEDURE bort_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   /*status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.*/
   IF NOT AVAILABLE bestkundallt THEN DO:
      RETURN NO-APPLY.
   END.
   MESSAGE "Vill du verkligen ta bort denna "  Guru.Konstanter:gbestk " ?"
   VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE bestkundallt.BESTNAMN
   UPDATE answer AS LOGICAL.
   IF answer THEN DO:
      {muswait.i}         
      RUN bestbort_UI IN bestapph (INPUT bestkundallt.BESTID,OUTPUT TABLE felmeddtemp).                   
      FIND FIRST felmeddtemp NO-ERROR.
      IF AVAILABLE felmeddtemp THEN DO:
         MESSAGE felmeddtemp.FELMEDD VIEW-AS ALERT-BOX.
         DELETE felmeddtemp.
         RETURN NO-APPLY.
      END.
      ELSE DO:
          IF BRW_BEST:VISIBLE IN FRAME {&FRAME-NAME} = TRUE THEN DO:
             FIND FIRST bestkundextra WHERE bestkundextra.BESTID = bestkundallt.BESTID NO-LOCK NO-ERROR.
             IF AVAILABLE bestkundextra THEN DELETE bestkundextra.
             DELETE bestkundallt.
             RUN selnextprevrow_UI IN brwproc[1].
             RUN refreshbrw_UI IN brwproc[1].
             RUN lastselectdyn_UI IN brwproc[1].        
          END.
          IF BRW_BESTAVD:VISIBLE = TRUE THEN DO:
             FIND FIRST bestkundallt WHERE bestkundallt.BESTID = bestkundextra.BESTID NO-LOCK NO-ERROR.
             IF AVAILABLE bestkundallt THEN DELETE bestkundallt.
             DELETE bestkundextra.
             RUN selnextprevrow_UI IN brwproc[2].
             RUN refreshbrw_UI IN brwproc[1].
             RUN refreshbrw_UI IN brwproc[2].
             RUN lastselectdyn_UI IN brwproc[2].        
          END.
      END.      
      {musarrow.i}
   END.    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI WINDOW-1  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-1)
  THEN DELETE WIDGET WINDOW-1.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI WINDOW-1  _DEFAULT-ENABLE
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
  DISPLAY CMB_BESTAVD FILL-IN_BESTID FILL-IN_BESTID-2 FILL-IN_ORT FILL-IN_ORT-2 
      WITH FRAME FRAME-A IN WINDOW WINDOW-1.
  ENABLE RECT-45 CMB_BESTAVD BRW_BESTAVD BRW_BEST FBTN_VPER FBTN_SKR BTN_NY 
         BTN_AND BTN_BORT BTN_VISAO BTN_SKR-2 FILL-IN_BESTID FILL-IN_BESTID-2 
         FILL-IN_ORT FILL-IN_ORT-2 BTN_AVB 
      WITH FRAME FRAME-A IN WINDOW WINDOW-1.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ny_UI WINDOW-1 
PROCEDURE ny_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   {muswait.i}   
   
   vemanvandare = ?.
   RUN BESTNYU.W (INPUT-OUTPUT vemanvandare).     
   
   IF musz = FALSE THEN DO: 
      RUN hamtaen_UI IN bestapph (INPUT vemanvandare,OUTPUT TABLE bestkundallt APPEND).                         
      RUN hamtaenextra_UI IN bestapph (INPUT vemanvandare,OUTPUT TABLE bestkundextra APPEND).
      FIND FIRST bestkundallt WHERE bestkundallt.BESTID = vemanvandare
      NO-LOCK NO-ERROR.   
      IF AVAILABLE bestkundallt THEN DO:
         RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(bestkundallt)).              
         RUN openbdyn_UI IN brwproc[1] (INPUT "").
         RUN lastselectdyn_UI IN brwproc[1].     
      END.
      IF CMB_BESTAVD = "Alla" THEN musz = musz.
      ELSE DO:
         FIND FIRST bestkundextra WHERE bestkundextra.BESTID = vemanvandare
         NO-LOCK NO-ERROR.   
         IF AVAILABLE bestkundextra THEN DO:
            RUN setlastrowid_UI IN brwproc[2] (INPUT ROWID(bestkundextra)).              
            IF CMB_BESTAVD = "ALLMÄN" THEN DO:
               valford = 'bestkundextra.AVDELNING = "' + CMB_BESTAVD + '"'.
            END.
            ELSE IF CMB_BESTAVD = "ej kopplad" THEN DO:
               valford = 'bestkundextra.AVDELNING = " "'.
            END.            
            ELSE DO:
               FIND FIRST avdelningtemp WHERE avdelningtemp.AVDELNINGNAMN = CMB_BESTAVD NO-LOCK NO-ERROR.
               valford = 'bestkundextra.AVDELNING = "'  + STRING(avdelningtemp.AVDELNINGNR) + '"'.
            END.
            RUN setcolsortvar_UI IN brwproc[2] (INPUT valford).
            RUN openbdynspec_UI IN brwproc[2].                         
            RUN lastselectdyn_UI IN brwproc[2].              
         END.
      END.
      
   END.
   musz = FALSE.
   
   {musarrow.i}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE skriv_UI WINDOW-1 
PROCEDURE skriv_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   /*status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.*/
   IF NOT AVAILABLE bestkundallt THEN DO:
      RETURN NO-APPLY.
   END.   
   musz = FALSE. 
   skrivut = TRUE.   
   RUN SKRIVVAL.W (INPUT FALSE).   
   IF musz = TRUE THEN DO:      
      musz = FALSE.
      skrivut = FALSE.  
      RETURN.
   END.          
   {AVBGOM.I}       
   RUN viskund_UI IN bestapph (INPUT bestkundallt.BESTID,OUTPUT TABLE tidut).                      
   RUN EKLOGS.P.
   
   {AVBFRAM.I}
   skrivut = FALSE.  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ut_UI WINDOW-1 
PROCEDURE ut_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   RUN visaalla_UI IN bestapph (OUTPUT TABLE tidut).                              
   IF skrivut = FALSE THEN DO:
      {AVBGOM.I}
      vemanvandare = Guru.Konstanter:gbestk.
      RUN UTRAPP.W (INPUT vemanvandare).      
      {AVBFRAM.I}
   END.
   ELSE DO:
      IF musz = TRUE THEN musz = FALSE.
      ELSE RUN EKLOGS.P.                  
   END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE visafak_UI WINDOW-1 
PROCEDURE visafak_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   /*status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.*/
   IF NOT AVAILABLE bestkundallt THEN DO:
      RETURN NO-APPLY.
   END.
   skrivut = FALSE.
   {AVBGOM.I}
   RUN viskund_UI IN bestapph (INPUT bestkundallt.BESTID,OUTPUT TABLE tidut).                      
   vemanvandare = "Visning av " + LC(Guru.Konstanter:gbestk) + " uppläggning".
   RUN UTRAPP.W (INPUT vemanvandare).
   
   {AVBFRAM.I}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

