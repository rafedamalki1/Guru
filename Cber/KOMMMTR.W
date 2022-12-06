&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
&Scoped-define WINDOW-NAME WINDOW-1


/* Temp-Table and Buffer definitions                                    */
 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS WINDOW-1 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 10/15/96 -  1:50 pm

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
DEFINE NEW SHARED VARIABLE mtrl_rowid AS ROWID NO-UNDO.
DEFINE SHARED VARIABLE vald_kundlev AS CHARACTER NO-UNDO.

DEFINE VARIABLE sok AS LOGICAL NO-UNDO.
DEFINE VARIABLE filnamn AS CHARACTER NO-UNDO.
DEFINE VARIABLE OKvald AS LOGICAL NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE aosok AS CHARACTER FORMAT "X(40)" NO-UNDO.
DEFINE VARIABLE posok AS CHARACTER FORMAT "X(11)" NO-UNDO.
DEFINE VARIABLE antal_valda AS INTEGER NO-UNDO.
DEFINE VARIABLE antal_raknare AS INTEGER NO-UNDO. 
DEFINE VARIABLE mtrlbapph AS HANDLE NO-UNDO.
DEFINE VARIABLE numrows AS INTEGER NO-UNDO.

DEFINE VARIABLE anyprintquery AS CHARACTER NO-UNDO.
DEFINE VARIABLE forstaenr AS CHARACTER NO-UNDO.
DEFINE VARIABLE delquery1 AS CHARACTER NO-UNDO.
DEFINE VARIABLE delquery2 AS CHARACTER NO-UNDO.
DEFINE VARIABLE kollenr AS CHARACTER NO-UNDO.
DEFINE VARIABLE felmedd AS CHARACTER NO-UNDO.
DEFINE VARIABLE sprow AS ROWID      NO-UNDO.
DEFINE VARIABLE uppdat AS LOGICAL NO-UNDO.
DEFINE VARIABLE namnvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE kollev AS LOGICAL NO-UNDO.
DEFINE VARIABLE enrvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE kollfel AS LOGICAL NO-UNDO.
DEFINE VARIABLE enrben AS CHARACTER NO-UNDO.
DEFINE VARIABLE vald_lev AS CHARACTER NO-UNDO.
DEFINE VARIABLE laddaproch AS HANDLE NO-UNDO.
DEFINE VARIABLE entryanv AS LOGICAL NO-UNDO.
DEFINE VARIABLE levinlast AS CHARACTER NO-UNDO.
DEFINE VARIABLE fildir AS CHARACTER NO-UNDO.
DEFINE VARIABLE mappvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE felfil AS CHARACTER NO-UNDO.
DEFINE VARIABLE kolllkod AS CHARACTER NO-UNDO.
DEFINE VARIABLE edataapph AS HANDLE NO-UNDO.
{EXTRADATA.I}
{DEFSOK.I}
&Scoped-define NEW NEW 
&Scoped-define SHARED SHARED
{SOKDEF.I}
{MTRLKOMTEMP.I}
&Scoped-define NEW 
&Scoped-define SHARED 
{MTRLTEMP.I}
{LEVTEMP.I}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE WINDOW
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A
&Scoped-define BROWSE-NAME BRW_HLEV

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES mtrltemp mtrlkomtemp

/* Definitions for BROWSE BRW_HLEV                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_HLEV mtrltemp.Enr mtrltemp.Benamning ~
mtrltemp.Enhet mtrltemp.NPRIS 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_HLEV 
&Scoped-define QUERY-STRING-BRW_HLEV FOR EACH mtrltemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_HLEV OPEN QUERY BRW_HLEV FOR EACH mtrltemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_HLEV mtrltemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_HLEV mtrltemp


/* Definitions for BROWSE BRW_VLEV                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_VLEV mtrlkomtemp.ENR ~
mtrlkomtemp.Benamning mtrlkomtemp.KOMMENTAR 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_VLEV mtrlkomtemp.KOMMENTAR 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_VLEV mtrlkomtemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_VLEV mtrlkomtemp
&Scoped-define QUERY-STRING-BRW_VLEV FOR EACH mtrlkomtemp NO-LOCK ~
    BY ersattemp.NAMN
&Scoped-define OPEN-QUERY-BRW_VLEV OPEN QUERY BRW_VLEV FOR EACH mtrlkomtemp NO-LOCK ~
    BY ersattemp.NAMN.
&Scoped-define TABLES-IN-QUERY-BRW_VLEV mtrlkomtemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_VLEV mtrlkomtemp


/* Definitions for FRAME FRAME-A                                        */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BRW_VLEV IMAGE-4 IMAGE-16 IMAGE-17 ~
FILL-IN-ENR FBTN_IEXC RAD_SOK BTN_NER BTN_UPP BRW_HLEV CMB_LEV FILL-IN-KOD ~
FILL-IN-BEN-2 RAD_SOK-2 BTN_AVB 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-ENR RAD_SOK CMB_LEV FILL-IN-KOD ~
RAD_SOK-2 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR WINDOW-1 AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE MENU POPUP-MENU-BRW_HLEV 
       MENU-ITEM m_Pris_information2 LABEL "Satsinformation"
       MENU-ITEM m_AvmarkeraHLEV LABEL "Avmarkera"     .


/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB 
     LABEL "Avsluta" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_NER 
     IMAGE-UP FILE "BILDER\redpilner.bmp":U
     LABEL "Ner" 
     SIZE 4 BY 1.5 TOOLTIP "Markerade tas bort från vallistan".

DEFINE BUTTON BTN_UPP 
     IMAGE-UP FILE "BILDER\pilupp":U
     LABEL "" 
     SIZE 4 BY 1.5 TOOLTIP "Markerade väljs".

DEFINE BUTTON FBTN_IEXC 
     LABEL "Import Exel" 
     SIZE 14 BY 1 TOOLTIP "Enr i första kolumnen, kommentar i andra kolumnen".

DEFINE VARIABLE CMB_LEV AS CHARACTER FORMAT "X(40)":U 
     LABEL "Leverantörer" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "0" 
     DROP-DOWN-LIST
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-BEN-2 AS CHARACTER FORMAT "X(40)":U 
     LABEL "Benämning" 
     VIEW-AS FILL-IN 
     SIZE 28.88 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN-ENR AS CHARACTER FORMAT "X(11)":U 
     LABEL "Enr" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN-KOD AS CHARACTER FORMAT "X(11)":U 
     LABEL "Enr" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .83 NO-UNDO.

DEFINE IMAGE IMAGE-16
     FILENAME "BILDER\sokpa.gif":U CONVERT-3D-COLORS
     SIZE 8 BY .83.

DEFINE IMAGE IMAGE-17
     FILENAME "BILDER\sokpa.gif":U CONVERT-3D-COLORS
     SIZE 8 BY .83.

DEFINE IMAGE IMAGE-4
     FILENAME "BILDER\sokpa.gif":U CONVERT-3D-COLORS
     SIZE 8 BY .83.

DEFINE VARIABLE RAD_SOK AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Början", 1,
"Någonstans", 2,
"Slutet", 3
     SIZE 35.5 BY .75 NO-UNDO.

DEFINE VARIABLE RAD_SOK-2 AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Början", 1,
"Någonstans", 2,
"Slutet", 3
     SIZE 35.5 BY .75 NO-UNDO.

DEFINE RECTANGLE RECT-22
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 72.5 BY 2.25
     BGCOLOR 8 .

DEFINE RECTANGLE RECT_STOR
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 46 BY 2
     BGCOLOR 8 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_HLEV FOR 
      mtrltemp SCROLLING.

DEFINE QUERY BRW_VLEV FOR 
      mtrlkomtemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_HLEV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_HLEV WINDOW-1 _STRUCTURED
  QUERY BRW_HLEV NO-LOCK DISPLAY
      mtrltemp.Enr FORMAT "X(11)":U
      mtrltemp.Benamning FORMAT "x(256)":U WIDTH 27
      mtrltemp.Enhet FORMAT "x(5)":U
      mtrltemp.NPRIS FORMAT ">>>>99.99":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING MULTIPLE SIZE 74 BY 9.75
         TITLE "Materiel".

DEFINE BROWSE BRW_VLEV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_VLEV WINDOW-1 _STRUCTURED
  QUERY BRW_VLEV NO-LOCK DISPLAY
      mtrlkomtemp.ENR COLUMN-LABEL "Enr" FORMAT "X(11)":U
      mtrlkomtemp.Benamning COLUMN-LABEL "Benämning" FORMAT "x(256)":U
            WIDTH 25
      mtrlkomtemp.KOMMENTAR COLUMN-LABEL "Kommentarer" FORMAT "X(94)":U
  ENABLE
      mtrlkomtemp.KOMMENTAR
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING MULTIPLE SIZE 122.5 BY 11.17
         TITLE "Kommentar till materiel. Varje post sparas när du lämnar raden.".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     BRW_VLEV AT ROW 1 COL 1.5
     FILL-IN-ENR AT ROW 12.38 COL 55 COLON-ALIGNED
     FBTN_IEXC AT ROW 12.88 COL 110
     RAD_SOK AT ROW 13.33 COL 52 NO-LABEL
     BTN_NER AT ROW 13.71 COL 23
     BTN_UPP AT ROW 13.71 COL 28
     BRW_HLEV AT ROW 15.75 COL 1.5
     CMB_LEV AT ROW 25.67 COL 22.63 COLON-ALIGNED
     FILL-IN-KOD AT ROW 27 COL 14 COLON-ALIGNED
     FILL-IN-BEN-2 AT ROW 27 COL 42.5 COLON-ALIGNED
     RAD_SOK-2 AT ROW 28 COL 10.38 NO-LABEL
     BTN_AVB AT ROW 28 COL 110.5
     RECT_STOR AT ROW 12.25 COL 43
     RECT-22 AT ROW 26.79 COL 1.5
     IMAGE-4 AT ROW 20.04 COL 48.63
     IMAGE-16 AT ROW 12.5 COL 43.5
     IMAGE-17 AT ROW 27.04 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 123.75 BY 28.29.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: WINDOW
   Temp-Tables and Buffers:
      TABLE: mtrlkomtemp T "?" NO-UNDO temp-db mtrlkomtemp
      TABLE: mtrltemp T "?" NO-UNDO temp-db mtrltemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW WINDOW-1 ASSIGN
         HIDDEN             = YES
         TITLE              = "Kommentarer till materiel"
         HEIGHT             = 28.42
         WIDTH              = 125
         MAX-HEIGHT         = 28.42
         MAX-WIDTH          = 125
         VIRTUAL-HEIGHT     = 28.42
         VIRTUAL-WIDTH      = 125
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
/* SETTINGS FOR WINDOW WINDOW-1
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME FRAME-A
   FRAME-NAME                                                           */
/* BROWSE-TAB BRW_VLEV 1 FRAME-A */
/* BROWSE-TAB BRW_HLEV BTN_UPP FRAME-A */
ASSIGN 
       BRW_HLEV:HIDDEN  IN FRAME FRAME-A                = TRUE
       BRW_HLEV:POPUP-MENU IN FRAME FRAME-A             = MENU POPUP-MENU-BRW_HLEV:HANDLE
       BRW_HLEV:MAX-DATA-GUESS IN FRAME FRAME-A         = 1000
       BRW_HLEV:ALLOW-COLUMN-SEARCHING IN FRAME FRAME-A = TRUE
       BRW_HLEV:COLUMN-RESIZABLE IN FRAME FRAME-A       = TRUE.

ASSIGN 
       BRW_VLEV:MAX-DATA-GUESS IN FRAME FRAME-A         = 40000
       BRW_VLEV:ALLOW-COLUMN-SEARCHING IN FRAME FRAME-A = TRUE.

ASSIGN 
       CMB_LEV:HIDDEN IN FRAME FRAME-A           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-BEN-2 IN FRAME FRAME-A
   NO-DISPLAY                                                           */
ASSIGN 
       FILL-IN-BEN-2:HIDDEN IN FRAME FRAME-A           = TRUE.

ASSIGN 
       FILL-IN-KOD:HIDDEN IN FRAME FRAME-A           = TRUE.

/* SETTINGS FOR RECTANGLE RECT-22 IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT_STOR IN FRAME FRAME-A
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-1)
THEN WINDOW-1:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_HLEV
/* Query rebuild information for BROWSE BRW_HLEV
     _TblList          = "Temp-Tables.mtrltemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   = Temp-Tables.mtrltemp.Enr
     _FldNameList[2]   > Temp-Tables.mtrltemp.Benamning
"mtrltemp.Benamning" ? "x(256)" "character" ? ? ? ? ? ? no ? no no "27" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   = Temp-Tables.mtrltemp.Enhet
     _FldNameList[4]   = Temp-Tables.mtrltemp.NPRIS
     _Query            is NOT OPENED
*/  /* BROWSE BRW_HLEV */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_VLEV
/* Query rebuild information for BROWSE BRW_VLEV
     _TblList          = "Temp-Tables.mtrlkomtemp"
     _Options          = "NO-LOCK"
     _OrdList          = "Temp-Tables.ersattemp.NAMN|yes"
     _FldNameList[1]   > Temp-Tables.mtrlkomtemp.ENR
"mtrlkomtemp.ENR" "Enr" "X(11)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.mtrlkomtemp.Benamning
"mtrlkomtemp.Benamning" "Benämning" "x(256)" "character" ? ? ? ? ? ? no ? no no "25" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.mtrlkomtemp.KOMMENTAR
"mtrlkomtemp.KOMMENTAR" "Kommentarer" "X(94)" "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_VLEV */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME BRW_VLEV
&Scoped-define SELF-NAME BRW_VLEV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_VLEV WINDOW-1
ON ENTRY OF BRW_VLEV IN FRAME FRAME-A /* Kommentar till materiel. Varje post sparas när du lämnar raden. */
DO:
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_VLEV WINDOW-1
ON LEAVE OF BRW_VLEV IN FRAME FRAME-A /* Kommentar till materiel. Varje post sparas när du lämnar raden. */
DO:
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_VLEV WINDOW-1
ON ROW-LEAVE OF BRW_VLEV IN FRAME FRAME-A /* Kommentar till materiel. Varje post sparas när du lämnar raden. */
DO:   
  IF AVAILABLE mtrlkomtemp THEN DO:
     RUN visa_UI.    
     EMPTY TEMP-TABLE emtrlkomtemp NO-ERROR.
     CREATE emtrlkomtemp.          
     BUFFER-COPY mtrlkomtemp TO emtrlkomtemp. 
     RUN admsparmtrlk_UI IN mtrlbapph (INPUT TABLE emtrlkomtemp ). 
     EMPTY TEMP-TABLE emtrlkomtemp NO-ERROR. 
     RUN visa_UI.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_VLEV WINDOW-1
ON VALUE-CHANGED OF BRW_VLEV IN FRAME FRAME-A /* Kommentar till materiel. Varje post sparas när du lämnar raden. */
DO:
   status-ok = BRW_VLEV:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mtrlkomtemp.ENR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mtrlkomtemp.ENR BRW_VLEV _BROWSE-COLUMN WINDOW-1
ON ENTRY OF mtrlkomtemp.ENR IN BROWSE BRW_VLEV /* Enr */
DO:
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mtrlkomtemp.ENR BRW_VLEV _BROWSE-COLUMN WINDOW-1
ON LEAVE OF mtrlkomtemp.ENR IN BROWSE BRW_VLEV /* Enr */
DO: 
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME mtrlkomtemp.KOMMENTAR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mtrlkomtemp.KOMMENTAR BRW_VLEV _BROWSE-COLUMN WINDOW-1
ON ENTRY OF mtrlkomtemp.KOMMENTAR IN BROWSE BRW_VLEV /* Kommentarer */
DO:
  IF AVAILABLE mtrlkomtemp THEN RUN visa_UI.
     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL mtrlkomtemp.KOMMENTAR BRW_VLEV _BROWSE-COLUMN WINDOW-1
ON LEAVE OF mtrlkomtemp.KOMMENTAR IN BROWSE BRW_VLEV /* Kommentarer */
DO:
   IF AVAILABLE mtrlkomtemp THEN DO:     
      IF mtrlkomtemp.KOMMENTAR NE INPUT BROWSE BRW_VLEV mtrlkomtemp.KOMMENTAR THEN DO:         
         IF mtrlkomtemp.ENR NE ""  THEN DO:
            mtrlkomtemp.KOMMENTAR = INPUT BROWSE BRW_VLEV mtrlkomtemp.KOMMENTAR. 
            RUN visa_UI.            
         END.
      END.   
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB WINDOW-1
ON CHOOSE OF BTN_AVB IN FRAME FRAME-A /* Avsluta */
DO:
  RUN bortblank_UI IN mtrlbapph.
  APPLY "CLOSE":U TO THIS-PROCEDURE.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NER
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NER WINDOW-1
ON CHOOSE OF BTN_NER IN FRAME FRAME-A /* Ner */
DO:
   ASSIGN
   antal_valda = BRW_VLEV:NUM-SELECTED-ROWS
   antal_raknare = 1.
   DO WHILE antal_raknare LE antal_valda:
      ASSIGN                                   
      status-ok = BRW_VLEV:FETCH-SELECTED-ROW(antal_raknare)
      mtrl_rowid = ROWID(mtrlkomtemp).             
      MESSAGE "Vill du ta bort kommentar för " + Guru.Konstanter:genk + " " +  mtrlkomtemp.ENR + " " + mtrlkomtemp.KOMMENTAR VIEW-AS ALERT-BOX
      QUESTION BUTTONS YES-NO UPDATE svar AS LOGICAL.         
      IF svar THEN DO TRANSACTION:
         EMPTY TEMP-TABLE felmeddtemp NO-ERROR.
         EMPTY TEMP-TABLE emtrlkomtemp.
         CREATE emtrlkomtemp.
         BUFFER-COPY mtrlkomtemp TO emtrlkomtemp.
         sprow = ROWID(mtrlkomtemp).
         RUN admbortmtrlk_UI IN mtrlbapph (INPUT-OUTPUT TABLE emtrlkomtemp,OUTPUT TABLE felmeddtemp ). 
         FIND FIRST felmeddtemp NO-LOCK NO-ERROR.
         IF AVAILABLE felmeddtemp THEN DO:
            MESSAGE felmeddtemp.FELMEDD VIEW-AS ALERT-BOX.
            DELETE felmeddtemp.                        
         END.            
         ELSE DO:
            EMPTY TEMP-TABLE emtrlkomtemp.
            FIND FIRST mtrlkomtemp WHERE ROWID(mtrlkomtemp) = sprow NO-ERROR.                                    
            DELETE mtrlkomtemp.          
            IF antal_raknare = antal_valda THEN RUN selnextprevrow_UI IN brwproc[1].                          
         END.         
      END.   
      antal_raknare = antal_raknare + 1.   
   END. 
   RUN setcolsortvar_UI IN brwproc[1] (INPUT " ").
   RUN openbdynspec_UI IN brwproc[1].
   RUN lastselectdyn_UI IN brwproc[1].            
   RUN title_UI IN brwproc[1].
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_UPP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_UPP WINDOW-1
ON CHOOSE OF BTN_UPP IN FRAME FRAME-A
DO:
   
   antal_valda = BRW_HLEV:NUM-SELECTED-ROWS.         
   antal_raknare = 1.
   IF antal_valda > 0 THEN DO:
      DO WHILE antal_raknare LE antal_valda:                                            
         status-ok = BRW_HLEV:FETCH-SELECTED-ROW(antal_raknare).
         FIND FIRST mtrlkomtemp WHERE mtrlkomtemp.ENR = mtrltemp.ENR NO-LOCK NO-ERROR.
         IF NOT AVAILABLE mtrlkomtemp  THEN DO:
            CREATE mtrlkomtemp.
            ASSIGN         
            mtrlkomtemp.ENR = mtrltemp.ENR  
            mtrlkomtemp.BENAMNING = mtrltemp.BENAMNING
            mtrlkomtemp.LEVKOD = mtrltemp.LEVKOD
            mtrlkomtemp.KOMMENTAR = ""
            mtrlkomtemp.OMREGTID = 1.            
         END.
         mtrl_rowid = ROWID(mtrlkomtemp).
         antal_raknare = antal_raknare + 1.
      END.
      RUN setcolsortvar_UI IN brwproc[1] (INPUT "").  
      RUN openbdynspec_UI IN brwproc[1].
      RUN title_UI IN brwproc[1].
      FIND FIRST mtrlkomtemp WHERE ROWID(mtrlkomtemp) = mtrl_rowid NO-LOCK NO-ERROR.
      IF AVAILABLE mtrlkomtemp THEN DO:      
         RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(mtrlkomtemp)).
         RUN lastselectdyn_UI IN brwproc[1].        
         APPLY "ENTRY" TO mtrlkomtemp.KOMMENTAR IN BROWSE BRW_VLEV.   
      END. 
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_LEV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_LEV WINDOW-1
ON VALUE-CHANGED OF CMB_LEV IN FRAME FRAME-A /* Leverantörer */
DO:
   CMB_LEV = INPUT CMB_LEV.
   FIND FIRST levtemp WHERE levtemp.LEVNAMN = CMB_LEV 
   USE-INDEX LEV NO-LOCK NO-ERROR.
   vald_lev = levtemp.LEVKOD. 
   levinlast = "".
   RUN levinlast_UI (INPUT vald_lev,OUTPUT levinlast).
   RUN setorgtitle_UI IN brwproc[2] (INPUT "Materiellista " + CMB_LEV + " " + vald_lev  + levinlast).
   RUN setcolsortvar_UI IN brwproc[2] (INPUT " WHERE KALKNR = 0 AND LEVKOD = '" + STRING(vald_lev) + "' USE-INDEX ENR ").
   RUN openbdynspec_UI IN brwproc[2].
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_IEXC
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_IEXC WINDOW-1
ON CHOOSE OF FBTN_IEXC IN FRAME FRAME-A /* Import Exel */
DO:   
   DEFINE VARIABLE OKvald AS LOGICAL NO-UNDO.
   EMPTY TEMP-TABLE emtrlkomtemp NO-ERROR. 
   EMPTY TEMP-TABLE felkmex NO-ERROR. 
   fildir = SESSION:TEMP-DIRECTORY.
   {SESSIONTEMPDIR.I}
   IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN fildir = webclienttempdir.
   OS-CREATE-DIR VALUE(fildir) NO-ERROR.
   mappvar = fildir.
   SYSTEM-DIALOG GET-FILE fildir
   TITLE          "Välj den excelfil som Ni vill läsa in"
   FILTERS        "All Files (*.xls;*.xlsx)"  "*.xls;*.xlsx"   
   INITIAL-DIR    mappvar
   UPDATE OKvald.      
   IF OKvald = TRUE THEN DO:                      
      {muswait.i}     
      {AMERICANEUROPEAN.I}   
      RUN KOMEXELIN.P (INPUT fildir,INPUT vald_lev, OUTPUT TABLE emtrlkomtemp ).
      {EUROPEANAMERICAN.I}         
      RUN enrvalexc_UI.      
      RUN setcolsortvar_UI IN brwproc[1] (INPUT "").
      RUN openbdynspec_UI IN brwproc[1].  
      RUN title_UI IN brwproc[1].
      RUN felmex_UI.               
   END.               
   {musarrow.i}  

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-BEN-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-BEN-2 WINDOW-1
ON ANY-KEY OF FILL-IN-BEN-2 IN FRAME FRAME-A /* Benämning */
DO:
  {TRYCKS.I}
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN-BEN-2 IN FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-BEN-2 WINDOW-1
ON MOUSE-SELECT-DBLCLICK OF FILL-IN-BEN-2 IN FRAME FRAME-A /* Benämning */
DO:
  {muswait.i}
   ASSIGN
   RAD_SOK-2 = INPUT FRAME {&FRAME-NAME} RAD_SOK-2
   FILL-IN-BEN-2 = INPUT FILL-IN-BEN-2.
   IF FILL-IN-BEN-2 = "" THEN DO:
      MESSAGE "Sökbegreppet kan inte vara blankt." VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN-BEN-2 IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
   END.
   stjarnvar = INDEX(FILL-IN-BEN-2,"*",1).
   IF stjarnvar NE 0 THEN DO:
      MESSAGE "Ni behöver inte använda tecknet * vid sökningen" VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
   IF RAD_SOK-2 = 1 THEN DO:
      ASSIGN
      aosok = FILL-IN-BEN-2.
      begvar = TRUE.
   END.
   ELSE IF RAD_SOK-2 = 2 THEN DO:
      ASSIGN
      aosok = "*" + FILL-IN-BEN-2 + "*"
      begvar = FALSE.      
   END.
   ELSE IF RAD_SOK = 3 THEN DO:
      ASSIGN
      aosok = "*" + FILL-IN-BEN-2.
      begvar = FALSE.
   END.
   RUN initsok_UI (INPUT 4,INPUT aosok).
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-ENR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-ENR WINDOW-1
ON ANY-KEY OF FILL-IN-ENR IN FRAME FRAME-A /* Enr */
DO:
   {TRYCKS.I}
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN-ENR IN FRAME {&FRAME-NAME}.
   END.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-ENR WINDOW-1
ON MOUSE-SELECT-DBLCLICK OF FILL-IN-ENR IN FRAME FRAME-A /* Enr */
DO:
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-KOD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-KOD WINDOW-1
ON ANY-KEY OF FILL-IN-KOD IN FRAME FRAME-A /* Enr */
DO:
  {TRYCKS.I}
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN-KOD IN FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-KOD WINDOW-1
ON MOUSE-SELECT-DBLCLICK OF FILL-IN-KOD IN FRAME FRAME-A /* Enr */
DO:
  {muswait.i}   
   ASSIGN
   RAD_SOK-2 = INPUT FRAME {&FRAME-NAME} RAD_SOK-2
   FILL-IN-KOD = INPUT  FILL-IN-KOD.
   IF FILL-IN-KOD = "" THEN DO:
      MESSAGE "Sökbegreppet kan inte vara blankt." VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN-KOD IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
   END.  
   stjarnvar = INDEX(FILL-IN-KOD,"*",1).
   IF stjarnvar NE 0 THEN DO:
      MESSAGE "Ni behöver inte använda tecknet * vid sökningen" VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
   IF RAD_SOK-2 = 1 THEN DO:
      ASSIGN
      posok = FILL-IN-KOD.
      begvar = TRUE.
   END.
   ELSE IF RAD_SOK-2 = 2 THEN DO:
      ASSIGN
      posok = "*" + FILL-IN-KOD + "*"
      begvar = FALSE.      
   END.
   ELSE IF RAD_SOK-2 = 3 THEN DO:
      ASSIGN
      posok = "*" + FILL-IN-KOD.
      begvar = FALSE.
   END.
   RUN initsok_UI (INPUT 3,INPUT posok).
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_AvmarkeraHLEV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_AvmarkeraHLEV WINDOW-1
ON CHOOSE OF MENU-ITEM m_AvmarkeraHLEV /* Avmarkera */
DO:
  status-ok = BRW_HLEV:DESELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_HLEV
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK WINDOW-1 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE DO:
   IF VALID-HANDLE(mtrlbapph) THEN DELETE PROCEDURE mtrlbapph.
   {BORTBRWPROC.I}
   RUN disable_UI.
END.
   

/* These events will close the window and terminate the procedure.      */
/* (NOTE: this will override any user-defined triggers previously       */
/*  defined on the window.)                                             */
ON WINDOW-CLOSE OF {&WINDOW-NAME} DO:
  IF VALID-HANDLE(edataapph) THEN DELETE PROCEDURE edataapph.
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
   EMPTY TEMP-TABLE levtemp NO-ERROR. 
   EMPTY TEMP-TABLE mtrltemp NO-ERROR. 
   {ALLSTARTDYN.I}   
   EMPTY TEMP-TABLE mtrlkomtemp NO-ERROR. 
   RUN hmtbenk_UI IN mtrlbapph ( OUTPUT TABLE mtrlkomtemp ). 
   ASSIGN WINDOW-1:TITLE = "Kommentarer till artiklar".         
   RUN enable_UI.   
   {FRMSIZE.I}           
   mtrlkomtemp.ENR:LABEL IN BROWSE BRW_VLEV = "Enr".
   RUN openbdynspec_UI IN brwproc[1].
   RUN title_UI IN brwproc[1].
   status-ok = CMB_LEV:DELETE("0"). 
   FIND FIRST levtemp WHERE levtemp.LEVKOD = vald_kundlev
   USE-INDEX LEV NO-LOCK NO-ERROR.
   ASSIGN
   status-ok = CMB_LEV:ADD-LAST(levtemp.LEVNAMN)
   CMB_LEV:SCREEN-VALUE = levtemp.LEVNAMN.
   CMB_LEV = levtemp.LEVNAMN.
   vald_lev = levtemp.LEVKOD.    
   levinlast = "".
   RUN levinlast_UI (INPUT vald_lev,OUTPUT levinlast).
   RUN setorgtitle_UI IN brwproc[2] (INPUT "Materiellista " + CMB_LEV + " " + vald_lev  + levinlast).   
   FOR EACH levtemp WHERE levtemp.LEVKOD NE vald_kundlev AND
   levtemp.LEVKOD NE "0" AND levtemp.BORTTAG = FALSE USE-INDEX LEV NO-LOCK:
      status-ok = CMB_LEV:ADD-LAST(levtemp.LEVNAMN).      
   END.
   FBTN_IEXC:LABEL = "Import Excel".
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
   RUN DYNBRW.P PERSISTENT SET brwproc[1]
         (INPUT BRW_VLEV:HANDLE IN FRAME {&FRAME-NAME}).       
   RUN DYNBRW.P PERSISTENT SET brwproc[2]
         (INPUT BRW_HLEV:HANDLE IN FRAME {&FRAME-NAME}). 
   RUN StartSokEnrLev_UI IN brwproc[2] (INPUT THIS-PROCEDURE).           
   RUN addfillin_UI IN brwproc[1] 
      (INPUT FILL-IN-ENR:HANDLE IN FRAME {&FRAME-NAME}, INPUT "ENR").
   RUN dynprogextra IN brwproc[2] (INPUT "rowdispextra_UI",INPUT THIS-PROCEDURE).
   RUN rowdispextrakor IN  brwproc[2] (INPUT TRUE).   
   RUN setdefaultcol_UI IN brwproc[1] (INPUT 1).   
   IF Guru.Konstanter:appcon THEN DO:
      RUN ADMERSATTAPP.P PERSISTENT SET mtrlbapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN ADMERSATTAPP.P PERSISTENT SET mtrlbapph.
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
   IF Guru.Konstanter:appcon THEN DO:
      RUN EXTRADATAHMT.P PERSISTENT SET edataapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT.                  
   END.
   ELSE DO:
      RUN EXTRADATAHMT.P PERSISTENT SET edataapph.      
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
  DISPLAY FILL-IN-ENR RAD_SOK CMB_LEV FILL-IN-KOD RAD_SOK-2 
      WITH FRAME FRAME-A IN WINDOW WINDOW-1.
  ENABLE BRW_VLEV IMAGE-4 IMAGE-16 IMAGE-17 FILL-IN-ENR FBTN_IEXC RAD_SOK 
         BTN_NER BTN_UPP BRW_HLEV CMB_LEV FILL-IN-KOD FILL-IN-BEN-2 RAD_SOK-2 
         BTN_AVB 
      WITH FRAME FRAME-A IN WINDOW WINDOW-1.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enrvalexc_UI WINDOW-1 
PROCEDURE enrvalexc_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/      
   FOR EACH emtrlkomtemp:        
      kolllkod =  emtrlkomtemp.LEVKOD.
      RUN kollenrk_UI IN mtrlbapph ( INPUT emtrlkomtemp.ENR,OUTPUT kollev,INPUT-OUTPUT kolllkod,OUTPUT enrben ). 
      IF kollev = TRUE THEN DO:      
         FIND FIRST mtrlkomtemp WHERE mtrlkomtemp.ENR = emtrlkomtemp.ENR NO-LOCK NO-ERROR.
         IF NOT AVAILABLE mtrlkomtemp  THEN DO:
            CREATE mtrlkomtemp.
            ASSIGN         
            mtrlkomtemp.ENR = emtrlkomtemp.ENR.
         END.
         ASSIGN
         mtrlkomtemp.BENAMNING = enrben
         mtrlkomtemp.LEVKOD = kolllkod
         mtrlkomtemp.KOMMENTAR = emtrlkomtemp.KOMMENTAR
         mtrlkomtemp.OMREGTID = 1.            
         
         EMPTY TEMP-TABLE ekmtrlkomtemp NO-ERROR.
         CREATE ekmtrlkomtemp.          
         BUFFER-COPY mtrlkomtemp TO ekmtrlkomtemp. 
         RUN admsparmtrlk_UI IN mtrlbapph (INPUT TABLE ekmtrlkomtemp ). 
         EMPTY TEMP-TABLE ekmtrlkomtemp NO-ERROR. 

      END.      
      ELSE DO:      
         CREATE felkmex.       
         ASSIGN 
         felkmex.ENR = emtrlkomtemp.ENR         
         felkmex.KOMMENTAR = emtrlkomtemp.KOMMENTAR.         
      END.  
   END.
   EMPTY TEMP-TABLE emtrlkomtemp NO-ERROR. 
   

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE felmex_UI WINDOW-1 
PROCEDURE felmex_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   FIND FIRST felkmex NO-ERROR.
   IF AVAILABLE felkmex THEN DO:
      felfil = SESSION:TEMP-DIR + STRING(TIME) + ".txt".       
      {AMERICANEUROPEAN.I}
      OUTPUT TO VALUE(felfil).         
      PUT "Enr med dessa kommenarer  blev ej inlästa från excel . Var god kontrollera!" AT 6.         
      PUT SKIP.         
      FOR EACH felkmex:           
         PUT UNFORMATTED felkmex.ENR AT 6  SUBSTRING(felkmex.KOMMENTAR,1,30) AT 30 .  
         PUT SKIP.
      END.
      {EUROPEANAMERICAN.I}
      RUN OPENDOC.P (felfil,"","",NO).         
   END.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE infoES_UI WINDOW-1 
PROCEDURE infoES_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/      
   DEFINE INPUT  PARAMETER valenr AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER brwname AS CHARACTER NO-UNDO.
   DEFINE VARIABLE levnamnvar AS CHARACTER NO-UNDO.
   /*GÅR TILL ELEKTROSKANDIAS HEMSIDA*/      
      
   status-ok = BRW_HLEV:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
   IF NOT AVAILABLE mtrltemp THEN RETURN.
   valenr = mtrltemp.ENR.
   FIND FIRST levtemp WHERE levtemp.LEVKOD = mtrltemp.LEVKOD
   NO-LOCK NO-ERROR.            
   IF status-ok THEN DO:   
      levnamnvar = levtemp.LEVNAMN.
      {LEVLANK.I}
   END.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initsok_UI WINDOW-1 
PROCEDURE initsok_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/           
   DEFINE INPUT  PARAMETER vad AS INTEGER    NO-UNDO.
   DEFINE INPUT PARAMETER sokpa AS CHARACTER NO-UNDO.
   DEFINE VARIABLE orgfraga AS CHARACTER NO-UNDO.
   DEFINE VARIABLE decc AS CHARACTER NO-UNDO.
   decc = STRING(1).
   
  IF vad = 3 THEN DO:      
      orgfraga = " WHERE KALKNR = " + STRING(0) + " AND LEVKOD = '" + STRING(vald_lev) + "'".
      tth = TEMP-TABLE mtrltemp:HANDLE.
      EMPTY TEMP-TABLE valsoktemp NO-ERROR. 
      CREATE valsoktemp.
      ASSIGN 
      valsoktemp.SOKCHAR[1] = "MTRL"     /*Skarp tabell*/
      valsoktemp.SOKCHAR[2] = orgfraga   /*Öppningsquery*/
      valsoktemp.SOKCHAR[3] = "ENR" /*sökfält*/
      valsoktemp.SOKCHAR[4] = "MTRLROW"  /*temptabells faltnamn för rowid*/
      valsoktemp.SOKCHAR[5] = sokpa.      /*sök på*/       
      RUN sokhmt_UI IN  brwproc[2] (INPUT TABLE valsoktemp). 
  END.
  IF vad = 4 THEN DO:      
      orgfraga = " WHERE KALKNR = " + STRING(0) + " AND LEVKOD = '" + STRING(vald_lev) + "'".
      /*orgfraga = " WHERE KALKNR = " + STRING(0) + " AND LEVKOD = '" + STRING(vald_lev) + "' USE-INDEX LEV".*/
      tth = TEMP-TABLE mtrltemp:HANDLE.
      EMPTY TEMP-TABLE valsoktemp NO-ERROR. 
      CREATE valsoktemp.
      ASSIGN 
      valsoktemp.SOKCHAR[1] = "MTRL"     /*Skarp tabell*/
      valsoktemp.SOKCHAR[2] = orgfraga   /*Öppningsquery*/
      valsoktemp.SOKCHAR[3] = "BENAMNING" /*sökfält*/
      valsoktemp.SOKCHAR[4] = "MTRLROW"  /*temptabells faltnamn för rowid*/
      valsoktemp.SOKCHAR[5] = sokpa.      /*sök på*/       
      RUN sokhmt_UI IN  brwproc[2] (INPUT TABLE valsoktemp). 
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE levinlast_UI WINDOW-1 
PROCEDURE levinlast_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/  
   DEFINE INPUT PARAMETER lekod AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER levinlast AS CHARACTER NO-UNDO.
   IF Guru.Konstanter:globforetag = "ELPA" THEN DO:
      EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
      CREATE inextradatatemp.          
      ASSIGN
      inextradatatemp.PROGRAM = "INLKAT"                   
      inextradatatemp.HUVUDCH = lekod.                                              
      RUN etabhamt_UI IN edataapph (INPUT TABLE inextradatatemp, OUTPUT TABLE extradatatemp). 
      FIND FIRST extradatatemp NO-LOCK NO-ERROR.
      IF AVAILABLE extradatatemp THEN DO:
         IF extradatatemp.SOKDATE[1] NE ? THEN levinlast = " Inläst " + STRING(extradatatemp.SOKDATE[1]).
         IF extradatatemp.SOKDATE[2] NE ? AND extradatatemp.SOKDATE[3] NE ?  THEN DO:      
            RUN tooltip_UI IN brwproc[2] (INPUT "Katalogen gäller från: " + STRING(extradatatemp.SOKDATE[2]) + " till: " + STRING(extradatatemp.SOKDATE[3])).          
         END.
         ELSE RUN tooltip_UI IN brwproc[2] (INPUT "").             
      END.
      ELSE RUN tooltip_UI IN brwproc[2] (INPUT "").             
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE rowdispextra_UI WINDOW-1 
PROCEDURE rowdispextra_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   DEFINE INPUT PARAMETER TABLE FOR coltemp.
   DEFINE INPUT PARAMETER brwh AS HANDLE NO-UNDO.      
   IF brwh:NAME = "BRW_HLEV" THEN RUN rowdispmtrl_UI (INPUT 1).       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE rowdispmtrl_UI WINDOW-1 
PROCEDURE rowdispmtrl_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/  
   DEFINE INPUT PARAMETER vad AS INTEGER NO-UNDO.   
   IF vad = 1 THEN DO:
      IF AVAILABLE mtrltemp THEN DO:
          IF mtrltemp.KUND = TRUE THEN DO: 
            ASSIGN
            mtrltemp.ENR:BGCOLOR IN BROWSE BRW_HLEV = Guru.Konstanter:varforetypval[28]
            mtrltemp.NPRIS:BGCOLOR IN BROWSE BRW_HLEV = Guru.Konstanter:varforetypval[28].          
         END.         
         ELSE IF mtrltemp.KUND = ? THEN DO: 
            ASSIGN
            mtrltemp.ENR:BGCOLOR IN BROWSE BRW_HLEV = Guru.Konstanter:varforetypval[38]
            mtrltemp.NPRIS:BGCOLOR IN BROWSE BRW_HLEV = Guru.Konstanter:varforetypval[38].          
         END.         
      END.
   END.   
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE visa_UI WINDOW-1 
PROCEDURE visa_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/                
   IF AVAILABLE mtrlkomtemp THEN DO:           
      DISPLAY
      mtrlkomtemp.ENR
      mtrlkomtemp.BENAMNING
      mtrlkomtemp.KOMMENTAR
      WITH BROWSE BRW_VLEV.                
   END.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

