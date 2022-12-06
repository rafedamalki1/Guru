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
DEFINE INPUT PARAMETER valvardnr AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER franvart AS INTEGER NO-UNDO.

/* Local Variable Definitions ---                                       */
{ALLDEF.I}
&SCOPED-DEFINE NEW NEW
&SCOPED-DEFINE SHARED SHARED
{BLOB.I}
DEFINE VARIABLE bloblog AS LOGICAL NO-UNDO.
DEFINE VARIABLE blobproch AS HANDLE NO-UNDO.
&Scoped-define NEW
{GLOBVAR2DEL1.I}
{REGVAR.I}
&Scoped-define SHARED SHARED
{ANSVMARK.I}
{MARKTEMP.I}   
{MARKVARD.I}
{OMRTEMPW.I}
{FASTIGHET.I}
{LANKOM.I}
FIND FIRST komuntemp NO-LOCK NO-ERROR.
IF NOT AVAILABLE komuntemp THEN DO:
   RUN LANKOM.P.
END.

&Scoped-define NEW NEW
{AONRDEF.I}
{ANVPERS.I}
DEFINE TEMP-TABLE kfastighettemp NO-UNDO LIKE fastighettemp.
DEFINE NEW SHARED VARIABLE reprowid AS ROWID NO-UNDO.

DEFINE SHARED VARIABLE vartpro AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE SHARED VARIABLE vart AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE SHARED VARIABLE faktrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE radvar AS INTEGER NO-UNDO.
DEFINE VARIABLE valdfast AS CHARACTER NO-UNDO.
DEFINE VARIABLE valdfastg AS CHARACTER NO-UNDO.
DEFINE VARIABLE fastighbet AS CHARACTER NO-UNDO.
DEFINE VARIABLE fastrec AS RECID NO-UNDO.
DEFINE VARIABLE antal_valda AS INTEGER NO-UNDO. 
DEFINE VARIABLE antal_raknare AS INTEGER NO-UNDO.  
DEFINE VARIABLE tot% AS INTEGER NO-UNDO.  
DEFINE VARIABLE val1 AS LOGICAL NO-UNDO.
DEFINE VARIABLE ortkalks AS CHARACTER NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE kommandoq AS CHARACTER NO-UNDO.
DEFINE VARIABLE fastvrec AS RECID NO-UNDO.
DEFINE VARIABLE hmtkommun AS CHARACTER NO-UNDO.
DEFINE VARIABLE hmtlan AS CHARACTER NO-UNDO.
DEFINE VARIABLE felmedd AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE fastigregapph AS HANDLE NO-UNDO.
DEFINE VARIABLE varderinginapph AS HANDLE NO-UNDO.
DEFINE VARIABLE fildir AS CHARACTER NO-UNDO.
DEFINE VARIABLE mappvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE felfil AS CHARACTER NO-UNDO.

DEFINE VARIABLE kommando AS CHARACTER FORMAT "X(20)" NO-UNDO.
DEFINE VARIABLE kommando2 AS CHARACTER FORMAT "X(20)" NO-UNDO.
DEFINE VARIABLE fnamn AS CHARACTER NO-UNDO.

DEFINE VARIABLE chExcelApplication      AS COM-HANDLE.
DEFINE VARIABLE chWorkbook              AS COM-HANDLE.
DEFINE VARIABLE chWorksheet             AS COM-HANDLE.

{IMPFAST.I}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BRW_FAST

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES fastighettemp fastvardtemp

/* Definitions for BROWSE BRW_FAST                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_FAST fastighettemp.BETECKNING ~
fastighettemp.KOMMUN fastighettemp.VAKER fastighettemp.SOCKEN 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_FAST fastighettemp.BETECKNING 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_FAST fastighettemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_FAST fastighettemp
&Scoped-define QUERY-STRING-BRW_FAST FOR EACH fastighettemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_FAST OPEN QUERY BRW_FAST FOR EACH fastighettemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_FAST fastighettemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_FAST fastighettemp


/* Definitions for BROWSE BRW_VFAST                                     */
&Scoped-define FIELDS-IN-QUERY-BRW_VFAST fastvardtemp.BETECKNING 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_VFAST 
&Scoped-define QUERY-STRING-BRW_VFAST FOR EACH fastvardtemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_VFAST OPEN QUERY BRW_VFAST FOR EACH fastvardtemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_VFAST fastvardtemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_VFAST fastvardtemp


/* Definitions for FRAME DEFAULT-FRAME                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-52 IMAGE-13 CMB_LAN FBTN_OK CMB_KOMMUN ~
BTN_HAMT BRW_FAST BRW_VFAST BTN_OVER BTN_BACK BTN_NY BTN_UPP BTN_BORT ~
BTN_IMPEX BTN_IMPEXTEKIS BTN_IMPEXMETRIA BTN_IMPEXVISMA BTN_IMPEXEON ~
BTN_IMPGURU BTN_EXPGURUMALL FILL-IN_BETECKNING FILL-IN-KOMMUN FILL-IN-lan ~
BTN_AVB 
&Scoped-Define DISPLAYED-OBJECTS CMB_LAN CMB_KOMMUN FILL-IN_BETECKNING ~
FILL-IN-KOMMUN FILL-IN-lan FILL-IN-TEXT 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD brwval C-Win 
FUNCTION brwval RETURNS LOGICAL
  ( INPUT valbrw AS INTEGER  )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB 
     LABEL "Avsluta":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_BACK 
     IMAGE-UP FILE "BILDER\prev-u":U
     LABEL "":L 
     SIZE 4 BY 1.21 TOOLTIP "Koppla från fastighet från värdering".

DEFINE BUTTON BTN_BORT 
     LABEL "Ta bort":L 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_EXPGURUMALL 
     LABEL "Export Gurumall" 
     SIZE 12 BY 1 TOOLTIP "Ta ut Excelmal med Guruformat för import av fastigheter och markägare".

DEFINE BUTTON BTN_HAMT 
     LABEL "Hämta och visa urval" 
     SIZE 22 BY 1.

DEFINE BUTTON BTN_IMPEX 
     LABEL "Import Exel" 
     SIZE 12 BY 1 TOOLTIP "Importera fastigheter och markägare från en Infotrader mall".

DEFINE BUTTON BTN_IMPEXEON 
     LABEL "Import Eon" 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_IMPEXMETRIA 
     LABEL "Import Metria" 
     SIZE 12 BY 1 TOOLTIP "Importera fastigheter och markägare från en Metria mall".

DEFINE BUTTON BTN_IMPEXTEKIS 
     LABEL "Import Tekis" 
     SIZE 12 BY 1 TOOLTIP "Importera fastigheter och markägare från en Tekis mall".

DEFINE BUTTON BTN_IMPEXVISMA 
     LABEL "Import Visma" 
     SIZE 12 BY 1 TOOLTIP "Importera fastigheter och markägare från en Visma mall".

DEFINE BUTTON BTN_IMPGURU 
     LABEL "Import Guru" 
     SIZE 12 BY 1 TOOLTIP "Importera fastigheter och markägare från en Guru mall".

DEFINE BUTTON BTN_NY 
     LABEL "Ny":L 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_OVER 
     IMAGE-UP FILE "BILDER\next-u":U
     LABEL "":L 
     SIZE 4 BY 1.21 TOOLTIP "Koppla fastighet till värdering".

DEFINE BUTTON BTN_UPP 
     LABEL "Ändra":L 
     SIZE 12 BY 1.

DEFINE BUTTON FBTN_OK 
     LABEL "Markägarförteckning/Koppla markägare till fastighet":L 
     SIZE 41.5 BY 1 TOOLTIP "Lägg upp/ändra på markägare, koppla markägare till fastighet".

DEFINE VARIABLE CMB_KOMMUN AS CHARACTER FORMAT "X(256)":U 
     LABEL "Kommun" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_LAN AS CHARACTER FORMAT "X(256)":U 
     LABEL "Län" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-KOMMUN AS CHARACTER FORMAT "X(256)":U 
     LABEL "Kommun" 
     VIEW-AS FILL-IN 
     SIZE 21 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN-lan AS CHARACTER FORMAT "X(256)":U 
     LABEL "Län" 
     VIEW-AS FILL-IN 
     SIZE 19 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN-TEXT AS CHARACTER FORMAT "X(256)":U INITIAL "Koppla fastighet till värdering" 
      VIEW-AS TEXT 
     SIZE 32.5 BY 1.5
     FONT 17 NO-UNDO.

DEFINE VARIABLE FILL-IN_BETECKNING AS CHARACTER FORMAT "x(40)" 
     LABEL "Fastighetsbeteckning" 
     VIEW-AS FILL-IN 
     SIZE 19.5 BY .83 NO-UNDO.

DEFINE IMAGE IMAGE-13
     FILENAME "BILDER\sokpa.gif":U CONVERT-3D-COLORS
     SIZE 8 BY .83.

DEFINE RECTANGLE RECT-52
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 101 BY 1.21.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_FAST FOR 
      fastighettemp SCROLLING.

DEFINE QUERY BRW_VFAST FOR 
      fastvardtemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_FAST
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_FAST C-Win _STRUCTURED
  QUERY BRW_FAST NO-LOCK DISPLAY
      fastighettemp.BETECKNING COLUMN-LABEL "Fastighetsbeteckning" FORMAT "X(40)":U
      fastighettemp.KOMMUN COLUMN-LABEL "Kommun" FORMAT "X(256)":U
            WIDTH 20
      fastighettemp.VAKER COLUMN-LABEL "Län" FORMAT "X(256)":U
            WIDTH 20
      fastighettemp.SOCKEN COLUMN-LABEL "Socken" FORMAT "X(256)":U
            WIDTH 20
  ENABLE
      fastighettemp.BETECKNING
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 73.5 BY 17
         TITLE "Fastighetsförteckning".

DEFINE BROWSE BRW_VFAST
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_VFAST C-Win _STRUCTURED
  QUERY BRW_VFAST NO-LOCK DISPLAY
      fastvardtemp.BETECKNING COLUMN-LABEL "Fastighetsbeteckning" FORMAT "X(256)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 43 BY 17
         TITLE "valda fastigheter" ROW-HEIGHT-CHARS 1 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     CMB_LAN AT ROW 1.5 COL 15.25 COLON-ALIGNED
     FBTN_OK AT ROW 1.5 COL 82.13
     CMB_KOMMUN AT ROW 2.71 COL 15.25 COLON-ALIGNED
     BTN_HAMT AT ROW 4.04 COL 17.25
     BRW_FAST AT ROW 5.29 COL 1.5
     BRW_VFAST AT ROW 5.29 COL 80.88
     BTN_OVER AT ROW 9.79 COL 75.63
     BTN_BACK AT ROW 13.29 COL 75.63
     BTN_NY AT ROW 23 COL 1.88
     BTN_UPP AT ROW 23 COL 14.63
     BTN_BORT AT ROW 23 COL 27.38
     BTN_IMPEX AT ROW 23 COL 40.13
     BTN_IMPEXTEKIS AT ROW 23 COL 52.88 WIDGET-ID 2
     BTN_IMPEXMETRIA AT ROW 23 COL 65.88 WIDGET-ID 4
     BTN_IMPEXVISMA AT ROW 23 COL 78.63 WIDGET-ID 6
     BTN_IMPEXEON AT ROW 23 COL 91.13 WIDGET-ID 8
     BTN_IMPGURU AT ROW 23 COL 103.63 WIDGET-ID 10
     BTN_EXPGURUMALL AT ROW 23 COL 111.5 WIDGET-ID 12
     FILL-IN_BETECKNING AT ROW 24.5 COL 22.5 COLON-ALIGNED
     FILL-IN-KOMMUN AT ROW 24.5 COL 50 COLON-ALIGNED
     FILL-IN-lan AT ROW 24.5 COL 77.5 COLON-ALIGNED
     BTN_AVB AT ROW 24.5 COL 109.63
     FILL-IN-TEXT AT ROW 3.54 COL 79.75 COLON-ALIGNED NO-LABEL
     RECT-52 AT ROW 24.29 COL 1.25
     IMAGE-13 AT ROW 24.5 COL 1.75
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 123.63 BY 25.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Temp-Tables and Buffers:
      TABLE: fastighettemp T "?" NO-UNDO temp-db fastighettemp
      TABLE: fastvardtemp T "?" NO-UNDO temp-db fastvardtemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Registrera/ändra  fastighet, koppla fastighet till värderingsnummer"
         HEIGHT             = 24.96
         WIDTH              = 123.25
         MAX-HEIGHT         = 27.38
         MAX-WIDTH          = 123.63
         VIRTUAL-HEIGHT     = 27.38
         VIRTUAL-WIDTH      = 123.63
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
   FRAME-NAME                                                           */
/* BROWSE-TAB BRW_FAST BTN_HAMT DEFAULT-FRAME */
/* BROWSE-TAB BRW_VFAST BRW_FAST DEFAULT-FRAME */
/* SETTINGS FOR FILL-IN FILL-IN-TEXT IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_FAST
/* Query rebuild information for BROWSE BRW_FAST
     _TblList          = "Temp-Tables.fastighettemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.fastighettemp.BETECKNING
"fastighettemp.BETECKNING" "Fastighetsbeteckning" "X(40)" "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.fastighettemp.KOMMUN
"fastighettemp.KOMMUN" "Kommun" "X(256)" "character" ? ? ? ? ? ? no ? no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.fastighettemp.VAKER
"fastighettemp.VAKER" "Län" "X(256)" "character" ? ? ? ? ? ? no ? no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.fastighettemp.SOCKEN
"fastighettemp.SOCKEN" "Socken" "X(256)" "character" ? ? ? ? ? ? no ? no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_FAST */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_VFAST
/* Query rebuild information for BROWSE BRW_VFAST
     _TblList          = "Temp-Tables.fastvardtemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.fastvardtemp.BETECKNING
"fastvardtemp.BETECKNING" "Fastighetsbeteckning" "X(256)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_VFAST */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Registrera/ändra  fastighet, koppla fastighet till värderingsnummer */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
  APPLY "CLOSE" TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Registrera/ändra  fastighet, koppla fastighet till värderingsnummer */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_FAST
&Scoped-define SELF-NAME BRW_FAST
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_FAST C-Win
ON MOUSE-SELECT-DBLCLICK OF BRW_FAST IN FRAME DEFAULT-FRAME /* Fastighetsförteckning */
DO:
   APPLY "CHOOSE" TO BTN_OVER.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_FAST C-Win
ON VALUE-CHANGED OF BRW_FAST IN FRAME DEFAULT-FRAME /* Fastighetsförteckning */
DO:
   status-ok = BRW_FAST:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.     
   IF AVAILABLE fastighettemp THEN DO:
       
      FIND FIRST fastvardtemp WHERE fastvardtemp.BETECKNING = fastighettemp.BETECKNING NO-LOCK NO-ERROR.
      IF AVAILABLE fastvardtemp THEN DO:
         RUN setlastrowid_UI IN brwproc[2] (INPUT ROWID(fastvardtemp)).
         RUN lastselectdyn_UI IN brwproc[2].  
      END.            
      ELSE DO:
         status-ok = BRW_VFAST:DESELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.     
      END.
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_VFAST
&Scoped-define SELF-NAME BRW_VFAST
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_VFAST C-Win
ON VALUE-CHANGED OF BRW_VFAST IN FRAME DEFAULT-FRAME /* valda fastigheter */
DO:
   status-ok = BRW_VFAST:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.     
   IF AVAILABLE fastvardtemp THEN DO:
      FIND FIRST fastighettemp WHERE fastighettemp.BETECKNING = fastvardtemp.BETECKNING NO-LOCK NO-ERROR.
      IF AVAILABLE fastighettemp THEN DO:
         RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(fastighettemp)).
         RUN lastselectdyn_UI IN brwproc[1].  
      END.            
      ELSE DO:
         IF Guru.Konstanter:appcon THEN DO:                           
            RUN PHMTENFAST.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
            (INPUT fastvardtemp.BETECKNING ,OUTPUT TABLE fastighettemp APPEND).
         END.
         ELSE DO:
            RUN PHMTENFAST.P  
            (INPUT fastvardtemp.BETECKNING,OUTPUT TABLE fastighettemp APPEND).
         END.              
         RUN openbdyn_UI IN brwproc[1] (INPUT "").
         RUN fetchselrowid_UI IN brwproc[1].
      END.
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB C-Win
ON CHOOSE OF BTN_AVB IN FRAME DEFAULT-FRAME /* Avsluta */
DO: 
   IF radvar = 1 THEN DO:      
      RUN btnok IN varderinginapph (INPUT valvardnr,INPUT TABLE fastvardtemp).
   END.
   APPLY "CLOSE" TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_BACK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_BACK C-Win
ON CHOOSE OF BTN_BACK IN FRAME DEFAULT-FRAME
DO:   
   IF radvar = 0 THEN RETURN NO-APPLY.
   IF brwval(2) = TRUE THEN RETURN NO-APPLY.
   antal_valda = BRW_VFAST:NUM-SELECTED-ROWS.
   antal_raknare = 1.
   DO WHILE antal_raknare LE antal_valda:                                      
      status-ok = BRW_VFAST:FETCH-SELECTED-ROW(antal_raknare).  
      fastvrec = RECID(fastvardtemp).
      felmedd = "".
      RUN bortcheck_UI IN varderinginapph (INPUT fastvardtemp.VARDNR,INPUT fastvardtemp.BETECKNING,OUTPUT felmedd).
      IF felmedd NE "" THEN DO:
         MESSAGE felmedd VIEW-AS ALERT-BOX TITLE "Meddelande".
         RETURN NO-APPLY.
      END.
      ELSE DO: 
         FIND FIRST fastvardtemp WHERE RECID(fastvardtemp) = fastvrec NO-LOCK NO-ERROR.
         DELETE fastvardtemp.                                           
         status-ok = BRW_VFAST:DESELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.     
      END.   
      antal_raknare = antal_raknare + 1.   
   END.           
   RUN valdafast_UI.
   status-ok = BRW_VFAST:DESELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_BORT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_BORT C-Win
ON CHOOSE OF BTN_BORT IN FRAME DEFAULT-FRAME /* Ta bort */
DO: 
   IF brwval(1) = TRUE THEN RETURN NO-APPLY.
   status-ok = BRW_FAST:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
   fastighbet = fastighettemp.BETECKNING.
   MESSAGE "Vill du verkligen ta bort fastighet " +   fastighettemp.BETECKNING + " ?"
   VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Bortag av fastighet"
   UPDATE answer AS LOGICAL.         
   IF answer THEN DO:
      EMPTY TEMP-TABLE felmeddtemp NO-ERROR. 
      RUN bort IN fastigregapph (INPUT fastighbet,OUTPUT TABLE felmeddtemp).
      FIND FIRST felmeddtemp NO-LOCK NO-ERROR.
      IF AVAILABLE felmeddtemp THEN DO:
         IF felmeddtemp.VAL = 5 THEN DO:
            MESSAGE felmeddtemp.FELMEDD VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE val AS LOGICAL.
            CASE val:
               WHEN TRUE THEN DO:
                  RUN delmarkfast IN fastigregapph (INPUT fastighbet).
                  RUN bort_UI.
               END.   
               WHEN FALSE THEN RETURN.
            END CASE. 
            DELETE felmeddtemp.
         END.
         ELSE IF felmeddtemp.VAL = 6 THEN DO:
            DELETE felmeddtemp.
            RUN bort_UI.
         END.
         ELSE DO:
            MESSAGE felmeddtemp.FELMEDD VIEW-AS ALERT-BOX TITLE "Meddelande".
            DELETE felmeddtemp.
            RETURN NO-APPLY.
         END.
      END.
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_EXPGURUMALL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_EXPGURUMALL C-Win
ON CHOOSE OF BTN_EXPGURUMALL IN FRAME DEFAULT-FRAME /* Export Gurumall */
DO:   
          
   fnamn = "Guruimport fastig markag.xlsx".   
   IF Guru.Konstanter:appcon THEN RUN FINNSTABELL.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT (INPUT "BLOBINFO", OUTPUT bloblog).
   ELSE RUN FINNSTABELL.P (INPUT "BLOBINFO", OUTPUT bloblog).
   IF bloblog = TRUE THEN DO:
      {FINNSDYNBLOB.I}
      DEFINE VARIABLE resid AS INTEGER NO-UNDO.
      RUN blobfil_UI IN blobproch (INPUT fnamn, OUTPUT resid).
      IF resid = ? THEN DO:
         kommando = SEARCH(fnamn).      
      END.
      ELSE DO:
         FIND FIRST blobinfotemp WHERE blobinfotemp.ID = resid NO-LOCK NO-ERROR.
         RUN blobopen_UI IN blobproch (INPUT blobinfotemp.FILNAMN, OUTPUT kommando).      
      END.
      RUN deleteproc_UI IN blobproch.
      IF VALID-HANDLE(blobproch) THEN DELETE PROCEDURE blobproch NO-ERROR.
   END.
   ELSE kommando = SEARCH(fnamn).   
   IF kommando = ? THEN DO:
      MESSAGE "Hittade inte " fnamn VIEW-AS ALERT-BOX.
      RETURN.       
   END.

   kommando2 = SESSION:TEMP-DIRECTORY + Guru.Konstanter:globanv + "\".
   {SESSIONTEMPDIR.I}
   IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN kommando2 = webclienttempdir.
   OS-CREATE-DIR VALUE(kommando2) NO-ERROR.
   kommando2 = kommando2 + fnamn.
   
   OS-COPY VALUE(kommando) VALUE(kommando2).
   kommando = kommando2.   
          
   OS-CREATE-DIR VALUE(kommando2) NO-ERROR.
   
   CREATE "Excel.Application" chExcelApplication.
   chExcelApplication:Visible = TRUE NO-ERROR.
   
   chWorkbook = chExcelApplication:Workbooks:OPEN(kommando)  NO-ERROR.
   chWorkSheet = chExcelApplication:Sheets:Item(1)  NO-ERROR.
   
   RELEASE OBJECT chExcelApplication NO-ERROR.      
   RELEASE OBJECT chWorkbook NO-ERROR.
   RELEASE OBJECT chWorksheet NO-ERROR.
   
   chExcelApplication:VISIBLE = TRUE NO-ERROR.   
         

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_HAMT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_HAMT C-Win
ON CHOOSE OF BTN_HAMT IN FRAME DEFAULT-FRAME /* Hämta och visa urval */
DO:   
   RUN btnhmt_UI.
   FOR EACH fastvardtemp:
      FIND FIRST fastighettemp WHERE fastighettemp.BETECKNING = fastvardtemp.BETECKNING NO-LOCK NO-ERROR.
      IF NOT AVAILABLE fastighettemp THEN DO:
         IF Guru.Konstanter:appcon THEN DO:                           
            RUN PHMTENFAST.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
            (INPUT fastvardtemp.BETECKNING ,OUTPUT TABLE fastighettemp APPEND).
         END.
         ELSE DO:
            RUN PHMTENFAST.P  
            (INPUT fastvardtemp.BETECKNING,OUTPUT TABLE fastighettemp APPEND).
         END.                              
      END.
   END.   
   RUN openbdyn_UI IN brwproc[1] (INPUT "").
   RUN fetchselrowid_UI IN brwproc[1].
   RUN title_UI IN brwproc[1].
   
   {musarrow.i}    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_IMPEX
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_IMPEX C-Win
ON CHOOSE OF BTN_IMPEX IN FRAME DEFAULT-FRAME /* Import Exel */
DO:   
   
   DEFINE VARIABLE OKvald AS LOGICAL NO-UNDO.
   
   fildir = SESSION:TEMP-DIRECTORY + Guru.Konstanter:globanv + "\".
   {SESSIONTEMPDIR.I}
   IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN fildir = webclienttempdir.
   OS-CREATE-DIR VALUE(fildir) NO-ERROR.
   mappvar = fildir.
   SYSTEM-DIALOG GET-FILE fildir
   TITLE          "Välj den Infotrader-fil som Ni vill läsa in"
   FILTERS        "All Files (*.xls;*.xlsx;*.xlsm)"  "*.xls;*.xlsx;*.xlsm"
   INITIAL-DIR    mappvar
   UPDATE OKvald.      
   IF OKvald = TRUE THEN DO:                
      {muswait.i}
      EMPTY TEMP-TABLE fasttmp NO-ERROR. 
      EMPTY TEMP-TABLE marktmp NO-ERROR.             
      /*nytt format Infotrader 20180227*/   
      RUN FASTEXELIN.P (INPUT 4,INPUT fildir,OUTPUT TABLE fasttmp ,OUTPUT TABLE marktmp ).
      EMPTY TEMP-TABLE felex NO-ERROR. 
      FOR EACH fasttmp:
         FIND FIRST komuntemp WHERE komuntemp.BENAMNING = fasttmp.KOMMUN NO-LOCK NO-ERROR.
         IF AVAILABLE komuntemp  THEN DO:
            FIND FIRST lantemp WHERE lantemp.LANID = komuntemp.LANID NO-LOCK NO-ERROR.
            IF AVAILABLE lantemp THEN DO:
               fasttmp.LAN = lantemp.BENAMNING .
            END.
         END.
      END.      
      RUN impfast_UI IN varderinginapph (INPUT valvardnr, INPUT TABLE fasttmp,INPUT TABLE marktmp,OUTPUT TABLE felex).      
      EMPTY TEMP-TABLE fastighettemp NO-ERROR. 
      EMPTY TEMP-TABLE fastvardtemp NO-ERROR. 
      EMPTY TEMP-TABLE varderingtemp NO-ERROR. 
      RUN fastighmt IN fastigregapph (INPUT valvardnr,OUTPUT TABLE varderingtemp,OUTPUT TABLE fastvardtemp).
            
      RUN hmtfast_UI.
      
      FIND FIRST fastvardtemp NO-ERROR.
      IF AVAILABLE fastvardtemp THEN DO:
         fastighbet = fastvardtemp.BETECKNING.
      END.      
      RUN uppdat_UI.

      /*FOR EACH felex: 
         MESSAGE felex.MARKAGARE felex.BETECKNING  VIEW-AS ALERT-BOX.
      END.      */
      FIND FIRST felex NO-ERROR.
      IF AVAILABLE felex THEN DO:
         felfil = SESSION:TEMP-DIR + STRING(TIME) + ".txt". 
         {AMERICANEUROPEAN.I}      
         OUTPUT TO VALUE(felfil).
         PUT "Dessa markägare har inget personnummer, eller så har fastigheten ingen markägare. Var god och kontrollera.!" AT 6.         
         PUT SKIP.         
         PUT SKIP.         
         FOR EACH felex:           
            PUT "Fastighet:" FORMAT "X(10)" AT 1 felex.BETECKNING FORMAT "X(30)"  AT 12 "Markägare:" FORMAT "X(10)" AT 50  felex.MARKAGARE FORMAT "X(30)"  AT 62.  
            PUT SKIP.
         END.
         OUTPUT CLOSE.
         {EUROPEANAMERICAN.I}
         RUN OPENDOC.P (felfil,"","",NO).         
      END.   
      {musarrow.i}
   END.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_IMPEXEON
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_IMPEXEON C-Win
ON CHOOSE OF BTN_IMPEXEON IN FRAME DEFAULT-FRAME /* Import Eon */
DO:   
   
   DEFINE VARIABLE OKvald AS LOGICAL NO-UNDO.
   
   fildir = SESSION:TEMP-DIRECTORY + Guru.Konstanter:globanv + "\".
   {SESSIONTEMPDIR.I}
   IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN fildir = webclienttempdir.
   OS-CREATE-DIR VALUE(fildir) NO-ERROR.
   mappvar = fildir.
   SYSTEM-DIALOG GET-FILE fildir
   TITLE          "Välj den Eon-fil som Ni vill läsa in"
   FILTERS        "All Files (*.xls;*.xlsx)"  "*.xls;*.xlsx"
   INITIAL-DIR    mappvar
   UPDATE OKvald.      
   IF OKvald = TRUE THEN DO:                
      {muswait.i}
      EMPTY TEMP-TABLE fasttmp NO-ERROR. 
      EMPTY TEMP-TABLE marktmp NO-ERROR.       
     
     RUN FASTEXELIN.P (INPUT 10, INPUT fildir,OUTPUT TABLE fasttmp ,OUTPUT TABLE marktmp ).    
      EMPTY TEMP-TABLE felex NO-ERROR. 
      FOR EACH fasttmp:
         FIND FIRST komuntemp WHERE komuntemp.BENAMNING = fasttmp.KOMMUN NO-LOCK NO-ERROR.
         IF AVAILABLE komuntemp  THEN DO:
            FIND FIRST lantemp WHERE lantemp.LANID = komuntemp.LANID NO-LOCK NO-ERROR.
            IF AVAILABLE lantemp THEN DO:
               fasttmp.LAN = lantemp.BENAMNING .
            END.
         END.
      END.      

      /*FOR EACH fasttmp WHERE NO-LOCK:
         MESSAGE fasttmp.beteckning fasttmp.kommun VIEW-AS ALERT-BOX.
      END.*/

     /* FOR EACH marktmp WHERE NO-LOCK:
         MESSAGE marktmp.beteckning marktmp.MARKAGARE  marktmp.GATUADRESS  marktmp.POSTNUMMER  marktmp.POSTADRESS  marktmp.PERSONNUMMER  marktmp.andel MARKTMP.TELEFON   VIEW-AS ALERT-BOX.
      END.*/
      
      RUN impfast_UI IN varderinginapph (INPUT valvardnr, INPUT TABLE fasttmp,INPUT TABLE marktmp,OUTPUT TABLE felex).      
      EMPTY TEMP-TABLE fastighettemp NO-ERROR. 
      EMPTY TEMP-TABLE fastvardtemp NO-ERROR. 
      EMPTY TEMP-TABLE varderingtemp NO-ERROR. 
      RUN fastighmt IN fastigregapph (INPUT valvardnr,OUTPUT TABLE varderingtemp,OUTPUT TABLE fastvardtemp).
            
      RUN hmtfast_UI.
      
      FIND FIRST fastvardtemp NO-ERROR.
      IF AVAILABLE fastvardtemp THEN DO:
         fastighbet = fastvardtemp.BETECKNING.
      END.      
      RUN uppdat_UI.

      /*FOR EACH felex: 
         MESSAGE felex.MARKAGARE felex.BETECKNING  VIEW-AS ALERT-BOX.
      END.      */
      FIND FIRST felex NO-ERROR.
      IF AVAILABLE felex THEN DO:
         felfil = SESSION:TEMP-DIR + STRING(TIME) + ".txt". 
         {AMERICANEUROPEAN.I}      
         OUTPUT TO VALUE(felfil).
         PUT "Dessa markägare har inget personnummer, eller så har fastigheten ingen markägare. Var god och kontrollera.!" AT 6.         
         PUT SKIP.         
         PUT SKIP.         
         FOR EACH felex:           
            PUT "Fastighet:" FORMAT "X(10)" AT 1 felex.BETECKNING FORMAT "X(30)"  AT 12 "Markägare:" FORMAT "X(10)" AT 50  felex.MARKAGARE FORMAT "X(30)"  AT 62.  
            PUT SKIP.
         END.
         OUTPUT CLOSE.
         {EUROPEANAMERICAN.I}
         RUN OPENDOC.P (felfil,"","",NO).         
      END.   
      {musarrow.i}
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_IMPEXMETRIA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_IMPEXMETRIA C-Win
ON CHOOSE OF BTN_IMPEXMETRIA IN FRAME DEFAULT-FRAME /* Import Metria */
DO:   
   
   DEFINE VARIABLE OKvald AS LOGICAL NO-UNDO.
   
   fildir = SESSION:TEMP-DIRECTORY + Guru.Konstanter:globanv + "\".
   {SESSIONTEMPDIR.I}
   IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN fildir = webclienttempdir.
   OS-CREATE-DIR VALUE(fildir) NO-ERROR.
   mappvar = fildir.
   
   IF Guru.Konstanter:globforetag = "tect" THEN DO:  
      MESSAGE "Importera från Metria- svara Ja." SKIP  
              "Importera från Metria utökad - svara Nej"
      VIEW-AS ALERT-BOX
         QUESTION BUTTONS YES-NO TITLE "Import"  UPDATE valM AS LOGICAL.
      IF valM = FALSE  THEN DO:   
         MESSAGE "Importera vanliga fastigheter- svara Ja." SKIP 
                 "Importera samfälligheter/Gemensamhetsanläggninar - svara Nej"
         VIEW-AS ALERT-BOX
            QUESTION BUTTONS YES-NO TITLE "Import"  UPDATE valFS AS LOGICAL.   
      END.
   END.
         
   SYSTEM-DIALOG GET-FILE fildir
   TITLE          "Välj den Metria-fil som Ni vill läsa in"
   FILTERS        "All Files (*.xls;*.xlsx)"  "*.xls;*.xlsx"
   INITIAL-DIR    mappvar
   UPDATE OKvald.      
   IF OKvald = TRUE THEN DO:                
      {muswait.i}
      EMPTY TEMP-TABLE fasttmp NO-ERROR. 
      EMPTY TEMP-TABLE marktmp NO-ERROR.
      IF Guru.Konstanter:globforetag = "tect" THEN DO:             
         IF  valM = TRUE THEN DO:         
            RUN FASTEXELIN.P (INPUT 5, INPUT fildir,OUTPUT TABLE fasttmp ,OUTPUT TABLE marktmp ).
         END.
         ELSE  DO:
            IF valFS = TRUE THEN RUN FASTEXELIN.P (INPUT 6, INPUT fildir,OUTPUT TABLE fasttmp ,OUTPUT TABLE marktmp ).
            IF valFS = FALSE  THEN RUN FASTEXELIN.P (INPUT 7, INPUT fildir,OUTPUT TABLE fasttmp ,OUTPUT TABLE marktmp ).
         END.
      END.
      ELSE DO:
         /*vattenfall har ett annat Metria format än tectel*/
         /*lule*/
         RUN FASTEXELIN.P (INPUT 11, INPUT fildir,OUTPUT TABLE fasttmp ,OUTPUT TABLE marktmp ).
      END.               
                     
      EMPTY TEMP-TABLE felex NO-ERROR. 
      FOR EACH fasttmp:
         FIND FIRST komuntemp WHERE komuntemp.BENAMNING = fasttmp.KOMMUN NO-LOCK NO-ERROR.
         IF AVAILABLE komuntemp  THEN DO:
            FIND FIRST lantemp WHERE lantemp.LANID = komuntemp.LANID NO-LOCK NO-ERROR.
            IF AVAILABLE lantemp THEN DO:
               fasttmp.LAN = lantemp.BENAMNING .
            END.
         END.
      END.      

      /*FOR EACH fasttmp WHERE NO-LOCK:
         MESSAGE fasttmp.beteckning fasttmp.kommun VIEW-AS ALERT-BOX.
      END.*/

     /* FOR EACH marktmp WHERE NO-LOCK:
         MESSAGE marktmp.beteckning marktmp.MARKAGARE  marktmp.GATUADRESS  marktmp.POSTNUMMER  marktmp.POSTADRESS  marktmp.PERSONNUMMER  marktmp.andel MARKTMP.TELEFON   VIEW-AS ALERT-BOX.
      END.*/
      
      RUN impfast_UI IN varderinginapph (INPUT valvardnr, INPUT TABLE fasttmp,INPUT TABLE marktmp,OUTPUT TABLE felex).      
      EMPTY TEMP-TABLE fastighettemp NO-ERROR. 
      EMPTY TEMP-TABLE fastvardtemp NO-ERROR. 
      EMPTY TEMP-TABLE varderingtemp NO-ERROR. 
      RUN fastighmt IN fastigregapph (INPUT valvardnr,OUTPUT TABLE varderingtemp,OUTPUT TABLE fastvardtemp).
            
      RUN hmtfast_UI.
      
      FIND FIRST fastvardtemp NO-ERROR.
      IF AVAILABLE fastvardtemp THEN DO:
         fastighbet = fastvardtemp.BETECKNING.
      END.      
      RUN uppdat_UI.

      /*FOR EACH felex: 
         MESSAGE felex.MARKAGARE felex.BETECKNING  VIEW-AS ALERT-BOX.
      END.      */
      FIND FIRST felex NO-ERROR.
      IF AVAILABLE felex THEN DO:
         felfil = SESSION:TEMP-DIR + STRING(TIME) + ".txt". 
         {AMERICANEUROPEAN.I}      
         OUTPUT TO VALUE(felfil).
         PUT "Dessa markägare har inget personnummer, eller så har fastigheten ingen markägare. Var god och kontrollera.!" AT 6.         
         PUT SKIP.         
         PUT SKIP.         
         FOR EACH felex:           
            PUT "Fastighet:" FORMAT "X(10)" AT 1 felex.BETECKNING FORMAT "X(30)"  AT 12 "Markägare:" FORMAT "X(10)" AT 50  felex.MARKAGARE FORMAT "X(30)"  AT 62.  
            PUT SKIP.
         END.
         OUTPUT CLOSE.
         {EUROPEANAMERICAN.I}
         RUN OPENDOC.P (felfil,"","",NO).         
      END.   
      {musarrow.i}
   END.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_IMPEXTEKIS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_IMPEXTEKIS C-Win
ON CHOOSE OF BTN_IMPEXTEKIS IN FRAME DEFAULT-FRAME /* Import Tekis */
DO:   
   
   DEFINE VARIABLE OKvald AS LOGICAL NO-UNDO.
   
   fildir = SESSION:TEMP-DIRECTORY + Guru.Konstanter:globanv + "\".
   {SESSIONTEMPDIR.I}
   IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN fildir = webclienttempdir.
   OS-CREATE-DIR VALUE(fildir) NO-ERROR.
   mappvar = fildir.
   SYSTEM-DIALOG GET-FILE fildir
   TITLE          "Välj den Tekis-fil som Ni vill läsa in"
   FILTERS        "All Files (*.xls;*.xlsx)"  "*.xls;*.xlsx"
   INITIAL-DIR    mappvar
   UPDATE OKvald.      
   IF OKvald = TRUE THEN DO:                
      {muswait.i}
      EMPTY TEMP-TABLE fasttmp NO-ERROR. 
      EMPTY TEMP-TABLE marktmp NO-ERROR.       
      
      IF Guru.Konstanter:globforetag = "UMEA" OR  Guru.Konstanter:globforetag = "UMBR" THEN RUN FASTEXELIN.P (INPUT 3, INPUT fildir,OUTPUT TABLE fasttmp ,OUTPUT TABLE marktmp ).
      /*ELSE IF Guru.Konstanter:globforetag = "POWE" THEN RUN FASTEXELIN.P (INPUT 8, INPUT fildir,OUTPUT TABLE fasttmp ,OUTPUT TABLE marktmp ).*/
      ELSE RUN FASTEXELIN.P (INPUT 2, INPUT fildir,OUTPUT TABLE fasttmp ,OUTPUT TABLE marktmp ).
      EMPTY TEMP-TABLE felex NO-ERROR. 
      FOR EACH fasttmp:
         FIND FIRST komuntemp WHERE komuntemp.BENAMNING = fasttmp.KOMMUN NO-LOCK NO-ERROR.
         IF AVAILABLE komuntemp  THEN DO:
            FIND FIRST lantemp WHERE lantemp.LANID = komuntemp.LANID NO-LOCK NO-ERROR.
            IF AVAILABLE lantemp THEN DO:
               fasttmp.LAN = lantemp.BENAMNING .
            END.
         END.
      END.      

      /*FOR EACH fasttmp WHERE NO-LOCK:
         MESSAGE fasttmp.beteckning fasttmp.kommun VIEW-AS ALERT-BOX.
      END.*/

     /* FOR EACH marktmp WHERE NO-LOCK:
         MESSAGE marktmp.beteckning marktmp.MARKAGARE  marktmp.GATUADRESS  marktmp.POSTNUMMER  marktmp.POSTADRESS  marktmp.PERSONNUMMER  marktmp.andel MARKTMP.TELEFON   VIEW-AS ALERT-BOX.
      END.*/
      
      RUN impfast_UI IN varderinginapph (INPUT valvardnr, INPUT TABLE fasttmp,INPUT TABLE marktmp,OUTPUT TABLE felex).      
      EMPTY TEMP-TABLE fastighettemp NO-ERROR. 
      EMPTY TEMP-TABLE fastvardtemp NO-ERROR. 
      EMPTY TEMP-TABLE varderingtemp NO-ERROR. 
      RUN fastighmt IN fastigregapph (INPUT valvardnr,OUTPUT TABLE varderingtemp,OUTPUT TABLE fastvardtemp).
            
      RUN hmtfast_UI.
      
      FIND FIRST fastvardtemp NO-ERROR.
      IF AVAILABLE fastvardtemp THEN DO:
         fastighbet = fastvardtemp.BETECKNING.
      END.      
      RUN uppdat_UI.

      /*FOR EACH felex: 
         MESSAGE felex.MARKAGARE felex.BETECKNING  VIEW-AS ALERT-BOX.
      END.      */
      FIND FIRST felex NO-ERROR.
      IF AVAILABLE felex THEN DO:
         felfil = SESSION:TEMP-DIR + STRING(TIME) + ".txt". 
         {AMERICANEUROPEAN.I}      
         OUTPUT TO VALUE(felfil).
         PUT "Dessa markägare har inget personnummer, eller så har fastigheten ingen markägare. Var god och kontrollera.!" AT 6.         
         PUT SKIP.         
         PUT SKIP.         
         FOR EACH felex:           
            PUT "Fastighet:" FORMAT "X(10)" AT 1 felex.BETECKNING FORMAT "X(30)"  AT 12 "Markägare:" FORMAT "X(10)" AT 50  felex.MARKAGARE FORMAT "X(30)"  AT 62.  
            PUT SKIP.
         END.
         OUTPUT CLOSE.
         {EUROPEANAMERICAN.I}
         RUN OPENDOC.P (felfil,"","",NO).         
      END.   
      {musarrow.i}
   END.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_IMPEXVISMA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_IMPEXVISMA C-Win
ON CHOOSE OF BTN_IMPEXVISMA IN FRAME DEFAULT-FRAME /* Import Visma */
DO:   
   
   DEFINE VARIABLE OKvald AS LOGICAL NO-UNDO.
   
   fildir = SESSION:TEMP-DIRECTORY + Guru.Konstanter:globanv + "\".
   {SESSIONTEMPDIR.I}
   IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN fildir = webclienttempdir.
   OS-CREATE-DIR VALUE(fildir) NO-ERROR.
   mappvar = fildir.
   SYSTEM-DIALOG GET-FILE fildir
   TITLE          "Välj den Visma-fil som Ni vill läsa in"
   FILTERS        "All Files (*.xls;*.xlsx)"  "*.xls;*.xlsx"
   INITIAL-DIR    mappvar
   UPDATE OKvald.      
   IF OKvald = TRUE THEN DO:                
      {muswait.i}
      EMPTY TEMP-TABLE fasttmp NO-ERROR. 
      EMPTY TEMP-TABLE marktmp NO-ERROR.       
     
      RUN FASTEXELIN.P (INPUT 9, INPUT fildir,OUTPUT TABLE fasttmp ,OUTPUT TABLE marktmp ).    
      EMPTY TEMP-TABLE felex NO-ERROR. 
      FOR EACH fasttmp:
         FIND FIRST komuntemp WHERE komuntemp.BENAMNING = fasttmp.KOMMUN NO-LOCK NO-ERROR.
         IF AVAILABLE komuntemp  THEN DO:
            FIND FIRST lantemp WHERE lantemp.LANID = komuntemp.LANID NO-LOCK NO-ERROR.
            IF AVAILABLE lantemp THEN DO:
               fasttmp.LAN = lantemp.BENAMNING .
            END.
         END.
      END.      

      /*FOR EACH fasttmp WHERE NO-LOCK:
         MESSAGE fasttmp.beteckning fasttmp.kommun VIEW-AS ALERT-BOX.
      END.*/

     /* FOR EACH marktmp WHERE NO-LOCK:
         MESSAGE marktmp.beteckning marktmp.MARKAGARE  marktmp.GATUADRESS  marktmp.POSTNUMMER  marktmp.POSTADRESS  marktmp.PERSONNUMMER  marktmp.andel MARKTMP.TELEFON   VIEW-AS ALERT-BOX.
      END.*/
      
      RUN impfast_UI IN varderinginapph (INPUT valvardnr, INPUT TABLE fasttmp,INPUT TABLE marktmp,OUTPUT TABLE felex).      
      EMPTY TEMP-TABLE fastighettemp NO-ERROR. 
      EMPTY TEMP-TABLE fastvardtemp NO-ERROR. 
      EMPTY TEMP-TABLE varderingtemp NO-ERROR. 
      RUN fastighmt IN fastigregapph (INPUT valvardnr,OUTPUT TABLE varderingtemp,OUTPUT TABLE fastvardtemp).
            
      RUN hmtfast_UI.
      
      FIND FIRST fastvardtemp NO-ERROR.
      IF AVAILABLE fastvardtemp THEN DO:
         fastighbet = fastvardtemp.BETECKNING.
      END.      
      RUN uppdat_UI.

      /*FOR EACH felex: 
         MESSAGE felex.MARKAGARE felex.BETECKNING  VIEW-AS ALERT-BOX.
      END.      */
      FIND FIRST felex NO-ERROR.
      IF AVAILABLE felex THEN DO:
         felfil = SESSION:TEMP-DIR + STRING(TIME) + ".txt". 
         {AMERICANEUROPEAN.I}      
         OUTPUT TO VALUE(felfil).
         PUT "Dessa markägare har inget personnummer, eller så har fastigheten ingen markägare. Var god och kontrollera.!" AT 6.         
         PUT SKIP.         
         PUT SKIP.         
         FOR EACH felex:           
            PUT "Fastighet:" FORMAT "X(10)" AT 1 felex.BETECKNING FORMAT "X(30)"  AT 12 "Markägare:" FORMAT "X(10)" AT 50  felex.MARKAGARE FORMAT "X(30)"  AT 62.  
            PUT SKIP.
         END.
         OUTPUT CLOSE.
         {EUROPEANAMERICAN.I}
         RUN OPENDOC.P (felfil,"","",NO).         
      END.   
      {musarrow.i}
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_IMPGURU
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_IMPGURU C-Win
ON CHOOSE OF BTN_IMPGURU IN FRAME DEFAULT-FRAME /* Import Guru */
DO:   
   
   DEFINE VARIABLE OKvald AS LOGICAL NO-UNDO.
   
   fildir = SESSION:TEMP-DIRECTORY + Guru.Konstanter:globanv + "\".
   {SESSIONTEMPDIR.I}
   IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN fildir = webclienttempdir.
   OS-CREATE-DIR VALUE(fildir) NO-ERROR.
   mappvar = fildir.
   SYSTEM-DIALOG GET-FILE fildir
   TITLE          "Välj den Guru-fil med fastigheter och markägare som Ni vill läsa in"
   FILTERS        "All Files (*.xls;*.xlsx)"  "*.xls;*.xlsx"
   INITIAL-DIR    mappvar
   UPDATE OKvald.      
   IF OKvald = TRUE THEN DO:                
      {muswait.i}
      EMPTY TEMP-TABLE fasttmp NO-ERROR. 
      EMPTY TEMP-TABLE marktmp NO-ERROR.       
     
     RUN FASTEXELIN.P (INPUT 12, INPUT fildir,OUTPUT TABLE fasttmp ,OUTPUT TABLE marktmp ).    
      EMPTY TEMP-TABLE felex NO-ERROR. 
      FOR EACH fasttmp:
         FIND FIRST komuntemp WHERE komuntemp.BENAMNING = fasttmp.KOMMUN NO-LOCK NO-ERROR.
         IF AVAILABLE komuntemp  THEN DO:
            FIND FIRST lantemp WHERE lantemp.LANID = komuntemp.LANID NO-LOCK NO-ERROR.
            IF AVAILABLE lantemp THEN DO:
               fasttmp.LAN = lantemp.BENAMNING .
            END.
         END.
      END.      

      /*FOR EACH fasttmp WHERE NO-LOCK:
         MESSAGE fasttmp.beteckning fasttmp.kommun VIEW-AS ALERT-BOX.
      END.*/

     /* FOR EACH marktmp WHERE NO-LOCK:
         MESSAGE marktmp.beteckning marktmp.MARKAGARE  marktmp.GATUADRESS  marktmp.POSTNUMMER  marktmp.POSTADRESS  marktmp.PERSONNUMMER  marktmp.andel MARKTMP.TELEFON   VIEW-AS ALERT-BOX.
      END.*/
      
      RUN impfast_UI IN varderinginapph (INPUT valvardnr, INPUT TABLE fasttmp,INPUT TABLE marktmp,OUTPUT TABLE felex).      
      EMPTY TEMP-TABLE fastighettemp NO-ERROR. 
      EMPTY TEMP-TABLE fastvardtemp NO-ERROR. 
      EMPTY TEMP-TABLE varderingtemp NO-ERROR. 
      RUN fastighmt IN fastigregapph (INPUT valvardnr,OUTPUT TABLE varderingtemp,OUTPUT TABLE fastvardtemp).
            
      RUN hmtfast_UI.
      
      FIND FIRST fastvardtemp NO-ERROR.
      IF AVAILABLE fastvardtemp THEN DO:
         fastighbet = fastvardtemp.BETECKNING.
      END.      
      RUN uppdat_UI.

      /*FOR EACH felex: 
         MESSAGE felex.MARKAGARE felex.BETECKNING  VIEW-AS ALERT-BOX.
      END.      */
      FIND FIRST felex NO-ERROR.
      IF AVAILABLE felex THEN DO:
         felfil = SESSION:TEMP-DIR + STRING(TIME) + ".txt". 
         {AMERICANEUROPEAN.I}      
         OUTPUT TO VALUE(felfil).
         PUT "Dessa markägare har inget personnummer, eller så har fastigheten ingen markägare. Var god och kontrollera.!" AT 6.         
         PUT SKIP.         
         PUT SKIP.         
         FOR EACH felex:           
            PUT "Fastighet:" FORMAT "X(10)" AT 1 felex.BETECKNING FORMAT "X(30)"  AT 12 "Markägare:" FORMAT "X(10)" AT 50  felex.MARKAGARE FORMAT "X(30)"  AT 62.  
            PUT SKIP.
         END.
         OUTPUT CLOSE.
         {EUROPEANAMERICAN.I}
         RUN OPENDOC.P (felfil,"","",NO).         
      END.   
      {musarrow.i}
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NY
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NY C-Win
ON CHOOSE OF BTN_NY IN FRAME DEFAULT-FRAME /* Ny */
DO:
   FIND FIRST fastighettemp USE-INDEX FAST NO-LOCK NO-ERROR.
   IF AVAILABLE fastighettemp THEN DO:
      status-ok = BRW_FAST:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
      APPLY "VALUE-CHANGED" TO BRW_FAST.
   END.
   fastighbet = ?.
   {muswait.i}
   vart = "NYA".  
   RUN ANDFASTIGN.W (INPUT CMB_LAN, INPUT CMB_KOMMUN, INPUT-OUTPUT fastighbet,INPUT TABLE kfastighettemp).   
   IF musz = TRUE THEN DO:
      musz = FALSE.      
      /* Om man försökt lägga upp en redan befintlig fastighet. Hämta upp denna*/
      FIND FIRST fastighettemp WHERE fastighettemp.BETECKNING = fastighbet NO-LOCK NO-ERROR.
      IF NOT AVAILABLE fastighettemp THEN DO:
         IF Guru.Konstanter:appcon THEN DO:                           
            RUN PHMTENFAST.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
            (INPUT fastighbet ,OUTPUT TABLE fastighettemp APPEND).
         END.
         ELSE DO:
            RUN PHMTENFAST.P  
            (INPUT fastighbet,OUTPUT TABLE fastighettemp APPEND).
         END.                              
      END.
   END.    
   RUN fast_UI.     
   FIND FIRST fastighettemp WHERE fastighettemp.BETECKNING = fastighbet NO-LOCK NO-ERROR.
   IF AVAILABLE fastighettemp  THEN DO:
      EMPTY TEMP-TABLE kfastighettemp  NO-ERROR. 
      CREATE kfastighettemp.
      BUFFER-COPY fastighettemp TO kfastighettemp.  
   END.
   {musarrow.i} 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_OVER
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OVER C-Win
ON CHOOSE OF BTN_OVER IN FRAME DEFAULT-FRAME
DO:
   IF radvar = 0 THEN RETURN NO-APPLY.
   IF brwval(1) = TRUE THEN RETURN NO-APPLY.
   musz = TRUE.            
   antal_valda = BRW_FAST:NUM-SELECTED-ROWS.         
   antal_raknare = 1.
   DO WHILE antal_raknare LE antal_valda:                                   
      status-ok = BRW_FAST:FETCH-SELECTED-ROW(antal_raknare).                          
      RUN over_UI.           
   END.       
   
   RUN openbdynspec_UI IN brwproc[2].      
   FIND FIRST fastvardtemp WHERE ROWID(fastvardtemp) = reprowid NO-LOCK NO-ERROR.
   IF AVAILABLE fastvardtemp THEN DO:
      RUN setlastrowid_UI IN brwproc[2] (INPUT ROWID(fastvardtemp)).
      RUN lastselectdyn_UI IN brwproc[2].        
   END.
   RUN title_UI IN brwproc[2].
   /*
   status-ok = BRW_FAST:DESELECT-ROWS().   
   APPLY "VALUE-CHANGED" TO BRW_VFAST.
   */
   musz = FALSE.        
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_UPP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_UPP C-Win
ON CHOOSE OF BTN_UPP IN FRAME DEFAULT-FRAME /* Ändra */
DO:
   IF brwval(1) = TRUE THEN RETURN NO-APPLY.
   RUN andra_UI. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_KOMMUN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_KOMMUN C-Win
ON VALUE-CHANGED OF CMB_KOMMUN IN FRAME DEFAULT-FRAME /* Kommun */
DO:
  CMB_KOMMUN = INPUT CMB_KOMMUN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_LAN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_LAN C-Win
ON VALUE-CHANGED OF CMB_LAN IN FRAME DEFAULT-FRAME /* Län */
DO:
   CMB_LAN = INPUT CMB_LAN.
   CMB_KOMMUN:LIST-ITEMS = "". 
   status-ok = CMB_KOMMUN:ADD-FIRST("Alla").
   IF CMB_LAN = "Alla" THEN DO:
      FOR EACH komuntemp USE-INDEX BENAMNING:
         status-ok = CMB_KOMMUN:ADD-LAST(komuntemp.BENAMNING).
      END. 
   END.
   ELSE DO:
      FIND FIRST lantemp WHERE lantemp.BENAMNING = CMB_LAN NO-LOCK NO-ERROR.     

      FOR EACH komuntemp WHERE komuntemp.LANID = lantemp.LANID USE-INDEX BENAMNING:
         status-ok = CMB_KOMMUN:ADD-LAST(komuntemp.BENAMNING).
      END.      
   END.
   CMB_KOMMUN:SCREEN-VALUE = "Alla".
   CMB_KOMMUN = "Alla".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_OK C-Win
ON CHOOSE OF FBTN_OK IN FRAME DEFAULT-FRAME /* Markägarförteckning/Koppla markägare till fastighet */
DO: 
   IF brwval(1) = TRUE THEN RETURN NO-APPLY.
   FIND FIRST varderingtemp WHERE varderingtemp.VARDNR = valvardnr NO-LOCK NO-ERROR.
   APPLY "VALUE-CHANGED" TO BRW_FAST.   
   valdfast = ?.
   IF radvar = 1 THEN DO:      
      RUN btnok IN varderinginapph (INPUT valvardnr,INPUT TABLE fastvardtemp).
   END.
   DEFINE VARIABLE korvar AS CHARACTER NO-UNDO.
   {AVBGOM.I} 
   fastighbet = fastighettemp.BETECKNING.
   /*markägare andel*/
   RUN MARKAGAREU3.W (INPUT valvardnr,INPUT 1,INPUT franvart,INPUT fastighbet,OUTPUT korvar).
   
   
   {AVBFRAM.I}
   RUN uppdat_UI.
   
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-KOMMUN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-KOMMUN C-Win
ON ANY-KEY OF FILL-IN-KOMMUN IN FRAME DEFAULT-FRAME /* Kommun */
DO:
  IF KEYFUNCTION(LASTKEY) = ("END-ERROR") THEN  RETURN NO-APPLY. 
  IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN-KOMMUN IN FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-KOMMUN C-Win
ON LEAVE OF FILL-IN-KOMMUN IN FRAME DEFAULT-FRAME /* Kommun */
DO:
  FILL-IN-KOMMUN = INPUT FILL-IN-KOMMUN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-KOMMUN C-Win
ON MOUSE-SELECT-DBLCLICK OF FILL-IN-KOMMUN IN FRAME DEFAULT-FRAME /* Kommun */
DO:
   FILL-IN-KOMMUN = INPUT FILL-IN-KOMMUN.
    status-ok = BRW_FAST:SELECT-FOCUSED-ROW() NO-ERROR.
    APPLY "VALUE-CHANGED" TO BRW_FAST.
    fastrec = RECID(fastighettemp).
    IF FILL-IN-KOMMUN = '' THEN DO:
       MESSAGE "Sökbegreppet kan inte vara blankt." VIEW-AS ALERT-BOX.
       APPLY "ENTRY" TO FILL-IN-KOMMUN IN FRAME {&FRAME-NAME}.
       RETURN NO-APPLY.
    END.
    ortkalks = '*' + FILL-IN-KOMMUN + '*'.
    FIND fastighettemp WHERE RECID(fastighettemp) = fastrec NO-LOCK NO-ERROR.
    FIND NEXT fastighettemp WHERE fastighettemp.KOMMUN MATCHES ortkalks
    USE-INDEX FASTIGHET NO-LOCK NO-ERROR.
    IF NOT AVAILABLE fastighettemp THEN DO:
       FIND FIRST fastighettemp WHERE fastighettemp.KOMMUN MATCHES ortkalks
       USE-INDEX FASTIGHET NO-LOCK NO-ERROR.
       IF NOT AVAILABLE fastighettemp THEN DO:
          MESSAGE "Det finns inget på sökbegreppet." VIEW-AS ALERT-BOX.
          APPLY "ENTRY" TO FILL-IN-KOMMUN IN FRAME {&FRAME-NAME}.
          RETURN NO-APPLY.
       END.
    END.
    IF AVAILABLE fastighettemp THEN DO:
       RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(fastighettemp)).
       RUN lastselectdyn_UI IN brwproc[1].       
    END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-lan
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-lan C-Win
ON ANY-KEY OF FILL-IN-lan IN FRAME DEFAULT-FRAME /* Län */
DO:
   IF KEYFUNCTION(LASTKEY) = ("END-ERROR") THEN  RETURN NO-APPLY. 
  IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN-LAN IN FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-lan C-Win
ON LEAVE OF FILL-IN-lan IN FRAME DEFAULT-FRAME /* Län */
DO:
  FILL-IN-LAN = INPUT FILL-IN-LAN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-lan C-Win
ON MOUSE-SELECT-DBLCLICK OF FILL-IN-lan IN FRAME DEFAULT-FRAME /* Län */
DO:
   FILL-IN-LAN = INPUT FILL-IN-LAN.
    status-ok = BRW_FAST:SELECT-FOCUSED-ROW() NO-ERROR.
    APPLY "VALUE-CHANGED" TO BRW_FAST.
    fastrec = RECID(fastighettemp).
    IF FILL-IN-LAN = '' THEN DO:
       MESSAGE "Sökbegreppet kan inte vara blankt." VIEW-AS ALERT-BOX.
       APPLY "ENTRY" TO FILL-IN-LAN IN FRAME {&FRAME-NAME}.
       RETURN NO-APPLY.
    END.
    ortkalks = '*' + FILL-IN-LAN + '*'.
    FIND fastighettemp WHERE RECID(fastighettemp) = fastrec NO-LOCK NO-ERROR.
    FIND NEXT fastighettemp WHERE fastighettemp.VAKER MATCHES ortkalks
    USE-INDEX FASTIGHET NO-LOCK NO-ERROR.
    IF NOT AVAILABLE fastighettemp THEN DO:
       FIND FIRST fastighettemp WHERE fastighettemp.VAKER MATCHES ortkalks
       USE-INDEX FASTIGHET NO-LOCK NO-ERROR.
       IF NOT AVAILABLE fastighettemp THEN DO:
          MESSAGE "Det finns inget på sökbegreppet." VIEW-AS ALERT-BOX.
          APPLY "ENTRY" TO FILL-IN-LAN IN FRAME {&FRAME-NAME}.
          RETURN NO-APPLY.
       END.
    END.
    IF AVAILABLE fastighettemp THEN DO:
       RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(fastighettemp)).
       RUN lastselectdyn_UI IN brwproc[1].       
    END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_BETECKNING
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_BETECKNING C-Win
ON ANY-KEY OF FILL-IN_BETECKNING IN FRAME DEFAULT-FRAME /* Fastighetsbeteckning */
DO:
   IF KEYFUNCTION(LASTKEY) = ("END-ERROR") THEN  RETURN NO-APPLY. 
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN_BETECKNING IN FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_BETECKNING C-Win
ON LEAVE OF FILL-IN_BETECKNING IN FRAME DEFAULT-FRAME /* Fastighetsbeteckning */
DO:
   FILL-IN_BETECKNING = INPUT FILL-IN_BETECKNING.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_BETECKNING C-Win
ON MOUSE-SELECT-DBLCLICK OF FILL-IN_BETECKNING IN FRAME DEFAULT-FRAME /* Fastighetsbeteckning */
DO: 
   FILL-IN_BETECKNING = INPUT FILL-IN_BETECKNING.
   status-ok = BRW_FAST:SELECT-FOCUSED-ROW() NO-ERROR.
   APPLY "VALUE-CHANGED" TO BRW_FAST.
   fastrec = RECID(fastighettemp).
   IF FILL-IN_BETECKNING = '' THEN DO:
      MESSAGE "Sökbegreppet kan inte vara blankt." VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN_BETECKNING IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
   END.
   ortkalks = '*' + FILL-IN_BETECKNING + '*'.
   FIND fastighettemp WHERE RECID(fastighettemp) = fastrec NO-LOCK NO-ERROR.
   FIND NEXT fastighettemp WHERE fastighettemp.BETECKNING MATCHES ortkalks
   USE-INDEX FASTIGHET NO-LOCK NO-ERROR.
   IF NOT AVAILABLE fastighettemp THEN DO:
      FIND FIRST fastighettemp WHERE fastighettemp.BETECKNING MATCHES ortkalks
      USE-INDEX FASTIGHET NO-LOCK NO-ERROR.
      IF NOT AVAILABLE fastighettemp THEN DO:
         MESSAGE "Det finns inget på sökbegreppet." VIEW-AS ALERT-BOX.
         APPLY "ENTRY" TO FILL-IN_BETECKNING IN FRAME {&FRAME-NAME}.
         RETURN NO-APPLY.
      END.
   END.
   IF AVAILABLE fastighettemp THEN DO:
      RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(fastighettemp)).
      RUN lastselectdyn_UI IN brwproc[1].       
   END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_FAST
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
   EMPTY TEMP-TABLE fastighettemp NO-ERROR. 
   RUN fastighmt IN fastigregapph (INPUT valvardnr,OUTPUT TABLE varderingtemp,OUTPUT TABLE fastvardtemp).
   
   status-ok = CMB_LAN:ADD-LAST("Alla").
   FOR EACH lantemp USE-INDEX BENAMNING:
      CMB_LAN:ADD-LAST(lantemp.BENAMNING).
   END.      
   status-ok = CMB_KOMMUN:ADD-LAST("Alla").
   FOR EACH komuntemp  USE-INDEX BENAMNING:
      CMB_KOMMUN:add-last(komuntemp.BENAMNING).
   END.
   DEBUGGER:SET-BREAK().
   RUN lankomhmt IN fastigregapph (INPUT valvardnr,INPUT TABLE lantemp, OUTPUT hmtkommun,OUTPUT hmtlan).  
   CMB_LAN:SCREEN-VALUE IN FRAME {&FRAME-NAME} = hmtlan.
   CMB_LAN = hmtlan.
   CMB_KOMMUN:SCREEN-VALUE IN FRAME {&FRAME-NAME} = hmtkommun.   
   CMB_KOMMUN = hmtkommun.
   /*CMB_KOMMUN:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING("Alla").   
   CMB_KOMMUN = "Alla".*/
   APPLY "VALUE-CHANGED" TO CMB_LAN.
   IF CMB_LAN NE "Alla" THEN RUN btnhmt_UI.      
   RUN openbdyn_UI IN brwproc[1] (INPUT "").
   RUN fetchselrowid_UI IN brwproc[1].
   
   IF valvardnr NE ? THEN DO:
      FIND FIRST varderingtemp WHERE varderingtemp.VARDNR = valvardnr NO-LOCK NO-ERROR.   
      IF AVAILABLE varderingtemp THEN DO:      
         {&WINDOW-NAME}:TITLE = "Registrera/ändra  fastighet, koppla fastighet till värderingsnummer : " + STRING(varderingtemp.VARDNR) + " " + varderingtemp.BENAMNING.     
      END.
   END.   
     
   RUN enable_UI.
   {FRMSIZE.I}   
   /*IF Guru.Konstanter:globforetag = "TECT" OR Guru.Konstanter:globforetag = "PICA"  OR Guru.Konstanter:globforetag = "SWEO" OR Guru.Konstanter:globforetag = "FORS" OR Guru.Konstanter:globforetag = "REJI" OR Guru.Konstanter:globforetag = "ELPC" OR Guru.Konstanter:globforetag = "SKOK" OR Guru.Konstanter:globforetag = "JSBF"  OR Guru.Konstanter:globforetag = "ATS" OR Guru.Konstanter:globforetag = "ELPA" THEN.
   ELSE BTN_IMPEX:HIDDEN = TRUE.*/
   IF Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "UMEA" OR Guru.Konstanter:globforetag = "UMBR" OR Guru.Konstanter:globforetag = "elpa" THEN BTN_IMPEXTEKIS:VISIBLE = TRUE.
   ELSE BTN_IMPEXTEKIS:VISIBLE = FALSE.
   IF Guru.Konstanter:globforetag = "TECT" OR Guru.Konstanter:globforetag = "VAST" OR Guru.Konstanter:globforetag = "lule" OR Guru.Konstanter:globforetag = "GRAN" THEN BTN_IMPEXMETRIA:VISIBLE = TRUE.
   ELSE BTN_IMPEXMETRIA:VISIBLE = FALSE.
   IF Guru.Konstanter:globforetag = "POWE" OR  Guru.Konstanter:globforetag = "WSP"   THEN BTN_IMPEXVISMA:VISIBLE = TRUE.
   ELSE BTN_IMPEXVISMA:VISIBLE = FALSE.
   IF   Guru.Konstanter:globforetag = "WSP" OR  Guru.Konstanter:globforetag = "GRAN"  THEN BTN_IMPEXEON:VISIBLE = TRUE.
   ELSE BTN_IMPEXEON:VISIBLE = FALSE.
   
   
   Guru.GlobalaVariabler:collefth = ?.
   
   IF Guru.GlobalaVariabler:collefth = ? AND BTN_NY:VISIBLE IN FRAME {&FRAME-NAME} = TRUE  THEN DO:
      Guru.GlobalaVariabler:collefth = BTN_NY:HANDLE.
   END.
   IF BTN_UPP:VISIBLE IN FRAME {&FRAME-NAME} = TRUE   THEN DO:
      IF Guru.GlobalaVariabler:collefth = ? THEN Guru.GlobalaVariabler:collefth = BTN_UPP:HANDLE.
      Guru.GlobalaVariabler:colrighth = BTN_UPP:HANDLE.           
      RUN buttcol_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   END.
   
   IF BTN_BORT:VISIBLE IN FRAME {&FRAME-NAME} = TRUE   THEN DO:
      IF Guru.GlobalaVariabler:collefth = ? THEN Guru.GlobalaVariabler:collefth = BTN_BORT:HANDLE.
      Guru.GlobalaVariabler:colrighth = BTN_BORT:HANDLE.           
      RUN buttcol_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   END.
   
   IF BTN_IMPEX:VISIBLE IN FRAME {&FRAME-NAME} = TRUE  THEN DO:
      IF Guru.GlobalaVariabler:collefth = ? THEN Guru.GlobalaVariabler:collefth = BTN_IMPEX:HANDLE.      
      Guru.GlobalaVariabler:colrighth = BTN_IMPEX:HANDLE.           
      RUN buttcol_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   END.
   IF BTN_IMPEXTEKIS:VISIBLE IN FRAME {&FRAME-NAME} = TRUE   THEN DO:      
      IF Guru.GlobalaVariabler:collefth = ? THEN Guru.GlobalaVariabler:collefth = BTN_IMPEXTEKIS:HANDLE.
      Guru.GlobalaVariabler:colrighth = BTN_IMPEXTEKIS:HANDLE.           
      RUN buttcol_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   END.
   IF BTN_IMPEXMETRIA:VISIBLE IN FRAME {&FRAME-NAME} = TRUE  THEN DO:      
      IF Guru.GlobalaVariabler:collefth = ? THEN Guru.GlobalaVariabler:collefth = BTN_IMPEXMETRIA:HANDLE.
      Guru.GlobalaVariabler:colrighth = BTN_IMPEXMETRIA:HANDLE.           
      RUN buttcol_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   END.
   IF BTN_IMPEXVISMA:VISIBLE IN FRAME {&FRAME-NAME} = TRUE  THEN DO:      
      IF Guru.GlobalaVariabler:collefth = ? THEN Guru.GlobalaVariabler:collefth = BTN_IMPEXVISMA:HANDLE.
      Guru.GlobalaVariabler:colrighth = BTN_IMPEXVISMA:HANDLE.           
      RUN buttcol_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   END.
   IF BTN_IMPEXEON:VISIBLE IN FRAME {&FRAME-NAME} = TRUE  THEN DO:      
      IF Guru.GlobalaVariabler:collefth = ? THEN Guru.GlobalaVariabler:collefth = BTN_IMPEXEON:HANDLE.
      Guru.GlobalaVariabler:colrighth = BTN_IMPEXEON:HANDLE.           
      RUN buttcol_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   END.
   IF BTN_IMPGURU:VISIBLE IN FRAME {&FRAME-NAME} = TRUE  THEN DO:      
      IF Guru.GlobalaVariabler:collefth = ? THEN Guru.GlobalaVariabler:collefth = BTN_IMPGURU:HANDLE.
      Guru.GlobalaVariabler:colrighth = BTN_IMPGURU:HANDLE.           
      RUN buttcol_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   END.
   IF BTN_EXPGURUMALL:VISIBLE IN FRAME {&FRAME-NAME} = TRUE  THEN DO:      
      IF Guru.GlobalaVariabler:collefth = ? THEN Guru.GlobalaVariabler:collefth = BTN_EXPGURUMALL:HANDLE.
      Guru.GlobalaVariabler:colrighth = BTN_EXPGURUMALL:HANDLE.           
      RUN buttcol_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   END.
   
   
   FIND FIRST fastighettemp NO-LOCK NO-ERROR.
       
   RUN hmtfast_UI.
   
   APPLY "VALUE-CHANGED" TO BRW_VFAST.
   /*IF radvar = 0 THEN DO:
      ASSIGN
      FILL-IN-TEXT:HIDDEN = TRUE
      BTN_OVER:HIDDEN = TRUE
      BTN_BACK:HIDDEN = TRUE
      BRW_VFAST:HIDDEN = TRUE
      BRW_FAST:WIDTH-CHARS = 120
      BTN_NY:COLUMN = BTN_NY:COLUMN + 20
      BTN_UPP:COLUMN = BTN_NY:COLUMN + 14
      BTN_BORT:COLUMN = BTN_UPP:COLUMN + 14
      BTN_IMPEX:COLUMN = BTN_BORT:COLUMN + 14.
      BTN_IMPEXTEKIS:COLUMN = BTN_IMPEX:COLUMN + 14.
      BTN_IMPEXVISMA:COLUMN = BTN_IMPEXTEKIS:COLUMN + 14.
      BTN_IMPEXEON:COLUMN = BTN_IMPEXVISMA:COLUMN + 14.
            
   END.*/
   BTN_IMPEX:LABEL = "Import Infotrader".
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
   ASSIGN
   fastighettemp.BETECKNING:READ-ONLY IN BROWSE BRW_FAST = TRUE.
   RUN DYNBRW.P PERSISTENT SET brwproc[1]
      (INPUT BRW_FAST:HANDLE IN FRAME {&FRAME-NAME}).   
   RUN DYNBRW.P PERSISTENT SET brwproc[2]
      (INPUT BRW_VFAST:HANDLE IN FRAME {&FRAME-NAME}).   

   
   IF Guru.Konstanter:appcon THEN DO:
      RUN VARDERINGUAPP.P PERSISTENT SET varderinginapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN VARDERINGUAPP.P PERSISTENT SET varderinginapph.
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
   status-ok = BRW_FAST:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
   APPLY "VALUE-CHANGED" TO BRW_FAST.
   fastighbet = fastighettemp.BETECKNING.
   {muswait.i}
   vart = "AND".
   RUN ANDFASTIGN.W (INPUT CMB_LAN, INPUT CMB_KOMMUN,INPUT-OUTPUT fastighbet, INPUT TABLE kfastighettemp).
   
   IF musz = TRUE THEN DO:
      musz = FALSE.
      RETURN.
   END.
   RUN openbdyn_UI IN brwproc[1] (INPUT "").
   FIND FIRST fastighettemp WHERE fastighettemp.BETECKNING = fastighbet NO-LOCK NO-ERROR.
   IF AVAILABLE fastighettemp THEN DO:
      EMPTY TEMP-TABLE kfastighettemp  NO-ERROR. 
      CREATE kfastighettemp.
      BUFFER-COPY fastighettemp TO kfastighettemp.    
      RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(fastighettemp)).
      RUN lastselectdyn_UI IN brwproc[1].        
   END.
   {musarrow.i}
   musz = FALSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE bort_UI C-Win 
PROCEDURE bort_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   status-ok = BRW_FAST:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
   fastighbet = fastighettemp.BETECKNING.
   {muswait.i}   
   
   RUN BOFASTIGHET.W (INPUT fastighbet).
   
   IF musz = FALSE THEN DO:
      FIND FIRST fastighettemp WHERE fastighettemp.BETECKNING = fastighbet NO-LOCK NO-ERROR.
      IF AVAILABLE fastighettemp THEN DO:
         DELETE fastighettemp.
      END.     
      RUN selnextprevrow_UI IN brwproc[1].
      RUN openbdyn_UI IN brwproc[1] (INPUT "").
      RUN lastselectdyn_UI IN brwproc[1].      
      RUN title_UI IN brwproc[1].
   END.  
   musz = FALSE.  
   {musarrow.i}   

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnhmt_UI C-Win 
PROCEDURE btnhmt_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   EMPTY TEMP-TABLE fastighettemp NO-ERROR. 
   RUN fasthmt IN fastigregapph 
   (INPUT CMB_LAN,INPUT CMB_KOMMUN,OUTPUT TABLE fastighettemp). 
   
   
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
  DISPLAY CMB_LAN CMB_KOMMUN FILL-IN_BETECKNING FILL-IN-KOMMUN FILL-IN-lan 
          FILL-IN-TEXT 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-52 IMAGE-13 CMB_LAN FBTN_OK CMB_KOMMUN BTN_HAMT BRW_FAST 
         BRW_VFAST BTN_OVER BTN_BACK BTN_NY BTN_UPP BTN_BORT BTN_IMPEX 
         BTN_IMPEXTEKIS BTN_IMPEXMETRIA BTN_IMPEXVISMA BTN_IMPEXEON BTN_IMPGURU 
         BTN_EXPGURUMALL FILL-IN_BETECKNING FILL-IN-KOMMUN FILL-IN-lan BTN_AVB 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fast_UI C-Win 
PROCEDURE fast_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   FIND FIRST fastighettemp USE-INDEX FAST NO-LOCK NO-ERROR.
   musz = TRUE.   
   IF AVAILABLE fastighettemp THEN DO:
      RUN openbdyn_UI IN brwproc[1] (INPUT "").
      FIND FIRST fastighettemp WHERE fastighettemp.BETECKNING = fastighbet NO-LOCK NO-ERROR.
      IF AVAILABLE fastighettemp THEN DO:
         RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(fastighettemp)).
         RUN lastselectdyn_UI IN brwproc[1].  
      END.
      musz = FALSE.      
   END.     
   IF musz = TRUE THEN DO:
      musz = FALSE.
   END. 
   RUN title_UI IN brwproc[1].
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE hmtfast_UI C-Win 
PROCEDURE hmtfast_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   IF valvardnr NE ? THEN DO:
      FIND FIRST varderingtemp WHERE varderingtemp.VARDNR = valvardnr NO-LOCK NO-ERROR.   
      IF AVAILABLE varderingtemp THEN DO:
         FIND FIRST fastvardtemp WHERE fastvardtemp.VARDNR = varderingtemp.VARDNR NO-LOCK NO-ERROR.
         IF AVAILABLE fastvardtemp THEN DO:
            FIND FIRST fastighettemp WHERE fastighettemp.BETECKNING = fastvardtemp.BETECKNING NO-LOCK NO-ERROR.
            IF AVAILABLE fastighettemp THEN DO:
               RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(fastighettemp)).
               RUN lastselectdyn_UI IN brwproc[1].                 
            END.                        
         END.
         IF radvar = 1 THEN RUN valdafast_UI.
      END.
   END.
   ELSE DO:
      status-ok = BRW_FAST:DESELECT-ROWS() IN FRAME {&FRAME-NAME} NO-ERROR.      
   END.  
   FOR EACH fastvardtemp:
      FIND FIRST fastighettemp WHERE fastighettemp.BETECKNING = fastvardtemp.BETECKNING NO-LOCK NO-ERROR.
      IF NOT AVAILABLE fastighettemp THEN DO:
         IF Guru.Konstanter:appcon THEN DO:                           
            RUN PHMTENFAST.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
            (INPUT fastvardtemp.BETECKNING ,OUTPUT TABLE fastighettemp APPEND).
         END.
         ELSE DO:
            RUN PHMTENFAST.P  
            (INPUT fastvardtemp.BETECKNING,OUTPUT TABLE fastighettemp APPEND).
         END.                              
      END.
   END.   
   RUN openbdyn_UI IN brwproc[1] (INPUT "").
   RUN fetchselrowid_UI IN brwproc[1].
   RUN title_UI IN brwproc[1].
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE over_UI C-Win 
PROCEDURE over_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/          
   FIND FIRST fastvardtemp WHERE fastvardtemp.VARDNR = valvardnr AND 
   fastvardtemp.BETECKNING = fastighettemp.BETECKNING NO-LOCK NO-ERROR. 
   IF AVAILABLE fastvardtemp THEN DO:      
      MESSAGE fastvardtemp.BETECKNING + " finns redan upplagt på värderingsnummer: "  + STRING(valvardnr)
      VIEW-AS ALERT-BOX TITLE "Meddelande".
      reprowid = ROWID(fastvardtemp).
   END.      
   ELSE DO:      
      CREATE fastvardtemp. 
      ASSIGN 
      fastvardtemp.BETECKNING = fastighettemp.BETECKNING     
      fastvardtemp.VARDNR = valvardnr.
      reprowid = ROWID(fastvardtemp).
   END.   
   antal_raknare = antal_raknare + 1.         
    

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE uppdat_UI C-Win 
PROCEDURE uppdat_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/          
   RUN lankomhmt IN fastigregapph (INPUT valvardnr,INPUT TABLE lantemp, OUTPUT hmtkommun,OUTPUT hmtlan).  
   CMB_LAN:SCREEN-VALUE IN FRAME {&FRAME-NAME} = hmtlan.
   CMB_LAN = hmtlan.
   
   APPLY "VALUE-CHANGED" TO CMB_LAN.
   IF CMB_LAN NE "Alla" THEN RUN btnhmt_UI.      
   CMB_KOMMUN:SCREEN-VALUE IN FRAME {&FRAME-NAME} = hmtkommun.   
   CMB_KOMMUN = hmtkommun.
   /*{AVBFRAM.I}*/
   RUN openbdyn_UI IN brwproc[1] (INPUT "").  
   FIND FIRST fastighettemp WHERE fastighettemp.BETECKNING = fastighbet NO-LOCK NO-ERROR.
   IF AVAILABLE fastighettemp THEN DO:
      RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(fastighettemp)).
      RUN lastselectdyn_UI IN brwproc[1].     
   END.  
   IF AVAILABLE fastighettemp THEN DO:
      FIND FIRST fastvardtemp WHERE fastvardtemp.BETECKNING = fastighettemp.BETECKNING NO-LOCK NO-ERROR.
      IF AVAILABLE fastvardtemp THEN DO:
         RUN setlastrowid_UI IN brwproc[2] (INPUT ROWID(fastvardtemp)).
         RUN lastselectdyn_UI IN brwproc[2].  
      END.            
      ELSE DO:
         status-ok = BRW_VFAST:DESELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.     
      END.
   END. 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valdafast_UI C-Win 
PROCEDURE valdafast_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/   
   FIND FIRST varderingtemp WHERE varderingtemp.VARDNR = valvardnr NO-LOCK NO-ERROR.   
   IF AVAILABLE varderingtemp THEN DO:
      FIND FIRST fastvardtemp WHERE fastvardtemp.VARDNR = varderingtemp.VARDNR NO-LOCK NO-ERROR.
      IF AVAILABLE fastvardtemp THEN DO:
         kommandoq = "fastvardtemp.VARDNR = " + STRING(varderingtemp.VARDNR). 
         RUN setcolsortvar_UI IN brwproc[2] (INPUT kommandoq).                        
         RUN openbdynspec_UI IN brwproc[2].                      
         IF AVAILABLE fastighettemp  THEN DO: 
            FIND FIRST fastvardtemp  WHERE fastvardtemp.VARDNR = varderingtemp.VARDNR AND fastvardtemp.BETECKNING = fastighettemp.BETECKNING USE-INDEX FAST NO-LOCK NO-ERROR.
         END.
         ELSE DO:
            FIND FIRST fastvardtemp WHERE fastvardtemp.VARDNR = varderingtemp.VARDNR NO-LOCK NO-ERROR.
         END.
         IF AVAILABLE fastvardtemp  THEN DO: 
            /*
            RUN setlastrowid_UI IN brwproc[2] (INPUT ROWID(fastvardtemp)).
            RUN lastselectdyn_UI IN brwproc[2].  
            */
            RUN setorgtitle_UI IN brwproc[2] (INPUT "Valda fastigheter på värderingnr: " + STRING(varderingtemp.VARDNR)).     
            RUN title_UI IN brwproc[2].                   
         END.         
      END.
      ELSE DO:         
         IF radvar = 1 THEN DO:
            kommandoq = "fastvardtemp.VARDNR = " + STRING(varderingtemp.VARDNR). 
            RUN setcolsortvar_UI IN brwproc[2] (INPUT kommandoq).                        
            RUN openbdynspec_UI IN brwproc[2].                 
            RUN setorgtitle_UI IN brwproc[2] (INPUT "Valda fastigheter på värderingnr : " + STRING(varderingtemp.VARDNR)).
            RUN title_UI IN brwproc[2].            
         END.         
         ELSE DO:
            
         END.
      END.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION brwval C-Win 
FUNCTION brwval RETURNS LOGICAL
  ( INPUT valbrw AS INTEGER  ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
   IF valbrw = 1 THEN DO:
      IF  BRW_FAST:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME} = 0 THEN RETURN TRUE.
      ELSE RETURN FALSE.   
   END.
   IF valbrw = 2 THEN DO:
      IF  BRW_VFAST:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME} = 0 THEN RETURN TRUE.
      ELSE RETURN FALSE.   
   END.
   
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

