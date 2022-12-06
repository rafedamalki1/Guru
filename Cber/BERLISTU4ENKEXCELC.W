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

  Created: 95/09/15 -  2:57 pm

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
DEFINE INPUT PARAMETER seluppvar AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER standardskriv AS LOGICAL NO-UNDO.
DEFINE INPUT PARAMETER skrivbord AS LOGICAL NO-UNDO.

DEFINE TEMP-TABLE bryttemp NO-UNDO
   FIELD ROWVAR AS INTEGER
   FIELD EGENK AS LOGICAL
   INDEX ROWVAR ROWVAR.

DEFINE TEMP-TABLE exceltidut NO-UNDO
   FIELD A AS CHARACTER
   FIELD B AS CHARACTER
   FIELD C AS CHARACTER
   FIELD D AS CHARACTER
   FIELD E AS CHARACTER
   FIELD F AS CHARACTER
   FIELD G AS CHARACTER
   FIELD H AS CHARACTER
   FIELD I AS CHARACTER
   FIELD J AS CHARACTER
   FIELD K AS CHARACTER
   FIELD L AS CHARACTER
   FIELD BCOLOR AS INTEGER
   FIELD RADNR AS INTEGER 
   FIELD SIDBRYT AS CHARACTER
   INDEX RADNR IS PRIMARY  RADNR
   INDEX EGENK  E.
   
DEFINE BUFFER exceltidutbuff FOR  exceltidut.  
   
DEFINE TEMP-TABLE ostemp NO-UNDO
   FIELD TEMPFIL AS CHARACTER.
DEFINE VARIABLE tempcolh AS HANDLE NO-UNDO.
DEFINE VARIABLE dynbrwh  AS HANDLE NO-UNDO.
DEFINE VARIABLE dynqueh AS HANDLE NO-UNDO.
DEFINE VARIABLE dynbuffh AS HANDLE NO-UNDO.
DEFINE VARIABLE sistarad AS INTEGER NO-UNDO.      


/* Local Variable Definitions ---                                       */ 
{ALLDEF.I}
&Scoped-define NEW 
{GLOBVAR2DEL1.I}
{REGVAR.I}
  
{ANMARKD.I}
{LISTDEF.I}
{EXECLIN.I}
                              
DEFINE SHARED VARIABLE visvalvar AS INTEGER NO-UNDO.   /* 1= progres vis 2 = excel 3 = IE 4 = pdf*/
DEFINE SHARED VARIABLE stolpval AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE edmedd    AS CHARACTER NO-UNDO.       
DEFINE SHARED VARIABLE valkonst  AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE prisvar   AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE bryt      AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE skrivut   AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE musz      AS LOGICAL NO-UNDO.         
DEFINE SHARED VARIABLE valaonr   AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE valdelnr  AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE valort    AS CHARACTER NO-UNDO. 
DEFINE SHARED VARIABLE valomrade AS CHARACTER NO-UNDO.  
/* DEFINE SHARED VARIABLE lin_rowid AS ROWID NO-UNDO. */

DEFINE SHARED VARIABLE katvar AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE kalkvar AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE vartpro AS CHARACTER FORMAT "X(3)" NO-UNDO.

DEFINE VARIABLE arrhjsum AS DECIMAL NO-UNDO.    
DEFINE VARIABLE bytgrupp AS CHARACTER NO-UNDO. 
DEFINE VARIABLE deltidut AS LOGICAL NO-UNDO.
DEFINE VARIABLE deltidutex AS LOGICAL NO-UNDO.
DEFINE VARIABLE edvar AS LOGICAL NO-UNDO.
DEFINE VARIABLE finns AS LOGICAL NO-UNDO.    
DEFINE VARIABLE forsta AS LOGICAL NO-UNDO.
DEFINE VARIABLE fyra AS LOGICAL NO-UNDO.
DEFINE VARIABLE kant AS INTEGER NO-UNDO.
DEFINE VARIABLE lage AS INTEGER NO-UNDO.
DEFINE VARIABLE mat2 AS LOGICAL NO-UNDO.
DEFINE VARIABLE mat3 AS LOGICAL NO-UNDO.
DEFINE VARIABLE mat4 AS LOGICAL NO-UNDO.
DEFINE VARIABLE mtextvar AS LOGICAL NO-UNDO.
DEFINE VARIABLE numsok AS LOGICAL NO-UNDO.
DEFINE VARIABLE numvar AS INTEGER NO-UNDO.
DEFINE VARIABLE sex AS LOGICAL NO-UNDO.
DEFINE VARIABLE sidlangd AS INTEGER NO-UNDO.
DEFINE VARIABLE sidlangdex AS INTEGER NO-UNDO.
DEFINE VARIABLE skapad AS LOGICAL NO-UNDO.
DEFINE VARIABLE stopvar AS LOGICAL NO-UNDO. 
DEFINE VARIABLE str AS CHARACTER FORMAT "X(130)" NO-UNDO.
DEFINE VARIABLE str0 AS CHARACTER FORMAT "X(130)" NO-UNDO.
DEFINE VARIABLE str1 AS CHARACTER FORMAT "X(130)" NO-UNDO. 
DEFINE VARIABLE str2 AS CHARACTER FORMAT "X(130)" NO-UNDO. 
DEFINE VARIABLE str3 AS CHARACTER FORMAT "X(130)" NO-UNDO.
DEFINE VARIABLE sumantal AS INTEGER NO-UNDO.
DEFINE VARIABLE sumpris AS DECIMAL NO-UNDO.    
DEFINE VARIABLE sumrec AS ROWID NO-UNDO.
DEFINE VARIABLE tant AS INTEGER NO-UNDO.
DEFINE VARIABLE totant AS INTEGER NO-UNDO.
DEFINE VARIABLE temptant AS INTEGER NO-UNDO.
DEFINE VARIABLE tiduttyp AS INTEGER NO-UNDO.
DEFINE VARIABLE totalsum AS DECIMAL NO-UNDO.
DEFINE VARIABLE totalt AS DECIMAL NO-UNDO. 
DEFINE VARIABLE upput AS LOGICAL NO-UNDO.
DEFINE VARIABLE uppvar AS LOGICAL NO-UNDO.


DEFINE VARIABLE bildvar AS LOGICAL NO-UNDO.

DEFINE VARIABLE utrow AS ROWID EXTENT 8 NO-UNDO.
DEFINE VARIABLE utrow1 AS ROWID NO-UNDO.
DEFINE VARIABLE utrow2 AS ROWID NO-UNDO.
DEFINE VARIABLE utrow3 AS ROWID NO-UNDO.
DEFINE VARIABLE utrow4 AS ROWID NO-UNDO.
DEFINE VARIABLE utrow5 AS ROWID NO-UNDO.
DEFINE VARIABLE utrow6 AS ROWID NO-UNDO.
DEFINE VARIABLE utrow7 AS ROWID NO-UNDO.
DEFINE VARIABLE utrow8 AS ROWID NO-UNDO.
DEFINE VARIABLE forstakonst AS LOGICAL NO-UNDO.
DEFINE VARIABLE xkordvar AS INTEGER NO-UNDO. 
DEFINE VARIABLE rowspar AS INTEGER NO-UNDO.
DEFINE VARIABLE vfontname    AS CHARACTER NO-UNDO. 
DEFINE VARIABLE vfontsize    AS INTEGER NO-UNDO.   
DEFINE VARIABLE vfonttype    AS LOGICAL NO-UNDO.   
DEFINE VARIABLE vfontheight  AS INTEGER NO-UNDO.   
DEFINE VARIABLE vfontcolor   AS INTEGER NO-UNDO.   
DEFINE VARIABLE sidraknare AS INTEGER NO-UNDO.
DEFINE VARIABLE sparlangd AS INTEGER NO-UNDO.
DEFINE VARIABLE kvaravsidan    AS INTEGER NO-UNDO.
DEFINE VARIABLE exkant        AS INTEGER NO-UNDO.
DEFINE VARIABLE extant        AS INTEGER NO-UNDO.
DEFINE VARIABLE forstaanmark AS LOGICAL NO-UNDO.
DEFINE VARIABLE berlistapph AS HANDLE NO-UNDO.
DEFINE VARIABLE musz2 AS LOGICAL NO-UNDO.
DEFINE VARIABLE printername AS CHARACTER NO-UNDO.
DEFINE VARIABLE berlistut AS LOGICAL NO-UNDO.
DEFINE VARIABLE ppvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE vartprospar AS CHARACTER NO-UNDO.


   
{TEMPUPP.I}
{TIDUTTTNEW.I}
  
DEFINE TEMP-TABLE tidut2
   FIELD UT AS CHARACTER FORMAT "X(132)".
   
DEFINE TEMP-TABLE tidut3
   FIELD UT AS CHARACTER FORMAT "X(132)".
   
DEFINE TEMP-TABLE tidut4
   FIELD UT AS CHARACTER FORMAT "X(132)". 
   
DEFINE TEMP-TABLE mtrl_temp2 
   FIELD NUM AS INTEGER    
   FIELD ENR AS CHARACTER
   FIELD BENAMNING AS CHARACTER
   FIELD ENHET AS CHARACTER
   FIELD ANTAL AS INTEGER
   FIELD PRIS AS DECIMAL 
   FIELD TOTPRIS AS DECIMAL
   FIELD LEVKOD AS CHARACTER   
   FIELD UPPLAG AS INTEGER 
   FIELD GRUPP AS INTEGER 
   FIELD XKORD AS INTEGER
   FIELD FORNR AS CHARACTER
   FIELD LINNR AS CHARACTER
   FIELD NATNR AS CHARACTER
   FIELD FRI1 AS INTEGER
   FIELD FRI2 AS INTEGER
   FIELD PAR AS INTEGER
   FIELD ORD AS INTEGER
   INDEX ENR IS PRIMARY ENR ASCENDING   
   INDEX LISTA UPPLAG GRUPP FORNR LINNR NATNR FRI1 FRI2 PAR
   INDEX LISTA2 UPPLAG FORNR LINNR NATNR FRI1 FRI2 PAR
   INDEX ENRX ENR UPPLAG XKORD
   INDEX ENRN ENR UPPLAG NUM
   INDEX ORD UPPLAG ORD PAR.   
   
{UPPLAGTEMP.I}
   
DEFINE TEMP-TABLE ord_temp
   FIELD NUM AS INTEGER 
   FIELD ORD AS INTEGER
   INDEX NUM IS PRIMARY NUM ASCENDING.   
   
DEFINE TEMP-TABLE grupp_temp
   FIELD KONSKOD AS INTEGER
   FIELD BENAMNING AS CHARACTER
   FIELD ORDNING AS INTEGER
   INDEX ORD IS PRIMARY ORDNING ASCENDING.       

DEFINE TEMP-TABLE grupp_temp2
   FIELD KONSKOD AS INTEGER
   FIELD BENAMNING AS CHARACTER
   FIELD ORDNING AS INTEGER
   INDEX ORD IS PRIMARY ORDNING ASCENDING.     
   
DEFINE TEMP-TABLE id_temp  
   FIELD NUM AS INTEGER 
   FIELD GRUPP AS INTEGER  
   FIELD FORNR AS CHARACTER
   FIELD LINNR AS CHARACTER
   FIELD NATNR AS CHARACTER
   FIELD FRI1 AS INTEGER
   FIELD FRI2 AS INTEGER 
   FIELD XKORD AS INTEGER
   FIELD ENDKOMB AS LOGICAL
   FIELD FRI3 AS CHARACTER
   FIELD ORD AS INTEGER
   INDEX NUM IS PRIMARY NUM ASCENDING
   INDEX ORD FORNR LINNR NATNR FRI1 FRI2 ASCENDING
   INDEX ORD2 ORD ASCENDING. 
   
  
DEFINE TEMP-TABLE ber_temp
   FIELD ARBKOD AS CHARACTER
   FIELD LOPNR AS INTEGER
   FIELD BENAMNING AS CHARACTER
   FIELD ENHET AS CHARACTER    
   FIELD ANTAL AS DECIMAL
   FIELD NUM AS INTEGER
   INDEX KOD ARBKOD LOPNR ASCENDING
   INDEX NUM NUM ARBKOD LOPNR ASCENDING.
   
DEFINE TEMP-TABLE ber_temp2
   FIELD ARBKOD AS CHARACTER
   FIELD LOPNR AS INTEGER
   FIELD BENAMNING AS CHARACTER
   FIELD ENHET AS CHARACTER    
   FIELD ANTAL AS DECIMAL
   FIELD NUM AS INTEGER
   INDEX KOD ARBKOD LOPNR ASCENDING
   INDEX NUM NUM ARBKOD LOPNR ASCENDING. 
   
DEFINE VARIABLE totea AS DECIMAL NO-UNDO.
DEFINE VARIABLE totarb AS DECIMAL NO-UNDO.
DEFINE VARIABLE totmask AS DECIMAL NO-UNDO.
DEFINE VARIABLE totmtrl AS DECIMAL NO-UNDO.
DEFINE VARIABLE totovr AS DECIMAL NO-UNDO.
DEFINE VARIABLE arbtim AS DECIMAL NO-UNDO.
DEFINE VARIABLE msktim AS DECIMAL NO-UNDO.
DEFINE VARIABLE bertim AS DECIMAL NO-UNDO.
DEFINE VARIABLE eavar AS DECIMAL NO-UNDO.
DEFINE VARIABLE arbvar AS DECIMAL NO-UNDO.
DEFINE VARIABLE mskvar AS DECIMAL NO-UNDO.
DEFINE VARIABLE faktvar AS DECIMAL NO-UNDO.
DEFINE VARIABLE kontroll AS LOGICAL NO-UNDO.
DEFINE VARIABLE prisvar2 AS DECIMAL NO-UNDO.
DEFINE VARIABLE sumea AS DECIMAL NO-UNDO.
DEFINE VARIABLE summont AS DECIMAL NO-UNDO.
DEFINE VARIABLE summask AS DECIMAL NO-UNDO.


DEFINE TEMP-TABLE kalk_temp NO-UNDO
   {KALKTEMPTAB.I}
   
DEFINE TEMP-TABLE kalk_temp2 NO-UNDO LIKE kalk_temp.


DEFINE NEW SHARED TEMP-TABLE extemp
   {EXTEMPDEF.I}
DEFINE NEW SHARED TEMP-TABLE innehall NO-UNDO   
   {BYGGINNEHALL.I}
DEFINE {&NEW} {&SHARED} TEMP-TABLE excoltemp
   FIELD COLNAME AS CHARACTER
   FIELD COLNUM AS INTEGER
   FIELD WDTH AS INTEGER.
&Scoped-define SHARED SHARED
{KONVALTEMP.I}      

 
DEFINE SHARED VARIABLE TOG_ANM AS LOGICAL INITIAL no 
     LABEL "Anmärkning" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.5 BY .86 NO-UNDO.

DEFINE SHARED VARIABLE TOG_ID AS LOGICAL INITIAL no 
     LABEL "Identitet" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.88 BY .86 NO-UNDO.

DEFINE SHARED VARIABLE TOG_SPANN AS LOGICAL INITIAL no 
     LABEL "Spann/kabellängd" 
     VIEW-AS TOGGLE-BOX
     SIZE 19 BY .86 NO-UNDO.

DEFINE SHARED VARIABLE TOG_MTRL AS LOGICAL INITIAL no 
     LABEL "Materiel" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.13 BY .86 NO-UNDO.

DEFINE SHARED VARIABLE TOG_P3 AS LOGICAL INITIAL no 
     LABEL "P3-koder" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.5 BY .86 NO-UNDO.

DEFINE SHARED VARIABLE TOG_SIDEXCEL AS LOGICAL INITIAL no 
     LABEL "Texter i sidokolumn i Excel" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.5 BY .79 NO-UNDO.     
     
DEFINE SHARED VARIABLE TOG_TEXT AS LOGICAL INITIAL no 
     LABEL "Mtrl text" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.5 BY .86 NO-UNDO.     

DEFINE SHARED VARIABLE TOG_UPP AS LOGICAL INITIAL no 
     LABEL "Upplag" 
     VIEW-AS TOGGLE-BOX
     SIZE 10.25 BY .86 NO-UNDO.
     
DEFINE SHARED VARIABLE TOG_VAL AS LOGICAL INITIAL no 
     LABEL "Konstruktionsval" 
     VIEW-AS TOGGLE-BOX
     SIZE 19 BY .86 NO-UNDO.     
DEFINE  SHARED VARIABLE TOG_KONSTRUKTION AS LOGICAL INITIAL no 
     LABEL "Konstruktionsgrupp" 
     VIEW-AS TOGGLE-BOX
     SIZE 21.5 BY .79 NO-UNDO.
DEFINE SHARED VARIABLE TOG_EGEN AS LOGICAL INITIAL no 
     LABEL "Egenkontroll" 
     VIEW-AS TOGGLE-BOX
     SIZE 15 BY .88
     BGCOLOR 8  NO-UNDO.
 DEFINE  SHARED VARIABLE TOG_JMFKONS AS LOGICAL INITIAL no 
     LABEL "Jämför mtrl med konstruktion" 
     VIEW-AS TOGGLE-BOX
     SIZE 31.88 BY .79 NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-VINST
&Scoped-define BROWSE-NAME BRW_UT

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES exceltidut

/* Definitions for BROWSE BRW_UT                                        */
&Scoped-define FIELDS-IN-QUERY-BRW_UT exceltidut.A exceltidut.B ~
exceltidut.C exceltidut.D exceltidut.E exceltidut.F exceltidut.G ~
exceltidut.H exceltidut.I exceltidut.J exceltidut.K exceltidut.L ~
exceltidut.RADNR exceltidut.SIDBRYT 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_UT 
&Scoped-define QUERY-STRING-BRW_UT FOR EACH exceltidut NO-LOCK
&Scoped-define OPEN-QUERY-BRW_UT OPEN QUERY BRW_UT FOR EACH exceltidut NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_UT exceltidut
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_UT exceltidut


/* Definitions for FRAME FRAME-VINST                                    */
&Scoped-define OPEN-BROWSERS-IN-QUERY-FRAME-VINST ~
    ~{&OPEN-QUERY-BRW_UT}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS TOG_KOMP BTN_SKRIV BTN_EXCEL BTN_HTML ~
BTN_PDF BTN_AVS 
&Scoped-Define DISPLAYED-OBJECTS TOG_KOMP 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR WINDOW-2 AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVS AUTO-END-KEY 
     LABEL "Avsluta":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_EXCEL 
     LABEL "Visa i Excel":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_HTML 
     LABEL "Visa i IE":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_PDF 
     LABEL "Visa i PDF":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_SKRIV 
     LABEL "Skriv ut":L 
     SIZE 14 BY 1.

DEFINE VARIABLE TOG_KOMP AS LOGICAL INITIAL no 
     LABEL "Komprimera antalet sidor till andra program" 
     VIEW-AS TOGGLE-BOX
     SIZE 55.63 BY .79 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_UT FOR 
      exceltidut SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_UT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_UT WINDOW-2 _STRUCTURED
  QUERY BRW_UT NO-LOCK DISPLAY
      exceltidut.A FORMAT "x(8)":U
      exceltidut.B FORMAT "x(8)":U
      exceltidut.C FORMAT "x(8)":U
      exceltidut.D FORMAT "x(8)":U
      exceltidut.E FORMAT "x(8)":U
      exceltidut.F FORMAT "x(8)":U
      exceltidut.G FORMAT "x(8)":U
      exceltidut.H FORMAT "x(8)":U
      exceltidut.I FORMAT "x(8)":U
      exceltidut.J FORMAT "x(8)":U
      exceltidut.K FORMAT "x(8)":U
      exceltidut.L FORMAT "x(8)":U
      exceltidut.RADNR FORMAT "->,>>>,>>9":U
      exceltidut.SIDBRYT FORMAT "x(8)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-LABELS NO-COLUMN-SCROLLING SIZE 92.5 BY 25.75.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-VINST
     BRW_UT AT ROW 1.5 COL 1.5
     TOG_KOMP AT ROW 27.38 COL 1.75
     BTN_SKRIV AT ROW 28.25 COL 19.13
     BTN_EXCEL AT ROW 28.25 COL 34.13
     BTN_HTML AT ROW 28.25 COL 49.13
     BTN_PDF AT ROW 28.25 COL 64.13
     BTN_AVS AT ROW 28.25 COL 80
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 94 BY 28.29.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: 
   Temp-Tables and Buffers:
      TABLE: exceltidut T "?" NO-UNDO temp-db exceltidut
      TABLE: ? T "?" NO-UNDO temp-db tidut
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW WINDOW-2 ASSIGN
         HIDDEN             = YES
         TITLE              = "Lista A) materiel/upplag, grupp, konstruktion, artikel"
         HEIGHT             = 28.42
         WIDTH              = 94
         MAX-HEIGHT         = 28.42
         MAX-WIDTH          = 94
         VIRTUAL-HEIGHT     = 28.42
         VIRTUAL-WIDTH      = 94
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
/* SETTINGS FOR WINDOW WINDOW-2
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME FRAME-VINST
   FRAME-NAME                                                           */
/* BROWSE-TAB BRW_UT 1 FRAME-VINST */
/* SETTINGS FOR BROWSE BRW_UT IN FRAME FRAME-VINST
   NO-ENABLE                                                            */
ASSIGN 
       BRW_UT:HIDDEN  IN FRAME FRAME-VINST                = TRUE
       BRW_UT:MAX-DATA-GUESS IN FRAME FRAME-VINST         = 1000.

ASSIGN 
       exceltidut.RADNR:VISIBLE IN BROWSE BRW_UT = FALSE
       exceltidut.SIDBRYT:VISIBLE IN BROWSE BRW_UT = FALSE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-2)
THEN WINDOW-2:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_UT
/* Query rebuild information for BROWSE BRW_UT
     _TblList          = "Temp-Tables.exceltidut"
     _Options          = "NO-LOCK"
     _FldNameList[1]   = Temp-Tables.exceltidut.A
     _FldNameList[2]   = Temp-Tables.exceltidut.B
     _FldNameList[3]   = Temp-Tables.exceltidut.C
     _FldNameList[4]   = Temp-Tables.exceltidut.D
     _FldNameList[5]   = Temp-Tables.exceltidut.E
     _FldNameList[6]   = Temp-Tables.exceltidut.F
     _FldNameList[7]   = Temp-Tables.exceltidut.G
     _FldNameList[8]   = Temp-Tables.exceltidut.H
     _FldNameList[9]   = Temp-Tables.exceltidut.I
     _FldNameList[10]   = Temp-Tables.exceltidut.J
     _FldNameList[11]   = Temp-Tables.exceltidut.K
     _FldNameList[12]   = Temp-Tables.exceltidut.L
     _FldNameList[13]   > Temp-Tables.exceltidut.RADNR
"exceltidut.RADNR" ? ? "integer" ? ? ? ? ? ? no ? no no ? no no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[14]   > Temp-Tables.exceltidut.SIDBRYT
"exceltidut.SIDBRYT" ? ? "character" ? ? ? ? ? ? no ? no no ? no no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE BRW_UT */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-VINST
/* Query rebuild information for FRAME FRAME-VINST
     _Options          = "NO-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME FRAME-VINST */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME BTN_AVS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVS WINDOW-2
ON CHOOSE OF BTN_AVS IN FRAME FRAME-VINST /* Avsluta */
DO:
   
   {BORTBRWPROC.I}
   RUN ostempdel_UI. 
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_EXCEL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_EXCEL WINDOW-2
ON CHOOSE OF BTN_EXCEL IN FRAME FRAME-VINST /* Visa i Excel */
DO: 
   /*kompsida*/
   {muswait.i}
   RUN ColBreddStart_UI.
   
   {musarrow.i}    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_HTML
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_HTML WINDOW-2
ON CHOOSE OF BTN_HTML IN FRAME FRAME-VINST /* Visa i IE */
DO: 
   /*kompsida*/
   {muswait.i}
   
   RUN UTBER4HTM.P (INPUT TABLE tidut,INPUT TOG_KOMP).
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_PDF
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_PDF WINDOW-2
ON CHOOSE OF BTN_PDF IN FRAME FRAME-VINST /* Visa i PDF */
DO: 
   /*kompsida*/
   {muswait.i}
  
  
   RUN UTBER4PDF15.P (INPUT TABLE tidut,INPUT TOG_KOMP).
   MESSAGE "Vill du skapa Innehållsförteckning?"
   VIEW-AS ALERT-BOX
   QUESTION BUTTONS YES-NO TITLE "Innehållsförteckning" UPDATE svar AS LOGICAL. 
   IF svar = TRUE THEN DO:
      RUN ByggInneStartPDF.P (INPUT TABLE tidut,INPUT TOG_KOMP).
      PDF:
      FOR EACH tidut WHERE NO-LOCK:
         IF SUBSTRING(tidut.UT,1,5) = "*****" THEN LEAVE PDF.
         IF SUBSTRING(tidut.UT,1,5) = "-----" THEN LEAVE PDF.
         IF SUBSTRING(tidut.UT,1,8) = "E-NUMMER" THEN LEAVE PDF.
         CREATE innehall.
         innehall.UT = tidut.UT.
         innehall.SIDA = 9999999.
      END.
      RUN ByggInnePdf.P (INPUT TABLE innehall).
      FOR EACH innehall WHERE innehall.SIDA = 9999999  NO-LOCK:
         DELETE innehall.
      END.
   END.  
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_SKRIV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_SKRIV WINDOW-2
ON CHOOSE OF BTN_SKRIV IN FRAME FRAME-VINST /* Skriv ut */
DO: 
   CREATE felmeddtemp.
   ASSIGN felmeddtemp.VAL = 3.
   berlistut = TRUE.
   RUN ut_UI.      
   {musarrow.i}    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_SKRIV WINDOW-2
ON MOUSE-MENU-CLICK OF BTN_SKRIV IN FRAME FRAME-VINST /* Skriv ut */
DO:
   RUN SIDLANGD.W.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_KOMP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_KOMP WINDOW-2
ON VALUE-CHANGED OF TOG_KOMP IN FRAME FRAME-VINST /* Komprimera antalet sidor till andra program */
DO:
   TOG_KOMP = INPUT TOG_KOMP.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_UT
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
   SESSION:PRINTER-CONTROL-HANDLE = 0.
   IF VALID-HANDLE(berlistapph) THEN DELETE PROCEDURE berlistapph.
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
  
   WINDOW-2:TITLE = seluppvar.
   
   {muswait.i}
   {ALLSTARTDYN.I}
   
   EMPTY TEMP-TABLE felmeddtemp NO-ERROR. 
   EMPTY TEMP-TABLE tidut NO-ERROR. 
   EMPTY TEMP-TABLE mtrl_temp NO-ERROR. 
   EMPTY TEMP-TABLE mtrl_temp2 NO-ERROR. 
   EMPTY TEMP-TABLE id_temp NO-ERROR. 
   EMPTY TEMP-TABLE grupp_temp NO-ERROR. 
   EMPTY TEMP-TABLE grupp_temp2 NO-ERROR. 
   EMPTY TEMP-TABLE uppl_temp NO-ERROR. 
   EMPTY TEMP-TABLE ord_temp NO-ERROR.
   /*Om användaren har valt btn_skriv i berlista, då har skrivval redan körts*/
   IF skrivut = TRUE THEN DO:
      CREATE felmeddtemp.
      ASSIGN felmeddtemp.VAL = 1.     
   END.
   ASSIGN printername = "".
   FIND FIRST felmeddtemp NO-ERROR.
   IF NOT AVAILABLE felmeddtemp THEN DO:   
      /*Användaren har valt bnt_visa i berlista.w*/
      /*Använd vald skrivare - kör inte skrivval.w*/
      IF standardskriv = TRUE THEN DO:
         RUN DEFPRINTER.P (OUTPUT ppvar,OUTPUT musz).
         IF musz = FALSE THEN DO:
            printername = ppvar.
            IF printername NE "" THEN DO:
               SESSION:PRINTER-NAME = printername.
               RUN skrivhmt_UI IN berlistapph (INPUT printername,
                                               OUTPUT globsidl,OUTPUT globsids).
            END.
            ELSE DO:
               MESSAGE "Ingen standardskrivare hittades, välj en skrivare." VIEW-AS ALERT-BOX TITLE "Meddelande".
               vartprospar = vartpro.
               vartpro = "berlist44".
               RUN SKRIVVAL.W (INPUT FALSE).                
               vartpro = vartprospar.
            END.
         END.
         ELSE DO:
            MESSAGE "Något är fel, ingen standardskrivare hittades! Kontakta Elpool." VIEW-AS ALERT-BOX TITLE "Meddelande".
            RETURN NO-APPLY.
         END.
      END.
      ELSE DO:
         /*Välj skrivare - kör skrivval.w*/
         CREATE felmeddtemp.
         ASSIGN felmeddtemp.VAL = 4.
         vartprospar = vartpro.
         vartpro = "berlist44".
         RUN SKRIVVAL.W (INPUT FALSE).                   
         vartpro = vartprospar.
      END.           
   END.
   IF globsids = 0  THEN globsids = 73.
   IF musz = TRUE THEN DO:
      RETURN NO-APPLY.
   END.
   musz = FALSE.   
/*    sidlangdex = globsids - 17. */
   /*FOREBILDER*/
  
   {LOGGOR.I}               
   RUN bildfinns_UI IN berlistapph (INPUT link).
   EMPTY TEMP-TABLE innehall NO-ERROR.
   
   FIND FIRST kon_val WHERE NO-LOCK NO-ERROR. 
   IF AVAILABLE kon_val THEN DO:
      IF kon_val.BERAONR = "" THEN.
      ELSE DO:
         FOR EACH kon_val WHERE NO-LOCK BY kon_val.BERAONR BY kon_val.num:
            CREATE innehall.
            ASSIGN 
            innehall.BERNR = INTEGER(kon_val.BERAONR).
            innehall.OMRADE = kon_val.omrade.
            innehall.NUM = kon_val.NUM. 
            innehall.ORD = kon_val.ORD.
            
         END.
         RUN Ininnehall_UI IN berlistapph (INPUT TABLE innehall).
      END.   
   END.   
  
   RUN togkonstin_UI  IN berlistapph (INPUT TOG_KONSTRUKTION).
   RUN TOG_JMFKONS_UI IN berlistapph (INPUT TOG_JMFKONS).  
   RUN skapalista2_UI IN berlistapph (INPUT stolpval,INPUT TOG_ANM,INPUT TOG_ID,INPUT TOG_SPANN,
                                     INPUT TOG_MTRL,INPUT TOG_P3,INPUT TOG_TEXT,
                                     INPUT TOG_UPP,INPUT TOG_VAL,INPUT TOG_EGEN,
                                     INPUT Guru.Konstanter:globforetag,INPUT Guru.Konstanter:globanvpkod,INPUT seluppvar,INPUT edmedd,INPUT valkonst,
                                     INPUT prisvar,INPUT bryt,INPUT skrivut,INPUT valaonr,
                                     INPUT valdelnr,INPUT valort,INPUT valomrade,
                                     INPUT katvar,INPUT kalkvar,INPUT globsids, INPUT skrivbord,
                                     OUTPUT musz2,INPUT TABLE kon_val,
                                     INPUT TABLE felmeddtemp,OUTPUT TABLE tidut,
                                     OUTPUT TABLE extemp).
   FOR EACH extemp:
      FIND FIRST exceltidut WHERE exceltidut.RADNR = extemp.ROWVAR NO-LOCK NO-ERROR.
      IF NOT AVAILABLE exceltidut THEN DO:
         CREATE exceltidut.
         ASSIGN  
         exceltidut.RADNR = extemp.ROWVAR
         exceltidut.BCOLOR =  extemp.BCOLOR.
      END.
      IF extemp.COLVAR = "A" THEN DO:
         IF extemp.DATA BEGINS "0" THEN extemp.DATA = "E" + extemp.DATA.
         exceltidut.A = exceltidut.A + extemp.DATA.
      END.  
      ELSE IF extemp.COLVAR = "B" THEN exceltidut.B = exceltidut.B + extemp.DATA.
      ELSE IF extemp.COLVAR = "C" THEN exceltidut.C = exceltidut.C + extemp.DATA.
      ELSE IF extemp.COLVAR = "D" THEN exceltidut.D = exceltidut.D + extemp.DATA.
      ELSE IF extemp.COLVAR = "E" THEN exceltidut.E = exceltidut.E + extemp.DATA.
      ELSE IF extemp.COLVAR = "F" THEN exceltidut.F = exceltidut.F + extemp.DATA.
      ELSE IF extemp.COLVAR = "G" THEN exceltidut.G = exceltidut.G + extemp.DATA.
      ELSE IF extemp.COLVAR = "H" THEN exceltidut.H = exceltidut.H + extemp.DATA.
      ELSE IF extemp.COLVAR = "I" THEN exceltidut.I = exceltidut.I + extemp.DATA.
      ELSE IF extemp.COLVAR = "J" THEN exceltidut.J = exceltidut.J + extemp.DATA.
      ELSE IF extemp.COLVAR = "K" THEN exceltidut.K = exceltidut.K + extemp.DATA.
      ELSE IF extemp.COLVAR = "L" THEN exceltidut.L = exceltidut.L + extemp.DATA.
   END.
  /* exceltidut.RADNR:VISIBLE IN BROWSE BRW_UT = TRUE.*/
   exceltidut.SIDBRYT:VISIBLE IN BROWSE BRW_UT = TRUE.
   RUN Kompinnehall_UI IN berlistapph. 
   RUN Utinnehall_UI IN berlistapph (OUTPUT TABLE innehall).
   FOR EACH exceltidut WHERE exceltidut.A MATCHES "*£*": 
      exceltidut.A = "".  
      exceltidut.SIDBRYT = "£".
   END.
   IF Guru.GlobalaVariabler:EgenskaperTillByggkontroll[2] = TRUE THEN DO:
      FOR EACH exceltidut WHERE exceltidut.E BEGINS "EGENKONTROLL":
         FIND LAST exceltidutbuff WHERE exceltidutbuff.A  = "£" AND exceltidutbuff.RADNR <= exceltidut.RADNR NO-LOCK NO-ERROR.
         IF AVAILABLE exceltidutbuff THEN DO:
            IF exceltidutbuff.RADNR < exceltidut.RADNR - 5 THEN.
            ELSE DO:
               exceltidut.SIDBRYT  = "£".
            END.   
         END.   
      END.    
   END.
   
   DEFINE VARIABLE sidstart AS INTEGER NO-UNDO.
   DEFINE VARIABLE sidslut AS INTEGER NO-UNDO.
   sidraknare = 1.
   sidstart = 1.
   FOR EACH exceltidut WHERE exceltidut.SIDBRYT  = "£" NO-LOCK:
      FOR EACH innehall WHERE innehall.SIDA = 0 AND innehall.ROWVAR < exceltidut.RADNR:
         innehall.SIDA = sidraknare.
      END.
      sidstart = exceltidut.RADNR.
      sidraknare = sidraknare + 1.
   END.
   FOR EACH innehall WHERE innehall.SIDA = 0 AND innehall.ROWVAR < exceltidutbuff.RADNR:
      innehall.SIDA = sidraknare.
   END.
   
   musz = musz2.
   IF musz = TRUE THEN DO:
      musz = FALSE.
      status-mus2 = SESSION:SET-WAIT-STATE("").
      APPLY "WINDOW-CLOSE" TO {&WINDOW-NAME}. 
      LEAVE MAIN-BLOCK. 
   END.                 
   ELSE DO:
      IF skrivut = FALSE THEN DO:   
         ENABLE BRW_UT WITH FRAME FRAME-VINST.
         BRW_UT:HIDDEN = FALSE.       
      END.
      ELSE DO:  
         RUN ut_UI.
         status-mus2 = SESSION:SET-WAIT-STATE("").
         APPLY "WINDOW-CLOSE" TO {&WINDOW-NAME}. 
         LEAVE MAIN-BLOCK. 
      END.
   END.
   RUN enable_UI.   
   {FRMSIZE.I}          
   ASSIGN
   Guru.GlobalaVariabler:collefth = ?.
   Guru.GlobalaVariabler:colrighth = BTN_AVS:HANDLE.      
   RUN buttcolh_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   Guru.GlobalaVariabler:colrighth = BTN_PDF:HANDLE.      
   RUN buttcolh_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   /*
   Guru.GlobalaVariabler:colrighth = BTN_HTML:HANDLE.      
   RUN buttcolh_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   */
   BTN_HTML:HIDDEN = TRUE.
   Guru.GlobalaVariabler:colrighth = BTN_EXCEL:HANDLE.      
   RUN buttcolh_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   Guru.GlobalaVariabler:colrighth = BTN_SKRIV:HANDLE.      
   RUN buttcolh_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   visvalvar = 2.
   IF visvalvar > 1 THEN DO:
      IF visvalvar = 2 THEN APPLY "CHOOSE" TO  BTN_EXCEL.
      IF visvalvar = 3 THEN APPLY "CHOOSE" TO  BTN_HTML.
      IF visvalvar = 4 THEN APPLY "CHOOSE" TO  BTN_PDF.
      APPLY "CHOOSE" TO BTN_AVS IN FRAME {&FRAME-NAME}. 
      RUN SetDefaultCursors IN Guru.Konstanter:hpApi.
      Guru.GlobalaVariabler:retvalkoll = FALSE.
      LEAVE MAIN-BLOCK.
   END.
   BTN_HTML:HIDDEN = TRUE.
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
      (INPUT BRW_UT:HANDLE IN FRAME {&FRAME-NAME}).
    
      IF Guru.Konstanter:appcon THEN DO:
         RUN BERLISTAPPU4.P PERSISTENT SET berlistapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT (INPUT Guru.Konstanter:varforetypchar[48], INPUT Guru.Konstanter:globanv, INPUT TOG_SIDEXCEL). 
      END.
      ELSE DO:
         RUN BERLISTAPPU4.P PERSISTENT SET berlistapph (INPUT Guru.Konstanter:varforetypchar[48], INPUT Guru.Konstanter:globanv, INPUT TOG_SIDEXCEL).
      END.
      RUN EgenskaperTillByggkontroll IN berlistapph (INPUT Guru.GlobalaVariabler:EgenskaperTillByggkontroll).    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ColBreddStart_UI WINDOW-2 
PROCEDURE ColBreddStart_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE VARIABLE extrabredd AS INTEGER NO-UNDO.
   DEFINE VARIABLE franfil AS CHARACTER NO-UNDO.
   DEFINE VARIABLE sparatxtxlsx AS CHARACTER NO-UNDO.
   DEFINE VARIABLE labelvar AS CHARACTER NO-UNDO.
   DEFINE VARIABLE tempcounter AS INTEGER NO-UNDO.
   DEFINE VARIABLE traffcoll AS CHARACTER NO-UNDO.
   DEFINE VARIABLE aa AS INTEGER NO-UNDO.
   aa = TIME.
   
   /*dynbuffh = dynqueh:GET-BUFFER-HANDLE.*/
   dynbrwh = BRW_UT:HANDLE IN FRAME {&FRAME-NAME}.
   dynqueh = dynbrwh:QUERY.
   dynbuffh = dynqueh:GET-BUFFER-HANDLE(1).
   
   tempcolh = dynbrwh:GET-BROWSE-COLUMN(1).  
   labelvar = "".
   DO WHILE VALID-HANDLE(tempcolh):
      IF tempcolh:VISIBLE = TRUE THEN labelvar = labelvar + tempcolh:LABEL + CHR(9).
      tempcolh = tempcolh:NEXT-COLUMN.         
   END.
   franfil = SESSION:TEMP-DIR + STRING(TIME) + ".txt". 
  
   tempcounter = 1.
   labelvar = REPLACE(labelvar,"!"," ").
   OUTPUT TO VALUE(franfil).
   IF labelvar NE ? THEN PUT UNFORMATTED  labelvar SKIP.
   FOR EACH exceltidut:  
      RUN excelut_UI.
      PUT SKIP.
      sistarad = exceltidut.RADNR.
         
   END.

   OUTPUT CLOSE.
   
   RUN open_UI (INPUT franfil).   
 
   {AONRUTSID.I}
   assign
   breddantal = 12
   bladvar = 0.
   iRad = 1.
   ASSIGN
   slutbredd = 16
   bredd[1] = 1
   bredd[2] = 12
   bredd[3] = 28
   bredd[4] = 43
   bredd[5] = 53
   bredd[6] = 60
   bredd[7] = 69
   bredd[8] = 77 
   bredd[9] = 93 
   bredd[10] = 110
   bredd[11] = 126
   bredd[12] = 140.
   
    ASSIGN
   slutbredd = 16
   utnr[1] = 1
   utnr[2] = 12
   utnr[3] = 28
   /*
   utnr[3] = 60
   */
   utnr[4] = 43
   utnr[5] = 53
   utnr[6] = 60
   utnr[7] = 69
   utnr[8] = 77 
   utnr[9] = 93 
   utnr[10] = 110
   utnr[11] = 126
   utnr[12] = 140
   allac[1] = "A"          
   allac[2] = "B"          
   allac[3] = "C"          
   allac[4] = "D"          
   allac[5] = "E"          
   allac[6] = "F"          
   allac[7] = "G"          
   allac[8] = "H"          
   allac[9] = "I"          
   allac[10] = "J"         
   allac[11] = "K"         
   allac[12] = "L"         
   allac[13] = "M"         
   allac[14] = "N"         
   allac[15] = "O"         
   allac[16] = "P"         
   allac[17] = "Q"         
   allac[18] = "R"         
   allac[19] = "S"         
   allac[20] = "T"         
   allac[21] = "U"         
   allac[22] = "V"         
   allac[23] = "W"         
   allac[24] = "X"         
   allac[25] = "Y"         
   allac[26] = "Z"         
   allac[27] = "AA"        
   allac[28] = "AB"
   
   allachar[1] = TRUE
   raknare = 1
   bildvar = FALSE.
   DO WHILE utnr[raknare + 1] > 0:
      bredd[raknare] = (utnr[raknare + 1]) - utnr[raknare].
      IF allac[raknare] = "A" THEN bredd[raknare] = bredd[raknare] + 2.
      IF allac[raknare] = "B" THEN bredd[raknare] = bredd[raknare] + 32.
      IF allac[raknare] = "D" THEN bredd[raknare] = bredd[raknare] + 2 + 2.
      IF allac[raknare] = "E" THEN bredd[raknare] = bredd[raknare] + 2 + 2.
      IF allac[raknare] = "E" THEN bredd[raknare] = bredd[raknare] + 2.
      IF allac[raknare] = "H" AND Guru.GlobalaVariabler:EgenskaperTillByggkontroll[1] = TRUE THEN bredd[raknare] = 5.
      raknare = raknare + 1.
   END.
   
   IF allac[1] = "" THEN RUN allac_UI.
   
   allachar[1] = TRUE.
   /*
   chWorkSheet:Range("A1:A300"):NumberFormat = "@" NO-ERROR.
   */
   RUN colbredd_UI.  
  
   RUN namnbladexcel_UI (INPUT "Byggprotokoll").
   RUN noscreenexcel_UI.
   /*
   chExcelApplication:VISIBLE = TRUE NO-ERROR.
   */
   raknare = 1.
   RUN kolumnexcel_UI.
   DEFINE VARIABLE radsnurra AS CHARACTER  NO-UNDO.
   chWorkSheet:Range("A1:L" + STRING(sistarad)):FONT:SIZE = 18 NO-ERROR.
   radsnurra =  "1".
   REPEAT: 
      RUN SokIRepExcel_UI (INPUT "Kryssruta","A" + radsnurra,INPUT "A" + STRING(sistarad),INPUT "",OUTPUT traffcoll).
      IF traffcoll = "" OR traffcoll = ? THEN LEAVE.
      radsnurra = REPLACE(traffcoll,"A","").
      radsnurra = REPLACE(radsnurra,"$","").
      RUN Kryssruta_UI (INPUT traffcoll, INPUT 1).    
   END.
   radsnurra =  "1".
   REPEAT: 
      RUN SokIRepExcel_UI (INPUT "Kryssruta","B" + radsnurra,INPUT "B" + STRING(sistarad),INPUT "",OUTPUT traffcoll).
      IF traffcoll = "" OR traffcoll = ? THEN LEAVE.
      radsnurra = REPLACE(traffcoll,"B","").
      radsnurra = REPLACE(radsnurra,"$","").
      RUN Kryssruta_UI (INPUT traffcoll, INPUT 1).   
   END.
   radsnurra =  "1".
   REPEAT: 
      RUN SokIRepExcel_UI (INPUT "Kryssruta","G" + radsnurra,INPUT "G" + STRING(sistarad),INPUT "",OUTPUT traffcoll).
      IF traffcoll = "" OR traffcoll = ? THEN LEAVE.
      radsnurra = REPLACE(traffcoll,"G","").
      radsnurra = REPLACE(radsnurra,"$","").
      RUN Kryssruta_UI (INPUT traffcoll, INPUT 1).    
   END.
   radsnurra =  "1".
   REPEAT: 
      RUN SokIRepExcel_UI (INPUT "Kryssruta","H" + radsnurra,INPUT "J" + STRING(sistarad),INPUT "",OUTPUT traffcoll).
      IF traffcoll = "" OR traffcoll = ? THEN LEAVE.
      radsnurra = REPLACE(traffcoll,"H","").
      radsnurra = REPLACE(radsnurra,"$","").
      RUN Kryssruta_UI (INPUT traffcoll, INPUT 1). 
   END.
   /*
   chExcelApplication:ScreenUpdating = TRUE.
  */
   radsnurra =  "1". 
   IF link NE ? THEN RUN imageexcelBygg_UI (INPUT link,INPUT "A",INPUT INTEGER(radsnurra)).
   REPEAT: 
      RUN SokIRepExcel_UI (INPUT "£","M" + radsnurra,INPUT "M" + STRING(sistarad),INPUT "",OUTPUT traffcoll).
      IF traffcoll = "" OR traffcoll = ? THEN LEAVE.
      cRange = traffcoll.
      radsnurra = REPLACE(traffcoll,"M","").
      radsnurra = REPLACE(radsnurra,"$","").
      IF sistarad <= INTEGER(radsnurra) + 5 THEN LEAVE.
      RUN sidbryt_UI.  
      IF link NE ? THEN RUN imageexcelBygg_UI (INPUT link,INPUT "A",INPUT INTEGER(radsnurra) ).
   END.
   RUN delColCh_UI (INPUT "M").
   RUN sidbrytbredd_UI  (INPUT 1).
   RUN innehall_UI.
   RUN slutexcel_UI.
   
   IF INDEX(franfil,".txt") > 0 THEN  sparatxtxlsx = REPLACE(franfil,".txt",".xls").     
   CREATE ostemp.
   ostemp.TEMPFIL = franfil.
   CREATE ostemp.
   ostemp.TEMPFIL = sparatxtxlsx. 
   IF Guru.Konstanter:globanv = "celpao" THEN DO:
     MESSAGE TIME - aa
     VIEW-AS ALERT-BOX.
   END.     
    /*
  
   IF  Guru.GlobalaVariabler:EgenskaperTillByggkontroll[2] = TRUE THEN DO:
      radsnurra =  "1". 
      REPEAT: 
         RUN sokiexcel_UI (INPUT "EGENKONTROLL","E" + radsnurra,INPUT "E" + STRING(sistarad),OUTPUT traffcoll).
         IF traffcoll = "" OR traffcoll = ? THEN LEAVE.
         radsnurra = REPLACE(traffcoll,"E","").
         radsnurra = REPLACE(radsnurra,"$","").
         cRange = "A" + STRING(INTEGER(radsnurra) - 1).
         FIND LAST bryttemp WHERE bryttemp.ROWVAR <= INTEGER(radsnurra) NO-LOCK NO-ERROR.
         
         IF AVAILABLE bryttemp THEN DO:
            IF bryttemp.ROWVAR <= INTEGER(radsnurra) - 6 THEN DO:
               CREATE bryttemp.
               bryttemp.EGENK = TRUE.
               bryttemp.ROWVAR = INTEGER(radsnurra) - 1.
            END.   
            IF bryttemp.EGENK = TRUE THEN DO:
               /*
               chWorkSheet:Rows(INTEGER(radsnurra) - 1):INSERT NO-ERROR.
               chWorkSheet:Rows(INTEGER(radsnurra) - 1):INSERT NO-ERROR.
               chWorkSheet:Rows(INTEGER(radsnurra) - 1):INSERT NO-ERROR.
               chWorkSheet:Rows(INTEGER(radsnurra) - 1):INSERT NO-ERROR.
               */
               chWorkSheet:Range(cRange):VALUE = "£" NO-ERROR.
               
               cRange = "c" + STRING(INTEGER(radsnurra) - 1).
               chWorkSheet:Range(cRange):VALUE = "£" NO-ERROR.
               radsnurra = STRING(INTEGER(radsnurra) + 6).
            END.
         END.      
         
      END.
   END.
   */   
  
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
  DISPLAY TOG_KOMP 
      WITH FRAME FRAME-VINST IN WINDOW WINDOW-2.
  ENABLE TOG_KOMP BTN_SKRIV BTN_EXCEL BTN_HTML BTN_PDF BTN_AVS 
      WITH FRAME FRAME-VINST IN WINDOW WINDOW-2.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-VINST}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE excelut_UI WINDOW-2 
PROCEDURE excelut_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE VARIABLE tempbufh AS HANDLE NO-UNDO.
   DEFINE VARIABLE decichar AS CHARACTER NO-UNDO.
   DEFINE VARIABLE datumh AS CHARACTER NO-UNDO.
   DEFINE VARIABLE arraynamn AS CHARACTER NO-UNDO.
   DEFINE VARIABLE arraycounter AS INTEGER NO-UNDO.
   tempcolh = dynbrwh:GET-BROWSE-COLUMN(1).    
   LOOP:
   DO WHILE VALID-HANDLE(tempcolh):
      IF tempcolh:NAME = ? THEN PUT UNFORMATTED CHR(9).
      ELSE IF tempcolh:VISIBLE = FALSE THEN "".
      ELSE DO:
         tempbufh = dynbuffh:BUFFER-FIELD(tempcolh:NAME) NO-ERROR.
         IF tempbufh:EXTENT > 0 THEN DO:
            IF arraynamn = tempbufh:NAME THEN arraycounter = arraycounter + 1.
            ELSE arraycounter = 1. 
            arraynamn = tempbufh:NAME.
            IF tempbufh:BUFFER-VALUE(arraycounter) = ? THEN DO:
               PUT UNFORMATTED CHR(9).
               tempcolh = tempcolh:NEXT-COLUMN.
               NEXT LOOP.
            END.
         END.
         ELSE DO:
            arraynamn = "".
            arraycounter = 0.
                
            IF tempbufh:BUFFER-VALUE = ? THEN DO:
               PUT UNFORMATTED CHR(9).
               tempcolh = tempcolh:NEXT-COLUMN.
               NEXT LOOP. 
            END.
         END.
         
         IF tempcolh:DATA-TYPE = "DECIMAL" THEN DO: 
            IF arraynamn = tempbufh:NAME THEN DO:
               IF tempbufh:BUFFER-VALUE(arraycounter) = "?" THEN tempbufh:BUFFER-VALUE(arraycounter) = "".
               decichar = REPLACE(STRING(ROUND(tempbufh:BUFFER-VALUE(arraycounter),2)),".",",").
            END.
            ELSE decichar = REPLACE(STRING(ROUND(tempbufh:BUFFER-VALUE,2)),".",",").               
            PUT UNFORMATTED STRING(decichar) + CHR(9).
         END.
         ELSE IF tempcolh:DATA-TYPE = "Date" THEN DO:
             IF arraynamn = tempbufh:NAME THEN DO:
               IF tempbufh:BUFFER-VALUE(arraycounter) = "?" THEN tempbufh:BUFFER-VALUE(arraycounter) = "".
               PUT UNFORMATTED STRING(tempbufh:BUFFER-VALUE,"99/99/9999") + CHR(9).
            END.
            ELSE  PUT UNFORMATTED STRING(tempbufh:BUFFER-VALUE,"99/99/9999") + CHR(9).
           
         END.
         ELSE IF tempcolh:NAME  = "ENR" OR tempcolh:LABEL = Guru.Konstanter:genk OR tempcolh:LABEL  = "Till Enr" OR tempcolh:LABEL  = "Från Enr"
         OR tempcolh:NAME  = "Till " + Guru.Konstanter:genk OR tempcolh:NAME  = "Från " + Guru.Konstanter:genk OR tempcolh:NAME  = "ENRERS" THEN DO:
            
            IF SUBSTRING(tempbufh:BUFFER-VALUE,1,1) = "E" THEN PUT UNFORMATTED STRING(tempbufh:BUFFER-VALUE) + CHR(9).
            ELSE IF STRING(tempbufh:BUFFER-VALUE) = "" THEN PUT UNFORMATTED STRING(tempbufh:BUFFER-VALUE) + CHR(9).
            ELSE PUT UNFORMATTED "E" + STRING(tempbufh:BUFFER-VALUE) + CHR(9).   
         END.
         ELSE IF tempcolh:DATA-TYPE = "LOGICAL" THEN DO:
            IF arraynamn = tempbufh:NAME THEN DO:
               IF tempbufh:BUFFER-VALUE(arraycounter) = "?" THEN tempbufh:BUFFER-VALUE(arraycounter) = "".
               PUT UNFORMATTED STRING(tempbufh:BUFFER-VALUE,tempcolh:FORMAT) + CHR(9).
            END.
            ELSE PUT UNFORMATTED STRING(tempbufh:BUFFER-VALUE(arraycounter),tempcolh:FORMAT) + CHR(9).
         END.   
         ELSE DO:
            IF arraynamn = tempbufh:NAME THEN DO:
               IF tempbufh:BUFFER-VALUE(arraycounter) = "?" THEN tempbufh:BUFFER-VALUE(arraycounter) = "".
               PUT UNFORMATTED STRING(tempbufh:BUFFER-VALUE(arraycounter)) + CHR(9).
            END.
            ELSE DO:   
               IF tempbufh:BUFFER-VALUE = "?" THEN tempbufh:BUFFER-VALUE = "".
               PUT UNFORMATTED STRING(tempbufh:BUFFER-VALUE) + CHR(9).
             END.  
         END.   
      END.
      
      tempcolh = tempcolh:NEXT-COLUMN.         
   END.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Ininnehall_UI WINDOW-2 
PROCEDURE Ininnehall_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   IF Guru.Konstanter:globforetag = "ATS"  THEN. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InnehallSida_UI WINDOW-2 
PROCEDURE InnehallSida_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   cRangefont = "A" + cRad + ":" + "F" + cRad.
   chWorkSheet:Range(cRangefont):FONT:NAME = "CALIBRI" NO-ERROR.
   chWorkSheet:Range(cRangefont):FONT:SIZE = 10 NO-ERROR.
   chWorkSheet:Range(cRangefont):FONT:Bold = FALSE NO-ERROR.
   chWorkSheet:Range(cRangefont):RowHeight = 12 NO-ERROR.
   chWorkSheet:Range(cRangefont):FONT:ColorIndex = 11 NO-ERROR.
  
   RUN Csel_UI ("A",innehall.BYGGNR).
   RUN Csel_UI ("B",innehall.FRIID).
   RUN Csel_UI ("C",innehall.KARTREF).
   RUN Csel_UI ("D",innehall.F1 
   /* + "   " +
   innehall.F2 + "   " +
   innehall.F3 + "   " +
   innehall.F4 + "   " +
   innehall.F5 + "   " +
   innehall.F6*/) .
   RUN Csel_UI ("F",STRING(innehall.SIDA)).
   /*
   RUN Csel_UI ("g",STRING(innehall.ROWVAR)).
*/
      {EXCELFEL.I}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE innehall_UI WINDOW-2 
PROCEDURE innehall_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   
   bladvar = 1.
   IF chExcelApplication:VERSION = "15.0" OR chExcelApplication:VERSION = "16.0" THEN antalbladstart = 2.
   RUN nyttbladexcel_UI.
   RUN namnbladexcel_UI ("Innehållsförteckning").
   raknare = 1.
   totheight = 1 .
   IF link NE ? THEN DO:
      RUN imageexcelBygg_UI (INPUT link,INPUT "A",INPUT 1).
      /*
      RUN imageexcel_UI (INPUT link,INPUT "A",INPUT 1).
      */
   END.   
   raknare = 6.
   FOR EACH innehall WHERE innehall.SIDA = 9999999 NO-LOCK:
      raknare = raknare + 1.
      cRad = STRING(raknare).
      cRangefont = "c" + cRad + ":" + "D" + cRad.
      chWorkSheet:Range(cRangefont):FONT:NAME = "CALIBRI" NO-ERROR.
      chWorkSheet:Range(cRangefont):FONT:SIZE = 9 NO-ERROR.
      chWorkSheet:Range(cRangefont):FONT:Bold = FALSE NO-ERROR.
      chWorkSheet:Range(cRangefont):RowHeight = 12 NO-ERROR.
      chWorkSheet:Range(cRangefont):FONT:ColorIndex = 11 NO-ERROR.
      IF INDEX(innehall.UT," :") > 0 THEN DO:
         RUN Csel_UI ("C",SUBSTRING(innehall.UT,1,27)).
         RUN Csel_UI ("D",SUBSTRING(innehall.UT,28)).
      END.
      ELSE RUN Csel_UI ("D",innehall.UT).
      DELETE innehall.
   END.
   
   raknare = raknare + 1.
   cRad = STRING(raknare).
   cRangefont = "A" + cRad + ":" + "F" + cRad.
   chWorkSheet:Range(cRangefont):FONT:NAME = "CALIBRI" NO-ERROR.
   chWorkSheet:Range(cRangefont):FONT:SIZE = 10 NO-ERROR.
   chWorkSheet:Range(cRangefont):FONT:Bold = FALSE NO-ERROR.
   chWorkSheet:Range(cRangefont):RowHeight = 12 NO-ERROR.
   chWorkSheet:Range(cRangefont):FONT:ColorIndex = 11 NO-ERROR.
   RUN Csel_UI ("A","Byggnr").
   RUN Csel_UI ("B","Fri Id").
   RUN Csel_UI ("C",Guru.Konstanter:varforetypchar[13]).
   RUN Csel_UI ("D","Konstruktion").
   RUN Csel_UI ("F","Sid").
  
   FOR EACH innehall  NO-LOCK BY ROWVAR :
     raknare = raknare + 1.    
     cRad = STRING(raknare).
     RUN InnehallSida_UI.
    
  END.
  chWorkSheet:Columns("a:ab"):EntireColumn:AutoFit NO-ERROR.
  chWindow = chExcelApplication:Windows:ITEM(1) NO-ERROR.
  RUN sidbrytbredd_UI  (INPUT 1).
  RUN valjblad_UI (1).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE KonstruktionPasidan_UI WINDOW-2 
PROCEDURE KonstruktionPasidan_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   FOR EACH exceltidut WHERE exceltidut.BCOLOR NE 0 NO-LOCK:
      FIND FIRST innehall WHERE innehall.rowvar = extemp.rowvar NO-LOCK NO-ERROR.
      IF AVAILABLE innehall THEN innehall.SIDA = sidraknare.
   END. 
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE openStart_UI WINDOW-2 
PROCEDURE openStart_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ostempdel_UI WINDOW-2 
PROCEDURE ostempdel_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   FOR EACH ostemp WHERE NO-LOCK:
      OS-DELETE VALUE(ostemp.TEMPFIL) NO-ERROR.
   END.
   EMPTY TEMP-TABLE ostemp NO-ERROR. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ut_UI WINDOW-2 
PROCEDURE ut_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  /*UT*/
   skrivut = FALSE.                         
   FIND LAST tidut NO-LOCK NO-ERROR.
   vartprospar = vartpro.
   vartpro = "berlist44".
   RUN SKRIVVAL.W (INPUT FALSE).
   vartpro = vartprospar.
   FIND FIRST felmeddtemp NO-ERROR.
   IF AVAILABLE felmeddtemp THEN DELETE felmeddtemp.

        
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

