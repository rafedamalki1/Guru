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

/* Local Variable Definitions ---                                       */ 
{ALLDEF.I}
&Scoped-define NEW 
{GLOBVAR2DEL1.I}
{REGVAR.I}  
{ANMARKD.I}
{LISTDEF.I}
                              
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
DEFINE VARIABLE link AS CHARACTER NO-UNDO.
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
DEFINE VARIABLE utnr AS INTEGER EXTENT 50 NO-UNDO.
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
   ASSIGN
   utnr[1] = 1
   utnr[2] = 12
   utnr[3] = 28
   utnr[4] = 43
   utnr[5] = 53
   utnr[6] = 60
   utnr[7] = 69
   utnr[8] = 77.   
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
&Scoped-define INTERNAL-TABLES tidut

/* Definitions for BROWSE BRW_UT                                        */
&Scoped-define FIELDS-IN-QUERY-BRW_UT tidut.ut 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_UT 
&Scoped-define QUERY-STRING-BRW_UT FOR EACH tidut NO-LOCK
&Scoped-define OPEN-QUERY-BRW_UT OPEN QUERY BRW_UT FOR EACH tidut NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_UT tidut
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_UT tidut


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
      tidut SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_UT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_UT WINDOW-2 _STRUCTURED
  QUERY BRW_UT NO-LOCK DISPLAY
      tidut.ut FORMAT "X(132)":U
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

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-2)
THEN WINDOW-2:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_UT
/* Query rebuild information for BROWSE BRW_UT
     _TblList          = "Temp-Tables.tidut"
     _Options          = "NO-LOCK"
     _FldNameList[1]   = Temp-Tables.tidut.ut
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
   DEFINE VARIABLE helput AS CHARACTER NO-UNDO.   
   DEFINE VARIABLE helpst AS INTEGER NO-UNDO.
   Guru.GlobalaVariabler:ByggproSid = 1.
   
   SIDEX:
   FOR EACH tidut WHERE NO-LOCK:
      IF SUBSTRING(tidut.UT,132) = "$" THEN  Guru.GlobalaVariabler:ByggproSid =  Guru.GlobalaVariabler:ByggproSid + 1.
   END.
   EXCELL:
   FOR EACH tidut WHERE NO-LOCK:
      IF SUBSTRING(tidut.UT,1,5) = "*****" THEN LEAVE EXCELL.
      IF SUBSTRING(tidut.UT,1,5) = "-----" THEN LEAVE EXCELL.
      IF SUBSTRING(tidut.UT,1,8) = "E-NUMMER" THEN LEAVE EXCELL.
      CREATE innehall.
      innehall.UT = tidut.UT.
      innehall.SIDA = 9999999.
   END.
  
  
   RUN BERLIST4EX.P (INPUT sidlangdex,INPUT TOG_KOMP, INPUT TABLE innehall).
  
   FOR EACH innehall WHERE innehall.SIDA = 9999999 NO-LOCK:
      DELETE innehall.
   END.
   Guru.GlobalaVariabler:ByggproSid = 0. 
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
   Guru.GlobalaVariabler:ByggproSid = 1.
   SIDPDF:
   FOR EACH tidut WHERE NO-LOCK:
      IF SUBSTRING(tidut.UT,132) = "$" THEN  Guru.GlobalaVariabler:ByggproSid =  Guru.GlobalaVariabler:ByggproSid + 1.
   END.
 
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
   Guru.GlobalaVariabler:ByggproSid = 0.
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
                                     
   
   IF Guru.Konstanter:globanv = "cELPAO" AND Guru.Konstanter:AppSpringSet[1] = "OEELPAclas" THEN DO:    
      RUN SetDefaultCursors IN Guru.Konstanter:hpApi.
      RUN Modules\Beredning\BYGGPROTTK.P (INPUT TABLE extemp).   
   END.
    
   RUN Kompinnehall_UI IN berlistapph. 
   RUN Utinnehall_UI IN berlistapph (OUTPUT TABLE innehall).                                      
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
      
   BTN_HTML:HIDDEN = TRUE.
   Guru.GlobalaVariabler:colrighth = BTN_EXCEL:HANDLE.      
   RUN buttcolh_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   
   Guru.GlobalaVariabler:colrighth = BTN_SKRIV:HANDLE.      
   RUN buttcolh_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
      
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
      DEBUGGER:SET-BREAK().
      RUN EgenskaperTillByggkontroll IN berlistapph (INPUT Guru.GlobalaVariabler:EgenskaperTillByggkontroll).    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ColBredd_UI WINDOW-2 
PROCEDURE ColBredd_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
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

