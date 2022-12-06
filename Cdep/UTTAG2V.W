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

  Created: 95/05/02 - 12:41 pm

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
DEFINE INPUT-OUTPUT PARAMETER valnr AS INTEGER NO-UNDO.
/* Local Variable Definitions ---                                       */
{ALLDEF.I}
{GLOBVAR2DEL1.I}
&Scoped-define NEW 
&Scoped-define SHARED SHARED
{LEVTEMP.I}
{DEPATEMP.I}
{LTRPTEMP.I}
{SPECMTRLTEMP.I}
{SOKDEF.I}
{HOPPSEK2W.I}
&Scoped-define NEW NEW
&Scoped-define SHARED SHARED
{MTRLTEMP.I}

DEFINE NEW SHARED VARIABLE nytt_bestnr2 AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE bestalld AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE fakt AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE foljesedel AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE spec_rowid AS ROWID NO-UNDO.
DEFINE NEW SHARED VARIABLE mtrl_rowid AS ROWID NO-UNDO.
DEFINE SHARED VARIABLE depatitta AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE ny AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE valnummer AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE valaonr AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE valdelnr AS INTEGER NO-UNDO.   
DEFINE SHARED VARIABLE valort AS CHARACTER NO-UNDO. 
DEFINE SHARED VARIABLE valomrade AS CHARACTER NO-UNDO.     
DEFINE SHARED VARIABLE valkund AS CHARACTER NO-UNDO. 
DEFINE SHARED VARIABLE valnamn AS CHARACTER NO-UNDO.    
DEFINE SHARED VARIABLE sokant AS LOGICAL NO-UNDO. 
DEFINE SHARED VARIABLE bestant AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE huvudlev AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE vald_kundlev AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE vald_lev AS CHARACTER NO-UNDO.  
DEFINE SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.  
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO. 
DEFINE SHARED VARIABLE vartpro AS CHARACTER FORMAT "X(3)" NO-UNDO.   
DEFINE SHARED VARIABLE vald_depa AS INTEGER NO-UNDO.

DEFINE VARIABLE mtrlbapph AS HANDLE NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE aosok AS CHARACTER FORMAT "X(40)" NO-UNDO.
DEFINE VARIABLE posok AS CHARACTER FORMAT "X(11)" NO-UNDO.
DEFINE VARIABLE antal_valda AS INTEGER NO-UNDO.
DEFINE VARIABLE antal_raknare AS INTEGER NO-UNDO. 

DEFINE VARIABLE lev AS CHARACTER NO-UNDO.  
DEFINE VARIABLE svar AS LOGICAL NO-UNDO.
DEFINE VARIABLE dbenamning AS CHARACTER NO-UNDO.  
DEFINE VARIABLE bnummer AS INTEGER NO-UNDO. 
DEFINE VARIABLE antalvar AS INTEGER NO-UNDO.
DEFINE VARIABLE enrval AS LOGICAL NO-UNDO.
DEFINE VARIABLE enrvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE enrvar2 AS CHARACTER NO-UNDO.
DEFINE VARIABLE forstaenr AS CHARACTER NO-UNDO.
DEFINE VARIABLE sistaenr AS CHARACTER NO-UNDO.
/*DEFINE VARIABLE laddabrwproch AS HANDLE NO-UNDO.
DEFINE VARIABLE ttmtrlhandle AS HANDLE NO-UNDO.*/
DEFINE VARIABLE felmedd AS CHARACTER NO-UNDO.
/*DEFINE VARIABLE brwsok AS CHARACTER NO-UNDO.
DEFINE VARIABLE multitid AS INTEGER NO-UNDO.*/
/*DEFINE VARIABLE multibrwsok AS CHARACTER NO-UNDO.*/
DEFINE VARIABLE anyprintquery AS CHARACTER NO-UNDO.
/*DEFINE VARIABLE orgfraga AS CHARACTER NO-UNDO.*/
DEFINE VARIABLE numrows AS INTEGER NO-UNDO.
DEFINE VARIABLE spenr AS CHARACTER  NO-UNDO.

DEFINE VARIABLE fackvar AS CHARACTER NO-UNDO.  
DEFINE VARIABLE var1 AS CHARACTER FORMAT "X(6)" NO-UNDO.
DEFINE VARIABLE var2 AS CHARACTER FORMAT "X(6)" NO-UNDO.   
DEFINE VARIABLE fackid AS CHARACTER FORMAT "X(8)" NO-UNDO.
DEFINE VARIABLE langd AS INTEGER NO-UNDO. 
&Scoped-define NEW  
&Scoped-define SHARED SHARED 
{bestnrtab.I}
/*DEFINE SHARED TEMP-TABLE best_nr_tab NO-UNDO
    FIELD Bestnr AS INTEGER 
    FIELD bestdatum AS DATE LABEL "Best-datum"                  
    FIELD bestallare AS CHARACTER LABEL "Beställare"                  
    INDEX BESTNR BESTNR DESCENDING.*/ 
    
DEFINE SHARED TEMP-TABLE gam_mtrl NO-UNDO
   FIELD DATUM AS DATE
   FIELD DATUM2 AS DATE
   FIELD KOM AS CHARACTER.    


{DEFSOK.I}

DEFINE NEW SHARED TEMP-TABLE aspec_mtrl NO-UNDO LIKE spec_mtrl
   FIELD NY AS LOGICAL
   FIELD ANDRAD AS LOGICAL
   FIELD BORT AS LOGICAL.   

DEFINE NEW SHARED TEMP-TABLE off_mtrl NO-UNDO     
    FIELD TOTALT AS DECIMAL.  
    
DEFINE NEW SHARED TEMP-TABLE skapa_mtrl NO-UNDO                
   FIELD FORE AS CHARACTER
   FIELD KADR AS CHARACTER  
   FIELD KPNR AS CHARACTER
   FIELD KORT AS CHARACTER
   FIELD BOX AS CHARACTER FORMAT "X(5)"
   FIELD FAX AS CHARACTER
   FIELD KIKONTAKT AS CHARACTER                            
   FIELD KITELE AS CHARACTER        
   FIELD BESTNAMN AS CHARACTER    
   FIELD TELE AS CHARACTER
   FIELD ADR AS CHARACTER
   FIELD PNR AS CHARACTER
   FIELD ORT AS CHARACTER
   FIELD KONTAKT AS CHARACTER 
   FIELD FAXK AS CHARACTER
   FIELD FAKADR AS CHARACTER
   FIELD FAKPNR AS CHARACTER
   FIELD FAKORT AS CHARACTER
   FIELD LEVADR AS CHARACTER
   FIELD LEVPNR AS CHARACTER
   FIELD LEVORT AS CHARACTER   
   FIELD DATUM AS DATE
   FIELD DATUM2 AS DATE
   FIELD MARK AS CHARACTER FORMAT "X(35)"
   FIELD KOM AS CHARACTER FORMAT "X(40)".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A
&Scoped-define BROWSE-NAME BRW_DEPA

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES mtrldeptemp mtrltemp spec_mtrl

/* Definitions for BROWSE BRW_DEPA                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_DEPA mtrldeptemp.FACKID mtrldeptemp.ENR ~
mtrldeptemp.BENAMNING mtrldeptemp.ENHET mtrldeptemp.NPRIS 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_DEPA 
&Scoped-define QUERY-STRING-BRW_DEPA FOR EACH mtrldeptemp NO-LOCK ~
    BY mtrldeptemp.ENR
&Scoped-define OPEN-QUERY-BRW_DEPA OPEN QUERY BRW_DEPA FOR EACH mtrldeptemp NO-LOCK ~
    BY mtrldeptemp.ENR.
&Scoped-define TABLES-IN-QUERY-BRW_DEPA mtrldeptemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_DEPA mtrldeptemp


/* Definitions for BROWSE BRW_HLEV                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_HLEV mtrltemp.Enr mtrltemp.Benamning ~
mtrltemp.Enhet mtrltemp.NPRIS 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_HLEV 
&Scoped-define QUERY-STRING-BRW_HLEV FOR EACH mtrltemp ~
      WHERE mtrltemp.LEVKOD = vald_lev NO-LOCK ~
    BY mtrltemp.Enr
&Scoped-define OPEN-QUERY-BRW_HLEV OPEN QUERY BRW_HLEV FOR EACH mtrltemp ~
      WHERE mtrltemp.LEVKOD = vald_lev NO-LOCK ~
    BY mtrltemp.Enr.
&Scoped-define TABLES-IN-QUERY-BRW_HLEV mtrltemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_HLEV mtrltemp


/* Definitions for BROWSE BRW_MTRL                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_MTRL spec_mtrl.Enr spec_mtrl.SALDO ~
spec_mtrl.Benamning spec_mtrl.Enhet spec_mtrl.NPRIS 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_MTRL spec_mtrl.SALDO 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_MTRL spec_mtrl
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_MTRL spec_mtrl
&Scoped-define QUERY-STRING-BRW_MTRL FOR EACH spec_mtrl NO-LOCK
&Scoped-define OPEN-QUERY-BRW_MTRL OPEN QUERY BRW_MTRL FOR EACH spec_mtrl NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_MTRL spec_mtrl
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_MTRL spec_mtrl


/* Definitions for FRAME FRAME-A                                        */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BRW_DEPA BRW_MTRL RAD_DEPA BRW_HLEV ~
FBTN_PRIS btn_over FBTN_FOLJ btn_back FBTN_VISA BTN_BEST BTN_AVB BTN_SPEC ~
BTN_LEV CMB_LEV FBTN_SKRIV FILL-IN-ENR FILL-IN-BEN FILL-IN-ENR2 ~
FILL-IN-ANTAL BTN_UP BTN_MIN RAD_SOK FBTN_VISA2 RECT-2 RECT-4 
&Scoped-Define DISPLAYED-OBJECTS RAD_DEPA CMB_LEV FILL-IN-ENR FILL-IN-BEN ~
FILL-IN-ENR2 FILL-IN-ANTAL RAD_SOK 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR WINDOW-1 AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB 
     LABEL "Avbryt":L 
     SIZE 14 BY 1.

DEFINE BUTTON btn_back 
     IMAGE-UP FILE "BILDER\prev-u":U
     LABEL "":L 
     SIZE 5.5 BY 1.75.

DEFINE BUTTON BTN_BEST 
     LABEL "Ok" 
     SIZE 14 BY 1
     FGCOLOR 1 .

DEFINE BUTTON BTN_LEV 
     LABEL "Huvudlev." 
     SIZE 12 BY 1
     BGCOLOR 8 .

DEFINE BUTTON BTN_MIN 
     LABEL "-" 
     SIZE 2.5 BY .75.

DEFINE BUTTON btn_over 
     IMAGE-UP FILE "BILDER\next-u":U
     LABEL "":L 
     SIZE 5.5 BY 1.75.

DEFINE BUTTON BTN_SPEC 
     LABEL "Spec.mtrl" 
     SIZE 12 BY 1
     FGCOLOR 1 .

DEFINE BUTTON BTN_UP 
     LABEL "+" 
     SIZE 2.5 BY .75.

DEFINE BUTTON FBTN_FOLJ 
     LABEL "Följesedel" 
     SIZE 14 BY 1
     FGCOLOR 1 .

DEFINE BUTTON FBTN_PRIS 
     LABEL "Tot. pris" 
     SIZE 14 BY 1
     FGCOLOR 1 .

DEFINE BUTTON FBTN_SKRIV 
     LABEL "Skriv ut" 
     SIZE 14 BY 1
     FGCOLOR 1 .

DEFINE BUTTON FBTN_VISA 
     LABEL "Mtrl.spec" 
     SIZE 14 BY 1
     FGCOLOR 1 .

DEFINE BUTTON FBTN_VISA2 
     LABEL "Visa" 
     SIZE 14 BY 1
     FGCOLOR 1 .

DEFINE VARIABLE CMB_LEV AS CHARACTER FORMAT "X(15)":U 
     LABEL "Leverantörer" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "0" 
     DROP-DOWN-LIST
     SIZE 18.25 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-ANTAL AS INTEGER FORMAT ">>>>>9":U INITIAL 1 
     LABEL "Antal" 
     VIEW-AS FILL-IN 
     SIZE 11.88 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN-BEN AS CHARACTER FORMAT "X(40)":U 
     LABEL "Benämning" 
     VIEW-AS FILL-IN 
     SIZE 19 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN-ENR AS CHARACTER FORMAT "X(11)":U 
     LABEL "Enr" 
     VIEW-AS FILL-IN 
     SIZE 19 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN-ENR2 AS CHARACTER FORMAT "X(11)":U 
     LABEL "Enr" 
     VIEW-AS FILL-IN 
     SIZE 19 BY .83 NO-UNDO.

DEFINE VARIABLE RAD_DEPA AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Depå-mtrl", 1,
"Total-mtrl", 2
     SIZE 31.75 BY 1 NO-UNDO.

DEFINE VARIABLE RAD_SOK AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Början", 1,
"Någonstans", 2,
"Slutet", 3
     SIZE 35.5 BY .83 NO-UNDO.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 42.5 BY 3.63
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 56.88 BY 3.63
     BGCOLOR 8 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_DEPA FOR 
      mtrldeptemp SCROLLING.

DEFINE QUERY BRW_HLEV FOR 
      mtrltemp SCROLLING.

DEFINE QUERY BRW_MTRL FOR 
      spec_mtrl SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_DEPA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_DEPA WINDOW-1 _STRUCTURED
  QUERY BRW_DEPA NO-LOCK DISPLAY
      mtrldeptemp.FACKID FORMAT "X(8)":U
      mtrldeptemp.ENR FORMAT "X(11)":U
      mtrldeptemp.BENAMNING FORMAT "x(40)":U WIDTH 30
      mtrldeptemp.ENHET FORMAT "x(5)":U
      mtrldeptemp.NPRIS FORMAT ">>>>99.99":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING MULTIPLE SIZE 42.5 BY 20
         TITLE "Materiellista för depå.".

DEFINE BROWSE BRW_HLEV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_HLEV WINDOW-1 _STRUCTURED
  QUERY BRW_HLEV NO-LOCK DISPLAY
      mtrltemp.Enr FORMAT "X(11)":U
      mtrltemp.Benamning FORMAT "x(40)":U WIDTH 30
      mtrltemp.Enhet FORMAT "x(5)":U
      mtrltemp.NPRIS FORMAT ">>>>99.99":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING MULTIPLE SIZE 42.5 BY 20
         TITLE "Materiellista huvudleverantör ".

DEFINE BROWSE BRW_MTRL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_MTRL WINDOW-1 _STRUCTURED
  QUERY BRW_MTRL NO-LOCK DISPLAY
      spec_mtrl.Enr FORMAT "X(11)":U
      spec_mtrl.SALDO COLUMN-LABEL "Antal" FORMAT ">>>>>9":U
      spec_mtrl.Benamning FORMAT "x(30)":U WIDTH 25
      spec_mtrl.Enhet FORMAT "x(5)":U
      spec_mtrl.NPRIS COLUMN-LABEL "Nettopris" FORMAT ">>>>>9.99":U
  ENABLE
      spec_mtrl.SALDO
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING MULTIPLE SIZE 56.88 BY 20
         TITLE "Vald materiel".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     BRW_DEPA AT ROW 2.67 COL 1.63
     BRW_MTRL AT ROW 2.67 COL 50.63
     RAD_DEPA AT ROW 1.25 COL 1.5 NO-LABEL
     BRW_HLEV AT ROW 2.67 COL 1.5
     FBTN_PRIS AT ROW 8 COL 110.88
     btn_over AT ROW 7.63 COL 44.75
     FBTN_FOLJ AT ROW 9.08 COL 110.88
     btn_back AT ROW 11.13 COL 44.75
     FBTN_VISA AT ROW 10.21 COL 110.88
     BTN_BEST AT ROW 26.83 COL 110.88
     BTN_AVB AT ROW 27.92 COL 110.88
     BTN_SPEC AT ROW 23.08 COL 73.5
     BTN_LEV AT ROW 23.08 COL 11.75
     CMB_LEV AT ROW 23.08 COL 13.75 COLON-ALIGNED
     FBTN_SKRIV AT ROW 12.38 COL 110.88
     FILL-IN-ENR AT ROW 25.54 COL 14.88 COLON-ALIGNED
     FILL-IN-BEN AT ROW 26.63 COL 14.88 COLON-ALIGNED
     FILL-IN-ENR2 AT ROW 25.54 COL 64.25 COLON-ALIGNED
     FILL-IN-ANTAL AT ROW 26.88 COL 64.25 COLON-ALIGNED
     BTN_UP AT ROW 26.5 COL 80.25
     BTN_MIN AT ROW 27.5 COL 80.25
     RAD_SOK AT ROW 27.67 COL 6.63 NO-LABEL
     FBTN_VISA2 AT ROW 11.29 COL 110.88
     "Sök på:" VIEW-AS TEXT
          SIZE 8 BY .83 AT ROW 25.54 COL 2
     "Hämta:" VIEW-AS TEXT
          SIZE 8.38 BY .83 AT ROW 25.54 COL 51.75
     RECT-2 AT ROW 25.29 COL 1.5
     RECT-4 AT ROW 25.29 COL 50.63
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 125 BY 28.42.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: 
   Temp-Tables and Buffers:
      TABLE: mtrldeptemp T "?" NO-UNDO temp-db mtrldeptemp
      TABLE: mtrltemp T "?" NO-UNDO temp-db mtrltemp
      TABLE: ? T "?" NO-UNDO temp-db sok_mtrl
      TABLE: ? T "?" NO-UNDO temp-db spec_mtrl
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW WINDOW-1 ASSIGN
         HIDDEN             = YES
         TITLE              = ""
         HEIGHT             = 28.42
         WIDTH              = 125
         MAX-HEIGHT         = 28.42
         MAX-WIDTH          = 125
         VIRTUAL-HEIGHT     = 28.42
         VIRTUAL-WIDTH      = 125
         RESIZE             = yes
         SCROLL-BARS        = no
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
   FRAME-NAME Custom                                                    */
/* BROWSE-TAB BRW_DEPA 1 FRAME-A */
/* BROWSE-TAB BRW_MTRL BRW_DEPA FRAME-A */
/* BROWSE-TAB BRW_HLEV RAD_DEPA FRAME-A */
ASSIGN 
       BRW_DEPA:HIDDEN  IN FRAME FRAME-A                = TRUE
       BRW_DEPA:MAX-DATA-GUESS IN FRAME FRAME-A         = 40000
       BRW_DEPA:ALLOW-COLUMN-SEARCHING IN FRAME FRAME-A = TRUE.

ASSIGN 
       BRW_HLEV:HIDDEN  IN FRAME FRAME-A                = TRUE
       BRW_HLEV:MAX-DATA-GUESS IN FRAME FRAME-A         = 40000
       BRW_HLEV:ALLOW-COLUMN-SEARCHING IN FRAME FRAME-A = TRUE.

ASSIGN 
       BRW_MTRL:MAX-DATA-GUESS IN FRAME FRAME-A         = 40000
       BRW_MTRL:ALLOW-COLUMN-SEARCHING IN FRAME FRAME-A = TRUE.

ASSIGN 
       BTN_LEV:HIDDEN IN FRAME FRAME-A           = TRUE.

ASSIGN 
       BTN_MIN:HIDDEN IN FRAME FRAME-A           = TRUE.

ASSIGN 
       BTN_UP:HIDDEN IN FRAME FRAME-A           = TRUE.

ASSIGN 
       CMB_LEV:HIDDEN IN FRAME FRAME-A           = TRUE.

ASSIGN 
       FILL-IN-ANTAL:HIDDEN IN FRAME FRAME-A           = TRUE.

ASSIGN 
       FILL-IN-BEN:HIDDEN IN FRAME FRAME-A           = TRUE.

ASSIGN 
       FILL-IN-ENR:HIDDEN IN FRAME FRAME-A           = TRUE.

ASSIGN 
       FILL-IN-ENR2:HIDDEN IN FRAME FRAME-A           = TRUE.

ASSIGN 
       RECT-4:HIDDEN IN FRAME FRAME-A           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-1)
THEN WINDOW-1:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_DEPA
/* Query rebuild information for BROWSE BRW_DEPA
     _TblList          = "Temp-Tables.mtrldeptemp"
     _Options          = "NO-LOCK"
     _OrdList          = "Temp-Tables.mtrldeptemp.ENR|yes"
     _FldNameList[1]   = Temp-Tables.mtrldeptemp.FACKID
     _FldNameList[2]   = Temp-Tables.mtrldeptemp.ENR
     _FldNameList[3]   > Temp-Tables.mtrldeptemp.BENAMNING
"mtrldeptemp.BENAMNING" ? ? "character" ? ? ? ? ? ? no ? no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   = Temp-Tables.mtrldeptemp.ENHET
     _FldNameList[5]   = Temp-Tables.mtrldeptemp.NPRIS
     _Query            is NOT OPENED
*/  /* BROWSE BRW_DEPA */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_HLEV
/* Query rebuild information for BROWSE BRW_HLEV
     _TblList          = "Temp-Tables.mtrltemp"
     _Options          = "NO-LOCK"
     _OrdList          = "Temp-Tables.mtrltemp.Enr|yes"
     _Where[1]         = "Temp-Tables.mtrltemp.LEVKOD = vald_lev"
     _FldNameList[1]   = Temp-Tables.mtrltemp.Enr
     _FldNameList[2]   > Temp-Tables.mtrltemp.Benamning
"mtrltemp.Benamning" ? ? "character" ? ? ? ? ? ? no ? no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   = Temp-Tables.mtrltemp.Enhet
     _FldNameList[4]   = Temp-Tables.mtrltemp.NPRIS
     _Query            is NOT OPENED
*/  /* BROWSE BRW_HLEV */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_MTRL
/* Query rebuild information for BROWSE BRW_MTRL
     _TblList          = "Temp-Tables.spec_mtrl"
     _Options          = "NO-LOCK"
     _FldNameList[1]   = Temp-Tables.spec_mtrl.Enr
     _FldNameList[2]   > Temp-Tables.spec_mtrl.SALDO
"spec_mtrl.SALDO" "Antal" ">>>>>9" "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.spec_mtrl.Benamning
"spec_mtrl.Benamning" ? "x(30)" "character" ? ? ? ? ? ? no ? no no "25" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   = Temp-Tables.spec_mtrl.Enhet
     _FldNameList[5]   > Temp-Tables.spec_mtrl.NPRIS
"spec_mtrl.NPRIS" "Nettopris" ">>>>>9.99" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_MTRL */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME BRW_DEPA
&Scoped-define SELF-NAME BRW_DEPA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_DEPA WINDOW-1
ON ANY-KEY OF BRW_DEPA IN FRAME FRAME-A /* Materiellista för depå. */
DO:
   IF KEYFUNCTION(LASTKEY) = ("END-ERROR") THEN  RETURN NO-APPLY. 
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "CHOOSE" TO BTN_OVER.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_DEPA WINDOW-1
ON MOUSE-SELECT-DBLCLICK OF BRW_DEPA IN FRAME FRAME-A /* Materiellista för depå. */
DO:
   APPLY "CHOOSE" TO BTN_OVER.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_HLEV
&Scoped-define SELF-NAME BRW_HLEV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_HLEV WINDOW-1
ON ANY-KEY OF BRW_HLEV IN FRAME FRAME-A /* Materiellista huvudleverantör  */
DO:
   IF KEYFUNCTION(LASTKEY) = ("END-ERROR") THEN  RETURN NO-APPLY. 
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "CHOOSE" TO BTN_OVER.
   END.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_HLEV WINDOW-1
ON MOUSE-SELECT-DBLCLICK OF BRW_HLEV IN FRAME FRAME-A /* Materiellista huvudleverantör  */
DO:
   APPLY "CHOOSE" TO BTN_OVER.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_MTRL
&Scoped-define SELF-NAME BRW_MTRL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_MTRL WINDOW-1
ON ANY-KEY OF BRW_MTRL IN FRAME FRAME-A /* Vald materiel */
DO:
   IF KEYFUNCTION(LASTKEY) = ("END-ERROR") THEN  RETURN NO-APPLY. 
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "CHOOSE" TO BTN_BACK.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_MTRL WINDOW-1
ON VALUE-CHANGED OF BRW_MTRL IN FRAME FRAME-A /* Vald materiel */
DO:
  RUN koll_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB WINDOW-1
ON CHOOSE OF BTN_AVB IN FRAME FRAME-A /* Avbryt */
DO: 
   IF depatitta = FALSE THEN DO:
      MESSAGE "OBS! Vill du spara dina ändringar?"
      VIEW-AS ALERT-BOX
      QUESTION BUTTONS YES-NO-CANCEL TITLE "Avbryt?" UPDATE svar AS LOGICAL.         
      IF svar THEN DO:        
         APPLY "CHOOSE" TO BTN_BEST IN FRAME {&FRAME-NAME}.        
      END.
      ELSE IF NOT svar THEN DO:       
         APPLY "CLOSE":U TO THIS-PROCEDURE.
      END.
      ELSE DO:
         musz = musz.
      END.
   END.
   ELSE DO:
      APPLY "CLOSE":U TO THIS-PROCEDURE.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_back
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_back WINDOW-1
ON CHOOSE OF btn_back IN FRAME FRAME-A
DO:   
   antal_valda = BRW_MTRL:NUM-SELECTED-ROWS NO-ERROR.
   antal_raknare = 1.
   DO WHILE antal_raknare LE antal_valda:                                   
      status-ok = BRW_MTRL:FETCH-SELECTED-ROW(antal_raknare).                      
      DELETE spec_mtrl.
      antal_raknare = antal_raknare + 1.   
   END.           
   RUN refreshbrw_UI IN brwproc[5].
   RUN koll_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_BEST
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_BEST WINDOW-1
ON CHOOSE OF BTN_BEST IN FRAME FRAME-A /* Ok */
DO:
   {muswait.i}  
   RUN okbest_UI IN mtrlbapph (INPUT valaonr, INPUT valdelnr,INPUT vald_depa ,INPUT valnummer ,INPUT ny, INPUT valkund ,INPUT nytt_bestnr2 ,
                               INPUT Guru.Konstanter:globanv, INPUT TABLE spec_mtrl ,INPUT-OUTPUT TABLE best_nr_tab, INPUT TABLE off_mtrl ,INPUT TABLE skapa_mtrl).                                        
   
   IF ny = TRUE THEN DO:
      valnr = nytt_bestnr2.
   END.
   ELSE valnr = valnummer.
   APPLY "GO" TO FRAME {&FRAME-NAME}.    
   APPLY "CLOSE":U TO THIS-PROCEDURE.     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_LEV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_LEV WINDOW-1
ON CHOOSE OF BTN_LEV IN FRAME FRAME-A /* Huvudlev. */
DO:
   
   {muswait.i}
   vald_lev = vald_kundlev.         
   RUN setorgtitle_UI IN brwproc[2] (INPUT "Materiellista : " + huvudlev).         
   /*BRW_HLEV:TITLE = "Materiellista huvudleverantör: " + huvudlev.   */
   ASSIGN 
   FILL-IN-BEN:HIDDEN = FALSE 
   FILL-IN-ENR:HIDDEN = FALSE    
   BTN_LEV:HIDDEN = TRUE
   CMB_LEV:HIDDEN = FALSE
   BRW_HLEV:HIDDEN = FALSE 
   CMB_LEV:SCREEN-VALUE = huvudlev
   vald_lev = vald_kundlev.   
   {musarrow.i}
   IF posok NE " " THEN DO:
      APPLY "ENTRY" TO FILL-IN-ENR IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
   END.
   ELSE DO:
      APPLY "ENTRY" TO FILL-IN-BEN IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
   END.               
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_MIN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_MIN WINDOW-1
ON CHOOSE OF BTN_MIN IN FRAME FRAME-A /* - */
DO: 
   FILL-IN-ANTAL = INPUT FILL-IN-ANTAL.
   IF FILL-IN-ANTAL >= 2 THEN DO:
      FILL-IN-ANTAL = FILL-IN-ANTAL - 1.
   END.   
   ELSE DO:
      MESSAGE "Antal kan inte vara mindre än 1!" VIEW-AS ALERT-BOX TITLE "Meddelande".
   END.         
   DISPLAY FILL-IN-ANTAL WITH FRAME {&FRAME-NAME}. 
   APPLY "ENTRY" TO FILL-IN-ANTAL.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_over
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_over WINDOW-1
ON CHOOSE OF btn_over IN FRAME FRAME-A
DO:   
   RAD_DEPA = INPUT RAD_DEPA.
   IF RAD_DEPA = 1 THEN DO: 
      antal_valda = BRW_DEPA:NUM-SELECTED-ROWS NO-ERROR.         
      antal_raknare = 1.
      DO WHILE antal_raknare LE antal_valda:                                   
         status-ok = BRW_DEPA:FETCH-SELECTED-ROW(antal_raknare).
         ASSIGN antalvar = 1
         enrval = FALSE.
         RUN over3_UI.
      END. 
      RUN setcolsortvar_UI IN brwproc[5] (INPUT "").
      RUN openbdynspec_UI IN brwproc[5].      
      FIND FIRST spec_mtrl WHERE ROWID(spec_mtrl) = spec_rowid   NO-LOCK NO-ERROR.
      IF AVAILABLE spec_mtrl THEN DO:      
         RUN setlastrowid_UI IN brwproc[5] (INPUT ROWID(spec_mtrl)).
         RUN lastselectdyn_UI IN brwproc[5].   
      END.      
      RUN title_UI IN brwproc[5].
      status-ok = BRW_MTRL:SELECT-FOCUSED-ROW() NO-ERROR.
      status-ok = BRW_DEPA:DESELECT-ROWS() NO-ERROR.
   END.
   ELSE DO:      
      antal_valda = BRW_HLEV:NUM-SELECTED-ROWS NO-ERROR.         
      antal_raknare = 1.
      DO WHILE antal_raknare LE antal_valda:                                   
         status-ok = BRW_HLEV:FETCH-SELECTED-ROW(antal_raknare).
         ASSIGN antalvar = 1
         enrval = FALSE.
         RUN over_UI.
      END. 
      RUN setcolsortvar_UI IN brwproc[5] (INPUT "").
      RUN openbdynspec_UI IN brwproc[5].
      
      FIND FIRST spec_mtrl WHERE ROWID(spec_mtrl) = spec_rowid NO-LOCK NO-ERROR.
      IF AVAILABLE spec_mtrl THEN DO:      
         RUN setlastrowid_UI IN brwproc[5] (INPUT ROWID(spec_mtrl)).
         RUN lastselectdyn_UI IN brwproc[5].   
      END.            
      RUN title_UI IN brwproc[5].
      status-ok = BRW_MTRL:SELECT-FOCUSED-ROW() NO-ERROR.
      status-ok = BRW_HLEV:DESELECT-ROWS() NO-ERROR.
      RUN koll_UI.      
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_SPEC
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_SPEC WINDOW-1
ON CHOOSE OF BTN_SPEC IN FRAME FRAME-A /* Spec.mtrl */
DO:           
   {muswait.i}
   {AVBGOM.I}
   musz = FALSE.   
   RUN DEPSPECANV2.W (INPUT 3).
   {AVBFRAM.I}
   IF musz = FALSE THEN DO:      
      RUN setcolsortvar_UI IN brwproc[5] (INPUT "").
      RUN openbdynspec_UI IN brwproc[5].     
      FIND FIRST spec_mtrl WHERE ROWID(spec_mtrl) = mtrl_rowid NO-LOCK NO-ERROR.
      IF AVAILABLE spec_mtrl THEN DO:      
         RUN setlastrowid_UI IN brwproc[5] (INPUT mtrl_rowid).
         RUN lastselectdyn_UI IN brwproc[5].   
      END.
      RUN title_UI IN brwproc[5].
      RUN koll_UI.
   END.
   ELSE DO:
      musz = FALSE.
   END.   
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_UP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_UP WINDOW-1
ON CHOOSE OF BTN_UP IN FRAME FRAME-A /* + */
DO: 
   ASSIGN
   FILL-IN-ANTAL = INPUT FILL-IN-ANTAL
   FILL-IN-ANTAL = FILL-IN-ANTAL + 1.
   DISPLAY FILL-IN-ANTAL WITH FRAME {&FRAME-NAME}. 
   APPLY "ENTRY" TO FILL-IN-ANTAL.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_LEV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_LEV WINDOW-1
ON VALUE-CHANGED OF CMB_LEV IN FRAME FRAME-A /* Leverantörer */
DO: 
   {muswait.i}                                
   lev = INPUT CMB_LEV.
   IF lev NE huvudlev THEN DO:
      ASSIGN                            
      CMB_LEV:HIDDEN = TRUE      
      BTN_LEV:HIDDEN = FALSE.            
      /*Niklas personlig spec_mtrl*/
      IF lev BEGINS Guru.Konstanter:globanv THEN DO:            
         vald_lev = "99" + Guru.Konstanter:globanv. 
      END.
      ELSE DO:
         FIND FIRST levtemp WHERE levtemp.LEVNAMN = lev 
         USE-INDEX LEV NO-LOCK NO-ERROR.
         vald_lev = levtemp.LEVKOD. 
      END.      
      IF vald_lev BEGINS "99" THEN DO:        
         RUN initsok_UI (INPUT 2,INPUT "").
      END.          
      RUN setorgtitle_UI IN brwproc[2] (INPUT "Materiellista : " + lev).         
      /*BRW_HLEV:TITLE = "Materiellista vald leverantör: " + lev .   */         
   END.
   ELSE DO:
      vald_lev = vald_kundlev.
   END.
   {musarrow.i}      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_FOLJ
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_FOLJ WINDOW-1
ON CHOOSE OF FBTN_FOLJ IN FRAME FRAME-A /* Följesedel */
DO:    
   {muswait.i}    
   fakt = FALSE. 
   {AVBGOM.I}     
   foljesedel = TRUE.     
   RUN FOLJTRP2V.W.
   {AVBFRAM.I}
   {musarrow.i}  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_PRIS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_PRIS WINDOW-1
ON CHOOSE OF FBTN_PRIS IN FRAME FRAME-A /* Tot. pris */
DO:    
   {muswait.i}
   
   RUN OFFUT2V.W.
   
   {musarrow.i}  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_SKRIV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_SKRIV WINDOW-1
ON CHOOSE OF FBTN_SKRIV IN FRAME FRAME-A /* Skriv ut */
DO: 
   RUN SKRIVVAL.W (INPUT FALSE).       
   IF musz = TRUE THEN musz = FALSE. 
   ELSE DO:    
      {muswait.i}             
      skrivut = TRUE.
      {AVBGOM.I}
      RUN VISAUTV.W.
      {AVBFRAM.I}
   END.     
   {musarrow.i}  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_SKRIV WINDOW-1
ON MOUSE-MENU-CLICK OF FBTN_SKRIV IN FRAME FRAME-A /* Skriv ut */
DO:
   RUN SIDLANGD.W.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_VISA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_VISA WINDOW-1
ON CHOOSE OF FBTN_VISA IN FRAME FRAME-A /* Mtrl.spec */
DO:            
   {muswait.i}  
   {AVBGOM.I}
   foljesedel = FALSE.
   RUN FOLJTRP2V.W.
   {AVBFRAM.I}
   {musarrow.i}   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_VISA2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_VISA2 WINDOW-1
ON CHOOSE OF FBTN_VISA2 IN FRAME FRAME-A /* Visa */
DO:            
   {muswait.i}
   IF musz = TRUE THEN musz = FALSE.   
   ASSIGN    
   skrivut = FALSE.   
   {AVBGOM.I}
   RUN VISAUTV.W.
   {AVBFRAM.I}
   {musarrow.i}   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-ANTAL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-ANTAL WINDOW-1
ON ANY-KEY OF FILL-IN-ANTAL IN FRAME FRAME-A /* Antal */
DO:
   
   ASSIGN  
   FILL-IN-ENR2 = INPUT FILL-IN-ENR2
   FILL-IN-ANTAL = INPUT FILL-IN-ANTAL.   
   {TRYCKS.I}
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      RUN enrval_UI.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-BEN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-BEN WINDOW-1
ON ANY-KEY OF FILL-IN-BEN IN FRAME FRAME-A /* Benämning */
DO:
   {TRYCKS.I}
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN-BEN IN FRAME {&FRAME-NAME}.
   END.
   IF KEYFUNCTION(LASTKEY) = ("+") THEN DO:
      APPLY "CHOOSE" TO BTN_OVER.
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-BEN WINDOW-1
ON MOUSE-SELECT-DBLCLICK OF FILL-IN-BEN IN FRAME FRAME-A /* Benämning */
DO:   
   {muswait.i}
   ASSIGN
   FILL-IN-BEN = INPUT FILL-IN-BEN
   RAD_DEPA = INPUT RAD_DEPA.   
   IF INDEX(FILL-IN-BEN,"+") NE 0 THEN DO:
      FILL-IN-BEN = TRIM(FILL-IN-BEN,"+").
      DISP FILL-IN-BEN WITH FRAME {&FRAME-NAME}.
   END.
   IF RAD_DEPA = 1 THEN DO:
      IF AVAILABLE mtrldeptemp THEN mtrl_rowid = ROWID(mtrldeptemp).
      IF FILL-IN-BEN = '' THEN DO:
         MESSAGE "Sökbegreppet kan inte vara blankt." VIEW-AS ALERT-BOX TITLE "Meddelande".
         APPLY "ENTRY" TO FILL-IN-BEN IN FRAME {&FRAME-NAME}.
         RETURN NO-APPLY.
      END.      
      aosok = '*' + FILL-IN-BEN + '*'.    
      status-ok = BRW_DEPA:DESELECT-ROWS() NO-ERROR.    
      FIND mtrldeptemp WHERE ROWID(mtrldeptemp) = mtrl_rowid NO-LOCK NO-ERROR.      
      FIND NEXT mtrldeptemp WHERE mtrldeptemp.BENAMNING MATCHES aosok AND 
      mtrldeptemp.DEPNR = vald_depa AND mtrldeptemp.IBDATUM = ?
      USE-INDEX ENR NO-LOCK NO-ERROR. 
      IF NOT AVAILABLE mtrldeptemp THEN DO:      
         FIND FIRST mtrldeptemp WHERE mtrldeptemp.BENAMNING MATCHES aosok AND 
         mtrldeptemp.DEPNR = vald_depa AND mtrldeptemp.IBDATUM = ?
         USE-INDEX ENR NO-LOCK NO-ERROR.   
         IF NOT AVAILABLE mtrldeptemp THEN DO: 
            MESSAGE "Det finns inget på sökbegreppet i depån. Välj total materiel och utför ny sökning."
            VIEW-AS ALERT-BOX TITLE "Meddelande". 
         END.
      END.            
      IF AVAILABLE mtrldeptemp THEN DO:               
         RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(mtrldeptemp)).
         RUN lastselectdyn_UI IN brwproc[1].                 
      END.      
   END.
   ELSE DO:
      {BENHMT2.I}      
      RUN initsok_UI (INPUT 1,INPUT aosok).
      /*ELSE DO:
         
         IF CMB_LEV = huvudlev THEN DO:
            RUN openbdyn_UI IN brwproc[2] (INPUT " WHERE mtrltemp.KALKNR = 0 AND mtrltemp.LEVKOD = '" + STRING(vald_kundlev) + "'").
            FIND FIRST mtrltemp NO-LOCK NO-ERROR.
            IF AVAILABLE mtrltemp THEN DO:
               RUN setlastrowid_UI IN brwproc[2] (INPUT ROWID(mtrltemp)).
               RUN lastselectdyn_UI IN brwproc[2].
            END.
         END.
         ELSE DO:
            RUN openbdyn_UI IN brwproc[2] (INPUT " WHERE mtrltemp.LEVKOD = '" + STRING(vald_lev) + "' AND mtrltemp.KALKNR = 0 ").
            FIND FIRST mtrltemp NO-LOCK NO-ERROR.
            IF AVAILABLE mtrltemp THEN DO:   
               RUN setlastrowid_UI IN brwproc[2] (INPUT ROWID(mtrltemp)).
               RUN lastselectdyn_UI IN brwproc[2].
            END.      
         END.               
      END.      */
   END.   
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
   IF KEYFUNCTION(LASTKEY) = ("+") THEN DO:
      APPLY "CHOOSE" TO BTN_OVER.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-ENR WINDOW-1
ON MOUSE-SELECT-DBLCLICK OF FILL-IN-ENR IN FRAME FRAME-A /* Enr */
DO:   
   {muswait.i} 
   ASSIGN
   FILL-IN-ENR = INPUT FILL-IN-ENR.
   RAD_DEPA = INPUT RAD_DEPA.
   IF INDEX(FILL-IN-ENR,"+") NE 0 THEN DO:
      FILL-IN-ENR = TRIM(FILL-IN-ENR,"+").
      DISP FILL-IN-ENR WITH FRAME {&FRAME-NAME}.
   END.
   IF RAD_DEPA = 1 THEN DO:
      IF AVAILABLE mtrldeptemp THEN mtrl_rowid = ROWID(mtrldeptemp).
      IF FILL-IN-ENR = '' THEN DO:
         MESSAGE "Sökbegreppet kan inte vara blankt." VIEW-AS ALERT-BOX TITLE "Meddelande".
         APPLY "ENTRY" TO FILL-IN-ENR IN FRAME {&FRAME-NAME}.
         RETURN NO-APPLY.
      END.      
      posok = '*' + FILL-IN-ENR + '*'.    
      status-ok = BRW_DEPA:DESELECT-ROWS() NO-ERROR.    
      FIND mtrldeptemp WHERE ROWID(mtrldeptemp) = mtrl_rowid NO-LOCK NO-ERROR.                  
      FIND NEXT mtrldeptemp WHERE mtrldeptemp.ENR MATCHES posok AND mtrldeptemp.DEPNR = vald_depa
      AND mtrldeptemp.IBDATUM = ? USE-INDEX ENR NO-LOCK NO-ERROR. 
      IF NOT AVAILABLE mtrldeptemp THEN DO:      
         FIND FIRST mtrldeptemp WHERE mtrldeptemp.ENR MATCHES posok AND 
         mtrldeptemp.DEPNR = vald_depa AND mtrldeptemp.IBDATUM = ? USE-INDEX ENR NO-LOCK NO-ERROR.   
         IF NOT AVAILABLE mtrldeptemp THEN DO: 
            MESSAGE "Det finns inget på sökbegreppet i depån. Välj total materiel och utför ny sökning."
            VIEW-AS ALERT-BOX TITLE "Meddelande". 
         END.
      END.    
      IF AVAILABLE mtrldeptemp THEN DO:                        
         RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(mtrldeptemp)).
         RUN lastselectdyn_UI IN brwproc[1].        
      END.      
   END.        
   ELSE DO:
      {ENRHMT2.I}
      RUN initsok_UI (INPUT 2,INPUT posok).
      /*ELSE DO:
         RUN setcolsortvar_UI IN brwproc[2] (INPUT " WHERE mtrltemp.KALKNR = 0 AND mtrltemp.LEVKOD = '" + STRING(vald_lev) + "' USE-INDEX ENR ").
         RUN openbdynspec_UI IN brwproc[2].
         FIND FIRST mtrltemp NO-LOCK NO-ERROR.
         IF AVAILABLE mtrltemp THEN DO:      
            RUN setlastrowid_UI IN brwproc[2] (INPUT ROWID(mtrltemp)).
            RUN lastselectdyn_UI IN brwproc[2].
         END.      
      END.*/
   END.   
   {musarrow.i}                   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-ENR2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-ENR2 WINDOW-1
ON ANY-KEY OF FILL-IN-ENR2 IN FRAME FRAME-A /* Enr */
DO:
   
   ASSIGN  
   FILL-IN-ENR2 = INPUT FILL-IN-ENR2
   FILL-IN-ANTAL = INPUT FILL-IN-ANTAL.
   {TRYCKS.I}
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      RUN enrval_UI.
   END.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RAD_DEPA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RAD_DEPA WINDOW-1
ON VALUE-CHANGED OF RAD_DEPA IN FRAME FRAME-A
DO:
   RAD_DEPA = INPUT RAD_DEPA.
   IF RAD_DEPA = 1 THEN DO:   
      ASSIGN
      BRW_DEPA:HIDDEN = FALSE
      BRW_HLEV:HIDDEN = TRUE      
      CMB_LEV:HIDDEN = TRUE
      BTN_LEV:HIDDEN = TRUE   
      FILL-IN-ENR:HIDDEN = FALSE
      FILL-IN-BEN:HIDDEN = FALSE
      RAD_SOK:HIDDEN = TRUE.   
      RUN setcolsortvar_UI IN brwproc[1] (INPUT "mtrldeptemp.DEPNR =  " + STRING(vald_depa) +  " AND mtrldeptemp.IBDATUM = ? ").
      RUN openbdynspec_UI IN brwproc[1]. 
      RUN title_UI IN brwproc[1].
   END.
   IF RAD_DEPA = 2 THEN DO:
      RUN changebrwh_UI IN brwproc[6] 
         (INPUT BRW_HLEV:HANDLE IN FRAME {&FRAME-NAME}, INPUT BRW_MTRL:HANDLE IN FRAME {&FRAME-NAME}).      
      ASSIGN
      BRW_DEPA:HIDDEN = TRUE
      BRW_HLEV:HIDDEN = FALSE            
      CMB_LEV:HIDDEN = FALSE
      BTN_LEV:HIDDEN = TRUE      
      FILL-IN-ENR:HIDDEN = FALSE
      FILL-IN-BEN:HIDDEN = FALSE       
      CMB_LEV:SCREEN-VALUE = huvudlev
      RAD_SOK:HIDDEN = FALSE            
      vald_lev = vald_kundlev.      
      EMPTY TEMP-TABLE sok_mtrl NO-ERROR. 
      
      /*RUN setcolsortvar_UI IN brwproc[2] (INPUT "mtrltemp.KALKNR = 0 AND  mtrltemp.LEVKOD = '" + STRING(vald_lev) + "'"). */
      RUN openbdynspec_UI IN brwproc[2].         
      
   END.            
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_DEPA
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
   RUN mtrlbhmt_UI IN mtrlbapph (INPUT vald_depa,OUTPUT TABLE mtrldeptemp,OUTPUT vald_kundlev, OUTPUT dbenamning,OUTPUT bnummer).                
   ASSIGN vald_lev = vald_kundlev.
   status-ok = CMB_LEV:DELETE("0"). 
   FIND FIRST levtemp WHERE levtemp.LEVKOD = vald_kundlev
   USE-INDEX LEV NO-LOCK NO-ERROR. 
   ASSIGN
   status-ok = CMB_LEV:ADD-LAST(levtemp.LEVNAMN)IN FRAME {&FRAME-NAME}
   CMB_LEV:SCREEN-VALUE = levtemp.LEVNAMN.
   ASSIGN
   Guru.SharedVariable:ValdmtrlLeverantor = levtemp.LEVKOD
   Guru.SharedVariable:ValdmtrlLeverantorName = levtemp.LEVNAMN. 
   RUN setorgtitle_UI IN brwproc[2] (INPUT "Materiellista : " + huvudlev).         
   FOR EACH levtemp WHERE levtemp.LEVKOD NE vald_kundlev AND 
   levtemp.LEVKOD NE "0" AND levtemp.BORTTAG = FALSE USE-INDEX LEV NO-LOCK:      
      /*Niklas personlig spec_mtrl*/
      IF levtemp.LEVKOD = "99" THEN DO:
         status-ok = CMB_LEV:ADD-LAST(Guru.Konstanter:globanv + " " + levtemp.LEVNAMN)IN FRAME {&FRAME-NAME}.
         status-ok = CMB_LEV:ADD-LAST(levtemp.LEVNAMN)IN FRAME {&FRAME-NAME}.
      END.
      ELSE DO:
         status-ok = CMB_LEV:ADD-LAST(levtemp.LEVNAMN)IN FRAME {&FRAME-NAME}.
      END.         
   END.
   ASSIGN
   FILL-IN-ENR:LABEL = Guru.Konstanter:genk
   mtrltemp.ENR:LABEL IN BROWSE BRW_HLEV = Guru.Konstanter:genk    
   SPEC_MTRL.ENR:LABEL IN BROWSE BRW_MTRL = Guru.Konstanter:genk.                 
   ASSIGN             
   CMB_LEV = INPUT CMB_LEV   
   vald_lev = vald_kundlev
   bestant = FALSE
   musz = FALSE
   RAD_DEPA = 2.
   
   EMPTY TEMP-TABLE spec_mtrl NO-ERROR.    
   FIND FIRST depatemp WHERE depatemp.DEP-NR = vald_depa USE-INDEX DEP-NR NO-LOCK NO-ERROR.       
   RUN mtrlbbhmt_UI IN mtrlbapph (INPUT valaonr,INPUT valdelnr,INPUT vald_depa,INPUT valnummer,INPUT ny, OUTPUT nytt_bestnr2, OUTPUT TABLE spec_mtrl,OUTPUT TABLE off_mtrl ,OUTPUT TABLE gam_mtrl).             
   RUN setcolsortvar_UI IN brwproc[1] (INPUT "mtrldeptemp.DEPNR =  " + STRING(vald_depa) +  " AND mtrldeptemp.IBDATUM = ? ").
   RUN openbdynspec_UI IN brwproc[1]. 
   RUN title_UI IN brwproc[1].
   RUN setcolsortvar_UI IN brwproc[5] (INPUT "").
   RUN openbdynspec_UI IN brwproc[5].
   RUN title_UI IN brwproc[5].
   IF ny = TRUE THEN DO:      
      {&WINDOW-NAME}:TITLE = "Uttag till " + CAPS(Guru.Konstanter:gaol) + " " + STRING(valaonr) + " DELNR: " + 
      STRING(valdelnr,Guru.Konstanter:varforetypchar[1]) + CAPS(Guru.Konstanter:gaonamnk) + ": " + valort 
      + " från depå - " + depatemp.BENAMNING.         
   END.
   /*BRW_HLEV:TITLE = "Materiellista huvudleverantör: " + huvudlev.*/
   RUN enable_UI.      
   {FRMSIZE.I}       
   /*FBTN_SKRIV:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.   */
   /*RUN setcolsortvar_UI IN brwproc[2] (INPUT "mtrltemp.KALKNR = 0 AND  mtrltemp.LEVKOD = '" + STRING(vald_kundlev) + "'").*/
   RUN openbdynspec_UI IN brwproc[2].         
   ASSIGN
   BRW_DEPA:HIDDEN = TRUE
   BRW_HLEV:HIDDEN = FALSE         
   BTN_LEV:HIDDEN = TRUE.
   IF depatitta = TRUE THEN DO:
      DISABLE BTN_BEST WITH FRAME {&FRAME-NAME}.
      DISABLE FBTN_PRIS WITH FRAME {&FRAME-NAME}.
   END.
   IF Guru.Konstanter:mtrlsekvar[6] = TRUE THEN DO:
      ASSIGN mtrltemp.NPRIS:VISIBLE IN BROWSE BRW_HLEV = FALSE.
      ASSIGN mtrldeptemp.NPRIS:VISIBLE IN BROWSE BRW_DEPA = FALSE.
      ASSIGN spec_mtrl.NPRIS:VISIBLE IN BROWSE BRW_MTRL = FALSE.
      FBTN_PRIS:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
   END.
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
      (INPUT BRW_DEPA:HANDLE IN FRAME {&FRAME-NAME}).
   RUN StartSokEnrLev_UI IN brwproc[1] (INPUT THIS-PROCEDURE).   
   /*RUN DYNBRW.P PERSISTENT SET brwproc[2]
      (INPUT BRW_HLEV:HANDLE IN FRAME {&FRAME-NAME}).   */
   RUN DYNBRW.P PERSISTENT SET brwproc[2]
         (INPUT BRW_HLEV:HANDLE IN FRAME {&FRAME-NAME}).
   RUN StartSokEnrLev_UI IN brwproc[2] (INPUT THIS-PROCEDURE).             
   RUN DYNBRW.P PERSISTENT SET brwproc[5]
      (INPUT BRW_MTRL:HANDLE IN FRAME {&FRAME-NAME}).
   RUN StartSokEnrLev_UI IN brwproc[5] (INPUT THIS-PROCEDURE).            
   RUN DYNARROW.P PERSISTENT SET brwproc[6] 
      (INPUT BRW_DEPA:HANDLE, INPUT BRW_MTRL:HANDLE ,
       INPUT ?, INPUT ?,
       INPUT ? , INPUT ?).
   RUN setcolindex_UI IN brwproc[5] (INPUT "ENR").
   IF Guru.Konstanter:appcon THEN DO:
      RUN MTRLBAPP.P PERSISTENT SET mtrlbapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN MTRLBAPP.P PERSISTENT SET mtrlbapph.
   END.         
   RUN setcolsortvar_UI IN brwproc[2] (INPUT " KALKNR = 0 AND LEVKOD = '" + STRING(vald_lev) + "'").
   RUN setdefaultcol_UI IN brwproc[1] (INPUT 2).
   RUN colsortdynbrw_UI IN brwproc[1] (INPUT "").   
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
  DISPLAY RAD_DEPA CMB_LEV FILL-IN-ENR FILL-IN-BEN FILL-IN-ENR2 FILL-IN-ANTAL 
          RAD_SOK 
      WITH FRAME FRAME-A IN WINDOW WINDOW-1.
  ENABLE BRW_DEPA BRW_MTRL RAD_DEPA BRW_HLEV FBTN_PRIS btn_over FBTN_FOLJ 
         btn_back FBTN_VISA BTN_BEST BTN_AVB BTN_SPEC BTN_LEV CMB_LEV 
         FBTN_SKRIV FILL-IN-ENR FILL-IN-BEN FILL-IN-ENR2 FILL-IN-ANTAL BTN_UP 
         BTN_MIN RAD_SOK FBTN_VISA2 RECT-2 RECT-4 
      WITH FRAME FRAME-A IN WINDOW WINDOW-1.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enrval_UI WINDOW-1 
PROCEDURE enrval_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  FIND FIRST mtrltemp WHERE mtrltemp.ENR = FILL-IN-ENR2 AND mtrltemp.LEVKOD = vald_lev
  NO-LOCK NO-ERROR.
  IF AVAILABLE mtrltemp THEN DO:
     ASSIGN
     antalvar = FILL-IN-ANTAL
     enrval = TRUE.
     RUN over_UI.
  END.  
  ELSE DO:
     EMPTY TEMP-TABLE emtrltemp NO-ERROR.     
     RUN hmtskap_UI IN mtrlbapph (INPUT FILL-IN-ENR2,INPUT vald_lev,OUTPUT TABLE emtrltemp).      
     FIND FIRST emtrltemp NO-LOCK NO-ERROR.
     IF AVAILABLE emtrltemp THEN DO:
        ASSIGN
        antalvar = FILL-IN-ANTAL
        enrval = TRUE.
        RUN over4_UI.        
      END.
      ELSE DO:      
         FIND FIRST levtemp WHERE levtemp.LEVKOD =  vald_lev NO-LOCK NO-ERROR.
         MESSAGE "Det finns ingen materiel med E-nummer:" + FILL-IN-ENR2 + "hos leverantör:" + STRING(levtemp.LEVNAMN) VIEW-AS ALERT-BOX
         TITLE "Meddelande".    
         RETURN.
      END.
  END.
  RUN openbdynspec_UI IN brwproc[5].  
  RUN setlastrowid_UI IN brwproc[5] (INPUT spec_rowid).
  RUN lastselectdyn_UI IN brwproc[5].
  RUN title_UI IN brwproc[5].
  status-ok = BRW_MTRL:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE infoes_UI WINDOW-1 
PROCEDURE infoes_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER valenr AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER brwname AS CHARACTER NO-UNDO.
   DEFINE VARIABLE valbrw AS INTEGER NO-UNDO.  
   DEFINE VARIABLE levnamnvar AS CHARACTER NO-UNDO.
   IF brwname = "BRW_HLEV" THEN valbrw = 1.  
   IF brwname = "BRW_MTRL" THEN valbrw = 2. 
   IF brwname = "BRW_DEPA" THEN valbrw = 3. 
   IF valbrw = 1 THEN DO:
      status-ok = BRW_HLEV:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}  NO-ERROR.
      IF status-ok THEN DO:
         valenr = mtrltemp.ENR.
         FIND FIRST levtemp WHERE levtemp.LEVKOD = mtrltemp.LEVKOD
         NO-LOCK NO-ERROR.      
      END.
   END.
   IF valbrw = 2 THEN DO:
      status-ok = BRW_MTRL:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}  NO-ERROR.
      IF status-ok THEN DO:
         valenr = spec_mtrl.ENR.
         FIND FIRST levtemp WHERE levtemp.LEVKOD = spec_mtrl.LEVKOD
         NO-LOCK NO-ERROR.      
      END.
   END.
   IF valbrw = 3 THEN DO:
      status-ok = BRW_DEPA:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}  NO-ERROR.
      IF status-ok THEN DO:
         valenr = mtrldeptemp.ENR.
         FIND FIRST levtemp WHERE levtemp.LEVKOD = mtrldeptemp.LEVKOD
         NO-LOCK NO-ERROR.      
      END.
   END.
   IF status-ok THEN DO:
      levnamnvar = levtemp.LEVNAMN.
      {LEVLANK.I}      
   END.   
END PROCEDURE.


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
   IF vad = 1 THEN DO:
      orgfraga = " WHERE KALKNR = " + STRING(0) + " AND LEVKOD = '" + STRING(vald_lev) + "' USE-INDEX LEV ".
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
   IF vad = 2 THEN DO:
     orgfraga = " WHERE KALKNR = " + STRING(0) + " AND LEVKOD = '" + STRING(vald_lev) + "' USE-INDEX LEV ".
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
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE koll_UI WINDOW-1 
PROCEDURE koll_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/      
   FIND FIRST off_mtrl NO-LOCK NO-ERROR.
   IF AVAILABLE off_mtrl THEN DO:
      MESSAGE "OBS! Tot.pris har tagits bort då Ni gjort förändringar i uttaget."
      VIEW-AS ALERT-BOX TITLE "Meddelande".
      DELETE off_mtrl.      
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE over2_UI WINDOW-1 
PROCEDURE over2_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   FIND FIRST spec_mtrl WHERE spec_mtrl.ENR = sok_mtrl.ENR AND 
   spec_mtrl.LEVKOD = sok_mtrl.LEVKOD
   NO-LOCK NO-ERROR. 
   IF NOT AVAILABLE spec_mtrl THEN DO:
      CREATE spec_mtrl. 
      ASSIGN
      spec_mtrl.ENR = sok_mtrl.ENR
      spec_mtrl.BENAMNING = sok_mtrl.BENAMNING
      spec_mtrl.ENHET = sok_mtrl.ENHET
      spec_mtrl.SALDO = antalvar
      spec_mtrl.NPRIS = sok_mtrl.NPRIS       
      spec_mtrl.BPRIS = sok_mtrl.NPRIS
      spec_mtrl.LEVKOD = sok_mtrl.LEVKOD      
      spec_rowid = ROWID(spec_mtrl).
   END.    
   antal_raknare = antal_raknare + 1.             
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE over3_UI WINDOW-1 
PROCEDURE over3_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/  
   FIND FIRST levtemp WHERE levtemp.LEVKOD = mtrldeptemp.LEVKOD 
   NO-LOCK NO-ERROR.
   IF levtemp.BORTTAG = FALSE THEN DO:     
      FIND FIRST spec_mtrl WHERE spec_mtrl.ENR = mtrldeptemp.ENR AND 
      spec_mtrl.LEVKOD = mtrldeptemp.LEVKOD NO-LOCK NO-ERROR. 
      IF AVAILABLE spec_mtrl THEN DO:
         ASSIGN
         spec_rowid = ROWID(spec_mtrl).
         IF enrval = TRUE THEN DO:
            ASSIGN
            spec_mtrl.SALDO = antalvar
            spec_rowid = ROWID(spec_mtrl).               
         END.      
      END.      
      ELSE DO:
         CREATE spec_mtrl. 
         ASSIGN
         spec_mtrl.ENR = mtrldeptemp.ENR
         spec_mtrl.BENAMNING = mtrldeptemp.BENAMNING
         spec_mtrl.ENHET = mtrldeptemp.ENHET
         spec_mtrl.SALDO = antalvar
         spec_mtrl.NPRIS = mtrldeptemp.NPRIS 
         spec_mtrl.BPRIS = mtrldeptemp.NPRIS
         spec_mtrl.LEVKOD = mtrldeptemp.LEVKOD                  
         spec_rowid = ROWID(spec_mtrl).
      END. 
   END.
   ELSE DO:      
      MESSAGE "Leverantör " + levtemp.LEVNAM + " är borttagen. Det går ej att göra beställning av E-nr " + mtrldeptemp.ENR + " med benämning " + mtrldeptemp.BENAMNING
      VIEW-AS ALERT-BOX TITLE "Meddelande".   
   END.   
   antal_raknare = antal_raknare + 1.                           
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE over4_UI WINDOW-1 
PROCEDURE over4_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/    
     /*Niklas personlig spec_mtrl*/
   IF emtrltemp.LEVKOD BEGINS "99" THEN DO:
      FIND FIRST spec_mtrl WHERE spec_mtrl.ENR = emtrltemp.ENR AND spec_mtrl.LEVKOD = "99" 
      NO-LOCK NO-ERROR. 
   END.
   ELSE DO:
      FIND FIRST spec_mtrl WHERE spec_mtrl.ENR = emtrltemp.ENR AND spec_mtrl.LEVKOD = emtrltemp.LEVKOD 
      NO-LOCK NO-ERROR. 
   END.          
   IF NOT AVAILABLE spec_mtrl THEN DO:
      CREATE spec_mtrl. 
      ASSIGN
      spec_mtrl.ENR = emtrltemp.ENR
      spec_mtrl.BENAMNING = emtrltemp.BENAMNING
      spec_mtrl.ENHET = emtrltemp.ENHET
      spec_mtrl.SALDO = antalvar
      spec_mtrl.NPRIS = emtrltemp.NPRIS 
      spec_mtrl.BPRIS = emtrltemp.NPRIS            
      spec_rowid = ROWID(spec_mtrl).
       /*Niklas personlig spec_mtrl*/
      IF emtrltemp.LEVKOD BEGINS "99" THEN spec_mtrl.LEVKOD = "99".           
      ELSE spec_mtrl.LEVKOD = emtrltemp.LEVKOD. 
   END. 
   antal_raknare = antal_raknare + 1.                     
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE over_UI WINDOW-1 
PROCEDURE over_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/  
   /*Niklas personlig spec_mtrl*/
   IF mtrltemp.LEVKOD BEGINS "99" THEN DO:
      FIND FIRST spec_mtrl WHERE spec_mtrl.ENR = mtrltemp.ENR AND spec_mtrl.LEVKOD = "99" 
      NO-LOCK NO-ERROR. 
   END.
   ELSE DO:
      FIND FIRST spec_mtrl WHERE spec_mtrl.ENR = mtrltemp.ENR AND spec_mtrl.LEVKOD = mtrltemp.LEVKOD 
      NO-LOCK NO-ERROR. 
   END.        
   IF NOT AVAILABLE spec_mtrl THEN DO:
      CREATE spec_mtrl. 
      ASSIGN
      spec_mtrl.ENR = mtrltemp.ENR
      spec_mtrl.BENAMNING = mtrltemp.BENAMNING
      spec_mtrl.ENHET = mtrltemp.ENHET
      spec_mtrl.SALDO = antalvar
      spec_mtrl.NPRIS = mtrltemp.NPRIS 
      spec_mtrl.BPRIS = mtrltemp.NPRIS            
      spec_rowid = ROWID(spec_mtrl).
      /*Niklas personlig spec_mtrl*/
      IF mtrltemp.LEVKOD BEGINS "99" THEN spec_mtrl.LEVKOD = "99".           
      ELSE spec_mtrl.LEVKOD = mtrltemp.LEVKOD. 
   END. 
   antal_raknare = antal_raknare + 1.                     
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

