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
/* Local Variable Definitions ---                                       */
{ALLDEF.I}
   
{GLOBVAR2DEL1.I}
&Scoped-define NEW NEW
&Scoped-define SHARED SHARED
{MTRLTEMP.I}
&Scoped-define NEW 
&Scoped-define SHARED SHARED
{SPECMTRLTEMP.I}
{LEVTEMP.I}
{SOKDEF.I}
{HOPPSEK2W.I}
DEFINE NEW SHARED TEMP-TABLE aspec_mtrl NO-UNDO LIKE spec_mtrl
   FIELD NY AS LOGICAL
   FIELD ANDRAD AS LOGICAL
   FIELD BORT AS LOGICAL.   

DEFINE NEW SHARED TEMP-TABLE pspec_mtrl NO-UNDO LIKE spec_mtrl
   FIELD ANVANDARE AS CHARACTER
   FIELD NYPRIS AS DECIMAL
   FIELD TIDPUNKT AS DECIMAL
   FIELD DATUM AS DATE. 

DEFINE NEW SHARED VARIABLE reprowid AS ROWID NO-UNDO.
DEFINE NEW SHARED VARIABLE mtrl_rowid AS ROWID NO-UNDO.
/*DEFINE NEW SHARED VARIABLE tth AS HANDLE NO-UNDO.*/
DEFINE SHARED VARIABLE depatitta AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE dbenamning AS CHARACTER NO-UNDO.  
DEFINE SHARED VARIABLE bestant AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE huvudlev AS CHARACTER FORMAT "X(25)" NO-UNDO.  
DEFINE SHARED VARIABLE vald_kundlev AS CHARACTER NO-UNDO.  
DEFINE SHARED VARIABLE vald_lev  AS CHARACTER NO-UNDO.  
DEFINE SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.  
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO. 
DEFINE SHARED VARIABLE vartpro AS CHARACTER FORMAT "X(3)" NO-UNDO.   
DEFINE SHARED VARIABLE vald_depa AS INTEGER NO-UNDO.

DEFINE VARIABLE mtrlbapph AS HANDLE NO-UNDO.
DEFINE VARIABLE nytt_bestnr AS INTEGER NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE aosok AS CHARACTER FORMAT "X(40)" NO-UNDO.
DEFINE VARIABLE posok AS CHARACTER FORMAT "X(11)" NO-UNDO.
DEFINE VARIABLE antal_valda AS INTEGER NO-UNDO.
DEFINE VARIABLE antal_raknare AS INTEGER NO-UNDO. 
DEFINE VARIABLE lev AS CHARACTER NO-UNDO.  
DEFINE VARIABLE invdat AS DATE NO-UNDO.
DEFINE VARIABLE svar AS LOGICAL NO-UNDO. 
DEFINE VARIABLE str AS CHARACTER FORMAT "X(96)" NO-UNDO.
DEFINE VARIABLE sparsaldo AS INTEGER NO-UNDO. 
DEFINE VARIABLE fackvar AS CHARACTER NO-UNDO.  
DEFINE VARIABLE var1 AS CHARACTER FORMAT "X(6)" NO-UNDO.
DEFINE VARIABLE var2 AS CHARACTER FORMAT "X(6)" NO-UNDO.   
DEFINE VARIABLE fackid AS CHARACTER FORMAT "X(8)" NO-UNDO.
DEFINE VARIABLE langd AS INTEGER NO-UNDO. 
DEFINE VARIABLE colnamevar AS CHARACTER NO-UNDO.
DEFINE VARIABLE ascdesclog AS LOGICAL NO-UNDO.
/*DEFINE VARIABLE laddabrwproch AS HANDLE NO-UNDO.
DEFINE VARIABLE ttmtrlhandle AS HANDLE NO-UNDO.*/
DEFINE VARIABLE felmedd AS CHARACTER NO-UNDO.
/*DEFINE VARIABLE brwsok AS CHARACTER NO-UNDO.
DEFINE VARIABLE multitid AS INTEGER NO-UNDO.  */
/*DEFINE VARIABLE multibrwsok AS CHARACTER NO-UNDO.*/
DEFINE VARIABLE anyprintquery AS CHARACTER NO-UNDO.
/*DEFINE VARIABLE orgfraga AS CHARACTER NO-UNDO.*/
DEFINE VARIABLE numrows AS INTEGER NO-UNDO.
DEFINE VARIABLE bkomm AS CHARACTER NO-UNDO.
DEFINE VARIABLE nettoh AS HANDLE NO-UNDO.
{TIDUTTTNEW.I}  


/*{EGENBEN.I}*/
{DEFSOK.I}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A
&Scoped-define BROWSE-NAME BRW_HLEV

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES mtrltemp spec_mtrl

/* Definitions for BROWSE BRW_HLEV                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_HLEV mtrltemp.Enr mtrltemp.Benamning ~
mtrltemp.Enhet mtrltemp.NPRIS 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_HLEV 
&Scoped-define QUERY-STRING-BRW_HLEV FOR EACH mtrltemp ~
      WHERE mtrltemp.LEVKOD = vald_kundlev ~
 AND mtrltemp.KALKNR = 0 NO-LOCK ~
    BY mtrltemp.Enr
&Scoped-define OPEN-QUERY-BRW_HLEV OPEN QUERY BRW_HLEV FOR EACH mtrltemp ~
      WHERE mtrltemp.LEVKOD = vald_kundlev ~
 AND mtrltemp.KALKNR = 0 NO-LOCK ~
    BY mtrltemp.Enr.
&Scoped-define TABLES-IN-QUERY-BRW_HLEV mtrltemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_HLEV mtrltemp


/* Definitions for BROWSE BRW_MTRL                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_MTRL spec_mtrl.FACKID spec_mtrl.ENR ~
spec_mtrl.BENAMNING spec_mtrl.SALDO spec_mtrl.ENHET spec_mtrl.BESTKVANT ~
spec_mtrl.BESTPUNKT spec_mtrl.OMSATT spec_mtrl.LEVKOD 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_MTRL spec_mtrl.BENAMNING ~
spec_mtrl.ENHET spec_mtrl.BESTKVANT spec_mtrl.BESTPUNKT spec_mtrl.OMSATT 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_MTRL spec_mtrl
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_MTRL spec_mtrl
&Scoped-define QUERY-STRING-BRW_MTRL FOR EACH spec_mtrl ~
      WHERE spec_mtrl.DEPNR = vald_depa NO-LOCK ~
    BY spec_mtrl.ENR
&Scoped-define OPEN-QUERY-BRW_MTRL OPEN QUERY BRW_MTRL FOR EACH spec_mtrl ~
      WHERE spec_mtrl.DEPNR = vald_depa NO-LOCK ~
    BY spec_mtrl.ENR.
&Scoped-define TABLES-IN-QUERY-BRW_MTRL spec_mtrl
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_MTRL spec_mtrl


/* Definitions for FRAME FRAME-A                                        */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RAD_LAGER RECT-2 RECT-4 IMAGE-13 IMAGE-14 ~
BRW_HLEV BRW_MTRL BTN_OVER BTN_BACK CMB_LEV BTN_LEV BTN_UPPANT BTN_SPEC ~
FBTN_BYT FBTN_VISA FILL-IN-ENR FILL-IN-DENR FILL-IN-DBEN FILL-IN-BEN ~
FILL-IN-FACKID1 FILL-IN-FACK FILL-IN-FACKID2 RAD_SOK FBTN_BEST BTN_AVB ~
FILL-IN-SOKALT 
&Scoped-Define DISPLAYED-OBJECTS RAD_LAGER CMB_LEV FILL-IN-ENR FILL-IN-DENR ~
FILL-IN-DBEN FILL-IN-BEN FILL-IN-FACKID1 FILL-IN-FACK FILL-IN-FACKID2 ~
RAD_SOK FILL-IN-SOKALT 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR WINDOW-1 AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE MENU POPUP-MENU-BRW_HLEV 
       MENU-ITEM m_Visa_information_via_Intern LABEL "Visa information via vald Leverantör".

DEFINE MENU POPUP-MENU-BRW_MTRL 
       MENU-ITEM m_Visa_information_via_Intern2 LABEL "Visa information via vald Leverantör".


/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB 
     LABEL "Avbryt":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_BACK 
     IMAGE-UP FILE "BILDER\prev-u":U
     LABEL "":L 
     SIZE 4 BY 1.21.

DEFINE BUTTON BTN_LEV 
     LABEL "Huvudleverantör" 
     SIZE 16 BY 1
     BGCOLOR 8 .

DEFINE BUTTON BTN_OVER 
     IMAGE-UP FILE "BILDER\next-u":U
     LABEL "":L 
     SIZE 4 BY 1.21.

DEFINE BUTTON BTN_PRIS 
     LABEL "Pris":L 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_SALDO 
     LABEL "Saldo":L 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_SPEC 
     LABEL "Spec.mtrl" 
     SIZE 12 BY 1
     FGCOLOR 1 .

DEFINE BUTTON BTN_UPPANT 
     LABEL "Depå info.":L 
     SIZE 12 BY 1.

DEFINE BUTTON FBTN_BEST 
     LABEL "Ok" 
     SIZE 14 BY 1
     FGCOLOR 1 .

DEFINE BUTTON FBTN_BYT 
     LABEL "Byte lev.":L 
     SIZE 14 BY 1.

DEFINE BUTTON FBTN_VISA 
     LABEL "Visa" 
     SIZE 14 BY 1
     FGCOLOR 1 .

DEFINE VARIABLE CMB_LEV AS CHARACTER FORMAT "X(25)":U 
     LABEL "Leverantörer" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "0" 
     DROP-DOWN-LIST
     SIZE 18.25 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-BEN AS CHARACTER FORMAT "X(40)" 
     LABEL "Benämning" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN-DBEN AS CHARACTER FORMAT "X(40)" 
     LABEL "Benämning" 
     VIEW-AS FILL-IN 
     SIZE 19.5 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN-DENR AS CHARACTER FORMAT "X(11)" 
     LABEL "Enr" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN-ENR AS CHARACTER FORMAT "X(11)" 
     LABEL "Enr" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN-FACK AS CHARACTER FORMAT "X(8)" 
     LABEL "Fack" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN-FACKID1 AS CHARACTER FORMAT "X(2)":U 
     LABEL "Fack-id (aa)" 
     VIEW-AS FILL-IN 
     SIZE 3.5 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN-FACKID2 AS CHARACTER FORMAT "X(6)":U 
     LABEL "+ (999999)" 
     VIEW-AS FILL-IN 
     SIZE 7.63 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN-SOKALT AS CHARACTER FORMAT "X(256)":U INITIAL "Sökalternativ:" 
      VIEW-AS TEXT 
     SIZE 11.25 BY .83 NO-UNDO.

DEFINE IMAGE IMAGE-13
     FILENAME "BILDER\sokpa.gif":U CONVERT-3D-COLORS
     SIZE 8 BY .83.

DEFINE IMAGE IMAGE-14
     FILENAME "BILDER\sokpa.gif":U CONVERT-3D-COLORS
     SIZE 8 BY .83.

DEFINE VARIABLE RAD_LAGER AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Lagerförd materiel", 1,
"Ej lagerförd materiel", 2
     SIZE 56.5 BY .75 NO-UNDO.

DEFINE VARIABLE RAD_SOK AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Början", 1,
"Någonstans", 2,
"Slutet", 3
     SIZE 27 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 42.5 BY 3.63
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 61.75 BY 2.42
     BGCOLOR 8 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_HLEV FOR 
      mtrltemp SCROLLING.

DEFINE QUERY BRW_MTRL FOR 
      spec_mtrl SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_HLEV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_HLEV WINDOW-1 _STRUCTURED
  QUERY BRW_HLEV NO-LOCK DISPLAY
      mtrltemp.Enr FORMAT "X(11)":U
      mtrltemp.Benamning FORMAT "x(256)":U WIDTH 25
      mtrltemp.Enhet COLUMN-LABEL "Enh" FORMAT "x(3)":U
      mtrltemp.NPRIS FORMAT ">>>>99.99":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING MULTIPLE SIZE 44 BY 18.5
         TITLE "".

DEFINE BROWSE BRW_MTRL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_MTRL WINDOW-1 _STRUCTURED
  QUERY BRW_MTRL NO-LOCK DISPLAY
      spec_mtrl.FACKID COLUMN-LABEL "Fack-id" FORMAT "X(8)":U
      spec_mtrl.ENR COLUMN-LABEL "Enr" FORMAT "X(11)":U
      spec_mtrl.BENAMNING COLUMN-LABEL "Benämning" FORMAT "x(256)":U
            WIDTH 43
      spec_mtrl.SALDO COLUMN-LABEL "Saldo" FORMAT ">>>>>9":U
      spec_mtrl.ENHET COLUMN-LABEL "Enh" FORMAT "x(3)":U
      spec_mtrl.BESTKVANT COLUMN-LABEL "Best-!kvant" FORMAT ">>>>9":U
      spec_mtrl.BESTPUNKT COLUMN-LABEL "Best-!punkt" FORMAT ">>>>9":U
      spec_mtrl.OMSATT COLUMN-LABEL "Omsättn.!/år" FORMAT ">>>>>>9":U
      spec_mtrl.LEVKOD COLUMN-LABEL "Lev-id" FORMAT "X(4)":U
  ENABLE
      spec_mtrl.BENAMNING
      spec_mtrl.ENHET
      spec_mtrl.BESTKVANT
      spec_mtrl.BESTPUNKT
      spec_mtrl.OMSATT
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING MULTIPLE SIZE 75.13 BY 18.5
         TITLE "Depåmateriel".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     RAD_LAGER AT ROW 1 COL 50.5 NO-LABEL
     BRW_HLEV AT ROW 2 COL 1.5
     BRW_MTRL AT ROW 2 COL 49.38
     BTN_OVER AT ROW 6.46 COL 45.38
     BTN_BACK AT ROW 9.96 COL 45.38
     CMB_LEV AT ROW 21.08 COL 14.5 COLON-ALIGNED
     BTN_LEV AT ROW 21.08 COL 16.63
     BTN_UPPANT AT ROW 21.25 COL 47.5
     BTN_SALDO AT ROW 21.25 COL 59.88
     BTN_PRIS AT ROW 21.25 COL 72.25
     BTN_SPEC AT ROW 21.25 COL 84.63
     FBTN_BYT AT ROW 21.25 COL 97
     FBTN_VISA AT ROW 21.25 COL 111.63
     FILL-IN-ENR AT ROW 23.17 COL 20.5 COLON-ALIGNED
     FILL-IN-DENR AT ROW 23.17 COL 62 COLON-ALIGNED
     FILL-IN-DBEN AT ROW 23.17 COL 87.63 COLON-ALIGNED
     FILL-IN-BEN AT ROW 24.17 COL 20.5 COLON-ALIGNED
     FILL-IN-FACKID1 AT ROW 24.17 COL 61.75 COLON-ALIGNED
     FILL-IN-FACK AT ROW 24.17 COL 62 COLON-ALIGNED
     FILL-IN-FACKID2 AT ROW 24.17 COL 79.13 COLON-ALIGNED
     RAD_SOK AT ROW 25.42 COL 15.25 NO-LABEL
     FBTN_BEST AT ROW 25.58 COL 96.63
     BTN_AVB AT ROW 25.58 COL 111.63
     FILL-IN-SOKALT AT ROW 25.46 COL 2.25 NO-LABEL
     RECT-2 AT ROW 22.96 COL 1.5
     RECT-4 AT ROW 22.96 COL 49.38
     IMAGE-13 AT ROW 23.17 COL 2.63
     IMAGE-14 AT ROW 23.17 COL 50.25
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 125 BY 26.17.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: 
   Temp-Tables and Buffers:
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
         HEIGHT             = 26.21
         WIDTH              = 125.25
         MAX-HEIGHT         = 28.42
         MAX-WIDTH          = 125.25
         VIRTUAL-HEIGHT     = 28.42
         VIRTUAL-WIDTH      = 125.25
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
   FRAME-NAME                                                           */
/* BROWSE-TAB BRW_HLEV IMAGE-14 FRAME-A */
/* BROWSE-TAB BRW_MTRL BRW_HLEV FRAME-A */
ASSIGN 
       BRW_HLEV:HIDDEN  IN FRAME FRAME-A                = TRUE
       BRW_HLEV:POPUP-MENU IN FRAME FRAME-A             = MENU POPUP-MENU-BRW_HLEV:HANDLE
       BRW_HLEV:MAX-DATA-GUESS IN FRAME FRAME-A         = 1000
       BRW_HLEV:ALLOW-COLUMN-SEARCHING IN FRAME FRAME-A = TRUE
       BRW_HLEV:COLUMN-RESIZABLE IN FRAME FRAME-A       = TRUE.

ASSIGN 
       BRW_MTRL:POPUP-MENU IN FRAME FRAME-A             = MENU POPUP-MENU-BRW_MTRL:HANDLE
       BRW_MTRL:MAX-DATA-GUESS IN FRAME FRAME-A         = 40000
       BRW_MTRL:ALLOW-COLUMN-SEARCHING IN FRAME FRAME-A = TRUE
       BRW_MTRL:COLUMN-RESIZABLE IN FRAME FRAME-A       = TRUE.

ASSIGN 
       BTN_LEV:HIDDEN IN FRAME FRAME-A           = TRUE.

/* SETTINGS FOR BUTTON BTN_PRIS IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BTN_SALDO IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       CMB_LEV:HIDDEN IN FRAME FRAME-A           = TRUE.

ASSIGN 
       FILL-IN-BEN:HIDDEN IN FRAME FRAME-A           = TRUE.

ASSIGN 
       FILL-IN-DBEN:HIDDEN IN FRAME FRAME-A           = TRUE.

ASSIGN 
       FILL-IN-DENR:HIDDEN IN FRAME FRAME-A           = TRUE.

ASSIGN 
       FILL-IN-ENR:HIDDEN IN FRAME FRAME-A           = TRUE.

ASSIGN 
       FILL-IN-FACK:HIDDEN IN FRAME FRAME-A           = TRUE.

ASSIGN 
       FILL-IN-FACKID1:HIDDEN IN FRAME FRAME-A           = TRUE.

ASSIGN 
       FILL-IN-FACKID2:HIDDEN IN FRAME FRAME-A           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-SOKALT IN FRAME FRAME-A
   ALIGN-L                                                              */
ASSIGN 
       FILL-IN-SOKALT:READ-ONLY IN FRAME FRAME-A        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-1)
THEN WINDOW-1:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_HLEV
/* Query rebuild information for BROWSE BRW_HLEV
     _TblList          = "Temp-Tables.mtrltemp"
     _Options          = "NO-LOCK"
     _OrdList          = "Temp-Tables.mtrltemp.Enr|yes"
     _Where[1]         = "Temp-Tables.mtrltemp.LEVKOD = vald_kundlev
 AND Temp-Tables.mtrltemp.KALKNR = 0"
     _FldNameList[1]   = Temp-Tables.mtrltemp.Enr
     _FldNameList[2]   > Temp-Tables.mtrltemp.Benamning
"mtrltemp.Benamning" ? "x(256)" "character" ? ? ? ? ? ? no ? no no "25" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.mtrltemp.Enhet
"mtrltemp.Enhet" "Enh" "x(3)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   = Temp-Tables.mtrltemp.NPRIS
     _Query            is NOT OPENED
*/  /* BROWSE BRW_HLEV */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_MTRL
/* Query rebuild information for BROWSE BRW_MTRL
     _TblList          = "Temp-Tables.spec_mtrl"
     _Options          = "NO-LOCK"
     _OrdList          = "Temp-Tables.spec_mtrl.ENR|yes"
     _Where[1]         = "spec_mtrl.DEPNR = vald_depa"
     _FldNameList[1]   > Temp-Tables.spec_mtrl.FACKID
"FACKID" "Fack-id" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.spec_mtrl.ENR
"ENR" "Enr" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.spec_mtrl.BENAMNING
"BENAMNING" "Benämning" "x(256)" "character" ? ? ? ? ? ? yes ? no no "43" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.spec_mtrl.SALDO
"SALDO" "Saldo" ">>>>>9" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.spec_mtrl.ENHET
"ENHET" "Enh" "x(3)" "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > Temp-Tables.spec_mtrl.BESTKVANT
"BESTKVANT" "Best-!kvant" ">>>>9" "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > Temp-Tables.spec_mtrl.BESTPUNKT
"BESTPUNKT" "Best-!punkt" ? "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > Temp-Tables.spec_mtrl.OMSATT
"OMSATT" "Omsättn.!/år" ">>>>>>9" "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > Temp-Tables.spec_mtrl.LEVKOD
"LEVKOD" "Lev-id" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_MTRL */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME BRW_HLEV
&Scoped-define SELF-NAME BRW_HLEV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_HLEV WINDOW-1
ON MOUSE-SELECT-DBLCLICK OF BRW_HLEV IN FRAME FRAME-A
DO:
    APPLY "CHOOSE"  TO BTN_OVER.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_HLEV WINDOW-1
ON VALUE-CHANGED OF BRW_HLEV IN FRAME FRAME-A
DO:
   status-ok = BRW_HLEV:SELECT-FOCUSED-ROW() IN FRAME FRAME-A.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_MTRL
&Scoped-define SELF-NAME BRW_MTRL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_MTRL WINDOW-1
ON MOUSE-SELECT-DBLCLICK OF BRW_MTRL IN FRAME FRAME-A /* Depåmateriel */
DO:               
   RUN msdbclick_UI.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_MTRL WINDOW-1
ON ROW-LEAVE OF BRW_MTRL IN FRAME FRAME-A /* Depåmateriel */
DO:
   IF AVAILABLE mtrltemp THEN DO:
     RUN visa_UI.    
     ASSIGN     
     spec_mtrl.BENAMNING = INPUT BROWSE BRW_MTRL spec_mtrl.BENAMNING 
     spec_mtrl.BESTKVANT = INPUT BROWSE BRW_MTRL spec_mtrl.BESTKVANT 
     spec_mtrl.BESTPUNKT = INPUT BROWSE BRW_MTRL spec_mtrl.BESTPUNKT 
     spec_mtrl.OMSATT    = INPUT BROWSE BRW_MTRL spec_mtrl.OMSATT.         
     RUN visa_UI. 
  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_MTRL WINDOW-1
ON VALUE-CHANGED OF BRW_MTRL IN FRAME FRAME-A /* Depåmateriel */
DO:
   status-ok = BRW_MTRL:SELECT-FOCUSED-ROW() IN FRAME FRAME-A.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME spec_mtrl.FACKID
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL spec_mtrl.FACKID BRW_MTRL _BROWSE-COLUMN WINDOW-1
ON ENTRY OF spec_mtrl.FACKID IN BROWSE BRW_MTRL /* Fack-id */
DO:
   status-ok = BRW_MTRL:SELECT-FOCUSED-ROW() IN FRAME FRAME-A.
   IF AVAILABLE spec_mtrl THEN RUN UPPANT2V.W (INPUT ROWID(spec_mtrl)).  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL spec_mtrl.FACKID BRW_MTRL _BROWSE-COLUMN WINDOW-1
ON MOUSE-SELECT-DBLCLICK OF spec_mtrl.FACKID IN BROWSE BRW_MTRL /* Fack-id */
DO:
   status-ok = BRW_MTRL:SELECT-FOCUSED-ROW() IN FRAME FRAME-A.
   IF AVAILABLE spec_mtrl THEN RUN UPPANT2V.W (INPUT ROWID(spec_mtrl)).
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME spec_mtrl.BENAMNING
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL spec_mtrl.BENAMNING BRW_MTRL _BROWSE-COLUMN WINDOW-1
ON ENTRY OF spec_mtrl.BENAMNING IN BROWSE BRW_MTRL /* Benämning */
DO:
   status-ok = BRW_MTRL:SELECT-FOCUSED-ROW() IN FRAME FRAME-A.
   IF AVAILABLE spec_mtrl THEN RUN visa_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL spec_mtrl.BENAMNING BRW_MTRL _BROWSE-COLUMN WINDOW-1
ON LEAVE OF spec_mtrl.BENAMNING IN BROWSE BRW_MTRL /* Benämning */
DO:
   IF AVAILABLE spec_mtrl THEN DO:   
      IF INPUT BROWSE BRW_MTRL spec_mtrl.BENAMNING = "" THEN DO:      
         MESSAGE  "Benämning får inte vara blankt." VIEW-AS ALERT-BOX.   
         RETURN NO-APPLY.
      END.               
      IF spec_mtrl.BENAMNING NE INPUT BROWSE BRW_MTRL spec_mtrl.BENAMNING THEN DO:     
         spec_mtrl.BENAMNING = INPUT BROWSE BRW_MTRL spec_mtrl.BENAMNING.
         DISPLAY spec_mtrl.BENAMNING WITH BROWSE BRW_MTRL.         
         RUN specand_UI.
      END.
   END.
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME spec_mtrl.SALDO
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL spec_mtrl.SALDO BRW_MTRL _BROWSE-COLUMN WINDOW-1
ON LEAVE OF spec_mtrl.SALDO IN BROWSE BRW_MTRL /* Saldo */
DO:
  /* Det ska inte vara för lätt att ändra på saldo. Bara luleå får ändra saldo.
  IF AVAILABLE spec_mtrl THEN DO:  
     sparsaldo = spec_mtrl.SALDO.
     IF spec_mtrl.SALDO NE INPUT BROWSE BRW_MTRL spec_mtrl.SALDO THEN DO:     
        spec_mtrl.SALDO = INPUT BROWSE BRW_MTRL spec_mtrl.SALDO.
        DISPLAY spec_mtrl.SALDO WITH BROWSE BRW_MTRL.            
   
        IF spec_mtrl.SALDO < sparsaldo THEN DO:
            CREATE svinn_temp.
            ASSIGN
            svinn_temp.ENR = spec_mtrl.ENR
            svinn_temp.BENAMNING = spec_mtrl.BENAMNING
            svinn_temp.ENHET = spec_mtrl.ENHET
            svinn_temp.PRIS = spec_mtrl.NPRIS       
            svinn_temp.LEV = spec_mtrl.LEV
            svinn_temp.ANTAL = sparsaldo - spec_mtrl.SALDO
            svinn_temp.UTTAG = TRUE.
        END.
        ELSE IF spec_mtrl.SALDO > sparsaldo THEN DO:  
            CREATE svinn_temp.
            ASSIGN
            svinn_temp.ENR = spec_mtrl.ENR
            svinn_temp.BENAMNING = spec_mtrl.BENAMNING
            svinn_temp.ENHET = spec_mtrl.ENHET
            svinn_temp.PRIS = spec_mtrl.NPRIS       
            svinn_temp.LEV = spec_mtrl.LEV
            svinn_temp.ANTAL = spec_mtrl.SALDO - sparsaldo
            svinn_temp.UTTAG = FALSE.
        END.     
        RUN specand_UI.
     END.
  END.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME spec_mtrl.ENHET
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL spec_mtrl.ENHET BRW_MTRL _BROWSE-COLUMN WINDOW-1
ON ENTRY OF spec_mtrl.ENHET IN BROWSE BRW_MTRL /* Enh */
DO:
   status-ok = BRW_MTRL:SELECT-FOCUSED-ROW() IN FRAME FRAME-A.
   IF AVAILABLE spec_mtrl THEN RUN visa_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL spec_mtrl.ENHET BRW_MTRL _BROWSE-COLUMN WINDOW-1
ON LEAVE OF spec_mtrl.ENHET IN BROWSE BRW_MTRL /* Enh */
DO:
  IF AVAILABLE spec_mtrl THEN DO:   
      IF INPUT BROWSE BRW_MTRL spec_mtrl.ENHET = "" THEN DO:      
         MESSAGE  "Enhet får inte vara blankt." VIEW-AS ALERT-BOX.   
         RETURN NO-APPLY.
      END.               
      IF spec_mtrl.ENHET NE INPUT BROWSE BRW_MTRL spec_mtrl.ENHET THEN DO:     
         spec_mtrl.ENHET = INPUT BROWSE BRW_MTRL spec_mtrl.ENHET.
         DISPLAY spec_mtrl.ENHET WITH BROWSE BRW_MTRL.         
         RUN specand_UI.
      END.
   END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME spec_mtrl.BESTKVANT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL spec_mtrl.BESTKVANT BRW_MTRL _BROWSE-COLUMN WINDOW-1
ON ENTRY OF spec_mtrl.BESTKVANT IN BROWSE BRW_MTRL /* Best-!kvant */
DO:
   status-ok = BRW_MTRL:SELECT-FOCUSED-ROW() IN FRAME FRAME-A.
   IF AVAILABLE spec_mtrl THEN RUN visa_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL spec_mtrl.BESTKVANT BRW_MTRL _BROWSE-COLUMN WINDOW-1
ON LEAVE OF spec_mtrl.BESTKVANT IN BROWSE BRW_MTRL /* Best-!kvant */
DO:   
   IF AVAILABLE spec_mtrl THEN DO:
     IF INPUT BROWSE BRW_MTRL spec_mtrl.Bestkvant = 0 THEN DO:      
         MESSAGE "Best-antal får inte vara 0."   
         VIEW-AS ALERT-BOX.
         RETURN NO-APPLY.
      END.  
      IF spec_mtrl.Bestkvant NE INPUT BROWSE BRW_MTRL spec_mtrl.Bestkvant THEN DO:     
         spec_mtrl.Bestkvant = INPUT BROWSE BRW_MTRL spec_mtrl.Bestkvant.
         DISPLAY spec_mtrl.Bestkvant WITH BROWSE BRW_MTRL.         
         RUN specand_UI.
      END.   
   END.
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL spec_mtrl.BESTKVANT BRW_MTRL _BROWSE-COLUMN WINDOW-1
ON MOUSE-SELECT-CLICK OF spec_mtrl.BESTKVANT IN BROWSE BRW_MTRL /* Best-!kvant */
DO:
   APPLY "ENTRY" TO spec_mtrl.BESTKVANT IN BROWSE BRW_MTRL.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME spec_mtrl.BESTPUNKT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL spec_mtrl.BESTPUNKT BRW_MTRL _BROWSE-COLUMN WINDOW-1
ON ENTRY OF spec_mtrl.BESTPUNKT IN BROWSE BRW_MTRL /* Best-!punkt */
DO:
   status-ok = BRW_MTRL:SELECT-FOCUSED-ROW() IN FRAME FRAME-A.
   IF AVAILABLE spec_mtrl THEN RUN visa_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL spec_mtrl.BESTPUNKT BRW_MTRL _BROWSE-COLUMN WINDOW-1
ON LEAVE OF spec_mtrl.BESTPUNKT IN BROWSE BRW_MTRL /* Best-!punkt */
DO:  
   IF AVAILABLE spec_mtrl THEN DO:   
      IF spec_mtrl.BESTPUNKT NE INPUT BROWSE BRW_MTRL spec_mtrl.BESTPUNKT THEN DO:     
         spec_mtrl.BESTPUNKT = INPUT BROWSE BRW_MTRL spec_mtrl.BESTPUNKT.
         DISPLAY spec_mtrl.BESTPUNKT WITH BROWSE BRW_MTRL.       
         RUN specand_UI.
      END.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME spec_mtrl.OMSATT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL spec_mtrl.OMSATT BRW_MTRL _BROWSE-COLUMN WINDOW-1
ON ENTRY OF spec_mtrl.OMSATT IN BROWSE BRW_MTRL /* Omsättn.!/år */
DO:
   status-ok = BRW_MTRL:SELECT-FOCUSED-ROW() IN FRAME FRAME-A.
   IF AVAILABLE spec_mtrl THEN RUN visa_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL spec_mtrl.OMSATT BRW_MTRL _BROWSE-COLUMN WINDOW-1
ON LEAVE OF spec_mtrl.OMSATT IN BROWSE BRW_MTRL /* Omsättn.!/år */
DO:
  IF AVAILABLE spec_mtrl THEN DO:   
     IF spec_mtrl.OMSATT NE INPUT BROWSE BRW_MTRL spec_mtrl.OMSATT THEN DO:     
        spec_mtrl.OMSATT = INPUT BROWSE BRW_MTRL spec_mtrl.OMSATT.
        DISPLAY spec_mtrl.OMSATT WITH BROWSE BRW_MTRL.            
        RUN specand_UI.
     END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL spec_mtrl.OMSATT BRW_MTRL _BROWSE-COLUMN WINDOW-1
ON MOUSE-SELECT-CLICK OF spec_mtrl.OMSATT IN BROWSE BRW_MTRL /* Omsättn.!/år */
DO:
  APPLY "ENTRY" TO spec_mtrl.OMSATT IN BROWSE BRW_MTRL.
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
      QUESTION BUTTONS YES-NO-CANCEL TITLE "Spara ändringar?" UPDATE svar.         
      IF svar THEN DO:
         APPLY "CHOOSE" TO FBTN_BEST IN FRAME {&FRAME-NAME}.
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


&Scoped-define SELF-NAME BTN_BACK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_BACK WINDOW-1
ON CHOOSE OF BTN_BACK IN FRAME FRAME-A
DO:   
   RUN btnback_UI.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_LEV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_LEV WINDOW-1
ON CHOOSE OF BTN_LEV IN FRAME FRAME-A /* Huvudleverantör */
DO:
   {muswait.i}
   
   vald_lev = vald_kundlev.         
   
   ASSIGN 
   FILL-IN-BEN:HIDDEN = FALSE 
   FILL-IN-ENR:HIDDEN = FALSE    
   BTN_LEV:HIDDEN = TRUE
   CMB_LEV:HIDDEN = FALSE
   BRW_HLEV:HIDDEN = FALSE 
   CMB_LEV:SCREEN-VALUE = huvudlev
   vald_lev = vald_kundlev   
   lev = huvudlev.
   RUN setorgtitle_UI IN brwproc[1] (INPUT "Materiellista : " + huvudlev).            
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


&Scoped-define SELF-NAME BTN_OVER
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OVER WINDOW-1
ON CHOOSE OF BTN_OVER IN FRAME FRAME-A
DO:    
   musz = TRUE.            
   antal_valda = BRW_HLEV:NUM-SELECTED-ROWS.         
   antal_raknare = 1.
   DO WHILE antal_raknare LE antal_valda:                                   
      status-ok = BRW_HLEV:FETCH-SELECTED-ROW(antal_raknare).                          
      RUN over_UI.           
   END.       
   
   IF RAD_LAGER = 1 THEN RUN setcolsortvar_UI IN brwproc[4] (INPUT "LAGER = TRUE").
   ELSE RUN setcolsortvar_UI IN brwproc[4] (INPUT "LAGER = FALSE").
   RUN openbdynspec_UI IN brwproc[4].
   
   FIND FIRST spec_mtrl WHERE ROWID(spec_mtrl) = reprowid NO-LOCK NO-ERROR.
   IF AVAILABLE spec_mtrl THEN DO:
      RUN setlastrowid_UI IN brwproc[4] (INPUT ROWID(spec_mtrl)).
      RUN lastselectdyn_UI IN brwproc[4].        
   END.
   status-ok = BRW_HLEV:DESELECT-ROWS().
   
   musz = FALSE.   
   
   
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_PRIS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_PRIS WINDOW-1
ON CHOOSE OF BTN_PRIS IN FRAME FRAME-A /* Pris */
DO:
   RUN btnpris_UI.      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_SALDO
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_SALDO WINDOW-1
ON CHOOSE OF BTN_SALDO IN FRAME FRAME-A /* Saldo */
DO:
   RUN btnsaldo_UI.      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_SPEC
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_SPEC WINDOW-1
ON CHOOSE OF BTN_SPEC IN FRAME FRAME-A /* Spec.mtrl */
DO: 
   {muswait.i}
   {AVBGOM.I}
   RUN DEPSPECANV2.W (INPUT 2).
   {AVBFRAM.I}      
   IF RAD_LAGER = 1 THEN RUN setcolsortvar_UI IN brwproc[4] (INPUT "LAGER = TRUE").
   ELSE RUN setcolsortvar_UI IN brwproc[4] (INPUT "LAGER = FALSE").
   RUN openbdynspec_UI IN brwproc[4].   
   {musarrow.i}  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_UPPANT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_UPPANT WINDOW-1
ON CHOOSE OF BTN_UPPANT IN FRAME FRAME-A /* Depå info. */
DO:   
   
   antal_valda = BRW_MTRL:NUM-SELECTED-ROWS.
   antal_raknare = 1.    
   DO WHILE antal_raknare LE antal_valda:   
      status-ok = BRW_MTRL:FETCH-SELECTED-ROW(antal_raknare).  
      
         reprowid = ROWID(spec_mtrl).
         RUN UPPANT2V.W (INPUT ROWID(spec_mtrl)).
      
         antal_raknare = antal_raknare + 1.       
   END.
   FIND FIRST spec_mtrl WHERE ROWID(spec_mtrl) = reprowid NO-LOCK NO-ERROR.
   IF AVAILABLE spec_mtrl THEN DO:
      RUN setlastrowid_UI IN brwproc[4] (INPUT ROWID(spec_mtrl)).
      RUN lastselectdyn_UI IN brwproc[4].        
   END. 
   /*RUN refreshbrw_UI IN brwproc[4].*/
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
      IF lev BEGINS globanv THEN DO:            
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
      RUN setorgtitle_UI IN brwproc[1] (INPUT "Materiellista : " + lev).         
      /*BRW_HLEV:TITLE = "Materiellista : " + lev.*/
      
   END.
   ELSE DO:
      vald_lev = vald_kundlev.
   END.      
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_BEST
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_BEST WINDOW-1
ON CHOOSE OF FBTN_BEST IN FRAME FRAME-A /* Ok */
DO:
   {muswait.i} 
   RUN mtrldepdat_UI IN mtrlbapph (INPUT vald_depa,OUTPUT invdat).                
   
   IF Guru.Konstanter:globforetag = "ELPA" OR Guru.Konstanter:globforetag = "SOLE" OR Guru.Konstanter:globforetag = "BORL" OR Guru.Konstanter:globforetag = "GKAL" OR 
   Guru.Konstanter:globforetag = "LULE" OR Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "BODE" THEN DO:
      RUN okdepv2_UI IN mtrlbapph 
         (INPUT vald_depa, INPUT TABLE aspec_mtrl, 
         INPUT TABLE svinn_temp, INPUT invdat, INPUT Guru.Konstanter:globanv).
      /*
      IF Guru.Konstanter:appcon THEN DO:                           
         RUN OKDEP2V.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
         (INPUT vald_depa, INPUT TABLE aspec_mtrl, 
         INPUT TABLE svinn_temp, INPUT invdat, INPUT Guru.Konstanter:globanv).
      END.
      ELSE DO:
         RUN OKDEP2V.P
         (INPUT vald_depa, INPUT TABLE aspec_mtrl, 
         INPUT TABLE svinn_temp, INPUT invdat, INPUT Guru.Konstanter:globanv).
      END.                         
      */
   END.   
   RUN okdepv_UI IN mtrlbapph (INPUT vald_depa, INPUT TABLE aspec_mtrl, INPUT invdat).
   IF Guru.Konstanter:globforetag = "LULE"  THEN DO:
      FIND FIRST pspec_mtrl NO-LOCK NO-ERROR.
      IF AVAILABLE pspec_mtrl THEN DO:
         RUN andpris_UI IN mtrlbapph (INPUT vald_depa, INPUT TABLE pspec_mtrl).
      END.
      /*Byter ut hela MTRL där MTRL.levkod = 41 */
      RUN uppdatkatpris_UI IN mtrlbapph (INPUT vald_depa).
   END.
   IF Guru.Konstanter:globforetag = "BODE"  THEN DO:
      FIND FIRST pspec_mtrl NO-LOCK NO-ERROR.
      IF AVAILABLE pspec_mtrl THEN DO:
         RUN andpris_UI IN mtrlbapph (INPUT vald_depa, INPUT TABLE pspec_mtrl).
      END.
      /*Byter ut hela MTRL där MTRL.levkod = 6 */
      RUN uppdatkatpris_UI IN mtrlbapph (INPUT vald_depa).
   END.
   IF Guru.Konstanter:globforetag = "ELPA" OR Guru.Konstanter:globforetag = "BORL" OR Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT"  THEN DO:
      FIND FIRST pspec_mtrl NO-LOCK NO-ERROR.
      IF AVAILABLE pspec_mtrl THEN DO:
         RUN andpris_UI IN mtrlbapph (INPUT vald_depa, INPUT TABLE pspec_mtrl).
      END.
   
   END.
   /*
   IF Guru.Konstanter:appcon THEN DO:                           
      RUN OKDEPV.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT vald_depa, INPUT TABLE aspec_mtrl, INPUT invdat).
   END.
   ELSE DO:
      RUN OKDEPV.P
      (INPUT vald_depa, INPUT TABLE aspec_mtrl, INPUT invdat).
   END.                
   */
   APPLY "GO" TO FRAME {&FRAME-NAME}.              
   APPLY "CLOSE":U TO THIS-PROCEDURE.      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_BYT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_BYT WINDOW-1
ON CHOOSE OF FBTN_BYT IN FRAME FRAME-A /* Byte lev. */
DO: 
   RUN btnbyt_UI.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_VISA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_VISA WINDOW-1
ON CHOOSE OF FBTN_VISA IN FRAME FRAME-A /* Visa */
DO:                  
   {muswait.i}   
   EMPTY TEMP-TABLE tidut NO-ERROR. 
   
str=
"=======================================================================================".      
   RUN getcolname_UI IN brwproc[4] (OUTPUT colnamevar ,OUTPUT ascdesclog).   
   RUN huvud_UI.
   RUN rubrik_UI.
   RUN klar_UI.      
   
   ASSIGN    
   skrivut = FALSE.
   {AVBGOM.I}   
   RUN VISALAGV.W (INPUT TABLE tidut).   
   {AVBFRAM.I}
   {musarrow.i}      
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
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-BEN WINDOW-1
ON MOUSE-SELECT-DBLCLICK OF FILL-IN-BEN IN FRAME FRAME-A /* Benämning */
DO:    
   {muswait.i}
   {BENHMT2.I}      
   RUN initsok_UI (INPUT 1,INPUT aosok).
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
   {muswait.i}
   {ENRHMT2.I}
   RUN initsok_UI (INPUT 2,INPUT posok).
   {musarrow.i}   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-FACK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-FACK WINDOW-1
ON ANY-KEY OF FILL-IN-FACK IN FRAME FRAME-A /* Fack */
DO:
   {TRYCKS.I}
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN-FACK IN FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-FACK WINDOW-1
ON MOUSE-SELECT-DBLCLICK OF FILL-IN-FACK IN FRAME FRAME-A /* Fack */
DO:
   ASSIGN
   FILL-IN-FACK = var1 + var2.    
   IF AVAILABLE spec_mtrl THEN mtrl_rowid = ROWID(spec_mtrl).
   IF FILL-IN-FACK = '' THEN DO:
      MESSAGE "Sökbegreppet kan inte vara blankt." VIEW-AS ALERT-BOX TITLE "Meddelande".
      APPLY "ENTRY" TO FILL-IN-FACKID1 IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
   END.      
   aosok = '*' + FILL-IN-FACK + '*'.
   status-ok = BRW_MTRL:DESELECT-ROWS().            
   FIND spec_mtrl WHERE ROWID(spec_mtrl) = mtrl_rowid NO-LOCK NO-ERROR.                  
   FIND NEXT spec_mtrl WHERE spec_mtrl.FACKID MATCHES aosok AND spec_mtrl.DEPNR = vald_depa
   NO-LOCK NO-ERROR. 
   IF NOT AVAILABLE spec_mtrl THEN DO:      
      FIND FIRST spec_mtrl WHERE spec_mtrl.FACKID MATCHES aosok AND spec_mtrl.DEPNR = vald_depa
      NO-LOCK NO-ERROR.   
      IF NOT AVAILABLE spec_mtrl THEN DO: 
         MESSAGE "Det finns inget på sökbegreppet i depån."
         VIEW-AS ALERT-BOX TITLE "Meddelande". 
      END.
   END.    
   IF AVAILABLE spec_mtrl THEN DO:               
      RUN setlastrowid_UI IN brwproc[4] (INPUT ROWID(spec_mtrl)).
      RUN lastselectdyn_UI IN brwproc[4].              
   END.               
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-FACKID1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-FACKID1 WINDOW-1
ON ANY-KEY OF FILL-IN-FACKID1 IN FRAME FRAME-A /* Fack-id (aa) */
DO:
   {TRYCKS.I}
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN-FACKID1 IN FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-FACKID1 WINDOW-1
ON LEAVE OF FILL-IN-FACKID1 IN FRAME FRAME-A /* Fack-id (aa) */
DO: 
   ASSIGN
   var1 = INPUT FILL-IN-FACKID1  
   var1 = TRIM(var1)
   langd = LENGTH(var1).  
   IF langd = 2 THEN DO:
      FILL-IN-FACKID1 = var1.      
   END.
   ELSE DO:
      DO WHILE langd < 2:
         ASSIGN
         var1 = var1 + " " 
         langd = langd + 1.
      END.  
      FILL-IN-FACKID1 = var1.     
   END.      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-FACKID1 WINDOW-1
ON MOUSE-SELECT-DBLCLICK OF FILL-IN-FACKID1 IN FRAME FRAME-A /* Fack-id (aa) */
DO: 
   ASSIGN
   var1 = INPUT FILL-IN-FACKID1  
   var1 = TRIM(var1)
   langd = LENGTH(var1).  
   IF langd = 2 THEN DO:
      FILL-IN-FACKID1 = var1.      
   END.
   ELSE DO:
      DO WHILE langd < 2:
         ASSIGN
         var1 = var1 + " " 
         langd = langd + 1.
      END.  
      FILL-IN-FACKID1 = var1.     
   END.                         
   APPLY "LEAVE" TO FILL-IN-FACKID2 IN FRAME FRAME-A.
   FILL-IN-FACK = var1 + var2.    
   FILL-IN-FACK:SCREEN-VALUE = var1 + var2.    
   APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN-FACK IN FRAME FRAME-A.
   FILL-IN-FACK:HIDDEN = TRUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-FACKID2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-FACKID2 WINDOW-1
ON ANY-KEY OF FILL-IN-FACKID2 IN FRAME FRAME-A /* + (999999) */
DO:
   {TRYCKS.I}
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN-FACKID2 IN FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-FACKID2 WINDOW-1
ON LEAVE OF FILL-IN-FACKID2 IN FRAME FRAME-A /* + (999999) */
DO: 
   ASSIGN
   var2 = INPUT FILL-IN-FACKID2  
   var2 = TRIM(var2)
   langd = LENGTH(var2).  
   IF langd = 6 THEN DO:
      FILL-IN-FACKID2 = var2.      
   END.
   ELSE DO:
      DO WHILE langd < 6:
         ASSIGN
         var2 = " " + var2
         langd = langd + 1.
      END.  
      FILL-IN-FACKID2 = var2.     
   END.      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-FACKID2 WINDOW-1
ON MOUSE-SELECT-DBLCLICK OF FILL-IN-FACKID2 IN FRAME FRAME-A /* + (999999) */
DO:
   ASSIGN
   var2 = INPUT FILL-IN-FACKID2  
   var2 = TRIM(var2)
   langd = LENGTH(var2).  
   IF langd = 6 THEN DO:
      FILL-IN-FACKID2 = var2.      
   END.
   ELSE DO:
      DO WHILE langd < 6:
         ASSIGN
         var2 = " " + var2
         langd = langd + 1.
      END.  
      FILL-IN-FACKID2 = var2.     
   END.      
   APPLY "LEAVE" TO FILL-IN-FACKID1 IN FRAME FRAME-A.   
   FILL-IN-FACK:SCREEN-VALUE = var1 + var2.    
   FILL-IN-FACK = var1 + var2.    
   APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN-FACK IN FRAME FRAME-A.
 /*FILL-IN-FACK:HIDDEN = TRUE.*/
   status-ok = BRW_MTRL:SELECT-FOCUSED-ROW().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Visa_information_via_Intern
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Visa_information_via_Intern WINDOW-1
ON CHOOSE OF MENU-ITEM m_Visa_information_via_Intern /* Visa information via vald Leverantör */
DO:
   RUN infoES_UI (INPUT 1). 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Visa_information_via_Intern2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Visa_information_via_Intern2 WINDOW-1
ON CHOOSE OF MENU-ITEM m_Visa_information_via_Intern2 /* Visa information via vald Leverantör */
DO:
   RUN infoES_UI (INPUT 2). 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RAD_LAGER
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RAD_LAGER WINDOW-1
ON VALUE-CHANGED OF RAD_LAGER IN FRAME FRAME-A
DO:
   RAD_LAGER = INPUT RAD_LAGER.
   IF RAD_LAGER = 1 THEN DO: 
      RUN setcolsortvar_UI IN brwproc[4] (INPUT "LAGER = TRUE").
      ASSIGN      
      BTN_OVER:HIDDEN = FALSE 
      BTN_SALDO:HIDDEN = FALSE 
      BTN_PRIS:HIDDEN = FALSE 
      BTN_SPEC:HIDDEN = FALSE 
      BTN_UPPANT:HIDDEN = FALSE.
   END.
   ELSE DO:
      RUN setcolsortvar_UI IN brwproc[4] (INPUT "LAGER = FALSE").
      ASSIGN      
      BTN_OVER:HIDDEN = TRUE 
      BTN_SALDO:HIDDEN = TRUE 
      BTN_PRIS:HIDDEN = TRUE 
      BTN_SPEC:HIDDEN = TRUE 
      BTN_UPPANT:HIDDEN = TRUE.
      
   END.
   RUN openbdynspec_UI IN brwproc[4].   
   RUN getfirst_UI IN brwproc[4].
   RUN lastselectdyn_UI IN brwproc[4].
   RUN title_UI IN brwproc[4].
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
   {BORTBRWPROC.I}
   IF VALID-HANDLE(mtrlbapph) THEN DELETE PROCEDURE mtrlbapph.   
   IF VALID-HANDLE(nettoh) THEN DELETE PROCEDURE nettoh NO-ERROR.      
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
      
   EMPTY TEMP-TABLE aspec_mtrl NO-ERROR.    
   EMPTY TEMP-TABLE spec_mtrl NO-ERROR.    
   EMPTY TEMP-TABLE sok_mtrl NO-ERROR.    
   EMPTY TEMP-TABLE ej_mtrl NO-ERROR.  
   EMPTY TEMP-TABLE ikmtrltemp NO-ERROR. 
   RUN mtrlehmt_UI IN mtrlbapph (INPUT vald_depa,INPUT Guru.Konstanter:globforetag ,OUTPUT vald_kundlev, OUTPUT dbenamning, OUTPUT TABLE spec_mtrl, OUTPUT TABLE ej_mtrl).                   
   IF varforetypval[29] = 1  THEN DO:                 
      /*nettopris beredning adm*/                     
      RUN adnettomark_UI IN nettoh (INPUT TABLE spec_mtrl,INPUT-OUTPUT TABLE ikmtrltemp ).                  
   END.   
   
   ASSIGN vald_lev = vald_kundlev.
   
   
   {&WINDOW-NAME}:TITLE = "Depå mtrl för depå - " + dbenamning.  
   
   status-ok = CMB_LEV:DELETE("0"). 
   FIND FIRST levtemp WHERE levtemp.LEVKOD = vald_kundlev
   USE-INDEX LEV NO-LOCK NO-ERROR. 
   ASSIGN
   status-ok = CMB_LEV:ADD-LAST(levtemp.LEVNAMN)IN FRAME {&FRAME-NAME}
   CMB_LEV:SCREEN-VALUE = levtemp.LEVNAMN
   huvudlev = levtemp.LEVNAMN.    
   ASSIGN lev = huvudlev.
   RUN setorgtitle_UI IN brwproc[1] (INPUT "Materiellista : " + huvudlev).            
   
   FOR EACH levtemp WHERE levtemp.LEVKOD NE vald_kundlev AND 
   levtemp.LEVKOD NE "0" AND levtemp.BORTTAG = FALSE USE-INDEX LEV NO-LOCK:      
      /*Niklas personlig spec_mtrl*/      
      IF levtemp.LEVKOD = "99" THEN DO:
         status-ok = CMB_LEV:ADD-LAST(globanv + " " + levtemp.LEVNAMN)IN FRAME {&FRAME-NAME}.
         status-ok = CMB_LEV:ADD-LAST(levtemp.LEVNAMN)IN FRAME {&FRAME-NAME}.
      END.
      ELSE DO:
         IF Guru.Konstanter:globforetag = "LULE" OR Guru.Konstanter:globforetag = "ELPA" THEN DO:
            IF levtemp.LEVKOD NE "41" THEN status-ok = CMB_LEV:ADD-LAST(levtemp.LEVNAMN)IN FRAME {&FRAME-NAME}.
         END.
         ELSE IF Guru.Konstanter:globforetag = "bode"  THEN DO:
            IF levtemp.LEVKOD NE "6" THEN status-ok = CMB_LEV:ADD-LAST(levtemp.LEVNAMN)IN FRAME {&FRAME-NAME}.
         END.
         ELSE DO:
            status-ok = CMB_LEV:ADD-LAST(levtemp.LEVNAMN)IN FRAME {&FRAME-NAME}.
         END.         
      END.   
   END.   
   ASSIGN
   mtrltemp.ENR:LABEL IN BROWSE BRW_HLEV = Guru.Konstanter:genk    
   spec_mtrl.ENR:LABEL IN BROWSE BRW_MTRL = Guru.Konstanter:genk   
   FILL-IN-DENR:LABEL = Guru.Konstanter:genk 
   FILL-IN-ENR:LABEL = Guru.Konstanter:genk.              
   IF Guru.Konstanter:mtrlsekvar[6] = TRUE THEN DO:
      ASSIGN mtrltemp.NPRIS:VISIBLE IN BROWSE BRW_HLEV = FALSE.
   END.
   IF Guru.Konstanter:globforetag = "ELPA"  OR Guru.Konstanter:globforetag = "GKAL" OR 
   Guru.Konstanter:globforetag = "LULE" OR Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "BORL" OR Guru.Konstanter:globforetag = "BODE" OR Guru.Konstanter:globforetag = "UMEA" 
   THEN DO:
      spec_mtrl.SALDO:VISIBLE IN BROWSE BRW_MTRL = TRUE.
      spec_mtrl.FACKID:VISIBLE IN BROWSE BRW_MTRL = FALSE.
      spec_mtrl.OMSATT:VISIBLE IN BROWSE BRW_MTRL = FALSE.
   END.
   ELSE DO:
      spec_mtrl.SALDO:VISIBLE IN BROWSE BRW_MTRL = FALSE. 
      spec_mtrl.FACKID:VISIBLE IN BROWSE BRW_MTRL = TRUE. 
      spec_mtrl.OMSATT:VISIBLE IN BROWSE BRW_MTRL = TRUE. 
   END.
   IF Guru.Konstanter:globforetag = "UMEA"  THEN DO:
      depatitta = TRUE.      
      spec_mtrl.BESTKVANT:VISIBLE IN BROWSE BRW_MTRL = FALSE.
      spec_mtrl.BESTPUNKT:VISIBLE IN BROWSE BRW_MTRL = FALSE.
      spec_mtrl.OMSATT:VISIBLE IN BROWSE BRW_MTRL = FALSE.
      /*DISABLE spec_mtrl.BENAMNING BROWSE BRW_MTRL.*/       
   END.   

   ASSIGN     
   CMB_LEV = INPUT CMB_LEV      
   bestant = FALSE. 
   RUN enable_UI.   
   {FRMSIZE.I}  
   
   IF Guru.Konstanter:globforetag = "ELPA" OR Guru.Konstanter:globforetag = "SOLE" 
     OR Guru.Konstanter:globforetag = "GKAL" OR 
   Guru.Konstanter:globforetag = "LULE" OR Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "BORL" OR Guru.Konstanter:globforetag = "BODE"
   THEN DO:              
      EMPTY TEMP-TABLE svinn_temp NO-ERROR. 
      
      ENABLE BTN_SALDO WITH FRAME {&FRAME-NAME}.
/*       IF Guru.Konstanter:globforetag = "GKAL" THEN BTN_SALDO:HIDDEN IN FRAME {&FRAME-NAME} = TRUE. */
/*       ELSE ENABLE BTN_SALDO WITH FRAME {&FRAME-NAME}.                              */
   END.  
   ELSE DO:
      BTN_SALDO:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
   END.         
   IF Guru.Konstanter:globforetag = "LULE" OR Guru.Konstanter:globforetag = "BODE" OR Guru.Konstanter:globforetag = "ELPA" THEN DO:
      ENABLE BTN_PRIS WITH FRAME {&FRAME-NAME}.
   END.
   ELSE DO:
      BTN_PRIS:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
   END.
   IF Guru.Konstanter:mtrlsekvar[6] = TRUE THEN BTN_PRIS:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
   RAD_LAGER = 1.
   IF RAD_LAGER = 1 THEN RUN setcolsortvar_UI IN brwproc[4] (INPUT "LAGER = TRUE").
   ELSE RUN setcolsortvar_UI IN brwproc[4] (INPUT "LAGER = FALSE").
   RUN openbdynspec_UI IN brwproc[4].
   RUN title_UI IN brwproc[4].
  /* RELEASE spec_mtrl NO-ERROR.   */
   GET FIRST BRW_MTRL.
   IF AVAILABLE spec_mtrl THEN DO:
      RUN setlastrowid_UI IN brwproc[4] (INPUT ROWID(spec_mtrl)).
      RUN lastselectdyn_UI IN brwproc[4].   
   END.
   ASSIGN   
   CMB_LEV:HIDDEN = FALSE
   BTN_LEV:HIDDEN = TRUE   
   FILL-IN-FACK:HIDDEN = TRUE. 
   IF depatitta = TRUE THEN DO:
      DISABLE FBTN_BEST WITH FRAME {&FRAME-NAME}.
      DISABLE FBTN_BYT WITH FRAME {&FRAME-NAME}.
      DISABLE BTN_UPPANT WITH FRAME {&FRAME-NAME}.
      DISABLE BTN_SALDO WITH FRAME {&FRAME-NAME}.
      DISABLE BTN_PRIS WITH FRAME {&FRAME-NAME}.
      DISABLE BTN_SPEC WITH FRAME {&FRAME-NAME}.
   END.
   {musarrow.i}   
   {WIN_M_SLUT.I}
   {KRYSSBORT.I}
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
   /*RUN DYNBRW.P PERSISTENT SET brwproc[1]
      (INPUT BRW_HLEV:HANDLE IN FRAME {&FRAME-NAME}).   */
   RUN DYNBRW.P PERSISTENT SET brwproc[4]
      (INPUT BRW_MTRL:HANDLE IN FRAME {&FRAME-NAME}). 
   RUN dynprogextra IN brwproc[4] (INPUT "rowdispextra_UI",INPUT THIS-PROCEDURE).
   RUN rowdispextrakor IN  brwproc[4] (INPUT TRUE).   
   RUN DYNBRW.P PERSISTENT SET brwproc[1]
    (INPUT BRW_HLEV:HANDLE IN FRAME {&FRAME-NAME}). 
   RUN dynprogextra IN brwproc[1] (INPUT "rowdispextra_UI",INPUT THIS-PROCEDURE).
   RUN rowdispextrakor IN  brwproc[1] (INPUT TRUE).   

   RUN DYNARROW.P PERSISTENT SET brwproc[{&ARROWS}]
      (INPUT BRW_HLEV:HANDLE, INPUT BRW_MTRL:HANDLE ,
       INPUT ?, INPUT ? ,
       INPUT ?, INPUT ?).

   /*
   RUN DYNARROW.P PERSISTENT SET brwproc[5] 
      (INPUT BRW_HLEV:HANDLE, INPUT BRW_MTRL:HANDLE ,
       INPUT ?, INPUT ?,
       INPUT ? , INPUT ?).
       */
   RUN setdefaultcol_UI IN brwproc[4] (INPUT 2).
   RUN colsortdynbrw_UI IN brwproc[4] (INPUT "").   
   IF Guru.Konstanter:appcon THEN DO:
      RUN MTRLBAPP.P PERSISTENT SET mtrlbapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN MTRLBAPP.P PERSISTENT SET mtrlbapph.
   END.      
   IF Guru.Konstanter:appcon THEN DO:
      RUN NETTOMARK.P PERSISTENT SET nettoh ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN NETTOMARK.P PERSISTENT SET nettoh.
   END. 
   /*Anders Olsson Elpool i Umeå AB  7 okt 2015 11:32:27
   LULE VILL INTE HA DETTA 
   RUN addfillindes_UI IN brwproc[4] (INPUT TRUE). 
   */
   

   RUN addfillin_UI IN brwproc[4] 
      (INPUT FILL-IN-DENR:HANDLE IN FRAME {&FRAME-NAME}, INPUT "ENR").
   RUN addfillin_UI IN brwproc[4] 
      (INPUT FILL-IN-DBEN:HANDLE IN FRAME {&FRAME-NAME}, INPUT "BENAMNING").
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnback_UI WINDOW-1 
PROCEDURE btnback_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/   
   ASSIGN
   antal_valda = BRW_MTRL:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME}
   antal_raknare = 1.
   DO WHILE antal_raknare LE antal_valda:                                   
      status-ok = BRW_MTRL:FETCH-SELECTED-ROW(antal_raknare). 
      IF Guru.Konstanter:globforetag = "GKAL" THEN DO:
         IF spec_mtrl.SALDO > 0 THEN DO:
            MESSAGE "Ni kan inte ta bort artikel " + spec_mtrl.ENR + " då saldot är större än 0."
            VIEW-AS ALERT-BOX TITLE "Meddelande".
         END.
         ELSE DO:      
            MESSAGE "Vill du ta bort " + LC(Guru.Konstanter:genk) + ":" + spec_mtrl.ENR + " med Benämningen:" + spec_mtrl.BENAMNING +
            " från depån?"
            VIEW-AS ALERT-BOX
            QUESTION BUTTONS YES-NO TITLE "Borttag" UPDATE svar.         
            IF svar THEN DO:    
               FIND FIRST aspec_mtrl WHERE aspec_mtrl.ENR = spec_mtrl.ENR AND aspec_mtrl.LEVKOD = spec_mtrl.LEVKOD NO-ERROR.
               IF NOT AVAILABLE aspec_mtrl THEN DO:                
                  CREATE aspec_mtrl.
                  ASSIGN aspec_mtrl.NY = FALSE
                  aspec_mtrl.ANDRAD = FALSE
                  aspec_mtrl.BORT = FALSE.
               END.
               BUFFER-COPY spec_mtrl TO aspec_mtrl.
               ASSIGN aspec_mtrl.NY = FALSE
               aspec_mtrl.ANDRAD = FALSE
               aspec_mtrl.BORT = TRUE.
               DELETE spec_mtrl.
            END.
            ELSE DO:
               musz = musz.
            END.      
         END.
      END.
      ELSE IF Guru.Konstanter:globforetag = "ELPA" OR Guru.Konstanter:globforetag = "LULE" OR Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "BODE" THEN DO:
         IF spec_mtrl.SALDO > 0 THEN DO:
            IF Guru.Konstanter:globforetag = "ELPA"  THEN DO:
               RUN KOMMSVINN.W (INPUT ROWID(spec_mtrl),OUTPUT bkomm).
               IF musz = TRUE THEN musz = FALSE.
               ELSE DO:               
                   CREATE svinn_temp.
                   ASSIGN
                   svinn_temp.ENR = spec_mtrl.ENR
                   SUBSTRING(svinn_temp.BENAMNING,1,59) = spec_mtrl.BENAMNING
                   svinn_temp.ENHET = spec_mtrl.ENHET
                   svinn_temp.PRIS = spec_mtrl.NPRIS       
                   svinn_temp.LEV = spec_mtrl.LEV      
                   svinn_temp.ANTAL = spec_mtrl.SALDO
                   svinn_temp.UTTAG = TRUE.                    
                   SUBSTRING(svinn_temp.BENAMNING,60) = bkomm.
                   FIND FIRST aspec_mtrl WHERE aspec_mtrl.ENR = spec_mtrl.ENR AND aspec_mtrl.LEVKOD = spec_mtrl.LEVKOD NO-ERROR.
                   IF NOT AVAILABLE aspec_mtrl THEN DO:                
                      CREATE aspec_mtrl. 
                      ASSIGN aspec_mtrl.NY = FALSE
                      aspec_mtrl.ANDRAD = FALSE
                      aspec_mtrl.BORT = FALSE.                   
                   END.
                   BUFFER-COPY spec_mtrl TO aspec_mtrl.
                   ASSIGN aspec_mtrl.NY = FALSE
                   aspec_mtrl.ANDRAD = FALSE
                   aspec_mtrl.BORT = TRUE.
                   DELETE spec_mtrl.                 
                END.             
            END.
            ELSE DO:            
               MESSAGE "Vill du ta bort " + LC(Guru.Konstanter:genk) + ":" + spec_mtrl.ENR + " med Benämningen:" + spec_mtrl.BENAMNING +
                " från depån? Saldot är större än 0 och en minus korrigering kommer att skapas."
                VIEW-AS ALERT-BOX
                QUESTION BUTTONS YES-NO TITLE "Borttag" UPDATE svar.         
                IF svar THEN DO:    
                   CREATE svinn_temp.
                   ASSIGN
                   svinn_temp.ENR = spec_mtrl.ENR
                   SUBSTRING(svinn_temp.BENAMNING,1,59) = spec_mtrl.BENAMNING
                   svinn_temp.ENHET = spec_mtrl.ENHET
                   svinn_temp.PRIS = spec_mtrl.NPRIS       
                   svinn_temp.LEV = spec_mtrl.LEV      
                   svinn_temp.ANTAL = spec_mtrl.SALDO
                   svinn_temp.UTTAG = TRUE.                      
                   FIND FIRST aspec_mtrl WHERE aspec_mtrl.ENR = spec_mtrl.ENR AND aspec_mtrl.LEVKOD = spec_mtrl.LEVKOD NO-ERROR.
                   IF NOT AVAILABLE aspec_mtrl THEN DO:                
                      CREATE aspec_mtrl. 
                      ASSIGN aspec_mtrl.NY = FALSE
                      aspec_mtrl.ANDRAD = FALSE
                      aspec_mtrl.BORT = FALSE.                   
                   END.
                   BUFFER-COPY spec_mtrl TO aspec_mtrl.
                   ASSIGN aspec_mtrl.NY = FALSE
                   aspec_mtrl.ANDRAD = FALSE
                   aspec_mtrl.BORT = TRUE.
                   DELETE spec_mtrl.                 
                END.             
            END.
         END.
         ELSE DO:
             MESSAGE "Vill du ta bort " + LC(Guru.Konstanter:genk) + ":" + spec_mtrl.ENR + " med Benämningen:" + spec_mtrl.BENAMNING +
             " från depån?"
             VIEW-AS ALERT-BOX
             QUESTION BUTTONS YES-NO TITLE "Borttag" UPDATE svar.         
             IF svar THEN DO:    
                FIND FIRST aspec_mtrl WHERE aspec_mtrl.ENR = spec_mtrl.ENR AND aspec_mtrl.LEVKOD = spec_mtrl.LEVKOD NO-ERROR.
                IF NOT AVAILABLE aspec_mtrl THEN DO:                
                   CREATE aspec_mtrl.
                   ASSIGN aspec_mtrl.NY = FALSE
                   aspec_mtrl.ANDRAD = FALSE
                   aspec_mtrl.BORT = FALSE.
                END.
                BUFFER-COPY spec_mtrl TO aspec_mtrl.
                ASSIGN aspec_mtrl.NY = FALSE
                aspec_mtrl.ANDRAD = FALSE
                aspec_mtrl.BORT = TRUE.
                DELETE spec_mtrl.
             END.             
         END.
      END.
      ELSE DO:
          MESSAGE "Vill du ta bort " + LC(Guru.Konstanter:genk) + ":" + spec_mtrl.ENR + " med Benämningen:" + spec_mtrl.BENAMNING +
          " från depån?"
          VIEW-AS ALERT-BOX
          QUESTION BUTTONS YES-NO TITLE "Borttag" UPDATE svar.         
          IF svar THEN DO:    
             FIND FIRST aspec_mtrl WHERE aspec_mtrl.ENR = spec_mtrl.ENR AND aspec_mtrl.LEVKOD = spec_mtrl.LEVKOD NO-ERROR.
             IF NOT AVAILABLE aspec_mtrl THEN DO:                
                CREATE aspec_mtrl.
                ASSIGN aspec_mtrl.NY = FALSE
                aspec_mtrl.ANDRAD = FALSE
                aspec_mtrl.BORT = FALSE.
             END.
             BUFFER-COPY spec_mtrl TO aspec_mtrl.
             ASSIGN aspec_mtrl.NY = FALSE
             aspec_mtrl.ANDRAD = FALSE
             aspec_mtrl.BORT = TRUE.
             DELETE spec_mtrl.
          END.             
      END.
      RUN valrattright_UI IN brwproc[4] (INPUT antal_raknare,INPUT svar,INPUT ROWID(spec_mtrl)).

      antal_raknare = antal_raknare + 1.  
   END.       
   IF RAD_LAGER = 1 THEN RUN setcolsortvar_UI IN brwproc[4] (INPUT "LAGER = TRUE").
   ELSE RUN setcolsortvar_UI IN brwproc[4] (INPUT "LAGER = FALSE").
   RUN openbdynspec_UI IN brwproc[4].   
   RUN lastselectdyn_UI IN brwproc[4].
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnbyt_UI WINDOW-1 
PROCEDURE btnbyt_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/    
   {muswait.i} 
   {AVBGOM.I}  
   MESSAGE "Vill du byta leverantör på alla atiklar svara Ja, om inte görs bytet på markerad(e) artiklar"
   VIEW-AS ALERT-BOX
   QUESTION BUTTONS YES-NO-CANCEL TITLE "Byte leverantör" UPDATE svar.         
   IF svar THEN DO:            
      RUN BYTLEVV.W (INPUT svar, INPUT ?, INPUT ?). 
   END.
   ELSE IF NOT svar THEN DO:       
      ASSIGN
      antal_valda = BRW_MTRL:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME}
      antal_raknare = 1.
      DO WHILE antal_raknare LE antal_valda:                                   
         status-ok = BRW_MTRL:FETCH-SELECTED-ROW(antal_raknare). 
         RUN BYTLEVV.W (INPUT svar, INPUT spec_mtrl.ENR, INPUT spec_mtrl.LEVKOD).
         antal_raknare = antal_raknare + 1.   
      END.
   END.
   ELSE musz = musz.
   {AVBFRAM.I}   
   IF RAD_LAGER = 1 THEN RUN setcolsortvar_UI IN brwproc[4] (INPUT "LAGER = TRUE").
   ELSE RUN setcolsortvar_UI IN brwproc[4] (INPUT "LAGER = FALSE").
   RUN openbdynspec_UI IN brwproc[4].   
   {musarrow.i}      
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnpris_UI WINDOW-1 
PROCEDURE btnpris_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/      
   IF AVAILABLE spec_mtrl THEN DO:   
      ASSIGN
      antal_valda = BRW_MTRL:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME}
      antal_raknare = 1.    
      DO WHILE antal_raknare LE antal_valda:   
         status-ok = BRW_MTRL:FETCH-SELECTED-ROW(antal_raknare).  
         reprowid = ROWID(spec_mtrl).
         
         RUN PRISDEP.W (INPUT ROWID(spec_mtrl)).
         
         antal_raknare = antal_raknare + 1.   
      END.  
      /*OPEN QUERY BRW_MTRL FOR EACH spec_mtrl NO-LOCK.*/
      IF RAD_LAGER = 1 THEN RUN setcolsortvar_UI IN brwproc[4] (INPUT "LAGER = TRUE").
      ELSE RUN setcolsortvar_UI IN brwproc[4] (INPUT "LAGER = FALSE").
      RUN openbdynspec_UI IN brwproc[4].   
      FIND FIRST spec_mtrl WHERE ROWID(spec_mtrl) = reprowid NO-LOCK NO-ERROR.
      IF AVAILABLE spec_mtrl THEN DO:
         RUN specand_UI.
         RUN setlastrowid_UI IN brwproc[4] (INPUT ROWID(spec_mtrl)).
         RUN lastselectdyn_UI IN brwproc[4].        
      END. 
      
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnsaldo_UI WINDOW-1 
PROCEDURE btnsaldo_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/      
   IF AVAILABLE spec_mtrl THEN DO:   
      ASSIGN
      antal_valda = BRW_MTRL:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME}
      antal_raknare = 1.    
      DO WHILE antal_raknare LE antal_valda:   
         status-ok = BRW_MTRL:FETCH-SELECTED-ROW(antal_raknare).  
         reprowid = ROWID(spec_mtrl).
         
         RUN SALDOV.W (INPUT ROWID(spec_mtrl)).
         
         antal_raknare = antal_raknare + 1.   
      END.  
      /*OPEN QUERY BRW_MTRL FOR EACH spec_mtrl NO-LOCK.*/
      IF RAD_LAGER = 1 THEN RUN setcolsortvar_UI IN brwproc[4] (INPUT "LAGER = TRUE").
      ELSE RUN setcolsortvar_UI IN brwproc[4] (INPUT "LAGER = FALSE").
      RUN openbdynspec_UI IN brwproc[4].   
      FIND FIRST spec_mtrl WHERE ROWID(spec_mtrl) = reprowid NO-LOCK NO-ERROR.
      IF AVAILABLE spec_mtrl THEN DO:
         RUN specand_UI.
         RUN setlastrowid_UI IN brwproc[4] (INPUT ROWID(spec_mtrl)).
         RUN lastselectdyn_UI IN brwproc[4].        
      END. 
      
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
  DISPLAY RAD_LAGER CMB_LEV FILL-IN-ENR FILL-IN-DENR FILL-IN-DBEN FILL-IN-BEN 
          FILL-IN-FACKID1 FILL-IN-FACK FILL-IN-FACKID2 RAD_SOK FILL-IN-SOKALT 
      WITH FRAME FRAME-A IN WINDOW WINDOW-1.
  ENABLE RAD_LAGER RECT-2 RECT-4 IMAGE-13 IMAGE-14 BRW_HLEV BRW_MTRL BTN_OVER 
         BTN_BACK CMB_LEV BTN_LEV BTN_UPPANT BTN_SPEC FBTN_BYT FBTN_VISA 
         FILL-IN-ENR FILL-IN-DENR FILL-IN-DBEN FILL-IN-BEN FILL-IN-FACKID1 
         FILL-IN-FACK FILL-IN-FACKID2 RAD_SOK FBTN_BEST BTN_AVB FILL-IN-SOKALT 
      WITH FRAME FRAME-A IN WINDOW WINDOW-1.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE huvud_UI WINDOW-1 
PROCEDURE huvud_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DO TRANSACTION:
      /*FIND DEPA WHERE DEPA.DEP-NR = vald_depa NO-LOCK NO-ERROR.         */
      IF musz = TRUE THEN musz = musz.   
      ELSE DO:                   
         CREATE tidut. 
         ASSIGN        
         SUBSTRING(tidut.UT,1) = "LAGER LISTA FÖR DEPÅ"
         SUBSTRING(tidut.UT,28) = dbenamning
         SUBSTRING(tidut.UT,50) = STRING(TODAY)
         SUBSTRING(tidut.UT,60) = STRING(TIME,"HH:MM:SS").                                                               
         CREATE tidut.
         IF colnamevar = "FACKID" THEN DO:      
            SUBSTRING(tidut.UT,1) = "SORTERAT PÅ FACK-ID".
         END.                   
         ELSE IF colnamevar = "ENR" THEN DO: 
            SUBSTRING(tidut.UT,1) = "SORTERAT PÅ " + CAPS(Guru.Konstanter:genk).
         END.                                       
         ELSE IF colnamevar = "BENAMNING" THEN DO:      
            SUBSTRING(tidut.UT,1) = "SORTERAT PÅ BENÄMNING".
         END. 
         ELSE IF colnamevar = "LEVKOD" THEN DO:      
            SUBSTRING(tidut.UT,1) = "SORTERAT PÅ LEVERANTÖR".
         END.                 
         CREATE tidut.                                    
         CREATE tidut.
      END.
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
   DEFINE INPUT PARAMETER vber AS INTEGER NO-UNDO.
   DEFINE VARIABLE valenr AS CHARACTER NO-UNDO.
   DEFINE VARIABLE levnamnvar AS CHARACTER NO-UNDO.
   IF vber = 1 THEN DO:
      status-ok = BRW_HLEV:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}  NO-ERROR.
      IF status-ok THEN DO:
         valenr = mtrltemp.ENR.
         FIND FIRST levtemp WHERE levtemp.LEVKOD = mtrltemp.LEVKOD
         NO-LOCK NO-ERROR.      
      END.
   END.
   IF vber = 2 THEN DO:
      status-ok = BRW_MTRL:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}  NO-ERROR.
      IF status-ok THEN DO:
         valenr = spec_mtrl.ENR.
         FIND FIRST levtemp WHERE levtemp.LEVKOD = spec_mtrl.LEVKOD
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
      RUN sokhmt_UI IN  brwproc[1] (INPUT TABLE valsoktemp).  
   END.
   IF vad = 2 THEN DO:
     orgfraga = " WHERE KALKNR = " + STRING(0) + " AND LEVKOD = '" + STRING(vald_lev) + "' USE-INDEX LEV".
     tth = TEMP-TABLE mtrltemp:HANDLE.
     EMPTY TEMP-TABLE valsoktemp NO-ERROR. 
     CREATE valsoktemp.
     ASSIGN 
     valsoktemp.SOKCHAR[1] = "MTRL"     /*Skarp tabell*/
     valsoktemp.SOKCHAR[2] = orgfraga   /*Öppningsquery*/
     valsoktemp.SOKCHAR[3] = "ENR" /*sökfält*/
     valsoktemp.SOKCHAR[4] = "MTRLROW"  /*temptabells faltnamn för rowid*/
     valsoktemp.SOKCHAR[5] = sokpa.      /*sök på*/       
     RUN sokhmt_UI IN  brwproc[1] (INPUT TABLE valsoktemp).  
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE klar_UI WINDOW-1 
PROCEDURE klar_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   &Scoped-define BROWSE-NAME BRW_MTRL
   DEBUGGER:SET-BREAK().
   GET FIRST {&BROWSE-NAME}  NO-LOCK.
   DO WHILE AVAILABLE(spec_mtrl):
      FIND FIRST levtemp WHERE levtemp.LEVKOD = spec_mtrl.LEV NO-LOCK NO-ERROR.                        
      CREATE tidut.      
      IF colnamevar = "FACKID" THEN DO:      
         ASSIGN
         SUBSTRING(tidut.UT,1) = spec_mtrl.FACKID  
         SUBSTRING(tidut.UT,10) = spec_mtrl.ENR
         SUBSTRING(tidut.UT,22) = SUBSTRING(spec_mtrl.BENAMNING,1,30).         
      END.
      ELSE IF colnamevar = "ENR" THEN DO:      
         ASSIGN
         SUBSTRING(tidut.UT,1) = spec_mtrl.ENR
         SUBSTRING(tidut.UT,13) = spec_mtrl.FACKID  
         SUBSTRING(tidut.UT,22) = SUBSTRING(spec_mtrl.BENAMNING,1,30).
      END.
      ELSE IF colnamevar = "BENAMNING" THEN DO:      
         ASSIGN
         SUBSTRING(tidut.UT,1) = SUBSTRING(spec_mtrl.BENAMNING,1,30)                    
         SUBSTRING(tidut.UT,27) = spec_mtrl.FACKID  
         SUBSTRING(tidut.UT,36) = spec_mtrl.ENR. 
      END.
      ELSE IF colnamevar = "LEVKOD" THEN DO:      
         ASSIGN
         SUBSTRING(tidut.UT,1) = spec_mtrl.LEVKOD
         SUBSTRING(tidut.UT,13) = spec_mtrl.ENR  
         SUBSTRING(tidut.UT,22) = SUBSTRING(spec_mtrl.BENAMNING,1,30).
      END.
      ELSE DO:      
         ASSIGN
         SUBSTRING(tidut.UT,1) = spec_mtrl.ENR
         SUBSTRING(tidut.UT,13) = spec_mtrl.FACKID  
         SUBSTRING(tidut.UT,22) = SUBSTRING(spec_mtrl.BENAMNING,1,30).
      END.
      RUN mtrl_UI.       
      GET NEXT {&BROWSE-NAME} NO-LOCK.
   END.              

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE msdbclick_UI WINDOW-1 
PROCEDURE msdbclick_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/    
   {muswait.i}
   
   RUN UPPANT2V.W (INPUT ROWID(spec_mtrl)). 
   
   RUN refreshbrw_UI IN brwproc[4].
   {musarrow.i}  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE mtrl_UI WINDOW-1 
PROCEDURE mtrl_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*SUBSTRING(tidut.UT,58) = "ENHET"
   SUBSTRING(tidut.UT,64) = "PRIS"
   SUBSTRING(tidut.UT,74) = "SALDO"       
   SUBSTRING(tidut.UT,80) = "LEVERANTÖR"
   SUBSTRING(tidut.UT,91) = "PUNKT"
   SUBSTRING(tidut.UT,96) = "KVANT".*/
   ASSIGN
   SUBSTRING(tidut.UT,58) = spec_mtrl.ENHET.
      
   IF Guru.Konstanter:mtrlsekvar[6] = FALSE THEN SUBSTRING(tidut.UT,61) = STRING(spec_mtrl.NPRIS,">>>>>9.99").
   SUBSTRING(tidut.UT,72) = STRING(spec_mtrl.SALDO). 
   IF AVAILABLE levtemp THEN SUBSTRING(tidut.UT,78) = SUBSTRING(levtemp.LEVNAMN,1,10).                                        
   ELSE SUBSTRING(tidut.UT,78) = "Lev.Saknas".
   ASSIGN
   SUBSTRING(tidut.UT,89) = STRING(spec_mtrl.BESTPUNKT)
   SUBSTRING(tidut.UT,95) = STRING(spec_mtrl.BESTKVANT).
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
   FIND FIRST spec_mtrl WHERE spec_mtrl.ENR = mtrltemp.ENR 
   NO-LOCK NO-ERROR. 
   IF AVAILABLE spec_mtrl THEN DO:      
      MESSAGE CAPS(Guru.Konstanter:genk) + ":" STRING(spec_mtrl.ENR) "finns redan upplagt i depån." 
      VIEW-AS ALERT-BOX TITLE "Meddelande".
      reprowid = ROWID(spec_mtrl).
   END.      
   ELSE DO:
      IF Guru.Konstanter:globforetag = "ELPA" OR Guru.Konstanter:globforetag = "SOLE" 
        OR Guru.Konstanter:globforetag = "GKAL" OR 
      Guru.Konstanter:globforetag = "LULE" OR Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "BORL" OR Guru.Konstanter:globforetag = "BODE" THEN DO: 
          
         FIND FIRST ej_mtrl WHERE ej_mtrl.ENR = mtrltemp.ENR NO-LOCK NO-ERROR.
         IF AVAILABLE ej_mtrl THEN DO: 
            CREATE spec_mtrl. 
            ASSIGN 
            spec_mtrl.DEPNR = ej_mtrl.DEPNR     
            spec_mtrl.ENR = ej_mtrl.ENR
            spec_mtrl.BENAMNING = ej_mtrl.BENAMNING
            spec_mtrl.ENHET = ej_mtrl.ENHET
            spec_mtrl.BESTKVANT = ej_mtrl.BESTKVANT
            spec_mtrl.BESTPUNKT = ej_mtrl.BESTPUNKT
            spec_mtrl.NPRIS = ej_mtrl.NPRIS  
            spec_mtrl.BPRIS = ej_mtrl.BPRIS 
            spec_mtrl.LEVKOD = ej_mtrl.LEV 
            spec_mtrl.FACKID = ej_mtrl.FACKID  
            spec_mtrl.INVANT = ej_mtrl.INVANT
            spec_mtrl.OMSATT = ej_mtrl.OMSATT    
            spec_mtrl.SALDO = ej_mtrl.SALDO.
            IF RAD_LAGER = 1 THEN spec_mtrl.LAGER = TRUE.
            ELSE spec_mtrl.LAGER = FALSE.            
            FIND FIRST aspec_mtrl WHERE aspec_mtrl.ENR = spec_mtrl.ENR AND aspec_mtrl.LEVKOD = spec_mtrl.LEVKOD NO-ERROR.
            IF AVAILABLE aspec_mtrl THEN DO:
               /*för att få in ett nytt pris i depån gör de först borttag av enr och lägger sedan dit det
               med rätt pris från katalog*/
               IF aspec_mtrl.BORT = TRUE  THEN DO:
                  CREATE aspec_mtrl.
                  ASSIGN aspec_mtrl.NY = FALSE
                  aspec_mtrl.ANDRAD = FALSE
                  aspec_mtrl.BORT = FALSE.
               END.
            END.
            IF NOT AVAILABLE aspec_mtrl THEN DO:                
               CREATE aspec_mtrl.
               ASSIGN aspec_mtrl.NY = FALSE
               aspec_mtrl.ANDRAD = FALSE
               aspec_mtrl.BORT = FALSE.
            END.
            
            BUFFER-COPY spec_mtrl TO aspec_mtrl.
            ASSIGN aspec_mtrl.NY = TRUE 
            aspec_mtrl.ANDRAD = FALSE
            aspec_mtrl.BORT = FALSE.
            musz = FALSE.
            
            RUN UPPANT2V.W (INPUT ROWID(spec_mtrl)).
            
            musz = TRUE.
         END.
         ELSE DO:
            FIND FIRST spec_mtrl WHERE spec_mtrl.ENR = mtrltemp.ENR AND 
            spec_mtrl.BESTKVANT NE 0 NO-LOCK NO-ERROR.
            IF NOT AVAILABLE spec_mtrl THEN DO:                   
                
               RUN UPPANT2V.W (INPUT ROWID(mtrltemp)).
               
            END.              
         END.
      END.
      ELSE DO:        
         FIND FIRST spec_mtrl WHERE spec_mtrl.ENR = mtrltemp.ENR AND 
         spec_mtrl.BESTKVANT NE 0 NO-LOCK NO-ERROR.
         IF NOT AVAILABLE spec_mtrl THEN DO:                
            
             
            RUN UPPANT2V.W (INPUT ROWID(mtrltemp)).
            
         END.                     
      END. 
   
   END.   
   antal_raknare = antal_raknare + 1.         
   IF varforetypval[29] = 1  THEN DO:                 
      FIND FIRST ikmtrltemp WHERE ikmtrltemp.LEVKOD = mtrltemp.LEVKOD AND ikmtrltemp.ENR = mtrltemp.ENR AND ikmtrltemp.KALKNR = 0
      NO-LOCK NO-ERROR.
      IF NOT AVAILABLE ikmtrltemp THEN DO:                        
         CREATE ikmtrltemp.
         BUFFER-COPY mtrltemp TO ikmtrltemp.                  
      END.
   END.
    
   /*
   tth = TEMP-TABLE mtrltemp:HANDLE. 
   MESSAGE "1 " STRING(tth) VIEW-AS ALERT-BOX.
   */
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
   IF brwh:NAME = "BRW_MTRL" THEN RUN rowdispmtrl_UI (INPUT 2).    
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
            mtrltemp.ENR:BGCOLOR IN BROWSE BRW_HLEV = varforetypval[28]
            mtrltemp.NPRIS:BGCOLOR IN BROWSE BRW_HLEV = varforetypval[28].
         END.
      END.
   END.   
   IF vad = 2  THEN DO:
      IF AVAILABLE spec_mtrl THEN DO:         
         FIND FIRST ikmtrltemp WHERE ikmtrltemp.LEVKOD = spec_mtrl.LEVKOD AND ikmtrltemp.ENR = spec_mtrl.ENR AND ikmtrltemp.KALKNR = 0
         AND ikmtrltemp.KUND = TRUE  NO-LOCK NO-ERROR.
         IF AVAILABLE ikmtrltemp THEN DO:                     
            ASSIGN
            spec_mtrl.ENR:BGCOLOR IN BROWSE BRW_MTRL = varforetypval[28].            
         END.
      END.

   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE rubrik_UI WINDOW-1 
PROCEDURE rubrik_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/  
   CREATE tidut. 
   ASSIGN      
   SUBSTRING(tidut.UT,89) = "BEST"
   SUBSTRING(tidut.UT,95) = "BEST".
   IF colnamevar = "FACKID" THEN DO:         
      ASSIGN
      str=                                                                    
"========.===========.===================================.===.=========.=====.==========.=====.=====.".                        
      CREATE tidut.          
      ASSIGN  
      SUBSTRING(tidut.UT,1) = "FACK-ID"         
      SUBSTRING(tidut.UT,10) = CAPS(Guru.Konstanter:genk)                                                                       
      SUBSTRING(tidut.UT,22) = "BENÄMNING".  
   END.            
   ELSE IF colnamevar = "ENR" THEN DO: 
      ASSIGN
      str=                                                                    
"===========.========.===================================.===.=========.=====.==========.=====.=====.".                  
      CREATE tidut.        
      ASSIGN
      SUBSTRING(tidut.UT,1) = CAPS(Guru.Konstanter:genk)      
      SUBSTRING(tidut.UT,13) = "FACK-ID"                                                                        
      SUBSTRING(tidut.UT,22) = "BENÄMNING".
   END.            
   ELSE IF colnamevar = "BENAMNING" THEN DO:      
      ASSIGN
      str=                                                                    
"===================================.========.===========.===.=========.=====.==========.=====.=====.".                  
      CREATE tidut.              
      ASSIGN
      SUBSTRING(tidut.UT,1) = "BENÄMNING"      
      SUBSTRING(tidut.UT,37) = "FACK-ID"             
      SUBSTRING(tidut.UT,46) = CAPS(Guru.Konstanter:genk).
   END. 
   ELSE IF colnamevar = "LEVKOD" THEN DO: 
      ASSIGN
      str=                                                                    
"===========.========.===================================.===.=========.=====.==========.=====.=====.".                  
      CREATE tidut.        
      ASSIGN
      SUBSTRING(tidut.UT,1) = "Lev-id"      
      SUBSTRING(tidut.UT,13) = CAPS(Guru.Konstanter:genk)                                                                        
      SUBSTRING(tidut.UT,22) = "BENÄMNING".
   END.            
   ELSE  DO: 
      ASSIGN
      str=                                                                    
"===========.========.===================================.===.=========.=====.==========.=====.=====.".                  
      CREATE tidut.        
      ASSIGN
      SUBSTRING(tidut.UT,1) = CAPS(Guru.Konstanter:genk)      
      SUBSTRING(tidut.UT,13) = "FACK-ID"                                                                        
      SUBSTRING(tidut.UT,22) = "BENÄMNING".
   END.            
    
   ASSIGN
   SUBSTRING(tidut.UT,58) = "ENH"
   SUBSTRING(tidut.UT,62) = "PRIS"
   SUBSTRING(tidut.UT,72) = "SALDO"       
   SUBSTRING(tidut.UT,78) = "LEVERANTÖR"
   SUBSTRING(tidut.UT,89) = "PUNKT"
   SUBSTRING(tidut.UT,95) = "KVANT".     
   
   /*SUBSTRING(tidut.UT,58) = "ENHET"
   SUBSTRING(tidut.UT,64) = "PUNKT"
   SUBSTRING(tidut.UT,70) = "KVANT"
   SUBSTRING(tidut.UT,76) = "LEVERANTÖR"
   SUBSTRING(tidut.UT,87) = "PRIS"  64
   SUBSTRING(tidut.UT,97) = "SALDO"       
   
   
   .*/
   
                                   
   CREATE tidut.       
   SUBSTRING(tidut.UT,1) = str.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE specand_UI WINDOW-1 
PROCEDURE specand_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/       
   FIND FIRST aspec_mtrl WHERE aspec_mtrl.ENR = spec_mtrl.ENR AND aspec_mtrl.LEVKOD = spec_mtrl.LEVKOD NO-ERROR.
   IF NOT AVAILABLE aspec_mtrl THEN DO:   
      CREATE aspec_mtrl.
      ASSIGN aspec_mtrl.NY = FALSE
      aspec_mtrl.ANDRAD = FALSE
      aspec_mtrl.BORT = FALSE.
   END.      
   BUFFER-COPY spec_mtrl TO aspec_mtrl.
   ASSIGN aspec_mtrl.ANDRAD = TRUE.   
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
   IF AVAILABLE spec_mtrl THEN DO:
      spec_mtrl.BENAMNING:FORMAT IN BROWSE BRW_MTRL = "X(256)".
      DISPLAY
      spec_mtrl.BENAMNING 
      spec_mtrl.BESTKVANT 
      spec_mtrl.BESTPUNKT 
      spec_mtrl.OMSATT    
      WITH BROWSE BRW_MTRL.         
   END.  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

