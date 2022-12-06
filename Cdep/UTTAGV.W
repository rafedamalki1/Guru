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

/* Local Variable Definitions ---                                       */
{ALLDEF.I}
{INKMTRLDEP.I}
&Scoped-define NEW                             
{GLOBVAR2DEL1.I}
DEFINE NEW SHARED VARIABLE nytt_bestnr2 AS INTEGER NO-UNDO. 
DEFINE NEW SHARED VARIABLE fakt AS LOGICAL NO-UNDO.    
DEFINE NEW SHARED VARIABLE rekvar AS INTEGER NO-UNDO.             
DEFINE NEW SHARED VARIABLE foljesedel AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE depatitta AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE valaonr AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE valdelnr AS INTEGER NO-UNDO.   
DEFINE SHARED VARIABLE valort AS CHARACTER NO-UNDO. 
DEFINE SHARED VARIABLE valomrade AS CHARACTER NO-UNDO.     
DEFINE SHARED VARIABLE valkund AS CHARACTER NO-UNDO. 
DEFINE SHARED VARIABLE valnamn AS CHARACTER NO-UNDO. 
DEFINE SHARED VARIABLE akval AS INTEGER NO-UNDO.  
DEFINE SHARED VARIABLE datvar AS DATE NO-UNDO.
DEFINE SHARED VARIABLE vartpro AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE SHARED VARIABLE vart AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.  
DEFINE SHARED VARIABLE vald_depa AS INTEGER NO-UNDO.

DEFINE VARIABLE mtrlbapph AS HANDLE NO-UNDO.
DEFINE VARIABLE valknrvis AS CHARACTER NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE aosok AS CHARACTER FORMAT "X(40)" NO-UNDO.
DEFINE VARIABLE posok AS CHARACTER FORMAT "X(11)" NO-UNDO.
DEFINE VARIABLE antal_valda AS INTEGER NO-UNDO.
DEFINE VARIABLE antal_raknare AS INTEGER NO-UNDO. 
DEFINE VARIABLE mtrl_rowid AS ROWID NO-UNDO.
DEFINE VARIABLE lev AS CHARACTER NO-UNDO.  
DEFINE VARIABLE svar AS LOGICAL NO-UNDO.  
DEFINE VARIABLE fackvar AS CHARACTER NO-UNDO.  
DEFINE VARIABLE fackid AS CHARACTER FORMAT "X(8)" NO-UNDO.
DEFINE VARIABLE rad AS INTEGER NO-UNDO.
DEFINE VARIABLE summa AS INTEGER NO-UNDO.
DEFINE VARIABLE enrval AS LOGICAL NO-UNDO.
DEFINE VARIABLE antalvar AS INTEGER NO-UNDO.
DEFINE VARIABLE satsinn AS LOGICAL NO-UNDO.
DEFINE VARIABLE val_rec AS ROWID NO-UNDO.
DEFINE VARIABLE antink AS INTEGER NO-UNDO.
DEFINE VARIABLE logresult AS LOGICAL NO-UNDO.
DEFINE VARIABLE hamtapp AS HANDLE NO-UNDO.
DEFINE VARIABLE lkopp AS LOGICAL NO-UNDO.
DEFINE VARIABLE koppl AS LOGICAL NO-UNDO.
DEFINE VARIABLE felmed AS CHARACTER NO-UNDO.
DEFINE VARIABLE andant AS LOGICAL NO-UNDO.
DEFINE VARIABLE leveransaoapph AS HANDLE NO-UNDO.

&Scoped-define NEW 
&Scoped-define SHARED SHARED
{LEVTEMP.I}
{DEPATEMP.I}
{SPECMTRLTEMP.I}
{HOPPSEK2W.I}

{LAGERTRUM.I}
{AOBESTMTRLDEP.I}

&Scoped-define NEW NEW
&Scoped-define SHARED SHARED
{MTRLTEMP.I}
    
&Scoped-define NEW 
&Scoped-define SHARED SHARED 
{BESTMTRLDEP.I}    

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
&Scoped-define INTERNAL-TABLES mtrldeptemp spec_mtrl

/* Definitions for BROWSE BRW_DEPA                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_DEPA mtrldeptemp.FACKID mtrldeptemp.ENR ~
mtrldeptemp.BENAMNING mtrldeptemp.ENHET mtrldeptemp.SALDO mtrldeptemp.NPRIS 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_DEPA 
&Scoped-define QUERY-STRING-BRW_DEPA FOR EACH mtrldeptemp ~
      WHERE mtrldeptemp.DEPNR = vald_depa NO-LOCK ~
    BY mtrldeptemp.ENR
&Scoped-define OPEN-QUERY-BRW_DEPA OPEN QUERY BRW_DEPA FOR EACH mtrldeptemp ~
      WHERE mtrldeptemp.DEPNR = vald_depa NO-LOCK ~
    BY mtrldeptemp.ENR.
&Scoped-define TABLES-IN-QUERY-BRW_DEPA mtrldeptemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_DEPA mtrldeptemp


/* Definitions for BROWSE BRW_MTRL                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_MTRL spec_mtrl.ENR spec_mtrl.SALDO ~
spec_mtrl.NPRIS spec_mtrl.BENAMNING spec_mtrl.ENHET 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_MTRL spec_mtrl.SALDO ~
spec_mtrl.NPRIS 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_MTRL spec_mtrl
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_MTRL spec_mtrl
&Scoped-define QUERY-STRING-BRW_MTRL FOR EACH spec_mtrl NO-LOCK ~
    BY spec_mtrl.ENR
&Scoped-define OPEN-QUERY-BRW_MTRL OPEN QUERY BRW_MTRL FOR EACH spec_mtrl NO-LOCK ~
    BY spec_mtrl.ENR.
&Scoped-define TABLES-IN-QUERY-BRW_MTRL spec_mtrl
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_MTRL spec_mtrl


/* Definitions for FRAME FRAME-A                                        */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BTN_KLP FBTN_PRIS FBTN_FOLJ FBTN_FAKT ~
FBTN_VISA FBTN_SKRIV BTN_BEST BTN_AVB btn_over btn_back BTN_SOK FILL-IN-ENR ~
FILL-IN-BEN FILL-IN-FACK FILL-IN-FACKID1 FILL-IN-FACKID2 FILL-IN-ENR2 ~
FILL-IN-ANTAL BTN_UP BTN_MIN FILL-IN-SALDO BTN_SATS FBTN_HAONR BTN_BOTR ~
RECT-2 BRW_DEPA RECT-4 BRW_MTRL 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-ENR FILL-IN-BEN FILL-IN-FACK ~
FILL-IN-FACKID1 FILL-IN-FACKID2 FILL-IN-ENR2 FILL-IN-ANTAL FILL-IN-SALDO 

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

DEFINE BUTTON BTN_BOTR 
     LABEL "Boka trumma" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_KLP 
     LABEL "Koppla P-plats" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_MIN 
     LABEL "-" 
     SIZE 2.5 BY .75.

DEFINE BUTTON btn_over 
     IMAGE-UP FILE "BILDER\next-u":U
     LABEL "":L 
     SIZE 5.5 BY 1.75.

DEFINE BUTTON BTN_SATS 
     LABEL "Satsinformation":L 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_SOK 
     LABEL "Beställningar":L 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_UP 
     LABEL "+" 
     SIZE 2.5 BY .75.

DEFINE BUTTON FBTN_FAKT 
     LABEL "Faktura" 
     SIZE 14 BY 1
     FGCOLOR 1 .

DEFINE BUTTON FBTN_FOLJ 
     LABEL "Följesedel" 
     SIZE 14 BY 1
     FGCOLOR 1 .

DEFINE BUTTON FBTN_HAONR 
     LABEL "Hämta från aonr" 
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
     LABEL "Visa" 
     SIZE 14 BY 1
     FGCOLOR 1 .

DEFINE VARIABLE FILL-IN-ANTAL AS INTEGER FORMAT ">>>>>9":U INITIAL 0 
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

DEFINE VARIABLE FILL-IN-FACK AS CHARACTER FORMAT "X(8)" 
     LABEL "Sök på Fack" 
     VIEW-AS FILL-IN 
     SIZE 19 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN-FACKID1 AS CHARACTER FORMAT "X(2)":U 
     LABEL "Fack-id (aa)" 
     VIEW-AS FILL-IN 
     SIZE 3.5 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN-FACKID2 AS CHARACTER FORMAT "X(6)":U 
     LABEL "+ (999999)" 
     VIEW-AS FILL-IN 
     SIZE 7.63 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN-SALDO AS INTEGER FORMAT ">>>>>9":U INITIAL 0 
      VIEW-AS TEXT 
     SIZE 7.63 BY .67 NO-UNDO.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 48.5 BY 3.63
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 55 BY 3.63
     BGCOLOR 8 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_DEPA FOR 
      mtrldeptemp SCROLLING.

DEFINE QUERY BRW_MTRL FOR 
      spec_mtrl SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_DEPA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_DEPA WINDOW-1 _STRUCTURED
  QUERY BRW_DEPA NO-LOCK DISPLAY
      mtrldeptemp.FACKID FORMAT "X(8)":U
      mtrldeptemp.ENR FORMAT "X(11)":U
      mtrldeptemp.BENAMNING FORMAT "x(256)":U WIDTH 23
      mtrldeptemp.ENHET COLUMN-LABEL "Enh" FORMAT "x(4)":U
      mtrldeptemp.SALDO FORMAT ">>>>>9":U
      mtrldeptemp.NPRIS COLUMN-LABEL "Pris" FORMAT ">>>>99.99":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING MULTIPLE SIZE 48.5 BY 20
         TITLE "Materiellista för depå.".

DEFINE BROWSE BRW_MTRL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_MTRL WINDOW-1 _STRUCTURED
  QUERY BRW_MTRL NO-LOCK DISPLAY
      spec_mtrl.ENR FORMAT "X(11)":U
      spec_mtrl.SALDO COLUMN-LABEL "Antal" FORMAT ">>>,>>9":U
      spec_mtrl.NPRIS COLUMN-LABEL "Pris" FORMAT ">>>>99.99":U
      spec_mtrl.BENAMNING COLUMN-LABEL "Benämning" FORMAT "x(256)":U
            WIDTH 25
      spec_mtrl.ENHET COLUMN-LABEL "Enh" FORMAT "x(3)":U
  ENABLE
      spec_mtrl.SALDO
      spec_mtrl.NPRIS
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING MULTIPLE SIZE 55.25 BY 20
         TITLE "Materiel för uttag".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     BTN_KLP AT ROW 16 COL 111.5 WIDGET-ID 4
     FBTN_PRIS AT ROW 8 COL 111.5
     FBTN_FOLJ AT ROW 9.13 COL 111.5
     FBTN_FAKT AT ROW 10.25 COL 111.5
     FBTN_VISA AT ROW 11.38 COL 111.5
     FBTN_SKRIV AT ROW 12.5 COL 111.5
     BTN_BEST AT ROW 26.83 COL 111.25
     BTN_AVB AT ROW 27.92 COL 111.25
     btn_over AT ROW 7.08 COL 50.13
     btn_back AT ROW 10.58 COL 50.13
     BTN_SOK AT ROW 21.88 COL 6.63
     FILL-IN-ENR AT ROW 25.58 COL 15.5 COLON-ALIGNED
     FILL-IN-BEN AT ROW 26.54 COL 15.5 COLON-ALIGNED
     FILL-IN-FACK AT ROW 27.58 COL 15.5 COLON-ALIGNED
     FILL-IN-FACKID1 AT ROW 27.58 COL 15.5 COLON-ALIGNED
     FILL-IN-FACKID2 AT ROW 27.58 COL 32.13 COLON-ALIGNED
     FILL-IN-ENR2 AT ROW 25.58 COL 68.13 COLON-ALIGNED
     FILL-IN-ANTAL AT ROW 27 COL 68.13 COLON-ALIGNED
     BTN_UP AT ROW 26.54 COL 83.13
     BTN_MIN AT ROW 27.67 COL 83.13
     FILL-IN-SALDO AT ROW 23.54 COL 26 NO-LABEL
     BTN_SATS AT ROW 21.88 COL 25.13
     FBTN_HAONR AT ROW 13.63 COL 111.5 WIDGET-ID 2
     BTN_BOTR AT ROW 14.75 COL 111.5 WIDGET-ID 6
     BRW_DEPA AT ROW 1.5 COL 1.5
     BRW_MTRL AT ROW 1.5 COL 56
     "Saldo senast markerad:" VIEW-AS TEXT
          SIZE 22.38 BY .67 AT ROW 23.58 COL 2.88
     "Sök på:" VIEW-AS TEXT
          SIZE 9.75 BY .83 AT ROW 25.58 COL 2.25
     "Hämta:" VIEW-AS TEXT
          SIZE 7 BY .83 AT ROW 25.58 COL 56.63
     RECT-2 AT ROW 25.29 COL 1.5
     RECT-4 AT ROW 25.29 COL 56
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
/* BROWSE-TAB BRW_DEPA RECT-2 FRAME-A */
/* BROWSE-TAB BRW_MTRL RECT-4 FRAME-A */
ASSIGN 
       BRW_DEPA:HIDDEN  IN FRAME FRAME-A                = TRUE
       BRW_DEPA:MAX-DATA-GUESS IN FRAME FRAME-A         = 40000
       BRW_DEPA:ALLOW-COLUMN-SEARCHING IN FRAME FRAME-A = TRUE.

ASSIGN 
       BRW_MTRL:MAX-DATA-GUESS IN FRAME FRAME-A         = 40000
       BRW_MTRL:ALLOW-COLUMN-SEARCHING IN FRAME FRAME-A = TRUE.

ASSIGN 
       BTN_MIN:HIDDEN IN FRAME FRAME-A           = TRUE.

ASSIGN 
       BTN_UP:HIDDEN IN FRAME FRAME-A           = TRUE.

ASSIGN 
       FBTN_FAKT:HIDDEN IN FRAME FRAME-A           = TRUE.

ASSIGN 
       FILL-IN-ANTAL:HIDDEN IN FRAME FRAME-A           = TRUE.

ASSIGN 
       FILL-IN-BEN:HIDDEN IN FRAME FRAME-A           = TRUE.

ASSIGN 
       FILL-IN-ENR:HIDDEN IN FRAME FRAME-A           = TRUE.

ASSIGN 
       FILL-IN-ENR2:HIDDEN IN FRAME FRAME-A           = TRUE.

ASSIGN 
       FILL-IN-FACK:HIDDEN IN FRAME FRAME-A           = TRUE.

ASSIGN 
       FILL-IN-FACKID1:HIDDEN IN FRAME FRAME-A           = TRUE.

ASSIGN 
       FILL-IN-FACKID2:HIDDEN IN FRAME FRAME-A           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-SALDO IN FRAME FRAME-A
   ALIGN-L                                                              */
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
     _Where[1]         = "Temp-Tables.mtrldeptemp.DEPNR = vald_depa"
     _FldNameList[1]   = Temp-Tables.mtrldeptemp.FACKID
     _FldNameList[2]   = Temp-Tables.mtrldeptemp.ENR
     _FldNameList[3]   > Temp-Tables.mtrldeptemp.BENAMNING
"mtrldeptemp.BENAMNING" ? "x(256)" "character" ? ? ? ? ? ? no ? no no "23" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.mtrldeptemp.ENHET
"mtrldeptemp.ENHET" "Enh" "x(4)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.mtrldeptemp.SALDO
"mtrldeptemp.SALDO" ? ">>>>>9" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > Temp-Tables.mtrldeptemp.NPRIS
"mtrldeptemp.NPRIS" "Pris" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_DEPA */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_MTRL
/* Query rebuild information for BROWSE BRW_MTRL
     _TblList          = "Temp-Tables.spec_mtrl"
     _Options          = "NO-LOCK"
     _OrdList          = "Temp-Tables.spec_mtrl.ENR|yes"
     _FldNameList[1]   = Temp-Tables.spec_mtrl.ENR
     _FldNameList[2]   > Temp-Tables.spec_mtrl.SALDO
"spec_mtrl.SALDO" "Antal" ? "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.spec_mtrl.NPRIS
"spec_mtrl.NPRIS" "Pris" ? "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.spec_mtrl.BENAMNING
"spec_mtrl.BENAMNING" "Benämning" "x(256)" "character" ? ? ? ? ? ? no ? no no "25" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.spec_mtrl.ENHET
"spec_mtrl.ENHET" "Enh" "x(3)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
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
ON MOUSE-MENU-CLICK OF BRW_DEPA IN FRAME FRAME-A /* Materiellista för depå. */
DO:                                                                   
  /* RUN infoES_UI (INPUT 1). 
  */  
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_DEPA WINDOW-1
ON VALUE-CHANGED OF BRW_DEPA IN FRAME FRAME-A /* Materiellista för depå. */
DO:   
   antal_valda = BRW_DEPA:NUM-SELECTED-ROWS NO-ERROR.  
   IF antal_valda = 0 THEN FILL-IN-SALDO = 0.
   ELSE FILL-IN-SALDO = mtrldeptemp.SALDO.
   DISPLAY FILL-IN-SALDO WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_MTRL
&Scoped-define SELF-NAME BRW_MTRL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_MTRL WINDOW-1
ON ANY-KEY OF BRW_MTRL IN FRAME FRAME-A /* Materiel för uttag */
DO:
   IF KEYFUNCTION(LASTKEY) = ("END-ERROR") THEN  RETURN NO-APPLY. 
   IF KEYFUNCTION(LASTKEY) = ("DELETE-CHARACTER") THEN DO:
      APPLY "CHOOSE" TO BTN_BACK.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_MTRL WINDOW-1
ON MOUSE-MENU-CLICK OF BRW_MTRL IN FRAME FRAME-A /* Materiel för uttag */
DO:                                                                   
  /* RUN infoES_UI (INPUT 2).   */
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
      QUESTION BUTTONS YES-NO-CANCEL TITLE "Spara ändringar?" UPDATE svar AS LOGICAL.         
      IF svar THEN DO:      
         APPLY "CHOOSE" TO BTN_BEST IN FRAME {&FRAME-NAME}.
      END.
      ELSE IF NOT svar THEN DO:
         APPLY "CLOSE":U TO THIS-PROCEDURE.   
      END.   
         
      /* ej tvingande koppla pplats 20200219
      ELSE IF NOT svar THEN DO:                
         /*släckt 20190609  snat vill boka pplats när de gör mottagning*/
         IF VALID-HANDLE(leveransaoapph)  THEN RUN bortbdepaouttag_UI IN leveransaoapph (INPUT valaonr, INPUT valdelnr,INPUT Guru.GlobalaVariabler:GuruVdepnr,INPUT Guru.GlobalaVariabler:GuruProjdepnr,INPUT nytt_bestnr2,INPUT Guru.Konstanter:globanv).         
         Guru.GlobalaVariabler:Guruvlplats = 0.
         APPLY "CLOSE":U TO THIS-PROCEDURE.   
      END.*/                    
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
   RUN openbdynspec_UI IN brwproc[5]. 
   RUN title_UI IN brwproc[5].
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_BEST
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_BEST WINDOW-1
ON CHOOSE OF BTN_BEST IN FRAME FRAME-A /* Ok */
DO:
   {muswait.i}   
   musz = FALSE.
     
   andant = FALSE.       
   FOR EACH spec_mtrl:         
      DO TRANSACTION:         
         FIND FIRST mtrldeptemp WHERE mtrldeptemp.DEPNR = Guru.GlobalaVariabler:GuruVdepnr AND
         mtrldeptemp.ENR = spec_mtrl.ENR AND mtrldeptemp.IBDATUM = ? EXCLUSIVE-LOCK NO-ERROR.
         IF mtrldeptemp.SALDO < spec_mtrl.SALDO THEN DO:
            MESSAGE "Uttaget för " + LC(Guru.Konstanter:genk) + ":" + spec_mtrl.ENR + " med Benämningen:" +
            spec_mtrl.BENAMNING + " är för stort. Begärt uttag:" + STRING(spec_mtrl.SALDO)
            + " möjligt uttag:" + STRING(mtrldeptemp.SALDO) + "Vill du ändra uttaget till detta? OBS! Om Nej, kommer inget uttag att utföras för denna artikel."
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "För stort uttag!" UPDATE svar AS LOGICAL.         
            IF svar THEN DO:
               ASSIGN
               spec_mtrl.SALDO = mtrldeptemp.SALDO.         
            END.
            ELSE DO:       
               DELETE spec_mtrl.          
            END.
            andant = TRUE.    
            musz = TRUE.               
         END.                
      END.
   END.     
   FIND FIRST off_mtrl NO-LOCK NO-ERROR.
   IF AVAILABLE off_mtrl THEN DO:
      IF musz = TRUE THEN DO:
         musz = FALSE.
         MESSAGE "Förändringar av antal har skett. Du får nu möjlighet att ändra Totalt pris."
         VIEW-AS ALERT-BOX TITLE "Meddelande".                    
         RUN OFFUTV.W.         
      END.
   END.
   IF Guru.Konstanter:globforetag = "snat" THEN DO:
      FOR EACH spec_mtrl NO-LOCK:
         FIND FIRST lagertrum  WHERE lagertrum.ENR = spec_mtrl.ENR AND lagertrum.TRUMMA = "S" AND lagertrum.LAGMETER > 0 NO-LOCK NO-ERROR.
         IF AVAILABLE lagertrum THEN DO:
            RUN setlastrowid_UI IN brwproc[5] (INPUT ROWID(spec_mtrl)).
            IF andant = TRUE THEN DO:
               andant = FALSE.
               RUN openbdynspec_UI IN brwproc[5].                 
            END.            
            status-ok = BRW_MTRL:DESELECT-ROWS() NO-ERROR.               
            RUN lastselectdyn_UI IN brwproc[5].                         
            MESSAGE "Enr " + spec_mtrl.ENR + " finns lagd på trumma. Välj - Boka trumma " VIEW-AS ALERT-BOX.            
            
            RETURN.                                     
         END.                    
      END.               
      /*ej tvingande koppla pplats. BESTDEPAO ska inte skapas om uttaget görs från depå - oavsett från vilken depå 20200219*/
      /*Ej lagerkopp Felavhjälpning inte tvingande att koppla till lager skapa ej bestdepao lena 201906*/      
      /*IF Guru.GlobalaVariabler:GuruVdepnr = 7 OR Guru.GlobalaVariabler:GuruVdepnr = 1 THEN.
      ELSE DO:
         IF VALID-HANDLE(leveransaoapph)  THEN RUN sparbdepaouttag_UI IN leveransaoapph (INPUT valaonr, INPUT valdelnr,INPUT Guru.GlobalaVariabler:GuruVdepnr,INPUT Guru.GlobalaVariabler:GuruProjdepnr,INPUT nytt_bestnr2,INPUT Guru.Konstanter:globanv, INPUT TABLE spec_mtrl).
      END.    
      */
      /*ej tvingande koppla pplats 20200219*/
      /*Felavhjälpning inte tvingande att koppla till lager lena 20190515*/
      /*
      FIND FIRST spec_mtrl NO-LOCK NO-ERROR.
      IF Guru.GlobalaVariabler:GuruVdepnr = 7 OR Guru.GlobalaVariabler:GuruVdepnr = 1 THEN koppl = TRUE.
      ELSE IF NOT AVAILABLE spec_mtrl THEN koppl = TRUE.
      ELSE DO:
         IF VALID-HANDLE(leveransaoapph)  THEN RUN checkbdepaouttag_UI IN leveransaoapph (INPUT valaonr, INPUT valdelnr,INPUT Guru.GlobalaVariabler:GuruVdepnr,INPUT Guru.GlobalaVariabler:GuruProjdepnr,INPUT nytt_bestnr2, OUTPUT koppl, OUTPUT felmed).
      END.   
      IF koppl = FALSE THEN DO:
         IF andant = TRUE THEN DO:
            andant = FALSE.
            RUN openbdynspec_UI IN brwproc[5].
         END.
         MESSAGE "Uttaget måste kopplas till en P-plats - tryck på Koppla P-plats " VIEW-AS ALERT-BOX.
                 
         RETURN.                     
      END.
      IF felmed NE  "" THEN DO:
         MESSAGE felmed VIEW-AS ALERT-BOX.     
         RETURN.                     
      END.*/   
   END. 
   IF Guru.Konstanter:globforetag = "ELPA" OR Guru.Konstanter:globforetag = "GKAL" OR 
   Guru.Konstanter:globforetag = "LULE" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "BODE" OR Guru.Konstanter:globforetag = "KRAF" THEN DO:       
      FOR EACH spec_mtrl:         
         DO TRANSACTION:         
            FIND FIRST mtrldeptemp WHERE mtrldeptemp.DEPNR = Guru.GlobalaVariabler:GuruVdepnr AND
            mtrldeptemp.ENR = spec_mtrl.ENR AND mtrldeptemp.IBDATUM = ? EXCLUSIVE-LOCK NO-ERROR.            
            IF AVAILABLE mtrldeptemp THEN DO:               
               mtrldeptemp.SALDO = mtrldeptemp.SALDO - spec_mtrl.SALDO.                
               EMPTY TEMP-TABLE emtrldeptemp NO-ERROR.
               CREATE emtrldeptemp.
               BUFFER-COPY mtrldeptemp TO emtrldeptemp.
               RUN uppmtrldep_UI IN mtrlbapph (INPUT TABLE emtrldeptemp).                                        
               EMPTY TEMP-TABLE emtrldeptemp NO-ERROR.
            END.   
         END.
      END.  
   END.
       
   
   FIND FIRST spec_mtrl WHERE NO-LOCK NO-ERROR.  
   IF AVAILABLE spec_mtrl THEN DO:
      IF Guru.Konstanter:globforetag = "ELPA" OR Guru.Konstanter:globforetag = "LULE" THEN DO:
        
         RUN ARTKODD.W (OUTPUT rekvar).
        
      END.    
   END.
   
   RUN okubest_UI IN mtrlbapph (INPUT valaonr, INPUT valdelnr,INPUT Guru.GlobalaVariabler:GuruVdepnr , INPUT valkund ,INPUT nytt_bestnr2 ,INPUT rekvar,
                               INPUT Guru.Konstanter:globanv, INPUT TABLE spec_mtrl , INPUT TABLE off_mtrl).   
   Guru.GlobalaVariabler:Guruvlplats = 0.                                                                       
   {musarrow.i}
   APPLY "GO" TO FRAME {&FRAME-NAME}.    
   APPLY "CLOSE":U TO THIS-PROCEDURE.     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_BOTR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_BOTR WINDOW-1
ON CHOOSE OF BTN_BOTR IN FRAME FRAME-A /* Boka trumma */
DO:   
   status-ok = BRW_MTRL:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
   IF AVAILABLE spec_mtrl THEN DO:   
      
      {AVBGOM.I}    
       
      RUN TRUMAO.W (INPUT 2, INPUT spec_mtrl.ENR, INPUT valaonr, INPUT valdelnr, INPUT spec_mtrl.SALDO, INPUT depatitta, INPUT "Depå" ).
      {AVBFRAM.I}
      DELETE spec_mtrl.                    
      RUN openbdynspec_UI IN brwproc[5].  
      /*FIND FIRST aotrum WHERE aotrum.ENR = spec_mtrl.ENR AND aotrum.AONR = valaonr AND aotrum.DELNR = valdelnr  NO-LOCK NO-ERROR.
      IF AVAILABLE aotrum THEN DO:
         /*cccc*/         
         
         MESSAGE "Enr " + aotrum.ENR + " är nu bokad på p-plats "  + STRING(aotrum.lagerplatsi)  +  " " + aotrum.LAGERPLATSC 
         VIEW-AS ALERT-BOX.          
      END.*/
   END.
 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_KLP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_KLP WINDOW-1
ON CHOOSE OF BTN_KLP IN FRAME FRAME-A /* Koppla P-plats */
DO:   
   /*ej tvingande koppla pplats 20200219*/
   /*IF Guru.GlobalaVariabler:GuruVdepnr = 7 OR Guru.GlobalaVariabler:GuruVdepnr = 1 THEN DO:
      MESSAGE "P-plats skall ej bokas för denna depå" 
      VIEW-AS ALERT-BOX.
      RETURN.
   END.   
   
   IF VALID-HANDLE(leveransaoapph)  THEN RUN sparbdepaouttag_UI IN leveransaoapph (INPUT valaonr, INPUT valdelnr,INPUT Guru.GlobalaVariabler:GuruVdepnr,INPUT Guru.GlobalaVariabler:GuruProjdepnr,INPUT nytt_bestnr2,INPUT Guru.Konstanter:globanv, INPUT TABLE spec_mtrl).
   EMPTY TEMP-TABLE lplagertrum NO-ERROR.
   {AVBGOM.I}  
   RUN LEVERANSVAO.W (INPUT 2, INPUT nytt_bestnr2,INPUT-OUTPUT TABLE lplagertrum, INPUT-OUTPUT TABLE eaobest_mtrl).
   {AVBFRAM.I}
   EMPTY TEMP-TABLE lplagertrum NO-ERROR.*/
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
   antal_valda = BRW_DEPA:NUM-SELECTED-ROWS NO-ERROR.         
   antal_raknare = 1.
   DO WHILE antal_raknare LE antal_valda:                                   
      status-ok = BRW_DEPA:FETCH-SELECTED-ROW(antal_raknare). 
      IF Guru.Konstanter:globforetag = "ELPA"  OR Guru.Konstanter:globforetag = "GKAL" OR 
      Guru.Konstanter:globforetag = "LULE" OR Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "BORL" OR Guru.Konstanter:globforetag = "BODE" OR Guru.Konstanter:globforetag = "KRAF" THEN DO:
         IF mtrldeptemp.SALDO > 0 THEN DO:
            ASSIGN
            antalvar = 1
            enrval = FALSE.
            RUN over3_UI.
         END.
         ELSE DO:
            MESSAGE "Saldot för " + LC(Guru.Konstanter:genk) + ":" + mtrldeptemp.ENR + " med Benämningen:" + 
            mtrldeptemp.BENAMNING + " är 0. Uttag ej möjligt." VIEW-AS ALERT-BOX
            TITLE "Meddelande".    
            antal_raknare = antal_raknare + 1.
         END.
      END.
      ELSE DO:
         ASSIGN
         antalvar = 1
         enrval = FALSE.
         RUN over3_UI.
      END.                    
   END. 
   
   RUN openbdynspec_UI IN brwproc[5].   
   status-ok = BRW_DEPA:DESELECT-ROWS() NO-ERROR.                
   RUN setlastrowid_UI IN brwproc[5] (INPUT val_rec).
   RUN lastselectdyn_UI IN brwproc[5].
   RUN title_UI IN brwproc[5].
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_SATS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_SATS WINDOW-1
ON CHOOSE OF BTN_SATS IN FRAME FRAME-A /* Satsinformation */
DO:           
   {muswait.i}  
   satsinn = FALSE.
   status-ok = BRW_DEPA:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
   
   RUN SATSDEPV.W (INPUT mtrldeptemp.ENR, INPUT mtrldeptemp.LEVKOD, INPUT-OUTPUT satsinn).
   
   IF satsinn = TRUE THEN DO:
      satsinn = FALSE.
      RUN openbdynspec_UI IN brwproc[5]. 
      RUN title_UI IN brwproc[5].
   END.
   
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_SOK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_SOK WINDOW-1
ON CHOOSE OF BTN_SOK IN FRAME FRAME-A /* Beställningar */
DO:         
   EMPTY TEMP-TABLE best_mtrl NO-ERROR. 
   
   antal_valda = BRW_DEPA:NUM-SELECTED-ROWS NO-ERROR.
   ASSIGN
   musz = FALSE
   antal_raknare = 1.    
   DO WHILE antal_raknare LE antal_valda:   
      status-ok = BRW_DEPA:FETCH-SELECTED-ROW(antal_raknare).  
      CREATE best_mtrl.
      ASSIGN
      best_mtrl.ENR = mtrldeptemp.ENR
      best_mtrl.BENAMNING = mtrldeptemp.BENAMNING
      best_mtrl.ENHET = mtrldeptemp.ENHET.
      antal_raknare = antal_raknare + 1.   
   END.         
   {muswait.i}
   {AVBGOM.I}
   RUN UTBESTV.W.
   {AVBFRAM.I}   
   EMPTY TEMP-TABLE best_mtrl NO-ERROR.    
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


&Scoped-define SELF-NAME FBTN_FAKT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_FAKT WINDOW-1
ON CHOOSE OF FBTN_FAKT IN FRAME FRAME-A /* Faktura */
DO:    
   {muswait.i} 
   fakt = TRUE. 
   {AVBGOM.I}
   foljesedel = FALSE.
   IF Guru.Konstanter:globforetag = "GKAL" OR Guru.Konstanter:globforetag = "LULE" OR Guru.Konstanter:globforetag = "ELPA" OR 
   Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "BORL" OR Guru.Konstanter:globforetag = "BODE" OR Guru.Konstanter:globforetag = "KRAF" THEN RUN FOLJTRP2V.W.
   /* bara för kund
   ELSE RUN FOLJTRPV.W.*/
   {AVBFRAM.I}
   fakt = FALSE.
   {musarrow.i}  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_FOLJ
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_FOLJ WINDOW-1
ON CHOOSE OF FBTN_FOLJ IN FRAME FRAME-A /* Följesedel */
DO:    
   {muswait.i} 
   IF akval = 1 THEN DO: 
      fakt = FALSE. 
      {AVBGOM.I} 
      foljesedel = TRUE.
      RUN FOLJTRP2V.W.
      {AVBFRAM.I}
   END.
   /*ELSE DO:
      fakt = FALSE.
      {AVBGOM.I}           
      RUN FOLJTRP.W.
      {AVBFRAM.I}
   END.*/
   {musarrow.i}  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_HAONR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_HAONR WINDOW-1
ON CHOOSE OF FBTN_HAONR IN FRAME FRAME-A /* Hämta från aonr */
DO:            
   RUN hamtaonr_UI.          
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_PRIS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_PRIS WINDOW-1
ON CHOOSE OF FBTN_PRIS IN FRAME FRAME-A /* Tot. pris */
DO:    
   {muswait.i}
   RUN OFFUTV.W.
   
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
ON CHOOSE OF FBTN_VISA IN FRAME FRAME-A /* Visa */
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
   IF Guru.Konstanter:globforetag = "SNAT"  THEN DO:            
      IF SUBSTRING(FILL-IN-ENR2,1,1) NE "E" THEN FILL-IN-ENR2 = "E" + FILL-IN-ENR2.                                       
   END.    
   {TRYCKS.I}
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      RUN enrval_UI.
   END.
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
   IF Guru.Konstanter:globforetag = "SNAT"  THEN DO:            
      IF SUBSTRING(FILL-IN-ENR2,1,1) NE "E" THEN FILL-IN-ENR2 = "E" + FILL-IN-ENR2.                                       
   END. 
   {TRYCKS.I}
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      RUN enrval_UI.
   END.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-FACK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-FACK WINDOW-1
ON ANY-KEY OF FILL-IN-FACK IN FRAME FRAME-A /* Sök på Fack */
DO:
   IF KEYFUNCTION(LASTKEY) = ("END-ERROR") THEN  RETURN NO-APPLY. 
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN-FACK IN FRAME {&FRAME-NAME}.
   END.
   IF KEYFUNCTION(LASTKEY) = ("+") THEN DO:
      APPLY "CHOOSE" TO BTN_OVER.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-FACK WINDOW-1
ON MOUSE-SELECT-DBLCLICK OF FILL-IN-FACK IN FRAME FRAME-A /* Sök på Fack */
DO:
   
   IF INDEX(FILL-IN-FACKID1,"+") NE 0 THEN DO:
      FILL-IN-FACKID1 = TRIM(FILL-IN-FACKID1,"+").
      DISP FILL-IN-FACKID1 WITH FRAME {&FRAME-NAME}.
   END.
   IF INDEX(FILL-IN-FACKID2,"+") NE 0 THEN DO:
      FILL-IN-FACKID2 = TRIM(FILL-IN-FACKID2,"+").
      FILL-IN-FACKID2 = TRIM(FILL-IN-FACKID2).
      DISP FILL-IN-FACKID2 WITH FRAME {&FRAME-NAME}.
   END.
   FILL-IN-FACKID1 = FILL-IN-FACKID1 + FILL(" ",2 - LENGTH(FILL-IN-FACKID1)).          
   FILL-IN-FACKID2 = FILL(" ",6 - LENGTH(FILL-IN-FACKID2)) + FILL-IN-FACKID2.          
   ASSIGN
   FILL-IN-FACK = FILL-IN-FACKID1 + FILL-IN-FACKID2.     
   IF AVAILABLE mtrldeptemp THEN mtrl_rowid = ROWID(mtrldeptemp).
   IF FILL-IN-FACK = '' THEN DO:
      MESSAGE "Sökbegreppet kan inte vara blankt." VIEW-AS ALERT-BOX TITLE "Meddelande".
      APPLY "ENTRY" TO FILL-IN-FACK IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
   END.      
   aosok = '*' + FILL-IN-FACK + '*'.
   status-ok = BRW_DEPA:DESELECT-ROWS() NO-ERROR.            
   FIND mtrldeptemp WHERE ROWID(mtrldeptemp) = mtrl_rowid NO-LOCK NO-ERROR.                  
   FIND NEXT mtrldeptemp WHERE mtrldeptemp.FACKID MATCHES aosok AND mtrldeptemp.DEPNR = Guru.GlobalaVariabler:GuruVdepnr
   AND mtrldeptemp.IBDATUM = ? NO-LOCK NO-ERROR. 
   IF NOT AVAILABLE mtrldeptemp THEN DO:      
      FIND FIRST mtrldeptemp WHERE mtrldeptemp.FACKID MATCHES aosok AND mtrldeptemp.DEPNR = Guru.GlobalaVariabler:GuruVdepnr
      AND mtrldeptemp.IBDATUM = ? NO-LOCK NO-ERROR.   
      IF NOT AVAILABLE mtrldeptemp THEN DO: 
         MESSAGE "Det finns inget på sökbegreppet i depån." 
         VIEW-AS ALERT-BOX TITLE "Meddelande". 
      END.
   END.    
   IF AVAILABLE mtrldeptemp THEN DO:                     
      RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(mtrldeptemp)).
      RUN lastselectdyn_UI IN brwproc[1].        
   END.               
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-FACKID1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-FACKID1 WINDOW-1
ON ANY-KEY OF FILL-IN-FACKID1 IN FRAME FRAME-A /* Fack-id (aa) */
DO:
   IF KEYFUNCTION(LASTKEY) = ("END-ERROR") THEN  RETURN NO-APPLY. 
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN-FACKID1 IN FRAME {&FRAME-NAME}.
   END.
   IF KEYFUNCTION(LASTKEY) = ("+") THEN DO:
      APPLY "CHOOSE" TO BTN_OVER.
      IF INDEX(FILL-IN-FACKID1,"+") NE 0 THEN DO:
         FILL-IN-FACKID1 = TRIM(FILL-IN-FACKID1,"+").
         DISP FILL-IN-FACKID1 WITH FRAME {&FRAME-NAME}.
      END.   
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-FACKID1 WINDOW-1
ON LEAVE OF FILL-IN-FACKID1 IN FRAME FRAME-A /* Fack-id (aa) */
DO: 
   
   ASSIGN
   FILL-IN-FACKID1 = INPUT FILL-IN-FACKID1  
   FILL-IN-FACKID1 = TRIM(FILL-IN-FACKID1).
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-FACKID1 WINDOW-1
ON MOUSE-SELECT-DBLCLICK OF FILL-IN-FACKID1 IN FRAME FRAME-A /* Fack-id (aa) */
DO:
   ASSIGN
   FILL-IN-FACKID1 = INPUT FILL-IN-FACKID1  
   FILL-IN-FACKID1 = TRIM(FILL-IN-FACKID1).
   APPLY "LEAVE" TO FILL-IN-FACKID2 IN FRAME FRAME-A.
   APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN-FACK IN FRAME FRAME-A.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-FACKID2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-FACKID2 WINDOW-1
ON ANY-KEY OF FILL-IN-FACKID2 IN FRAME FRAME-A /* + (999999) */
DO:
   IF KEYFUNCTION(LASTKEY) = ("END-ERROR") THEN  RETURN NO-APPLY. 
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN-FACKID2 IN FRAME {&FRAME-NAME}.
   END.
   IF KEYFUNCTION(LASTKEY) = ("+") THEN DO:
      APPLY "CHOOSE" TO BTN_OVER.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-FACKID2 WINDOW-1
ON LEAVE OF FILL-IN-FACKID2 IN FRAME FRAME-A /* + (999999) */
DO: 
   ASSIGN
   FILL-IN-FACKID2 = INPUT FILL-IN-FACKID2  
   FILL-IN-FACKID2 = TRIM(FILL-IN-FACKID2).
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-FACKID2 WINDOW-1
ON MOUSE-SELECT-DBLCLICK OF FILL-IN-FACKID2 IN FRAME FRAME-A /* + (999999) */
DO:
   ASSIGN
   FILL-IN-FACKID2 = INPUT FILL-IN-FACKID2.
   FILL-IN-FACKID2 = TRIM(FILL-IN-FACKID2).
   APPLY "LEAVE" TO FILL-IN-FACKID1 IN FRAME FRAME-A.
   APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN-FACK IN FRAME FRAME-A.
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
   {BORTBRWPROC.I}
   IF VALID-HANDLE(mtrlbapph) THEN DELETE PROCEDURE mtrlbapph.
   IF VALID-HANDLE(leveransaoapph) THEN DELETE PROCEDURE leveransaoapph NO-ERROR.
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
   
   EMPTY TEMP-TABLE spec_mtrl NO-ERROR. 
   
   FIND FIRST depatemp WHERE depatemp.DEP-NR = Guru.GlobalaVariabler:GuruVdepnr USE-INDEX DEP-NR NO-LOCK NO-ERROR.       
   IF akval = 1 THEN DO:  
      {&WINDOW-NAME}:TITLE = "Uttag till " + CAPS(Guru.Konstanter:gaol) + " " + STRING(valaonr) + " DELNR: " + 
      STRING(valdelnr,Guru.Konstanter:varforetypchar[1]) + CAPS(Guru.Konstanter:gaonamnk) + ": " + valort 
      + " från depå - " + depatemp.BENAMNING.   
   END.                      
   RUN mtrluhmt_UI IN mtrlbapph (INPUT Guru.GlobalaVariabler:GuruVdepnr,OUTPUT TABLE mtrldeptemp ,OUTPUT nytt_bestnr2).             
   RUN setcolsortvar_UI IN brwproc[1] (INPUT "mtrldeptemp.DEPNR =  " + STRING(Guru.GlobalaVariabler:GuruVdepnr) +  " AND mtrldeptemp.IBDATUM = ? ").
   
   RUN openbdynspec_UI IN brwproc[1]. 
   RUN title_UI IN brwproc[1].
   RUN setcolsortvar_UI IN brwproc[5] (INPUT "").
   RUN openbdynspec_UI IN brwproc[5]. 
   RUN title_UI IN brwproc[5].
   FILL-IN-ANTAL = 1.
   
   IF Guru.Konstanter:globforetag = "LULE" OR  Guru.Konstanter:globforetag = "BODE" OR Guru.Konstanter:globforetag = "KRAF"   THEN DO:     
      mtrldeptemp.FACKID:VISIBLE IN BROWSE BRW_DEPA = FALSE.      
   END.
   ELSE DO:      
      mtrldeptemp.FACKID:VISIBLE IN BROWSE BRW_DEPA = TRUE.        
   END.
   FBTN_HAONR:LABEL = "Hämta från " + Guru.Konstanter:gaok.
   
   
   RUN enable_UI.
   {FRMSIZE.I}
   
         
      
   
   Guru.GlobalaVariabler:collefth = ?.
   Guru.GlobalaVariabler:colrighth = FBTN_PRIS:HANDLE.           
   RUN buttrow_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   Guru.GlobalaVariabler:colrighth = FBTN_FOLJ:HANDLE.           
   RUN buttrow_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   Guru.GlobalaVariabler:colrighth = FBTN_FAKT:HANDLE.           
   RUN buttrow_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   Guru.GlobalaVariabler:colrighth = FBTN_VISA:HANDLE.           
   RUN buttrow_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   Guru.GlobalaVariabler:colrighth = FBTN_SKRIV:HANDLE.           
   RUN buttrow_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   IF FBTN_HAONR:VISIBLE = TRUE THEN DO:
      Guru.GlobalaVariabler:colrighth = FBTN_HAONR:HANDLE.           
      RUN buttrow_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   END.   
   
   
   IF Guru.Konstanter:varforetypval[57] = 1 THEN DO:
      /*TRUMMA*/
      BTN_BOTR:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
      Guru.GlobalaVariabler:colrighth = BTN_BOTR:HANDLE.           
      RUN buttrow_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   END.
   ELSE DO:
      BTN_BOTR:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.   
   END.   
   IF Guru.Konstanter:globforetag = "cSNAT" THEN DO:
      /*ej tvingande koppla pplats 20200219*/
      /*släckt 20190609  snat vill boka pplats när de gör mottagning- bort*/
      BTN_KLP:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
      Guru.GlobalaVariabler:colrighth = BTN_KLP:HANDLE.           
      RUN buttrow_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).       
   END.   
   ELSE DO:
      BTN_KLP:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.      
   END.
   IF Guru.Konstanter:globforetag = "GKAL" OR Guru.Konstanter:globforetag = "LULE" OR  Guru.Konstanter:globforetag = "ELPA" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "BORL"
   OR Guru.Konstanter:globforetag = "BODE"  THEN DO:      
      musz = musz.
   END.
   ELSE DO:
      ASSIGN
      RECT-4:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      FILL-IN-ENR2:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      FILL-IN-ANTAL:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      BTN_UP:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      BTN_MIN:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
   END.
   {FRMSIZE.I}    
     
   IF akval = 1 THEN DO:
      IF Guru.Konstanter:globforetag = "GKAL" OR Guru.Konstanter:globforetag = "LULE" OR Guru.Konstanter:globforetag = "ELPA" OR Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "BORL" 
      OR Guru.Konstanter:globforetag = "BODE" THEN DO:
         FBTN_FAKT:LABEL = "Mtrl.spec".
      END.
      ELSE FBTN_FAKT:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.      
   END.   
   ASSIGN   
   FILL-IN-FACK:HIDDEN = TRUE.
   FILL-IN-ENR:LABEL = Guru.Konstanter:genk.
   mtrldeptemp.ENR:LABEL IN BROWSE BRW_DEPA = Guru.Konstanter:genk.
   spec_mtrl.ENR:LABEL IN BROWSE BRW_MTRL = Guru.Konstanter:genk.
   IF depatitta = TRUE THEN DO:
      DISABLE BTN_BEST WITH FRAME {&FRAME-NAME}.
      DISABLE FBTN_PRIS WITH FRAME {&FRAME-NAME}.
      DISABLE BTN_OVER WITH FRAME {&FRAME-NAME}.
      DISABLE BTN_BACK WITH FRAME {&FRAME-NAME}.
      DISABLE BTN_BOTR WITH FRAME {&FRAME-NAME}.
      DISABLE BTN_KLP WITH FRAME {&FRAME-NAME}.  
                
   END.
   IF Guru.Konstanter:mtrlsekvar[6] = TRUE THEN DO:
      ASSIGN spec_mtrl.NPRIS:VISIBLE IN BROWSE BRW_MTRL = FALSE.
      FBTN_PRIS:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
   END.
   IF Guru.Konstanter:globforetag = "bode" THEN FBTN_HAONR:HIDDEN IN FRAME {&FRAME-NAME} = false.
   ELSE IF Guru.Konstanter:globforetag = "lule" THEN FBTN_HAONR:HIDDEN IN FRAME {&FRAME-NAME} = false.
   ELSE IF Guru.Konstanter:globforetag = "kraf" THEN FBTN_HAONR:HIDDEN IN FRAME {&FRAME-NAME} = false.
   ELSE IF Guru.Konstanter:globforetag = "cSNAT" THEN FBTN_HAONR:HIDDEN IN FRAME {&FRAME-NAME} = false.
   /*kalmar depåändring ABC nu   de måste ha spegling för detta*/
   ELSE IF Guru.Konstanter:globforetag = "GKAL" THEN FBTN_HAONR:HIDDEN IN FRAME {&FRAME-NAME} = false.
   ELSE FBTN_HAONR:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
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
   RUN DYNBRW.P PERSISTENT SET brwproc[1]
      (INPUT BRW_DEPA:HANDLE IN FRAME {&FRAME-NAME}).   
   RUN StartSokEnrLev_UI IN brwproc[1] (INPUT THIS-PROCEDURE).    
   RUN DYNBRW.P PERSISTENT SET brwproc[5]
      (INPUT BRW_MTRL:HANDLE IN FRAME {&FRAME-NAME}).         
   RUN StartSokEnrLev_UI IN brwproc[5] (INPUT THIS-PROCEDURE).   
   RUN DYNARROW.P PERSISTENT SET brwproc[6] 
      (INPUT BRW_DEPA:HANDLE, INPUT BRW_MTRL:HANDLE ,
       INPUT ?, INPUT ?,
       INPUT ? , INPUT ?).
   /*RUN setdefaultcol_UI IN brwproc[1] (INPUT 2).
   RUN colsortdynbrw_UI IN brwproc[1] (INPUT "").*/ 
   RUN setcolindex_UI IN brwproc[1] (INPUT "mtrldeptemp.ENR").  
   IF Guru.Konstanter:appcon THEN DO:
      RUN MTRLBAPP.P PERSISTENT SET mtrlbapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN MTRLBAPP.P PERSISTENT SET mtrlbapph.
   END.   
   
   IF Guru.Konstanter:appcon THEN DO:
      RUN FINNSTABELL.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT (INPUT "INKBER", OUTPUT logresult).
   END.
   ELSE DO:
      RUN FINNSTABELL.P (INPUT "INKBER", OUTPUT logresult).
   END.   
   IF logresult = TRUE THEN DO:
      IF Guru.Konstanter:appcon THEN DO:
         RUN INKDIVHAMTAPP.P PERSISTENT SET hamtapp ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
      END.
      ELSE DO:
         RUN INKDIVHAMTAPP.P PERSISTENT SET hamtapp.
      END.
   END.
   
   IF Guru.Konstanter:appcon THEN DO:
      RUN FINNSTABELL.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT (INPUT "BESTDEPAO", OUTPUT logresult).
   END.
   ELSE DO:
      RUN FINNSTABELL.P (INPUT "BESTDEPAO", OUTPUT logresult).
   END.   
   IF logresult = TRUE THEN DO:      
      IF Guru.Konstanter:appcon THEN DO:
         RUN LEVAPPVAO.P PERSISTENT SET leveransaoapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
      END.
      ELSE DO:
         RUN LEVAPPVAO.P PERSISTENT SET leveransaoapph.
      END.
   END.
   
   
   
   
   /*uttag ska inte ha uppdateringsbart pris om det är FIFO. Priset hämtas från fifoposterna*/
   IF Guru.Konstanter:varforetypval[54] = 1 THEN DO:    
      spec_mtrl.NPRIS:READ-ONLY IN BROWSE BRW_MTRL = TRUE.      
   END.   
   
   RUN addfillin_UI IN brwproc[1] 
      (INPUT FILL-IN-ENR:HANDLE IN FRAME {&FRAME-NAME}, INPUT "ENR").
   RUN addfillin_UI IN brwproc[1] 
      (INPUT FILL-IN-BEN:HANDLE IN FRAME {&FRAME-NAME}, INPUT "BENAMNING").
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
  DISPLAY FILL-IN-ENR FILL-IN-BEN FILL-IN-FACK FILL-IN-FACKID1 FILL-IN-FACKID2 
          FILL-IN-ENR2 FILL-IN-ANTAL FILL-IN-SALDO 
      WITH FRAME FRAME-A IN WINDOW WINDOW-1.
  ENABLE BTN_KLP FBTN_PRIS FBTN_FOLJ FBTN_FAKT FBTN_VISA FBTN_SKRIV BTN_BEST 
         BTN_AVB btn_over btn_back BTN_SOK FILL-IN-ENR FILL-IN-BEN FILL-IN-FACK 
         FILL-IN-FACKID1 FILL-IN-FACKID2 FILL-IN-ENR2 FILL-IN-ANTAL BTN_UP 
         BTN_MIN FILL-IN-SALDO BTN_SATS FBTN_HAONR BTN_BOTR RECT-2 BRW_DEPA 
         RECT-4 BRW_MTRL 
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
  FIND FIRST mtrldeptemp WHERE mtrldeptemp.ENR = FILL-IN-ENR2 AND mtrldeptemp.DEPNR = Guru.GlobalaVariabler:GuruVdepnr AND
  mtrldeptemp.IBDATUM = ? NO-LOCK NO-ERROR.
  IF AVAILABLE mtrldeptemp THEN DO:
     IF mtrldeptemp.SALDO > 0 THEN DO:
        IF mtrldeptemp.SALDO - FILL-IN-ANTAL >= 0 THEN DO:
           ASSIGN
           antalvar = FILL-IN-ANTAL
           enrval = TRUE.
           RUN over3_UI.
        END.
        ELSE DO:
           MESSAGE "Uttag för stort. Saldot för " + LC(Guru.Konstanter:genk) + ":" + mtrldeptemp.ENR + " med Benämningen:" + 
           mtrldeptemp.BENAMNING + " är " STRING(mtrldeptemp.SALDO) + "." VIEW-AS ALERT-BOX
           TITLE "Meddelande".    
           RETURN.
        END.
     END.
     ELSE DO:
        MESSAGE "Saldot för " + LC(Guru.Konstanter:genk) + ":" + mtrldeptemp.ENR + " med Benämningen:" + 
        mtrldeptemp.BENAMNING + " är 0. Uttag ej möjligt." VIEW-AS ALERT-BOX
        TITLE "Meddelande".    
        RETURN.
     END.
  END.
  ELSE DO:
     MESSAGE "Det finns ingen lagerförd materiel med E-nummer:" + FILL-IN-ENR2 + ". Uttag ej möjligt." VIEW-AS ALERT-BOX
     TITLE "Meddelande".    
     RETURN.
  END.         
  
  RUN openbdynspec_UI IN brwproc[5].  
  RUN setlastrowid_UI IN brwproc[5] (INPUT val_rec).
  RUN lastselectdyn_UI IN brwproc[5].
  RUN title_UI IN brwproc[5].
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE hamtaonr_UI WINDOW-1 
PROCEDURE hamtaonr_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   RUN kollinkaonr_UI IN hamtapp (INPUT valaonr,INPUT valdelnr, OUTPUT antink,OUTPUT TABLE inkmtrldep,OUTPUT TABLE einkmtrldep).
   IF antink = 0 THEN DO:
      MESSAGE "Inget inköp med leverantör depå är upplagt!"
         VIEW-AS ALERT-BOX TITLE "Meddelande".
   END. 
   ELSE DO:
      FIND FIRST einkmtrldep NO-ERROR.
      IF AVAILABLE einkmtrldep THEN DO:  
         FOR EACH inkmtrldep WHERE inkmtrldep.INKID = einkmtrldep.INKID AND inkmtrldep.INKBESTID = einkmtrldep.INKBESTID:
            FIND FIRST mtrldeptemp WHERE mtrldeptemp.ENR =  inkmtrldep.ENR  NO-LOCK NO-ERROR.
            IF AVAILABLE mtrldeptemp THEN DO:
               FIND FIRST spec_mtrl WHERE spec_mtrl.ENR = mtrldeptemp.ENR NO-LOCK NO-ERROR.
               IF NOT AVAILABLE spec_mtrl THEN DO:               
                  CREATE spec_mtrl.  
                  ASSIGN
                  spec_mtrl.ENR = mtrldeptemp.ENR
                  spec_mtrl.BENAMNING = mtrldeptemp.BENAMNING
                  spec_mtrl.ENHET = mtrldeptemp.ENHET
                  spec_mtrl.SALDO = inkmtrldep.INKANTAL               
                  spec_mtrl.BPRIS = mtrldeptemp.BPRIS
                  spec_mtrl.NPRIS = mtrldeptemp.NPRIS          
                  spec_mtrl.LEVKOD = mtrldeptemp.LEVKOD
                  val_rec = ROWID(spec_mtrl).
               END.   
            END.   
         END.   
      END.   
            
      RUN openbdynspec_UI IN brwproc[5].  
      RUN setlastrowid_UI IN brwproc[5] (INPUT val_rec).
      RUN lastselectdyn_UI IN brwproc[5].
      RUN title_UI IN brwproc[5].
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
   DEFINE VARIABLE valbrw AS INTEGER NO-UNDO.  
   DEFINE VARIABLE levnamnvar AS CHARACTER NO-UNDO.
   IF brwname = "BRW_DEPA" THEN valbrw = 1.  
   IF brwname = "BRW_MTRL" THEN valbrw = 2. 
    
   /*GÅR TILL ELEKTROSKANDIAS HEMSIDA*/      
   IF valbrw = 1 THEN DO:   
      status-ok = BRW_DEPA:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
      valenr = mtrldeptemp.ENR.
      FIND FIRST levtemp WHERE levtemp.LEVKOD = mtrldeptemp.LEVKOD
      NO-LOCK NO-ERROR.      
   END.
   ELSE IF valbrw = 2 THEN DO:
      status-ok = BRW_MTRL:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
      valenr = spec_mtrl.ENR.
      FIND FIRST levtemp WHERE levtemp.LEVKOD = spec_mtrl.LEVKOD
      NO-LOCK NO-ERROR.      
   END.   
   IF status-ok THEN DO:
      levnamnvar = levtemp.LEVNAMN.
      {LEVLANK.I}
   END.
   
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
   FIND FIRST spec_mtrl WHERE spec_mtrl.ENR = mtrldeptemp.ENR AND 
   spec_mtrl.LEVKOD = mtrldeptemp.LEVKOD NO-LOCK NO-ERROR. 
   IF AVAILABLE spec_mtrl THEN DO:
      IF enrval = TRUE THEN DO:
         ASSIGN
         spec_mtrl.SALDO = FILL-IN-ANTAL
         val_rec = ROWID(spec_mtrl).               
      END.      
   END.      
   ELSE DO:
      CREATE spec_mtrl. 
      ASSIGN
      spec_mtrl.ENR = mtrldeptemp.ENR
      spec_mtrl.BENAMNING = mtrldeptemp.BENAMNING
      spec_mtrl.ENHET = mtrldeptemp.ENHET
      spec_mtrl.SALDO = antalvar               
      spec_mtrl.BPRIS = mtrldeptemp.BPRIS
      spec_mtrl.NPRIS = mtrldeptemp.NPRIS          
      spec_mtrl.LEVKOD = mtrldeptemp.LEVKOD
      val_rec = ROWID(spec_mtrl).
   END. 
   antal_raknare = antal_raknare + 1.                           
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

