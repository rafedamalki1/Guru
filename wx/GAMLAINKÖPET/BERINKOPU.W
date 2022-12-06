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

  Created: 08/15/97 -  1:46 pm

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
&Scoped-define NEW 
{GLOBVAR2DEL1.I}
/*{EGENBEN.I}*/
{BERMTRL.I}
{SCHAKTADM.I}
{BERLINKABTEMP.I}
{BESTSTATTEMP.I}
&Scoped-define SHARED SHARED
{LEVTEMP.I}
   
{HUVLEVTEMP.I}
{HOPPSEK2W.I}
{MTRLTAB.I}
&Scoped-define NEW NEW
&Scoped-define SHARED SHARED
{BESTMTRL.I}
{KONVALTEMP.I}
DEFINE NEW SHARED TEMP-TABLE ebest_mtrl2 NO-UNDO  LIKE best_mtrl  
FIELD MTRLROW AS ROWID.  

DEFINE NEW SHARED VARIABLE datvar AS DATE NO-UNDO.  
DEFINE NEW SHARED VARIABLE klockansek AS INTEGER NO-UNDO.  
DEFINE NEW SHARED VARIABLE vald_depa AS INTEGER NO-UNDO. 
DEFINE NEW SHARED VARIABLE gammal2 AS LOGICAL NO-UNDO. 
DEFINE NEW SHARED VARIABLE bestvar AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE priset AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE delbest AS LOGICAL NO-UNDO.

DEFINE NEW SHARED VARIABLE kalkrow AS ROWID NO-UNDO.
DEFINE SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.      
DEFINE SHARED VARIABLE valaonr AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE valdelnr AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE valomrade AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE valort AS CHARACTER NO-UNDO.
DEFINE VARIABLE nyberapph AS HANDLE NO-UNDO.
DEFINE VARIABLE levbytapph AS HANDLE NO-UNDO.
DEFINE VARIABLE felmedd AS CHARACTER NO-UNDO.
DEFINE VARIABLE val AS INTEGER NO-UNDO.
DEFINE VARIABLE skipkontroll AS LOGICAL NO-UNDO.
DEFINE VARIABLE mtrl_rowid AS ROWID NO-UNDO.
DEFINE VARIABLE komsort AS CHARACTER NO-UNDO.
DEFINE VARIABLE helpvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE svar AS LOGICAL NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO. 
DEFINE VARIABLE antal_valda AS INTEGER NO-UNDO.
DEFINE VARIABLE antal_raknare AS INTEGER NO-UNDO.
DEFINE VARIABLE sumantal AS INTEGER NO-UNDO.
DEFINE VARIABLE enrvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE levvar AS CHARACTER NO-UNDO. 
DEFINE VARIABLE datumvar AS DATE NO-UNDO.
DEFINE VARIABLE gammal AS LOGICAL NO-UNDO.  
DEFINE VARIABLE nextvar AS LOGICAL NO-UNDO.                          
DEFINE VARIABLE depant AS INTEGER NO-UNDO. 
DEFINE VARIABLE nytt_bestnr AS INTEGER.   
DEFINE VARIABLE nytt_bestnr2 AS INTEGER. 
DEFINE VARIABLE best AS LOGICAL NO-UNDO.
DEFINE VARIABLE leve AS LOGICAL NO-UNDO.
DEFINE VARIABLE rabatt AS DECIMAL FORMAT "->9.99" NO-UNDO. 
DEFINE VARIABLE dat_rowid AS ROWID NO-UNDO. 
DEFINE VARIABLE dep AS LOGICAL NO-UNDO.
DEFINE VARIABLE till_rowid AS ROWID NO-UNDO.
DEFINE VARIABLE kontrollsumma AS INTEGER NO-UNDO.
DEFINE VARIABLE kontrollsumma2 AS INTEGER NO-UNDO.
DEFINE VARIABLE berinkopapph AS HANDLE NO-UNDO.  
DEFINE VARIABLE nettoh AS HANDLE NO-UNDO.
DEFINE VARIABLE valev AS CHARACTER NO-UNDO.
DEFINE VARIABLE felbest AS LOGICAL NO-UNDO.
DEFINE VARIABLE lve AS CHARACTER NO-UNDO.   
DEFINE VARIABLE flerlev AS CHARACTER NO-UNDO.
DEFINE VARIABLE visgamb AS LOGICAL NO-UNDO.
/*ny*/
DEFINE VARIABLE delmer AS LOGICAL NO-UNDO.

/*slut ny*/

DEFINE TEMP-TABLE prev_temp    
    FIELD ENR AS CHARACTER
    FIELD BENAMNING AS CHARACTER
    FIELD ENHET AS CHARACTER
    FIELD PRIS AS DECIMAL       
    FIELD ANTAL AS INTEGER     
    FIELD BESTANT AS INTEGER    
    FIELD LEVKOD AS CHARACTER 
    FIELD BERLEV AS CHARACTER        
    FIELD DBEST AS CHARACTER
    FIELD DATUM AS DATE    
    INDEX DATUM DATUM ASCENDING   
    INDEX LEV ENR LEVKOD ASCENDING
    INDEX ENR IS PRIMARY ENR ASCENDING. 
 
DEFINE TEMP-TABLE next_temp    
    FIELD ENR AS CHARACTER
    FIELD BENAMNING AS CHARACTER
    FIELD ENHET AS CHARACTER
    FIELD PRIS AS DECIMAL       
    FIELD ANTAL AS INTEGER     
    FIELD BESTANT AS INTEGER    
    FIELD LEVKOD AS CHARACTER 
    FIELD BERLEV AS CHARACTER        
    FIELD DBEST AS CHARACTER
    FIELD DATUM AS DATE    
    INDEX DATUM DATUM ASCENDING  
    INDEX LEV ENR LEVKOD ASCENDING
    INDEX ENR IS PRIMARY ENR ASCENDING.            
    
DEFINE TEMP-TABLE mtrl_temp    
    FIELD ENR AS CHARACTER
    FIELD BENAMNING AS CHARACTER
    FIELD ENHET AS CHARACTER
    FIELD PRIS AS DECIMAL       
    FIELD ANTAL AS INTEGER     
    FIELD BESTANT AS INTEGER    
    FIELD LEVKOD AS CHARACTER 
    FIELD BERLEV AS CHARACTER
    FIELD DATUM AS DATE 
    FIELD ELNR AS INTEGER
    INDEX ENR IS PRIMARY ENR ASCENDING.
    
DEFINE TEMP-TABLE lin_temp    
    FIELD ENR AS CHARACTER
    FIELD BENAMNING AS CHARACTER
    FIELD ENHET AS CHARACTER
    FIELD PRIS AS INTEGER      
    FIELD METER AS INTEGER
    FIELD TOTMETER AS INTEGER
    FIELD LEDARE AS INTEGER
    FIELD UPPLAG AS INTEGER             
    FIELD LEVKOD AS CHARACTER 
    FIELD DATUM AS DATE
    INDEX ENR IS PRIMARY ENR ASCENDING.        
    
DEFINE TEMP-TABLE lin_next    
    FIELD ENR AS CHARACTER
    FIELD BENAMNING AS CHARACTER
    FIELD ENHET AS CHARACTER
    FIELD PRIS AS DECIMAL       
    FIELD ANTAL AS INTEGER     
    FIELD BESTANT AS INTEGER    
    FIELD LEVKOD AS CHARACTER 
    FIELD BERLEV AS CHARACTER        
    FIELD DBEST AS CHARACTER
    FIELD DATUM AS DATE    
    INDEX DATUM DATUM ASCENDING  
    INDEX LEV ENR LEVKOD ASCENDING
    INDEX ENR IS PRIMARY ENR ASCENDING.            
    
DEFINE TEMP-TABLE lin_prev    
    FIELD ENR AS CHARACTER
    FIELD BENAMNING AS CHARACTER
    FIELD ENHET AS CHARACTER
    FIELD PRIS AS DECIMAL       
    FIELD ANTAL AS INTEGER     
    FIELD BESTANT AS INTEGER    
    FIELD LEVKOD AS CHARACTER 
    FIELD BERLEV AS CHARACTER        
    FIELD DBEST AS CHARACTER
    FIELD DATUM AS DATE    
    INDEX DATUM DATUM ASCENDING  
    INDEX LEV ENR LEVKOD ASCENDING
    INDEX ENR IS PRIMARY ENR ASCENDING.                
    
{DATTABBER.I}

DEFINE TEMP-TABLE depa_mtrl    
    FIELD ENR AS CHARACTER
    FIELD ANTAL AS INTEGER
    INDEX ENR IS PRIMARY ENR ASCENDING.

/*DEFINE TEMP-TABLE ltemp
   FIELD LEVKOD AS CHARACTER
   FIELD LEVNAMN AS CHARACTER.   */
/* DEFINE QUERY mtrlq FOR BERMTRL.    */
/* DEFINE QUERY nyq FOR BERMTRL.      */
/* DEFINE QUERY linq FOR BERLINKAB.   */
/* DEFINE QUERY nylinq FOR BERLINKAB. */
/* DEFINE QUERY skyddq FOR KSKYDD.    */



DEFINE BUFFER bestbuff FOR best_mtrl. 
DEFINE BUFFER linbuff FOR lin_temp.
DEFINE BUFFER levbuff FOR levtemp.

DEFINE TEMP-TABLE ebest_mtrl3 NO-UNDO LIKE best_mtrl.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A
&Scoped-define BROWSE-NAME BRW_DAT

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES dat_tab best_mtrl bestatlevtemp

/* Definitions for BROWSE BRW_DAT                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_DAT dat_tab.DATUM dat_tab.KLOCKAN 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_DAT 
&Scoped-define QUERY-STRING-BRW_DAT FOR EACH dat_tab NO-LOCK ~
    BY dat_tab.DATUM
&Scoped-define OPEN-QUERY-BRW_DAT OPEN QUERY BRW_DAT FOR EACH dat_tab NO-LOCK ~
    BY dat_tab.DATUM.
&Scoped-define TABLES-IN-QUERY-BRW_DAT dat_tab
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_DAT dat_tab


/* Definitions for BROWSE BRW_MTRL                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_MTRL best_mtrl.ENR best_mtrl.BENAMNING ~
best_mtrl.LEVKOD best_mtrl.LEVNAMN best_mtrl.ANTAL best_mtrl.ENHET ~
best_mtrl.DBEST best_mtrl.BESTANT 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_MTRL 
&Scoped-define QUERY-STRING-BRW_MTRL FOR EACH best_mtrl NO-LOCK ~
    BY best_mtrl.ENR
&Scoped-define OPEN-QUERY-BRW_MTRL OPEN QUERY BRW_MTRL FOR EACH best_mtrl NO-LOCK ~
    BY best_mtrl.ENR.
&Scoped-define TABLES-IN-QUERY-BRW_MTRL best_mtrl
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_MTRL best_mtrl


/* Definitions for BROWSE BRW_STATUS                                    */
&Scoped-define FIELDS-IN-QUERY-BRW_STATUS bestatlevtemp.DATUM ~
bestatlevtemp.TID bestatlevtemp.BESTALLD bestatlevtemp.LEVNAMN ~
bestatlevtemp.ANVANDARE bestatlevtemp.BERDATUM 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_STATUS 
&Scoped-define QUERY-STRING-BRW_STATUS FOR EACH bestatlevtemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_STATUS OPEN QUERY BRW_STATUS FOR EACH bestatlevtemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_STATUS bestatlevtemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_STATUS bestatlevtemp


/* Definitions for FRAME FRAME-A                                        */
&Scoped-define OPEN-BROWSERS-IN-QUERY-FRAME-A ~
    ~{&OPEN-QUERY-BRW_DAT}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BRW_DAT BRW_STATUS BRW_MTRL FBTN_BEST ~
FBTN_VISA FBTN_SKRIV FBTN_RENSA FBTN_MIN FBTN_MER FBTN_DEL FBTN_JAMF ~
CMB_LEVTILL FBTN_LEV FBTN_OK BTN_AVB 
&Scoped-Define DISPLAYED-OBJECTS CMB_LEVTILL FILL-IN-BYT 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD klockan60 WINDOW-1 
FUNCTION klockan60 RETURNS DECIMAL
   ( INPUT ber100 AS DECIMAL ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR WINDOW-1 AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB 
     LABEL "Avbryt" 
     SIZE 14 BY 1.

DEFINE BUTTON FBTN_BEST 
     LABEL "Beställning" 
     SIZE 14 BY 1.

DEFINE BUTTON FBTN_DEL 
     LABEL "Delinköp" 
     SIZE 14 BY 1.

DEFINE BUTTON FBTN_JAMF 
     LABEL "Jämför pris" 
     SIZE 14 BY 1.

DEFINE BUTTON FBTN_LEV 
     LABEL "Byt leverant." 
     SIZE 14 BY 1.

DEFINE BUTTON FBTN_MER 
     LABEL "Öka antal" 
     SIZE 14 BY 1.

DEFINE BUTTON FBTN_MIN 
     LABEL "Från depå" 
     SIZE 14 BY 1.

DEFINE BUTTON FBTN_OK AUTO-END-KEY 
     LABEL "Ok" 
     SIZE 14 BY 1.

DEFINE BUTTON FBTN_RENSA 
     LABEL "Återställ" 
     SIZE 14 BY 1.

DEFINE BUTTON FBTN_SKRIV 
     LABEL "Skriv ut" 
     SIZE 14 BY 1.

DEFINE BUTTON FBTN_VISA 
     LABEL "Visa" 
     SIZE 14 BY 1.

DEFINE VARIABLE CMB_LEVTILL AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "0" 
     DROP-DOWN-LIST
     SIZE 18.25 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-BYT AS CHARACTER FORMAT "X(256)":U INITIAL "Vid byte välj leverantör:" 
      VIEW-AS TEXT 
     SIZE 14 BY .63 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_DAT FOR 
      dat_tab SCROLLING.

DEFINE QUERY BRW_MTRL FOR 
      best_mtrl SCROLLING.

DEFINE QUERY BRW_STATUS FOR 
      bestatlevtemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_DAT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_DAT WINDOW-1 _STRUCTURED
  QUERY BRW_DAT NO-LOCK DISPLAY
      dat_tab.DATUM COLUMN-LABEL "Ändringsdatum" FORMAT "99/99/99":U
      dat_tab.KLOCKAN COLUMN-LABEL "Klockan" FORMAT ">9.99":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SIZE 23.88 BY 5.25
         TITLE "Välj Beställning:".

DEFINE BROWSE BRW_MTRL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_MTRL WINDOW-1 _STRUCTURED
  QUERY BRW_MTRL NO-LOCK DISPLAY
      best_mtrl.ENR COLUMN-LABEL "Enr" FORMAT "X(11)":U
      best_mtrl.BENAMNING COLUMN-LABEL "Benämning" FORMAT "x(40)":U
            WIDTH 47
      best_mtrl.LEVKOD COLUMN-LABEL "Lev-id" FORMAT "X(4)":U
      best_mtrl.LEVNAMN COLUMN-LABEL "Leverantör" FORMAT "X(256)":U
            WIDTH 10
      best_mtrl.ANTAL COLUMN-LABEL "Antal" FORMAT ">>>,>>9":U
      best_mtrl.ENHET COLUMN-LABEL "Enhet" FORMAT "x(5)":U WIDTH 6
      best_mtrl.DBEST COLUMN-LABEL "Till" FORMAT "X(5)":U WIDTH 6
      best_mtrl.BESTANT COLUMN-LABEL "Saldo" FORMAT "->,>>>,>>9":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING MULTIPLE SIZE 94 BY 20.25
         TITLE "Materiel för inköp. Välj leverantör och gör din beställning".

DEFINE BROWSE BRW_STATUS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_STATUS WINDOW-1 _STRUCTURED
  QUERY BRW_STATUS NO-LOCK DISPLAY
      bestatlevtemp.DATUM COLUMN-LABEL "Datum" FORMAT "99/99/99":U
      bestatlevtemp.TID FORMAT "99.99":U
      bestatlevtemp.BESTALLD FORMAT "X(18)":U
      bestatlevtemp.LEVNAMN COLUMN-LABEL "Leverantör" FORMAT "x(256)":U
            WIDTH 40
      bestatlevtemp.ANVANDARE COLUMN-LABEL "Användare" FORMAT "x(12)":U
      bestatlevtemp.BERDATUM COLUMN-LABEL "Ber.datum" FORMAT "99/99/99":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 84.5 BY 5.25
         TITLE "Endast visning av beställningsstatus".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     BRW_DAT AT ROW 2 COL 1.5
     BRW_STATUS AT ROW 2 COL 28.75
     BRW_MTRL AT ROW 7.75 COL 1.5
     FBTN_BEST AT ROW 7.92 COL 99.25
     FBTN_VISA AT ROW 9.04 COL 99.25
     FBTN_SKRIV AT ROW 10.13 COL 99.25
     FBTN_RENSA AT ROW 11.21 COL 99.25
     FBTN_MIN AT ROW 12.33 COL 99.25
     FBTN_MER AT ROW 13.42 COL 99.25
     FBTN_DEL AT ROW 14.5 COL 99.25
     FBTN_JAMF AT ROW 15.58 COL 99.25
     CMB_LEVTILL AT ROW 17.92 COL 94 COLON-ALIGNED NO-LABEL
     FBTN_LEV AT ROW 18.92 COL 99.25
     FBTN_OK AT ROW 28.17 COL 84.25
     BTN_AVB AT ROW 28.17 COL 99.25
     FILL-IN-BYT AT ROW 17.04 COL 94 COLON-ALIGNED NO-LABEL
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 113.5 BY 28.42.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: 
   Temp-Tables and Buffers:
      TABLE: bestatlevtemp T "?" NO-UNDO temp-db bestatlevtemp
      TABLE: ? T "?" NO-UNDO temp-db dat_tab
      TABLE: ? T "?" NO-UNDO temp-db best_mtrl
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW WINDOW-1 ASSIGN
         HIDDEN             = YES
         TITLE              = "Window 1"
         HEIGHT             = 28.42
         WIDTH              = 114
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
/* BROWSE-TAB BRW_DAT 1 FRAME-A */
/* BROWSE-TAB BRW_STATUS BRW_DAT FRAME-A */
/* BROWSE-TAB BRW_MTRL BRW_STATUS FRAME-A */
ASSIGN 
       BRW_MTRL:MAX-DATA-GUESS IN FRAME FRAME-A         = 10000.

ASSIGN 
       BRW_STATUS:ALLOW-COLUMN-SEARCHING IN FRAME FRAME-A = TRUE.

ASSIGN 
       CMB_LEVTILL:HIDDEN IN FRAME FRAME-A           = TRUE.

ASSIGN 
       FBTN_BEST:HIDDEN IN FRAME FRAME-A           = TRUE.

ASSIGN 
       FBTN_DEL:HIDDEN IN FRAME FRAME-A           = TRUE.

ASSIGN 
       FBTN_JAMF:HIDDEN IN FRAME FRAME-A           = TRUE.

ASSIGN 
       FBTN_LEV:HIDDEN IN FRAME FRAME-A           = TRUE.

ASSIGN 
       FBTN_MER:HIDDEN IN FRAME FRAME-A           = TRUE.

ASSIGN 
       FBTN_MIN:HIDDEN IN FRAME FRAME-A           = TRUE.

ASSIGN 
       FBTN_RENSA:HIDDEN IN FRAME FRAME-A           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-BYT IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       FILL-IN-BYT:READ-ONLY IN FRAME FRAME-A        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-1)
THEN WINDOW-1:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_DAT
/* Query rebuild information for BROWSE BRW_DAT
     _TblList          = "Temp-Tables.dat_tab"
     _Options          = "NO-LOCK"
     _OrdList          = "Temp-Tables.dat_tab.DATUM|yes"
     _FldNameList[1]   > Temp-Tables.dat_tab.DATUM
"dat_tab.DATUM" "Ändringsdatum" ? "date" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.dat_tab.KLOCKAN
"dat_tab.KLOCKAN" "Klockan" ">9.99" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE BRW_DAT */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_MTRL
/* Query rebuild information for BROWSE BRW_MTRL
     _TblList          = "Temp-Tables.best_mtrl"
     _Options          = "NO-LOCK"
     _OrdList          = "Temp-Tables.best_mtrl.ENR|yes"
     _FldNameList[1]   > Temp-Tables.best_mtrl.ENR
"best_mtrl.ENR" "Enr" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.best_mtrl.BENAMNING
"best_mtrl.BENAMNING" "Benämning" ? "character" ? ? ? ? ? ? no ? no no "47" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.best_mtrl.LEVKOD
"best_mtrl.LEVKOD" "Lev-id" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.best_mtrl.LEVNAMN
"best_mtrl.LEVNAMN" "Leverantör" "X(256)" "character" ? ? ? ? ? ? no ? no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.best_mtrl.ANTAL
"best_mtrl.ANTAL" "Antal" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > Temp-Tables.best_mtrl.ENHET
"best_mtrl.ENHET" "Enhet" ? "character" ? ? ? ? ? ? no ? no no "6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > Temp-Tables.best_mtrl.DBEST
"best_mtrl.DBEST" "Till" ? "character" ? ? ? ? ? ? no ? no no "6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > Temp-Tables.best_mtrl.BESTANT
"best_mtrl.BESTANT" "Saldo" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_MTRL */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_STATUS
/* Query rebuild information for BROWSE BRW_STATUS
     _TblList          = "Temp-Tables.bestatlevtemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.bestatlevtemp.DATUM
"bestatlevtemp.DATUM" "Datum" ? "date" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   = Temp-Tables.bestatlevtemp.TID
     _FldNameList[3]   = Temp-Tables.bestatlevtemp.BESTALLD
     _FldNameList[4]   > Temp-Tables.bestatlevtemp.LEVNAMN
"bestatlevtemp.LEVNAMN" "Leverantör" "x(256)" "character" ? ? ? ? ? ? no ? no no "40" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.bestatlevtemp.ANVANDARE
"bestatlevtemp.ANVANDARE" "Användare" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > Temp-Tables.bestatlevtemp.BERDATUM
"bestatlevtemp.BERDATUM" "Ber.datum" ? "date" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_STATUS */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-A
/* Query rebuild information for FRAME FRAME-A
     _Query            is NOT OPENED
*/  /* FRAME FRAME-A */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME BRW_DAT
&Scoped-define SELF-NAME BRW_DAT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_DAT WINDOW-1
ON VALUE-CHANGED OF BRW_DAT IN FRAME FRAME-A /* Välj Beställning: */
DO:        
   FIND FIRST dat_tab NO-LOCK NO-ERROR.
   IF AVAILABLE dat_tab THEN DO:   
      ASSIGN
      status-ok = BRW_DAT:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
      ASSIGN
      klockansek = dat_tab.DELNR
      datvar = dat_tab.DATUM.
      /* fler best per dag          */
      FIND FIRST best_mtrl WHERE best_mtrl.DATUM = dat_tab.DATUM AND best_mtrl.DELNR = dat_tab.DELNR NO-ERROR.
      IF AVAILABLE best_mtrl THEN DO:
         IF bestvar = TRUE THEN bestvar = bestvar.
         ELSE DO:
            IF best_mtrl.KLAR = TRUE THEN DO:
               ASSIGN         
               FBTN_MER:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
               FBTN_MIN:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
               FBTN_RENSA:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
               FBTN_DEL:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
               FBTN_LEV:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
               CMB_LEVTILL:HIDDEN IN FRAME {&FRAME-NAME} = TRUE 
               FILL-IN-BYT:HIDDEN IN FRAME {&FRAME-NAME} = TRUE. 
            END.
            ELSE DO:             
               ASSIGN         
               FBTN_MER:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
               FBTN_MIN:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
               FBTN_RENSA:HIDDEN IN FRAME {&FRAME-NAME} = FALSE               
               FBTN_LEV:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
               CMB_LEVTILL:HIDDEN IN FRAME {&FRAME-NAME} = FALSE 
               FILL-IN-BYT:HIDDEN IN FRAME {&FRAME-NAME} = FALSE. 
               /*IF Guru.Konstanter:globforetag = "ELPA" OR Guru.Konstanter:globforetag = "CELPA" THEN DO:*/
                  IF delbest = TRUE THEN
                  FBTN_DEL:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
               /*END.
               IF Guru.Konstanter:globforetag = "umea" THEN DO:
                  ASSIGN
                  FBTN_MER:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
                  FBTN_MIN:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
                  IF delbest = TRUE THEN
                  FBTN_DEL:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
               END.*/
            END.
         END.       
      END.  
      /* fler best per dag          */
      helpvar = " WHERE best_mtrl.DATUM = " + STRING(dat_tab.DATUM) + " AND best_mtrl.DELNR = " + STRING(dat_tab.DELNR).
      RUN setcolsortvar_UI IN brwproc[2] (INPUT helpvar) .
      RUN openbdynspec_UI IN brwproc[2].
      /*
      OPEN QUERY BRW_MTRL FOR EACH best_mtrl WHERE best_mtrl.DATUM = dat_tab.DATUM
      BY best_mtrl.ENR. 
      */
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB WINDOW-1
ON CHOOSE OF BTN_AVB IN FRAME FRAME-A /* Avbryt */
DO:
   IF bestvar = FALSE THEN DO:
      MESSAGE "OBS! Vill du spara dina ändringar?"
      VIEW-AS ALERT-BOX
      QUESTION BUTTONS YES-NO TITLE "Spara ändringar?" UPDATE svar.         
      IF svar THEN DO:
         APPLY "CHOOSE" TO FBTN_OK IN FRAME {&FRAME-NAME}.
      END.
      ELSE IF NOT svar THEN DO: 
         RUN delete_UI.                           
         APPLY "CLOSE":U TO THIS-PROCEDURE.   
         RUN slutrens_UI.
      END.                    
      ELSE DO:
         musz = musz.
         RETURN NO-APPLY.
      END.  
   END.
   ELSE DO:
      MESSAGE "Ni har gjort en beställning och uppgifterna kommer att lagras."
      VIEW-AS ALERT-BOX TITLE "Meddelande".
      APPLY "CHOOSE" TO FBTN_OK IN FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_BEST
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_BEST WINDOW-1
ON CHOOSE OF FBTN_BEST IN FRAME FRAME-A /* Beställning */
DO:  
   {muswait.i}         
   ASSIGN
   status-ok = BRW_DAT:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
   ASSIGN
   klockansek = dat_tab.DELNR
   datvar = dat_tab.DATUM.
   RUN datlev_UI IN berinkopapph (INPUT valaonr,INPUT valomrade, INPUT TABLE dat_tab).

   IF gammal = TRUE THEN DO:
      IF delbest = FALSE THEN DO:      
         FIND FIRST dat_tab USE-INDEX DATUM NO-LOCK NO-ERROR.
         IF datvar > dat_tab.DATUM THEN gammal2 = TRUE.
         ELSE gammal2 = FALSE.
      END.
      ELSE DO:
         
         /* fler best per dag*/
         FIND FIRST kon_val WHERE DATE(TRIM(SUBSTRING(kon_val.ANMARK,1,15))) = datvar AND
         INTEGER(TRIM(SUBSTRING(kon_val.ANMARK,20,15))) = dat_tab.DELNR
         NO-LOCK NO-ERROR.
         IF NOT AVAILABLE kon_val THEN DO:            
            EMPTY TEMP-TABLE kon_val NO-ERROR.            
            RUN kolldel3_UI IN berinkopapph 
            (INPUT valaonr,INPUT valomrade, INPUT datvar, OUTPUT TABLE kon_val, INPUT Guru.Konstanter:globforetag).                                       
         END.
         IF visgamb = TRUE THEN DO:         
            /*test hämta kon_val för redan gjorda delbest beställnignar för att få ut byggprotokollet i gammal delbest*/
            FIND FIRST kon_val WHERE DATE(TRIM(SUBSTRING(kon_val.ANMARK,1,15))) = datvar AND
            INTEGER(TRIM(SUBSTRING(kon_val.ANMARK,20,15))) = dat_tab.DELNR
            NO-LOCK NO-ERROR.
            IF NOT AVAILABLE kon_val THEN DO:            
               EMPTY TEMP-TABLE kon_val NO-ERROR.            
               RUN kolldel4_UI IN berinkopapph 
               (INPUT valaonr,INPUT valomrade, INPUT datvar,INPUT dat_tab.DELNR, OUTPUT TABLE kon_val, INPUT Guru.Konstanter:globforetag).                                       
            END.            
         END.
         gammal2 = FALSE.
      END.
   END.
   IF valev NE "" THEN DO:
      FIND FIRST levtemp WHERE levtemp.LEVKOD = valev NO-ERROR.
   END.
   ELSE DO:
     FIND FIRST huvlevtemp WHERE huvlevtemp.DEP-NR = 999 NO-LOCK NO-ERROR. 
      FIND FIRST levtemp WHERE levtemp.LEVKOD = huvlevtemp.LEVKOD
      NO-LOCK NO-ERROR.
   END.
   
   /* lena 20090121 vald_lev utbytt mot valev . Sök efter Beredningens leverantör- ej huvudlev
   FIND FIRST huvlevtemp WHERE huvlevtemp.DEP-NR = 999 NO-LOCK NO-ERROR. 
   FIND FIRST levtemp WHERE levtemp.LEVKOD = huvlevtemp.LEVKOD
   NO-LOCK NO-ERROR. 
   /* fler best per dag*/
   FIND FIRST best_mtrl WHERE best_mtrl.DATUM = datvar AND best_mtrl.DELNR = dat_tab.DELNR AND
   (best_mtrl.LEVKOD NE huvlevtemp.LEVKOD AND best_mtrl.LEVKOD NE "0")
   NO-LOCK NO-ERROR. */
   /* fler best per dag*/
   FIND FIRST best_mtrl WHERE best_mtrl.DATUM = datvar AND best_mtrl.DELNR = dat_tab.DELNR AND
   (best_mtrl.LEVKOD NE levtemp.LEVKOD AND best_mtrl.LEVKOD NE "0")
   NO-LOCK NO-ERROR. 
   IF AVAILABLE best_mtrl THEN DO:
      IF best_mtrl.KLAR = FALSE THEN DO:
         lve = " ".
         flerlev = "".
         FOR EACH best_mtrl WHERE best_mtrl.LEVKOD NE levtemp.LEVKOD AND best_mtrl.DATUM = datvar AND 
         best_mtrl.DELNR = dat_tab.DELNR AND best_mtrl.ANTAL > 0 AND best_mtrl.LEVKOD NE "0" BY best_mtrl.LEVKOD:        
            FIND FIRST levbuff WHERE levbuff.LEVKOD = best_mtrl.LEVKOD NO-LOCK NO-ERROR.         
            IF levbuff.LEVKOD NE lve THEN DO:  
               flerlev = flerlev + " " + levbuff.LEVNAMN.
               lve = best_mtrl.LEVKOD. 
            END.                
         END.         
         MESSAGE "OBS! Det finns materiel  som ej kommer att beställas från " + levtemp.LEVNAMN + "." SKIP
                 "Följande leverantörer är också använda:" + flerlev SKIP
                 "Ni kan byta leverantör genom att markera artikel och klicka på knappen byt leverantör. Vill Ni fortsätta?"
         VIEW-AS ALERT-BOX
         QUESTION BUTTONS YES-NO TITLE "Byte av leverantör." UPDATE svar.         
         IF NOT svar THEN DO: 
            musz = musz.
            RETURN NO-APPLY.   
         END.                    
      END.   
   END.
   {AVBGOM.I}
   
   RUN BERTRPU.W.
   {AVBFRAM.I}
   
   IF bestvar = TRUE THEN DO:
      ASSIGN         
      FBTN_MER:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      FBTN_MIN:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      FBTN_RENSA:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      FBTN_DEL:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      FBTN_LEV:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      CMB_LEVTILL:HIDDEN IN FRAME {&FRAME-NAME} = TRUE 
      FILL-IN-BYT:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.       
   END.
   ASSIGN
   status-ok = BRW_DAT:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
   ASSIGN
   klockansek = dat_tab.DELNR
   datvar = dat_tab.DATUM.
   RUN status_UI IN berinkopapph (INPUT valaonr,INPUT valomrade, OUTPUT TABLE bestatlevtemp).
   RUN setcolindex_UI IN brwproc[3] (INPUT "DATUM BY TID").
   RUN openbdynspec_UI IN brwproc[3].
   RUN setcolindex_UI IN brwproc[3] (INPUT "").
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_DEL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_DEL WINDOW-1
ON CHOOSE OF FBTN_DEL IN FRAME FRAME-A /* Delinköp */
DO:     
   DEFINE VARIABLE gamdelbest AS LOGICAL NO-UNDO.
   {muswait.i}            
   {AVBGOM.I}   
   gamdelbest = delbest. 
   delbest = TRUE.
   RUN VALKONSTU2.W.   
   IF musz = TRUE THEN DO:
      delbest = gamdelbest. 
      {AVBFRAM.I}
      APPLY "CHOOSE" TO BTN_AVB.
      RETURN NO-APPLY.
   END.
   IF musz = FALSE THEN DO:      
      IF Guru.Konstanter:appcon THEN DO:
         RUN DELINKOP.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
         (INPUT valaonr,INPUT valomrade,INPUT Guru.Konstanter:globforetag,INPUT TABLE kon_val, OUTPUT TABLE mtrl_temp).
      END.
      ELSE DO:
         RUN DELINKOP.P
         (INPUT valaonr,INPUT valomrade,INPUT Guru.Konstanter:globforetag,INPUT TABLE kon_val, OUTPUT TABLE mtrl_temp).      
      END.   
      FIND FIRST mtrl_temp NO-LOCK NO-ERROR.
      IF AVAILABLE mtrl_temp THEN DO:      
         FIND FIRST best_mtrl WHERE best_mtrl.KLAR = FALSE NO-LOCK NO-ERROR.
         /* fler best per dag*/
         FIND FIRST dat_tab WHERE dat_tab.DATUM = best_mtrl.DATUM AND
         dat_tab.DELNR = best_mtrl.DELNR 
         NO-LOCK NO-ERROR.
         IF AVAILABLE dat_tab THEN DELETE dat_tab.
         FOR EACH best_mtrl WHERE best_mtrl.KLAR = FALSE:         
            DELETE best_mtrl.
         END.
         FOR EACH mtrl_temp BREAK BY mtrl_temp.LEVKOD BY mtrl_temp.ENR:                
            ACCUMULATE mtrl_temp.ANTAL (TOTAL BY mtrl_temp.LEVKOD BY mtrl_temp.ENR).       
            IF LAST-OF(mtrl_temp.ENR) THEN DO:
               CREATE best_mtrl.
               ASSIGN 
               best_mtrl.ENR = mtrl_temp.ENR
               best_mtrl.BENAMNING = mtrl_temp.BENAMNING 
               best_mtrl.ENHET = mtrl_temp.ENHET 
               best_mtrl.LEVKOD = mtrl_temp.LEVKOD
               best_mtrl.PRIS = mtrl_temp.PRIS
               best_mtrl.OPRIS = mtrl_temp.PRIS
               best_mtrl.DATUM = mtrl_temp.DATUM
               best_mtrl.ANTAL = (ACCUM TOTAL BY mtrl_temp.ENR mtrl_temp.ANTAL).            
               RUN levnamn_UI.
            END.     
         END.      
         RUN dat_UI.
         OPEN QUERY BRW_DAT FOR EACH dat_tab USE-INDEX DATUM NO-LOCK.
         FIND LAST dat_tab NO-LOCK NO-ERROR.
         IF AVAILABLE dat_tab THEN DO:                  
            dat_rowid = ROWID(dat_tab).      
            RUN repo_UI (INPUT 1, INPUT dat_rowid).
            status-ok = BRW_DAT:SELECT-FOCUSED-ROW() NO-ERROR. 
            APPLY "VALUE-CHANGED" TO BRW_DAT IN FRAME {&FRAME-NAME}.
         END.         
      END.   
      ELSE DO:
         FOR EACH best_mtrl WHERE best_mtrl.KLAR = FALSE:         
            DELETE best_mtrl.
         END.
         RUN openbdynspec_UI IN brwproc[2].
         MESSAGE "Inget materiel att beställa!"
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
         APPLY "CLOSE":U TO THIS-PROCEDURE.
         RUN slutrens_UI.
         RETURN NO-APPLY.
      END.     
      {AVBFRAM.I}  
      END.
   {musarrow.i}   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_JAMF
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_JAMF WINDOW-1
ON CHOOSE OF FBTN_JAMF IN FRAME FRAME-A /* Jämför pris */
DO:
   
   IF musz = TRUE THEN musz = FALSE.   
   IF Guru.Konstanter:mtrlsekvar[6] = TRUE THEN RETURN.   
   {muswait.i}      
   {AVBGOM.I} 
   RUN JAMFLEV.W.
   {AVBFRAM.I}
   {musarrow.i}  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_LEV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_LEV WINDOW-1
ON CHOOSE OF FBTN_LEV IN FRAME FRAME-A /* Byt leverant. */
DO:      
   CMB_LEVTILL = INPUT CMB_LEVTILL.
   EMPTY TEMP-TABLE ebest_mtrl2 NO-ERROR.
   musz = FALSE.
   MESSAGE "OBS! Vill Ni byta levarantör på alla markerade artiklar till " CMB_LEVTILL "?"
   VIEW-AS ALERT-BOX
   QUESTION BUTTONS YES-NO TITLE "Byte av leverantör?" UPDATE svar.         
   IF svar = FALSE THEN DO:
      RETURN NO-APPLY.
   END.
   IF svar THEN DO:
      antal_valda = BRW_MTRL:NUM-SELECTED-ROWS.
      antal_raknare = 1.
      DO WHILE antal_raknare LE antal_valda:                                   
         status-ok = BRW_MTRL:FETCH-SELECTED-ROW(antal_raknare).
         IF AVAILABLE best_mtrl THEN DO:
            RUN setlastrowid_UI IN brwproc[2] (INPUT ROWID(best_mtrl)).
            IF best_mtrl.LEVKOD = "0" THEN DO: 
               MESSAGE TRIM(best_mtrl.BENAMNING) + " är en beställning från depå. Byte av leverantör ej möjligt." 
               VIEW-AS ALERT-BOX TITLE "Meddelande".
               musz = TRUE.
            END.
            ELSE DO: 
               IF best_mtrl.DBEST = "RETUR" THEN DO: 
                  MESSAGE TRIM(best_mtrl.BENAMNING) + " är en retur. Byte av leverantör ej möjligt." 
                  VIEW-AS ALERT-BOX TITLE "Meddelande".
                  musz = TRUE.
               END.
               ELSE DO:   
                  /* fler best per dag */
                  FIND FIRST bestbuff WHERE bestbuff.ENR = best_mtrl.ENR AND 
                  bestbuff.LEVKOD = "0" AND bestbuff.BERLEV = best_mtrl.LEVKOD 
                  AND bestbuff.DATUM = best_mtrl.DATUM AND bestbuff.DELNR = best_mtrl.DELNR NO-LOCK NO-ERROR.
                  IF AVAILABLE bestbuff THEN DO:                     
                     MESSAGE "Beställning från depå finns för " + TRIM(best_mtrl.BENAMNING) + ". Byte av leverantör ej möjligt." 
                     VIEW-AS ALERT-BOX TITLE "Meddelande".
                     musz = TRUE.
                  END.      
               END.   
            END. 
            CREATE ebest_mtrl2.
            BUFFER-COPY best_mtrl TO ebest_mtrl2.
            ebest_mtrl2.MTRLROW = ROWID(best_mtrl).
         END.
         antal_raknare = antal_raknare + 1.   
      END.     
      IF musz = FALSE THEN DO:
         RUN levbyt_UI.          
      END.
      ELSE DO:
         EMPTY TEMP-TABLE ebest_mtrl2 NO-ERROR.
         musz = FALSE.
      END.
   END.   
   RUN refreshbrw_UI IN brwproc[2].
   RUN lastselectdyn_UI IN brwproc[2].
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_MER
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_MER WINDOW-1
ON CHOOSE OF FBTN_MER IN FRAME FRAME-A /* Öka antal */
DO:     
   antal_valda = BRW_MTRL:NUM-SELECTED-ROWS.
   antal_raknare = 1.
   DO WHILE antal_raknare LE antal_valda:                                   
      status-ok = BRW_MTRL:FETCH-SELECTED-ROW(antal_raknare).
      IF AVAILABLE best_mtrl THEN DO:
         IF best_mtrl.LEVKOD = "0" THEN DO: 
            MESSAGE TRIM(best_mtrl.BENAMNING) + " är en beställning från depå. Öka antal ej möjligt." 
            VIEW-AS ALERT-BOX TITLE "Meddelande".
         END.
         ELSE DO: 
            IF best_mtrl.DBEST = "RETUR" THEN DO: 
               MESSAGE TRIM(best_mtrl.BENAMNING) + " är en retur till depå. Öka antal ej möjligt." 
               VIEW-AS ALERT-BOX TITLE "Meddelande".
            END.
            ELSE DO:   
                 /* fler best per dag          */
               FIND FIRST bestbuff WHERE bestbuff.ENR = best_mtrl.ENR AND 
               bestbuff.LEVKOD = "0" AND bestbuff.BERLEV = best_mtrl.LEVKOD AND
               bestbuff.DATUM = best_mtrl.DATUM AND bestbuff.DELNR = best_mtrl.DELNR NO-LOCK NO-ERROR.
               IF NOT AVAILABLE bestbuff THEN DO:
                  {muswait.i}
                  IF best_mtrl.DBEST = "DEPÅ" THEN DO:
                     ASSIGN
                     enrvar = best_mtrl.ENR
                     levvar = best_mtrl.LEVKOD
                     klockansek =  best_mtrl.DELNR
                     datumvar = best_mtrl.DATUM.
                     RUN levnamn_UI.
                     FIND FIRST best_mtrl WHERE best_mtrl.ENR = enrvar AND 
                     best_mtrl.LEVKOD = levvar AND best_mtrl.DBEST = "" AND
                     best_mtrl.DATUM = datumvar AND best_mtrl.DELNR = klockansek NO-LOCK.
                     RUN levnamn_UI.
                  END.         
                              
                  till_rowid = ROWID(best_mtrl).
                  RUN BERDEPAU.W (INPUT ROWID(best_mtrl)). 
                  FIND FIRST best_mtrl WHERE ROWID(best_mtrl) = till_rowid NO-LOCK NO-ERROR.
                  RUN levnamn_UI.
                   /* fler best per dag          */
                  helpvar = " WHERE best_mtrl.DATUM = " + STRING(dat_tab.DATUM) + " AND best_mtrl.DELNR = " + STRING(dat_tab.DELNR).
                  RUN setlastrowid_UI IN brwproc[2] (INPUT till_rowid).
                  
                  {musarrow.i}     
               END.
               ELSE DO:
                  MESSAGE "Beställning från depå finns för " + TRIM(best_mtrl.BENAMNING) + ". Öka antal ej möjligt." 
                  VIEW-AS ALERT-BOX TITLE "Meddelande".
               END.      
            END.   
         END.   
      END.
      antal_raknare = antal_raknare + 1.   
   END.    
   FOR EACH best_mtrl WHERE best_mtrl.LEVKOD = "0":
      RUN levnamn_UI.
   END.
   RUN setcolsortvar_UI IN brwproc[2] (INPUT helpvar) .
   RUN openbdynspec_UI IN brwproc[2].   
   RUN lastselectdyn_UI IN brwproc[2].
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_MIN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_MIN WINDOW-1
ON CHOOSE OF FBTN_MIN IN FRAME FRAME-A /* Från depå */
DO:
   antal_valda = BRW_MTRL:NUM-SELECTED-ROWS.
   antal_raknare = 1.
   DO WHILE antal_raknare LE antal_valda:                                   
      status-ok = BRW_MTRL:FETCH-SELECTED-ROW(antal_raknare). 
      IF AVAILABLE best_mtrl THEN DO:
         IF best_mtrl.DBEST = "DEPÅ" THEN DO: 
            MESSAGE TRIM(best_mtrl.BENAMNING) + " är en utökad beställning. Beställning från depå ej möjlig." 
            VIEW-AS ALERT-BOX TITLE "Meddelande".
         END.
         ELSE DO:  
            IF best_mtrl.DBEST = "RETUR" THEN DO: 
               MESSAGE TRIM(best_mtrl.BENAMNING) + " är en retur till depå. Beställning från depå ej möjlig." 
               VIEW-AS ALERT-BOX TITLE "Meddelande".
            END.
            ELSE DO:
               /* fler best per dag          */
               FIND FIRST bestbuff WHERE bestbuff.ENR = best_mtrl.ENR AND 
               bestbuff.LEVKOD = best_mtrl.LEVKOD AND bestbuff.DBEST = "DEPÅ" 
               AND bestbuff.DATUM = best_mtrl.DATUM AND bestbuff.DELNR = best_mtrl.DELNR NO-LOCK NO-ERROR.
               IF NOT AVAILABLE bestbuff THEN DO:                    
                  IF best_mtrl.LEVKOD = "0" THEN DO: 
                     RUN levnamn_UI.
                     ASSIGN
                     enrvar = best_mtrl.ENR
                     levvar = best_mtrl.BERLEV
                     klockansek =  best_mtrl.DELNR
                     datumvar = best_mtrl.DATUM.
                     FIND FIRST best_mtrl WHERE best_mtrl.ENR = enrvar AND  
                     best_mtrl.LEVKOD = levvar AND best_mtrl.DATUM = datumvar AND best_mtrl.DELNR = klockansek NO-LOCK.
                     RUN levnamn_UI.
                  END.           
                  {muswait.i}   
                  till_rowid = ROWID(best_mtrl).      
                  RUN BERLAGERU.W (INPUT ROWID(best_mtrl)).          
                  {musarrow.i}                     
               END.
               ELSE DO: 
                  MESSAGE "En utökad beställning finns för " + TRIM(best_mtrl.BENAMNING) + ". Beställning från depå ej möjlig." 
                  VIEW-AS ALERT-BOX TITLE "Meddelande".  
               END.   
            END.   
         END.   
      END.
      antal_raknare = antal_raknare + 1.   
   END.   
   /* fler best per dag          */
   helpvar = " WHERE best_mtrl.DATUM = " + STRING(dat_tab.DATUM) + " AND best_mtrl.DELNR = " + STRING(dat_tab.DELNR).   
   FOR EACH best_mtrl WHERE best_mtrl.LEVKOD = "0":
      RUN levnamn_UI.
   END.
   RUN setcolsortvar_UI IN brwproc[2] (INPUT helpvar) .
   RUN openbdynspec_UI IN brwproc[2].
   RUN setlastrowid_UI IN brwproc[2] (INPUT till_rowid).
   RUN lastselectdyn_UI IN brwproc[2].
   /*
   OPEN QUERY BRW_MTRL FOR EACH best_mtrl WHERE best_mtrl.DATUM = dat_tab.DATUM
   BY best_mtrl.ENR.
   FIND best_mtrl WHERE ROWID(best_mtrl) = till_rowid NO-LOCK NO-ERROR.
   RUN repo_UI (INPUT 2, INPUT till_rowid).
   status-ok = BRW_MTRL:SELECT-FOCUSED-ROW() NO-ERROR.         
   */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_OK WINDOW-1
ON CHOOSE OF FBTN_OK IN FRAME FRAME-A /* Ok */
DO:            
   IF delbest = FALSE THEN DO:   
      IF Guru.Konstanter:appcon THEN DO:                           
         RUN OKINKOP.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
         (INPUT valaonr, INPUT valomrade, INPUT TABLE best_mtrl, INPUT gammal, INPUT bestvar, INPUT Guru.Konstanter:globforetag,INPUT klockansek).         
      END.
      ELSE DO:
         RUN OKINKOP.P
         (INPUT valaonr, INPUT valomrade, INPUT TABLE best_mtrl, INPUT gammal, INPUT bestvar, INPUT Guru.Konstanter:globforetag,INPUT klockansek).
      END.   
      musz = TRUE.
   END.
   ELSE DO:      
      IF Guru.Konstanter:appcon THEN DO:                           
         RUN OKDELINKOP.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
         (INPUT valaonr, INPUT valomrade, INPUT TABLE best_mtrl, INPUT TABLE kon_val, INPUT gammal, INPUT bestvar, INPUT Guru.Konstanter:globforetag,INPUT klockansek).
      END.
      ELSE DO:
         RUN OKDELINKOP.P
         (INPUT valaonr, INPUT valomrade, INPUT TABLE best_mtrl, INPUT TABLE kon_val, INPUT gammal, INPUT bestvar, INPUT Guru.Konstanter:globforetag,INPUT klockansek).
      END.   
      musz = TRUE.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_RENSA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_RENSA WINDOW-1
ON CHOOSE OF FBTN_RENSA IN FRAME FRAME-A /* Återställ */
DO:  
   MESSAGE "Vill du återställa inköpsprotokollet? Vid återställning avslutas inköpsrutinen."
   VIEW-AS ALERT-BOX
   QUESTION BUTTONS YES-NO TITLE "Återställa?" UPDATE svar. 
   IF svar THEN DO:    
      IF gammal = TRUE THEN DO:  
         RUN rensa_UI IN berinkopapph (INPUT valaonr,INPUT valomrade, INPUT delbest,INPUT TABLE dat_tab,OUTPUT felbest).
         IF felbest = TRUE THEN DO:
            MESSAGE "Du har något tekniskt fel på din beställning!" SKIP  
            "Kontakta Elpool 090-184540 omgående!" VIEW-AS ALERT-BOX.
         END.
         RUN delete_UI.                           
      END.               
      APPLY "CLOSE":U TO THIS-PROCEDURE.
      RUN slutrens_UI.
   END.   
   /*
   MESSAGE "OBS! Återställning av inköpsprotokoll kommer att ske. Vill du avsluta inköpsrutinen när det är klart?"
   VIEW-AS ALERT-BOX
   QUESTION BUTTONS YES-NO-CANCEL TITLE "Återställa?" UPDATE svar. 
   IF svar THEN DO:    
      IF gammal = TRUE THEN DO:  
         RUN rensa_UI IN berinkopapph (INPUT valaonr,INPUT valomrade, INPUT delbest).        
         RUN delete_UI.                           
      END.               
      APPLY "CLOSE":U TO THIS-PROCEDURE.            
   END.   
   ELSE IF NOT svar THEN DO:
      EMPTY TEMP-TABLE best_mtrl NO-ERROR. 
      EMPTY TEMP-TABLE prev_temp NO-ERROR. 
      EMPTY TEMP-TABLE next_temp NO-ERROR. 
      EMPTY TEMP-TABLE lin_prev NO-ERROR. 
      EMPTY TEMP-TABLE lin_next NO-ERROR. 
      EMPTY TEMP-TABLE mtrl_temp NO-ERROR. 
      EMPTY TEMP-TABLE lin_temp NO-ERROR.      
      EMPTY TEMP-TABLE dat_tab NO-ERROR. 
      IF gammal = TRUE THEN DO:
         RUN finnsbest_UI.
      END.
      ELSE DO:    
         RUN ny_UI.
      END. 
      OPEN QUERY BRW_DAT FOR EACH dat_tab USE-INDEX DATUM NO-LOCK.
      FIND FIRST dat_tab WHERE dat_tab.DATUM = datvar NO-LOCK NO-ERROR.
      dat_rowid = ROWID(dat_tab).      
      RUN repo_UI (INPUT 1, INPUT dat_rowid).
      status-ok = BRW_DAT:SELECT-FOCUSED-ROW() NO-ERROR.     
      /* fler best per dag          */
      helpvar = " WHERE best_mtrl.DATUM = " + STRING(dat_tab.DATUM) + " AND best_mtrl.DELNR = " + STRING(dat_tab.DELNR).
      RUN setcolsortvar_UI IN brwproc[2] (INPUT helpvar) .
      RUN openbdynspec_UI IN brwproc[2].
      /*
      OPEN QUERY BRW_MTRL FOR EACH best_mtrl WHERE best_mtrl.DATUM = dat_tab.DATUM
      BY best_mtrl.ENR. 
      */
   END.
   ELSE DO: 
      musz = musz.
   END.
   */                    
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
      priset = FALSE.
      MESSAGE "OBS! Vill du skriva ut med pris?"
      VIEW-AS ALERT-BOX
      QUESTION BUTTONS YES-NO TITLE "Pris?" UPDATE svar.         
      IF svar THEN DO:
         priset = TRUE.
         IF Guru.Konstanter:mtrlsekvar[6] = TRUE THEN priset = FALSE.
      END.
      ELSE DO:
         priset = FALSE.
      END.
      RUN getquery_UI IN brwproc[2] (OUTPUT komsort).
      {AVBGOM.I}      
      RUN VISINKOPU.W (INPUT komsort).
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
   priset = FALSE.
   MESSAGE "OBS! Vill du visa med pris?"
   VIEW-AS ALERT-BOX
   QUESTION BUTTONS YES-NO TITLE "Pris?" UPDATE svar.         
   IF svar THEN DO:
      priset = TRUE.
      IF Guru.Konstanter:mtrlsekvar[6] = TRUE THEN priset = FALSE.
   END.
   ELSE DO:
      priset = FALSE.
   END.      
   
   RUN getquery_UI IN brwproc[2] (OUTPUT komsort).
   {AVBGOM.I}
   RUN VISINKOPU.W (INPUT komsort).
   {AVBFRAM.I}
   {musarrow.i}  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK WINDOW-1 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* These events will close the window and terminate the procedure.      */
/* (NOTE: this will override any user-defined triggers previously       */
/*  defined on the window.)                                             */
ON WINDOW-CLOSE OF {&WINDOW-NAME} DO:          
   IF musz = FALSE THEN DO:
      IF bestvar = FALSE THEN DO:
         MESSAGE "OBS! Vill du spara dina ändringar?"
         VIEW-AS ALERT-BOX
         QUESTION BUTTONS YES-NO TITLE "Spara ändringar?" UPDATE svar.         
         IF svar THEN DO:
            APPLY "CHOOSE" TO FBTN_OK IN FRAME {&FRAME-NAME}.
         END.
         ELSE IF NOT svar THEN DO: 
            RUN delete_UI.                           
            APPLY "CLOSE":U TO THIS-PROCEDURE.
           
         END.                    
         ELSE DO:
            musz = musz.
            RETURN NO-APPLY.
         END.  
      END.
      ELSE DO:
         MESSAGE "Ni har gjort en beställning och uppgifterna kommer att lagras."
         VIEW-AS ALERT-BOX TITLE "Meddelande".
         APPLY "CHOOSE" TO FBTN_OK IN FRAME {&FRAME-NAME}.
      END.   
   END.
   ELSE DO:
      musz = FALSE.   
      APPLY "CLOSE":U TO THIS-PROCEDURE.
   END.
   RUN slutrens_UI.
   
END.
ON ENDKEY, END-ERROR OF {&WINDOW-NAME} ANYWHERE DO:   
   IF musz = FALSE THEN DO:
      MESSAGE "OBS! Vill du spara dina ändringar?"
      VIEW-AS ALERT-BOX
      QUESTION BUTTONS YES-NO TITLE "Spara ändringar?" UPDATE svar.         
      IF svar THEN DO:
         APPLY "CHOOSE" TO FBTN_OK IN FRAME {&FRAME-NAME}.
      END.
      ELSE IF NOT svar THEN DO:    
         RUN delete_UI. 
         FIND LAST dat_tab USE-INDEX DATUM NO-LOCK NO-ERROR.
         RUN delete_UI IN berinkopapph (INPUT kalkrow,INPUT datvar,INPUT klockansek).
         APPLY "CLOSE":U TO THIS-PROCEDURE.
      END.                    
      ELSE DO:
         musz = musz.
         RETURN NO-APPLY.
      END.   
   END.   
   ELSE DO:
      musz = FALSE.   
      APPLY "CLOSE":U TO THIS-PROCEDURE.
   END.   
   RUN slutrens_UI.
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
   klockansek = TIME.
   IF klockansek = 0 THEN klockansek = 1.
   EMPTY TEMP-TABLE best_mtrl NO-ERROR. 
   EMPTY TEMP-TABLE prev_temp NO-ERROR. 
   EMPTY TEMP-TABLE next_temp NO-ERROR. 
   EMPTY TEMP-TABLE lin_prev NO-ERROR. 
   EMPTY TEMP-TABLE lin_next NO-ERROR.
   EMPTY TEMP-TABLE mtrl_temp NO-ERROR. 
   EMPTY TEMP-TABLE lin_temp NO-ERROR.   
   EMPTY TEMP-TABLE dat_tab NO-ERROR.    
   
   RUN laddatemptab_UI IN berinkopapph 
      (INPUT valaonr,
       INPUT valomrade,
       OUTPUT kalkrow,
       OUTPUT TABLE bermtrltemp,
       OUTPUT TABLE kskyddtemp,OUTPUT TABLE berlinkabtemp).

   ASSIGN
   gammal = FALSE
   gammal2 = FALSE.      
   {&WINDOW-NAME}:TITLE = "Inköpsprotokoll för beredning nr:" + STRING(valaonr) + " benämning:" + valort.
   FIND FIRST bermtrltemp WHERE bermtrltemp.AONR = valaonr AND 
   bermtrltemp.OMRADE = valomrade AND bermtrltemp.INKOP = TRUE USE-INDEX INKOP NO-LOCK NO-ERROR.
   IF AVAILABLE bermtrltemp THEN DO:  
      RUN finnsbest_UI.
      IF musz = TRUE THEN DO:
         LEAVE MAIN-BLOCK. 
      END.      
   END.
   ELSE DO: 
      RUN ny_UI.      
      IF musz = TRUE THEN DO:
         APPLY "CLOSE":U TO THIS-PROCEDURE.
         RUN slutrens_UI.
         LEAVE MAIN-BLOCK. 
      END.      
   END.        
   IF Guru.Konstanter:globforetag = "CELPA" OR Guru.Konstanter:globforetag = "LULE" OR Guru.Konstanter:globforetag = "BODE" THEN DO:
      EMPTY TEMP-TABLE depa_mtrl NO-ERROR. 
      RUN laddadepsaldo_UI IN berinkopapph (OUTPUT TABLE depa_mtrl).   
      FOR EACH best_mtrl:
         FIND FIRST depa_mtrl WHERE depa_mtrl.ENR = best_mtrl.ENR NO-LOCK NO-ERROR.
         IF AVAILABLE depa_mtrl THEN DO:
            best_mtrl.BESTANT = depa_mtrl.ANTAL.
         END.
      END.
   END.
   IF varforetypval[29] = 1  THEN DO:                 
      /*nettopris beredning inköp*/               
      EMPTY TEMP-TABLE ikmtrltemp NO-ERROR.       
      RUN innettomark_UI IN nettoh (INPUT TABLE best_mtrl, OUTPUT TABLE ikmtrltemp).      
      
   END.   
   status-ok = CMB_LEVTILL:DELETE("0"). 
   FOR EACH levtemp WHERE levtemp.LEVKOD NE "0" AND levtemp.LEVKOD NE "99" AND 
      levtemp.BORTTAG = FALSE USE-INDEX LEV NO-LOCK:              
      status-ok = CMB_LEVTILL:ADD-LAST(levtemp.LEVNAMN)IN FRAME {&FRAME-NAME}.              
   END.
   
   RUN hamtlev_UI IN nyberapph (INPUT valomrade,INPUT valaonr,OUTPUT valev).
   IF valev = "" THEN DO:
      FIND FIRST huvlevtemp WHERE huvlevtemp.DEP-NR = 999 NO-LOCK NO-ERROR.
      IF AVAILABLE huvlevtemp THEN DO:
         ASSIGN
         valev = huvlevtemp.LEVKOD.
      END.
   END.
   FIND FIRST levtemp WHERE levtemp.LEVKOD = valev
   USE-INDEX LEV NO-LOCK NO-ERROR.
   
   IF NOT AVAILABLE levtemp THEN DO:
      FIND FIRST levtemp USE-INDEX LEV NO-LOCK NO-ERROR.
   END.
   IF AVAILABLE levtemp THEN DO:         
      CMB_LEVTILL:SCREEN-VALUE = levtemp.LEVNAMN.  
   END.
   RUN enable_UI.   
   ASSIGN
   
   FBTN_JAMF:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.   
   IF Guru.Konstanter:globforetag = "CELPA" OR Guru.Konstanter:globforetag = "LULE" THEN DO:      
      musz = musz.
   END.
   ELSE DO:
      best_mtrl.BESTANT:VISIBLE IN BROWSE BRW_MTRL = FALSE.
      best_mtrl.BENAMNING:WIDTH IN BROWSE BRW_MTRL = 50.
   END.
   {FRMSIZE.I}     
   /*OPEN QUERY BRW_DAT FOR EACH dat_tab USE-INDEX DATUM NO-LOCK.*/
   FIND LAST dat_tab NO-LOCK NO-ERROR.
   IF AVAILABLE dat_tab THEN DO:                  
      dat_rowid = ROWID(dat_tab).      
      RUN repo_UI (INPUT 1, INPUT dat_rowid).
      status-ok = BRW_DAT:SELECT-FOCUSED-ROW() NO-ERROR. 
      APPLY "VALUE-CHANGED" TO BRW_DAT IN FRAME {&FRAME-NAME}.
   END.   
   {musarrow.i}
   
   RUN status_UI IN berinkopapph (INPUT valaonr,INPUT valomrade, OUTPUT TABLE bestatlevtemp).
   RUN setcolindex_UI IN brwproc[3] (INPUT "DATUM BY TID").
   RUN openbdynspec_UI IN brwproc[3].
   RUN setcolindex_UI IN brwproc[3] (INPUT "").  
   BEST_MTRL.ENR:LABEL IN BROWSE BRW_MTRL = Guru.Konstanter:genk.
   Guru.GlobalaVariabler:colrighth = FBTN_BEST:HANDLE.           
   RUN buttrow_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   Guru.GlobalaVariabler:colrighth = FBTN_VISA:HANDLE.           
   RUN buttrow_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   Guru.GlobalaVariabler:colrighth = FBTN_SKRIV:HANDLE.           
   RUN buttrow_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   Guru.GlobalaVariabler:colrighth = FBTN_RENSA:HANDLE.           
   RUN buttrow_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   Guru.GlobalaVariabler:colrighth = FBTN_MIN:HANDLE.           
   RUN buttrow_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   Guru.GlobalaVariabler:colrighth = FBTN_MER:HANDLE.           
   RUN buttrow_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   Guru.GlobalaVariabler:colrighth = FBTN_DEL:HANDLE.           
   RUN buttrow_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   Guru.GlobalaVariabler:colrighth = FBTN_JAMF:HANDLE. 
   RUN buttrow_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
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
  Purpose:    
  Parameters: 
  Notes:       
-------------------------------------------------------------*/    
   RUN DYNBRW.P PERSISTENT SET brwproc[1] 
      (INPUT BRW_DAT:HANDLE IN FRAME {&FRAME-NAME}).
   RUN DYNBRW.P PERSISTENT SET brwproc[2] 
      (INPUT BRW_MTRL:HANDLE IN FRAME {&FRAME-NAME}).
   RUN DYNBRW.P PERSISTENT SET brwproc[3] 
      (INPUT BRW_STATUS:HANDLE IN FRAME {&FRAME-NAME}).
   RUN dynprogextra IN brwproc[2] (INPUT "rowdispextra_UI",INPUT THIS-PROCEDURE).
   RUN rowdispextrakor IN  brwproc[2] (INPUT TRUE).   
   IF Guru.Konstanter:appcon THEN DO:
      RUN NETTOMARK.P PERSISTENT SET nettoh ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN NETTOMARK.P PERSISTENT SET nettoh.
   END. 
   IF Guru.Konstanter:appcon THEN DO:
      RUN BERINKOPAPP.P PERSISTENT SET berinkopapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN BERINKOPAPP.P PERSISTENT SET berinkopapph.
   END. 
   IF Guru.Konstanter:appcon THEN DO:
      RUN LEVBYTEAPP.P PERSISTENT SET levbytapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN LEVBYTEAPP.P PERSISTENT SET levbytapph.
   END. 
   IF Guru.Konstanter:appcon THEN DO:
      RUN NYBERAPP.P PERSISTENT SET nyberapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT (INPUT varforetypchar[48]). 
   END.
   ELSE DO:
      RUN NYBERAPP.P PERSISTENT SET nyberapph (INPUT varforetypchar[48]).
   END. 
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE bytt_UI WINDOW-1 
PROCEDURE bytt_UI :
/* -----------------------------------------------------------
  Purpose:    
  Parameters: 
  Notes:       
-------------------------------------------------------------*/       
   IF felmedd NE "" THEN DO:
      IF felmedd = "skipkontroll" THEN svar = TRUE.
      ELSE DO:
         MESSAGE felmedd VIEW-AS ALERT-BOX
         QUESTION BUTTONS YES-NO TITLE "Meddelande" UPDATE svar.         
      END.      
      IF svar THEN DO:        
         FIND FIRST ebest_mtrl2 WHERE ROWID(ebest_mtrl2) = mtrl_rowid NO-ERROR.
         /* fler best per dag          */
         FIND FIRST bestbuff WHERE bestbuff.ENR = ebest_mtrl2.ENR AND
         bestbuff.LEVKOD = ebest_mtrl2.LEVKOD AND bestbuff.DATUM = datvar AND bestbuff.DELNR = klockansek
         NO-LOCK NO-ERROR.
         IF AVAILABLE bestbuff THEN DO:               
            bestbuff.ANTAL = bestbuff.ANTAL + ebest_mtrl2.ANTAL.
            FIND best_mtrl WHERE ROWID(best_mtrl) = ebest_mtrl2.MTRLROW NO-LOCK NO-ERROR.
            IF AVAILABLE best_mtrl THEN DO:
               DELETE best_mtrl.
            END.               
            DELETE ebest_mtrl2.
         END.  
         ELSE DO:
            FIND best_mtrl WHERE ROWID(best_mtrl) = ebest_mtrl2.MTRLROW NO-LOCK NO-ERROR.
            IF AVAILABLE best_mtrl THEN DO:
               BUFFER-COPY ebest_mtrl2 TO best_mtrl.
               RUN levnamn_UI.
            END. 
            DELETE ebest_mtrl2.
         END.
      END.
      
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE dat_UI WINDOW-1 
PROCEDURE dat_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/  
   /* fler best per dag*/
  
  FOR EACH best_mtrl:
      IF best_mtrl.DELNR = 0 THEN best_mtrl.DELNR = klockansek.
      FIND FIRST dat_tab WHERE dat_tab.DATUM = best_mtrl.DATUM AND dat_tab.DELNR = best_mtrl.DELNR
      USE-INDEX DATUM NO-LOCK NO-ERROR.
      IF NOT AVAILABLE dat_tab THEN DO:
         CREATE dat_tab.
         ASSIGN
         dat_tab.DELNR = best_mtrl.DELNR
         dat_tab.DATUM = best_mtrl.DATUM.
         dat_tab.KLOCKAN = klockan60(best_mtrl.DELNR / 3600).          
      END.
   END.            
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE delete_UI WINDOW-1 
PROCEDURE delete_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/     
   FIND LAST dat_tab USE-INDEX DATUM NO-LOCK NO-ERROR.
   FIND FIRST best_mtrl WHERE best_mtrl.DATUM = dat_tab.DATUM NO-ERROR.
   IF AVAILABLE best_mtrl THEN DO:      
      IF best_mtrl.KLAR = FALSE THEN DO:  
         RUN delete_UI IN berinkopapph (INPUT kalkrow,INPUT dat_tab.datum,INPUT dat_tab.DELNR).         
         
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE dummykontroll_UI WINDOW-1 
PROCEDURE dummykontroll_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/  
   IF Guru.Konstanter:appcon THEN DO:                           
      RUN DUMINKOP.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT valaonr, INPUT valomrade, OUTPUT kontrollsumma).
   END.
   ELSE DO:
      RUN DUMINKOP.P
      (INPUT valaonr, INPUT valomrade, OUTPUT kontrollsumma).
   END.   
   FOR EACH best_mtrl:       
      ACCUMULATE best_mtrl.ANTAL (TOTAL).             
   END.
   kontrollsumma2 = ACCUM TOTAL best_mtrl.ANTAL.   
   IF kontrollsumma = kontrollsumma2 THEN DO:
      musz = musz.
   END.
   ELSE DO:
      MESSAGE "Ett problem vid summeringen av artiklarna har uppstått. Kontakta Elpool i Umeå 090 - 18 45 45." kontrollsumma  kontrollsumma2
      VIEW-AS ALERT-BOX TITLE "Meddelande".
   END.
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
  DISPLAY CMB_LEVTILL FILL-IN-BYT 
      WITH FRAME FRAME-A IN WINDOW WINDOW-1.
  ENABLE BRW_DAT BRW_STATUS BRW_MTRL FBTN_BEST FBTN_VISA FBTN_SKRIV FBTN_RENSA 
         FBTN_MIN FBTN_MER FBTN_DEL FBTN_JAMF CMB_LEVTILL FBTN_LEV FBTN_OK 
         BTN_AVB 
      WITH FRAME FRAME-A IN WINDOW WINDOW-1.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE finnsbest_UI WINDOW-1 
PROCEDURE finnsbest_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/  
   /*IF Guru.Konstanter:globforetag = "ELPA" OR Guru.Konstanter:globforetag = "UMEA" OR Guru.Konstanter:globforetag = "CELPA" THEN DO:      */
      /*FINNS DET DELBESTÄLLNINGAR*/
      visgamb = FALSE.
      RUN kolldel_UI IN berinkopapph (INPUT valaonr,INPUT valomrade, OUTPUT delbest, INPUT Guru.Konstanter:globforetag).         
      RUN gammal_UI.    
      IF delbest = TRUE THEN DO:   
         /*FINNS DET NÅGOT SOM INTE ÄR BESTÄLLT*/
         RUN kolldel2_UI IN berinkopapph (INPUT valaonr,INPUT valomrade, OUTPUT delmer, INPUT Guru.Konstanter:globforetag).
         IF delmer = TRUE THEN DO:
            FIND FIRST bermtrltemp WHERE bermtrltemp.AONR = valaonr AND 
            bermtrltemp.OMRADE = valomrade AND bermtrltemp.INKOP = TRUE AND bermtrltemp.KLAR = FALSE 
            USE-INDEX INKOP NO-LOCK NO-ERROR.
            IF AVAILABLE bermtrltemp THEN DO:
               /*HÄMTAR KONSTRUKTIONER SOM INTE ÄR DELBESTÄLLDA*/
               RUN kolldel3_UI IN berinkopapph (INPUT valaonr,INPUT valomrade, INPUT bermtrltemp.DATUM, OUTPUT TABLE kon_val, INPUT Guru.Konstanter:globforetag).               
            END.
            ELSE DO:  
               /* fler best per dag*/
               /*
               FIND LAST bermtrltemp WHERE bermtrltemp.AONR = valaonr AND 
               bermtrltemp.OMRADE = valomrade AND bermtrltemp.INKOP = TRUE AND bermtrltemp.KLAR = TRUE 
               USE-INDEX INKOP NO-LOCK NO-ERROR.
               IF AVAILABLE bermtrltemp THEN DO:                  
                  IF bermtrltemp.DATUM = TODAY THEN DO:
                     MESSAGE "Ni har redan gjort ett delinköp idag. Går endast att göra ett / dag" VIEW-AS ALERT-BOX.
                     RETURN.
                  END.
               END.
               */
               MESSAGE "Vill Ni fortsätta med nästa delbeställning?" VIEW-AS ALERT-BOX 
               QUESTION BUTTONS YES-NO TITLE "Delbeställning" UPDATE svar.

               IF svar THEN DO:   
                  RUN VALKONSTU2.W.
                  IF musz = TRUE THEN DO:
                     APPLY "CLOSE":U TO THIS-PROCEDURE.
                     RUN slutrens_UI.
                     RETURN.
                  END.
                  musz = FALSE.
                  IF Guru.Konstanter:appcon THEN DO:
                     RUN DELINKOP.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
                     (INPUT valaonr,INPUT valomrade,INPUT Guru.Konstanter:globforetag,INPUT TABLE kon_val, OUTPUT TABLE mtrl_temp).      
                  END.
                  ELSE DO:
                     RUN DELINKOP.P
                     (INPUT valaonr,INPUT valomrade,INPUT Guru.Konstanter:globforetag,INPUT TABLE kon_val, OUTPUT TABLE mtrl_temp).      
                  END. 
                  FIND FIRST mtrl_temp NO-LOCK NO-ERROR.
                  IF AVAILABLE mtrl_temp THEN DO:                                 
                     FOR EACH mtrl_temp BREAK BY mtrl_temp.LEVKOD BY mtrl_temp.ENR:                
                        ACCUMULATE mtrl_temp.ANTAL (TOTAL BY mtrl_temp.LEVKOD BY mtrl_temp.ENR).       
                        IF LAST-OF(mtrl_temp.ENR) THEN DO:
                           CREATE best_mtrl.
                           ASSIGN 
                           best_mtrl.ENR = mtrl_temp.ENR
                           best_mtrl.BENAMNING = mtrl_temp.BENAMNING 
                           best_mtrl.ENHET = mtrl_temp.ENHET 
                           best_mtrl.LEVKOD = mtrl_temp.LEVKOD
                           best_mtrl.PRIS = mtrl_temp.PRIS
                           best_mtrl.OPRIS = mtrl_temp.PRIS
                           best_mtrl.DATUM = mtrl_temp.DATUM
                           best_mtrl.ANTAL = (ACCUM TOTAL BY mtrl_temp.ENR mtrl_temp.ANTAL).                           
                           RUN levnamn_UI.
                        END.     
                     END.                     
                     RUN dat_UI.
                  END.   
                  ELSE DO:
                     /*CCC*/
                     RUN openbdynspec_UI IN brwproc[2].
                     MESSAGE "Inget ytterligare materiel valt!"                  
                     VIEW-AS ALERT-BOX INFO BUTTONS OK.
                  END.            
               END.               
               ELSE DO:
                  visgamb = TRUE.
               END.
            END.
         END.         
         ELSE DO:
            FIND FIRST bermtrltemp WHERE bermtrltemp.AONR = valaonr AND 
            bermtrltemp.OMRADE = valomrade AND bermtrltemp.INKOP = TRUE AND bermtrltemp.KLAR = FALSE 
            USE-INDEX INKOP NO-LOCK NO-ERROR.
            IF AVAILABLE bermtrltemp THEN DO:
               RUN kolldel3_UI IN berinkopapph (INPUT valaonr,INPUT valomrade, INPUT bermtrltemp.DATUM, OUTPUT TABLE kon_val, INPUT Guru.Konstanter:globforetag).               
            END.
         END.
      END.
   /*END.
   ELSE DO:   
      delbest = FALSE.
      RUN gammal_UI.
   END.*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE gammal2_UI WINDOW-1 
PROCEDURE gammal2_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/  
   FIND LAST best_mtrl USE-INDEX DATUM NO-LOCK.      
   datvar = best_mtrl.DATUM.
   
   /*IF Guru.Konstanter:globforetag = "ELPA" OR Guru.Konstanter:globforetag = "UMEA" OR Guru.Konstanter:globforetag = "CELPA" THEN DO:*/
   RUN gammalume_UI IN berinkopapph (INPUT valaonr,INPUT valomrade, INPUT datvar, INPUT Guru.Konstanter:globforetag, OUTPUT TABLE best_mtrl APPEND).
   FOR EACH best_mtrl WHERE best_mtrl.LEVNAMN = "":
      RUN levnamn_UI.
   END.
   /*END.
   ELSE DO:   
      FIND FIRST bermtrltemp WHERE bermtrltemp.AONR = valaonr AND 
      bermtrltemp.OMRADE = valomrade AND bermtrltemp.INKOP = FALSE AND
      bermtrltemp.DATUM > datvar USE-INDEX DATUM NO-LOCK NO-ERROR.
      IF AVAILABLE bermtrltemp THEN DO:       
         ASSIGN
         nextvar = TRUE.       
         OPEN QUERY nyq FOR EACH bermtrltemp WHERE bermtrltemp.AONR = valaonr AND 
         bermtrltemp.OMRADE = valomrade AND bermtrltemp.INKOP = FALSE AND
         bermtrltemp.DATUM > datvar USE-INDEX DATUM NO-LOCK.
         GET FIRST nyq NO-LOCK.
         DO WHILE AVAILABLE(bermtrltemp):
            IF bermtrltemp.ANTAL > 0 THEN DO:
               RUN mtrl_UI.
            END.      
            GET NEXT nyq NO-LOCK.
         END.          
         CLOSE QUERY nyq. 
   
         OPEN QUERY skyddq FOR EACH kskyddtemp WHERE kskyddtemp.AONR = valaonr AND 
         kskyddtemp.OMRADE = valomrade AND kskyddtemp.BERED = TRUE AND
         kskyddtemp.DATUM > datvar USE-INDEX OMR NO-LOCK.
         GET FIRST skyddq NO-LOCK.
         DO WHILE AVAILABLE(kskyddtemp):             
            RUN skydd_UI.           
            GET NEXT skyddq NO-LOCK.
         END.          
         CLOSE QUERY skyddq.
         RUN summa2_UI. 
         nextvar = FALSE.  
         EMPTY TEMP-TABLE mtrl_temp NO-ERROR.          
         OPEN QUERY nyq FOR EACH bermtrltemp WHERE bermtrltemp.AONR = valaonr AND 
         bermtrltemp.OMRADE = valomrade AND bermtrltemp.INKOP = FALSE AND
         bermtrltemp.DATUM = datvar USE-INDEX DATUM NO-LOCK.
         GET FIRST nyq NO-LOCK.
         DO WHILE AVAILABLE(bermtrltemp):
            IF bermtrltemp.ANTAL > 0 THEN DO:
               RUN mtrl_UI.
            END.      
            GET NEXT nyq NO-LOCK.
         END.          
         CLOSE QUERY nyq.   
         OPEN QUERY skyddq FOR EACH kskyddtemp WHERE kskyddtemp.AONR = valaonr AND 
         kskyddtemp.OMRADE = valomrade AND kskyddtemp.BERED = TRUE AND
         kskyddtemp.DATUM = datvar USE-INDEX OMR NO-LOCK.
         GET FIRST skyddq NO-LOCK.
         DO WHILE AVAILABLE(kskyddtemp):             
            RUN skydd_UI.           
            GET NEXT skyddq NO-LOCK.
         END.          
         CLOSE QUERY skyddq.
         RUN summa2_UI. 
         nextvar = TRUE.
         EMPTY TEMP-TABLE lin_temp NO-ERROR.          
         OPEN QUERY nylinq FOR EACH berlinkabtemp WHERE berlinkabtemp.AONR = valaonr AND 
         berlinkabtemp.OMRADE = valomrade AND berlinkabtemp.DATUM > datvar AND berlinkabtemp.KORTKOD = ?
         USE-INDEX INKOP NO-LOCK.
         GET FIRST nylinq NO-LOCK.
         DO WHILE AVAILABLE(berlinkabtemp):
            IF berlinkabtemp.METER > 0 THEN DO:
               RUN linor_UI.
            END.      
            GET NEXT nylinq NO-LOCK.
         END.          
         CLOSE QUERY nylinq.
         RUN sumlin2_UI.  
         nextvar = FALSE.
         EMPTY TEMP-TABLE lin_temp NO-ERROR.          
         OPEN QUERY nylinq FOR EACH berlinkabtemp WHERE berlinkabtemp.AONR = valaonr AND 
         berlinkabtemp.OMRADE = valomrade AND berlinkabtemp.DATUM = datvar AND 
         berlinkabtemp.KORTKOD = ?
         USE-INDEX INKOP NO-LOCK.
         GET FIRST nylinq NO-LOCK.
         DO WHILE AVAILABLE(berlinkabtemp):
            IF berlinkabtemp.METER > 0 THEN DO:
               RUN linor_UI.
            END.      
            GET NEXT nylinq NO-LOCK.
         END.          
         CLOSE QUERY nylinq. 
         RUN sumlin2_UI.       
         FIND FIRST next_temp NO-ERROR.
         IF AVAILABLE next_temp THEN DO:         
            datvar = next_temp.DATUM.
            FOR EACH next_temp:
               FIND FIRST prev_temp WHERE prev_temp.ENR = next_temp.ENR AND
               prev_temp.LEVKOD = next_temp.LEVKOD USE-INDEX LEV NO-ERROR.
               IF AVAILABLE prev_temp THEN DO:
                  IF next_temp.ANTAL > prev_temp.ANTAL THEN DO:
                     CREATE best_mtrl.
                     ASSIGN                                 
                     best_mtrl.ENR = next_temp.ENR
                     best_mtrl.BENAMNING = next_temp.BENAMNING 
                     best_mtrl.ENHET = next_temp.ENHET   
                     best_mtrl.PRIS = next_temp.PRIS
                     best_mtrl.LEVKOD = next_temp.LEVKOD   
                     best_mtrl.PRIS = next_temp.PRIS
                     best_mtrl.OPRIS = next_temp.PRIS
                     best_mtrl.DATUM = datvar                         
                     best_mtrl.ANTAL = next_temp.ANTAL - prev_temp.ANTAL.                     
                  END.  
                  ELSE IF next_temp.ANTAL < prev_temp.ANTAL THEN DO:
                     CREATE best_mtrl.
                     ASSIGN                                 
                     best_mtrl.ENR = next_temp.ENR
                     best_mtrl.BENAMNING = next_temp.BENAMNING 
                     best_mtrl.ENHET = next_temp.ENHET   
                     best_mtrl.PRIS = next_temp.PRIS
                     best_mtrl.LEVKOD = next_temp.LEVKOD   
                     best_mtrl.PRIS = next_temp.PRIS 
                     best_mtrl.OPRIS = next_temp.PRIS
                     best_mtrl.DATUM = datvar 
                     best_mtrl.DBEST = "RETUR"                        
                     best_mtrl.ANTAL = prev_temp.ANTAL - next_temp.ANTAL.                     
                  END.
                  ELSE DO: 
                     musz = musz.                 
                  END.                 
                  DELETE prev_temp.       
               END.         
               ELSE DO:  
                  CREATE best_mtrl.
                  ASSIGN                                 
                  best_mtrl.ENR = next_temp.ENR
                  best_mtrl.BENAMNING = next_temp.BENAMNING 
                  best_mtrl.ENHET = next_temp.ENHET   
                  best_mtrl.PRIS = next_temp.PRIS
                  best_mtrl.LEVKOD = next_temp.LEVKOD   
                  best_mtrl.PRIS = next_temp.PRIS  
                  best_mtrl.OPRIS = next_temp.PRIS
                  best_mtrl.DATUM = datvar                         
                  best_mtrl.ANTAL = next_temp.ANTAL.                  
               END.
            END.
            FOR EACH prev_temp: 
               CREATE best_mtrl.
               ASSIGN                                 
               best_mtrl.ENR = prev_temp.ENR
               best_mtrl.BENAMNING = prev_temp.BENAMNING 
               best_mtrl.ENHET = prev_temp.ENHET   
               best_mtrl.PRIS = prev_temp.PRIS
               best_mtrl.LEVKOD = prev_temp.LEVKOD   
               best_mtrl.PRIS = prev_temp.PRIS 
               best_mtrl.OPRIS = prev_temp.PRIS
               best_mtrl.DATUM = datvar 
               best_mtrl.DBEST = "RETUR"                        
               best_mtrl.ANTAL = prev_temp.ANTAL.               
            END.     
         END.   
      END.
   END.*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE gammal_UI WINDOW-1 
PROCEDURE gammal_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/  
   EMPTY TEMP-TABLE mtrl_temp NO-ERROR.    
   ASSIGN   
   gammal = TRUE.   
   IF Guru.Konstanter:appcon THEN DO:                           
      RUN GAMINKOP.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT valaonr, INPUT valomrade, OUTPUT TABLE best_mtrl).
   END.
   ELSE DO:
      RUN GAMINKOP.P
      (INPUT valaonr, INPUT valomrade, OUTPUT TABLE best_mtrl).
   END.                     
   IF delbest = FALSE THEN DO:   
      RUN gammal2_UI.  
   END.
   RUN dat_UI.     
   FOR EACH best_mtrl WHERE best_mtrl.LEVNAMN = "":
      RUN levnamn_UI.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE konval_UI WINDOW-1 
PROCEDURE konval_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/ 
   CREATE kon_val. 
   ASSIGN
   kon_val.BERAONR = ekon_val.BERAONR
   kon_val.OMRADE = ekon_val.OMRADE
   kon_val.F1 = ekon_val.F1
   kon_val.F2 = ekon_val.F2
   kon_val.ID2 = ekon_val.ID2
   kon_val.EXTRA = ekon_val.EXTRA
   kon_val.NUM = ekon_val.NUM
   kon_val.GRUPP = ekon_val.GRUPP
   /*kon_val.ORD = ordning
   SUBSTRING(kon_val.ANMARK,1,15) = STRING(TODAY)
   SUBSTRING(kon_val.ANMARK,20,15) = STRING(klockansek)*/
   kon_val.UPPLAG = ekon_val.UPPLAG.
   /*spec_rowid = ROWID(kon_val).*/
   ASSIGN
   kon_val.EXTRA1 = SUBSTRING(kon_val.EXTRA,3)
   kon_val.EXTRA2 = SUBSTRING(kon_val.EXTRA,1,1).
   /*RUN laddaberid_UI IN valkonstapph (INPUT ekon_val.BERAONR,INPUT ekon_val.OMRADE,
                                      OUTPUT TABLE beridtemp).

   FIND FIRST beridtemp WHERE beridtemp.AONR = kon_val2.BERAONR AND 
   beridtemp.OMRADE = kon_val2.OMRADE AND beridtemp.NUM = kon_val2.NUM NO-LOCK NO-ERROR.
   IF AVAILABLE beridtemp THEN DO:
      IF beridtemp.XKORD NE ? THEN DO:
         ASSIGN
         kombnr = kombnr + 1
         kon_val.SKAPNUM = kombnr.
         OPEN QUERY idq FOR EACH idbuff WHERE idbuff.AONR = kon_val2.BERAONR AND 
         idbuff.OMRADE = kon_val2.OMRADE AND idbuff.XKORD = beridtemp.XKORD NO-LOCK.
         GET FIRST idq NO-LOCK.
         DO WHILE AVAILABLE(idbuff):
            IF idbuff.NUM = beridtemp.NUM THEN musz = musz.
            ELSE DO:
               RUN laddaberval_UI IN valkonstapph (INPUT kon_val2.BERAONR,INPUT kon_val2.OMRADE,
                                                   INPUT idbuff.NUM,OUTPUT ktypkodvar,OUTPUT bernumvar).
               ordning = ordning + 1.
               CREATE kon_val. 
               ASSIGN
               kon_val.BERAONR = kon_val2.BERAONR
               kon_val.OMRADE = kon_val2.OMRADE
               kon_val.F1 = ktypkodvar                        
               kon_val.NUM = bernumvar
               kon_val.ORD = ordning
               kon_val.SKAPNUM = kombnr
               kon_val.UPPLAG = kon_val2.UPPLAG.
               IF idbuff.FRI2 = ? THEN kon_val.ID2 = idbuff.NATNR.
               ELSE kon_val.ID2 = STRING(idbuff.FRI2).
               kon_val.EXTRA = idbuff.FRI3.
               ASSIGN
               kon_val.EXTRA1 = SUBSTRING(kon_val.EXTRA,3)
               kon_val.EXTRA2 = SUBSTRING(kon_val.EXTRA,1,1)
               SUBSTRING(kon_val.ANMARK,1,15) = STRING(TODAY)
               SUBSTRING(kon_val.ANMARK,20,15) = STRING(klockansek).               
            END.
            GET NEXT idq NO-LOCK.
         END.
         CLOSE QUERY idq.
      END.
      ELSE DO:
         kon_val.SKAPNUM = ?.
      END.
   END.*/
      

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE levbyt_UI WINDOW-1 
PROCEDURE levbyt_UI :
/* -----------------------------------------------------------
  Purpose:    
  Parameters: 
  Notes:       
-------------------------------------------------------------*/    
   
   ASSIGN
   felmedd = ""
   val = 0.   
   RUN btnok_UI IN levbytapph (INPUT CMB_LEVTILL,OUTPUT felmedd).
   IF felmedd NE "" THEN DO:
      MESSAGE felmedd VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
   ELSE DO:      
      FIND FIRST levtemp WHERE levtemp.LEVNAMN = CMB_LEVTILL
      USE-INDEX LEV NO-LOCK NO-ERROR.
      IF Guru.Konstanter:globforetag = "CELPA" {GLOBVES.I} THEN DO:
         IF levtemp.LEVKOD = "17" THEN DO:
            skipkontroll = TRUE.
         END.
         IF levtemp.LEVKOD = "11" THEN DO:
            skipkontroll = TRUE.
         END.
      END.
      ELSE DO:
         skipkontroll = FALSE.
      END.
      FOR EACH ebest_mtrl2:
         IF ebest_mtrl2.LEVKOD = levtemp.LEVKOD THEN DO:
             musz = musz.
         END.
         ELSE DO:            
            IF skipkontroll = TRUE THEN DO:
               ASSIGN
               mtrl_rowid = ROWID(ebest_mtrl2)
               felmedd = "skipkontroll"
               ebest_mtrl2.LEVKOD = levtemp.LEVKOD.
               RUN bytt_UI.
            END.
            ELSE DO:            
               EMPTY TEMP-TABLE ebest_mtrl3 NO-ERROR.
               mtrl_rowid = ROWID(ebest_mtrl2).
               CREATE ebest_mtrl3.
               BUFFER-COPY ebest_mtrl2 TO ebest_mtrl3.
               RUN kontroll_UI IN levbytapph (INPUT levtemp.LEVKOD,INPUT CMB_LEVTILL,OUTPUT felmedd,OUTPUT val,
                                        INPUT-OUTPUT TABLE ebest_mtrl3).            
               FIND FIRST ebest_mtrl3 NO-LOCK NO-ERROR.            
               BUFFER-COPY ebest_mtrl3 TO ebest_mtrl2.
               RUN bytt_UI.           
            END.
         END.
      END.      
   END.      
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE levnamn_UI WINDOW-1 
PROCEDURE levnamn_UI :
/* -----------------------------------------------------------
  Purpose:    
  Parameters: 
  Notes:       
-------------------------------------------------------------*/   
   FIND FIRST levtemp WHERE levtemp.LEVKOD =  best_mtrl.LEVKOD NO-LOCK NO-ERROR.
   IF AVAILABLE levtemp THEN  best_mtrl.LEVNAMN = levtemp.LEVNAMN.
   IF best_mtrl.LEVKOD = "0" THEN best_mtrl.LEVNAMN = "Depå".
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE linor_UI WINDOW-1 
PROCEDURE linor_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/  
   CREATE lin_temp.
   ASSIGN
   lin_temp.ENR = berlinkabtemp.ENR
   lin_temp.BENAMNING = berlinkabtemp.BENAMNING
   lin_temp.ENHET = berlinkabtemp.ENHET         
   lin_temp.PRIS = berlinkabtemp.PRIS
   lin_temp.METER = berlinkabtemp.METER  
   lin_temp.LEDARE = berlinkabtemp.LEDARE
   lin_temp.TOTMETER = berlinkabtemp.TOTMETER     
   lin_temp.LEVKOD = berlinkabtemp.LEVKOD
   lin_temp.UPPLAG = berlinkabtemp.UPPLAG
   lin_temp.DATUM = berlinkabtemp.DATUM. 
   IF Guru.Konstanter:globforetag = "CELPA" {GLOBVES.I} THEN DO:
      IF berlinkabtemp.LEVKOD = "12" OR berlinkabtemp.LEVKOD = "13" THEN lin_temp.LEVKOD = "16".
   END.
   ELSE IF Guru.Konstanter:globforetag = "UMEA" THEN DO:
      lin_temp.LEVKOD = "1".
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE mtrl_UI WINDOW-1 
PROCEDURE mtrl_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/  
   CREATE mtrl_temp.
   ASSIGN
   mtrl_temp.ENR = bermtrltemp.ENR
   mtrl_temp.BENAMNING = bermtrltemp.BENAMNING
   mtrl_temp.ENHET = bermtrltemp.ENHET         
   mtrl_temp.PRIS = bermtrltemp.PRIS
   mtrl_temp.ANTAL = bermtrltemp.ANTAL     
   mtrl_temp.LEVKOD = bermtrltemp.LEVKOD
   mtrl_temp.DATUM = bermtrltemp.DATUM.
   IF Guru.Konstanter:globforetag = "CELPA" {GLOBVES.I} THEN DO:
      IF bermtrltemp.LEVKOD = "12" OR bermtrltemp.LEVKOD = "13"  THEN mtrl_temp.LEVKOD = "16".
   END.
   ELSE IF Guru.Konstanter:globforetag = "UMEA" THEN DO:
      mtrl_temp.LEVKOD = "1".
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE nyinkop_UI WINDOW-1 
PROCEDURE nyinkop_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/    
   EMPTY TEMP-TABLE mtrl_temp NO-ERROR.    
   IF Guru.Konstanter:appcon THEN DO:                           
      RUN APPINKOP.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT valaonr, INPUT valomrade, INPUT Guru.Konstanter:globforetag, OUTPUT TABLE best_mtrl).
   END.
   ELSE DO:
      RUN APPINKOP.P
      (INPUT valaonr, INPUT valomrade, INPUT Guru.Konstanter:globforetag, OUTPUT TABLE best_mtrl).
   END.      
   RUN dat_UI. 
   FOR EACH best_mtrl WHERE best_mtrl.LEVNAMN = "":
      RUN levnamn_UI.
   END.
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
   /*ny*/
   MESSAGE "Vill Ni göra delbeställning?" SKIP
           "(I delbeställning beställs hela konstruktioner eller helt upplag.)"   VIEW-AS ALERT-BOX 
   QUESTION BUTTONS YES-NO TITLE "Delbeställning" UPDATE svar.
   IF svar THEN DO:
      delbest = TRUE.
      RUN VALKONSTU2.W.
      IF musz = TRUE THEN DO:
         
         RETURN.
      END.
      IF Guru.Konstanter:appcon THEN DO:
         RUN DELINKOP.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
         (INPUT valaonr,INPUT valomrade,INPUT Guru.Konstanter:globforetag,INPUT TABLE kon_val, OUTPUT TABLE mtrl_temp).      
      END.
      ELSE DO:
         RUN DELINKOP.P
         (INPUT valaonr,INPUT valomrade,INPUT Guru.Konstanter:globforetag,INPUT TABLE kon_val, OUTPUT TABLE mtrl_temp).      
      END. 
      FIND FIRST mtrl_temp NO-LOCK NO-ERROR.
      IF AVAILABLE mtrl_temp THEN DO:            
         FOR EACH mtrl_temp BREAK BY mtrl_temp.LEVKOD BY mtrl_temp.ENR:                
            ACCUMULATE mtrl_temp.ANTAL (TOTAL BY mtrl_temp.LEVKOD BY mtrl_temp.ENR).       
            IF LAST-OF(mtrl_temp.ENR) THEN DO:
               CREATE best_mtrl.
               ASSIGN 
               best_mtrl.ENR = mtrl_temp.ENR
               best_mtrl.BENAMNING = mtrl_temp.BENAMNING 
               best_mtrl.ENHET = mtrl_temp.ENHET 
               best_mtrl.LEVKOD = mtrl_temp.LEVKOD
               best_mtrl.PRIS = mtrl_temp.PRIS
               best_mtrl.OPRIS = mtrl_temp.PRIS
               best_mtrl.DATUM = mtrl_temp.DATUM
               best_mtrl.ANTAL = (ACCUM TOTAL BY mtrl_temp.ENR mtrl_temp.ANTAL). 
               RUN levnamn_UI.
            END.     
         END.
         RUN dat_UI.
      END.   
      ELSE DO:
         RUN openbdynspec_UI IN brwproc[2].
         MESSAGE "Inget materiel att beställa!"
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
         musz = TRUE.
         RETURN.
      END.            
   END.
   ELSE DO:
      delbest = FALSE.
      RUN nyinkop_UI.
      RUN dummykontroll_UI.
   END.
         
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE repo_UI WINDOW-1 
PROCEDURE repo_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/      
   DEFINE INPUT PARAMETER brwvar AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER browrec AS ROWID NO-UNDO.
   IF brwvar = 1 THEN DO:
      &Scoped-define BROWSE-NAME BRW_DAT
      {&BROWSE-NAME}:SET-REPOSITIONED-ROW(35,"ALWAYS") IN FRAME {&FRAME-NAME}.
      REPOSITION {&BROWSE-NAME} TO ROWID browrec NO-ERROR.  
   END.
   IF brwvar = 2 THEN DO:
      &Scoped-define BROWSE-NAME BRW_MTRL
      {&BROWSE-NAME}:SET-REPOSITIONED-ROW(35,"ALWAYS") IN FRAME {&FRAME-NAME}.
      REPOSITION {&BROWSE-NAME} TO ROWID browrec NO-ERROR.  
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
   IF brwh:NAME = "BRW_MTRL" THEN RUN rowdispmtrl_UI (INPUT 5).    
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
   IF vad = 5 THEN DO:  
      IF AVAILABLE best_mtrl THEN DO:         
         IF varforetypval[29] = 1  THEN DO:                 
            /*nettopris beredning inköp*/         
            FIND FIRST ikmtrltemp WHERE ikmtrltemp.LEVKOD =  best_mtrl.LEVKOD AND ikmtrltemp.ENR =  best_mtrl.ENR AND ikmtrltemp.KALKNR = 0
            AND ikmtrltemp.KUND = TRUE  NO-LOCK NO-ERROR.
            IF AVAILABLE ikmtrltemp THEN DO:
               /*STOLPAR turkosa fortum  gula övriga*/
               best_mtrl.ENR:BGCOLOR IN BROWSE BRW_MTRL = varforetypval[28].               
            END.
            ELSE DO:
               FIND FIRST ikmtrltemp WHERE ikmtrltemp.LEVKOD =  best_mtrl.LEVKOD AND ikmtrltemp.ENR =  best_mtrl.ENR AND ikmtrltemp.KALKNR = 0
               AND ikmtrltemp.KUND = ?  NO-LOCK NO-ERROR.
               IF AVAILABLE ikmtrltemp THEN DO:
                  /*STOLPAR turkosa fortum  gula övriga*/
                  best_mtrl.ENR:BGCOLOR IN BROWSE BRW_MTRL = varforetypval[38].               
               END.
            END.
            
         END.
      END.
      
   END.   

   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE skapa_UI WINDOW-1 
PROCEDURE skapa_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/  
/*    FOR EACH best_mtrl WHERE best_mtrl.KLAR = TRUE:                                           */
/*       DELETE best_mtrl.                                                                      */
/*    END.                                                                                      */
/*    FOR EACH best_mtrl:                                                                       */
/*       DO TRANSACTION:                                                                        */
/*          CREATE BERMTRL.                                                                     */
/*          ASSIGN                                                                              */
/*          BERMTRL.AONR = valaonr                                                              */
/*          BERMTRL.OMRADE = valomrade                                                          */
/*          BERMTRL.ENR = best_mtrl.ENR                                                         */
/*          BERMTRL.BENAMNING = best_mtrl.BENAMNING                                             */
/*          BERMTRL.ENHET = best_mtrl.ENHET                                                     */
/*          BERMTRL.PRIS = best_mtrl.PRIS                                                       */
/*          BERMTRL.OPRIS = best_mtrl.OPRIS                                                     */
/*          BERMTRL.ANTAL = best_mtrl.ANTAL                                                     */
/*          BERMTRL.LEVKOD = best_mtrl.LEVKOD                                                   */
/*          BERMTRL.BERLEV = best_mtrl.BERLEV                                                   */
/*          BERMTRL.DBEST = best_mtrl.DBEST                                                     */
/*          BERMTRL.DATUM = best_mtrl.DATUM                                                     */
/*          BERMTRL.INKOP = TRUE.                                                               */
/*          IF bestvar = TRUE THEN BERMTRL.KLAR = TRUE.                                         */
/*       END.                                                                                   */
/*    END.                                                                                      */
/*    IF Guru.Konstanter:globforetag = "ELPA" OR Guru.Konstanter:globforetag = "SOLE" THEN DO:                                  */
/*       dep = FALSE.                                                                           */
/*       IF bestvar = TRUE THEN DO:                                                             */
/*          FIND FIRST best_mtrl WHERE best_mtrl.LEVKOD = "0" NO-LOCK NO-ERROR.                 */
/*          IF AVAILABLE best_mtrl THEN DO:                                                     */
/*             dep = TRUE.                                                                      */
/*          END.                                                                                */
/*       END.                                                                                   */
/*       IF dep = TRUE THEN DO:                                                                 */
/*          depant = 0.                                                                         */
/*          FOR EACH DEPA NO-LOCK:                                                              */
/*             depant = depant + 1.                                                             */
/*          END.                                                                                */
/*          IF depant = 1 THEN DO:                                                              */
/*             FIND FIRST DEPA NO-LOCK NO-ERROR.                                                */
/*             vald_depa = DEPA.DEP-NR.                                                         */
/*          END.                                                                                */
/*          ELSE DO:                                                                            */
/*             {muswait.i}                                                                      */
/*             {AVBGOM.I}                                                                       */
/*             RUN DEPVAL.W.                                                                    */
/*             {AVBFRAM.I}                                                                      */
/*             {musarrow.i}                                                                     */
/*          END.                                                                                */
/*          FIND LAST BERBEST WHERE BERBEST.DEPNR = vald_depa USE-INDEX BEST NO-LOCK NO-ERROR.  */
/*          IF NOT AVAILABLE BERBEST THEN DO:                                                   */
/*             ASSIGN                                                                           */
/*             nytt_bestnr2 = 1.                                                                */
/*          END.                                                                                */
/*          ELSE DO:                                                                            */
/*             ASSIGN                                                                           */
/*             nytt_bestnr2 = BERBEST.BESTNR + 1.                                               */
/*          END.                                                                                */
/*          ASSIGN                                                                              */
/*          best = FALSE                                                                        */
/*          leve = FALSE.                                                                       */
/*          FOR EACH best_mtrl:                                                                 */
/*             IF best_mtrl.LEVKOD = "0" THEN DO:                                               */
/*                CREATE BERBEST.                                                               */
/*                ASSIGN                                                                        */
/*                BERBEST.BESTNR = nytt_bestnr2                                                 */
/*                BERBEST.ENR = best_mtrl.ENR                                                   */
/*                BERBEST.BENAMNING = best_mtrl.BENAMNING                                       */
/*                BERBEST.PRIS = best_mtrl.PRIS                                                 */
/*                BERBEST.ENHET = best_mtrl.ENHET                                               */
/*                BERBEST.ANTAL = best_mtrl.ANTAL                                               */
/*                BERBEST.LEVKOD = best_mtrl.LEVKOD                                             */
/*                BERBEST.AONR = valaonr                                                        */
/*                BERBEST.DELNR = valdelnr                                                      */
/*                BERBEST.DEPNR = vald_depa                                                     */
/*                BERBEST.BESTDATUM = best_mtrl.DATUM                                           */
/*                BERBEST.BESTALLARE = Guru.Konstanter:globanv                                                  */
/*                BERBEST.UTTAG = TRUE                                                          */
/*                best = TRUE.                                                                  */                
/*                BERBEST.PRIS = best_mtrl.OPRIS.                                               */
/*             END.                                                                             */
/*             ELSE DO:                                                                         */
/*                musz = musz.                                                                  */
/*             END.                                                                             */
/*          END.                                                                                */
/*          MESSAGE "VIll du skicka meddelande till depåansvarig?"                              */
/*          VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Meddelande"                        */
/*          UPDATE svar AS LOGICAL.                                                             */
/*          IF svar THEN DO TRANSACTION:                                                        */
/*             FIND FIRST DEPA WHERE DEPA.DEP-NR = vald_depa USE-INDEX DEP-NR NO-LOCK NO-ERROR. */
/*             CREATE MEDDELANDE.                                                               */
/*             ASSIGN                                                                           */
/*             MEDDELANDE.SDATUM = TODAY                                                        */
/*             MEDDELANDE.EMOTAGET = FALSE                                                      */
/*             MEDDELANDE.MOTTAGARE = DEPA.ANVANDARE                                            */
/*             MEDDELANDE.SANDARE = Guru.Konstanter:globanv.                                                    */
/*             MEDDELANDE.MEDD = "En 'beställning' med Nr:" + STRING(nytt_bestnr2) +            */
/*             " från Aonr:" + valaonr + " Delnr:" +                                            */
/*             STRING(valdelnr) + " har skapats mot depån.".                                    */
/*          END.                                                                                */
/*       END.                                                                                   */
/*    END.                                                                                      */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE skydd_UI WINDOW-1 
PROCEDURE skydd_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/  
   CREATE mtrl_temp.
   ASSIGN
   mtrl_temp.ENR = kskyddtemp.ENR
   mtrl_temp.BENAMNING = kskyddtemp.BENAMNING
   mtrl_temp.ENHET = kskyddtemp.ENHET         
   mtrl_temp.PRIS = kskyddtemp.PRIS
   mtrl_temp.ANTAL = kskyddtemp.ANTAL * kskyddtemp.METER    
   mtrl_temp.LEVKOD = kskyddtemp.LEVKOD
   mtrl_temp.DATUM = kskyddtemp.DATUM.
   IF Guru.Konstanter:globforetag = "CELPA" {GLOBVES.I} THEN DO:
      IF kskyddtemp.LEVKOD = "12" OR kskyddtemp.LEVKOD = "13"  THEN mtrl_temp.LEVKOD = "16".
   END.
   ELSE IF Guru.Konstanter:globforetag = "UMEA" THEN DO:
      mtrl_temp.LEVKOD = "1".
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE slutrens_UI WINDOW-1 
PROCEDURE slutrens_UI :
/* -----------------------------------------------------------
  Purpose:    
  Parameters: 
  Notes:       
-------------------------------------------------------------*/    
   {BORTBRWPROC.I}
   IF VALID-HANDLE(nyberapph) THEN DELETE PROCEDURE nyberapph NO-ERROR.
   IF VALID-HANDLE(levbytapph) THEN DELETE PROCEDURE levbytapph.
   IF VALID-HANDLE(berinkopapph) THEN DELETE PROCEDURE berinkopapph.
   IF VALID-HANDLE(nettoh) THEN DELETE PROCEDURE nettoh NO-ERROR.      
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sumlin2_UI WINDOW-1 
PROCEDURE sumlin2_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/  
   FOR EACH lin_temp WHERE lin_temp.TOTMETER > 0 AND
   lin_temp.UPPLAG = ?:
      DELETE lin_temp.
   END.
   FOR EACH lin_temp WHERE lin_temp.UPPLAG NE ? AND lin_temp.TOTMETER > 0:
      FOR EACH linbuff WHERE linbuff.ENR = lin_temp.ENR AND 
      linbuff.LEVKOD = lin_temp.LEVKOD AND linbuff.TOTMETER = 0:
         DELETE linbuff.
      END.
   END. 
   FOR EACH lin_temp WHERE lin_temp.UPPLAG = ?:
      lin_temp.TOTMETER = lin_temp.METER * lin_temp.LEDARE.
   END.              
   sumantal = 0.
   FOR EACH lin_temp BREAK BY lin_temp.LEVKOD BY lin_temp.ENR:      
   ACCUMULATE lin_temp.TOTMETER (TOTAL BY lin_temp.LEVKOD BY lin_temp.ENR).       
      IF LAST-OF(lin_temp.ENR) THEN DO:
         IF nextvar = TRUE THEN DO:
            CREATE next_temp.
            ASSIGN                                 
            next_temp.ENR = lin_temp.ENR
            next_temp.BENAMNING = lin_temp.BENAMNING 
            next_temp.ENHET = lin_temp.ENHET   
            next_temp.PRIS = lin_temp.PRIS
            next_temp.LEVKOD = lin_temp.LEVKOD   
            next_temp.PRIS = lin_temp.PRIS
            next_temp.DATUM = lin_temp.DATUM                         
            next_temp.ANTAL = (ACCUM TOTAL BY lin_temp.ENR lin_temp.TOTMETER).            
         END.
         ELSE DO:
            CREATE prev_temp.
            ASSIGN                                 
            prev_temp.ENR = lin_temp.ENR
            prev_temp.BENAMNING = lin_temp.BENAMNING 
            prev_temp.ENHET = lin_temp.ENHET   
            prev_temp.PRIS = lin_temp.PRIS
            prev_temp.LEVKOD = lin_temp.LEVKOD   
            prev_temp.PRIS = lin_temp.PRIS
            prev_temp.DATUM = lin_temp.DATUM                         
            prev_temp.ANTAL = (ACCUM TOTAL BY lin_temp.ENR lin_temp.TOTMETER).            
         END.   
      END.     
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE summa2_UI WINDOW-1 
PROCEDURE summa2_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/  
   sumantal = 0.
   FOR EACH mtrl_temp BREAK BY mtrl_temp.LEVKOD BY mtrl_temp.ENR:      
   ACCUMULATE mtrl_temp.ANTAL (TOTAL BY mtrl_temp.LEVKOD BY mtrl_temp.ENR).       
      IF LAST-OF(mtrl_temp.ENR) THEN DO: 
         IF nextvar = TRUE THEN DO:
            CREATE next_temp.
            ASSIGN                                 
            next_temp.ENR = mtrl_temp.ENR
            next_temp.BENAMNING = mtrl_temp.BENAMNING 
            next_temp.ENHET = mtrl_temp.ENHET   
            next_temp.PRIS = mtrl_temp.PRIS
            next_temp.LEVKOD = mtrl_temp.LEVKOD   
            next_temp.PRIS = mtrl_temp.PRIS
            next_temp.DATUM = mtrl_temp.DATUM                         
            next_temp.ANTAL = (ACCUM TOTAL BY mtrl_temp.ENR mtrl_temp.ANTAL).            
         END.
         ELSE DO: 
            CREATE prev_temp.
            ASSIGN                                 
            prev_temp.ENR = mtrl_temp.ENR
            prev_temp.BENAMNING = mtrl_temp.BENAMNING 
            prev_temp.ENHET = mtrl_temp.ENHET   
            prev_temp.PRIS = mtrl_temp.PRIS
            prev_temp.LEVKOD = mtrl_temp.LEVKOD   
            prev_temp.PRIS = mtrl_temp.PRIS
            prev_temp.DATUM = mtrl_temp.DATUM                         
            prev_temp.ANTAL = (ACCUM TOTAL BY mtrl_temp.ENR mtrl_temp.ANTAL).            
         END.      
      END.     
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION klockan60 WINDOW-1 
FUNCTION klockan60 RETURNS DECIMAL
   ( INPUT ber100 AS DECIMAL ):
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
 RETURN TRUNCATE(ber100,0) + ((ber100 - TRUNCATE(ber100,0)) / 100) * 60 . 

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

