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
DEFINE INPUT-OUTPUT PARAMETER ktypkodvar AS CHARACTER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER kodvar     AS CHARACTER NO-UNDO.
/* Local Variable Definitions ---                                       */
{ALLDEF.I}
&Scoped-define NEW  
{GLOBVAR2DEL1.I}

{DEFSOK.I}
{SOKDEF.I}
&Scoped-define SHARED SHARED
{BERSKAP.I}
{HUVLEVTEMP.I}
{LEVTEMP.I}
{MTRLTEMP.I}

DEFINE NEW SHARED VARIABLE skapben AS CHARACTER NO-UNDO.       
DEFINE NEW SHARED VARIABLE skapkod AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE skapmod AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE ny AS LOGICAL NO-UNDO.    
DEFINE SHARED VARIABLE typkod AS CHARACTER NO-UNDO. 
DEFINE SHARED VARIABLE skap_row AS ROWID NO-UNDO.  
DEFINE VARIABLE sparkod AS CHARACTER NO-UNDO. 
DEFINE VARIABLE spartyp AS CHARACTER NO-UNDO.  
DEFINE SHARED VARIABLE huvudlev AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE vald_kundlev AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE vald_lev AS CHARACTER NO-UNDO.  
DEFINE SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.  
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO. 
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE aosok AS CHARACTER FORMAT "X(40)" NO-UNDO.
DEFINE VARIABLE posok AS CHARACTER FORMAT "X(11)" NO-UNDO.
DEFINE VARIABLE antal_valda AS INTEGER NO-UNDO.
DEFINE VARIABLE antal_raknare AS INTEGER NO-UNDO. 
DEFINE VARIABLE mtrl_rowid AS ROWID NO-UNDO.
DEFINE VARIABLE lev AS CHARACTER NO-UNDO.  
DEFINE VARIABLE val AS LOGICAL NO-UNDO.
DEFINE VARIABLE svar AS LOGICAL NO-UNDO.
DEFINE VARIABLE skapandapph AS HANDLE NO-UNDO.
DEFINE VARIABLE laddaproch AS HANDLE NO-UNDO.
DEFINE VARIABLE felmedd AS CHARACTER NO-UNDO.
DEFINE VARIABLE mtrlhmtapph AS HANDLE NO-UNDO.
DEFINE VARIABLE satsvar AS LOGICAL NO-UNDO.
DEFINE VARIABLE nettoh AS HANDLE NO-UNDO.
DEFINE NEW SHARED TEMP-TABLE skap_mtrl
   FIELD KTYPKOD AS CHARACTER
   FIELD KOD AS CHARACTER  
   FIELD ENR AS CHARACTER 
   FIELD BENAMNING AS CHARACTER 
   FIELD ENHET AS CHARACTER 
   FIELD ANTAL AS INTEGER
   FIELD PRIS AS DECIMAL
   FIELD LEVKOD AS CHARACTER
   FIELD MODUL AS INTEGER.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A
&Scoped-define BROWSE-NAME BRW_HLEV

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES mtrltemp skap_mtrl

/* Definitions for BROWSE BRW_HLEV                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_HLEV mtrltemp.Enr mtrltemp.Benamning ~
mtrltemp.Enhet mtrltemp.NPRIS 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_HLEV 
&Scoped-define QUERY-STRING-BRW_HLEV FOR EACH mtrltemp NO-LOCK ~
    BY mtrltemp.Enr
&Scoped-define OPEN-QUERY-BRW_HLEV OPEN QUERY BRW_HLEV FOR EACH mtrltemp NO-LOCK ~
    BY mtrltemp.Enr.
&Scoped-define TABLES-IN-QUERY-BRW_HLEV mtrltemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_HLEV mtrltemp


/* Definitions for BROWSE BRW_MTRL                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_MTRL skap_mtrl.ENR skap_mtrl.BENAMNING ~
skap_mtrl.ENHET skap_mtrl.ANTAL skap_mtrl.PRIS skap_mtrl.LEVKOD 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_MTRL 
&Scoped-define QUERY-STRING-BRW_MTRL FOR EACH skap_mtrl ~
      WHERE skap_mtrl.KTYPKOD = CMB_TYP NO-LOCK ~
    BY skap_mtrl.ENR
&Scoped-define OPEN-QUERY-BRW_MTRL OPEN QUERY BRW_MTRL FOR EACH skap_mtrl ~
      WHERE skap_mtrl.KTYPKOD = CMB_TYP NO-LOCK ~
    BY skap_mtrl.ENR.
&Scoped-define TABLES-IN-QUERY-BRW_MTRL skap_mtrl
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_MTRL skap_mtrl


/* Definitions for FRAME FRAME-A                                        */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 IMAGE-8 FILL-IN-KOD FILL-IN-SKAPBEN ~
FILL-IN-MOD BRW_HLEV BRW_MTRL FBTN_VISA FBTN_SKRIV btn_over btn_back ~
CMB_LEV BTN_ANT FILL-IN-ENR FILL-IN-BEN RAD_SOK FBTN_OK BTN_AVB ~
FILL-IN-SOKALT 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-KOD FILL-IN-SKAPBEN FILL-IN-MOD ~
CMB_LEV FILL-IN-ENR FILL-IN-BEN RAD_SOK FILL-IN-SOKALT 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR WINDOW-1 AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_ANT 
     LABEL "Antal" 
     SIZE 12 BY 1
     FGCOLOR 1 .

DEFINE BUTTON BTN_AVB AUTO-END-KEY 
     LABEL "Avbryt":L 
     SIZE 14 BY 1.

DEFINE BUTTON btn_back 
     IMAGE-UP FILE "BILDER\prev-u":U
     LABEL "":L 
     SIZE 4 BY 1.21.

DEFINE BUTTON btn_over 
     IMAGE-UP FILE "BILDER\next-u":U
     LABEL "":L 
     SIZE 4 BY 1.21.

DEFINE BUTTON FBTN_OK 
     LABEL "Ok" 
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

DEFINE VARIABLE CMB_LEV AS CHARACTER FORMAT "X(15)":U 
     LABEL "Leverantörer" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "0" 
     DROP-DOWN-LIST
     SIZE 18.25 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-BEN AS CHARACTER FORMAT "X(40)":U 
     LABEL "Benämning" 
     VIEW-AS FILL-IN 
     SIZE 27 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-ENR AS CHARACTER FORMAT "X(11)":U 
     LABEL "Enr" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-KOD AS CHARACTER FORMAT "X(12)":U 
     LABEL "Kod" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-MOD AS INTEGER FORMAT ">>>9":U INITIAL 0 
     LABEL "Moduler" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-SKAPBEN AS CHARACTER FORMAT "X(40)":U 
     LABEL "Benämning" 
     VIEW-AS FILL-IN 
     SIZE 33.5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-SOKALT AS CHARACTER FORMAT "X(256)":U INITIAL "Sökalternativ:" 
      VIEW-AS TEXT 
     SIZE 12.63 BY .63 NO-UNDO.

DEFINE IMAGE IMAGE-8
     FILENAME "BILDER\sokpa.gif":U CONVERT-3D-COLORS
     SIZE 8 BY .83.

DEFINE VARIABLE RAD_SOK AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Början", 1,
"Någonstans", 2,
"Slutet", 3
     SIZE 29.5 BY .83 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 45 BY 4.17
     BGCOLOR 8 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_HLEV FOR 
      mtrltemp SCROLLING.

DEFINE QUERY BRW_MTRL FOR 
      skap_mtrl SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_HLEV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_HLEV WINDOW-1 _STRUCTURED
  QUERY BRW_HLEV NO-LOCK DISPLAY
      mtrltemp.Enr FORMAT "X(11)":U
      mtrltemp.Benamning FORMAT "x(40)":U WIDTH 20
      mtrltemp.Enhet FORMAT "x(5)":U
      mtrltemp.NPRIS FORMAT ">>>>99.99":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING MULTIPLE SIZE 45 BY 15
         TITLE "Materiellista för vald leverantör.".

DEFINE BROWSE BRW_MTRL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_MTRL WINDOW-1 _STRUCTURED
  QUERY BRW_MTRL NO-LOCK DISPLAY
      skap_mtrl.ENR COLUMN-LABEL "Enr" FORMAT "X(11)":U
      skap_mtrl.BENAMNING COLUMN-LABEL "Benämning" FORMAT "x(40)":U
            WIDTH 25
      skap_mtrl.ENHET COLUMN-LABEL "Enhet" FORMAT "x(5)":U
      skap_mtrl.ANTAL COLUMN-LABEL "Antal" FORMAT ">>>,>>9":U
      skap_mtrl.PRIS COLUMN-LABEL "Pris" FORMAT ">>>>99.99":U
      skap_mtrl.LEVKOD COLUMN-LABEL "Lev-id" FORMAT "X(8)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING MULTIPLE SIZE 66.5 BY 15
         TITLE "Upplagda materiel".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     FILL-IN-KOD AT ROW 1.5 COL 10.75 COLON-ALIGNED
     FILL-IN-SKAPBEN AT ROW 2.75 COL 10.75 COLON-ALIGNED
     FILL-IN-MOD AT ROW 4 COL 10.75 COLON-ALIGNED
     BRW_HLEV AT ROW 5.21 COL 1.5
     BRW_MTRL AT ROW 5.21 COL 51
     FBTN_VISA AT ROW 8 COL 118
     FBTN_SKRIV AT ROW 9.08 COL 118
     btn_over AT ROW 9.96 COL 46.75
     btn_back AT ROW 13.46 COL 46.75
     CMB_LEV AT ROW 20.46 COL 16.5 COLON-ALIGNED
     BTN_ANT AT ROW 20.46 COL 78
     FILL-IN-ENR AT ROW 22.04 COL 16.5 COLON-ALIGNED
     FILL-IN-BEN AT ROW 23.38 COL 16.5 COLON-ALIGNED
     RAD_SOK AT ROW 24.88 COL 16.25 NO-LABEL
     FBTN_OK AT ROW 24.88 COL 95.5
     BTN_AVB AT ROW 24.88 COL 110.5
     FILL-IN-SOKALT AT ROW 24.96 COL 2.38 NO-LABEL
     RECT-6 AT ROW 21.83 COL 1.5
     IMAGE-8 AT ROW 22 COL 2.25
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 136.25 BY 25.17.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: 
   Temp-Tables and Buffers:
      TABLE: mtrltemp T "?" NO-UNDO temp-db mtrltemp
      TABLE: ? T "?" NO-UNDO temp-db skap_mtrl
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW WINDOW-1 ASSIGN
         HIDDEN             = YES
         TITLE              = ""
         HEIGHT             = 25.17
         WIDTH              = 136.25
         MAX-HEIGHT         = 27.67
         MAX-WIDTH          = 136.25
         VIRTUAL-HEIGHT     = 27.67
         VIRTUAL-WIDTH      = 136.25
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
/* BROWSE-TAB BRW_HLEV FILL-IN-MOD FRAME-A */
/* BROWSE-TAB BRW_MTRL BRW_HLEV FRAME-A */
ASSIGN 
       BRW_HLEV:HIDDEN  IN FRAME FRAME-A                = TRUE
       BRW_HLEV:MAX-DATA-GUESS IN FRAME FRAME-A         = 10000
       BRW_HLEV:ALLOW-COLUMN-SEARCHING IN FRAME FRAME-A = TRUE
       BRW_HLEV:COLUMN-RESIZABLE IN FRAME FRAME-A       = TRUE.

ASSIGN 
       BRW_MTRL:MAX-DATA-GUESS IN FRAME FRAME-A         = 10000
       BRW_MTRL:ALLOW-COLUMN-SEARCHING IN FRAME FRAME-A = TRUE
       BRW_MTRL:COLUMN-RESIZABLE IN FRAME FRAME-A       = TRUE.

ASSIGN 
       CMB_LEV:HIDDEN IN FRAME FRAME-A           = TRUE.

ASSIGN 
       FILL-IN-BEN:HIDDEN IN FRAME FRAME-A           = TRUE.

ASSIGN 
       FILL-IN-ENR:HIDDEN IN FRAME FRAME-A           = TRUE.

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
     _FldNameList[1]   = Temp-Tables.mtrltemp.Enr
     _FldNameList[2]   > Temp-Tables.mtrltemp.Benamning
"mtrltemp.Benamning" ? ? "character" ? ? ? ? ? ? no ? no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   = Temp-Tables.mtrltemp.Enhet
     _FldNameList[4]   = Temp-Tables.mtrltemp.NPRIS
     _Query            is NOT OPENED
*/  /* BROWSE BRW_HLEV */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_MTRL
/* Query rebuild information for BROWSE BRW_MTRL
     _TblList          = "Temp-Tables.skap_mtrl"
     _Options          = "NO-LOCK"
     _OrdList          = "Temp-Tables.skap_mtrl.ENR|yes"
     _Where[1]         = "skap_mtrl.KTYPKOD = CMB_TYP"
     _FldNameList[1]   > Temp-Tables.skap_mtrl.ENR
"skap_mtrl.ENR" "Enr" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.skap_mtrl.BENAMNING
"skap_mtrl.BENAMNING" "Benämning" ? "character" ? ? ? ? ? ? no ? no no "25" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.skap_mtrl.ENHET
"skap_mtrl.ENHET" "Enhet" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.skap_mtrl.ANTAL
"skap_mtrl.ANTAL" "Antal" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.skap_mtrl.PRIS
"skap_mtrl.PRIS" "Pris" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > Temp-Tables.skap_mtrl.LEVKOD
"skap_mtrl.LEVKOD" "Lev-id" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_MTRL */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME BRW_HLEV
&Scoped-define SELF-NAME BRW_HLEV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_HLEV WINDOW-1
ON MOUSE-SELECT-DBLCLICK OF BRW_HLEV IN FRAME FRAME-A /* Materiellista för vald leverantör. */
DO:
    APPLY "CHOOSE" TO BTN_OVER.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_HLEV WINDOW-1
ON START-SEARCH OF BRW_HLEV IN FRAME FRAME-A /* Materiellista för vald leverantör. */
DO:
   APPLY "END-SEARCH" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_MTRL
&Scoped-define SELF-NAME BRW_MTRL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_MTRL WINDOW-1
ON MOUSE-SELECT-DBLCLICK OF BRW_MTRL IN FRAME FRAME-A /* Upplagda materiel */
DO:
   {muswait.i} 
   
   RUN SKAPANTU.W (INPUT ROWID(skap_mtrl)).    
   RUN setcolsortvar_UI IN brwproc[2] (INPUT ""). 
   RUN openbdynspec_UI IN brwproc[2].
   {musarrow.i}    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_ANT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_ANT WINDOW-1
ON CHOOSE OF BTN_ANT IN FRAME FRAME-A /* Antal */
DO:
   antal_valda = BRW_MTRL:NUM-SELECTED-ROWS.
   antal_raknare = 1.    
   DO WHILE antal_raknare LE antal_valda:   
      status-ok = BRW_MTRL:FETCH-SELECTED-ROW(antal_raknare).  
      IF AVAILABLE skap_mtrl THEN RUN SKAPANTU.W (INPUT ROWID(skap_mtrl)).

      antal_raknare = antal_raknare + 1.   
   END.   
   RUN setcolsortvar_UI IN brwproc[2] (INPUT ""). 
   RUN openbdynspec_UI IN brwproc[2].
   status-ok = BRW_MTRL:DESELECT-ROWS().
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB WINDOW-1
ON CHOOSE OF BTN_AVB IN FRAME FRAME-A /* Avbryt */
DO:     
   IF INPUT FILL-IN-KOD = "" THEN DO:  
      musz = TRUE.
      APPLY "CLOSE":U TO THIS-PROCEDURE.   
   END.
   ELSE DO:   
      MESSAGE "OBS! Vill du spara dina ändringar?"
      VIEW-AS ALERT-BOX
      QUESTION BUTTONS YES-NO TITLE "Spara ändringar?" UPDATE svar AS LOGICAL.         
      IF svar THEN DO:
         APPLY "CHOOSE" TO FBTN_OK IN FRAME {&FRAME-NAME}.
      END.
      ELSE DO:
         musz = TRUE.
         APPLY "CLOSE":U TO THIS-PROCEDURE.   
      END.                       
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_back
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_back WINDOW-1
ON CHOOSE OF btn_back IN FRAME FRAME-A
DO:   
   antal_valda = BRW_MTRL:NUM-SELECTED-ROWS.
   antal_raknare = 1.
   DO WHILE antal_raknare LE antal_valda: 
      ASSIGN                                  
      status-ok = BRW_MTRL:FETCH-SELECTED-ROW(antal_raknare).
      IF AVAILABLE skap_mtrl THEN DELETE skap_mtrl.
      IF antal_raknare = antal_valda THEN RUN selnextprevrow_UI IN brwproc[2].
      antal_raknare = antal_raknare + 1.   
   END.     
   RUN setcolsortvar_UI IN brwproc[2] (INPUT ""). 
   RUN openbdynspec_UI IN brwproc[2].
    RUN lastselectdyn_UI IN brwproc[2].
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_over
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_over WINDOW-1
ON CHOOSE OF btn_over IN FRAME FRAME-A
DO:                   
   antal_valda = BRW_HLEV:NUM-SELECTED-ROWS.         
   antal_raknare = 1.
   DO WHILE antal_raknare LE antal_valda:                                   
      status-ok = BRW_HLEV:FETCH-SELECTED-ROW(antal_raknare).
      IF AVAILABLE mtrltemp THEN DO:
         RUN over_UI.
         RUN setlastrowid_UI IN brwproc[2] (INPUT ROWID(skap_mtrl)).
      END.
      antal_raknare = antal_raknare + 1. 
   END.          
   RUN setcolsortvar_UI IN brwproc[2] (INPUT ""). 
   RUN openbdynspec_UI IN brwproc[2].
   RUN lastselectdyn_UI IN brwproc[2].
   status-ok = BRW_HLEV:DESELECT-ROWS().             
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


&Scoped-define SELF-NAME CMB_LEV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_LEV WINDOW-1
ON VALUE-CHANGED OF CMB_LEV IN FRAME FRAME-A /* Leverantörer */
DO:                                 
   lev = INPUT CMB_LEV.
   IF lev NE huvudlev THEN DO:
      val = TRUE.
      RUN setorgtitle_UI IN brwproc[1] (INPUT "Materiellista " + lev).
      FIND FIRST levtemp WHERE levtemp.LEVNAMN = lev 
      USE-INDEX LEV NO-LOCK NO-ERROR.
      vald_lev = levtemp.LEVKOD.  
   END.
   ELSE DO:
      RUN setorgtitle_UI IN brwproc[1] (INPUT "Materiellista huvudleverantör " + lev).         
      val = FALSE.
      vald_lev = vald_kundlev.     
   END.      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_OK WINDOW-1
ON CHOOSE OF FBTN_OK IN FRAME FRAME-A /* Ok */
DO:  
   {muswait.i} 
   ASSIGN
   FILL-IN-SKAPBEN = INPUT FILL-IN-SKAPBEN  
   FILL-IN-KOD     = INPUT FILL-IN-KOD      
   FILL-IN-MOD     = INPUT FILL-IN-MOD.     
   
   ASSIGN 
   skapben = FILL-IN-SKAPBEN 
   skapkod = FILL-IN-KOD
   skapmod = FILL-IN-MOD
   kodvar = FILL-IN-KOD.   
   RUN btnok_UI IN skapandapph (INPUT ny,INPUT typkod,INPUT spartyp,
                                INPUT sparkod,INPUT skap_row,INPUT skapkod,
                                INPUT skapben,INPUT skapmod,INPUT TABLE skap_mtrl).   
   APPLY "CLOSE":U TO THIS-PROCEDURE.                  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_OK WINDOW-1
ON MOUSE-MENU-CLICK OF FBTN_OK IN FRAME FRAME-A /* Ok */
DO:
   RUN SIDLANGD.W.
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
      ASSIGN
      skapben = INPUT FILL-IN-SKAPBEN
      skapkod = INPUT FILL-IN-KOD
      skapmod = INPUT FILL-IN-MOD
      skrivut = TRUE.
      {AVBGOM.I}
      RUN VISSKAPU.W.
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
   ASSIGN
   skapben = INPUT FILL-IN-SKAPBEN
   skapkod = INPUT FILL-IN-KOD
   skapmod = INPUT FILL-IN-MOD.
   IF musz = TRUE THEN musz = FALSE.   
   ASSIGN    
   skrivut = FALSE.   
   {AVBGOM.I}
   RUN VISSKAPU.W.
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


&Scoped-define SELF-NAME FILL-IN-KOD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-KOD WINDOW-1
ON LEAVE OF FILL-IN-KOD IN FRAME FRAME-A /* Kod */
DO:  
   IF INPUT FILL-IN-KOD = "" THEN DO: 
      MESSAGE "Kod kan inte vara blank!" 
      VIEW-AS ALERT-BOX TITLE "Meddelande".
      RETURN NO-APPLY.  
   END.
   ELSE DO:
      ASSIGN
      skapkod = INPUT FILL-IN-KOD.
      felmedd = "".
      RUN nykod_UI IN skapandapph (INPUT ny,INPUT skapkod,
                                   INPUT sparkod,OUTPUT felmedd).
      IF felmedd NE "" THEN DO:
         MESSAGE felmedd VIEW-AS ALERT-BOX TITLE "Meddelande".
         felmedd = "".
         RETURN NO-APPLY.
      END.
   END.   
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
ON CLOSE OF THIS-PROCEDURE 
DO:
   {BORTBRWPROC.I}
   IF VALID-HANDLE(mtrlhmtapph) THEN DELETE PROCEDURE mtrlhmtapph.
   IF VALID-HANDLE(laddaproch) THEN DELETE PROCEDURE laddaproch.
   IF VALID-HANDLE(skapandapph) THEN DELETE PROCEDURE skapandapph.
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
       
   IF ny = TRUE THEN DO:
      ASSIGN WINDOW-1:TITLE = "Ny uppläggning av kabelskåp: " + typkod.    
   END.
   ELSE DO: 
      
      FIND FIRST berskaptemp WHERE berskaptemp.BERSKAPROW = skap_row NO-LOCK.      
      ASSIGN WINDOW-1:TITLE = "Ändring av - " + berskaptemp.BENAMNING.    
      ASSIGN
      FILL-IN-KOD = berskaptemp.KOD
      FILL-IN-SKAPBEN = berskaptemp.BENAMNING
      FILL-IN-MOD = berskaptemp.MODUL
      sparkod = berskaptemp.KOD
      spartyp = berskaptemp.KTYPKOD. 
      OPEN QUERY skapq FOR EACH berskaptemp WHERE berskaptemp.KTYPKOD = spartyp AND
      berskaptemp.KOD = sparkod AND berskaptemp.ENR NE "" USE-INDEX KOD NO-LOCK.
      GET FIRST skapq NO-LOCK.
      DO WHILE AVAILABLE(berskaptemp):
         CREATE skap_mtrl.
         ASSIGN
         skap_mtrl.KTYPKOD = berskaptemp.KTYPKOD 
         skap_mtrl.KOD = berskaptemp.KOD
         skap_mtrl.ENR = berskaptemp.ENR 
         skap_mtrl.BENAMNING = berskaptemp.BENAMNING 
         skap_mtrl.ENHET = berskaptemp.ENHET 
         skap_mtrl.ANTAL = berskaptemp.ANTAL 
         skap_mtrl.PRIS = berskaptemp.PRIS 
         skap_mtrl.LEVKOD = berskaptemp.LEVKOD.
         GET NEXT skapq NO-LOCK.
      END.            
   END.           
   IF Guru.Konstanter:varforetypval[29] = 1  THEN DO:        
      /*nettopris beredning adm*/                
      EMPTY TEMP-TABLE ikmtrltemp NO-ERROR. 
      RUN ksknettomark_UI IN nettoh (INPUT TABLE skap_mtrl, OUTPUT TABLE ikmtrltemp APPEND).                  
   END.
   FIND FIRST huvlevtemp WHERE huvlevtemp.DEP-NR = 999 NO-LOCK NO-ERROR.
   IF AVAILABLE huvlevtemp THEN DO:  
      vald_kundlev = huvlevtemp.LEVKOD.
   END.                              
   ELSE DO:
      FIND FIRST levtemp WHERE levtemp.LEVKOD NE "0" 
      AND levtemp.BORTTAG = FALSE NO-LOCK NO-ERROR.
      vald_kundlev = levtemp.LEVKOD.
   END.   
   status-ok = CMB_LEV:DELETE("0"). 
   FIND FIRST levtemp WHERE levtemp.LEVKOD = vald_kundlev
   USE-INDEX LEV NO-LOCK NO-ERROR. 
   ASSIGN
   status-ok = CMB_LEV:ADD-LAST(levtemp.LEVNAMN)IN FRAME {&FRAME-NAME}
   CMB_LEV:SCREEN-VALUE = levtemp.LEVNAMN.
   ASSIGN
   Guru.SharedVariable:ValdmtrlLeverantor = levtemp.LEVKOD
   Guru.SharedVariable:ValdmtrlLeverantorName = levtemp.LEVNAMN.
   FOR EACH levtemp WHERE levtemp.LEVKOD NE vald_kundlev AND 
   levtemp.LEVKOD NE "0" AND levtemp.BORTTAG = FALSE USE-INDEX LEV NO-LOCK:      
      status-ok = CMB_LEV:ADD-LAST(levtemp.LEVNAMN)IN FRAME {&FRAME-NAME}.              
   END.
   
   FILL-IN-ENR:LABEL = Guru.Konstanter:genk.
   mtrltemp.ENR:LABEL IN BROWSE BRW_HLEV = Guru.Konstanter:genk. 
   skap_mtrl.ENR:LABEL IN BROWSE BRW_MTRL = Guru.Konstanter:genk.
   
   ASSIGN            
   vald_lev = vald_kundlev   
   val = FALSE
   satsvar = FALSE.
   RUN setorgtitle_UI IN brwproc[1] (INPUT "Materiellista huvudleverantör " + huvudlev).
   RUN enable_UI.   
   {FRMSIZE.I} 
   RUN setcolsortvar_UI IN brwproc[2] (INPUT ""). 
   RUN openbdynspec_UI IN brwproc[2].
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
  Purpose:      
  Parameters:  
  Notes:       
-------------------------------------------------------------*/ 
   RUN DYNBRW.P PERSISTENT SET brwproc[1] 
      (INPUT BRW_HLEV:HANDLE IN FRAME {&FRAME-NAME}).
   RUN StartSokEnrLev_UI IN brwproc[1] (INPUT THIS-PROCEDURE).   
   RUN dynprogextra IN brwproc[1] (INPUT "rowdispextra_UI",INPUT THIS-PROCEDURE).
   RUN rowdispextrakor IN  brwproc[1] (INPUT TRUE).   
   
   RUN DYNBRW.P PERSISTENT SET brwproc[2] 
      (INPUT BRW_MTRL:HANDLE IN FRAME {&FRAME-NAME}).
   RUN StartSokEnrLev_UI IN brwproc[2] (INPUT THIS-PROCEDURE).   
   RUN dynprogextra IN brwproc[2] (INPUT "rowdispextra_UI",INPUT THIS-PROCEDURE).
   RUN rowdispextrakor IN  brwproc[2] (INPUT TRUE).   
   RUN setcolindex_UI IN brwproc[2] (INPUT "ENR").
/*    RUN settitlenum_UI IN brwproc[1] (INPUT TRUE). */
   RUN settitlenum_UI IN brwproc[2] (INPUT TRUE).

   IF Guru.Konstanter:appcon THEN DO:
      RUN MTRLHMT.P PERSISTENT SET mtrlhmtapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN MTRLHMT.P PERSISTENT SET mtrlhmtapph.
   END.  
   IF Guru.Konstanter:appcon THEN DO:
      RUN SKAPANDAPP.P PERSISTENT SET skapandapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN SKAPANDAPP.P PERSISTENT SET skapandapph.
   END.  
   IF Guru.Konstanter:appcon THEN DO:
      RUN NETTOMARK.P PERSISTENT SET nettoh ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN NETTOMARK.P PERSISTENT SET nettoh.
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
   tthandle = TEMP-TABLE huvlevtemp:HANDLE.
   RUN laddatemp_UI IN laddaproch (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "HUVUDLEV", INPUT "").
   
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
  DISPLAY FILL-IN-KOD FILL-IN-SKAPBEN FILL-IN-MOD CMB_LEV FILL-IN-ENR 
          FILL-IN-BEN RAD_SOK FILL-IN-SOKALT 
      WITH FRAME FRAME-A IN WINDOW WINDOW-1.
  ENABLE RECT-6 IMAGE-8 FILL-IN-KOD FILL-IN-SKAPBEN FILL-IN-MOD BRW_HLEV 
         BRW_MTRL FBTN_VISA FBTN_SKRIV btn_over btn_back CMB_LEV BTN_ANT 
         FILL-IN-ENR FILL-IN-BEN RAD_SOK FBTN_OK BTN_AVB FILL-IN-SOKALT 
      WITH FRAME FRAME-A IN WINDOW WINDOW-1.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
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
   IF brwname = "BRW_HLEV" THEN valbrw = 1. 
   IF brwname = "BRW_MTRL" THEN valbrw = 2.
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
         valenr = skap_mtrl.ENR.
         FIND FIRST levtemp WHERE levtemp.LEVKOD = skap_mtrl.LEVKOD
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
      orgfraga = " WHERE KALKNR = " + STRING(0) + " AND LEVKOD = '" + STRING(vald_lev) + "' USE-INDEX LEV".
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE over_UI WINDOW-1 
PROCEDURE over_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/    
   FIND FIRST skap_mtrl WHERE skap_mtrl.ENR = mtrltemp.ENR AND skap_mtrl.LEVKOD = mtrltemp.LEVKOD 
   NO-LOCK NO-ERROR. 
   IF AVAILABLE skap_mtrl THEN DO:
      musz = musz.
   END.      
   ELSE DO:
      CREATE skap_mtrl. 
      ASSIGN               
      skap_mtrl.ENR = mtrltemp.ENR
      skap_mtrl.BENAMNING = mtrltemp.BENAMNING
      skap_mtrl.ENHET = mtrltemp.ENHET 
      skap_mtrl.ANTAL = 1              
      skap_mtrl.PRIS = mtrltemp.NPRIS          
      skap_mtrl.LEVKOD = mtrltemp.LEVKOD.              
      IF Guru.Konstanter:varforetypval[29] = 1  THEN DO:                 
         /*nettopris beredning inköp*/
         FIND FIRST ikmtrltemp WHERE ikmtrltemp.LEVKOD = mtrltemp.LEVKOD AND ikmtrltemp.ENR = mtrltemp.ENR AND ikmtrltemp.KALKNR = 0
         NO-LOCK NO-ERROR.
         IF NOT AVAILABLE ikmtrltemp THEN DO:                        
            CREATE ikmtrltemp.
            BUFFER-COPY mtrltemp TO ikmtrltemp.                  
         END.
      END.
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
   IF vad = 2 THEN DO: 
      IF AVAILABLE skap_mtrl THEN DO:         
         FIND FIRST ikmtrltemp WHERE ikmtrltemp.LEVKOD = skap_mtrl.LEVKOD AND ikmtrltemp.ENR = skap_mtrl.ENR AND ikmtrltemp.KALKNR = 0
         AND ikmtrltemp.KUND = TRUE  NO-LOCK NO-ERROR.
         IF AVAILABLE ikmtrltemp THEN DO:    
            ASSIGN
            skap_mtrl.ENR:BGCOLOR IN BROWSE BRW_MTRL = Guru.Konstanter:varforetypval[28]
            skap_mtrl.PRIS:BGCOLOR IN BROWSE BRW_MTRL = Guru.Konstanter:varforetypval[28].                   
         END.
         ELSE DO:
            FIND FIRST ikmtrltemp WHERE ikmtrltemp.LEVKOD = skap_mtrl.LEVKOD AND ikmtrltemp.ENR = skap_mtrl.ENR AND ikmtrltemp.KALKNR = 0
            AND ikmtrltemp.KUND = ?  NO-LOCK NO-ERROR.
            IF AVAILABLE ikmtrltemp THEN DO:    
               ASSIGN
               skap_mtrl.ENR:BGCOLOR IN BROWSE BRW_MTRL = Guru.Konstanter:varforetypval[38]
               skap_mtrl.PRIS:BGCOLOR IN BROWSE BRW_MTRL = Guru.Konstanter:varforetypval[38].                   
            END.
         END.
      END.
   END.   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

