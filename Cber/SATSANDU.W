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
&Scoped-define NEW 
{GLOBVAR2DEL1.I} 
{DEFSOK.I}
{SOKDEF.I}
&Scoped-define SHARED SHARED 
{LEVTEMP.I}
{MTRLTEMP.I}
DEFINE SHARED VARIABLE huvudlev AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO. 
DEFINE SHARED VARIABLE ny AS LOGICAL NO-UNDO.    
DEFINE SHARED VARIABLE skap_row AS ROWID NO-UNDO.  
DEFINE SHARED VARIABLE skapben AS CHARACTER NO-UNDO.       
DEFINE SHARED VARIABLE skapenh AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE skapenr AS CHARACTER NO-UNDO.       
DEFINE SHARED VARIABLE skapkod AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE skappris AS DECIMAL NO-UNDO.
DEFINE SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.  
DEFINE SHARED VARIABLE typkod AS CHARACTER NO-UNDO. 
DEFINE SHARED VARIABLE vald_kundlev AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE vald_lev AS CHARACTER NO-UNDO.  
DEFINE VARIABLE antal_raknare AS INTEGER NO-UNDO. 
DEFINE VARIABLE antal_valda AS INTEGER NO-UNDO.
DEFINE VARIABLE aosok AS CHARACTER FORMAT "X(40)" NO-UNDO.
DEFINE VARIABLE lev AS CHARACTER NO-UNDO.  
DEFINE VARIABLE mtrl_rowid AS ROWID NO-UNDO.
DEFINE VARIABLE posok AS CHARACTER FORMAT "X(11)" NO-UNDO.
DEFINE VARIABLE prisvar AS DECIMAL NO-UNDO.
DEFINE VARIABLE sats_row AS ROWID NO-UNDO.
DEFINE VARIABLE sparenr AS CHARACTER NO-UNDO.
DEFINE VARIABLE sparkod AS CHARACTER NO-UNDO. 
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE svar AS LOGICAL NO-UNDO.
DEFINE VARIABLE val AS LOGICAL NO-UNDO.
DEFINE VARIABLE vald_lev2 AS CHARACTER NO-UNDO.  
DEFINE VARIABLE satsandapph AS HANDLE NO-UNDO.
DEFINE VARIABLE felmedd AS CHARACTER NO-UNDO.
DEFINE VARIABLE enhvar  AS CHARACTER NO-UNDO.
DEFINE VARIABLE mtrlhmtapph AS HANDLE NO-UNDO.
DEFINE VARIABLE satsvar AS LOGICAL NO-UNDO.
DEFINE VARIABLE nettoh AS HANDLE NO-UNDO.
DEFINE SHARED TEMP-TABLE sats_mtrl
   FIELD KOD AS CHARACTER
   FIELD ENR AS CHARACTER 
   FIELD BENAMNING AS CHARACTER LABEL "Benämning"
   FIELD ENHET AS CHARACTER LABEL "Enhet" 
   FIELD ANTAL AS INTEGER LABEL "Antal"
   FIELD PRIS AS DECIMAL LABEL "Pris"
   FIELD LEVKOD AS CHARACTER.

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
&Scoped-define INTERNAL-TABLES mtrltemp sats_mtrl

/* Definitions for BROWSE BRW_HLEV                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_HLEV mtrltemp.Enr mtrltemp.Benamning ~
mtrltemp.Enhet mtrltemp.NPRIS 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_HLEV 
&Scoped-define QUERY-STRING-BRW_HLEV FOR EACH mtrltemp NO-LOCK ~
    BY mtrltemp.Enr INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BRW_HLEV OPEN QUERY BRW_HLEV FOR EACH mtrltemp NO-LOCK ~
    BY mtrltemp.Enr INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BRW_HLEV mtrltemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_HLEV mtrltemp


/* Definitions for BROWSE BRW_MTRL                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_MTRL sats_mtrl.ENR sats_mtrl.ANTAL ~
sats_mtrl.BENAMNING sats_mtrl.ENHET sats_mtrl.PRIS 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_MTRL 
&Scoped-define QUERY-STRING-BRW_MTRL FOR EACH sats_mtrl NO-LOCK
&Scoped-define OPEN-QUERY-BRW_MTRL OPEN QUERY BRW_MTRL FOR EACH sats_mtrl NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_MTRL sats_mtrl
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_MTRL sats_mtrl


/* Definitions for FRAME FRAME-A                                        */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 IMAGE-6 FILL-IN-KOD FILL-IN-SKAPBEN ~
FILL-IN-ENH FILL-IN-PRIS BRW_HLEV BRW_MTRL FBTN_PRIS FBTN_VISA FBTN_SKRIV ~
btn_over btn_back BTN_ANT FILL-IN-ENR FILL-IN-BEN RAD_SOK BTN_OK BTN_AVB ~
FILL-IN-SOKALT 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-KOD FILL-IN-SKAPBEN ~
FILL-IN-ENRSATS FILL-IN-ENH FILL-IN-PRIS FILL-IN-ENR FILL-IN-BEN RAD_SOK ~
FILL-IN-SOKALT 

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

DEFINE BUTTON BTN_OK 
     LABEL "Ok" 
     SIZE 14 BY 1
     FGCOLOR 1 .

DEFINE BUTTON btn_over 
     IMAGE-UP FILE "BILDER\next-u":U
     LABEL "":L 
     SIZE 4 BY 1.21.

DEFINE BUTTON FBTN_PRIS 
     LABEL "Ber. pris" 
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

DEFINE VARIABLE FILL-IN-BEN AS CHARACTER FORMAT "X(40)":U 
     LABEL "Benämning" 
     VIEW-AS FILL-IN 
     SIZE 28.13 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-ENH AS CHARACTER FORMAT "X(3)":U 
     LABEL "Enhet" 
     VIEW-AS FILL-IN 
     SIZE 5.5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-ENR AS CHARACTER FORMAT "X(11)":U 
     LABEL "Enr" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-ENRSATS AS CHARACTER FORMAT "X(11)":U 
     LABEL "Enr" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-KOD AS CHARACTER FORMAT "X(11)":U 
     LABEL "Kod" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-PRIS AS DECIMAL FORMAT "->>>>9.99":U INITIAL 0 
     LABEL "Pris" 
     VIEW-AS FILL-IN 
     SIZE 10.38 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-SKAPBEN AS CHARACTER FORMAT "X(30)":U 
     LABEL "Benämning" 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-SOKALT AS CHARACTER FORMAT "X(256)":U INITIAL "Sökalternativ:" 
      VIEW-AS TEXT 
     SIZE 11 BY .63 NO-UNDO.

DEFINE IMAGE IMAGE-6
     FILENAME "BILDER\sokpa.gif":U CONVERT-3D-COLORS
     SIZE 8 BY .75.

DEFINE VARIABLE RAD_SOK AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Början", 1,
"Någonstans", 2,
"Slutet", 3
     SIZE 31.5 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 46.5 BY 4.25
     BGCOLOR 8 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_HLEV FOR 
      mtrltemp SCROLLING.

DEFINE QUERY BRW_MTRL FOR 
      sats_mtrl SCROLLING.
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
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING MULTIPLE SIZE 46.5 BY 15.71
         TITLE "Materiellista leverantör.".

DEFINE BROWSE BRW_MTRL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_MTRL WINDOW-1 _STRUCTURED
  QUERY BRW_MTRL NO-LOCK DISPLAY
      sats_mtrl.ENR COLUMN-LABEL "Enr" FORMAT "X(11)":U
      sats_mtrl.ANTAL COLUMN-LABEL "Antal" FORMAT ">>>>9":U
      sats_mtrl.BENAMNING COLUMN-LABEL "Benämning" FORMAT "x(25)":U
      sats_mtrl.ENHET COLUMN-LABEL "Enhet" FORMAT "x(5)":U
      sats_mtrl.PRIS COLUMN-LABEL "Pris" FORMAT ">>>>99.99":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING MULTIPLE SIZE 57.88 BY 15.71
         TITLE "Upplagda materiel".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     FILL-IN-KOD AT ROW 1.5 COL 12.5 COLON-ALIGNED
     FILL-IN-SKAPBEN AT ROW 2.75 COL 12.5 COLON-ALIGNED
     FILL-IN-ENRSATS AT ROW 4 COL 12.5 COLON-ALIGNED
     FILL-IN-ENH AT ROW 4 COL 36.5 COLON-ALIGNED
     FILL-IN-PRIS AT ROW 4 COL 51.5 COLON-ALIGNED
     BRW_HLEV AT ROW 5.58 COL 1.5
     BRW_MTRL AT ROW 5.58 COL 52.63
     FBTN_PRIS AT ROW 8 COL 111
     FBTN_VISA AT ROW 9.08 COL 111
     FBTN_SKRIV AT ROW 10.21 COL 111
     btn_over AT ROW 10.54 COL 48.25
     btn_back AT ROW 14.04 COL 48.25
     BTN_ANT AT ROW 21.63 COL 75.5
     FILL-IN-ENR AT ROW 21.92 COL 16.38 COLON-ALIGNED
     FILL-IN-BEN AT ROW 23.17 COL 16.38 COLON-ALIGNED
     RAD_SOK AT ROW 24.67 COL 16 NO-LABEL
     BTN_OK AT ROW 24.92 COL 96
     BTN_AVB AT ROW 24.92 COL 111
     FILL-IN-SOKALT AT ROW 24.71 COL 2.5 NO-LABEL
     RECT-6 AT ROW 21.63 COL 1.5
     IMAGE-6 AT ROW 21.83 COL 2.13
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 124.63 BY 25.21.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: WINDOW
   Temp-Tables and Buffers:
      TABLE: mtrltemp T "?" NO-UNDO temp-db mtrltemp
      TABLE: ? T "?" NO-UNDO temp-db sats_mtrl
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW WINDOW-1 ASSIGN
         HIDDEN             = YES
         TITLE              = ""
         HEIGHT             = 25.21
         WIDTH              = 124.75
         MAX-HEIGHT         = 27.88
         MAX-WIDTH          = 125.25
         VIRTUAL-HEIGHT     = 27.88
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
/* BROWSE-TAB BRW_HLEV FILL-IN-PRIS FRAME-A */
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
       FILL-IN-BEN:HIDDEN IN FRAME FRAME-A           = TRUE.

ASSIGN 
       FILL-IN-ENR:HIDDEN IN FRAME FRAME-A           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-ENRSATS IN FRAME FRAME-A
   NO-ENABLE                                                            */
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
     _Options          = "NO-LOCK INDEXED-REPOSITION"
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
     _TblList          = "Temp-Tables.sats_mtrl"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.sats_mtrl.ENR
"sats_mtrl.ENR" "Enr" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.sats_mtrl.ANTAL
"sats_mtrl.ANTAL" "Antal" ">>>>9" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.sats_mtrl.BENAMNING
"sats_mtrl.BENAMNING" "Benämning" "x(25)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.sats_mtrl.ENHET
"sats_mtrl.ENHET" "Enhet" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.sats_mtrl.PRIS
"sats_mtrl.PRIS" "Pris" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_MTRL */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME BRW_HLEV
&Scoped-define SELF-NAME BRW_HLEV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_HLEV WINDOW-1
ON MOUSE-SELECT-DBLCLICK OF BRW_HLEV IN FRAME FRAME-A /* Materiellista leverantör. */
DO:
    APPLY "CHOOSE" TO BTN_OVER.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_HLEV WINDOW-1
ON START-SEARCH OF BRW_HLEV IN FRAME FRAME-A /* Materiellista leverantör. */
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
   
   RUN SATSANTU.W (INPUT ROWID(sats_mtrl)). 
   RUN refreshbrw_UI IN brwproc[2].
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
      IF AVAILABLE sats_mtrl THEN RUN SATSANTU.W (INPUT ROWID(sats_mtrl)).
     
      antal_raknare = antal_raknare + 1.   
   END.   
   RUN refreshbrw_UI IN brwproc[2].
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
         APPLY "CHOOSE" TO BTN_OK IN FRAME {&FRAME-NAME}.
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
      IF AVAILABLE sats_mtrl THEN DELETE sats_mtrl.
      IF antal_raknare = antal_valda THEN RUN selnextprevrow_UI IN brwproc[2].
      antal_raknare = antal_raknare + 1.   
   END.     
   RUN openbdynspec_UI IN brwproc[2].
   RUN lastselectdyn_UI IN brwproc[2].
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK WINDOW-1
ON CHOOSE OF BTN_OK IN FRAME FRAME-A /* Ok */
DO:  
   {muswait.i}  
   ASSIGN
   FILL-IN-ENRSATS = INPUT FILL-IN-ENRSATS
   FILL-IN-KOD     = INPUT FILL-IN-KOD    
   FILL-IN-SKAPBEN = INPUT FILL-IN-SKAPBEN
   enhvar = INPUT FILL-IN-ENH
   prisvar = INPUT FILL-IN-PRIS.
   felmedd = "".
   RUN okcheck_UI IN satsandapph (INPUT FILL-IN-ENRSATS,INPUT vald_lev,OUTPUT felmedd).
   IF felmedd NE "" THEN DO:
      MESSAGE felmedd VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Fortsätta?" UPDATE svar AS LOGICAL.         
      felmedd = "".
      IF svar THEN DO:
         musz = musz.
      END.
      ELSE DO:
         APPLY "ENTRY" TO FILL-IN-ENRSATS IN FRAME {&FRAME-NAME}.
         RETURN NO-APPLY.
      END.     
   END.
   RUN btnok_UI IN satsandapph (INPUT ny,INPUT-OUTPUT skap_row,INPUT sparkod,
                                INPUT FILL-IN-ENRSATS,INPUT FILL-IN-KOD,
                                INPUT FILL-IN-SKAPBEN,INPUT enhvar,INPUT prisvar,
                                INPUT vald_lev,INPUT TABLE sats_mtrl).
   IF ny = FALSE THEN DO:
      FIND FIRST satstemp WHERE satstemp.SATSROW = skap_row NO-LOCK NO-ERROR.
      IF AVAILABLE satstemp THEN DO:
         DELETE satstemp.
      END.
   END.
   musz = FALSE.
   APPLY "CLOSE":U TO THIS-PROCEDURE.                  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK WINDOW-1
ON MOUSE-MENU-CLICK OF BTN_OK IN FRAME FRAME-A /* Ok */
DO:
   RUN SIDLANGD.W.
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
         FIND FIRST sats_mtrl WHERE sats_mtrl.ENR = mtrltemp.ENR NO-LOCK NO-ERROR. 
         IF NOT AVAILABLE sats_mtrl THEN DO:
            IF Guru.Konstanter:varforetypval[29] = 1  THEN DO:                 
               /*nettopris beredning */
               FIND FIRST ikmtrltemp WHERE ikmtrltemp.LEVKOD = mtrltemp.LEVKOD AND ikmtrltemp.ENR = mtrltemp.ENR AND ikmtrltemp.KALKNR = 0
               NO-LOCK NO-ERROR.
               IF NOT AVAILABLE ikmtrltemp THEN DO:                        
                  CREATE ikmtrltemp.
                  BUFFER-COPY mtrltemp TO ikmtrltemp.                  
               END.
            END.
            RUN over_UI IN satsandapph (INPUT mtrltemp.MTRLROW,OUTPUT TABLE sats_mtrl APPEND).         
         END.
      END.
      IF antal_raknare = antal_valda THEN DO:
         FIND FIRST sats_mtrl WHERE sats_mtrl.ENR = mtrltemp.ENR NO-LOCK NO-ERROR.
         IF AVAILABLE sats_mtrl THEN RUN setlastrowid_UI IN brwproc[2] (INPUT ROWID(sats_mtrl)).
      END.
      antal_raknare = antal_raknare + 1.   
   END.   
   RUN openbdynspec_UI IN brwproc[2].                                         
   RUN lastselectdyn_UI IN brwproc[2].   
   IF AVAILABLE mtrltemp THEN DO:
      status-ok = BRW_HLEV:DESELECT-ROWS().
   END.
       
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_PRIS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_PRIS WINDOW-1
ON CHOOSE OF FBTN_PRIS IN FRAME FRAME-A /* Ber. pris */
DO:
   {muswait.i}   
   MESSAGE "Vill Ni att materielsatsens pris ska vara summan av dess ingående delar, 'klicka' Ja. Om inte 'klicka' Nej och ange det pris som ska gälla."
   VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Pris?" UPDATE svar AS LOGICAL. 
   IF SVAR THEN DO:
      prisvar = 0.
      FOR EACH sats_mtrl:
         prisvar = prisvar + sats_mtrl.ANTAL * sats_mtrl.PRIS.
      END.
      FILL-IN-PRIS = prisvar.
      DISPLAY FILL-IN-PRIS WITH FRAME {&FRAME-NAME}.    
   END.
   ELSE DO:
      RETURN NO-APPLY.
   END.   
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
      ASSIGN
      skapenr = INPUT FILL-IN-ENRSATS      
      skapenh = INPUT FILL-IN-ENH 
      skappris = INPUT FILL-IN-PRIS
      skapben = INPUT FILL-IN-SKAPBEN
      skapkod = INPUT FILL-IN-KOD                
      skrivut = TRUE.
      {AVBGOM.I}
      RUN VISSATSU.W.
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
   skapenr = INPUT FILL-IN-ENRSATS      
   skapenh = INPUT FILL-IN-ENH 
   skappris = INPUT FILL-IN-PRIS
   skapben = INPUT FILL-IN-SKAPBEN
   skapkod = INPUT FILL-IN-KOD.
   IF musz = TRUE THEN musz = FALSE.   
   ASSIGN    
   skrivut = FALSE.   
   {AVBGOM.I}
   RUN VISSATSU.W.
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
   ASSIGN
   RAD_SOK = INPUT RAD_SOK
   FILL-IN-BEN = INPUT FILL-IN-BEN.
   IF FILL-IN-BEN = "" THEN DO:
      MESSAGE "Sökbegreppet kan inte vara blankt." VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN-BEN IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
   END.
   stjarnvar = INDEX(FILL-IN-BEN,"*",1).
   IF stjarnvar NE 0 THEN DO:
      MESSAGE "Ni behöver inte använda tecknet * vid sökningen" VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
   IF RAD_SOK = 1 THEN DO:
      ASSIGN
      aosok = FILL-IN-BEN.
      begvar = TRUE.
   END.
   ELSE IF RAD_SOK = 2 THEN DO:
      ASSIGN
      aosok = "*" + FILL-IN-BEN + "*"
      begvar = FALSE.      
   END.
   ELSE IF RAD_SOK = 3 THEN DO:
      ASSIGN
      aosok = "*" + FILL-IN-BEN.
      begvar = FALSE.
   END.   
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


&Scoped-define SELF-NAME FILL-IN-ENRSATS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-ENRSATS WINDOW-1
ON LEAVE OF FILL-IN-ENRSATS IN FRAME FRAME-A /* Enr */
DO:  
   felmedd = "".
   IF INPUT FILL-IN-ENRSATS = "" THEN DO: 
      MESSAGE Guru.Konstanter:genk " kan inte vara blank!" 
      VIEW-AS ALERT-BOX TITLE "Meddelande".
      RETURN NO-APPLY.  
   END.
   ELSE DO:
      RUN enrsats_UI IN satsandapph (INPUT ny,INPUT sparenr,INPUT FILL-IN-ENRSATS,
                                     INPUT vald_lev,OUTPUT felmedd).
      IF felmedd NE "" THEN DO:
         MESSAGE felmedd VIEW-AS ALERT-BOX TITLE "Meddelande".
         felmedd = "".
         RETURN NO-APPLY.   
      END.
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-KOD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-KOD WINDOW-1
ON LEAVE OF FILL-IN-KOD IN FRAME FRAME-A /* Kod */
DO:  
   DEBUGGER:SET-BREAK().
   IF INPUT FILL-IN-KOD = "" THEN DO: 
      MESSAGE "Kod kan inte vara blank!" 
      VIEW-AS ALERT-BOX TITLE "Meddelande".
      RETURN NO-APPLY.  
   END.
   ELSE DO:
      FILL-IN-KOD = input FILL-IN-KOD.
      FILL-IN-ENRSATS =  FILL-IN-KOD.
      RUN kod_UI IN satsandapph (INPUT ny,INPUT sparkod,INPUT FILL-IN-KOD,
                                     INPUT vald_lev,OUTPUT felmedd).   
      IF felmedd NE "" THEN DO:
         MESSAGE felmedd VIEW-AS ALERT-BOX TITLE "Meddelande".
         felmedd = "".
         RETURN NO-APPLY.   
      END.
      RUN enrsats_UI IN satsandapph (INPUT ny,INPUT sparenr,INPUT FILL-IN-ENRSATS,
                                     INPUT vald_lev,OUTPUT felmedd).
      IF felmedd NE "" THEN DO:
         MESSAGE felmedd VIEW-AS ALERT-BOX TITLE "Meddelande".
         felmedd = "".
         RETURN NO-APPLY.   
      END.                                     
   END.  
   
   display FILL-IN-ENRSATS WITH FRAME FRAME-A IN WINDOW WINDOW-1.
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
   IF VALID-HANDLE(satsandapph) THEN DELETE PROCEDURE satsandapph.
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
   EMPTY TEMP-TABLE sats_mtrl NO-ERROR. 
   EMPTY TEMP-TABLE ikmtrltemp NO-ERROR. 
   IF ny = TRUE THEN DO:
      ASSIGN WINDOW-1:TITLE = "Ny materielsats".    
   END.
   ELSE DO:
      FIND FIRST satstemp WHERE satstemp.SATSROW = skap_row NO-LOCK.
      ASSIGN WINDOW-1:TITLE = "Ändring av - " + satstemp.BENAMNING.    
      ASSIGN
      mtrltemp.ENR:LABEL IN BROWSE BRW_HLEV = Guru.Konstanter:genk
      sats_mtrl.ENR:LABEL IN BROWSE BRW_MTRL = Guru.Konstanter:genk
      FILL-IN-ENR:LABEL = Guru.Konstanter:genk
      FILL-IN-ENRSATS:LABEL = Guru.Konstanter:genk
      FILL-IN-KOD = satstemp.KOD
      FILL-IN-SKAPBEN = satstemp.BENAMNING
      FILL-IN-ENRSATS = satstemp.ENR
      FILL-IN-ENH = satstemp.ENHET
      FILL-IN-PRIS = satstemp.PRIS
      sparkod = satstemp.KOD
      sparenr = satstemp.ENR
      satsvar = TRUE.
      RUN satsmtrl_UI IN satsandapph (INPUT sparkod,INPUT vald_lev,
                                      OUTPUT TABLE sats_mtrl).                       
      IF Guru.Konstanter:varforetypval[29] = 1  THEN DO:        
         /*nettopris beredning adm*/                
         EMPTY TEMP-TABLE ikmtrltemp NO-ERROR. 
         RUN satsnettomark_UI IN nettoh (INPUT TABLE sats_mtrl, OUTPUT TABLE ikmtrltemp APPEND).                  
      END.      
   END.                   
   RUN enable_UI.   
   {FRMSIZE.I}   
   RUN setcolsortvar_UI IN brwproc[1] (INPUT " WHERE KALKNR = '" + STRING(0) + "' AND LEVKOD = '" + STRING(vald_lev) + "'").  
   vald_lev2 = vald_lev.         
   RUN openbdynspec_UI IN brwproc[2].                                         
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
   IF Guru.Konstanter:appcon THEN DO:
      RUN MTRLHMT.P PERSISTENT SET mtrlhmtapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN MTRLHMT.P PERSISTENT SET mtrlhmtapph.
   END.  
   IF Guru.Konstanter:appcon THEN DO:
      RUN SATSANDAPP.P PERSISTENT SET satsandapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN SATSANDAPP.P PERSISTENT SET satsandapph.
   END.  
   IF Guru.Konstanter:appcon THEN DO:
      RUN NETTOMARK.P PERSISTENT SET nettoh ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN NETTOMARK.P PERSISTENT SET nettoh.
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
  DISPLAY FILL-IN-KOD FILL-IN-SKAPBEN FILL-IN-ENRSATS FILL-IN-ENH FILL-IN-PRIS 
          FILL-IN-ENR FILL-IN-BEN RAD_SOK FILL-IN-SOKALT 
      WITH FRAME FRAME-A IN WINDOW WINDOW-1.
  ENABLE RECT-6 IMAGE-6 FILL-IN-KOD FILL-IN-SKAPBEN FILL-IN-ENH FILL-IN-PRIS 
         BRW_HLEV BRW_MTRL FBTN_PRIS FBTN_VISA FBTN_SKRIV btn_over btn_back 
         BTN_ANT FILL-IN-ENR FILL-IN-BEN RAD_SOK BTN_OK BTN_AVB FILL-IN-SOKALT 
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
         valenr = sats_mtrl.ENR.
         FIND FIRST levtemp WHERE levtemp.LEVKOD = sats_mtrl.LEVKOD
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
      orgfraga = " WHERE KALKNR = " + STRING(0) + " AND LEVKOD = '" + STRING(vald_lev2) + "' USE-INDEX LEV".
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
     orgfraga = " WHERE KALKNR = " + STRING(0) + " AND LEVKOD = '" + STRING(vald_lev2) + "' USE-INDEX LEV".
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
      IF AVAILABLE sats_mtrl THEN DO:         
         FIND FIRST ikmtrltemp WHERE ikmtrltemp.LEVKOD = sats_mtrl.LEVKOD AND ikmtrltemp.ENR = sats_mtrl.ENR AND ikmtrltemp.KALKNR = 0
         AND ikmtrltemp.KUND = TRUE  NO-LOCK NO-ERROR.
         IF AVAILABLE ikmtrltemp THEN DO:                     
            ASSIGN
            sats_mtrl.ENR:BGCOLOR IN BROWSE BRW_MTRL = Guru.Konstanter:varforetypval[28]
            sats_mtrl.PRIS:BGCOLOR IN BROWSE BRW_MTRL = Guru.Konstanter:varforetypval[28].                   
         END.
         ELSE DO:
            FIND FIRST ikmtrltemp WHERE ikmtrltemp.LEVKOD = sats_mtrl.LEVKOD AND ikmtrltemp.ENR = sats_mtrl.ENR AND ikmtrltemp.KALKNR = 0
            AND ikmtrltemp.KUND = ?  NO-LOCK NO-ERROR.
            IF AVAILABLE ikmtrltemp THEN DO:                     
               ASSIGN
               sats_mtrl.ENR:BGCOLOR IN BROWSE BRW_MTRL = Guru.Konstanter:varforetypval[38]
               sats_mtrl.PRIS:BGCOLOR IN BROWSE BRW_MTRL = Guru.Konstanter:varforetypval[38].                   
            END.
         END.
      END.
   END.   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

