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

  Created: 02/14/97 -  3:20 pm

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
{BMTRL.I}
&Scoped-define SHARED SHARED
{LEVTEMP.I}
&SCOPED-DEFINE NEW NEW
{MTRLTEMP.I}
{SMTRL.I}
{KONSTRMTRL.I}

DEFINE NEW SHARED VARIABLE satstrue AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE vald_depa AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE varalla AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE valgrupp AS INTEGER NO-UNDO. 
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO. 
DEFINE VARIABLE antal_valda AS INTEGER NO-UNDO.
DEFINE VARIABLE antal_raknare AS INTEGER NO-UNDO. 
DEFINE NEW SHARED VARIABLE mtrl_rowid AS ROWID NO-UNDO.   
DEFINE VARIABLE satsvar AS LOGICAL NO-UNDO.
DEFINE VARIABLE satsvar2 AS LOGICAL NO-UNDO.
DEFINE VARIABLE temprow AS ROWID NO-UNDO.

/*MATERIEL DEFINITIONER*/

DEFINE SHARED VARIABLE sok AS LOGICAL NO-UNDO.  
DEFINE SHARED VARIABLE sokant AS LOGICAL NO-UNDO. 
DEFINE SHARED VARIABLE bestant AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE huvudlev AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE valkalknr AS INTEGER NO-UNDO. 
DEFINE SHARED VARIABLE kalkmtrl AS LOGICAL NO-UNDO.   
DEFINE SHARED VARIABLE vald_kundlev AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE vald_lev AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.  
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO. 
DEFINE SHARED VARIABLE vartpro AS CHARACTER FORMAT "X(3)" NO-UNDO.  
DEFINE VARIABLE aosok AS CHARACTER FORMAT "X(40)" NO-UNDO.
DEFINE VARIABLE posok AS CHARACTER FORMAT "X(11)" NO-UNDO.
DEFINE VARIABLE lev AS CHARACTER NO-UNDO.  
DEFINE VARIABLE svar AS LOGICAL NO-UNDO. 
DEFINE VARIABLE mtxtillyapph AS HANDLE NO-UNDO.
DEFINE VARIABLE mtrlhmtapph AS HANDLE NO-UNDO.
DEFINE VARIABLE felmedd AS CHARACTER NO-UNDO.

DEFINE VARIABLE tempbufh AS HANDLE NO-UNDO.
DEFINE VARIABLE sokstring AS CHARACTER FORMAT "x(40)" EXTENT 10 NO-UNDO.
DEFINE VARIABLE nyaquery AS CHARACTER NO-UNDO.
DEFINE VARIABLE orgbufh AS HANDLE NO-UNDO.
DEFINE VARIABLE appqueh AS HANDLE NO-UNDO.
DEFINE VARIABLE valdkommandoquery AS CHARACTER NO-UNDO.
DEFINE VARIABLE tempbufmerah AS HANDLE NO-UNDO.
DEFINE VARIABLE enrspar AS CHARACTER NO-UNDO.
    
/* DEFINE QUERY mtrlq FOR MTRLBER.    */
/*                                    */
/* DEFINE QUERY sok FOR MTRL.         */
/*                                    */
/* DEFINE BUFFER mtrlbuf FOR MTRLBER. */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A
&Scoped-define BROWSE-NAME BRW_HLEV

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES mtrltemp mtrldeptemp

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


/* Definitions for BROWSE BRW_HLEV2                                     */
&Scoped-define FIELDS-IN-QUERY-BRW_HLEV2 mtrldeptemp.ENR ~
mtrldeptemp.BENAMNING mtrldeptemp.ENHET mtrldeptemp.LEVKOD ~
mtrldeptemp.SALDO mtrldeptemp.NPRIS mtrldeptemp.LAGER 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_HLEV2 
&Scoped-define QUERY-STRING-BRW_HLEV2 FOR EACH mtrldeptemp NO-LOCK ~
    BY mtrldeptemp.Enr
&Scoped-define OPEN-QUERY-BRW_HLEV2 OPEN QUERY BRW_HLEV2 FOR EACH mtrldeptemp NO-LOCK ~
    BY mtrldeptemp.Enr.
&Scoped-define TABLES-IN-QUERY-BRW_HLEV2 mtrldeptemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_HLEV2 mtrldeptemp


/* Definitions for FRAME FRAME-A                                        */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS IMAGE-10 IMAGE-11 RECT-3 RECT-4 BRW_HLEV2 ~
BRW_HLEV BTN_BYT CMB_LEV TOG_BER FILL-IN-KOMM TOG_MTRL FILL-IN-ENR2 ~
FILL-IN-ENR FILL-IN-BEN2 FILL-IN-BEN RAD_SOK2 RAD_SOK BTN_AVB ~
FILL-IN-SOKALT FILL-IN-SOKALT-2 
&Scoped-Define DISPLAYED-OBJECTS CMB_LEV TOG_BER FILL-IN-KOMM TOG_MTRL ~
FILL-IN-ENR2 FILL-IN-ENR FILL-IN-BEN2 FILL-IN-BEN RAD_SOK2 RAD_SOK ~
FILL-IN-SOKALT FILL-IN-SOKALT-2 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR WINDOW-1 AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB AUTO-END-KEY DEFAULT 
     LABEL "Avsluta" 
     SIZE 14 BY 1
     BGCOLOR 8 .

DEFINE BUTTON BTN_BYT 
     LABEL "Byt mtrl" 
     SIZE 14 BY 1.

DEFINE VARIABLE CMB_LEV AS CHARACTER FORMAT "X(40)":U 
     LABEL "Leverantörer" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "0" 
     DROP-DOWN-LIST
     SIZE 18.25 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-BEN AS CHARACTER FORMAT "X(40)":U 
     LABEL "Ben" 
     VIEW-AS FILL-IN 
     SIZE 29.5 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN-BEN2 AS CHARACTER FORMAT "X(40)":U 
     LABEL "Ben" 
     VIEW-AS FILL-IN 
     SIZE 29.5 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN-ENR AS CHARACTER FORMAT "X(11)":U 
     LABEL "Enr" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN-ENR2 AS CHARACTER FORMAT "X(11)":U 
     LABEL "Enr" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN-KOMM AS CHARACTER FORMAT "X(256)":U 
     LABEL "Kommentar" 
     VIEW-AS FILL-IN 
     SIZE 55 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-SOKALT AS CHARACTER FORMAT "X(256)":U INITIAL "Sökalternativ:" 
      VIEW-AS TEXT 
     SIZE 14 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN-SOKALT-2 AS CHARACTER FORMAT "X(256)":U INITIAL "Sökalternativ:" 
      VIEW-AS TEXT 
     SIZE 14 BY .83 NO-UNDO.

DEFINE IMAGE IMAGE-10
     FILENAME "BILDER\sokpa.gif":U
     SIZE 8 BY .83.

DEFINE IMAGE IMAGE-11
     FILENAME "BILDER\sokpa.gif":U
     SIZE 8 BY .83.

DEFINE VARIABLE RAD_SOK AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Början", 1,
"Någonstans", 2,
"Slutet", 3
     SIZE 35.63 BY .83 NO-UNDO.

DEFINE VARIABLE RAD_SOK2 AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Början", 1,
"Någonstans", 2,
"Slutet", 3
     SIZE 30.5 BY .83 NO-UNDO.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 46 BY 3.63
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 55.38 BY 3.63
     BGCOLOR 8 .

DEFINE VARIABLE TOG_BER AS LOGICAL INITIAL no 
     LABEL "Gör bytet även i beredningsupplägg" 
     VIEW-AS TOGGLE-BOX
     SIZE 37.5 BY .79 NO-UNDO.

DEFINE VARIABLE TOG_MTRL AS LOGICAL INITIAL no 
     LABEL "Gör bytet även i övriga leverantörskataloger" 
     VIEW-AS TOGGLE-BOX
     SIZE 37.5 BY .79 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_HLEV FOR 
      mtrltemp SCROLLING.

DEFINE QUERY BRW_HLEV2 FOR 
      mtrldeptemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_HLEV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_HLEV WINDOW-1 _STRUCTURED
  QUERY BRW_HLEV NO-LOCK DISPLAY
      mtrltemp.Enr FORMAT "X(11)":U
      mtrltemp.Benamning FORMAT "x(256)":U WIDTH 25
      mtrltemp.Enhet FORMAT "x(5)":U
      mtrltemp.NPRIS FORMAT ">>>>99.99":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 54 BY 19.42
         TITLE "Materiellista huvudleverantör.".

DEFINE BROWSE BRW_HLEV2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_HLEV2 WINDOW-1 _STRUCTURED
  QUERY BRW_HLEV2 NO-LOCK DISPLAY
      mtrldeptemp.ENR FORMAT "X(11)":U
      mtrldeptemp.BENAMNING FORMAT "x(40)":U
      mtrldeptemp.ENHET FORMAT "x(5)":U
      mtrldeptemp.LEVKOD COLUMN-LABEL "Lev-id" FORMAT "X(4)":U
      mtrldeptemp.SALDO FORMAT ">>>>>9":U
      mtrldeptemp.NPRIS COLUMN-LABEL "Pris" FORMAT ">>>>99.99":U
      mtrldeptemp.LAGER COLUMN-LABEL "Lager" FORMAT "Ja/Nej":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 54 BY 19.42
         TITLE "Depåmateriel".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     BRW_HLEV2 AT ROW 2.58 COL 1.5
     BRW_HLEV AT ROW 2.58 COL 56.5
     BTN_BYT AT ROW 8 COL 111.13
     CMB_LEV AT ROW 22.5 COL 68.13
     TOG_BER AT ROW 22.88 COL 1.5
     FILL-IN-KOMM AT ROW 23.75 COL 50.75 COLON-ALIGNED
     TOG_MTRL AT ROW 23.88 COL 1.5
     FILL-IN-ENR2 AT ROW 25.38 COL 15.38 COLON-ALIGNED
     FILL-IN-ENR AT ROW 25.38 COL 70.5 COLON-ALIGNED
     FILL-IN-BEN2 AT ROW 26.42 COL 15.38 COLON-ALIGNED
     FILL-IN-BEN AT ROW 26.42 COL 70.5 COLON-ALIGNED
     RAD_SOK2 AT ROW 27.79 COL 16.13 NO-LABEL
     RAD_SOK AT ROW 27.79 COL 69.25 NO-LABEL
     BTN_AVB AT ROW 27.79 COL 111.13
     FILL-IN-SOKALT AT ROW 27.79 COL 2 NO-LABEL
     FILL-IN-SOKALT-2 AT ROW 27.79 COL 53.38 NO-LABEL
     "Från" VIEW-AS TEXT
          SIZE 5 BY 1 AT ROW 1.5 COL 17
          FONT 17
     "Till" VIEW-AS TEXT
          SIZE 5 BY 1 AT ROW 1.5 COL 73.75
          FONT 17
     IMAGE-10 AT ROW 25.38 COL 2.13
     IMAGE-11 AT ROW 25.38 COL 54
     RECT-3 AT ROW 25.17 COL 1.5
     RECT-4 AT ROW 25.17 COL 52.88
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 124.88 BY 28.29
         CANCEL-BUTTON BTN_AVB.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: 
   Temp-Tables and Buffers:
      TABLE: mtrldeptemp T "?" NO-UNDO temp-db mtrldeptemp
      TABLE: mtrltemp T "?" NO-UNDO temp-db mtrltemp
      TABLE: satstemp T "?" NO-UNDO temp-db satstemp
      TABLE: ? T "?" NO-UNDO temp-db spec_mtrl
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW WINDOW-1 ASSIGN
         HIDDEN             = YES
         TITLE              = "Byt Enr X till Y"
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
/* BROWSE-TAB BRW_HLEV2 RECT-4 FRAME-A */
/* BROWSE-TAB BRW_HLEV BRW_HLEV2 FRAME-A */
ASSIGN 
       BRW_HLEV:HIDDEN  IN FRAME FRAME-A                = TRUE
       BRW_HLEV:MAX-DATA-GUESS IN FRAME FRAME-A         = 40000
       BRW_HLEV:ALLOW-COLUMN-SEARCHING IN FRAME FRAME-A = TRUE.

ASSIGN 
       BRW_HLEV2:HIDDEN  IN FRAME FRAME-A                = TRUE
       BRW_HLEV2:MAX-DATA-GUESS IN FRAME FRAME-A         = 40000
       BRW_HLEV2:ALLOW-COLUMN-SEARCHING IN FRAME FRAME-A = TRUE.

/* SETTINGS FOR COMBO-BOX CMB_LEV IN FRAME FRAME-A
   ALIGN-L                                                              */
ASSIGN 
       CMB_LEV:HIDDEN IN FRAME FRAME-A           = TRUE.

ASSIGN 
       FILL-IN-BEN:HIDDEN IN FRAME FRAME-A           = TRUE.

ASSIGN 
       FILL-IN-BEN2:HIDDEN IN FRAME FRAME-A           = TRUE.

ASSIGN 
       FILL-IN-ENR:HIDDEN IN FRAME FRAME-A           = TRUE.

ASSIGN 
       FILL-IN-ENR2:HIDDEN IN FRAME FRAME-A           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-SOKALT IN FRAME FRAME-A
   ALIGN-L                                                              */
ASSIGN 
       FILL-IN-SOKALT:READ-ONLY IN FRAME FRAME-A        = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-SOKALT-2 IN FRAME FRAME-A
   ALIGN-L                                                              */
ASSIGN 
       FILL-IN-SOKALT-2:READ-ONLY IN FRAME FRAME-A        = TRUE.

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
"mtrltemp.Benamning" ? "x(256)" "character" ? ? ? ? ? ? no ? no no "25" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   = Temp-Tables.mtrltemp.Enhet
     _FldNameList[4]   = Temp-Tables.mtrltemp.NPRIS
     _Query            is NOT OPENED
*/  /* BROWSE BRW_HLEV */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_HLEV2
/* Query rebuild information for BROWSE BRW_HLEV2
     _TblList          = "Temp-Tables.mtrldeptemp"
     _Options          = "NO-LOCK"
     _OrdList          = "Temp-Tables.mtrldeptemp.Enr|yes"
     _FldNameList[1]   = Temp-Tables.mtrldeptemp.ENR
     _FldNameList[2]   = Temp-Tables.mtrldeptemp.BENAMNING
     _FldNameList[3]   = Temp-Tables.mtrldeptemp.ENHET
     _FldNameList[4]   > Temp-Tables.mtrldeptemp.LEVKOD
"LEVKOD" "Lev-id" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.mtrldeptemp.SALDO
"SALDO" ? ">>>>>9" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > Temp-Tables.mtrldeptemp.NPRIS
"NPRIS" "Pris" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > Temp-Tables.mtrldeptemp.LAGER
"LAGER" "Lager" "Ja/Nej" "logical" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_HLEV2 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME BRW_HLEV
&Scoped-define SELF-NAME BRW_HLEV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_HLEV WINDOW-1
ON START-SEARCH OF BRW_HLEV IN FRAME FRAME-A /* Materiellista huvudleverantör. */
DO:
   APPLY "END-SEARCH" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_HLEV2
&Scoped-define SELF-NAME BRW_HLEV2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_HLEV2 WINDOW-1
ON START-SEARCH OF BRW_HLEV2 IN FRAME FRAME-A /* Depåmateriel */
DO:
   APPLY "END-SEARCH" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_BYT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_BYT WINDOW-1
ON CHOOSE OF BTN_BYT IN FRAME FRAME-A /* Byt mtrl */
DO:   
   {muswait.i}
   FILL-IN-KOMM = INPUT FILL-IN-KOMM.
   IF FILL-IN-KOMM = "" THEN DO:
      MESSAGE "Kommentar till byte av enr som påverkar saldon är obligatoriskt" VIEW-AS ALERT-BOX TITLE "Meddelande".
      UNDO,RETRY.
   END.      
   EMPTY TEMP-TABLE spec_mtrl NO-ERROR. 
   EMPTY TEMP-TABLE byt_mtrl NO-ERROR. 
   status-ok = BRW_HLEV2:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}.
   mtrl_rowid = mtrldeptemp.MTRLROW.
   CREATE spec_mtrl.
   ASSIGN
   spec_mtrl.ENR = mtrldeptemp.ENR
   spec_mtrl.BENAMNING = mtrldeptemp.BENAMNING
   spec_mtrl.ENHET = mtrldeptemp.ENHET
   spec_mtrl.NPRIS = mtrldeptemp.NPRIS
   spec_mtrl.LEVKOD = mtrldeptemp.LEVKOD
   spec_mtrl.BERKVANT = 1
   spec_mtrl.SATS = FALSE.   
   FIND FIRST spec_mtrl NO-LOCK NO-ERROR.   
   status-ok = BRW_HLEV:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}.
   mtrl_rowid = mtrltemp.MTRLROW.
   CREATE byt_mtrl.
   ASSIGN
   byt_mtrl.ENR = mtrltemp.ENR
   SUBSTRING(byt_mtrl.BENAMNING,1,59) = mtrltemp.BENAMNING
   byt_mtrl.ENHET = mtrltemp.ENHET
   byt_mtrl.NPRIS = mtrltemp.NPRIS
   byt_mtrl.LEVKOD = mtrltemp.LEVKOD
   byt_mtrl.BERKVANT = 1
   byt_mtrl.SATS = FALSE   
   SUBSTRING(byt_mtrl.BENAMNING,60) = FILL-IN-KOMM.
   FIND FIRST byt_mtrl NO-LOCK NO-ERROR.
   
   FIND FIRST mtrldeptemp WHERE mtrldeptemp.ENR = byt_mtrl.ENR NO-LOCK NO-ERROR.
   IF AVAILABLE mtrldeptemp THEN DO:
      /* Om enr som det ska bytas till redan är upplagt  ge möjlighet att slå ihop saldon lena 20080521*/
      MESSAGE LC(Guru.Konstanter:genk)+ " " + byt_mtrl.ENR +  " finns redan upplagd i depå. Uppdatera saldot för " + LC(Guru.Konstanter:genk) + " " +  byt_mtrl.ENR + " och ta bort  " + LC(Guru.Konstanter:genk) + " " + spec_mtrl.ENR + " istället!" VIEW-AS ALERT-BOX.            
   END.
   ELSE DO:   
      MESSAGE "Är det korrekt att Ni vill ersätta " + LC(Guru.Konstanter:genk) + ":" + spec_mtrl.ENR + " med " + LC(Guru.Konstanter:genk) + ":" +
      byt_mtrl.ENR + " i depån?" VIEW-AS ALERT-BOX
      QUESTION BUTTONS YES-NO TITLE "Meddelande" UPDATE svar.         
      IF svar THEN DO:
         IF Guru.Konstanter:appcon THEN DO:                           
            RUN DEPBYTXTY.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
            (INPUT vald_depa, INPUT Guru.Konstanter:globanv, INPUT 1, INPUT TABLE spec_mtrl, INPUT TABLE byt_mtrl).
         END.
         ELSE DO:
            RUN DEPBYTXTY.P
            (INPUT vald_depa, INPUT Guru.Konstanter:globanv, INPUT 1, INPUT TABLE spec_mtrl, INPUT TABLE byt_mtrl).
         END.
         DEBUGGER:SET-BREAK().
         enrspar = byt_mtrl.ENR.
         IF byt_mtrl.ENR = spec_mtrl.ENR THEN DO:
            DELETE mtrldeptemp.
            RUN selnextprevrow_UI IN brwproc[1]. 
            RUN refreshbrw_UI IN brwproc[1].
            RUN lastselectdyn_UI IN brwproc[1]. 
            RUN title_UI IN brwproc[1].
         END.
         ELSE DO:           
            /*RUN selnextprevrow_UI IN brwproc[1].
            RUN lastselectdyn_UI IN brwproc[1].
            IF AVAILABLE mtrldeptemp THEN enrspar = mtrldeptemp.ENR.          */  
            RUN laddamtrlber_UI IN mtxtillyapph (INPUT vald_depa, OUTPUT vald_kundlev, OUTPUT TABLE mtrldeptemp).                                   
            RUN openbdynspec_UI IN brwproc[1].             
            FIND FIRST mtrldeptemp WHERE mtrldeptemp.ENR = enrspar NO-LOCK NO-ERROR.
            RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(mtrldeptemp)).
            RUN lastselectdyn_UI IN brwproc[1].            
         END.
         MESSAGE "Byte av artikel klart!"
         VIEW-AS ALERT-BOX TITLE "Meddelande".
      END.   
   END.
   IF TOG_BER = TRUE THEN DO:            
      MESSAGE "Är det korrekt att Ni vill ersätta " + LC(Guru.Konstanter:genk) + ":" + spec_mtrl.ENR + " med " + LC(Guru.Konstanter:genk) + ":" +
      byt_mtrl.ENR + " på alla konstruktioner?" VIEW-AS ALERT-BOX
      QUESTION BUTTONS YES-NO TITLE "Meddelande" UPDATE svar.         
      IF svar THEN DO:
         IF Guru.Konstanter:appcon THEN DO:                           
            RUN DEPBYTXTY.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
            (INPUT vald_depa, INPUT Guru.Konstanter:globanv, INPUT 3, INPUT TABLE spec_mtrl, INPUT TABLE byt_mtrl).
         END.
         ELSE DO:
            RUN DEPBYTXTY.P
            (INPUT vald_depa, INPUT Guru.Konstanter:globanv, INPUT 3, INPUT TABLE spec_mtrl, INPUT TABLE byt_mtrl).
         END.
         MESSAGE "Byte av artikel klart!"
         VIEW-AS ALERT-BOX TITLE "Meddelande".
      END.        
   END.   
   IF TOG_MTRL = TRUE THEN DO:            
      MESSAGE "Är det korrekt att Ni vill ersätta " + LC(Guru.Konstanter:genk) + ":" + spec_mtrl.ENR + " med " + LC(Guru.Konstanter:genk) + ":" +
      byt_mtrl.ENR + " i alla materielkataloger?" VIEW-AS ALERT-BOX
      QUESTION BUTTONS YES-NO TITLE "Meddelande" UPDATE svar.         
      IF svar THEN DO:
         IF Guru.Konstanter:appcon THEN DO:                           
            RUN DEPBYTXTY.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
            (INPUT vald_depa, INPUT Guru.Konstanter:globanv, INPUT 2, INPUT TABLE spec_mtrl, INPUT TABLE byt_mtrl).
         END.
         ELSE DO:
            RUN DEPBYTXTY.P
            (INPUT vald_depa, INPUT Guru.Konstanter:globanv, INPUT 2, INPUT TABLE spec_mtrl, INPUT TABLE byt_mtrl).
         END.
         MESSAGE "Byte av artikel klart!"
         VIEW-AS ALERT-BOX TITLE "Meddelande".
      END.       
   END.   
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_LEV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_LEV WINDOW-1
ON VALUE-CHANGED OF CMB_LEV IN FRAME FRAME-A /* Leverantörer */
DO: 
   ASSIGN           
   lev = INPUT CMB_LEV.      
   ASSIGN                      
   BRW_HLEV:HIDDEN = FALSE                 
   satsvar = FALSE.
   IF lev = huvudlev THEN DO:
      RUN setorgtitle_UI IN brwproc[2] (INPUT "Materiellista huvudleverantör " + lev).         
   END.
   ELSE DO:
      RUN setorgtitle_UI IN brwproc[2] (INPUT "Materiellista " + lev).                  
   END.  
   FIND FIRST levtemp WHERE levtemp.LEVNAMN = lev USE-INDEX LEV NO-LOCK NO-ERROR.
   IF AVAILABLE levtemp THEN DO:
      vald_lev = levtemp.LEVKOD. 
      RUN setcolsortvar_UI IN brwproc[2] (INPUT " WHERE mtrltemp.LEVKOD = '" + vald_lev + "' AND mtrltemp.KALKNR = 0").
      RUN openbdynspec_UI IN brwproc[2].                
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-BEN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-BEN WINDOW-1
ON ANY-KEY OF FILL-IN-BEN IN FRAME FRAME-A /* Ben */
DO:
   {TRYCKS.I}
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN-BEN IN FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-BEN WINDOW-1
ON MOUSE-SELECT-DBLCLICK OF FILL-IN-BEN IN FRAME FRAME-A /* Ben */
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
   sokstring[1] = FILL-IN-BEN.
   RUN sokoch_UI (sokstring[1]).
   IF RAD_SOK = 1 THEN DO:
      ASSIGN
      sokstring[1] = sokstring[1].
      begvar = TRUE.
   END.
   ELSE IF RAD_SOK = 2 THEN DO:
      ASSIGN
      sokstring[1] = "*" + sokstring[1] + "*"
      begvar = FALSE.      
   END.
   ELSE IF RAD_SOK = 3 THEN DO:
      ASSIGN
      sokstring[1] = "*" + sokstring[1].
      begvar = FALSE.
   END.      
   felmedd = "".
   EMPTY TEMP-TABLE mtrltemp NO-ERROR. 
   EMPTY TEMP-TABLE satstemp NO-ERROR. 
   RUN benhmt_UI IN mtrlhmtapph (INPUT sokstring[1],INPUT satsvar,INPUT begvar,INPUT vald_lev,
                 OUTPUT felmedd,OUTPUT TABLE mtrltemp,OUTPUT TABLE satstemp).
   IF felmedd NE "" THEN DO:
      MESSAGE felmedd VIEW-AS ALERT-BOX TITLE "Meddelande".
      felmedd = "".
      APPLY "ENTRY" TO FILL-IN-BEN IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
   END. 
   ELSE DO:
      IF sokstring[2] = "" THEN .
      ELSE RUN sokmera_UI.
      IF satsvar = FALSE THEN DO:         
         RUN setcolsortvar_UI IN brwproc[2] (INPUT " WHERE mtrltemp.LEVKOD = '" + vald_lev + "' AND mtrltemp.KALKNR = 0").
         RUN openbdynspec_UI IN brwproc[2].                
         FIND FIRST mtrltemp NO-LOCK NO-ERROR.
         IF AVAILABLE mtrltemp THEN DO:
            RUN setlastrowid_UI IN brwproc[2] (INPUT ROWID(mtrltemp)).
            RUN lastselectdyn_UI IN brwproc[2].
         END.                 
      END.
      ELSE DO:
         RUN setcolsortvar_UI IN brwproc[3] (INPUT " WHERE satstemp.LEVKOD = '" + vald_lev + "' AND satstemp.SATS = TRUE").
         RUN openbdynspec_UI IN brwproc[3].                
         FIND FIRST satstemp NO-LOCK NO-ERROR.
         IF AVAILABLE satstemp THEN DO:
            RUN setlastrowid_UI IN brwproc[3] (INPUT ROWID(satstemp)).
            RUN lastselectdyn_UI IN brwproc[3].
         END.
      END.
   END.
   {musarrow.i}                                 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-BEN2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-BEN2 WINDOW-1
ON ANY-KEY OF FILL-IN-BEN2 IN FRAME FRAME-A /* Ben */
DO:
   {TRYCKS.I}
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN-BEN2 IN FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-BEN2 WINDOW-1
ON MOUSE-SELECT-DBLCLICK OF FILL-IN-BEN2 IN FRAME FRAME-A /* Ben */
DO: 
   ASSIGN
   RAD_SOK2 = INPUT RAD_SOK2
   FILL-IN-BEN2 = INPUT FILL-IN-BEN2.
   IF AVAILABLE mtrldeptemp THEN temprow = ROWID(mtrldeptemp).
   IF FILL-IN-BEN2 = "" THEN DO:
      MESSAGE "Sökbegreppet kan inte vara blankt." VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN-BEN2 IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
   END.
   stjarnvar = INDEX(FILL-IN-BEN2,"*",1).
   IF stjarnvar NE 0 THEN DO:
      MESSAGE "Ni behöver inte använda tecknet * vid sökningen" VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
   IF RAD_SOK2 = 1 THEN DO:
      ASSIGN
      aosok = FILL-IN-BEN2.
      begvar = TRUE.
   END.
   ELSE IF RAD_SOK2 = 2 THEN DO:
      ASSIGN
      aosok = "*" + FILL-IN-BEN2 + "*"
      begvar = FALSE.      
   END.
   ELSE IF RAD_SOK2 = 3 THEN DO:
      ASSIGN
      aosok = "*" + FILL-IN-BEN2.
      begvar = FALSE.
   END.      
   status-ok = BRW_HLEV2:DESELECT-ROWS(). 
   IF begvar = FALSE THEN DO:
      FIND mtrldeptemp WHERE ROWID(mtrldeptemp) = temprow NO-LOCK NO-ERROR.
      FIND NEXT mtrldeptemp WHERE mtrldeptemp.BENAMNING MATCHES aosok 
      USE-INDEX ENR NO-LOCK NO-ERROR. 
      IF NOT AVAILABLE mtrldeptemp THEN DO:      
         FIND FIRST mtrldeptemp WHERE mtrldeptemp.BENAMNING MATCHES aosok
         USE-INDEX ENR NO-LOCK NO-ERROR.   
         IF NOT AVAILABLE mtrldeptemp THEN DO:
            MESSAGE "Det finns inget på sökbegreppet hos vald leverantör."
            VIEW-AS ALERT-BOX TITLE "Sökning".      
            APPLY "ENTRY" TO FILL-IN-BEN2 IN FRAME {&FRAME-NAME}.
            RETURN NO-APPLY.               
         END. 
      END.
   END.   
   ELSE DO:
      FIND mtrldeptemp WHERE ROWID(mtrldeptemp) = temprow NO-LOCK NO-ERROR.
      FIND NEXT mtrldeptemp WHERE mtrldeptemp.BENAMNING BEGINS aosok 
      USE-INDEX ENR NO-LOCK NO-ERROR. 
      IF NOT AVAILABLE mtrldeptemp THEN DO:      
         FIND FIRST mtrldeptemp WHERE mtrldeptemp.BENAMNING BEGINS aosok
         USE-INDEX ENR NO-LOCK NO-ERROR.   
         IF NOT AVAILABLE mtrldeptemp THEN DO:
            MESSAGE "Det finns inget på sökbegreppet hos vald leverantör."
            VIEW-AS ALERT-BOX TITLE "Sökning".      
            APPLY "ENTRY" TO FILL-IN-BEN2 IN FRAME {&FRAME-NAME}.
            RETURN NO-APPLY.               
         END. 
      END.      
   END.
   IF AVAILABLE mtrldeptemp THEN DO:
      RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(mtrldeptemp)).
      RUN lastselectdyn_UI IN brwproc[1].
   END.                               
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
   felmedd = "".
   EMPTY TEMP-TABLE mtrltemp NO-ERROR. 
   RUN enrhmt_UI IN mtrlhmtapph (INPUT posok,INPUT begvar,INPUT vald_lev,
                                    OUTPUT felmedd,OUTPUT TABLE mtrltemp).
   IF felmedd NE "" THEN DO:
      MESSAGE felmedd VIEW-AS ALERT-BOX TITLE "Meddelande".
      felmedd = "".
      APPLY "ENTRY" TO FILL-IN-ENR IN FRAME {&FRAME-NAME}.
      RETURN.
   END.
   ELSE DO:
      RUN setcolsortvar_UI IN brwproc[2] (INPUT " WHERE mtrltemp.LEVKOD = '" + vald_lev + "' AND mtrltemp.KALKNR = 0").
      RUN openbdynspec_UI IN brwproc[2].                
      FIND FIRST mtrltemp NO-LOCK NO-ERROR.
      IF AVAILABLE mtrltemp THEN DO:      
         RUN setlastrowid_UI IN brwproc[2] (INPUT ROWID(mtrltemp)).
         RUN lastselectdyn_UI IN brwproc[2].
      END.
   END.
   {musarrow.i}            
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-ENR2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-ENR2 WINDOW-1
ON ANY-KEY OF FILL-IN-ENR2 IN FRAME FRAME-A /* Enr */
DO:
   {TRYCKS.I}
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN-ENR2 IN FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-ENR2 WINDOW-1
ON MOUSE-SELECT-DBLCLICK OF FILL-IN-ENR2 IN FRAME FRAME-A /* Enr */
DO: 
   ASSIGN
   FILL-IN-ENR2 = INPUT FILL-IN-ENR2
   RAD_SOK2 = INPUT RAD_SOK2.
   IF FILL-IN-ENR2 = "" THEN DO:
      MESSAGE "Sökbegreppet kan inte vara blankt." VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN-ENR2 IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
   END.
   IF AVAILABLE mtrldeptemp THEN DO:
      ASSIGN temprow = ROWID(mtrldeptemp).
      status-ok = BRW_HLEV2:DESELECT-ROWS().
   END.
   stjarnvar = INDEX(FILL-IN-ENR2,"*",1).
   IF stjarnvar NE 0 THEN DO:
      MESSAGE "Ni behöver inte använda tecknet * vid sökningen" VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
   IF RAD_SOK2 = 1 THEN DO:
      ASSIGN
      posok = FILL-IN-ENR2.
      begvar = TRUE.
   END.
   ELSE IF RAD_SOK2 = 2 THEN DO:
      ASSIGN
      posok = "*" + FILL-IN-ENR2 + "*"
      begvar = FALSE.      
   END.
   ELSE IF RAD_SOK2 = 3 THEN DO:
      ASSIGN
      posok = "*" + FILL-IN-ENR2.
      begvar = FALSE.
   END.
   IF begvar = FALSE THEN DO:
      FIND mtrldeptemp WHERE ROWID(mtrldeptemp) = temprow NO-LOCK NO-ERROR.
      FIND NEXT mtrldeptemp WHERE mtrldeptemp.ENR MATCHES posok
      USE-INDEX ENR NO-LOCK NO-ERROR. 
      IF NOT AVAILABLE mtrldeptemp THEN DO:      
         FIND FIRST mtrldeptemp WHERE mtrldeptemp.ENR MATCHES posok
         USE-INDEX ENR NO-LOCK NO-ERROR.   
         IF NOT AVAILABLE mtrldeptemp THEN DO:
            MESSAGE "Det finns inget på sökbegreppet hos vald leverantör."
            VIEW-AS ALERT-BOX TITLE "Sökning".      
            APPLY "ENTRY" TO FILL-IN-ENR2 IN FRAME {&FRAME-NAME}.
            RETURN NO-APPLY.               
         END. 
      END.
   END.   
   ELSE DO:
      FIND mtrldeptemp WHERE ROWID(mtrldeptemp) = temprow NO-LOCK NO-ERROR.
      FIND NEXT mtrldeptemp WHERE mtrldeptemp.ENR BEGINS posok
      USE-INDEX ENR NO-LOCK NO-ERROR. 
      IF NOT AVAILABLE mtrldeptemp THEN DO:      
         FIND FIRST mtrldeptemp WHERE mtrldeptemp.ENR BEGINS posok
         USE-INDEX ENR NO-LOCK NO-ERROR.   
         IF NOT AVAILABLE mtrldeptemp THEN DO:
            MESSAGE "Det finns inget på sökbegreppet hos vald leverantör."
            VIEW-AS ALERT-BOX TITLE "Sökning".      
            APPLY "ENTRY" TO FILL-IN-ENR2 IN FRAME {&FRAME-NAME}.
            RETURN NO-APPLY.               
         END. 
      END.      
   END.
   IF AVAILABLE mtrldeptemp THEN DO:
      RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(mtrldeptemp)).
      RUN lastselectdyn_UI IN brwproc[1].
   END.                        
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RAD_SOK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RAD_SOK WINDOW-1
ON VALUE-CHANGED OF RAD_SOK IN FRAME FRAME-A
DO:
  RAD_SOK = INPUT RAD_SOK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RAD_SOK2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RAD_SOK2 WINDOW-1
ON VALUE-CHANGED OF RAD_SOK2 IN FRAME FRAME-A
DO:
  RAD_SOK2 = INPUT RAD_SOK2.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_BER
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_BER WINDOW-1
ON VALUE-CHANGED OF TOG_BER IN FRAME FRAME-A /* Gör bytet även i beredningsupplägg */
DO:
   TOG_BER = INPUT TOG_BER.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_MTRL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_MTRL WINDOW-1
ON VALUE-CHANGED OF TOG_MTRL IN FRAME FRAME-A /* Gör bytet även i övriga leverantörskataloger */
DO:
   TOG_MTRL = INPUT TOG_MTRL.
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
   IF VALID-HANDLE(mtxtillyapph) THEN DELETE PROCEDURE mtxtillyapph.
   IF VALID-HANDLE(mtrlhmtapph) THEN DELETE PROCEDURE mtrlhmtapph.
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
   valgrupp = ?.      
   EMPTY TEMP-TABLE spec_mtrl NO-ERROR. 
   EMPTY TEMP-TABLE byt_mtrl NO-ERROR.
   EMPTY TEMP-TABLE mtrldeptemp NO-ERROR.
   EMPTY TEMP-TABLE mtrltemp NO-ERROR.
   EMPTY TEMP-TABLE satstemp NO-ERROR.   
   mtrldeptemp.ENR:LABEL IN BROWSE BRW_HLEV2 = Guru.Konstanter:genk. 
   mtrltemp.ENR:LABEL IN BROWSE BRW_HLEV = Guru.Konstanter:genk.  
   RUN laddamtrlber_UI IN mtxtillyapph (INPUT vald_depa, OUTPUT vald_kundlev, OUTPUT TABLE mtrldeptemp).
   ASSIGN
   TOG_BER = FALSE
   TOG_MTRL = FALSE
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
      /*Niklas personlig spec_mtrl*/      
      IF levtemp.LEVKOD = "99" THEN DO:
         status-ok = CMB_LEV:ADD-LAST(Guru.Konstanter:globanv + " " + levtemp.LEVNAMN)IN FRAME {&FRAME-NAME}.
         status-ok = CMB_LEV:ADD-LAST(levtemp.LEVNAMN)IN FRAME {&FRAME-NAME}.
      END.
      ELSE DO:
         IF Guru.Konstanter:globforetag = "LULE" THEN DO:
            IF levtemp.LEVKOD NE "41" OR Guru.Konstanter:globforetag = "ELPA" THEN status-ok = CMB_LEV:ADD-LAST(levtemp.LEVNAMN)IN FRAME {&FRAME-NAME}.
         END.
         ELSE IF Guru.Konstanter:globforetag = "bode" THEN DO:
            IF levtemp.LEVKOD NE "6" THEN status-ok = CMB_LEV:ADD-LAST(levtemp.LEVNAMN)IN FRAME {&FRAME-NAME}.
         END.
         ELSE IF Guru.Konstanter:globforetag = "KRAF" THEN DO:
            IF levtemp.LEVKOD NE "6" THEN status-ok = CMB_LEV:ADD-LAST(levtemp.LEVNAMN)IN FRAME {&FRAME-NAME}.
         END.
         /*kalmar depåändring abc nu*/
         ELSE IF Guru.Konstanter:globforetag = "GKAL" THEN DO:
            IF levtemp.LEVKOD NE "20" THEN status-ok = CMB_LEV:ADD-LAST(levtemp.LEVNAMN)IN FRAME {&FRAME-NAME}.
         END.
         ELSE IF Guru.Konstanter:globforetag = "SNAT" THEN DO:
            IF levtemp.LEVKOD = "20" THEN.
            ELSE IF levtemp.LEVKOD = "21" THEN.
            ELSE status-ok = CMB_LEV:ADD-LAST(levtemp.LEVNAMN)IN FRAME {&FRAME-NAME}.
         END.
         ELSE DO:
            status-ok = CMB_LEV:ADD-LAST(levtemp.LEVNAMN)IN FRAME {&FRAME-NAME}.
         END.         
      END.   
   END.  
        
   ASSIGN 
   CMB_LEV = INPUT CMB_LEV   
   vald_lev = vald_kundlev   
   bestant = FALSE.   
   FIND FIRST mtrldeptemp NO-LOCK NO-ERROR.
   IF AVAILABLE mtrldeptemp THEN DO:
/*       RUN setcolsortvar_UI IN brwproc[1] (INPUT " WHERE mtrldeptemp.LEVKOD = '" + vald_lev + "' USE-INDEX ENR").  */
      RUN openbdynspec_UI IN brwproc[1].        
   END.
   RUN enable_UI.   
   IF Guru.Konstanter:globforetag = "LULE" OR Guru.Konstanter:globforetag = "BODE"  OR Guru.Konstanter:globforetag = "KRAF"  THEN DO:
      TOG_MTRL:HIDDEN = TRUE.
      TOG_BER:HIDDEN = TRUE.
   END.
   {FRMSIZE.I}         
   APPLY "VALUE-CHANGED" TO CMB_LEV.      
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
      (INPUT BRW_HLEV2:HANDLE IN FRAME {&FRAME-NAME}).    
   RUN DYNBRW.P PERSISTENT SET brwproc[2]
      (INPUT BRW_HLEV:HANDLE IN FRAME {&FRAME-NAME}).         
   RUN settitlenum_UI IN brwproc[1] (INPUT TRUE).
   RUN settitlenum_UI IN brwproc[2] (INPUT TRUE).   

   IF Guru.Konstanter:appcon THEN DO:
      RUN DEPXTILLYAPP.P PERSISTENT SET mtxtillyapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN DEPXTILLYAPP.P PERSISTENT SET mtxtillyapph.
   END.  
   IF Guru.Konstanter:appcon THEN DO:
      RUN MTRLHMT.P PERSISTENT SET mtrlhmtapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN MTRLHMT.P PERSISTENT SET mtrlhmtapph.
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
  DISPLAY CMB_LEV TOG_BER FILL-IN-KOMM TOG_MTRL FILL-IN-ENR2 FILL-IN-ENR 
          FILL-IN-BEN2 FILL-IN-BEN RAD_SOK2 RAD_SOK FILL-IN-SOKALT 
          FILL-IN-SOKALT-2 
      WITH FRAME FRAME-A IN WINDOW WINDOW-1.
  ENABLE IMAGE-10 IMAGE-11 RECT-3 RECT-4 BRW_HLEV2 BRW_HLEV BTN_BYT CMB_LEV 
         TOG_BER FILL-IN-KOMM TOG_MTRL FILL-IN-ENR2 FILL-IN-ENR FILL-IN-BEN2 
         FILL-IN-BEN RAD_SOK2 RAD_SOK BTN_AVB FILL-IN-SOKALT FILL-IN-SOKALT-2 
      WITH FRAME FRAME-A IN WINDOW WINDOW-1.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE kommandoskap_UI WINDOW-1 
PROCEDURE kommandoskap_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT  PARAMETER sokmera AS CHARACTER NO-UNDO.
   DEFINE VARIABLE extrafalth5 AS HANDLE NO-UNDO.
   DEFINE VARIABLE faltvar AS CHARACTER NO-UNDO.
   faltvar = "benamning".
   valdkommandoquery = "".
   IF sokstring[1] NE "" THEN DO:
      extrafalth5 = tempbufh:BUFFER-FIELD(faltvar).
      IF begvar = FALSE THEN DO:
         IF extrafalth5:DATA-TYPE = "CHARACTER" THEN DO:
            IF nyaquery = "" THEN valdkommandoquery = " FOR EACH " + orgbufh:TABLE + " WHERE " + faltvar + " MATCHES '" + sokmera + "'".
            ELSE valdkommandoquery = " FOR EACH " + orgbufh:TABLE + " WHERE " + faltvar + " MATCHES '" + sokmera + "' AND " + nyaquery.
         END.
         ELSE IF extrafalth5:DATA-TYPE = "INTEGER" THEN DO: 
            IF nyaquery = "" THEN valdkommandoquery = " FOR EACH " + orgbufh:TABLE + " WHERE " + faltvar + " MATCHES " + STRING(sokmera).
            ELSE valdkommandoquery = " FOR EACH " + orgbufh:TABLE + " WHERE " + faltvar + " MATCHES " + STRING(sokmera) + " AND " + nyaquery.
         END.
         ELSE IF extrafalth5:DATA-TYPE = "LOGICAL" THEN DO: 
            IF nyaquery = "" THEN valdkommandoquery = " FOR EACH " + orgbufh:TABLE + " WHERE " + faltvar + " MATCHES " + STRING(sokmera).
            ELSE valdkommandoquery = " FOR EACH " + orgbufh:TABLE + " WHERE " + faltvar + " MATCHES " + STRING(sokmera) + " AND " + nyaquery.
         END.          
         ELSE IF extrafalth5:DATA-TYPE = "DECIMAL" THEN DO: 
            IF nyaquery = "" THEN valdkommandoquery = " FOR EACH " + orgbufh:TABLE + " WHERE " + faltvar + " MATCHES " + STRING(sokmera).
            ELSE valdkommandoquery = " FOR EACH " + orgbufh:TABLE + " WHERE " + faltvar + " MATCHES " + STRING(sokmera) + " AND " + nyaquery.
         END.          
      END.
      ELSE DO:
         IF extrafalth5:DATA-TYPE = "CHARACTER" THEN DO:
            IF nyaquery = "" THEN valdkommandoquery = " FOR EACH " + orgbufh:TABLE + " WHERE " + faltvar + " BEGINS '" + sokmera + "'".
            ELSE valdkommandoquery = " FOR EACH " + orgbufh:TABLE + " WHERE " + faltvar + " BEGINS '" + sokmera + "' AND " +  nyaquery.
         END.
         ELSE IF extrafalth5:DATA-TYPE = "INTEGER" THEN DO: 
            IF nyaquery = "" THEN valdkommandoquery = " FOR EACH " + orgbufh:TABLE + " WHERE " + faltvar + " BEGINS " + STRING(sokmera).
            ELSE valdkommandoquery = " FOR EACH " + orgbufh:TABLE + " WHERE " + faltvar + " BEGINS " + STRING(sokmera) + " AND " + nyaquery.
         END.
         ELSE IF extrafalth5:DATA-TYPE = "LOGICAL" THEN DO: 
            IF nyaquery = "" THEN valdkommandoquery = " FOR EACH " + orgbufh:TABLE + " WHERE " + faltvar + " BEGINS " + STRING(sokmera).
            ELSE valdkommandoquery = " FOR EACH " + orgbufh:TABLE + " WHERE " + faltvar + " BEGINS " + STRING(sokmera) + " AND " + nyaquery.
         END.       
         ELSE IF extrafalth5:DATA-TYPE = "DECIMAL" THEN DO: 
            IF nyaquery = "" THEN valdkommandoquery = " FOR EACH " + orgbufh:TABLE + " WHERE " + faltvar + " BEGINS " + STRING(sokmera).
            ELSE valdkommandoquery = " FOR EACH " + orgbufh:TABLE + " WHERE " + faltvar + " BEGINS " + STRING(sokmera) + " AND " + nyaquery.
         END.       
      END.
      DELETE OBJECT extrafalth5 NO-ERROR. 
      extrafalth5 = ?.
   END.
   ELSE DO:
      valdkommandoquery = " FOR EACH " + orgbufh:TABLE + " WHERE " + nyaquery.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sokmera_UI WINDOW-1 
PROCEDURE sokmera_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE VARIABLE rkollvar AS INTEGER NO-UNDO.
   DEFINE VARIABLE ttcopyh AS HANDLE NO-UNDO.
   DELETE WIDGET-POOL "dynpool" NO-ERROR.
   IF satsvar = FALSE THEN tempbufh = TEMP-TABLE mtrltemp:HANDLE:DEFAULT-BUFFER-HANDLE.
   ELSE tempbufh = TEMP-TABLE satstemp:HANDLE:DEFAULT-BUFFER-HANDLE.
   CREATE TEMP-TABLE ttcopyh. 
   ttcopyh:CREATE-LIKE(tempbufh).
   ttcopyh:TEMP-TABLE-PREPARE("ttkopia").
   tempbufmerah = ttcopyh:DEFAULT-BUFFER-HANDLE.
   nyaquery = "".
   rkollvar = 2.
   REPEAT: 
      
      orgbufh = tempbufh.
      IF sokstring[rkollvar] = "" THEN LEAVE.
      RUN starwars_UI (INPUT sokstring[rkollvar]).
      IF begvar = ? THEN LEAVE.
      RUN kommandoskap_UI (INPUT sokstring[rkollvar]).
      /* Skapar query och kopplar queryn till den skarpa tabellens buffer */
      CREATE QUERY appqueh.
      appqueh:SET-BUFFERS(orgbufh).
      appqueh:QUERY-PREPARE(valdkommandoquery).
      appqueh:QUERY-OPEN().      
      appqueh:GET-FIRST(NO-LOCK).
      IF appqueh:QUERY-OFF-END = TRUE THEN DO:
         orgbufh:EMPTY-TEMP-TABLE  NO-ERROR.
         felmedd = "Det finns inget på sökbegreppet.". 
         LEAVE.     
      END.
      ELSE DO:
         tempbufmerah:EMPTY-TEMP-TABLE  NO-ERROR.
         DO WHILE appqueh:QUERY-OFF-END = FALSE:
            tempbufmerah:BUFFER-CREATE().
            tempbufmerah:BUFFER-COPY(orgbufh) NO-ERROR.
            appqueh:GET-NEXT(NO-LOCK).
         END.         
      END.
      orgbufh:EMPTY-TEMP-TABLE  NO-ERROR.
      CREATE QUERY appqueh.
      appqueh:SET-BUFFERS(tempbufmerah).
      appqueh:QUERY-PREPARE("FOR EACH " + tempbufmerah:TABLE).
      appqueh:QUERY-OPEN().      
      appqueh:GET-FIRST(NO-LOCK).
      DO WHILE appqueh:QUERY-OFF-END = FALSE:
         orgbufh:BUFFER-CREATE().
         orgbufh:BUFFER-COPY(tempbufmerah) NO-ERROR.
         appqueh:GET-NEXT(NO-LOCK).             
      END.      
      tempbufmerah:EMPTY-TEMP-TABLE  NO-ERROR.
      rkollvar = rkollvar + 1.
      IF rkollvar > 10 THEN LEAVE.
   END.  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sokoch_UI WINDOW-1 
PROCEDURE sokoch_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT  PARAMETER sokstringorg AS CHARACTER NO-UNDO.
   DEFINE VARIABLE forsta AS CHARACTER NO-UNDO.
   DEFINE VARIABLE sista AS CHARACTER NO-UNDO.
   DEFINE VARIABLE kollvar AS INTEGER NO-UNDO.
   DEFINE VARIABLE rkollvar AS INTEGER NO-UNDO.
   IF INDEX(sokstringorg,"&&") = 0  THEN RETURN.
   IF SUBSTRING(sokstringorg,1,1) = "*" THEN  forsta = "*".
   IF SUBSTRING(sokstringorg,LENGTH(sokstringorg),1) = "*" THEN  sista = "*".
   sokstringorg = REPLACE(sokstringorg,"*","").
   rkollvar = 1.
   kollvar = 1.
   REPEAT: 
      IF kollvar >= LENGTH(sokstringorg) THEN LEAVE.
      IF rkollvar > 1 THEN DO:
         ASSIGN
         forsta = "*"
         sista = "*".
      END.   
      IF INDEX(sokstringorg,"&&",kollvar) = 0 THEN DO: 
         sokstring[rkollvar] = TRIM(forsta + SUBSTRING(sokstringorg,kollvar) + sista).
         LEAVE.
      END.
      sokstring[rkollvar] = TRIM(forsta + SUBSTRING(sokstringorg,kollvar,INDEX(sokstringorg,"&&",kollvar) - kollvar) + sista).
      kollvar = INDEX(sokstringorg,"&&",kollvar) + 2.
      rkollvar = rkollvar + 1.
      IF rkollvar > 10 THEN LEAVE.
   END.  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE starwars_UI WINDOW-1 
PROCEDURE starwars_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT  PARAMETER stringkoll AS CHARACTER NO-UNDO.
   IF INDEX(stringkoll,"*",1) = 0 THEN DO:
      begvar = TRUE.
      RETURN.
   END.
   IF SUBSTRING(stringkoll,1,1) = "*" THEN DO:
      begvar = FALSE.
      RETURN.      
   END.  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

