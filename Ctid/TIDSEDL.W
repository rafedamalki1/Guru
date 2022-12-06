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

/* Local Variable Definitions ---                                       */
{ALLDEF.I}
&Scoped-define NEW NEW
{TIDSEDPERS.I}
&Scoped-define NEW
&Scoped-define SHARED SHARED
{TIDPERS.I}
{GODTEMP.I}
{GLOBVAR2DEL1.I}
{REGVAR.I}
{SOKDEF.I}

DEFINE NEW SHARED VARIABLE perssokrec AS RECID NO-UNDO.
DEFINE NEW SHARED VARIABLE fnytid AS DECIMAL FORMAT "-99.99" NO-UNDO.
DEFINE NEW SHARED VARIABLE tillrec AS RECID NO-UNDO.
DEFINE NEW SHARED VARIABLE RAD_ALLTID AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "", 1
     SIZE 79.38 BY 1
     BGCOLOR 8  NO-UNDO.
DEFINE SHARED VARIABLE globanvextra AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE manvaxla AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE vaxla AS LOGICAL NO-UNDO.   
DEFINE SHARED VARIABLE tidsedrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE tidsedlog AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE tidtabrec AS RECID NO-UNDO.   
DEFINE SHARED VARIABLE persrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE persrec2 AS RECID NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE dirtid AS LOGICAL NO-UNDO. 
DEFINE SHARED VARIABLE dirtidoveraknare AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE dirtidrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE korda AS INTEGER NO-UNDO.
DEFINE VARIABLE str AS CHARACTER FORMAT "X(140)" NO-UNDO.
DEFINE VARIABLE knappvar AS LOGICAL NO-UNDO.
DEFINE VARIABLE regdatumspar AS DATE NO-UNDO.
DEFINE VARIABLE spregdatum AS DATE NO-UNDO.
DEFINE VARIABLE regvnrspar AS INTEGER FORMAT "999" NO-UNDO.
DEFINE VARIABLE huvud AS LOGICAL NO-UNDO.
DEFINE VARIABLE startdatum AS DATE NO-UNDO.
DEFINE VARIABLE slutdatum AS DATE NO-UNDO. 
DEFINE VARIABLE hjvnr AS INTEGER FORMAT "999" NO-UNDO.   
DEFINE VARIABLE tillochmedklar AS LOGICAL NO-UNDO.
DEFINE VARIABLE dirtiddatum AS CHARACTER NO-UNDO.
DEFINE VARIABLE dirtidstart AS CHARACTER NO-UNDO.
DEFINE VARIABLE dirtidslut AS CHARACTER NO-UNDO.
DEFINE VARIABLE dirtidlon AS CHARACTER NO-UNDO.
DEFINE VARIABLE dirtidtra AS CHARACTER NO-UNDO.
DEFINE VARIABLE dirtidber AS CHARACTER NO-UNDO.
DEFINE VARIABLE dirtidover AS CHARACTER NO-UNDO.
DEFINE VARIABLE vidirtidlon AS CHARACTER NO-UNDO.
DEFINE VARIABLE vidirtidtra AS CHARACTER NO-UNDO.
DEFINE VARIABLE vidirtidber AS CHARACTER NO-UNDO.
DEFINE VARIABLE vidirtidover AS CHARACTER NO-UNDO.
DEFINE VARIABLE sparavdatum AS DATE NO-UNDO.
DEFINE VARIABLE ingsal AS DECIMAL FORMAT "-99.99" NO-UNDO.
DEFINE VARIABLE seku AS INTEGER FORMAT "-9999999" NO-UNDO.
DEFINE VARIABLE sign AS LOGICAL NO-UNDO.
DEFINE VARIABLE hjkorda AS INTEGER NO-UNDO.
DEFINE VARIABLE fpaminn AS LOGICAL NO-UNDO.
DEFINE VARIABLE excellista AS INTEGER NO-UNDO.
DEFINE VARIABLE entrepejtid AS LOGICAL NO-UNDO.
DEFINE VARIABLE otbeordapph AS HANDLE NO-UNDO.
DEFINE VARIABLE lasanv AS CHARACTER NO-UNDO.
DEFINE VARIABLE persapph AS HANDLE NO-UNDO.
DEFINE VARIABLE ptraff AS LOGICAL NO-UNDO.
DEFINE TEMP-TABLE invartemp   
   FIELD GA AS CHARACTER 
   FIELD GM AS LOGICAL 
   FIELD SK AS LOGICAL 
   FIELD TI AS RECID 
   FIELD PER AS RECID 
   FIELD PER2 AS RECID 
   FIELD MU AS LOGICAL    
   FIELD REGST AS DECIMAL  
   FIELD REGSU AS DECIMAL 
   FIELD RV AS INTEGER FORMAT "999" 
   FIELD RDAG AS CHARACTER FORMAT "X(3)"         
   FIELD RD AS DATE 
   FIELD RM AS INTEGER FORMAT "99" 
   FIELD RMN AS CHARACTER  
   FIELD REGA AS INTEGER FORMAT "99" 
   FIELD RT AS DECIMAL
   FIELD BD AS DATE 
   FIELD AD AS DATE 
   FIELD NY AS DECIMAL 
   FIELD SEK AS INTEGER FORMAT "-9999999" 
   FIELD RSEK AS INTEGER 
   FIELD REGS AS INTEGER 
   FIELD GL AS CHARACTER.   

DEFINE TEMP-TABLE tidpers2 LIKE tidpers.       

{TIDUTTTNEW.I}  
   
DEFINE NEW SHARED TEMP-TABLE tidut2
   FIELD UT AS CHARACTER FORMAT "X(132)".  
DEFINE TEMP-TABLE tidut3
   FIELD UT AS CHARACTER FORMAT "X(132)".
DEFINE TEMP-TABLE kollut LIKE tidut.      
    
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE RAD_AND AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "ÄNDRING TID", 1,
"ÄNDRING LÖN", 2,
"ÄNDRING BEREDSKAP", 3,
"ÄNDRING TRAKT", 4,
"ÄNDRING ÖVERTIDSTILL", 5
     SIZE 24.25 BY 4.36 BGCOLOR 8 NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-TIDS
&Scoped-define BROWSE-NAME BRW_UT

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tidut tidut2

/* Definitions for BROWSE BRW_UT                                        */
&Scoped-define FIELDS-IN-QUERY-BRW_UT tidut.ut 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_UT 
&Scoped-define QUERY-STRING-BRW_UT FOR EACH tidut NO-LOCK
&Scoped-define OPEN-QUERY-BRW_UT OPEN QUERY BRW_UT FOR EACH tidut NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_UT tidut
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_UT tidut


/* Definitions for BROWSE BRW_UT2                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_UT2 tidut2.ut 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_UT2 
&Scoped-define QUERY-STRING-BRW_UT2 FOR EACH tidut2 NO-LOCK
&Scoped-define OPEN-QUERY-BRW_UT2 OPEN QUERY BRW_UT2 FOR EACH tidut2 NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_UT2 tidut2
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_UT2 tidut2


/* Definitions for FRAME FRAME-TIDS                                     */
&Scoped-define OPEN-BROWSERS-IN-QUERY-FRAME-TIDS ~
    ~{&OPEN-QUERY-BRW_UT}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS TOG_HEL BTN_EXCEL BTN_NAAVB BTN_VTID ~
BTN_FGOD BTN_KONTROLL BTN_SKRIV BTN_AVB 
&Scoped-Define DISPLAYED-OBJECTS TOG_HEL 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR WINDOW-2 AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE MENU POPUP-MENU-BRW_UT 
       MENU-ITEM m_Klar_text_till_koder LABEL "Klartext till koder"
       MENU-ITEM m_ndring_av_tidsedel LABEL "Ändring av tidsedel".

DEFINE MENU POPUP-MENU-BRW_UT2 
       MENU-ITEM m_Klart_text_till_koder2 LABEL "Klartext till koder"
       MENU-ITEM m_ndring_av_tidsedel2 LABEL "Ändring av tidsedel".


/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AND 
     LABEL "Ändra" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_AVB AUTO-END-KEY 
     LABEL "Avsluta":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_EXCEL 
     LABEL "Till Excel":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_FGOD 
     LABEL "Färdig tids" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_HTML 
     LABEL "Visa i IE":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_KONTROLL 
     LABEL "Kontroll" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_NAAVB 
     LABEL "Nästa":L 
     SIZE 14 BY 1
     FONT 4.

DEFINE BUTTON BTN_SKRIV 
     LABEL "Skriv ut":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_VTID 
     LABEL "Välj tids." 
     SIZE 14 BY 1.

DEFINE {&NEW} SHARED VARIABLE RAD_TIDSVAL AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Alla tidsedlar för perioden", 1,
"Godkända", 2,
"Ej godkända", 3
     SIZE 31.5 BY 2
     BGCOLOR 8  NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE TOGGLE-MONTH AS LOGICAL INITIAL no 
     LABEL "Två tidsedlar vid månadasskifte":L 
     VIEW-AS TOGGLE-BOX
     SIZE 34.5 BY 1 NO-UNDO.

DEFINE VARIABLE TOG_HEL AS LOGICAL INITIAL no 
     LABEL "Hela tids." 
     VIEW-AS TOGGLE-BOX
     SIZE 8.5 BY .63
     FONT 4 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_UT FOR 
      tidut SCROLLING.

DEFINE QUERY BRW_UT2 FOR 
      tidut2 SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_UT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_UT WINDOW-2 _STRUCTURED
  QUERY BRW_UT NO-LOCK DISPLAY
      tidut.ut FORMAT "X(132)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-LABELS NO-COLUMN-SCROLLING SIZE 123.88 BY 25.

DEFINE BROWSE BRW_UT2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_UT2 WINDOW-2 _STRUCTURED
  QUERY BRW_UT2 NO-LOCK DISPLAY
      tidut2.ut FORMAT "X(132)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-LABELS NO-COLUMN-SCROLLING SIZE 123.88 BY 25.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-TIDS
     BRW_UT AT ROW 1.5 COL 1.5
     BRW_UT2 AT ROW 1.5 COL 1.5
     RAD_TIDSVAL AT ROW 18.5 COL 53.63 NO-LABEL
     TOGGLE-MONTH AT ROW 19 COL 11.13
     TOG_HEL AT ROW 27.13 COL 1
     BTN_EXCEL AT ROW 28 COL 1 WIDGET-ID 2
     BTN_NAAVB AT ROW 28 COL 6.38
     BTN_VTID AT ROW 28 COL 21.38
     BTN_AND AT ROW 28 COL 36.38
     BTN_FGOD AT ROW 28 COL 51.38
     BTN_KONTROLL AT ROW 28 COL 66.38
     BTN_SKRIV AT ROW 28 COL 81.38
     BTN_HTML AT ROW 28 COL 96.38
     BTN_AVB AT ROW 28 COL 111
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 125 BY 28.42.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: 
   Temp-Tables and Buffers:
      TABLE: ? T "?" NO-UNDO temp-db tidut
      TABLE: ? T "?" NO-UNDO temp-db tidut2
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW WINDOW-2 ASSIGN
         HIDDEN             = YES
         TITLE              = "Tidsedel"
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
/* SETTINGS FOR FRAME FRAME-TIDS
   FRAME-NAME                                                           */
/* BROWSE-TAB BRW_UT 1 FRAME-TIDS */
/* BROWSE-TAB BRW_UT2 BRW_UT FRAME-TIDS */
/* SETTINGS FOR BROWSE BRW_UT IN FRAME FRAME-TIDS
   NO-ENABLE                                                            */
ASSIGN 
       BRW_UT:HIDDEN  IN FRAME FRAME-TIDS                = TRUE
       BRW_UT:POPUP-MENU IN FRAME FRAME-TIDS             = MENU POPUP-MENU-BRW_UT:HANDLE.

/* SETTINGS FOR BROWSE BRW_UT2 IN FRAME FRAME-TIDS
   NO-ENABLE                                                            */
ASSIGN 
       BRW_UT2:HIDDEN  IN FRAME FRAME-TIDS                = TRUE
       BRW_UT2:POPUP-MENU IN FRAME FRAME-TIDS             = MENU POPUP-MENU-BRW_UT2:HANDLE.

/* SETTINGS FOR BUTTON BTN_AND IN FRAME FRAME-TIDS
   NO-ENABLE                                                            */
ASSIGN 
       BTN_AND:HIDDEN IN FRAME FRAME-TIDS           = TRUE.

/* SETTINGS FOR BUTTON BTN_HTML IN FRAME FRAME-TIDS
   NO-ENABLE                                                            */
ASSIGN 
       BTN_HTML:HIDDEN IN FRAME FRAME-TIDS           = TRUE.

/* SETTINGS FOR RADIO-SET RAD_TIDSVAL IN FRAME FRAME-TIDS
   NO-DISPLAY SHARED NO-ENABLE                                          */
ASSIGN 
       RAD_TIDSVAL:HIDDEN IN FRAME FRAME-TIDS           = TRUE.

/* SETTINGS FOR TOGGLE-BOX TOGGLE-MONTH IN FRAME FRAME-TIDS
   NO-DISPLAY SHARED NO-ENABLE                                          */
ASSIGN 
       TOGGLE-MONTH:HIDDEN IN FRAME FRAME-TIDS           = TRUE.

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

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_UT2
/* Query rebuild information for BROWSE BRW_UT2
     _TblList          = "Temp-Tables.tidut2"
     _Options          = "NO-LOCK"
     _FldNameList[1]   = Temp-Tables.tidut2.ut
     _Query            is NOT OPENED
*/  /* BROWSE BRW_UT2 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-TIDS
/* Query rebuild information for FRAME FRAME-TIDS
     _Options          = "NO-LOCK "
     _Query            is NOT OPENED
*/  /* FRAME FRAME-TIDS */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME BRW_UT
&Scoped-define SELF-NAME BRW_UT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_UT WINDOW-2
ON MOUSE-MENU-CLICK OF BRW_UT IN FRAME FRAME-TIDS
DO:
   {muswait.i}    
   status-ok = BRW_UT:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.   
   ASSIGN
   vidirtidover = SUBSTRING(tidut.ut,57,4) 
   vidirtidlon = SUBSTRING(tidut.ut,74,5)
   vidirtidtra = SUBSTRING(tidut.ut,91,4)
   vidirtidber = SUBSTRING(tidut.ut,105,4).              
   {SOKSTART.I}
   ASSIGN
   soktemp.SOKVAL = 20
   soktemp.SOKCHAR[1] = tidpers.PERSONALKOD
   soktemp.SOKCHAR[2] = vidirtidlon
   soktemp.SOKCHAR[3] = vidirtidber
   soktemp.SOKCHAR[4] = vidirtidtra 
   soktemp.SOKCHAR[5] = vidirtidover.    
   {SOKANROP.I}   
   RUN KODTEXT.W (INPUT tidpers.PERSONALKOD,INPUT soktemp.SOKINT[1],INPUT soktemp.SOKCHAR[1]).
   {musarrow.i} 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_UT WINDOW-2
ON MOUSE-SELECT-DBLCLICK OF BRW_UT IN FRAME FRAME-TIDS
DO:     
   {SOKSTART.I}   
   ASSIGN
   soktemp.SOKVAL = 25
   soktemp.SOKCHAR[1] = tidpers.PERSONALKOD
   soktemp.SOKCHAR[2] = Guru.Konstanter:globanv
   soktemp.SOKINT[1] = Guru.Konstanter:globniv.
   {SOKANROP.I}
   IF soktemp.SOKLOG[1] = TRUE THEN DO:      
      musz = musz.
   END.
   ELSE DO:
      IF vaxla = FALSE THEN DO:  
         IF tillochmeddatum NE ? THEN DO:
            IF tillochmedklar = TRUE THEN musz = musz.
            ELSE RUN andra_UI. 
         END.
         ELSE RUN andra_UI.   
      END.
   END.      
   RUN sattmus_UI.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_UT2
&Scoped-define SELF-NAME BRW_UT2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_UT2 WINDOW-2
ON MOUSE-MENU-CLICK OF BRW_UT2 IN FRAME FRAME-TIDS
DO:
   {muswait.i} 
   status-ok = BRW_UT2:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.   
   ASSIGN
   vidirtidover = SUBSTRING(tidut2.ut,57,4) 
   vidirtidlon = SUBSTRING(tidut2.ut,74,5)      
   vidirtidtra = SUBSTRING(tidut2.ut,91,4)
   vidirtidber = SUBSTRING(tidut2.ut,105,4).   
   {SOKSTART.I}
   ASSIGN
   soktemp.SOKVAL = 20
   soktemp.SOKCHAR[1] = tidpers.PERSONALKOD
   soktemp.SOKCHAR[2] = vidirtidlon
   soktemp.SOKCHAR[3] = vidirtidber
   soktemp.SOKCHAR[4] = vidirtidtra 
   soktemp.SOKCHAR[5] = vidirtidover. 
   {SOKANROP.I}      
   RUN KODTEXT.W (INPUT tidpers.PERSONALKOD,INPUT soktemp.SOKINT[1],INPUT soktemp.SOKCHAR[1]).
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_UT2 WINDOW-2
ON MOUSE-SELECT-DBLCLICK OF BRW_UT2 IN FRAME FRAME-TIDS
DO:
   {SOKSTART.I}
   ASSIGN
   soktemp.SOKVAL = 25
   soktemp.SOKCHAR[1] = tidpers.PERSONALKOD
   soktemp.SOKCHAR[2] = Guru.Konstanter:globanv
   soktemp.SOKINT[1] = Guru.Konstanter:globniv.
   {SOKANROP.I}
   IF soktemp.SOKLOG[1] = TRUE THEN DO:      
      musz = musz.
   END.     
   ELSE DO:
      IF vaxla = FALSE THEN DO:  
         IF tillochmeddatum NE ? THEN DO:
            IF tillochmedklar = TRUE THEN musz = musz.
            ELSE RUN andra_UI. 
         END.
         ELSE RUN andra_UI.            
      END.
   END.   
   RUN sattmus_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AND
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AND WINDOW-2
ON CHOOSE OF BTN_AND IN FRAME FRAME-TIDS /* Ändra */
DO:            
   
   RUN andra_UI.       
   RUN sattmus_UI.
   
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB WINDOW-2
ON CHOOSE OF BTN_AVB IN FRAME FRAME-TIDS /* Avsluta */
DO:
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_EXCEL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_EXCEL WINDOW-2
ON CHOOSE OF BTN_EXCEL IN FRAME FRAME-TIDS /* Till Excel */
DO:
   excellista = 28.
   {AMERICANEUROPEAN.I}
   RUN AKIVINEX.P (INPUT excellista,INPUT TABLE tidut).
   {EUROPEANAMERICAN.I}             
   {musarrow.i}    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_FGOD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_FGOD WINDOW-2
ON CHOOSE OF BTN_FGOD IN FRAME FRAME-TIDS /* Färdig tids */
DO:     
   /*forfärdig*/   
   DO:      
      tillochmeddatum = ?.
      {SOKSTART.I}
      ASSIGN
      soktemp.SOKVAL = 26
      soktemp.SOKCHAR[1] = tidpers.PERSONALKOD
      soktemp.SOKINT[1] = YEAR(regdatum)
      soktemp.SOKINT[2] = MONTH(regdatum).
      {SOKANROP.I}           
      IF soktemp.SOKLOG[1] = TRUE AND Guru.Konstanter:tadmsekvar[3] = TRUE THEN RUN fbort_UI.
      /*mittsverige vill att användarna själva ska kunna ta bort sin färdigrapportering 20211118*/
      ELSE IF soktemp.SOKLOG[1] = TRUE AND Guru.Konstanter:globforetag = "MISV"  THEN RUN fbort_UI.
      ELSE IF soktemp.SOKLOG[1] = TRUE THEN musz = musz.
      ELSE DO:      
         /*forfärdig*/
         IF Guru.Konstanter:tadmsekvar[3] = TRUE OR Guru.Konstanter:globforetag = "MISV" THEN DO:
            IF korda = 3 OR BTN_FGOD:LABEL = "Ta bort färdig" THEN DO:
               RUN fbort_UI.
            END.
            ELSE DO:
               MESSAGE "Vill du FÄRDIGRAPPORTERA tidsedel svara JA!" SKIP   
                       "Vill du TA BORT FÄRDIGRAPPORTERING svara NEJ" 
                        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO 
               TITLE "Färdigrapportera"
               UPDATE fvalet1 AS LOGICAL.
               CASE fvalet1:
                  WHEN TRUE THEN RUN fgod_UI.   
                  WHEN FALSE THEN RUN fbort_UI.
               END CASE.                       
            END.
         END.
         ELSE RUN fgod_UI.       
      END.
   END.
   IF vaxla = TRUE THEN DO:  
      ASSIGN
      BTN_NAAVB:HIDDEN = TRUE   
      BTN_AND:HIDDEN = TRUE
      BTN_VTID:HIDDEN =TRUE.
   END.                   
   RUN sattmus_UI.
   RUN fixbtn_UI.   
   APPLY "VALUE-CHANGED" TO TOG_HEL IN FRAME {&FRAME-NAME}.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_HTML
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_HTML WINDOW-2
ON CHOOSE OF BTN_HTML IN FRAME FRAME-TIDS /* Visa i IE */
DO:
   regmnr = MONTH(bdatum).
   RUN MANNAMN.P.
   {muswait.i}
   RUN UTTIDHTM.P (INPUT tidpers.PERSONALKOD,INPUT TABLE tidut).   
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_KONTROLL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_KONTROLL WINDOW-2
ON CHOOSE OF BTN_KONTROLL IN FRAME FRAME-TIDS /* Kontroll */
DO:
   {muswait.i}    
   regdatum = DATE(regmnr,01,regar).    
   bdatum = regdatum.  
   IF MONTH(regdatum) = 12 THEN avdatum = DATE(12,31,YEAR(regdatum)). 
   ELSE avdatum = DATE((MONTH(regdatum) + 1),01,YEAR(regdatum)) - 1.
   IF MONTH(TODAY) = MONTH(avdatum) AND YEAR(TODAY) = YEAR(avdatum)
   THEN DO:      
      IF MONTH(TODAY) = MONTH(TODAY - 1 ) THEN regdatum = TODAY - 1.
      ELSE regdatum = TODAY.
           
      {AVBGOMD.I}
      RUN KTRLDAT.W.
      {AVBFRAMD.I}
      
      IF musz = TRUE THEN DO:
         musz = FALSE.
         RETURN.
      END.                     
      avdatum = regdatum.
   END.        
   RUN KTRLTID.W.
   RUN sattmus_UI.   
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NAAVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NAAVB WINDOW-2
ON CHOOSE OF BTN_NAAVB IN FRAME FRAME-TIDS /* Nästa */
DO:
   {muswait.i}     
   IF NOT AVAILABLE tidpers THEN DO:      
      APPLY "WINDOW-CLOSE":U TO WINDOW-2. 
   END.
   ELSE DO:       
      RUN nytid_UI.    
      IF musz = TRUE THEN DO:
         musz = musz.
      END.   
      ELSE DO:
         RUN apput_UI.
         RUN knappar_UI.
         APPLY "VALUE-CHANGED" TO TOG_HEL IN FRAME {&FRAME-NAME}.             
         {musarrow.i}            
      END.
   END.
   RUN sattmus_UI.
   RUN fixbtn_UI.
   IF musz = TRUE THEN DO:
      musz = FALSE.
      APPLY "WINDOW-CLOSE":U TO WINDOW-2.
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_SKRIV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_SKRIV WINDOW-2
ON CHOOSE OF BTN_SKRIV IN FRAME FRAME-TIDS /* Skriv ut */
DO: 
   regmnr = MONTH(bdatum).
   RUN MANNAMN.P.
   {SOKSTART.I}
      
   ASSIGN
   soktemp.SOKVAL = 92
   soktemp.SOKCHAR[1] = tidpers.PERSONALKOD.
   {SOKANROP.I}
   globanvextra = Guru.Konstanter:globanv.
   IF soktemp.SOKCHAR[2] NE "" THEN globanvextra = soktemp.SOKCHAR[2].
   RUN SKRIVVAL.W (INPUT TRUE).
   globanvextra = "".
   {muswait.i}       
   IF musz = TRUE THEN musz = FALSE.
   ELSE DO:
      {PRINTLIGGANDE.I}
      RUN TIDSEDS.P (INPUT tidpers.PERSONALKOD). 
      OUTPUT CLOSE.                            
   END.
   RUN sattmus_UI.
    {musarrow.i}    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_SKRIV WINDOW-2
ON MOUSE-MENU-CLICK OF BTN_SKRIV IN FRAME FRAME-TIDS /* Skriv ut */
DO:
   RUN SIDLANGD.W.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_VTID
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_VTID WINDOW-2
ON CHOOSE OF BTN_VTID IN FRAME FRAME-TIDS /* Välj tids. */
DO:
   RUN VTIDVAL.W.     
   {muswait.i}         
   IF musz = TRUE THEN musz = FALSE.
   ELSE DO:
      FIND vtid WHERE RECID(vtid) = perssokrec NO-LOCK NO-ERROR.      
      FIND FIRST tidpers WHERE tidpers.PERSONALKOD = vtid.PERSONALKOD 
      USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.
      ASSIGN                       
      bdatum = DATE(vtid.MANNR,01,VTID.AR)
      persrec = tidpers.TIDPERSREC.                  
      RUN avvecka_UI.    
      regdatum = bdatum. 
      musz = TRUE.                   
      RUN nytid_UI.       
      regdatum = bdatum.
      RUN apput_UI.              
      RUN knappar_UI.
      APPLY "VALUE-CHANGED" TO TOG_HEL IN FRAME {&FRAME-NAME}.                        
   END.                                  
   {musarrow.i}   
   musz = FALSE.  
   RUN sattmus_UI.
   RUN fixbtn_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Klart_text_till_koder2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Klart_text_till_koder2 WINDOW-2
ON CHOOSE OF MENU-ITEM m_Klart_text_till_koder2 /* Klartext till koder */
DO:
   {muswait.i} 
   status-ok = BRW_UT2:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.   
   ASSIGN
   vidirtidover = SUBSTRING(tidut2.ut,57,4) 
   vidirtidlon = SUBSTRING(tidut2.ut,74,5)      
   vidirtidtra = SUBSTRING(tidut2.ut,91,4)
   vidirtidber = SUBSTRING(tidut2.ut,105,4).   
   {SOKSTART.I}
   ASSIGN
   soktemp.SOKVAL = 20
   soktemp.SOKCHAR[1] = tidpers.PERSONALKOD
   soktemp.SOKCHAR[2] = vidirtidlon
   soktemp.SOKCHAR[3] = vidirtidber
   soktemp.SOKCHAR[4] = vidirtidtra 
   soktemp.SOKCHAR[5] = vidirtidover. 
   {SOKANROP.I}   
   RUN KODTEXT.W (INPUT tidpers.PERSONALKOD,INPUT soktemp.SOKINT[1],INPUT soktemp.SOKCHAR[1]).
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Klar_text_till_koder
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Klar_text_till_koder WINDOW-2
ON CHOOSE OF MENU-ITEM m_Klar_text_till_koder /* Klartext till koder */
DO:
   {muswait.i}    
   status-ok = BRW_UT:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.   
   ASSIGN
   vidirtidover = SUBSTRING(tidut.ut,57,4) 
   vidirtidlon = SUBSTRING(tidut.ut,74,5)
   vidirtidtra = SUBSTRING(tidut.ut,91,4)
   vidirtidber = SUBSTRING(tidut.ut,105,4).              
   {SOKSTART.I}
   ASSIGN
   soktemp.SOKVAL = 20
   soktemp.SOKCHAR[1] = tidpers.PERSONALKOD
   soktemp.SOKCHAR[2] = vidirtidlon
   soktemp.SOKCHAR[3] = vidirtidber
   soktemp.SOKCHAR[4] = vidirtidtra 
   soktemp.SOKCHAR[5] = vidirtidover.    
   {SOKANROP.I}   
   RUN KODTEXT.W (INPUT tidpers.PERSONALKOD,INPUT soktemp.SOKINT[1],INPUT soktemp.SOKCHAR[1]).
   {musarrow.i} 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_ndring_av_tidsedel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_ndring_av_tidsedel WINDOW-2
ON CHOOSE OF MENU-ITEM m_ndring_av_tidsedel /* Ändring av tidsedel */
DO:
   {SOKSTART.I}
   ASSIGN
   soktemp.SOKVAL = 25
   soktemp.SOKCHAR[1] = tidpers.PERSONALKOD
   soktemp.SOKCHAR[2] = Guru.Konstanter:globanv
   soktemp.SOKINT[1] = Guru.Konstanter:globniv.
   {SOKANROP.I}
   IF soktemp.SOKLOG[1] = TRUE THEN DO:      
      musz = musz.
   END.     
   ELSE DO:
      IF vaxla = FALSE THEN DO:  
         IF tillochmeddatum NE ? THEN DO:
            IF tillochmedklar = TRUE THEN musz = musz.
            ELSE RUN andra_UI. 
         END.
         ELSE RUN andra_UI.            
      END.
   END.
   RUN sattmus_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_ndring_av_tidsedel2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_ndring_av_tidsedel2 WINDOW-2
ON CHOOSE OF MENU-ITEM m_ndring_av_tidsedel2 /* Ändring av tidsedel */
DO:
   {SOKSTART.I}
   ASSIGN
   soktemp.SOKVAL = 25
   soktemp.SOKCHAR[1] = tidpers.PERSONALKOD
   soktemp.SOKCHAR[2] = Guru.Konstanter:globanv
   soktemp.SOKINT[1] = Guru.Konstanter:globniv.
   {SOKANROP.I}
   IF soktemp.SOKLOG[1] = TRUE THEN DO:      
      musz = musz.
   END.
   ELSE DO:
      IF vaxla = FALSE THEN DO:  
         IF tillochmeddatum NE ? THEN DO:
            IF tillochmedklar = TRUE THEN musz = musz.
            ELSE RUN andra_UI. 
         END.
         ELSE RUN andra_UI.   
      END.
   END.      
   RUN sattmus_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_HEL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_HEL WINDOW-2
ON VALUE-CHANGED OF TOG_HEL IN FRAME FRAME-TIDS /* Hela tids. */
DO:
   {muswait.i}
   TOG_HEL = INPUT TOG_HEL.
   IF TOG_HEL = TRUE THEN DO:
      BRW_UT:HIDDEN = TRUE.
      BRW_UT2:HIDDEN = FALSE.
      EMPTY TEMP-TABLE tidut2 NO-ERROR.       
      FOR EACH tidut:
         CREATE tidut2. 
         ASSIGN
         tidut2.UT = tidut.UT.
      END.   
      OPEN QUERY BRW_UT2 FOR EACH tidut2 NO-LOCK. 
   END.
   ELSE DO:
      BRW_UT:HIDDEN = FALSE.
      BRW_UT2:HIDDEN = TRUE.
      OPEN QUERY BRW_UT FOR EACH tidut NO-LOCK. 
   END.
   {musarrow.i}      
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
ON CLOSE OF THIS-PROCEDURE DO: 
   {BORTBRWPROC.I}        
   IF VALID-HANDLE(otbeordapph) THEN DELETE PROCEDURE otbeordapph NO-ERROR.
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
   {ALLSTARTDYN.I}
   ASSIGN
   BRW_UT:FONT = 24
   BRW_UT2:FONT = 20.              

   {muswait.i} 
   
   hjkorda = ?.
   IF korda = 4 THEN DO:
      ASSIGN hjkorda = 4 korda = 3.
   END.
   /*forfärdig*/
   IF Guru.Konstanter:tadmsekvar[3] = TRUE OR Guru.Konstanter:globforetag = "MISV"   THEN DO:
      BTN_FGOD:LABEL = "   Färdig/Ta bort färdig".
   END.
   IF vaxla = FALSE THEN DO:                    
      FIND FIRST tidpers WHERE tidpers.TIDPERSREC = persrec
      USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.
      IF NOT AVAILABLE tidpers THEN FIND FIRST tidpers USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.      
   END.
   ELSE DO:
      FIND FIRST tidpers WHERE tidpers.TIDPERSREC = persrec
      USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.
   END.
   IF NOT AVAILABLE tidpers THEN DO:
      MESSAGE "Det finns ingen tid registrerad under denna period." VIEW-AS ALERT-BOX.
      LEAVE MAIN-BLOCK.
   END.        
   IF skrivut = FALSE THEN DO:        
      IF RAD_TIDSVAL = 2  AND (korda = 2 OR korda = 3 ) THEN DO:
         sparavdatum = avdatum.
         IF korda = 2 THEN ASSIGN avdatum = avdatum.
         ELSE IF korda = 3 THEN ASSIGN avdatum = TODAY.            
         IF hjkorda = 4 THEN DO:         
            avdatum = sparavdatum.
            FOR EACH tidpers USE-INDEX PERSONALKOD NO-LOCK.
               
               {SOKSTART.I}
               ASSIGN
               soktemp.SOKVAL = 23
               soktemp.SOKINT[1] = korda
               soktemp.SOKCHAR[1] = tidpers.PERSONALKOD
               soktemp.SOKDATE[1] = regdatum.
               {SOKANROP.I}
               
               IF soktemp.SOKDATE[2] NE ? THEN DO:                     
                  regmnr = MONTH(regdatum).
                  RUN MANNAMN.P.           
                  CREATE vtid.      
                  ASSIGN
                  vtid.EFTERNAMN = tidpers.EFTERNAMN 
                  vtid.FORNAMN = tidpers.FORNAMN 
                  vtid.PERSONALKOD = tidpers.PERSONALKOD 
                  vtid.MANNR = MONTH(regdatum)
                  vtid.AR = YEAR(regdatum)
                  vtid.MANAD = regmannamn.
               END.                                                            
            END.
         END.
         ELSE DO:                     
            FOR EACH tidpers USE-INDEX PERSONALKOD NO-LOCK.
               {SOKSTART.I}
               ASSIGN
               soktemp.SOKVAL = 22
               soktemp.SOKINT[1] = korda
               soktemp.SOKCHAR[1] = tidpers.PERSONALKOD
               soktemp.SOKDATE[1] = avdatum.
               {SOKANROP.I}                              
               IF soktemp.SOKDATE[2] NE ? THEN DO:                  
                  regdatum = soktemp.SOKDATE[2].
                  regmnr = MONTH(regdatum).
                  RUN MANNAMN.P.           
                  CREATE vtid.      
                  ASSIGN
                  vtid.EFTERNAMN = tidpers.EFTERNAMN 
                  vtid.FORNAMN = tidpers.FORNAMN 
                  vtid.PERSONALKOD = tidpers.PERSONALKOD 
                  vtid.MANNR = MONTH(regdatum)
                  vtid.AR = YEAR(regdatum)
                  vtid.MANAD = regmannamn.
                  skapvtid:
                  REPEAT TRANSACTION:                  
                     IF MONTH(regdatum) = 12 THEN regdatum = DATE(01,01,YEAR(regdatum) + 1). 
                     ELSE regdatum = DATE(MONTH(regdatum) + 1,01,YEAR(regdatum)).                         
                     IF regdatum > avdatum THEN LEAVE skapvtid.
                     {SOKSTART.I}
                     ASSIGN
                     soktemp.SOKVAL = 23
                     soktemp.SOKCHAR[1] = tidpers.PERSONALKOD
                     soktemp.SOKDATE[1] = regdatum.
                     {SOKANROP.I}
                     
                     IF soktemp.SOKDATE[2] NE ? THEN DO:                     
                        regmnr = MONTH(regdatum).
                        RUN MANNAMN.P.           
                        CREATE vtid.      
                        ASSIGN
                        vtid.EFTERNAMN = tidpers.EFTERNAMN 
                        vtid.FORNAMN = tidpers.FORNAMN 
                        vtid.PERSONALKOD = tidpers.PERSONALKOD 
                        vtid.MANNR = MONTH(regdatum)
                        vtid.AR = YEAR(regdatum)
                        vtid.MANAD = regmannamn.
                     END.                                                            
                  END.
               END.
            END.               
            avdatum = sparavdatum.
         END.
      END.
      ELSE DO:
         hjvnr = regvnr.
         FOR EACH tidpers USE-INDEX PERSONALKOD NO-LOCK:
            ASSIGN            
            regdatum = bdatum.
            REPEAT TRANSACTION:
               regmnr = MONTH(regdatum).
               RUN MANNAMN.P.           
               CREATE vtid.      
               ASSIGN
               vtid.EFTERNAMN = tidpers.EFTERNAMN 
               vtid.FORNAMN = tidpers.FORNAMN 
               vtid.PERSONALKOD = tidpers.PERSONALKOD 
               vtid.MANNR = MONTH(regdatum)
               vtid.AR = YEAR(regdatum)
               vtid.MANAD = regmannamn.
               IF MONTH(regdatum) = 12 THEN regdatum = DATE(01,01,YEAR(regdatum) + 1). 
               ELSE regdatum = DATE(MONTH(regdatum) + 1,01,YEAR(regdatum)).                         
               IF regdatum > avdatum THEN LEAVE.                        
            END.
         END.   
      END.                   
      ASSIGN
      regdatum = bdatum
      regvnr = hjvnr.
      IF vaxla = FALSE THEN DO:                    
         FIND FIRST tidpers WHERE tidpers.TIDPERSREC = persrec
         USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.
         IF NOT AVAILABLE tidpers THEN FIND FIRST tidpers USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.         
      END.
      ELSE DO:
         FIND FIRST tidpers WHERE tidpers.TIDPERSREC = persrec
         USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.
      END.   
   END.  
   ASSIGN
   persrec = tidpers.TIDPERSREC
   tidtabrec = 0   
   startdatum = bdatum
   slutdatum = avdatum
   regdatum = bdatum.     
   IF RAD_TIDSVAL = 2  AND (korda = 2 OR korda = 3) THEN DO:   
      IF hjkorda = 4 THEN DO:
         {SOKSTART.I}
         ASSIGN
         soktemp.SOKVAL = 23
         soktemp.SOKINT[1] = korda
         soktemp.SOKCHAR[1] = tidpers.PERSONALKOD
         soktemp.SOKDATE[1] = regdatum.
         {SOKANROP.I}               
         IF soktemp.SOKDATE[2] NE ? THEN DO:
            ASSIGN 
            regdatum = soktemp.SOKDATE[2]
            bdatum = soktemp.SOKDATE[2].
            RUN REGVEC.P.           
         END.   
         ELSE DO:
            ASSIGN 
            regdatum = DATE(MONTH(slutdatum),01,YEAR(slutdatum)).
            bdatum = DATE(MONTH(slutdatum),01,YEAR(slutdatum)).
            RUN REGVEC.P.
         END. 
      END.
      ELSE DO:      
         {SOKSTART.I}
         ASSIGN
         soktemp.SOKVAL = 22
         soktemp.SOKINT[1] = korda
         soktemp.SOKCHAR[1] = tidpers.PERSONALKOD
         soktemp.SOKDATE[1] = avdatum.
         {SOKANROP.I}               
         IF soktemp.SOKDATE[2] NE ? THEN DO:
            ASSIGN 
            regdatum = soktemp.SOKDATE[2]
            bdatum = soktemp.SOKDATE[2].
            RUN REGVEC.P.           
         END.   
         ELSE DO:
            ASSIGN 
            regdatum = DATE(MONTH(slutdatum),01,YEAR(slutdatum)).
            bdatum = DATE(MONTH(slutdatum),01,YEAR(slutdatum)).
            RUN REGVEC.P.
         END. 
      END.
   END.
   RUN avvecka_UI.
   ASSIGN
   regdatum = bdatum  
   musz = TRUE
   huvud = TRUE.   
   RUN nytid_UI.    /*FÖRSTA GUBBE*/   
   ASSIGN
   regdatum = bdatum   
   huvud = TRUE.    
   musz = FALSE.      
   IF NOT AVAILABLE tidpers THEN DO:
      LEAVE MAIN-BLOCK.
   END.
   RUN apput_UI.              
   IF skrivut = FALSE THEN DO:      
      ENABLE BRW_UT WITH FRAME {&FRAME-NAME}.
      ENABLE BRW_UT2 WITH FRAME {&FRAME-NAME}.       
      BRW_UT:HIDDEN = FALSE.
      BRW_UT2:HIDDEN = TRUE. 
      OPEN QUERY BRW_UT FOR EACH tidut NO-LOCK.
      IF Guru.Konstanter:globforetag = "Celpa" THEN DO: 
         TOG_HEL = TRUE.
         BRW_UT:HIDDEN = TRUE.
         BRW_UT2:HIDDEN = FALSE.
         EMPTY TEMP-TABLE tidut2 NO-ERROR. 
         FOR EACH tidut:
            CREATE tidut2. 
            ASSIGN
            tidut2.UT = tidut.UT.
         END.   
         OPEN QUERY BRW_UT2 FOR EACH tidut2 NO-LOCK. 
      END.
   END.
   ELSE DO:
      REPEAT:
         RUN TIDSEDS.P (INPUT tidpers.PERSONALKOD). 
         RUN skrivut_UI.
         IF musz = TRUE THEN DO:
            musz = FALSE.
            LEAVE.
         END.       
      END.
      /*status-mus2 = CURRENT-WINDOW:LOAD-MOUSE-POINTER("ARROW").*/
      status-mus2 = SESSION:SET-WAIT-STATE("").
      APPLY "WINDOW-CLOSE" TO {&WINDOW-NAME}.
      LEAVE MAIN-BLOCK.      
   END.    
   RUN enable_UI.   
   RUN knappar_UI.
   
   {FRMSIZE.I}  
   /*forfärdig*/        
   tillochmeddatum = ?.
   {SOKSTART.I}
   ASSIGN
   soktemp.SOKVAL = 26
   soktemp.SOKCHAR[1] = tidpers.PERSONALKOD
   soktemp.SOKINT[1] = YEAR(regdatum)
   soktemp.SOKINT[2] = MONTH(regdatum).
   {SOKANROP.I}           
   IF soktemp.SOKLOG[1] = TRUE THEN DO: 
      /*redan godkänd*/
      ASSIGN BTN_FGOD:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
      /*forfärdig*/      
      BTN_FGOD:LABEL = "Ta bort färdig".                     
   END.   
   /*f- på tidregitaben*/
   IF soktemp.SOKLOG[3] = FALSE THEN DO:       
      IF soktemp.SOKLOG[1] = TRUE AND Guru.Konstanter:globforetag = "MISV" THEN DO:
         ASSIGN BTN_FGOD:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.             
      END.              
      ELSE IF soktemp.SOKLOG[1] = TRUE AND Guru.Konstanter:tadmsekvar[3] = FALSE THEN DO:
         ASSIGN BTN_FGOD:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.             
      END.      
      ELSE DO:          
         BTN_FGOD:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.                            
      END.
   END.
   ELSE DO:
       ASSIGN BTN_FGOD:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.             
   END.
   IF soktemp.SOKLOG[2] = TRUE THEN DO:         
      ASSIGN
      tillochmedklar = soktemp.SOKLOG[1]
      tillochmeddatum = soktemp.SOKDATE[1].
   END.
   IF vaxla = TRUE THEN DO:  
      ASSIGN
      BTN_NAAVB:HIDDEN = TRUE   
      BTN_AND:HIDDEN = TRUE
      BTN_VTID:HIDDEN =TRUE.
   END.            
   {musarrow.i}
   IF korda = 3 THEN  BTN_FGOD:LABEL = "Ta bort färdig".   
   IF Guru.Konstanter:varforetypchar[9] = "1" THEN DO:      
      entrepejtid = FALSE.
      RUN kollentrep_UI IN otbeordapph (INPUT tidpers.PERSONALKOD, OUTPUT entrepejtid).
      IF entrepejtid = TRUE THEN DO:
         DISABLE BTN_FGOD BTN_KONTROLL WITH FRAME {&FRAME-NAME}.
      END.   
   END.   
                          
   RUN fixbtn_UI.
   {WIN_M_SLUT.I}
      
   IF vaxla = TRUE THEN DO:
      MENU-ITEM m_ndring_av_tidsedel:SENSITIVE IN MENU POPUP-MENU-BRW_UT = FALSE.
      MENU-ITEM m_ndring_av_tidsedel2:SENSITIVE IN MENU POPUP-MENU-BRW_UT2 = FALSE.
   END.
   IF NOT THIS-PROCEDURE:PERSISTENT THEN
   WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE allstartbrw_UI WINDOW-2 
PROCEDURE allstartbrw_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   IF Guru.Konstanter:appcon THEN DO:                           
      RUN OVERTIDBRDAPP.P PERSISTENT SET otbeordapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT.
      RUN PHMTAPP.P PERSISTENT SET persapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT.
   END.
   ELSE DO:
      RUN OVERTIDBRDAPP.P PERSISTENT SET otbeordapph.
      RUN PHMTAPP.P PERSISTENT SET persapph.
   END. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE andra_UI WINDOW-2 
PROCEDURE andra_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/   
   IF TOG_HEL = FALSE THEN DO: 
      status-ok = BRW_UT:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
      APPLY "VALUE-CHANGED" TO BRW_UT.   
   END.
   ELSE DO:
      status-ok = BRW_UT2:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
      APPLY "VALUE-CHANGED" TO BRW_UT2.
   END.   
   musz = FALSE.
   RUN TIDSAND.W.

   IF musz = TRUE THEN musz = FALSE.
   ELSE DO:              
      ASSIGN 
      vaxla = TRUE        
      regvnrspar = regvnr
      regdatumspar = bdatum    
      tidsedrec = RECID(tidpers) 
      tidsedlog = TRUE.                              
      IF TOG_HEL = FALSE THEN DO:         
         IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" THEN DO:   
            ASSIGN
            dirtid = TRUE
            dirtiddatum = SUBSTRING(tidut.ut,5,2)
            dirtidstart = SUBSTRING(tidut.ut,8,5)
            dirtidslut = SUBSTRING(tidut.ut,14,5)
            vidirtidover = SUBSTRING(tidut.ut,57,4)         
            vidirtidtra = SUBSTRING(tidut.ut,74,5)
            vidirtidber = SUBSTRING(tidut.ut,105,4).                                         
         END.
         ELSE DO:         
            ASSIGN
            dirtid = TRUE
            dirtiddatum = SUBSTRING(tidut.ut,5,2)
            dirtidstart = SUBSTRING(tidut.ut,8,5)
            dirtidslut = SUBSTRING(tidut.ut,14,5)
            vidirtidover = SUBSTRING(tidut.ut,58,4)
            vidirtidlon = SUBSTRING(tidut.ut,76,4)
            vidirtidtra = SUBSTRING(tidut.ut,91,4)
            vidirtidber = SUBSTRING(tidut.ut,115,4).                    
         END.                  
      END.
      ELSE DO:                
         ASSIGN
         dirtid = TRUE
         dirtiddatum = SUBSTRING(tidut2.ut,5,2)
         dirtidstart = SUBSTRING(tidut2.ut,8,5)
         dirtidslut = SUBSTRING(tidut2.ut,14,5)
         vidirtidover = SUBSTRING(tidut2.ut,58,4)
         vidirtidlon = SUBSTRING(tidut2.ut,76,4)
         vidirtidtra = SUBSTRING(tidut2.ut,91,4)
         vidirtidber = SUBSTRING(tidut2.ut,115,4).                    
      END.                 
      {SOKSTART.I}
      ASSIGN
      soktemp.SOKVAL = 27
      soktemp.SOKCHAR[1] = tidpers.PERSONALKOD
      soktemp.SOKCHAR[2] = vidirtidlon
      soktemp.SOKCHAR[3] = vidirtidber
      soktemp.SOKCHAR[4] = vidirtidtra
      soktemp.SOKCHAR[5] = vidirtidover
      soktemp.SOKINT[1] = YEAR(regdatum)
      soktemp.SOKINT[2] = MONTH(regdatum).
      {SOKANROP.I}
      ASSIGN 
      dirtidlon = soktemp.SOKCHAR[2]
      dirtidber = soktemp.SOKCHAR[3]
      dirtidtra = soktemp.SOKCHAR[4]     
      dirtidover = soktemp.SOKCHAR[5].      
      RUN dirtid_UI.      
      RUN ALLAAND.W (INPUT RAD_AND,INPUT tidpers.PERSONALKOD).      
      {muswait.i} 
      regvnr = regvnrspar.         
      ASSIGN   
      vaxla = FALSE.
      bdatum = regdatumspar.
      persrec = tidpers.TIDPERSREC.                  
      RUN avvecka_UI.    
      regdatum = bdatum. 
      musz = TRUE.        
      RUN nytid_UI.       
      regdatum = bdatum.
      RUN apput_UI.              
      
      RUN knappar_UI.
      
      APPLY "VALUE-CHANGED" TO TOG_HEL IN FRAME {&FRAME-NAME}.          
      {AVBFRAM.I}         
      {musarrow.i}  
   END.   
   musz = FALSE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE apput_UI WINDOW-2 
PROCEDURE apput_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/   
   regar = YEAR(regdatum).
   regmnr = MONTH(regdatum).   
   IF Guru.Konstanter:appcon THEN DO: 
      RUN TIDSLAPP.P  ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT tidpers.PERSONALKOD,
      INPUT bdatum,INPUT avdatum,INPUT regdatum,INPUT startdatum,INPUT slutdatum,INPUT RAD_TIDSVAL,INPUT korda,
      OUTPUT knappvar,
      OUTPUT TABLE tidut).
   END.
   ELSE DO:
      RUN TIDSLAPP.P 
      (INPUT tidpers.PERSONALKOD,
      INPUT bdatum,INPUT avdatum,INPUT regdatum,INPUT startdatum,INPUT slutdatum,INPUT RAD_TIDSVAL,INPUT korda,
      OUTPUT knappvar,
      OUTPUT TABLE tidut).
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE avvecka_UI WINDOW-2 
PROCEDURE avvecka_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   IF MONTH(bdatum) = 12 THEN avdatum = DATE(12,31,YEAR(bdatum)). 
   ELSE avdatum = DATE(MONTH(bdatum) + 1,01,YEAR(bdatum)) - 1.         
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE dirtid_UI WINDOW-2 
PROCEDURE dirtid_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/   
   IF dirtiddatum >= "01" AND dirtiddatum <= "31" THEN DO: 
      regdatum = DATE(MONTH(bdatum),INTEGER(dirtiddatum),YEAR(bdatum)).      
   END.
   ELSE DO: 
      dirtid = FALSE.     
      RETURN.   
   END.
   
   IF RAD_AND = 1 THEN DO:                   
      IF DECIMAL(dirtidstart) = ? THEN DO: 
         dirtid = FALSE.     
         RETURN.   
      END.
      IF DECIMAL(dirtidslut) = ? THEN DO: 
         dirtid = FALSE.     
         RETURN.   
      END.      
      {SOKSTART.I}
      ASSIGN
      soktemp.SOKVAL = 28
      soktemp.SOKCHAR[1] = tidpers.PERSONALKOD
      soktemp.SOKDEC[1] = DECIMAL(dirtidstart)
      soktemp.SOKDEC[2] = DECIMAL(dirtidslut)
      soktemp.SOKDATE[1] = regdatum      
      soktemp.SOKINT[1] = 1.
      {SOKANROP.I}
      IF soktemp.SOKINT[2] NE 0 THEN DO:
         dirtidrec = soktemp.SOKINT[2].            
      END.
      ELSE DO:
         dirtid = FALSE.          
         RETURN.   
      END.                
   END.
   ELSE IF RAD_AND = 2 THEN DO:
      IF dirtidlon = "" THEN DO: 
         dirtid = FALSE.     
         RETURN.   
      END.
      {SOKSTART.I}
      ASSIGN
      soktemp.SOKVAL = 28
      soktemp.SOKCHAR[1] = tidpers.PERSONALKOD
      soktemp.SOKCHAR[2] = dirtidlon
      soktemp.SOKDATE[1] = regdatum      
      soktemp.SOKINT[1] = 2.
      {SOKANROP.I}
      IF soktemp.SOKINT[2] NE 0 THEN DO:
         dirtidrec = soktemp.SOKINT[2].            
      END.
      ELSE DO:
         dirtid = FALSE.     
         RETURN.   
      END.
   END.
   ELSE IF RAD_AND = 3 THEN DO:
      IF dirtidber = "" THEN DO: 
         dirtid = FALSE.     
         RETURN.   
      END.
      {SOKSTART.I}
      ASSIGN
      soktemp.SOKVAL = 28
      soktemp.SOKCHAR[1] = tidpers.PERSONALKOD
      soktemp.SOKCHAR[2] = dirtidber
      soktemp.SOKDATE[1] = regdatum      
      soktemp.SOKINT[1] = 3.
      {SOKANROP.I}
      IF soktemp.SOKINT[2] NE 0 THEN DO:
         dirtidrec = soktemp.SOKINT[2].            
      END.
      ELSE DO:
         dirtid = FALSE.     
         RETURN.   
      END.
   END.
   ELSE IF RAD_AND = 4 THEN DO:
      IF dirtidtra = "" THEN DO: 
         dirtid = FALSE.     
         RETURN.   
      END.
      {SOKSTART.I}
      ASSIGN
      soktemp.SOKVAL = 28
      soktemp.SOKCHAR[1] = tidpers.PERSONALKOD
      soktemp.SOKCHAR[2] = dirtidtra
      soktemp.SOKDATE[1] = regdatum      
      soktemp.SOKINT[1] = 4.
      {SOKANROP.I}
      IF soktemp.SOKINT[2] NE 0 THEN DO:
         dirtidrec = soktemp.SOKINT[2].            
      END.
      ELSE DO:
         dirtid = FALSE.     
         RETURN.   
      END.
   END.
   ELSE IF RAD_AND = 5 THEN DO:      
      IF dirtidover = "" THEN DO: 
         dirtid = FALSE.     
         RETURN.   
      END.
      {SOKSTART.I}
      ASSIGN
      soktemp.SOKVAL = 28
      soktemp.SOKCHAR[1] = tidpers.PERSONALKOD
      soktemp.SOKCHAR[2] = dirtidover
      soktemp.SOKDATE[1] = regdatum      
      soktemp.SOKINT[1] = 5.
      {SOKANROP.I}                    
      IF soktemp.SOKINT[2] NE 0 THEN DO:
         ASSIGN
         dirtidrec = soktemp.SOKINT[2]
         dirtidoveraknare = soktemp.SOKINT[3].         
      END.
      ELSE DO:
         dirtid = FALSE.     
         RETURN.   
      END.
   END.   

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
  DISPLAY TOG_HEL 
      WITH FRAME FRAME-TIDS IN WINDOW WINDOW-2.
  ENABLE TOG_HEL BTN_EXCEL BTN_NAAVB BTN_VTID BTN_FGOD BTN_KONTROLL BTN_SKRIV 
         BTN_AVB 
      WITH FRAME FRAME-TIDS IN WINDOW WINDOW-2.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-TIDS}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fbort_UI WINDOW-2 
PROCEDURE fbort_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   {muswait.i}
  
   IF Guru.Konstanter:appcon THEN DO:                           
      RUN TIDSTOPP.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT 6,INPUT tidpers.PERSONALKOD,INPUT Guru.Konstanter:globanv, OUTPUT lasanv,INPUT-OUTPUT TABLE felmeddtemp).
   END.
   ELSE DO:
      RUN TIDSTOPP.P 
      (INPUT 6,INPUT tidpers.PERSONALKOD,INPUT Guru.Konstanter:globanv, OUTPUT lasanv,INPUT-OUTPUT TABLE felmeddtemp).
   END.
   FIND FIRST felmeddtemp NO-LOCK NO-ERROR.
   IF AVAILABLE felmeddtemp THEN DO:
      IF Guru.Konstanter:globanv = lasanv THEN DO:    
         MESSAGE felmeddtemp.FELMEDD  VIEW-AS ALERT-BOX
         QUESTION BUTTONS YES-NO-CANCEL UPDATE val4 AS LOGICAL.
         DELETE felmeddtemp.
         CASE val4:                  
            WHEN TRUE THEN DO:           
               RUN lasupptid_UI IN persapph (INPUT  tidpers.PERSONALKOD, OUTPUT ptraff).
               IF ptraff = TRUE THEN DO:
                  MESSAGE tidpers.PERSONALKOD + " är nu upplåst!."
                  VIEW-AS ALERT-BOX.
               END.                         
            END.                  
         END CASE.
      END.   
      ELSE DO:   
         MESSAGE felmeddtemp.FELMEDD VIEW-AS ALERT-BOX.
         DELETE felmeddtemp.                    
         RETURN NO-APPLY.
      END.          
   END.
   EMPTY TEMP-TABLE appmarkpers NO-ERROR.    
   spregdatum = regdatum.
   ASSIGN
   regdatum = TODAY.
   
   CREATE appmarkpers.
   ASSIGN 
   appmarkpers.PERSONALKOD = tidpers.PERSONALKOD.      
   {ANDFBOIN.I}
   {SOKSTART.I}
   ASSIGN
   soktemp.SOKVAL = 26
   soktemp.SOKCHAR[1] = tidpers.PERSONALKOD
   soktemp.SOKINT[1] = YEAR(regdatum)
   soktemp.SOKINT[2] = MONTH(regdatum).
   {SOKANROP.I}
   FIND FIRST tidut WHERE tidut.UT BEGINS "   NAMN:" NO-LOCK NO-ERROR.
   FIND NEXT tidut EXCLUSIVE-LOCK NO-ERROR.
   IF soktemp.SOKLOG[2] = TRUE AND soktemp.SOKDATE[1] NE ? THEN DO:       
      ASSIGN SUBSTRING(tidut.UT,10,30) = "                              ".                                
      ASSIGN SUBSTRING(tidut.UT,40,132) = "GODKÄND TILL OCH MED " + STRING(soktemp.SOKDATE[1]).                                
   END.      
   ELSE ASSIGN SUBSTRING(tidut.UT,40,132) = " " .
   IF TOG_HEL = TRUE THEN status-ok = BRW_UT2:REFRESH() IN FRAME {&FRAME-NAME}.
   ELSE status-ok = BRW_UT:REFRESH() IN FRAME {&FRAME-NAME}.
   IF korda = 3 THEN.
   ELSE DO:
      ASSIGN BTN_AND:HIDDEN = FALSE.
      ENABLE BTN_AND WITH FRAME {&FRAME-NAME}. 
   END.
   regdatum = spregdatum.
   IF Guru.Konstanter:appcon THEN DO:                           
      RUN TIDSTOPP.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT 2,INPUT tidpers.PERSONALKOD,INPUT Guru.Konstanter:globanv, OUTPUT lasanv,INPUT-OUTPUT TABLE felmeddtemp).
   END.
   ELSE DO:
      RUN TIDSTOPP.P 
      (INPUT 2,INPUT tidpers.PERSONALKOD,INPUT Guru.Konstanter:globanv, OUTPUT lasanv,INPUT-OUTPUT TABLE felmeddtemp).
   END.   
   {musarrow.i}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fgod_UI WINDOW-2 
PROCEDURE fgod_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/      

   IF regdatum = ? THEN DO:
      regdatum = TODAY.        
   END.
   IF regdatum > avdatum THEN DO:
      regdatum = avdatum.
   END.
   ELSE IF regdatum < bdatum THEN DO:
      regdatum = avdatum.
   END. 
   IF MONTH(regdatum) = MONTH(TODAY) AND YEAR(regdatum) = YEAR(TODAY) THEN DO:
      regdatum = TODAY - 1.
      /*FÖR ATT KLARA DEN FÖRSTa I MÅNADEN*/
      IF MONTH(regdatum) NE regmnr THEN regdatum = TODAY.     
   END. 
   ELSE DO:         
      IF regmnr = 12 THEN regdatum = DATE(12,31,YEAR(regdatum)). 
      ELSE regdatum = DATE((MONTH(regdatum) + 1),01,YEAR(regdatum)) - 1.     
   END.       
   RUN REGVEC.P.    
   {AVBGOMD.I}
   RUN GODDATUM.W.               
   {AVBFRAMD.I}
   {muswait.i}             
   IF musz = TRUE THEN DO:
      musz = FALSE.
      RETURN.
   END.    
   /*forfärdig*/
   
   DO:
      EMPTY TEMP-TABLE kollut NO-ERROR.       
      EMPTY TEMP-TABLE invartemp NO-ERROR.       
      sparavdatum = avdatum.
      avdatum = regdatum.      
      CREATE invartemp.
      ASSIGN
      invartemp.GA = Guru.Konstanter:globanv
      invartemp.GM = gvisatidpermanad
      invartemp.SK = skrivut 
      invartemp.TI = tidtabrec 
      invartemp.PER = persrec 
      invartemp.PER2 = persrec2 
      invartemp.MU = musz    
      invartemp.REGST = regstart  
      invartemp.REGSU = regslut 
      invartemp.RV = regvnr 
      invartemp.RDAG = regdagnamn         
      invartemp.RD = regdatum 
      invartemp.RM = MONTH(regdatum)
      invartemp.RMN = regmannamn 
      invartemp.REGA = YEAR(regdatum) 
      invartemp.RT = regtotalt       
      invartemp.BD = bdatum 
      invartemp.AD = avdatum 
      invartemp.NY = nytid 
      invartemp.SEK = sekunder 
      invartemp.RSEK = regstartsek 
      invartemp.REGS = regslutsek 
      invartemp.GL = Guru.Konstanter:globforetag.   
      EMPTY TEMP-TABLE tidpers2 NO-ERROR.       
      CREATE tidpers2.
      BUFFER-COPY tidpers TO tidpers2.
      IF Guru.Konstanter:appcon THEN DO:                                 
         RUN APKTRL2.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
         (INPUT TABLE invartemp, INPUT TABLE tidpers2, OUTPUT TABLE kollut).
      END.
      ELSE DO:
         RUN APKTRL2.P 
         (INPUT TABLE invartemp, INPUT TABLE tidpers2, OUTPUT TABLE kollut).
      END.      
      musz = FALSE.      
      fpaminn = TRUE.
      IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "celpa" THEN DO:                  
         FOR EACH kollut WHERE SUBSTRING(kollut.UT,57,11) = "Påminnelse!" :         
            MESSAGE SUBSTRING(kollut.UT,57) + " " + SUBSTRING(kollut.UT,44,8) + " för " + tidpers.PERSONALKOD
            + " Vill du färdigrapportera ändå?"  VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO 
            TITLE "Påminnelse arbete under veckovila" UPDATE valet3 AS LOGICAL.
            CASE valet3:               
               WHEN FALSE THEN DO:                     
                  fpaminn = FALSE.
               END.
            END CASE.         
            DELETE kollut.
         END.
         FOR EACH kollut WHERE SUBSTRING(kollut.UT,57,8) = "Varning!" :         
            MESSAGE SUBSTRING(kollut.UT,57) + " Gäller " + tidpers.PERSONALKOD
            + " Vill du färdigrapportera ändå?"  VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO 
            TITLE "Varning" UPDATE valet6 AS LOGICAL.
            CASE valet6:               
               WHEN FALSE THEN DO:                     
                  fpaminn = FALSE.
               END.
            END CASE.         
            DELETE kollut.
         END.
         FOR EACH kollut WHERE SUBSTRING(kollut.UT,57,11) = "Påminnelse:" :         
            MESSAGE SUBSTRING(kollut.UT,57) + " Vill du färdigrapportera för " + tidpers.PERSONALKOD + " ändå?"  VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO 
            TITLE "Påminnelse ATK-rapportering" UPDATE valet10 AS LOGICAL.
            CASE valet10:               
               WHEN FALSE THEN DO:                     
                  fpaminn = FALSE.
               END.
            END CASE.         
            DELETE kollut.
         END.
         FOR EACH kollut WHERE SUBSTRING(kollut.UT,57,11) = "Påminnelse-" :
            DELETE kollut.
         END.      
      END.
      IF Guru.Konstanter:globforetag = "LULE" OR Guru.Konstanter:globforetag = "elpa" THEN DO:                  
         FOR EACH kollut WHERE SUBSTRING(kollut.UT,57,11) = "Påminnelse!" :         
            MESSAGE SUBSTRING(kollut.UT,57) +  " för " + tidpers.PERSONALKOD + " ? " 
            + " Vill du färdigrapportera ändå?"  VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO 
            TITLE "Påminnelse parkering" UPDATE valet5 AS LOGICAL.
            CASE valet5:               
               WHEN FALSE THEN DO:                     
                  fpaminn = FALSE.
               END.
            END CASE.         
            DELETE kollut.
         END.
         FOR EACH kollut WHERE SUBSTRING(kollut.UT,57,8) = "Varning!" :         
            MESSAGE SUBSTRING(kollut.UT,57) + " Gäller " + tidpers.PERSONALKOD
            + " Vill du färdigrapportera ändå?"  VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO 
            TITLE "Varning" UPDATE valet7 AS LOGICAL.
            CASE valet7:               
               WHEN FALSE THEN DO:                     
                  fpaminn = FALSE.
               END.
            END CASE.         
            DELETE kollut.
         END.      
      END.      
      IF Guru.Konstanter:globforetag = "GKAL" OR Guru.Konstanter:globforetag = "elpa" THEN DO:                  
         FOR EACH kollut WHERE SUBSTRING(kollut.UT,57,11) = "Dubbelcheck" :         
            MESSAGE SUBSTRING(kollut.UT,57) + " " + SUBSTRING(kollut.UT,44,8) + " för " + tidpers.PERSONALKOD
            + " Vill du färdigrapportera ändå?"  VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO 
            TITLE "Dubbelcheck: Tidskrivning hel dag på dygnsvila (172)." UPDATE valet4 AS LOGICAL.
            CASE valet4:               
               WHEN FALSE THEN DO:                     
                  fpaminn = FALSE.
               END.
            END CASE.         
            DELETE kollut.
         END.
         FOR EACH kollut WHERE SUBSTRING(kollut.UT,57,8) = "Varning!" :         
            MESSAGE SUBSTRING(kollut.UT,57) + " Gäller " + tidpers.PERSONALKOD
            + " Vill du färdigrapportera ändå?"  VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO 
            TITLE "Varning" UPDATE valet8 AS LOGICAL.
            CASE valet8:               
               WHEN FALSE THEN DO:                     
                  fpaminn = FALSE.
               END.
            END CASE.         
            DELETE kollut.
         END.
         FOR EACH kollut WHERE SUBSTRING(kollut.UT,57,11) = "Påminnelse-" :
            DELETE kollut.
         END.   
         FOR EACH kollut WHERE SUBSTRING(kollut.UT,57,11) = "Påminnelse:" :         
            MESSAGE SUBSTRING(kollut.UT,57) + " Vill du färdigrapportera för " + tidpers.PERSONALKOD + " ändå?"  VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO 
            TITLE "Påminnelse ATK och friskvårdsrapportering" UPDATE valet9 AS LOGICAL.
            CASE valet9:               
               WHEN FALSE THEN DO:                     
                  fpaminn = FALSE.
               END.
            END CASE.         
            DELETE kollut.
         END.              
      END.
      avdatum = sparavdatum.
      ASSIGN   
      str=                                                                              
 "===========================================================================================================".
      
      FIND FIRST kollut WHERE kollut.UT = str NO-LOCK NO-ERROR. 
      FIND NEXT kollut NO-LOCK NO-ERROR.
      IF AVAILABLE kollut THEN DO:    
         /*forfärdig*/         
         DO:
            IF Guru.Konstanter:globforetag = "GKAL" OR Guru.Konstanter:globforetag = "LULE" OR Guru.Konstanter:globforetag = "GRAN" OR Guru.Konstanter:globforetag = "SKOK"  OR Guru.Konstanter:globforetag = "elpa" THEN DO:               
               {SOKSTART.I}
               ASSIGN
               soktemp.SOKVAL = 13
               soktemp.SOKCHAR[1] = tidpers.PERSONALKOD
               soktemp.SOKLOG[1] = TRUE.              
               {SOKANROP.I}        
               IF soktemp.SOKCHAR[4] = "TIMANSTÄLLD" OR Guru.Konstanter:globanv = "mariel" OR Guru.Konstanter:globanv = "Celpao" THEN DO:
                  MESSAGE "Det SAKNAS registreringar eller finns ofullständiga registreringar!" SUBSTRING(kollut.UT,44,8) skip
                  "Du kan kontrollera vad som saknas via KONTROLL"
                  "Vill du FÄRDIGRAPPORTERA ändå?." SKIP         
                  VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO 
                  TITLE "Ofullständig tidrapport"
                  UPDATE valet1 AS LOGICAL.
                  CASE valet1:
                     WHEN TRUE THEN DO:                     
                        musz = TRUE.   
                     END.           
                  END CASE.      
               END.
               ELSE DO:
                  MESSAGE "Det går EJ att färdigrapportera eftersom det SAKNAS registreringar eller finns ofullständiga registreringar!" SUBSTRING(kollut.UT,44,8) skip
                  "Du kan kontrollera vad som saknas via KONTROLL"            
                  VIEW-AS ALERT-BOX TITLE "Ofullständig tidrapport".
                  musz = FALSE.             
               END.
            END.
            ELSE DO:           
               MESSAGE "Det går EJ att färdigrapportera eftersom det SAKNAS registreringar eller finns ofullständiga registreringar!" SUBSTRING(kollut.UT,44,8) skip
               "Du kan kontrollera vad som saknas via KONTROLL"            
               VIEW-AS ALERT-BOX TITLE "Ofullständig tidrapport".
               musz = FALSE.
            END.
         END.         
      END.
      ELSE ASSIGN musz = TRUE. 
   END.   
   IF musz = TRUE AND fpaminn = TRUE THEN DO:
      musz = FALSE.
      EMPTY TEMP-TABLE appmarkpers NO-ERROR. 
      EMPTY TEMP-TABLE godmarkpers NO-ERROR.       
      CREATE godmarkpers.
      ASSIGN
      godmarkpers.EFTERNAMN = tidpers.EFTERNAMN
      godmarkpers.FORNAMN = tidpers.FORNAMN
      godmarkpers.PERSONALKOD = tidpers.PERSONALKOD
      godmarkpers.GODKAND = "SAKNAR TID"    
      godmarkpers.TIDPERSREC = tidpers.TIDPERSREC.
      godmarkpers.VECKONUMMER = regvnr.
      tillochmeddatum = ?.
      {SOKSTART.I}
      ASSIGN
      soktemp.SOKVAL = 26
      soktemp.SOKCHAR[1] = godmarkpers.PERSONALKOD
      soktemp.SOKINT[1] = YEAR(regdatum)
      soktemp.SOKINT[2] = MONTH(regdatum).
      {SOKANROP.I}
      IF soktemp.SOKLOG[2] = TRUE THEN DO:
         ASSIGN
         tillochmedklar = soktemp.SOKLOG[2]
         godmarkpers.DATUM = soktemp.SOKDATE[1]            
         tillochmeddatum = soktemp.SOKDATE[1].
      END.   
      ELSE DO:
         godmarkpers.DATUM = ?.
      END.
      tidtabrec = RECID(godmarkpers).      
      IF Guru.Konstanter:appcon THEN DO:                           
         RUN TIDSTOPP.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
         (INPUT 6,INPUT tidpers.PERSONALKOD,INPUT Guru.Konstanter:globanv, OUTPUT lasanv,INPUT-OUTPUT TABLE felmeddtemp).
      END.
      ELSE DO:
         RUN TIDSTOPP.P 
         (INPUT 6,INPUT tidpers.PERSONALKOD,INPUT Guru.Konstanter:globanv, OUTPUT lasanv,INPUT-OUTPUT TABLE felmeddtemp).
      END.
      FIND FIRST felmeddtemp NO-LOCK NO-ERROR.
      IF AVAILABLE felmeddtemp THEN DO:
         IF Guru.Konstanter:globanv = lasanv THEN DO:    
            MESSAGE felmeddtemp.FELMEDD  VIEW-AS ALERT-BOX
            QUESTION BUTTONS YES-NO-CANCEL UPDATE val4 AS LOGICAL.
            DELETE felmeddtemp.
            CASE val4:                  
               WHEN TRUE THEN DO:           
                  RUN lasupptid_UI IN persapph (INPUT  tidpers.PERSONALKOD, OUTPUT ptraff).
                  IF ptraff = TRUE THEN DO:
                     MESSAGE tidpers.PERSONALKOD + " är nu upplåst!."
                     VIEW-AS ALERT-BOX.
                  END.                         
               END.                  
            END CASE.
         END.   
         ELSE DO:      
            MESSAGE felmeddtemp.FELMEDD VIEW-AS ALERT-BOX.
            DELETE felmeddtemp.                     
            RETURN NO-APPLY.
         END.          
      END.
      IF godmarkpers.DATUM <= regdatum OR godmarkpers.DATUM = ? THEN DO:
         CREATE appmarkpers.
         ASSIGN appmarkpers.PERSONALKOD = godmarkpers.PERSONALKOD
         appmarkpers.DATUM = regdatum
         appmarkpers.VECKONUMMER = godmarkpers.VECKONUMMER.
      END.
      ELSE IF godmarkpers.DATUM > regdatum THEN ASSIGN regdatum = godmarkpers.DATUM.
      ASSIGN 
      godmarkpers.DATUM = regdatum
      godmarkpers.GODKAND = "F".         
      {GODKFIN.I}         
      EMPTY TEMP-TABLE appmarkpers NO-ERROR. 
      FIND FIRST tidut WHERE tidut.UT BEGINS "   NAMN:" NO-LOCK NO-ERROR.
      FIND NEXT tidut EXCLUSIVE-LOCK NO-ERROR.
      ASSIGN SUBSTRING(tidut.UT,40) = 
      "FÄRDIG FÖR GODKÄNNANDE TILL OCH MED " + STRING(regdatum).     
      IF Guru.Konstanter:appcon THEN DO:                           
         RUN TIDSTOPP.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
         (INPUT 2,INPUT tidpers.PERSONALKOD,INPUT Guru.Konstanter:globanv, OUTPUT lasanv,INPUT-OUTPUT TABLE felmeddtemp).
      END.
      ELSE DO:
         RUN TIDSTOPP.P 
         (INPUT 2,INPUT tidpers.PERSONALKOD,INPUT Guru.Konstanter:globanv, OUTPUT lasanv,INPUT-OUTPUT TABLE felmeddtemp).
      END.   
   END.
   
   musz = FALSE.
   {musarrow.i}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fixbtn_UI WINDOW-2 
PROCEDURE fixbtn_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/     
  Guru.GlobalaVariabler:collefth =  BTN_AVB:HANDLE IN FRAME {&FRAME-NAME}. 
  IF BTN_FGOD:VISIBLE IN FRAME {&FRAME-NAME} = TRUE THEN DO:
     Guru.GlobalaVariabler:colrighth = BTN_FGOD:HANDLE.   
     RUN buttcolh_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).       
  END.
  IF BTN_KONTROLL:VISIBLE = TRUE THEN DO:
     Guru.GlobalaVariabler:colrighth = BTN_KONTROLL:HANDLE.           
     RUN buttcolh_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).       
  END.  
  IF BTN_AND:VISIBLE = TRUE THEN DO:
     Guru.GlobalaVariabler:colrighth = BTN_AND:HANDLE.           
     RUN buttcolh_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).       
  END.   
  IF BTN_SKRIV:VISIBLE = TRUE THEN DO:
     Guru.GlobalaVariabler:colrighth = BTN_SKRIV:HANDLE.           
     RUN buttcolh_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).       
  END.   
  IF BTN_EXCEL:VISIBLE = TRUE THEN DO:
     Guru.GlobalaVariabler:colrighth = BTN_EXCEL:HANDLE.           
     RUN buttcolh_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).       
  END.   
  IF BTN_VTID:VISIBLE = TRUE THEN DO:
     Guru.GlobalaVariabler:colrighth = BTN_VTID:HANDLE.           
     RUN buttcolh_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).       
  END.   
  IF BTN_NAAVB:VISIBLE = TRUE THEN DO:
     Guru.GlobalaVariabler:colrighth = BTN_NAAVB:HANDLE.    
     RUN buttcolh_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).       
  END.
  Guru.GlobalaVariabler:collefth = ?.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE knappar_UI WINDOW-2 
PROCEDURE knappar_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/  
   IF knappvar = TRUE THEN DO:
      ENABLE BTN_AND WITH FRAME {&FRAME-NAME}.            
      BTN_FGOD:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
      knappvar = FALSE.
   END.
   ELSE DO:
      /*forfärdig*/      
      DO:   
         {SOKSTART.I}
         ASSIGN
         soktemp.SOKVAL = 26
         soktemp.SOKCHAR[1] = tidpers.PERSONALKOD
         soktemp.SOKINT[1] = YEAR(regdatum)
         soktemp.SOKINT[2] = MONTH(regdatum).
         {SOKANROP.I}           
         IF soktemp.SOKLOG[1] = TRUE THEN DO:                      
            BTN_FGOD:LABEL = "Ta bort färdig".                        
         END.
         ELSE DO:
            BTN_FGOD:LABEL = "   Färdig/Ta bort färdig".            
         END.      
         BTN_FGOD:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.       
         ENABLE BTN_AND WITH FRAME {&FRAME-NAME}. 
      END.      
   END.   
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE nytid_UI WINDOW-2 
PROCEDURE nytid_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   REPEAT:               
      IF musz = TRUE THEN musz = FALSE.
      ELSE bdatum = avdatum + 1.
      regdatum = bdatum.  
      RUN REGVEC.P.     
      IF slutdatum < bdatum THEN DO:                  
         FIND NEXT tidpers USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.
         IF NOT AVAILABLE tidpers THEN DO:
            IF huvud = TRUE THEN DO:
               musz = TRUE.
               LEAVE.
            END.
            ELSE DO:            
               /*status-mus2 = CURRENT-WINDOW:LOAD-MOUSE-POINTER("ARROW").*/
               status-mus2 = SESSION:SET-WAIT-STATE("").
               musz = TRUE.
               LEAVE.
            END.
         END.          
         IF  RAD_TIDSVAL = 2  AND (korda = 2 OR korda = 3) THEN DO:   
            {SOKSTART.I}
            ASSIGN
            soktemp.SOKVAL = 22
            soktemp.SOKINT[1] = korda
            soktemp.SOKCHAR[1] = tidpers.PERSONALKOD
            soktemp.SOKDATE[1] = avdatum.
            {SOKANROP.I}               
            IF soktemp.SOKDATE[2] NE ? THEN DO:
               ASSIGN regdatum = soktemp.SOKDATE[2]
               bdatum = soktemp.SOKDATE[2].
               RUN REGVEC.P.           
            END.   
            ELSE DO:
               ASSIGN regdatum = DATE(MONTH(slutdatum),01,YEAR(slutdatum)).
               bdatum = DATE(MONTH(slutdatum),01,YEAR(slutdatum)).
               RUN REGVEC.P.
            END.   
         END.  
         ELSE DO: 
            regdatum = startdatum.  
            RUN REGVEC.P.
            ASSIGN
            bdatum = startdatum.
         END.
         ASSIGN      
         persrec = tidpers.TIDPERSREC
         tidtabrec = 0.         
      END.         
      RUN avvecka_UI.             
      {SOKSTART.I}
      ASSIGN
      soktemp.SOKVAL = 24
      soktemp.SOKINT[1] = korda
      soktemp.SOKINT[2] = RAD_TIDSVAL
      soktemp.SOKCHAR[1] = tidpers.PERSONALKOD
      soktemp.SOKDATE[1] = bdatum
      soktemp.SOKDATE[2] = avdatum.
      {SOKANROP.I}      
      IF soktemp.SOKLOG[1] = TRUE THEN DO:         
         IF soktemp.SOKLOG[2] = FALSE THEN NEXT.
         IF RAD_TIDSVAL = 2 AND korda = 1 THEN NEXT.      /*2004-01-20 tillägg /Lena */   
         IF RAD_TIDSVAL = 2 AND korda = 2 THEN NEXT.
         IF RAD_TIDSVAL = 2 AND korda = 3 THEN NEXT.         
         IF RAD_TIDSVAL = 3 THEN NEXT.                   /*2004-01-20 tillägg /Lena*/
      END.      
      LEAVE.               
   END. 
      
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sattmus_UI WINDOW-2 
PROCEDURE sattmus_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   RUN SetDefaultCursors IN Guru.Konstanter:hpApi.
   Guru.GlobalaVariabler:retvalkoll = FALSE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE skrivut_UI WINDOW-2 
PROCEDURE skrivut_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  /*UTSKRIFT*/
   RUN nytid_UI.
   IF musz = TRUE THEN DO:
      musz = musz.
   END.   
   ELSE DO:     
      RUN apput_UI.      
   END.  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

