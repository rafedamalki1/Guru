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

  Created: 95/05/02 -  1:43 pm

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
{REGVAR.I}
{EXTRADATA.I}  
{HOPPSEK2W.I}
DEFINE NEW SHARED VARIABLE defprisproch AS HANDLE NO-UNDO. /* Procedure handle, DEFPRISAPP.P */
DEFINE NEW SHARED VARIABLE laddaproch AS HANDLE NO-UNDO. /* DYNLADDATEMP.P */
DEFINE NEW SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE fakkoproch AS HANDLE NO-UNDO. /* FAKKAPP.P */
DEFINE NEW SHARED VARIABLE overtexproch AS HANDLE NO-UNDO. /* Procedure handle till OVERTEXAPP.P */

DEFINE VARIABLE overtextid AS CHARACTER NO-UNDO.
DEFINE VARIABLE felmedd AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.   
DEFINE VARIABLE defprisid AS CHARACTER NO-UNDO.
DEFINE VARIABLE edataapph AS HANDLE NO-UNDO.
DEFINE VARIABLE ktoproch AS HANDLE NO-UNDO.
{TIDUTTTNEW.I}
DEFINE TEMP-TABLE kbtemp
   FIELD K1 AS CHARACTER
   FIELD K2 AS CHARACTER
   FIELD K3 AS CHARACTER
   FIELD K4 AS CHARACTER
   FIELD K5 AS CHARACTER.
DEFINE TEMP-TABLE jurtemp
   FIELD JUDID AS CHARACTER
   FIELD NAMN AS CHARACTER
   FIELD VIJUDID AS CHARACTER.

&SCOPED-DEFINE NEW NEW
&SCOPED-DEFINE SHARED SHARED
{BEFTEMP.I}
{OVERTEXTEMP.I}
{FAKKOTEMP.I}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE WINDOW
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A
&Scoped-define BROWSE-NAME BRW_DPRIS

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES defpristemp extradatatemp overtextfakttemp

/* Definitions for BROWSE BRW_DPRIS                                     */
&Scoped-define FIELDS-IN-QUERY-BRW_DPRIS defpristemp.PRISNUM ~
defpristemp.PRISTEXT defpristemp.STARTDATUM defpristemp.OVER 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_DPRIS defpristemp.PRISNUM 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_DPRIS defpristemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_DPRIS defpristemp
&Scoped-define QUERY-STRING-BRW_DPRIS FOR EACH defpristemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_DPRIS OPEN QUERY BRW_DPRIS FOR EACH defpristemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_DPRIS defpristemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_DPRIS defpristemp


/* Definitions for BROWSE BRW_FRIP                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_FRIP extradatatemp.SOKCHAR[6] ~
extradatatemp.SOKDEC[1] extradatatemp.SOKDATE[1] extradatatemp.SOKDATE[2] ~
extradatatemp.SOKCHAR[10] extradatatemp.SOKCHAR[9] 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_FRIP extradatatemp.SOKCHAR[6] ~
extradatatemp.SOKDEC[1] extradatatemp.SOKDATE[1] extradatatemp.SOKDATE[2] ~
extradatatemp.SOKCHAR[10] extradatatemp.SOKCHAR[9] 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_FRIP extradatatemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_FRIP extradatatemp
&Scoped-define QUERY-STRING-BRW_FRIP FOR EACH extradatatemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_FRIP OPEN QUERY BRW_FRIP FOR EACH extradatatemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_FRIP extradatatemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_FRIP extradatatemp


/* Definitions for BROWSE BRW_OTEXT                                     */
&Scoped-define FIELDS-IN-QUERY-BRW_OTEXT overtextfakttemp.OTEXT 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_OTEXT 
&Scoped-define QUERY-STRING-BRW_OTEXT FOR EACH overtextfakttemp NO-LOCK ~
    BY overtextfakttemp.OTEXT
&Scoped-define OPEN-QUERY-BRW_OTEXT OPEN QUERY BRW_OTEXT FOR EACH overtextfakttemp NO-LOCK ~
    BY overtextfakttemp.OTEXT.
&Scoped-define TABLES-IN-QUERY-BRW_OTEXT overtextfakttemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_OTEXT overtextfakttemp


/* Definitions for FRAME FRAME-A                                        */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RAD_PRIS BRW_FRIP BRW_DPRIS BRW_OTEXT ~
FBTN_VPER FBTN_SKR FBTN_SPARA BTN_NY BTN_AND BTN_BORT BTN_AVB 
&Scoped-Define DISPLAYED-OBJECTS RAD_PRIS 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR WINDOW-1 AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE MENU POPUP-MENU-BRW_FRIP 
       MENU-ITEM m_Stt_startdatum LABEL "Sätt startdatum"
       MENU-ITEM m_Stt_slutdatum LABEL "Sätt slutdatum".


/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AND 
     LABEL "Ändra":L 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_AVB 
     LABEL "Avsluta":L 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_BORT 
     LABEL "Ta bort":L 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_NY 
     LABEL "Ny":L 
     SIZE 12 BY 1.

DEFINE BUTTON FBTN_SKR 
     LABEL "Skriv ut":L 
     SIZE 12 BY 1.

DEFINE BUTTON FBTN_SPARA 
     LABEL "Snabbspara":L 
     SIZE 12 BY 1.

DEFINE BUTTON FBTN_VPER 
     LABEL "Visa":L 
     SIZE 12 BY 1.

DEFINE VARIABLE RAD_PRIS AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Prislistor", 1,
"Fri prislista", 2
     SIZE 69.5 BY 1.29 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_DPRIS FOR 
      defpristemp SCROLLING.

DEFINE QUERY BRW_FRIP FOR 
      extradatatemp SCROLLING.

DEFINE QUERY BRW_OTEXT FOR 
      overtextfakttemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_DPRIS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_DPRIS WINDOW-1 _STRUCTURED
  QUERY BRW_DPRIS NO-LOCK DISPLAY
      defpristemp.PRISNUM COLUMN-LABEL "Pris!lista" FORMAT ">>>>>>9":U
      defpristemp.PRISTEXT COLUMN-LABEL "Text" FORMAT "X(20)":U
      defpristemp.STARTDATUM COLUMN-LABEL "Startdatum" FORMAT "99/99/99":U
      defpristemp.OVER COLUMN-LABEL "Listtyp" FORMAT "Övertid/Normaltid":U
  ENABLE
      defpristemp.PRISNUM
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 51.25 BY 17.92
         TITLE "Prislistor".

DEFINE BROWSE BRW_FRIP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_FRIP WINDOW-1 _STRUCTURED
  QUERY BRW_FRIP NO-LOCK DISPLAY
      extradatatemp.SOKCHAR[6] COLUMN-LABEL "Faxt.text" FORMAT "X(256)":U
            WIDTH 40
      extradatatemp.SOKDEC[1] COLUMN-LABEL "Pris" FORMAT "->>>>>>9.999":U
      extradatatemp.SOKDATE[1] COLUMN-LABEL "Start!datum" FORMAT "99/99/99":U
      extradatatemp.SOKDATE[2] COLUMN-LABEL "Slut!datum" FORMAT "99/99/99":U
      extradatatemp.SOKCHAR[10] COLUMN-LABEL "Sortering!Märkning" FORMAT "X(256)":U
            WIDTH 12
      extradatatemp.SOKCHAR[9] COLUMN-LABEL "Företag" FORMAT "X(256)":U
            WIDTH 10
  ENABLE
      extradatatemp.SOKCHAR[6]
      extradatatemp.SOKDEC[1]
      extradatatemp.SOKDATE[1]
      extradatatemp.SOKDATE[2]
      extradatatemp.SOKCHAR[10]
      extradatatemp.SOKCHAR[9]
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SIZE 105.5 BY 17.92 NO-EMPTY-SPACE.

DEFINE BROWSE BRW_OTEXT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_OTEXT WINDOW-1 _STRUCTURED
  QUERY BRW_OTEXT NO-LOCK DISPLAY
      overtextfakttemp.OTEXT COLUMN-LABEL "Övertidstext" FORMAT "X(25)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 37.5 BY 17.92.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     RAD_PRIS AT ROW 1.42 COL 1.5 NO-LABEL
     BRW_FRIP AT ROW 3.08 COL 1.5
     BRW_DPRIS AT ROW 3.08 COL 16.88
     BRW_OTEXT AT ROW 3.13 COL 23.38
     FBTN_VPER AT ROW 3.96 COL 107.38
     FBTN_SKR AT ROW 5.04 COL 107.38
     FBTN_SPARA AT ROW 6.17 COL 107.38
     BTN_NY AT ROW 21.42 COL 20.38
     BTN_AND AT ROW 21.42 COL 36.13
     BTN_BORT AT ROW 21.42 COL 51.88
     BTN_AVB AT ROW 21.42 COL 107.38
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 119.88 BY 21.54.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: WINDOW
   Temp-Tables and Buffers:
      TABLE: defpristemp T "?" NO-UNDO temp-db defpristemp
      TABLE: extradatatemp T "?" NO-UNDO temp-db extradatatemp
      TABLE: overtextfakttemp T "?" NO-UNDO temp-db overtextfakttemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW WINDOW-1 ASSIGN
         HIDDEN             = YES
         TITLE              = "Prislistor"
         HEIGHT             = 21.58
         WIDTH              = 119.88
         MAX-HEIGHT         = 21.58
         MAX-WIDTH          = 119.88
         VIRTUAL-HEIGHT     = 21.58
         VIRTUAL-WIDTH      = 119.88
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
/* SETTINGS FOR WINDOW WINDOW-1
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME FRAME-A
   FRAME-NAME                                                           */
/* BROWSE-TAB BRW_FRIP RAD_PRIS FRAME-A */
/* BROWSE-TAB BRW_DPRIS BRW_FRIP FRAME-A */
/* BROWSE-TAB BRW_OTEXT BRW_DPRIS FRAME-A */
ASSIGN 
       BRW_FRIP:POPUP-MENU IN FRAME FRAME-A             = MENU POPUP-MENU-BRW_FRIP:HANDLE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-1)
THEN WINDOW-1:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_DPRIS
/* Query rebuild information for BROWSE BRW_DPRIS
     _TblList          = "Temp-Tables.defpristemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.defpristemp.PRISNUM
"defpristemp.PRISNUM" "Pris!lista" ">>>>>>9" "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.defpristemp.PRISTEXT
"defpristemp.PRISTEXT" "Text" "X(20)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.defpristemp.STARTDATUM
"defpristemp.STARTDATUM" "Startdatum" ? "date" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.defpristemp.OVER
"defpristemp.OVER" "Listtyp" ? "logical" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_DPRIS */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_FRIP
/* Query rebuild information for BROWSE BRW_FRIP
     _TblList          = "Temp-Tables.extradatatemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.extradatatemp.SOKCHAR[6]
"extradatatemp.SOKCHAR[6]" "Faxt.text" "X(256)" "character" ? ? ? ? ? ? yes ? no no "40" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.extradatatemp.SOKDEC[1]
"extradatatemp.SOKDEC[1]" "Pris" "->>>>>>9.999" "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.extradatatemp.SOKDATE[1]
"extradatatemp.SOKDATE[1]" "Start!datum" ? "date" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.extradatatemp.SOKDATE[2]
"extradatatemp.SOKDATE[2]" "Slut!datum" ? "date" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.extradatatemp.SOKCHAR[10]
"extradatatemp.SOKCHAR[10]" "Sortering!Märkning" "X(256)" "character" ? ? ? ? ? ? yes ? no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > Temp-Tables.extradatatemp.SOKCHAR[9]
"extradatatemp.SOKCHAR[9]" "Företag" "X(256)" "character" ? ? ? ? ? ? yes ? no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_FRIP */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_OTEXT
/* Query rebuild information for BROWSE BRW_OTEXT
     _TblList          = "Temp-Tables.overtextfakttemp"
     _Options          = "NO-LOCK"
     _OrdList          = "Temp-Tables.overtextfakttemp.OTEXT|yes"
     _FldNameList[1]   > Temp-Tables.overtextfakttemp.OTEXT
"overtextfakttemp.OTEXT" "Övertidstext" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_OTEXT */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME BRW_DPRIS
&Scoped-define SELF-NAME BRW_DPRIS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_DPRIS WINDOW-1
ON MOUSE-SELECT-DBLCLICK OF BRW_DPRIS IN FRAME FRAME-A /* Prislistor */
DO:
   run andra_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_FRIP
&Scoped-define SELF-NAME BRW_FRIP
&Scoped-define SELF-NAME extradatatemp.SOKDATE[1]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL extradatatemp.SOKDATE[1] BRW_FRIP _BROWSE-COLUMN WINDOW-1
ON MOUSE-MENU-CLICK OF extradatatemp.SOKDATE[1] IN BROWSE BRW_FRIP /* Start!datum */
DO:
   Guru.GlobalaVariabler:regdatum = INPUT BROWSE BRW_FRIP extradatatemp.SOKDATE[1].
   IF Guru.GlobalaVariabler:regdatum = ? THEN Guru.GlobalaVariabler:regdatum = TODAY.
   RUN AlmanBtn.w.
   extradatatemp.SOKDATE[1] = Guru.GlobalaVariabler:regdatum.   
   DISPLAY extradatatemp.SOKDATE[1] WITH BROWSE BRW_FRIP.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME extradatatemp.SOKDATE[2]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL extradatatemp.SOKDATE[2] BRW_FRIP _BROWSE-COLUMN WINDOW-1
ON MOUSE-MENU-CLICK OF extradatatemp.SOKDATE[2] IN BROWSE BRW_FRIP /* Slut!datum */
DO:
   Guru.GlobalaVariabler:regdatum = INPUT BROWSE BRW_FRIP extradatatemp.SOKDATE[2].
   IF Guru.GlobalaVariabler:regdatum = ? THEN regdatum = TODAY.
   RUN AlmanBtn.w.
   extradatatemp.SOKDATE[2] = Guru.GlobalaVariabler:regdatum.   
   DISPLAY extradatatemp.SOKDATE[2] WITH BROWSE BRW_FRIP.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_OTEXT
&Scoped-define SELF-NAME BRW_OTEXT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_OTEXT WINDOW-1
ON MOUSE-SELECT-DBLCLICK OF BRW_OTEXT IN FRAME FRAME-A
DO:
   run andra_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AND
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AND WINDOW-1
ON CHOOSE OF BTN_AND IN FRAME FRAME-A /* Ändra */
DO:
   RUN andra_UI. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB WINDOW-1
ON CHOOSE OF BTN_AVB IN FRAME FRAME-A /* Avsluta */
DO:
   APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_BORT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_BORT WINDOW-1
ON CHOOSE OF BTN_BORT IN FRAME FRAME-A /* Ta bort */
DO:
   RUN bort_UI.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NY
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NY WINDOW-1
ON CHOOSE OF BTN_NY IN FRAME FRAME-A /* Ny */
DO:
   RUN ny_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_SKR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_SKR WINDOW-1
ON CHOOSE OF FBTN_SKR IN FRAME FRAME-A /* Skriv ut */
DO:    
   {muswait.i}
   skrivut = TRUE. 
   IF RAD_PRIS = 1 THEN RUN SKRIVVAL.W (INPUT FALSE).
   ELSE IF RAD_PRIS = 2 THEN RUN SKRIVVAL.W (INPUT TRUE).
   IF musz = TRUE THEN DO:
      musz = FALSE.
      skrivut = FALSE.  
   END.
   ELSE DO:
      RUN ut_UI. 
   END.  
   skrivut = FALSE.
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_SKR WINDOW-1
ON MOUSE-MENU-CLICK OF FBTN_SKR IN FRAME FRAME-A /* Skriv ut */
DO:
   RUN SIDLANGD.W.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_SPARA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_SPARA WINDOW-1
ON CHOOSE OF FBTN_SPARA IN FRAME FRAME-A /* Snabbspara */
DO:
   RUN spara_UI.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_VPER
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_VPER WINDOW-1
ON CHOOSE OF FBTN_VPER IN FRAME FRAME-A /* Visa */
DO:
   skrivut = FALSE.  
   RUN ut_UI.  
   {musarrow.i}   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Stt_slutdatum
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Stt_slutdatum WINDOW-1
ON CHOOSE OF MENU-ITEM m_Stt_slutdatum /* Sätt slutdatum */
DO:
   APPLY "MOUSE-MENU-CLICK" TO extradatatemp.SOKDATE[2] IN BROWSE BRW_FRIP.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Stt_startdatum
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Stt_startdatum WINDOW-1
ON CHOOSE OF MENU-ITEM m_Stt_startdatum /* Sätt startdatum */
DO:
   APPLY "MOUSE-MENU-CLICK" TO extradatatemp.SOKDATE[1] IN BROWSE BRW_FRIP.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RAD_PRIS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RAD_PRIS WINDOW-1
ON VALUE-CHANGED OF RAD_PRIS IN FRAME FRAME-A
DO:
   RAD_PRIS = INPUT RAD_PRIS.
   ASSIGN
   /*FILL-IN-text = "Prislistor:" */
   FBTN_SKR:HIDDEN = TRUE 
   FBTN_VPER:HIDDEN = TRUE
   FBTN_SPARA:HIDDEN = TRUE
   BRW_DPRIS:HIDDEN = TRUE
   BRW_OTEXT:HIDDEN = TRUE
   BRW_FRIP:HIDDEN = TRUE.
   IF RAD_PRIS = 1 THEN DO:
      ASSIGN
      FBTN_SKR:HIDDEN = FALSE 
      FBTN_VPER:HIDDEN = FALSE
      BRW_DPRIS:HIDDEN = FALSE.
   END.
   IF RAD_PRIS = 2 THEN DO:
      ASSIGN
      FBTN_SKR:HIDDEN = FALSE 
      FBTN_VPER:HIDDEN = FALSE
      FBTN_SPARA:HIDDEN = FALSE
      BRW_FRIP:HIDDEN = FALSE.      
   END.
   IF RAD_PRIS = 3 THEN DO:
      ASSIGN
     /* FILL-IN-text = "Övertidstext:"*/
      BRW_OTEXT:HIDDEN = FALSE.
      FIND FIRST overtextfakttemp NO-LOCK NO-ERROR.
      IF NOT AVAILABLE overtextfakttemp THEN DO:
         OPEN QUERY befq FOR EACH befattningstemp NO-LOCK BY befattningstemp.NAMN.
         GET FIRST befq NO-LOCK.
         DO WHILE AVAILABLE(befattningstemp):
            overtextid = "".
            RUN btnokovertext_UI IN overtexproch
               (INPUT-OUTPUT overtextid, INPUT befattningstemp.BEFATTNING, INPUT befattningstemp.NAMN).
            CREATE overtextfakttemp.
            ASSIGN
            overtextfakttemp.BEFATTNING = befattningstemp.BEFATTNING
            overtextfakttemp.OTEXT = befattningstemp.NAMN
            overtextfakttemp.OTEXTID = overtextid.
            GET NEXT befq NO-LOCK.
         END.
      END.
      RUN setdefaultcolbyname_UI IN brwproc[3] (INPUT "OTEXT").
      RUN openbdynspec_UI IN brwproc[3].
   END.
   /*DISPLAY FILL-IN-text WITH FRAME {&FRAME-NAME}. */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_DPRIS
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
   RUN spara_UI.
   IF VALID-HANDLE(ktoproch) THEN DELETE PROCEDURE ktoproch.
   IF VALID-HANDLE(defprisproch) THEN DELETE PROCEDURE defprisproch. 
   IF VALID-HANDLE(laddaproch) THEN DELETE PROCEDURE laddaproch. 
   IF VALID-HANDLE(edataapph) THEN DELETE PROCEDURE edataapph.      
   IF VALID-HANDLE(fakkoproch) THEN DELETE PROCEDURE fakkoproch.
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
   
   IF Guru.Konstanter:regsekvar[11] = TRUE THEN RAD_PRIS:ADD-LAST("Texter till övertid", 3).    
   RAD_PRIS = 1.
   RUN enable_UI.   
   {FRMSIZE.I}  
   FIND FIRST defpristemp NO-LOCK NO-ERROR.
   RUN kbjurhamt_UI IN ktoproch (INPUT Guru.Konstanter:globforetag,OUTPUT TABLE kbtemp,OUTPUT TABLE jurtemp).  
   RUN laddakontotemp_UI IN fakkoproch 
      (OUTPUT TABLE kundfodrantemp, 
       OUTPUT TABLE intakttemp, 
       OUTPUT TABLE motparttemp, 
       OUTPUT TABLE momstemp).
   APPLY "VALUE-CHANGED" TO RAD_PRIS.
   RUN openbdynspec_UI IN brwproc[1].
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
  Purpose: Changing screen-value for combo-box CMB_OMR     
  Parameters:  Input = Screen-value for CMB_FOR
  Notes:       
-------------------------------------------------------------*/  
   defpristemp.PRISNUM:READ-ONLY IN BROWSE BRW_DPRIS = TRUE.
   RUN DYNBRW.P PERSISTENT SET brwproc[1]
      (INPUT BRW_DPRIS:HANDLE IN FRAME {&FRAME-NAME}).  
   RUN DYNBRW.P PERSISTENT SET brwproc[2]
      (INPUT BRW_FRIP:HANDLE IN FRAME {&FRAME-NAME}).  
   RUN DYNBRW.P PERSISTENT SET brwproc[3]
      (INPUT BRW_OTEXT:HANDLE IN FRAME {&FRAME-NAME}).      
   IF Guru.Konstanter:appcon THEN DO:                           
      RUN OVERTEXAPP.P PERSISTENT SET overtexproch ON Guru.Konstanter:apphand TRANSACTION DISTINCT.
   END.
   ELSE DO:
      RUN OVERTEXAPP.P PERSISTENT SET overtexproch.
   END.
   
  
   RUN setcolindex_UI IN brwproc[2] (INPUT "SOKCHAR[10] BY SOKCHAR[6]").

   tthandle = TEMP-TABLE defpristemp:HANDLE.
   IF Guru.Konstanter:appcon THEN DO:
      RUN DYNLADDATEMP.P PERSISTENT SET laddaproch ON Guru.Konstanter:apphand TRANSACTION DISTINCT
         (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "DEFPRISLISTA", INPUT "").
   END.
   ELSE DO:
      RUN DYNLADDATEMP.P PERSISTENT SET laddaproch
         (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "DEFPRISLISTA", INPUT "").
   END.
   tthandle = TEMP-TABLE prislotemp:HANDLE.
   RUN laddatemp_UI IN laddaproch (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "PRISLISTOVRIGT", INPUT "").
   tthandle = TEMP-TABLE befattningstemp:HANDLE.
   RUN laddatemp_UI IN laddaproch (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "BEFATTNINGSTAB", INPUT "").
   tthandle = TEMP-TABLE prislistfakttemp:HANDLE.
   RUN laddatemp_UI IN laddaproch (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "PRISLISTFAKT", INPUT "").
   tthandle = TEMP-TABLE overprislisttemp:HANDLE.
   RUN laddatemp_UI IN laddaproch (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "OVERPRISLISTA", INPUT "").
   tthandle = TEMP-TABLE overtextfakttemp:HANDLE.
   RUN laddatemp_UI IN laddaproch (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "OVERTEXTFAKT", INPUT "").
   tthandle = TEMP-TABLE defpristemp:HANDLE.   
   RUN laddatemp_UI IN laddaproch (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "DEFPRISLISTA", INPUT "").
   IF Guru.Konstanter:appcon THEN DO:                           
      RUN DEFPRISAPP.P PERSISTENT SET defprisproch ON Guru.Konstanter:apphand TRANSACTION DISTINCT.
   END.
   ELSE DO:
      RUN DEFPRISAPP.P PERSISTENT SET defprisproch.
   END.
   IF Guru.Konstanter:appcon THEN DO:
      RUN FAKKAPP.P PERSISTENT SET fakkoproch ON Guru.Konstanter:apphand TRANSACTION DISTINCT.         
   END.
   ELSE DO:
      RUN FAKKAPP.P PERSISTENT SET fakkoproch.         
   END.
   IF Guru.Konstanter:appcon THEN DO:                           
      RUN EXTRADATAHMT.P PERSISTENT SET edataapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT.
   END.
   ELSE DO:
      RUN EXTRADATAHMT.P PERSISTENT SET edataapph.
   END.                                 
   /*konton*/
   IF Guru.Konstanter:appcon THEN DO:                           
      RUN KTOAPP.P PERSISTENT SET ktoproch ON Guru.Konstanter:apphand TRANSACTION DISTINCT (INPUT Guru.Konstanter:globanv).
   END.
   ELSE DO:
      RUN KTOAPP.P PERSISTENT SET ktoproch (INPUT Guru.Konstanter:globanv).   
   END.
   EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
   CREATE inextradatatemp.
   ASSIGN
   inextradatatemp.PROGRAM = "FRIPRIS"                   
   inextradatatemp.HUVUDCH = ?              
   inextradatatemp.HUVUDINT = ?.      
   RUN etabhamt_UI IN edataapph (INPUT TABLE inextradatatemp,OUTPUT TABLE extradatatemp).
   FOR EACH extradatatemp:
      IF extradatatemp.SOKLOG[1] = TRUE THEN DELETE extradatatemp.
      ELSE extradatatemp.SOKDEC[1] = extradatatemp.SOKINT[5] + extradatatemp.SOKINT[6] / 1000.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE andra_UI WINDOW-1 
PROCEDURE andra_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  
   IF RAD_PRIS = 1 THEN DO:
      BRW_DPRIS:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
      
      defprisid = defpristemp.PRISID.
      RUN DPRISNY.W (INPUT-OUTPUT defprisid). 
      
      RUN openbdynspec_UI IN brwproc[1].
      FIND FIRST defpristemp WHERE defpristemp.PRISID = defprisid NO-LOCK NO-ERROR.
      IF AVAILABLE defpristemp THEN DO: 
         ENABLE BTN_AND BTN_BORT FBTN_SKR FBTN_VPER WITH FRAME {&FRAME-NAME}.
         RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(defpristemp)).
         RUN lastselectdyn_UI IN brwproc[1].
      END.       
      musz = FALSE. 
   END.
   IF RAD_PRIS = 2 THEN DO:

      BRW_FRIP:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
      IF NOT AVAILABLE extradatatemp THEN RETURN.
      EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
      CREATE inextradatatemp.          
      BUFFER-COPY extradatatemp TO inextradatatemp.
      RUN NYPRISF.W (INPUT-OUTPUT TABLE inextradatatemp).
      FIND FIRST inextradatatemp NO-LOCK NO-ERROR.
      IF inextradatatemp.SOKINT[1] NE   extradatatemp.SOKINT[1] THEN inextradatatemp.SOKDATE[3] = TODAY.
      IF inextradatatemp.SOKINT[2] NE   extradatatemp.SOKINT[2] THEN inextradatatemp.SOKDATE[3] = TODAY.
      IF inextradatatemp.SOKINT[3] NE   extradatatemp.SOKINT[3] THEN inextradatatemp.SOKDATE[3] = TODAY.
      IF inextradatatemp.SOKINT[4] NE   extradatatemp.SOKINT[4] THEN inextradatatemp.SOKDATE[3] = TODAY.
      IF inextradatatemp.SOKCHAR[1] NE  extradatatemp.SOKCHAR[1] THEN inextradatatemp.SOKDATE[3] = TODAY.
      IF inextradatatemp.SOKCHAR[2] NE  extradatatemp.SOKCHAR[2] THEN inextradatatemp.SOKDATE[3] = TODAY.
      IF inextradatatemp.SOKCHAR[3] NE  extradatatemp.SOKCHAR[3] THEN inextradatatemp.SOKDATE[3] = TODAY.
      IF inextradatatemp.SOKCHAR[4] NE  extradatatemp.SOKCHAR[4] THEN inextradatatemp.SOKDATE[3] = TODAY.
      IF inextradatatemp.SOKCHAR[5] NE  extradatatemp.SOKCHAR[5] THEN inextradatatemp.SOKDATE[3] = TODAY.
      BUFFER-COPY inextradatatemp TO extradatatemp. 
   END.
   IF RAD_PRIS = 3 THEN DO:
      BRW_OTEXT:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}.
      IF AVAILABLE overtextfakttemp THEN DO:
         overtextid = overtextfakttemp.OTEXTID.
         
         RUN OTEXTNY.W (INPUT-OUTPUT overtextid).
         
         RUN openbdynspec_UI IN brwproc[3].
         FIND FIRST overtextfakttemp WHERE overtextfakttemp.OTEXTID = overtextid NO-LOCK NO-ERROR.
         IF AVAILABLE overtextfakttemp THEN DO:
            RUN setlastrowid_UI IN brwproc[3] (INPUT ROWID(overtextfakttemp)).
            RUN lastselectdyn_UI IN brwproc[3].
         END.
      END.      
      musz = FALSE.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE bort_UI WINDOW-1 
PROCEDURE bort_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   IF RAD_PRIS = 1 THEN DO:
      BRW_DPRIS:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
      IF AVAILABLE defpristemp THEN DO:
         MESSAGE "Vill du ta bort denna post ?"
         VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE defpristemp.PRISTEXT
         UPDATE answer AS LOGICAL.
         IF answer THEN DO:
            defprisid = defpristemp.PRISID.
            RUN tabortdefpris_UI IN defprisproch (INPUT defprisid).
            FIND FIRST defpristemp WHERE defpristemp.PRISID = defprisid NO-LOCK NO-ERROR.
            IF AVAILABLE defpristemp THEN DO:
               DELETE defpristemp.
               RUN selnextprevrow_UI IN brwproc[1].
               RUN openbdynspec_UI IN brwproc[1].
               RUN lastselectdyn_UI IN brwproc[1].
            END.            
         END.   
      END.
   END.
   IF RAD_PRIS = 2 THEN DO:
      BRW_FRIP:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
      IF AVAILABLE extradatatemp THEN DO:
         MESSAGE "Vill du ta bort denna post ?"
         VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE extradatatemp.SOKCHAR[6]
         UPDATE answer1 AS LOGICAL.
         IF answer1 THEN DO:
            EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
            CREATE inextradatatemp.          
            BUFFER-COPY extradatatemp TO inextradatatemp.           
            ASSIGN
            inextradatatemp.SOKDATE[3] = TODAY
            inextradatatemp.SOKLOG[1] = TRUE.
            RUN exbort_UI IN edataapph (INPUT TABLE inextradatatemp).        
            DELETE extradatatemp.
            RUN selnextprevrow_UI IN brwproc[2].
            RUN openbdynspec_UI IN brwproc[2].
            RUN lastselectdyn_UI IN brwproc[2].            
         END.   
      END.
   END.
   IF RAD_PRIS = 3 THEN DO:
      BRW_OTEXT:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}.
      IF AVAILABLE overtextfakttemp THEN DO:
         MESSAGE "Vill du ta bort denna post ?"
         VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE overtextfakttemp.OTEXT
         UPDATE answer2 AS LOGICAL.
         IF answer2 THEN DO:
            {muswait.i}
            overtextid = overtextfakttemp.OTEXTID.
            RUN tabortovertex_UI IN overtexproch
               (INPUT overtextid, OUTPUT felmedd).
            IF felmedd = "" THEN DO:
               FIND FIRST overtextfakttemp WHERE overtextfakttemp.OTEXTID = overtextid NO-LOCK NO-ERROR.
               IF AVAILABLE overtextfakttemp THEN DO:
                  DELETE overtextfakttemp.
                  RUN selnextprevrow_UI IN brwproc[3].
                  RUN openbdynspec_UI IN brwproc[3].
                  RUN lastselectdyn_UI IN brwproc[3].
               END.
            END.
            ELSE MESSAGE felmedd  VIEW-AS ALERT-BOX.
            {musarrow.i}
         END.
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
  DISPLAY RAD_PRIS 
      WITH FRAME FRAME-A IN WINDOW WINDOW-1.
  ENABLE RAD_PRIS BRW_FRIP BRW_DPRIS BRW_OTEXT FBTN_VPER FBTN_SKR FBTN_SPARA 
         BTN_NY BTN_AND BTN_BORT BTN_AVB 
      WITH FRAME FRAME-A IN WINDOW WINDOW-1.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
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
   DEFINE VARIABLE erow AS ROWID NO-UNDO.
   IF RAD_PRIS = 1 THEN DO:
      
      defprisid = "".
      musz = FALSE.
      RUN DPRISNY.W (INPUT-OUTPUT defprisid).
      
      IF musz = TRUE THEN DO: 
         FIND FIRST defpristemp WHERE defpristemp.PRISID = defprisid NO-ERROR.
         IF AVAILABLE defpristemp THEN DELETE defpristemp.
      END.
      ELSE DO:
         RUN openbdynspec_UI IN brwproc[1].
         FIND FIRST defpristemp WHERE defpristemp.PRISID = defprisid NO-LOCK NO-ERROR.
         IF AVAILABLE defpristemp THEN DO: 
            RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(defpristemp)).
            RUN lastselectdyn_UI IN brwproc[1].
         END.                                                          
      END.
      musz = FALSE.    
   END.
   IF RAD_PRIS = 2 THEN DO:
      EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
      CREATE inextradatatemp.          
      ASSIGN
      inextradatatemp.PROGRAM = "FRIPRIS"                   
      inextradatatemp.HUVUDCH = ?              
      inextradatatemp.HUVUDINT = ?.      
      RUN sista_UI IN edataapph (INPUT FALSE,INPUT-OUTPUT TABLE inextradatatemp).
      FIND FIRST inextradatatemp NO-LOCK NO-ERROR.
      CREATE extradatatemp.
      ASSIGN
      extradatatemp.PROGRAM = "FRIPRIS"
      extradatatemp.HUVUDINT = 1
      extradatatemp.SOKDATE[1] = TODAY      
      extradatatemp.SOKCHAR[6] = "Ny post!".      
      IF AVAILABLE inextradatatemp THEN DO:         
         extradatatemp.HUVUDINT = inextradatatemp.HUVUDINT + 1.
      END.    
      ELSE CREATE inextradatatemp.
      BUFFER-COPY extradatatemp TO inextradatatemp.
      RUN sparaextra_UI IN edataapph (INPUT TABLE inextradatatemp).
      erow = ROWID(extradatatemp).
      RUN openbdynspec_UI IN brwproc[2].
      RUN setlastrowid_UI IN brwproc[2] (INPUT erow).
      RUN lastselectdyn_UI IN brwproc[2].
      
   END.
   IF RAD_PRIS = 3 THEN DO:
      overtextid = "".
      
      RUN OTEXTNY.W (INPUT-OUTPUT overtextid).
      
      
      RUN openbdynspec_UI IN brwproc[3].
      FIND FIRST overtextfakttemp WHERE overtextfakttemp.OTEXTID = overtextid NO-LOCK NO-ERROR.
      IF AVAILABLE overtextfakttemp THEN DO:
         RUN setlastrowid_UI IN brwproc[3] (INPUT ROWID(overtextfakttemp)).
         RUN lastselectdyn_UI IN brwproc[3].
      END.                                                    
   END.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE spara_UI WINDOW-1 
PROCEDURE spara_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   FOR EACH extradatatemp:
      ASSIGN
      extradatatemp.SOKINT[5] = TRUNCATE(extradatatemp.SOKDEC[1],0)
      extradatatemp.SOKINT[6] = (extradatatemp.SOKDEC[1] - TRUNCATE(extradatatemp.SOKDEC[1],0)) * 1000.
    /*  extradatatemp.SOKDEC[1] = 0.*/
   END.
   RUN extraspar_UI IN edataapph (INPUT TABLE extradatatemp).  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ut_UI WINDOW-1 
PROCEDURE ut_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/  
   IF RAD_PRIS = 1 THEN DO:
      EMPTY TEMP-TABLE tidut NO-ERROR. 
      CREATE tidut.
      ASSIGN
      SUBSTRING(tidut.UT,1) = "FASTA PRISLISTOR"
      SUBSTRING(tidut.UT,30) = STRING(TODAY).
      CREATE tidut.      
      BRW_DPRIS:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
      IF AVAILABLE defpristemp THEN DO:
         CREATE tidut.              
         ASSIGN
         SUBSTRING(tidut.UT,1) = "PRISLISTA"
         SUBSTRING(tidut.UT,22) = "STARTDATUM".                                      
         CREATE tidut.
         ASSIGN
         SUBSTRING(tidut.UT,1) = STRING(defpristemp.PRISTEXT,"X(20)")
         SUBSTRING(tidut.UT,22) = STRING(defpristemp.STARTDATUM).      
         IF defpristemp.OVER = TRUE THEN DO:
            OPEN QUERY overq FOR EACH overprislisttemp WHERE overprislisttemp.PRISID = defpristemp.PRISID NO-LOCK,
            EACH overtextfakttemp WHERE overtextfakttemp.OTEXTID = overprislisttemp.OTEXTID BY overtextfakttemp.OTEXT.                 
            GET FIRST overq NO-LOCK.
            IF AVAILABLE overprislisttemp THEN DO:
               CREATE tidut.
               SUBSTRING(tidut.UT,25) = "ÖVERTIDSPRISER FÖR :".    
               CREATE tidut.
               ASSIGN 
               SUBSTRING(tidut.UT,11) = "TEXT"   
               SUBSTRING(tidut.UT,32) = "PRIS"
               SUBSTRING(tidut.UT,38) = "DAGAR"
               SUBSTRING(tidut.UT,48) = "START"
               SUBSTRING(tidut.UT,54) = "SLUT". 
               CREATE tidut.
               ASSIGN                           
               SUBSTRING(tidut.UT,11) = "====================.=====.=========.=====.=====".     
            END.
            DO WHILE AVAILABLE(overprislisttemp):
               CREATE tidut.
               ASSIGN                                                                
               SUBSTRING(tidut.UT,11) = overtextfakttemp.OTEXT   
               SUBSTRING(tidut.UT,32) = STRING(overprislisttemp.PRISA,">>>>9")
               SUBSTRING(tidut.UT,38) = overprislisttemp.DAGTYP
               SUBSTRING(tidut.UT,48) = STRING(overprislisttemp.START,">9.99")  
               SUBSTRING(tidut.UT,54) = STRING(overprislisttemp.SLUT,">9.99").       
               GET NEXT overq NO-LOCK.
            END.
         END.
         ELSE DO:  
            OPEN QUERY kundq FOR EACH prislistfakttemp WHERE prislistfakttemp.PRISID = defpristemp.PRISID NO-LOCK,
            EACH befattningstemp WHERE befattningstemp.BEFATTNING = prislistfakttemp.BEFATTNING NO-LOCK.
            GET FIRST kundq NO-LOCK.
            IF AVAILABLE prislistfakttemp THEN DO:
               CREATE tidut.
               SUBSTRING(tidut.UT,25) = "TIMPRISER FÖR :".   
               CREATE tidut. 
               ASSIGN 
               SUBSTRING(tidut.UT,11) = "BEFATTNING"   
               SUBSTRING(tidut.UT,32) = "TIM PRIS"
               SUBSTRING(tidut.UT,42) = "RES PRIS". 
               CREATE tidut.
               ASSIGN                                         
               SUBSTRING(tidut.UT,11) = "====================.=========.=========".                                  
            END.
            DO WHILE AVAILABLE(prislistfakttemp):
               CREATE tidut.
               ASSIGN                                                                
               SUBSTRING(tidut.UT,11) = befattningstemp.NAMN   
               SUBSTRING(tidut.UT,32) = STRING(prislistfakttemp.PRISA,">>>>>>>>9")
               SUBSTRING(tidut.UT,42) = STRING(prislistfakttemp.PRISRES,">>>>>>>>9").                       
               GET NEXT kundq NO-LOCK.
            END.
         END.                                    
         CREATE tidut.      
      END.
      FIND FIRST tidut NO-LOCK NO-ERROR.
      IF NOT AVAILABLE tidut THEN RETURN.
      IF skrivut = FALSE THEN DO:
         {AVBGOM.I}
         RUN UTRAPP.W (INPUT "Prislistor").
         {AVBFRAM.I}
      END.
      ELSE DO:
         IF musz = TRUE THEN musz = FALSE.
         ELSE RUN EKLOGS.P.                  
      END. 
   END.
   IF RAD_PRIS = 2 THEN DO:
      EMPTY TEMP-TABLE tidut NO-ERROR. 
      CREATE tidut.
      ASSIGN
      SUBSTRING(tidut.UT,1) = "Fri prislista"
      SUBSTRING(tidut.UT,30) = STRING(TODAY).
      CREATE tidut.      
      GET FIRST BRW_FRIP.
      /*COLUMN-LABEL "Faxt.text" FORMAT "X(256)":U         
        COLUMN-LABEL "Pris" FORMAT "->>>>>>9.999":U        
        COLUMN-LABEL "Start!datum" FORMAT "99/99/99":U     
        COLUMN-LABEL "Slut!datum" FORMAT "99/99/99":U      
        COLUMN-LABEL "Sortering!Märkning" FORMAT "X(8)":U  */
      FIND FIRST kbtemp NO-ERROR.
      CREATE tidut.      
      ASSIGN
      SUBSTRING(tidut.UT,61) = "KUND"
      SUBSTRING(tidut.UT,67) = "INTÄK"
      SUBSTRING(tidut.UT,73) = "MOTPA"
      SUBSTRING(tidut.UT,79) = "MOMS"

      SUBSTRING(tidut.UT,85) = CAPS(SUBSTRING(kbtemp.K1,1,5))
      SUBSTRING(tidut.UT,91) = CAPS(SUBSTRING(kbtemp.K2,1,5))
      SUBSTRING(tidut.UT,97) = CAPS(SUBSTRING(kbtemp.K3,1,5))
      SUBSTRING(tidut.UT,103) = CAPS(SUBSTRING(kbtemp.K4,1,5))
      SUBSTRING(tidut.UT,109) = CAPS(SUBSTRING(kbtemp.K5,1,5)).
      DO WHILE AVAILABLE(extradatatemp):
         CREATE tidut.
         ASSIGN
         SUBSTRING(tidut.UT,1) = SUBSTRING(extradatatemp.SOKCHAR[6],1,20)        
         SUBSTRING(tidut.UT,22) = STRING(extradatatemp.SOKDEC[1],"->>>>>>9.999").
         IF extradatatemp.SOKDATE[1] NE ? THEN SUBSTRING(tidut.UT,36) = STRING(extradatatemp.SOKDATE[1]). 
         IF extradatatemp.SOKDATE[2] NE ? THEN SUBSTRING(tidut.UT,45) = STRING(extradatatemp.SOKDATE[2]).
         ASSIGN
         SUBSTRING(tidut.UT,55) = SUBSTRING(extradatatemp.SOKCHAR[10],1,5).          
         FIND FIRST kundfodrantemp WHERE kundfodrantemp.KUNDKONTOID = extradatatemp.SOKINT[1] NO-LOCK NO-ERROR.
         IF AVAILABLE kundfodrantemp THEN SUBSTRING(tidut.UT,61) = kundfodrantemp.KUNDKONTO.
         FIND FIRST intakttemp WHERE intakttemp.INTAKTID = extradatatemp.SOKINT[2] NO-LOCK NO-ERROR.
         IF AVAILABLE intakttemp THEN SUBSTRING(tidut.UT,67) = intakttemp.INTAKTKONTO.
         FIND FIRST motparttemp WHERE motparttemp.MOTPARTID = extradatatemp.SOKINT[3] NO-LOCK NO-ERROR.
         IF AVAILABLE motparttemp THEN SUBSTRING(tidut.UT,73) = motparttemp.MOTPART.
         FIND FIRST momstemp WHERE momstemp.MOMSID = extradatatemp.SOKINT[4] NO-LOCK NO-ERROR.
         IF AVAILABLE momstemp THEN SUBSTRING(tidut.UT,79) = momstemp.MOMSKOD.
         ASSIGN
         SUBSTRING(tidut.UT,85) = extradatatemp.SOKCHAR[1]  
         SUBSTRING(tidut.UT,91) = extradatatemp.SOKCHAR[2]  
         SUBSTRING(tidut.UT,97) = extradatatemp.SOKCHAR[3]  
         SUBSTRING(tidut.UT,103) = extradatatemp.SOKCHAR[4]  
         SUBSTRING(tidut.UT,109) = extradatatemp.SOKCHAR[5].  
         GET NEXT BRW_FRIP.
      END.
      FIND FIRST tidut NO-LOCK NO-ERROR.
      IF NOT AVAILABLE tidut THEN RETURN.
      IF skrivut = FALSE THEN DO:
         {AVBGOM.I}
         RUN UTRAPPL.W (INPUT "Prislistor").
         {AVBFRAM.I}
      END.
      ELSE DO:
         IF musz = TRUE THEN musz = FALSE.
         ELSE RUN EKLOGL.P.                  
      END. 
   END.
   IF RAD_PRIS = 3 THEN DO:
   END.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

