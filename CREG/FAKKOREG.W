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
&Scoped-define NEW NEW
DEFINE NEW SHARED VARIABLE fakkoproch AS HANDLE NO-UNDO. /* FAKKAPP.P */
DEFINE NEW SHARED VARIABLE brec AS RECID NO-UNDO.
DEFINE NEW SHARED VARIABLE borec AS RECID NO-UNDO.
DEFINE NEW SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE regdatum AS DATE NO-UNDO.
DEFINE SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.   
DEFINE VARIABLE xpos AS INTEGER NO-UNDO.
DEFINE VARIABLE maxwidth AS INTEGER NO-UNDO.
DEFINE VARIABLE colh AS HANDLE NO-UNDO.
DEFINE VARIABLE valstyrrec AS RECID NO-UNDO.
DEFINE VARIABLE tempi AS INTEGER NO-UNDO.
DEFINE VARIABLE tempwidth AS INTEGER NO-UNDO.

{TIDUTTTNEW.I}  
DEFINE TEMP-TABLE momsraknartemp
   FIELD ROWM AS ROWID
   FIELD MOMSNR AS CHARACTER
   INDEX ROWM ROWM
   INDEX MOMSNR MOMSNR.
&SCOPED-DEFINE NEW NEW
&SCOPED-DEFINE SHARED SHARED
{FAKKOTEMP.I}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE WINDOW
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-A
&Scoped-define BROWSE-NAME BRW_K1

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES kundfodrantemp intakttemp motparttemp ~
momstemp styrkfmtemp

/* Definitions for BROWSE BRW_K1                                        */
&Scoped-define FIELDS-IN-QUERY-BRW_K1 kundfodrantemp.KUNDKONTO ~
kundfodrantemp.KUNDFODTEXT 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_K1 kundfodrantemp.KUNDKONTO ~
kundfodrantemp.KUNDFODTEXT 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_K1 kundfodrantemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_K1 kundfodrantemp
&Scoped-define QUERY-STRING-BRW_K1 FOR EACH kundfodrantemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_K1 OPEN QUERY BRW_K1 FOR EACH kundfodrantemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_K1 kundfodrantemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_K1 kundfodrantemp


/* Definitions for BROWSE BRW_K2                                        */
&Scoped-define FIELDS-IN-QUERY-BRW_K2 intakttemp.INTAKTKONTO ~
intakttemp.INTAKTTEXT 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_K2 intakttemp.INTAKTKONTO ~
intakttemp.INTAKTTEXT 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_K2 intakttemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_K2 intakttemp
&Scoped-define QUERY-STRING-BRW_K2 FOR EACH intakttemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_K2 OPEN QUERY BRW_K2 FOR EACH intakttemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_K2 intakttemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_K2 intakttemp


/* Definitions for BROWSE BRW_K3                                        */
&Scoped-define FIELDS-IN-QUERY-BRW_K3 motparttemp.MOTPART ~
motparttemp.MOTPARTTEXT 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_K3 motparttemp.MOTPART ~
motparttemp.MOTPARTTEXT 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_K3 motparttemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_K3 motparttemp
&Scoped-define QUERY-STRING-BRW_K3 FOR EACH motparttemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_K3 OPEN QUERY BRW_K3 FOR EACH motparttemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_K3 motparttemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_K3 motparttemp


/* Definitions for BROWSE BRW_K4                                        */
&Scoped-define FIELDS-IN-QUERY-BRW_K4 momstemp.MOMSKOD momstemp.MOMSTEXT ~
momstemp.MOMSEXTERNT momstemp.MOMSNR 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_K4 momstemp.MOMSKOD ~
momstemp.MOMSTEXT momstemp.MOMSEXTERNT momstemp.MOMSNR 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_K4 momstemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_K4 momstemp
&Scoped-define QUERY-STRING-BRW_K4 FOR EACH momstemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_K4 OPEN QUERY BRW_K4 FOR EACH momstemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_K4 momstemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_K4 momstemp


/* Definitions for BROWSE BRW_KSDYN                                     */
&Scoped-define FIELDS-IN-QUERY-BRW_KSDYN styrkfmtemp.KUNDTYP ~
styrkfmtemp.KUNDFODTEXT styrkfmtemp.KUNDKONTO styrkfmtemp.FAKTTYPTEXT ~
styrkfmtemp.MOMSKOD styrkfmtemp.INTAKTKONTO styrkfmtemp.MOTPART 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_KSDYN 
&Scoped-define QUERY-STRING-BRW_KSDYN FOR EACH styrkfmtemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_KSDYN OPEN QUERY BRW_KSDYN FOR EACH styrkfmtemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_KSDYN styrkfmtemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_KSDYN styrkfmtemp


/* Definitions for FRAME FRAME-A                                        */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-47 RAD_KONTO BTN_NVE-3 FILL-IN_FDATUM ~
BTN_FVE-3 BTN_NVE-4 FILL-IN_TDATUM BTN_FVE-4 FBTN_VPER FBTN_SKR BRW_KSDYN ~
BRW_K4 BRW_K1 BRW_K3 BRW_K2 BTN_BORT BTN_AVB BTN_NY BTN_UPP FILL-IN-GODK 
&Scoped-Define DISPLAYED-OBJECTS RAD_KONTO FILL-IN_FDATUM FILL-IN_TDATUM ~
FILL-IN-GODK 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR WINDOW-1 AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB 
     LABEL "Avsluta":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_BORT 
     LABEL "Ta bort":L 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_FVE-3 
     LABEL "-" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_FVE-4 
     LABEL "-" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_NVE-3 
     LABEL "+" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_NVE-4 
     LABEL "+" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_NY 
     LABEL "Ny":L 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_UPP 
     LABEL "Ändra":L 
     SIZE 12 BY 1.

DEFINE BUTTON FBTN_SKR 
     LABEL "Skriv ut":L 
     SIZE 14 BY 1.

DEFINE BUTTON FBTN_VPER 
     LABEL "Visa":L 
     SIZE 14 BY 1.

DEFINE VARIABLE FILL-IN-GODK AS CHARACTER FORMAT "X(256)":U INITIAL "Godkänd bokföringsperiod:" 
      VIEW-AS TEXT 
     SIZE 28.25 BY .63
     FONT 17 NO-UNDO.

DEFINE VARIABLE FILL-IN_FDATUM AS DATE FORMAT "99/99/99" INITIAL 09/19/03 
     LABEL "Från och med" 
     VIEW-AS FILL-IN 
     SIZE 9 BY .92.

DEFINE VARIABLE FILL-IN_TDATUM AS DATE FORMAT "99/99/99" INITIAL 09/19/03 
     LABEL "Till och med" 
     VIEW-AS FILL-IN 
     SIZE 9 BY .92.

DEFINE VARIABLE RAD_KONTO AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "", 1
     SIZE 26.88 BY 9.17 NO-UNDO.

DEFINE RECTANGLE RECT-47
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 52.5 BY 6.08.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_K1 FOR 
      kundfodrantemp SCROLLING.

DEFINE QUERY BRW_K2 FOR 
      intakttemp SCROLLING.

DEFINE QUERY BRW_K3 FOR 
      motparttemp SCROLLING.

DEFINE QUERY BRW_K4 FOR 
      momstemp SCROLLING.

DEFINE QUERY BRW_KSDYN FOR 
      styrkfmtemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_K1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_K1 WINDOW-1 _STRUCTURED
  QUERY BRW_K1 NO-LOCK DISPLAY
      kundfodrantemp.KUNDKONTO COLUMN-LABEL "Kundfodrans!konto" FORMAT "X(6)":U
      kundfodrantemp.KUNDFODTEXT COLUMN-LABEL "Text" FORMAT "X(35)":U
  ENABLE
      kundfodrantemp.KUNDKONTO
      kundfodrantemp.KUNDFODTEXT
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SIZE 52.13 BY 13.29.

DEFINE BROWSE BRW_K2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_K2 WINDOW-1 _STRUCTURED
  QUERY BRW_K2 NO-LOCK DISPLAY
      intakttemp.INTAKTKONTO COLUMN-LABEL "Intäktkonto" FORMAT "X(6)":U
      intakttemp.INTAKTTEXT COLUMN-LABEL "Text" FORMAT "X(35)":U
  ENABLE
      intakttemp.INTAKTKONTO
      intakttemp.INTAKTTEXT
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SIZE 52.13 BY 13.29.

DEFINE BROWSE BRW_K3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_K3 WINDOW-1 _STRUCTURED
  QUERY BRW_K3 NO-LOCK DISPLAY
      motparttemp.MOTPART COLUMN-LABEL "Motpart" FORMAT "X(6)":U
      motparttemp.MOTPARTTEXT COLUMN-LABEL "Text" FORMAT "X(35)":U
  ENABLE
      motparttemp.MOTPART
      motparttemp.MOTPARTTEXT
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SIZE 52.13 BY 13.29.

DEFINE BROWSE BRW_K4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_K4 WINDOW-1 _STRUCTURED
  QUERY BRW_K4 NO-LOCK DISPLAY
      momstemp.MOMSKOD COLUMN-LABEL "Moms!konto" FORMAT "X(6)":U
      momstemp.MOMSTEXT COLUMN-LABEL "Text" FORMAT "X(35)":U
      momstemp.MOMSEXTERNT COLUMN-LABEL "Moms!i procent" FORMAT ">>9.99":U
      momstemp.MOMSNR COLUMN-LABEL "Moms!kod" FORMAT "X(4)":U
  ENABLE
      momstemp.MOMSKOD
      momstemp.MOMSTEXT
      momstemp.MOMSEXTERNT
      momstemp.MOMSNR
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SIZE 66.88 BY 13.29.

DEFINE BROWSE BRW_KSDYN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_KSDYN WINDOW-1 _STRUCTURED
  QUERY BRW_KSDYN NO-LOCK DISPLAY
      styrkfmtemp.KUNDTYP COLUMN-LABEL "Kundtyp" FORMAT "X(20)":U
      styrkfmtemp.KUNDFODTEXT COLUMN-LABEL "Text" FORMAT "X(35)":U
      styrkfmtemp.KUNDKONTO COLUMN-LABEL "Kundfodrans!konto" FORMAT "X(6)":U
      styrkfmtemp.FAKTTYPTEXT COLUMN-LABEL "Faktureringstyp" FORMAT "X(45)":U
      styrkfmtemp.MOMSKOD COLUMN-LABEL "Moms!konto" FORMAT "X(6)":U
      styrkfmtemp.INTAKTKONTO COLUMN-LABEL "Intäkts!konto" FORMAT "X(6)":U
      styrkfmtemp.MOTPART COLUMN-LABEL "Motpart" FORMAT "X(6)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 83 BY 13.29.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     RAD_KONTO AT ROW 1.5 COL 1.5 NO-LABEL
     BTN_NVE-3 AT ROW 2.83 COL 71.5
     FILL-IN_FDATUM AT ROW 3.25 COL 59 COLON-ALIGNED
     BTN_FVE-3 AT ROW 3.83 COL 71.5
     BTN_NVE-4 AT ROW 5.04 COL 71.5
     FILL-IN_TDATUM AT ROW 5.46 COL 59 COLON-ALIGNED
     BTN_FVE-4 AT ROW 6.08 COL 71.5
     FBTN_VPER AT ROW 8 COL 91
     FBTN_SKR AT ROW 9.1 COL 91
     BRW_KSDYN AT ROW 11.33 COL 1.5
     BRW_K4 AT ROW 11.33 COL 5.13
     BRW_K1 AT ROW 11.33 COL 13.13
     BRW_K3 AT ROW 11.33 COL 13.25
     BRW_K2 AT ROW 11.33 COL 13.38
     BTN_BORT AT ROW 25.21 COL 47
     BTN_AVB AT ROW 25.21 COL 91
     BTN_NY AT ROW 25.25 COL 19
     BTN_UPP AT ROW 25.25 COL 33
     FILL-IN-GODK AT ROW 1.63 COL 30.5 COLON-ALIGNED NO-LABEL
     RECT-47 AT ROW 1.13 COL 30.75
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 104.63 BY 25.67.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: WINDOW
   Temp-Tables and Buffers:
      TABLE: intakttemp T "?" NO-UNDO temp-db intakttemp
      TABLE: kundfodrantemp T "?" NO-UNDO temp-db kundfodrantemp
      TABLE: momstemp T "?" NO-UNDO temp-db momstemp
      TABLE: motparttemp T "?" NO-UNDO temp-db motparttemp
      TABLE: styrkfmtemp T "?" NO-UNDO temp-db styrkfmtemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW WINDOW-1 ASSIGN
         HIDDEN             = YES
         TITLE              = "Konton för fakturering"
         HEIGHT             = 25.75
         WIDTH              = 104.88
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
/* SETTINGS FOR WINDOW WINDOW-1
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME FRAME-A
                                                                        */
/* BROWSE-TAB BRW_KSDYN FBTN_SKR FRAME-A */
/* BROWSE-TAB BRW_K4 BRW_KSDYN FRAME-A */
/* BROWSE-TAB BRW_K1 BRW_K4 FRAME-A */
/* BROWSE-TAB BRW_K3 BRW_K1 FRAME-A */
/* BROWSE-TAB BRW_K2 BRW_K3 FRAME-A */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-1)
THEN WINDOW-1:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_K1
/* Query rebuild information for BROWSE BRW_K1
     _TblList          = "Temp-Tables.kundfodrantemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.kundfodrantemp.KUNDKONTO
"kundfodrantemp.KUNDKONTO" "Kundfodrans!konto" "X(6)" "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.kundfodrantemp.KUNDFODTEXT
"kundfodrantemp.KUNDFODTEXT" "Text" "X(35)" "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_K1 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_K2
/* Query rebuild information for BROWSE BRW_K2
     _TblList          = "Temp-Tables.intakttemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.intakttemp.INTAKTKONTO
"intakttemp.INTAKTKONTO" "Intäktkonto" "X(6)" "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.intakttemp.INTAKTTEXT
"intakttemp.INTAKTTEXT" "Text" "X(35)" "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_K2 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_K3
/* Query rebuild information for BROWSE BRW_K3
     _TblList          = "Temp-Tables.motparttemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.motparttemp.MOTPART
"motparttemp.MOTPART" "Motpart" "X(6)" "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.motparttemp.MOTPARTTEXT
"motparttemp.MOTPARTTEXT" "Text" "X(35)" "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_K3 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_K4
/* Query rebuild information for BROWSE BRW_K4
     _TblList          = "Temp-Tables.momstemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.momstemp.MOMSKOD
"momstemp.MOMSKOD" "Moms!konto" "X(6)" "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.momstemp.MOMSTEXT
"momstemp.MOMSTEXT" "Text" "X(35)" "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.momstemp.MOMSEXTERNT
"momstemp.MOMSEXTERNT" "Moms!i procent" ? "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[4]   > Temp-Tables.momstemp.MOMSNR
"momstemp.MOMSNR" "Moms!kod" "X(4)" "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_K4 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_KSDYN
/* Query rebuild information for BROWSE BRW_KSDYN
     _TblList          = "Temp-Tables.styrkfmtemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.styrkfmtemp.KUNDTYP
"styrkfmtemp.KUNDTYP" "Kundtyp" "X(20)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.styrkfmtemp.KUNDFODTEXT
"styrkfmtemp.KUNDFODTEXT" "Text" "X(35)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.styrkfmtemp.KUNDKONTO
"styrkfmtemp.KUNDKONTO" "Kundfodrans!konto" "X(6)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[4]   > Temp-Tables.styrkfmtemp.FAKTTYPTEXT
"styrkfmtemp.FAKTTYPTEXT" "Faktureringstyp" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[5]   > Temp-Tables.styrkfmtemp.MOMSKOD
"styrkfmtemp.MOMSKOD" "Moms!konto" "X(6)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[6]   > Temp-Tables.styrkfmtemp.INTAKTKONTO
"styrkfmtemp.INTAKTKONTO" "Intäkts!konto" "X(6)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[7]   > Temp-Tables.styrkfmtemp.MOTPART
"styrkfmtemp.MOTPART" "Motpart" "X(6)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_KSDYN */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-A
/* Query rebuild information for FRAME FRAME-A
     _Options          = "NO-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME FRAME-A */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME BRW_K1
&Scoped-define SELF-NAME BRW_K1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_K1 WINDOW-1
ON ROW-LEAVE OF BRW_K1 IN FRAME FRAME-A
DO:
   IF AVAILABLE kundfodrantemp THEN DO:
      DISPLAY kundfodrantemp.KUNDKONTO kundfodrantemp.KUNDFODTEXT WITH BROWSE BRW_K1.
      ASSIGN
      kundfodrantemp.KUNDKONTO = INPUT BROWSE BRW_K1 kundfodrantemp.KUNDKONTO
      kundfodrantemp.KUNDFODTEXT = INPUT BROWSE BRW_K1 kundfodrantemp.KUNDFODTEXT.
      IF kundfodrantemp.KUNDKONTO = "" THEN DO:
         MESSAGE "Kontot kan inte vara blankt!" VIEW-AS ALERT-BOX.
         /*
         RUN setlastrowid_UI IN brwproc[4] (INPUT ROWID(momstemp)).              
         RUN lastselectdyn_UI IN brwproc[4].            
         */
         RETURN NO-APPLY.
      END.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME kundfodrantemp.KUNDKONTO
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL kundfodrantemp.KUNDKONTO BRW_K1 _BROWSE-COLUMN WINDOW-1
ON ENTRY OF kundfodrantemp.KUNDKONTO IN BROWSE BRW_K1 /* Kundfodrans!konto */
DO:
   IF AVAILABLE kundfodrantemp THEN 
   DISPLAY kundfodrantemp.KUNDKONTO kundfodrantemp.KUNDFODTEXT WITH BROWSE BRW_K1.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL kundfodrantemp.KUNDKONTO BRW_K1 _BROWSE-COLUMN WINDOW-1
ON LEAVE OF kundfodrantemp.KUNDKONTO IN BROWSE BRW_K1 /* Kundfodrans!konto */
DO:
   IF AVAILABLE kundfodrantemp THEN DO:
      kundfodrantemp.KUNDKONTO = INPUT BROWSE BRW_K1 kundfodrantemp.KUNDKONTO.
      DISPLAY kundfodrantemp.KUNDKONTO WITH BROWSE BRW_K1 NO-ERROR.   
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME kundfodrantemp.KUNDFODTEXT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL kundfodrantemp.KUNDFODTEXT BRW_K1 _BROWSE-COLUMN WINDOW-1
ON ENTRY OF kundfodrantemp.KUNDFODTEXT IN BROWSE BRW_K1 /* Text */
DO:
   IF AVAILABLE kundfodrantemp THEN 
   DISPLAY kundfodrantemp.KUNDKONTO kundfodrantemp.KUNDFODTEXT WITH BROWSE BRW_K1.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL kundfodrantemp.KUNDFODTEXT BRW_K1 _BROWSE-COLUMN WINDOW-1
ON LEAVE OF kundfodrantemp.KUNDFODTEXT IN BROWSE BRW_K1 /* Text */
DO:
   IF AVAILABLE kundfodrantemp THEN DO:
      kundfodrantemp.KUNDFODTEXT = INPUT BROWSE BRW_K1 kundfodrantemp.KUNDFODTEXT.
      DISPLAY kundfodrantemp.KUNDFODTEXT WITH BROWSE BRW_K1 NO-ERROR.   
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_K2
&Scoped-define SELF-NAME BRW_K2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_K2 WINDOW-1
ON ROW-LEAVE OF BRW_K2 IN FRAME FRAME-A
DO:
   intakttemp.INTAKTKONTO = INPUT BROWSE BRW_K2 intakttemp.INTAKTKONTO.
   IF intakttemp.INTAKTKONTO = "" THEN DO:
      MESSAGE "Kontot kan inte vara blankt!" VIEW-AS ALERT-BOX.
      /*
      RUN setlastrowid_UI IN brwproc[4] (INPUT ROWID(momstemp)).              
      RUN lastselectdyn_UI IN brwproc[4].            
      */
      RETURN NO-APPLY.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME intakttemp.INTAKTKONTO
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL intakttemp.INTAKTKONTO BRW_K2 _BROWSE-COLUMN WINDOW-1
ON ENTRY OF intakttemp.INTAKTKONTO IN BROWSE BRW_K2 /* Intäktkonto */
DO:
   IF AVAILABLE intakttemp THEN 
   DISPLAY intakttemp.INTAKTKONTO intakttemp.INTAKTTEXT WITH BROWSE BRW_K2 NO-ERROR. 
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL intakttemp.INTAKTKONTO BRW_K2 _BROWSE-COLUMN WINDOW-1
ON LEAVE OF intakttemp.INTAKTKONTO IN BROWSE BRW_K2 /* Intäktkonto */
DO:
   IF AVAILABLE intakttemp THEN DO:
      intakttemp.INTAKTKONTO = INPUT BROWSE BRW_K2 intakttemp.INTAKTKONTO.
      DISPLAY intakttemp.INTAKTKONTO WITH BROWSE BRW_K2 NO-ERROR.   
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME intakttemp.INTAKTTEXT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL intakttemp.INTAKTTEXT BRW_K2 _BROWSE-COLUMN WINDOW-1
ON ENTRY OF intakttemp.INTAKTTEXT IN BROWSE BRW_K2 /* Text */
DO:
   IF AVAILABLE intakttemp THEN 
   DISPLAY intakttemp.INTAKTKONTO intakttemp.INTAKTTEXT WITH BROWSE BRW_K2 NO-ERROR. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL intakttemp.INTAKTTEXT BRW_K2 _BROWSE-COLUMN WINDOW-1
ON LEAVE OF intakttemp.INTAKTTEXT IN BROWSE BRW_K2 /* Text */
DO:
   IF AVAILABLE intakttemp THEN DO:
      intakttemp.INTAKTTEXT = INPUT BROWSE BRW_K2 intakttemp.INTAKTTEXT.
      DISPLAY intakttemp.INTAKTTEXT WITH BROWSE BRW_K2 NO-ERROR.   
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_K3
&Scoped-define SELF-NAME BRW_K3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_K3 WINDOW-1
ON ROW-LEAVE OF BRW_K3 IN FRAME FRAME-A
DO:
   motparttemp.MOTPART = INPUT BROWSE BRW_K3 motparttemp.MOTPART.
   IF motparttemp.MOTPART = "" THEN DO:
      MESSAGE "Kontot kan inte vara blankt!" VIEW-AS ALERT-BOX.
      /*
      RUN setlastrowid_UI IN brwproc[4] (INPUT ROWID(momstemp)).              
      RUN lastselectdyn_UI IN brwproc[4].            
      */
      RETURN NO-APPLY.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME motparttemp.MOTPART
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL motparttemp.MOTPART BRW_K3 _BROWSE-COLUMN WINDOW-1
ON ENTRY OF motparttemp.MOTPART IN BROWSE BRW_K3 /* Motpart */
DO:
   IF AVAILABLE motparttemp THEN 
   DISPLAY motparttemp.MOTPART motparttemp.MOTPARTTEXT WITH BROWSE BRW_K3 NO-ERROR. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL motparttemp.MOTPART BRW_K3 _BROWSE-COLUMN WINDOW-1
ON LEAVE OF motparttemp.MOTPART IN BROWSE BRW_K3 /* Motpart */
DO:
   IF AVAILABLE motparttemp THEN DO:
      motparttemp.MOTPART = INPUT BROWSE BRW_K3 motparttemp.MOTPART.
      DISPLAY motparttemp.MOTPART WITH BROWSE BRW_K3 NO-ERROR.   
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME motparttemp.MOTPARTTEXT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL motparttemp.MOTPARTTEXT BRW_K3 _BROWSE-COLUMN WINDOW-1
ON ENTRY OF motparttemp.MOTPARTTEXT IN BROWSE BRW_K3 /* Text */
DO:
   IF AVAILABLE motparttemp THEN 
   DISPLAY motparttemp.MOTPART motparttemp.MOTPARTTEXT WITH BROWSE BRW_K3 NO-ERROR. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL motparttemp.MOTPARTTEXT BRW_K3 _BROWSE-COLUMN WINDOW-1
ON LEAVE OF motparttemp.MOTPARTTEXT IN BROWSE BRW_K3 /* Text */
DO:
   IF AVAILABLE motparttemp THEN DO:
      motparttemp.MOTPARTTEXT = INPUT BROWSE BRW_K3 motparttemp.MOTPARTTEXT.
      DISPLAY motparttemp.MOTPARTTEXT WITH BROWSE BRW_K3 NO-ERROR.   
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_K4
&Scoped-define SELF-NAME BRW_K4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_K4 WINDOW-1
ON ROW-LEAVE OF BRW_K4 IN FRAME FRAME-A
DO:
   
   ASSIGN
   momstemp.MOMSKOD = INPUT BROWSE BRW_K4 momstemp.MOMSKOD.
   momstemp.MOMSNR = INPUT BROWSE BRW_K4 momstemp.MOMSNR.
   DISPLAY momstemp.MOMSKOD momstemp.MOMSNR WITH BROWSE BRW_K4 NO-ERROR. 
   IF momstemp.MOMSNR = momstemp.MOMSKOD THEN DO:
      MESSAGE "Momskoden kan inte vara lika som momskontot!" VIEW-AS ALERT-BOX.
      /*
      RUN setlastrowid_UI IN brwproc[4] (INPUT ROWID(momstemp)).              
      RUN lastselectdyn_UI IN brwproc[4].            
      */
      RETURN NO-APPLY.

   END.
   IF momstemp.MOMSNR = "" THEN DO:
      MESSAGE "Momskoden kan inte vara blank!" VIEW-AS ALERT-BOX.
      /*
      RUN setlastrowid_UI IN brwproc[4] (INPUT ROWID(momstemp)).              
      RUN lastselectdyn_UI IN brwproc[4].            
      */
      RETURN NO-APPLY.

   END.
   FIND FIRST momsraknartemp WHERE momsraknartemp.ROWM NE ROWID(momstemp) AND momsraknartemp.MOMSNR = momstemp.MOMSNR NO-LOCK NO-ERROR.
   IF NOT AVAILABLE momsraknartemp THEN DO:
      FIND FIRST momsraknartemp WHERE momsraknartemp.ROWM = ROWID(momstemp) NO-LOCK NO-ERROR.
      IF NOT AVAILABLE  momsraknartemp THEN CREATE  momsraknartemp.
      ASSIGN
      momsraknartemp.ROWM = ROWID(momstemp)
      momsraknartemp.MOMSNR = momstemp.MOMSNR.
   END.     
   ELSE DO:
      MESSAGE "Momskoden finns redan!" VIEW-AS ALERT-BOX.
      /*
      RUN setlastrowid_UI IN brwproc[4] (INPUT ROWID(momstemp)).              
      RUN lastselectdyn_UI IN brwproc[4].            
      */
      RETURN NO-APPLY.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME momstemp.MOMSKOD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL momstemp.MOMSKOD BRW_K4 _BROWSE-COLUMN WINDOW-1
ON ENTRY OF momstemp.MOMSKOD IN BROWSE BRW_K4 /* Moms!konto */
DO:
   IF AVAILABLE momstemp THEN 
   DISPLAY momstemp.MOMSEXTERNT momstemp.MOMSKOD momstemp.MOMSNR momstemp.MOMSTEXT WITH BROWSE BRW_K4.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL momstemp.MOMSKOD BRW_K4 _BROWSE-COLUMN WINDOW-1
ON LEAVE OF momstemp.MOMSKOD IN BROWSE BRW_K4 /* Moms!konto */
DO:
   IF AVAILABLE momstemp THEN DO:
      momstemp.MOMSKOD = INPUT BROWSE BRW_K4 momstemp.MOMSKOD.
      DISPLAY momstemp.MOMSKOD WITH BROWSE BRW_K4 NO-ERROR.   
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME momstemp.MOMSTEXT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL momstemp.MOMSTEXT BRW_K4 _BROWSE-COLUMN WINDOW-1
ON ENTRY OF momstemp.MOMSTEXT IN BROWSE BRW_K4 /* Text */
DO:
   IF AVAILABLE momstemp THEN 
   DISPLAY momstemp.MOMSEXTERNT momstemp.MOMSKOD momstemp.MOMSNR momstemp.MOMSTEXT WITH BROWSE BRW_K4.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL momstemp.MOMSTEXT BRW_K4 _BROWSE-COLUMN WINDOW-1
ON LEAVE OF momstemp.MOMSTEXT IN BROWSE BRW_K4 /* Text */
DO:
   IF AVAILABLE momstemp THEN DO:
      momstemp.MOMSTEXT = INPUT BROWSE BRW_K4 momstemp.MOMSTEXT.
      DISPLAY momstemp.MOMSTEXT WITH BROWSE BRW_K4 NO-ERROR.   
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME momstemp.MOMSEXTERNT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL momstemp.MOMSEXTERNT BRW_K4 _BROWSE-COLUMN WINDOW-1
ON ENTRY OF momstemp.MOMSEXTERNT IN BROWSE BRW_K4 /* Moms!i procent */
DO:
   IF AVAILABLE momstemp THEN 
   DISPLAY momstemp.MOMSEXTERNT momstemp.MOMSKOD momstemp.MOMSNR momstemp.MOMSTEXT WITH BROWSE BRW_K4.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL momstemp.MOMSEXTERNT BRW_K4 _BROWSE-COLUMN WINDOW-1
ON LEAVE OF momstemp.MOMSEXTERNT IN BROWSE BRW_K4 /* Moms!i procent */
DO:
   IF AVAILABLE momstemp THEN DO:
      momstemp.MOMSEXTERNT = INPUT BROWSE BRW_K4 momstemp.MOMSEXTERNT.
      DISPLAY momstemp.MOMSEXTERNT WITH BROWSE BRW_K4 NO-ERROR.   
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME momstemp.MOMSNR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL momstemp.MOMSNR BRW_K4 _BROWSE-COLUMN WINDOW-1
ON ENTRY OF momstemp.MOMSNR IN BROWSE BRW_K4 /* Moms!kod */
DO:
   IF AVAILABLE momstemp THEN 
   DISPLAY momstemp.MOMSEXTERNT momstemp.MOMSKOD momstemp.MOMSNR momstemp.MOMSTEXT WITH BROWSE BRW_K4.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL momstemp.MOMSNR BRW_K4 _BROWSE-COLUMN WINDOW-1
ON LEAVE OF momstemp.MOMSNR IN BROWSE BRW_K4 /* Moms!kod */
DO:
   IF AVAILABLE momstemp THEN DO:
      momstemp.MOMSNR = INPUT BROWSE BRW_K4 momstemp.MOMSNR.
      DISPLAY momstemp.MOMSNR WITH BROWSE BRW_K4 NO-ERROR.   
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB WINDOW-1
ON CHOOSE OF BTN_AVB IN FRAME FRAME-A /* Avsluta */
DO:
   APPLY "CLOSE" TO THIS-PROCEDURE.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_BORT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_BORT WINDOW-1
ON CHOOSE OF BTN_BORT IN FRAME FRAME-A /* Ta bort */
DO:
   RUN bort_UI.
   {musarrow.i}  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_FVE-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_FVE-3 WINDOW-1
ON CHOOSE OF BTN_FVE-3 IN FRAME FRAME-A /* - */
DO: 
   FILL-IN_FDATUM = INPUT FILL-IN_FDATUM - 1.
   DISPLAY FILL-IN_FDATUM WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_FVE-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_FVE-4 WINDOW-1
ON CHOOSE OF BTN_FVE-4 IN FRAME FRAME-A /* - */
DO:   
   FILL-IN_TDATUM = INPUT FILL-IN_TDATUM - 1.
   DISPLAY FILL-IN_TDATUM WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NVE-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NVE-3 WINDOW-1
ON CHOOSE OF BTN_NVE-3 IN FRAME FRAME-A /* + */
DO:   
   FILL-IN_FDATUM = INPUT FILL-IN_FDATUM + 1. 
   DISPLAY FILL-IN_FDATUM WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NVE-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NVE-4 WINDOW-1
ON CHOOSE OF BTN_NVE-4 IN FRAME FRAME-A /* + */
DO:   
   FILL-IN_TDATUM = INPUT FILL-IN_TDATUM + 1.
   DISPLAY FILL-IN_TDATUM WITH FRAME {&FRAME-NAME}.
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


&Scoped-define SELF-NAME BTN_UPP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_UPP WINDOW-1
ON CHOOSE OF BTN_UPP IN FRAME FRAME-A /* Ändra */
DO:
   RUN andra_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_SKR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_SKR WINDOW-1
ON CHOOSE OF FBTN_SKR IN FRAME FRAME-A /* Skriv ut */
DO:    
   {muswait.i}
   skrivut = TRUE.  
   RUN SKRIVVAL.W (INPUT FALSE).
   IF musz = TRUE THEN DO:
      musz = FALSE.
      skrivut = FALSE.  
   END.
   ELSE DO:
      IF RAD_KONTO < 5 THEN RUN ut_UI. 
      ELSE RUN styrut_UI.
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


&Scoped-define SELF-NAME FBTN_VPER
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_VPER WINDOW-1
ON CHOOSE OF FBTN_VPER IN FRAME FRAME-A /* Visa */
DO:
   skrivut = FALSE.  
   IF RAD_KONTO < 5 THEN RUN ut_UI. 
   ELSE RUN styrut_UI.  
   {musarrow.i}   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_FDATUM
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_FDATUM WINDOW-1
ON MOUSE-MENU-CLICK OF FILL-IN_FDATUM IN FRAME FRAME-A /* Från och med */
DO:
   Guru.GlobalaVariabler:regdatum = INPUT FILL-IN_FDATUM.
   RUN AlmanBtn.w.
   FILL-IN_FDATUM = Guru.GlobalaVariabler:regdatum.   
   DISPLAY FILL-IN_FDATUM WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_TDATUM
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_TDATUM WINDOW-1
ON MOUSE-MENU-CLICK OF FILL-IN_TDATUM IN FRAME FRAME-A /* Till och med */
DO:
   Guru.GlobalaVariabler:regdatum = INPUT FILL-IN_TDATUM.
   RUN AlmanBtn.w.
   FILL-IN_TDATUM = Guru.GlobalaVariabler:regdatum.
   DISPLAY FILL-IN_TDATUM WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RAD_KONTO
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RAD_KONTO WINDOW-1
ON VALUE-CHANGED OF RAD_KONTO IN FRAME FRAME-A
DO:  
   {muswait.i}
   ASSIGN
   RAD_KONTO = INPUT RAD_KONTO
   BRW_K1:HIDDEN = TRUE
   BRW_K2:HIDDEN = TRUE   
   BRW_K3:HIDDEN = TRUE      
   BRW_K4:HIDDEN = TRUE
   BRW_KSDYN:HIDDEN = TRUE
   BTN_UPP:HIDDEN = TRUE
   BTN_BORT:HIDDEN = TRUE
   FBTN_SKR:HIDDEN = TRUE
   FBTN_VPER:HIDDEN = TRUE.
   IF RAD_KONTO < 5 THEN DO:
      /*FILL-IN-TXT:SCREEN-VALUE = "Konto:".*/
      RUN sethidden_UI IN brwproc[RAD_KONTO] (INPUT FALSE).
      RUN openbdynspec_UI IN brwproc[RAD_KONTO].         
      RUN getnumresult_UI IN brwproc[RAD_KONTO] (OUTPUT tempi).
      IF RAD_KONTO = 1 THEN DO:
         GET FIRST BRW_K1.
         IF AVAILABLE kundfodrantemp THEN DO:
            APPLY "ENTRY" TO kundfodrantemp.KUNDKONTO IN BROWSE BRW_K1.
         END.
      END.
      IF RAD_KONTO = 2 THEN DO:
         GET FIRST BRW_K2.
         IF AVAILABLE intakttemp THEN DO:
            APPLY "ENTRY" TO intakttemp.INTAKTKONTO IN BROWSE BRW_K2.
         END.
      END.
      IF RAD_KONTO = 3 THEN DO:
         GET FIRST BRW_K3.
         IF AVAILABLE motparttemp THEN DO:
            APPLY "ENTRY" TO motparttemp.MOTPART IN BROWSE BRW_K3.
         END.
      END.
      IF RAD_KONTO = 4 THEN DO:
         GET FIRST BRW_K4.
         IF AVAILABLE momstemp THEN DO:
            APPLY "ENTRY" TO momstemp.MOMSKOD IN BROWSE BRW_K4.
         END.
      END.
      IF tempi NE 0 THEN DO:
         ASSIGN         
         BTN_BORT:HIDDEN IN FRAME {&FRAME-NAME} = FALSE 
         FBTN_SKR:HIDDEN IN FRAME {&FRAME-NAME} = FALSE 
         FBTN_VPER:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
      END.
   END.
   ELSE DO:
      IF RAD_KONTO = 5 THEN RUN brwfix_UI(INPUT "1,2,3").
      ELSE IF RAD_KONTO = 6 THEN RUN brwfix_UI(INPUT "1,4,5,6").
      ELSE IF RAD_KONTO = 7 THEN RUN brwfix_UI(INPUT "1,7").
      ELSE IF RAD_KONTO = 8 THEN RUN brwfix_UI(INPUT "1,4,5").
      ASSIGN
      /*FILL-IN-TXT:SCREEN-VALUE = "Styrning:"*/
      BRW_KSDYN:HIDDEN = FALSE.   
      
      RUN setcolsortvar_UI IN brwproc[5] (INPUT "styrkfmtemp.STYRVAL = " + STRING(RAD_KONTO)) .
      RUN openbdynspec_UI IN brwproc[5].         
      GET FIRST BRW_KSDYN NO-LOCK.
      IF AVAILABLE styrkfmtemp THEN DO:
         APPLY "HOME" TO BRW_KSDYN.
         BRW_KSDYN:SELECT-FOCUSED-ROW().
         ASSIGN
         BTN_UPP:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
         BTN_BORT:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
         FBTN_SKR:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
         FBTN_VPER:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
      END.
   END.
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_K1
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
   ASSIGN
   FILL-IN_FDATUM = INPUT FRAME {&FRAME-NAME} FILL-IN_FDATUM
   FILL-IN_TDATUM = INPUT FRAME {&FRAME-NAME} FILL-IN_TDATUM. 
   RUN sparadatum_UI IN fakkoproch (INPUT FILL-IN_FDATUM, INPUT FILL-IN_TDATUM).
   RUN sparatemp_UI IN fakkoproch
      (INPUT TABLE kundfodrantemp, INPUT TABLE intakttemp, INPUT TABLE motparttemp, INPUT TABLE momstemp).
   
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
   
   RUN setcolsortvar_UI IN brwproc[1] (INPUT "kundfodrantemp.BORT = FALSE").
   RUN setcolsortvar_UI IN brwproc[2] (INPUT "intakttemp.BORT = FALSE").
   RUN setcolsortvar_UI IN brwproc[3] (INPUT "motparttemp.BORT = FALSE").
   RUN setcolsortvar_UI IN brwproc[4] (INPUT "momstemp.BORT = FALSE").
   BRW_KSDYN:ALLOW-COLUMN-SEARCHING = TRUE.
   maxwidth = BRW_KSDYN:WIDTH-PIXELS.
   xpos = BRW_KSDYN:X.
   RAD_KONTO:DELETE("").
   RAD_KONTO:ADD-LAST("Kundfodran",1).
   RAD_KONTO:ADD-LAST("Intäktskonto",2).
   RAD_KONTO:ADD-LAST("Motpart",3).
   RAD_KONTO:ADD-LAST("Momskonto",4).
   RAD_KONTO:ADD-LAST("Styrning Kundfodran",5).
   RAD_KONTO:ADD-LAST("Styrning Intäktskonto",6).
   RAD_KONTO:ADD-LAST("Styrning Motpart",7).
   RAD_KONTO:ADD-LAST("Styrning Momskonto",8).
   RUN enable_UI.   
   {FRMSIZE.I}  
   {musarrow.i}
   RAD_KONTO = 1.      
   APPLY "VALUE-CHANGED" TO RAD_KONTO IN FRAME {&FRAME-NAME}.
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
      (INPUT BRW_K1:HANDLE IN FRAME {&FRAME-NAME}).
   RUN DYNBRW.P PERSISTENT SET brwproc[2] 
      (INPUT BRW_K2:HANDLE IN FRAME {&FRAME-NAME}).
   RUN DYNBRW.P PERSISTENT SET brwproc[3] 
      (INPUT BRW_K3:HANDLE IN FRAME {&FRAME-NAME}).
   RUN DYNBRW.P PERSISTENT SET brwproc[4] 
      (INPUT BRW_K4:HANDLE IN FRAME {&FRAME-NAME}).
   RUN DYNBRW.P PERSISTENT SET brwproc[5] 
      (INPUT BRW_KSDYN:HANDLE IN FRAME {&FRAME-NAME}).
   IF Guru.Konstanter:appcon THEN DO:
      RUN FAKKAPP.P PERSISTENT SET fakkoproch ON Guru.Konstanter:apphand TRANSACTION DISTINCT.         
   END.
   ELSE DO:
      RUN FAKKAPP.P PERSISTENT SET fakkoproch.         
   END.
   RUN laddakfmtemp_UI IN fakkoproch 
      (OUTPUT TABLE styrkfmtemp, OUTPUT TABLE faktureringstyptemp, OUTPUT TABLE kundtyptemp).
   RUN laddakontotemp_UI IN fakkoproch 
      (OUTPUT TABLE kundfodrantemp, OUTPUT TABLE intakttemp, OUTPUT TABLE motparttemp, OUTPUT TABLE momstemp).
   EMPTY TEMP-TABLE momsraknartemp NO-ERROR. 
   FOR EACH momstemp:
      CREATE momsraknartemp.
      ASSIGN
      momsraknartemp.ROWM = ROWID(momstemp)
      momsraknartemp.MOMSNR = momstemp.MOMSNR.
   END.
   RUN laddadatum_UI IN fakkoproch (OUTPUT FILL-IN_FDATUM, OUTPUT FILL-IN_TDATUM).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE andra_UI WINDOW-1 
PROCEDURE andra_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   BRW_KSDYN:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}.
   IF AVAILABLE styrkfmtemp THEN DO:
      valstyrrec = styrkfmtemp.STYRREC.      
      RUN NYSTYR.W (INPUT RAD_KONTO, INPUT-OUTPUT valstyrrec).
      RUN setcolsortvar_UI IN brwproc[5] (INPUT "styrkfmtemp.STYRVAL = " + STRING(RAD_KONTO)) .
      
      RUN openbdynspec_UI IN brwproc[5].
      FIND FIRST styrkfmtemp WHERE styrkfmtemp.STYRREC = valstyrrec NO-LOCK NO-ERROR.
      IF AVAILABLE styrkfmtemp THEN DO:
         RUN setlastrowid_UI IN brwproc[5] (INPUT ROWID(styrkfmtemp)).
         RUN lastselectdyn_UI IN brwproc[5].
      END.
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
   MESSAGE "Vill du verkligen ta bort detta konto?"
   VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Konton" 
   UPDATE answer AS LOGICAL.
   IF answer THEN DO TRANSACTION:
      {muswait.i}
      IF RAD_KONTO = 1 THEN DO: 
         RUN tabortkonto_UI IN fakkoproch (INPUT RAD_KONTO, INPUT kundfodrantemp.KUNDKONTOID).
         DELETE kundfodrantemp.
         RUN selnextprevrow_UI IN brwproc[RAD_KONTO].
         RUN openbdynspec_UI IN brwproc[RAD_KONTO].
         RUN lastselectdyn_UI IN brwproc[RAD_KONTO].
         IF NOT AVAILABLE kundfodrantemp THEN DO:
            ASSIGN         
            BTN_BORT:HIDDEN IN FRAME {&FRAME-NAME} = TRUE 
            FBTN_SKR:HIDDEN IN FRAME {&FRAME-NAME} = TRUE 
            FBTN_VPER:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
         END.                                              
      END.  
      ELSE IF RAD_KONTO = 2 THEN DO:
         RUN tabortkonto_UI IN fakkoproch (INPUT RAD_KONTO, INPUT intakttemp.INTAKTID).
         DELETE intakttemp.
         RUN selnextprevrow_UI IN brwproc[RAD_KONTO].
         RUN openbdynspec_UI IN brwproc[RAD_KONTO].
         RUN lastselectdyn_UI IN brwproc[RAD_KONTO].
         IF NOT AVAILABLE intakttemp THEN DO:
            ASSIGN         
            BTN_BORT:HIDDEN IN FRAME {&FRAME-NAME} = TRUE 
            FBTN_SKR:HIDDEN IN FRAME {&FRAME-NAME} = TRUE 
            FBTN_VPER:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
         END.                                              
      END.  
      ELSE IF RAD_KONTO = 3 THEN DO:
         RUN tabortkonto_UI IN fakkoproch (INPUT RAD_KONTO, INPUT motparttemp.MOTPARTID).
         DELETE motparttemp.
         RUN selnextprevrow_UI IN brwproc[RAD_KONTO].
         RUN openbdynspec_UI IN brwproc[RAD_KONTO].
         RUN lastselectdyn_UI IN brwproc[RAD_KONTO].
         IF NOT AVAILABLE motparttemp THEN DO:
            ASSIGN         
            BTN_BORT:HIDDEN IN FRAME {&FRAME-NAME} = TRUE 
            FBTN_SKR:HIDDEN IN FRAME {&FRAME-NAME} = TRUE 
            FBTN_VPER:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
         END.                                              
      END.  
      ELSE IF RAD_KONTO = 4 THEN DO:
         RUN tabortkonto_UI IN fakkoproch (INPUT RAD_KONTO, INPUT momstemp.MOMSID).
         DELETE momstemp.
         RUN selnextprevrow_UI IN brwproc[RAD_KONTO].
         RUN openbdynspec_UI IN brwproc[RAD_KONTO].
         RUN lastselectdyn_UI IN brwproc[RAD_KONTO].
         IF NOT AVAILABLE momstemp THEN DO:
            ASSIGN         
            BTN_BORT:HIDDEN IN FRAME {&FRAME-NAME} = TRUE 
            FBTN_SKR:HIDDEN IN FRAME {&FRAME-NAME} = TRUE 
            FBTN_VPER:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
         END.                                              
      END.
      ELSE DO:
         RUN tabortstyr_UI IN fakkoproch (INPUT RAD_KONTO, INPUT styrkfmtemp.STYRREC).
         DELETE styrkfmtemp.
         RUN selnextprevrow_UI IN brwproc[5].
         RUN setcolsortvar_UI IN brwproc[5] (INPUT "styrkfmtemp.STYRVAL = " + STRING(RAD_KONTO)) .
         RUN openbdynspec_UI IN brwproc[5].
         RUN lastselectdyn_UI IN brwproc[5].
         IF BRW_KSDYN:NUM-SELECTED-ROWS = 0 THEN DO:
            ASSIGN         
            BTN_BORT:HIDDEN IN FRAME {&FRAME-NAME} = TRUE 
            BTN_UPP:HIDDEN IN FRAME {&FRAME-NAME} = TRUE 
            FBTN_SKR:HIDDEN IN FRAME {&FRAME-NAME} = TRUE 
            FBTN_VPER:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
         END.                                              
      END.                                                                           
   END.    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE brwfix_UI WINDOW-1 
PROCEDURE brwfix_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER strvisible AS CHARACTER NO-UNDO.
   DEFINE VARIABLE winh AS HANDLE NO-UNDO.
   DEFINE VARIABLE brwksh AS HANDLE NO-UNDO.
   ASSIGN
   brwksh = BRW_KSDYN:HANDLE IN FRAME {&FRAME-NAME}. 
   winh = {&WINDOW-NAME}:HANDLE.
   maxwidth = winh:WIDTH-PIXELS  - 100.
   tempi = 1.
   
   brwksh:NO-EMPTY-SPACE = FALSE NO-ERROR. 
   BRW_KSDYN:VISIBLE IN FRAME {&FRAME-NAME} = FALSE.
   DO WHILE tempi LE BRW_KSDYN:NUM-COLUMNS:
      colh = BRW_KSDYN:GET-BROWSE-COLUMN(tempi).
      colh:VISIBLE = FALSE.
      tempi = tempi + 1.
   END.
   tempi = 1.
   tempwidth = 0.
   DO WHILE tempi LE NUM-ENTRIES(strvisible, ","):
      colh = BRW_KSDYN:GET-BROWSE-COLUMN(INTEGER(ENTRY(tempi, strvisible, ","))).
      colh:VISIBLE = TRUE.
      tempwidth = tempwidth + colh:WIDTH-PIXELS.
      tempi = tempi + 1.
   END.      
   /*
   IF (tempwidth + 40) < 400 THEN BRW_KSDYN:WIDTH-PIXELS = 400.
   BRW_KSDYN:X = 10.
   */
   IF maxwidth < tempwidth + 40  THEN BRW_KSDYN:WIDTH-PIXELS = maxwidth.
   ELSE BRW_KSDYN:WIDTH-PIXELS = tempwidth + 40.
   
   tempi = (maxwidth / 2) - ( BRW_KSDYN:WIDTH-PIXELS / 2). 
   /*
   BRW_KSDYN:X = xpos + tempi.
   */
   BRW_KSDYN:VISIBLE = TRUE.
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
  DISPLAY RAD_KONTO FILL-IN_FDATUM FILL-IN_TDATUM FILL-IN-GODK 
      WITH FRAME FRAME-A IN WINDOW WINDOW-1.
  ENABLE RECT-47 RAD_KONTO BTN_NVE-3 FILL-IN_FDATUM BTN_FVE-3 BTN_NVE-4 
         FILL-IN_TDATUM BTN_FVE-4 FBTN_VPER FBTN_SKR BRW_KSDYN BRW_K4 BRW_K1 
         BRW_K3 BRW_K2 BTN_BORT BTN_AVB BTN_NY BTN_UPP FILL-IN-GODK 
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
   DEFINE VARIABLE idtemp AS INTEGER NO-UNDO.
   DEFINE VARIABLE temprow AS ROWID NO-UNDO.
   {muswait.i}
   IF RAD_KONTO < 5 THEN DO:
      RUN skapakonto_UI IN fakkoproch (INPUT RAD_KONTO, OUTPUT idtemp).
      IF RAD_KONTO = 1 THEN DO: 
         CREATE kundfodrantemp.
         ASSIGN
         temprow = ROWID(kundfodrantemp)
         kundfodrantemp.BORT = FALSE
         kundfodrantemp.KUNDKONTOID = idtemp
         kundfodrantemp.KUNDKONTO = STRING(idtemp).
      END.
      ELSE IF RAD_KONTO = 2 THEN DO: 
         CREATE intakttemp.
         ASSIGN
         temprow = ROWID(intakttemp)
         intakttemp.BORT = FALSE
         intakttemp.INTAKTID = idtemp
         intakttemp.INTAKTKONTO = STRING(idtemp).
      END.  
      ELSE IF RAD_KONTO = 3 THEN DO:    
         CREATE motparttemp.
         ASSIGN
         temprow = ROWID(motparttemp)
         motparttemp.BORT = FALSE
         motparttemp.MOTPARTID = idtemp
         motparttemp.MOTPART = STRING(idtemp).
      END.  
      ELSE IF RAD_KONTO = 4 THEN DO: 
         CREATE momstemp.
         ASSIGN
         temprow = ROWID(momstemp)
         momstemp.BORT = FALSE
         momstemp.MOMSID = idtemp
         momstemp.MOMSKOD = STRING(idtemp).
        /* momstemp.MOMSNR = STRING(idtemp).*/
         /*
         CREATE momsraknartemp.
         ASSIGN
         momsraknartemp.ROWM = ROWID(momstemp)
         momsraknartemp.MOMSNR = momstemp.MOMSNR.
         */
      END.
      RUN setlastrowid_UI IN brwproc[RAD_KONTO] (INPUT temprow).
      RUN openbdynspec_UI IN brwproc[RAD_KONTO].
      RUN lastselectdyn_UI IN brwproc[RAD_KONTO].
      IF RAD_KONTO = 4 THEN DO: 
         APPLY "ENTRY" TO momstemp.MOMSKOD IN BROWSE BRW_K4.
      END.
      ASSIGN
      BTN_BORT:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      FBTN_SKR:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      FBTN_VPER:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
   END.
   ELSE DO: 
      valstyrrec = ?.
      RUN NYSTYR.W (INPUT RAD_KONTO, INPUT-OUTPUT valstyrrec).
      RUN setcolsortvar_UI IN brwproc[5] (INPUT "styrkfmtemp.STYRVAL = " + STRING(RAD_KONTO)) .
      RUN openbdynspec_UI IN brwproc[5].
      FIND FIRST styrkfmtemp WHERE styrkfmtemp.STYRREC = valstyrrec NO-LOCK NO-ERROR.
      IF AVAILABLE styrkfmtemp THEN DO:
         RUN setlastrowid_UI IN brwproc[5] (INPUT ROWID(styrkfmtemp)).
         RUN lastselectdyn_UI IN brwproc[5].
         ASSIGN         
         BTN_BORT:HIDDEN IN FRAME {&FRAME-NAME} = FALSE 
         BTN_UPP:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
         FBTN_SKR:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
         FBTN_VPER:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
      END.
   END.
   {musarrow.i}
   musz = FALSE. 
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE styrut_UI WINDOW-1 
PROCEDURE styrut_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   EMPTY TEMP-TABLE tidut NO-ERROR.    
   CREATE tidut.
   SUBSTRING(tidut.UT,30) = STRING(TODAY).
   CREATE tidut.
   CREATE tidut.            
   SUBSTRING(tidut.UT,1) = "KONTON FÖR FAKTURERING".  
   IF RAD_KONTO = 5 THEN DO:
      SUBSTRING(tidut.UT,27) = "STYR KUNDFODRANSKONTON".
      CREATE tidut.
      ASSIGN
      SUBSTRING(tidut.UT,58) = "KUNDFODRANS".
      CREATE tidut.
      ASSIGN
      SUBSTRING(tidut.UT,1) = "KUNDTYP"  
      SUBSTRING(tidut.UT,22) = "TEXT"
      SUBSTRING(tidut.UT,58) = "KONTO".                           
      CREATE tidut.
      ASSIGN tidut.UT = "====================.===================================.===========".
   END.
   ELSE IF RAD_KONTO = 6 THEN DO:
      SUBSTRING(tidut.UT,27) = "STYR INTÄKTSKONTON".
      CREATE tidut.
      ASSIGN
      SUBSTRING(tidut.UT,68) = "MOMS"
      SUBSTRING(tidut.UT,74) = "INTÄKTS".
      CREATE tidut.
      ASSIGN
      SUBSTRING(tidut.UT,1) = "KUNDTYP"  
      SUBSTRING(tidut.UT,22) = "FAKTURERINGSTYP"
      SUBSTRING(tidut.UT,68) = "KONTO"
      SUBSTRING(tidut.UT,74) = "KONTO".         
      CREATE tidut.
      ASSIGN tidut.UT = "====================.=============================================.=====.=======".
   END.
   ELSE IF RAD_KONTO = 7 THEN DO:
      SUBSTRING(tidut.UT,27) = "STYR MOTPART".
      CREATE tidut.
      ASSIGN
      SUBSTRING(tidut.UT,1) = "KUNDTYP"  
      SUBSTRING(tidut.UT,22) = "MOTPART".
      CREATE tidut.
      ASSIGN tidut.UT = "====================.=======".
   END.
   ELSE IF RAD_KONTO = 8 THEN DO:
      ASSIGN
      SUBSTRING(tidut.UT,27) = "STYR MOMSKONTON".
      CREATE tidut.
      ASSIGN
      SUBSTRING(tidut.UT,68) = "MOMS".
      CREATE tidut.
      ASSIGN
      SUBSTRING(tidut.UT,1) = "KUNDTYP"  
      SUBSTRING(tidut.UT,22) = "FAKTURERINGSTYP"
      SUBSTRING(tidut.UT,68) = "KONTO".                  
      CREATE tidut.
      ASSIGN tidut.UT = "====================.=============================================.=====".
   END. 
   GET FIRST BRW_KSDYN NO-LOCK.
   DO WHILE AVAILABLE(styrkfmtemp):
         CREATE tidut.
         IF RAD_KONTO = 5 THEN DO:
            ASSIGN
            SUBSTRING(tidut.UT,1) = STRING(styrkfmtemp.KUNDTYP,"X(20)")  
            SUBSTRING(tidut.UT,22) = STRING(styrkfmtemp.KUNDFODTEXT,"X(35)")
            SUBSTRING(tidut.UT,58) = STRING(styrkfmtemp.KUNDKONTO,"X(5)").                  
         END.
         ELSE IF RAD_KONTO = 6 THEN DO:
            ASSIGN
            SUBSTRING(tidut.UT,1) = STRING(styrkfmtemp.KUNDTYP,"X(20)")
            SUBSTRING(tidut.UT,22) = STRING(styrkfmtemp.FAKTTYPTEXT,"X(45)")
            SUBSTRING(tidut.UT,68) = STRING(styrkfmtemp.MOMSKOD,"X(5)")
            SUBSTRING(tidut.UT,74) = STRING(styrkfmtemp.INTAKTKONTO,"X(5)").
         END.
         ELSE IF RAD_KONTO = 7 THEN DO:
            ASSIGN
            SUBSTRING(tidut.UT,1) = STRING(styrkfmtemp.KUNDTYP,"X(20)")
            SUBSTRING(tidut.UT,22) = STRING(styrkfmtemp.MOTPART,"X(5)").
         END.
         ELSE IF RAD_KONTO = 8 THEN DO:
            ASSIGN
            SUBSTRING(tidut.UT,1) = STRING(styrkfmtemp.KUNDTYP,"X(20)")
            SUBSTRING(tidut.UT,22) = STRING(styrkfmtemp.FAKTTYPTEXT,"X(45)")
            SUBSTRING(tidut.UT,68) = STRING(styrkfmtemp.MOMSKOD,"X(5)").            
         END.
         GET NEXT BRW_KSDYN NO-LOCK.
   END.                                                                            
   IF skrivut = FALSE THEN DO:
      {AVBGOM.I}
      RUN UTRAPP.W (INPUT "Konton för fakturering").
      {AVBFRAM.I}
   END.
   ELSE DO:
      IF musz = TRUE THEN musz = FALSE.
      ELSE RUN EKLOGS.P.                  
   END.
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
   EMPTY TEMP-TABLE tidut NO-ERROR.    
   CREATE tidut.
   SUBSTRING(tidut.UT,30) = STRING(TODAY).
   CREATE tidut.
   CREATE tidut.            
   SUBSTRING(tidut.UT,1) = "Konton för fakturering".  
   IF RAD_KONTO = 1 THEN SUBSTRING(tidut.UT,27) = "KUNDFODRANSKONTON".
   IF RAD_KONTO = 2 THEN SUBSTRING(tidut.UT,27) = "INTÄKTSKONTON".
   IF RAD_KONTO = 3 THEN SUBSTRING(tidut.UT,27) = "MOTPART".
   IF RAD_KONTO = 4 THEN SUBSTRING(tidut.UT,27) = "MOMSKONTON".     
   CREATE tidut.
   CREATE tidut.
   SUBSTRING(tidut.UT,1) = "KONTO".
   SUBSTRING(tidut.UT,8) = "TEXT".                                
   IF RAD_KONTO = 4 THEN DO:
      ASSIGN
      SUBSTRING(tidut.UT,50) = "EXTERN"
      SUBSTRING(tidut.UT,57) = "INTERN". 
   END.    
   CREATE tidut.
   IF RAD_KONTO = 4 THEN ASSIGN tidut.UT = "======.=========================================.======.======".
   ELSE ASSIGN tidut.UT = "======.==================================".
   IF RAD_KONTO = 1 THEN DO:
      &Scoped-define BROWSE-NAME BRW_K1
      GET FIRST {&BROWSE-NAME} NO-LOCK.
      DO WHILE AVAILABLE(kundfodrantemp):
         CREATE tidut.
         ASSIGN
         SUBSTRING(tidut.UT,1) = kundfodrantemp.KUNDKONTO   
         SUBSTRING(tidut.UT,8) = kundfodrantemp.KUNDFODTEXT.      
         GET NEXT {&BROWSE-NAME} NO-LOCK.
      END.              
   END.
   IF RAD_KONTO = 2 THEN DO:
      &Scoped-define BROWSE-NAME BRW_K2
      GET FIRST {&BROWSE-NAME} NO-LOCK.
      DO WHILE AVAILABLE(intakttemp):
         CREATE tidut.
         ASSIGN
         SUBSTRING(tidut.UT,1) = intakttemp.INTAKTKONTO    
         SUBSTRING(tidut.UT,8) = intakttemp.INTAKTTEXT.      
         GET NEXT {&BROWSE-NAME} NO-LOCK.
      END.  
   END.
   IF RAD_KONTO = 3 THEN DO:
      &Scoped-define BROWSE-NAME BRW_K3
      GET FIRST {&BROWSE-NAME} NO-LOCK.
      DO WHILE AVAILABLE(motparttemp):
         CREATE tidut.
         ASSIGN
         SUBSTRING(tidut.UT,1) = motparttemp.MOTPART    
         SUBSTRING(tidut.UT,8) = motparttemp.MOTPARTTEXT.      
         GET NEXT {&BROWSE-NAME} NO-LOCK.
      END.  
   END.
   IF RAD_KONTO = 4 THEN DO:
      &Scoped-define BROWSE-NAME BRW_K4
      GET FIRST {&BROWSE-NAME} NO-LOCK.
      DO WHILE AVAILABLE(momstemp):
         CREATE tidut.
         ASSIGN
         SUBSTRING(tidut.UT,1) = momstemp.MOMSKOD     
         SUBSTRING(tidut.UT,8) = momstemp.MOMSTEXT
         SUBSTRING(tidut.UT,50) = STRING(momstemp.MOMSEXTERNT,">>9.99") 
         SUBSTRING(tidut.UT,57) = STRING(momstemp.MOMSINTERNT,">>9.99").      
         GET NEXT {&BROWSE-NAME} NO-LOCK.
      END.
   END.
   IF skrivut = FALSE THEN DO:
      {AVBGOM.I}
      RUN UTRAPP.W (INPUT "Konton för fakturering").
      {AVBFRAM.I}
   END.
   ELSE DO:
      IF musz = TRUE THEN musz = FALSE.
      ELSE RUN EKLOGS.P.                  
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

