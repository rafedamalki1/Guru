&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win




&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 

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
DEFINE INPUT PARAMETER period AS INTEGER NO-UNDO.
/* Local Variable Definitions ---                                       */
{ALLDEF.I}
{GLOBVAR2DEL1.I}
{REGVAR.I}

{LOP.I}
&Scoped-define SHARED SHARED 
{OMRTEMPW.I}
DEFINE NEW SHARED VARIABLE prec AS RECID NO-UNDO.
DEFINE NEW SHARED VARIABLE loprec AS RECID NO-UNDO.
DEFINE NEW SHARED VARIABLE valrec AS RECID NO-UNDO.
DEFINE NEW SHARED VARIABLE andrec AS RECID NO-UNDO.
DEFINE NEW SHARED VARIABLE andra AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE typpris AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE katvar AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE skatvar AS INTEGER NO-UNDO. 
DEFINE SHARED VARIABLE utomr AS CHARACTER NO-UNDO. 
DEFINE SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE typ_val AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE aosok AS CHARACTER FORMAT "X(5)" NO-UNDO.
DEFINE VARIABLE ortsok AS CHARACTER FORMAT "X(40)" NO-UNDO.
DEFINE VARIABLE antal_valda AS INTEGER NO-UNDO.
DEFINE VARIABLE antal_raknare AS INTEGER NO-UNDO.
DEFINE VARIABLE bortrec AS RECID NO-UNDO.
DEFINE VARIABLE antalvar AS DECIMAL NO-UNDO.
DEFINE VARIABLE udokalkylapph AS HANDLE NO-UNDO.
DEFINE VARIABLE laddaproch AS HANDLE NO-UNDO.
DEFINE VARIABLE tabvar AS CHARACTER NO-UNDO.

DEFINE TEMP-TABLE kalk_temp
   FIELD ARBKOD AS CHARACTER 
   FIELD LOPNR AS INTEGER 
   FIELD ENHET AS CHARACTER 
   FIELD BENAMNING AS CHARACTER
   INDEX KOD ARBKOD LOPNR ASCENDING.

DEFINE TEMP-TABLE kalk_temp2
   FIELD ARBKOD AS CHARACTER
   FIELD LOPNR AS INTEGER
   FIELD ENHET AS CHARACTER 
   FIELD BENAMNING AS CHARACTER
   FIELD ANTAL AS DECIMAL
   INDEX KOD ARBKOD LOPNR ASCENDING.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BRW_KALK

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES kalk_temp loptemp1 loptemp2 loptemp3 ~
loptemp5 ptemp1 ptemp2 ptemp3 ptemp5

/* Definitions for BROWSE BRW_KALK                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_KALK kalk_temp.ARBKOD kalk_temp.LOPNR ~
kalk_temp.ENHET kalk_temp.BENAMNING 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_KALK 
&Scoped-define QUERY-STRING-BRW_KALK FOR EACH kalk_temp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_KALK OPEN QUERY BRW_KALK FOR EACH kalk_temp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_KALK kalk_temp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_KALK kalk_temp


/* Definitions for BROWSE BRW_LOP1                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_LOP1 loptemp1.LOPNR loptemp1.ENHET ~
loptemp1.BENAMNING 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_LOP1 
&Scoped-define QUERY-STRING-BRW_LOP1 FOR EACH loptemp1 NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BRW_LOP1 OPEN QUERY BRW_LOP1 FOR EACH loptemp1 NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BRW_LOP1 loptemp1
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_LOP1 loptemp1


/* Definitions for BROWSE BRW_LOP2                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_LOP2 loptemp2.LOPNR loptemp2.ENHET ~
loptemp2.BENAMNING 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_LOP2 
&Scoped-define QUERY-STRING-BRW_LOP2 FOR EACH loptemp2 NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BRW_LOP2 OPEN QUERY BRW_LOP2 FOR EACH loptemp2 NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BRW_LOP2 loptemp2
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_LOP2 loptemp2


/* Definitions for BROWSE BRW_LOP3                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_LOP3 loptemp3.LOPNR loptemp3.ENHET ~
loptemp3.BENAMNING 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_LOP3 
&Scoped-define QUERY-STRING-BRW_LOP3 FOR EACH loptemp3 NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BRW_LOP3 OPEN QUERY BRW_LOP3 FOR EACH loptemp3 NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BRW_LOP3 loptemp3
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_LOP3 loptemp3


/* Definitions for BROWSE BRW_LOP5                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_LOP5 loptemp5.LOPNR loptemp5.ENHET ~
loptemp5.BENAMNING 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_LOP5 
&Scoped-define QUERY-STRING-BRW_LOP5 FOR EACH loptemp5 NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BRW_LOP5 OPEN QUERY BRW_LOP5 FOR EACH loptemp5 NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BRW_LOP5 loptemp5
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_LOP5 loptemp5


/* Definitions for BROWSE BRW_P1                                        */
&Scoped-define FIELDS-IN-QUERY-BRW_P1 ptemp1.ARBKOD ptemp1.BENAMNING 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_P1 
&Scoped-define QUERY-STRING-BRW_P1 FOR EACH ptemp1 NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BRW_P1 OPEN QUERY BRW_P1 FOR EACH ptemp1 NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BRW_P1 ptemp1
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_P1 ptemp1


/* Definitions for BROWSE BRW_P2                                        */
&Scoped-define FIELDS-IN-QUERY-BRW_P2 ptemp2.ARBKOD ptemp2.BENAMNING 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_P2 
&Scoped-define QUERY-STRING-BRW_P2 FOR EACH ptemp2 NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BRW_P2 OPEN QUERY BRW_P2 FOR EACH ptemp2 NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BRW_P2 ptemp2
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_P2 ptemp2


/* Definitions for BROWSE BRW_P3                                        */
&Scoped-define FIELDS-IN-QUERY-BRW_P3 ptemp3.ARBKOD ptemp3.BENAMNING 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_P3 
&Scoped-define QUERY-STRING-BRW_P3 FOR EACH ptemp3 NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BRW_P3 OPEN QUERY BRW_P3 FOR EACH ptemp3 NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BRW_P3 ptemp3
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_P3 ptemp3


/* Definitions for BROWSE BRW_P5                                        */
&Scoped-define FIELDS-IN-QUERY-BRW_P5 ptemp5.ARBKOD ptemp5.BENAMNING 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_P5 
&Scoped-define QUERY-STRING-BRW_P5 FOR EACH ptemp5 NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BRW_P5 OPEN QUERY BRW_P5 FOR EACH ptemp5 NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BRW_P5 ptemp5
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_P5 ptemp5


/* Definitions for FRAME DEFAULT-FRAME                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BTN_VISA FILL-IN-KOD FILL-IN-BEN BTN_AVB ~
BRW_KALK BTN_SKAPA BTN_BORT BRW_LOP5 BTN_ALLOVER BTN_ALLBACK RECT-22 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-KOD FILL-IN-BEN 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_ALLBACK 
     IMAGE-UP FILE "BILDER\rewind-u":U
     LABEL "Alla aonr i listan":L 
     SIZE 5.88 BY 1.96 TOOLTIP "Alla valda löpnr tas bort från vallistan"
     FONT 11.

DEFINE BUTTON BTN_ALLOVER 
     IMAGE-UP FILE "BILDER\forwrd-u":U
     LABEL "Alla aonr i listan":L 
     SIZE 5.88 BY 1.96 TOOLTIP "Alla löpnr väljs"
     FONT 11.

DEFINE BUTTON BTN_AVB AUTO-END-KEY 
     LABEL "Avbryt" 
     SIZE 14 BY 1
     BGCOLOR 8 .

DEFINE BUTTON BTN_BORT 
     IMAGE-UP FILE "bilder\prev-u":U
     LABEL "Ta bort" 
     SIZE 5.88 BY 1.96 TOOLTIP "Markerade löpnr tas bort".

DEFINE BUTTON BTN_SKAPA 
     IMAGE-UP FILE "bilder\next-u":U
     LABEL "Skapa" 
     SIZE 5.88 BY 1.96 TOOLTIP "Markerade löpnr väljs".

DEFINE BUTTON BTN_VISA 
     LABEL "Visa i excel" 
     SIZE 14 BY 1.

DEFINE VARIABLE FILL-IN-BEN AS CHARACTER FORMAT "X(40)":U 
     LABEL "Benämning" 
     VIEW-AS FILL-IN 
     SIZE 25.25 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN-KOD AS CHARACTER FORMAT "X(5)":U 
     LABEL "Arbetskod" 
     VIEW-AS FILL-IN 
     SIZE 7 BY .83 NO-UNDO.

DEFINE RECTANGLE RECT-22
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 52 BY 2.42
     BGCOLOR 8 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_KALK FOR 
      kalk_temp SCROLLING.

DEFINE QUERY BRW_LOP1 FOR 
      loptemp1 SCROLLING.

DEFINE QUERY BRW_LOP2 FOR 
      loptemp2 SCROLLING.

DEFINE QUERY BRW_LOP3 FOR 
      loptemp3 SCROLLING.

DEFINE QUERY BRW_LOP5 FOR 
      loptemp5 SCROLLING.

DEFINE QUERY BRW_P1 FOR 
      ptemp1 SCROLLING.

DEFINE QUERY BRW_P2 FOR 
      ptemp2 SCROLLING.

DEFINE QUERY BRW_P3 FOR 
      ptemp3 SCROLLING.

DEFINE QUERY BRW_P5 FOR 
      ptemp5 SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_KALK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_KALK C-Win _STRUCTURED
  QUERY BRW_KALK NO-LOCK DISPLAY
      kalk_temp.ARBKOD COLUMN-LABEL "Arbets!kod" FORMAT "X(5)":U
      kalk_temp.LOPNR COLUMN-LABEL "Nr" FORMAT ">>>":U
      kalk_temp.ENHET COLUMN-LABEL "Enhet" FORMAT "x(5)":U
      kalk_temp.BENAMNING COLUMN-LABEL "Benämning" FORMAT "x(256)":U
            WIDTH 20
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING MULTIPLE SIZE 39.38 BY 15
         TITLE "Arbetskoder/Löpnr för uppföljning".

DEFINE BROWSE BRW_LOP1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_LOP1 C-Win _STRUCTURED
  QUERY BRW_LOP1 NO-LOCK DISPLAY
      loptemp1.LOPNR COLUMN-LABEL "Nr" FORMAT ">>>":U
      loptemp1.ENHET COLUMN-LABEL "Enh" FORMAT "X(3)":U
      loptemp1.BENAMNING FORMAT "X(40)":U WIDTH 35
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING MULTIPLE SIZE 45 BY 15
         TITLE "Löpnr kopplade till arbetskoden".

DEFINE BROWSE BRW_LOP2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_LOP2 C-Win _STRUCTURED
  QUERY BRW_LOP2 NO-LOCK DISPLAY
      loptemp2.LOPNR COLUMN-LABEL "Nr" FORMAT ">>>":U
      loptemp2.ENHET COLUMN-LABEL "Enh" FORMAT "X(3)":U
      loptemp2.BENAMNING FORMAT "X(40)":U WIDTH 35
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING MULTIPLE SIZE 45 BY 15
         TITLE "Löpnr kopplade till arbetskoden".

DEFINE BROWSE BRW_LOP3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_LOP3 C-Win _STRUCTURED
  QUERY BRW_LOP3 NO-LOCK DISPLAY
      loptemp3.LOPNR COLUMN-LABEL "Nr" FORMAT ">>>":U
      loptemp3.ENHET COLUMN-LABEL "Enh" FORMAT "X(3)":U
      loptemp3.BENAMNING FORMAT "X(256)":U WIDTH 35
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING MULTIPLE SIZE 45 BY 15
         TITLE "Löpnr kopplade till arbetskoden".

DEFINE BROWSE BRW_LOP5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_LOP5 C-Win _STRUCTURED
  QUERY BRW_LOP5 NO-LOCK DISPLAY
      loptemp5.LOPNR COLUMN-LABEL "Nr" FORMAT ">>>":U
      loptemp5.ENHET COLUMN-LABEL "Enh" FORMAT "X(3)":U
      loptemp5.BENAMNING FORMAT "X(256)":U WIDTH 35
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING MULTIPLE SIZE 45 BY 15
         TITLE "Löpnr kopplade till arbetskoden".

DEFINE BROWSE BRW_P1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_P1 C-Win _STRUCTURED
  QUERY BRW_P1 NO-LOCK DISPLAY
      ptemp1.ARBKOD FORMAT "X(5)":U
      ptemp1.BENAMNING FORMAT "X(40)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SIZE 52 BY 9.25
         TITLE "Arbetskoder typ 1".

DEFINE BROWSE BRW_P2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_P2 C-Win _STRUCTURED
  QUERY BRW_P2 NO-LOCK DISPLAY
      ptemp2.ARBKOD FORMAT "X(5)":U
      ptemp2.BENAMNING FORMAT "X(40)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SIZE 52 BY 9.25
         TITLE "Arbetskoder typ 2".

DEFINE BROWSE BRW_P3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_P3 C-Win _STRUCTURED
  QUERY BRW_P3 NO-LOCK DISPLAY
      ptemp3.ARBKOD FORMAT "X(5)":U
      ptemp3.BENAMNING FORMAT "X(40)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SIZE 52 BY 9.25
         TITLE "Arbetskoder typ 3".

DEFINE BROWSE BRW_P5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_P5 C-Win _STRUCTURED
  QUERY BRW_P5 NO-LOCK DISPLAY
      ptemp5.ARBKOD FORMAT "X(5)":U
      ptemp5.BENAMNING FORMAT "X(40)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SIZE 52 BY 9.25
         TITLE "Arbetskoder sambyggnad/samförläggning".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     BRW_P1 AT ROW 1.5 COL 25.63
     BRW_P2 AT ROW 1.5 COL 25.63
     BRW_P3 AT ROW 1.5 COL 25.63
     BTN_VISA AT ROW 8 COL 95.5
     FILL-IN-KOD AT ROW 11.25 COL 46.5 COLON-ALIGNED
     FILL-IN-BEN AT ROW 12.29 COL 46.63 COLON-ALIGNED
     BTN_AVB AT ROW 27.79 COL 95.5
     BRW_LOP2 AT ROW 13.92 COL 1.5
     BRW_LOP3 AT ROW 13.92 COL 1.5
     BRW_KALK AT ROW 13.92 COL 53.63
     BRW_LOP1 AT ROW 13.92 COL 1.5
     BTN_SKAPA AT ROW 19 COL 47.38
     BTN_BORT AT ROW 21.75 COL 47.38
     BRW_LOP5 AT ROW 13.92 COL 1.5
     BRW_P5 AT ROW 1.5 COL 25.63
     BTN_ALLOVER AT ROW 16.29 COL 47.38
     BTN_ALLBACK AT ROW 24.46 COL 47.38
     "Sök på:" VIEW-AS TEXT
          SIZE 9.75 BY .83 AT ROW 11.25 COL 26.25
     RECT-22 AT ROW 10.96 COL 25.63
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 109.63 BY 28.42.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Temp-Tables and Buffers:
      TABLE: kalk_temp T "?" NO-UNDO temp-db kalk_temp
      TABLE: loptemp1 T "?" NO-UNDO temp-db loptemp1
      TABLE: loptemp2 T "?" NO-UNDO temp-db loptemp2
      TABLE: loptemp3 T "?" NO-UNDO temp-db loptemp3
      TABLE: loptemp5 T "?" NO-UNDO temp-db loptemp5
      TABLE: ptemp1 T "?" NO-UNDO temp-db ptemp1
      TABLE: ptemp2 T "?" NO-UNDO temp-db ptemp2
      TABLE: ptemp3 T "?" NO-UNDO temp-db ptemp3
      TABLE: ptemp5 T "?" NO-UNDO temp-db ptemp5
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "<insert window title>"
         HEIGHT             = 28.42
         WIDTH              = 109.63
         MAX-HEIGHT         = 28.42
         MAX-WIDTH          = 109.75
         VIRTUAL-HEIGHT     = 28.42
         VIRTUAL-WIDTH      = 109.75
         RESIZE             = yes
         SCROLL-BARS        = no
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
/* SETTINGS FOR WINDOW C-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   Custom                                                               */
/* BROWSE-TAB BRW_P1 1 DEFAULT-FRAME */
/* BROWSE-TAB BRW_P2 BRW_P1 DEFAULT-FRAME */
/* BROWSE-TAB BRW_P3 BRW_P2 DEFAULT-FRAME */
/* BROWSE-TAB BRW_LOP2 BTN_AVB DEFAULT-FRAME */
/* BROWSE-TAB BRW_LOP3 BRW_LOP2 DEFAULT-FRAME */
/* BROWSE-TAB BRW_KALK BRW_LOP3 DEFAULT-FRAME */
/* BROWSE-TAB BRW_LOP1 BRW_KALK DEFAULT-FRAME */
/* BROWSE-TAB BRW_LOP5 BTN_BORT DEFAULT-FRAME */
/* BROWSE-TAB BRW_P5 BRW_LOP5 DEFAULT-FRAME */
ASSIGN 
       BRW_KALK:ALLOW-COLUMN-SEARCHING IN FRAME DEFAULT-FRAME = TRUE.

/* SETTINGS FOR BROWSE BRW_LOP1 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       BRW_LOP1:HIDDEN  IN FRAME DEFAULT-FRAME                = TRUE
       BRW_LOP1:MAX-DATA-GUESS IN FRAME DEFAULT-FRAME         = 1000
       BRW_LOP1:ALLOW-COLUMN-SEARCHING IN FRAME DEFAULT-FRAME = TRUE.

/* SETTINGS FOR BROWSE BRW_LOP2 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       BRW_LOP2:HIDDEN  IN FRAME DEFAULT-FRAME                = TRUE
       BRW_LOP2:MAX-DATA-GUESS IN FRAME DEFAULT-FRAME         = 1000
       BRW_LOP2:ALLOW-COLUMN-SEARCHING IN FRAME DEFAULT-FRAME = TRUE.

/* SETTINGS FOR BROWSE BRW_LOP3 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       BRW_LOP3:HIDDEN  IN FRAME DEFAULT-FRAME                = TRUE
       BRW_LOP3:MAX-DATA-GUESS IN FRAME DEFAULT-FRAME         = 1000
       BRW_LOP3:ALLOW-COLUMN-SEARCHING IN FRAME DEFAULT-FRAME = TRUE.

ASSIGN 
       BRW_LOP5:HIDDEN  IN FRAME DEFAULT-FRAME                = TRUE
       BRW_LOP5:MAX-DATA-GUESS IN FRAME DEFAULT-FRAME         = 1000
       BRW_LOP5:ALLOW-COLUMN-SEARCHING IN FRAME DEFAULT-FRAME = TRUE.

/* SETTINGS FOR BROWSE BRW_P1 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       BRW_P1:HIDDEN  IN FRAME DEFAULT-FRAME                = TRUE
       BRW_P1:MAX-DATA-GUESS IN FRAME DEFAULT-FRAME         = 1000
       BRW_P1:ALLOW-COLUMN-SEARCHING IN FRAME DEFAULT-FRAME = TRUE.

/* SETTINGS FOR BROWSE BRW_P2 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       BRW_P2:HIDDEN  IN FRAME DEFAULT-FRAME                = TRUE
       BRW_P2:MAX-DATA-GUESS IN FRAME DEFAULT-FRAME         = 1000
       BRW_P2:ALLOW-COLUMN-SEARCHING IN FRAME DEFAULT-FRAME = TRUE.

/* SETTINGS FOR BROWSE BRW_P3 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       BRW_P3:HIDDEN  IN FRAME DEFAULT-FRAME                = TRUE
       BRW_P3:MAX-DATA-GUESS IN FRAME DEFAULT-FRAME         = 1000
       BRW_P3:ALLOW-COLUMN-SEARCHING IN FRAME DEFAULT-FRAME = TRUE.

/* SETTINGS FOR BROWSE BRW_P5 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       BRW_P5:HIDDEN  IN FRAME DEFAULT-FRAME                = TRUE
       BRW_P5:MAX-DATA-GUESS IN FRAME DEFAULT-FRAME         = 1000
       BRW_P5:ALLOW-COLUMN-SEARCHING IN FRAME DEFAULT-FRAME = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_KALK
/* Query rebuild information for BROWSE BRW_KALK
     _TblList          = "Temp-Tables.kalk_temp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.kalk_temp.ARBKOD
"kalk_temp.ARBKOD" "Arbets!kod" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.kalk_temp.LOPNR
"kalk_temp.LOPNR" "Nr" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.kalk_temp.ENHET
"kalk_temp.ENHET" "Enhet" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[4]   > Temp-Tables.kalk_temp.BENAMNING
"kalk_temp.BENAMNING" "Benämning" "x(256)" "character" ? ? ? ? ? ? no ? no no "20" yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_KALK */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_LOP1
/* Query rebuild information for BROWSE BRW_LOP1
     _TblList          = "Temp-Tables.loptemp1"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > Temp-Tables.loptemp1.LOPNR
"loptemp1.LOPNR" "Nr" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.loptemp1.ENHET
"loptemp1.ENHET" "Enh" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.loptemp1.BENAMNING
"loptemp1.BENAMNING" ? ? "character" ? ? ? ? ? ? no ? no no "35" yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_LOP1 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_LOP2
/* Query rebuild information for BROWSE BRW_LOP2
     _TblList          = "Temp-Tables.loptemp2"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > Temp-Tables.loptemp2.LOPNR
"loptemp2.LOPNR" "Nr" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.loptemp2.ENHET
"loptemp2.ENHET" "Enh" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.loptemp2.BENAMNING
"loptemp2.BENAMNING" ? ? "character" ? ? ? ? ? ? no ? no no "35" yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_LOP2 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_LOP3
/* Query rebuild information for BROWSE BRW_LOP3
     _TblList          = "Temp-Tables.loptemp3"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > Temp-Tables.loptemp3.LOPNR
"loptemp3.LOPNR" "Nr" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.loptemp3.ENHET
"loptemp3.ENHET" "Enh" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.loptemp3.BENAMNING
"loptemp3.BENAMNING" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "35" yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_LOP3 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_LOP5
/* Query rebuild information for BROWSE BRW_LOP5
     _TblList          = "Temp-Tables.loptemp5"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > Temp-Tables.loptemp5.LOPNR
"loptemp5.LOPNR" "Nr" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.loptemp5.ENHET
"loptemp5.ENHET" "Enh" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.loptemp5.BENAMNING
"loptemp5.BENAMNING" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "35" yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_LOP5 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_P1
/* Query rebuild information for BROWSE BRW_P1
     _TblList          = "Temp-Tables.ptemp1"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   = Temp-Tables.ptemp1.ARBKOD
     _FldNameList[2]   = Temp-Tables.ptemp1.BENAMNING
     _Query            is NOT OPENED
*/  /* BROWSE BRW_P1 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_P2
/* Query rebuild information for BROWSE BRW_P2
     _TblList          = "Temp-Tables.ptemp2"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   = Temp-Tables.ptemp2.ARBKOD
     _FldNameList[2]   = Temp-Tables.ptemp2.BENAMNING
     _Query            is NOT OPENED
*/  /* BROWSE BRW_P2 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_P3
/* Query rebuild information for BROWSE BRW_P3
     _TblList          = "Temp-Tables.ptemp3"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   = Temp-Tables.ptemp3.ARBKOD
     _FldNameList[2]   = Temp-Tables.ptemp3.BENAMNING
     _Query            is NOT OPENED
*/  /* BROWSE BRW_P3 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_P5
/* Query rebuild information for BROWSE BRW_P5
     _TblList          = "Temp-Tables.ptemp5"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   = Temp-Tables.ptemp5.ARBKOD
     _FldNameList[2]   = Temp-Tables.ptemp5.BENAMNING
     _Query            is NOT OPENED
*/  /* BROWSE BRW_P5 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* <insert window title> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* <insert window title> */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_LOP1
&Scoped-define SELF-NAME BRW_LOP1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_LOP1 C-Win
ON MOUSE-MENU-CLICK OF BRW_LOP1 IN FRAME DEFAULT-FRAME /* Löpnr kopplade till arbetskoden */
DO: 
   ASSIGN
   status-ok = BRW_LOP1:DESELECT-ROWS() IN FRAME {&FRAME-NAME}.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_LOP1 C-Win
ON MOUSE-SELECT-DBLCLICK OF BRW_LOP1 IN FRAME DEFAULT-FRAME /* Löpnr kopplade till arbetskoden */
DO:
   APPLY "CHOOSE" TO BTN_SKAPA.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_LOP1 C-Win
ON VALUE-CHANGED OF BRW_LOP1 IN FRAME DEFAULT-FRAME /* Löpnr kopplade till arbetskoden */
DO:
   ASSIGN
   status-ok = BRW_LOP1:SELECT-FOCUSED-ROW() NO-ERROR.
   loprec = RECID(loptemp1). 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_LOP2
&Scoped-define SELF-NAME BRW_LOP2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_LOP2 C-Win
ON MOUSE-MENU-CLICK OF BRW_LOP2 IN FRAME DEFAULT-FRAME /* Löpnr kopplade till arbetskoden */
DO: 
   ASSIGN
   status-ok = BRW_LOP2:DESELECT-ROWS() IN FRAME {&FRAME-NAME}.     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_LOP2 C-Win
ON MOUSE-SELECT-DBLCLICK OF BRW_LOP2 IN FRAME DEFAULT-FRAME /* Löpnr kopplade till arbetskoden */
DO:
   APPLY "CHOOSE" TO BTN_SKAPA.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_LOP2 C-Win
ON VALUE-CHANGED OF BRW_LOP2 IN FRAME DEFAULT-FRAME /* Löpnr kopplade till arbetskoden */
DO:
   ASSIGN
   status-ok = BRW_LOP2:SELECT-FOCUSED-ROW() NO-ERROR.
   loprec = RECID(loptemp2).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_LOP3
&Scoped-define SELF-NAME BRW_LOP3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_LOP3 C-Win
ON MOUSE-MENU-CLICK OF BRW_LOP3 IN FRAME DEFAULT-FRAME /* Löpnr kopplade till arbetskoden */
DO: 
   ASSIGN
   status-ok = BRW_LOP3:DESELECT-ROWS() IN FRAME {&FRAME-NAME}.   
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_LOP3 C-Win
ON MOUSE-SELECT-DBLCLICK OF BRW_LOP3 IN FRAME DEFAULT-FRAME /* Löpnr kopplade till arbetskoden */
DO:
   APPLY "CHOOSE" TO BTN_SKAPA.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_LOP3 C-Win
ON VALUE-CHANGED OF BRW_LOP3 IN FRAME DEFAULT-FRAME /* Löpnr kopplade till arbetskoden */
DO:
   ASSIGN
   status-ok = BRW_LOP3:SELECT-FOCUSED-ROW() NO-ERROR.
   loprec = RECID(loptemp3).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_LOP5
&Scoped-define SELF-NAME BRW_LOP5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_LOP5 C-Win
ON MOUSE-MENU-CLICK OF BRW_LOP5 IN FRAME DEFAULT-FRAME /* Löpnr kopplade till arbetskoden */
DO: 
   
   ASSIGN
   status-ok = BRW_LOP5:DESELECT-ROWS() IN FRAME {&FRAME-NAME}.   
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_LOP5 C-Win
ON MOUSE-SELECT-DBLCLICK OF BRW_LOP5 IN FRAME DEFAULT-FRAME /* Löpnr kopplade till arbetskoden */
DO:
   APPLY "CHOOSE" TO BTN_SKAPA.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_LOP5 C-Win
ON VALUE-CHANGED OF BRW_LOP5 IN FRAME DEFAULT-FRAME /* Löpnr kopplade till arbetskoden */
DO:
   ASSIGN
   status-ok = BRW_LOP5:SELECT-FOCUSED-ROW() NO-ERROR.
   loprec = RECID(loptemp5).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_P1
&Scoped-define SELF-NAME BRW_P1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_P1 C-Win
ON VALUE-CHANGED OF BRW_P1 IN FRAME DEFAULT-FRAME /* Arbetskoder typ 1 */
DO:
   ASSIGN
   status-ok = BRW_P1:SELECT-FOCUSED-ROW() NO-ERROR.
   prec = RECID(ptemp1). 
   OPEN QUERY BRW_LOP1 FOR EACH loptemp1
   WHERE loptemp1.ARBKOD = ptemp1.ARBKOD AND loptemp1.KATAR = katvar USE-INDEX AR NO-LOCK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_P2
&Scoped-define SELF-NAME BRW_P2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_P2 C-Win
ON VALUE-CHANGED OF BRW_P2 IN FRAME DEFAULT-FRAME /* Arbetskoder typ 2 */
DO:
   ASSIGN
   status-ok = BRW_P2:SELECT-FOCUSED-ROW() NO-ERROR.
   prec = RECID(ptemp2).
   OPEN QUERY BRW_LOP2 FOR EACH loptemp2
   WHERE loptemp2.ARBKOD = ptemp2.ARBKOD AND loptemp2.KATAR = katvar USE-INDEX AR NO-LOCK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_P3
&Scoped-define SELF-NAME BRW_P3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_P3 C-Win
ON VALUE-CHANGED OF BRW_P3 IN FRAME DEFAULT-FRAME /* Arbetskoder typ 3 */
DO:
   ASSIGN
   status-ok = BRW_P3:SELECT-FOCUSED-ROW() NO-ERROR.
   prec = RECID(ptemp3).
   OPEN QUERY BRW_LOP3 FOR EACH loptemp3
   WHERE loptemp3.ARBKOD = ptemp3.ARBKOD AND loptemp3.KATAR = katvar USE-INDEX AR NO-LOCK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_P5
&Scoped-define SELF-NAME BRW_P5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_P5 C-Win
ON VALUE-CHANGED OF BRW_P5 IN FRAME DEFAULT-FRAME /* Arbetskoder sambyggnad/samförläggning */
DO:
   ASSIGN
   status-ok = BRW_P5:SELECT-FOCUSED-ROW() NO-ERROR.
   prec = RECID(ptemp5).
   OPEN QUERY BRW_LOP5 FOR EACH loptemp5
   WHERE loptemp5.ARBKOD = ptemp5.ARBKOD AND loptemp5.KATAR = skatvar USE-INDEX AR NO-LOCK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_ALLBACK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_ALLBACK C-Win
ON CHOOSE OF BTN_ALLBACK IN FRAME DEFAULT-FRAME /* Alla aonr i listan */
DO:
   EMPTY TEMP-TABLE kalk_temp NO-ERROR. 
   RUN refreshbrw_UI IN brwproc[9].
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_ALLOVER
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_ALLOVER C-Win
ON CHOOSE OF BTN_ALLOVER IN FRAME DEFAULT-FRAME /* Alla aonr i listan */
DO:
   IF typ_val = 1 THEN DO:
      GET FIRST BRW_LOP1 NO-LOCK.
      DO WHILE AVAILABLE(loptemp1):
         FIND FIRST kalk_temp WHERE kalk_temp.ARBKOD = loptemp1.ARBKOD AND
         kalk_temp.LOPNR = loptemp1.LOPNR NO-LOCK NO-ERROR.
         IF NOT AVAILABLE kalk_temp THEN DO:
            RUN skapatyp1_UI.
         END.
         GET NEXT BRW_LOP1 NO-LOCK.
      END.
   END.
   ELSE IF typ_val = 2 THEN DO:
      GET FIRST BRW_LOP2 NO-LOCK.
      DO WHILE AVAILABLE(loptemp2):
         FIND FIRST kalk_temp WHERE kalk_temp.ARBKOD = loptemp2.ARBKOD AND
         kalk_temp.LOPNR = loptemp2.LOPNR NO-LOCK NO-ERROR.
         IF NOT AVAILABLE kalk_temp THEN DO:
            RUN skapatyp2_UI.
         END.
         GET NEXT BRW_LOP2 NO-LOCK.
      END.
   END.
   ELSE IF typ_val = 3 THEN DO:
      GET FIRST BRW_LOP3 NO-LOCK.
      DO WHILE AVAILABLE(loptemp3):
         FIND FIRST kalk_temp WHERE kalk_temp.ARBKOD = loptemp3.ARBKOD AND
         kalk_temp.LOPNR = loptemp3.LOPNR NO-LOCK NO-ERROR.
         IF NOT AVAILABLE kalk_temp THEN DO:
            RUN skapatyp3_UI.
         END.
         GET NEXT BRW_LOP3 NO-LOCK.
      END.
   END.   
   OPEN QUERY BRW_KALK FOR EACH kalk_temp USE-INDEX KOD NO-LOCK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB C-Win
ON CHOOSE OF BTN_AVB IN FRAME DEFAULT-FRAME /* Avbryt */
DO:
   APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_BORT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_BORT C-Win
ON CHOOSE OF BTN_BORT IN FRAME DEFAULT-FRAME /* Ta bort */
DO:
   RUN btnbort_UI.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_SKAPA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_SKAPA C-Win
ON CHOOSE OF BTN_SKAPA IN FRAME DEFAULT-FRAME /* Skapa */
DO:
   {muswait.i}
   antalvar = 1.
   IF typ_val = 1 THEN DO:
      antal_valda = BRW_LOP1:NUM-SELECTED-ROWS.
      antal_raknare = 1.
      DO WHILE antal_raknare LE antal_valda:
         status-ok = BRW_LOP1:FETCH-SELECTED-ROW(antal_raknare).         
         RUN skapatyp1_UI.   
         valrec = RECID(kalk_temp).          
         antal_raknare = antal_raknare + 1.
      END.
      ASSIGN
      status-ok = BRW_LOP1:DESELECT-ROWS() IN FRAME {&FRAME-NAME}.   
   END.
   ELSE IF typ_val = 2 THEN DO:
      antal_valda = BRW_LOP2:NUM-SELECTED-ROWS.
      antal_raknare = 1.
      DO WHILE antal_raknare LE antal_valda:
         status-ok = BRW_LOP2:FETCH-SELECTED-ROW(antal_raknare).         
         RUN skapatyp2_UI.   
         valrec = RECID(kalk_temp).          
         antal_raknare = antal_raknare + 1.
      END.
      ASSIGN
      status-ok = BRW_LOP2:DESELECT-ROWS() IN FRAME {&FRAME-NAME}.   
   END.
   ELSE IF typ_val = 3 THEN DO:
      antal_valda = BRW_LOP3:NUM-SELECTED-ROWS.
      antal_raknare = 1.
      DO WHILE antal_raknare LE antal_valda:
         status-ok = BRW_LOP3:FETCH-SELECTED-ROW(antal_raknare).         
         RUN skapatyp3_UI.   
         valrec = RECID(kalk_temp).          
         antal_raknare = antal_raknare + 1.
      END.
      ASSIGN
      status-ok = BRW_LOP3:DESELECT-ROWS() IN FRAME {&FRAME-NAME}.   
   END.   
   ELSE IF typ_val = 5 THEN DO:
      antal_valda = BRW_LOP5:NUM-SELECTED-ROWS.
      antal_raknare = 1.
      DO WHILE antal_raknare LE antal_valda:
         status-ok = BRW_LOP5:FETCH-SELECTED-ROW(antal_raknare).         
         RUN skapatyp5_UI.   
         valrec = RECID(kalk_temp).          
         antal_raknare = antal_raknare + 1.
      END.
      ASSIGN
      status-ok = BRW_LOP5:DESELECT-ROWS() IN FRAME {&FRAME-NAME}.   
   END.   
   OPEN QUERY BRW_KALK FOR EACH kalk_temp USE-INDEX KOD NO-LOCK.
   FIND FIRST kalk_temp WHERE RECID(kalk_temp) = valrec NO-LOCK NO-ERROR.
   IF AVAILABLE kalk_temp THEN DO: 
      RUN setlastrowid_UI IN brwproc[9] (INPUT ROWID(kalk_temp)).
      RUN lastselectdyn_UI IN brwproc[9].
   END.  
 
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_VISA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_VISA C-Win
ON CHOOSE OF BTN_VISA IN FRAME DEFAULT-FRAME /* Visa i excel */
DO:
   {muswait.i}
   skrivut = FALSE.
   musz = FALSE.   
   
   EMPTY TEMP-TABLE kalk_temp2 NO-ERROR. 
   IF musz = FALSE THEN DO:
      {AVBGOM.I}                 
      IF Guru.Konstanter:appcon THEN DO:
         RUN VKALKUPU1.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
         (INPUT utomr, INPUT period, INPUT bdatum,
         INPUT avdatum, INPUT typ_val, INPUT TABLE kalk_temp, OUTPUT TABLE kalk_temp2).
      END.
      ELSE DO:
         RUN VKALKUPU1.P
         (INPUT utomr, INPUT period, INPUT bdatum,
         INPUT avdatum, INPUT typ_val,  INPUT TABLE kalk_temp, OUTPUT TABLE kalk_temp2).
      END.
      RUN VKALKUPPU.P (INPUT utomr, INPUT period, INPUT bdatum,
      INPUT avdatum, INPUT typ_val, INPUT Guru.Konstanter:globforetag, INPUT TABLE kalk_temp2).           
      {AVBFRAM.I}
   END.
   ELSE musz = FALSE.
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-BEN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-BEN C-Win
ON ANY-KEY OF FILL-IN-BEN IN FRAME DEFAULT-FRAME /* Benämning */
DO:
   {TRYCKS.I}
    
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN-BEN IN FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-BEN C-Win
ON MOUSE-SELECT-DBLCLICK OF FILL-IN-BEN IN FRAME DEFAULT-FRAME /* Benämning */
DO:
   FILL-IN-BEN = INPUT FILL-IN-BEN.
   IF typ_val = 1 THEN DO:
      status-ok = BRW_P1:SELECT-FOCUSED-ROW() NO-ERROR.
      APPLY "VALUE-CHANGED" TO BRW_P1.
      prec = RECID(ptemp1).
      IF FILL-IN-BEN = '' THEN DO:
         MESSAGE "Sökbegreppet kan inte vara blankt." VIEW-AS ALERT-BOX.
         APPLY "ENTRY" TO FILL-IN-BEN IN FRAME {&FRAME-NAME}.
         RETURN NO-APPLY.
      END.      
      ortsok = '*' + FILL-IN-BEN + '*'.   
      FIND ptemp1 WHERE RECID(ptemp1) = prec NO-LOCK NO-ERROR.
      FIND NEXT ptemp1 WHERE ptemp1.BENAMNING MATCHES ortsok 
      AND ptemp1.KATAR = katvar NO-LOCK NO-ERROR.   
      IF NOT AVAILABLE ptemp1 THEN DO:      
         FIND FIRST ptemp1 WHERE ptemp1.BENAMNING MATCHES ortsok
         AND ptemp1.KATAR = katvar  NO-LOCK NO-ERROR.      
         IF NOT AVAILABLE ptemp1 THEN DO:
            MESSAGE "Det finns inget på sökbegreppet." VIEW-AS ALERT-BOX.
            APPLY "ENTRY" TO FILL-IN-BEN IN FRAME {&FRAME-NAME}.
            RETURN NO-APPLY.
         END.
      END.
      IF AVAILABLE ptemp1 THEN DO:      
         RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(ptemp1)).
         RUN lastselectdyn_UI IN brwproc[1].
      END.
   END.
   ELSE IF typ_val = 2 THEN DO:
      status-ok = BRW_P2:SELECT-FOCUSED-ROW() NO-ERROR.
      APPLY "VALUE-CHANGED" TO BRW_P2.
      prec = RECID(ptemp2).
      IF FILL-IN-BEN = '' THEN DO:
         MESSAGE "Sökbegreppet kan inte vara blankt." VIEW-AS ALERT-BOX.
         APPLY "ENTRY" TO FILL-IN-BEN IN FRAME {&FRAME-NAME}.
         RETURN NO-APPLY.
      END.      
      ortsok = '*' + FILL-IN-BEN + '*'.   
      FIND ptemp2 WHERE RECID(ptemp2) = prec NO-LOCK NO-ERROR.
      FIND NEXT ptemp2 WHERE ptemp2.BENAMNING MATCHES ortsok 
      AND ptemp2.KATAR = katvar  NO-LOCK NO-ERROR.   
      IF NOT AVAILABLE ptemp2 THEN DO:      
         FIND FIRST ptemp2 WHERE ptemp2.BENAMNING MATCHES ortsok
         AND ptemp2.KATAR = katvar  NO-LOCK NO-ERROR.      
         IF NOT AVAILABLE ptemp2 THEN DO:
            MESSAGE "Det finns inget på sökbegreppet." VIEW-AS ALERT-BOX.
            APPLY "ENTRY" TO FILL-IN-BEN IN FRAME {&FRAME-NAME}.
            RETURN NO-APPLY.
         END.
      END.
      IF AVAILABLE ptemp2 THEN DO:
         RUN setlastrowid_UI IN brwproc[2] (INPUT ROWID(ptemp2)).
         RUN lastselectdyn_UI IN brwproc[2].
      END.
   END.  
   ELSE IF typ_val = 3 THEN DO:
      status-ok = BRW_P3:SELECT-FOCUSED-ROW() NO-ERROR.
      APPLY "VALUE-CHANGED" TO BRW_P3.
      prec = RECID(ptemp3).
      IF FILL-IN-BEN = '' THEN DO:
         MESSAGE "Sökbegreppet kan inte vara blankt." VIEW-AS ALERT-BOX.
         APPLY "ENTRY" TO FILL-IN-BEN IN FRAME {&FRAME-NAME}.
         RETURN NO-APPLY.
      END.      
      ortsok = '*' + FILL-IN-BEN + '*'.   
      FIND ptemp3 WHERE RECID(ptemp3) = prec NO-LOCK NO-ERROR.
      FIND NEXT ptemp3 WHERE ptemp3.BENAMNING MATCHES ortsok 
      AND ptemp3.KATAR = katvar  NO-LOCK NO-ERROR.   
      IF NOT AVAILABLE ptemp3 THEN DO:      
         FIND FIRST ptemp3 WHERE ptemp3.BENAMNING MATCHES ortsok
         AND ptemp3.KATAR = katvar  NO-LOCK NO-ERROR.      
         IF NOT AVAILABLE ptemp3 THEN DO:
            MESSAGE "Det finns inget på sökbegreppet." VIEW-AS ALERT-BOX.
            APPLY "ENTRY" TO FILL-IN-BEN IN FRAME {&FRAME-NAME}.
            RETURN NO-APPLY.
         END.
      END.
      IF AVAILABLE ptemp3 THEN DO:
         RUN setlastrowid_UI IN brwproc[3] (INPUT ROWID(ptemp3)).
         RUN lastselectdyn_UI IN brwproc[3].
      END.
   END.
   ELSE IF typ_val = 5 THEN DO:
      status-ok = BRW_P5:SELECT-FOCUSED-ROW() NO-ERROR.
      APPLY "VALUE-CHANGED" TO BRW_P5.
      prec = RECID(ptemp5).
      IF FILL-IN-BEN = '' THEN DO:
         MESSAGE "Sökbegreppet kan inte vara blankt." VIEW-AS ALERT-BOX.
         APPLY "ENTRY" TO FILL-IN-BEN IN FRAME {&FRAME-NAME}.
         RETURN NO-APPLY.
      END.      
      ortsok = '*' + FILL-IN-BEN + '*'.   
      FIND ptemp5 WHERE RECID(ptemp5) = prec NO-LOCK NO-ERROR.
      FIND NEXT ptemp5 WHERE ptemp5.BENAMNING MATCHES ortsok 
      AND ptemp5.KATAR = skatvar  NO-LOCK NO-ERROR.   
      IF NOT AVAILABLE ptemp5 THEN DO:      
         FIND FIRST ptemp5 WHERE ptemp5.BENAMNING MATCHES ortsok
         AND ptemp5.KATAR = skatvar  NO-LOCK NO-ERROR.      
         IF NOT AVAILABLE ptemp5 THEN DO:
            MESSAGE "Det finns inget på sökbegreppet." VIEW-AS ALERT-BOX.
            APPLY "ENTRY" TO FILL-IN-BEN IN FRAME {&FRAME-NAME}.
            RETURN NO-APPLY.
         END.
      END.
      IF AVAILABLE ptemp5 THEN DO:
         RUN setlastrowid_UI IN brwproc[4] (INPUT ROWID(ptemp5)).
         RUN lastselectdyn_UI IN brwproc[4].
      END.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-KOD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-KOD C-Win
ON ANY-KEY OF FILL-IN-KOD IN FRAME DEFAULT-FRAME /* Arbetskod */
DO:
   {TRYCKS.I}
    
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN-KOD IN FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-KOD C-Win
ON MOUSE-SELECT-DBLCLICK OF FILL-IN-KOD IN FRAME DEFAULT-FRAME /* Arbetskod */
DO:
   FILL-IN-KOD = INPUT FILL-IN-KOD.
   IF typ_val = 1 THEN DO:
      status-ok = BRW_P1:SELECT-FOCUSED-ROW() NO-ERROR.
      APPLY "VALUE-CHANGED" TO BRW_P1.
      prec = RECID(ptemp1).
      IF FILL-IN-KOD = '' THEN DO:
         MESSAGE "Sökbegreppet kan inte vara blankt." VIEW-AS ALERT-BOX.
         APPLY "ENTRY" TO FILL-IN-KOD IN FRAME {&FRAME-NAME}.
         RETURN NO-APPLY.
      END.      
      aosok = '*' + FILL-IN-KOD + '*'.   
      FIND ptemp1 WHERE RECID(ptemp1) = prec NO-LOCK NO-ERROR.
      FIND NEXT ptemp1 WHERE ptemp1.ARBKOD MATCHES aosok 
      AND ptemp1.KATAR = katvar USE-INDEX AR NO-LOCK NO-ERROR.   
      IF NOT AVAILABLE ptemp1 THEN DO:      
         FIND FIRST ptemp1 WHERE ptemp1.ARBKOD MATCHES aosok
         AND ptemp1.KATAR = katvar USE-INDEX AR NO-LOCK NO-ERROR.      
         IF NOT AVAILABLE ptemp1 THEN DO:
            MESSAGE "Det finns inget på sökbegreppet." VIEW-AS ALERT-BOX.
            APPLY "ENTRY" TO FILL-IN-KOD IN FRAME {&FRAME-NAME}.
            RETURN NO-APPLY.
         END.
      END.
      IF AVAILABLE ptemp1 THEN DO:
         RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(ptemp1)).
         RUN lastselectdyn_UI IN brwproc[1].
      END.
   END.
   ELSE IF typ_val = 2 THEN DO:
      status-ok = BRW_P2:SELECT-FOCUSED-ROW() NO-ERROR.
      APPLY "VALUE-CHANGED" TO BRW_P2.
      prec = RECID(ptemp2).
      IF FILL-IN-KOD = '' THEN DO:
         MESSAGE "Sökbegreppet kan inte vara blankt." VIEW-AS ALERT-BOX.
         APPLY "ENTRY" TO FILL-IN-KOD IN FRAME {&FRAME-NAME}.
         RETURN NO-APPLY.
      END.      
      aosok = '*' + FILL-IN-KOD + '*'.   
      FIND ptemp2 WHERE RECID(ptemp2) = prec NO-LOCK NO-ERROR.
      FIND NEXT ptemp2 WHERE ptemp2.ARBKOD MATCHES aosok 
      AND ptemp2.KATAR = katvar USE-INDEX AR NO-LOCK NO-ERROR.   
      IF NOT AVAILABLE ptemp2 THEN DO:      
         FIND FIRST ptemp2 WHERE ptemp2.ARBKOD MATCHES aosok
         AND ptemp2.KATAR = katvar USE-INDEX AR NO-LOCK NO-ERROR.      
         IF NOT AVAILABLE ptemp2 THEN DO:
            MESSAGE "Det finns inget på sökbegreppet." VIEW-AS ALERT-BOX.
            APPLY "ENTRY" TO FILL-IN-KOD IN FRAME {&FRAME-NAME}.
            RETURN NO-APPLY.
         END.
      END.
      IF AVAILABLE ptemp2 THEN DO:
         RUN setlastrowid_UI IN brwproc[2] (INPUT ROWID(ptemp2)).
         RUN lastselectdyn_UI IN brwproc[2].
      END.
   END.  
   ELSE IF typ_val = 3 THEN DO:
      status-ok = BRW_P3:SELECT-FOCUSED-ROW() NO-ERROR.
      APPLY "VALUE-CHANGED" TO BRW_P3.
      prec = RECID(ptemp3).
      IF FILL-IN-KOD = '' THEN DO:
         MESSAGE "Sökbegreppet kan inte vara blankt." VIEW-AS ALERT-BOX.
         APPLY "ENTRY" TO FILL-IN-KOD IN FRAME {&FRAME-NAME}.
         RETURN NO-APPLY.
      END.      
      aosok = '*' + FILL-IN-KOD + '*'.   
      FIND ptemp3 WHERE RECID(ptemp3) = prec NO-LOCK NO-ERROR.
      FIND NEXT ptemp3 WHERE ptemp3.ARBKOD MATCHES aosok 
      AND ptemp3.KATAR = katvar USE-INDEX AR NO-LOCK NO-ERROR.   
      IF NOT AVAILABLE ptemp3 THEN DO:      
         FIND FIRST ptemp3 WHERE ptemp3.ARBKOD MATCHES aosok
         AND ptemp3.KATAR = katvar USE-INDEX AR NO-LOCK NO-ERROR.      
         IF NOT AVAILABLE ptemp3 THEN DO:
            MESSAGE "Det finns inget på sökbegreppet." VIEW-AS ALERT-BOX.
            APPLY "ENTRY" TO FILL-IN-KOD IN FRAME {&FRAME-NAME}.
            RETURN NO-APPLY.
         END.
      END.
      IF AVAILABLE ptemp3 THEN DO:
         RUN setlastrowid_UI IN brwproc[3] (INPUT ROWID(ptemp3)).
         RUN lastselectdyn_UI IN brwproc[3].
      END.
   END. 
   ELSE IF typ_val = 5 THEN DO:
      status-ok = BRW_P5:SELECT-FOCUSED-ROW() NO-ERROR.
      APPLY "VALUE-CHANGED" TO BRW_P5.
      prec = RECID(ptemp5).
      IF FILL-IN-KOD = '' THEN DO:
         MESSAGE "Sökbegreppet kan inte vara blankt." VIEW-AS ALERT-BOX.
         APPLY "ENTRY" TO FILL-IN-KOD IN FRAME {&FRAME-NAME}.
         RETURN NO-APPLY.
      END.      
      aosok = '*' + FILL-IN-KOD + '*'.   
      FIND ptemp5 WHERE RECID(ptemp5) = prec NO-LOCK NO-ERROR.
      FIND NEXT ptemp5 WHERE ptemp5.ARBKOD MATCHES aosok 
      AND ptemp5.KATAR = skatvar USE-INDEX AR NO-LOCK NO-ERROR.   
      IF NOT AVAILABLE ptemp5 THEN DO:      
         FIND FIRST ptemp5 WHERE ptemp5.ARBKOD MATCHES aosok
         AND ptemp5.KATAR = skatvar USE-INDEX AR NO-LOCK NO-ERROR.      
         IF NOT AVAILABLE ptemp5 THEN DO:
            MESSAGE "Det finns inget på sökbegreppet." VIEW-AS ALERT-BOX.
            APPLY "ENTRY" TO FILL-IN-KOD IN FRAME {&FRAME-NAME}.
            RETURN NO-APPLY.
         END.
      END.
      IF AVAILABLE ptemp5 THEN DO:
         RUN setlastrowid_UI IN brwproc[4] (INPUT ROWID(ptemp5)).
         RUN lastselectdyn_UI IN brwproc[4].
      END.
   END. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_KALK
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
DO:
   {BORTBRWPROC.I}
   IF VALID-HANDLE(udokalkylapph) THEN DELETE PROCEDURE udokalkylapph.
   RUN disable_UI.
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
   ASSIGN
   katvar = YEAR(bdatum)
   skatvar = YEAR(bdatum).  
   {ALLSTARTDYN.I}
   FIND FIRST omrtemp WHERE omrtemp.OMRADE = utomr 
   USE-INDEX OMR NO-LOCK NO-ERROR.
   IF typ_val = 1 THEN DO:
      {&WINDOW-NAME}:TITLE = "Uppföljning kalkyl typ 1 " + STRING(katvar) + " " +
      omrtemp.NAMN.   
   END.
   ELSE IF typ_val = 2 THEN DO:
      {&WINDOW-NAME}:TITLE = "Uppföljning kalkyl typ 2 " + STRING(katvar) + " " +
      omrtemp.NAMN.      
   END.
   ELSE IF typ_val = 3 THEN DO:
      {&WINDOW-NAME}:TITLE = "Uppföljning kalkyl typ 3 " + STRING(katvar) + " " +
      omrtemp.NAMN.
   END.       
   RUN enable_UI.   
   {FRMSIZE.I}   
   RUN visa_UI.
   {musarrow.i}
   {WIN_M_SLUT.I}
   IF NOT THIS-PROCEDURE:PERSISTENT THEN
   WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE allstartbrw_UI C-Win 
PROCEDURE allstartbrw_UI :
/* -----------------------------------------------------------
  Purpose: Changing screen-value for combo-box CMB_OMR     
  Parameters:  Input = Screen-value for CMB_FOR
  Notes:       
-------------------------------------------------------------*/ 
   
   RUN DYNBRW.P PERSISTENT SET brwproc[1]
      (INPUT BRW_P1:HANDLE IN FRAME {&FRAME-NAME}).  
   RUN DYNBRW.P PERSISTENT SET brwproc[2]
      (INPUT BRW_P2:HANDLE IN FRAME {&FRAME-NAME}).  
   RUN DYNBRW.P PERSISTENT SET brwproc[3]
      (INPUT BRW_P3:HANDLE IN FRAME {&FRAME-NAME}).  
   RUN DYNBRW.P PERSISTENT SET brwproc[4]
      (INPUT BRW_P5:HANDLE IN FRAME {&FRAME-NAME}).  
   RUN DYNBRW.P PERSISTENT SET brwproc[5]
      (INPUT BRW_LOP1:HANDLE IN FRAME {&FRAME-NAME}).  
   RUN DYNBRW.P PERSISTENT SET brwproc[6]
      (INPUT BRW_LOP2:HANDLE IN FRAME {&FRAME-NAME}).  
   RUN DYNBRW.P PERSISTENT SET brwproc[7]
      (INPUT BRW_LOP3:HANDLE IN FRAME {&FRAME-NAME}).     
   RUN DYNBRW.P PERSISTENT SET brwproc[8]
      (INPUT BRW_LOP5:HANDLE IN FRAME {&FRAME-NAME}).  
    RUN DYNBRW.P PERSISTENT SET brwproc[9]
      (INPUT BRW_KALK:HANDLE IN FRAME {&FRAME-NAME}).  

   IF typ_val = 1 THEN DO:
      tthandle = TEMP-TABLE loptemp1:HANDLE.    
      tabvar = "LOP1".
   END.
   ELSE IF typ_val = 2 THEN DO: 
      tthandle = TEMP-TABLE loptemp2:HANDLE.
      tabvar = "LOP2".
   END.
   ELSE IF typ_val = 3 THEN DO: 
      tthandle = TEMP-TABLE loptemp3:HANDLE.  
      tabvar = "LOP3".
   END.
   ELSE IF typ_val = 5 THEN DO: 
      tthandle = TEMP-TABLE loptemp5:HANDLE.  
      tabvar = "LOP5".
   END.
   IF Guru.Konstanter:appcon THEN DO:
      RUN DYNLADDATEMP.P PERSISTENT SET laddaproch ON Guru.Konstanter:apphand TRANSACTION DISTINCT
         (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT tabvar, INPUT "").
   END.
   ELSE DO:
      RUN DYNLADDATEMP.P PERSISTENT SET laddaproch
         (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT tabvar, INPUT "").
   END.  
   IF typ_val = 1 THEN DO:
      tthandle = TEMP-TABLE ptemp1:HANDLE.
      RUN laddatemp_UI IN laddaproch (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "P1", INPUT "").
   END.
   ELSE IF typ_val = 2 THEN DO: 
      tthandle = TEMP-TABLE ptemp2:HANDLE.
      RUN laddatemp_UI IN laddaproch (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "P2", INPUT "").
   END.
   ELSE IF typ_val = 3 THEN DO: 
      tthandle = TEMP-TABLE ptemp3:HANDLE.
      RUN laddatemp_UI IN laddaproch (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "P3", INPUT "").
   END.
   ELSE IF typ_val = 5 THEN DO: 
      tthandle = TEMP-TABLE ptemp5:HANDLE.
      RUN laddatemp_UI IN laddaproch (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "P5", INPUT "").
   END.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnbort_UI C-Win 
PROCEDURE btnbort_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   {muswait.i}
   ASSIGN
   antal_valda = BRW_KALK:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME}
   antal_raknare = 1.
   DO WHILE antal_raknare LE antal_valda:
      status-ok = BRW_KALK:FETCH-SELECTED-ROW(antal_raknare).   
      bortrec = RECID(kalk_temp).         
      IF antal_raknare = 1 AND antal_valda > 1 THEN DO:
         MESSAGE "Vill du ta bort markerade poster ?"
         VIEW-AS ALERT-BOX
         QUESTION BUTTONS YES-NO TITLE "Ta bort?" UPDATE svar AS LOGICAL.                    
      END.
      ELSE IF antal_valda = 1 THEN DO:
         MESSAGE "Vill du ta bort arbetskod:" + kalk_temp.ARBKOD + " löpnr:" + STRING(kalk_temp.LOPNR,Guru.Konstanter:varforetypchar[6]) + "?"
         VIEW-AS ALERT-BOX
         QUESTION BUTTONS YES-NO TITLE "Ta bort?" UPDATE svar.         
      END.        
      IF svar THEN DO TRANSACTION:
         FIND kalk_temp WHERE RECID(kalk_temp) = bortrec EXCLUSIVE-LOCK NO-ERROR.         
         DELETE kalk_temp.
      END.
      RUN valrattright_UI IN brwproc[9] 
      (INPUT antal_raknare,INPUT svar,INPUT ROWID(kalk_temp)).
      antal_raknare = antal_raknare + 1.
   END.   
   RUN refreshbrw_UI IN brwproc[9].
   RUN lastselectdyn_UI IN brwproc[9].
   {musarrow.i}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
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
  DISPLAY FILL-IN-KOD FILL-IN-BEN 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE BTN_VISA FILL-IN-KOD FILL-IN-BEN BTN_AVB BRW_KALK BTN_SKAPA BTN_BORT 
         BRW_LOP5 BTN_ALLOVER BTN_ALLBACK RECT-22 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE skapatyp1_UI C-Win 
PROCEDURE skapatyp1_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DO TRANSACTION:
      CREATE kalk_temp.
      ASSIGN      
      kalk_temp.ARBKOD = loptemp1.ARBKOD
      kalk_temp.LOPNR = loptemp1.LOPNR
      kalk_temp.BENAMNING = loptemp1.BENAMNING
      kalk_temp.ENHET = loptemp1.ENHET.
   END.
   valrec = RECID(kalk_temp).   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE skapatyp2_UI C-Win 
PROCEDURE skapatyp2_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DO TRANSACTION:
      CREATE kalk_temp.
      ASSIGN      
      kalk_temp.ARBKOD = loptemp2.ARBKOD
      kalk_temp.LOPNR = loptemp2.LOPNR
      kalk_temp.BENAMNING = loptemp2.BENAMNING
      kalk_temp.ENHET = loptemp2.ENHET.
   END.
   valrec = RECID(kalk_temp).   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE skapatyp3_UI C-Win 
PROCEDURE skapatyp3_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DO TRANSACTION:
      CREATE kalk_temp.
      ASSIGN          
      kalk_temp.ARBKOD = loptemp3.ARBKOD
      kalk_temp.LOPNR = loptemp3.LOPNR
      kalk_temp.BENAMNING = loptemp3.BENAMNING
      kalk_temp.ENHET = loptemp3.ENHET.
   END.
   valrec = RECID(kalk_temp).   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE skapatyp5_UI C-Win 
PROCEDURE skapatyp5_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DO TRANSACTION:
      CREATE kalk_temp.
      ASSIGN      
      kalk_temp.ARBKOD = loptemp5.ARBKOD
      kalk_temp.LOPNR = loptemp5.LOPNR
      kalk_temp.BENAMNING = loptemp5.BENAMNING
      kalk_temp.ENHET = loptemp5.ENHET.
   END.
   valrec = RECID(kalk_temp).   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE visa_UI C-Win 
PROCEDURE visa_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/         
      IF typ_val = 1 THEN DO:       
         ENABLE BRW_P1 WITH FRAME {&FRAME-NAME}.
         ENABLE BRW_LOP1 WITH FRAME {&FRAME-NAME}.         
         OPEN QUERY BRW_P1 FOR EACH ptemp1 WHERE ptemp1.KATAR = katvar USE-INDEX AR NO-LOCK INDEXED-REPOSITION.
         APPLY "VALUE-CHANGED" TO BRW_P1 IN FRAME {&FRAME-NAME}.
      END.
      ELSE IF typ_val = 2 THEN DO:       
         ENABLE BRW_P2 WITH FRAME {&FRAME-NAME}.
         ENABLE BRW_LOP2 WITH FRAME {&FRAME-NAME}.         
         OPEN QUERY BRW_P2 FOR EACH ptemp2 WHERE ptemp2.KATAR = katvar USE-INDEX AR NO-LOCK INDEXED-REPOSITION.
         APPLY "VALUE-CHANGED" TO BRW_P2 IN FRAME {&FRAME-NAME}.
      END.
      ELSE IF typ_val = 3 THEN DO:        
         ENABLE BRW_P3 WITH FRAME {&FRAME-NAME}.
         ENABLE BRW_LOP3 WITH FRAME {&FRAME-NAME}.         
         OPEN QUERY BRW_P3 FOR EACH ptemp3 WHERE ptemp3.KATAR = katvar USE-INDEX AR NO-LOCK INDEXED-REPOSITION.
         APPLY "VALUE-CHANGED" TO BRW_P3 IN FRAME {&FRAME-NAME}.
      END. 
      ELSE IF typ_val = 5 THEN DO:        
         ENABLE BRW_P5 WITH FRAME {&FRAME-NAME}.
         ENABLE BRW_LOP5 WITH FRAME {&FRAME-NAME}.         
         OPEN QUERY BRW_P5 FOR EACH ptemp5 WHERE ptemp5.KATAR = skatvar USE-INDEX AR NO-LOCK INDEXED-REPOSITION.
         APPLY "VALUE-CHANGED" TO BRW_P5 IN FRAME {&FRAME-NAME}.
      END. 
      ASSIGN
      BTN_SKAPA:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      BTN_BORT:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      FILL-IN-KOD:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      FILL-IN-BEN:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

