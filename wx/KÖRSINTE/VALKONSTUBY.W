&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win





&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File:  VALKONSTUBY.W Byt undergrupp - ej mtrl JUST NU ej i drift.

  Description: 

  Input Parameters:
      <none>

  Output Parameters:CCC
      <none>

  Author: 

  Created: 

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
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
{BERIDTEMP.I}
{BERMTRL.I}
{BBENAMNTEMP.I}
&Scoped-define NEW 
&Scoped-define SHARED SHARED
{KONVALTEMP.I}
/*{KONSTGRUPP.I}*/

{KONSTRMTRL.I}
&Scoped-define NEW NEW
{KONVAL2TEMP.I}
{BEREDNINGTEMP.I}
{F1TILL6.I}



DEFINE NEW SHARED VARIABLE avbvar AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE kalkrow2 AS ROWID NO-UNDO.
DEFINE NEW SHARED VARIABLE valaonr2 AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE valdelnr2 AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE valort2 AS CHARACTER NO-UNDO. 
DEFINE NEW SHARED VARIABLE valomrade2 AS CHARACTER NO-UNDO. 
DEFINE NEW SHARED VARIABLE katvar2 AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE kalkvar2 AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE aonrrow2 AS ROWID NO-UNDO.

DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE valaonr AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE valdelnr AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE valomrade AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE kalkrow AS ROWID NO-UNDO.
DEFINE VARIABLE valdatum AS DATE NO-UNDO.
DEFINE VARIABLE antal_valda AS INTEGER NO-UNDO.
DEFINE VARIABLE antal_raknare AS INTEGER NO-UNDO. 
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE spec_rowid AS ROWID NO-UNDO.
DEFINE VARIABLE ordning AS INTEGER NO-UNDO.
DEFINE VARIABLE kombnr AS INTEGER NO-UNDO.
DEFINE VARIABLE valkonstapph AS HANDLE NO-UNDO.
DEFINE VARIABLE konmtrlapph AS HANDLE NO-UNDO.
DEFINE VARIABLE ktypkodvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE bernumvar AS INTEGER NO-UNDO.
DEFINE VARIABLE laddaproch AS HANDLE NO-UNDO.
DEFINE VARIABLE datvar99 AS DATE NO-UNDO.
DEFINE NEW SHARED TEMP-TABLE list_mtrl2 
   {LISTMTRLTT.I}
DEFINE TEMP-TABLE ot NO-UNDO
      FIELD ORD AS INTEGER
      FIELD OROW AS ROWID
      INDEX ORD ORD
      INDEX NORD ORD DESCENDING.
         
DEFINE BUFFER idbuff FOR beridtemp.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BRW_B2

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES konstvaltemp konsttemp kon_val kon_val2

/* Definitions for BROWSE BRW_B2                                        */
&Scoped-define FIELDS-IN-QUERY-BRW_B2 konstvaltemp.KVALKOD ~
konstvaltemp.BENAMNING konstvaltemp.BILD 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_B2 
&Scoped-define QUERY-STRING-BRW_B2 FOR EACH konstvaltemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_B2 OPEN QUERY BRW_B2 FOR EACH konstvaltemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_B2 konstvaltemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_B2 konstvaltemp


/* Definitions for BROWSE BRW_KON2                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_KON2 konsttemp.KTYPKOD ~
konsttemp.BENAMNING konsttemp.BILD 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_KON2 
&Scoped-define QUERY-STRING-BRW_KON2 FOR EACH konsttemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_KON2 OPEN QUERY BRW_KON2 FOR EACH konsttemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_KON2 konsttemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_KON2 konsttemp


/* Definitions for BROWSE BRW_MTRL2                                     */
&Scoped-define FIELDS-IN-QUERY-BRW_MTRL2 kon_val.F1 kon_val.ID2 ~
kon_val.EXTRA1 kon_val.UPPLAG kon_val.F2 kon_val.F3 kon_val.F4 kon_val.F5 ~
kon_val.F6 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_MTRL2 
&Scoped-define QUERY-STRING-BRW_MTRL2 FOR EACH kon_val NO-LOCK
&Scoped-define OPEN-QUERY-BRW_MTRL2 OPEN QUERY BRW_MTRL2 FOR EACH kon_val NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_MTRL2 kon_val
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_MTRL2 kon_val


/* Definitions for BROWSE BRW_VAL                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_VAL kon_val2.ORD kon_val2.F1 ~
kon_val2.ID2 kon_val2.EXTRA1 kon_val2.UPPLAG kon_val2.F2 kon_val2.F3 ~
kon_val2.F4 kon_val2.F5 kon_val2.F6 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_VAL 
&Scoped-define QUERY-STRING-BRW_VAL FOR EACH kon_val2 NO-LOCK
&Scoped-define OPEN-QUERY-BRW_VAL OPEN QUERY BRW_VAL FOR EACH kon_val2 NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_VAL kon_val2
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_VAL kon_val2


/* Definitions for FRAME DEFAULT-FRAME                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS CMB_VALKGRP BRW_VAL BRW_MTRL2 btn_over ~
btn_back CMB_VALKGRPTILL CMB_F1 BRW_KON2 BRW_B2 CMB_F2 CMB_F3 CMB_F4 CMB_F5 ~
FBTN_OK BTN_AVB CMB_F6 FILL-IN-B2 FILL-IN-B1 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-BERNR FILL-IN-BEN FILL-IN-AONR ~
FILL-IN-DELNR CMB_VALKGRP CMB_VALKGRPTILL CMB_F1 CMB_F2 CMB_F3 CMB_F4 ~
CMB_F5 CMB_F6 FILL-IN-B2 FILL-IN-B1 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB AUTO-END-KEY 
     LABEL "Avbryt" 
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
     SIZE 14 BY 1.

DEFINE VARIABLE CMB_F1 AS CHARACTER FORMAT "X(256)":U 
     LABEL "F1" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 16 BY .96 NO-UNDO.

DEFINE VARIABLE CMB_F2 AS CHARACTER FORMAT "X(256)":U 
     LABEL "F2" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 16 BY .96 NO-UNDO.

DEFINE VARIABLE CMB_F3 AS CHARACTER FORMAT "X(256)":U 
     LABEL "F3" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 16 BY .96 NO-UNDO.

DEFINE VARIABLE CMB_F4 AS CHARACTER FORMAT "X(256)":U 
     LABEL "F4" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 16 BY .96 NO-UNDO.

DEFINE VARIABLE CMB_F5 AS CHARACTER FORMAT "X(256)":U 
     LABEL "F5" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 16 BY .96 NO-UNDO.

DEFINE VARIABLE CMB_F6 AS CHARACTER FORMAT "X(256)":U 
     LABEL "F6" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 16 BY .96 NO-UNDO.

DEFINE VARIABLE CMB_VALKGRP AS CHARACTER FORMAT "X(30)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "0" 
     DROP-DOWN-LIST
     SIZE 40 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_VALKGRPTILL AS CHARACTER FORMAT "X(30)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "0" 
     DROP-DOWN-LIST
     SIZE 40 BY .96 NO-UNDO.

DEFINE VARIABLE FILL-IN-AONR AS CHARACTER FORMAT "X(256)":U 
     LABEL "Aonr" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-B1 AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 36 BY .71
     BGCOLOR 7  NO-UNDO.

DEFINE VARIABLE FILL-IN-B2 AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 36 BY .71
     BGCOLOR 7  NO-UNDO.

DEFINE VARIABLE FILL-IN-BEN AS CHARACTER FORMAT "X(256)":U 
     LABEL "Benämning" 
     VIEW-AS FILL-IN 
     SIZE 42.5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-BERNR AS INTEGER FORMAT ">>>>>>9":U INITIAL 0 
     LABEL "Beredning nr" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-DELNR AS INTEGER FORMAT ">99":U INITIAL 0 
     LABEL "Delnr" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_B2 FOR 
      konstvaltemp SCROLLING.

DEFINE QUERY BRW_KON2 FOR 
      konsttemp SCROLLING.

DEFINE QUERY BRW_MTRL2 FOR 
      kon_val SCROLLING.

DEFINE QUERY BRW_VAL FOR 
      kon_val2 SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_B2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_B2 C-Win _STRUCTURED
  QUERY BRW_B2 NO-LOCK DISPLAY
      konstvaltemp.KVALKOD COLUMN-LABEL "Kod" FORMAT "X(12)":U
            WIDTH 10
      konstvaltemp.BENAMNING FORMAT "X(256)":U WIDTH 21
      konstvaltemp.BILD COLUMN-LABEL "B" FORMAT "x(1)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING MULTIPLE SIZE 41.5 BY 6.25.

DEFINE BROWSE BRW_KON2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_KON2 C-Win _STRUCTURED
  QUERY BRW_KON2 NO-LOCK DISPLAY
      konsttemp.KTYPKOD COLUMN-LABEL "Kod" FORMAT "X(12)":U WIDTH 10
      konsttemp.BENAMNING FORMAT "X(256)":U WIDTH 21
      konsttemp.BILD COLUMN-LABEL "B" FORMAT "x(1)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SIZE 41.5 BY 6.5.

DEFINE BROWSE BRW_MTRL2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_MTRL2 C-Win _STRUCTURED
  QUERY BRW_MTRL2 NO-LOCK DISPLAY
      kon_val.F1 COLUMN-LABEL "F1" FORMAT "X(256)":U WIDTH 7
      kon_val.ID2 FORMAT "X(256)":U WIDTH 3
      kon_val.EXTRA1 COLUMN-LABEL "Fri id" FORMAT "X(256)":U WIDTH 5
      kon_val.UPPLAG COLUMN-LABEL "Up" FORMAT ">>":U
      kon_val.F2 FORMAT "X(256)":U WIDTH 7
      kon_val.F3 FORMAT "X(256)":U WIDTH 7
      kon_val.F4 FORMAT "X(256)":U WIDTH 7
      kon_val.F5 FORMAT "X(256)":U WIDTH 7
      kon_val.F6 FORMAT "X(256)":U WIDTH 7
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING MULTIPLE SIZE 55.88 BY 15
         TITLE "Valda byggnummer".

DEFINE BROWSE BRW_VAL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_VAL C-Win _STRUCTURED
  QUERY BRW_VAL NO-LOCK DISPLAY
      kon_val2.ORD COLUMN-LABEL "Skr!ord" FORMAT ">>9":U
      kon_val2.F1 COLUMN-LABEL "F1" FORMAT "X(256)":U WIDTH 7
      kon_val2.ID2 FORMAT "X(256)":U WIDTH 3
      kon_val2.EXTRA1 COLUMN-LABEL "Fri id" FORMAT "X(256)":U WIDTH 5
      kon_val2.UPPLAG COLUMN-LABEL "Up" FORMAT ">>":U
      kon_val2.F2 FORMAT "X(256)":U WIDTH 7
      kon_val2.F3 FORMAT "X(256)":U WIDTH 7
      kon_val2.F4 FORMAT "X(256)":U WIDTH 7
      kon_val2.F5 FORMAT "X(256)":U WIDTH 7
      kon_val2.F6 FORMAT "X(256)":U WIDTH 7
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING MULTIPLE SIZE 58 BY 15
         TITLE "Byggnummer".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     FILL-IN-BERNR AT ROW 1.5 COL 16 COLON-ALIGNED
     FILL-IN-BEN AT ROW 1.5 COL 47 COLON-ALIGNED
     FILL-IN-AONR AT ROW 2.67 COL 16 COLON-ALIGNED
     FILL-IN-DELNR AT ROW 2.67 COL 38 COLON-ALIGNED
     CMB_VALKGRP AT ROW 3.92 COL 1 NO-LABEL WIDGET-ID 2
     BRW_VAL AT ROW 5.21 COL 1.5
     BRW_MTRL2 AT ROW 5.21 COL 64
     btn_over AT ROW 11.46 COL 59.75
     btn_back AT ROW 14.96 COL 59.75
     CMB_VALKGRPTILL AT ROW 20.38 COL 42 NO-LABEL WIDGET-ID 30
     CMB_F1 AT ROW 22 COL 10 WIDGET-ID 4
     BRW_KON2 AT ROW 22.5 COL 43 WIDGET-ID 300
     BRW_B2 AT ROW 22.5 COL 43 WIDGET-ID 200
     CMB_F2 AT ROW 23.25 COL 10 WIDGET-ID 6
     CMB_F3 AT ROW 24.5 COL 10 WIDGET-ID 8
     CMB_F4 AT ROW 25.75 COL 10 WIDGET-ID 10
     CMB_F5 AT ROW 27 COL 10 WIDGET-ID 12
     FBTN_OK AT ROW 27.96 COL 90.75
     BTN_AVB AT ROW 27.96 COL 105.88
     CMB_F6 AT ROW 28.25 COL 10 WIDGET-ID 14
     FILL-IN-B2 AT ROW 21.54 COL 43 NO-LABEL WIDGET-ID 34
     FILL-IN-B1 AT ROW 21.63 COL 43 NO-LABEL WIDGET-ID 32
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 119.75 BY 28.42.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Temp-Tables and Buffers:
      TABLE: konsttemp T "?" NO-UNDO temp-db konsttemp
      TABLE: konstvaltemp T "?" NO-UNDO temp-db konstvaltemp
      TABLE: kon_val T "?" NO-UNDO temp-db kon_val
      TABLE: kon_val2 T "?" NO-UNDO temp-db kon_val2
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Val av byggnummer"
         HEIGHT             = 28.58
         WIDTH              = 120
         MAX-HEIGHT         = 29.71
         MAX-WIDTH          = 124
         VIRTUAL-HEIGHT     = 29.71
         VIRTUAL-WIDTH      = 124
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
   FRAME-NAME                                                           */
/* BROWSE-TAB BRW_VAL CMB_VALKGRP DEFAULT-FRAME */
/* BROWSE-TAB BRW_MTRL2 BRW_VAL DEFAULT-FRAME */
/* BROWSE-TAB BRW_KON2 CMB_F1 DEFAULT-FRAME */
/* BROWSE-TAB BRW_B2 BRW_KON2 DEFAULT-FRAME */
ASSIGN 
       BRW_B2:HIDDEN  IN FRAME DEFAULT-FRAME                = TRUE
       BRW_B2:COLUMN-RESIZABLE IN FRAME DEFAULT-FRAME       = TRUE.

ASSIGN 
       BRW_KON2:HIDDEN  IN FRAME DEFAULT-FRAME                = TRUE
       BRW_KON2:COLUMN-RESIZABLE IN FRAME DEFAULT-FRAME       = TRUE.

ASSIGN 
       BRW_MTRL2:HIDDEN  IN FRAME DEFAULT-FRAME                = TRUE
       BRW_MTRL2:MAX-DATA-GUESS IN FRAME DEFAULT-FRAME         = 10000
       BRW_MTRL2:ALLOW-COLUMN-SEARCHING IN FRAME DEFAULT-FRAME = TRUE.

ASSIGN 
       BRW_VAL:MAX-DATA-GUESS IN FRAME DEFAULT-FRAME         = 300
       BRW_VAL:ALLOW-COLUMN-SEARCHING IN FRAME DEFAULT-FRAME = TRUE.

/* SETTINGS FOR COMBO-BOX CMB_F1 IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
ASSIGN 
       CMB_F1:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR COMBO-BOX CMB_F2 IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
ASSIGN 
       CMB_F2:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR COMBO-BOX CMB_F3 IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
ASSIGN 
       CMB_F3:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR COMBO-BOX CMB_F4 IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
ASSIGN 
       CMB_F4:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR COMBO-BOX CMB_F5 IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
ASSIGN 
       CMB_F5:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR COMBO-BOX CMB_F6 IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
ASSIGN 
       CMB_F6:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR COMBO-BOX CMB_VALKGRP IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
/* SETTINGS FOR COMBO-BOX CMB_VALKGRPTILL IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
ASSIGN 
       CMB_VALKGRPTILL:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-AONR IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       FILL-IN-AONR:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-B1 IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
ASSIGN 
       FILL-IN-B1:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-B2 IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
ASSIGN 
       FILL-IN-B2:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-BEN IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       FILL-IN-BEN:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-BERNR IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       FILL-IN-BERNR:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-DELNR IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       FILL-IN-DELNR:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_B2
/* Query rebuild information for BROWSE BRW_B2
     _TblList          = "Temp-Tables.konstvaltemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.konstvaltemp.KVALKOD
"konstvaltemp.KVALKOD" "Kod" "X(12)" "character" ? ? ? ? ? ? no ? no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.konstvaltemp.BENAMNING
"konstvaltemp.BENAMNING" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "21" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.konstvaltemp.BILD
"konstvaltemp.BILD" "B" "x(1)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_B2 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_KON2
/* Query rebuild information for BROWSE BRW_KON2
     _TblList          = "Temp-Tables.konsttemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.konsttemp.KTYPKOD
"konsttemp.KTYPKOD" "Kod" "X(12)" "character" ? ? ? ? ? ? no ? no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.konsttemp.BENAMNING
"konsttemp.BENAMNING" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "21" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.konsttemp.BILD
"konsttemp.BILD" "B" "x(1)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_KON2 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_MTRL2
/* Query rebuild information for BROWSE BRW_MTRL2
     _TblList          = "Temp-Tables.kon_val"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.kon_val.F1
"kon_val.F1" "F1" "X(256)" "character" ? ? ? ? ? ? no ? no no "7" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.kon_val.ID2
"kon_val.ID2" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "3" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.kon_val.EXTRA1
"kon_val.EXTRA1" "Fri id" "X(256)" "character" ? ? ? ? ? ? no ? no no "5" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.kon_val.UPPLAG
"kon_val.UPPLAG" "Up" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.kon_val.F2
"kon_val.F2" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "7" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > Temp-Tables.kon_val.F3
"kon_val.F3" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "7" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > Temp-Tables.kon_val.F4
"kon_val.F4" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "7" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > Temp-Tables.kon_val.F5
"kon_val.F5" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "7" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > Temp-Tables.kon_val.F6
"kon_val.F6" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "7" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_MTRL2 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_VAL
/* Query rebuild information for BROWSE BRW_VAL
     _TblList          = "Temp-Tables.kon_val2"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.kon_val2.ORD
"kon_val2.ORD" "Skr!ord" ">>9" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.kon_val2.F1
"kon_val2.F1" "F1" "X(256)" "character" ? ? ? ? ? ? no ? no no "7" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.kon_val2.ID2
"kon_val2.ID2" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "3" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.kon_val2.EXTRA1
"kon_val2.EXTRA1" "Fri id" "X(256)" "character" ? ? ? ? ? ? no ? no no "5" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.kon_val2.UPPLAG
"kon_val2.UPPLAG" "Up" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > Temp-Tables.kon_val2.F2
"kon_val2.F2" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "7" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > Temp-Tables.kon_val2.F3
"kon_val2.F3" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "7" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > Temp-Tables.kon_val2.F4
"kon_val2.F4" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "7" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > Temp-Tables.kon_val2.F5
"kon_val2.F5" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "7" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > Temp-Tables.kon_val2.F6
"kon_val2.F6" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "7" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_VAL */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Val av byggnummer */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Val av byggnummer */
DO:
  /* This event will close the window and terminate the procedure.  */
  musz = TRUE.
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_B2
&Scoped-define SELF-NAME BRW_B2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_B2 C-Win
ON MOUSE-MENU-CLICK OF BRW_B2 IN FRAME DEFAULT-FRAME
DO:  
/*   IF finns2 = TRUE THEN DO:
      ASSIGN
      status-ok = BRW_B2:DESELECT-ROWS() IN FRAME {&FRAME-NAME}
      mark = FALSE. 
   END.*/   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_B2 C-Win
ON VALUE-CHANGED OF BRW_B2 IN FRAME DEFAULT-FRAME
DO: 
/*   ASSIGN
   brw = 2.    
   IF AVAILABLE konstvaltemp THEN hjkvalkod = konstvaltemp.KVALKOD.   
   RUN markerade_UI.  
   IF finns2 = TRUE THEN ASSIGN mark = TRUE.*/          
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_KON2
&Scoped-define SELF-NAME BRW_KON2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_KON2 C-Win
ON VALUE-CHANGED OF BRW_KON2 IN FRAME DEFAULT-FRAME
DO: 
/*   {muswait.i}
   ASSIGN
   status-ok = BRW_KON2:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
   ASSIGN
   konstvalvar = ""
   hjkvalkod = "".
   IF AVAILABLE konsttemp THEN konstvalvar = konsttemp.KTYPKOD.   
   mark = FALSE.   
   RUN bytkonst_UI.      
   {musarrow.i}*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_MTRL2
&Scoped-define SELF-NAME BRW_MTRL2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_MTRL2 C-Win
ON VALUE-CHANGED OF BRW_MTRL2 IN FRAME DEFAULT-FRAME /* Valda byggnummer */
DO:
   {muswait.i}
   status-ok = BRW_MTRL2:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.   
   RUN vcbrwval_UI.
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_VAL
&Scoped-define SELF-NAME BRW_VAL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_VAL C-Win
ON MOUSE-SELECT-DBLCLICK OF BRW_VAL IN FRAME DEFAULT-FRAME /* Byggnummer */
DO:
    APPLY "CHOOSE" TO BTN_OVER.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_VAL C-Win
ON VALUE-CHANGED OF BRW_VAL IN FRAME DEFAULT-FRAME /* Byggnummer */
DO:
   {muswait.i}
   status-ok = BRW_VAL:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.   
   RUN vcbrwval2_UI.
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB C-Win
ON CHOOSE OF BTN_AVB IN FRAME DEFAULT-FRAME /* Avbryt */
DO:    
   musz = TRUE.
   RETURN.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_back
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_back C-Win
ON CHOOSE OF btn_back IN FRAME DEFAULT-FRAME
DO:   
   antal_valda = BRW_MTRL2:NUM-SELECTED-ROWS.
   antal_raknare = 1.
   DO WHILE antal_raknare LE antal_valda:                                   
      status-ok = BRW_MTRL2:FETCH-SELECTED-ROW(antal_raknare).                      
      DELETE kon_val.   
      IF antal_raknare = antal_valda THEN RUN selnextprevrow_UI IN brwproc[{&RIGHT-BROWSE}].
      antal_raknare = antal_raknare + 1.   
   END.
   RUN openbdynspec_UI IN brwproc[{&RIGHT-BROWSE}].   
   RUN lastselectdyn_UI IN brwproc[{&RIGHT-BROWSE}].   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_over
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_over C-Win
ON CHOOSE OF btn_over IN FRAME DEFAULT-FRAME
DO:      
   antal_valda = BRW_VAL:NUM-SELECTED-ROWS.         
   antal_raknare = 1.
   IF antal_valda > 0 THEN DO:
      
      DO WHILE antal_raknare LE antal_valda:                                   
         status-ok = BRW_VAL:FETCH-SELECTED-ROW(antal_raknare).
         
         ordning = ordning + 1.
         CREATE kon_val. 
         ASSIGN
         kon_val.BERAONR = kon_val2.BERAONR
         kon_val.OMRADE = kon_val2.OMRADE
         kon_val.F1 = kon_val2.F1
         kon_val.F2 = kon_val2.F2             
         kon_val.F3 = kon_val2.F3 
         kon_val.F4 = kon_val2.F4 
         kon_val.F5 = kon_val2.F5 
         kon_val.F6 = kon_val2.F6             
         kon_val.ID2 = kon_val2.ID2
         kon_val.EXTRA = kon_val2.EXTRA
         kon_val.NUM = kon_val2.NUM
         kon_val.KSKAP = kon_val2.KSKAP
         kon_val.GRUPP = kon_val2.GRUPP
         kon_val.ORD = kon_val2.ORD       
         kon_val.UPPLAG = kon_val2.UPPLAG.
         
         spec_rowid = ROWID(kon_val).
         RUN extrakon_UI.                        
   
         IF antal_raknare = antal_valda THEN RUN selnextprevrow_UI IN brwproc[{&LEFT-BROWSE}].                   
         antal_raknare = antal_raknare + 1.
      END.
      RUN openbdynspec_UI IN brwproc[{&RIGHT-BROWSE}].                  
      
      FIND FIRST kon_val WHERE ROWID(kon_val) = spec_rowid NO-LOCK NO-ERROR.
      IF AVAILABLE kon_val THEN DO:
         RUN setlastrowid_UI IN brwproc[{&RIGHT-BROWSE}] (INPUT ROWID(kon_val)).
         RUN lastselectdyn_UI IN brwproc[{&RIGHT-BROWSE}].
      END.
      RUN vcbrwval_UI.
      RUN openbdynspec_UI IN brwproc[{&LEFT-BROWSE}].           
      RUN lastselectdyn_UI IN brwproc[{&LEFT-BROWSE}].
      
      EMPTY TEMP-TABLE f1temp NO-ERROR.
      EMPTY TEMP-TABLE f2temp NO-ERROR.
      EMPTY TEMP-TABLE f3temp NO-ERROR.
      EMPTY TEMP-TABLE f4temp NO-ERROR.
      EMPTY TEMP-TABLE f5temp NO-ERROR.
      EMPTY TEMP-TABLE f6temp NO-ERROR.  
      CMB_F1:LIST-ITEMS = "".
      CMB_F1:LIST-ITEMS = "".
      CMB_F2:LIST-ITEMS = "".
      CMB_F3:LIST-ITEMS = "".
      CMB_F4:LIST-ITEMS = "".
      CMB_F5:LIST-ITEMS = "".
      CMB_F6:LIST-ITEMS = "". 
      status-ok = CMB_F1:ADD-LAST("Välj").
      status-ok = CMB_F2:ADD-LAST("Välj").
      status-ok = CMB_F3:ADD-LAST("Välj").
      status-ok = CMB_F4:ADD-LAST("Välj").
      status-ok = CMB_F5:ADD-LAST("Välj").
      status-ok = CMB_F6:ADD-LAST("Välj").
      ASSIGN
      CMB_F1:SCREEN-VALUE = "Välj"
      CMB_F2:SCREEN-VALUE = "Välj"
      CMB_F3:SCREEN-VALUE = "Välj"
      CMB_F4:SCREEN-VALUE = "Välj"
      CMB_F5:SCREEN-VALUE = "Välj"
      CMB_F6:SCREEN-VALUE = "Välj".
      FOR EACH kon_val NO-LOCK:
         IF kon_val.F1 NE "" THEN DO:
            FIND FIRST f1temp WHERE f1temp.F1 = kon_val.F1 NO-ERROR.
            IF NOT AVAILABLE f1temp THEN DO:
               CREATE f1temp.
               assign f1temp.F1 = kon_val.F1.
               status-ok = CMB_F1:ADD-LAST(kon_val.F1).               
            END.
         END.   
         IF kon_val.F2 NE "" THEN DO:
            FIND FIRST f2temp WHERE f2temp.F2 = kon_val.F2 NO-ERROR.
            IF NOT AVAILABLE f2temp THEN DO:
               CREATE f2temp.
               assign f2temp.F2 = kon_val.F2.
               status-ok = CMB_F2:ADD-LAST(kon_val.F2).               
            END.
         END.
         IF kon_val.F3 NE "" THEN DO:   
            FIND FIRST f3temp WHERE f3temp.F3 = kon_val.F3 NO-ERROR.
            IF NOT AVAILABLE f3temp THEN DO:
               CREATE f3temp.
               assign f3temp.F3 = kon_val.F3.
               status-ok = CMB_F3:ADD-LAST(kon_val.F3).               
            END.
         END.
         IF kon_val.F4 NE "" THEN DO:   
            FIND FIRST f4temp WHERE f4temp.F4 = kon_val.F4 NO-ERROR.
            IF NOT AVAILABLE f4temp THEN DO:
               CREATE f4temp.
               assign f4temp.F4 = kon_val.F4.
               status-ok = CMB_F4:ADD-LAST(kon_val.F4).               
            END.
         END.
         IF kon_val.F5 NE "" THEN DO:   
            FIND FIRST f5temp WHERE f5temp.F5 = kon_val.F5 NO-ERROR.
            IF NOT AVAILABLE f5temp THEN DO:
               CREATE f5temp.
               assign f5temp.F5 = kon_val.F5.          
               status-ok = CMB_F5:ADD-LAST(kon_val.F5).                    
            END.
         END.
         IF kon_val.F6 NE "" THEN DO:   
            FIND FIRST f6temp WHERE f6temp.F6 = kon_val.F6 NO-ERROR.
            IF NOT AVAILABLE f6temp THEN DO:
               CREATE f6temp.
               assign f6temp.F6 = kon_val.F6.
               status-ok = CMB_F6:ADD-LAST(kon_val.F6).
            END.
         END.  
         /*IF AVAILABLE f1temp  THEN CMB_F1:SCREEN-VALUE = f1temp.F1.
         IF AVAILABLE f2temp  THEN CMB_F2:SCREEN-VALUE = f2temp.F2.
         IF AVAILABLE f3temp  THEN CMB_F3:SCREEN-VALUE = f3temp.F3.
         IF AVAILABLE f4temp  THEN CMB_F4:SCREEN-VALUE = f4temp.F4.
         IF AVAILABLE f5temp  THEN CMB_F5:SCREEN-VALUE = f5temp.F5.
         IF AVAILABLE f6temp  THEN CMB_F6:SCREEN-VALUE = f6temp.F6.*/       
      END.
   END.                     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_F1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_F1 C-Win
ON VALUE-CHANGED OF CMB_F1 IN FRAME DEFAULT-FRAME /* F1 */
DO:  
   CMB_VALKGRPTILL = INPUT CMB_VALKGRPTILL.
   CMB_F1 = INPUT CMB_F1.
   IF CMB_F1 NE "Välj" THEN DO:     
      BRW_KON2:HIDDEN = FALSE.
      BRW_B2:HIDDEN = TRUE.
   END.
   IF AVAILABLE konstgrptemp THEN DO:
      RUN setcolsortvar_UI IN brwproc[3] (INPUT " WHERE KONSKOD = '" + STRING(konstgrptemp.KONSKOD) + "' USE-INDEX ORD").  
      RUN openbdynspec_UI IN brwproc[3].
   END.     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_F2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_F2 C-Win
ON VALUE-CHANGED OF CMB_F2 IN FRAME DEFAULT-FRAME /* F2 */
DO:
   CMB_F2 = INPUT CMB_F2.
   IF CMB_F2 NE "Välj" THEN DO:     
      BRW_KON2:HIDDEN = TRUE.
      BRW_B2:HIDDEN = FALSE.
   END.

   FIND FIRST kon_val NO-ERROR.
   FIND FIRST bbenamntemp WHERE bbenamntemp.KONSKOD = kon_val.GRUPP  USE-INDEX KOD NO-LOCK NO-ERROR.      
   IF AVAILABLE bbenamntemp THEN DO:          
      EMPTY TEMP-TABLE konstvaltemp NO-ERROR. 
      RUN radkonto_UI IN konmtrlapph (INPUT bbenamntemp.B2,INPUT konstgrptemp.KONSKOD,INPUT kon_val.F1,
                                      OUTPUT TABLE konstvaltemp).
        
      RUN setcolsortvar_UI IN brwproc[4] (INPUT "USE-INDEX ORD").  
      RUN openbdynspec_UI IN brwproc[4].
   END.          

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_F3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_F3 C-Win
ON VALUE-CHANGED OF CMB_F3 IN FRAME DEFAULT-FRAME /* F3 */
DO:
  IF CMB_F2 NE "Välj" THEN DO:     
     BRW_KON2:HIDDEN = TRUE.
     BRW_B2:HIDDEN = FALSE.
  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_F4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_F4 C-Win
ON VALUE-CHANGED OF CMB_F4 IN FRAME DEFAULT-FRAME /* F4 */
DO:
  /**/

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_F5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_F5 C-Win
ON VALUE-CHANGED OF CMB_F5 IN FRAME DEFAULT-FRAME /* F5 */
DO:
  /**/

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_F6
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_F6 C-Win
ON VALUE-CHANGED OF CMB_F6 IN FRAME DEFAULT-FRAME /* F6 */
DO:
  /**/

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_VALKGRP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_VALKGRP C-Win
ON VALUE-CHANGED OF CMB_VALKGRP IN FRAME DEFAULT-FRAME
DO:
   CMB_VALKGRP = INPUT CMB_VALKGRP.
   CMB_VALKGRPTILL = CMB_VALKGRP.
   CMB_VALKGRPTILL:SCREEN-VALUE = CMB_VALKGRP:SCREEN-VALUE.
   FIND FIRST konstgrptemp WHERE konstgrptemp.BENAMNING = CMB_VALKGRP:SCREEN-VALUE  NO-ERROR.
   IF AVAILABLE  konstgrptemp THEN DO:  
      RUN setcolsortvar_UI IN brwproc[{&LEFT-BROWSE}]  (INPUT " WHERE GRUPP = '" + STRING(konstgrptemp.KONSKOD) + "'").
      RUN setcolsortvar_UI IN brwproc[{&RIGHT-BROWSE}]  (INPUT " WHERE GRUPP = '" + STRING(konstgrptemp.KONSKOD)  + "'").       
      RUN openbdynspec_UI IN brwproc[{&RIGHT-BROWSE}].
      RUN openbdynspec_UI IN brwproc[{&LEFT-BROWSE}].
   END. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_VALKGRPTILL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_VALKGRPTILL C-Win
ON VALUE-CHANGED OF CMB_VALKGRPTILL IN FRAME DEFAULT-FRAME
DO:
   CMB_VALKGRPTILL = INPUT CMB_VALKGRPTILL.
   FIND FIRST konstgrptemp WHERE konstgrptemp.BENAMNING = CMB_VALKGRPTILL:SCREEN-VALUE  NO-ERROR.
   IF AVAILABLE  konstgrptemp THEN DO:  
      
      RUN setcolsortvar_UI IN brwproc[3] (INPUT " WHERE KONSKOD = '" + STRING(konstgrptemp.KONSKOD) + "' USE-INDEX ORD").  
      RUN openbdynspec_UI IN brwproc[3].

   END. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_OK C-Win
ON CHOOSE OF FBTN_OK IN FRAME DEFAULT-FRAME /* Ok */
DO:
   {muswait.i} 
   ordning = 0.
   GET FIRST BRW_MTRL2.
   DO WHILE AVAILABLE(kon_val):
      ordning = ordning + 1.
      kon_val.ORD = ordning.
      GET NEXT BRW_MTRL2.
   END.
   
            
   APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_B2
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
   IF VALID-HANDLE(valkonstapph) THEN DELETE PROCEDURE valkonstapph.
   IF VALID-HANDLE(laddaproch) THEN DELETE PROCEDURE laddaproch.
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
    IF Guru.Konstanter:globforetag = "UMEA" THEN DO:
      ASSIGN
      BRW_MTRL2:TITLE = "Valda byggnummer" 
      BRW_VAL:TITLE = "Byggnummer"
      C-Win:TITLE   = "Val av byggnummer".
   END.
   ELSE IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" THEN DO:
      ASSIGN
      BRW_MTRL2:TITLE = "Valda objekt" 
      BRW_VAL:TITLE = "Objekt"
      C-Win:TITLE   = "Val av objekt".
   END.
   ELSE DO:
      ASSIGN
      BRW_MTRL2:TITLE = "Valda konstruktioner" 
      BRW_VAL:TITLE = "Konstruktioner"
      C-Win:TITLE   = "Val av konstruktioner".
   END.
   {muswait.i}
   {ALLSTARTDYN.I}
   &Scoped-define FORMATNAMN FILL-IN-DELNR   
   {DELNRFORMAT.I}
   EMPTY TEMP-TABLE beredningtemp NO-ERROR. 
   EMPTY TEMP-TABLE bermtrltemp NO-ERROR. 
   RUN ladda_UI IN valkonstapph (INPUT valaonr,INPUT valomrade,OUTPUT kalkrow,
                                 OUTPUT TABLE beredningtemp,OUTPUT TABLE bermtrltemp).
   
   FIND FIRST beredningtemp WHERE beredningtemp.BERNR = INTEGER(valaonr) AND
   beredningtemp.OMRADE = valomrade USE-INDEX BERNR NO-LOCK NO-ERROR.
   ASSIGN
   FILL-IN-BERNR = beredningtemp.BERNR
   FILL-IN-AONR = beredningtemp.AONR
   FILL-IN-DELNR = beredningtemp.DELNR
   FILL-IN-BEN = beredningtemp.BENAMNING
   kalkrow2 = kalkrow
   valomrade2 = beredningtemp.OMRADE
   ordning = 0
   kombnr = 0.
   EMPTY TEMP-TABLE kon_val2 NO-ERROR.    
   FIND FIRST bermtrltemp WHERE bermtrltemp.AONR = valaonr AND 
   bermtrltemp.OMRADE = valomrade AND bermtrltemp.INKOP = FALSE 
   USE-INDEX DATUM NO-LOCK NO-ERROR.
   IF AVAILABLE bermtrltemp THEN DO:
      datvar99 = bermtrltemp.DATUM.
   END.
   ELSE DO:
      datvar99 = TODAY.
   END. 
   EMPTY TEMP-TABLE kon_val2 NO-ERROR. 
   EMPTY TEMP-TABLE list_mtrl2 NO-ERROR.    
   IF Guru.Konstanter:appcon THEN DO:                           
      RUN VALBER2U.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT valaonr, INPUT valomrade, INPUT datvar99, OUTPUT TABLE kon_val2,
      OUTPUT TABLE list_mtrl2).
   END.
   ELSE DO:
      RUN VALBER2U.P
      (INPUT valaonr, INPUT valomrade, INPUT datvar99, OUTPUT TABLE kon_val2,
      OUTPUT TABLE list_mtrl2).
   END.
   CMB_VALKGRP:LIST-ITEMS = "".
   CMB_VALKGRPTILL:LIST-ITEMS = "".
   FOR EACH konstgrptemp NO-LOCK:
      FIND FIRST kon_val2 WHERE kon_val2.GRUPP = konstgrptemp.KONSKOD   NO-LOCK NO-ERROR.
      IF AVAILABLE kon_val2 THEN DO:
         ASSIGN
         status-ok = CMB_VALKGRP:ADD-LAST(konstgrptemp.BENAMNING).                    
      END.
      status-ok = CMB_VALKGRPTILL:ADD-LAST(konstgrptemp.BENAMNING). 
   END.
   FIND FIRST kon_val2 NO-ERROR.
   FIND FIRST konstgrptemp WHERE konstgrptemp.KONSKOD = kon_val2.GRUPP  NO-ERROR.
      
   RUN setcolsortvar_UI IN brwproc[{&LEFT-BROWSE}]  (INPUT " WHERE GRUPP = '" + STRING(konstgrptemp.KONSKOD) + "'").
   RUN setcolsortvar_UI IN brwproc[{&RIGHT-BROWSE}]  (INPUT " WHERE GRUPP = '" + STRING(konstgrptemp.KONSKOD)  + "'").       
   RUN openbdynspec_UI IN brwproc[{&RIGHT-BROWSE}].
   RUN openbdynspec_UI IN brwproc[{&LEFT-BROWSE}].
   CMB_VALKGRP:SCREEN-VALUE = konstgrptemp.BENAMNING.
   CMB_VALKGRPTILL:SCREEN-VALUE = konstgrptemp.BENAMNING.
     
     
   
   EMPTY TEMP-TABLE kon_val  NO-ERROR. 
   FIND LAST kon_val USE-INDEX ORD NO-LOCK NO-ERROR.
   IF AVAILABLE kon_val THEN ordning = kon_val.ORD.
   FIND LAST kon_val USE-INDEX NUM2 NO-LOCK NO-ERROR.
   IF AVAILABLE kon_val THEN DO:
      IF kon_val.SKAPNUM NE ? THEN
      kombnr = kon_val.SKAPNUM.
   END.
   IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" THEN DO:
      kon_val2.ID2:LABEL IN BROWSE BRW_VAL = "Objektnr".
   END.
   ELSE IF Guru.Konstanter:globforetag = "UMEA" THEN DO:
      kon_val2.ID2:LABEL IN BROWSE BRW_VAL = "Byggnr".
   END.
   ELSE DO:
      kon_val2.ID2:LABEL IN BROWSE BRW_VAL = "Id".
   END.
   IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" THEN DO:
      kon_val.ID2:LABEL IN BROWSE BRW_MTRL2 = "Objektnr".
   END.
   ELSE IF Guru.Konstanter:globforetag = "UMEA" THEN DO:
      kon_val.ID2:LABEL IN BROWSE BRW_MTRL2 = "Byggnr".
   END.
   ELSE DO:
      kon_val.ID2:LABEL IN BROWSE BRW_MTRL2 = "Id".
   END.      
   RUN enable_UI.   
   {FRMSIZE.I}  
   APPLY "VALUE-CHANGED" TO BRW_VAL IN FRAME {&FRAME-NAME}.
   APPLY "VALUE-CHANGED" TO BRW_MTRL2 IN FRAME {&FRAME-NAME}.
   /*
    RUN setcolindex_UI IN brwproc[{&LEFT-BROWSE}] (INPUT "").
    */
    RUN resetcolindex_UI IN brwproc[{&LEFT-BROWSE}].
    
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
  Purpose:    
  Parameters:  
  Notes:       
-------------------------------------------------------------*/    
   RUN DYNBRW.P PERSISTENT SET brwproc[{&LEFT-BROWSE}] 
      (INPUT BRW_VAL:HANDLE IN FRAME {&FRAME-NAME}).
   RUN DYNBRW.P PERSISTENT SET brwproc[{&RIGHT-BROWSE}] 
      (INPUT BRW_MTRL2:HANDLE IN FRAME {&FRAME-NAME}).
   RUN DYNBRW.P PERSISTENT SET brwproc[3] 
      (INPUT BRW_KON2:HANDLE IN FRAME {&FRAME-NAME}).
   RUN DYNBRW.P PERSISTENT SET brwproc[4] 
      (INPUT BRW_B2:HANDLE IN FRAME {&FRAME-NAME}).
   /*
   RUN brwsetupstop_UI IN brwproc[{&RIGHT-BROWSE}] (INPUT 1).
   */
   /*RUN setcolsortvar_UI IN brwproc[{&LEFT-BROWSE}] (INPUT "KSKAP = FALSE").*/
   tthandle = TEMP-TABLE bbenamntemp:HANDLE.
   IF Guru.Konstanter:appcon THEN DO:
      RUN DYNLADDATEMP.P PERSISTENT SET laddaproch ON Guru.Konstanter:apphand TRANSACTION DISTINCT
         (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "BBENAMNING", INPUT "").
   END.
   ELSE DO:
      RUN DYNLADDATEMP.P PERSISTENT SET laddaproch
         (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "BBENAMNING", INPUT "").
   END.
   tthandle = TEMP-TABLE konstgrptemp:HANDLE. 
   RUN laddatemp_UI IN laddaproch (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "KONSTGRUPP", INPUT "").
   tthandle = TEMP-TABLE konsttemp:HANDLE.
   RUN laddatemp_UI IN laddaproch (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "KONSTRUKTION", INPUT "").
   
   /*RUN setcolsortvar_UI IN brwproc[{&LEFT-BROWSE}]  (INPUT " WHERE GRUPP = '" + STRING(konstgrptemp.KONSKOD)).
   RUN setcolsortvar_UI IN brwproc[{&RIGHT-BROWSE}]  (INPUT " WHERE GRUPP = '" + STRING(konstgrptemp.KONSKOD)).*/   
   
   RUN setcolindex_UI IN brwproc[{&LEFT-BROWSE}] (INPUT "ORD").
   
   RUN setcolindex_UI IN brwproc[{&RIGHT-BROWSE}] (INPUT "ORD").
      
   IF Guru.Konstanter:appcon THEN DO:
      RUN VALKONSTAPP.P PERSISTENT SET valkonstapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN VALKONSTAPP.P PERSISTENT SET valkonstapph.
   END. 
   IF Guru.Konstanter:appcon THEN DO:
      RUN KONMTRLAPP.P PERSISTENT SET konmtrlapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN KONMTRLAPP.P PERSISTENT SET konmtrlapph.
   END.

   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnordnerupp_UI C-Win 
PROCEDURE btnordnerupp_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER upp AS LOGICAL NO-UNDO.
   DEFINE VARIABLE frannr AS INTEGER NO-UNDO.
   DEFINE VARIABLE tillnr AS INTEGER NO-UNDO.
   DEFINE VARIABLE till_rowid AS ROWID NO-UNDO.
   status-ok = BRW_MTRL2:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
   IF AVAILABLE kon_val THEN DO:
      ASSIGN
      frannr = kon_val.ORD
      till_rowid = ROWID(kon_val).      
      IF upp = FALSE THEN status-ok = BRW_MTRL2:SELECT-NEXT-ROW() IN FRAME {&FRAME-NAME}.  
      ELSE status-ok = BRW_MTRL2:SELECT-PREV-ROW() IN FRAME {&FRAME-NAME}.  
      IF status-ok = TRUE THEN DO:
         ASSIGN
         tillnr = kon_val.ORD
         kon_val.ORD = frannr.   
         FIND kon_val WHERE ROWID(kon_val) = till_rowid.
         kon_val.ORD = tillnr.
         RUN openbdynspec_UI IN brwproc[{&RIGHT-BROWSE}].
         FIND kon_val WHERE ROWID(kon_val) = till_rowid NO-LOCK NO-ERROR.
         IF AVAILABLE kon_val THEN DO:
            RUN setlastrowid_UI IN brwproc[{&RIGHT-BROWSE}] (INPUT ROWID(kon_val)).
            RUN lastselectdyn_UI IN brwproc[{&RIGHT-BROWSE}].
         END.
      END.   
   END.
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
  DISPLAY FILL-IN-BERNR FILL-IN-BEN FILL-IN-AONR FILL-IN-DELNR CMB_VALKGRP 
          CMB_VALKGRPTILL CMB_F1 CMB_F2 CMB_F3 CMB_F4 CMB_F5 CMB_F6 FILL-IN-B2 
          FILL-IN-B1 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE CMB_VALKGRP BRW_VAL BRW_MTRL2 btn_over btn_back CMB_VALKGRPTILL CMB_F1 
         BRW_KON2 BRW_B2 CMB_F2 CMB_F3 CMB_F4 CMB_F5 FBTN_OK BTN_AVB CMB_F6 
         FILL-IN-B2 FILL-IN-B1 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE extrakon_UI C-Win 
PROCEDURE extrakon_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   {EXTRAKON.I}
  
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE konlabel2_UI C-Win 
PROCEDURE konlabel2_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   DEFINE INPUT PARAMETER vad AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER f1var AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER f2var AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER f3var AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER f4var AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER f5var AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER f6var AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER f7var AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER f8var AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER f9var AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER idvar AS CHARACTER NO-UNDO. /* ny input istället för id (från bervalsw.p när man klickar i BRW_VAL) */ 
   IF vad = "VAL" THEN DO:
      ASSIGN
      kon_val2.F1:LABEL IN BROWSE BRW_VAL = f1var
      kon_val2.F2:LABEL IN BROWSE BRW_VAL = f2var  
      kon_val2.F3:LABEL IN BROWSE BRW_VAL = f3var
      kon_val2.F4:LABEL IN BROWSE BRW_VAL = f4var      
      kon_val2.F5:LABEL IN BROWSE BRW_VAL = f5var      
      kon_val2.F6:LABEL IN BROWSE BRW_VAL = f6var.
      /*IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT"  THEN DO:
         kon_val2.ID2:LABEL IN BROWSE BRW_VAL = "Objektnr".
      END.
      ELSE IF Guru.Konstanter:globforetag = "UMEA" OR Guru.Konstanter:globforetag = "cGRAN" OR Guru.Konstanter:globforetag = "cGKAL" THEN DO:
         kon_val2.ID2:LABEL IN BROWSE BRW_VAL = "Byggnr".
      END.
      ELSE DO:
         /*kon_val.ID2:LABEL IN BROWSE BRW_VAL = "Id".*/ /* GAMMAL */
         kon_val2.ID2:LABEL IN BROWSE BRW_VAL = idvar. /* istället för "id" i BRW_VAL */
      END.*/ 
   END.

   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE konlabel_UI C-Win 
PROCEDURE konlabel_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   DEFINE INPUT PARAMETER vad AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER f1var AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER f2var AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER f3var AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER f4var AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER f5var AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER f6var AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER f7var AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER f8var AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER f9var AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER idvar AS CHARACTER NO-UNDO. /* ny input istället för id (från bervalsw.p när man klickar i BRW_VAL) */ 
   IF vad = "VAL" THEN DO:
      ASSIGN
      kon_val.F1:LABEL IN BROWSE BRW_MTRL2 = f1var
      kon_val.F2:LABEL IN BROWSE BRW_MTRL2 = f2var  
      kon_val.F3:LABEL IN BROWSE BRW_MTRL2 = f3var
      kon_val.F4:LABEL IN BROWSE BRW_MTRL2 = f4var      
      kon_val.F5:LABEL IN BROWSE BRW_MTRL2 = f5var      
      kon_val.F6:LABEL IN BROWSE BRW_MTRL2 = f6var.       
   END.   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE nerupp_UI C-Win 
PROCEDURE nerupp_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   EMPTY TEMP-TABLE ot NO-ERROR. 
   antal_valda = BRW_MTRL2:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME}. 
   antal_raknare = 1.
   IF antal_valda > 0 THEN DO:
      DO WHILE antal_raknare LE antal_valda:                                   
         status-ok = BRW_MTRL2:FETCH-SELECTED-ROW(antal_raknare).
         CREATE ot.
         ASSIGN
         ot.OROW = ROWID(kon_val)
         ot.ORD = kon_val.ORD.                            
         antal_raknare = antal_raknare + 1.
      END.
      FIND FIRST ot USE-INDEX ORD NO-LOCK NO-ERROR.
      antal_raknare = ot.ORD.
      FOR EACH ot USE-INDEX NORD:
         FIND kon_val WHERE ROWID(kon_val) = ot.OROW NO-ERROR.
         IF AVAILABLE kon_val THEN kon_val.ORD = antal_raknare.
         antal_raknare = antal_raknare + 1.
      END.     
      RUN openbdynspec_UI IN brwproc[{&RIGHT-BROWSE}].                  
   END.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE vcbrwval2_UI C-Win 
PROCEDURE vcbrwval2_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/  
   IF AVAILABLE kon_val2 THEN DO:
      /*
      RUN setlastrowid_UI IN brwproc[{&LEFT-BROWSE}] (INPUT ROWID(kon_val2)).
      RUN lastselectdyn_UI IN brwproc[{&LEFT-BROWSE}].      
      */
      FIND FIRST bbenamntemp WHERE bbenamntemp.KONSKOD = kon_val2.GRUPP
      USE-INDEX KOD NO-LOCK NO-ERROR.
      IF AVAILABLE bbenamntemp THEN DO:
         IF AVAILABLE bbenamntemp THEN DO:
            RUN konlabel2_UI 
               (INPUT "VAL",
                INPUT bbenamntemp.B1,
            INPUT bbenamntemp.B2,
            INPUT bbenamntemp.B3,
            INPUT bbenamntemp.B4,
            INPUT bbenamntemp.B5,
            INPUT bbenamntemp.B6,
            INPUT "",
            INPUT "",
            INPUT "",
            INPUT bbenamntemp.ID2). /* NY */   
         END.                  
         /*IF INDEX(kon_val2.F1:LABEL IN BROWSE BRW_VAL," ^") = 0 AND INDEX(kon_val2.F1:LABEL," v") = 0 THEN kon_val2.F1:LABEL IN BROWSE BRW_VAL = SUBSTRING(bbenamntemp.B1,1,20).      
         ELSE IF INDEX(kon_val2.F1:LABEL," ^") NE 0 THEN kon_val2.F1:LABEL IN BROWSE BRW_VAL = SUBSTRING(bbenamntemp.B1,1,20) + " ^".      
         ELSE IF INDEX(kon_val2.F1:LABEL," v") NE 0 THEN kon_val2.F1:LABEL IN BROWSE BRW_VAL = SUBSTRING(bbenamntemp.B1,1,20) + " v".*/      
      END.
      IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" THEN DO:
         IF INDEX(kon_val2.ID2:LABEL IN BROWSE BRW_VAL," ^") = 0 AND INDEX(kon_val2.ID2:LABEL," v") = 0 
            THEN kon_val2.ID2:LABEL IN BROWSE BRW_VAL = "Objektnr".
         ELSE IF INDEX(kon_val2.ID2:LABEL," ^") NE 0 THEN kon_val2.ID2:LABEL IN BROWSE BRW_VAL = "Objektnr" + " ^".      
         ELSE IF INDEX(kon_val2.ID2:LABEL," v") NE 0 THEN kon_val2.ID2:LABEL IN BROWSE BRW_VAL = "Objektnr" + " v".      
         
      END.
      ELSE IF Guru.Konstanter:globforetag = "UMEA" THEN DO:
         IF INDEX(kon_val2.ID2:LABEL IN BROWSE BRW_VAL," ^") = 0 AND INDEX(kon_val2.ID2:LABEL," v") = 0 
            THEN kon_val2.ID2:LABEL IN BROWSE BRW_VAL = "Byggnr".
         ELSE IF INDEX(kon_val2.ID2:LABEL," ^") NE 0 THEN kon_val2.ID2:LABEL IN BROWSE BRW_VAL = "Byggnr" + " ^".      
         ELSE IF INDEX(kon_val2.ID2:LABEL," v") NE 0 THEN kon_val2.ID2:LABEL IN BROWSE BRW_VAL = "Byggnr" + " v".      
      END.
      ELSE DO:
         IF INDEX(kon_val2.ID2:LABEL IN BROWSE BRW_VAL," ^") = 0 AND INDEX(kon_val2.ID2:LABEL," v") = 0 
            THEN kon_val2.ID2:LABEL IN BROWSE BRW_VAL = "Id".
         ELSE IF INDEX(kon_val2.ID2:LABEL," ^") NE 0 THEN kon_val2.ID2:LABEL IN BROWSE BRW_VAL = "Id" + " ^".      
         ELSE IF INDEX(kon_val2.ID2:LABEL," v") NE 0 THEN kon_val2.ID2:LABEL IN BROWSE BRW_VAL = "Id" + " v".      
      END.
      
      
      {&OPEN-BROWSERS-IN-QUERY-{&FRAME-NAME}}            
   END.
   ELSE DO:
      /*
      ASSIGN      
      kon_val2.F1:LABEL IN BROWSE BRW_VAL = "".     
      IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" THEN DO:
         kon_val2.ID2:LABEL IN BROWSE BRW_VAL = "Objektnr".
      END.
      ELSE IF Guru.Konstanter:globforetag = "UMEA" THEN DO:
         kon_val2.ID2:LABEL IN BROWSE BRW_VAL = "Byggnr".
      END.
      ELSE DO:
         kon_val2.ID2:LABEL IN BROWSE BRW_VAL = "Id".
      END.
      */
      {&OPEN-BROWSERS-IN-QUERY-{&FRAME-NAME}}
      RETURN.
   END.           
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE vcbrwval_UI C-Win 
PROCEDURE vcbrwval_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/   
   IF AVAILABLE kon_val THEN DO:
      /*
      RUN setlastrowid_UI IN brwproc[{&RIGHT-BROWSE}] (INPUT ROWID(kon_val)).
      RUN lastselectdyn_UI IN brwproc[{&RIGHT-BROWSE}].
      */
      FIND FIRST bbenamntemp WHERE bbenamntemp.KONSKOD = kon_val.GRUPP
      USE-INDEX KOD NO-LOCK NO-ERROR.      
      IF AVAILABLE bbenamntemp THEN DO:
         RUN konlabel_UI 
               (INPUT "VAL",
                INPUT bbenamntemp.B1,
            INPUT bbenamntemp.B2,
            INPUT bbenamntemp.B3,
            INPUT bbenamntemp.B4,
            INPUT bbenamntemp.B5,
            INPUT bbenamntemp.B6,
            INPUT "",
            INPUT "",
            INPUT "",
            INPUT bbenamntemp.ID2). /* NY */ 
         /*IF INDEX(kon_val.F1:LABEL IN BROWSE BRW_MTRL2," ^") = 0 AND INDEX(kon_val.F1:LABEL," v") = 0 THEN kon_val.F1:LABEL IN BROWSE BRW_MTRL2 = SUBSTRING(bbenamntemp.B1,1,20).      
         ELSE IF INDEX(kon_val.F1:LABEL," ^") NE 0 THEN kon_val.F1:LABEL IN BROWSE BRW_MTRL2 = SUBSTRING(bbenamntemp.B1,1,20) + " ^".      
         ELSE IF INDEX(kon_val.F1:LABEL," v") NE 0 THEN kon_val.F1:LABEL IN BROWSE BRW_MTRL2 = SUBSTRING(bbenamntemp.B1,1,20) + " v".*/               
      END.
      IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" THEN DO:
         IF INDEX(kon_val.ID2:LABEL IN BROWSE BRW_MTRL2," ^") = 0 AND INDEX(kon_val.ID2:LABEL," v") = 0 
            THEN kon_val.ID2:LABEL IN BROWSE BRW_MTRL2 = "Objektnr".
         ELSE IF INDEX(kon_val.ID2:LABEL," ^") NE 0 THEN kon_val.ID2:LABEL IN BROWSE BRW_MTRL2 = "Objektnr" + " ^".      
         ELSE IF INDEX(kon_val.ID2:LABEL," v") NE 0 THEN kon_val.ID2:LABEL IN BROWSE BRW_MTRL2 = "Objektnr" + " v".      
         
      END.
      ELSE IF Guru.Konstanter:globforetag = "UMEA" THEN DO:
         IF INDEX(kon_val.ID2:LABEL IN BROWSE BRW_MTRL2," ^") = 0 AND INDEX(kon_val.ID2:LABEL," v") = 0 
            THEN kon_val.ID2:LABEL IN BROWSE BRW_MTRL2 = "Byggnr".
         ELSE IF INDEX(kon_val.ID2:LABEL," ^") NE 0 THEN kon_val.ID2:LABEL IN BROWSE BRW_MTRL2 = "Byggnr" + " ^".      
         ELSE IF INDEX(kon_val.ID2:LABEL," v") NE 0 THEN kon_val.ID2:LABEL IN BROWSE BRW_MTRL2 = "Byggnr" + " v".      
      END.
      ELSE DO:
         IF INDEX(kon_val.ID2:LABEL IN BROWSE BRW_MTRL2," ^") = 0 AND INDEX(kon_val.ID2:LABEL," v") = 0 
            THEN kon_val.ID2:LABEL IN BROWSE BRW_MTRL2 = "Id".
         ELSE IF INDEX(kon_val.ID2:LABEL," ^") NE 0 THEN kon_val.ID2:LABEL IN BROWSE BRW_MTRL2 = "Id" + " ^".      
         ELSE IF INDEX(kon_val.ID2:LABEL," v") NE 0 THEN kon_val.ID2:LABEL IN BROWSE BRW_MTRL2 = "Id" + " v".      
      END.
      
      {&OPEN-BROWSERS-IN-QUERY-{&FRAME-NAME}}         
   END.
   ELSE DO:
       /*
      ASSIGN      
      kon_val.F1:LABEL IN BROWSE BRW_MTRL2 = "".     
      IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" THEN DO:
         kon_val.ID2:LABEL IN BROWSE BRW_MTRL2 = "Objektnr".
      END.
      ELSE IF Guru.Konstanter:globforetag = "UMEA" THEN DO:
         kon_val.ID2:LABEL IN BROWSE BRW_MTRL2 = "Byggnr".
      END.
      ELSE DO:
         kon_val.ID2:LABEL IN BROWSE BRW_MTRL2 = "Id".
      END.
      */
      {&OPEN-BROWSERS-IN-QUERY-{&FRAME-NAME}}
      RETURN.
   END.              
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

