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

  Created: 05/29/97 -  3:23 pm

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
DEFINE INPUT PARAMETER kon_rowid AS ROWID.
DEFINE NEW SHARED VARIABLE kalkvar AS LOGICAL NO-UNDO.
kalkvar = TRUE.
/* Local Variable Definitions ---                                       */ 
{ALLDEF.I}
&Scoped-define NEW
{GLOBVAR2DEL1.I}
&Scoped-define SHARED SHARED
{KONSTRMTRL.I}
{ANNAMN.I}
{BBENAMNTEMP.I}
{KONVALTEMP.I} 
{KONID.I} 
{HOPPSEK2W.I}
&Scoped-define NEW NEW
DEFINE NEW SHARED VARIABLE prisvar AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO. 
DEFINE SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.
DEFINE VARIABLE sparnum AS INTEGER NO-UNDO.
 


DEFINE TEMP-TABLE temp2 NO-UNDO
   FIELD KVALKOD AS CHARACTER
   FIELD BENAMNING AS CHARACTER LABEL "Benämning".   
   
DEFINE TEMP-TABLE temp3 NO-UNDO
   FIELD KVALKOD AS CHARACTER
   FIELD BENAMNING AS CHARACTER LABEL "Benämning".   
   
DEFINE TEMP-TABLE temp4 NO-UNDO
   FIELD KVALKOD AS CHARACTER
   FIELD BENAMNING AS CHARACTER LABEL "Benämning".   
      
DEFINE TEMP-TABLE temp5 NO-UNDO
   FIELD KVALKOD AS CHARACTER
   FIELD BENAMNING AS CHARACTER LABEL "Benämning".   
   
DEFINE TEMP-TABLE temp6 NO-UNDO
   FIELD KVALKOD AS CHARACTER
   FIELD BENAMNING AS CHARACTER LABEL "Benämning".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-A
&Scoped-define BROWSE-NAME BRW_B2

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES temp2 temp3 temp4 temp5 temp6

/* Definitions for BROWSE BRW_B2                                        */
&Scoped-define FIELDS-IN-QUERY-BRW_B2 temp2.KVALKOD temp2.BENAMNING 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_B2 
&Scoped-define QUERY-STRING-BRW_B2 FOR EACH temp2 NO-LOCK
&Scoped-define OPEN-QUERY-BRW_B2 OPEN QUERY BRW_B2 FOR EACH temp2 NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_B2 temp2
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_B2 temp2


/* Definitions for BROWSE BRW_B3                                        */
&Scoped-define FIELDS-IN-QUERY-BRW_B3 temp3.KVALKOD temp3.BENAMNING 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_B3 
&Scoped-define QUERY-STRING-BRW_B3 FOR EACH temp3 NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BRW_B3 OPEN QUERY BRW_B3 FOR EACH temp3 NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BRW_B3 temp3
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_B3 temp3


/* Definitions for BROWSE BRW_B4                                        */
&Scoped-define FIELDS-IN-QUERY-BRW_B4 temp4.KVALKOD temp4.BENAMNING 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_B4 
&Scoped-define QUERY-STRING-BRW_B4 FOR EACH temp4 NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BRW_B4 OPEN QUERY BRW_B4 FOR EACH temp4 NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BRW_B4 temp4
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_B4 temp4


/* Definitions for BROWSE BRW_B5                                        */
&Scoped-define FIELDS-IN-QUERY-BRW_B5 temp5.KVALKOD temp5.BENAMNING 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_B5 
&Scoped-define QUERY-STRING-BRW_B5 FOR EACH temp5 NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BRW_B5 OPEN QUERY BRW_B5 FOR EACH temp5 NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BRW_B5 temp5
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_B5 temp5


/* Definitions for BROWSE BRW_B6                                        */
&Scoped-define FIELDS-IN-QUERY-BRW_B6 temp6.KVALKOD temp6.BENAMNING 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_B6 
&Scoped-define QUERY-STRING-BRW_B6 FOR EACH temp6 NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BRW_B6 OPEN QUERY BRW_B6 FOR EACH temp6 NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BRW_B6 temp6
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_B6 temp6


/* Definitions for FRAME FRAME-A                                        */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-3 RECT-4 BRW_B2 BRW_B3 BTN_VISA ~
BTN_SKRIV BRW_B4 BRW_B5 BRW_B6 TOG_MTRL TOG_ANM RAD_PRIS TOG_UPP TOG_KOMB ~
TOG_ID TOG_TEXT TOG_SPANN TOG_P3 BTN_AVB FILL-IN-B1 FILL-IN-B2 FILL-IN-B3 ~
FILL-IN-1 FILL-IN-11 FILL-IN-B4 FILL-IN-B5 FILL-IN-B6 FILL-IN-FORD ~
FILL-IN-LINJE FILL-IN-NAT FILL-IN-7 FILL-IN-8 FILL-IN-9 FILL-IN-IDNR1 ~
FILL-IN-IDNR2 FILL-IN-IDNR3 FILL-IN-FRI1 FILL-IN-FRI2 FILL-IN-FRI3 
&Scoped-Define DISPLAYED-OBJECTS TOG_MTRL TOG_ANM RAD_PRIS TOG_UPP TOG_KOMB ~
TOG_ID TOG_TEXT TOG_SPANN TOG_P3 FILL-IN-B1 FILL-IN-B2 FILL-IN-B3 FILL-IN-1 ~
FILL-IN-11 FILL-IN-B4 FILL-IN-B5 FILL-IN-B6 FILL-IN-FORD FILL-IN-LINJE ~
FILL-IN-NAT FILL-IN-7 FILL-IN-8 FILL-IN-9 FILL-IN-IDNR1 FILL-IN-IDNR2 ~
FILL-IN-IDNR3 FILL-IN-FRI1 FILL-IN-FRI2 FILL-IN-FRI3 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR WINDOW-1 AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB AUTO-END-KEY 
     LABEL "Avsluta" 
     SIZE 14 BY 1
     BGCOLOR 8 .

DEFINE BUTTON BTN_SKRIV 
     LABEL "Skriv ut" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_VISA 
     LABEL "Visa" 
     SIZE 14 BY 1.

DEFINE VARIABLE FILL-IN-1 AS CHARACTER FORMAT "X(25)":U 
      VIEW-AS TEXT 
     SIZE 40 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-11 AS CHARACTER FORMAT "X(25)":U 
      VIEW-AS TEXT 
     SIZE 40 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-7 AS CHARACTER FORMAT "X(10)":U 
      VIEW-AS TEXT 
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-8 AS CHARACTER FORMAT "X(10)":U 
      VIEW-AS TEXT 
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-9 AS CHARACTER FORMAT "X(10)":U 
      VIEW-AS TEXT 
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-B1 AS CHARACTER FORMAT "X(20)":U 
      VIEW-AS TEXT 
     SIZE 36.13 BY 1
     BGCOLOR 7  NO-UNDO.

DEFINE VARIABLE FILL-IN-B2 AS CHARACTER FORMAT "X(20)":U 
      VIEW-AS TEXT 
     SIZE 36.13 BY 1
     BGCOLOR 7  NO-UNDO.

DEFINE VARIABLE FILL-IN-B3 AS CHARACTER FORMAT "X(20)":U 
      VIEW-AS TEXT 
     SIZE 36.13 BY 1
     BGCOLOR 7  NO-UNDO.

DEFINE VARIABLE FILL-IN-B4 AS CHARACTER FORMAT "X(20)":U 
      VIEW-AS TEXT 
     SIZE 36.13 BY 1
     BGCOLOR 7  NO-UNDO.

DEFINE VARIABLE FILL-IN-B5 AS CHARACTER FORMAT "X(20)":U 
      VIEW-AS TEXT 
     SIZE 36.13 BY 1
     BGCOLOR 7  NO-UNDO.

DEFINE VARIABLE FILL-IN-B6 AS CHARACTER FORMAT "X(20)":U 
      VIEW-AS TEXT 
     SIZE 36.13 BY 1
     BGCOLOR 7  NO-UNDO.

DEFINE VARIABLE FILL-IN-FORD AS CHARACTER FORMAT "X(25)":U 
      VIEW-AS TEXT 
     SIZE 30 BY 1
     BGCOLOR 7  NO-UNDO.

DEFINE VARIABLE FILL-IN-FRI1 AS INTEGER FORMAT ">>>>>>>>>>":U INITIAL 0 
      VIEW-AS TEXT 
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-FRI2 AS INTEGER FORMAT ">>>>>>>>>>":U INITIAL 0 
      VIEW-AS TEXT 
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-FRI3 AS CHARACTER FORMAT "X(20)":U 
      VIEW-AS TEXT 
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-IDNR1 AS CHARACTER FORMAT "X(20)":U 
      VIEW-AS TEXT 
     SIZE 30 BY 1
     BGCOLOR 7  NO-UNDO.

DEFINE VARIABLE FILL-IN-IDNR2 AS CHARACTER FORMAT "X(20)":U 
      VIEW-AS TEXT 
     SIZE 30 BY 1
     BGCOLOR 7  NO-UNDO.

DEFINE VARIABLE FILL-IN-IDNR3 AS CHARACTER FORMAT "X(20)":U 
      VIEW-AS TEXT 
     SIZE 30 BY 1
     BGCOLOR 7  NO-UNDO.

DEFINE VARIABLE FILL-IN-LINJE AS CHARACTER FORMAT "X(25)":U 
      VIEW-AS TEXT 
     SIZE 30 BY 1
     BGCOLOR 7  NO-UNDO.

DEFINE VARIABLE FILL-IN-NAT AS CHARACTER FORMAT "X(25)":U 
      VIEW-AS TEXT 
     SIZE 30 BY 1
     BGCOLOR 7  NO-UNDO.

DEFINE VARIABLE RAD_PRIS AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Pris", 1,
"Ej pris", 2
     SIZE 9.88 BY 2.25 NO-UNDO.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 110 BY 6.63
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 110 BY 4.46
     BGCOLOR 8 .

DEFINE {&NEW} SHARED VARIABLE TOG_ANM AS LOGICAL INITIAL no 
     LABEL "Anmärkning" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.5 BY .88 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE TOG_ID AS LOGICAL INITIAL no 
     LABEL "Identitet" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.88 BY .88 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE TOG_KOMB AS LOGICAL INITIAL no 
     LABEL "Kombination" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.5 BY .88 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE TOG_MTRL AS LOGICAL INITIAL no 
     LABEL "Materiel" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.13 BY .88 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE TOG_P3 AS LOGICAL INITIAL no 
     LABEL "P3-koder" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.5 BY .88 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE TOG_SPANN AS LOGICAL INITIAL no 
     LABEL "Spann/kabellängd" 
     VIEW-AS TOGGLE-BOX
     SIZE 19 BY .88 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE TOG_TEXT AS LOGICAL INITIAL no 
     LABEL "Mtrl text" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.5 BY .88 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE TOG_UPP AS LOGICAL INITIAL no 
     LABEL "Upplag" 
     VIEW-AS TOGGLE-BOX
     SIZE 10.25 BY .88 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_B2 FOR 
      temp2 SCROLLING.

DEFINE QUERY BRW_B3 FOR 
      temp3 SCROLLING.

DEFINE QUERY BRW_B4 FOR 
      temp4 SCROLLING.

DEFINE QUERY BRW_B5 FOR 
      temp5 SCROLLING.

DEFINE QUERY BRW_B6 FOR 
      temp6 SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_B2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_B2 WINDOW-1 _STRUCTURED
  QUERY BRW_B2 NO-LOCK DISPLAY
      temp2.KVALKOD COLUMN-LABEL "Kod" FORMAT "X(8)":U WIDTH 9
      temp2.BENAMNING FORMAT "X(256)":U WIDTH 23
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SIZE 36.13 BY 5.

DEFINE BROWSE BRW_B3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_B3 WINDOW-1 _STRUCTURED
  QUERY BRW_B3 NO-LOCK DISPLAY
      temp3.KVALKOD COLUMN-LABEL "Kod" FORMAT "X(8)":U WIDTH 9
      temp3.BENAMNING FORMAT "X(256)":U WIDTH 23
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SIZE 36.13 BY 5.

DEFINE BROWSE BRW_B4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_B4 WINDOW-1 _STRUCTURED
  QUERY BRW_B4 NO-LOCK DISPLAY
      temp4.KVALKOD COLUMN-LABEL "Kod" FORMAT "X(8)":U WIDTH 9
      temp4.BENAMNING FORMAT "X(256)":U WIDTH 23
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SIZE 36.13 BY 5.

DEFINE BROWSE BRW_B5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_B5 WINDOW-1 _STRUCTURED
  QUERY BRW_B5 NO-LOCK DISPLAY
      temp5.KVALKOD COLUMN-LABEL "Kod" FORMAT "X(8)":U WIDTH 9
      temp5.BENAMNING FORMAT "X(256)":U WIDTH 23
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SIZE 36.13 BY 5.

DEFINE BROWSE BRW_B6
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_B6 WINDOW-1 _STRUCTURED
  QUERY BRW_B6 NO-LOCK DISPLAY
      temp6.KVALKOD COLUMN-LABEL "Kod" FORMAT "X(8)":U WIDTH 9
      temp6.BENAMNING FORMAT "X(256)":U WIDTH 23
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SIZE 36.13 BY 5.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     BRW_B2 AT ROW 3.67 COL 38.13
     BRW_B3 AT ROW 3.67 COL 74.75
     BTN_VISA AT ROW 8 COL 111.38
     BTN_SKRIV AT ROW 9.08 COL 111.38
     BRW_B4 AT ROW 10.04 COL 1.5
     BRW_B5 AT ROW 10.04 COL 38.13
     BRW_B6 AT ROW 10.04 COL 74.75
     TOG_MTRL AT ROW 22.33 COL 46.5
     TOG_ANM AT ROW 22.33 COL 77.75
     RAD_PRIS AT ROW 23.08 COL 24.88 NO-LABEL
     TOG_UPP AT ROW 23.33 COL 46.5
     TOG_KOMB AT ROW 23.33 COL 77.75
     TOG_ID AT ROW 24.33 COL 46.5
     TOG_TEXT AT ROW 24.33 COL 77.75
     TOG_SPANN AT ROW 25.33 COL 46.5
     TOG_P3 AT ROW 25.33 COL 77.75
     BTN_AVB AT ROW 25.5 COL 111.38
     FILL-IN-B1 AT ROW 2.63 COL 1.5 NO-LABEL
     FILL-IN-B2 AT ROW 2.63 COL 36.13 COLON-ALIGNED NO-LABEL
     FILL-IN-B3 AT ROW 2.63 COL 72.75 COLON-ALIGNED NO-LABEL
     FILL-IN-1 AT ROW 3.67 COL 2.25 NO-LABEL
     FILL-IN-11 AT ROW 5.04 COL 2.25 NO-LABEL
     FILL-IN-B4 AT ROW 9 COL 1.5 NO-LABEL
     FILL-IN-B5 AT ROW 9 COL 36.13 COLON-ALIGNED NO-LABEL
     FILL-IN-B6 AT ROW 9 COL 72.75 COLON-ALIGNED NO-LABEL
     FILL-IN-FORD AT ROW 17 COL 4.63 NO-LABEL
     FILL-IN-LINJE AT ROW 17 COL 41.13 NO-LABEL
     FILL-IN-NAT AT ROW 17 COL 75.75 COLON-ALIGNED NO-LABEL
     FILL-IN-7 AT ROW 18.13 COL 4.63 NO-LABEL
     FILL-IN-8 AT ROW 18.13 COL 39 COLON-ALIGNED NO-LABEL
     FILL-IN-9 AT ROW 18.13 COL 75.75 COLON-ALIGNED NO-LABEL
     FILL-IN-IDNR1 AT ROW 19.58 COL 4.63 NO-LABEL
     FILL-IN-IDNR2 AT ROW 19.58 COL 39.13 COLON-ALIGNED NO-LABEL
     FILL-IN-IDNR3 AT ROW 19.58 COL 75.75 COLON-ALIGNED NO-LABEL
     FILL-IN-FRI1 AT ROW 20.75 COL 4.63 NO-LABEL
     FILL-IN-FRI2 AT ROW 20.75 COL 39 COLON-ALIGNED NO-LABEL
     FILL-IN-FRI3 AT ROW 20.75 COL 75.75 COLON-ALIGNED NO-LABEL
     "Val:" VIEW-AS TEXT
          SIZE 17.88 BY .88 AT ROW 1.38 COL 3.13
     "Identifikation:" VIEW-AS TEXT
          SIZE 24 BY .88 AT ROW 15.75 COL 3.88
     RECT-3 AT ROW 15.5 COL 1
     RECT-4 AT ROW 22.13 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 124.63 BY 25.63.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: 
   Temp-Tables and Buffers:
      TABLE: ? T "?" NO-UNDO temp-db temp2
      TABLE: ? T "?" NO-UNDO temp-db temp3
      TABLE: ? T "?" NO-UNDO temp-db temp4
      TABLE: ? T "?" NO-UNDO temp-db temp5
      TABLE: ? T "?" NO-UNDO temp-db temp6
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW WINDOW-1 ASSIGN
         HIDDEN             = YES
         TITLE              = "Information"
         HEIGHT             = 25.71
         WIDTH              = 124.88
         MAX-HEIGHT         = 26.46
         MAX-WIDTH          = 124.88
         VIRTUAL-HEIGHT     = 26.46
         VIRTUAL-WIDTH      = 124.88
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
                                                                        */
/* BROWSE-TAB BRW_B2 RECT-4 FRAME-A */
/* BROWSE-TAB BRW_B3 BRW_B2 FRAME-A */
/* BROWSE-TAB BRW_B4 BTN_SKRIV FRAME-A */
/* BROWSE-TAB BRW_B5 BRW_B4 FRAME-A */
/* BROWSE-TAB BRW_B6 BRW_B5 FRAME-A */
ASSIGN 
       BRW_B2:HIDDEN  IN FRAME FRAME-A                = TRUE
       BRW_B2:COLUMN-RESIZABLE IN FRAME FRAME-A       = TRUE.

ASSIGN 
       BRW_B3:HIDDEN  IN FRAME FRAME-A                = TRUE
       BRW_B3:COLUMN-RESIZABLE IN FRAME FRAME-A       = TRUE.

ASSIGN 
       BRW_B4:HIDDEN  IN FRAME FRAME-A                = TRUE
       BRW_B4:COLUMN-RESIZABLE IN FRAME FRAME-A       = TRUE.

ASSIGN 
       BRW_B5:HIDDEN  IN FRAME FRAME-A                = TRUE
       BRW_B5:COLUMN-RESIZABLE IN FRAME FRAME-A       = TRUE.

ASSIGN 
       BRW_B6:HIDDEN  IN FRAME FRAME-A                = TRUE
       BRW_B6:COLUMN-RESIZABLE IN FRAME FRAME-A       = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-1 IN FRAME FRAME-A
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FILL-IN-11 IN FRAME FRAME-A
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FILL-IN-7 IN FRAME FRAME-A
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FILL-IN-B1 IN FRAME FRAME-A
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FILL-IN-B4 IN FRAME FRAME-A
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FILL-IN-FORD IN FRAME FRAME-A
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FILL-IN-FRI1 IN FRAME FRAME-A
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FILL-IN-IDNR1 IN FRAME FRAME-A
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FILL-IN-LINJE IN FRAME FRAME-A
   ALIGN-L                                                              */
/* SETTINGS FOR TOGGLE-BOX TOG_ANM IN FRAME FRAME-A
   SHARED                                                               */
/* SETTINGS FOR TOGGLE-BOX TOG_ID IN FRAME FRAME-A
   SHARED                                                               */
/* SETTINGS FOR TOGGLE-BOX TOG_KOMB IN FRAME FRAME-A
   SHARED                                                               */
/* SETTINGS FOR TOGGLE-BOX TOG_MTRL IN FRAME FRAME-A
   SHARED                                                               */
/* SETTINGS FOR TOGGLE-BOX TOG_P3 IN FRAME FRAME-A
   SHARED                                                               */
/* SETTINGS FOR TOGGLE-BOX TOG_SPANN IN FRAME FRAME-A
   SHARED                                                               */
/* SETTINGS FOR TOGGLE-BOX TOG_TEXT IN FRAME FRAME-A
   SHARED                                                               */
/* SETTINGS FOR TOGGLE-BOX TOG_UPP IN FRAME FRAME-A
   SHARED                                                               */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-1)
THEN WINDOW-1:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_B2
/* Query rebuild information for BROWSE BRW_B2
     _TblList          = "Temp-Tables.temp2"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.temp2.KVALKOD
"KVALKOD" "Kod" ? "character" ? ? ? ? ? ? no ? no no "9" yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.temp2.BENAMNING
"BENAMNING" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "23" yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_B2 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_B3
/* Query rebuild information for BROWSE BRW_B3
     _TblList          = "Temp-Tables.temp3"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > Temp-Tables.temp3.KVALKOD
"KVALKOD" "Kod" ? "character" ? ? ? ? ? ? no ? no no "9" yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.temp3.BENAMNING
"BENAMNING" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "23" yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_B3 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_B4
/* Query rebuild information for BROWSE BRW_B4
     _TblList          = "Temp-Tables.temp4"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > Temp-Tables.temp4.KVALKOD
"KVALKOD" "Kod" ? "character" ? ? ? ? ? ? no ? no no "9" yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.temp4.BENAMNING
"BENAMNING" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "23" yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_B4 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_B5
/* Query rebuild information for BROWSE BRW_B5
     _TblList          = "Temp-Tables.temp5"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > Temp-Tables.temp5.KVALKOD
"KVALKOD" "Kod" ? "character" ? ? ? ? ? ? no ? no no "9" yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.temp5.BENAMNING
"BENAMNING" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "23" yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_B5 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_B6
/* Query rebuild information for BROWSE BRW_B6
     _TblList          = "Temp-Tables.temp6"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > Temp-Tables.temp6.KVALKOD
"KVALKOD" "Kod" ? "character" ? ? ? ? ? ? no ? no no "9" yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.temp6.BENAMNING
"BENAMNING" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "23" yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_B6 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME BTN_SKRIV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_SKRIV WINDOW-1
ON CHOOSE OF BTN_SKRIV IN FRAME FRAME-A /* Skriv ut */
DO:   
   {muswait.i}        
   RUN SKRIVVAL.W (INPUT FALSE).       
   IF musz = TRUE THEN musz = FALSE. 
   ELSE DO:                     
      skrivut = TRUE.
      {AVBGOM.I}
      RUN VISKONU.W (INPUT kon_rowid).
      {AVBFRAM.I}
   END.       
   {musarrow.i}     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_SKRIV WINDOW-1
ON MOUSE-MENU-CLICK OF BTN_SKRIV IN FRAME FRAME-A /* Skriv ut */
DO:
   RUN SIDLANGD.W.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_VISA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_VISA WINDOW-1
ON CHOOSE OF BTN_VISA IN FRAME FRAME-A /* Visa */
DO:           
   {muswait.i}                        
   skrivut = FALSE.
   {AVBGOM.I}
   RUN VISKONU.W (INPUT kon_rowid).     
   {AVBFRAM.I}
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RAD_PRIS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RAD_PRIS WINDOW-1
ON VALUE-CHANGED OF RAD_PRIS IN FRAME FRAME-A
DO:
   ASSIGN
   RAD_PRIS = INPUT RAD_PRIS
   prisvar = RAD_PRIS.       
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_ANM
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_ANM WINDOW-1
ON VALUE-CHANGED OF TOG_ANM IN FRAME FRAME-A /* Anmärkning */
DO:
   TOG_ANM  = INPUT TOG_ANM.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_ID
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_ID WINDOW-1
ON VALUE-CHANGED OF TOG_ID IN FRAME FRAME-A /* Identitet */
DO:
   TOG_ID = INPUT TOG_ID.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_KOMB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_KOMB WINDOW-1
ON VALUE-CHANGED OF TOG_KOMB IN FRAME FRAME-A /* Kombination */
DO:
   TOG_KOMB = INPUT TOG_KOMB.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_MTRL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_MTRL WINDOW-1
ON VALUE-CHANGED OF TOG_MTRL IN FRAME FRAME-A /* Materiel */
DO:
   TOG_MTRL = INPUT TOG_MTRL.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_P3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_P3 WINDOW-1
ON VALUE-CHANGED OF TOG_P3 IN FRAME FRAME-A /* P3-koder */
DO:
   TOG_P3 = INPUT TOG_P3.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_SPANN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_SPANN WINDOW-1
ON VALUE-CHANGED OF TOG_SPANN IN FRAME FRAME-A /* Spann/kabellängd */
DO:
   TOG_SPANN = INPUT TOG_SPANN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_TEXT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_TEXT WINDOW-1
ON VALUE-CHANGED OF TOG_TEXT IN FRAME FRAME-A /* Mtrl text */
DO:
   TOG_TEXT = INPUT TOG_TEXT.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_UPP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_UPP WINDOW-1
ON VALUE-CHANGED OF TOG_UPP IN FRAME FRAME-A /* Upplag */
DO:
   TOG_UPP = INPUT TOG_UPP.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_B2
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
   {muswait.i}
   EMPTY TEMP-TABLE temp2  NO-ERROR. 
   EMPTY TEMP-TABLE temp3 NO-ERROR. 
   EMPTY TEMP-TABLE temp4 NO-ERROR. 
   EMPTY TEMP-TABLE temp5 NO-ERROR. 
   EMPTY TEMP-TABLE temp6 NO-ERROR.       
   FIND kon_val WHERE ROWID(kon_val) = kon_rowid NO-LOCK NO-ERROR.
   FIND FIRST bbenamntemp WHERE bbenamntemp.KONSKOD = kon_val.GRUPP USE-INDEX KOD
   NO-LOCK NO-ERROR.
   FIND FIRST annamntemp NO-LOCK NO-ERROR.  
   ASSIGN 
   FILL-IN-FORD = annamntemp.TAB1
   FILL-IN-LINJE = annamntemp.TAB2 
   FILL-IN-NAT = annamntemp.TAB3       
   FILL-IN-IDNR1 = bbenamntemp.ID1
   FILL-IN-IDNR2 = bbenamntemp.ID2.
   IF Guru.Konstanter:globforetag = "GRAN" OR Guru.Konstanter:globforetag = "NAEK" OR Guru.Konstanter:globforetag = "CPOMA" OR Guru.Konstanter:globforetag = "GKAL" THEN DO:
      FILL-IN-IDNR3 = "Littera:".
   END.
   ELSE DO:
      FILL-IN-IDNR3 = "Fri id:".
   END.   
   ASSIGN
   FILL-IN-B1 = bbenamntemp.B1
   FILL-IN-B2 = bbenamntemp.B2
   FILL-IN-B3 = bbenamntemp.B3  
   FILL-IN-B4 = bbenamntemp.B4
   FILL-IN-B5 = bbenamntemp.B5
   FILL-IN-B6 = bbenamntemp.B6
   FILL-IN-1 = kon_val.F1
   sparnum = kon_val.NUM.  
   
   IF kon_val.ID = TRUE THEN DO: 
      FIND FIRST kon_id WHERE kon_id.NUM = kon_val.NUM USE-INDEX NUM NO-LOCK NO-ERROR.                 
      ASSIGN
      FILL-IN-7 = kon_id.FORNR 
      FILL-IN-8 = kon_id.LINNR
      FILL-IN-9 = kon_id.NATNR
      FILL-IN-FRI1 = kon_id.FRI1   
      FILL-IN-FRI2 = kon_id.FRI2
      FILL-IN-FRI3 = kon_id.FRI3.
   END.
   
   FIND FIRST konsttemp WHERE konsttemp.KONSKOD = kon_val.GRUPP AND 
   konsttemp.KTYPKOD = kon_val.F1 USE-INDEX KOD NO-LOCK NO-ERROR. 
   IF AVAILABLE konsttemp THEN FILL-IN-11 = konsttemp.BENAMNING.   
   FOR EACH kon_val WHERE kon_val.NUM = sparnum AND /*kon_val.KSKAP = TRUE AND*/
   kon_val.F2 NE "":
      FIND FIRST konstvaltemp WHERE konstvaltemp.KONSKOD = kon_val.GRUPP AND
      konstvaltemp.KTYPKOD = kon_val.F1 AND konstvaltemp.KVALKOD = kon_val.F2 AND
      konstvaltemp.BB = bbenamntemp.B2 AND konstvaltemp.KOPP = TRUE 
      USE-INDEX KOPIA NO-LOCK NO-ERROR.
      IF AVAILABLE konstvaltemp THEN DO:
         CREATE temp2. 
         ASSIGN
         temp2.KVALKOD = kon_val.F2
         temp2.BENAMNING = konstvaltemp.BENAMNING.
      END.   
   END.    
   FOR EACH kon_val WHERE kon_val.NUM = sparnum AND /*kon_val.KSKAP = TRUE AND*/
   kon_val.F3 NE "":
      FIND FIRST konstvaltemp WHERE konstvaltemp.KONSKOD = kon_val.GRUPP AND
      konstvaltemp.KTYPKOD = kon_val.F1 AND konstvaltemp.KVALKOD = kon_val.F3 AND
      konstvaltemp.BB = bbenamntemp.B3 AND konstvaltemp.KOPP = TRUE 
      USE-INDEX KOPIA NO-LOCK NO-ERROR.
      IF AVAILABLE konstvaltemp THEN DO:
         CREATE temp3. 
         ASSIGN
         temp3.KVALKOD = kon_val.F3
         temp3.BENAMNING = konstvaltemp.BENAMNING.
      END.   
   END.     
   FOR EACH kon_val WHERE kon_val.NUM = sparnum AND /*kon_val.KSKAP = TRUE AND*/
   kon_val.F4 NE "":
      FIND FIRST konstvaltemp WHERE konstvaltemp.KONSKOD = kon_val.GRUPP AND
      konstvaltemp.KTYPKOD = kon_val.F1 AND konstvaltemp.KVALKOD = kon_val.F4 AND
      konstvaltemp.BB = bbenamntemp.B4 AND konstvaltemp.KOPP = TRUE 
      USE-INDEX KOPIA NO-LOCK NO-ERROR.
      IF AVAILABLE konstvaltemp THEN DO:
         CREATE temp4. 
         ASSIGN
         temp4.KVALKOD = kon_val.F4
         temp4.BENAMNING = konstvaltemp.BENAMNING.
      END.   
   END.   
   FOR EACH kon_val WHERE kon_val.NUM = sparnum AND /*kon_val.KSKAP = TRUE AND*/
   kon_val.F5 NE "":
      FIND FIRST konstvaltemp WHERE konstvaltemp.KONSKOD = kon_val.GRUPP AND
      konstvaltemp.KTYPKOD = kon_val.F1 AND konstvaltemp.KVALKOD = kon_val.F5 AND
      konstvaltemp.BB = bbenamntemp.B5 AND konstvaltemp.KOPP = TRUE 
      USE-INDEX KOPIA NO-LOCK NO-ERROR.
      IF AVAILABLE konstvaltemp THEN DO:
         CREATE temp5. 
         ASSIGN
         temp5.KVALKOD = kon_val.F5
         temp5.BENAMNING = konstvaltemp.BENAMNING.
      END.   
   END.
   FOR EACH kon_val WHERE kon_val.NUM = sparnum AND /*kon_val.KSKAP = TRUE AND*/
   kon_val.F6 NE "":
      FIND FIRST konstvaltemp WHERE konstvaltemp.KONSKOD = kon_val.GRUPP AND
      konstvaltemp.KTYPKOD = kon_val.F1 AND konstvaltemp.KVALKOD = kon_val.F6 AND
      konstvaltemp.BB = bbenamntemp.B6 AND konstvaltemp.KOPP = TRUE 
      USE-INDEX KOPIA NO-LOCK NO-ERROR.
      IF AVAILABLE konstvaltemp THEN DO:
         CREATE temp6. 
         ASSIGN
         temp6.KVALKOD = kon_val.F6
         temp6.BENAMNING = konstvaltemp.BENAMNING.
      END.   
   END.                                     
   OPEN QUERY BRW_B2 FOR EACH temp2 NO-LOCK .
   OPEN QUERY BRW_B3 FOR EACH temp3 NO-LOCK .      
   OPEN QUERY BRW_B4 FOR EACH temp4 NO-LOCK .
   OPEN QUERY BRW_B5 FOR EACH temp5 NO-LOCK.
   OPEN QUERY BRW_B6 FOR EACH temp6 NO-LOCK.
   ASSIGN
   TOG_ANM = FALSE
   TOG_MTRL = FALSE
   TOG_TEXT = FALSE
   TOG_UPP = FALSE
   TOG_SPANN = FALSE
   TOG_ID = FALSE
   TOG_KOMB = FALSE
   TOG_P3 = FALSE
   RAD_PRIS = 2
   prisvar = 2.
   RUN enable_UI.   
   {FRMSIZE.I}  
   IF Guru.Konstanter:mtrlsekvar[6] = TRUE THEN DO:
      RAD_PRIS:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
      DISABLE RAD_PRIS WITH FRAME {&FRAME-NAME}.
   END.
   FIND kon_val WHERE ROWID(kon_val) = kon_rowid NO-LOCK NO-ERROR.
   FIND FIRST kon_id WHERE kon_id.NUM = kon_val.NUM USE-INDEX NUM
   NO-LOCK NO-ERROR.
   IF AVAILABLE kon_id THEN DO:
      IF kon_val.ID = FALSE THEN TOG_ID:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
      IF kon_id.XKORD = ? THEN TOG_KOMB:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
   END.
   ELSE DO:
      ASSIGN
      TOG_ID:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      TOG_KOMB:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
   END.   
   IF kon_val.ANMARK = "" THEN TOG_ANM:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
   IF kon_val.UPPLAG = ? THEN TOG_UPP:HIDDEN IN FRAME {&FRAME-NAME} = TRUE. 
   TOG_P3:HIDDEN IN FRAME {&FRAME-NAME} = TRUE. 
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
   RUN DYNBRW.P PERSISTENT SET brwproc[1] 
      (INPUT BRW_B2:HANDLE IN FRAME FRAME-A).
   RUN DYNBRW.P PERSISTENT SET brwproc[2] 
      (INPUT BRW_B3:HANDLE IN FRAME FRAME-A).
   RUN DYNBRW.P PERSISTENT SET brwproc[3] 
      (INPUT BRW_B4:HANDLE IN FRAME FRAME-A).
   RUN DYNBRW.P PERSISTENT SET brwproc[4] 
      (INPUT BRW_B5:HANDLE IN FRAME FRAME-A).
   RUN DYNBRW.P PERSISTENT SET brwproc[5] 
      (INPUT BRW_B6:HANDLE IN FRAME FRAME-A).
   RUN brwsetupstop_UI IN brwproc[1] (INPUT 1).
   RUN brwsetupstop_UI IN brwproc[2] (INPUT 1).
   RUN brwsetupstop_UI IN brwproc[3] (INPUT 1).
   RUN brwsetupstop_UI IN brwproc[4] (INPUT 1).
   RUN brwsetupstop_UI IN brwproc[5] (INPUT 1).

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
  DISPLAY TOG_MTRL TOG_ANM RAD_PRIS TOG_UPP TOG_KOMB TOG_ID TOG_TEXT TOG_SPANN 
          TOG_P3 FILL-IN-B1 FILL-IN-B2 FILL-IN-B3 FILL-IN-1 FILL-IN-11 
          FILL-IN-B4 FILL-IN-B5 FILL-IN-B6 FILL-IN-FORD FILL-IN-LINJE 
          FILL-IN-NAT FILL-IN-7 FILL-IN-8 FILL-IN-9 FILL-IN-IDNR1 FILL-IN-IDNR2 
          FILL-IN-IDNR3 FILL-IN-FRI1 FILL-IN-FRI2 FILL-IN-FRI3 
      WITH FRAME FRAME-A IN WINDOW WINDOW-1.
  ENABLE RECT-3 RECT-4 BRW_B2 BRW_B3 BTN_VISA BTN_SKRIV BRW_B4 BRW_B5 BRW_B6 
         TOG_MTRL TOG_ANM RAD_PRIS TOG_UPP TOG_KOMB TOG_ID TOG_TEXT TOG_SPANN 
         TOG_P3 BTN_AVB FILL-IN-B1 FILL-IN-B2 FILL-IN-B3 FILL-IN-1 FILL-IN-11 
         FILL-IN-B4 FILL-IN-B5 FILL-IN-B6 FILL-IN-FORD FILL-IN-LINJE 
         FILL-IN-NAT FILL-IN-7 FILL-IN-8 FILL-IN-9 FILL-IN-IDNR1 FILL-IN-IDNR2 
         FILL-IN-IDNR3 FILL-IN-FRI1 FILL-IN-FRI2 FILL-IN-FRI3 
      WITH FRAME FRAME-A IN WINDOW WINDOW-1.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

