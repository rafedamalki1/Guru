&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
&Scoped-define WINDOW-NAME WINDOW-3



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS WINDOW-3 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 01/28/97 -  3:43 pm

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
DEFINE INPUT PARAMETER varkod AS CHARACTER NO-UNDO.
/* Local Variable Definitions ---                                       */
{ALLDEF.I}
&Scoped-define NEW 
{GLOBVAR2DEL1.I}
/*{EGENBEN.I}*/
&Scoped-define SHARED SHARED
{KONVALTEMP.I}
&Scoped-define NEW NEW
&Scoped-define SHARED SHARED
{SMTRL.I}
{LOP.I}
{KONSTRMTRL.I}
{BBENAMNTEMP.I}
 

DEFINE VARIABLE KalkClasserStart AS HANDLE NO-UNDO.  
DEFINE SHARED VARIABLE kongrkod AS INTEGER NO-UNDO.
DEFINE VARIABLE katvar AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE typkod AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE bestant AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE huvudlev AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO. 
DEFINE SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.  
DEFINE SHARED VARIABLE sok AS LOGICAL NO-UNDO.  
DEFINE SHARED VARIABLE sokant AS LOGICAL NO-UNDO. 
DEFINE SHARED VARIABLE vald_kundlev AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE vald_lev AS CHARACTER NO-UNDO.  
DEFINE SHARED VARIABLE vartpro AS CHARACTER FORMAT "X(3)" NO-UNDO. 

DEFINE VARIABLE konstvalrow AS ROWID NO-UNDO.
/*
DEFINE VARIABLE konstvalrow2 AS ROWID NO-UNDO.
*/
DEFINE VARIABLE antal_raknare AS INTEGER NO-UNDO. 
DEFINE VARIABLE antal_valda AS INTEGER NO-UNDO.
DEFINE VARIABLE aosok AS CHARACTER FORMAT "X(40)" NO-UNDO.
DEFINE VARIABLE arbkodvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE lopnrvar AS INTEGER NO-UNDO.
DEFINE VARIABLE brw AS INTEGER NO-UNDO. 
DEFINE VARIABLE counter AS INTEGER NO-UNDO. 
DEFINE VARIABLE finns AS LOGICAL NO-UNDO EXTENT 10.
DEFINE VARIABLE firstkod AS INTEGER NO-UNDO. 
DEFINE VARIABLE kabelskap AS LOGICAL NO-UNDO. 
DEFINE VARIABLE lev AS CHARACTER NO-UNDO.  
DEFINE VARIABLE mark AS LOGICAL NO-UNDO. 
DEFINE VARIABLE posok AS CHARACTER FORMAT "X(11)" NO-UNDO.
DEFINE VARIABLE satsvar AS LOGICAL NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO. 
DEFINE VARIABLE svar AS LOGICAL.
DEFINE VARIABLE val AS LOGICAL NO-UNDO.
DEFINE VARIABLE laddaproch AS HANDLE NO-UNDO.
DEFINE SHARED VARIABLE kalkkoppapph AS HANDLE NO-UNDO.
DEFINE VARIABLE mtrlhmtapph AS HANDLE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-ANDRA
&Scoped-define BROWSE-NAME BRW_B2

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES konstvaltemp konsttemp loptemp3 spec_mtrl ~
kalkbertemp ptemp3

/* Definitions for BROWSE BRW_B2                                        */
&Scoped-define FIELDS-IN-QUERY-BRW_B2 konstvaltemp.KVALKOD ~
konstvaltemp.BENAMNING 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_B2 
&Scoped-define QUERY-STRING-BRW_B2 FOR EACH konstvaltemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_B2 OPEN QUERY BRW_B2 FOR EACH konstvaltemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_B2 konstvaltemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_B2 konstvaltemp


/* Definitions for BROWSE BRW_B3                                        */
&Scoped-define FIELDS-IN-QUERY-BRW_B3 konstvaltemp.KVALKOD ~
konstvaltemp.BENAMNING 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_B3 
&Scoped-define QUERY-STRING-BRW_B3 FOR EACH konstvaltemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_B3 OPEN QUERY BRW_B3 FOR EACH konstvaltemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_B3 konstvaltemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_B3 konstvaltemp


/* Definitions for BROWSE BRW_B4                                        */
&Scoped-define FIELDS-IN-QUERY-BRW_B4 konstvaltemp.KVALKOD ~
konstvaltemp.BENAMNING 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_B4 
&Scoped-define QUERY-STRING-BRW_B4 FOR EACH konstvaltemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_B4 OPEN QUERY BRW_B4 FOR EACH konstvaltemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_B4 konstvaltemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_B4 konstvaltemp


/* Definitions for BROWSE BRW_B5                                        */
&Scoped-define FIELDS-IN-QUERY-BRW_B5 konstvaltemp.KVALKOD ~
konstvaltemp.BENAMNING 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_B5 
&Scoped-define QUERY-STRING-BRW_B5 FOR EACH konstvaltemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_B5 OPEN QUERY BRW_B5 FOR EACH konstvaltemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_B5 konstvaltemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_B5 konstvaltemp


/* Definitions for BROWSE BRW_B6                                        */
&Scoped-define FIELDS-IN-QUERY-BRW_B6 konstvaltemp.KVALKOD ~
konstvaltemp.BENAMNING 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_B6 
&Scoped-define QUERY-STRING-BRW_B6 FOR EACH konstvaltemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_B6 OPEN QUERY BRW_B6 FOR EACH konstvaltemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_B6 konstvaltemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_B6 konstvaltemp


/* Definitions for BROWSE BRW_KON                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_KON konsttemp.KTYPKOD ~
konsttemp.BENAMNING 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_KON 
&Scoped-define QUERY-STRING-BRW_KON FOR EACH konsttemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_KON OPEN QUERY BRW_KON FOR EACH konsttemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_KON konsttemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_KON konsttemp


/* Definitions for BROWSE BRW_LOP3                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_LOP3 loptemp3.LOPNR loptemp3.ENHET ~
loptemp3.BENAMNING loptemp3.F2 loptemp3.F3 loptemp3.F4 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_LOP3 
&Scoped-define QUERY-STRING-BRW_LOP3 FOR EACH loptemp3 NO-LOCK
&Scoped-define OPEN-QUERY-BRW_LOP3 OPEN QUERY BRW_LOP3 FOR EACH loptemp3 NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_LOP3 loptemp3
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_LOP3 loptemp3


/* Definitions for BROWSE BRW_MTRL                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_MTRL spec_mtrl.ARBKOD spec_mtrl.LOPNR ~
spec_mtrl.ANTAL spec_mtrl.Enhet spec_mtrl.Benamning 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_MTRL spec_mtrl.ANTAL 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_MTRL spec_mtrl
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_MTRL spec_mtrl
&Scoped-define QUERY-STRING-BRW_MTRL FOR EACH spec_mtrl NO-LOCK
&Scoped-define OPEN-QUERY-BRW_MTRL OPEN QUERY BRW_MTRL FOR EACH spec_mtrl NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_MTRL spec_mtrl
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_MTRL spec_mtrl


/* Definitions for BROWSE BRW_MTRLBER                                   */
&Scoped-define FIELDS-IN-QUERY-BRW_MTRLBER kalkbertemp.ARBKOD ~
kalkbertemp.LOPNR kalkbertemp.BENAMNING kalkbertemp.ENHET kalkbertemp.ANTAL ~
kalkbertemp.F1 kalkbertemp.F2 kalkbertemp.F3 kalkbertemp.F4 kalkbertemp.F5 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_MTRLBER kalkbertemp.ANTAL 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_MTRLBER kalkbertemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_MTRLBER kalkbertemp
&Scoped-define QUERY-STRING-BRW_MTRLBER FOR EACH kalkbertemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_MTRLBER OPEN QUERY BRW_MTRLBER FOR EACH kalkbertemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_MTRLBER kalkbertemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_MTRLBER kalkbertemp


/* Definitions for BROWSE BRW_P3                                        */
&Scoped-define FIELDS-IN-QUERY-BRW_P3 ptemp3.ARBKOD ptemp3.BENAMNING 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_P3 
&Scoped-define QUERY-STRING-BRW_P3 FOR EACH ptemp3 NO-LOCK
&Scoped-define OPEN-QUERY-BRW_P3 OPEN QUERY BRW_P3 FOR EACH ptemp3 NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_P3 ptemp3
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_P3 ptemp3


/* Definitions for FRAME FRAME-ANDRA                                    */

/* Definitions for FRAME FRAME-C                                        */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BRW_MTRLBER BTN_NY BTN_ANDE BTN_ANDA ~
BTN_BORT 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR WINDOW-3 AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_ANDA 
     LABEL "Ändra alla" 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_ANDE 
     LABEL "Ändra" 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_BORT 
     LABEL "Ta bort" 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_NY 
     LABEL "Ny" 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_AVB AUTO-END-KEY DEFAULT 
     LABEL "Avsluta" 
     SIZE 14 BY 1
     BGCOLOR 8 .

DEFINE BUTTON btn_back 
     IMAGE-UP FILE "BILDER\prev-u":U
     LABEL "":L 
     SIZE 4 BY 1.21 TOOLTIP "Tar bort markerade poster".

DEFINE BUTTON BTN_BORT2 
     LABEL "Ta Bort" 
     SIZE 12 BY 1.

DEFINE BUTTON btn_over 
     IMAGE-UP FILE "BILDER\next-u":U
     LABEL "":L 
     SIZE 4 BY 1.21 TOOLTIP "Lägger till markerade poster".

DEFINE BUTTON BTN_RENSA 
     LABEL "Rensa":L 
     SIZE 12 BY 1.

DEFINE BUTTON FBTN_ANDRA 
     LABEL "Ändra" 
     SIZE 14 BY 1.

DEFINE BUTTON FBTN_ANDRA-2 
     LABEL "Åter" 
     SIZE 14 BY 1.

DEFINE BUTTON FBTN_HAMTA 
     LABEL "Hämta" 
     SIZE 14 BY 1.

DEFINE BUTTON FBTN_SKAPA 
     LABEL "Koppla" 
     SIZE 12 BY 1.

DEFINE VARIABLE CMB_VAL AS CHARACTER FORMAT "X(30)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "0" 
     DROP-DOWN-LIST
     SIZE 28.5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-B1 AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 36.13 BY 1
     BGCOLOR 7  NO-UNDO.

DEFINE VARIABLE FILL-IN-B2 AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 36.13 BY 1
     BGCOLOR 7  NO-UNDO.

DEFINE VARIABLE FILL-IN-B3 AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 36.13 BY 1
     BGCOLOR 7  NO-UNDO.

DEFINE VARIABLE FILL-IN-B4 AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 36.13 BY 1
     BGCOLOR 7  NO-UNDO.

DEFINE VARIABLE FILL-IN-B5 AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 36.13 BY 1
     BGCOLOR 7  NO-UNDO.

DEFINE VARIABLE FILL-IN-B6 AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 36.13 BY 1
     BGCOLOR 7  NO-UNDO.

DEFINE VARIABLE RAD_VAL AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Arbetskod", 1,
"Löpnr", 2
     SIZE 27.75 BY .67 NO-UNDO.

DEFINE VARIABLE TOG_KVAR AS LOGICAL INITIAL no 
     LABEL "Rensa ej bilden" 
     VIEW-AS TOGGLE-BOX
     SIZE 26 BY .67 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_B2 FOR 
      konstvaltemp SCROLLING.

DEFINE QUERY BRW_B3 FOR 
      konstvaltemp SCROLLING.

DEFINE QUERY BRW_B4 FOR 
      konstvaltemp SCROLLING.

DEFINE QUERY BRW_B5 FOR 
      konstvaltemp SCROLLING.

DEFINE QUERY BRW_B6 FOR 
      konstvaltemp SCROLLING.

DEFINE QUERY BRW_KON FOR 
      konsttemp SCROLLING.

DEFINE QUERY BRW_LOP3 FOR 
      loptemp3 SCROLLING.

DEFINE QUERY BRW_MTRL FOR 
      spec_mtrl SCROLLING.

DEFINE QUERY BRW_MTRLBER FOR 
      kalkbertemp SCROLLING.

DEFINE QUERY BRW_P3 FOR 
      ptemp3 SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_B2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_B2 WINDOW-3 _STRUCTURED
  QUERY BRW_B2 NO-LOCK DISPLAY
      konstvaltemp.KVALKOD COLUMN-LABEL "Kod" FORMAT "X(9)":U WIDTH 10
      konstvaltemp.BENAMNING FORMAT "X(256)":U WIDTH 28
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING MULTIPLE SIZE 36.13 BY 5.42.

DEFINE BROWSE BRW_B3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_B3 WINDOW-3 _STRUCTURED
  QUERY BRW_B3 NO-LOCK DISPLAY
      konstvaltemp.KVALKOD COLUMN-LABEL "Kod" FORMAT "X(9)":U WIDTH 10
      konstvaltemp.BENAMNING FORMAT "X(256)":U WIDTH 28
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING MULTIPLE SIZE 36.13 BY 5.42.

DEFINE BROWSE BRW_B4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_B4 WINDOW-3 _STRUCTURED
  QUERY BRW_B4 NO-LOCK DISPLAY
      konstvaltemp.KVALKOD COLUMN-LABEL "Kod" FORMAT "X(9)":U WIDTH 10
      konstvaltemp.BENAMNING FORMAT "X(256)":U WIDTH 28
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING MULTIPLE SIZE 36.13 BY 5.42.

DEFINE BROWSE BRW_B5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_B5 WINDOW-3 _STRUCTURED
  QUERY BRW_B5 NO-LOCK DISPLAY
      konstvaltemp.KVALKOD COLUMN-LABEL "Kod" FORMAT "X(9)":U WIDTH 10
      konstvaltemp.BENAMNING FORMAT "X(256)":U WIDTH 28
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING MULTIPLE SIZE 36.13 BY 5.42.

DEFINE BROWSE BRW_B6
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_B6 WINDOW-3 _STRUCTURED
  QUERY BRW_B6 NO-LOCK DISPLAY
      konstvaltemp.KVALKOD COLUMN-LABEL "Kod" FORMAT "X(9)":U WIDTH 10
      konstvaltemp.BENAMNING FORMAT "X(256)":U WIDTH 28
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING MULTIPLE SIZE 36.13 BY 5.42.

DEFINE BROWSE BRW_KON
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_KON WINDOW-3 _STRUCTURED
  QUERY BRW_KON NO-LOCK DISPLAY
      konsttemp.KTYPKOD COLUMN-LABEL "Kod" FORMAT "X(9)":U WIDTH 10
      konsttemp.BENAMNING FORMAT "X(256)":U WIDTH 28
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SIZE 36.13 BY 5.42.

DEFINE BROWSE BRW_LOP3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_LOP3 WINDOW-3 _STRUCTURED
  QUERY BRW_LOP3 NO-LOCK DISPLAY
      loptemp3.LOPNR COLUMN-LABEL "Löp!nr" FORMAT ">>>":U
      loptemp3.ENHET FORMAT "X(5)":U
      loptemp3.BENAMNING FORMAT "X(256)":U WIDTH 30
      loptemp3.F2 COLUMN-LABEL "Montörs!timmar" FORMAT ">>>9.99":U
      loptemp3.F3 COLUMN-LABEL "Ledn.b.!maskin" FORMAT ">>>9.99":U
      loptemp3.F4 COLUMN-LABEL "Kab.sch.!maskin" FORMAT ">>>9.99":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING MULTIPLE SIZE 43.5 BY 12
         TITLE "Löpnr kopplade till arbetskoden".

DEFINE BROWSE BRW_MTRL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_MTRL WINDOW-3 _STRUCTURED
  QUERY BRW_MTRL NO-LOCK DISPLAY
      spec_mtrl.ARBKOD COLUMN-LABEL "Arbets!kod" FORMAT "X(5)":U
      spec_mtrl.LOPNR COLUMN-LABEL "Löp!nr" FORMAT ">>>":U
      spec_mtrl.ANTAL FORMAT ">>>>9.999":U WIDTH 10
      spec_mtrl.Enhet FORMAT "x(5)":U
      spec_mtrl.Benamning FORMAT "x(256)":U WIDTH 30
  ENABLE
      spec_mtrl.ANTAL
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING MULTIPLE SIZE 61.13 BY 11.08
         TITLE "Valda koder".

DEFINE BROWSE BRW_MTRLBER
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_MTRLBER WINDOW-3 _STRUCTURED
  QUERY BRW_MTRLBER NO-LOCK DISPLAY
      kalkbertemp.ARBKOD COLUMN-LABEL "ArbetS!kod" FORMAT "X(5)":U
      kalkbertemp.LOPNR COLUMN-LABEL "Löp!nr" FORMAT ">>>":U
      kalkbertemp.BENAMNING FORMAT "X(256)":U WIDTH 26
      kalkbertemp.ENHET FORMAT "X(5)":U
      kalkbertemp.ANTAL FORMAT ">>>>9.999":U
      kalkbertemp.F1 FORMAT "X(10)":U
      kalkbertemp.F2 FORMAT "X(10)":U
      kalkbertemp.F3 FORMAT "X(10)":U
      kalkbertemp.F4 FORMAT "X(10)":U
      kalkbertemp.F5 FORMAT "X(10)":U
  ENABLE
      kalkbertemp.ANTAL
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING MULTIPLE SIZE 109 BY 14.5
         TITLE "Kopplade P3 koder".

DEFINE BROWSE BRW_P3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_P3 WINDOW-3 _STRUCTURED
  QUERY BRW_P3 NO-LOCK DISPLAY
      ptemp3.ARBKOD COLUMN-LABEL "Arbets!kod" FORMAT "X(5)":U
      ptemp3.BENAMNING FORMAT "X(256)":U WIDTH 35
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING MULTIPLE SIZE 43.5 BY 12
         TITLE "Arbetskoder typ 3".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-C
     CMB_VAL AT ROW 1.17 COL 2.38 NO-LABEL
     BRW_KON AT ROW 3.58 COL 1.5
     BRW_B2 AT ROW 3.58 COL 38.13
     BRW_B3 AT ROW 3.58 COL 74.75
     FBTN_HAMTA AT ROW 8 COL 111.13
     FBTN_ANDRA AT ROW 9.08 COL 111.13
     FBTN_ANDRA-2 AT ROW 9.08 COL 111.13
     BRW_B4 AT ROW 10.29 COL 1.5
     BRW_B5 AT ROW 10.29 COL 38.13
     BRW_B6 AT ROW 10.29 COL 74.75
     RAD_VAL AT ROW 16.08 COL 4.5 NO-LABEL
     TOG_KVAR AT ROW 16.08 COL 60
     BRW_P3 AT ROW 17 COL 1.5
     BRW_LOP3 AT ROW 17 COL 1.5
     BRW_MTRL AT ROW 17 COL 49.63
     btn_over AT ROW 20.58 COL 45.25
     btn_back AT ROW 23.13 COL 45.25
     FBTN_SKAPA AT ROW 28.21 COL 61.38
     BTN_BORT2 AT ROW 28.21 COL 76.5
     BTN_RENSA AT ROW 28.21 COL 91.13
     BTN_AVB AT ROW 28.21 COL 111.13
     FILL-IN-B1 AT ROW 2.58 COL 1.5 NO-LABEL
     FILL-IN-B2 AT ROW 2.58 COL 38.13 NO-LABEL
     FILL-IN-B3 AT ROW 2.58 COL 74.75 NO-LABEL
     FILL-IN-B4 AT ROW 9.29 COL 1.5 NO-LABEL
     FILL-IN-B5 AT ROW 9.29 COL 38.13 NO-LABEL
     FILL-IN-B6 AT ROW 9.29 COL 74.75 NO-LABEL
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 124.5 BY 28.33
         CANCEL-BUTTON BTN_AVB.

DEFINE FRAME FRAME-ANDRA
     BRW_MTRLBER AT ROW 1.25 COL 1.5
     BTN_NY AT ROW 15.88 COL 23.88
     BTN_ANDE AT ROW 15.88 COL 41.38
     BTN_ANDA AT ROW 15.88 COL 58.88
     BTN_BORT AT ROW 15.88 COL 76.5
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 110.25 BY 16.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: 
   Temp-Tables and Buffers:
      TABLE: kalkbertemp T "?" NO-UNDO temp-db kalkbertemp
      TABLE: konsttemp T "?" NO-UNDO temp-db konsttemp
      TABLE: konstvaltemp T "?" NO-UNDO temp-db konstvaltemp
      TABLE: loptemp3 T "?" NO-UNDO temp-db loptemp3
      TABLE: ptemp3 T "?" NO-UNDO temp-db ptemp3
      TABLE: ? T "?" NO-UNDO temp-db spec_mtrl
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW WINDOW-3 ASSIGN
         HIDDEN             = YES
         TITLE              = "Administration av P3-koder kopplade till konstruktioner"
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
/* SETTINGS FOR WINDOW WINDOW-3
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* REPARENT FRAME */
ASSIGN FRAME FRAME-ANDRA:FRAME = FRAME FRAME-C:HANDLE.

/* SETTINGS FOR FRAME FRAME-ANDRA
                                                                        */
/* BROWSE-TAB BRW_MTRLBER 1 FRAME-ANDRA */
ASSIGN 
       FRAME FRAME-ANDRA:HIDDEN           = TRUE.

ASSIGN 
       BRW_MTRLBER:MAX-DATA-GUESS IN FRAME FRAME-ANDRA         = 40000
       BRW_MTRLBER:COLUMN-RESIZABLE IN FRAME FRAME-ANDRA       = TRUE.

/* SETTINGS FOR FRAME FRAME-C
                                                                        */

DEFINE VARIABLE XXTABVALXX AS LOGICAL NO-UNDO.

ASSIGN XXTABVALXX = FRAME FRAME-ANDRA:MOVE-BEFORE-TAB-ITEM (CMB_VAL:HANDLE IN FRAME FRAME-C)
/* END-ASSIGN-TABS */.

/* BROWSE-TAB BRW_KON CMB_VAL FRAME-C */
/* BROWSE-TAB BRW_B2 BRW_KON FRAME-C */
/* BROWSE-TAB BRW_B3 BRW_B2 FRAME-C */
/* BROWSE-TAB BRW_B4 FBTN_ANDRA-2 FRAME-C */
/* BROWSE-TAB BRW_B5 BRW_B4 FRAME-C */
/* BROWSE-TAB BRW_B6 BRW_B5 FRAME-C */
/* BROWSE-TAB BRW_P3 TOG_KVAR FRAME-C */
/* BROWSE-TAB BRW_LOP3 BRW_P3 FRAME-C */
/* BROWSE-TAB BRW_MTRL BRW_LOP3 FRAME-C */
ASSIGN 
       BRW_B2:HIDDEN  IN FRAME FRAME-C                = TRUE
       BRW_B2:COLUMN-RESIZABLE IN FRAME FRAME-C       = TRUE.

ASSIGN 
       BRW_B3:HIDDEN  IN FRAME FRAME-C                = TRUE
       BRW_B3:COLUMN-RESIZABLE IN FRAME FRAME-C       = TRUE.

ASSIGN 
       BRW_B4:HIDDEN  IN FRAME FRAME-C                = TRUE
       BRW_B4:COLUMN-RESIZABLE IN FRAME FRAME-C       = TRUE.

ASSIGN 
       BRW_B5:HIDDEN  IN FRAME FRAME-C                = TRUE
       BRW_B5:COLUMN-RESIZABLE IN FRAME FRAME-C       = TRUE.

ASSIGN 
       BRW_B6:HIDDEN  IN FRAME FRAME-C                = TRUE
       BRW_B6:COLUMN-RESIZABLE IN FRAME FRAME-C       = TRUE.

ASSIGN 
       BRW_KON:HIDDEN  IN FRAME FRAME-C                = TRUE
       BRW_KON:COLUMN-RESIZABLE IN FRAME FRAME-C       = TRUE.

ASSIGN 
       BRW_LOP3:HIDDEN  IN FRAME FRAME-C                = TRUE
       BRW_LOP3:MAX-DATA-GUESS IN FRAME FRAME-C         = 1000
       BRW_LOP3:COLUMN-RESIZABLE IN FRAME FRAME-C       = TRUE.

ASSIGN 
       BRW_MTRL:MAX-DATA-GUESS IN FRAME FRAME-C         = 50000
       BRW_MTRL:COLUMN-RESIZABLE IN FRAME FRAME-C       = TRUE.

ASSIGN 
       BRW_P3:HIDDEN  IN FRAME FRAME-C                = TRUE
       BRW_P3:MAX-DATA-GUESS IN FRAME FRAME-C         = 1000
       BRW_P3:ALLOW-COLUMN-SEARCHING IN FRAME FRAME-C = TRUE
       BRW_P3:COLUMN-RESIZABLE IN FRAME FRAME-C       = TRUE.

/* SETTINGS FOR COMBO-BOX CMB_VAL IN FRAME FRAME-C
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FILL-IN-B1 IN FRAME FRAME-C
   ALIGN-L                                                              */
ASSIGN 
       FILL-IN-B1:HIDDEN IN FRAME FRAME-C           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-B2 IN FRAME FRAME-C
   ALIGN-L                                                              */
ASSIGN 
       FILL-IN-B2:HIDDEN IN FRAME FRAME-C           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-B3 IN FRAME FRAME-C
   ALIGN-L                                                              */
ASSIGN 
       FILL-IN-B3:HIDDEN IN FRAME FRAME-C           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-B4 IN FRAME FRAME-C
   ALIGN-L                                                              */
ASSIGN 
       FILL-IN-B4:HIDDEN IN FRAME FRAME-C           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-B5 IN FRAME FRAME-C
   ALIGN-L                                                              */
ASSIGN 
       FILL-IN-B5:HIDDEN IN FRAME FRAME-C           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-B6 IN FRAME FRAME-C
   ALIGN-L                                                              */
ASSIGN 
       FILL-IN-B6:HIDDEN IN FRAME FRAME-C           = TRUE.

/* SETTINGS FOR TOGGLE-BOX TOG_KVAR IN FRAME FRAME-C
   NO-ENABLE                                                            */
ASSIGN 
       TOG_KVAR:HIDDEN IN FRAME FRAME-C           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-3)
THEN WINDOW-3:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_B2
/* Query rebuild information for BROWSE BRW_B2
     _TblList          = "Temp-Tables.konstvaltemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.konstvaltemp.KVALKOD
"konstvaltemp.KVALKOD" "Kod" "X(9)" "character" ? ? ? ? ? ? no ? no no "10" yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.konstvaltemp.BENAMNING
"konstvaltemp.BENAMNING" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "28" yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_B2 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_B3
/* Query rebuild information for BROWSE BRW_B3
     _TblList          = "Temp-Tables.konstvaltemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.konstvaltemp.KVALKOD
"konstvaltemp.KVALKOD" "Kod" "X(9)" "character" ? ? ? ? ? ? no ? no no "10" yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.konstvaltemp.BENAMNING
"konstvaltemp.BENAMNING" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "28" yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_B3 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_B4
/* Query rebuild information for BROWSE BRW_B4
     _TblList          = "Temp-Tables.konstvaltemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.konstvaltemp.KVALKOD
"konstvaltemp.KVALKOD" "Kod" "X(9)" "character" ? ? ? ? ? ? no ? no no "10" yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.konstvaltemp.BENAMNING
"konstvaltemp.BENAMNING" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "28" yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_B4 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_B5
/* Query rebuild information for BROWSE BRW_B5
     _TblList          = "Temp-Tables.konstvaltemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.konstvaltemp.KVALKOD
"konstvaltemp.KVALKOD" "Kod" "X(9)" "character" ? ? ? ? ? ? no ? no no "10" yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.konstvaltemp.BENAMNING
"konstvaltemp.BENAMNING" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "28" yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_B5 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_B6
/* Query rebuild information for BROWSE BRW_B6
     _TblList          = "Temp-Tables.konstvaltemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.konstvaltemp.KVALKOD
"konstvaltemp.KVALKOD" "Kod" "X(9)" "character" ? ? ? ? ? ? no ? no no "10" yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.konstvaltemp.BENAMNING
"konstvaltemp.BENAMNING" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "28" yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_B6 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_KON
/* Query rebuild information for BROWSE BRW_KON
     _TblList          = "Temp-Tables.konsttemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.konsttemp.KTYPKOD
"konsttemp.KTYPKOD" "Kod" "X(9)" "character" ? ? ? ? ? ? no ? no no "10" yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.konsttemp.BENAMNING
"konsttemp.BENAMNING" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "28" yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_KON */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_LOP3
/* Query rebuild information for BROWSE BRW_LOP3
     _TblList          = "Temp-Tables.loptemp3"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.loptemp3.LOPNR
"loptemp3.LOPNR" "Löp!nr" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.loptemp3.ENHET
"loptemp3.ENHET" ? "X(5)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.loptemp3.BENAMNING
"loptemp3.BENAMNING" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "30" yes no no "U" "" ""
     _FldNameList[4]   > Temp-Tables.loptemp3.F2
"loptemp3.F2" "Montörs!timmar" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[5]   > Temp-Tables.loptemp3.F3
"loptemp3.F3" "Ledn.b.!maskin" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[6]   > Temp-Tables.loptemp3.F4
"loptemp3.F4" "Kab.sch.!maskin" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_LOP3 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_MTRL
/* Query rebuild information for BROWSE BRW_MTRL
     _TblList          = "Temp-Tables.spec_mtrl"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.spec_mtrl.ARBKOD
"spec_mtrl.ARBKOD" "Arbets!kod" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.spec_mtrl.LOPNR
"spec_mtrl.LOPNR" "Löp!nr" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.spec_mtrl.ANTAL
"spec_mtrl.ANTAL" ? ">>>>9.999" "integer" ? ? ? ? ? ? yes ? no no "10" yes no no "U" "" ""
     _FldNameList[4]   = Temp-Tables.spec_mtrl.Enhet
     _FldNameList[5]   > Temp-Tables.spec_mtrl.Benamning
"spec_mtrl.Benamning" ? "x(256)" "character" ? ? ? ? ? ? no ? no no "30" yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_MTRL */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_MTRLBER
/* Query rebuild information for BROWSE BRW_MTRLBER
     _TblList          = "Temp-Tables.kalkbertemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.kalkbertemp.ARBKOD
"kalkbertemp.ARBKOD" "ArbetS!kod" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.kalkbertemp.LOPNR
"kalkbertemp.LOPNR" "Löp!nr" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.kalkbertemp.BENAMNING
"kalkbertemp.BENAMNING" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "26" yes no no "U" "" ""
     _FldNameList[4]   > Temp-Tables.kalkbertemp.ENHET
"kalkbertemp.ENHET" ? "X(5)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[5]   > Temp-Tables.kalkbertemp.ANTAL
"kalkbertemp.ANTAL" ? ">>>>9.999" "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[6]   > Temp-Tables.kalkbertemp.F1
"kalkbertemp.F1" ? "X(10)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[7]   > Temp-Tables.kalkbertemp.F2
"kalkbertemp.F2" ? "X(10)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[8]   > Temp-Tables.kalkbertemp.F3
"kalkbertemp.F3" ? "X(10)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[9]   > Temp-Tables.kalkbertemp.F4
"kalkbertemp.F4" ? "X(10)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[10]   > Temp-Tables.kalkbertemp.F5
"kalkbertemp.F5" ? "X(10)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_MTRLBER */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_P3
/* Query rebuild information for BROWSE BRW_P3
     _TblList          = "Temp-Tables.ptemp3"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.ptemp3.ARBKOD
"ptemp3.ARBKOD" "Arbets!kod" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.ptemp3.BENAMNING
"ptemp3.BENAMNING" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "35" yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_P3 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-ANDRA
/* Query rebuild information for FRAME FRAME-ANDRA
     _Query            is NOT OPENED
*/  /* FRAME FRAME-ANDRA */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME BRW_B2
&Scoped-define FRAME-NAME FRAME-C
&Scoped-define SELF-NAME BRW_B2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_B2 WINDOW-3
ON VALUE-CHANGED OF BRW_B2 IN FRAME FRAME-C
DO: 
   RUN vc_UI (INPUT 2).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_B3
&Scoped-define SELF-NAME BRW_B3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_B3 WINDOW-3
ON VALUE-CHANGED OF BRW_B3 IN FRAME FRAME-C
DO: 
   RUN vc_UI (INPUT 3).  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_B4
&Scoped-define SELF-NAME BRW_B4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_B4 WINDOW-3
ON VALUE-CHANGED OF BRW_B4 IN FRAME FRAME-C
DO: 
   RUN vc_UI (INPUT 4).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_B5
&Scoped-define SELF-NAME BRW_B5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_B5 WINDOW-3
ON VALUE-CHANGED OF BRW_B5 IN FRAME FRAME-C
DO: 
   RUN vc_UI (INPUT 5).
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_B6
&Scoped-define SELF-NAME BRW_B6
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_B6 WINDOW-3
ON VALUE-CHANGED OF BRW_B6 IN FRAME FRAME-C
DO: 
   RUN vc_UI (INPUT 6).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_KON
&Scoped-define SELF-NAME BRW_KON
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_KON WINDOW-3
ON VALUE-CHANGED OF BRW_KON IN FRAME FRAME-C
DO: 
   {muswait.i}
   ASSIGN
   status-ok = BRW_KON:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
   /*IF typkod NE konsttemp.KTYPKOD THEN konstvalrow2 = ?. */
   ASSIGN typkod = konsttemp.KTYPKOD.
   BRW_MTRLBER:TITLE IN FRAME FRAME-ANDRA = "Ändring av P2/P3-koder kopplade till konstruktion " + typkod.
   mark = FALSE.    
   RUN bytkonst_UI.   
   RUN markerade_UI (INPUT 0).
   
   RUN bortinaktiv_UI.
/*    APPLY "CHOOSE" TO BTN_HAMTA. */
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_LOP3
&Scoped-define SELF-NAME BRW_LOP3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_LOP3 WINDOW-3
ON MOUSE-SELECT-DBLCLICK OF BRW_LOP3 IN FRAME FRAME-C /* Löpnr kopplade till arbetskoden */
DO:
    APPLY "CHOOSE" TO BTN_OVER.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_LOP3 WINDOW-3
ON VALUE-CHANGED OF BRW_LOP3 IN FRAME FRAME-C /* Löpnr kopplade till arbetskoden */
DO:
   ASSIGN
   status-ok = BRW_LOP3:SELECT-FOCUSED-ROW() NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_MTRL
&Scoped-define SELF-NAME BRW_MTRL
&Scoped-define SELF-NAME spec_mtrl.ANTAL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL spec_mtrl.ANTAL BRW_MTRL _BROWSE-COLUMN WINDOW-3
ON ENTRY OF spec_mtrl.ANTAL IN BROWSE BRW_MTRL /* antal */
DO:
   IF AVAILABLE spec_mtrl THEN DO:
      status-ok = BRW_MTRL:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
      DISPLAY spec_mtrl.ANTAL WITH BROWSE BRW_MTRL.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL spec_mtrl.ANTAL BRW_MTRL _BROWSE-COLUMN WINDOW-3
ON LEAVE OF spec_mtrl.ANTAL IN BROWSE BRW_MTRL /* antal */
DO:
   IF AVAILABLE spec_mtrl THEN DO:
      status-ok = BRW_MTRL:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
      spec_mtrl.ANTAL = INPUT BROWSE BRW_MTRL spec_mtrl.ANTAL.
      DISPLAY spec_mtrl.ANTAL WITH BROWSE BRW_MTRL.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_MTRLBER
&Scoped-define FRAME-NAME FRAME-ANDRA
&Scoped-define SELF-NAME BRW_MTRLBER
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_MTRLBER WINDOW-3
ON MOUSE-SELECT-DBLCLICK OF BRW_MTRLBER IN FRAME FRAME-ANDRA /* Kopplade P3 koder */
DO: 
   
   &Scoped-define FRAME-NAME FRAME-C
   {muswait.i} 
   
   RUN BERBENU.W (INPUT typkod).
   &Scoped-define FRAME-NAME FRAME-C
   
   {musarrow.i}
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME kalkbertemp.ANTAL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL kalkbertemp.ANTAL BRW_MTRLBER _BROWSE-COLUMN WINDOW-3
ON ENTRY OF kalkbertemp.ANTAL IN BROWSE BRW_MTRLBER /* Antal */
DO:   
   IF AVAILABLE kalkbertemp THEN DO:
      status-ok = BRW_MTRLBER:SELECT-FOCUSED-ROW() IN FRAME FRAME-ANDRA NO-ERROR.
      DISPLAY kalkbertemp.ANTAL WITH BROWSE BRW_MTRLBER.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL kalkbertemp.ANTAL BRW_MTRLBER _BROWSE-COLUMN WINDOW-3
ON LEAVE OF kalkbertemp.ANTAL IN BROWSE BRW_MTRLBER /* Antal */
DO:
   IF AVAILABLE kalkbertemp THEN DO:
      status-ok = BRW_MTRLBER:SELECT-FOCUSED-ROW() IN FRAME  FRAME-ANDRA NO-ERROR.
      kalkbertemp.ANTAL = INPUT BROWSE BRW_MTRLBER kalkbertemp.ANTAL.
      DISPLAY kalkbertemp.ANTAL WITH BROWSE BRW_MTRLBER.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_P3
&Scoped-define FRAME-NAME FRAME-C
&Scoped-define SELF-NAME BRW_P3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_P3 WINDOW-3
ON MOUSE-SELECT-DBLCLICK OF BRW_P3 IN FRAME FRAME-C /* Arbetskoder typ 3 */
DO:
    APPLY "CHOOSE" TO BTN_OVER.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_P3 WINDOW-3
ON VALUE-CHANGED OF BRW_P3 IN FRAME FRAME-C /* Arbetskoder typ 3 */
DO:
   ASSIGN
   status-ok = BRW_P3:SELECT-FOCUSED-ROW() NO-ERROR.
   FIND FIRST loptemp3 WHERE loptemp3.ARBKOD = ptemp3.ARBKOD NO-ERROR.
   IF NOT AVAILABLE loptemp3 THEN DO:
      IF varforetypchar[48] NE "" THEN DO:
         RUN Loptempn3_UI IN KalkClasserStart (INPUT 3, OUTPUT TABLE loptemp3 APPEND).
      END.
      ELSE RUN laddaenlop3_UI IN kalkkoppapph (INPUT ptemp3.KATAR,INPUT ptemp3.ARBKOD, OUTPUT TABLE loptemp3 APPEND).
   END.
   BRW_LOP3:TITLE = "Löpnr kopplade till " + ptemp3.ARBKOD.
   RUN setcolsortvar_UI IN brwproc[7] (INPUT " WHERE loptemp3.ARBKOD = '" + ptemp3.ARBKOD + "'").
   RUN openbdynspec_UI IN brwproc[7].   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-ANDRA
&Scoped-define SELF-NAME BTN_ANDA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_ANDA WINDOW-3
ON CHOOSE OF BTN_ANDA IN FRAME FRAME-ANDRA /* Ändra alla */
DO:
   antal_valda = BRW_MTRLBER:NUM-SELECTED-ROWS.
   IF antal_valda > 0 THEN DO:
      FIND FIRST spec_mtrl NO-LOCK NO-ERROR.
      IF AVAILABLE spec_mtrl THEN DO:
         ASSIGN
         antal_valda = BRW_MTRLBER:NUM-SELECTED-ROWS
         antal_raknare = 1.
         IF antal_valda > 1 THEN DO:
            MESSAGE "Vid ändring av alla kan endast en kod åtgången markeras."
            VIEW-AS ALERT-BOX TITLE "Meddelande".
            status-ok = BRW_MTRLBER:DESELECT-ROWS().
         END.
         ELSE DO:   
            DO WHILE antal_raknare LE antal_valda:
               ASSIGN                                   
               status-ok = BRW_MTRLBER:FETCH-SELECTED-ROW(antal_raknare).
               IF AVAILABLE kalkbertemp THEN DO:
                  MESSAGE "Vill du ersätta alla - " + kalkbertemp.ARBKOD + " " + STRING(kalkbertemp.LOPNR,Guru.Konstanter:varforetypchar[6]) VIEW-AS ALERT-BOX
                  QUESTION BUTTONS YES-NO TITLE "Meddelande" UPDATE svar AS LOGICAL.         
                  IF svar THEN DO:
                     RUN andra_UI IN kalkkoppapph (INPUT kalkbertemp.KTYPKOD,INPUT kalkbertemp.KALKROW,INPUT kalkbertemp.ARBKOD,
                                                   INPUT kalkbertemp.LOPNR,INPUT TABLE spec_mtrl,
                                                   INPUT-OUTPUT TABLE kalkbertemp).
                     
                  END.
               END.
               antal_raknare = antal_raknare + 1.
            END. 
            RUN setcolsortvar_UI IN brwproc[10] (INPUT " WHERE KTYPKOD = '" + typkod + "'").  
            RUN openbdynspec_UI IN brwproc[10].  
            
         END.
      END.
      ELSE DO:
         MESSAGE "Du har inte valt någon kod som ändringen ska gälla för."
         VIEW-AS ALERT-BOX TITLE "Meddelande".
      END.      
   END.
   ELSE DO:
      MESSAGE "Du har inte valt någon kod som ändringen ska gälla för."
      VIEW-AS ALERT-BOX TITLE "Meddelande".
   END.      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_ANDE
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_ANDE WINDOW-3
ON CHOOSE OF BTN_ANDE IN FRAME FRAME-ANDRA /* Ändra */
DO: 
   antal_valda = BRW_MTRLBER:NUM-SELECTED-ROWS.
   IF antal_valda > 0 THEN DO:
      FIND FIRST spec_mtrl NO-LOCK NO-ERROR.
      IF AVAILABLE spec_mtrl THEN DO:
         ASSIGN
         antal_valda = BRW_MTRLBER:NUM-SELECTED-ROWS
         antal_raknare = 1.
         DO WHILE antal_raknare LE antal_valda:
            ASSIGN                                   
            status-ok = BRW_MTRLBER:FETCH-SELECTED-ROW(antal_raknare).
            IF AVAILABLE kalkbertemp THEN DO:
               MESSAGE "Vill du ersätta kod - " + kalkbertemp.ARBKOD + " " + STRING(kalkbertemp.LOPNR,Guru.Konstanter:varforetypchar[6]) VIEW-AS ALERT-BOX
               QUESTION BUTTONS YES-NO TITLE "Meddelande" UPDATE svar AS LOGICAL.         
               IF svar THEN DO:
                  RUN enstaka_UI IN kalkkoppapph (INPUT kalkbertemp.KTYPKOD,INPUT kalkbertemp.KALKROW,
                                                  INPUT kalkbertemp.F1,INPUT kalkbertemp.F2,INPUT kalkbertemp.F3,INPUT kalkbertemp.F4,
                                                  INPUT kalkbertemp.F5,INPUT TABLE spec_mtrl,
                                                  INPUT-OUTPUT TABLE kalkbertemp).
                  
                 
               END.
            END.
            antal_raknare = antal_raknare + 1.   
         END. 
         RUN setcolsortvar_UI IN brwproc[10] (INPUT " WHERE KTYPKOD = '" + typkod + "'").  
         RUN openbdynspec_UI IN brwproc[10].  
      END.
      ELSE DO:
         MESSAGE "Du har inte valt någon kod som ändringen ska gälla för."
         VIEW-AS ALERT-BOX TITLE "Meddelande".
      END.      
   END.
   ELSE DO:
      MESSAGE "Du har inte valt någon kopplad kod som ändringen ska gälla för."
      VIEW-AS ALERT-BOX TITLE "Meddelande".
   END.      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-C
&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB WINDOW-3
ON CHOOSE OF BTN_AVB IN FRAME FRAME-C /* Avsluta */
DO:
   /*
   RETURN.
   */
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_back
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_back WINDOW-3
ON CHOOSE OF btn_back IN FRAME FRAME-C
DO:   
   EMPTY TEMP-TABLE ekalkbertemp NO-ERROR. 
   ASSIGN
   antal_valda = BRW_MTRL:NUM-SELECTED-ROWS
   antal_raknare = 1.
   DO WHILE antal_raknare LE antal_valda:                                   
      status-ok = BRW_MTRL:FETCH-SELECTED-ROW(antal_raknare).                      
      IF AVAILABLE spec_mtrl THEN DO:         
         DELETE spec_mtrl.
      END.
      antal_raknare = antal_raknare + 1.   
   END.
   RUN refreshbrw_UI IN brwproc[8].
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-ANDRA
&Scoped-define SELF-NAME BTN_BORT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_BORT WINDOW-3
ON CHOOSE OF BTN_BORT IN FRAME FRAME-ANDRA /* Ta bort */
DO:
   ASSIGN
   antal_valda = BRW_MTRLBER:NUM-SELECTED-ROWS
   antal_raknare = 1.
   DO WHILE antal_raknare LE antal_valda:
      ASSIGN                                   
      status-ok = BRW_MTRLBER:FETCH-SELECTED-ROW(antal_raknare).
      IF AVAILABLE kalkbertemp THEN DO:
         MESSAGE "Vill du ta bort kod - " + kalkbertemp.ARBKOD + " " + STRING(kalkbertemp.LOPNR,Guru.Konstanter:varforetypchar[6]) VIEW-AS ALERT-BOX
         QUESTION BUTTONS YES-NO TITLE "Meddelande" UPDATE svar AS LOGICAL.         
         IF svar THEN DO:
            RUN bort_UI IN kalkkoppapph (INPUT kalkbertemp.KALKROW).               
            DELETE kalkbertemp.  
            IF antal_raknare = antal_valda THEN RUN selnextprevrow_UI IN brwproc[10].
            
         END.
      END.
      antal_raknare = antal_raknare + 1.   
   END. 
   RUN refreshbrw_UI IN brwproc[10].
   RUN lastselectdyn_UI IN brwproc[10].
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-C
&Scoped-define SELF-NAME BTN_BORT2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_BORT2 WINDOW-3
ON CHOOSE OF BTN_BORT2 IN FRAME FRAME-C /* Ta Bort */
DO:
  /*
  ta bort + pilvänster  
  */
  EMPTY TEMP-TABLE ekalkbertemp NO-ERROR. 
   ASSIGN
   antal_valda = BRW_MTRL:NUM-SELECTED-ROWS
   antal_raknare = 1.
   MESSAGE "Vill du ta bort markerade koder ? " VIEW-AS ALERT-BOX
   QUESTION BUTTONS YES-NO TITLE "Meddelande" UPDATE svar2 AS LOGICAL.         
   IF svar2 THEN DO:
      DO WHILE antal_raknare LE antal_valda:                                   
         status-ok = BRW_MTRL:FETCH-SELECTED-ROW(antal_raknare).                      
         IF AVAILABLE spec_mtrl THEN DO:
            EMPTY TEMP-TABLE ekalkbertemp NO-ERROR.          
            CREATE ekalkbertemp.
            ASSIGN
            ekalkbertemp.KTYPKOD = konsttemp.KTYPKOD 
            ekalkbertemp.ARBKOD = spec_mtrl.ARBKOD
            ekalkbertemp.LOPNR = spec_mtrl.LOPNR
            ekalkbertemp.BENAMNING = spec_mtrl.BENAMNING 
            ekalkbertemp.ENHET = spec_mtrl.ENHET
            ekalkbertemp.ANTAL = spec_mtrl.ANTAL.
            IF AVAILABLE konstvaltemp THEN DO:
               IF brw = 2 THEN ekalkbertemp.F1 = konstvaltemp.KVALKOD.               
               IF brw = 3 THEN ekalkbertemp.F2 = konstvaltemp.KVALKOD.               
               IF brw = 4 THEN ekalkbertemp.F3 = konstvaltemp.KVALKOD.               
               IF brw = 5 THEN ekalkbertemp.F4 = konstvaltemp.KVALKOD.               
               IF brw = 6 THEN ekalkbertemp.F5 = konstvaltemp.KVALKOD.                     
            END.
            RUN bortkalkber_UI IN kalkkoppapph (INPUT TABLE ekalkbertemp).
            DELETE spec_mtrl.
         END.
         antal_raknare = antal_raknare + 1.   
      END.
      RUN refreshbrw_UI IN brwproc[8].  
   END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-ANDRA
&Scoped-define SELF-NAME BTN_NY
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NY WINDOW-3
ON CHOOSE OF BTN_NY IN FRAME FRAME-ANDRA /* Ny */
DO:
   antal_valda = BRW_MTRLBER:NUM-SELECTED-ROWS.
   IF antal_valda > 0 THEN DO:
      FIND FIRST spec_mtrl NO-LOCK NO-ERROR.
      IF AVAILABLE spec_mtrl THEN DO:
         ASSIGN
         antal_valda = BRW_MTRLBER:NUM-SELECTED-ROWS
         antal_raknare = 1.
         DO WHILE antal_raknare LE antal_valda:
            ASSIGN                                   
            status-ok = BRW_MTRLBER:FETCH-SELECTED-ROW(antal_raknare).
            IF AVAILABLE kalkbertemp THEN DO:
               MESSAGE "Vill du lägga upp ny(a) kod(er) till - " + kalkbertemp.KTYPKOD + " " + kalkbertemp.F1 + 
               " " + kalkbertemp.F2 + " " + kalkbertemp.F3 + " " + kalkbertemp.F4 + " " + kalkbertemp.F5 VIEW-AS ALERT-BOX
               QUESTION BUTTONS YES-NO TITLE "Meddelande" UPDATE svar AS LOGICAL.         
               IF svar THEN DO:
                  RUN ny_UI IN kalkkoppapph (INPUT kalkbertemp.KTYPKOD,INPUT kalkbertemp.KALKROW,    
                                             INPUT kalkbertemp.F1,INPUT kalkbertemp.F2,           
                                             INPUT kalkbertemp.F3,INPUT kalkbertemp.F4,INPUT kalkbertemp.F5,  
                                             INPUT TABLE spec_mtrl,
                                             INPUT-OUTPUT TABLE kalkbertemp). 
                  
               END.
            END.
            antal_raknare = antal_raknare + 1.   
         END. 
         RUN setcolsortvar_UI IN brwproc[10] (INPUT " WHERE KTYPKOD = '" + typkod + "'").  
         RUN openbdynspec_UI IN brwproc[10].  
         
      END.
      ELSE DO:
         MESSAGE "Du har inte valt någon kod som ska läggas upp."
         VIEW-AS ALERT-BOX TITLE "Meddelande".
      END.      
   END.
   ELSE DO:
      MESSAGE "Du har inte valt någon kopplad kod som kan gälla som mall för upplägg av ny kod."
      VIEW-AS ALERT-BOX TITLE "Meddelande".
   END.      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-C
&Scoped-define SELF-NAME btn_over
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_over WINDOW-3
ON CHOOSE OF btn_over IN FRAME FRAME-C
DO:    
   IF RAD_VAL = 1 THEN DO:
      ASSIGN
      antal_valda = BRW_P3:NUM-SELECTED-ROWS        
      antal_raknare = 1.
      DO WHILE antal_raknare LE antal_valda:                    
         ASSIGN            
         status-ok = BRW_P3:FETCH-SELECTED-ROW(antal_raknare).     
         IF AVAILABLE ptemp3 THEN DO:
            FOR EACH loptemp3 WHERE loptemp3.ARBKOD = ptemp3.ARBKOD:
               CREATE spec_mtrl.
               ASSIGN
               spec_mtrl.ARBKOD = loptemp3.ARBKOD
               spec_mtrl.LOPNR = loptemp3.LOPNR
               spec_mtrl.BENAMNING = SUBSTRING(loptemp3.BENAMNING,1,49)
               spec_mtrl.ENHET = loptemp3.ENHET
               spec_mtrl.ANTAL = 1.      
            END.
         END.
         antal_raknare = antal_raknare + 1.
      END.
      status-ok = BRW_P3:DESELECT-ROWS().    
   END.
   ELSE DO:
      ASSIGN
      antal_valda = BRW_LOP3:NUM-SELECTED-ROWS        
      antal_raknare = 1.
      DO WHILE antal_raknare LE antal_valda:                    
         ASSIGN            
         status-ok = BRW_LOP3:FETCH-SELECTED-ROW(antal_raknare).     
         IF AVAILABLE loptemp3 THEN DO:
            CREATE spec_mtrl.
            ASSIGN
            spec_mtrl.ARBKOD = loptemp3.ARBKOD
            spec_mtrl.LOPNR = loptemp3.LOPNR
            spec_mtrl.BENAMNING = SUBSTRING(loptemp3.BENAMNING,1,49)
            spec_mtrl.ENHET = loptemp3.ENHET
            spec_mtrl.ANTAL = 1.      
         END.
         antal_raknare = antal_raknare + 1.
      END. 
      status-ok = BRW_LOP3:DESELECT-ROWS().              
   END.   
   RUN setcolindex_UI IN brwproc[8] (INPUT " ARBKOD BY LOPNR ").
   RUN openbdynspec_UI IN brwproc[8].
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_RENSA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_RENSA WINDOW-3
ON CHOOSE OF BTN_RENSA IN FRAME FRAME-C /* Rensa */
DO: 
   {muswait.i}   
   EMPTY TEMP-TABLE spec_mtrl NO-ERROR. 
   RUN openbdynspec_UI IN brwproc[8].
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_VAL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_VAL WINDOW-3
ON VALUE-CHANGED OF CMB_VAL IN FRAME FRAME-C
DO:  
   {muswait.i} 
   ASSIGN
   CMB_VAL = INPUT CMB_VAL
   mark = FALSE.
   FIND FIRST konstgrptemp WHERE konstgrptemp.BENAMNING = CMB_VAL NO-LOCK NO-ERROR. 
   IF AVAILABLE konstgrptemp THEN ASSIGN kongrkod = konstgrptemp.KONSKOD.
   ELSE DO:
      MESSAGE "Konstruktionen är ej färdig för kalkylkoder!" VIEW-AS ALERT-BOX.
      APPLY "CLOSE" TO THIS-PROCEDURE.
      musz = TRUE.
      RETURN NO-APPLY.
   END.
   FIND FIRST konstvaltemp WHERE konstvaltemp.KONSKOD = konstgrptemp.KONSKOD NO-LOCK NO-ERROR.
   IF NOT AVAILABLE konstvaltemp THEN DO:
      RUN laddatempen_UI IN kalkkoppapph (INPUT konstgrptemp.KONSKOD, OUTPUT TABLE konstvaltemp APPEND).
   END.

   RUN bytgrupp_UI.      
   {musarrow.i}      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_ANDRA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_ANDRA WINDOW-3
ON CHOOSE OF FBTN_ANDRA IN FRAME FRAME-C /* Ändra */
DO:
   IF FRAME FRAME-ANDRA:HIDDEN = TRUE THEN DO:
      ASSIGN
      kalkbertemp.F1:LABEL IN BROWSE BRW_MTRLBER = FILL-IN-B2
      kalkbertemp.F2:LABEL IN BROWSE BRW_MTRLBER = FILL-IN-B3
      kalkbertemp.F3:LABEL IN BROWSE BRW_MTRLBER = FILL-IN-B4
      kalkbertemp.F4:LABEL IN BROWSE BRW_MTRLBER = FILL-IN-B5
      kalkbertemp.F5:LABEL IN BROWSE BRW_MTRLBER = FILL-IN-B6
      FBTN_ANDRA-2:HIDDEN = FALSE 
      FBTN_ANDRA:HIDDEN = TRUE
      FBTN_HAMTA:HIDDEN = TRUE
      FBTN_ANDRA:LABEL = "Åter"
      FBTN_SKAPA:HIDDEN = TRUE   
      FRAME FRAME-ANDRA:HIDDEN = FALSE.
   END.
   ELSE IF FRAME FRAME-ANDRA:HIDDEN = FALSE THEN DO:
      ASSIGN    
      FBTN_ANDRA-2:HIDDEN = TRUE 
      FBTN_ANDRA:HIDDEN = FALSE
      FBTN_HAMTA:HIDDEN = FALSE
      FBTN_ANDRA:LABEL = "Ändra"
      FBTN_SKAPA:HIDDEN = FALSE   
      FRAME FRAME-ANDRA:HIDDEN = TRUE.
      /*konstvalrow2 = ?.*/
      APPLY "CHOOSE" TO FBTN_HAMTA.
      RETURN NO-APPLY.
   END.
   EMPTY TEMP-TABLE spec_mtrl NO-ERROR. 
   RUN setcolindex_UI IN brwproc[8] (INPUT " ARBKOD BY LOPNR ").
   RUN openbdynspec_UI IN brwproc[8]. 
   RUN kalkhmt_UI IN kalkkoppapph (INPUT typkod,INPUT varforetypval[36],OUTPUT TABLE kalkbertemp).
   FIND FIRST kalkbertemp NO-LOCK NO-ERROR.
   RUN setcolsortvar_UI IN brwproc[10] (INPUT " WHERE KTYPKOD = '" + typkod + "'").  
   RUN openbdynspec_UI IN brwproc[10].   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_ANDRA-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_ANDRA-2 WINDOW-3
ON CHOOSE OF FBTN_ANDRA-2 IN FRAME FRAME-C /* Åter */
DO:
   ASSIGN
   FBTN_ANDRA-2:HIDDEN = TRUE 
   FBTN_ANDRA:HIDDEN = FALSE.
   APPLY "CHOOSE" TO FBTN_ANDRA.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_HAMTA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_HAMTA WINDOW-3
ON CHOOSE OF FBTN_HAMTA IN FRAME FRAME-C /* Hämta */
DO: 
   
   EMPTY TEMP-TABLE kon_val NO-ERROR. 
   antal_valda = 0.
   IF brw = 2 THEN antal_valda = BRW_B2:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME}.      
   IF brw = 3 THEN antal_valda = BRW_B3:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME}.
   IF brw = 4 THEN antal_valda = BRW_B4:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME}.
   IF brw = 5 THEN antal_valda = BRW_B5:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME}.
   IF brw = 6 THEN antal_valda = BRW_B6:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME}.               
   IF antal_valda > 1 THEN DO:
      BRW_MTRL:TITLE = "Valda koder för flerval".
      EMPTY TEMP-TABLE spec_mtrl NO-ERROR. 
      RUN setcolindex_UI IN brwproc[8] (INPUT " ARBKOD BY LOPNR ").
      RUN openbdynspec_UI IN brwproc[8]. 
      RETURN NO-APPLY.
   END.
   {muswait.i}
   RUN hamta_UI.   
   EMPTY TEMP-TABLE spec_mtrl NO-ERROR. 
   BRW_MTRL:TITLE = "Valda koder".
   IF Guru.Konstanter:appcon THEN DO:                           
      RUN ADMHMTKONKALK.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT kon_val.NUM, INPUT TABLE kon_val,INPUT varforetypval[36],OUTPUT TABLE spec_mtrl).
   END.
   ELSE DO:
      RUN ADMHMTKONKALK.P      
      (INPUT kon_val.NUM, INPUT TABLE kon_val,INPUT varforetypval[36],OUTPUT TABLE spec_mtrl).
   END.             
   
   RUN setcolindex_UI IN brwproc[8] (INPUT " ARBKOD BY LOPNR ").
   RUN openbdynspec_UI IN brwproc[8]. 
   
   RUN bortaktiv_UI.
   {musarrow.i}           
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_SKAPA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_SKAPA WINDOW-3
ON CHOOSE OF FBTN_SKAPA IN FRAME FRAME-C /* Koppla */
DO: 
   {muswait.i}
   EMPTY TEMP-TABLE ekalkbertemp NO-ERROR.
   /* 
   FOR EACH spec_mtrl:
      CREATE ekalkbertemp.
      ASSIGN
      ekalkbertemp.KTYPKOD = konsttemp.KTYPKOD 
      ekalkbertemp.ARBKOD = spec_mtrl.ARBKOD
      ekalkbertemp.LOPNR = spec_mtrl.LOPNR
      ekalkbertemp.BENAMNING = spec_mtrl.BENAMNING 
      ekalkbertemp.ENHET = spec_mtrl.ENHET
      ekalkbertemp.ANTAL = spec_mtrl.ANTAL.
      IF AVAILABLE konstvaltemp THEN DO:
         IF brw = 2 THEN ekalkbertemp.F1 = konstvaltemp.KVALKOD.               
         IF brw = 3 THEN ekalkbertemp.F2 = konstvaltemp.KVALKOD.               
         IF brw = 4 THEN ekalkbertemp.F3 = konstvaltemp.KVALKOD.               
         IF brw = 5 THEN ekalkbertemp.F4 = konstvaltemp.KVALKOD.               
         IF brw = 6 THEN ekalkbertemp.F5 = konstvaltemp.KVALKOD.                     
      END.
   END.
   /*BORTKOMMENTERAD bortkalkber_UI, köra checkkalkber_UI istället och ta bort från TT istället för från DB och lägga till igen */
   
   RUN checkkalkber_UI IN kalkkoppapph (INPUT-OUTPUT TABLE ekalkbertemp).
   /*RUN bortkalkber_UI IN kalkkoppapph (INPUT TABLE ekalkbertemp).*/
*/
   RUN skapamtrl_UI.
   RUN setcolindex_UI IN brwproc[8] (INPUT " ARBKOD BY LOPNR ").
   RUN openbdynspec_UI IN brwproc[8].
   MESSAGE "P2/P3 koder är nu kopplade."
   VIEW-AS ALERT-BOX TITLE "Meddelande".
   APPLY "CHOOSE" TO FBTN_HAMTA.
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RAD_VAL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RAD_VAL WINDOW-3
ON VALUE-CHANGED OF RAD_VAL IN FRAME FRAME-C
DO:
   RAD_VAL = INPUT RAD_VAL.
   IF RAD_VAL = 1 THEN DO:
      ASSIGN
      BRW_LOP3:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      BRW_P3:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
   END.
   ELSE DO:
      ASSIGN
      BRW_LOP3:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      BRW_P3:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_KVAR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_KVAR WINDOW-3
ON VALUE-CHANGED OF TOG_KVAR IN FRAME FRAME-C /* Rensa ej bilden */
DO:
   TOG_KVAR = INPUT TOG_KVAR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-ANDRA
&Scoped-define BROWSE-NAME BRW_B2
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK WINDOW-3 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
DO:
   {BORTBRWPROC.I}
   IF VALID-HANDLE(laddaproch) THEN DELETE PROCEDURE laddaproch.
   IF VALID-HANDLE(mtrlhmtapph) THEN DELETE PROCEDURE mtrlhmtapph.
   IF VALID-HANDLE(KalkClasserStart) THEN DELETE PROCEDURE KalkClasserStart NO-ERROR.
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
   &Scoped-define FRAME-NAME FRAME-C
   {WIN_M_START.I}
   {muswait.i}
   {ALLSTARTDYN.I}
   EMPTY TEMP-TABLE kalkbertemp NO-ERROR.      
   ASSIGN   
   status-ok = CMB_VAL:DELETE("0").
   FOR EACH konstgrptemp USE-INDEX ORD NO-LOCK:
      FIND FIRST konsttemp WHERE konsttemp.KONSKOD = konstgrptemp.KONSKOD NO-LOCK NO-ERROR.
      IF AVAILABLE konsttemp THEN DO:         
         status-ok = CMB_VAL:ADD-LAST(konstgrptemp.BENAMNING)IN FRAME {&FRAME-NAME}.
      END.                      
   END.   
   FIND FIRST konstgrptemp WHERE konstgrptemp.KONSKOD = kongrkod NO-LOCK NO-ERROR.
   IF AVAILABLE konstgrptemp THEN DO:
      FIND FIRST konsttemp WHERE konsttemp.KONSKOD = konstgrptemp.KONSKOD NO-LOCK NO-ERROR.
      IF AVAILABLE konsttemp THEN DO:    
         CMB_VAL:SCREEN-VALUE = konstgrptemp.BENAMNING.
      END.
   END.
   ASSIGN 
   counter = 0
   mark = FALSE  
   CMB_VAL = INPUT CMB_VAL.
   EMPTY TEMP-TABLE ekalkbertemp NO-ERROR. 
   EMPTY TEMP-TABLE spec_mtrl NO-ERROR. 
   EMPTY TEMP-TABLE kon_val NO-ERROR.    
/*    APPLY "VALUE-CHANGED" TO CMB_VAL. */
   ASSIGN
   bestant = FALSE
   val = FALSE
   sok = FALSE
   RAD_VAL = 1.             
   RUN enable_UI.
   FRAME FRAME-ANDRA:HIDDEN = TRUE.
   {FRMSIZEF.I}  
   ASSIGN  
   BRW_B2:HIDDEN = TRUE  
   FILL-IN-B2:HIDDEN = TRUE
   BRW_B3:HIDDEN = TRUE  
   FILL-IN-B3:HIDDEN = TRUE
   BRW_B4:HIDDEN = TRUE  
   FILL-IN-B4:HIDDEN = TRUE
   BRW_B5:HIDDEN = TRUE  
   FILL-IN-B5:HIDDEN = TRUE
   BRW_B6:HIDDEN = TRUE  
   FILL-IN-B6:HIDDEN = TRUE 
   BRW_LOP3:HIDDEN IN FRAME {&FRAME-NAME} = TRUE  
   FBTN_ANDRA-2:HIDDEN = TRUE 
   FBTN_ANDRA:HIDDEN = FALSE.
   APPLY "VALUE-CHANGED" TO CMB_VAL IN FRAME {&FRAME-NAME}. 
   IF musz = TRUE  THEN DO:
      musz = FALSE.
      LEAVE MAIN-BLOCK.
   END.
   
   RUN openbdynspec_UI IN brwproc[9].   
   APPLY "VALUE-CHANGED" TO BRW_P3 IN FRAME {&FRAME-NAME}.
   
   IF varkod NE ? THEN
   FIND FIRST konsttemp WHERE konsttemp.KTYPKOD = varkod AND konsttemp.KONSKOD = kongrkod NO-LOCK NO-ERROR.
   IF AVAILABLE konsttemp THEN DO:
      RUN setlastrowid_UI IN brwproc[6] (INPUT ROWID(konsttemp)).              
      RUN lastselectdyn_UI IN brwproc[6].      
   END.
   TOG_KVAR:HIDDEN IN FRAME FRAME-C = TRUE.
  {musarrow.i}  
   {WIN_M_SLUT.I}
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE allstartbrw_UI WINDOW-3 
PROCEDURE allstartbrw_UI :
/* -----------------------------------------------------------
  Purpose:      
  Parameters:  
  Notes:       
-------------------------------------------------------------*/ 
   RUN DYNBRW.P PERSISTENT SET brwproc[1]
      (INPUT BRW_B2:HANDLE IN FRAME {&FRAME-NAME}).    
   RUN brwsetupstop_UI IN brwproc[1] (INPUT 1).
   RUN DYNBRW.P PERSISTENT SET brwproc[2]
      (INPUT BRW_B3:HANDLE IN FRAME {&FRAME-NAME}).    
   RUN brwsetupstop_UI IN brwproc[2] (INPUT 1).
   RUN DYNBRW.P PERSISTENT SET brwproc[3]
      (INPUT BRW_B4:HANDLE IN FRAME {&FRAME-NAME}).           
   RUN brwsetupstop_UI IN brwproc[3] (INPUT 1).
   RUN DYNBRW.P PERSISTENT SET brwproc[4]
      (INPUT BRW_B5:HANDLE IN FRAME {&FRAME-NAME}).        
   RUN brwsetupstop_UI IN brwproc[4] (INPUT 1).
   RUN DYNBRW.P PERSISTENT SET brwproc[5]
      (INPUT BRW_B6:HANDLE IN FRAME {&FRAME-NAME}).    
   RUN brwsetupstop_UI IN brwproc[5] (INPUT 1).
   RUN DYNBRW.P PERSISTENT SET brwproc[6]
      (INPUT BRW_KON:HANDLE IN FRAME {&FRAME-NAME}). 
   RUN brwsetupstop_UI IN brwproc[6] (INPUT 1).
   RUN DYNBRW.P PERSISTENT SET brwproc[7]
      (INPUT BRW_LOP3:HANDLE IN FRAME {&FRAME-NAME}).                                         
   RUN DYNBRW.P PERSISTENT SET brwproc[8]
      (INPUT BRW_MTRL:HANDLE IN FRAME {&FRAME-NAME}).      
   RUN DYNBRW.P PERSISTENT SET brwproc[9]
      (INPUT BRW_P3:HANDLE IN FRAME {&FRAME-NAME}).
   RUN DYNBRW.P PERSISTENT SET brwproc[10]
      (INPUT BRW_MTRLBER:HANDLE IN FRAME FRAME-ANDRA).
   RUN setcolindex_UI IN brwproc[1] (INPUT "ORDNING"). 
   RUN setcolindex_UI IN brwproc[2] (INPUT "ORDNING"). 
   RUN setcolindex_UI IN brwproc[3] (INPUT "ORDNING"). 
   RUN setcolindex_UI IN brwproc[4] (INPUT "ORDNING"). 
   RUN setcolindex_UI IN brwproc[5] (INPUT "ORDNING"). 
   RUN setcolindex_UI IN brwproc[6] (INPUT "ORDNING"). 
   RUN setcolindex_UI IN brwproc[7] (INPUT "ARBKOD BY LOPNR"). 
   RUN setcolindex_UI IN brwproc[9] (INPUT "ARBKOD"). 
   RUN setcolindex_UI IN brwproc[10] (INPUT "kalkbertemp.F1 BY kalkbertemp.F2 BY kalkbertemp.F3 BY kalkbertemp.F4 BY
   kalkbertemp.F5 BY kalkbertemp.ARBKOD BY kalkbertemp.LOPNR"). 
   RUN addmenuitem_UI IN brwproc[1] (INPUT BRW_B2:HANDLE,INPUT "Avmarkera",INPUT "avmark_UI").
   RUN addmenuitem_UI IN brwproc[2] (INPUT BRW_B3:HANDLE,INPUT "Avmarkera",INPUT "avmark_UI").
   RUN addmenuitem_UI IN brwproc[3] (INPUT BRW_B4:HANDLE,INPUT "Avmarkera",INPUT "avmark_UI").
   RUN addmenuitem_UI IN brwproc[4] (INPUT BRW_B5:HANDLE,INPUT "Avmarkera",INPUT "avmark_UI").
   RUN addmenuitem_UI IN brwproc[5] (INPUT BRW_B6:HANDLE,INPUT "Avmarkera",INPUT "avmark_UI").
   IF varforetypchar[48] NE "" THEN DO:    
      IF NOT VALID-HANDLE(KalkClasserStart) THEN RUN Modules\Kalkyl\KalkClasserStart.P PERSISTENT SET KalkClasserStart.
   END.
   IF Guru.Konstanter:appcon THEN DO:
      RUN MTRLHMT.P PERSISTENT SET mtrlhmtapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN MTRLHMT.P PERSISTENT SET mtrlhmtapph.
   END.    
   IF Guru.Konstanter:appcon THEN DO:
      RUN DYNLADDATEMP.P PERSISTENT SET laddaproch ON Guru.Konstanter:apphand TRANSACTION DISTINCT
         (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "", INPUT "").
   END.
   ELSE DO:
      RUN DYNLADDATEMP.P PERSISTENT SET laddaproch
         (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "", INPUT "").
   END.
   
   IF varforetypchar[48] NE "" THEN DO:
      RUN Ptemp3_UI IN KalkClasserStart (INPUT 3, OUTPUT TABLE ptemp3 APPEND).
   END.
   ELSE DO:   
      tthandle = TEMP-TABLE ptemp3:HANDLE.
      RUN laddatemp_UI IN laddaproch (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "P3", INPUT "").
      RUN ladda_UI IN kalkkoppapph (OUTPUT katvar).
      FOR EACH ptemp3 WHERE ptemp3.KATAR NE katvar:
         DELETE ptemp3.
      END.
   END.   
   tthandle = TEMP-TABLE konstgrptemp:HANDLE.
   RUN laddatemp_UI IN laddaproch (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "KONSTGRUPP", INPUT "").
   tthandle = TEMP-TABLE bbenamntemp:HANDLE. 
   RUN laddatemp_UI IN laddaproch (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "BBENAMNING", INPUT "").
   tthandle = TEMP-TABLE konsttemp:HANDLE.
   RUN laddatemp_UI IN laddaproch (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "KONSTRUKTION", INPUT "").   
   

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE avmark_UI WINDOW-3 
PROCEDURE avmark_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/    
   IF brw = 2 THEN status-ok = BRW_B2:DESELECT-ROWS() IN FRAME {&FRAME-NAME} NO-ERROR.
   IF brw = 3 THEN status-ok = BRW_B3:DESELECT-ROWS() IN FRAME {&FRAME-NAME} NO-ERROR.
   IF brw = 4 THEN status-ok = BRW_B4:DESELECT-ROWS() IN FRAME {&FRAME-NAME} NO-ERROR.
   IF brw = 5 THEN status-ok = BRW_B5:DESELECT-ROWS() IN FRAME {&FRAME-NAME} NO-ERROR.
   IF brw = 6 THEN status-ok = BRW_B6:DESELECT-ROWS() IN FRAME {&FRAME-NAME} NO-ERROR.              
   mark = FALSE.
   APPLY "CHOOSE" TO FBTN_HAMTA.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE bytgrupp_UI WINDOW-3 
PROCEDURE bytgrupp_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/    
   FIND FIRST bbenamntemp WHERE bbenamntemp.KONSKOD = kongrkod NO-LOCK NO-ERROR.
   IF AVAILABLE bbenamntemp THEN DO:
      ASSIGN FILL-IN-B1 = " " + bbenamntemp.B1.
   END.
   DISPLAY FILL-IN-B1 WITH FRAME {&FRAME-NAME}.
   RUN setcolsortvar_UI IN brwproc[6] (INPUT " WHERE konsttemp.KONSKOD = " + STRING(kongrkod) + " USE-INDEX ORD ").
   RUN openbdynspec_UI IN brwproc[6].
   APPLY "VALUE-CHANGED" TO BRW_KON IN FRAME {&FRAME-NAME}. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE bytkonst_UI WINDOW-3 
PROCEDURE bytkonst_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/       
   DEFINE VARIABLE postvald AS CHARACTER NO-UNDO.
   IF bbenamntemp.B2 NE " " THEN DO:
      ASSIGN
      BRW_B2:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      FILL-IN-B2 = " " + bbenamntemp.B2.
      DISPLAY FILL-IN-B2 WITH FRAME {&FRAME-NAME}.
      postvald = "".
      antal_valda = BRW_B2:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME}. 
      IF antal_valda > 0 THEN DO:
         status-ok = BRW_B2:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
         postvald = konstvaltemp.KVALKOD.                     
      END.
      RUN setcolsortvar_UI IN brwproc[1] (INPUT " WHERE konstvaltemp.KONSKOD = " + STRING(kongrkod) + " AND konstvaltemp.KTYPKOD = '" + typkod + "' AND 
                                       konstvaltemp.KOPP = TRUE AND konstvaltemp.BB = '" + bbenamntemp.B2 + "' USE-INDEX ORD").
      RUN openbdynspec_UI IN brwproc[1]. 
      IF AVAILABLE konstvaltemp THEN finns[2] = TRUE.
      ELSE finns[2] = FALSE.  
      ASSIGN
      BRW_B2:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      FILL-IN-B2:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
      RUN postvald_UI (INPUT 1,INPUT postvald,INPUT bbenamntemp.B2).
   END. 
   ELSE DO:
      ASSIGN
      BRW_B2:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      FILL-IN-B2:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
   END.   
   IF bbenamntemp.B3 NE " " THEN DO: 
      ASSIGN
      BRW_B3:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      FILL-IN-B3 = " " + bbenamntemp.B3.
      DISPLAY FILL-IN-B3 WITH FRAME {&FRAME-NAME}.  
      postvald = "".
      antal_valda = BRW_B3:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME}. 
      IF antal_valda > 0 THEN DO:
         status-ok = BRW_B3:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
         postvald = konstvaltemp.KVALKOD.                     
      END.
      RUN setcolsortvar_UI IN brwproc[2] (INPUT " WHERE konstvaltemp.KONSKOD = " + STRING(kongrkod) + " AND konstvaltemp.KTYPKOD = '" + typkod + "' AND 
                                       konstvaltemp.KOPP = TRUE AND konstvaltemp.BB = '" + bbenamntemp.B3 + "' USE-INDEX ORD").
      RUN openbdynspec_UI IN brwproc[2].
      IF AVAILABLE konstvaltemp THEN finns[3] = TRUE.
      ELSE finns[3] = FALSE.  
      ASSIGN
      BRW_B3:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      FILL-IN-B3:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
      RUN postvald_UI (INPUT 2,INPUT postvald,INPUT bbenamntemp.B3).
   END.  
   ELSE DO: 
      ASSIGN
      BRW_B3:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      FILL-IN-B3:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
   END.  
   IF bbenamntemp.B4 NE " " THEN DO:
      ASSIGN
      BRW_B4:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      FILL-IN-B4 = " " + bbenamntemp.B4.
      DISPLAY FILL-IN-B4 WITH FRAME {&FRAME-NAME}.
      postvald = "".
      antal_valda = BRW_B4:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME}. 
      IF antal_valda > 0 THEN DO:
         status-ok = BRW_B4:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
         postvald = konstvaltemp.KVALKOD.                     
      END.
      RUN setcolsortvar_UI IN brwproc[3] (INPUT " WHERE konstvaltemp.KONSKOD = " + STRING(kongrkod) + " AND konstvaltemp.KTYPKOD = '" + typkod + "' AND 
                                       konstvaltemp.KOPP = TRUE AND konstvaltemp.BB = '" + bbenamntemp.B4 + "' USE-INDEX ORD").
      RUN openbdynspec_UI IN brwproc[3].
      IF AVAILABLE konstvaltemp THEN finns[4] = TRUE.
      ELSE finns[4] = FALSE.  
      ASSIGN
      BRW_B4:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      FILL-IN-B4:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
      RUN postvald_UI (INPUT 3,INPUT postvald,INPUT bbenamntemp.B4).
   END.
   ELSE DO: 
      ASSIGN
      BRW_B4:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      FILL-IN-B4:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
   END.    
   IF bbenamntemp.B5 NE " " THEN DO:
      ASSIGN
      BRW_B5:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      FILL-IN-B5 = " " + bbenamntemp.B5.
      DISPLAY FILL-IN-B5 WITH FRAME {&FRAME-NAME}.
      postvald = "".
      antal_valda = BRW_B5:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME}. 
      IF antal_valda > 0 THEN DO:
         status-ok = BRW_B5:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
         postvald = konstvaltemp.KVALKOD.                     
      END.
      RUN setcolsortvar_UI IN brwproc[4] (INPUT " WHERE konstvaltemp.KONSKOD = " + STRING(kongrkod) + " AND konstvaltemp.KTYPKOD = '" + typkod + "' AND 
                                       konstvaltemp.KOPP = TRUE AND konstvaltemp.BB = '" + bbenamntemp.B5 + "' USE-INDEX ORD").
      RUN openbdynspec_UI IN brwproc[4].
      IF AVAILABLE konstvaltemp THEN finns[5] = TRUE.
      ELSE finns[5] = FALSE. 
      ASSIGN
      BRW_B5:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      FILL-IN-B5:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
      RUN postvald_UI (INPUT 4,INPUT postvald,INPUT bbenamntemp.B5).
   END.       
   ELSE DO:
      ASSIGN
      BRW_B5:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      FILL-IN-B5:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
   END. 
   IF bbenamntemp.B6 NE " " THEN DO: 
      ASSIGN
      BRW_B6:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      FILL-IN-B6 = " " + bbenamntemp.B6.
      DISPLAY FILL-IN-B6 WITH FRAME {&FRAME-NAME}.
      postvald = "".
      antal_valda = BRW_B6:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME}. 
      IF antal_valda > 0 THEN DO:
         status-ok = BRW_B6:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
         postvald = konstvaltemp.KVALKOD.                     
      END.
      RUN setcolsortvar_UI IN brwproc[5] (INPUT " WHERE konstvaltemp.KONSKOD = " + STRING(kongrkod) + " AND 
      konstvaltemp.KTYPKOD = '" + typkod + "' AND 
      konstvaltemp.KOPP = TRUE AND konstvaltemp.BB = '" + bbenamntemp.B6 + "' USE-INDEX ORD").
      RUN openbdynspec_UI IN brwproc[5].
      IF AVAILABLE konstvaltemp THEN finns[6] = TRUE.
      ELSE finns[6] = FALSE.           
      ASSIGN
      BRW_B6:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      FILL-IN-B6:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
      RUN postvald_UI (INPUT 5,INPUT postvald,INPUT bbenamntemp.B6).
   END. 
   ELSE DO:
      ASSIGN
      BRW_B6:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      FILL-IN-B6:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
   END.   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI WINDOW-3  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-3)
  THEN DELETE WIDGET WINDOW-3.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI WINDOW-3  _DEFAULT-ENABLE
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
  ENABLE BRW_MTRLBER BTN_NY BTN_ANDE BTN_ANDA BTN_BORT 
      WITH FRAME FRAME-ANDRA IN WINDOW WINDOW-3.
  VIEW FRAME FRAME-ANDRA IN WINDOW WINDOW-3.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-ANDRA}
  DISPLAY CMB_VAL RAD_VAL TOG_KVAR FILL-IN-B1 FILL-IN-B2 FILL-IN-B3 FILL-IN-B4 
          FILL-IN-B5 FILL-IN-B6 
      WITH FRAME FRAME-C IN WINDOW WINDOW-3.
  ENABLE CMB_VAL BRW_KON BRW_B2 BRW_B3 FBTN_HAMTA FBTN_ANDRA FBTN_ANDRA-2 
         BRW_B4 BRW_B5 BRW_B6 RAD_VAL BRW_P3 BRW_LOP3 BRW_MTRL btn_over 
         btn_back FBTN_SKAPA BTN_BORT2 BTN_RENSA BTN_AVB FILL-IN-B1 FILL-IN-B2 
         FILL-IN-B3 FILL-IN-B4 FILL-IN-B5 FILL-IN-B6 
      WITH FRAME FRAME-C IN WINDOW WINDOW-3.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-C}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE hamta_UI WINDOW-3 
PROCEDURE hamta_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/      
    
   FIND konstvaltemp WHERE  ROWID(konstvaltemp) = konstvalrow NO-LOCK NO-ERROR.
   CREATE kon_val.
   ASSIGN 
   kon_val.NUM = counter                         
   kon_val.GRUPP = kongrkod
   kon_val.F1 = konsttemp.KTYPKOD.    
   IF bbenamntemp.B2 NE " " THEN DO:
      ASSIGN
      antal_valda = BRW_B2:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME}. 
      IF antal_valda > 0 THEN DO:   
         kon_val.F2 = konstvaltemp.KVALKOD.                             
      END.
   END.   
   IF bbenamntemp.B3 NE " " THEN DO:
       antal_valda = BRW_B3:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME}. 
      IF antal_valda > 0 THEN DO:
         kon_val.F3 = konstvaltemp.KVALKOD.                            
      END.
   END.     
   IF bbenamntemp.B4 NE " " THEN DO:
      antal_valda = BRW_B4:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME}. 
      IF antal_valda > 0 THEN DO:
         kon_val.F4 = konstvaltemp.KVALKOD.                              
      END.
   END.   
   IF bbenamntemp.B5 NE " " THEN DO:
      antal_valda = BRW_B5:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME}. 
      IF antal_valda > 0 THEN DO:
         kon_val.F5 = konstvaltemp.KVALKOD.                     
      END.
   END.          
   IF bbenamntemp.B6 NE " " THEN DO:
      antal_valda = BRW_B6:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME}. 
      IF antal_valda > 0 THEN DO:
         kon_val.F6 = konstvaltemp.KVALKOD.                                
      END.
   END.    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE markerade_UI WINDOW-3 
PROCEDURE markerade_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/    
   DEFINE INPUT PARAMETER avm AS INTEGER NO-UNDO.
   IF avm NE 2 THEN status-ok = BRW_B2:DESELECT-ROWS() IN FRAME {&FRAME-NAME} NO-ERROR.
   IF avm NE 3 THEN status-ok = BRW_B3:DESELECT-ROWS() IN FRAME {&FRAME-NAME} NO-ERROR.
   IF avm NE 4 THEN status-ok = BRW_B4:DESELECT-ROWS() IN FRAME {&FRAME-NAME} NO-ERROR.
   IF avm NE 5 THEN status-ok = BRW_B5:DESELECT-ROWS() IN FRAME {&FRAME-NAME} NO-ERROR.
   IF avm NE 6 THEN status-ok = BRW_B6:DESELECT-ROWS() IN FRAME {&FRAME-NAME} NO-ERROR.      
   
     
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE postvald_UI WINDOW-3 
PROCEDURE postvald_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/    
   DEFINE INPUT PARAMETER brwvald AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER postvald AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER benvar AS CHARACTER NO-UNDO.
   FIND FIRST konstvaltemp WHERE konstvaltemp.KONSKOD = kongrkod AND 
   konstvaltemp.KTYPKOD = typkod AND konstvaltemp.KOPP = TRUE AND konstvaltemp.BB = benvar AND
   konstvaltemp.KVALKOD = postvald NO-ERROR.
   IF NOT AVAILABLE konstvaltemp THEN RETURN.
   RUN setlastrowid_UI IN brwproc[brwvald] (INPUT ROWID(konstvaltemp)).
   RUN lastselectdyn_UI IN brwproc[brwvald].      
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE skapamtrl_UI WINDOW-3 
PROCEDURE skapamtrl_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/ 
   EMPTY TEMP-TABLE ekalkbertemp NO-ERROR. 
   IF mark = TRUE THEN DO:
      IF brw = 2 THEN DO:
         antal_valda = BRW_B2:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME}.
         antal_raknare = 1.    
         
         DO WHILE antal_raknare LE antal_valda:   
            status-ok = BRW_B2:FETCH-SELECTED-ROW(antal_raknare).
            IF AVAILABLE konstvaltemp THEN DO:
               FOR EACH spec_mtrl: 
                  RUN tabell_UI.
                  ekalkbertemp.F1 = konstvaltemp.KVALKOD.               
               END.
            END.
            antal_raknare = antal_raknare + 1.   
         END.                             
      END.     
      ELSE IF brw = 3 THEN DO:
         antal_valda = BRW_B3:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME}.
         antal_raknare = 1.    
         DO WHILE antal_raknare LE antal_valda:   
            status-ok = BRW_B3:FETCH-SELECTED-ROW(antal_raknare).
            IF AVAILABLE konstvaltemp THEN DO:
               FOR EACH spec_mtrl: 
                  RUN tabell_UI.
                  ekalkbertemp.F2 = konstvaltemp.KVALKOD.               
               END.
            END.
            antal_raknare = antal_raknare + 1.   
         END.                             
      END.
      ELSE IF brw = 4 THEN DO:
         antal_valda = BRW_B4:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME}.
         antal_raknare = 1.    
         DO WHILE antal_raknare LE antal_valda:   
            status-ok = BRW_B4:FETCH-SELECTED-ROW(antal_raknare).
            IF AVAILABLE konstvaltemp THEN DO:
               FOR EACH spec_mtrl: 
                  RUN tabell_UI.
                  ekalkbertemp.F3 = konstvaltemp.KVALKOD.               
               END.
            END.
            antal_raknare = antal_raknare + 1.   
         END.                             
      END.
      ELSE IF brw = 5 THEN DO:
         antal_valda = BRW_B5:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME}.
         antal_raknare = 1.    
         DO WHILE antal_raknare LE antal_valda:   
            status-ok = BRW_B5:FETCH-SELECTED-ROW(antal_raknare).
            IF AVAILABLE konstvaltemp THEN DO:
               FOR EACH spec_mtrl: 
                  RUN tabell_UI.
                  ekalkbertemp.F4 = konstvaltemp.KVALKOD.               
               END.   
            END.
            antal_raknare = antal_raknare + 1.   
         END.                             
      END.
      ELSE DO:
         antal_valda = BRW_B6:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME}.
         antal_raknare = 1.    
         DO WHILE antal_raknare LE antal_valda:   
            status-ok = BRW_B6:FETCH-SELECTED-ROW(antal_raknare).
            IF AVAILABLE konstvaltemp THEN DO:
               FOR EACH spec_mtrl: 
                  RUN tabell_UI.
                  ekalkbertemp.F5 = konstvaltemp.KVALKOD.               
               END.   
            END.
            antal_raknare = antal_raknare + 1.   
         END.                             
      END.
   END.               
   ELSE DO:     
      FOR EACH spec_mtrl: 
         RUN tabell_UI.         
      END.       
   END.
   RUN sparkalkber2_UI IN kalkkoppapph (INPUT TABLE ekalkbertemp).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tabell_UI WINDOW-3 
PROCEDURE tabell_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/      
   CREATE ekalkbertemp.
   ASSIGN  
   ekalkbertemp.KTYPKOD = konsttemp.KTYPKOD 
   ekalkbertemp.ARBKOD = spec_mtrl.ARBKOD
   ekalkbertemp.LOPNR = spec_mtrl.LOPNR
   ekalkbertemp.BENAMNING = spec_mtrl.BENAMNING 
   ekalkbertemp.ENHET = spec_mtrl.ENHET
   ekalkbertemp.ANTAL = spec_mtrl.ANTAL.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE vc_UI WINDOW-3 
PROCEDURE vc_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/    
   DEFINE INPUT PARAMETER brwvald AS INTEGER NO-UNDO.
   IF AVAILABLE konstvaltemp THEN konstvalrow = ROWID(konstvaltemp).
   ASSIGN
   brw = brwvald.    
   RUN markerade_UI (INPUT brw).  
   IF finns[brwvald] = TRUE THEN DO:
      ASSIGN
      mark = TRUE.             
   END.      
/*    APPLY "CHOOSE" TO FBTN_HAMTA IN FRAME {&FRAME-NAME}. */
   RUN bortinaktiv_UI.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disablabort_UI WINDOW-3 
PROCEDURE bortinaktiv_UI :
   BTN_BORT2:VISIBLE IN FRAME FRAME-C = FALSE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disablabort_UI WINDOW-3 
PROCEDURE bortaktiv_UI :
   BTN_BORT2:VISIBLE IN FRAME FRAME-C = TRUE.   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
