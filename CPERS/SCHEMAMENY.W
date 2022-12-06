&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME DIALOG-2





&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS DIALOG-2 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 95/06/14 -  1:24 pm

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{ALLDEF.I}
{GLOBVAR2DEL1.I}

&Scoped-define NEW
&Scoped-define SHARED SHARED
{DHMT.I}        
{RHMT.I}
{VHMT.I}
{REGVAR.I}
{PHMT.I}
{HOPALLA.I}
DEFINE NEW SHARED VARIABLE schproch AS HANDLE NO-UNDO.   /* SCHAPP.P */
DEFINE NEW SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE nummervar AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE vecosrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE klocka AS DECIMAL NO-UNDO. 
DEFINE VARIABLE dagtotkoll AS LOGICAL NO-UNDO.
DEFINE VARIABLE str AS CHARACTER FORMAT "X(80)" NO-UNDO.
DEFINE VARIABLE sekkaffestarten AS INTEGER NO-UNDO.
DEFINE VARIABLE sekkaffeslutet AS INTEGER NO-UNDO.
DEFINE VARIABLE sekfrukoststarten AS INTEGER NO-UNDO.
DEFINE VARIABLE sekfrukostslutet AS INTEGER NO-UNDO.
DEFINE VARIABLE seklunchstarten AS INTEGER NO-UNDO.
DEFINE VARIABLE seklunchslutet AS INTEGER NO-UNDO.
DEFINE VARIABLE sekslut AS INTEGER NO-UNDO.
DEFINE VARIABLE sekstart AS INTEGER NO-UNDO.
DEFINE VARIABLE lunchen AS INTEGER NO-UNDO.
DEFINE VARIABLE frukost AS INTEGER NO-UNDO.
DEFINE VARIABLE rasten AS INTEGER NO-UNDO.
DEFINE VARIABLE tidsummasek AS INTEGER NO-UNDO.
DEFINE VARIABLE totdagsek AS INTEGER NO-UNDO.
DEFINE VARIABLE felmedd AS CHARACTER NO-UNDO.
DEFINE VARIABLE orgpos AS INTEGER NO-UNDO.
DEFINE VARIABLE rulid AS INTEGER NO-UNDO.
DEFINE VARIABLE ordnvar AS INTEGER NO-UNDO.
DEFINE VARIABLE antal_valda AS INTEGER NO-UNDO.
DEFINE VARIABLE antal_raknare AS INTEGER NO-UNDO.
DEFINE VARIABLE temp_fran AS INTEGER NO-UNDO.
DEFINE VARIABLE temp_till AS INTEGER NO-UNDO.
DEFINE VARIABLE andra AS LOGICAL NO-UNDO.
DEFINE VARIABLE lastrulid AS INTEGER NO-UNDO.
DEFINE VARIABLE kollpersmedd AS CHARACTER NO-UNDO.
DEFINE VARIABLE radspar AS INTEGER NO-UNDO.

{TIDUTTTNEW.I}  
DEFINE BUFFER arbtidbuff FOR arbtidtemp.
DEFINE BUFFER arbma FOR arbtidtemp.
DEFINE BUFFER arbti FOR arbtidtemp.
DEFINE BUFFER arbon FOR arbtidtemp.
DEFINE BUFFER arbto FOR arbtidtemp.
DEFINE BUFFER arbfr FOR arbtidtemp.
DEFINE BUFFER arblo FOR arbtidtemp.
DEFINE BUFFER arbso FOR arbtidtemp.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DIALOG-2
&Scoped-define BROWSE-NAME BRW_DAG

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES arbtidtemp rullvectemp veckoarbtemp

/* Definitions for BROWSE BRW_DAG                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_DAG arbtidtemp.ARBTIDKOD ~
arbtidtemp.START arbtidtemp.FRUKOSTSTART arbtidtemp.FRUKOSTSLUT ~
arbtidtemp.LUNCHSTART arbtidtemp.LUNCHSLUT arbtidtemp.KAFESTART ~
arbtidtemp.KAFESLUT arbtidtemp.SLUT arbtidtemp.TOTDAGTID 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_DAG arbtidtemp.ARBTIDKOD ~
arbtidtemp.START arbtidtemp.FRUKOSTSTART arbtidtemp.FRUKOSTSLUT ~
arbtidtemp.LUNCHSTART arbtidtemp.LUNCHSLUT arbtidtemp.KAFESTART ~
arbtidtemp.KAFESLUT arbtidtemp.SLUT 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_DAG arbtidtemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_DAG arbtidtemp
&Scoped-define QUERY-STRING-BRW_DAG FOR EACH arbtidtemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_DAG OPEN QUERY BRW_DAG FOR EACH arbtidtemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_DAG arbtidtemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_DAG arbtidtemp


/* Definitions for BROWSE BRW_RULLVEC                                   */
&Scoped-define FIELDS-IN-QUERY-BRW_RULLVEC rullvectemp.VECKOSCHEMA 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_RULLVEC 
&Scoped-define QUERY-STRING-BRW_RULLVEC FOR EACH rullvectemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_RULLVEC OPEN QUERY BRW_RULLVEC FOR EACH rullvectemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_RULLVEC rullvectemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_RULLVEC rullvectemp


/* Definitions for BROWSE BRW_VEC                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_VEC veckoarbtemp.VECKOSCHEMA 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_VEC 
&Scoped-define QUERY-STRING-BRW_VEC FOR EACH veckoarbtemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_VEC OPEN QUERY BRW_VEC FOR EACH veckoarbtemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_VEC veckoarbtemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_VEC veckoarbtemp


/* Definitions for FRAME FRAME-DAG                                      */

/* Definitions for FRAME FRAME-VECRULL                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS MBTN_DAG MBTN_VEC MBTN_RUL RECT-24 RAD_VAL ~
BTN_SKR BTN_VEVISA BTN_AVB 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB 
     LABEL "Avsluta":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_SKR 
     LABEL "Skriv ut":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_VEVISA 
     LABEL "V-Visa" 
     SIZE 14 BY 1.

DEFINE BUTTON MBTN_DAG 
     LABEL "Dagschema" 
     SIZE 14 BY 1.

DEFINE BUTTON MBTN_RUL 
     LABEL "Rullande schema" 
     SIZE 18 BY 1.

DEFINE BUTTON MBTN_VEC 
     LABEL "Veckoschema" 
     SIZE 14 BY 1.

DEFINE VARIABLE RAD_VAL AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Dagschema", 1,
"Veckoschema", 2,
"Rullande schema", 3
     SIZE 31.88 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-24
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 118.75 BY .08.

DEFINE BUTTON BTN_BORT-2 
     LABEL "Ta bort":L 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_NY-2 
     LABEL "Ny":L 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_SSPARA-2 
     LABEL "Snabbspara" 
     SIZE 12 BY 1.

DEFINE VARIABLE FILL-TEXT-DAG AS CHARACTER FORMAT "X(256)":U INITIAL "Ändra dagschema" 
      VIEW-AS TEXT 
     SIZE 28 BY 1.21
     FONT 17 NO-UNDO.

DEFINE BUTTON BTN_AND 
     LABEL "Ändra":L 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_BACK 
     IMAGE-UP FILE "BILDER\prev-u.bmp":U
     LABEL "" 
     SIZE 4 BY 1.21.

DEFINE BUTTON BTN_BORT 
     LABEL "Ta bort":L 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_NER 
     IMAGE-UP FILE "BILDER\pilner.bmp":U
     LABEL "" 
     SIZE 4 BY 1.5.

DEFINE BUTTON BTN_NY 
     LABEL "Ny":L 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_OVER 
     IMAGE-UP FILE "BILDER\next-u.bmp":U
     LABEL "" 
     SIZE 4 BY 1.21.

DEFINE BUTTON BTN_UPP-2 
     IMAGE-UP FILE "BILDER\pilupp.bmp":U
     LABEL "" 
     SIZE 4 BY 1.5.

DEFINE VARIABLE CMB_RUL AS INTEGER FORMAT "99":U INITIAL 0 
     LABEL "Rullande schema" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "0" 
     DROP-DOWN-LIST
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-BORJ1 AS DECIMAL FORMAT "99.99":U INITIAL 0 
     LABEL "Börjar" 
      VIEW-AS TEXT 
     SIZE 7 BY .63 NO-UNDO.

DEFINE VARIABLE FILL-IN-BORJ2 AS DECIMAL FORMAT "99.99":U INITIAL 0 
      VIEW-AS TEXT 
     SIZE 7 BY .63 NO-UNDO.

DEFINE VARIABLE FILL-IN-BORJ3 AS DECIMAL FORMAT "99.99":U INITIAL 0 
      VIEW-AS TEXT 
     SIZE 7 BY .63 NO-UNDO.

DEFINE VARIABLE FILL-IN-BORJ4 AS DECIMAL FORMAT "99.99":U INITIAL 0 
      VIEW-AS TEXT 
     SIZE 7 BY .63 NO-UNDO.

DEFINE VARIABLE FILL-IN-BORJ5 AS DECIMAL FORMAT "99.99":U INITIAL 0 
      VIEW-AS TEXT 
     SIZE 7 BY .63 NO-UNDO.

DEFINE VARIABLE FILL-IN-BORJ6 AS DECIMAL FORMAT "99.99":U INITIAL 0 
      VIEW-AS TEXT 
     SIZE 7 BY .63 NO-UNDO.

DEFINE VARIABLE FILL-IN-BORJ7 AS DECIMAL FORMAT "99.99":U INITIAL 0 
      VIEW-AS TEXT 
     SIZE 7 BY .63 NO-UNDO.

DEFINE VARIABLE FILL-IN-DSCH1 AS INTEGER FORMAT ">99":U INITIAL 0 
     LABEL "Dagschema" 
      VIEW-AS TEXT 
     SIZE 4.5 BY .63 NO-UNDO.

DEFINE VARIABLE FILL-IN-DSCH2 AS INTEGER FORMAT ">99":U INITIAL 0 
      VIEW-AS TEXT 
     SIZE 4.5 BY .63 NO-UNDO.

DEFINE VARIABLE FILL-IN-DSCH3 AS INTEGER FORMAT ">99":U INITIAL 0 
      VIEW-AS TEXT 
     SIZE 4.5 BY .63 NO-UNDO.

DEFINE VARIABLE FILL-IN-DSCH4 AS INTEGER FORMAT ">99":U INITIAL 0 
      VIEW-AS TEXT 
     SIZE 4 BY .63 NO-UNDO.

DEFINE VARIABLE FILL-IN-DSCH5 AS INTEGER FORMAT ">99":U INITIAL 0 
      VIEW-AS TEXT 
     SIZE 4.5 BY .63 NO-UNDO.

DEFINE VARIABLE FILL-IN-DSCH6 AS INTEGER FORMAT ">99":U INITIAL 0 
      VIEW-AS TEXT 
     SIZE 4.5 BY .63 NO-UNDO.

DEFINE VARIABLE FILL-IN-DSCH7 AS INTEGER FORMAT ">99":U INITIAL 0 
      VIEW-AS TEXT 
     SIZE 4.5 BY .63 NO-UNDO.

DEFINE VARIABLE FILL-IN-SLU1 AS DECIMAL FORMAT "99.99":U INITIAL 0 
     LABEL "Slutar" 
      VIEW-AS TEXT 
     SIZE 7 BY .63 NO-UNDO.

DEFINE VARIABLE FILL-IN-SLU2 AS DECIMAL FORMAT "99.99":U INITIAL 0 
      VIEW-AS TEXT 
     SIZE 7 BY .63 NO-UNDO.

DEFINE VARIABLE FILL-IN-SLU3 AS DECIMAL FORMAT "99.99":U INITIAL 0 
      VIEW-AS TEXT 
     SIZE 7 BY .63 NO-UNDO.

DEFINE VARIABLE FILL-IN-SLU4 AS DECIMAL FORMAT "99.99":U INITIAL 0 
      VIEW-AS TEXT 
     SIZE 7 BY .63 NO-UNDO.

DEFINE VARIABLE FILL-IN-SLU5 AS DECIMAL FORMAT "99.99":U INITIAL 0 
      VIEW-AS TEXT 
     SIZE 7 BY .63 NO-UNDO.

DEFINE VARIABLE FILL-IN-SLU6 AS DECIMAL FORMAT "99.99":U INITIAL 0 
      VIEW-AS TEXT 
     SIZE 7 BY .63 NO-UNDO.

DEFINE VARIABLE FILL-IN-SLU7 AS DECIMAL FORMAT "99.99":U INITIAL 0 
      VIEW-AS TEXT 
     SIZE 7 BY .63 NO-UNDO.

DEFINE VARIABLE FILL-IN-TIM1 AS DECIMAL FORMAT "99.99":U INITIAL 0 
     LABEL "Tim/Dag" 
      VIEW-AS TEXT 
     SIZE 7 BY .63 NO-UNDO.

DEFINE VARIABLE FILL-IN-TIM2 AS DECIMAL FORMAT "99.99":U INITIAL 0 
      VIEW-AS TEXT 
     SIZE 7 BY .63 NO-UNDO.

DEFINE VARIABLE FILL-IN-TIM3 AS DECIMAL FORMAT "99.99":U INITIAL 0 
      VIEW-AS TEXT 
     SIZE 7 BY .63 NO-UNDO.

DEFINE VARIABLE FILL-IN-TIM4 AS DECIMAL FORMAT "99.99":U INITIAL 0 
      VIEW-AS TEXT 
     SIZE 7 BY .63 NO-UNDO.

DEFINE VARIABLE FILL-IN-TIM5 AS DECIMAL FORMAT "99.99":U INITIAL 0 
      VIEW-AS TEXT 
     SIZE 7 BY .63 NO-UNDO.

DEFINE VARIABLE FILL-IN-TIM6 AS DECIMAL FORMAT "99.99":U INITIAL 0 
      VIEW-AS TEXT 
     SIZE 7 BY .63 NO-UNDO.

DEFINE VARIABLE FILL-IN-TIM7 AS DECIMAL FORMAT "99.99":U INITIAL 0 
      VIEW-AS TEXT 
     SIZE 7 BY .63 NO-UNDO.

DEFINE VARIABLE FILL-IN-TOTV AS DECIMAL FORMAT "99.99":U INITIAL 0 
      VIEW-AS TEXT 
     SIZE 7 BY .63 NO-UNDO.

DEFINE VARIABLE FILL-IN-VECO1 AS INTEGER FORMAT ">99":U INITIAL 0 
      VIEW-AS TEXT 
     SIZE 4.5 BY .63
     FGCOLOR 4  NO-UNDO.

DEFINE VARIABLE FILL-IN_TEXT AS CHARACTER FORMAT "X(256)":U 
     LABEL "Benämning" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-TEXT AS CHARACTER FORMAT "X(256)":U INITIAL "Ändra dagschema" 
      VIEW-AS TEXT 
     SIZE 28 BY 1.21
     FONT 17 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_DAG FOR 
      arbtidtemp SCROLLING.

DEFINE QUERY BRW_RULLVEC FOR 
      rullvectemp SCROLLING.

DEFINE QUERY BRW_VEC FOR 
      veckoarbtemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_DAG
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_DAG DIALOG-2 _STRUCTURED
  QUERY BRW_DAG NO-LOCK DISPLAY
      arbtidtemp.ARBTIDKOD COLUMN-LABEL "Dag!schema" FORMAT ">99":U
      arbtidtemp.START COLUMN-LABEL "Arbetets!början" FORMAT "99.99":U
      arbtidtemp.FRUKOSTSTART COLUMN-LABEL "Frukost!börjar" FORMAT "99.99":U
      arbtidtemp.FRUKOSTSLUT COLUMN-LABEL "Frukost!slutar" FORMAT "99.99":U
      arbtidtemp.LUNCHSTART COLUMN-LABEL " Lunch!börjar" FORMAT "99.99":U
      arbtidtemp.LUNCHSLUT COLUMN-LABEL "Lunch!slutar" FORMAT "99.99":U
      arbtidtemp.KAFESTART COLUMN-LABEL "Kafferast!börjar" FORMAT "99.99":U
      arbtidtemp.KAFESLUT COLUMN-LABEL "Kafferast!slutar" FORMAT "99.99":U
      arbtidtemp.SLUT COLUMN-LABEL "Arbetets!slut" FORMAT "99.99":U
      arbtidtemp.TOTDAGTID COLUMN-LABEL "Tot arb.!tid" FORMAT "99.99":U
  ENABLE
      arbtidtemp.ARBTIDKOD
      arbtidtemp.START
      arbtidtemp.FRUKOSTSTART
      arbtidtemp.FRUKOSTSLUT
      arbtidtemp.LUNCHSTART
      arbtidtemp.LUNCHSLUT
      arbtidtemp.KAFESTART
      arbtidtemp.KAFESLUT
      arbtidtemp.SLUT
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SIZE 84 BY 23.42
         TITLE "Dagschema".

DEFINE BROWSE BRW_RULLVEC
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_RULLVEC DIALOG-2 _STRUCTURED
  QUERY BRW_RULLVEC NO-LOCK DISPLAY
      rullvectemp.VECKOSCHEMA COLUMN-LABEL "Veckoschema" FORMAT ">99":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING MULTIPLE SIZE 14.5 BY 13
         TITLE "Valda".

DEFINE BROWSE BRW_VEC
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_VEC DIALOG-2 _STRUCTURED
  QUERY BRW_VEC NO-LOCK DISPLAY
      veckoarbtemp.VECKOSCHEMA COLUMN-LABEL "Veckoschema" FORMAT ">99":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING MULTIPLE SIZE 14.5 BY 13
         TITLE "Tillgängliga".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-2
     MBTN_DAG AT ROW 1.08 COL 1
     MBTN_VEC AT ROW 1.08 COL 15
     MBTN_RUL AT ROW 1.08 COL 29
     RAD_VAL AT ROW 1.08 COL 68.38 NO-LABEL
     BTN_SKR AT ROW 5.75 COL 102.88
     BTN_VEVISA AT ROW 8.75 COL 102.88 WIDGET-ID 2
     BTN_AVB AT ROW 27.88 COL 102.88
     RECT-24 AT ROW 2.08 COL 1
     SPACE(0.00) SKIP(27.25)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Skapa eller ändra scheman":L.

DEFINE FRAME FRAME-DAG
     BRW_DAG AT ROW 2.58 COL 1.5
     BTN_NY-2 AT ROW 26.63 COL 16.5
     BTN_BORT-2 AT ROW 26.63 COL 33
     BTN_SSPARA-2 AT ROW 26.63 COL 49.5
     FILL-TEXT-DAG AT ROW 1.17 COL 1.5 NO-LABEL
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 2.38
         SIZE 100 BY 27.

DEFINE FRAME FRAME-VECRULL
     FILL-IN_TEXT AT ROW 1.75 COL 64.75 COLON-ALIGNED
     CMB_RUL AT ROW 1.79 COL 46.5 COLON-ALIGNED
     BRW_VEC AT ROW 12.33 COL 27
     BRW_RULLVEC AT ROW 12.33 COL 52
     BTN_UPP-2 AT ROW 14.92 COL 67.25
     BTN_OVER AT ROW 14.96 COL 44.63
     BTN_NER AT ROW 16.83 COL 67.25
     BTN_BACK AT ROW 16.88 COL 44.63
     BTN_NY AT ROW 26.54 COL 24.25
     BTN_AND AT ROW 26.54 COL 40.75
     BTN_BORT AT ROW 26.54 COL 57.25
     FILL-TEXT AT ROW 1.42 COL 1.5 NO-LABEL
     FILL-IN-DSCH1 AT ROW 7.75 COL 11.13 COLON-ALIGNED
     FILL-IN-DSCH2 AT ROW 7.75 COL 20.13 COLON-ALIGNED NO-LABEL
     FILL-IN-DSCH3 AT ROW 7.75 COL 29 COLON-ALIGNED NO-LABEL
     FILL-IN-DSCH4 AT ROW 7.75 COL 38.13 COLON-ALIGNED NO-LABEL
     FILL-IN-DSCH5 AT ROW 7.75 COL 47.13 COLON-ALIGNED NO-LABEL
     FILL-IN-DSCH6 AT ROW 7.75 COL 56.13 COLON-ALIGNED NO-LABEL
     FILL-IN-DSCH7 AT ROW 7.75 COL 65.13 COLON-ALIGNED NO-LABEL
     FILL-IN-TOTV AT ROW 7.75 COL 74.25 COLON-ALIGNED NO-LABEL
     FILL-IN-VECO1 AT ROW 7.75 COL 84.5 COLON-ALIGNED NO-LABEL
     FILL-IN-BORJ1 AT ROW 8.96 COL 11.13 COLON-ALIGNED
     FILL-IN-BORJ2 AT ROW 8.96 COL 20.13 COLON-ALIGNED NO-LABEL
     FILL-IN-BORJ3 AT ROW 8.96 COL 29.13 COLON-ALIGNED NO-LABEL
     FILL-IN-BORJ4 AT ROW 8.96 COL 38.13 COLON-ALIGNED NO-LABEL
     FILL-IN-BORJ5 AT ROW 8.96 COL 47.13 COLON-ALIGNED NO-LABEL
     FILL-IN-BORJ6 AT ROW 8.96 COL 56.13 COLON-ALIGNED NO-LABEL
     FILL-IN-BORJ7 AT ROW 8.96 COL 65.13 COLON-ALIGNED NO-LABEL
     FILL-IN-SLU1 AT ROW 10.17 COL 11.13 COLON-ALIGNED
     FILL-IN-SLU2 AT ROW 10.17 COL 20.13 COLON-ALIGNED NO-LABEL
     FILL-IN-SLU3 AT ROW 10.17 COL 29.13 COLON-ALIGNED NO-LABEL
     FILL-IN-SLU4 AT ROW 10.17 COL 38.13 COLON-ALIGNED NO-LABEL
     FILL-IN-SLU5 AT ROW 10.17 COL 47.13 COLON-ALIGNED NO-LABEL
     FILL-IN-SLU6 AT ROW 10.17 COL 56.13 COLON-ALIGNED NO-LABEL
     FILL-IN-SLU7 AT ROW 10.17 COL 65.13 COLON-ALIGNED NO-LABEL
     FILL-IN-TIM1 AT ROW 11.33 COL 11.13 COLON-ALIGNED
     FILL-IN-TIM2 AT ROW 11.33 COL 20.13 COLON-ALIGNED NO-LABEL
     FILL-IN-TIM3 AT ROW 11.33 COL 29.13 COLON-ALIGNED NO-LABEL
     FILL-IN-TIM4 AT ROW 11.33 COL 38.13 COLON-ALIGNED NO-LABEL
     FILL-IN-TIM5 AT ROW 11.33 COL 47.13 COLON-ALIGNED NO-LABEL
     FILL-IN-TIM6 AT ROW 11.33 COL 56.13 COLON-ALIGNED NO-LABEL
     FILL-IN-TIM7 AT ROW 11.33 COL 65.13 COLON-ALIGNED NO-LABEL
     "Tisdag" VIEW-AS TEXT
          SIZE 8 BY .63 AT ROW 6.75 COL 22.13
     "Tim/vecka" VIEW-AS TEXT
          SIZE 8 BY .63 AT ROW 6.75 COL 76.25
     "Lördag" VIEW-AS TEXT
          SIZE 8 BY .63 AT ROW 6.75 COL 58.13
     "Söndag" VIEW-AS TEXT
          SIZE 8 BY .63 AT ROW 6.75 COL 67.13
     "Fredag" VIEW-AS TEXT
          SIZE 8 BY .63 AT ROW 6.75 COL 49.13
     "Veckoschema" VIEW-AS TEXT
          SIZE 11.5 BY .63 AT ROW 6.75 COL 86.5
     "Torsdag" VIEW-AS TEXT
          SIZE 8 BY .63 AT ROW 6.75 COL 40.13
     "Måndag" VIEW-AS TEXT
          SIZE 8 BY .63 AT ROW 6.75 COL 13
     "Onsdag" VIEW-AS TEXT
          SIZE 8 BY .63 AT ROW 6.75 COL 31
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 2.38
         SIZE 100 BY 27.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Temp-Tables and Buffers:
      TABLE: arbtidtemp T "?" NO-UNDO temp-db arbtidtemp
      TABLE: rullvectemp T "?" NO-UNDO temp-db rullvectemp
      TABLE: veckoarbtemp T "?" NO-UNDO temp-db veckoarbtemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* REPARENT FRAME */
ASSIGN FRAME FRAME-DAG:FRAME = FRAME DIALOG-2:HANDLE
       FRAME FRAME-VECRULL:FRAME = FRAME DIALOG-2:HANDLE.

/* SETTINGS FOR DIALOG-BOX DIALOG-2
   NOT-VISIBLE FRAME-NAME                                               */
ASSIGN 
       FRAME DIALOG-2:SCROLLABLE       = FALSE
       FRAME DIALOG-2:HIDDEN           = TRUE.

ASSIGN 
       MBTN_DAG:HIDDEN IN FRAME DIALOG-2           = TRUE.

ASSIGN 
       MBTN_RUL:HIDDEN IN FRAME DIALOG-2           = TRUE.

ASSIGN 
       MBTN_VEC:HIDDEN IN FRAME DIALOG-2           = TRUE.

/* SETTINGS FOR RADIO-SET RAD_VAL IN FRAME DIALOG-2
   NO-DISPLAY                                                           */
ASSIGN 
       RAD_VAL:HIDDEN IN FRAME DIALOG-2           = TRUE.

/* SETTINGS FOR FRAME FRAME-DAG
                                                                        */
/* BROWSE-TAB BRW_DAG 1 FRAME-DAG */
/* SETTINGS FOR FILL-IN FILL-TEXT-DAG IN FRAME FRAME-DAG
   ALIGN-L                                                              */
/* SETTINGS FOR FRAME FRAME-VECRULL
                                                                        */
/* BROWSE-TAB BRW_VEC CMB_RUL FRAME-VECRULL */
/* BROWSE-TAB BRW_RULLVEC BRW_VEC FRAME-VECRULL */
/* SETTINGS FOR BUTTON BTN_BACK IN FRAME FRAME-VECRULL
   NO-ENABLE                                                            */
ASSIGN 
       BTN_BACK:HIDDEN IN FRAME FRAME-VECRULL           = TRUE.

/* SETTINGS FOR BUTTON BTN_OVER IN FRAME FRAME-VECRULL
   NO-ENABLE                                                            */
ASSIGN 
       BTN_OVER:HIDDEN IN FRAME FRAME-VECRULL           = TRUE.

ASSIGN 
       FILL-IN-BORJ5:HIDDEN IN FRAME FRAME-VECRULL           = TRUE.

/* SETTINGS FOR FILL-IN FILL-TEXT IN FRAME FRAME-VECRULL
   ALIGN-L                                                              */
ASSIGN 
       FILL-TEXT:READ-ONLY IN FRAME FRAME-VECRULL        = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_DAG
/* Query rebuild information for BROWSE BRW_DAG
     _TblList          = "Temp-Tables.arbtidtemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.arbtidtemp.ARBTIDKOD
"arbtidtemp.ARBTIDKOD" "Dag!schema" ">99" "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.arbtidtemp.START
"arbtidtemp.START" "Arbetets!början" ? "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.arbtidtemp.FRUKOSTSTART
"arbtidtemp.FRUKOSTSTART" "Frukost!börjar" ? "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.arbtidtemp.FRUKOSTSLUT
"arbtidtemp.FRUKOSTSLUT" "Frukost!slutar" ? "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.arbtidtemp.LUNCHSTART
"arbtidtemp.LUNCHSTART" " Lunch!börjar" ? "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > Temp-Tables.arbtidtemp.LUNCHSLUT
"arbtidtemp.LUNCHSLUT" "Lunch!slutar" ? "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > Temp-Tables.arbtidtemp.KAFESTART
"arbtidtemp.KAFESTART" "Kafferast!börjar" ? "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > Temp-Tables.arbtidtemp.KAFESLUT
"arbtidtemp.KAFESLUT" "Kafferast!slutar" ? "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > Temp-Tables.arbtidtemp.SLUT
"arbtidtemp.SLUT" "Arbetets!slut" ? "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > Temp-Tables.arbtidtemp.TOTDAGTID
"arbtidtemp.TOTDAGTID" "Tot arb.!tid" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_DAG */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_RULLVEC
/* Query rebuild information for BROWSE BRW_RULLVEC
     _TblList          = "Temp-Tables.rullvectemp"
     _Options          = "NO-LOCK "
     _FldNameList[1]   > Temp-Tables.rullvectemp.VECKOSCHEMA
"rullvectemp.VECKOSCHEMA" "Veckoschema" ">99" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_RULLVEC */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_VEC
/* Query rebuild information for BROWSE BRW_VEC
     _TblList          = "Temp-Tables.veckoarbtemp"
     _Options          = "NO-LOCK "
     _FldNameList[1]   > Temp-Tables.veckoarbtemp.VECKOSCHEMA
"veckoarbtemp.VECKOSCHEMA" "Veckoschema" ">99" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_VEC */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX DIALOG-2
/* Query rebuild information for DIALOG-BOX DIALOG-2
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX DIALOG-2 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME DIALOG-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-2 DIALOG-2
ON END-ERROR OF FRAME DIALOG-2 /* Skapa eller ändra scheman */
DO:
   FIND FIRST arbtidtemp WHERE arbtidtemp.TYP NE 0 NO-ERROR.
   IF AVAILABLE arbtidtemp OR andra = TRUE THEN DO:
      MESSAGE "Vill du spara alla förändringar innan du avslutar?"
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE val AS LOGICAL.
      IF val = TRUE AND AVAILABLE arbtidtemp THEN RUN sparadag_UI IN schproch (INPUT TABLE arbtidtemp).      
      IF andra = TRUE THEN DO:
        RUN spararull_UI IN schproch 
        (INPUT val, INPUT rulid, INPUT Guru.Konstanter:globanv, OUTPUT lastrulid,
         INPUT-OUTPUT TABLE rullschtemp, INPUT-OUTPUT TABLE rullvectemp).      
      END.
   END.   
   musz = TRUE.
   IF VALID-HANDLE(schproch) THEN DELETE PROCEDURE schproch.
   {BORTBRWPROC.I}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-2 DIALOG-2
ON ENDKEY OF FRAME DIALOG-2 /* Skapa eller ändra scheman */
DO:
   APPLY "END-ERROR":U TO FRAME DIALOG-2.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_DAG
&Scoped-define FRAME-NAME FRAME-DAG
&Scoped-define SELF-NAME BRW_DAG
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_DAG DIALOG-2
ON ROW-LEAVE OF BRW_DAG IN FRAME FRAME-DAG /* Dagschema */
DO:
   dagtotkoll = FALSE.
   RUN dagtot_UI.
   IF dagtotkoll = TRUE THEN DO:
      MESSAGE "Någon post är fel för totaltiden blir inte rätt!" VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
   dagtotkoll = FALSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_DAG DIALOG-2
ON VALUE-CHANGED OF BRW_DAG IN FRAME FRAME-DAG /* Dagschema */
DO:
   BRW_DAG:SELECT-FOCUSED-ROW() NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME arbtidtemp.ARBTIDKOD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL arbtidtemp.ARBTIDKOD BRW_DAG _BROWSE-COLUMN DIALOG-2
ON ENTRY OF arbtidtemp.ARBTIDKOD IN BROWSE BRW_DAG /* Dag!schema */
DO:
   DISPLAY arbtidtemp.ARBTIDKOD WITH BROWSE {&BROWSE-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL arbtidtemp.ARBTIDKOD BRW_DAG _BROWSE-COLUMN DIALOG-2
ON LEAVE OF arbtidtemp.ARBTIDKOD IN BROWSE BRW_DAG /* Dag!schema */
DO:
   IF arbtidtemp.TYP = 0 THEN DO:
      arbtidtemp.ARBTIDKOD = INPUT BROWSE {&BROWSE-NAME} arbtidtemp.ARBTIDKOD.
   END.     

   FIND FIRST arbtidbuff WHERE arbtidbuff.ARBTIDKOD = arbtidtemp.ARBTIDKOD AND
   RECID(arbtidbuff) NE RECID(arbtidtemp) USE-INDEX ARBTIDKOD NO-LOCK NO-ERROR.
   IF AVAILABLE arbtidbuff THEN DO:
      MESSAGE "Dagschema" arbtidtemp.ARBTIDKOD " finns redan upplagt!" VIEW-AS ALERT-BOX. 
      RETURN NO-APPLY.         
   END.
   ASSIGN
   arbtidtemp.TYP = 2
   arbtidtemp.ARBTIDKOD = INPUT arbtidtemp.ARBTIDKOD
   arbtidtemp.ARBTIDKOD:READ-ONLY IN BROWSE {&BROWSE-NAME} = TRUE.
   DISPLAY arbtidtemp.ARBTIDKOD WITH BROWSE {&BROWSE-NAME}.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME arbtidtemp.START
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL arbtidtemp.START BRW_DAG _BROWSE-COLUMN DIALOG-2
ON ENTRY OF arbtidtemp.START IN BROWSE BRW_DAG /* Arbetets!början */
DO:
  DISPLAY arbtidtemp.START WITH BROWSE {&BROWSE-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL arbtidtemp.START BRW_DAG _BROWSE-COLUMN DIALOG-2
ON LEAVE OF arbtidtemp.START IN BROWSE BRW_DAG /* Arbetets!början */
DO:
   IF arbtidtemp.START = INPUT BROWSE {&BROWSE-NAME} arbtidtemp.START THEN musz = musz.
   ELSE DO:
     IF INPUT BROWSE {&BROWSE-NAME} arbtidtemp.START < 0 OR INPUT BROWSE {&BROWSE-NAME} arbtidtemp.START > 24 THEN DO:
         MESSAGE "Fel tidangivelse!" VIEW-AS ALERT-BOX.
         DISPLAY arbtidtemp.START WITH BROWSE {&BROWSE-NAME}.
         RETURN NO-APPLY.
      END.

      ASSIGN
      arbtidtemp.TYP = 2
      arbtidtemp.START = INPUT BROWSE {&BROWSE-NAME} arbtidtemp.START.   
      DISPLAY arbtidtemp.START WITH BROWSE {&BROWSE-NAME}.

      RUN dagtot_UI.

   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL arbtidtemp.START BRW_DAG _BROWSE-COLUMN DIALOG-2
ON MOUSE-MENU-CLICK OF arbtidtemp.START IN BROWSE BRW_DAG /* Arbetets!början */
DO:
   klocka = INPUT BROWSE {&BROWSE-NAME} arbtidtemp.START.
   RUN klock_UI (INPUT 1).
   arbtidtemp.START = klocka.
   DISPLAY arbtidtemp.START WITH BROWSE {&BROWSE-NAME}.
   RUN dagtot_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME arbtidtemp.FRUKOSTSTART
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL arbtidtemp.FRUKOSTSTART BRW_DAG _BROWSE-COLUMN DIALOG-2
ON ENTRY OF arbtidtemp.FRUKOSTSTART IN BROWSE BRW_DAG /* Frukost!börjar */
DO:
   DISPLAY arbtidtemp.FRUKOSTSTART WITH BROWSE {&BROWSE-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL arbtidtemp.FRUKOSTSTART BRW_DAG _BROWSE-COLUMN DIALOG-2
ON LEAVE OF arbtidtemp.FRUKOSTSTART IN BROWSE BRW_DAG /* Frukost!börjar */
DO:
  IF arbtidtemp.FRUKOSTSTART = INPUT BROWSE {&BROWSE-NAME} arbtidtemp.FRUKOSTSTART THEN musz = musz.
   ELSE DO:
      IF INPUT BROWSE {&BROWSE-NAME} arbtidtemp.FRUKOSTSTART < 0 OR INPUT BROWSE {&BROWSE-NAME} arbtidtemp.FRUKOSTSTART > 24 THEN DO:
         MESSAGE "Fel tidangivelse!" VIEW-AS ALERT-BOX.
         DISPLAY arbtidtemp.FRUKOSTSTART WITH BROWSE {&BROWSE-NAME}.
         RETURN NO-APPLY.
      END.
      ASSIGN
      arbtidtemp.TYP = 2
      arbtidtemp.FRUKOSTSTART = INPUT BROWSE {&BROWSE-NAME} arbtidtemp.FRUKOSTSTART.   
      DISPLAY arbtidtemp.FRUKOSTSTART WITH BROWSE {&BROWSE-NAME}.
      RUN dagtot_UI.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL arbtidtemp.FRUKOSTSTART BRW_DAG _BROWSE-COLUMN DIALOG-2
ON MOUSE-MENU-CLICK OF arbtidtemp.FRUKOSTSTART IN BROWSE BRW_DAG /* Frukost!börjar */
DO:
   klocka = INPUT BROWSE {&BROWSE-NAME} arbtidtemp.FRUKOSTSTART.
   RUN klock_UI (INPUT 1).
   arbtidtemp.FRUKOSTSTART = klocka.
   DISPLAY arbtidtemp.FRUKOSTSTART WITH BROWSE {&BROWSE-NAME}.
   RUN dagtot_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME arbtidtemp.FRUKOSTSLUT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL arbtidtemp.FRUKOSTSLUT BRW_DAG _BROWSE-COLUMN DIALOG-2
ON ENTRY OF arbtidtemp.FRUKOSTSLUT IN BROWSE BRW_DAG /* Frukost!slutar */
DO:
   DISPLAY arbtidtemp.FRUKOSTSLUT WITH BROWSE {&BROWSE-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL arbtidtemp.FRUKOSTSLUT BRW_DAG _BROWSE-COLUMN DIALOG-2
ON LEAVE OF arbtidtemp.FRUKOSTSLUT IN BROWSE BRW_DAG /* Frukost!slutar */
DO:
   IF arbtidtemp.FRUKOSTSLUT = INPUT BROWSE {&BROWSE-NAME} arbtidtemp.FRUKOSTSLUT THEN musz = musz.
   ELSE DO:
      IF INPUT BROWSE {&BROWSE-NAME} arbtidtemp.FRUKOSTSLUT < 0 OR INPUT BROWSE {&BROWSE-NAME} arbtidtemp.FRUKOSTSLUT > 24 THEN DO:
         MESSAGE "Fel tidangivelse!" VIEW-AS ALERT-BOX.
         DISPLAY arbtidtemp.FRUKOSTSLUT WITH BROWSE {&BROWSE-NAME}.
         RETURN NO-APPLY.
      END.
      ASSIGN
      arbtidtemp.TYP = 2
      arbtidtemp.FRUKOSTSLUT = INPUT BROWSE {&BROWSE-NAME} arbtidtemp.FRUKOSTSLUT.   
      DISPLAY arbtidtemp.FRUKOSTSLUT WITH BROWSE {&BROWSE-NAME}.
      RUN dagtot_UI.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL arbtidtemp.FRUKOSTSLUT BRW_DAG _BROWSE-COLUMN DIALOG-2
ON MOUSE-MENU-CLICK OF arbtidtemp.FRUKOSTSLUT IN BROWSE BRW_DAG /* Frukost!slutar */
DO:
   klocka = INPUT BROWSE {&BROWSE-NAME} arbtidtemp.FRUKOSTSLUT.
   RUN klock_UI (INPUT 2).
   arbtidtemp.FRUKOSTSLUT = klocka.
   DISPLAY arbtidtemp.FRUKOSTSLUT WITH BROWSE {&BROWSE-NAME}.
   RUN dagtot_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME arbtidtemp.LUNCHSTART
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL arbtidtemp.LUNCHSTART BRW_DAG _BROWSE-COLUMN DIALOG-2
ON ENTRY OF arbtidtemp.LUNCHSTART IN BROWSE BRW_DAG /*  Lunch!börjar */
DO:
  DISPLAY arbtidtemp.LUNCHSTART WITH BROWSE {&BROWSE-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL arbtidtemp.LUNCHSTART BRW_DAG _BROWSE-COLUMN DIALOG-2
ON LEAVE OF arbtidtemp.LUNCHSTART IN BROWSE BRW_DAG /*  Lunch!börjar */
DO:
  IF arbtidtemp.LUNCHSTART = INPUT BROWSE {&BROWSE-NAME} arbtidtemp.LUNCHSTART THEN musz = musz.
   ELSE DO:
      IF INPUT BROWSE {&BROWSE-NAME} arbtidtemp.LUNCHSTART < 0 OR INPUT BROWSE {&BROWSE-NAME} arbtidtemp.LUNCHSTART > 24 THEN DO:
         MESSAGE "Fel tidangivelse!" VIEW-AS ALERT-BOX.
         DISPLAY arbtidtemp.LUNCHSTART WITH BROWSE {&BROWSE-NAME}.
         RETURN NO-APPLY.
      END.
      ASSIGN
      arbtidtemp.TYP = 2
      arbtidtemp.LUNCHSTART = INPUT BROWSE {&BROWSE-NAME} arbtidtemp.LUNCHSTART.   
      DISPLAY arbtidtemp.LUNCHSTART WITH BROWSE {&BROWSE-NAME}.
      RUN dagtot_UI.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL arbtidtemp.LUNCHSTART BRW_DAG _BROWSE-COLUMN DIALOG-2
ON MOUSE-MENU-CLICK OF arbtidtemp.LUNCHSTART IN BROWSE BRW_DAG /*  Lunch!börjar */
DO:
   klocka = INPUT BROWSE {&BROWSE-NAME} arbtidtemp.LUNCHSTART.
   RUN klock_UI (INPUT 1).
   arbtidtemp.LUNCHSTART = klocka.
   DISPLAY arbtidtemp.LUNCHSTART WITH BROWSE {&BROWSE-NAME}.
   RUN dagtot_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME arbtidtemp.LUNCHSLUT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL arbtidtemp.LUNCHSLUT BRW_DAG _BROWSE-COLUMN DIALOG-2
ON ENTRY OF arbtidtemp.LUNCHSLUT IN BROWSE BRW_DAG /* Lunch!slutar */
DO:
  DISPLAY arbtidtemp.LUNCHSLUT WITH BROWSE {&BROWSE-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL arbtidtemp.LUNCHSLUT BRW_DAG _BROWSE-COLUMN DIALOG-2
ON LEAVE OF arbtidtemp.LUNCHSLUT IN BROWSE BRW_DAG /* Lunch!slutar */
DO:
  IF arbtidtemp.LUNCHSLUT = INPUT BROWSE {&BROWSE-NAME} arbtidtemp.LUNCHSLUT THEN musz = musz.
   ELSE DO:
      IF INPUT BROWSE {&BROWSE-NAME} arbtidtemp.LUNCHSLUT < 0 OR INPUT BROWSE {&BROWSE-NAME} arbtidtemp.LUNCHSLUT > 24 THEN DO:
         MESSAGE "Fel tidangivelse!" VIEW-AS ALERT-BOX.
         DISPLAY arbtidtemp.LUNCHSLUT WITH BROWSE {&BROWSE-NAME}.
         RETURN NO-APPLY.
      END.
      ASSIGN
      arbtidtemp.TYP = 2
      arbtidtemp.LUNCHSLUT = INPUT BROWSE {&BROWSE-NAME} arbtidtemp.LUNCHSLUT.   
      DISPLAY arbtidtemp.LUNCHSLUT WITH BROWSE {&BROWSE-NAME}.
      RUN dagtot_UI.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL arbtidtemp.LUNCHSLUT BRW_DAG _BROWSE-COLUMN DIALOG-2
ON MOUSE-MENU-CLICK OF arbtidtemp.LUNCHSLUT IN BROWSE BRW_DAG /* Lunch!slutar */
DO:
   klocka = INPUT BROWSE {&BROWSE-NAME} arbtidtemp.LUNCHSLUT.
   RUN klock_UI (INPUT 2).
   arbtidtemp.LUNCHSLUT = klocka.
   DISPLAY arbtidtemp.LUNCHSLUT WITH BROWSE {&BROWSE-NAME}.
   RUN dagtot_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME arbtidtemp.KAFESTART
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL arbtidtemp.KAFESTART BRW_DAG _BROWSE-COLUMN DIALOG-2
ON ENTRY OF arbtidtemp.KAFESTART IN BROWSE BRW_DAG /* Kafferast!börjar */
DO:
  DISPLAY arbtidtemp.KAFESTART WITH BROWSE {&BROWSE-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL arbtidtemp.KAFESTART BRW_DAG _BROWSE-COLUMN DIALOG-2
ON LEAVE OF arbtidtemp.KAFESTART IN BROWSE BRW_DAG /* Kafferast!börjar */
DO:
   IF arbtidtemp.KAFESTART = INPUT BROWSE {&BROWSE-NAME} arbtidtemp.KAFESTART THEN musz = musz.
   ELSE DO:
      IF INPUT BROWSE {&BROWSE-NAME} arbtidtemp.KAFESTART < 0 OR INPUT BROWSE {&BROWSE-NAME} arbtidtemp.KAFESTART > 24 THEN DO:
         MESSAGE "Fel tidangivelse!" VIEW-AS ALERT-BOX.
         DISPLAY arbtidtemp.KAFESTART WITH BROWSE {&BROWSE-NAME}.
         RETURN NO-APPLY.
      END.
      ASSIGN
      arbtidtemp.TYP = 2
      arbtidtemp.KAFESTART = INPUT BROWSE {&BROWSE-NAME} arbtidtemp.KAFESTART.   
      DISPLAY arbtidtemp.KAFESTART WITH BROWSE {&BROWSE-NAME}.
      RUN dagtot_UI.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL arbtidtemp.KAFESTART BRW_DAG _BROWSE-COLUMN DIALOG-2
ON MOUSE-MENU-CLICK OF arbtidtemp.KAFESTART IN BROWSE BRW_DAG /* Kafferast!börjar */
DO:
   klocka = INPUT BROWSE {&BROWSE-NAME} arbtidtemp.KAFESTART.
   RUN klock_UI (INPUT 1).
   arbtidtemp.KAFESTART = klocka.
   DISPLAY arbtidtemp.KAFESTART WITH BROWSE {&BROWSE-NAME}.
   RUN dagtot_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME arbtidtemp.KAFESLUT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL arbtidtemp.KAFESLUT BRW_DAG _BROWSE-COLUMN DIALOG-2
ON ENTRY OF arbtidtemp.KAFESLUT IN BROWSE BRW_DAG /* Kafferast!slutar */
DO:
   DISPLAY arbtidtemp.KAFESLUT WITH BROWSE {&BROWSE-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL arbtidtemp.KAFESLUT BRW_DAG _BROWSE-COLUMN DIALOG-2
ON LEAVE OF arbtidtemp.KAFESLUT IN BROWSE BRW_DAG /* Kafferast!slutar */
DO:
   IF arbtidtemp.KAFESLUT = INPUT BROWSE {&BROWSE-NAME} arbtidtemp.KAFESLUT THEN musz = musz.
   ELSE DO:
      IF INPUT BROWSE {&BROWSE-NAME} arbtidtemp.KAFESLUT < 0 OR INPUT BROWSE {&BROWSE-NAME} arbtidtemp.KAFESLUT > 24 THEN DO:
         MESSAGE "Fel tidangivelse!" VIEW-AS ALERT-BOX.
         DISPLAY arbtidtemp.KAFESLUT WITH BROWSE {&BROWSE-NAME}.
         RETURN NO-APPLY.
      END.
      ASSIGN
      arbtidtemp.TYP = 2
      arbtidtemp.KAFESLUT = INPUT BROWSE {&BROWSE-NAME} arbtidtemp.KAFESLUT.   
      DISPLAY arbtidtemp.KAFESLUT WITH BROWSE {&BROWSE-NAME}.
      RUN dagtot_UI.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL arbtidtemp.KAFESLUT BRW_DAG _BROWSE-COLUMN DIALOG-2
ON MOUSE-MENU-CLICK OF arbtidtemp.KAFESLUT IN BROWSE BRW_DAG /* Kafferast!slutar */
DO:
   klocka = INPUT BROWSE {&BROWSE-NAME} arbtidtemp.KAFESLUT.
   RUN klock_UI (INPUT 2).
   arbtidtemp.KAFESLUT = klocka.
   DISPLAY arbtidtemp.KAFESLUT WITH BROWSE {&BROWSE-NAME}.
   RUN dagtot_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME arbtidtemp.SLUT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL arbtidtemp.SLUT BRW_DAG _BROWSE-COLUMN DIALOG-2
ON ENTRY OF arbtidtemp.SLUT IN BROWSE BRW_DAG /* Arbetets!slut */
DO:
  DISPLAY arbtidtemp.SLUT WITH BROWSE {&BROWSE-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL arbtidtemp.SLUT BRW_DAG _BROWSE-COLUMN DIALOG-2
ON LEAVE OF arbtidtemp.SLUT IN BROWSE BRW_DAG /* Arbetets!slut */
DO:
   IF arbtidtemp.SLUT = INPUT BROWSE {&BROWSE-NAME} arbtidtemp.SLUT THEN musz = musz.
   ELSE DO:
      IF INPUT BROWSE {&BROWSE-NAME} arbtidtemp.SLUT < 0 OR INPUT BROWSE {&BROWSE-NAME} arbtidtemp.SLUT > 24 THEN DO:
         MESSAGE "Fel tidangivelse!" VIEW-AS ALERT-BOX.
         DISPLAY arbtidtemp.SLUT WITH BROWSE {&BROWSE-NAME}.
         RETURN NO-APPLY.
      END.
      ASSIGN
      arbtidtemp.TYP = 2
      arbtidtemp.SLUT = INPUT BROWSE {&BROWSE-NAME} arbtidtemp.SLUT.   
      DISPLAY arbtidtemp.SLUT WITH BROWSE {&BROWSE-NAME}.
      RUN dagtot_UI.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL arbtidtemp.SLUT BRW_DAG _BROWSE-COLUMN DIALOG-2
ON MOUSE-MENU-CLICK OF arbtidtemp.SLUT IN BROWSE BRW_DAG /* Arbetets!slut */
DO:
   klocka = INPUT BROWSE {&BROWSE-NAME} arbtidtemp.SLUT.
   RUN klock_UI (INPUT 2).  
   arbtidtemp.SLUT = klocka.
   DISPLAY arbtidtemp.SLUT WITH BROWSE {&BROWSE-NAME}.
   RUN dagtot_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_RULLVEC
&Scoped-define FRAME-NAME FRAME-VECRULL
&Scoped-define SELF-NAME BRW_RULLVEC
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_RULLVEC DIALOG-2
ON VALUE-CHANGED OF BRW_RULLVEC IN FRAME FRAME-VECRULL /* Valda */
DO:
   IF AVAILABLE rullvectemp THEN DO:
      RUN update_UI (INPUT rullvectemp.VECKOSCHEMA).
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_VEC
&Scoped-define SELF-NAME BRW_VEC
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_VEC DIALOG-2
ON MOUSE-SELECT-DBLCLICK OF BRW_VEC IN FRAME FRAME-VECRULL /* Tillgängliga */
DO:
  APPLY "CHOOSE" TO BTN_OVER.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_VEC DIALOG-2
ON VALUE-CHANGED OF BRW_VEC IN FRAME FRAME-VECRULL /* Tillgängliga */
DO:
   IF AVAILABLE veckoarbtemp THEN DO:
      RUN update_UI (INPUT veckoarbtemp.VECKOSCHEMA).
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AND
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AND DIALOG-2
ON CHOOSE OF BTN_AND IN FRAME FRAME-VECRULL /* Ändra */
DO:
   IF RAD_VAL = 2 THEN DO:
      BRW_VEC:SELECT-FOCUSED-ROW() NO-ERROR.
      IF AVAILABLE veckoarbtemp THEN DO:
         IF veckoarbtemp.VECKOSCHEMA = 00 THEN
         MESSAGE "Det går inte att ändra veckoschema 00!" VIEW-AS ALERT-BOX.
         ELSE DO:
            vecosrec = RECID(veckoarbtemp).
            nummervar = veckoarbtemp.VECKOSCHEMA.
            {muswait.i}            
            RUN ANDVECOS.W (INPUT 2).
            IF musz = FALSE THEN DO:
               RUN openbdynspec_UI IN brwproc[2].
               FIND FIRST veckoarbtemp WHERE veckoarbtemp.VECKOSCHEMA = nummervar NO-LOCK NO-ERROR.
               RUN selectbyrowid_UI IN brwproc[2] (INPUT ROWID(veckoarbtemp)).
               RUN update_UI (INPUT nummervar).
            END.
            ELSE musz = FALSE.
            {musarrow.i}
         END.
      END.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DIALOG-2
&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB DIALOG-2
ON CHOOSE OF BTN_AVB IN FRAME DIALOG-2 /* Avsluta */
DO:
   APPLY "END-ERROR":U TO FRAME DIALOG-2.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-VECRULL
&Scoped-define SELF-NAME BTN_BACK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_BACK DIALOG-2
ON CHOOSE OF BTN_BACK IN FRAME FRAME-VECRULL
DO:
   IF kollpersmedd = "" THEN DO:
      antal_valda = BRW_RULLVEC:NUM-SELECTED-ROWS.
      IF antal_valda = 0 THEN DO:
         MESSAGE "Inget är markerat" VIEW-AS ALERT-BOX.      
      END.
      ELSE DO:
         andra = TRUE.
         antal_raknare = 1.
         DO WHILE antal_raknare LE antal_valda:
            BRW_RULLVEC:FETCH-SELECTED-ROW(antal_raknare).
            DELETE rullvectemp.
            antal_raknare = antal_raknare + 1.
         END.
         RUN openbdynspec_UI IN brwproc[3].
         GET NEXT BRW_RULLVEC.
      END.   
   END.
   ELSE MESSAGE kollpersmedd VIEW-AS ALERT-BOX TITLE "Meddelande!".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_BORT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_BORT DIALOG-2
ON CHOOSE OF BTN_BORT IN FRAME FRAME-VECRULL /* Ta bort */
DO:
   IF RAD_VAL = 2 THEN DO:
      BRW_VEC:SELECT-FOCUSED-ROW() NO-ERROR.
      IF AVAILABLE veckoarbtemp THEN DO:
         IF veckoarbtemp.VECKOSCHEMA = 00 THEN
         MESSAGE "Det går inte att ta bort veckoschema 00!" VIEW-AS ALERT-BOX.
         ELSE DO:
            MESSAGE "Vill du ta bort veckoschema " veckoarbtemp.VECKOSCHEMA "?"
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE val2 AS LOGICAL.        
            IF val2 = TRUE THEN DO:
               RUN tabortvec_UI IN schproch (INPUT veckoarbtemp.VECKOSCHEMA, OUTPUT felmedd).
               IF felmedd = "" THEN DO:
                  DELETE veckoarbtemp.
                  RUN selnextprevrow_UI IN brwproc[2].
                  RUN openbdynspec_UI IN brwproc[2].
                  RUN lastselectdyn_UI IN brwproc[2].
                  RUN update_UI (INPUT veckoarbtemp.VECKOSCHEMA).
               END.
               ELSE DO:
                  MESSAGE felmedd VIEW-AS ALERT-BOX TITLE "Meddelande".
               END.
            END.
         END.
      END.
   END.
   ELSE IF RAD_VAL = 3 THEN DO:
      IF AVAILABLE rullschtemp THEN DO:
         MESSAGE "Vill du ta bort rullande schema " rulid "?"
         VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE val3 AS LOGICAL.
         IF val3 = TRUE THEN DO:           
            RUN tabortrull_UI IN schproch (INPUT rulid, OUTPUT felmedd).
            IF felmedd = "" THEN DO:
               FOR EACH rullvectemp WHERE rullvectemp.RULLID = rulid NO-LOCK:
                  DELETE rullvectemp.
               END.
               FIND FIRST rullschtemp WHERE rullschtemp.RULLID = rulid NO-LOCK NO-ERROR.
               IF AVAILABLE rullschtemp THEN DELETE rullschtemp.
               CMB_RUL:DELETE(STRING(rulid)) IN FRAME FRAME-VECRULL.
               CMB_RUL:SCREEN-VALUE = CMB_RUL:ENTRY(CMB_RUL:NUM-ITEMS) IN FRAME FRAME-VECRULL.
               rulid = INTEGER(CMB_RUL:SCREEN-VALUE).
               RUN rullchange_UI.
            END.
            ELSE MESSAGE felmedd VIEW-AS ALERT-BOX TITLE "Meddelande!".
         END.
      END.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-DAG
&Scoped-define SELF-NAME BTN_BORT-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_BORT-2 DIALOG-2
ON CHOOSE OF BTN_BORT-2 IN FRAME FRAME-DAG /* Ta bort */
DO:
   BRW_DAG:SELECT-FOCUSED-ROW() NO-ERROR.
   FIND FIRST arbtidtemp WHERE arbtidtemp.ARBTIDKOD = INPUT BROWSE BRW_DAG arbtidtemp.ARBTIDKOD NO-LOCK NO-ERROR.
   IF AVAILABLE arbtidtemp THEN DO:
      IF arbtidtemp.ARBTIDKOD = 00 THEN DO:
         MESSAGE "Det går inte att ta bort arbetstidschema 00!" VIEW-AS ALERT-BOX.      
      END.
      ELSE DO:
         MESSAGE "Vill verkligen ta bort denna post ?"
         VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE val AS LOGICAL.
         IF val = TRUE THEN DO:
            RUN tabortdag_UI IN schproch (INPUT arbtidtemp.ARBTIDKOD, OUTPUT felmedd).
            IF felmedd = "" THEN DO:
               DELETE arbtidtemp.
               RUN selnextprevrow_UI IN brwproc[1].
               RUN openbdynspec_UI IN brwproc[1].
               RUN lastselectdyn_UI IN brwproc[1].
            END.
            ELSE DO:
               MESSAGE felmedd VIEW-AS ALERT-BOX TITLE "Meddelande".
            END.
         END.
      END.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-VECRULL
&Scoped-define SELF-NAME BTN_NER
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NER DIALOG-2
ON CHOOSE OF BTN_NER IN FRAME FRAME-VECRULL
DO:
   IF kollpersmedd = "" THEN RUN piluppner_UI (INPUT FALSE).   
   ELSE MESSAGE kollpersmedd VIEW-AS ALERT-BOX TITLE "Meddelande!".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NY
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NY DIALOG-2
ON CHOOSE OF BTN_NY IN FRAME FRAME-VECRULL /* Ny */
DO:
   IF RAD_VAL = 2 THEN DO:
      CREATE veckoarbtemp.
      vecosrec = RECID(veckoarbtemp).
      nummervar = 0.
      {muswait.i}
      RUN ANDVECOS.W (INPUT 1).
      {musarrow.i}
      IF musz = FALSE THEN DO:
         RUN openbdynspec_UI IN brwproc[2].
         FIND FIRST veckoarbtemp WHERE veckoarbtemp.VECKOSCHEMA = nummervar NO-LOCK NO-ERROR.
         RUN selectbyrowid_UI IN brwproc[2] (INPUT ROWID(veckoarbtemp)).
         RUN update_UI (INPUT nummervar).         
      END.
      ELSE DO:
         FIND FIRST veckoarbtemp WHERE RECID(veckoarbtemp) = vecosrec NO-LOCK NO-ERROR.
         IF AVAILABLE veckoarbtemp THEN DELETE veckoarbtemp.
      END.
      musz = FALSE.
   END.
   ELSE IF RAD_VAL = 3 THEN DO:
      RUN skaparull_UI IN schproch (OUTPUT rulid).
      FIND FIRST rullschtemp WHERE rullschtemp.RULLID = rulid NO-LOCK NO-ERROR.
      IF NOT AVAILABLE rullschtemp THEN DO:
         CREATE rullschtemp.
         rullschtemp.RULLID = rulid.
      END.
      CMB_RUL:ADD-LAST(STRING(rulid,"99")).
      ASSIGN
      kollpersmedd = ""
      FILL-IN_TEXT:SCREEN-VALUE = ""
      CMB_RUL:SCREEN-VALUE = STRING(rulid,"99").
      RUN rullchange_UI.
      APPLY "ENTRY" TO FILL-IN_TEXT IN FRAME FRAME-VECRULL.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-DAG
&Scoped-define SELF-NAME BTN_NY-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NY-2 DIALOG-2
ON CHOOSE OF BTN_NY-2 IN FRAME FRAME-DAG /* Ny */
DO:    
   CREATE arbtidtemp.
   RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(arbtidtemp)).
   RUN openbdynspec_UI IN brwproc[1].
   RUN lastselectdyn_UI IN brwproc[1].
   arbtidtemp.ARBTIDKOD:READ-ONLY IN BROWSE BRW_DAG = FALSE.
   APPLY "ENTRY" TO arbtidtemp.ARBTIDKOD IN BROWSE BRW_DAG.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-VECRULL
&Scoped-define SELF-NAME BTN_OVER
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OVER DIALOG-2
ON CHOOSE OF BTN_OVER IN FRAME FRAME-VECRULL
DO:
   IF kollpersmedd = "" THEN RUN valda_UI.
   ELSE MESSAGE kollpersmedd VIEW-AS ALERT-BOX TITLE "Meddelande!".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DIALOG-2
&Scoped-define SELF-NAME BTN_SKR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_SKR DIALOG-2
ON CHOOSE OF BTN_SKR IN FRAME DIALOG-2 /* Skriv ut */
DO:    
   {muswait.i}
   IF RAD_VAL = 1 THEN DO:   
      RUN ut_UI.
      RUN SKRIVVAL.W (INPUT FALSE).
      IF musz = TRUE THEN musz = FALSE.
      ELSE DO:
         RUN EKLOGS.P.
      END. 
   END.     
   IF RAD_VAL = 2 THEN DO:
      skrivut = TRUE.
      RUN SKRIVVAL.W (INPUT FALSE).
      IF musz = TRUE THEN DO:
         musz = FALSE.
         skrivut = FALSE.
      END.
      RUN ut_UI.   
      skrivut = FALSE.
   END.   
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_SKR DIALOG-2
ON MOUSE-MENU-CLICK OF BTN_SKR IN FRAME DIALOG-2 /* Skriv ut */
DO:
   RUN SIDLANGD.W.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-DAG
&Scoped-define SELF-NAME BTN_SSPARA-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_SSPARA-2 DIALOG-2
ON CHOOSE OF BTN_SSPARA-2 IN FRAME FRAME-DAG /* Snabbspara */
DO:
   RUN sparadag_UI IN schproch (INPUT TABLE arbtidtemp).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-VECRULL
&Scoped-define SELF-NAME BTN_UPP-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_UPP-2 DIALOG-2
ON CHOOSE OF BTN_UPP-2 IN FRAME FRAME-VECRULL
DO:
   IF kollpersmedd = "" THEN RUN piluppner_UI (INPUT TRUE).   
   ELSE MESSAGE kollpersmedd VIEW-AS ALERT-BOX TITLE "Meddelande!".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DIALOG-2
&Scoped-define SELF-NAME BTN_VEVISA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_VEVISA DIALOG-2
ON CHOOSE OF BTN_VEVISA IN FRAME DIALOG-2 /* Visa */
DO:
   EMPTY TEMP-TABLE earbtidtemp NO-ERROR.
   EMPTY TEMP-TABLE erullvectemp NO-ERROR. 
   IF RAD_VAL = 1 THEN DO:     
      IF AVAILABLE  arbtidtemp THEN DO:
         CREATE   earbtidtemp.
         BUFFER-COPY arbtidtemp TO earbtidtemp.
      END.
  END.    
  IF RAD_VAL = 3 THEN DO:     
      IF AVAILABLE  rullschtemp THEN DO:
         CREATE erullschtemp.
         BUFFER-COPY rullschtemp TO erullschtemp.
         FOR EACH rullvectemp WHERE rullvectemp.RULLID = rullschtemp.RULLID: 
            CREATE   erullvectemp.
            BUFFER-COPY rullvectemp TO erullvectemp.
         END.   
      END.
  END.        
  RUN VECKOVIS.W (INPUT RAD_VAL,INPUT TABLE earbtidtemp,INPUT TABLE erullschtemp,INPUT TABLE erullvectemp,INPUT TABLE arbtidtemp, INPUT TABLE veckoarbtemp).
     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-VECRULL
&Scoped-define SELF-NAME CMB_RUL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_RUL DIALOG-2
ON VALUE-CHANGED OF CMB_RUL IN FRAME FRAME-VECRULL /* Rullande schema */
DO:
   IF andra = TRUE THEN DO:
      MESSAGE "Vill du spara förändringar för rullande schema " rulid "?"
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE val4 AS LOGICAL.
      RUN spararull_UI IN schproch 
      (INPUT val4, INPUT rulid, INPUT Guru.Konstanter:globanv, OUTPUT lastrulid,
      INPUT-OUTPUT TABLE rullschtemp, INPUT-OUTPUT TABLE rullvectemp).
      IF lastrulid NE ? THEN DO:
         CMB_RUL:DELETE(STRING(lastrulid)) IN FRAME FRAME-VECRULL.
         CMB_RUL:SCREEN-VALUE = CMB_RUL:ENTRY(CMB_RUL:NUM-ITEMS) IN FRAME FRAME-VECRULL.         
      END.
      andra = FALSE.
   END.
   CMB_RUL = INPUT CMB_RUL.
   rulid = CMB_RUL.
   RUN kollpers_UI IN schproch (INPUT rulid, OUTPUT kollpersmedd).
   RUN rullchange_UI.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_TEXT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_TEXT DIALOG-2
ON LEAVE OF FILL-IN_TEXT IN FRAME FRAME-VECRULL /* Benämning */
DO:
   FILL-IN_TEXT = INPUT FILL-IN_TEXT.
   IF AVAILABLE rullschtemp THEN DO:
      rullschtemp.BENAMNING = FILL-IN_TEXT.
      andra = TRUE.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DIALOG-2
&Scoped-define SELF-NAME MBTN_DAG
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL MBTN_DAG DIALOG-2
ON CHOOSE OF MBTN_DAG IN FRAME DIALOG-2 /* Dagschema */
DO:
  RAD_VAL = 1.
  APPLY "VALUE-CHANGED" TO RAD_VAL.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME MBTN_RUL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL MBTN_RUL DIALOG-2
ON CHOOSE OF MBTN_RUL IN FRAME DIALOG-2 /* Rullande schema */
DO:
  RAD_VAL = 3.
  APPLY "VALUE-CHANGED" TO RAD_VAL.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME MBTN_VEC
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL MBTN_VEC DIALOG-2
ON CHOOSE OF MBTN_VEC IN FRAME DIALOG-2 /* Veckoschema */
DO:
   RAD_VAL = 2.
   APPLY "VALUE-CHANGED" TO RAD_VAL.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RAD_VAL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RAD_VAL DIALOG-2
ON VALUE-CHANGED OF RAD_VAL IN FRAME DIALOG-2
DO:
/*    RAD_VAL = INPUT RAD_VAL. */
   RUN radval_UI (INPUT RAD_VAL).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_DAG
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK DIALOG-2 


/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.

/* Add Trigger to equate WINDOW-CLOSE to END-ERROR                      */
ON WINDOW-CLOSE OF FRAME {&FRAME-NAME} APPLY "END-ERROR":U TO SELF.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
   &Scoped-define FRAME-NAME DIALOG-2
   {DIA_M_START.I}
   {ALLSTARTDYN.I}      
   rulid = ?.
   FOR EACH rullschtemp :
      CMB_RUL:ADD-LAST(STRING(rullschtemp.RULLID,"99")). 
      rulid = rullschtemp.RULLID.
   END.
   ASSIGN
   MBTN_DAG:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
   MBTN_VEC:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
   MBTN_RUL:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
   RUN enable_UI. 
   RUN openbdynspec_UI IN brwproc[1].
   RUN openbdynspec_UI IN brwproc[2].
   CMB_RUL:DELETE("00") IN FRAME FRAME-VECRULL.
   CMB_RUL:SCREEN-VALUE = STRING(rulid,"99").
   RUN rullchange_UI.
   ASSIGN
   orgpos = BRW_VEC:X IN FRAME FRAME-VECRULL
   andra = FALSE.
   RUN kollpers_UI IN schproch (INPUT rulid, OUTPUT kollpersmedd).
   RAD_VAL = 1.
   RUN radval_UI (INPUT RAD_VAL).
   RAD_VAL:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
   
   {FRMSIZEDF.I}      
   ENABLE BTN_OVER BTN_BACK WITH FRAME FRAME-VECRULL.
   {musarrow.i}
    IF Guru.Konstanter:persekvar[5] = FALSE THEN DO:
       ASSIGN
       BTN_NY-2:HIDDEN = TRUE   
       BTN_BORT-2:HIDDEN = TRUE
       BTN_SSPARA-2:HIDDEN = TRUE

       BTN_NY:HIDDEN = TRUE       
       BTN_AND:HIDDEN = TRUE
       BTN_BORT:HIDDEN = TRUE
       BTN_OVER:HIDDEN = TRUE
       BTN_UPP-2:HIDDEN = TRUE
       BTN_BACK:HIDDEN = TRUE
       BTN_NER:HIDDEN = TRUE.
       DISABLE FILL-IN_TEXT WITH FRAME FRAME-VECRULL.
    END.
   {DIA_M_SLUT.I}
   APPLY "VALUE-CHANGED" TO BRW_DAG.
   WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE allstartbrw_UI DIALOG-2 
PROCEDURE allstartbrw_UI :
/* -----------------------------------------------------------
  Purpose: Changing screen-value for combo-box CMB_OMR     
  Parameters:  Input = Screen-value for CMB_FOR
  Notes:       
-------------------------------------------------------------*/ 
   arbtidtemp.ARBTIDKOD:READ-ONLY IN BROWSE {&BROWSE-NAME} = TRUE.
   RUN DYNBRW.P PERSISTENT SET brwproc[1] 
      (INPUT BRW_DAG:HANDLE IN FRAME FRAME-DAG).
   RUN DYNBRW.P PERSISTENT SET brwproc[2] 
      (INPUT BRW_VEC:HANDLE IN FRAME FRAME-VECRULL).
   RUN DYNBRW.P PERSISTENT SET brwproc[3] 
      (INPUT BRW_RULLVEC:HANDLE IN FRAME FRAME-VECRULL).
   IF Guru.Konstanter:appcon THEN RUN SCHAPP.P PERSISTENT SET schproch ON Guru.Konstanter:apphand TRANSACTION DISTINCT.
   ELSE RUN SCHAPP.P PERSISTENT SET schproch.
   RUN laddatemp_UI IN schproch 
     (OUTPUT TABLE arbtidtemp, OUTPUT TABLE veckoarbtemp, 
      OUTPUT TABLE rullvectemp, OUTPUT TABLE rullschtemp).  
   RUN setcolindex_UI IN brwproc[3] (INPUT "ORDNING").
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE dagtotkoll_UI DIALOG-2 
PROCEDURE dagtotkoll_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   IF totdagsek < 0 THEN dagtotkoll = TRUE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE dagtot_UI DIALOG-2 
PROCEDURE dagtot_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   /*om typ = 0 sätt typ till 2*/
   nytid = arbtidtemp.FRUKOSTSTART.
   RUN TIMSEK.P.
   sekfrukoststarten = sekunder.
   nytid = arbtidtemp.FRUKOSTSLUT.
   RUN TIMSEK.P.
   sekfrukostslutet = sekunder.
   nytid = arbtidtemp.LUNCHSLUT.
   RUN TIMSEK.P.
   seklunchslutet = sekunder.
   nytid = arbtidtemp.LUNCHSTART.
   RUN TIMSEK.P.
   seklunchstarten = sekunder.
   nytid = arbtidtemp.KAFESTART.
   RUN TIMSEK.P.
   sekkaffestarten = sekunder.
   nytid = arbtidtemp.KAFESLUT.
   RUN TIMSEK.P.
   sekkaffeslutet = sekunder.
   rasten = sekkaffeslutet - sekkaffestarten.
   frukost = sekfrukostslutet - sekfrukoststarten.
   lunchen = seklunchslutet - seklunchstarten.
   nytid = arbtidtemp.SLUT.
   RUN TIMSEK.P.
   sekslut = sekunder.
   nytid = arbtidtemp.START.
   RUN TIMSEK.P.
   sekstart = sekunder.
   totdagsek = sekslut - sekstart - lunchen - frukost - rasten.
   sekunder = totdagsek.
   RUN dagtotkoll_UI.
   RUN SEKTIM.P.
   ASSIGN 
   arbtidtemp.TOTDAGTID = nytid.
   DISPLAY arbtidtemp.TOTDAGTID WITH BROWSE {&BROWSE-NAME}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI DIALOG-2  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  HIDE FRAME DIALOG-2.
  HIDE FRAME FRAME-DAG.
  HIDE FRAME FRAME-VECRULL.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI DIALOG-2  _DEFAULT-ENABLE
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
  ENABLE MBTN_DAG MBTN_VEC MBTN_RUL RECT-24 RAD_VAL BTN_SKR BTN_VEVISA BTN_AVB 
      WITH FRAME DIALOG-2.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-2}
  DISPLAY FILL-TEXT-DAG 
      WITH FRAME FRAME-DAG.
  ENABLE BRW_DAG BTN_NY-2 BTN_BORT-2 BTN_SSPARA-2 FILL-TEXT-DAG 
      WITH FRAME FRAME-DAG.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-DAG}
  DISPLAY FILL-IN_TEXT CMB_RUL FILL-TEXT FILL-IN-DSCH1 FILL-IN-DSCH2 
          FILL-IN-DSCH3 FILL-IN-DSCH4 FILL-IN-DSCH5 FILL-IN-DSCH6 FILL-IN-DSCH7 
          FILL-IN-TOTV FILL-IN-VECO1 FILL-IN-BORJ1 FILL-IN-BORJ2 FILL-IN-BORJ3 
          FILL-IN-BORJ4 FILL-IN-BORJ5 FILL-IN-BORJ6 FILL-IN-BORJ7 FILL-IN-SLU1 
          FILL-IN-SLU2 FILL-IN-SLU3 FILL-IN-SLU4 FILL-IN-SLU5 FILL-IN-SLU6 
          FILL-IN-SLU7 FILL-IN-TIM1 FILL-IN-TIM2 FILL-IN-TIM3 FILL-IN-TIM4 
          FILL-IN-TIM5 FILL-IN-TIM6 FILL-IN-TIM7 
      WITH FRAME FRAME-VECRULL.
  ENABLE FILL-IN_TEXT CMB_RUL BRW_VEC BRW_RULLVEC BTN_UPP-2 BTN_NER BTN_NY 
         BTN_AND BTN_BORT FILL-TEXT FILL-IN-DSCH1 FILL-IN-DSCH2 FILL-IN-DSCH3 
         FILL-IN-DSCH4 FILL-IN-DSCH5 FILL-IN-DSCH6 FILL-IN-DSCH7 FILL-IN-TOTV 
         FILL-IN-VECO1 FILL-IN-BORJ1 FILL-IN-BORJ2 FILL-IN-BORJ3 FILL-IN-BORJ4 
         FILL-IN-BORJ5 FILL-IN-BORJ6 FILL-IN-BORJ7 FILL-IN-SLU1 FILL-IN-SLU2 
         FILL-IN-SLU3 FILL-IN-SLU4 FILL-IN-SLU5 FILL-IN-SLU6 FILL-IN-SLU7 
         FILL-IN-TIM1 FILL-IN-TIM2 FILL-IN-TIM3 FILL-IN-TIM4 FILL-IN-TIM5 
         FILL-IN-TIM6 FILL-IN-TIM7 
      WITH FRAME FRAME-VECRULL.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-VECRULL}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE klock_UI DIALOG-2 
PROCEDURE klock_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER vart AS INTEGER NO-UNDO.
   RUN KLOCKAN.W.
   IF vart = 1 THEN DO:
     IF klocka = 24.00 THEN klocka = 00.00. 
   END.
   IF vart = 2 THEN DO:
     IF klocka = 00.00 THEN klocka = 24.00.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE piluppner_UI DIALOG-2 
PROCEDURE piluppner_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER upp AS LOGICAL NO-UNDO.

   IF AVAILABLE rullvectemp THEN DO:
      andra = TRUE.
      temp_fran = rullvectemp.ORDNING.
      IF upp THEN GET PREV BRW_RULLVEC.
      ELSE GET NEXT BRW_RULLVEC.
      IF AVAILABLE rullvectemp THEN DO:
         temp_till = rullvectemp.ORDNING.
         rullvectemp.ORDNING = temp_fran.
         IF upp THEN GET NEXT BRW_RULLVEC.
         ELSE GET PREV BRW_RULLVEC.
         rullvectemp.ORDNING = temp_till.
         RUN setlastrowid_UI IN brwproc[3] (INPUT ROWID(rullvectemp)).
         RUN openbdynspec_UI IN brwproc[3].
         RUN lastselectdyn_UI IN brwproc[3].
      END.
      ELSE DO: 
         IF upp THEN GET NEXT BRW_RULLVEC.
         ELSE GET PREV BRW_RULLVEC.
      END.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE radval_UI DIALOG-2 
PROCEDURE radval_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER rval AS INTEGER NO-UNDO.
   IF rval = 1 THEN DO:
      FILL-TEXT-DAG:SCREEN-VALUE IN FRAME FRAME-DAG = "Ändra dagschema".
      BTN_VEVISA:LABEL IN FRAME DIALOG-2 = "Visa ingående v-sch".
      DISPLAY BTN_VEVISA WITH FRAME DIALOG-2.
      FRAME FRAME-VECRULL:HIDDEN = TRUE.
      FRAME FRAME-DAG:HIDDEN = FALSE.
      
   END.
   ELSE DO:
      FRAME FRAME-VECRULL:HIDDEN = FALSE.
      FRAME FRAME-DAG:HIDDEN = TRUE.
      IF rval = 2 THEN DO:
         ASSIGN 
         BTN_AND:HIDDEN IN FRAME FRAME-VECRULL = FALSE
         BTN_SKR:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
         BTN_BACK:HIDDEN IN FRAME FRAME-VECRULL = TRUE
         BTN_OVER:HIDDEN IN FRAME FRAME-VECRULL = TRUE 
         BTN_NER:HIDDEN IN FRAME FRAME-VECRULL = TRUE
         BTN_UPP-2:HIDDEN IN FRAME FRAME-VECRULL = TRUE
         BRW_VEC:X IN FRAME FRAME-VECRULL = ((FRAME FRAME-VECRULL:WIDTH-PIXELS / 2) - (BRW_VEC:WIDTH-PIXELS / 2))
         BRW_RULLVEC:HIDDEN IN FRAME FRAME-VECRULL = TRUE
         CMB_RUL:HIDDEN IN FRAME FRAME-VECRULL = TRUE
         FILL-IN_TEXT:HIDDEN IN FRAME FRAME-VECRULL = TRUE
         FILL-TEXT:SCREEN-VALUE IN FRAME FRAME-VECRULL = "Ändra veckoschema".
         
         BTN_VEVISA:LABEL in FRAME DIALOG-2 = "Visa v-sch".
         DISPLAY BTN_VEVISA WITH FRAME DIALOG-2 .
      END.
      ELSE IF rval = 3 THEN DO:
         ASSIGN 
         BTN_AND:HIDDEN IN FRAME FRAME-VECRULL = TRUE
         BTN_SKR:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
         BTN_BACK:HIDDEN IN FRAME FRAME-VECRULL = FALSE
         BTN_OVER:HIDDEN IN FRAME FRAME-VECRULL = FALSE 
         BTN_NER:HIDDEN IN FRAME FRAME-VECRULL = FALSE
         BTN_UPP-2:HIDDEN IN FRAME FRAME-VECRULL = FALSE
         BRW_VEC:X IN FRAME FRAME-VECRULL = orgpos
         BRW_RULLVEC:HIDDEN IN FRAME FRAME-VECRULL = FALSE
         CMB_RUL:HIDDEN IN FRAME FRAME-VECRULL = FALSE
         FILL-IN_TEXT:HIDDEN IN FRAME FRAME-VECRULL = FALSE
         FILL-TEXT:SCREEN-VALUE IN FRAME FRAME-VECRULL = "Ändra rullande schema".
         BTN_VEVISA:LABEL in FRAME DIALOG-2 = "Visa ing v-sch".
         DISPLAY BTN_VEVISA WITH FRAME DIALOG-2 .
      END.
   END.
   IF Guru.Konstanter:persekvar[5] = FALSE THEN DO:
       ASSIGN
       BTN_NY-2:HIDDEN = TRUE   
       BTN_BORT-2:HIDDEN = TRUE
       BTN_SSPARA-2:HIDDEN = TRUE

       BTN_NY:HIDDEN = TRUE       
       BTN_AND:HIDDEN = TRUE
       BTN_BORT:HIDDEN = TRUE
       BTN_OVER:HIDDEN = TRUE
       BTN_UPP-2:HIDDEN = TRUE
       BTN_BACK:HIDDEN = TRUE
       BTN_NER:HIDDEN = TRUE.
       DISABLE FILL-IN_TEXT WITH FRAME FRAME-VECRULL.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE rullchange_UI DIALOG-2 
PROCEDURE rullchange_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   FIND FIRST rullschtemp WHERE rullschtemp.RULLID = rulid NO-LOCK NO-ERROR.
   IF AVAILABLE rullschtemp THEN DO.
      FILL-IN_TEXT:SCREEN-VALUE IN FRAME FRAME-VECRULL = rullschtemp.BENAMNING.
      RUN setcolsortvar_UI IN brwproc[3] (INPUT "rullvectemp.RULLID = " + STRING(rulid)).
      RUN openbdynspec_UI IN brwproc[3].
      OPEN QUERY BRW_RULLVEC FOR EACH rullvectemp WHERE rullvectemp.RULLID = rulid NO-LOCK.
      FIND FIRST rullvectemp WHERE rullvectemp.RULLID = rulid NO-LOCK NO-ERROR.
      IF AVAILABLE rullvectemp THEN DO:
         RUN update_UI (INPUT rullvectemp.VECKOSCHEMA).
         BRW_RULLVEC:SELECT-FOCUSED-ROW() NO-ERROR.
         IF AVAILABLE rullvectemp THEN DO:
            BRW_VEC:DESELECT-ROWS() NO-ERROR.
            FIND FIRST veckoarbtemp WHERE veckoarbtemp.VECKOSCHEMA = rullvectemp.VECKOSCHEMA NO-LOCK NO-ERROR.
            IF AVAILABLE veckoarbtemp THEN RUN selectbyrowid_UI IN brwproc[2] (INPUT ROWID(veckoarbtemp)).
         END.
      END.      
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE update_UI DIALOG-2 
PROCEDURE update_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER vesch AS INTEGER NO-UNDO.

   FIND FIRST veckoarbtemp WHERE veckoarbtemp.VECKOSCHEMA = vesch NO-LOCK NO-ERROR.
   FILL-IN-VECO1 = veckoarbtemp.VECKOSCHEMA.                                         
   FILL-IN-TOTV = veckoarbtemp.TOTVECKTID.                                           
   FIND FIRST arbtidtemp WHERE arbtidtemp.ARBTIDKOD = veckoarbtemp.ARBTIDMAN 
   USE-INDEX ARBTIDKOD NO-LOCK NO-ERROR.                                          
   FILL-IN-DSCH1 = arbtidtemp.ARBTIDKOD.                                             
   FILL-IN-BORJ1 = arbtidtemp.START.                                                 
   FILL-IN-SLU1 = arbtidtemp.SLUT.                                                   
   FILL-IN-TIM1 = arbtidtemp.TOTDAGTID.                                              
   FIND FIRST arbtidtemp WHERE arbtidtemp.ARBTIDKOD = veckoarbtemp.ARBTIDTIS         
   USE-INDEX ARBTIDKOD NO-LOCK NO-ERROR.                                          
   FILL-IN-DSCH2 = arbtidtemp.ARBTIDKOD.                                             
   FILL-IN-BORJ2 = arbtidtemp.START.                                                 
   FILL-IN-SLU2 = arbtidtemp.SLUT.                                                   
   FILL-IN-TIM2 = arbtidtemp.TOTDAGTID.                                              
   FIND FIRST arbtidtemp WHERE arbtidtemp.ARBTIDKOD = veckoarbtemp.ARBTIDONS         
   USE-INDEX ARBTIDKOD NO-LOCK NO-ERROR.                                             
   FILL-IN-DSCH3 = arbtidtemp.ARBTIDKOD.                                             
   FILL-IN-BORJ3 = arbtidtemp.START.                                                 
   FILL-IN-SLU3 = arbtidtemp.SLUT.                                                   
   FILL-IN-TIM3 = arbtidtemp.TOTDAGTID.                                              
   FIND FIRST arbtidtemp WHERE arbtidtemp.ARBTIDKOD = veckoarbtemp.ARBTIDTOR         
   USE-INDEX ARBTIDKOD NO-LOCK NO-ERROR.                                             
   FILL-IN-DSCH4 = arbtidtemp.ARBTIDKOD.                                             
   FILL-IN-BORJ4 = arbtidtemp.START.                                                 
   FILL-IN-SLU4 = arbtidtemp.SLUT.                                                   
   FILL-IN-TIM4 = arbtidtemp.TOTDAGTID.                                              
   FIND FIRST arbtidtemp WHERE arbtidtemp.ARBTIDKOD = veckoarbtemp.ARBTIDFRE         
   USE-INDEX ARBTIDKOD NO-LOCK NO-ERROR.                                             
   FILL-IN-DSCH5 = arbtidtemp.ARBTIDKOD.                                             
   FILL-IN-BORJ5 = arbtidtemp.START.                                                 
   FILL-IN-SLU5 = arbtidtemp.SLUT.                                                   
   FILL-IN-TIM5 = arbtidtemp.TOTDAGTID.                                              
   FIND FIRST arbtidtemp WHERE arbtidtemp.ARBTIDKOD = veckoarbtemp.ARBTIDLOR         
   USE-INDEX ARBTIDKOD NO-LOCK NO-ERROR.                                             
   FILL-IN-DSCH6 = arbtidtemp.ARBTIDKOD.                                             
   FILL-IN-BORJ6 = arbtidtemp.START.                                                 
   FILL-IN-SLU6 = arbtidtemp.SLUT.                                                   
   FILL-IN-TIM6 = arbtidtemp.TOTDAGTID.                                              
   FIND FIRST arbtidtemp WHERE arbtidtemp.ARBTIDKOD = veckoarbtemp.ARBTIDSON         
   USE-INDEX ARBTIDKOD NO-LOCK NO-ERROR.                                             
   FILL-IN-DSCH7 = arbtidtemp.ARBTIDKOD.                                             
   FILL-IN-BORJ7 = arbtidtemp.START.                                                 
   FILL-IN-SLU7 = arbtidtemp.SLUT.                                                   
   FILL-IN-TIM7 = arbtidtemp.TOTDAGTID.                                                    
   DISPLAY                                                                           
   FILL-IN-VECO1 FILL-IN-DSCH1 FILL-IN-DSCH2                
   FILL-IN-DSCH3 FILL-IN-DSCH4 FILL-IN-DSCH5 FILL-IN-DSCH6 FILL-IN-DSCH7          
   FILL-IN-TOTV FILL-IN-BORJ1 FILL-IN-BORJ2 FILL-IN-BORJ3 FILL-IN-BORJ4           
   FILL-IN-BORJ5 FILL-IN-BORJ6 FILL-IN-BORJ7 FILL-IN-SLU1 FILL-IN-SLU2            
   FILL-IN-SLU3 FILL-IN-SLU4 FILL-IN-SLU5 FILL-IN-SLU6 FILL-IN-SLU7               
   FILL-IN-TIM1 FILL-IN-TIM2 FILL-IN-TIM3 FILL-IN-TIM4 FILL-IN-TIM5               
   FILL-IN-TIM6 FILL-IN-TIM7    
   WITH FRAME FRAME-VECRULL.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ut_UI DIALOG-2 
PROCEDURE ut_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   FOR EACH tidut:
      DELETE tidut.
   END.
   IF RAD_VAL = 1 THEN DO:
      str = "======.========.=======.======.======.======.=========.=========.========.=======".
      CREATE tidut.
      ASSIGN
      SUBSTRING(tidut.UT,1) = "Dagscheman"
      SUBSTRING(tidut.UT,30) = STRING(TODAY).
      CREATE tidut.
      CREATE tidut.
      ASSIGN
      SUBSTRING(tidut.UT,1) = "Dag"
      SUBSTRING(tidut.UT,8) = "Arbetet"
      SUBSTRING(tidut.UT,17) = "Frukost"
      SUBSTRING(tidut.UT,25) = "Frukost"
      SUBSTRING(tidut.UT,32) = "Lunch"
      SUBSTRING(tidut.UT,39) = "Lunch"
      SUBSTRING(tidut.UT,47) = "Kafferast"
      SUBSTRING(tidut.UT,57) = "Kafferast"
      SUBSTRING(tidut.UT,67) = "Arbetet"
      SUBSTRING(tidut.UT,76) = "Tot arb.".
      CREATE tidut.
      ASSIGN
      SUBSTRING(tidut.UT,1) = "schema"
      SUBSTRING(tidut.UT,8) = "börjar"
      SUBSTRING(tidut.UT,17) = "börjar"
      SUBSTRING(tidut.UT,25) = "slutar"
      SUBSTRING(tidut.UT,32) = "börjar"
      SUBSTRING(tidut.UT,39) = "slutar"
      SUBSTRING(tidut.UT,47) = "börjar"
      SUBSTRING(tidut.UT,57) = "slutar"
      SUBSTRING(tidut.UT,67) = "slutar"
      SUBSTRING(tidut.UT,76) = "tid".
      CREATE tidut.
      ASSIGN tidut.UT = str.
      
      GET FIRST BRW_DAG NO-LOCK.
      DO WHILE AVAILABLE(arbtidtemp):
         CREATE tidut.
         ASSIGN
         SUBSTRING(tidut.UT,1) = STRING(arbtidtemp.ARBTIDKOD,"99")
         SUBSTRING(tidut.UT,8) = STRING(arbtidtemp.START,"99.99")
         SUBSTRING(tidut.UT,17) = STRING(arbtidtemp.FRUKOSTSTART,"99.99")
         SUBSTRING(tidut.UT,25) = STRING(arbtidtemp.FRUKOSTSLUT,"99.99")
         SUBSTRING(tidut.UT,32) = STRING(arbtidtemp.KAFESTART,"99.99")
         SUBSTRING(tidut.UT,39) = STRING(arbtidtemp.KAFESLUT,"99.99")
         SUBSTRING(tidut.UT,47) = STRING(arbtidtemp.LUNCHSTART,"99.99")
         SUBSTRING(tidut.UT,57) = STRING(arbtidtemp.LUNCHSLUT,"99.99")
         SUBSTRING(tidut.UT,67) = STRING(arbtidtemp.SLUT,"99.99")
         SUBSTRING(tidut.UT,76) = STRING(arbtidtemp.TOTDAGTID,"99.99").
         GET NEXT BRW_DAG NO-LOCK.
      END.  
   END.
   ELSE IF RAD_VAL = 2 THEN DO:
      str = "==========.======.======.======.======.======.======.======.=========.========".
      FOR EACH tidut:
         DELETE tidut.
      END.   
      CREATE tidut.
      ASSIGN
      SUBSTRING(tidut.UT,1) = "Veckoscheman"
      SUBSTRING(tidut.UT,30) = STRING(TODAY).
      CREATE tidut.
      CREATE tidut.    
      ASSIGN          
      SUBSTRING(tidut.UT,12) = "Måndag"
      SUBSTRING(tidut.UT,19) = "Tisdag"
      SUBSTRING(tidut.UT,26) = "Onsdag"
      SUBSTRING(tidut.UT,33) = "Torsdag"
      SUBSTRING(tidut.UT,40) = "Fredag"
      SUBSTRING(tidut.UT,47) = "Lördag"
      SUBSTRING(tidut.UT,54) = "Söndag"
      SUBSTRING(tidut.UT,61) = "Tim/vecka"
      SUBSTRING(tidut.UT,71) = "Veckosch".                           
      CREATE tidut.
      ASSIGN tidut.UT = str.
      OPEN QUERY priq FOR EACH veckoarbtemp NO-LOCK.
      GET FIRST priq NO-LOCK.
      DO WHILE AVAILABLE(veckoarbtemp):
         FIND arbma WHERE arbma.ARBTIDKOD = veckoarbtemp.ARBTIDMAN
         USE-INDEX ARBTIDKOD NO-LOCK NO-ERROR.  
         FIND arbti WHERE arbti.ARBTIDKOD = veckoarbtemp.ARBTIDTIS
         USE-INDEX ARBTIDKOD NO-LOCK NO-ERROR.
         FIND arbon WHERE arbon.ARBTIDKOD = veckoarbtemp.ARBTIDONS
         USE-INDEX ARBTIDKOD NO-LOCK NO-ERROR.
         FIND arbto WHERE arbto.ARBTIDKOD = veckoarbtemp.ARBTIDTOR
         USE-INDEX ARBTIDKOD NO-LOCK NO-ERROR.
         FIND arbfr WHERE arbfr.ARBTIDKOD = veckoarbtemp.ARBTIDFRE
         USE-INDEX ARBTIDKOD NO-LOCK NO-ERROR.
         FIND arblo WHERE arblo.ARBTIDKOD = veckoarbtemp.ARBTIDLOR
         USE-INDEX ARBTIDKOD NO-LOCK NO-ERROR.
         FIND arbso WHERE arbso.ARBTIDKOD = veckoarbtemp.ARBTIDSON
         USE-INDEX ARBTIDKOD NO-LOCK NO-ERROR.   
         CREATE tidut.    
         ASSIGN          
         SUBSTRING(tidut.UT,1) = "Dagschema:"
         SUBSTRING(tidut.UT,12) = STRING(arbma.ARBTIDKOD,"99")
         SUBSTRING(tidut.UT,19) = STRING(arbti.ARBTIDKOD,"99")
         SUBSTRING(tidut.UT,26) = STRING(arbon.ARBTIDKOD,"99")
         SUBSTRING(tidut.UT,33) = STRING(arbto.ARBTIDKOD,"99")
         SUBSTRING(tidut.UT,40) = STRING(arbfr.ARBTIDKOD,"99")
         SUBSTRING(tidut.UT,47) = STRING(arblo.ARBTIDKOD,"99")
         SUBSTRING(tidut.UT,54) = STRING(arbso.ARBTIDKOD,"99")
         SUBSTRING(tidut.UT,61) = STRING(veckoarbtemp.TOTVECKTID,"99.99")
         SUBSTRING(tidut.UT,71) = STRING(veckoarbtemp.VECKOSCHEMA,">99").
         CREATE tidut.    
         ASSIGN          
         SUBSTRING(tidut.UT,1) = "Börjar:"
         SUBSTRING(tidut.UT,12) = STRING(arbma.START,"99.99")
         SUBSTRING(tidut.UT,19) = STRING(arbti.START,"99.99")
         SUBSTRING(tidut.UT,26) = STRING(arbon.START,"99.99")
         SUBSTRING(tidut.UT,33) = STRING(arbto.START,"99.99")
         SUBSTRING(tidut.UT,40) = STRING(arbfr.START,"99.99")
         SUBSTRING(tidut.UT,47) = STRING(arblo.START,"99.99")
         SUBSTRING(tidut.UT,54) = STRING(arbso.START,"99.99").
         CREATE tidut.    
         ASSIGN          
         SUBSTRING(tidut.UT,1) = "Slutar:"
         SUBSTRING(tidut.UT,12) = STRING(arbma.SLUT,"99.99")
         SUBSTRING(tidut.UT,19) = STRING(arbti.SLUT,"99.99")
         SUBSTRING(tidut.UT,26) = STRING(arbon.SLUT,"99.99")
         SUBSTRING(tidut.UT,33) = STRING(arbto.SLUT,"99.99")
         SUBSTRING(tidut.UT,40) = STRING(arbfr.SLUT,"99.99")
         SUBSTRING(tidut.UT,47) = STRING(arblo.SLUT,"99.99")
         SUBSTRING(tidut.UT,54) = STRING(arbso.SLUT,"99.99").
         CREATE tidut.    
         ASSIGN          
         SUBSTRING(tidut.UT,1) = "Tim/dag:"
         SUBSTRING(tidut.UT,12) = STRING(arbma.TOTDAGTID,"99.99")
         SUBSTRING(tidut.UT,19) = STRING(arbti.TOTDAGTID,"99.99")
         SUBSTRING(tidut.UT,26) = STRING(arbon.TOTDAGTID,"99.99")
         SUBSTRING(tidut.UT,33) = STRING(arbto.TOTDAGTID,"99.99")
         SUBSTRING(tidut.UT,40) = STRING(arbfr.TOTDAGTID,"99.99")
         SUBSTRING(tidut.UT,47) = STRING(arblo.TOTDAGTID,"99.99")
         SUBSTRING(tidut.UT,54) = STRING(arbso.TOTDAGTID,"99.99").
         CREATE tidut.
         GET NEXT priq NO-LOCK.
      END.
      CLOSE QUERY priq.       
      IF musz = TRUE THEN musz = FALSE.
      ELSE RUN EKLOGS.P.  
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valda_UI DIALOG-2 
PROCEDURE valda_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/   
   antal_valda = BRW_VEC:NUM-SELECTED-ROWS IN FRAME FRAME-VECRULL.
   IF antal_valda = 0 THEN DO:      
      MESSAGE "Inget är markerat" VIEW-AS ALERT-BOX.          
      RETURN.                
   END.
   ELSE DO:
      andra = TRUE.
      GET LAST BRW_RULLVEC.
      IF AVAILABLE rullvectemp THEN ordnvar = rullvectemp.ORDNING.
      ELSE ordnvar = 0.
      antal_raknare = 1.
      DO WHILE antal_raknare LE antal_valda :
         ordnvar = ordnvar + 1.
         BRW_VEC:FETCH-SELECTED-ROW(antal_raknare).
         IF musz = TRUE THEN musz = FALSE.
         ELSE DO:
            CREATE rullvectemp. 
            ASSIGN 
            rullvectemp.ORDNING = ordnvar
            rullvectemp.VECKOSCHEMA = veckoarbtemp.VECKOSCHEMA
            rullvectemp.RULLID = rulid.
         END.
         antal_raknare = antal_raknare + 1.
      END.
   END.
   BRW_VEC:DESELECT-ROWS() NO-ERROR.
   RUN openbdynspec_UI IN brwproc[3].
   GET LAST BRW_RULLVEC.
   RUN selectbyrowid_UI IN brwproc[3] (INPUT ROWID(rullvectemp)).   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

