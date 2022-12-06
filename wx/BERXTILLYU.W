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
DEFINE INPUT  PARAMETER radvad AS INTEGER NO-UNDO.
/* Local Variable Definitions ---                                       */ 
{ALLDEF.I}
&Scoped-define NEW 
{GLOBVAR2DEL1.I}
/*{EGENBEN.I}*/
{LISTDEF.I} 
{DEFSOK.I} 
{SOKDEF.I}
&Scoped-define SHARED SHARED
{LISTMTRL.I}
{MTRLTEMP.I}
{LEVTEMP.I}
{HUVLEVTEMP.I}
{HOPPSEK2W.I}
{KALKTEMP2.I}
&Scoped-define NEW NEW
{SMTRL.I} 
/*Behövs för att satsinneu.w skall fungera*/
{KONSTRMTRL.I}
{LOPTEMP.I}
{PTEMP.I}

DEFINE NEW SHARED VARIABLE satstrue AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE satsinn AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE satsmtrl AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE lev2 AS CHARACTER NO-UNDO.  
DEFINE SHARED VARIABLE valaonr AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE valdelnr AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE valort AS CHARACTER NO-UNDO. 
DEFINE SHARED VARIABLE valomrade AS CHARACTER NO-UNDO.  
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO. 
DEFINE VARIABLE antal_valda AS INTEGER NO-UNDO.
DEFINE VARIABLE antal_raknare AS INTEGER NO-UNDO. 
DEFINE NEW SHARED VARIABLE mtrl_rowid AS ROWID NO-UNDO.   
DEFINE VARIABLE satsvar AS LOGICAL NO-UNDO.
DEFINE VARIABLE satsvar2 AS LOGICAL NO-UNDO.
DEFINE VARIABLE nettoh AS HANDLE NO-UNDO.
/*MATERIEL DEFINITIONER*/

DEFINE SHARED VARIABLE huvudlev AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE valkalknr AS INTEGER NO-UNDO. 
DEFINE SHARED VARIABLE kalkmtrl AS LOGICAL NO-UNDO.   
DEFINE SHARED VARIABLE vald_kundlev AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE vald_lev AS CHARACTER NO-UNDO.
DEFINE VARIABLE vald_lev2 AS CHARACTER NO-UNDO.  
DEFINE SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.  
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO. 
DEFINE SHARED VARIABLE vartpro AS CHARACTER FORMAT "X(3)" NO-UNDO.   
DEFINE VARIABLE aosok AS CHARACTER FORMAT "X(40)" NO-UNDO.
DEFINE VARIABLE posok AS CHARACTER FORMAT "X(11)" NO-UNDO.
DEFINE VARIABLE lev AS CHARACTER NO-UNDO.  
DEFINE VARIABLE svar AS LOGICAL NO-UNDO. 
DEFINE VARIABLE spec_rowid AS ROWID NO-UNDO.
DEFINE VARIABLE mtrlhmtapph AS HANDLE NO-UNDO.
DEFINE VARIABLE laddaproch AS HANDLE NO-UNDO.
DEFINE VARIABLE felmedd AS CHARACTER NO-UNDO.
DEFINE VARIABLE lkod AS CHARACTER NO-UNDO.
DEFINE VARIABLE lekod AS CHARACTER NO-UNDO.
DEFINE VARIABLE levinlast AS CHARACTER NO-UNDO.
DEFINE VARIABLE antmtrlber AS INTEGER NO-UNDO.
DEFINE VARIABLE berkalk_rowid AS ROWID NO-UNDO.
DEFINE VARIABLE edataapph AS HANDLE NO-UNDO.

DEFINE VARIABLE logresult AS LOGICAL NO-UNDO.
DEFINE VARIABLE KalkClasserStart AS HANDLE NO-UNDO.  
DEFINE VARIABLE extrkodapph AS HANDLE NO-UNDO.
DEFINE VARIABLE extrbefbapph AS HANDLE NO-UNDO.

{EXTRADATA.I}


/* DEFINE QUERY mtrlq FOR MTRLBER. */
DEFINE BUFFER mtrlbuff FOR list_mtrl.
DEFINE BUFFER sumkalkbuff FOR sumkalk_temp.



/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A
&Scoped-define BROWSE-NAME BRW_BER

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES list_mtrl mtrltemp sumkalk_temp bytkalk_temp ~
loptemp spec_mtrl ptemp satstemp

/* Definitions for BROWSE BRW_BER                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_BER list_mtrl.ENR list_mtrl.BENAMNING ~
list_mtrl.ANTAL list_mtrl.ENHET list_mtrl.PRIS list_mtrl.LEVKOD 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_BER list_mtrl.ENR 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_BER list_mtrl
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_BER list_mtrl
&Scoped-define QUERY-STRING-BRW_BER FOR EACH list_mtrl NO-LOCK
&Scoped-define OPEN-QUERY-BRW_BER OPEN QUERY BRW_BER FOR EACH list_mtrl NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_BER list_mtrl
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_BER list_mtrl


/* Definitions for BROWSE BRW_BERLEV                                    */
&Scoped-define FIELDS-IN-QUERY-BRW_BERLEV list_mtrl.ENR list_mtrl.BENAMNING ~
list_mtrl.ANTAL list_mtrl.ENHET list_mtrl.PRIS list_mtrl.LEVKOD 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_BERLEV 
&Scoped-define QUERY-STRING-BRW_BERLEV FOR EACH list_mtrl NO-LOCK
&Scoped-define OPEN-QUERY-BRW_BERLEV OPEN QUERY BRW_BERLEV FOR EACH list_mtrl NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_BERLEV list_mtrl
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_BERLEV list_mtrl


/* Definitions for BROWSE BRW_HLEV                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_HLEV mtrltemp.Enr mtrltemp.Benamning ~
mtrltemp.Enhet mtrltemp.NPRIS 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_HLEV 
&Scoped-define QUERY-STRING-BRW_HLEV FOR EACH mtrltemp NO-LOCK ~
    BY MTRL.Enr
&Scoped-define OPEN-QUERY-BRW_HLEV OPEN QUERY BRW_HLEV FOR EACH mtrltemp NO-LOCK ~
    BY MTRL.Enr.
&Scoped-define TABLES-IN-QUERY-BRW_HLEV mtrltemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_HLEV mtrltemp


/* Definitions for BROWSE BRW_KALKSUM                                   */
&Scoped-define FIELDS-IN-QUERY-BRW_KALKSUM sumkalk_temp.ARBKOD ~
sumkalk_temp.LOPNR sumkalk_temp.BENAMNING sumkalk_temp.ANTAL ~
sumkalk_temp.ENHET 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_KALKSUM 
&Scoped-define QUERY-STRING-BRW_KALKSUM FOR EACH sumkalk_temp NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BRW_KALKSUM OPEN QUERY BRW_KALKSUM FOR EACH sumkalk_temp NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BRW_KALKSUM sumkalk_temp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_KALKSUM sumkalk_temp


/* Definitions for BROWSE BRW_KOD                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_KOD bytkalk_temp.ARBKOD ~
bytkalk_temp.LOPNR bytkalk_temp.BENAMNING bytkalk_temp.ENHET 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_KOD 
&Scoped-define QUERY-STRING-BRW_KOD FOR EACH bytkalk_temp NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BRW_KOD OPEN QUERY BRW_KOD FOR EACH bytkalk_temp NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BRW_KOD bytkalk_temp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_KOD bytkalk_temp


/* Definitions for BROWSE BRW_LOP                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_LOP loptemp.LOPNR loptemp.ENHET ~
loptemp.BENAMNING 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_LOP 
&Scoped-define QUERY-STRING-BRW_LOP FOR EACH loptemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_LOP OPEN QUERY BRW_LOP FOR EACH loptemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_LOP loptemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_LOP loptemp


/* Definitions for BROWSE BRW_MTRL                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_MTRL spec_mtrl.Enr spec_mtrl.Benamning ~
spec_mtrl.ANTAL spec_mtrl.Enhet spec_mtrl.NPRIS 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_MTRL 
&Scoped-define QUERY-STRING-BRW_MTRL FOR EACH spec_mtrl NO-LOCK
&Scoped-define OPEN-QUERY-BRW_MTRL OPEN QUERY BRW_MTRL FOR EACH spec_mtrl NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_MTRL spec_mtrl
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_MTRL spec_mtrl


/* Definitions for BROWSE BRW_P                                         */
&Scoped-define FIELDS-IN-QUERY-BRW_P ptemp.ARBKOD ptemp.BENAMNING 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_P 
&Scoped-define QUERY-STRING-BRW_P FOR EACH ptemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_P OPEN QUERY BRW_P FOR EACH ptemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_P ptemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_P ptemp


/* Definitions for BROWSE BRW_SATS                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_SATS satstemp.KOD satstemp.BENAMNING 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_SATS 
&Scoped-define QUERY-STRING-BRW_SATS FOR EACH satstemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_SATS OPEN QUERY BRW_SATS FOR EACH satstemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_SATS satstemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_SATS satstemp


/* Definitions for FRAME FRAME-ERSATT                                   */

/* Definitions for FRAME FRAME-ERSKALK                                  */

/* Definitions for FRAME FRAME-LEVBYT                                   */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RAD_ERSLEV BTN_AVB 
&Scoped-Define DISPLAYED-OBJECTS RAD_ERSLEV 

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

DEFINE VARIABLE RAD_ERSLEV AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Ersätt/Lägg till/Ta bort mtrl", 1,
"Byt leverantör", 2
     SIZE 52.5 BY .75 NO-UNDO.

DEFINE BUTTON BTN_ALLBACK 
     IMAGE-UP FILE "BILDER\rewind-u":U NO-FOCUS FLAT-BUTTON
     LABEL "Alla aonr i listan":L 
     SIZE 4 BY 1.21 TOOLTIP "Alla valda aonr tas bort från vallistan"
     FONT 11.

DEFINE BUTTON BTN_ANTAL 
     LABEL "Antal":L 
     SIZE 12 BY 1.

DEFINE BUTTON btn_back 
     IMAGE-UP FILE "BILDER\prev-u":U
     LABEL "":L 
     SIZE 4 BY 1.21.

DEFINE BUTTON BTN_BYT 
     LABEL "Byt mtrl" 
     SIZE 14 BY 1.

DEFINE BUTTON btn_over 
     IMAGE-UP FILE "BILDER\next-u":U
     LABEL "":L 
     SIZE 4 BY 1.21.

DEFINE VARIABLE CMB_LEV AS CHARACTER FORMAT "X(40)":U 
     LABEL "Leverantörer" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "0" 
     DROP-DOWN-LIST
     SIZE 27.5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-BEN AS CHARACTER FORMAT "X(40)":U 
     LABEL "Ben" 
     VIEW-AS FILL-IN 
     SIZE 19.75 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN-ENR AS CHARACTER FORMAT "X(11)":U 
     LABEL "Enr" 
     VIEW-AS FILL-IN 
     SIZE 13.63 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN-KOD AS CHARACTER FORMAT "X(11)":U 
     LABEL "Kod" 
     VIEW-AS FILL-IN 
     SIZE 13.38 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN-SOKALT AS CHARACTER FORMAT "X(256)":U INITIAL "Sökalternativ:" 
      VIEW-AS TEXT 
     SIZE 13 BY .63 NO-UNDO.

DEFINE IMAGE IMAGE-8
     FILENAME "BILDER\sokpa.gif":U CONVERT-3D-COLORS
     SIZE 8 BY .83.

DEFINE VARIABLE RAD_SOK AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Början", 1,
"Någonstans", 2,
"Slutet", 3
     SIZE 39.5 BY .63 NO-UNDO.

DEFINE VARIABLE RAD_VAL AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Ersätt", 1,
"Lägg till", 2,
"Ta bort", 3
     SIZE 13.5 BY 4.25 NO-UNDO.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 54.88 BY 2.5
     BGCOLOR 8 .

DEFINE BUTTON btn_backk 
     IMAGE-UP FILE "BILDER\prev-u":U
     LABEL "":L 
     SIZE 4 BY 1.21.

DEFINE BUTTON BTN_BYTK 
     LABEL "Byt kalkylkod" 
     SIZE 14 BY 1.

DEFINE BUTTON btn_overk 
     IMAGE-UP FILE "BILDER\next-u":U
     LABEL "":L 
     SIZE 4 BY 1.21.

DEFINE VARIABLE RAD_VALK AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Ersätt", 1,
"Lägg till", 2,
"Ta bort", 3
     SIZE 13.5 BY 4.25 NO-UNDO.

DEFINE BUTTON BTN_BYTLEV 
     LABEL "Byt leverantör" 
     SIZE 14 BY 1 TOOLTIP "Byt till vald leverantör för markerade artiklar"
     FGCOLOR 1 .

DEFINE VARIABLE CMB_LEVTILL AS CHARACTER FORMAT "X(15)":U 
     LABEL "Till lev" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "0" 
     DROP-DOWN-LIST
     SIZE 18.25 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_BER FOR 
      list_mtrl SCROLLING.

DEFINE QUERY BRW_BERLEV FOR 
      list_mtrl SCROLLING.

DEFINE QUERY BRW_HLEV FOR 
      mtrltemp SCROLLING.

DEFINE QUERY BRW_KALKSUM FOR 
      sumkalk_temp SCROLLING.

DEFINE QUERY BRW_KOD FOR 
      bytkalk_temp SCROLLING.

DEFINE QUERY BRW_LOP FOR 
      loptemp SCROLLING.

DEFINE QUERY BRW_MTRL FOR 
      spec_mtrl SCROLLING.

DEFINE QUERY BRW_P FOR 
      ptemp SCROLLING.

DEFINE QUERY BRW_SATS FOR 
      satstemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_BER
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_BER WINDOW-1 _STRUCTURED
  QUERY BRW_BER NO-LOCK DISPLAY
      list_mtrl.ENR FORMAT "X(11)":U
      list_mtrl.BENAMNING FORMAT "x(40)":U WIDTH 45
      list_mtrl.ANTAL FORMAT ">>>>>9":U WIDTH 11
      list_mtrl.ENHET FORMAT "x(5)":U
      list_mtrl.PRIS FORMAT ">>>>99.99":U
      list_mtrl.LEVKOD COLUMN-LABEL "Lev-id" FORMAT "x(4)":U WIDTH 4.25
  ENABLE
      list_mtrl.ENR
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 90.5 BY 10.46
         TITLE "Sammanställning beredningsmateriel" FIT-LAST-COLUMN.

DEFINE BROWSE BRW_BERLEV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_BERLEV WINDOW-1 _STRUCTURED
  QUERY BRW_BERLEV NO-LOCK DISPLAY
      list_mtrl.ENR FORMAT "X(11)":U
      list_mtrl.BENAMNING FORMAT "x(40)":U
      list_mtrl.ANTAL FORMAT ">>>>>9":U
      list_mtrl.ENHET FORMAT "x(5)":U
      list_mtrl.PRIS FORMAT ">>>>99.99":U
      list_mtrl.LEVKOD COLUMN-LABEL "Lev-id" FORMAT "x(4)":U WIDTH 2.13
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 78.5 BY 25.5
         TITLE "Sammanställning beredningsmateriel" FIT-LAST-COLUMN.

DEFINE BROWSE BRW_HLEV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_HLEV WINDOW-1 _STRUCTURED
  QUERY BRW_HLEV NO-LOCK DISPLAY
      mtrltemp.Enr FORMAT "X(11)":U
      mtrltemp.Benamning FORMAT "x(256)":U WIDTH 30
      mtrltemp.Enhet FORMAT "x(7)":U
      mtrltemp.NPRIS FORMAT ">>>>99.99":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 54.88 BY 10.92
         TITLE "Från".

DEFINE BROWSE BRW_KALKSUM
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_KALKSUM WINDOW-1 _STRUCTURED
  QUERY BRW_KALKSUM NO-LOCK DISPLAY
      sumkalk_temp.ARBKOD COLUMN-LABEL "Kod" FORMAT "X(5)":U
      sumkalk_temp.LOPNR COLUMN-LABEL "Löpnr" FORMAT ">>>":U
      sumkalk_temp.BENAMNING COLUMN-LABEL "Benämning" FORMAT "x(256)":U
            WIDTH 30
      sumkalk_temp.ANTAL COLUMN-LABEL "Antal" FORMAT "->>,>>9.999":U
      sumkalk_temp.ENHET COLUMN-LABEL "Enhet" FORMAT "x(5)":U WIDTH .63
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 56.5 BY 17
         TITLE "Sammanställning kalkylkoder" FIT-LAST-COLUMN.

DEFINE BROWSE BRW_KOD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_KOD WINDOW-1 _STRUCTURED
  QUERY BRW_KOD NO-LOCK DISPLAY
      bytkalk_temp.ARBKOD COLUMN-LABEL "Kod" FORMAT "X(5)":U
      bytkalk_temp.LOPNR COLUMN-LABEL "Löp!nr" FORMAT ">>>":U
      bytkalk_temp.BENAMNING COLUMN-LABEL "Benämning" FORMAT "x(40)":U
      bytkalk_temp.ENHET COLUMN-LABEL "Enhet" FORMAT "x(5)":U WIDTH .13
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 52.5 BY 7.38
         TITLE "Vald kod" FIT-LAST-COLUMN.

DEFINE BROWSE BRW_LOP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_LOP WINDOW-1 _STRUCTURED
  QUERY BRW_LOP NO-LOCK DISPLAY
      loptemp.LOPNR COLUMN-LABEL "Löpnr" FORMAT ">>>":U
      loptemp.ENHET COLUMN-LABEL "Enh" FORMAT "X(3)":U
      loptemp.BENAMNING COLUMN-LABEL "Benämning" FORMAT "X(256)":U
            WIDTH 30
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SIZE 32 BY 12.75
         TITLE "Löpnr kopplade till arbetskoden".

DEFINE BROWSE BRW_MTRL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_MTRL WINDOW-1 _STRUCTURED
  QUERY BRW_MTRL NO-LOCK DISPLAY
      spec_mtrl.Enr FORMAT "X(11)":U
      spec_mtrl.Benamning FORMAT "x(256)":U WIDTH 38
      spec_mtrl.ANTAL FORMAT ">>>>>9":U
      spec_mtrl.Enhet FORMAT "x(5)":U WIDTH 5.13
      spec_mtrl.NPRIS FORMAT ">>>>99.99":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 64.5 BY 10.92
         TITLE "Vald materiel" FIT-LAST-COLUMN.

DEFINE BROWSE BRW_P
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_P WINDOW-1 _STRUCTURED
  QUERY BRW_P NO-LOCK DISPLAY
      ptemp.ARBKOD COLUMN-LABEL "Kod" FORMAT "X(5)":U
      ptemp.BENAMNING COLUMN-LABEL "Benämning" FORMAT "X(40)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING MULTIPLE SIZE 32 BY 11.25
         TITLE "Arbetskoder".

DEFINE BROWSE BRW_SATS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_SATS WINDOW-1 _STRUCTURED
  QUERY BRW_SATS NO-LOCK DISPLAY
      satstemp.KOD FORMAT "X(11)":U
      satstemp.BENAMNING FORMAT "x(256)":U WIDTH 40
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 55 BY 10.92
         TITLE "Från".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     RAD_ERSLEV AT ROW 1.21 COL 2.25 NO-LABEL
     BTN_AVB AT ROW 28.25 COL 111.5
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 125 BY 28.42
         CANCEL-BUTTON BTN_AVB.

DEFINE FRAME FRAME-ERSKALK
     BRW_P AT ROW 1.25 COL 1.5 WIDGET-ID 300
     BRW_KALKSUM AT ROW 1.25 COL 36 WIDGET-ID 400
     RAD_VALK AT ROW 2.71 COL 93.88 NO-LABEL WIDGET-ID 4
     BTN_BYTK AT ROW 3.88 COL 110.75 WIDGET-ID 2
     BRW_LOP AT ROW 13.25 COL 1.5 WIDGET-ID 200
     BRW_KOD AT ROW 18.58 COL 40 WIDGET-ID 500
     btn_overk AT ROW 19.71 COL 35 WIDGET-ID 14
     btn_backk AT ROW 21.96 COL 35 WIDGET-ID 12
     "Obs! Går ej att ångra" VIEW-AS TEXT
          SIZE 21.5 BY 1.75 AT ROW 7 COL 94.63 WIDGET-ID 8
          FGCOLOR 12 FONT 17
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 2.1
         SIZE 125 BY 26.08 WIDGET-ID 100.

DEFINE FRAME FRAME-ERSATT
     BTN_ALLBACK AT ROW 19.88 COL 56.5
     BRW_BER AT ROW 1.25 COL 2.5
     RAD_VAL AT ROW 1.5 COL 93.88 NO-LABEL
     BTN_BYT AT ROW 2.96 COL 111.5
     BRW_HLEV AT ROW 12.21 COL 1.5
     BRW_SATS AT ROW 12.21 COL 1.5
     BRW_MTRL AT ROW 12.21 COL 60.75
     btn_over AT ROW 15.71 COL 56.5
     btn_back AT ROW 17.96 COL 56.5
     CMB_LEV AT ROW 23.29 COL 18.38 COLON-ALIGNED
     BTN_ANTAL AT ROW 23.5 COL 87
     FILL-IN-ENR AT ROW 24.75 COL 13.88 COLON-ALIGNED
     FILL-IN-KOD AT ROW 24.75 COL 13.88 COLON-ALIGNED
     FILL-IN-BEN AT ROW 24.75 COL 33.5 COLON-ALIGNED
     RAD_SOK AT ROW 25.88 COL 16.5 NO-LABEL
     FILL-IN-SOKALT AT ROW 25.96 COL 2.5 NO-LABEL
     "Obs! Går ej att ångra" VIEW-AS TEXT
          SIZE 21.5 BY 1.75 AT ROW 4.08 COL 103.88
          FGCOLOR 12 FONT 17
     RECT-3 AT ROW 24.5 COL 1.5
     IMAGE-8 AT ROW 24.83 COL 2.25
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 2.1
         SIZE 125 BY 26.08.

DEFINE FRAME FRAME-LEVBYT
     BRW_BERLEV AT ROW 1.25 COL 2.5
     CMB_LEVTILL AT ROW 2.96 COL 89.5 COLON-ALIGNED
     BTN_BYTLEV AT ROW 2.96 COL 111.5
     "Obs! Går ej att ångra" VIEW-AS TEXT
          SIZE 21.5 BY 1.75 AT ROW 4.08 COL 103.88
          FGCOLOR 12 FONT 17
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 2.1
         SIZE 125 BY 26.08.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: 
   Temp-Tables and Buffers:
      TABLE: bytkalk_temp T "?" NO-UNDO temp-db bytkalk_temp
      TABLE: list_mtrl T "?" NO-UNDO temp-db list_mtrl
      TABLE: loptemp T "?" NO-UNDO temp-db loptemp
      TABLE: mtrltemp T "?" NO-UNDO temp-db mtrltemp
      TABLE: ptemp T "?" NO-UNDO temp-db ptemp
      TABLE: satstemp T "?" NO-UNDO temp-db satstemp
      TABLE: sumkalk_temp T "?" NO-UNDO temp-db sumkalk_temp
      TABLE: ? T "?" NO-UNDO temp-db spec_mtrl
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW WINDOW-1 ASSIGN
         HIDDEN             = YES
         TITLE              = "Window 1"
         HEIGHT             = 28.58
         WIDTH              = 125
         MAX-HEIGHT         = 32.75
         MAX-WIDTH          = 128
         VIRTUAL-HEIGHT     = 32.75
         VIRTUAL-WIDTH      = 128
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
/* REPARENT FRAME */
ASSIGN FRAME FRAME-ERSATT:FRAME = FRAME FRAME-A:HANDLE
       FRAME FRAME-ERSKALK:FRAME = FRAME FRAME-A:HANDLE
       FRAME FRAME-LEVBYT:FRAME = FRAME FRAME-A:HANDLE.

/* SETTINGS FOR FRAME FRAME-A
   FRAME-NAME                                                           */

DEFINE VARIABLE XXTABVALXX AS LOGICAL NO-UNDO.

ASSIGN XXTABVALXX = FRAME FRAME-LEVBYT:MOVE-AFTER-TAB-ITEM (RAD_ERSLEV:HANDLE IN FRAME FRAME-A)
       XXTABVALXX = FRAME FRAME-ERSKALK:MOVE-BEFORE-TAB-ITEM (BTN_AVB:HANDLE IN FRAME FRAME-A)
       XXTABVALXX = FRAME FRAME-ERSATT:MOVE-BEFORE-TAB-ITEM (FRAME FRAME-ERSKALK:HANDLE)
       XXTABVALXX = FRAME FRAME-LEVBYT:MOVE-BEFORE-TAB-ITEM (FRAME FRAME-ERSATT:HANDLE)
/* END-ASSIGN-TABS */.

/* SETTINGS FOR FRAME FRAME-ERSATT
                                                                        */
/* BROWSE-TAB BRW_BER IMAGE-8 FRAME-ERSATT */
/* BROWSE-TAB BRW_HLEV BTN_BYT FRAME-ERSATT */
/* BROWSE-TAB BRW_SATS BRW_HLEV FRAME-ERSATT */
/* BROWSE-TAB BRW_MTRL BRW_SATS FRAME-ERSATT */
ASSIGN 
       BRW_HLEV:HIDDEN  IN FRAME FRAME-ERSATT                = TRUE
       BRW_HLEV:MAX-DATA-GUESS IN FRAME FRAME-ERSATT         = 40000
       BRW_HLEV:ALLOW-COLUMN-SEARCHING IN FRAME FRAME-ERSATT = TRUE.

ASSIGN 
       BRW_MTRL:MAX-DATA-GUESS IN FRAME FRAME-ERSATT         = 1000
       BRW_MTRL:ALLOW-COLUMN-SEARCHING IN FRAME FRAME-ERSATT = TRUE.

ASSIGN 
       BRW_SATS:HIDDEN  IN FRAME FRAME-ERSATT                = TRUE
       BRW_SATS:MAX-DATA-GUESS IN FRAME FRAME-ERSATT         = 50000
       BRW_SATS:ALLOW-COLUMN-SEARCHING IN FRAME FRAME-ERSATT = TRUE.

ASSIGN 
       CMB_LEV:HIDDEN IN FRAME FRAME-ERSATT           = TRUE.

ASSIGN 
       FILL-IN-BEN:HIDDEN IN FRAME FRAME-ERSATT           = TRUE.

ASSIGN 
       FILL-IN-ENR:HIDDEN IN FRAME FRAME-ERSATT           = TRUE.

ASSIGN 
       FILL-IN-KOD:HIDDEN IN FRAME FRAME-ERSATT           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-SOKALT IN FRAME FRAME-ERSATT
   ALIGN-L                                                              */
ASSIGN 
       FILL-IN-SOKALT:READ-ONLY IN FRAME FRAME-ERSATT        = TRUE.

/* SETTINGS FOR FRAME FRAME-ERSKALK
                                                                        */
/* BROWSE-TAB BRW_P TEXT-3 FRAME-ERSKALK */
/* BROWSE-TAB BRW_KALKSUM BRW_P FRAME-ERSKALK */
/* BROWSE-TAB BRW_LOP BTN_BYTK FRAME-ERSKALK */
/* BROWSE-TAB BRW_KOD BRW_LOP FRAME-ERSKALK */
ASSIGN 
       BRW_LOP:HIDDEN  IN FRAME FRAME-ERSKALK                = TRUE
       BRW_LOP:MAX-DATA-GUESS IN FRAME FRAME-ERSKALK         = 1000
       BRW_LOP:COLUMN-RESIZABLE IN FRAME FRAME-ERSKALK       = TRUE.

ASSIGN 
       BRW_P:HIDDEN  IN FRAME FRAME-ERSKALK                = TRUE
       BRW_P:MAX-DATA-GUESS IN FRAME FRAME-ERSKALK         = 1000
       BRW_P:ALLOW-COLUMN-SEARCHING IN FRAME FRAME-ERSKALK = TRUE
       BRW_P:COLUMN-RESIZABLE IN FRAME FRAME-ERSKALK       = TRUE.

/* SETTINGS FOR FRAME FRAME-LEVBYT
                                                                        */
/* BROWSE-TAB BRW_BERLEV TEXT-1 FRAME-LEVBYT */
ASSIGN 
       CMB_LEVTILL:HIDDEN IN FRAME FRAME-LEVBYT           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-1)
THEN WINDOW-1:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_BER
/* Query rebuild information for BROWSE BRW_BER
     _TblList          = "Temp-Tables.list_mtrl"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.list_mtrl.ENR
"list_mtrl.ENR" ? ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.list_mtrl.BENAMNING
"list_mtrl.BENAMNING" ? ? "character" ? ? ? ? ? ? no ? no no "45" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.list_mtrl.ANTAL
"list_mtrl.ANTAL" ? ">>>>>9" "integer" ? ? ? ? ? ? no ? no no "11" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   = Temp-Tables.list_mtrl.ENHET
     _FldNameList[5]   = Temp-Tables.list_mtrl.PRIS
     _FldNameList[6]   > Temp-Tables.list_mtrl.LEVKOD
"list_mtrl.LEVKOD" "Lev-id" ? "character" ? ? ? ? ? ? no ? no no "4.25" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_BER */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_BERLEV
/* Query rebuild information for BROWSE BRW_BERLEV
     _TblList          = "Temp-Tables.list_mtrl"
     _Options          = "NO-LOCK"
     _FldNameList[1]   = Temp-Tables.list_mtrl.ENR
     _FldNameList[2]   = Temp-Tables.list_mtrl.BENAMNING
     _FldNameList[3]   > Temp-Tables.list_mtrl.ANTAL
"list_mtrl.ANTAL" ? ">>>>>9" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   = Temp-Tables.list_mtrl.ENHET
     _FldNameList[5]   = Temp-Tables.list_mtrl.PRIS
     _FldNameList[6]   > Temp-Tables.list_mtrl.LEVKOD
"list_mtrl.LEVKOD" "Lev-id" ? "character" ? ? ? ? ? ? no ? no no "2.13" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_BERLEV */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_HLEV
/* Query rebuild information for BROWSE BRW_HLEV
     _TblList          = "Temp-Tables.mtrltemp"
     _Options          = "NO-LOCK"
     _OrdList          = "rt9.MTRL.Enr|yes"
     _FldNameList[1]   = Temp-Tables.mtrltemp.Enr
     _FldNameList[2]   > Temp-Tables.mtrltemp.Benamning
"mtrltemp.Benamning" ? "x(256)" "character" ? ? ? ? ? ? no ? no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.mtrltemp.Enhet
"mtrltemp.Enhet" ? "x(7)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   = Temp-Tables.mtrltemp.NPRIS
     _Query            is NOT OPENED
*/  /* BROWSE BRW_HLEV */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_KALKSUM
/* Query rebuild information for BROWSE BRW_KALKSUM
     _TblList          = "Temp-Tables.sumkalk_temp"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > Temp-Tables.sumkalk_temp.ARBKOD
"sumkalk_temp.ARBKOD" "Kod" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.sumkalk_temp.LOPNR
"sumkalk_temp.LOPNR" "Löpnr" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.sumkalk_temp.BENAMNING
"sumkalk_temp.BENAMNING" "Benämning" "x(256)" "character" ? ? ? ? ? ? no ? no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.sumkalk_temp.ANTAL
"sumkalk_temp.ANTAL" "Antal" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.sumkalk_temp.ENHET
"sumkalk_temp.ENHET" "Enhet" ? "character" ? ? ? ? ? ? no ? no no ".63" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_KALKSUM */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_KOD
/* Query rebuild information for BROWSE BRW_KOD
     _TblList          = "Temp-Tables.bytkalk_temp"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > Temp-Tables.bytkalk_temp.ARBKOD
"bytkalk_temp.ARBKOD" "Kod" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.bytkalk_temp.LOPNR
"bytkalk_temp.LOPNR" "Löp!nr" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.bytkalk_temp.BENAMNING
"bytkalk_temp.BENAMNING" "Benämning" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.bytkalk_temp.ENHET
"bytkalk_temp.ENHET" "Enhet" ? "character" ? ? ? ? ? ? no ? no no ".13" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_KOD */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_LOP
/* Query rebuild information for BROWSE BRW_LOP
     _TblList          = "Temp-Tables.loptemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.loptemp.LOPNR
"loptemp.LOPNR" "Löpnr" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.loptemp.ENHET
"loptemp.ENHET" "Enh" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.loptemp.BENAMNING
"loptemp.BENAMNING" "Benämning" "X(256)" "character" ? ? ? ? ? ? no ? no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_LOP */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_MTRL
/* Query rebuild information for BROWSE BRW_MTRL
     _TblList          = "Temp-Tables.spec_mtrl"
     _Options          = "NO-LOCK"
     _FldNameList[1]   = Temp-Tables.spec_mtrl.Enr
     _FldNameList[2]   > Temp-Tables.spec_mtrl.Benamning
"spec_mtrl.Benamning" ? "x(256)" "character" ? ? ? ? ? ? no ? no no "38" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.spec_mtrl.ANTAL
"spec_mtrl.ANTAL" ? ">>>>>9" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.spec_mtrl.Enhet
"spec_mtrl.Enhet" ? ? "character" ? ? ? ? ? ? no ? no no "5.13" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   = Temp-Tables.spec_mtrl.NPRIS
     _Query            is NOT OPENED
*/  /* BROWSE BRW_MTRL */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_P
/* Query rebuild information for BROWSE BRW_P
     _TblList          = "Temp-Tables.ptemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.ptemp.ARBKOD
"ptemp.ARBKOD" "Kod" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.ptemp.BENAMNING
"ptemp.BENAMNING" "Benämning" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_P */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_SATS
/* Query rebuild information for BROWSE BRW_SATS
     _TblList          = "Temp-Tables.satstemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   = Temp-Tables.satstemp.KOD
     _FldNameList[2]   > Temp-Tables.satstemp.BENAMNING
"satstemp.BENAMNING" ? "x(256)" "character" ? ? ? ? ? ? no ? no no "40" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_SATS */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-A
/* Query rebuild information for FRAME FRAME-A
     _Query            is NOT OPENED
*/  /* FRAME FRAME-A */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME BRW_BER
&Scoped-define FRAME-NAME FRAME-ERSATT
&Scoped-define SELF-NAME BRW_BER
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_BER WINDOW-1
ON ROW-LEAVE OF BRW_BER IN FRAME FRAME-ERSATT /* Sammanställning beredningsmateriel */
DO:
  IF AVAILABLE list_mtrl THEN DO:
         DISPLAY list_mtrl.ENR WITH BROWSE BRW_BER NO-ERROR.
      END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_HLEV
&Scoped-define SELF-NAME BRW_HLEV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_HLEV WINDOW-1
ON MOUSE-SELECT-DBLCLICK OF BRW_HLEV IN FRAME FRAME-ERSATT /* Från */
DO:
   APPLY "CHOOSE" TO BTN_OVER.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_HLEV WINDOW-1
ON START-SEARCH OF BRW_HLEV IN FRAME FRAME-ERSATT /* Från */
DO:
  /* APPLY "END-SEARCH" TO SELF.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_LOP
&Scoped-define FRAME-NAME FRAME-ERSKALK
&Scoped-define SELF-NAME BRW_LOP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_LOP WINDOW-1
ON MOUSE-MENU-CLICK OF BRW_LOP IN FRAME FRAME-ERSKALK /* Löpnr kopplade till arbetskoden */
DO:
  /* RUN entryantal_UI.
   BRW_LOP:DESELECT-ROWS() IN FRAME {&FRAME-NAME} NO-ERROR.*/   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_LOP WINDOW-1
ON MOUSE-SELECT-DBLCLICK OF BRW_LOP IN FRAME FRAME-ERSKALK /* Löpnr kopplade till arbetskoden */
DO:
   /*RUN entryantal_UI.
   RUN btnskapa_UI.*/
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_LOP WINDOW-1
ON VALUE-CHANGED OF BRW_LOP IN FRAME FRAME-ERSKALK /* Löpnr kopplade till arbetskoden */
DO:
   ASSIGN
   status-ok = BRW_P:SELECT-FOCUSED-ROW() NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_P
&Scoped-define SELF-NAME BRW_P
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_P WINDOW-1
ON MOUSE-SELECT-DBLCLICK OF BRW_P IN FRAME FRAME-ERSKALK /* Arbetskoder */
DO:
    /*APPLY "CHOOSE" TO BTN_OVER.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_P WINDOW-1
ON VALUE-CHANGED OF BRW_P IN FRAME FRAME-ERSKALK /* Arbetskoder */
DO:
   ASSIGN
   status-ok = BRW_P:SELECT-FOCUSED-ROW() NO-ERROR.
   IF AVAILABLE ptemp THEN DO:
      FIND FIRST loptemp WHERE loptemp.ARBKOD = ptemp.ARBKOD AND loptemp.KATAR = ptemp.KATAR NO-ERROR.
      IF AVAILABLE loptemp THEN ASSIGN BRW_LOP:TITLE = "Löpnr kopplade till " + loptemp.ARBKOD + "- Typ" + STRING(loptemp.TYP).
   
      /*BRW_LOP:TITLE = "Löpnr kopplade till " + ptemp.ARBKOD.*/
      RUN setcolsortvar_UI IN brwproc[6] (INPUT " WHERE loptemp.ARBKOD = '" + ptemp.ARBKOD + "'").
      RUN openbdynspec_UI IN brwproc[6].   
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_SATS
&Scoped-define FRAME-NAME FRAME-ERSATT
&Scoped-define SELF-NAME BRW_SATS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_SATS WINDOW-1
ON MOUSE-MENU-CLICK OF BRW_SATS IN FRAME FRAME-ERSATT /* Från */
DO:
   {muswait.i}
   
   satstrue = TRUE.
   RUN SATSINNEU.W (INPUT ROWID(satstemp)).
   satstrue = FALSE.
   
   {musarrow.i}
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_SATS WINDOW-1
ON MOUSE-SELECT-DBLCLICK OF BRW_SATS IN FRAME FRAME-ERSATT /* Från */
DO:
   /*
   {muswait.i}
   
   satstrue = TRUE.
   RUN SATSINNEU.W (INPUT ROWID(satstemp)).
   satstrue = FALSE.
   
   {musarrow.i}
   */
   APPLY "CHOOSE" TO BTN_OVER.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_SATS WINDOW-1
ON START-SEARCH OF BRW_SATS IN FRAME FRAME-ERSATT /* Från */
DO:
   APPLY "END-SEARCH" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_ALLBACK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_ALLBACK WINDOW-1
ON CHOOSE OF BTN_ALLBACK IN FRAME FRAME-ERSATT /* Alla aonr i listan */
DO:
   EMPTY TEMP-TABLE spec_mtrl NO-ERROR. 
   RUN openbdynspec_UI IN brwproc[3].   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_ANTAL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_ANTAL WINDOW-1
ON CHOOSE OF BTN_ANTAL IN FRAME FRAME-ERSATT /* Antal */
DO:             
   status-ok = BRW_MTRL:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.   
   ASSIGN  
   musz = FALSE.  
   
   RUN BYTANTALU.W (INPUT ROWID(spec_mtrl)).
   RUN refreshbrw_UI IN brwproc[3].
   
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-A
&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB WINDOW-1
ON CHOOSE OF BTN_AVB IN FRAME FRAME-A /* Avsluta */
DO:
   EMPTY TEMP-TABLE list_mtrl NO-ERROR. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-ERSATT
&Scoped-define SELF-NAME btn_back
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_back WINDOW-1
ON CHOOSE OF btn_back IN FRAME FRAME-ERSATT
DO:   
   antal_valda = BRW_MTRL:NUM-SELECTED-ROWS.
   antal_raknare = 1.
   DO WHILE antal_raknare LE antal_valda:                                   
      status-ok = BRW_MTRL:FETCH-SELECTED-ROW(antal_raknare) NO-ERROR.                      
      DELETE spec_mtrl.
      antal_raknare = antal_raknare + 1.   
   END.    
   RUN refreshbrw_UI IN brwproc[3].
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-ERSKALK
&Scoped-define SELF-NAME btn_backk
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_backk WINDOW-1
ON CHOOSE OF btn_backk IN FRAME FRAME-ERSKALK
DO:
   EMPTY TEMP-TABLE bytkalk_temp NO-ERROR.
   RUN refreshbrw_UI IN brwproc[9].     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-ERSATT
&Scoped-define SELF-NAME BTN_BYT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_BYT WINDOW-1
ON CHOOSE OF BTN_BYT IN FRAME FRAME-ERSATT /* Byt mtrl */
DO:   
   status-ok = BRW_BER:SELECT-FOCUSED-ROW() IN FRAME FRAME-ERSATT NO-ERROR.
   status-ok = BRW_MTRL:SELECT-FOCUSED-ROW() IN FRAME FRAME-ERSATT NO-ERROR.
   DEFINE VARIABLE enrvarhj AS CHARACTER NO-UNDO.
   enrvarhj = "".
   {muswait.i}
   svar = FALSE.
   IF RAD_VAL = 1 THEN DO:
      IF NOT AVAILABLE spec_mtrl THEN DO: 
         MESSAGE "Ni måste välja en artiklel att ersätta med." VIEW-AS ALERT-BOX.
         RETURN NO-APPLY.
      END.
      FIND FIRST spec_mtrl WHERE spec_mtrl.ENR = list_mtrl.ENR NO-LOCK NO-ERROR.
      IF AVAILABLE spec_mtrl THEN DO:
         MESSAGE "Du kan inte ersätta samma " LC(Guru.Konstanter:genk) "!" VIEW-AS ALERT-BOX.
         RETURN NO-APPLY.
      END.
      antal_valda = 0.
      FOR EACH spec_mtrl:
         antal_valda = antal_valda + 1.
      END.            
      IF antal_valda > 1 THEN DO:
         MESSAGE "Ni kan endast ersätta en artikel mot en annan." 
         VIEW-AS ALERT-BOX TITLE "Meddelande".         
         RETURN NO-APPLY.
      END.
      MESSAGE "Är det korrekt att Ni vill ersätta " + LC(Guru.Konstanter:genk) + ":" + list_mtrl.ENR + " med vald materiel?" 
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Meddelande" UPDATE svar.         
   END.
   ELSE IF RAD_VAL = 2 THEN DO:
      IF NOT AVAILABLE spec_mtrl THEN DO: 
         MESSAGE "Ni måste välja en artiklel att lägga till." VIEW-AS ALERT-BOX.
         RETURN NO-APPLY.
      END.
      FIND FIRST spec_mtrl WHERE spec_mtrl.ENR = list_mtrl.ENR NO-LOCK NO-ERROR.
      IF AVAILABLE spec_mtrl THEN DO:
         MESSAGE "Du kan inte lägga till samma " LC(Guru.Konstanter:genk) "!" VIEW-AS ALERT-BOX.
         RETURN NO-APPLY.
      END.
      MESSAGE "Är det korrekt att Ni vill lägga till vald materiel på alla ställen där " + LC(Guru.Konstanter:genk) + ":" + list_mtrl.ENR + " förekommer?" 
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Meddelande" UPDATE svar.         
   END.
   ELSE DO:
      MESSAGE "Är det korrekt att Ni vill ta bort materiel " + LC(Guru.Konstanter:genk) + ":" + list_mtrl.ENR + " på alla ställen där det förekommer?" 
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Meddelande" UPDATE svar.         
   END.
   IF svar THEN DO:
      IF Guru.Konstanter:appcon THEN DO:           
         RUN PBERXTIY.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
         (INPUT RAD_VAL, INPUT TABLE spec_mtrl, INPUT list_mtrl.ENR, INPUT list_mtrl.LEVKOD,
         INPUT valaonr, INPUT valomrade).
      END.
      ELSE DO:
         RUN PBERXTIY.P
         (INPUT RAD_VAL, INPUT TABLE spec_mtrl, INPUT list_mtrl.ENR, INPUT list_mtrl.LEVKOD, 
         INPUT valaonr, INPUT valomrade).
      END.
      IF RAD_VAL = 1 THEN DO:
         FOR EACH spec_mtrl:
            CREATE mtrlbuff.
            BUFFER-COPY list_mtrl TO mtrlbuff.
            ASSIGN
            mtrlbuff.ENR = spec_mtrl.ENR
            mtrlbuff.BENAMNING = spec_mtrl.BENAMNING
            mtrlbuff.ENHET = spec_mtrl.ENHET
            mtrlbuff.PRIS = spec_mtrl.NPRIS
            mtrlbuff.LEVKOD = spec_mtrl.LEVKOD
            mtrl_rowid = ROWID(mtrlbuff).                        
         END.   
         DELETE list_mtrl.
         
         RUN openbdynspec_UI IN brwproc[1].
         RUN setlastrowid_UI IN brwproc[1] (INPUT mtrl_rowid).
         RUN lastselectdyn_UI IN brwproc[1].
         RUN openbdynspec_UI IN brwproc[5].
         EMPTY TEMP-TABLE spec_mtrl NO-ERROR. 
         RUN openbdynspec_UI IN brwproc[3].
      END.
      ELSE IF RAD_VAL = 2 THEN DO:
         FIND FIRST spec_mtrl NO-LOCK NO-ERROR.
         enrvarhj = spec_mtrl.ENR.
         EMPTY TEMP-TABLE list_mtrl NO-ERROR.          
         RUN hamta_UI.         
         RUN openbdynspec_UI IN brwproc[1].
         FIND FIRST list_mtrl WHERE list_mtrl.ENR = enrvarhj NO-LOCK NO-ERROR.
         IF AVAILABLE list_mtrl THEN DO:
            RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(list_mtrl)).
            RUN lastselectdyn_UI IN brwproc[1].
         END.
         RUN openbdynspec_UI IN brwproc[5].
      END.
      ELSE IF RAD_VAL = 3 THEN DO:
         enrvarhj = list_mtrl.ENR.
         RUN getnext_UI IN brwproc[1].
         IF NOT AVAILABLE list_mtrl THEN DO:
            FIND FIRST list_mtrl WHERE list_mtrl.ENR = enrvarhj NO-LOCK NO-ERROR.
            RUN getprev_UI IN brwproc[1].
         END.
         IF AVAILABLE list_mtrl THEN enrvarhj = list_mtrl.ENR.
         EMPTY TEMP-TABLE list_mtrl NO-ERROR.          
         RUN hamta_UI.         
         RUN openbdynspec_UI IN brwproc[1].
         FIND FIRST list_mtrl WHERE list_mtrl.ENR = enrvarhj NO-LOCK NO-ERROR.
         IF AVAILABLE list_mtrl THEN DO:
            RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(list_mtrl)).
            RUN lastselectdyn_UI IN brwproc[1].
         END.
         RUN openbdynspec_UI IN brwproc[5].
      END.
   END.
   ELSE DO:
      RETURN NO-APPLY.
   END.            
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-ERSKALK
&Scoped-define SELF-NAME BTN_BYTK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_BYTK WINDOW-1
ON CHOOSE OF BTN_BYTK IN FRAME FRAME-ERSKALK /* Byt kalkylkod */
DO:   
   status-ok = BRW_KALKSUM:SELECT-FOCUSED-ROW() IN FRAME FRAME-ERSKALK NO-ERROR.
   status-ok = BRW_KOD:SELECT-FOCUSED-ROW() IN FRAME FRAME-ERSKALK NO-ERROR.
   DEFINE VARIABLE enrvarhj AS CHARACTER NO-UNDO.
   enrvarhj = "".
   {muswait.i}
   svar = FALSE.
   DEBUGGER:SET-BREAK().
   IF RAD_VALK = 1 THEN DO:
      IF NOT AVAILABLE bytkalk_temp THEN DO: 
         MESSAGE "Ni måste välja en kalkylkod att ersätta med." VIEW-AS ALERT-BOX.
         RETURN NO-APPLY.
      END.
      IF bytkalk_temp.ARBKOD = sumkalk_temp.ARBKOD AND bytkalk_temp.LOPNR = sumkalk_temp.LOPNR THEN DO:
         MESSAGE "Du kan inte ersätta samma kalkylkod!" VIEW-AS ALERT-BOX.
         RETURN NO-APPLY.
      END.   
    
      MESSAGE "Är det korrekt att Ni vill ersätta kalkylkod:" + sumkalk_temp.ARBKOD + STRING(sumkalk_temp.LOPNR,">99") + " med följande kalkylkod: " + bytkalk_temp.ARBKOD + STRING(bytkalk_temp.LOPNR,">99") +  "?" 
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Meddelande" UPDATE svar.         
   END.
   ELSE IF RAD_VALK = 2 THEN DO:
      IF NOT AVAILABLE bytkalk_temp THEN DO: 
         MESSAGE "Ni måste välja en kalkylkod att lägga till." VIEW-AS ALERT-BOX.
         RETURN NO-APPLY.
      END.
      IF bytkalk_temp.ARBKOD = sumkalk_temp.ARBKOD AND bytkalk_temp.LOPNR = sumkalk_temp.LOPNR THEN DO:
         MESSAGE "Du kan inte lägga till samma kalkylkod!" VIEW-AS ALERT-BOX.
         RETURN NO-APPLY.
      END.      
      MESSAGE "Är det korrekt att Ni vill lägga till följande kalkylkod: " + bytkalk_temp.ARBKOD + STRING(bytkalk_temp.LOPNR,">99") + " på alla ställen där kalkylkod :" + sumkalk_temp.ARBKOD + STRING(sumkalk_temp.LOPNR,">99") + " förekommer?" 
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Meddelande" UPDATE svar.         
   END.
   ELSE DO:
      IF NOT AVAILABLE sumkalk_temp THEN DO: 
         MESSAGE "Ni måste välja den kalkylkod som skall tas bort." VIEW-AS ALERT-BOX.
         RETURN NO-APPLY.
      END.
      MESSAGE "Är det korrekt att Ni vill ta bort kalkylkod :" + sumkalk_temp.ARBKOD + STRING(sumkalk_temp.LOPNR,">99") + " på alla ställen där det förekommer?" 
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Meddelande" UPDATE svar.         
   END.
   IF svar THEN DO:     
      EMPTY TEMP-TABLE ebytkalk_temp  NO-ERROR.
      EMPTY TEMP-TABLE esumkalk_temp  NO-ERROR.
      
      CREATE esumkalk_temp.
      BUFFER-COPY sumkalk_temp TO esumkalk_temp.
      IF AVAILABLE bytkalk_temp THEN DO:
         CREATE ebytkalk_temp.
         BUFFER-COPY bytkalk_temp TO ebytkalk_temp.
      END.    
      RUN uppdatberkalk_UI IN extrkodapph (INPUT valaonr,INPUT valomrade, INPUT RAD_VALK,  INPUT TABLE esumkalk_temp, INPUT TABLE ebytkalk_temp).  
      FIND FIRST  ebytkalk_temp NO-LOCK NO-ERROR.
      FIND FIRST esumkalk_temp NO-LOCK NO-ERROR.
      IF RAD_VALK = 1 THEN DO:         
         FIND FIRST sumkalk_temp  WHERE sumkalk_temp.ARBKOD = esumkalk_temp.ARBKOD AND sumkalk_temp.LOPNR = esumkalk_temp.LOPNR NO-ERROR.
         IF AVAILABLE sumkalk_temp THEN DO:
            ASSIGN 
            sumkalk_temp.ARBKOD = ebytkalk_temp.ARBKOD 
            sumkalk_temp.LOPNR = ebytkalk_temp.LOPNR.
            berkalk_rowid = ROWID(sumkalk_temp).
         END.   
         EMPTY TEMP-TABLE bytkalk_temp NO-ERROR. 
         RUN openbdynspec_UI IN brwproc[9].
          
         RUN openbdynspec_UI IN brwproc[8].
         RUN setlastrowid_UI IN brwproc[8] (INPUT berkalk_rowid).
         RUN lastselectdyn_UI IN brwproc[8].
         
      END.
      ELSE IF RAD_VALK = 2 THEN DO:         
         CREATE sumkalkbuff.          
         BUFFER-COPY sumkalk_temp TO sumkalkbuff .
         ASSIGN 
         sumkalkbuff.ARBKOD = ebytkalk_temp.ARBKOD 
         sumkalkbuff.LOPNR = ebytkalk_temp.LOPNR.
         berkalk_rowid = ROWID(sumkalkbuff).
         
         EMPTY TEMP-TABLE bytkalk_temp NO-ERROR. 
         RUN openbdynspec_UI IN brwproc[9].
          
         RUN openbdynspec_UI IN brwproc[8].
         RUN setlastrowid_UI IN brwproc[8] (INPUT berkalk_rowid).
         RUN lastselectdyn_UI IN brwproc[8].        
      END.
      ELSE IF RAD_VALK = 3 THEN DO:
         DELETE sumkalk_temp.         
         RUN selnextprevrow_UI IN brwproc[8].
         RUN openbdyn_UI IN brwproc[8] (INPUT "").
         RUN lastselectdyn_UI IN brwproc[8].
         EMPTY TEMP-TABLE bytkalk_temp NO-ERROR. 
         RUN openbdynspec_UI IN brwproc[9].                 
      END.
   END.
   ELSE DO:
      RETURN NO-APPLY.
   END.            
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-LEVBYT
&Scoped-define SELF-NAME BTN_BYTLEV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_BYTLEV WINDOW-1
ON CHOOSE OF BTN_BYTLEV IN FRAME FRAME-LEVBYT /* Byt leverantör */
DO:  
   
   {muswait.i} 
   ASSIGN    
   skrivut = FALSE.   
   CMB_LEVTILL = INPUT CMB_LEVTILL.
   IF CMB_LEVTILL BEGINS globanv THEN lkod = "99" + Guru.Konstanter:globanv.
   ELSE DO:
      FIND FIRST levtemp WHERE levtemp.LEVNAMN = CMB_LEVTILL 
      USE-INDEX LEV NO-LOCK NO-ERROR.
      lkod = levtemp.LEVKOD.
   END.        
   antal_valda = BRW_BERLEV:NUM-SELECTED-ROWS.
   IF antal_valda = 0 THEN DO:      
      MESSAGE "Ingen artikel är markerad." VIEW-AS ALERT-BOX.          
      RETURN NO-APPLY.
   END.
   
   EMPTY TEMP-TABLE elist_mtrl NO-ERROR. 
   MESSAGE "OBS! Vill Ni byta levarantör på alla markerade artiklar?"
   VIEW-AS ALERT-BOX
   QUESTION BUTTONS YES-NO TITLE "Byte av leverantör?" UPDATE svar.         
   IF svar THEN DO:
      antal_valda = BRW_BERLEV:NUM-SELECTED-ROWS.
      antal_raknare = 1.
      IF antal_valda > 0 THEN DO:
         DO WHILE antal_raknare LE antal_valda:                                   
            status-ok = BRW_BERLEV:FETCH-SELECTED-ROW(antal_raknare).
            RUN setlastrowid_UI IN brwproc[2] (INPUT ROWID(list_mtrl)).            
            CREATE elist_mtrl.
            BUFFER-COPY list_mtrl TO elist_mtrl.            
            antal_raknare = antal_raknare + 1.   
         END.              
         {AVBGOM.I}                               
         RUN VBYTLEVA.W (INPUT lkod,INPUT TABLE elist_mtrl,INPUT antmtrlber).
         {AVBFRAM.I}  
         EMPTY TEMP-TABLE list_mtrl NO-ERROR.          
         EMPTY TEMP-TABLE elist_mtrl NO-ERROR. 
         RUN hamta_UI.                  
         RUN openbdyn_UI IN brwproc[5] (INPUT "").
         RUN openbdyn_UI IN brwproc[1] (INPUT "").         
         status-ok = BRW_BERLEV:DESELECT-ROWS().
      END.
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-ERSATT
&Scoped-define SELF-NAME btn_over
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_over WINDOW-1
ON CHOOSE OF btn_over IN FRAME FRAME-ERSATT
DO:
   IF satsvar = FALSE THEN DO:   
      antal_valda = BRW_HLEV:NUM-SELECTED-ROWS.         
      antal_raknare = 1.
      IF antal_valda > 0 THEN DO:
         DO WHILE antal_raknare LE antal_valda:                                   
            status-ok = BRW_HLEV:FETCH-SELECTED-ROW(antal_raknare).
            RUN over_UI.
            antal_raknare = antal_raknare + 1.
         END.    
         RUN openbdyn_UI IN brwproc[3] (INPUT "").
         FIND FIRST spec_mtrl NO-LOCK NO-ERROR.
         IF AVAILABLE spec_mtrl THEN DO:
            RUN setlastrowid_UI IN brwproc[3] (INPUT ROWID(spec_mtrl)).
            RUN lastselectdyn_UI IN brwproc[3].
         END.         
         status-ok = BRW_HLEV:DESELECT-ROWS().
      END.            
   END.
   ELSE DO:
      ASSIGN
      antal_valda = BRW_SATS:NUM-SELECTED-ROWS
      antal_raknare = 1.
      DO WHILE antal_raknare LE antal_valda:                                   
         status-ok = BRW_SATS:FETCH-SELECTED-ROW(antal_raknare).                      
         RUN over2_UI.    
         antal_raknare = antal_raknare + 1.
      END.
      RUN openbdyn_UI IN brwproc[3] (INPUT "").
      FIND FIRST spec_mtrl NO-LOCK NO-ERROR.
      IF AVAILABLE spec_mtrl THEN DO:
         RUN setlastrowid_UI IN brwproc[3] (INPUT ROWID(spec_mtrl)).
         RUN lastselectdyn_UI IN brwproc[3].
      END.  
      status-ok = BRW_SATS:DESELECT-ROWS().
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-ERSKALK
&Scoped-define SELF-NAME btn_overk
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_overk WINDOW-1
ON CHOOSE OF btn_overk IN FRAME FRAME-ERSKALK
DO:      
   EMPTY TEMP-TABLE bytkalk_temp NO-ERROR.       
   antal_valda = BRW_LOP:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME}.
   antal_raknare = 1.
   IF antal_valda = 0 THEN RETURN NO-APPLY.   
   DO WHILE antal_raknare LE antal_valda:
      status-ok = BRW_LOP:FETCH-SELECTED-ROW(antal_raknare) NO-ERROR.
      FIND FIRST bytkalk_temp WHERE  bytkalk_temp.ARBKOD = loptemp.ARBKOD AND bytkalk_temp.LOPNR = loptemp.LOPNR NO-LOCK NO-ERROR.
      IF NOT AVAILABLE bytkalk_temp THEN DO:
         CREATE bytkalk_temp.
         ASSIGN                               
         bytkalk_temp.ARBKOD = loptemp.ARBKOD
         bytkalk_temp.LOPNR = loptemp.LOPNR
         bytkalk_temp.BENAMNING = SUBSTRING(loptemp.BENAMNING,1,49)
         bytkalk_temp.ENHET = loptemp.ENHET.                       
      END.
      antal_raknare = antal_raknare + 1.
   END.
   RUN openbdynspec_UI IN brwproc[9].
   RUN lastselectdyn_UI IN brwproc[9].                           
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-ERSATT
&Scoped-define SELF-NAME CMB_LEV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_LEV WINDOW-1
ON VALUE-CHANGED OF CMB_LEV IN FRAME FRAME-ERSATT /* Leverantörer */
DO:                                 
   ASSIGN           
   lev = INPUT CMB_LEV.   
   IF SUBSTRING(lev,1,5) = "Sats " THEN DO:
      ASSIGN                      
      BRW_HLEV:HIDDEN = TRUE               
      FILL-IN-ENR:HIDDEN = TRUE      
      BRW_SATS:HIDDEN = FALSE         
      FILL-IN-KOD:HIDDEN = FALSE 
      satsvar = TRUE.
      RUN setorgtitle_UI IN brwproc[4] (INPUT "Satslista " + lev).         
      FIND FIRST levtemp WHERE levtemp.LEVNAMN = SUBSTRING(lev,6,20) 
      USE-INDEX LEV NO-LOCK NO-ERROR.
      vald_lev = levtemp.LEVKOD.
   END.
   ELSE DO:
      ASSIGN                      
      BRW_HLEV:HIDDEN = FALSE 
      FILL-IN-ENR:HIDDEN = FALSE
      BRW_SATS:HIDDEN = TRUE
      FILL-IN-KOD:HIDDEN = TRUE                 
      satsvar = FALSE.
      IF lev BEGINS globanv THEN vald_lev = "99" + Guru.Konstanter:globanv.
      ELSE DO:
         FIND FIRST levtemp WHERE levtemp.LEVNAMN = lev 
         USE-INDEX LEV NO-LOCK NO-ERROR.
         vald_lev = levtemp.LEVKOD.
      END.     
      levinlast = "".
      RUN levinlast_UI (INPUT vald_lev,OUTPUT levinlast).
      RUN setorgtitle_UI IN brwproc[2] (INPUT "Materiellista " + lev + levinlast).         
      RUN setcolsortvar_UI IN brwproc[2] (INPUT " WHERE KALKNR = 0 AND LEVKOD = '" + STRING(vald_lev) + "' USE-INDEX ENR ").
      RUN openbdynspec_UI IN brwproc[2].
   END.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-LEVBYT
&Scoped-define SELF-NAME CMB_LEVTILL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_LEVTILL WINDOW-1
ON VALUE-CHANGED OF CMB_LEVTILL IN FRAME FRAME-LEVBYT /* Till lev */
DO:                                 
   lev2 = INPUT CMB_LEVTILL.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-ERSATT
&Scoped-define SELF-NAME FILL-IN-BEN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-BEN WINDOW-1
ON ANY-KEY OF FILL-IN-BEN IN FRAME FRAME-ERSATT /* Ben */
DO:
   {TRYCKS.I}
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN-BEN IN FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-BEN WINDOW-1
ON MOUSE-SELECT-DBLCLICK OF FILL-IN-BEN IN FRAME FRAME-ERSATT /* Ben */
DO: 
   {muswait.i}
   {BENHMT2.I}      
   RUN initsok_UI (INPUT 1,INPUT aosok).
   {musarrow.i}                                 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-ENR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-ENR WINDOW-1
ON ANY-KEY OF FILL-IN-ENR IN FRAME FRAME-ERSATT /* Enr */
DO:
   {TRYCKS.I}
   IF KEYFUNCTION(LASTKEY) = ("END-ERROR") THEN  RETURN NO-APPLY. 
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN-ENR IN FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-ENR WINDOW-1
ON MOUSE-SELECT-DBLCLICK OF FILL-IN-ENR IN FRAME FRAME-ERSATT /* Enr */
DO: 
   {muswait.i}
   {ENRHMT2.I}
   RUN initsok_UI (INPUT 2,INPUT posok).
   {musarrow.i}                   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-KOD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-KOD WINDOW-1
ON ANY-KEY OF FILL-IN-KOD IN FRAME FRAME-ERSATT /* Kod */
DO:
   {TRYCKS.I}
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN-KOD IN FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-KOD WINDOW-1
ON MOUSE-SELECT-DBLCLICK OF FILL-IN-KOD IN FRAME FRAME-ERSATT /* Kod */
DO: 
   {muswait.i}
   {ENRHMT3.I}
   felmedd = "".
   RUN kodhmt_UI IN mtrlhmtapph (INPUT posok,INPUT begvar,
                                    INPUT vald_lev,OUTPUT felmedd,OUTPUT TABLE satstemp).
   IF felmedd NE "" THEN DO:
      MESSAGE felmedd VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN-KOD IN FRAME {&FRAME-NAME}.
      RETURN.
   END.
   ELSE DO:
      RUN setcolsortvar_UI IN brwproc[4] (INPUT " LEVKOD = '" + STRING(vald_lev) + "' AND SATS = TRUE ").
      RUN openbdynspec_UI IN brwproc[4].
      FIND FIRST satstemp NO-LOCK NO-ERROR.
      IF AVAILABLE satstemp THEN DO:  
         RUN setlastrowid_UI IN brwproc[4] (INPUT ROWID(satstemp)).
         RUN lastselectdyn_UI IN brwproc[4].
      END.      
   END.            
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-A
&Scoped-define SELF-NAME RAD_ERSLEV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RAD_ERSLEV WINDOW-1
ON VALUE-CHANGED OF RAD_ERSLEV IN FRAME FRAME-A
DO:
  RAD_ERSLEV = INPUT RAD_ERSLEV.
  IF RAD_ERSLEV = 1 THEN DO:
     ASSIGN FRAME FRAME-LEVBYT:HIDDEN = TRUE.
     ASSIGN FRAME FRAME-ERSATT:HIDDEN = FALSE.
     ASSIGN FRAME FRAME-ERSKALK:HIDDEN = TRUE.
  END.
  IF  RAD_ERSLEV = 2 THEN DO:
     ASSIGN FRAME FRAME-LEVBYT:HIDDEN = FALSE.
     ASSIGN FRAME FRAME-ERSATT:HIDDEN = TRUE.
     ASSIGN FRAME FRAME-ERSKALK:HIDDEN = TRUE.
  END.
  IF  RAD_ERSLEV = 3 THEN DO:
      ASSIGN FRAME FRAME-LEVBYT:HIDDEN = TRUE.
      ASSIGN FRAME FRAME-ERSATT:HIDDEN = TRUE.
      ASSIGN FRAME FRAME-ERSKALK:HIDDEN = FALSE.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-ERSATT
&Scoped-define SELF-NAME RAD_SOK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RAD_SOK WINDOW-1
ON VALUE-CHANGED OF RAD_SOK IN FRAME FRAME-ERSATT
DO:
  RAD_SOK = INPUT RAD_SOK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RAD_VAL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RAD_VAL WINDOW-1
ON VALUE-CHANGED OF RAD_VAL IN FRAME FRAME-ERSATT
DO:
   RAD_VAL = INPUT RAD_VAL.
   IF RAD_VAL = 1 THEN DO:
      BTN_ANTAL:HIDDEN = TRUE.
      BTN_BYT:LABEL = "Ersätt mtrl".
   END.
   ELSE IF RAD_VAL = 2 THEN DO:
      BTN_ANTAL:HIDDEN = FALSE.
      BTN_BYT:LABEL = "Lägg till mtrl".
   END.
   ELSE DO:
      BTN_ANTAL:HIDDEN = TRUE.
      BTN_BYT:LABEL = "Ta bort mtrl".
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-ERSKALK
&Scoped-define SELF-NAME RAD_VALK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RAD_VALK WINDOW-1
ON VALUE-CHANGED OF RAD_VALK IN FRAME FRAME-ERSKALK
DO:
   RAD_VALK = INPUT RAD_VALK.
   IF RAD_VALK = 1 THEN DO:      
      BTN_BYTK:LABEL = "Ersätt kalkylkod".
   END.
   ELSE IF RAD_VALK = 2 THEN DO:
      
      BTN_BYTK:LABEL = "Lägg till kalkylkod".
   END.
   ELSE DO:      
      BTN_BYTK:LABEL = "Ta bort kalkylkod".
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-A
&Scoped-define BROWSE-NAME BRW_BER
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
   IF VALID-HANDLE(laddaproch) THEN DELETE PROCEDURE laddaproch.
   IF VALID-HANDLE(mtrlhmtapph) THEN DELETE PROCEDURE mtrlhmtapph.   
   IF VALID-HANDLE(edataapph) THEN DELETE PROCEDURE edataapph.
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
   {&WINDOW-NAME}:TITLE = "Ersätt/lägg till materiel i beredning " + valaonr + " " + valort.         
   EMPTY TEMP-TABLE spec_mtrl NO-ERROR. 
   EMPTY TEMP-TABLE list_mtrl NO-ERROR.    
   EMPTY TEMP-TABLE mtrltemp NO-ERROR.    
   EMPTY TEMP-TABLE ikmtrltemp NO-ERROR. 
   EMPTY TEMP-TABLE eikmtrltemp NO-ERROR. 
   EMPTY TEMP-TABLE kalk_temp NO-ERROR.
   EMPTY TEMP-TABLE ptemp NO-ERROR. 
   EMPTY TEMP-TABLE loptemp NO-ERROR.
   RAD_VAL = 1.
   RAD_VALK = 1.   
   RUN hamtlev_UI IN mtrlhmtapph (INPUT valomrade, INPUT valaonr, OUTPUT vald_lev).   
   IF vald_lev = "" THEN DO:
      /*xtillyvar = "". */
      FIND FIRST huvlevtemp WHERE huvlevtemp.DEP-NR = 999 NO-LOCK NO-ERROR.
      IF AVAILABLE huvlevtemp THEN DO:
         vald_kundlev = huvlevtemp.LEVKOD.
      END.
      ELSE DO:
         FIND FIRST levtemp WHERE levtemp.LEVKOD NE "0"
         AND levtemp.BORTTAG = FALSE NO-LOCK NO-ERROR.
         vald_kundlev = levtemp.LEVKOD.
      END.
   END.
   ELSE DO:
      ASSIGN
      /*xtillyvar = vald_lev*/
      vald_kundlev = vald_lev.
   END.
   
   /*FIND FIRST huvlevtemp WHERE huvlevtemp.DEP-NR = 999 NO-LOCK NO-ERROR.
   IF AVAILABLE huvlevtemp THEN DO:  
      vald_kundlev = huvlevtemp.LEVKOD.
   END.                              
   ELSE DO:
      FIND FIRST levtemp WHERE levtemp.LEVKOD NE "0" 
      AND levtemp.BORTTAG = FALSE NO-LOCK NO-ERROR.
      vald_kundlev = levtemp.LEVKOD.
   END. */
   mtrltemp.ENR:LABEL IN BROWSE BRW_HLEV = Guru.Konstanter:genk.    
   ASSIGN
   status-ok = CMB_LEV:DELETE("0").   
   status-ok = CMB_LEVTILL:DELETE("0").   
   FIND FIRST levtemp WHERE levtemp.LEVKOD = vald_kundlev
   USE-INDEX LEV NO-LOCK NO-ERROR. 
   &Scoped-define FRAME-NAME FRAME-ERSATT
   
   ASSIGN
   status-ok = CMB_LEV:ADD-LAST(levtemp.LEVNAMN)IN FRAME FRAME-ERSATT
   CMB_LEV:SCREEN-VALUE = levtemp.LEVNAMN   
   
   status-ok = CMB_LEVTILL:ADD-LAST(levtemp.LEVNAMN)IN FRAME FRAME-LEVBYT
   CMB_LEVTILL:SCREEN-VALUE = levtemp.LEVNAMN   
   huvudlev = levtemp.LEVNAMN. 
   FOR EACH levtemp WHERE levtemp.LEVKOD NE vald_kundlev AND 
   levtemp.LEVKOD NE "0" AND levtemp.BORTTAG = FALSE USE-INDEX LEV NO-LOCK:      
      ASSIGN
      status-ok = CMB_LEV:ADD-LAST(levtemp.LEVNAMN)IN FRAME FRAME-ERSATT.      
      status-ok = CMB_LEVTILL:ADD-LAST(levtemp.LEVNAMN)IN FRAME FRAME-LEVBYT.
      IF levtemp.LEVKOD = "99" THEN DO:
         status-ok = CMB_LEV:ADD-LAST(globanv + " " + levtemp.LEVNAMN)IN FRAME FRAME-ERSATT.
         status-ok = CMB_LEVTILL:ADD-LAST(globanv + " " + levtemp.LEVNAMN)IN FRAME FRAME-LEVBYT.         
      END.      
   END.        
   FIND FIRST huvlevtemp WHERE huvlevtemp.DEP-NR = 999 NO-LOCK NO-ERROR.
   FOR EACH levtemp WHERE  
   levtemp.LEVKOD NE "0" AND levtemp.LEVKOD NE "99" AND
   levtemp.BORTTAG = FALSE USE-INDEX LEV NO-LOCK:
      IF AVAILABLE huvlevtemp AND  huvlevtemp.LEVKOD = levtemp.LEVKOD THEN status-ok = CMB_LEV:ADD-LAST("Sats " + levtemp.LEVNAMN).
      ELSE IF levtemp.LEVKOD = vald_kundlev THEN status-ok = CMB_LEV:ADD-LAST("Sats " + levtemp.LEVNAMN).                   
   END.        
   ASSIGN 
   CMB_LEV = INPUT CMB_LEV   
   vald_lev = vald_kundlev.

   RUN hamta_UI.
   
   
   /*OPEN QUERY BRW_BER FOR EACH list_mtrl USE-INDEX ENR NO-LOCK.*/
   RUN openbdynspec_UI IN brwproc[1].
   RUN openbdynspec_UI IN brwproc[5].
   BTN_BYT:LABEL = "Ersätt mtrl".
   RAD_ERSLEV = 1.
   IF radvad = 2 THEN DO:
      RAD_ERSLEV = 2.      
   END.       
   status-ok = RAD_ERSLEV:ADD-LAST("Ersätt/Lägg till/Ta bort - kalkylkoder.",3).
      
   IF radvad = 3 THEN DO:
      RAD_ERSLEV = 3.      
      EMPTY TEMP-TABLE sumkalk_temp NO-ERROR. 
      RUN hmtberkalk_UI IN mtrlhmtapph (INPUT valaonr,INPUT valomrade,OUTPUT TABLE sumkalk_temp).
      RUN openbdynspec_UI IN brwproc[8]. 
      IF varforetypchar[48] NE "" THEN DO:
         IF varforetypval[36] = 1 THEN DO:
            RUN Ptemp_UI IN KalkClasserStart (INPUT 2, OUTPUT TABLE ptemp APPEND).
            RUN Loptempn_UI IN KalkClasserStart (INPUT 2, OUTPUT TABLE loptemp APPEND).
         END.   
         ELSE DO:
            RUN Ptemp_UI IN KalkClasserStart (INPUT 3, OUTPUT TABLE ptemp APPEND).
            RUN Loptempn_UI IN KalkClasserStart (INPUT 3, OUTPUT TABLE loptemp APPEND).
         END.   
      END.
      ELSE RUN ladda2_UI IN extrkodapph (INPUT valaonr,INPUT valomrade,OUTPUT TABLE ptemp,OUTPUT TABLE loptemp).   
      IF varforetypval[36] = 1 THEN BRW_P:TITLE = "Arbetskoder Typ 2".
      ELSE BRW_P:TITLE = "Arbetskoder Typ 3".      
   END.
      
   RUN enable_UI.
      
   {FRMSIZEF.I}     
   IF RAD_ERSLEV = 1 THEN DO:
      ASSIGN FRAME FRAME-LEVBYT:HIDDEN = TRUE.
      ASSIGN FRAME FRAME-ERSATT:HIDDEN = FALSE.
      ASSIGN FRAME FRAME-ERSKALK:HIDDEN = TRUE.
   END.
   IF  RAD_ERSLEV = 2 THEN DO:
      ASSIGN FRAME FRAME-LEVBYT:HIDDEN = FALSE.
      ASSIGN FRAME FRAME-ERSATT:HIDDEN = TRUE.
      ASSIGN FRAME FRAME-ERSKALK:HIDDEN = TRUE.
   END.
   IF  RAD_ERSLEV = 3 THEN DO:
      ASSIGN FRAME FRAME-LEVBYT:HIDDEN = TRUE.
      ASSIGN FRAME FRAME-ERSATT:HIDDEN = TRUE.
      ASSIGN FRAME FRAME-ERSKALK:HIDDEN = FALSE.
      ENABLE BRW_P WITH FRAME FRAME-ERSKALK.
      ENABLE BRW_LOP WITH FRAME FRAME-ERSKALK.
      RUN setcolsortvar_UI IN brwproc[7] (INPUT "").
      RUN openbdynspec_UI IN brwproc[7].   
      APPLY "VALUE-CHANGED" TO BRW_P IN FRAME FRAME-ERSKALK.
   END.
    
   
   ASSIGN   
   BTN_ANTAL:HIDDEN = TRUE.   
   APPLY "VALUE-CHANGED" TO CMB_LEV.      
   ASSIGN   
   BRW_SATS:HIDDEN = TRUE
   FILL-IN-KOD:HIDDEN = TRUE. 
   
   IF Guru.Konstanter:mtrlsekvar[6] = TRUE THEN DO:
      ASSIGN
      mtrltemp.NPRIS:VISIBLE IN BROWSE BRW_HLEV = FALSE.      
      list_mtrl.PRIS:VISIBLE IN BROWSE BRW_BER = FALSE.     
      list_mtrl.PRIS:VISIBLE IN BROWSE BRW_BERLEV = FALSE.     
      spec_mtrl.NPRIS:VISIBLE IN BROWSE BRW_MTRL = FALSE.             
       
   END.
   ELSE DO:
      ASSIGN
      mtrltemp.NPRIS:VISIBLE IN BROWSE BRW_HLEV = TRUE.      
      list_mtrl.PRIS:VISIBLE IN BROWSE BRW_BER = TRUE.      
      list_mtrl.PRIS:VISIBLE IN BROWSE BRW_BERLEV = TRUE.     
      spec_mtrl.NPRIS:VISIBLE IN BROWSE BRW_MTRL = TRUE.             
   END.

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
      (INPUT BRW_BER:HANDLE IN FRAME FRAME-ERSATT).
   RUN dynprogextra IN brwproc[1] (INPUT "rowdispextra_UI",INPUT THIS-PROCEDURE).
   RUN rowdispextrakor IN  brwproc[1] (INPUT TRUE).   
   RUN DYNBRW.P PERSISTENT SET brwproc[2] 
      (INPUT BRW_HLEV:HANDLE IN FRAME FRAME-ERSATT).
   RUN dynprogextra IN brwproc[2] (INPUT "rowdispextra_UI",INPUT THIS-PROCEDURE).
   RUN rowdispextrakor IN  brwproc[2] (INPUT TRUE).   
   RUN DYNBRW.P PERSISTENT SET brwproc[3] 
      (INPUT BRW_MTRL:HANDLE IN FRAME FRAME-ERSATT).
   RUN dynprogextra IN brwproc[3] (INPUT "rowdispextra_UI",INPUT THIS-PROCEDURE).
   RUN rowdispextrakor IN  brwproc[3] (INPUT TRUE).   
   RUN DYNBRW.P PERSISTENT SET brwproc[4] 
      (INPUT BRW_SATS:HANDLE IN FRAME FRAME-ERSATT).   
   RUN DYNBRW.P PERSISTENT SET brwproc[5] 
      (INPUT BRW_BERLEV:HANDLE IN FRAME FRAME-LEVBYT).
      
   RUN DYNBRW.P PERSISTENT SET brwproc[6] 
      (INPUT BRW_LOP:HANDLE IN FRAME FRAME-ERSKALK).
   RUN DYNBRW.P PERSISTENT SET brwproc[7] 
      (INPUT BRW_P:HANDLE IN FRAME FRAME-ERSKALK).
   RUN DYNBRW.P PERSISTENT SET brwproc[8] 
      (INPUT BRW_KALKSUM:HANDLE IN FRAME FRAME-ERSKALK).
   RUN addmenuitem_UI IN brwproc[8] (INPUT BRW_KALKSUM:HANDLE,INPUT "Gå till kod",INPUT "gatillkod_UI").   
      RUN DYNBRW.P PERSISTENT SET brwproc[9] 
      (INPUT BRW_KOD:HANDLE IN FRAME FRAME-ERSKALK).   
      
   RUN dynprogextra IN brwproc[5] (INPUT "rowdispextra_UI",INPUT THIS-PROCEDURE).
   RUN rowdispextrakor IN  brwproc[5] (INPUT TRUE).   
   RUN settitlenum_UI IN brwproc[2] (INPUT TRUE).
   RUN settitlenum_UI IN brwproc[4] (INPUT TRUE).
   RUN setcolindex_UI IN brwproc[3] (INPUT "ENR").
   
   RUN addmenuitem_UI IN brwproc[1] (INPUT BRW_BER:HANDLE,INPUT "Visa information via vald Leverantör",INPUT "bervisa_UI").
   RUN addmenuitem_UI IN brwproc[2] (INPUT BRW_HLEV:HANDLE,INPUT "Visa information via vald Leverantör",INPUT "hlevvisa_UI").
   RUN addmenuitem_UI IN brwproc[2] (INPUT BRW_HLEV:HANDLE,INPUT "Avmarkera",INPUT "avmarkhlev_UI").
   RUN addmenuitem_UI IN brwproc[3] (INPUT BRW_MTRL:HANDLE,INPUT "Visa information via vald Leverantör",INPUT "mtrlvisa_UI").
   RUN addmenuitem_UI IN brwproc[3] (INPUT BRW_MTRL:HANDLE,INPUT "Avmarkera",INPUT "avmarkmtrl_UI").
   RUN addmenuitem_UI IN brwproc[4] (INPUT BRW_SATS:HANDLE,INPUT "Visa information via vald Leverantör",INPUT "satsvisa_UI").
   RUN addmenuitem_UI IN brwproc[5] (INPUT BRW_BERLEV:HANDLE,INPUT "Visa information via vald Leverantör",INPUT "berlevvisa_UI").
   
   RUN addmenuitem_UI IN brwproc[8] (INPUT BRW_KALKSUM:HANDLE,INPUT "Kommentarer",INPUT "kommentar_UI").   
   RUN addmenuitem_UI IN brwproc[6] (INPUT BRW_LOP:HANDLE,INPUT "Kommentarer",INPUT "btnkom_UI").
   IF Guru.Konstanter:appcon THEN DO:
      RUN MTRLHMT.P PERSISTENT SET mtrlhmtapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN MTRLHMT.P PERSISTENT SET mtrlhmtapph.
   END.
   tthandle = TEMP-TABLE levtemp:HANDLE.
   IF Guru.Konstanter:appcon THEN DO:
      RUN DYNLADDATEMP.P PERSISTENT SET laddaproch ON Guru.Konstanter:apphand TRANSACTION DISTINCT
         (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "LEVERANTOR", INPUT " WHERE BORTTAG = FALSE ").
   END.
   ELSE DO:
      RUN DYNLADDATEMP.P PERSISTENT SET laddaproch
         (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "LEVERANTOR", INPUT " WHERE BORTTAG = FALSE ").
   END.
   tthandle = TEMP-TABLE huvlevtemp:HANDLE.
   RUN laddatemp_UI IN laddaproch (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "HUVUDLEV", INPUT "").
   IF Guru.Konstanter:appcon THEN DO:
      RUN NETTOMARK.P PERSISTENT SET nettoh ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN NETTOMARK.P PERSISTENT SET nettoh.
   END. 
   IF Guru.Konstanter:appcon THEN DO:
      RUN EXTRADATAHMT.P PERSISTENT SET edataapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT.                  
   END.
   ELSE DO:
      RUN EXTRADATAHMT.P PERSISTENT SET edataapph.      
   END.
   
   IF varforetypchar[48] NE "" THEN DO:    
      IF NOT VALID-HANDLE(KalkClasserStart) THEN RUN Modules\Kalkyl\KalkClasserStart.P PERSISTENT SET KalkClasserStart.
   END. 
   IF Guru.Konstanter:appcon THEN DO:
      RUN EXTRKODAPP.P PERSISTENT SET extrkodapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN EXTRKODAPP.P PERSISTENT SET extrkodapph.
   END.    
   IF Guru.Konstanter:appcon = TRUE THEN DO:
      RUN FINNSTABELL.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT (INPUT "KALKBEFB", OUTPUT logresult).
   END.
   ELSE DO:
      RUN FINNSTABELL.P (INPUT "KALKBEFB", OUTPUT logresult).
   END.      
   IF logresult = TRUE THEN DO:
      IF Guru.Konstanter:appcon THEN DO:
         RUN EXTRKODBAPP.P PERSISTENT SET extrbefbapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
      END.
      ELSE DO:
         RUN EXTRKODBAPP.P PERSISTENT SET extrbefbapph.
      END. 
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE avmarkhlev_UI WINDOW-1 
PROCEDURE avmarkhlev_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/    
   &Scoped-define FRAME-NAME FRAME-ERSATT      
   status-ok = BRW_HLEV:DESELECT-ROWS() IN FRAME {&FRAME-NAME} NO-ERROR .      
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE avmarkmtrl_UI WINDOW-1 
PROCEDURE avmarkmtrl_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/    
   &Scoped-define FRAME-NAME FRAME-ERSATT      
   status-ok = BRW_MTRL:DESELECT-ROWS() IN FRAME {&FRAME-NAME} NO-ERROR .      
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE berlevvisa_UI WINDOW-1 
PROCEDURE berlevvisa_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/    
   RUN infoES_UI (INPUT 5).   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE bervisa_UI WINDOW-1 
PROCEDURE bervisa_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/    
   RUN infoES_UI (INPUT 4).   
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
  DISPLAY RAD_ERSLEV 
      WITH FRAME FRAME-A IN WINDOW WINDOW-1.
  ENABLE RAD_ERSLEV BTN_AVB 
      WITH FRAME FRAME-A IN WINDOW WINDOW-1.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  DISPLAY RAD_VAL CMB_LEV FILL-IN-ENR FILL-IN-KOD FILL-IN-BEN RAD_SOK 
          FILL-IN-SOKALT 
      WITH FRAME FRAME-ERSATT IN WINDOW WINDOW-1.
  ENABLE BTN_ALLBACK RECT-3 IMAGE-8 BRW_BER RAD_VAL BTN_BYT BRW_HLEV BRW_SATS 
         BRW_MTRL btn_over btn_back CMB_LEV BTN_ANTAL FILL-IN-ENR FILL-IN-KOD 
         FILL-IN-BEN RAD_SOK FILL-IN-SOKALT 
      WITH FRAME FRAME-ERSATT IN WINDOW WINDOW-1.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-ERSATT}
  DISPLAY RAD_VALK 
      WITH FRAME FRAME-ERSKALK IN WINDOW WINDOW-1.
  ENABLE BRW_P BRW_KALKSUM RAD_VALK BTN_BYTK BRW_LOP BRW_KOD btn_overk 
         btn_backk 
      WITH FRAME FRAME-ERSKALK IN WINDOW WINDOW-1.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-ERSKALK}
  DISPLAY CMB_LEVTILL 
      WITH FRAME FRAME-LEVBYT IN WINDOW WINDOW-1.
  ENABLE BRW_BERLEV CMB_LEVTILL BTN_BYTLEV 
      WITH FRAME FRAME-LEVBYT IN WINDOW WINDOW-1.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-LEVBYT}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE gatillkod_UI WINDOW-1 
PROCEDURE gatillkod_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   IF NOT AVAILABLE sumkalk_temp THEN DO:
      RETURN.
   END.
   IF sumkalk_temp.ARBKOD = "EGEN" THEN DO:
      RETURN.
   END.
   FIND FIRST ptemp WHERE ptemp.ARBKOD = sumkalk_temp.ARBKOD NO-LOCK NO-ERROR.
   IF AVAILABLE ptemp THEN DO:
      RUN setlastrowid_UI IN brwproc[7] (INPUT ROWID(ptemp)).        
      RUN lastselectdyn_UI IN brwproc[7].
      FIND FIRST loptemp WHERE loptemp.ARBKOD = sumkalk_temp.ARBKOD AND loptemp.LOPNR = sumkalk_temp.LOPNR NO-LOCK NO-ERROR.
      IF AVAILABLE loptemp THEN DO:
         RUN setlastrowid_UI IN brwproc[6] (INPUT ROWID(loptemp)).        
         RUN lastselectdyn_UI IN brwproc[6].
      END.
   END.
   {musarrow.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE hamta_UI WINDOW-1 
PROCEDURE hamta_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   {LISTP.I}   
   
   FOR EACH mtrl_temp BREAK BY mtrl_temp.LEVKOD BY mtrl_temp.ENR:      
      ACCUMULATE mtrl_temp.ANTAL (TOTAL BY mtrl_temp.LEVKOD BY mtrl_temp.ENR).       
      IF LAST-OF(mtrl_temp.ENR) THEN DO:
         CREATE list_mtrl.
         ASSIGN 
         list_mtrl.ENR = mtrl_temp.ENR
         list_mtrl.BENAMNING = mtrl_temp.BENAMNING 
         list_mtrl.ENHET = LC(mtrl_temp.ENHET)          
         list_mtrl.LEVKOD = mtrl_temp.LEVKOD
         list_mtrl.PRIS = mtrl_temp.PRIS
         list_mtrl.ANTAL = (ACCUM TOTAL BY mtrl_temp.ENR mtrl_temp.ANTAL).
                                          
      END.     
   END.    
   FOR EACH lin_upp:
      FIND FIRST list_mtrl WHERE list_mtrl.ENR = lin_upp.ENR AND 
      list_mtrl.LEVKOD = lin_upp.LEVKOD NO-LOCK NO-ERROR.
      IF AVAILABLE list_mtrl THEN DO:                      
         ASSIGN
         list_mtrl.ANTAL = list_mtrl.ANTAL + lin_upp.TOTMETER.        
      END.
      ELSE DO:                    
         CREATE list_mtrl.
         ASSIGN 
         list_mtrl.ENR = lin_upp.ENR
         list_mtrl.BENAMNING = lin_upp.BENAMNING 
         list_mtrl.ENHET = LC(lin_upp.ENHET)          
         list_mtrl.LEVKOD = lin_upp.LEVKOD
         list_mtrl.PRIS = mtrl_temp.PRIS
         list_mtrl.ANTAL = lin_upp.TOTMETER.
                                        
      END.
   END.                      
   FOR EACH list_mtrl: 
      IF list_mtrl.ANTAL = 0 THEN DELETE list_mtrl.
   END.
   antmtrlber = 0.  
   FOR EACH list_mtrl:
      antmtrlber = antmtrlber + 1.
   END.   
   IF varforetypval[29] = 1  THEN DO:                 
      /*nettopris beredning adm*/                
      RUN listnettomark_UI IN nettoh (INPUT TABLE list_mtrl, OUTPUT TABLE ikmtrltemp APPEND).                  
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE hlevvisa_UI WINDOW-1 
PROCEDURE hlevvisa_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/    
   RUN infoES_UI (INPUT 2).   
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
   DEFINE INPUT PARAMETER valbrw AS INTEGER NO-UNDO.   
   DEFINE VARIABLE valenr AS CHARACTER NO-UNDO.
   
   DEFINE VARIABLE levnamnvar AS CHARACTER NO-UNDO.
   
    
   /*GÅR TILL ELEKTROSKANDIAS HEMSIDA*/   
   IF valbrw = 1 THEN DO:   
      
      status-ok = BRW_MTRL:SELECT-FOCUSED-ROW() IN FRAME FRAME-ERSATT NO-ERROR.
      IF status-ok THEN DO:
         valenr = spec_mtrl.ENR.
         FIND FIRST levtemp WHERE levtemp.LEVKOD = spec_mtrl.LEVKOD
         NO-LOCK NO-ERROR.      
      END.
   END.
   ELSE IF valbrw = 2 THEN DO:
      status-ok = BRW_HLEV:SELECT-FOCUSED-ROW() IN FRAME FRAME-ERSATT NO-ERROR.
      IF status-ok THEN DO:
         valenr = mtrltemp.ENR.
         FIND FIRST levtemp WHERE levtemp.LEVKOD = mtrltemp.LEVKOD
         NO-LOCK NO-ERROR.      
      END.
   END.
   ELSE IF valbrw = 3 THEN DO:
      status-ok = BRW_SATS:SELECT-FOCUSED-ROW() IN FRAME FRAME-ERSATT NO-ERROR.
      IF status-ok THEN DO:
         valenr = satstemp.KOD.
         FIND FIRST levtemp WHERE levtemp.LEVKOD = satstemp.LEVKOD
         NO-LOCK NO-ERROR.      
      END.
   END.   
   ELSE IF valbrw = 4 THEN DO:
      status-ok = BRW_BER:SELECT-FOCUSED-ROW() IN FRAME FRAME-ERSATT NO-ERROR.
      IF status-ok THEN DO:
         valenr = list_mtrl.ENR.
         FIND FIRST levtemp WHERE levtemp.LEVKOD = list_mtrl.LEVKOD
         NO-LOCK NO-ERROR.      
      END.
   END. 
   ELSE IF valbrw = 5 THEN DO:      
      status-ok = BRW_BERLEV:SELECT-FOCUSED-ROW() IN FRAME FRAME-LEVBYT NO-ERROR.
      IF status-ok THEN DO:
         valenr = list_mtrl.ENR.
         FIND FIRST levtemp WHERE levtemp.LEVKOD = list_mtrl.LEVKOD
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
      IF satsvar = FALSE THEN DO:
         orgfraga = " WHERE KALKNR = " + STRING(0) + " AND LEVKOD = '" + STRING(vald_lev) + "' USE-INDEX LEV".
         tth = TEMP-TABLE mtrltemp:HANDLE.
         EMPTY TEMP-TABLE valsoktemp NO-ERROR. 
         CREATE valsoktemp.
         ASSIGN 
         valsoktemp.SOKCHAR[1] = "MTRL"     /*Skarp tabell*/
         valsoktemp.SOKCHAR[2] = orgfraga   /*Öppningsquery*/
         valsoktemp.SOKCHAR[3] = "BENAMNING" /*sökfält*/
         valsoktemp.SOKCHAR[4] = "MTRLROW"  /*temptabells faltnamn för rowid*/
         valsoktemp.SOKCHAR[5] = sokpa.      /*sök på*/       
         RUN sokhmt_UI IN  brwproc[2] (INPUT TABLE valsoktemp). 
      END.
      ELSE DO:
         orgfraga = " WHERE SATS = TRUE AND LEVKOD = '" + STRING(vald_lev) + "' USE-INDEX KOD ".
         tth = TEMP-TABLE satstemp:HANDLE.
         EMPTY TEMP-TABLE valsoktemp NO-ERROR. 
         CREATE valsoktemp.
         ASSIGN 
         valsoktemp.SOKCHAR[1] = "SATS"     /*Skarp tabell*/
         valsoktemp.SOKCHAR[2] = orgfraga   /*Öppningsquery*/
         valsoktemp.SOKCHAR[3] = "BENAMNING"
         valsoktemp.SOKCHAR[4] = "SATSROW"  /*temptabells faltnamn för rowid*/
         valsoktemp.SOKCHAR[5] = sokpa.      /*temptabells unika sökfaltnamn om flera skriv "aonr,delnr"*/
         RUN sokhmt_UI IN brwproc[4] (INPUT TABLE valsoktemp).                  
      END.      
      
   END. 
   IF vad = 2 THEN DO:
      IF satsvar = FALSE THEN DO:
         orgfraga = " WHERE KALKNR = " + STRING(0) + " AND LEVKOD = '" + STRING(vald_lev) + "' USE-INDEX LEV".
         tth = TEMP-TABLE mtrltemp:HANDLE.
         EMPTY TEMP-TABLE valsoktemp NO-ERROR. 
         CREATE valsoktemp.
         ASSIGN 
         valsoktemp.SOKCHAR[1] = "MTRL"     /*Skarp tabell*/
         valsoktemp.SOKCHAR[2] = orgfraga   /*Öppningsquery*/
         valsoktemp.SOKCHAR[3] = "ENR" /*sökfält*/
         valsoktemp.SOKCHAR[4] = "MTRLROW"  /*temptabells faltnamn för rowid*/
         valsoktemp.SOKCHAR[5] = sokpa.      /*sök på*/       
         RUN sokhmt_UI IN  brwproc[2] (INPUT TABLE valsoktemp). 
      END.
      ELSE DO:
         orgfraga = " WHERE SATS = TRUE AND LEVKOD = '" + STRING(vald_lev) + "' USE-INDEX KOD ".
         tth = TEMP-TABLE satstemp:HANDLE.
         EMPTY TEMP-TABLE valsoktemp NO-ERROR. 
         CREATE valsoktemp.
         ASSIGN 
         valsoktemp.SOKCHAR[1] = "SATS"     /*Skarp tabell*/
         valsoktemp.SOKCHAR[2] = orgfraga   /*Öppningsquery*/
         valsoktemp.SOKCHAR[3] = "ENR"
         valsoktemp.SOKCHAR[4] = "SATSROW"  /*temptabells faltnamn för rowid*/
         valsoktemp.SOKCHAR[5] = sokpa.      /*temptabells unika sökfaltnamn om flera skriv "aonr,delnr"*/
         RUN sokhmt_UI IN brwproc[4] (INPUT TABLE valsoktemp).                  
      END.      
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE levinlast_UI WINDOW-1 
PROCEDURE levinlast_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/  
   DEFINE INPUT PARAMETER lekod AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER levinlast AS CHARACTER NO-UNDO.   
   EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
   EMPTY TEMP-TABLE extradatatemp NO-ERROR. 
   CREATE inextradatatemp.          
   ASSIGN
   inextradatatemp.PROGRAM = "INLKAT"                   
   inextradatatemp.HUVUDCH = lekod.                                              
   RUN etabhamt_UI IN edataapph (INPUT TABLE inextradatatemp, OUTPUT TABLE extradatatemp). 
   FIND FIRST extradatatemp NO-LOCK NO-ERROR.
   IF AVAILABLE extradatatemp THEN DO:
      levinlast = " Inläst " + STRING(extradatatemp.SOKDATE[1]).
      IF extradatatemp.SOKDATE[2] NE ? AND extradatatemp.SOKDATE[2] NE ?  THEN DO:      
         RUN tooltip_UI IN brwproc[2] (INPUT "Katalogen gäller från: " + STRING(extradatatemp.SOKDATE[2]) + " till: " + STRING(extradatatemp.SOKDATE[3])).          
      END.
      ELSE RUN tooltip_UI IN brwproc[2] (INPUT "").          
   END.   
   ELSE RUN tooltip_UI IN brwproc[2] (INPUT "").             
   EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
   EMPTY TEMP-TABLE extradatatemp NO-ERROR. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE mtrlvisa_UI WINDOW-1 
PROCEDURE mtrlvisa_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/    
   RUN infoES_UI (INPUT 1).   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE over2_UI WINDOW-1 
PROCEDURE over2_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/  
   FIND FIRST spec_mtrl WHERE spec_mtrl.ENR = satstemp.ENR AND spec_mtrl.LEVKOD = satstemp.LEVKOD
   NO-LOCK NO-ERROR.
   IF AVAILABLE spec_mtrl THEN DO:
      ASSIGN
      mtrl_rowid = ROWID(spec_mtrl).            
   END.
   ELSE DO:
      CREATE spec_mtrl.
      ASSIGN      
      spec_mtrl.ENR = satstemp.ENR
      spec_mtrl.BENAMNING = satstemp.BENAMNING
      spec_mtrl.ANTAL = 1
      spec_mtrl.ENHET = satstemp.ENHET
      spec_mtrl.NPRIS = satstemp.PRIS 
      spec_mtrl.LEVKOD = satstemp.LEVKOD                 
      mtrl_rowid = ROWID(spec_mtrl).
   END.         
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE over_UI WINDOW-1 
PROCEDURE over_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/  
   FIND FIRST spec_mtrl WHERE spec_mtrl.ENR = mtrltemp.ENR AND spec_mtrl.LEVKOD = mtrltemp.LEVKOD
   NO-LOCK NO-ERROR.
   IF AVAILABLE spec_mtrl THEN DO:
      ASSIGN
      mtrl_rowid = ROWID(spec_mtrl).            
   END.
   ELSE DO:
      CREATE spec_mtrl.
      ASSIGN      
      spec_mtrl.ENR = mtrltemp.ENR
      spec_mtrl.BENAMNING = mtrltemp.BENAMNING
      spec_mtrl.ANTAL = 1
      spec_mtrl.ENHET = mtrltemp.ENHET
      spec_mtrl.NPRIS = mtrltemp.NPRIS 
      spec_mtrl.LEVKOD = mtrltemp.LEVKOD                 
      mtrl_rowid = ROWID(spec_mtrl).      
      IF varforetypval[29] = 1  THEN DO:                 
         /*nettopris beredning inköp*/
         FIND FIRST ikmtrltemp WHERE ikmtrltemp.LEVKOD = mtrltemp.LEVKOD AND ikmtrltemp.ENR = mtrltemp.ENR AND ikmtrltemp.KALKNR = 0
         NO-LOCK NO-ERROR.
         IF NOT AVAILABLE ikmtrltemp THEN DO:                        
            CREATE ikmtrltemp.
            BUFFER-COPY mtrltemp TO ikmtrltemp.                  
         END.
         /*FIND FIRST kmtrltemp WHERE kmtrltemp.LEVKOD = mtrltemp.LEVKOD AND kmtrltemp.ENR = mtrltemp.ENR AND kmtrltemp.KALKNR = 0
         NO-LOCK NO-ERROR.
         IF NOT AVAILABLE kmtrltemp THEN DO:                        
            CREATE kmtrltemp.
            BUFFER-COPY mtrltemp TO kmtrltemp.                  
         END.*/
      END.
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
   IF brwh:NAME = "BRW_BER" THEN RUN rowdispmtrl_UI (INPUT 2).    
   IF brwh:NAME = "BRW_BERLEV" THEN RUN rowdispmtrl_UI (INPUT 2).    
   IF brwh:NAME = "BRW_MTRL" THEN RUN rowdispmtrl_UI (INPUT 3).    
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
            /*nettopris gul fortum turkos övriga*/
            ASSIGN
            mtrltemp.ENR:BGCOLOR IN BROWSE BRW_HLEV = varforetypval[28]
            mtrltemp.NPRIS:BGCOLOR IN BROWSE BRW_HLEV = varforetypval[28].            
         END.
         ELSE IF mtrltemp.KUND = ? THEN DO:
            /*nettopris gul fortum turkos övriga*/
            ASSIGN
            mtrltemp.ENR:BGCOLOR IN BROWSE BRW_HLEV = varforetypval[38]
            mtrltemp.NPRIS:BGCOLOR IN BROWSE BRW_HLEV = varforetypval[38].            
         END.
      END.
   END.   
   IF vad = 2 THEN DO:
      IF AVAILABLE list_mtrl THEN DO:         
         FIND FIRST ikmtrltemp WHERE ikmtrltemp.LEVKOD = list_mtrl.LEVKOD AND ikmtrltemp.ENR = list_mtrl.ENR AND ikmtrltemp.KALKNR = 0
         AND ikmtrltemp.KUND = TRUE  NO-LOCK NO-ERROR.
         IF AVAILABLE ikmtrltemp THEN DO:                     
            ASSIGN
            list_mtrl.ENR:BGCOLOR IN BROWSE BRW_BER = varforetypval[28]
            list_mtrl.PRIS:BGCOLOR IN BROWSE BRW_BER = varforetypval[28]
            list_mtrl.ENR:BGCOLOR IN BROWSE BRW_BERLEV = varforetypval[28]
            list_mtrl.PRIS:BGCOLOR IN BROWSE BRW_BERLEV = varforetypval[28].          
         END.
         ELSE DO:         
            FIND FIRST ikmtrltemp WHERE ikmtrltemp.LEVKOD = list_mtrl.LEVKOD AND ikmtrltemp.ENR = list_mtrl.ENR AND ikmtrltemp.KALKNR = 0
            AND ikmtrltemp.KUND = ?  NO-LOCK NO-ERROR.
            IF AVAILABLE ikmtrltemp THEN DO:                     
               ASSIGN
               list_mtrl.ENR:BGCOLOR IN BROWSE BRW_BER = varforetypval[38]
               list_mtrl.PRIS:BGCOLOR IN BROWSE BRW_BER = varforetypval[38]
               list_mtrl.ENR:BGCOLOR IN BROWSE BRW_BERLEV = varforetypval[38]
               list_mtrl.PRIS:BGCOLOR IN BROWSE BRW_BERLEV = varforetypval[38].          
            END.
         END.
      END.
   END.
   IF vad = 3 THEN DO:
      IF AVAILABLE spec_mtrl THEN DO:         
         FIND FIRST ikmtrltemp WHERE ikmtrltemp.LEVKOD = spec_mtrl.LEVKOD AND ikmtrltemp.ENR = spec_mtrl.ENR AND ikmtrltemp.KALKNR = 0
         AND ikmtrltemp.KUND = TRUE  NO-LOCK NO-ERROR.
         IF AVAILABLE ikmtrltemp THEN DO:                     
            ASSIGN
            spec_mtrl.ENR:BGCOLOR IN BROWSE BRW_MTRL = varforetypval[28]
            spec_mtrl.NPRIS:BGCOLOR IN BROWSE BRW_MTRL = varforetypval[28].          
         END.
         ELSE DO:
            FIND FIRST ikmtrltemp WHERE ikmtrltemp.LEVKOD = spec_mtrl.LEVKOD AND ikmtrltemp.ENR = spec_mtrl.ENR AND ikmtrltemp.KALKNR = 0
            AND ikmtrltemp.KUND = ?  NO-LOCK NO-ERROR.
            IF AVAILABLE ikmtrltemp THEN DO:                     
               ASSIGN
               spec_mtrl.ENR:BGCOLOR IN BROWSE BRW_MTRL = varforetypval[38]
               spec_mtrl.NPRIS:BGCOLOR IN BROWSE BRW_MTRL = varforetypval[38].          
            END.
         END.
      END.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE satsvisa_UI WINDOW-1 
PROCEDURE satsvisa_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/    
   RUN infoES_UI (INPUT 3).   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

