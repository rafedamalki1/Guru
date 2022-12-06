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

  Created: 95/07/04 - 11:31 am

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

{OMRTEMPW.I}                                  
{REGVAR.I}
&Scoped-define SHARED SHARED 

{DIRDEF.I}
&Scoped-define NEW NEW
{AVTAONRTEMP.I}
{AUTOMREGTEMP.I}
{ANSPROJBER.I}
{VISUTSOKAONR.I}


DEFINE NEW SHARED VARIABLE listnr AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE omrvar AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE qsort AS CHARACTER FORMAT "x(65)" NO-UNDO.
DEFINE SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE vartpro AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE SHARED VARIABLE vart AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE muszval AS INTEGER NO-UNDO.

DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE antal_valda AS INTEGER NO-UNDO.
DEFINE VARIABLE antal_raknare AS INTEGER NO-UNDO.
DEFINE VARIABLE projvisapph AS HANDLE NO-UNDO.
DEFINE VARIABLE pkodf AS LOGICAL.
DEFINE VARIABLE arbartkodf AS LOGICAL.
DEFINE VARIABLE anlnrf AS LOGICAL.


DEFINE NEW SHARED VARIABLE FILL-IN-PRISTYP AS CHARACTER FORMAT "X(9)":U INITIAL ? 
     LABEL "DEBITERING" 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1
       NO-UNDO.

DEFINE NEW SHARED VARIABLE FILL-IN_ANLNR AS CHARACTER FORMAT "x(15)" 
     LABEL "ANL.NR." 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1
      .

DEFINE NEW SHARED VARIABLE FILL-IN_ARBANSVARIG AS CHARACTER FORMAT "x(5)" 
     LABEL "ARBETSANSVARIG" 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1
      .

DEFINE NEW SHARED VARIABLE FILL-IN_ARBARTKOD AS INTEGER FORMAT ">>>" INITIAL 0 
     LABEL "ARBETSART" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1
      .

DEFINE NEW SHARED VARIABLE FILL-IN_BEREDARE AS CHARACTER FORMAT "x(5)" 
     LABEL "BEREDARE/PROJEKTÖR" 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1
      .

DEFINE NEW SHARED VARIABLE FILL-IN_OMRADE AS CHARACTER FORMAT "x(6)" 
     LABEL "OMRÅDE" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1
      .

DEFINE NEW SHARED VARIABLE FILL-IN_PKOD AS INTEGER FORMAT ">>" INITIAL 0 
     LABEL "PRIORITET" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1
      .

DEFINE NEW SHARED VARIABLE FILL-IN_SLUTVNR AS INTEGER FORMAT ">>>" INITIAL 0 
     LABEL "SLUT  VNR" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1
      .

DEFINE NEW SHARED VARIABLE FILL-IN_STARTVNR AS INTEGER FORMAT ">>>" INITIAL 0 
     LABEL "START VNR" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1
      .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE WINDOW
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-B
&Scoped-define BROWSE-NAME BRW_EJTIDL

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ejtidslagtemp tidslagtemp

/* Definitions for BROWSE BRW_EJTIDL                                    */
&Scoped-define FIELDS-IN-QUERY-BRW_EJTIDL ejtidslagtemp.TIDLAGE ~
ejtidslagtemp.AKTIVITET1 ejtidslagtemp.AKTIVITET2 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_EJTIDL 
&Scoped-define QUERY-STRING-BRW_EJTIDL FOR EACH ejtidslagtemp NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BRW_EJTIDL OPEN QUERY BRW_EJTIDL FOR EACH ejtidslagtemp NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BRW_EJTIDL ejtidslagtemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_EJTIDL ejtidslagtemp


/* Definitions for BROWSE BRW_TIDL                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_TIDL tidslagtemp.TIDLAGE ~
tidslagtemp.AKTIVITET1 tidslagtemp.AKTIVITET2 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_TIDL 
&Scoped-define QUERY-STRING-BRW_TIDL FOR EACH tidslagtemp NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BRW_TIDL OPEN QUERY BRW_TIDL FOR EACH tidslagtemp NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BRW_TIDL tidslagtemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_TIDL tidslagtemp


/* Definitions for FRAME FRAME-B                                        */
&Scoped-define OPEN-BROWSERS-IN-QUERY-FRAME-B ~
    ~{&OPEN-QUERY-BRW_EJTIDL}~
    ~{&OPEN-QUERY-BRW_TIDL}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BRW_TIDL TOG_EJAV TOG_AVS TOG_KONTO TOG_BEN ~
RAD_FAST RAD_SORT FBTN_VLIST SEL_OMR SEL_LIST BRW_EJTIDL BTN_AVB 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-LISTA TOG_EJAV TOG_AVS TOG_KONTO ~
TOG_BEN RAD_FAST RAD_SORT FILL-IN-OMRADE SEL_OMR SEL_LIST 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR WINDOW-2 AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB AUTO-END-KEY 
     LABEL "Avsluta":L 
     SIZE 14 BY 1.

DEFINE BUTTON FBTN_VLIST 
     LABEL "Visa":L 
     SIZE 14 BY 1.

DEFINE VARIABLE FILL-IN-LISTA AS CHARACTER FORMAT "X(256)":U INITIAL "Gör ett urval och välj lista:" 
     VIEW-AS FILL-IN 
     SIZE 32.5 BY 1
     FONT 17 NO-UNDO.

DEFINE VARIABLE FILL-IN-MELL AS CHARACTER FORMAT "X(6)":U 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-OCH AS CHARACTER FORMAT "X(3)":U 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-OMRADE AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 21.38 BY 1
     FONT 17 NO-UNDO.

DEFINE VARIABLE FILL-IN-SLUTD AS DATE FORMAT "99/99/99":U 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-STARTD AS DATE FORMAT "99/99/99":U 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE RAD_FAST AS LOGICAL 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Tillfälliga aonr", no,
"Fasta aonr", yes
     SIZE 42.63 BY 1 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE RAD_SORT AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "CCC", 1
     SIZE 42.63 BY 1 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE SEL_LIST AS CHARACTER INITIAL ? 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 86.25 BY 11 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE SEL_OMR AS CHARACTER INITIAL ? 
     VIEW-AS SELECTION-LIST SINGLE NO-DRAG SCROLLBAR-VERTICAL 
     SIZE 22 BY 11 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE TOG_ANL AS LOGICAL INITIAL no 
     LABEL "Alla anläggningar" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY 1 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE TOG_AVS AS LOGICAL INITIAL no 
     LABEL "Avslutade":L 
     VIEW-AS TOGGLE-BOX
     SIZE 13.25 BY 1 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE TOG_BEN AS LOGICAL INITIAL no 
     LABEL "Visa benämning" 
     VIEW-AS TOGGLE-BOX
     SIZE 16.5 BY .83 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE TOG_EJAV AS LOGICAL INITIAL no 
     LABEL "Ej avslutade":L 
     VIEW-AS TOGGLE-BOX
     SIZE 18.5 BY 1 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE TOG_KONTO AS LOGICAL INITIAL no 
     LABEL "Visa konto" 
     VIEW-AS TOGGLE-BOX
     SIZE 19 BY .92 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_EJTIDL FOR 
      ejtidslagtemp SCROLLING.

DEFINE QUERY BRW_TIDL FOR 
      tidslagtemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_EJTIDL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_EJTIDL WINDOW-2 _STRUCTURED
  QUERY BRW_EJTIDL NO-LOCK DISPLAY
      ejtidslagtemp.TIDLAGE COLUMN-LABEL "Tidläge" FORMAT "X(132)":U
            WIDTH 15
      ejtidslagtemp.AKTIVITET1 COLUMN-LABEL "Aktivitet" FORMAT "X(132)":U
            WIDTH 10
      ejtidslagtemp.AKTIVITET2 COLUMN-LABEL "Aktivitet" FORMAT "X(132)":U
            WIDTH 6.63
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH MULTIPLE SIZE 35.5 BY 11
         TITLE "Markera tidslägen som EJ ska sökas fram" FIT-LAST-COLUMN.

DEFINE BROWSE BRW_TIDL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_TIDL WINDOW-2 _STRUCTURED
  QUERY BRW_TIDL NO-LOCK DISPLAY
      tidslagtemp.TIDLAGE COLUMN-LABEL "Tidläge" FORMAT "X(132)":U
            WIDTH 15
      tidslagtemp.AKTIVITET1 COLUMN-LABEL "Aktivitet" FORMAT "X(132)":U
            WIDTH 10
      tidslagtemp.AKTIVITET2 COLUMN-LABEL "Aktivitet" FORMAT "X(132)":U
            WIDTH 6.63
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING MULTIPLE SIZE 35.5 BY 11
         TITLE "Markera tidslägen som skall sökas fram" FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-B
     BRW_TIDL AT ROW 1.17 COL 1.5 WIDGET-ID 100
     FILL-IN-LISTA AT ROW 1.63 COL 2.5 NO-LABEL WIDGET-ID 2
     TOG_EJAV AT ROW 2.88 COL 1.5
     TOG_AVS AT ROW 2.88 COL 27.88
     FILL-IN-MELL AT ROW 2.88 COL 39.38 COLON-ALIGNED NO-LABEL
     FILL-IN-STARTD AT ROW 2.88 COL 48.38 COLON-ALIGNED NO-LABEL
     FILL-IN-OCH AT ROW 2.88 COL 58.88 COLON-ALIGNED NO-LABEL
     FILL-IN-SLUTD AT ROW 2.88 COL 64.38 COLON-ALIGNED NO-LABEL
     TOG_KONTO AT ROW 3.96 COL 1.5
     TOG_BEN AT ROW 3.96 COL 27.88
     RAD_FAST AT ROW 5 COL 1.5 NO-LABEL
     RAD_SORT AT ROW 6.08 COL 1.5 NO-LABEL
     FBTN_VLIST AT ROW 6.5 COL 110.63
     FILL-IN-OMRADE AT ROW 7 COL 1.5 NO-LABEL
     SEL_OMR AT ROW 8.25 COL 12.38 NO-LABEL
     SEL_LIST AT ROW 8.25 COL 38 NO-LABEL
     BRW_EJTIDL AT ROW 12.25 COL 1.5 WIDGET-ID 200
     BTN_AVB AT ROW 22.04 COL 110.63
     TOG_ANL AT ROW 22.08 COL 1.5
     "Välj lista" VIEW-AS TEXT
          SIZE 23 BY 1.25 AT ROW 6.75 COL 38.13
          FONT 17
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 124.75 BY 22.33.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: WINDOW
   Temp-Tables and Buffers:
      TABLE: ejtidslagtemp T "?" NO-UNDO temp-db ejtidslagtemp
      TABLE: tidslagtemp T "?" NO-UNDO temp-db tidslagtemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW WINDOW-2 ASSIGN
         HIDDEN             = YES
         TITLE              = "Listor för arbetsordernummer"
         HEIGHT             = 22.5
         WIDTH              = 125
         MAX-HEIGHT         = 27.25
         MAX-WIDTH          = 125
         VIRTUAL-HEIGHT     = 27.25
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
/* SETTINGS FOR FRAME FRAME-B
   FRAME-NAME                                                           */
/* BROWSE-TAB BRW_TIDL TEXT-1 FRAME-B */
/* BROWSE-TAB BRW_EJTIDL SEL_LIST FRAME-B */
/* SETTINGS FOR FILL-IN FILL-IN-LISTA IN FRAME FRAME-B
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN FILL-IN-MELL IN FRAME FRAME-B
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN-MELL:HIDDEN IN FRAME FRAME-B           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-OCH IN FRAME FRAME-B
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN-OCH:HIDDEN IN FRAME FRAME-B           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-OMRADE IN FRAME FRAME-B
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN FILL-IN-SLUTD IN FRAME FRAME-B
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN-SLUTD:HIDDEN IN FRAME FRAME-B           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-STARTD IN FRAME FRAME-B
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN-STARTD:HIDDEN IN FRAME FRAME-B           = TRUE.

/* SETTINGS FOR RADIO-SET RAD_FAST IN FRAME FRAME-B
   SHARED                                                               */
/* SETTINGS FOR RADIO-SET RAD_SORT IN FRAME FRAME-B
   SHARED                                                               */
/* SETTINGS FOR SELECTION-LIST SEL_LIST IN FRAME FRAME-B
   SHARED                                                               */
/* SETTINGS FOR SELECTION-LIST SEL_OMR IN FRAME FRAME-B
   SHARED                                                               */
/* SETTINGS FOR TOGGLE-BOX TOG_ANL IN FRAME FRAME-B
   NO-DISPLAY SHARED NO-ENABLE                                          */
ASSIGN 
       TOG_ANL:HIDDEN IN FRAME FRAME-B           = TRUE.

/* SETTINGS FOR TOGGLE-BOX TOG_AVS IN FRAME FRAME-B
   SHARED                                                               */
/* SETTINGS FOR TOGGLE-BOX TOG_BEN IN FRAME FRAME-B
   SHARED                                                               */
/* SETTINGS FOR TOGGLE-BOX TOG_EJAV IN FRAME FRAME-B
   SHARED                                                               */
/* SETTINGS FOR TOGGLE-BOX TOG_KONTO IN FRAME FRAME-B
   SHARED                                                               */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-2)
THEN WINDOW-2:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_EJTIDL
/* Query rebuild information for BROWSE BRW_EJTIDL
     _TblList          = "Temp-Tables.ejtidslagtemp"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > Temp-Tables.ejtidslagtemp.TIDLAGE
"ejtidslagtemp.TIDLAGE" "Tidläge" "X(132)" "character" ? ? ? ? ? ? no ? no no "15" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.ejtidslagtemp.AKTIVITET1
"ejtidslagtemp.AKTIVITET1" "Aktivitet" "X(132)" "character" ? ? ? ? ? ? no ? no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.ejtidslagtemp.AKTIVITET2
"ejtidslagtemp.AKTIVITET2" "Aktivitet" "X(132)" "character" ? ? ? ? ? ? no ? no no "6.63" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE BRW_EJTIDL */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_TIDL
/* Query rebuild information for BROWSE BRW_TIDL
     _TblList          = "Temp-Tables.tidslagtemp"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > Temp-Tables.tidslagtemp.TIDLAGE
"tidslagtemp.TIDLAGE" "Tidläge" "X(132)" "character" ? ? ? ? ? ? no ? no no "15" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.tidslagtemp.AKTIVITET1
"tidslagtemp.AKTIVITET1" "Aktivitet" "X(132)" "character" ? ? ? ? ? ? no ? no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.tidslagtemp.AKTIVITET2
"tidslagtemp.AKTIVITET2" "Aktivitet" "X(132)" "character" ? ? ? ? ? ? no ? no no "6.63" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE BRW_TIDL */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB WINDOW-2
ON CHOOSE OF BTN_AVB IN FRAME FRAME-B /* Avsluta */
DO:   
   RUN avsProjVis_UI IN projvisapph.
   DELETE PROCEDURE projvisapph NO-ERROR. 
   projvisapph = ?.
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_VLIST
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_VLIST WINDOW-2
ON CHOOSE OF FBTN_VLIST IN FRAME FRAME-B /* Visa */
DO:
   skrivut = FALSE.  
   RUN visa_UI.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-SLUTD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-SLUTD WINDOW-2
ON LEAVE OF FILL-IN-SLUTD IN FRAME FRAME-B
DO:
   FILL-IN-SLUTD = INPUT FILL-IN-SLUTD.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-SLUTD WINDOW-2
ON MOUSE-MENU-CLICK OF FILL-IN-SLUTD IN FRAME FRAME-B
DO:
   ASSIGN
   FILL-IN-SLUTD = INPUT FILL-IN-SLUTD
   Guru.GlobalaVariabler:regdatum = INPUT FILL-IN-SLUTD.
   RUN AlmanBtn.w.
   FILL-IN-SLUTD = Guru.GlobalaVariabler:regdatum.
   DISPLAY FILL-IN-SLUTD WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-STARTD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-STARTD WINDOW-2
ON LEAVE OF FILL-IN-STARTD IN FRAME FRAME-B
DO:
   FILL-IN-STARTD = INPUT FILL-IN-STARTD.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-STARTD WINDOW-2
ON MOUSE-MENU-CLICK OF FILL-IN-STARTD IN FRAME FRAME-B
DO:
   ASSIGN
   FILL-IN-STARTD = INPUT FILL-IN-STARTD
   regdatum = INPUT FILL-IN-STARTD.
   RUN AlmanBtn.w.
   FILL-IN-STARTD = regdatum.
   DISPLAY FILL-IN-STARTD WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RAD_FAST
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RAD_FAST WINDOW-2
ON VALUE-CHANGED OF RAD_FAST IN FRAME FRAME-B
DO:
   RAD_FAST = INPUT RAD_FAST. 
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RAD_SORT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RAD_SORT WINDOW-2
ON VALUE-CHANGED OF RAD_SORT IN FRAME FRAME-B
DO:
   RAD_SORT = INPUT RAD_SORT. 
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME SEL_LIST
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL SEL_LIST WINDOW-2
ON MOUSE-SELECT-DBLCLICK OF SEL_LIST IN FRAME FRAME-B
DO:
   APPLY "CHOOSE" TO FBTN_VLIST.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL SEL_LIST WINDOW-2
ON VALUE-CHANGED OF SEL_LIST IN FRAME FRAME-B
DO:
   SEL_LIST = INPUT SEL_LIST.
   IF SEL_LIST = "Alla " + Guru.Konstanter:gaol + " / " + Guru.Konstanter:gomrk THEN listnr = 1.
   IF SEL_LIST = "Lista debiteringspris / " + Guru.Konstanter:gomrk THEN listnr = 2.
   IF SEL_LIST = "Lista " + LC(Guru.Konstanter:gartk)+ " / " + Guru.Konstanter:gomrk THEN listnr = 3.
   IF SEL_LIST = "Lista " + LC(Guru.Konstanter:gpriok) + " / " + Guru.Konstanter:gomrk THEN listnr = 4.
   IF SEL_LIST = "Lista start - slut VNR / " + Guru.Konstanter:gomrk THEN listnr = 5.
   IF SEL_LIST = "Lista " + Guru.Konstanter:gbestk + " / " + Guru.Konstanter:gomrk THEN listnr = 6.
   IF SEL_LIST = "Lista "  + Guru.Konstanter:gprojk + " / " + Guru.Konstanter:gomrk THEN listnr = 15.
   IF SEL_LIST = "Lista " + Guru.Konstanter:gberek + " / " + Guru.Konstanter:gomrk THEN listnr = 7.
   IF SEL_LIST = "Lista " + Guru.Konstanter:garbak + " / " + Guru.Konstanter:gomrk THEN listnr = 8.   
   IF SEL_LIST = "Lista arbetsbesked / " + Guru.Konstanter:gomrk THEN listnr = 9.
   IF SEL_LIST = "Lista anläggningsnr / " + Guru.Konstanter:gomrk THEN listnr = 10.
   IF SEL_LIST = "Lista " + Guru.Konstanter:gaok + " med avrop / " + Guru.Konstanter:gomrk THEN listnr = 12.
   IF SEL_LIST = "Alla " + Guru.Konstanter:gaok + " / " + Guru.Konstanter:gomrk + " med visning av ansvarig" THEN listnr = 13.
   IF SEL_LIST = "Alla " + Guru.Konstanter:gaok + " / " + Guru.Konstanter:gomrk + " med utomstående " + Guru.Konstanter:gbestk THEN listnr = 14.
   IF SEL_LIST = "Alla " + Guru.Konstanter:gaol + " med arbetsuppgift / " + Guru.Konstanter:gomrk THEN listnr = 16.
   IF SEL_LIST = "Alla " + Guru.Konstanter:gaol + " med skapat datum / " + Guru.Konstanter:gomrk THEN listnr = 17.
   IF SEL_LIST = "Alla " + Guru.Konstanter:gaol + " med " + LC(Guru.Konstanter:gartk) + " / " + Guru.Konstanter:gomrk THEN listnr = 19.
   
   
   IF SEL_LIST = "Alla " + Guru.Konstanter:gaol + " med Personalliggare"  THEN DO:
      listnr = 21.      
      ASSIGN
      SEL_OMR:HIDDEN = TRUE
      TOG_EJAV:HIDDEN = TRUE
      TOG_AVS :HIDDEN = TRUE           
      TOG_BEN:HIDDEN = TRUE 
      RAD_FAST:HIDDEN = TRUE            
      TOG_KONTO = TRUE
      TOG_KONTO:HIDDEN = TRUE
      FILL-IN-OMRADE:HIDDEN = TRUE
      FILL-IN-LISTA:HIDDEN = TRUE.
      BRW_TIDL:HIDDEN = TRUE.
      BRW_EJTIDL:HIDDEN = TRUE.
   END.   
   ELSE IF SEL_LIST = "Alla " + Guru.Konstanter:gaol + " i arbeta vidare med konto" THEN DO:
      listnr = 18.
      ASSIGN
      TOG_EJAV:HIDDEN = TRUE
      TOG_AVS :HIDDEN = TRUE           
      TOG_BEN:HIDDEN = TRUE 
      RAD_FAST:HIDDEN = TRUE
      SEL_OMR:HIDDEN = TRUE      
      TOG_KONTO = TRUE
      TOG_KONTO:HIDDEN = TRUE.
      FILL-IN-OMRADE:HIDDEN = TRUE.
      FILL-IN-LISTA:HIDDEN = TRUE.
      BRW_TIDL:HIDDEN = TRUE.
      BRW_EJTIDL:HIDDEN = TRUE.      
   END.
   ELSE IF SEL_LIST = "Alla " + Guru.Konstanter:gaol + " i arbeta vidare med - Alla uppgifter" THEN DO:
      listnr = 20.
      ASSIGN
      TOG_EJAV:HIDDEN = TRUE
      TOG_AVS :HIDDEN = TRUE             
      TOG_BEN:HIDDEN = TRUE 
      RAD_FAST:HIDDEN = TRUE
      SEL_OMR:HIDDEN = TRUE
      TOG_KONTO = FALSE
      TOG_KONTO:HIDDEN = TRUE
      FILL-IN-OMRADE:HIDDEN = TRUE.
      FILL-IN-LISTA:HIDDEN = TRUE.
      BRW_TIDL:HIDDEN = TRUE.
      BRW_EJTIDL:HIDDEN = TRUE.
   END.
   ELSE IF SEL_LIST = "Alla " + Guru.Konstanter:gaol + " i arbeta vidare med - med markerade " + Guru.Konstanter:gtidll + " från övre vallista men ej de markerade " + Guru.Konstanter:gtidll + " från nedre vallista" THEN DO: 
      listnr = 22.
      ASSIGN
      TOG_EJAV:HIDDEN = TRUE
      TOG_AVS :HIDDEN = TRUE             
      TOG_BEN:HIDDEN = TRUE 
      RAD_FAST:HIDDEN = TRUE
      SEL_OMR:HIDDEN = TRUE
      TOG_KONTO = FALSE
      TOG_KONTO:HIDDEN = TRUE
      FILL-IN-OMRADE:HIDDEN = TRUE.
      FILL-IN-LISTA:HIDDEN = TRUE.
      BRW_TIDL:HIDDEN = FALSE.
      BRW_EJTIDL:HIDDEN = FALSE.
   
      RUN hamttidl_UI IN projvisapph (OUTPUT TABLE  tidslagtemp ).
      FOR EACH tidslagtemp :
         CREATE ejtidslagtemp.
         BUFFER-COPY tidslagtemp TO ejtidslagtemp .
      END.
      RUN openbdynspec_UI IN brwproc[1].
      RUN openbdynspec_UI IN brwproc[2].
   END.
   ELSE IF SEL_LIST = "Alla " + Guru.Konstanter:gaol + " i arbeta vidare - alla " + Guru.Konstanter:gtidll THEN DO: 
      listnr = 23.
      ASSIGN
      TOG_EJAV:HIDDEN = TRUE
      TOG_AVS :HIDDEN = TRUE             
      TOG_BEN:HIDDEN = TRUE 
      RAD_FAST:HIDDEN = TRUE
      SEL_OMR:HIDDEN = TRUE
      TOG_KONTO = FALSE
      TOG_KONTO:HIDDEN = TRUE
      FILL-IN-OMRADE:HIDDEN = TRUE.
      FILL-IN-LISTA:HIDDEN = TRUE.
      BRW_TIDL:HIDDEN = TRUE.
      BRW_EJTIDL:HIDDEN = TRUE.
   END.   
   
   ELSE DO:
      ASSIGN
      TOG_EJAV:HIDDEN = FALSE
      TOG_AVS :HIDDEN = FALSE
      TOG_KONTO:HIDDEN = FALSE 
      TOG_BEN:HIDDEN = FALSE 
      RAD_FAST:HIDDEN = FALSE
      SEL_OMR:HIDDEN = FALSE
      FILL-IN-OMRADE:HIDDEN = FALSE
      TOG_KONTO = FALSE.
      DISPLAY TOG_KONTO WITH FRAME FRAME-B.
      FILL-IN-LISTA:HIDDEN = FALSE.
      BRW_TIDL:HIDDEN = TRUE.
      BRW_EJTIDL:HIDDEN = TRUE.
   END.
   IF listnr = 1 OR listnr = 13 OR listnr = 16 OR listnr = 17 OR listnr = 19  THEN DO:
      RAD_SORT:HIDDEN = FALSE. 
   END.   
   ELSE RAD_SORT:HIDDEN = TRUE. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME SEL_OMR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL SEL_OMR WINDOW-2
ON VALUE-CHANGED OF SEL_OMR IN FRAME FRAME-B
DO:
   SEL_OMR = INPUT SEL_OMR.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_ANL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_ANL WINDOW-2
ON VALUE-CHANGED OF TOG_ANL IN FRAME FRAME-B /* Alla anläggningar */
DO:
   TOG_ANL = INPUT TOG_ANL.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_AVS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_AVS WINDOW-2
ON VALUE-CHANGED OF TOG_AVS IN FRAME FRAME-B /* Avslutade */
DO:
   TOG_AVS = INPUT TOG_AVS.
   IF TOG_AVS = TRUE THEN DO:
      ENABLE FILL-IN-SLUTD FILL-IN-STARTD WITH FRAME {&FRAME-NAME}.
      DISPLAY FILL-IN-MELL FILL-IN-OCH FILL-IN-SLUTD FILL-IN-STARTD WITH FRAME {&FRAME-NAME}.
   END.   
   ELSE DO:       
      ASSIGN
      FILL-IN-MELL:HIDDEN = TRUE
      FILL-IN-OCH:HIDDEN = TRUE  
      FILL-IN-SLUTD:HIDDEN = TRUE  
      FILL-IN-STARTD:HIDDEN = TRUE.  
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_BEN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_BEN WINDOW-2
ON VALUE-CHANGED OF TOG_BEN IN FRAME FRAME-B /* Visa benämning */
DO:
   TOG_BEN = INPUT TOG_BEN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_EJAV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_EJAV WINDOW-2
ON VALUE-CHANGED OF TOG_EJAV IN FRAME FRAME-B /* Ej avslutade */
DO:
   TOG_EJAV = INPUT TOG_EJAV.
   IF TOG_AVS = TRUE THEN DO:
      ENABLE FILL-IN-SLUTD FILL-IN-STARTD WITH FRAME {&FRAME-NAME}.
      DISPLAY FILL-IN-MELL FILL-IN-OCH FILL-IN-SLUTD FILL-IN-STARTD WITH FRAME {&FRAME-NAME}.
   END.   
   ELSE DO:                      
      ASSIGN
      FILL-IN-MELL:HIDDEN = TRUE
      FILL-IN-OCH:HIDDEN = TRUE  
      FILL-IN-SLUTD:HIDDEN = TRUE  
      FILL-IN-STARTD:HIDDEN = TRUE.  
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_KONTO
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_KONTO WINDOW-2
ON VALUE-CHANGED OF TOG_KONTO IN FRAME FRAME-B /* Visa konto */
DO:
    TOG_KONTO = INPUT TOG_KONTO.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_EJTIDL
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK WINDOW-2 


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
   {muswait.i}
   {ALLSTARTDYN.I}
   status-ok = SEL_OMR:ADD-LAST("ALLA UTAN ORG.ENHET").
   status-ok = SEL_OMR:ADD-LAST("ALLA").
   FOR EACH omrtemp USE-INDEX OMR NO-LOCK:
      status-ok = SEL_OMR:ADD-LAST(omrtemp.NAMN).
   END.
   WINDOW-2:TITLE = "Listor för " + LC(Guru.Konstanter:gaol).
   FILL-IN-OMRADE = "Välj " + LC(Guru.Konstanter:gomrl).
   {TILLFAST.I}
   FIND FIRST utsokaonr WHERE utsokaonr.FASTAAONR = TRUE NO-LOCK NO-ERROR.
   IF NOT AVAILABLE utsokaonr THEN DO:
      status-ok = RAD_FAST:DELETE(Guru.Konstanter:gfastl + " " + LC(Guru.Konstanter:gaok)).
   END.
   BRW_TIDL:TITLE = "Markera " + Guru.Konstanter:gtidll + " som ska sökas fram".
   BRW_EJTIDL:TITLE = "Markera " + Guru.Konstanter:gtidll + " som EJ ska sökas fram". 
   status-ok = RAD_SORT:DELETE("CCC").
   RAD_SORT:ADD-LAST("Sortera på " + LC(Guru.Konstanter:gomrk), 1). 
   RAD_SORT:ADD-LAST("Sortera på " + LC(Guru.Konstanter:gaok), 2).
   RAD_SORT = 1.
   ASSIGN
   TOG_BEN = TRUE
   TOG_KONTO = FALSE.   
   status-ok = SEL_LIST:ADD-LAST("Alla " + Guru.Konstanter:gaol + " i arbeta vidare med - Alla uppgifter").
   IF Guru.Konstanter:varforetypval[2] = 0 THEN  status-ok = SEL_LIST:ADD-LAST("Alla " + Guru.Konstanter:gaol + " i arbeta vidare med konto").   
   status-ok = SEL_LIST:ADD-LAST("Alla " + Guru.Konstanter:gaol + " / " + Guru.Konstanter:gomrk).
   status-ok = SEL_LIST:ADD-LAST("Lista debiteringspris / " + Guru.Konstanter:gomrk).
   status-ok = SEL_LIST:ADD-LAST("Lista " + LC(Guru.Konstanter:gartk)+ " / " + Guru.Konstanter:gomrk).
   status-ok = SEL_LIST:ADD-LAST("Lista " + LC(Guru.Konstanter:gpriok) + " / " + Guru.Konstanter:gomrk).
   status-ok = SEL_LIST:ADD-LAST("Lista " + Guru.Konstanter:gbestk + " / " + Guru.Konstanter:gomrk).
   status-ok = SEL_LIST:ADD-LAST("Lista "  + Guru.Konstanter:gprojk + " / " + Guru.Konstanter:gomrk).
   status-ok = SEL_LIST:ADD-LAST("Lista " + Guru.Konstanter:gberek + " / " + Guru.Konstanter:gomrk).
   status-ok = SEL_LIST:ADD-LAST("Lista " + Guru.Konstanter:garbak + " / " + Guru.Konstanter:gomrk).   
   status-ok = SEL_LIST:ADD-LAST("Lista anläggningsnr / " + Guru.Konstanter:gomrk).
   IF Guru.Konstanter:globforetag = "ELPA" OR Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" THEN
   status-ok = SEL_LIST:ADD-LAST("Lista " + Guru.Konstanter:gaok + " med avrop / " + Guru.Konstanter:gomrk).
   IF Guru.Konstanter:globforetag = "ELPA" OR Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" THEN
   status-ok = SEL_LIST:ADD-LAST("Alla " + Guru.Konstanter:gaok + " / " + Guru.Konstanter:gomrk + " med visning av ansvarig").   
   status-ok = SEL_LIST:ADD-LAST("Alla " + Guru.Konstanter:gaok + " / " + Guru.Konstanter:gomrk + " med utomstående " + Guru.Konstanter:gbestk).
   status-ok = SEL_LIST:ADD-LAST("Alla " + Guru.Konstanter:gaol + " med arbetsuppgift / " + Guru.Konstanter:gomrk). 
   status-ok = SEL_LIST:ADD-LAST("Alla " + Guru.Konstanter:gaol + " med skapat datum / " + Guru.Konstanter:gomrk).    
   status-ok = SEL_LIST:ADD-LAST("Alla " + Guru.Konstanter:gaol + " med " + LC(Guru.Konstanter:gartk) + " / " + Guru.Konstanter:gomrk).   
   IF Guru.Konstanter:varforetypchar[9] = "1" THEN  status-ok = SEL_LIST:ADD-LAST("Alla " + Guru.Konstanter:gaol + " med Personalliggare").      
   status-ok = SEL_LIST:ADD-LAST("Alla " + Guru.Konstanter:gaol + " i arbeta vidare med - med markerade " + Guru.Konstanter:gtidll + " från övre vallista men ej de markerade " + Guru.Konstanter:gtidll + " från nedre vallista").   
   status-ok = SEL_LIST:ADD-LAST("Alla " + Guru.Konstanter:gaol + " i arbeta vidare - alla " + Guru.Konstanter:gtidll ).   
   ASSIGN   
   TOG_EJAV = TRUE
   TOG_AVS = FALSE
   FILL-IN-MELL = "mellan" 
   FILL-IN-OCH = "och"    
   FILL-IN-SLUTD = TODAY
   FILL-IN-STARTD = DATE(01,01,YEAR(TODAY)).
   RUN enable_UI.   
   ASSIGN
   TOG_EJAV:HIDDEN = TRUE
   TOG_AVS :HIDDEN = TRUE             
   TOG_BEN:HIDDEN = TRUE 
   RAD_FAST:HIDDEN = TRUE
   SEL_OMR:HIDDEN = TRUE
   TOG_KONTO = FALSE
   TOG_KONTO:HIDDEN = TRUE
   FILL-IN-OMRADE:HIDDEN = TRUE
   FILL-IN-LISTA:HIDDEN = TRUE
   BRW_TIDL:HIDDEN = TRUE
   BRW_EJTIDL:HIDDEN = TRUE.
   {FRMSIZE.I}  
   RUN VISMEDDU.W.
   RAD_SORT:HIDDEN = TRUE. 
   {musarrow.i}   
   {WIN_M_SLUT.I}
   IF NOT THIS-PROCEDURE:PERSISTENT THEN
   WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE allstartbrw_UI WINDOW-2 
PROCEDURE allstartbrw_UI :
/* -----------------------------------------------------------
  Purpose: Changing screen-value for combo-box CMB_OMR     
  Parameters:  Input = Screen-value for CMB_FOR
  Notes:       
-------------------------------------------------------------*/    
   RUN DYNBRW.P PERSISTENT SET brwproc[1]
      (INPUT BRW_TIDL:HANDLE IN FRAME {&FRAME-NAME}).
      RUN DYNBRW.P PERSISTENT SET brwproc[2]
      (INPUT BRW_EJTIDL:HANDLE IN FRAME {&FRAME-NAME}).
   RUN addmenuitem_UI IN brwproc[1] (INPUT BRW_TIDL:HANDLE,INPUT "Avmarkera",INPUT "avmarktidl_UI").
   RUN addmenuitem_UI IN brwproc[2] (INPUT BRW_EJTIDL:HANDLE,INPUT "Avmarkera",INPUT "avmarkejtidl_UI"). 
          
   IF Guru.Konstanter:appcon THEN DO:      
       RUN PROJRVISAPP.P PERSISTENT SET projvisapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:      
      RUN PROJRVISAPP.P PERSISTENT SET projvisapph.
     
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE avmarkejtidl_UI WINDOW-2 
PROCEDURE avmarkejtidl_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  status-ok = BRW_EJTIDL:DESELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE avmarktidl_UI WINDOW-2 
PROCEDURE avmarktidl_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   status-ok = BRW_TIDL:DESELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
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
  DISPLAY FILL-IN-LISTA TOG_EJAV TOG_AVS TOG_KONTO TOG_BEN RAD_FAST RAD_SORT 
          FILL-IN-OMRADE SEL_OMR SEL_LIST 
      WITH FRAME FRAME-B IN WINDOW WINDOW-2.
  ENABLE BRW_TIDL TOG_EJAV TOG_AVS TOG_KONTO TOG_BEN RAD_FAST RAD_SORT 
         FBTN_VLIST SEL_OMR SEL_LIST BRW_EJTIDL BTN_AVB 
      WITH FRAME FRAME-B IN WINDOW WINDOW-2.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-B}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE skrivare_UI WINDOW-2 
PROCEDURE skrivare_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   IF skrivut = TRUE AND listnr = 17  THEN RUN SKRIVVAL.W (INPUT TRUE).   
   ELSE IF skrivut = TRUE THEN RUN SKRIVVAL.W (INPUT FALSE).
   

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE visa_UI WINDOW-2 
PROCEDURE visa_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/        
   IF SEL_LIST = ? OR SEL_LIST = "" THEN DO:
      MESSAGE "Välj en lista! "VIEW-AS ALERT-BOX.
      RETURN.
   END. 
   IF TOG_AVS = FALSE AND TOG_EJAV = FALSE THEN DO:
      MESSAGE "Fyll i ""Ej avslutade"" ELLER ""avslutade"" ELLER båda!" VIEW-AS ALERT-BOX.
      RETURN.
   END.
   IF listnr NE 5 THEN DO:
      IF TOG_BEN = FALSE AND TOG_KONTO = FALSE THEN DO:
         MESSAGE 
         "Fyll i ""Visa konto"" ELLER ""Visa benämning"" ELLER båda!" VIEW-AS ALERT-BOX.
         RETURN.
      END.
   END.
   IF FILL-IN-STARTD > FILL-IN-SLUTD THEN DO:
      MESSAGE "Startdatum kan inte vara större än slutdatum! " VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN-STARTD IN FRAME {&FRAME-NAME}. 
      RETURN.
   END.
   IF listnr = 18 OR listnr = 20 OR listnr = 21 OR listnr = 22 OR listnr = 23 THEN.
   ELSE DO:
      IF SEL_OMR = "" OR SEL_OMR = ? THEN DO:
         MESSAGE "Markera för vilken/vilka " + LC(Guru.Konstanter:gomrl) + "!" 
         VIEW-AS ALERT-BOX TITLE "Meddelande".
         RETURN.
      END.
   END.                 
   ASSIGN
   bdatum = FILL-IN-STARTD
   avdatum = FILL-IN-SLUTD.   
   IF listnr = 18 THEN DO:
      RUN skrivare_UI.
      IF musz = TRUE THEN DO:
         ASSIGN
         musz = FALSE
         skrivut = FALSE.  
      END.        
      ELSE DO:
         {AVBGOM.I}
         RUN RAPPORTU.W. 
         {AVBFRAM.I}
      END.
   END.
   ELSE IF listnr = 20 THEN DO:      
      {AVBGOM.I}
      EMPTY TEMP-TABLE visutsokaonr NO-ERROR.
      DEBUGGER:SET-BREAK().       
      RUN hamtprojupp_UI IN projvisapph (INPUT TABLE valdaao, OUTPUT TABLE  evisutsokaonr).
       
      RUN PROJRVIS.W (INPUT TABLE evisutsokaonr,  INPUT listnr). 
      {AVBFRAM.I}
   END.   
   ELSE IF listnr = 21 THEN DO:
      /*PERSONALLIGGARE*/
      {AVBGOM.I}
      EMPTY TEMP-TABLE eplutsokaonr NO-ERROR.
      DEBUGGER:SET-BREAK().       
      RUN hamtpliggare_UI IN projvisapph (OUTPUT TABLE  eplutsokaonr).
      
      RUN PROJPLVIS.W (INPUT TABLE eplutsokaonr). 
      {AVBFRAM.I}
   END.
   ELSE IF listnr = 22 THEN DO:
      {AVBGOM.I}

      EMPTY TEMP-TABLE vtidslagtemp NO-ERROR.
      EMPTY TEMP-TABLE vejtidslagtemp NO-ERROR.
         
      antal_valda = BRW_TIDL:NUM-SELECTED-ROWS NO-ERROR.      
      antal_raknare = 1.
      DO WHILE antal_raknare LE antal_valda TRANSACTION:
         status-ok = BRW_TIDL:FETCH-SELECTED-ROW(antal_raknare).
         CREATE vtidslagtemp.
         BUFFER-COPY tidslagtemp TO vtidslagtemp.
         antal_raknare = antal_raknare + 1.
      END.
      antal_valda = BRW_EJTIDL:NUM-SELECTED-ROWS NO-ERROR.      
      antal_raknare = 1.
      DO WHILE antal_raknare LE antal_valda TRANSACTION:
         status-ok = BRW_EJTIDL:FETCH-SELECTED-ROW(antal_raknare).
         CREATE vejtidslagtemp.
         BUFFER-COPY ejtidslagtemp TO vejtidslagtemp.
         antal_raknare = antal_raknare + 1.
      END.          
      EMPTY TEMP-TABLE visutsokaonr NO-ERROR.             
      RUN hamtprojupptidl_UI IN projvisapph (INPUT TABLE valdaao,INPUT TABLE vtidslagtemp,INPUT TABLE vejtidslagtemp, OUTPUT TABLE  evisutsokaonr).             
      RUN PROJRVIS.W (INPUT TABLE evisutsokaonr , INPUT listnr).       
      {AVBFRAM.I}
   END.
   ELSE IF listnr = 23 THEN DO:
      {AVBGOM.I}
      RUN hamtprojuppalltidl_UI IN projvisapph (INPUT TABLE valdaao, OUTPUT TABLE  evisutsokaonr).             
      RUN PROJRVIS.W (INPUT TABLE evisutsokaonr , INPUT listnr).             
      {AVBFRAM.I}
   END.    
   
   ELSE DO:
      {muswait.i}
      RUN URVALU.W.
      IF musz = TRUE THEN DO:
         ASSIGN
         musz = FALSE
         skrivut = FALSE.                          
      END. 
      ELSE DO:    
         RUN skrivare_UI.
         IF musz = TRUE THEN DO:
            ASSIGN
            musz = FALSE
            skrivut = FALSE.  
         END.        
         ELSE DO:
            RUN RAPPORTU.W.             
         END.
      END.                      
      
      skrivut = FALSE.
   END.   
   
   {musarrow.i}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

