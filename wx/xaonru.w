&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
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
&Scoped-define NEW NEW
&Scoped-define SHARED SHARED
&Scoped-define NEW NEW
DEF VAR musz AS LOGICAL.
DEFINE TEMP-TABLE uppvaltemp  NO-UNDO
   FIELD VALDLISTA AS CHARACTER
   FIELD VISPERAR AS LOGICAL
   FIELD VISGODKANDA AS LOGICAL
   FIELD DELNRKOLL AS LOGICAL
   FIELD ENDBEST AS LOGICAL
   FIELD STARTDATUM AS DATE
   FIELD SLUTDATUM AS DATE
   FIELD AVSLUTSTART AS DATE
   FIELD AVSLUTSLUT AS DATE
   FIELD TILLFALLFAST AS INTEGER
   FIELD BESTID AS CHARACTER
   FIELD BESTNAMN AS CHARACTER
   FIELD OMRNAMN AS CHARACTER
   FIELD OMRADE AS CHARACTER
   FIELD AVDNAMN AS CHARACTER
   FIELD AVDNR   AS CHARACTER   
   FIELD FAKTTYP AS CHARACTER  
   FIELD PAAV AS INTEGER
   FIELD ARBANSVARIG AS CHARACTER 
   FIELD BEREDARE AS CHARACTER
   FIELD PROJEKTOR AS CHARACTER.

DEFINE {&NEW} {&SHARED} TEMP-TABLE utsokaonr NO-UNDO
   FIELD AONR AS CHARACTER
   FIELD DELNR AS INTEGER
   FIELD OMRADE AS CHARACTER
   FIELD ORT AS CHARACTER
   FIELD BESTID AS CHARACTER
   FIELD AONRAVDATUM AS DATE
   FIELD STARTDATUM AS DATE
   FIELD AONRREC AS RECID
   FIELD FAKTTYP AS CHARACTER
   FIELD FAKTNR AS INTEGER   
   FIELD ARBANSVARIG AS CHARACTER 
   FIELD BEREDARE AS CHARACTER
   FIELD PROJEKTOR AS CHARACTER
   FIELD TABORT AS LOGICAL
   FIELD FASTAAONR AS LOGICAL
   FIELD TRAKTAMENTE AS INTEGER
   FIELD PRISTYP AS CHARACTER
   FIELD UTRYCKNING AS LOGICAL
   /*FIELD ARBUPPG AS CHARACTER*/
   INDEX AONR IS PRIMARY AONR DELNR
   INDEX OMRADE OMRADE AONR DELNR
   INDEX ORT ORT AONR DELNR
   INDEX TABORT TABORT.    
DEFINE TEMP-TABLE valdaao  NO-UNDO
   FIELD AONR AS CHARACTER
   FIELD DELNR AS INTEGER
   FIELD OMRADE AS CHARACTER
   FIELD ORT AS CHARACTER
   FIELD BESTID AS CHARACTER
   FIELD AONRAVDATUM AS DATE
   FIELD STARTDATUM AS DATE
   FIELD AONRREC AS RECID
   FIELD FAKTTYP AS CHARACTER
   FIELD FAKTNR AS INTEGER
   FIELD ARBANSVARIG AS CHARACTER
   FIELD BEREDARE AS CHARACTER
   FIELD FASTAAONR AS LOGICAL
   INDEX AONR IS PRIMARY AONR DELNR
   INDEX OMRADE OMRADE AONR DELNR
   INDEX ORT ORT AONR DELNR.    
                               

DEFINE NEW SHARED VARIABLE kalkrec AS RECID NO-UNDO.
DEFINE NEW SHARED VARIABLE rubrikvar AS CHARACTER FORMAT "X(30)" NO-UNDO.
DEFINE NEW SHARED VARIABLE saonrrec AS RECID NO-UNDO.
DEFINE NEW SHARED VARIABLE aoplan AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE vardrec AS RECID NO-UNDO.
DEFINE NEW SHARED VARIABLE avbryt AS LOGICAL NO-UNDO.                           
DEFINE VARIABLE skick AS LOGICAL NO-UNDO.
DEFINE VARIABLE efel AS CHARACTER FORMAT "X(30)" NO-UNDO.
DEFINE VARIABLE valdarec AS RECID NO-UNDO.
DEFINE VARIABLE my1hand AS WIDGET-HANDL NO-UNDO.
DEFINE VARIABLE uppar AS INTEGER NO-UNDO.
DEFINE VARIABLE aosok AS CHARACTER FORMAT "X(8)" NO-UNDO. 
DEFINE VARIABLE ortssok AS CHARACTER NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE antal_valda AS INTEGER NO-UNDO. 
DEFINE VARIABLE antal_raknare AS INTEGER NO-UNDO.  
DEFINE VARIABLE wh AS WIDGET-HANDLE.
DEFINE VARIABLE gatill AS CHARACTER NO-UNDO.



/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BRW_AONR

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES utsokaonr valdaao

/* Definitions for BROWSE BRW_AONR                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_AONR utsokaonr.OMRADE utsokaonr.AONR ~
utsokaonr.DELNR utsokaonr.ORT 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_AONR utsokaonr.AONR 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_AONR utsokaonr
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_AONR utsokaonr
&Scoped-define OPEN-QUERY-BRW_AONR OPEN QUERY BRW_AONR FOR EACH utsokaonr NO-LOCK ~
    BY utsokaonr.OMRADE ~
       BY utsokaonr.AONR ~
        BY utsokaonr.DELNR.
&Scoped-define TABLES-IN-QUERY-BRW_AONR utsokaonr
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_AONR utsokaonr


/* Definitions for BROWSE BRW_VAONR                                     */
&Scoped-define FIELDS-IN-QUERY-BRW_VAONR valdaao.AONR valdaao.DELNR ~
valdaao.ORT 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_VAONR valdaao.AONR 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_VAONR valdaao
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_VAONR valdaao
&Scoped-define OPEN-QUERY-BRW_VAONR OPEN QUERY BRW_VAONR FOR EACH valdaao NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_VAONR valdaao
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_VAONR valdaao


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-BRW_AONR}~
    ~{&OPEN-QUERY-BRW_VAONR}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BTN_NVE-3 BTN_NVE-4 TOG_PAGA TOG_AVSLUTADE ~
FILL-IN-AVSTARTD FILL-IN-AVSLUTD BTN_FVE-3 BTN_FVE-4 TOG_TILLF TOG_FASTA ~
CMB_OMR CMB_PROJ CMB_BESORG CMB_BERE CMB_FAK CMB_ANSV BTN_HAMT BRW_AONR ~
BRW_VAONR BTN_ALLOVER BTN_OVER BTN_BACK BTN_ALLBACK BTN_AVB FILL-IN_SAONR ~
FILL-IN_ORT FILL-IN_EAONR FILL-IN_DELNR RECT-50 RECT-51 RECT-52 RECT-8 
&Scoped-Define DISPLAYED-OBJECTS TOG_PAGA TOG_AVSLUTADE FILL-IN-MELL ~
FILL-IN-AVSTARTD FILL-IN-OCH FILL-IN-AVSLUTD TOG_TILLF TOG_FASTA CMB_OMR ~
CMB_PROJ CMB_BESORG CMB_BERE CMB_FAK CMB_ANSV FILL-IN_SAONR FILL-IN_ORT ~
FILL-IN_EAONR FILL-IN_DELNR FILL-IN-AOTEXT FILL-IN-SOK FILL-IN-VAL 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_ALLBACK 
     IMAGE-UP FILE "BILDER\first-u":U
     LABEL "Alla aonr i listan":L 
     SIZE 5.5 BY 1.77 TOOLTIP "Alla valda aonr tas bort från vallistan"
     FONT 11.

DEFINE BUTTON BTN_ALLOVER 
     IMAGE-UP FILE "BILDER\last-u":U
     LABEL "Alla aonr i listan":L 
     SIZE 5.5 BY 1.77 TOOLTIP "Alla aonr väljs"
     FONT 11.

DEFINE BUTTON BTN_AVB AUTO-END-KEY 
     LABEL "Avsluta":L 
     SIZE 15 BY 1.18.

DEFINE BUTTON BTN_AVROP 
     LABEL "Best - Avrop":L 
     SIZE 15 BY 1.18 TOOLTIP "Besställning/Avrop".

DEFINE BUTTON BTN_AVSAONR 
     LABEL "Avsluta aonr":L 
     SIZE 15 BY 1.18 TOOLTIP "Avsluta aonr:et. Ingen tidskrivning skall kan registreras mera".

DEFINE BUTTON BTN_BACK 
     IMAGE-UP FILE "BILDER\rewind-u":U
     LABEL "":L 
     SIZE 5.5 BY 1.77 TOOLTIP "Markerade tas bort från vallistan".

DEFINE BUTTON BTN_BER 
     LABEL "Bereda":L 
     SIZE 15 BY 1.18 TOOLTIP "Beredningsmodul".

DEFINE BUTTON BTN_BORT 
     LABEL "Ta bort":L 
     SIZE 10 BY 1.5 TOOLTIP "Ta bort ett nytt aonr".

DEFINE BUTTON BTN_EXTRA 
     LABEL "Enstaka aonr" 
     SIZE 5 BY 1 TOOLTIP "Tryck här för välja enstaka aonr."
     FONT 11.

DEFINE BUTTON BTN_FVE-3 
     LABEL "-" 
     SIZE 2.5 BY .77.

DEFINE BUTTON BTN_FVE-4 
     LABEL "-" 
     SIZE 2.5 BY .77.

DEFINE BUTTON BTN_HAMT 
     LABEL "Hämta och visa urval" 
     SIZE 20.88 BY 1.18 TOOLTIP "Dina val ovan avgör vilka aonr du får i listan nedan."
     FONT 11.

DEFINE BUTTON BTN_KALK 
     LABEL "Kalkylera":L 
     SIZE 15 BY 1.18 TOOLTIP "Kalkylera olika typer av kalkyler".

DEFINE BUTTON BTN_KOPI 
     LABEL "Kopiera":L 
     SIZE 15 BY 1.18 TOOLTIP "Kopiera ett aonr".

DEFINE BUTTON BTN_KOST 
     LABEL "Kostnadsreg":L 
     SIZE 15 BY 1.18 TOOLTIP "Registrera externa kostnader eller visa kostnader från andra system.".

DEFINE BUTTON BTN_MARK 
     LABEL "Markvärdera":L 
     SIZE 15 BY 1.18 TOOLTIP "Markvärderingsmodul".

DEFINE BUTTON BTN_NVE-3 
     LABEL "+" 
     SIZE 2.5 BY .77.

DEFINE BUTTON BTN_NVE-4 
     LABEL "+" 
     SIZE 2.5 BY .77.

DEFINE BUTTON BTN_NY 
     LABEL "Ny":L 
     SIZE 10 BY 1.5 TOOLTIP "Skapa ett nytt aonr.".

DEFINE BUTTON BTN_OVER 
     IMAGE-UP FILE "BILDER\forwrd-u":U
     LABEL "":L 
     SIZE 5.5 BY 1.77 TOOLTIP "Markerade väljs".

DEFINE BUTTON BTN_RAPP 
     LABEL "Rapporter":L 
     SIZE 15 BY 1.18 TOOLTIP "Rapport sammanstälning".

DEFINE BUTTON BTN_UNDER 
     LABEL "Underindela":L 
     SIZE 15 BY 1.18 TOOLTIP "Underindela dvs skapa nya delnr".

DEFINE BUTTON BTN_UPP 
     LABEL "Aonr huvud":L 
     SIZE 15 BY 1.18 TOOLTIP "Här kan du ändra allt som rör aonr:et".

DEFINE BUTTON BTN_VISAO 
     LABEL "Visa":L 
     SIZE 15 BY 1.18 TOOLTIP "Visa/skriv ut allt som rör aonr:et".

DEFINE VARIABLE CMB_ANSV AS CHARACTER FORMAT "X(256)":U 
     LABEL "Arbetsansvarig" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 22.5 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_BERE AS CHARACTER FORMAT "X(256)":U 
     LABEL "Beredare" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 22.5 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_BESORG AS CHARACTER FORMAT "X(256)":U 
     LABEL "Best./Kund" 
     VIEW-AS COMBO-BOX INNER-LINES 15
     DROP-DOWN-LIST
     SIZE 22.5 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_FAK AS CHARACTER FORMAT "X(256)":U 
     LABEL "Fakturakat." 
     VIEW-AS COMBO-BOX INNER-LINES 6
     LIST-ITEMS "Fastpris","Löpande räkning" 
     DROP-DOWN-LIST
     SIZE 22.5 BY 1 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE CMB_OMR AS CHARACTER FORMAT "X(256)":U INITIAL ? 
     LABEL "Område" 
     VIEW-AS COMBO-BOX INNER-LINES 15
     DROP-DOWN-LIST
     SIZE 22.5 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_PROJ AS CHARACTER FORMAT "X(256)":U 
     LABEL "Projektör" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 22.5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-AOTEXT AS CHARACTER FORMAT "X(256)":U INITIAL "Gör urval av arbetsorder" 
      VIEW-AS TEXT 
     SIZE 37.13 BY 1
     FONT 17 NO-UNDO.

DEFINE VARIABLE FILL-IN-AR AS INTEGER FORMAT "9999":U INITIAL ? 
     VIEW-AS FILL-IN 
     SIZE 9 BY .91 NO-UNDO.

DEFINE VARIABLE FILL-IN-AVSLUTD AS DATE FORMAT "99/99/99":U 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1 TOOLTIP "Avslutade mellan angivna datum" NO-UNDO.

DEFINE VARIABLE FILL-IN-AVSTARTD AS DATE FORMAT "99/99/99":U 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1 TOOLTIP "Avslutade mellan angivna datum" NO-UNDO.

DEFINE VARIABLE FILL-IN-MELL AS CHARACTER FORMAT "X(6)":U 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-OCH AS CHARACTER FORMAT "X(3)":U 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-SOK AS CHARACTER FORMAT "X(256)":U INITIAL "Sök i urval:" 
      VIEW-AS TEXT 
     SIZE 12.5 BY .68 NO-UNDO.

DEFINE VARIABLE FILL-IN-VAL AS CHARACTER FORMAT "X(256)":U INITIAL "Hämta aonr:" 
      VIEW-AS TEXT 
     SIZE 9.75 BY .68 NO-UNDO.

DEFINE VARIABLE FILL-IN_DELNR AS INTEGER FORMAT ">99" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 TOOLTIP "Tryck RETURN här för välja enstaka aonr.".

DEFINE VARIABLE FILL-IN_EAONR AS CHARACTER FORMAT "X(6)" 
     LABEL "Aonr" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 TOOLTIP "Tryck RETURN här för välja enstaka aonr.".

DEFINE VARIABLE FILL-IN_ORT AS CHARACTER FORMAT "x(40)" 
     LABEL "Ort/Benämning" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN_SAONR AS CHARACTER FORMAT "X(6)" 
     LABEL "Aonr" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE RAD_FAST AS LOGICAL 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Tillfälliga aonr", no,
"Fasta aonr", yes
     SIZE 24 BY 1
     BGCOLOR 8  NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE RAD_PAAV AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Pågående", 1,
"Avslutade", 2
     SIZE 20 BY 1 TOOLTIP "Utsökningen ska gälla antingen pågående eller avslutade eller både och."
     BGCOLOR 8  NO-UNDO.

DEFINE RECTANGLE RECT-50
     EDGE-PIXELS 4 GRAPHIC-EDGE  NO-FILL 
     SIZE 48.5 BY 2.45
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-51
     EDGE-PIXELS 4 GRAPHIC-EDGE  NO-FILL 
     SIZE 18 BY 23.77.

DEFINE RECTANGLE RECT-52
     EDGE-PIXELS 4 GRAPHIC-EDGE  NO-FILL 
     SIZE 23 BY 2.45
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-8
     EDGE-PIXELS 4 GRAPHIC-EDGE  NO-FILL 
     SIZE 80 BY 23.77
     BGCOLOR 8 .

DEFINE VARIABLE TOG_AVSLUTADE AS LOGICAL INITIAL no 
     LABEL "Avslutade" 
     VIEW-AS TOGGLE-BOX
     SIZE 19 BY .86 TOOLTIP "Utsökningen ska gälla avslutade mellan angivna datum." NO-UNDO.

DEFINE VARIABLE TOG_FASTA AS LOGICAL INITIAL no 
     LABEL "Fasta" 
     VIEW-AS TOGGLE-BOX
     SIZE 20 BY .86 NO-UNDO.

DEFINE VARIABLE TOG_PAGA AS LOGICAL INITIAL no 
     LABEL "Pågående" 
     VIEW-AS TOGGLE-BOX
     SIZE 20 BY .86 TOOLTIP "Utsökningen ska gälla pågående." NO-UNDO.

DEFINE VARIABLE TOG_TILLF AS LOGICAL INITIAL no 
     LABEL "Tillfälliga" 
     VIEW-AS TOGGLE-BOX
     SIZE 20 BY .86 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_AONR FOR 
      utsokaonr SCROLLING.

DEFINE QUERY BRW_VAONR FOR 
      valdaao SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_AONR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_AONR C-Win _STRUCTURED
  QUERY BRW_AONR NO-LOCK DISPLAY
      utsokaonr.OMRADE COLUMN-LABEL "Område" FORMAT "X(6)":U
      utsokaonr.AONR COLUMN-LABEL "Aonr" FORMAT "X(6)":U
      utsokaonr.DELNR COLUMN-LABEL "Delnr" FORMAT ">99":U
      utsokaonr.ORT COLUMN-LABEL "Ort/Benämning" FORMAT "x(40)":U
  ENABLE
      utsokaonr.AONR
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING MULTIPLE SIZE 47 BY 9.95
         TITLE "Urvalsresultat".

DEFINE BROWSE BRW_VAONR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_VAONR C-Win _STRUCTURED
  QUERY BRW_VAONR NO-LOCK DISPLAY
      valdaao.AONR COLUMN-LABEL "Aonr" FORMAT "X(6)":U
      valdaao.DELNR COLUMN-LABEL "Delnr" FORMAT ">99":U
      valdaao.ORT COLUMN-LABEL "Ort/Benämning" FORMAT "x(40)":U
  ENABLE
      valdaao.AONR
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING MULTIPLE SIZE 21.75 BY 9.95
         TITLE "Arbeta vidare med".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     BTN_NVE-3 AT ROW 2.09 COL 59.63
     BTN_NVE-4 AT ROW 2.09 COL 77
     BTN_UPP AT ROW 2.32 COL 82.5
     TOG_PAGA AT ROW 2.36 COL 2
     TOG_AVSLUTADE AT ROW 2.36 COL 23.5
     FILL-IN-MELL AT ROW 2.36 COL 40.5 COLON-ALIGNED NO-LABEL
     FILL-IN-AVSTARTD AT ROW 2.36 COL 47.63 COLON-ALIGNED NO-LABEL
     FILL-IN-OCH AT ROW 2.36 COL 61 COLON-ALIGNED NO-LABEL
     FILL-IN-AVSLUTD AT ROW 2.36 COL 65 COLON-ALIGNED NO-LABEL
     BTN_FVE-3 AT ROW 2.95 COL 59.63
     BTN_FVE-4 AT ROW 2.95 COL 77
     TOG_TILLF AT ROW 3.45 COL 2
     TOG_FASTA AT ROW 3.45 COL 23.5
     BTN_UNDER AT ROW 4.14 COL 82.5
     CMB_OMR AT ROW 4.55 COL 8.5
     CMB_PROJ AT ROW 4.55 COL 54.5 COLON-ALIGNED
     CMB_BESORG AT ROW 5.82 COL 14.5 COLON-ALIGNED
     CMB_BERE AT ROW 5.82 COL 54.5 COLON-ALIGNED
     BTN_KOPI AT ROW 5.95 COL 82.5
     CMB_FAK AT ROW 7.09 COL 14.5 COLON-ALIGNED
     CMB_ANSV AT ROW 7.09 COL 54.5 COLON-ALIGNED
     BTN_VISAO AT ROW 7.77 COL 82.5
     BTN_HAMT AT ROW 8.36 COL 16.5
     BTN_AVSAONR AT ROW 9.59 COL 82.5
     BRW_AONR AT ROW 9.73 COL 2
     BRW_VAONR AT ROW 9.73 COL 58
     BTN_ALLOVER AT ROW 11.36 COL 51
     BTN_RAPP AT ROW 11.41 COL 82.5
     BTN_KOST AT ROW 13.23 COL 82.5
     BTN_OVER AT ROW 13.55 COL 51
     BTN_KALK AT ROW 15.05 COL 82.5
     BTN_BACK AT ROW 15.77 COL 51
     BTN_BER AT ROW 16.86 COL 82.5
     BTN_ALLBACK AT ROW 17.95 COL 51
     BTN_MARK AT ROW 18.68 COL 82.5
     BTN_NY AT ROW 20.36 COL 58
     BTN_BORT AT ROW 20.36 COL 69.75
     BTN_AVROP AT ROW 20.5 COL 82.5
     BTN_AVB AT ROW 22.27 COL 82.5
     FILL-IN_SAONR AT ROW 23.09 COL 8.5 COLON-ALIGNED
     FILL-IN_ORT AT ROW 23.09 COL 31 COLON-ALIGNED
     FILL-IN_EAONR AT ROW 23.09 COL 63.5 COLON-ALIGNED AUTO-RETURN 
     FILL-IN_DELNR AT ROW 23.09 COL 72.38 COLON-ALIGNED NO-LABEL
     FILL-IN-AR AT ROW 23.64 COL 85 COLON-ALIGNED NO-LABEL
     RAD_PAAV AT ROW 23.91 COL 78 NO-LABEL
     BTN_EXTRA AT ROW 23.91 COL 88.5
     RAD_FAST AT ROW 24.18 COL 75 NO-LABEL
     FILL-IN-AOTEXT AT ROW 1.27 COL 1 COLON-ALIGNED NO-LABEL
     FILL-IN-SOK AT ROW 22.27 COL 2.5 NO-LABEL
     FILL-IN-VAL AT ROW 22.27 COL 57.5 COLON-ALIGNED NO-LABEL
     RECT-50 AT ROW 22 COL 2
     RECT-51 AT ROW 1 COL 81
     RECT-52 AT ROW 22 COL 57
     RECT-8 AT ROW 1 COL 1
     "Funktioner:" VIEW-AS TEXT
          SIZE 11.5 BY .91 AT ROW 1.27 COL 82.5
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 98.13 BY 24.18.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Temp-Tables and Buffers:
      TABLE: utsokaonr T "?" NO-UNDO temp-db utsokaonr
      TABLE: valdaao T "?" NO-UNDO temp-db valdaao
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "<insert window title>"
         HEIGHT             = 24.41
         WIDTH              = 98.88
         MAX-HEIGHT         = 25
         MAX-WIDTH          = 100
         VIRTUAL-HEIGHT     = 25
         VIRTUAL-WIDTH      = 100
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
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
                                                                        */
/* BROWSE-TAB BRW_AONR BTN_AVSAONR DEFAULT-FRAME */
/* BROWSE-TAB BRW_VAONR BRW_AONR DEFAULT-FRAME */
ASSIGN 
       BRW_AONR:MAX-DATA-GUESS IN FRAME DEFAULT-FRAME         = 1000.

ASSIGN 
       BRW_VAONR:MAX-DATA-GUESS IN FRAME DEFAULT-FRAME         = 10000.

/* SETTINGS FOR BUTTON BTN_AVROP IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       BTN_AVROP:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON BTN_AVSAONR IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       BTN_AVSAONR:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON BTN_BER IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       BTN_BER:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON BTN_BORT IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       BTN_BORT:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON BTN_EXTRA IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       BTN_EXTRA:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

ASSIGN 
       BTN_FVE-3:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

ASSIGN 
       BTN_FVE-4:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON BTN_KALK IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       BTN_KALK:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON BTN_KOPI IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       BTN_KOPI:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON BTN_KOST IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       BTN_KOST:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON BTN_MARK IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       BTN_MARK:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

ASSIGN 
       BTN_NVE-3:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

ASSIGN 
       BTN_NVE-4:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON BTN_NY IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       BTN_NY:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON BTN_RAPP IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       BTN_RAPP:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON BTN_UNDER IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       BTN_UNDER:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON BTN_UPP IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       BTN_UPP:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR BUTTON BTN_VISAO IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       BTN_VISAO:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR COMBO-BOX CMB_OMR IN FRAME DEFAULT-FRAME
   SHARED ALIGN-L                                                       */
/* SETTINGS FOR FILL-IN FILL-IN-AOTEXT IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-AR IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN-AR:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

ASSIGN 
       FILL-IN-AVSLUTD:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

ASSIGN 
       FILL-IN-AVSTARTD:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-MELL IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       FILL-IN-MELL:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-OCH IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       FILL-IN-OCH:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-SOK IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN FILL-IN-VAL IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR RADIO-SET RAD_FAST IN FRAME DEFAULT-FRAME
   NO-DISPLAY SHARED NO-ENABLE                                          */
ASSIGN 
       RAD_FAST:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR RADIO-SET RAD_PAAV IN FRAME DEFAULT-FRAME
   NO-DISPLAY SHARED NO-ENABLE                                          */
ASSIGN 
       RAD_PAAV:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_AONR
/* Query rebuild information for BROWSE BRW_AONR
     _TblList          = "Temp-Tables.utsokaonr"
     _Options          = "NO-LOCK"
     _OrdList          = "Temp-Tables.utsokaonr.OMRADE|yes,Temp-Tables.utsokaonr.AONR|yes,Temp-Tables.utsokaonr.DELNR|yes"
     _FldNameList[1]   > Temp-Tables.utsokaonr.OMRADE
"utsokaonr.OMRADE" "Område" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.utsokaonr.AONR
"utsokaonr.AONR" "Aonr" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.utsokaonr.DELNR
"utsokaonr.DELNR" "Delnr" ">99" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[4]   > Temp-Tables.utsokaonr.ORT
"utsokaonr.ORT" "Ort/Benämning" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is OPENED
*/  /* BROWSE BRW_AONR */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_VAONR
/* Query rebuild information for BROWSE BRW_VAONR
     _TblList          = "Temp-Tables.valdaao"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.valdaao.AONR
"valdaao.AONR" "Aonr" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.valdaao.DELNR
"valdaao.DELNR" "Delnr" ">99" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.valdaao.ORT
"valdaao.ORT" "Ort/Benämning" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is OPENED
*/  /* BROWSE BRW_VAONR */
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




&Scoped-define BROWSE-NAME BRW_AONR
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
   RUN enable_UI.
  {FRMSIZE.I}
      
   IF NOT THIS-PROCEDURE:PERSISTENT THEN
   WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */


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
  DISPLAY TOG_PAGA TOG_AVSLUTADE FILL-IN-MELL FILL-IN-AVSTARTD FILL-IN-OCH 
          FILL-IN-AVSLUTD TOG_TILLF TOG_FASTA CMB_OMR CMB_PROJ CMB_BESORG 
          CMB_BERE CMB_FAK CMB_ANSV FILL-IN_SAONR FILL-IN_ORT FILL-IN_EAONR 
          FILL-IN_DELNR FILL-IN-AOTEXT FILL-IN-SOK FILL-IN-VAL 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE BTN_NVE-3 BTN_NVE-4 TOG_PAGA TOG_AVSLUTADE FILL-IN-AVSTARTD 
         FILL-IN-AVSLUTD BTN_FVE-3 BTN_FVE-4 TOG_TILLF TOG_FASTA CMB_OMR 
         CMB_PROJ CMB_BESORG CMB_BERE CMB_FAK CMB_ANSV BTN_HAMT BRW_AONR 
         BRW_VAONR BTN_ALLOVER BTN_OVER BTN_BACK BTN_ALLBACK BTN_AVB 
         FILL-IN_SAONR FILL-IN_ORT FILL-IN_EAONR FILL-IN_DELNR RECT-50 RECT-51 
         RECT-52 RECT-8 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

