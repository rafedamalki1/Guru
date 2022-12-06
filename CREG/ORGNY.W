&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win


/* Temp-Table and Buffer definitions                                    */



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
DEFINE INPUT-OUTPUT PARAMETER vem AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER vemanvandare AS CHARACTER NO-UNDO.
/* Local Variable Definitions ---                                       */
{ALLDEF.I}
{ANVTEMPS.I}
{PRIARBANLTEMP.I}
DEFINE TEMP-TABLE omranvandartemp NO-UNDO LIKE anvandartemp.
DEFINE VARIABLE anvapph AS HANDLE NO-UNDO.
&Scoped-define NEW 
{OMRALLT.I}
{GLOBVAR2DEL1.I}
DEFINE VARIABLE priapph AS HANDLE NO-UNDO.
DEFINE SHARED VARIABLE orgapph AS HANDLE NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE VARIABLE stat AS LOGICAL NO-UNDO.
DEFINE VARIABLE  status-ok AS LOGICAL NO-UNDO.



{SOKDEF.I}

   /**/
DEFINE VARIABLE entryepost AS LOGICAL NO-UNDO.
   DEFINE VARIABLE edataapph AS HANDLE NO-UNDO.
DEFINE VARIABLE bestapph AS HANDLE NO-UNDO.
{KONTAKTPERSTEMP.I}
{BESTKUNDALLT.I}
{EXTRADATA.I}
{EXTRATAB.I}  


/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BRW_ANV

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES anvandartemp omranvandartemp kontaktperstemp ~
priotemp prioomrtemp omrallt

/* Definitions for BROWSE BRW_ANV                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_ANV anvandartemp.ANVANDARE ~
anvandartemp.AV-NAMN anvandartemp.PERSONALKOD anvandartemp.KATEGORI ~
anvandartemp.FAKTURAREF 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_ANV 
&Scoped-define QUERY-STRING-BRW_ANV FOR EACH anvandartemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_ANV OPEN QUERY BRW_ANV FOR EACH anvandartemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_ANV anvandartemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_ANV anvandartemp


/* Definitions for BROWSE BRW_ANVOMR                                    */
&Scoped-define FIELDS-IN-QUERY-BRW_ANVOMR omranvandartemp.ANVANDARE ~
omranvandartemp.AV-NAMN omranvandartemp.PERSONALKOD 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_ANVOMR 
&Scoped-define QUERY-STRING-BRW_ANVOMR FOR EACH omranvandartemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_ANVOMR OPEN QUERY BRW_ANVOMR FOR EACH omranvandartemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_ANVOMR omranvandartemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_ANVOMR omranvandartemp


/* Definitions for BROWSE BRW_KONTAKT                                   */
&Scoped-define FIELDS-IN-QUERY-BRW_KONTAKT kontaktperstemp.NAMN ~
kontaktperstemp.EPOST kontaktperstemp.TELEFON kontaktperstemp.MOBIL ~
kontaktperstemp.FAX 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_KONTAKT kontaktperstemp.NAMN ~
kontaktperstemp.EPOST kontaktperstemp.TELEFON kontaktperstemp.MOBIL ~
kontaktperstemp.FAX 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_KONTAKT kontaktperstemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_KONTAKT kontaktperstemp
&Scoped-define QUERY-STRING-BRW_KONTAKT FOR EACH kontaktperstemp WHERE TRUE /* Join to bestkundallt incomplete */ NO-LOCK
&Scoped-define OPEN-QUERY-BRW_KONTAKT OPEN QUERY BRW_KONTAKT FOR EACH kontaktperstemp WHERE TRUE /* Join to bestkundallt incomplete */ NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_KONTAKT kontaktperstemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_KONTAKT kontaktperstemp


/* Definitions for BROWSE BRW_PRI                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_PRI priotemp.PKOD priotemp.P-BENAMNING ~
priotemp.AKTIV 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_PRI priotemp.PKOD 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_PRI priotemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_PRI priotemp
&Scoped-define QUERY-STRING-BRW_PRI FOR EACH priotemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_PRI OPEN QUERY BRW_PRI FOR EACH priotemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_PRI priotemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_PRI priotemp


/* Definitions for BROWSE BRW_PRIOMR                                    */
&Scoped-define FIELDS-IN-QUERY-BRW_PRIOMR prioomrtemp.PKOD ~
prioomrtemp.P-BENAMNING prioomrtemp.AKTIV 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_PRIOMR prioomrtemp.AKTIV 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_PRIOMR prioomrtemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_PRIOMR prioomrtemp
&Scoped-define QUERY-STRING-BRW_PRIOMR FOR EACH prioomrtemp NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BRW_PRIOMR OPEN QUERY BRW_PRIOMR FOR EACH prioomrtemp NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BRW_PRIOMR prioomrtemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_PRIOMR prioomrtemp


/* Definitions for FRAME FRAME-ANV                                      */

/* Definitions for FRAME FRAME-GRUND                                    */
&Scoped-define QUERY-STRING-FRAME-GRUND FOR EACH omrallt SHARE-LOCK
&Scoped-define OPEN-QUERY-FRAME-GRUND OPEN QUERY FRAME-GRUND FOR EACH omrallt SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-FRAME-GRUND omrallt
&Scoped-define FIRST-TABLE-IN-QUERY-FRAME-GRUND omrallt


/* Definitions for FRAME FRAME-PRIO                                     */
&Scoped-define OPEN-BROWSERS-IN-QUERY-FRAME-PRIO ~
    ~{&OPEN-QUERY-BRW_PRI}~
    ~{&OPEN-QUERY-BRW_PRIOMR}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RAD_GRUND BTN_OK BTN_AVBRYT 
&Scoped-Define DISPLAYED-OBJECTS RAD_GRUND 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD tabortblanka C-Win 
FUNCTION tabortblanka RETURNS CHARACTER
  ( INPUT namnblanka AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVBRYT 
     LABEL "Avbryt":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_OK 
     LABEL "Ok":L 
     SIZE 14 BY 1.

DEFINE VARIABLE RAD_GRUND AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Grunduppgifter", 1,
"Områdes ansvariga", 2
     SIZE 49.5 BY 1 NO-UNDO.

DEFINE BUTTON BTN_ALLBACK 
     IMAGE-UP FILE "BILDER\rewind-u":U
     LABEL "Alla aonr i listan":L 
     SIZE 4 BY 1.21 TOOLTIP "Alla valda plannr tas bort från vallistan"
     FONT 11.

DEFINE BUTTON BTN_ALLOVER 
     IMAGE-UP FILE "BILDER\forwrd-u":U
     LABEL "Alla plannr i listan":L 
     SIZE 4 BY 1.21 TOOLTIP "Alla plannr väljs"
     FONT 11.

DEFINE BUTTON BTN_BACK 
     IMAGE-UP FILE "BILDER\prev-u":U
     LABEL "":L 
     SIZE 4 BY 1.21 TOOLTIP "Markerade tas bort från vallistan".

DEFINE BUTTON BTN_OVER 
     IMAGE-UP FILE "BILDER\next-u":U
     LABEL "":L 
     SIZE 4 BY 1.21 TOOLTIP "Markerade väljs".

DEFINE BUTTON BTN_ANDRA 
     LABEL "Ändra" 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_BORT 
     LABEL "Ta bort" 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_NY 
     LABEL "Ny" 
     SIZE 12 BY 1.

DEFINE VARIABLE CMB_AVD AS CHARACTER FORMAT "X(256)":U 
     LABEL "Avdelning" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 21.5 BY 1 NO-UNDO.

DEFINE BUTTON BTN_ALLBACK-2 
     IMAGE-UP FILE "BILDER\rewind-u":U
     LABEL "Alla aonr i listan":L 
     SIZE 4 BY 1.21 TOOLTIP "Alla valda plannr tas bort från vallistan"
     FONT 11.

DEFINE BUTTON BTN_ALLOVER-2 
     IMAGE-UP FILE "BILDER\forwrd-u":U
     LABEL "Alla plannr i listan":L 
     SIZE 4 BY 1.21 TOOLTIP "Alla plannr väljs"
     FONT 11.

DEFINE BUTTON BTN_BACK-2 
     IMAGE-UP FILE "BILDER\prev-u":U
     LABEL "":L 
     SIZE 4 BY 1.21 TOOLTIP "Markerade tas bort från vallistan".

DEFINE BUTTON BTN_OVER-2 
     IMAGE-UP FILE "BILDER\next-u":U
     LABEL "":L 
     SIZE 4 BY 1.21 TOOLTIP "Markerade väljs".

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_ANV FOR 
      anvandartemp SCROLLING.

DEFINE QUERY BRW_ANVOMR FOR 
      omranvandartemp SCROLLING.

DEFINE QUERY BRW_KONTAKT FOR 
      kontaktperstemp SCROLLING.

DEFINE QUERY BRW_PRI FOR 
      priotemp SCROLLING.

DEFINE QUERY BRW_PRIOMR FOR 
      prioomrtemp SCROLLING.

DEFINE QUERY FRAME-GRUND FOR 
      omrallt SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_ANV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_ANV C-Win _STRUCTURED
  QUERY BRW_ANV NO-LOCK DISPLAY
      anvandartemp.ANVANDARE COLUMN-LABEL "Användare" FORMAT "x(256)":U
            WIDTH 12
      anvandartemp.AV-NAMN COLUMN-LABEL "Användarnamn" FORMAT "x(256)":U
            WIDTH 34.13
      anvandartemp.PERSONALKOD COLUMN-LABEL "Enhet/!Sign" FORMAT "x(5)":U
      anvandartemp.KATEGORI COLUMN-LABEL "Kategori" FORMAT "x(256)":U
            WIDTH 8
      anvandartemp.FAKTURAREF COLUMN-LABEL "Faktura" FORMAT "x(256)":U
            WIDTH 8
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING MULTIPLE SIZE 56.13 BY 17.96
         FONT 4
         TITLE "Användare".

DEFINE BROWSE BRW_ANVOMR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_ANVOMR C-Win _STRUCTURED
  QUERY BRW_ANVOMR NO-LOCK DISPLAY
      omranvandartemp.ANVANDARE COLUMN-LABEL "Användare" FORMAT "x(256)":U
            WIDTH 12
      omranvandartemp.AV-NAMN COLUMN-LABEL "Användarnamn" FORMAT "x(256)":U
            WIDTH 34.13
      omranvandartemp.PERSONALKOD COLUMN-LABEL "Enhet/!Sign" FORMAT "x(5)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING MULTIPLE SIZE 56.13 BY 17.96
         FONT 4
         TITLE "Ansvariga".

DEFINE BROWSE BRW_KONTAKT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_KONTAKT C-Win _STRUCTURED
  QUERY BRW_KONTAKT NO-LOCK DISPLAY
      kontaktperstemp.NAMN COLUMN-LABEL "Namn" FORMAT "X(256)":U
            WIDTH 17
      kontaktperstemp.EPOST COLUMN-LABEL "E-post" FORMAT "X(256)":U
            WIDTH 20.25
      kontaktperstemp.TELEFON COLUMN-LABEL "Telefon" FORMAT "X(256)":U
            WIDTH 12
      kontaktperstemp.MOBIL COLUMN-LABEL "Mobil" FORMAT "X(256)":U
            WIDTH 12
      kontaktperstemp.FAX COLUMN-LABEL "Fax" FORMAT "X(256)":U
            WIDTH 12
  ENABLE
      kontaktperstemp.NAMN
      kontaktperstemp.EPOST
      kontaktperstemp.TELEFON
      kontaktperstemp.MOBIL
      kontaktperstemp.FAX
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING MULTIPLE SIZE 61 BY 13.75
         FONT 4
         TITLE "Alla kontaktpersoner" FIT-LAST-COLUMN.

DEFINE BROWSE BRW_PRI
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_PRI C-Win _STRUCTURED
  QUERY BRW_PRI NO-LOCK DISPLAY
      priotemp.PKOD COLUMN-LABEL "Prioritet" FORMAT ">>9":U
      priotemp.P-BENAMNING COLUMN-LABEL "Benämning" FORMAT "x(25)":U
      priotemp.AKTIV COLUMN-LABEL "Aktiv" FORMAT "Ja/Nej":U
  ENABLE
      priotemp.PKOD
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 48 BY 17.25
         TITLE "Prioritet".

DEFINE BROWSE BRW_PRIOMR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_PRIOMR C-Win _STRUCTURED
  QUERY BRW_PRIOMR NO-LOCK DISPLAY
      prioomrtemp.PKOD COLUMN-LABEL "Prioritet" FORMAT ">>":U
      prioomrtemp.P-BENAMNING COLUMN-LABEL "Benämning" FORMAT "x(25)":U
            WIDTH 11.13
      prioomrtemp.AKTIV COLUMN-LABEL "Aktiv" FORMAT "ja/nej":U
  ENABLE
      prioomrtemp.AKTIV
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 48 BY 17.25
         TITLE "Prioritet" FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     RAD_GRUND AT ROW 1.25 COL 3.5 NO-LABEL
     BTN_OK AT ROW 28 COL 95
     BTN_AVBRYT AT ROW 28 COL 110
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 125 BY 28.42.

DEFINE FRAME FRAME-GRUND
     omrallt.OMRADE AT ROW 1.79 COL 19 COLON-ALIGNED
          LABEL "Område"
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
     omrallt.NAMN AT ROW 1.79 COL 44.5 COLON-ALIGNED
          LABEL "Benämning"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     BRW_KONTAKT AT ROW 1.79 COL 64.25
     omrallt.PROJINT1 AT ROW 3.79 COL 19 COLON-ALIGNED
          LABEL "Aonr intervall"
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
     omrallt.PROJINT2 AT ROW 3.79 COL 34.5 COLON-ALIGNED
          LABEL "--"
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
     omrallt.SISTPROJ AT ROW 4.92 COL 19 COLON-ALIGNED
          LABEL "Nästa lediga nr"
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
     omrallt.KALKYLINT1 AT ROW 6.63 COL 19 COLON-ALIGNED
          LABEL "Kalkyl intervall" FORMAT "->>>>>>9"
          VIEW-AS FILL-IN 
          SIZE 11 BY 1
     omrallt.KALKYLINT2 AT ROW 6.63 COL 34.5 COLON-ALIGNED
          LABEL "--" FORMAT "->>>>>>9"
          VIEW-AS FILL-IN 
          SIZE 11 BY 1
     omrallt.KALKYLSIST AT ROW 7.79 COL 19 COLON-ALIGNED
          LABEL "Nästa lediga nr" FORMAT ">>>>>>9"
          VIEW-AS FILL-IN 
          SIZE 11 BY 1
     omrallt.PLANINT1 AT ROW 9.67 COL 19 COLON-ALIGNED
          LABEL "Plan intervall"
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
     omrallt.PLANINT2 AT ROW 9.67 COL 34.5 COLON-ALIGNED
          LABEL "--"
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
     omrallt.SISTPLAN AT ROW 11.04 COL 19 COLON-ALIGNED
          LABEL "Nästa lediga nr"
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
     omrallt.VARDINT1 AT ROW 13.04 COL 19 COLON-ALIGNED
          LABEL "Värdering interv."
          VIEW-AS FILL-IN 
          SIZE 11 BY 1
     omrallt.VARDINT2 AT ROW 13.04 COL 34.5 COLON-ALIGNED
          LABEL "--"
          VIEW-AS FILL-IN 
          SIZE 11 BY 1
     omrallt.VARDSIST AT ROW 14.21 COL 19 COLON-ALIGNED
          LABEL "Nästa lediga nr"
          VIEW-AS FILL-IN 
          SIZE 11 BY 1
     omrallt.ORGIDNUM AT ROW 15.96 COL 19 COLON-ALIGNED
          LABEL "Nummerisk.org"
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
     omrallt.KONTOOB AT ROW 15.96 COL 44.5 COLON-ALIGNED
          LABEL "Obliga.konto" FORMAT "Ja/Nej"
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
     BTN_NY AT ROW 16.33 COL 79.88
     BTN_ANDRA AT ROW 16.33 COL 93
     BTN_BORT AT ROW 16.33 COL 106.13
     omrallt.OMKMTR AT ROW 17.17 COL 19 COLON-ALIGNED
          LABEL "Materielomkostnad"
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
     omrallt.OMKOSTP AT ROW 18.38 COL 19 COLON-ALIGNED
          LABEL "Omkostnadspåslag"
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
     CMB_AVD AT ROW 20.46 COL 19 COLON-ALIGNED
     omrallt.GATUADR AT ROW 21.75 COL 19 COLON-ALIGNED
          LABEL "Gatuadress"
          VIEW-AS FILL-IN 
          SIZE 26 BY 1
     omrallt.POSTNR AT ROW 22.96 COL 19 COLON-ALIGNED
          LABEL "Postnummer"
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
     omrallt.POSTANST AT ROW 22.96 COL 44.5 COLON-ALIGNED
          LABEL "Postadress"
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     omrallt.TELVXL AT ROW 24.25 COL 19 COLON-ALIGNED
          LABEL "Telefonnr" FORMAT "x(256)"
          VIEW-AS FILL-IN 
          SIZE 13 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 3
         SIZE 125 BY 24.75.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME FRAME-GRUND
     omrallt.TELJOUR AT ROW 24.25 COL 44.5 COLON-ALIGNED
          LABEL "Journummer" FORMAT "x(256)"
          VIEW-AS FILL-IN 
          SIZE 13 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 3
         SIZE 125 BY 24.75.

DEFINE FRAME FRAME-PRIO
     BRW_PRI AT ROW 2.25 COL 3 WIDGET-ID 200
     BRW_PRIOMR AT ROW 2.25 COL 61.63 WIDGET-ID 300
     BTN_ALLOVER-2 AT ROW 7.5 COL 52.38 WIDGET-ID 4
     BTN_OVER-2 AT ROW 9.67 COL 52.38 WIDGET-ID 8
     BTN_BACK-2 AT ROW 11.88 COL 52.38 WIDGET-ID 6
     BTN_ALLBACK-2 AT ROW 14.08 COL 52.38 WIDGET-ID 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 3
         SIZE 125 BY 24.75 WIDGET-ID 100.

DEFINE FRAME FRAME-ANV
     BRW_ANV AT ROW 2.5 COL 1.88
     BRW_ANVOMR AT ROW 2.5 COL 63.5
     BTN_ALLOVER AT ROW 7.5 COL 59
     BTN_OVER AT ROW 9.67 COL 59
     BTN_BACK AT ROW 11.88 COL 59
     BTN_ALLBACK AT ROW 14.08 COL 59
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 3
         SIZE 125 BY 24.75.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Temp-Tables and Buffers:
      TABLE: anvandartemp T "?" NO-UNDO temp-db anvandartemp
      TABLE: kontaktperstemp T "?" NO-UNDO temp-db kontaktperstemp
      TABLE: omrallt T "?" NO-UNDO temp-db omrallt
      TABLE: omranvandartemp T "?" NO-UNDO temp-db omranvandartemp
      TABLE: prioomrtemp T "?" NO-UNDO temp-db prioomrtemp
      TABLE: priotemp T "?" NO-UNDO temp-db priotemp
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
         WIDTH              = 125
         MAX-HEIGHT         = 37.67
         MAX-WIDTH          = 160
         VIRTUAL-HEIGHT     = 37.67
         VIRTUAL-WIDTH      = 160
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
/* REPARENT FRAME */
ASSIGN FRAME FRAME-ANV:FRAME = FRAME DEFAULT-FRAME:HANDLE
       FRAME FRAME-GRUND:FRAME = FRAME DEFAULT-FRAME:HANDLE
       FRAME FRAME-PRIO:FRAME = FRAME DEFAULT-FRAME:HANDLE.

/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */

DEFINE VARIABLE XXTABVALXX AS LOGICAL NO-UNDO.

ASSIGN XXTABVALXX = FRAME FRAME-PRIO:MOVE-AFTER-TAB-ITEM (RAD_GRUND:HANDLE IN FRAME DEFAULT-FRAME)
       XXTABVALXX = FRAME FRAME-GRUND:MOVE-BEFORE-TAB-ITEM (BTN_OK:HANDLE IN FRAME DEFAULT-FRAME)
       XXTABVALXX = FRAME FRAME-ANV:MOVE-BEFORE-TAB-ITEM (FRAME FRAME-GRUND:HANDLE)
       XXTABVALXX = FRAME FRAME-PRIO:MOVE-BEFORE-TAB-ITEM (FRAME FRAME-ANV:HANDLE)
/* END-ASSIGN-TABS */.

/* SETTINGS FOR FRAME FRAME-ANV
                                                                        */
/* BROWSE-TAB BRW_ANV 1 FRAME-ANV */
/* BROWSE-TAB BRW_ANVOMR BRW_ANV FRAME-ANV */
ASSIGN 
       BRW_ANV:ALLOW-COLUMN-SEARCHING IN FRAME FRAME-ANV = TRUE.

ASSIGN 
       BRW_ANVOMR:ALLOW-COLUMN-SEARCHING IN FRAME FRAME-ANV = TRUE.

/* SETTINGS FOR FRAME FRAME-GRUND
   NOT-VISIBLE                                                          */
/* BROWSE-TAB BRW_KONTAKT NAMN FRAME-GRUND */
/* SETTINGS FOR FILL-IN omrallt.GATUADR IN FRAME FRAME-GRUND
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN omrallt.KALKYLINT1 IN FRAME FRAME-GRUND
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN omrallt.KALKYLINT2 IN FRAME FRAME-GRUND
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN omrallt.KALKYLSIST IN FRAME FRAME-GRUND
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN omrallt.KONTOOB IN FRAME FRAME-GRUND
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN omrallt.NAMN IN FRAME FRAME-GRUND
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN omrallt.OMKMTR IN FRAME FRAME-GRUND
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN omrallt.OMKOSTP IN FRAME FRAME-GRUND
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN omrallt.OMRADE IN FRAME FRAME-GRUND
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN omrallt.ORGIDNUM IN FRAME FRAME-GRUND
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN omrallt.PLANINT1 IN FRAME FRAME-GRUND
   EXP-LABEL                                                            */
ASSIGN 
       omrallt.PLANINT1:HIDDEN IN FRAME FRAME-GRUND           = TRUE.

/* SETTINGS FOR FILL-IN omrallt.PLANINT2 IN FRAME FRAME-GRUND
   EXP-LABEL                                                            */
ASSIGN 
       omrallt.PLANINT2:HIDDEN IN FRAME FRAME-GRUND           = TRUE.

/* SETTINGS FOR FILL-IN omrallt.POSTANST IN FRAME FRAME-GRUND
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN omrallt.POSTNR IN FRAME FRAME-GRUND
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN omrallt.PROJINT1 IN FRAME FRAME-GRUND
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN omrallt.PROJINT2 IN FRAME FRAME-GRUND
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN omrallt.SISTPLAN IN FRAME FRAME-GRUND
   EXP-LABEL                                                            */
ASSIGN 
       omrallt.SISTPLAN:HIDDEN IN FRAME FRAME-GRUND           = TRUE.

/* SETTINGS FOR FILL-IN omrallt.SISTPROJ IN FRAME FRAME-GRUND
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN omrallt.TELJOUR IN FRAME FRAME-GRUND
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN omrallt.TELVXL IN FRAME FRAME-GRUND
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN omrallt.VARDINT1 IN FRAME FRAME-GRUND
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN omrallt.VARDINT2 IN FRAME FRAME-GRUND
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN omrallt.VARDSIST IN FRAME FRAME-GRUND
   EXP-LABEL                                                            */
/* SETTINGS FOR FRAME FRAME-PRIO
                                                                        */
/* BROWSE-TAB BRW_PRI 1 FRAME-PRIO */
/* BROWSE-TAB BRW_PRIOMR BRW_PRI FRAME-PRIO */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_ANV
/* Query rebuild information for BROWSE BRW_ANV
     _TblList          = "Temp-Tables.anvandartemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.anvandartemp.ANVANDARE
"anvandartemp.ANVANDARE" "Användare" "x(256)" "character" ? ? ? ? ? ? no ? no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.anvandartemp.AV-NAMN
"anvandartemp.AV-NAMN" "Användarnamn" "x(256)" "character" ? ? ? ? ? ? no ? no no "34.13" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.anvandartemp.PERSONALKOD
"anvandartemp.PERSONALKOD" "Enhet/!Sign" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.anvandartemp.KATEGORI
"anvandartemp.KATEGORI" "Kategori" "x(256)" "character" ? ? ? ? ? ? no ? no no "8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.anvandartemp.FAKTURAREF
"anvandartemp.FAKTURAREF" "Faktura" "x(256)" "character" ? ? ? ? ? ? no ? no no "8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_ANV */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_ANVOMR
/* Query rebuild information for BROWSE BRW_ANVOMR
     _TblList          = "Temp-Tables.omranvandartemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.omranvandartemp.ANVANDARE
"omranvandartemp.ANVANDARE" "Användare" "x(256)" "character" ? ? ? ? ? ? no ? no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.omranvandartemp.AV-NAMN
"omranvandartemp.AV-NAMN" "Användarnamn" "x(256)" "character" ? ? ? ? ? ? no ? no no "34.13" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.omranvandartemp.PERSONALKOD
"omranvandartemp.PERSONALKOD" "Enhet/!Sign" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_ANVOMR */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_KONTAKT
/* Query rebuild information for BROWSE BRW_KONTAKT
     _TblList          = "Temp-Tables.kontaktperstemp WHERE Temp-Tables.bestkundallt <external> ..."
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.kontaktperstemp.NAMN
"kontaktperstemp.NAMN" "Namn" "X(256)" "character" ? ? ? ? ? ? yes ? no no "17" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.kontaktperstemp.EPOST
"kontaktperstemp.EPOST" "E-post" "X(256)" "character" ? ? ? ? ? ? yes ? no no "20.25" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.kontaktperstemp.TELEFON
"kontaktperstemp.TELEFON" "Telefon" "X(256)" "character" ? ? ? ? ? ? yes ? no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.kontaktperstemp.MOBIL
"kontaktperstemp.MOBIL" "Mobil" "X(256)" "character" ? ? ? ? ? ? yes ? no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.kontaktperstemp.FAX
"kontaktperstemp.FAX" "Fax" "X(256)" "character" ? ? ? ? ? ? yes ? no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_KONTAKT */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_PRI
/* Query rebuild information for BROWSE BRW_PRI
     _TblList          = "Temp-Tables.priotemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.priotemp.PKOD
"priotemp.PKOD" "Prioritet" ">>9" "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.priotemp.P-BENAMNING
"priotemp.P-BENAMNING" "Benämning" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.priotemp.AKTIV
"priotemp.AKTIV" "Aktiv" ? "logical" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE BRW_PRI */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_PRIOMR
/* Query rebuild information for BROWSE BRW_PRIOMR
     _TblList          = "Temp-Tables.prioomrtemp"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > Temp-Tables.prioomrtemp.PKOD
"prioomrtemp.PKOD" "Prioritet" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.prioomrtemp.P-BENAMNING
"prioomrtemp.P-BENAMNING" "Benämning" ? "character" ? ? ? ? ? ? no ? no no "11.13" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.prioomrtemp.AKTIV
"prioomrtemp.AKTIV" "Aktiv" ? "logical" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE BRW_PRIOMR */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-GRUND
/* Query rebuild information for FRAME FRAME-GRUND
     _TblList          = "Temp-Tables.omrallt"
     _Query            is NOT OPENED
*/  /* FRAME FRAME-GRUND */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* <insert window title> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
    musz = TRUE.
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


&Scoped-define BROWSE-NAME BRW_KONTAKT
&Scoped-define FRAME-NAME FRAME-GRUND
&Scoped-define SELF-NAME BRW_KONTAKT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_KONTAKT C-Win
ON MOUSE-SELECT-DBLCLICK OF BRW_KONTAKT IN FRAME FRAME-GRUND /* Alla kontaktpersoner */
DO:
   /*
   IF AVAILABLE kontaktperstemp THEN DO:
      ASSIGN
      FILL-IN-EPOST          = kontaktperstemp.EPOST   
      bestkundallt.FAXNR     =  kontaktperstemp.MOBIL   
      bestkundallt.KONTAKT   =  kontaktperstemp.NAMN    
      bestkundallt.TEL        = kontaktperstemp.TELEFON. 
      DISPLAY FILL-IN-EPOST bestkundallt.FAXNR  bestkundallt.KONTAKT bestkundallt.TEL WITH FRAME FRAME-GRUND.   
   END.
   */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_KONTAKT C-Win
ON ROW-LEAVE OF BRW_KONTAKT IN FRAME FRAME-GRUND /* Alla kontaktpersoner */
DO:
   status-ok = BRW_KONTAKT:SELECT-FOCUSED-ROW() IN FRAME FRAME-GRUND NO-ERROR.
   IF AVAILABLE kontaktperstemp THEN DO:
      DISPLAY  
      kontaktperstemp.NAMN      
      kontaktperstemp.EPOST   
      kontaktperstemp.TELEFON 
      kontaktperstemp.MOBIL   
      kontaktperstemp.FAX
      WITH BROWSE BRW_KONTAKT.
      kontaktperstemp.NAMN    = INPUT BROWSE BRW_KONTAKT kontaktperstemp.NAMN   .
      kontaktperstemp.EPOST   = INPUT BROWSE BRW_KONTAKT kontaktperstemp.EPOST  .
      kontaktperstemp.TELEFON = INPUT BROWSE BRW_KONTAKT kontaktperstemp.TELEFON.
      kontaktperstemp.MOBIL   = INPUT BROWSE BRW_KONTAKT kontaktperstemp.MOBIL  .
      kontaktperstemp.FAX   = INPUT BROWSE BRW_KONTAKT kontaktperstemp.FAX  .
      DISPLAY  kontaktperstemp.EPOST WITH BROWSE BRW_KONTAKT.
   END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_KONTAKT C-Win
ON VALUE-CHANGED OF BRW_KONTAKT IN FRAME FRAME-GRUND /* Alla kontaktpersoner */
DO:
   status-ok = BRW_KONTAKT:SELECT-FOCUSED-ROW() IN FRAME FRAME-GRUND NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME kontaktperstemp.NAMN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL kontaktperstemp.NAMN BRW_KONTAKT _BROWSE-COLUMN C-Win
ON ENTRY OF kontaktperstemp.NAMN IN BROWSE BRW_KONTAKT /* Namn */
DO:
   entryepost = TRUE.
   IF AVAILABLE kontaktperstemp THEN DISPLAY kontaktperstemp.NAMN kontaktperstemp.EPOST kontaktperstemp.TELEFON
   kontaktperstemp.MOBIL kontaktperstemp.FAX WITH BROWSE BRW_KONTAKT. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL kontaktperstemp.NAMN BRW_KONTAKT _BROWSE-COLUMN C-Win
ON LEAVE OF kontaktperstemp.NAMN IN BROWSE BRW_KONTAKT /* Namn */
DO:
   IF AVAILABLE kontaktperstemp THEN DO:
      IF kontaktperstemp.NAMN NE INPUT BROWSE BRW_KONTAKT kontaktperstemp.NAMN THEN DO:
         kontaktperstemp.NAMN = INPUT BROWSE BRW_KONTAKT kontaktperstemp.NAMN.   
         DISPLAY  kontaktperstemp.NAMN WITH BROWSE BRW_KONTAKT.         
      END.
      ELSE /*kontaktperstemp.NAMN:READ-ONLY IN BROWSE BRW_KONTAKT = TRUE.*/
   END.   
   entryepost = FALSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME kontaktperstemp.EPOST
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL kontaktperstemp.EPOST BRW_KONTAKT _BROWSE-COLUMN C-Win
ON ENTRY OF kontaktperstemp.EPOST IN BROWSE BRW_KONTAKT /* E-post */
DO:
   entryepost = TRUE.
   IF AVAILABLE kontaktperstemp THEN DISPLAY kontaktperstemp.NAMN kontaktperstemp.EPOST kontaktperstemp.TELEFON
   kontaktperstemp.MOBIL kontaktperstemp.FAX WITH BROWSE BRW_KONTAKT.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL kontaktperstemp.EPOST BRW_KONTAKT _BROWSE-COLUMN C-Win
ON LEAVE OF kontaktperstemp.EPOST IN BROWSE BRW_KONTAKT /* E-post */
DO:
   IF AVAILABLE kontaktperstemp THEN DO:
      IF kontaktperstemp.EPOST NE INPUT BROWSE BRW_KONTAKT kontaktperstemp.EPOST THEN DO:
         kontaktperstemp.EPOST = INPUT BROWSE BRW_KONTAKT kontaktperstemp.EPOST.   
         DISPLAY  kontaktperstemp.EPOST WITH BROWSE BRW_KONTAKT.
         IF kontaktperstemp.EPOST NE "" THEN DO:
            RUN EPOSTKOLL.P (INPUT kontaktperstemp.EPOST,OUTPUT musz).
            IF musz = FALSE THEN DO:
               APPLY "ENTRY" TO kontaktperstemp.EPOST IN BROWSE BRW_KONTAKT.
               RETURN NO-APPLY.               
            END.               
         END.
         musz = FALSE.
      END.
      ELSE /*kontaktperstemp.EPOST:READ-ONLY IN BROWSE BRW_KONTAKT = TRUE.*/
   END.   
   entryepost = FALSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME kontaktperstemp.TELEFON
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL kontaktperstemp.TELEFON BRW_KONTAKT _BROWSE-COLUMN C-Win
ON ENTRY OF kontaktperstemp.TELEFON IN BROWSE BRW_KONTAKT /* Telefon */
DO:
   entryepost = TRUE.
   IF AVAILABLE kontaktperstemp THEN DISPLAY kontaktperstemp.NAMN kontaktperstemp.EPOST kontaktperstemp.TELEFON
   kontaktperstemp.MOBIL kontaktperstemp.FAX WITH BROWSE BRW_KONTAKT.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL kontaktperstemp.TELEFON BRW_KONTAKT _BROWSE-COLUMN C-Win
ON LEAVE OF kontaktperstemp.TELEFON IN BROWSE BRW_KONTAKT /* Telefon */
DO:
   IF AVAILABLE kontaktperstemp THEN DO:
      IF kontaktperstemp.TELEFON NE INPUT BROWSE BRW_KONTAKT kontaktperstemp.TELEFON THEN DO:
         kontaktperstemp.TELEFON = INPUT BROWSE BRW_KONTAKT kontaktperstemp.TELEFON.            
         kontaktperstemp.TELEFON = tabortblanka(kontaktperstemp.TELEFON ).   
         DISPLAY  kontaktperstemp.TELEFON WITH BROWSE BRW_KONTAKT.         
      END.
      ELSE /*kontaktperstemp.TELEFON:READ-ONLY IN BROWSE BRW_KONTAKT = TRUE.*/
   END.   
   entryepost = FALSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME kontaktperstemp.MOBIL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL kontaktperstemp.MOBIL BRW_KONTAKT _BROWSE-COLUMN C-Win
ON ENTRY OF kontaktperstemp.MOBIL IN BROWSE BRW_KONTAKT /* Mobil */
DO:
   entryepost = TRUE.
   IF AVAILABLE kontaktperstemp THEN DISPLAY kontaktperstemp.NAMN kontaktperstemp.EPOST kontaktperstemp.TELEFON
   kontaktperstemp.MOBIL kontaktperstemp.FAX WITH BROWSE BRW_KONTAKT.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL kontaktperstemp.MOBIL BRW_KONTAKT _BROWSE-COLUMN C-Win
ON LEAVE OF kontaktperstemp.MOBIL IN BROWSE BRW_KONTAKT /* Mobil */
DO:
   IF AVAILABLE kontaktperstemp THEN DO:
      IF kontaktperstemp.MOBIL NE INPUT BROWSE BRW_KONTAKT kontaktperstemp.MOBIL THEN DO:
         kontaktperstemp.MOBIL = INPUT BROWSE BRW_KONTAKT kontaktperstemp.MOBIL.   
         kontaktperstemp.MOBIL = tabortblanka(kontaktperstemp.MOBIL). 
         DISPLAY  kontaktperstemp.MOBIL WITH BROWSE BRW_KONTAKT.         
      END.
      ELSE /*kontaktperstemp.MOBIL:READ-ONLY IN BROWSE BRW_KONTAKT = TRUE.*/
   END.   
   entryepost = FALSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME kontaktperstemp.FAX
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL kontaktperstemp.FAX BRW_KONTAKT _BROWSE-COLUMN C-Win
ON ENTRY OF kontaktperstemp.FAX IN BROWSE BRW_KONTAKT /* Fax */
DO:
   entryepost = TRUE.
   IF AVAILABLE kontaktperstemp THEN DISPLAY kontaktperstemp.NAMN kontaktperstemp.EPOST kontaktperstemp.TELEFON
   kontaktperstemp.MOBIL kontaktperstemp.FAX WITH BROWSE BRW_KONTAKT.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL kontaktperstemp.FAX BRW_KONTAKT _BROWSE-COLUMN C-Win
ON LEAVE OF kontaktperstemp.FAX IN BROWSE BRW_KONTAKT /* Fax */
DO:
   IF AVAILABLE kontaktperstemp THEN DO:
      IF kontaktperstemp.FAX NE INPUT BROWSE BRW_KONTAKT kontaktperstemp.FAX THEN DO:
         kontaktperstemp.FAX = INPUT BROWSE BRW_KONTAKT kontaktperstemp.FAX.
         kontaktperstemp.FAX = tabortblanka(kontaktperstemp.FAX).    
         DISPLAY  kontaktperstemp.FAX WITH BROWSE BRW_KONTAKT.         
      END.
      ELSE /*kontaktperstemp.NAMN:READ-ONLY IN BROWSE BRW_KONTAKT = TRUE.*/
   END.   
   entryepost = FALSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_PRI
&Scoped-define FRAME-NAME FRAME-PRIO
&Scoped-define SELF-NAME BRW_PRI
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_PRI C-Win
ON MOUSE-SELECT-DBLCLICK OF BRW_PRI IN FRAME FRAME-PRIO /* Prioritet */
DO:
   run andra.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-GRUND
&Scoped-define SELF-NAME BTN_ANDRA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_ANDRA C-Win
ON CHOOSE OF BTN_ANDRA IN FRAME FRAME-GRUND /* Ändra */
DO:
   IF AVAILABLE kontaktperstemp THEN DO:
      ASSIGN
      kontaktperstemp.NAMN:READ-ONLY IN BROWSE BRW_KONTAKT = FALSE
      kontaktperstemp.EPOST:READ-ONLY IN BROWSE BRW_KONTAKT = FALSE
      kontaktperstemp.TELEFON:READ-ONLY IN BROWSE BRW_KONTAKT = FALSE
      kontaktperstemp.MOBIL:READ-ONLY IN BROWSE BRW_KONTAKT = FALSE.
      APPLY "ENTRY" TO kontaktperstemp.NAMN IN BROWSE BRW_KONTAKT. 
      DISPLAY BRW_KONTAKT WITH FRAME FRAME-GRUND.  
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME BTN_AVBRYT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVBRYT C-Win
ON CHOOSE OF BTN_AVBRYT IN FRAME DEFAULT-FRAME /* Avbryt */
DO:
   musz = TRUE.
   {BORTBRWPROC.I}
   
   IF VALID-HANDLE(anvapph) THEN DO:
      RUN avs_UI IN anvapph.
      DELETE PROCEDURE anvapph NO-ERROR.
   END.   
   IF VALID-HANDLE(priapph) THEN DELETE PROCEDURE priapph.
   
   APPLY "END-ERROR":U TO FRAME {&FRAME-NAME}.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-GRUND
&Scoped-define SELF-NAME BTN_BORT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_BORT C-Win
ON CHOOSE OF BTN_BORT IN FRAME FRAME-GRUND /* Ta bort */
DO:
   /*status-ok = BRW_KONTAKT:SELECT-FOCUSED-ROW() IN FRAME FRAME-GRUND NO-ERROR.*/
   BRW_KONTAKT:SELECT-FOCUSED-ROW() IN FRAME FRAME-GRUND NO-ERROR.
   IF AVAILABLE kontaktperstemp THEN DO:
      MESSAGE "Vill du ta bort " kontaktperstemp.NAMN "?"
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Meddelande" UPDATE svar AS LOGICAL.
      IF svar = FALSE THEN RETURN NO-APPLY.
      DELETE kontaktperstemp.
      RUN selnextprevrow_UI IN brwproc[1].
      RUN openbdyn_UI IN brwproc[1] (INPUT "").
      RUN lastselectdyn_UI IN brwproc[1].
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NY
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NY C-Win
ON CHOOSE OF BTN_NY IN FRAME FRAME-GRUND /* Ny */
DO:
   CREATE kontaktperstemp.
   /*
   ASSIGN eptemp.EPOST = FILL-IN_MOTTAGARE.
   */
   RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(kontaktperstemp)).            
   RUN openbdyn_UI IN brwproc[1] (INPUT "").
   RUN lastselectdyn_UI IN brwproc[1].
   ASSIGN
   kontaktperstemp.NAMN:READ-ONLY IN BROWSE BRW_KONTAKT = FALSE   
   kontaktperstemp.EPOST:READ-ONLY IN BROWSE BRW_KONTAKT = FALSE  
   kontaktperstemp.TELEFON:READ-ONLY IN BROWSE BRW_KONTAKT = FALSE
   kontaktperstemp.MOBIL:READ-ONLY IN BROWSE BRW_KONTAKT = FALSE. 
   APPLY "ENTRY" TO kontaktperstemp.NAMN IN BROWSE BRW_KONTAKT. 
   DISPLAY BRW_KONTAKT WITH FRAME FRAME-GRUND.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME BTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK C-Win
ON CHOOSE OF BTN_OK IN FRAME DEFAULT-FRAME /* Ok */
DO:  
   {muswait.i}
   ASSIGN
   omrallt.OMRADE       = INPUT FRAME FRAME-GRUND omrallt.OMRADE    
   omrallt.NAMN         = INPUT omrallt.NAMN      
   omrallt.PROJINT1     = INPUT omrallt.PROJINT1  
   omrallt.PROJINT2     = INPUT omrallt.PROJINT2  
   omrallt.SISTPROJ     = INPUT omrallt.SISTPROJ  
   omrallt.KALKYLINT1   = INPUT omrallt.KALKYLINT1
   omrallt.KALKYLINT2   = INPUT omrallt.KALKYLINT2
   omrallt.KALKYLSIST   = INPUT omrallt.KALKYLSIST
   omrallt.PLANINT1     = INPUT omrallt.PLANINT1  
   omrallt.PLANINT2     = INPUT omrallt.PLANINT2  
   omrallt.SISTPLAN     = INPUT omrallt.SISTPLAN  
   omrallt.VARDINT1     = INPUT omrallt.VARDINT1  
   omrallt.VARDINT2     = INPUT omrallt.VARDINT2  
   omrallt.VARDSIST     = INPUT omrallt.VARDSIST  
   omrallt.ORGIDNUM     = INPUT omrallt.ORGIDNUM  
   omrallt.KONTOOB      = INPUT omrallt.KONTOOB   
   omrallt.OMKMTR       = INPUT omrallt.OMKMTR    
   omrallt.OMKOSTP      = INPUT omrallt.OMKOSTP   
   /*omrallt.KUNDPRISF    = INPUT omrallt.KUNDPRISF */
   omrallt.GATUADR      = INPUT omrallt.GATUADR   
   omrallt.POSTNR       = INPUT omrallt.POSTNR    
   omrallt.POSTANST     = INPUT omrallt.POSTANST  
   omrallt.TELVXL       = INPUT omrallt.TELVXL    
   omrallt.TELJOUR      = INPUT omrallt.TELJOUR.   
   vem = omrallt.OMRADE.
   vemanvandare = omrallt.OMRADE.
   RAD_GRUND = 1.
   APPLY "VALUE-CHANGED" TO RAD_GRUND IN FRAME DEFAULT-FRAME.
   EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
   EMPTY TEMP-TABLE extradatatemp NO-ERROR.
   CREATE inextradatatemp.          
   ASSIGN
   inextradatatemp.PROGRAM = "BESTEPOST2"                   
   inextradatatemp.HUVUDCH = vemanvandare.
   RUN exbort_UI IN edataapph (INPUT TABLE inextradatatemp).
   EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
   EMPTY TEMP-TABLE extradatatemp NO-ERROR.
   FOR EACH kontaktperstemp:
      CREATE inextradatatemp.          
      ASSIGN
      inextradatatemp.PROGRAM = "BESTEPOST2"                   
      inextradatatemp.HUVUDCH = vemanvandare
      inextradatatemp.SOKCHAR[1]  =  kontaktperstemp.NAMN   
      inextradatatemp.SOKCHAR[2]  =  kontaktperstemp.EPOST  
      inextradatatemp.SOKCHAR[3]  =  kontaktperstemp.TELEFON
      inextradatatemp.SOKCHAR[4]  =  kontaktperstemp.MOBIL
      inextradatatemp.SOKCHAR[5]  =  kontaktperstemp.FAX.        
   END.  
   RUN sparaextra_UI IN edataapph (INPUT TABLE inextradatatemp). 
   EMPTY TEMP-TABLE extradatatemp NO-ERROR. 
   EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
   
   
   RUN ok_UI.
   IF musz = TRUE THEN DO:
      musz = FALSE.
      RETURN NO-APPLY.
   END.
   APPLY "CLOSE":U TO THIS-PROCEDURE.


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-GRUND
&Scoped-define SELF-NAME CMB_AVD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_AVD C-Win
ON LEAVE OF CMB_AVD IN FRAME FRAME-GRUND /* Avdelning */
DO:
   CMB_AVD = INPUT CMB_AVD.
   IF vem = ? AND CMB_AVD = '' THEN DO:
      MESSAGE "Fältet får inte vara blankt.".
      RETURN NO-APPLY.
   END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_AVD C-Win
ON VALUE-CHANGED OF CMB_AVD IN FRAME FRAME-GRUND /* Avdelning */
DO:
   ASSIGN CMB_AVD = INPUT CMB_AVD.
   FIND FIRST juromravd WHERE juromravd.TYP = "AVD" AND juromravd.NAMN = CMB_AVD 
   NO-LOCK NO-ERROR.
   ASSIGN omrallt.AVDELNINGNR = juromravd.AVDELNINGNR.    
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME omrallt.KONTOOB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL omrallt.KONTOOB C-Win
ON MOUSE-SELECT-CLICK OF omrallt.KONTOOB IN FRAME FRAME-GRUND /* Obliga.konto */
DO:                                                                           
   IF INPUT omrallt.KONTOOB = FALSE THEN omrallt.KONTOOB = TRUE. 
   IF INPUT omrallt.KONTOOB = TRUE THEN omrallt.KONTOOB = FALSE.
   DISPLAY omrallt.KONTOOB WITH FRAME {&FRAME-NAME}.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME omrallt.OMRADE
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL omrallt.OMRADE C-Win
ON LEAVE OF omrallt.OMRADE IN FRAME FRAME-GRUND /* Område */
DO:
   FIND FIRST juromravd WHERE juromravd.TYP = "OMR" AND juromravd.ID = INPUT omrallt.OMRADE 
   NO-LOCK NO-ERROR.
   IF AVAILABLE juromravd THEN DO:
      MESSAGE "Det finns redan ett " + LC(Guru.Konstanter:gomrk) + " med denna beteckning." 
      VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
   {SOKSTART.I}
   ASSIGN
   soktemp.SOKVAL = 74
   soktemp.SOKCHAR[1] = INPUT omrallt.OMRADE. 
   {SOKANROP.I}
   IF soktemp.SOKINT[1] = 1 THEN DO:
      MESSAGE "Det finns redan ett " + LC(Guru.Konstanter:gomrk) + " med denna beteckning." 
      VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
   {SOKSTART.I}
   ASSIGN
   soktemp.SOKVAL = 73
   soktemp.SOKCHAR[1] = INPUT omrallt.OMRADE. 
   {SOKANROP.I}
   IF soktemp.SOKINT[1] = 1 THEN DO:
      MESSAGE Guru.Konstanter:gomrl + "/organisation får inte ha samma nr som någon " Guru.Konstanter:gbestk VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.         
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME RAD_GRUND
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RAD_GRUND C-Win
ON VALUE-CHANGED OF RAD_GRUND IN FRAME DEFAULT-FRAME
DO:
   RAD_GRUND = INPUT RAD_GRUND.
   IF RAD_GRUND = 1 THEN DO:
      ASSIGN
      FRAME FRAME-GRUND:HIDDEN = FALSE      
      FRAME FRAME-ANV:HIDDEN = TRUE
      FRAME FRAME-PRIO:HIDDEN = TRUE.
   END.
   IF RAD_GRUND = 2 THEN DO:
      ASSIGN
      FRAME FRAME-GRUND:HIDDEN = TRUE
      FRAME FRAME-ANV:HIDDEN = FALSE
      FRAME FRAME-PRIO:HIDDEN = TRUE.
   END.
   IF RAD_GRUND = 3 THEN DO:
      ASSIGN
      FRAME FRAME-GRUND:HIDDEN = TRUE
      FRAME FRAME-ANV:HIDDEN = TRUE
      FRAME FRAME-PRIO:HIDDEN = FALSE.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-GRUND
&Scoped-define SELF-NAME omrallt.TELJOUR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL omrallt.TELJOUR C-Win
ON LEAVE OF omrallt.TELJOUR IN FRAME FRAME-GRUND /* Journummer */
DO:
   ASSIGN omrallt.TELJOUR = INPUT omrallt.TELJOUR . 
   omrallt.TELJOUR = tabortblanka(omrallt.TELJOUR ).
   DISPLAY omrallt.TELJOUR WITH FRAME FRAME-GRUND. 

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME omrallt.TELVXL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL omrallt.TELVXL C-Win
ON LEAVE OF omrallt.TELVXL IN FRAME FRAME-GRUND /* Telefonnr */
DO:
   ASSIGN omrallt.TELVXL = INPUT omrallt.TELVXL. 
   omrallt.TELVXL = tabortblanka(omrallt.TELVXL).
   DISPLAY omrallt.TELVXL WITH FRAME FRAME-GRUND. 

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BRW_ANV
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE DO: 
   {BORTBRWPROC.I}
   IF VALID-HANDLE(anvapph) THEN DO:
      RUN avs_UI IN anvapph.
      DELETE PROCEDURE anvapph NO-ERROR.
   END.
   IF VALID-HANDLE(priapph) THEN DELETE PROCEDURE priapph.
   
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
   {ALLSTARTDYN.I}
   ASSIGN 
   omrallt.OMRADE:FORMAT = "x(8)"
   omrallt.OMRADE:LABEL = Guru.Konstanter:gomrk
   omrallt.PROJINT1:LABEL = Guru.Konstanter:gaok + "intervall" 
   omrallt.PLANINT1:LABEL = Guru.Konstanter:gplk + "intervall".
   status-ok = RAD_GRUND:DELETE("Områdes ansvariga").
   RAD_GRUND = 1.
      
   &SCOPED-DEFINE FRAME-NAME FRAME-GRUND 
   &Scoped-define FORMATNAMN omrallt.PROJINT1   
   {AOFORMAT3.I}
   &Scoped-define FORMATNAMN omrallt.PROJINT2   
   {AOFORMAT3.I}
   &Scoped-define FORMATNAMN omrallt.SISTPROJ
   {AOFORMAT3.I}
   CMB_AVD:LABEL = Guru.Konstanter:gavdl.
   ASSIGN C-Win:TITLE = Guru.Konstanter:gomrl + "/Organisation".
   RUN orghmten_UI IN orgapph (INPUT vem,OUTPUT TABLE omrallt).                 
   FIND FIRST omrallt NO-ERROR.
   IF Guru.Konstanter:varforetypchar[4] NE "" THEN DO:
      RUN omrhamta_UI IN anvapph (INPUT omrallt.OMRADE,OUTPUT TABLE omranvandartemp).
   END.
   FOR EACH juromravd WHERE juromravd.TYP = "AVD" NO-LOCK:      
      stat = CMB_AVD:ADD-LAST(juromravd.NAMN).
   END.  
   FIND FIRST juromravd WHERE juromravd.TYP = "AVD" AND juromravd.AVDELNINGNR = omrallt.AVDELNINGNR
   USE-INDEX AVDELNINGNR NO-LOCK NO-ERROR.
   IF AVAILABLE juromravd THEN DO:
      CMB_AVD:SCREEN-VALUE = juromravd.NAMN.            
   END.  
   IF vem = ? THEN DO:
      ASSIGN FRAME {&FRAME-NAME}:TITLE = "Ny organisation".
      FIND FIRST juromravd WHERE juromravd.TYP = "AVD" USE-INDEX AVDELNINGNR NO-LOCK NO-ERROR.
      IF AVAILABLE juromravd THEN DO:
         ASSIGN CMB_AVD:SCREEN-VALUE = juromravd.NAMN.
      END.        
   END.
   ELSE DO:
      ASSIGN FRAME {&FRAME-NAME}:TITLE = "Ändra organisation -" + omrallt.NAMN.
   END.
   IF Guru.Konstanter:varforetypval[45] =  0 THEN DO:
      RAD_GRUND:ADD-LAST(Guru.Konstanter:gomrl + " ansvariga", 2).
   END.   
   IF Guru.Konstanter:varforetypval[45] = 1 THEN DO:
      RAD_GRUND = 1.
      RAD_GRUND:ADD-LAST( Guru.Konstanter:gpriol + " för " + Guru.Konstanter:gomrl , 3).
      BRW_PRI:TITLE = Guru.Konstanter:gpriok.
      priotemp.PKOD:LABEL IN BROWSE BRW_PRI = Guru.Konstanter:gpriok.
      RUN prihmt2_UI IN priapph (OUTPUT TABLE priotemp).
      FIND FIRST omrallt NO-ERROR.      
      RUN priomrhmt_UI IN priapph (INPUT omrallt.OMRADE,INPUT TABLE priotemp,OUTPUT TABLE prioomrtemp).     
      FOR EACH priotemp WHERE priotemp.AKTIV = FALSE:
         DELETE priotemp.
      END.             
      RUN openbdyn_UI IN brwproc[5] (INPUT "").
      BRW_PRIOMR:TITLE = Guru.Konstanter:gpriok + " för " + Guru.Konstanter:gomrl + " " + omrallt.OMRADE.
      prioomrtemp.PKOD:LABEL IN BROWSE BRW_PRIOMR = Guru.Konstanter:gpriok.
      RUN openbdyn_UI IN brwproc[6] (INPUT ""). 
   END.        
   IF Guru.Konstanter:varforetypval[1] = 2 THEN DO:
      omrallt.OMKMTR:LABEL = "Mtrl. påslag i %". 
      omrallt.OMKOSTP:LABEL = "Frm.tj påslag i %".  
      omrallt.OMKMTR:FORMAT = ">>9". 
      omrallt.OMKOSTP:FORMAT = ">>9".        
   END.
   ELSE DO:
      musz = musz.      
   END.     
   IF vem = ? AND  Guru.Konstanter:varforetypval[2] = 0 THEN DO:
      MESSAGE "För nytt " Guru.Konstanter:gomrl " meddela debet- och kredit-konto för ekonomikörningar till Elpool 090-184540!" VIEW-AS ALERT-BOX.
   END.
   RUN enable_UI.
   APPLY "VALUE-CHANGED" TO RAD_GRUND IN FRAME DEFAULT-FRAME.
   IF Guru.Konstanter:varforetypchar[4] = "" THEN RAD_GRUND:HIDDEN IN FRAME DEFAULT-FRAME = TRUE. 
   IF Guru.Konstanter:varforetypval[45] = 1 THEN RAD_GRUND:HIDDEN IN FRAME DEFAULT-FRAME = FALSE.
   {FRMSIZEF.I}  
   IF vem NE ? THEN DO:     
      DISABLE omrallt.OMRADE WITH FRAME {&FRAME-NAME}.
   END.
   /*PLANFOR*/
   IF Guru.Konstanter:varforetypval[7] = 1 THEN DO: 
      ASSIGN
      omrallt.PLANINT1:HIDDEN = FALSE
      omrallt.PLANINT2:HIDDEN = FALSE
      omrallt.SISTPLAN:HIDDEN = FALSE.
   END.
   ELSE DO:
      ASSIGN
      omrallt.PLANINT1:HIDDEN = TRUE
      omrallt.PLANINT2:HIDDEN = TRUE
      omrallt.SISTPLAN:HIDDEN = TRUE.
   END.     
   
   



   /* NYA GREJOR */
   
   IF Guru.Konstanter:appcon THEN DO:
      RUN EXTRADATAHMT.P PERSISTENT SET edataapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT.
      IF Guru.Konstanter:varforetypval[27] = 0 THEN RUN BESTKUNDAPP.P PERSISTENT SET bestapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN EXTRADATAHMT.P PERSISTENT SET edataapph.      
      IF Guru.Konstanter:varforetypval[27] = 0 THEN RUN BESTKUNDAPP.P PERSISTENT SET bestapph.
   END.

     
   EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
   EMPTY TEMP-TABLE extradatatemp NO-ERROR.
   IF vemanvandare = ? THEN.
   ELSE DO:
      CREATE inextradatatemp.          
      ASSIGN
      inextradatatemp.PROGRAM = "BESTEPOST2"                   
      /*inextradatatemp.HUVUDCH = bestkundallt.BESTID.*/
      inextradatatemp.HUVUDCH = vemanvandare. /*input från orgreg.w */
      RUN etabhamt_UI IN edataapph (INPUT TABLE inextradatatemp, OUTPUT TABLE extradatatemp). 
      FOR EACH extradatatemp:
         CREATE kontaktperstemp.
         ASSIGN
         kontaktperstemp.NAMN    = extradatatemp.SOKCHAR[1]
         kontaktperstemp.EPOST   = extradatatemp.SOKCHAR[2]
         kontaktperstemp.TELEFON = extradatatemp.SOKCHAR[3]
         kontaktperstemp.MOBIL   = extradatatemp.SOKCHAR[4]
         kontaktperstemp.FAX   = extradatatemp.SOKCHAR[5].
      END.
   END.
   EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
   EMPTY TEMP-TABLE extradatatemp NO-ERROR.       
  
   RUN openbdyn_UI IN brwproc[1] (INPUT "").
   RUN lastselectdyn_UI IN brwproc[1].
   RUN openbdyn_UI IN brwproc[2] (INPUT "").
   RUN openbdyn_UI IN brwproc[3] (INPUT "").
   /*ccc*/
   IF Guru.Konstanter:globanv = CHR(69) + CHR(76) + CHR(80) + CHR(65) + CHR(79)  THEN.
   ELSE DO:
     anvandartemp.FAKTURAREF:VISIBLE IN BROWSE BRW_ANV = FALSE .
    
   END.
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
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   RUN DYNBRW.P PERSISTENT SET brwproc[1] (INPUT BRW_KONTAKT:HANDLE IN FRAME FRAME-GRUND).
   RUN DYNBRW.P PERSISTENT SET brwproc[2] (INPUT BRW_ANV:HANDLE IN FRAME FRAME-ANV).
   RUN DYNBRW.P PERSISTENT SET brwproc[3] (INPUT BRW_ANVOMR:HANDLE IN FRAME FRAME-ANV).
   RUN DYNBRW.P PERSISTENT SET brwproc[5] (INPUT BRW_PRI:HANDLE IN FRAME FRAME-PRIO).
   RUN DYNBRW.P PERSISTENT SET brwproc[6] (INPUT BRW_PRIOMR:HANDLE IN FRAME FRAME-PRIO).
   RUN DYNARROW.P PERSISTENT SET brwproc[4] (INPUT BRW_ANV:HANDLE,INPUT BRW_ANVOMR:HANDLE,
                                             INPUT BTN_OVER:HANDLE, INPUT BTN_ALLOVER:HANDLE,
                                             INPUT BTN_ALLBACK:HANDLE, INPUT BTN_BACK:HANDLE).
   RUN DYNARROW.P PERSISTENT SET brwproc[7] (INPUT BRW_PRI:HANDLE,INPUT BRW_PRIOMR:HANDLE,
                                             INPUT BTN_OVER-2:HANDLE, INPUT BTN_ALLOVER-2:HANDLE,
                                             INPUT BTN_ALLBACK-2:HANDLE, INPUT BTN_BACK-2:HANDLE).
                                                          
   IF Guru.Konstanter:varforetypchar[4] NE "" THEN DO:
      IF Guru.Konstanter:appcon THEN DO:
         RUN ANVDYNAPP.P PERSISTENT SET anvapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
      END.
      ELSE DO:
         RUN ANVDYNAPP.P PERSISTENT SET anvapph.      
      END.
      RUN hamta_UI IN anvapph (INPUT TRUE,OUTPUT TABLE anvandartemp). 
   END.
   IF Guru.Konstanter:varforetypval[45] = 1 THEN DO:
      IF Guru.Konstanter:appcon THEN DO:
         RUN PRIAPP.P PERSISTENT SET priapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
      END.
      ELSE DO:
         RUN PRIAPP.P PERSISTENT SET priapph.
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
  DISPLAY RAD_GRUND 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RAD_GRUND BTN_OK BTN_AVBRYT 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  ENABLE BRW_ANV BRW_ANVOMR BTN_ALLOVER BTN_OVER BTN_BACK BTN_ALLBACK 
      WITH FRAME FRAME-ANV IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-ANV}
  DISPLAY CMB_AVD 
      WITH FRAME FRAME-GRUND IN WINDOW C-Win.
  IF AVAILABLE omrallt THEN 
    DISPLAY omrallt.OMRADE omrallt.NAMN omrallt.PROJINT1 omrallt.PROJINT2 
          omrallt.SISTPROJ omrallt.KALKYLINT1 omrallt.KALKYLINT2 
          omrallt.KALKYLSIST omrallt.PLANINT1 omrallt.PLANINT2 omrallt.SISTPLAN 
          omrallt.VARDINT1 omrallt.VARDINT2 omrallt.VARDSIST omrallt.ORGIDNUM 
          omrallt.KONTOOB omrallt.OMKMTR omrallt.OMKOSTP omrallt.GATUADR 
          omrallt.POSTNR omrallt.POSTANST omrallt.TELVXL omrallt.TELJOUR 
      WITH FRAME FRAME-GRUND IN WINDOW C-Win.
  ENABLE omrallt.OMRADE omrallt.NAMN BRW_KONTAKT omrallt.PROJINT1 
         omrallt.PROJINT2 omrallt.SISTPROJ omrallt.KALKYLINT1 
         omrallt.KALKYLINT2 omrallt.KALKYLSIST omrallt.PLANINT1 
         omrallt.PLANINT2 omrallt.SISTPLAN omrallt.VARDINT1 omrallt.VARDINT2 
         omrallt.VARDSIST omrallt.ORGIDNUM omrallt.KONTOOB BTN_NY BTN_ANDRA 
         BTN_BORT omrallt.OMKMTR omrallt.OMKOSTP CMB_AVD omrallt.GATUADR 
         omrallt.POSTNR omrallt.POSTANST omrallt.TELVXL omrallt.TELJOUR 
      WITH FRAME FRAME-GRUND IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-GRUND}
  ENABLE BRW_PRI BRW_PRIOMR BTN_ALLOVER-2 BTN_OVER-2 BTN_BACK-2 BTN_ALLBACK-2 
      WITH FRAME FRAME-PRIO IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-PRIO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ok_UI C-Win 
PROCEDURE ok_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   musz = FALSE.
   ASSIGN
   omrallt.AVDELNINGNAMN = omrallt.OMRADE
   omrallt.PROJINT1 = LEFT-TRIM(omrallt.PROJINT1)
   omrallt.PROJINT2 = LEFT-TRIM(omrallt.PROJINT2)
   omrallt.SISTPROJ = LEFT-TRIM(omrallt.SISTPROJ).
   ASSIGN
   omrallt.PLANINT1 = LEFT-TRIM(omrallt.PLANINT1)
   omrallt.PLANINT2 = LEFT-TRIM(omrallt.PLANINT2)
   omrallt.SISTPLAN = LEFT-TRIM(omrallt.SISTPLAN).
   IF LENGTH(omrallt.PROJINT2) = 1 THEN DO:
      ASSIGN
      omrallt.PROJINT1 = STRING(INTEGER(omrallt.PROJINT1),"9")
      omrallt.SISTPROJ = STRING(INTEGER(omrallt.SISTPROJ),"9").
   END.
   IF LENGTH(omrallt.PROJINT2) = 2 THEN DO:
      ASSIGN
      omrallt.PROJINT1 = STRING(INTEGER(omrallt.PROJINT1),"99")
      omrallt.SISTPROJ = STRING(INTEGER(omrallt.SISTPROJ),"99").
   END.
   IF LENGTH(omrallt.PROJINT2) = 3 THEN DO:
      ASSIGN
      omrallt.PROJINT1 = STRING(INTEGER(omrallt.PROJINT1),"999")
      omrallt.SISTPROJ = STRING(INTEGER(omrallt.SISTPROJ),"999").
   END.
   IF LENGTH(omrallt.PROJINT2) = 4 THEN DO:
      ASSIGN
      omrallt.PROJINT1 = STRING(INTEGER(omrallt.PROJINT1),"9999")
      omrallt.SISTPROJ = STRING(INTEGER(omrallt.SISTPROJ),"9999").
   END.
   IF LENGTH(omrallt.PROJINT2) = 5 THEN DO:
      ASSIGN
      omrallt.PROJINT1 = STRING(INTEGER(omrallt.PROJINT1),"99999")
      omrallt.SISTPROJ = STRING(INTEGER(omrallt.SISTPROJ),"99999").
   END.
   IF LENGTH(omrallt.PROJINT2) = 6 THEN DO:
      ASSIGN
      omrallt.PROJINT1 = STRING(INTEGER(omrallt.PROJINT1),"999999")
      omrallt.SISTPROJ = STRING(INTEGER(omrallt.SISTPROJ),"999999").
   END.
   IF LENGTH(omrallt.PLANINT2) = 1 THEN DO:
      ASSIGN
      omrallt.PLANINT1 = STRING(INTEGER(omrallt.PLANINT1),"9")
      omrallt.SISTPLAN = STRING(INTEGER(omrallt.SISTPLAN),"9").
   END.
   IF LENGTH(omrallt.PLANINT2) = 2 THEN DO:
      ASSIGN
      omrallt.PLANINT1 = STRING(INTEGER(omrallt.PLANINT1),"99")
      omrallt.SISTPLAN = STRING(INTEGER(omrallt.SISTPLAN),"99").
   END.
   IF LENGTH(omrallt.PLANINT2) = 3 THEN DO:
      ASSIGN
      omrallt.PLANINT1 = STRING(INTEGER(omrallt.PLANINT1),"999")
      omrallt.SISTPLAN = STRING(INTEGER(omrallt.SISTPLAN),"999").
   END.
   IF LENGTH(omrallt.PLANINT2) = 4 THEN DO:
      ASSIGN
      omrallt.PLANINT1 = STRING(INTEGER(omrallt.PLANINT1),"9999")
      omrallt.SISTPLAN = STRING(INTEGER(omrallt.SISTPLAN),"9999").
   END.
   IF LENGTH(omrallt.PLANINT2) = 5 THEN DO:
      ASSIGN
      omrallt.PLANINT1 = STRING(INTEGER(omrallt.PLANINT1),"99999")
      omrallt.SISTPLAN = STRING(INTEGER(omrallt.SISTPLAN),"99999").
   END.
   IF LENGTH(omrallt.PLANINT2) = 6 THEN DO:
      ASSIGN
      omrallt.PLANINT1 = STRING(INTEGER(omrallt.PLANINT1),"999999")
      omrallt.SISTPLAN = STRING(INTEGER(omrallt.SISTPLAN),"999999").
   END.
   IF omrallt.OMRADE  = "" THEN DO:
      MESSAGE " Orgnr får inte vara blank".
      /*  status-mus2 = CURRENT-WINDOW:LOAD-MOUSE-POINTER("ARROW").*/
      APPLY "ENTRY" TO omrallt.OMRADE IN FRAME {&FRAME-NAME}.
      musz = TRUE.
      RETURN.
   END.  
      
   CMB_AVD = INPUT CMB_AVD.
   IF vem = ? AND CMB_AVD = '' THEN DO:  
      MESSAGE Guru.Konstanter:gavdl + " får inte vara blank".      
      APPLY "ENTRY" TO CMB_AVD IN FRAME {&FRAME-NAME}.
      musz = TRUE.
      RETURN.
   END.
   FIND FIRST juromravd WHERE juromravd.TYP = "AVD" AND juromravd.NAMN = CMB_AVD
   NO-LOCK NO-ERROR.
   omrallt.AVDELNINGNR = juromravd.AVDELNINGNR.
   IF omrallt.PROJINT1 = "" THEN DO:
      MESSAGE Guru.Konstanter:gaol + "-intervall får inte vara blank".       
      APPLY "ENTRY" TO omrallt.PROJINT1 IN FRAME {&FRAME-NAME}.
      musz = TRUE.
      RETURN.
   END.  
   IF omrallt.PROJINT2 = "" THEN DO:
      MESSAGE Guru.Konstanter:gaol + "-intervall får inte vara blank".     
      APPLY "ENTRY" TO omrallt.PROJINT2 IN FRAME {&FRAME-NAME}.
      musz = TRUE.
      RETURN.
   END.
   IF omrallt.PROJINT1 = omrallt.PROJINT2 THEN musz = musz.
   ELSE DO:
      IF INTEGER(omrallt.SISTPROJ) < INTEGER(omrallt.PROJINT1) THEN DO:
         MESSAGE "Aktuellt " + LC(Guru.Konstanter:gaok) + " måste ligga inom intervallet.".      
         APPLY "ENTRY" TO omrallt.SISTPROJ IN FRAME {&FRAME-NAME}.
         musz = TRUE.
         RETURN.
      END.    
      IF INTEGER(omrallt.SISTPROJ) > INTEGER(omrallt.PROJINT2) THEN DO:
         MESSAGE "Aktuellt " + LC(Guru.Konstanter:gaok) + " måste ligga inom intervallet.".      
         APPLY "ENTRY" TO omrallt.SISTPROJ IN FRAME {&FRAME-NAME}.
         musz = TRUE.
         RETURN.
      END.              
      IF INTEGER(omrallt.PROJINT2) / INTEGER(omrallt.PROJINT1) >= 10 THEN DO:
         MESSAGE "Seriens start och slutvärde skall vara inom samma 10-potens.".
         APPLY "ENTRY" TO omrallt.PROJINT1 IN FRAME {&FRAME-NAME}.
         musz = TRUE.
         RETURN.
      END.
   END.
   IF omrallt.KALKYLINT1 = 0 THEN DO:
      MESSAGE " Kalkylnr-intervall får inte vara blank".      
      APPLY "ENTRY" TO omrallt.KALKYLINT1 IN FRAME {&FRAME-NAME}.
      musz = TRUE.
      RETURN.
   END.    
   IF omrallt.KALKYLINT2 = 0 THEN DO:
      MESSAGE " Kalkylnr-intervall får inte vara blank".      
      APPLY "ENTRY" TO omrallt.KALKYLINT2 IN FRAME {&FRAME-NAME}.
      musz = TRUE.
      RETURN.
   END.   
   IF omrallt.KALKYLINT1 = omrallt.KALKYLINT2 THEN musz = musz.
   ELSE DO:
      IF omrallt.KALKYLSIST < omrallt.KALKYLINT1 THEN DO:
         MESSAGE "Aktuellt kalkylnr måste ligga inom intervallet.".      
         APPLY "ENTRY" TO omrallt.KALKYLSIST IN FRAME {&FRAME-NAME}.
         musz = TRUE.
         RETURN.
      END.    
      IF omrallt.KALKYLSIST > omrallt.KALKYLINT2 THEN DO:
         MESSAGE "Aktuellt kalkynr måste ligga inom intervallet.".     
         APPLY "ENTRY" TO omrallt.KALKYLSIST IN FRAME {&FRAME-NAME}. 
         musz = TRUE.
         RETURN.
      END.
   END.  
   /*PLANFOR*/
   IF Guru.Konstanter:varforetypval[7] = 1 THEN DO: 
      IF omrallt.PLANINT1 = "" THEN DO:
         MESSAGE " " + Guru.Konstanter:gplk + "-intervall får inte vara blank".       
         APPLY "ENTRY" TO omrallt.PLANINT1 IN FRAME {&FRAME-NAME}.
         musz = TRUE.
         RETURN.
      END.  
      IF omrallt.PLANINT2 = "" THEN DO:
         MESSAGE " " + Guru.Konstanter:gplk + "-intervall får inte vara blank".     
         APPLY "ENTRY" TO omrallt.PLANINT2 IN FRAME {&FRAME-NAME}.
         musz = TRUE.
         RETURN.
      END.
      IF omrallt.PLANINT1 = omrallt.PLANINT2 THEN musz = musz.
      ELSE DO:
         IF INTEGER(omrallt.SISTPLAN) < INTEGER(omrallt.PLANINT1) THEN DO:
            MESSAGE "Aktuellt " + LC(Guru.Konstanter:gpll) + " måste ligga inom intervallet.".      
            APPLY "ENTRY" TO omrallt.SISTPLAN IN FRAME {&FRAME-NAME}.
            musz = TRUE.
            RETURN.
         END.    
         IF INTEGER(omrallt.SISTPLAN) > INTEGER(omrallt.PLANINT2) THEN DO:
            MESSAGE "Aktuellt " + LC(Guru.Konstanter:gpll) + " måste ligga inom intervallet.".      
            APPLY "ENTRY" TO omrallt.SISTPLAN IN FRAME {&FRAME-NAME}.
            musz = TRUE.
            RETURN.
         END.
         IF INTEGER(omrallt.PLANINT2) / INTEGER(omrallt.PLANINT1) >= 10 THEN DO:
            MESSAGE "Seriens start och slutvärde skall vara inom samma 10-potens.".
            APPLY "ENTRY" TO omrallt.PLANINT1 IN FRAME {&FRAME-NAME}.
            musz = TRUE.
            RETURN.
         END.
      END.
   END.      
   IF omrallt.VARDINT1 = omrallt.VARDINT2 THEN musz = musz.
   ELSE DO:
      IF omrallt.VARDSIST < omrallt.VARDINT1 THEN DO:
         MESSAGE "Aktuellt värderingsnr måste ligga inom intervallet.".      
         APPLY "ENTRY" TO omrallt.VARDSIST IN FRAME {&FRAME-NAME}.
         musz = TRUE.
         RETURN.
      END.    
      IF omrallt.VARDSIST > omrallt.VARDINT2 THEN DO:
         MESSAGE "Aktuellt värderingsnr måste ligga inom intervallet.".     
         APPLY "ENTRY" TO omrallt.VARDSIST IN FRAME {&FRAME-NAME}. 
         musz = TRUE.
         RETURN.
      END.
   END.
   RUN intervallkoll_UI IN orgapph (INPUT TABLE omrallt,OUTPUT TABLE felmeddtemp).
   FIND FIRST felmeddtemp NO-ERROR.
   IF AVAILABLE felmeddtemp THEN DO:
      MESSAGE felmeddtemp.FELMEDD VIEW-AS ALERT-BOX.
      DELETE felmeddtemp.
      musz = TRUE.
      RETURN.
   END.        
   IF omrallt.OMRADE = "" THEN ASSIGN omrallt.OMRADE = omrallt.OMRADE.
   IF Guru.Konstanter:varforetypchar[4] NE "" THEN DO:
      RUN omrsparhamta_UI IN anvapph (INPUT omrallt.OMRADE,INPUT TABLE omranvandartemp). 
   END.
   IF Guru.Konstanter:varforetypval[45] = 1 THEN DO:
      RUN prioomrspar_UI IN priapph (INPUT omrallt.OMRADE,INPUT TABLE prioomrtemp).
      /*ccc*/
   END.   
   {musarrow.i}   
   APPLY "GO" TO BTN_OK IN FRAME DEFAULT-FRAME.  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION tabortblanka C-Win 
FUNCTION tabortblanka RETURNS CHARACTER
  ( INPUT namnblanka AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  namnblanka = REPLACE(namnblanka," ","").
  RETURN   namnblanka.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

