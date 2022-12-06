&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
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

DEFINE INPUT  PARAMETER aonrvar AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER delnrvar AS INTEGER NO-UNDO.

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE btnberh AS HANDLE NO-UNDO.



DEFINE VARIABLE ordningnr AS INTEGER NO-UNDO.
DEFINE VARIABLE vguru AS CHARACTER NO-UNDO.
{WHANDLTEMP.I}
{ALLDEF.I}
&Scoped-define NEW 
&Scoped-define SHARED SHARED
{GLOBVAR2DEL1.I}
{AVDELNINGTEMP.I}
{BERIN.I}
/*BEREDNING*/
{BERVAR2.I}
{HOPALLA.I}  
DEFINE NEW SHARED VARIABLE summeradberedning AS LOGICAL NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME AMULTISTART-FRAME
&Scoped-define BROWSE-NAME BRW_MAXMALL

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES urberedningtemp valberedningtemp

/* Definitions for BROWSE BRW_URBER                                     */
&Scoped-define FIELDS-IN-QUERY-BRW_URBER urberedningtemp.OMRADE ~
urberedningtemp.BERNR urberedningtemp.BENAMNING urberedningtemp.AONR ~
urberedningtemp.DELNR urberedningtemp.AKTIV urberedningtemp.ANVANDARE 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_URBER 
&Scoped-define QUERY-STRING-BRW_URBER FOR EACH urberedningtemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_URBER OPEN QUERY BRW_URBER FOR EACH urberedningtemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_URBER urberedningtemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_URBER urberedningtemp


/* Definitions for BROWSE BRW_VBER                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_VBER valberedningtemp.OMRADE ~
valberedningtemp.BERNR valberedningtemp.BENAMNING valberedningtemp.AONR ~
valberedningtemp.DELNR valberedningtemp.AKTIV valberedningtemp.ANVANDARE 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_VBER 
&Scoped-define QUERY-STRING-BRW_VBER FOR EACH valberedningtemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_VBER OPEN QUERY BRW_VBER FOR EACH valberedningtemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_VBER valberedningtemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_VBER valberedningtemp


/* Definitions for FRAME FRAME-BER                                      */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-MENY 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE RECTANGLE RECT-MENY
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 125 BY .08.

DEFINE BUTTON BTN_ADM 
     LABEL "Administration" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_ALLBACK-2 
     IMAGE-UP FILE "BILDER\rewind-u":U NO-FOCUS FLAT-BUTTON
     LABEL "Alla aonr i listan":L 
     SIZE 4 BY 1.21 TOOLTIP "Alla valda aonr tas bort från vallistan"
     FONT 11.

DEFINE BUTTON BTN_ALLOVER-2 
     IMAGE-UP FILE "BILDER\forwrd-u":U NO-FOCUS FLAT-BUTTON
     LABEL "Alla aonr i listan":L 
     SIZE 4 BY 1.21 TOOLTIP "Alla aonr väljs"
     FONT 11.

DEFINE BUTTON BTN_AOF-2 
     LABEL "spara favorit" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_ATG 
     LABEL "Åtgärder" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_ATT 
     LABEL "Attestera" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_AVB-2 
     LABEL "AvslutaR":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_BA 
     LABEL "Begär attest" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_BACK-2 
     IMAGE-UP FILE "BILDER\prev-u":U NO-FOCUS FLAT-BUTTON
     LABEL "":L 
     SIZE 4 BY 1.21 TOOLTIP "Markerade tas bort från vallistan".

DEFINE BUTTON BTN_BORT-2 
     LABEL "Ta bort":L 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_EXP 
     LABEL "Export" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_GJORDABES 
     LABEL "Gjorda beställningar" 
     SIZE 14 BY 1 TOOLTIP "Gjorda beställningar".

DEFINE BUTTON BTN_HAMT-2 
     LABEL "Hämta och visa urval" 
     SIZE 22 BY 1 TOOLTIP "Dina val ovan avgör vilka aonr du får i listan nedan.".

DEFINE BUTTON BTN_HAOF-2 
     LABEL "HÄMTA favorit" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_IMP 
     LABEL "Import" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_INAKTIV 
     LABEL "Aktiv/Inaktiv":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_INK 
     IMAGE-UP FILE "bilder/xbtn_inkopsmal.gif":U
     LABEL "Inköp" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_KALK-2 
     IMAGE-UP FILE "BILDER\xbtn_bered.gif":U
     LABEL "Bereda":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_KOPI-2 
     LABEL "Kopiera" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_LAS 
     IMAGE-UP FILE "bilder/xbtn_lastaber.gif":U
     LABEL "Låsta beredningar" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_LIST 
     LABEL "Listor" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_NY-2 
     LABEL "Ny":L 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_OVER-2 
     IMAGE-UP FILE "BILDER\next-u":U NO-FOCUS FLAT-BUTTON
     LABEL "":L 
     SIZE 4 BY 1.21 TOOLTIP "Markerade väljs".

DEFINE BUTTON BTN_SCHAKTP 
     IMAGE-UP FILE "bilder/xbtn_smalschakt.gif":U
     LABEL "Schaktprotokoll":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_UPP-2 
     LABEL "Beredningshuvud":L 
     SIZE 14 BY 1.

DEFINE VARIABLE CMB_AVD-2 AS CHARACTER FORMAT "X(256)":U INITIAL ? 
     LABEL "Avdelning" 
     VIEW-AS COMBO-BOX INNER-LINES 15
     DROP-DOWN-LIST
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_JURP-2 AS CHARACTER FORMAT "X(256)":U INITIAL ? 
     LABEL "Juridisp" 
     VIEW-AS COMBO-BOX INNER-LINES 15
     DROP-DOWN-LIST
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_OMR-2 AS CHARACTER FORMAT "X(256)":U INITIAL ? 
     LABEL "Område" 
     VIEW-AS COMBO-BOX INNER-LINES 15
     DROP-DOWN-LIST
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_UTF-2 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Utfärdare" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-AOTEXT-B AS CHARACTER FORMAT "X(256)":U INITIAL "Gör urval av beredning" 
      VIEW-AS TEXT 
     SIZE 26.5 BY 1.21
     FGCOLOR 5 FONT 17 NO-UNDO.

DEFINE VARIABLE FILL-IN-BEN AS CHARACTER FORMAT "x(40)" 
     LABEL "Benämning" 
     VIEW-AS FILL-IN 
     SIZE 26 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN-BERNR AS INTEGER FORMAT ">>>>>>9" INITIAL 0 
     LABEL "Beredningsnr" 
     VIEW-AS FILL-IN 
     SIZE 11.5 BY .83
     FGCOLOR 5  NO-UNDO.

DEFINE VARIABLE FILL-IN-HBERNR AS INTEGER FORMAT "->>>>>>>>9" INITIAL 0 
     LABEL "Beredningnr" 
     VIEW-AS FILL-IN 
     SIZE 10.5 BY .83 TOOLTIP "Tryck RETURN här för välja enstaka beredningar."
     FGCOLOR 5 .

DEFINE VARIABLE FILL-IN-HTEXT AS CHARACTER FORMAT "X(256)":U INITIAL "Hämta:" 
      VIEW-AS TEXT 
     SIZE 6.5 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN_AONR AS CHARACTER FORMAT "X(8)" 
     LABEL "Aonr" 
     VIEW-AS FILL-IN 
     SIZE 8 BY .83
     FGCOLOR 2  NO-UNDO.

DEFINE VARIABLE FILL-IN_DELNR-2 AS INTEGER FORMAT ">99" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 4 BY .83 TOOLTIP "Tryck RETURN här för välja enstaka aonr."
     FGCOLOR 2 .

DEFINE VARIABLE FILL-IN_EAONR-2 AS CHARACTER FORMAT "X(6)" 
     LABEL "Aonr" 
     VIEW-AS FILL-IN 
     SIZE 8 BY .83 TOOLTIP "Tryck RETURN här för välja enstaka aonr."
     FGCOLOR 2 .

DEFINE IMAGE IMAGE-7
     FILENAME "BILDER\sokpa.gif":U CONVERT-3D-COLORS
     SIZE 8 BY .83.

DEFINE RECTANGLE RECT-22
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 49.5 BY 2.33
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-62
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 53.5 BY 1.21.

DEFINE VARIABLE TOG_AKT-2 AS LOGICAL INITIAL yes 
     LABEL "Aktiva":L 
     VIEW-AS TOGGLE-BOX
     SIZE 9 BY .71 NO-UNDO.

DEFINE VARIABLE TOG_ALLABER AS LOGICAL INITIAL no 
     LABEL "Ej delnr" 
     VIEW-AS TOGGLE-BOX
     SIZE 7.75 BY .79 NO-UNDO.

DEFINE VARIABLE TOG_BERAO AS LOGICAL INITIAL no 
     LABEL "Endast ej kopplade" 
     VIEW-AS TOGGLE-BOX
     SIZE 20.5 BY .75 NO-UNDO.

DEFINE VARIABLE TOG_BERNY AS LOGICAL INITIAL no 
     LABEL "" 
     VIEW-AS TOGGLE-BOX
     SIZE 1.5 BY .79 NO-UNDO.

DEFINE VARIABLE TOG_INAKT-2 AS LOGICAL INITIAL no 
     LABEL "Inaktiva":L 
     VIEW-AS TOGGLE-BOX
     SIZE 11.63 BY .71 NO-UNDO.

DEFINE VARIABLE TOG_SCH AS LOGICAL INITIAL no 
     LABEL "Endast schakt" 
     VIEW-AS TOGGLE-BOX
     SIZE 16.13 BY .75 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_URBER FOR 
      urberedningtemp SCROLLING.

DEFINE QUERY BRW_VBER FOR 
      valberedningtemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_MAXMALL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_MAXMALL C-Win _STRUCTURED
  
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING MULTIPLE SIZE 80 BY 20
         FONT 4
         TITLE "Arbeta vidare med".

DEFINE BROWSE BRW_URBER
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_URBER C-Win _STRUCTURED
  QUERY BRW_URBER NO-LOCK DISPLAY
      urberedningtemp.OMRADE FORMAT "x(6)":U
      urberedningtemp.BERNR COLUMN-LABEL "Ber.nr" FORMAT "->,>>>,>>9":U
            WIDTH 5.5 COLUMN-FGCOLOR 5 LABEL-FGCOLOR 5
      urberedningtemp.BENAMNING COLUMN-LABEL "Benämning" FORMAT "X(256)":U
            WIDTH 16.5
      urberedningtemp.AONR COLUMN-LABEL "Aonr" FORMAT "X(6)":U
            COLUMN-FGCOLOR 2 LABEL-FGCOLOR 2
      urberedningtemp.DELNR COLUMN-LABEL "Del!nr" FORMAT ">99":U
            COLUMN-FGCOLOR 2 LABEL-FGCOLOR 2
      urberedningtemp.AKTIV COLUMN-LABEL "Aktiv/!Inaktiv" FORMAT "Aktiv/Inaktiv":U
      urberedningtemp.ANVANDARE COLUMN-LABEL "Utfärdare" FORMAT "x(12)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 49.5 BY 15.46
         FONT 4
         TITLE "Urvalsresultat".

DEFINE BROWSE BRW_VBER
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_VBER C-Win _STRUCTURED
  QUERY BRW_VBER NO-LOCK DISPLAY
      valberedningtemp.OMRADE FORMAT "x(6)":U
      valberedningtemp.BERNR COLUMN-LABEL "Ber.nr" FORMAT "->,>>>,>>9":U
            WIDTH 7 COLUMN-FGCOLOR 5 LABEL-FGCOLOR 5
      valberedningtemp.BENAMNING COLUMN-LABEL "Benämning" FORMAT "X(256)":U
            WIDTH 19
      valberedningtemp.AONR COLUMN-LABEL "Aonr" FORMAT "X(6)":U
            COLUMN-FGCOLOR 2 LABEL-FGCOLOR 2
      valberedningtemp.DELNR COLUMN-LABEL "Del!nr" FORMAT ">99":U
            COLUMN-FGCOLOR 2 LABEL-FGCOLOR 2
      valberedningtemp.AKTIV COLUMN-LABEL "Aktiv/!Inaktiv" FORMAT "Aktiv/Inaktiv":U
      valberedningtemp.ANVANDARE COLUMN-LABEL "Utfärdare" FORMAT "x(12)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 53.5 BY 15.46
         FONT 4
         TITLE "Arbeta vidare med".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME AMULTISTART-FRAME
     RECT-MENY AT ROW 3.38 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 125 BY 28.42.

DEFINE FRAME FRAME-BER
     BTN_ALLBACK-2 AT ROW 19.42 COL 51.5
     BTN_ALLOVER-2 AT ROW 12.5 COL 51.5
     TOG_AKT-2 AT ROW 2.17 COL 3
     TOG_INAKT-2 AT ROW 2.17 COL 11.25
     TOG_BERAO AT ROW 2.17 COL 19.63
     TOG_SCH AT ROW 2.17 COL 35.13
     BRW_MAXMALL AT ROW 3 COL 10 WIDGET-ID 100
     CMB_JURP-2 AT ROW 3 COL 3.25
     CMB_AVD-2 AT ROW 4.08 COL 2.25
     CMB_OMR-2 AT ROW 5.17 COL 11.25 COLON-ALIGNED
     CMB_UTF-2 AT ROW 6.29 COL 11.25 COLON-ALIGNED
     BTN_HAMT-2 AT ROW 7.58 COL 18.5
     BTN_KALK-2 AT ROW 7.96 COL 111 WIDGET-ID 22
     BTN_UPP-2 AT ROW 8.5 COL 111 WIDGET-ID 32
     BRW_URBER AT ROW 8.79 COL 1.5
     BRW_VBER AT ROW 8.79 COL 56.13
     TOG_BERNY AT ROW 9.08 COL 109.38
     BTN_LIST AT ROW 9.21 COL 111 WIDGET-ID 28
     BTN_SCHAKTP AT ROW 10.25 COL 111 WIDGET-ID 30
     BTN_INK AT ROW 10.71 COL 111 WIDGET-ID 20
     BTN_HAOF-2 AT ROW 11 COL 111 WIDGET-ID 14
     BTN_AOF-2 AT ROW 11 COL 111 WIDGET-ID 4
     BTN_GJORDABES AT ROW 12.21 COL 111 WIDGET-ID 34
     BTN_ATG AT ROW 12.21 COL 111 WIDGET-ID 6
     BTN_KOPI-2 AT ROW 13.46 COL 111 WIDGET-ID 24
     BTN_INAKTIV AT ROW 14.71 COL 111 WIDGET-ID 18
     BTN_ADM AT ROW 16.21 COL 111 WIDGET-ID 2
     BTN_LAS AT ROW 17.46 COL 111 WIDGET-ID 26
     BTN_EXP AT ROW 18.46 COL 111 WIDGET-ID 12
     BTN_IMP AT ROW 19.71 COL 111 WIDGET-ID 16
     BTN_BA AT ROW 20.96 COL 111 WIDGET-ID 10
     BTN_ATT AT ROW 22.21 COL 111 WIDGET-ID 8
     BTN_NY-2 AT ROW 24.42 COL 67.63
     BTN_BACK-2 AT ROW 17.08 COL 51.5
     BTN_BORT-2 AT ROW 24.42 COL 84.63
     FILL-IN-BERNR AT ROW 24.71 COL 21.88 COLON-ALIGNED
     FILL-IN_AONR AT ROW 24.71 COL 39.25 COLON-ALIGNED
     FILL-IN-BEN AT ROW 25.83 COL 21.88 COLON-ALIGNED
     FILL-IN-HBERNR AT ROW 25.83 COL 69.38 COLON-ALIGNED AUTO-RETURN 
     FILL-IN_EAONR-2 AT ROW 25.83 COL 86.13 COLON-ALIGNED AUTO-RETURN 
     FILL-IN_DELNR-2 AT ROW 25.83 COL 95 COLON-ALIGNED NO-LABEL
     BTN_AVB-2 AT ROW 25.83 COL 111
     TOG_ALLABER AT ROW 25.88 COL 101.63
     BTN_OVER-2 AT ROW 14.79 COL 51.5
     FILL-IN-AOTEXT-B AT ROW 1 COL 2.25 NO-LABEL
     FILL-IN-HTEXT AT ROW 25.83 COL 55 COLON-ALIGNED NO-LABEL
     IMAGE-7 AT ROW 24.75 COL 2.25
     RECT-22 AT ROW 24.5 COL 1.5
     RECT-62 AT ROW 25.63 COL 56.13
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 125 BY 28.42.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Temp-Tables and Buffers:
      TABLE: ansvarigtemp T "?" NO-UNDO temp-db ansvarigtemp
      TABLE: anvandartemp T "?" NO-UNDO temp-db anvandartemp
      TABLE: aotidslagtemp T "?" NO-UNDO temp-db aotidslagtemp
      TABLE: avdelningtemp T "?" NO-UNDO temp-db avdelningtemp
      TABLE: depatemp T "?" NO-UNDO temp-db depatemp
      TABLE: faktplantemp T "?" NO-UNDO temp-db faktplantemp
      TABLE: godkannartemp T "?" NO-UNDO temp-db godkannartemp
      TABLE: jurperstemp T "?" NO-UNDO temp-db jurperstemp
      TABLE: ktotemp T "?" NO-UNDO temp-db ktotemp
      TABLE: markpers T "?" NO-UNDO temp-db markpers
      TABLE: mspec_mtrlextra T "?" NO-UNDO temp-db mspec_mtrlextra
      TABLE: mtrltemp T "?" NO-UNDO temp-db mtrltemp
      TABLE: mvalvardtemp T "?" NO-UNDO temp-db mvalvardtemp
      TABLE: omrtemp T "?" NO-UNDO temp-db omrtemp
      TABLE: personaltemp T "?" NO-UNDO temp-db personaltemp
      TABLE: plannrtemp T "?" NO-UNDO temp-db plannrtemp
      TABLE: pmpersonaltemp T "?" NO-UNDO temp-db pmpersonaltemp
      TABLE: rvisa T "?" NO-UNDO temp-db rvisa
      TABLE: urberedningtemp T "?" NO-UNDO temp-db urberedningtemp
      TABLE: urstorntemp T "?" NO-UNDO temp-db urstorntemp
      TABLE: urvardtemp T "?" NO-UNDO temp-db urvardtemp
      TABLE: utsokaonr T "?" NO-UNDO temp-db utsokaonr
      TABLE: utvaldfasttemp T "?" NO-UNDO temp-db utvaldfasttemp
      TABLE: valberedningtemp T "?" NO-UNDO temp-db valberedningtemp
      TABLE: valdaaotemp T "?" NO-UNDO temp-db valdaaotemp
      TABLE: valdfasttemp T "?" NO-UNDO temp-db valdfasttemp
      TABLE: valperstemp T "?" NO-UNDO temp-db valperstemp
      TABLE: valplantemp T "?" NO-UNDO temp-db valplantemp
      TABLE: vavdelningtemp T "?" NO-UNDO temp-db vavdelningtemp
      TABLE: vfaktplantemp T "?" NO-UNDO temp-db vfaktplantemp
      TABLE: visaupp T "?" NO-UNDO temp-db visaupp
      TABLE: vomrtemp T "?" NO-UNDO temp-db vomrtemp
      TABLE: vstorntemp T "?" NO-UNDO temp-db vstorntemp
      TABLE: xgurutemp T "?" NO-UNDO temp-db xgurutemp
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
         MAX-HEIGHT         = 28.42
         MAX-WIDTH          = 125
         VIRTUAL-HEIGHT     = 28.42
         VIRTUAL-WIDTH      = 125
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
ASSIGN FRAME FRAME-BER:FRAME = FRAME AMULTISTART-FRAME:HANDLE.

/* SETTINGS FOR FRAME AMULTISTART-FRAME
   FRAME-NAME                                                           */
/* SETTINGS FOR FRAME FRAME-BER
                                                                        */
/* BROWSE-TAB BRW_MAXMALL TOG_SCH FRAME-BER */
/* BROWSE-TAB BRW_URBER BTN_UPP-2 FRAME-BER */
/* BROWSE-TAB BRW_VBER BRW_URBER FRAME-BER */
ASSIGN 
       BRW_MAXMALL:HIDDEN  IN FRAME FRAME-BER                = TRUE.

ASSIGN 
       BRW_URBER:ALLOW-COLUMN-SEARCHING IN FRAME FRAME-BER = TRUE
       BRW_URBER:COLUMN-RESIZABLE IN FRAME FRAME-BER       = TRUE.

ASSIGN 
       BRW_VBER:HIDDEN  IN FRAME FRAME-BER                = TRUE
       BRW_VBER:MAX-DATA-GUESS IN FRAME FRAME-BER         = 1000
       BRW_VBER:ALLOW-COLUMN-SEARCHING IN FRAME FRAME-BER = TRUE
       BRW_VBER:COLUMN-RESIZABLE IN FRAME FRAME-BER       = TRUE.

/* SETTINGS FOR BUTTON BTN_ADM IN FRAME FRAME-BER
   NO-ENABLE                                                            */
ASSIGN 
       BTN_ADM:HIDDEN IN FRAME FRAME-BER           = TRUE.

/* SETTINGS FOR BUTTON BTN_ALLOVER-2 IN FRAME FRAME-BER
   NO-ENABLE                                                            */
ASSIGN 
       BTN_ALLOVER-2:HIDDEN IN FRAME FRAME-BER           = TRUE.

/* SETTINGS FOR BUTTON BTN_AOF-2 IN FRAME FRAME-BER
   NO-ENABLE                                                            */
ASSIGN 
       BTN_AOF-2:HIDDEN IN FRAME FRAME-BER           = TRUE.

/* SETTINGS FOR BUTTON BTN_ATG IN FRAME FRAME-BER
   NO-ENABLE                                                            */
ASSIGN 
       BTN_ATG:HIDDEN IN FRAME FRAME-BER           = TRUE.

/* SETTINGS FOR BUTTON BTN_ATT IN FRAME FRAME-BER
   NO-ENABLE                                                            */
ASSIGN 
       BTN_ATT:HIDDEN IN FRAME FRAME-BER           = TRUE.

/* SETTINGS FOR BUTTON BTN_BA IN FRAME FRAME-BER
   NO-ENABLE                                                            */
ASSIGN 
       BTN_BA:HIDDEN IN FRAME FRAME-BER           = TRUE.

/* SETTINGS FOR BUTTON BTN_BORT-2 IN FRAME FRAME-BER
   NO-ENABLE                                                            */
ASSIGN 
       BTN_BORT-2:HIDDEN IN FRAME FRAME-BER           = TRUE.

/* SETTINGS FOR BUTTON BTN_EXP IN FRAME FRAME-BER
   NO-ENABLE                                                            */
ASSIGN 
       BTN_EXP:HIDDEN IN FRAME FRAME-BER           = TRUE.

/* SETTINGS FOR BUTTON BTN_GJORDABES IN FRAME FRAME-BER
   NO-ENABLE                                                            */
ASSIGN 
       BTN_GJORDABES:HIDDEN IN FRAME FRAME-BER           = TRUE.

/* SETTINGS FOR BUTTON BTN_HAOF-2 IN FRAME FRAME-BER
   NO-ENABLE                                                            */
ASSIGN 
       BTN_HAOF-2:HIDDEN IN FRAME FRAME-BER           = TRUE.

/* SETTINGS FOR BUTTON BTN_IMP IN FRAME FRAME-BER
   NO-ENABLE                                                            */
ASSIGN 
       BTN_IMP:HIDDEN IN FRAME FRAME-BER           = TRUE.

/* SETTINGS FOR BUTTON BTN_INAKTIV IN FRAME FRAME-BER
   NO-ENABLE                                                            */
ASSIGN 
       BTN_INAKTIV:HIDDEN IN FRAME FRAME-BER           = TRUE.

/* SETTINGS FOR BUTTON BTN_INK IN FRAME FRAME-BER
   NO-ENABLE                                                            */
ASSIGN 
       BTN_INK:HIDDEN IN FRAME FRAME-BER           = TRUE.

/* SETTINGS FOR BUTTON BTN_KALK-2 IN FRAME FRAME-BER
   NO-ENABLE                                                            */
ASSIGN 
       BTN_KALK-2:HIDDEN IN FRAME FRAME-BER           = TRUE.

/* SETTINGS FOR BUTTON BTN_LAS IN FRAME FRAME-BER
   NO-ENABLE                                                            */
ASSIGN 
       BTN_LAS:HIDDEN IN FRAME FRAME-BER           = TRUE.

/* SETTINGS FOR BUTTON BTN_LIST IN FRAME FRAME-BER
   NO-ENABLE                                                            */
ASSIGN 
       BTN_LIST:HIDDEN IN FRAME FRAME-BER           = TRUE.

/* SETTINGS FOR BUTTON BTN_NY-2 IN FRAME FRAME-BER
   NO-ENABLE                                                            */
ASSIGN 
       BTN_NY-2:HIDDEN IN FRAME FRAME-BER           = TRUE.

/* SETTINGS FOR BUTTON BTN_SCHAKTP IN FRAME FRAME-BER
   NO-ENABLE                                                            */
ASSIGN 
       BTN_SCHAKTP:HIDDEN IN FRAME FRAME-BER           = TRUE.

/* SETTINGS FOR BUTTON BTN_UPP-2 IN FRAME FRAME-BER
   NO-ENABLE                                                            */
ASSIGN 
       BTN_UPP-2:HIDDEN IN FRAME FRAME-BER           = TRUE.

/* SETTINGS FOR COMBO-BOX CMB_AVD-2 IN FRAME FRAME-BER
   ALIGN-L                                                              */
/* SETTINGS FOR COMBO-BOX CMB_JURP-2 IN FRAME FRAME-BER
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FILL-IN-AOTEXT-B IN FRAME FRAME-BER
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN FILL-IN-HTEXT IN FRAME FRAME-BER
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX TOG_BERNY IN FRAME FRAME-BER
   NO-ENABLE                                                            */
ASSIGN 
       TOG_BERNY:HIDDEN IN FRAME FRAME-BER           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_MAXMALL
/* Query rebuild information for BROWSE BRW_MAXMALL
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* BROWSE BRW_MAXMALL */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_URBER
/* Query rebuild information for BROWSE BRW_URBER
     _TblList          = "Temp-Tables.urberedningtemp"
     _Options          = "NO-LOCK "
     _FldNameList[1]   = Temp-Tables.urberedningtemp.OMRADE
     _FldNameList[2]   > Temp-Tables.urberedningtemp.BERNR
"urberedningtemp.BERNR" "Ber.nr" ? "integer" ? 5 ? ? 5 ? no ? no no "5.5" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.urberedningtemp.BENAMNING
"urberedningtemp.BENAMNING" "Benämning" "X(256)" "character" ? ? ? ? ? ? no ? no no "16.5" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.urberedningtemp.AONR
"urberedningtemp.AONR" "Aonr" ? "character" ? 2 ? ? 2 ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.urberedningtemp.DELNR
"urberedningtemp.DELNR" "Del!nr" ">99" "integer" ? 2 ? ? 2 ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > Temp-Tables.urberedningtemp.AKTIV
"urberedningtemp.AKTIV" "Aktiv/!Inaktiv" "Aktiv/Inaktiv" "logical" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > Temp-Tables.urberedningtemp.ANVANDARE
"urberedningtemp.ANVANDARE" "Utfärdare" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_URBER */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_VBER
/* Query rebuild information for BROWSE BRW_VBER
     _TblList          = "Temp-Tables.valberedningtemp"
     _Options          = "NO-LOCK "
     _FldNameList[1]   = Temp-Tables.valberedningtemp.OMRADE
     _FldNameList[2]   > Temp-Tables.valberedningtemp.BERNR
"valberedningtemp.BERNR" "Ber.nr" ? "integer" ? 5 ? ? 5 ? no ? no no "7" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.valberedningtemp.BENAMNING
"valberedningtemp.BENAMNING" "Benämning" "X(256)" "character" ? ? ? ? ? ? no ? no no "19" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.valberedningtemp.AONR
"valberedningtemp.AONR" "Aonr" ? "character" ? 2 ? ? 2 ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.valberedningtemp.DELNR
"valberedningtemp.DELNR" "Del!nr" ">99" "integer" ? 2 ? ? 2 ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > Temp-Tables.valberedningtemp.AKTIV
"valberedningtemp.AKTIV" "Aktiv/!Inaktiv" "Aktiv/Inaktiv" "logical" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > Temp-Tables.valberedningtemp.ANVANDARE
"valberedningtemp.ANVANDARE" "Utfärdare" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_VBER */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-BER
/* Query rebuild information for FRAME FRAME-BER
     _Query            is NOT OPENED
*/  /* FRAME FRAME-BER */
&ANALYZE-RESUME

 

&Scoped-define BROWSE-NAME BRW_MAXMALL

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */

ON CLOSE OF THIS-PROCEDURE DO:
    {BORTBRWPROC.I}
   IF VALID-HANDLE(btnberh) THEN DELETE PROCEDURE btnberh.
   RUN disable_UI.
END.   

ON WINDOW-CLOSE OF {&WINDOW-NAME} DO:   
   
     
END.
ON ENDKEY, END-ERROR OF {&WINDOW-NAME} ANYWHERE DO:   
   RETURN NO-APPLY.
   
END.


/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:   
   {STARTWIN2.I} 
   {FRMSIZEF.I} 
   IF Guru.Konstanter:mtrlsekvar[5] = TRUE THEN DO:
      RUN btnberstart_UI (INPUT 0, INPUT "", INPUT 0, INPUT TABLE berintemp).                  
   END. 
   ELSE DO:
     APPLY "CLOSE" TO THIS-PROCEDURE. 
     LEAVE MAIN-BLOCK.
   END.
   RUN enable_UI.  
   {WIN_M_SLUT.I}   
    IF NOT THIS-PROCEDURE:PERSISTENT THEN
   WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE avb_UI C-Win 
PROCEDURE avb_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE VARIABLE vc AS CHARACTER FORMAT "x(20)". 
   DEFINE VARIABLE vcnr AS CHARACTER.    
   DEFINE VARIABLE franfil  AS CHARACTER NO-UNDO.      
   RUN avs_UI.   
   APPLY "CLOSE" TO THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE avs_UI C-Win 
PROCEDURE avs_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE VARIABLE vc AS CHARACTER FORMAT "x(20)". 
   DEFINE VARIABLE vcnr AS CHARACTER.    
   DEFINE VARIABLE franfil  AS CHARACTER NO-UNDO.      
   IF VALID-HANDLE(btnberh) THEN DO:
      RUN borthand_UI IN btnberh.
      DELETE PROCEDURE btnberh NO-ERROR.      
   END.   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnber0_UI C-Win 
PROCEDURE btnber0_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   RUN btnberstart_UI (INPUT 1, INPUT "", INPUT 0, INPUT TABLE berintemp).   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnberstart_UI C-Win 
PROCEDURE btnberstart_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER franvart AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER beraonr AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER berdelnr AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER TABLE FOR berintemp.     
   HIDE FRAME FRAME-START.
   HIDE FRAME FRAME-WEBB.      
   IF NOT VALID-HANDLE(btnberh) THEN DO:
      RUN btnber_UI  (INPUT franvart, INPUT beraonr, INPUT berdelnr, INPUT TABLE berintemp).       
   END.  
   ELSE RUN franstart_UI IN btnberh (INPUT franvart, INPUT beraonr, INPUT berdelnr, INPUT TABLE berintemp). 
   RUN frame_UI (INPUT "BER").
   /*direkt här*/
   IF franvart = 0 OR franvart = 1 THEN franvart = franvart. 
   ELSE RUN repo_UI IN btnberh. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnber_UI C-Win 
PROCEDURE btnber_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER franvart AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER beraonr AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER berdelnr AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER TABLE FOR berintemp.
   EMPTY TEMP-TABLE whandltemp NO-ERROR.
   ordningnr = 1.
   CREATE whandltemp.
   &Scoped-define EXTRAFRAME FRAME-BER
   {BERWHND.I}
   
   RUN BERMENY.P PERSISTENT SET btnberh (INPUT THIS-PROCEDURE ,INPUT framesizeh,INPUT TABLE whandltemp,
                                         INPUT franvart,INPUT beraonr,INPUT berdelnr,INPUT TABLE berintemp).                                           
   RUN kopplaaonr_UI IN btnberh (INPUT aonrvar, INPUT delnrvar ).     
   FRAME FRAME-BER:HIDDEN = FALSE.   
   ASSIGN
   BTN_KALK-2:HIDDEN = TRUE
   BTN_UPP-2:HIDDEN = TRUE
   BTN_LIST:HIDDEN = TRUE
   BTN_SCHAKTP:HIDDEN = TRUE
   BTN_INK:HIDDEN = TRUE
   BTN_HAOF-2:HIDDEN = TRUE
   BTN_AOF-2:HIDDEN = TRUE
   BTN_ATG:HIDDEN = TRUE
   BTN_INAKTIV:HIDDEN = TRUE
   BTN_ADM:HIDDEN = TRUE
   BTN_LAS:HIDDEN = TRUE
   BTN_EXP:HIDDEN = TRUE
   BTN_IMP:HIDDEN = TRUE
   BTN_BA:HIDDEN = TRUE
   BTN_BORT-2:HIDDEN = TRUE
   BTN_NY-2:HIDDEN = TRUE
   BTN_ATT:HIDDEN = TRUE
   BTN_GJORDABES:HIDDEN = TRUE.      
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
  ENABLE RECT-MENY 
      WITH FRAME AMULTISTART-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-AMULTISTART-FRAME}
  DISPLAY TOG_AKT-2 TOG_INAKT-2 TOG_BERAO TOG_SCH CMB_JURP-2 CMB_AVD-2 CMB_OMR-2 
          CMB_UTF-2 TOG_BERNY FILL-IN-BERNR FILL-IN_AONR FILL-IN-BEN 
          FILL-IN-HBERNR FILL-IN_EAONR-2 FILL-IN_DELNR-2 TOG_ALLABER 
          FILL-IN-AOTEXT-B FILL-IN-HTEXT 
      WITH FRAME FRAME-BER IN WINDOW C-Win.
  ENABLE IMAGE-7 BTN_ALLBACK-2 RECT-22 RECT-62 TOG_AKT-2 TOG_INAKT-2 TOG_BERAO 
         TOG_SCH CMB_JURP-2 CMB_AVD-2 CMB_OMR-2 CMB_UTF-2 BTN_HAMT-2 BRW_URBER 
         BRW_VBER BTN_KOPI-2 BTN_BACK-2 FILL-IN-BERNR FILL-IN_AONR FILL-IN-BEN 
         FILL-IN-HBERNR FILL-IN_EAONR-2 FILL-IN_DELNR-2 BTN_AVB-2 TOG_ALLABER 
         BTN_OVER-2 
      WITH FRAME FRAME-BER IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-BER}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE frame_UI C-Win 
PROCEDURE frame_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER vilkenframe AS CHARACTER NO-UNDO.  
   IF vilkenframe = "BER" AND FRAME FRAME-BER:HIDDEN = FALSE THEN RETURN.   
   ASSIGN
   FRAME FRAME-BER:HIDDEN = TRUE.   
   IF vilkenframe = "BER" THEN FRAME FRAME-BER:HIDDEN = FALSE.   
   C-Win:TITLE = "Kopiera och koppla ett " + Guru.Konstanter:gaok.   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE goma_UI C-Win 
PROCEDURE goma_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  /*ska finnas*/
   DEFINE INPUT PARAMETER gomvar AS LOGICAL NO-UNDO.
   DEFINE INPUT PARAMETER vad AS CHARACTER NO-UNDO.  
   RUN avb_UI.
  RETURN.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE hoj_UI C-Win 
PROCEDURE hoj_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER varifran AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER brworgwith   AS DECIMAL NO-UNDO.
   DEFINE OUTPUT PARAMETER brworghight  AS DECIMAL NO-UNDO.
   DEFINE OUTPUT PARAMETER brworgrow    AS DECIMAL NO-UNDO.
   DEFINE OUTPUT PARAMETER brworgcol    AS DECIMAL NO-UNDO.
   DEFINE OUTPUT PARAMETER btnorgrow    AS DECIMAL NO-UNDO.
   DEFINE OUTPUT PARAMETER btnorgcol    AS DECIMAL NO-UNDO.   
   IF varifran = "BER" THEN DO:
      ASSIGN
      brworgwith = BRW_VBER:WIDTH-CHARS IN FRAME FRAME-BER
      brworghight = BRW_VBER:HEIGHT-CHARS
      brworgrow = BRW_VBER:ROW
      brworgcol = BRW_VBER:COLUMN
      btnorgrow = BTN_NY-2:ROW IN FRAME FRAME-BER
      btnorgcol = BTN_NY-2:COL.   
      RETURN.
   END.     
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE kolltt_UI C-Win 
PROCEDURE kolltt_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   BTN_AVB-2:LOAD-IMAGE ("BILDER\xbtn_avs.gif") IN FRAME FRAME-BER.   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE whandle_UI C-Win 
PROCEDURE whandle_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT-OUTPUT PARAMETER ordnr AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER ordh AS HANDLE NO-UNDO.
   ASSIGN
   whandltemp.WF[ordnr] = ordh.
   ordnr = ordnr + 1.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

