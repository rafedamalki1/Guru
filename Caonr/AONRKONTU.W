&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME DIALOG-2


/* Temp-Table and Buffer definitions                                    */



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS DIALOG-2 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 95/05/11 - 10:15 am

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEFINE INPUT PARAMETER vartvar AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER aonrvar AS CHARACTER NO-UNDO. 
DEFINE INPUT PARAMETER delnrvar AS INTEGER NO-UNDO. 
DEFINE INPUT PARAMETER plannrvar AS CHARACTER NO-UNDO. 
DEFINE INPUT PARAMETER artalvar AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER fastaplan AS LOGICAL NO-UNDO.
DEFINE INPUT PARAMETER omrvar AS CHARACTER NO-UNDO.
/* Local Variable Definitions ---                                       */
{ALLDEF.I}
&Scoped-define NEW 
{GLOBVAR2DEL1.I}
{REGVAR.I}

&Scoped-define SHARED SHARED
{AVTPLANTEMP.I}
DEFINE TEMP-TABLE plkont NO-UNDO LIKE plankonttemp.
{OMRTEMPW.I}
&Scoped-define NEW NEW
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE aonrrec AS RECID NO-UNDO.    
DEFINE VARIABLE skick AS LOGICAL NO-UNDO.
DEFINE VARIABLE efel AS CHARACTER FORMAT "X(30)" NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE antal_valda AS INTEGER NO-UNDO.
DEFINE VARIABLE antal_raknare AS INTEGER NO-UNDO.    
DEFINE VARIABLE ktorec AS RECID NO-UNDO.
DEFINE VARIABLE vadgora AS INTEGER NO-UNDO.
DEFINE VARIABLE jid AS CHARACTER NO-UNDO.
DEFINE VARIABLE medd AS CHARACTER NO-UNDO.
DEFINE VARIABLE amne AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE benvar AS CHARACTER NO-UNDO.
DEFINE TEMP-TABLE kontotemp
   FIELD KKBENAMNING AS CHARACTER FORMAT "X(10)"
   FIELD KK1 AS CHARACTER
   FIELD KK2 AS CHARACTER
   FIELD KK3 AS CHARACTER
   FIELD KK4 AS CHARACTER 
   FIELD KK5 AS CHARACTER 
   INDEX konttemp IS PRIMARY KKBENAMNING ASCENDING.
DEFINE TEMP-TABLE exkontotemp NO-UNDO LIKE kontotemp.
DEFINE TEMP-TABLE seltemp
   FIELD KONTO AS CHARACTER 
   FIELD KONTONR AS CHARACTER   
   FIELD BENAMNING AS CHARACTER
   INDEX KONTO  IS PRIMARY KONTO KONTONR ASCENDING.
DEFINE TEMP-TABLE aoktoinnan
   FIELD AONR AS CHARACTER
   FIELD DELNR AS INTEGER
   FIELD K1 AS CHARACTER
   FIELD K2 AS CHARACTER
   FIELD K3 AS CHARACTER
   FIELD K4 AS CHARACTER 
   FIELD K5 AS CHARACTER 
   FIELD SATS% AS INTEGER.
DEFINE TEMP-TABLE planktoinnan
   FIELD PLANNR AS CHARACTER
   FIELD ARTAL AS INTEGER
   FIELD K1 AS CHARACTER
   FIELD K2 AS CHARACTER
   FIELD K3 AS CHARACTER
   FIELD K4 AS CHARACTER 
   FIELD K5 AS CHARACTER 
   FIELD SATS% AS INTEGER.
DEFINE TEMP-TABLE aokont
   FIELD AONR AS CHARACTER
   FIELD DELNR AS INTEGER
   FIELD K1 AS CHARACTER
   FIELD K2 AS CHARACTER
   FIELD K3 AS CHARACTER
   FIELD K4 AS CHARACTER 
   FIELD K5 AS CHARACTER 
   FIELD SATS% AS INTEGER
   FIELD RECTIDVIS AS RECID.   
DEFINE TEMP-TABLE kstrang
   FIELD BENAMNING AS CHARACTER
   FIELD K1 AS CHARACTER
   FIELD K2 AS CHARACTER
   FIELD K3 AS CHARACTER
   FIELD K4 AS CHARACTER
   FIELD K5 AS CHARACTER
   FIELD OMRADE AS CHARACTER
   FIELD RADNR AS INTEGER
   FIELD RECTIDVIS AS RECID
   INDEX KONTO K1 K2 K3 K4 K5
   INDEX KONTO2 RADNR.
DEFINE TEMP-TABLE ktotemp
   FIELD KONTO AS CHARACTER
   FIELD KONTONR AS CHARACTER
   FIELD BENAMNING AS CHARACTER
   FIELD JUDID AS CHARACTER
   FIELD VIJUDID AS CHARACTER
   FIELD OMRADE AS CHARACTER
   FIELD AVDELNINGNR AS INTEGER
   FIELD AKTIV AS LOGICAL
   FIELD RECTIDVIS AS RECID
   INDEX KONTO KONTO KONTONR.
DEFINE TEMP-TABLE kbtemp
   FIELD K1 AS CHARACTER
   FIELD K2 AS CHARACTER
   FIELD K3 AS CHARACTER
   FIELD K4 AS CHARACTER
   FIELD K5 AS CHARACTER.
DEFINE TEMP-TABLE jurtemp
   FIELD JUDID AS CHARACTER
   FIELD NAMN AS CHARACTER
   FIELD VIJUDID AS CHARACTER.   
DEFINE TEMP-TABLE aotemp
   FIELD AONR AS CHARACTER
   FIELD DELNR AS INTEGER
   FIELD OMRADE AS CHARACTER.
DEFINE TEMP-TABLE plantemp
   FIELD PLANNR AS CHARACTER
   FIELD ARTAL AS INTEGER
   FIELD OMRADE AS CHARACTER. 
DEFINE TEMP-TABLE omtemp
   FIELD OMRADE AS CHARACTER
   FIELD AVDELNINGNR AS INTEGER
   FIELD KONTOOB AS LOGICAL.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DIALOG-2
&Scoped-define BROWSE-NAME BRW_AONRKTO

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES aokont ktotemp plkont kontotemp

/* Definitions for BROWSE BRW_AONRKTO                                   */
&Scoped-define FIELDS-IN-QUERY-BRW_AONRKTO aokont.K1 aokont.K2 aokont.K3 ~
aokont.K4 aokont.K5 aokont.SATS% 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_AONRKTO aokont.SATS% 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_AONRKTO aokont
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_AONRKTO aokont
&Scoped-define QUERY-STRING-BRW_AONRKTO FOR EACH aokont NO-LOCK
&Scoped-define OPEN-QUERY-BRW_AONRKTO OPEN QUERY BRW_AONRKTO FOR EACH aokont NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_AONRKTO aokont
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_AONRKTO aokont


/* Definitions for BROWSE BRW_K1                                        */
&Scoped-define FIELDS-IN-QUERY-BRW_K1 ktotemp.KONTONR 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_K1 
&Scoped-define QUERY-STRING-BRW_K1 FOR EACH ktotemp ~
      WHERE ktotemp.KONTO = "k1" NO-LOCK
&Scoped-define OPEN-QUERY-BRW_K1 OPEN QUERY BRW_K1 FOR EACH ktotemp ~
      WHERE ktotemp.KONTO = "k1" NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_K1 ktotemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_K1 ktotemp


/* Definitions for BROWSE BRW_K2                                        */
&Scoped-define FIELDS-IN-QUERY-BRW_K2 ktotemp.KONTONR 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_K2 
&Scoped-define QUERY-STRING-BRW_K2 FOR EACH ktotemp ~
      WHERE ktotemp.KONTO = "k2" NO-LOCK
&Scoped-define OPEN-QUERY-BRW_K2 OPEN QUERY BRW_K2 FOR EACH ktotemp ~
      WHERE ktotemp.KONTO = "k2" NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_K2 ktotemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_K2 ktotemp


/* Definitions for BROWSE BRW_K3                                        */
&Scoped-define FIELDS-IN-QUERY-BRW_K3 ktotemp.KONTONR 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_K3 
&Scoped-define QUERY-STRING-BRW_K3 FOR EACH ktotemp ~
      WHERE ktotemp.KONTO = "k3" NO-LOCK
&Scoped-define OPEN-QUERY-BRW_K3 OPEN QUERY BRW_K3 FOR EACH ktotemp ~
      WHERE ktotemp.KONTO = "k3" NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_K3 ktotemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_K3 ktotemp


/* Definitions for BROWSE BRW_K4                                        */
&Scoped-define FIELDS-IN-QUERY-BRW_K4 ktotemp.KONTONR 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_K4 
&Scoped-define QUERY-STRING-BRW_K4 FOR EACH ktotemp ~
      WHERE ktotemp.KONTO = "k4" NO-LOCK
&Scoped-define OPEN-QUERY-BRW_K4 OPEN QUERY BRW_K4 FOR EACH ktotemp ~
      WHERE ktotemp.KONTO = "k4" NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_K4 ktotemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_K4 ktotemp


/* Definitions for BROWSE BRW_K5                                        */
&Scoped-define FIELDS-IN-QUERY-BRW_K5 ktotemp.KONTONR 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_K5 
&Scoped-define QUERY-STRING-BRW_K5 FOR EACH ktotemp ~
      WHERE ktotemp.KONTO = "k5" NO-LOCK
&Scoped-define OPEN-QUERY-BRW_K5 OPEN QUERY BRW_K5 FOR EACH ktotemp ~
      WHERE ktotemp.KONTO = "k5" NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_K5 ktotemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_K5 ktotemp


/* Definitions for BROWSE BRW_PLANKTO                                   */
&Scoped-define FIELDS-IN-QUERY-BRW_PLANKTO plkont.K1 plkont.K2 plkont.K3 ~
plkont.K4 plkont.K5 plkont.SATS% 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_PLANKTO plkont.SATS% 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_PLANKTO plkont
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_PLANKTO plkont
&Scoped-define QUERY-STRING-BRW_PLANKTO FOR EACH plkont NO-LOCK
&Scoped-define OPEN-QUERY-BRW_PLANKTO OPEN QUERY BRW_PLANKTO FOR EACH plkont NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_PLANKTO plkont
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_PLANKTO plkont


/* Definitions for BROWSE BRW_STR                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_STR kontotemp.KKBENAMNING kontotemp.KK1 ~
kontotemp.KK2 kontotemp.KK3 kontotemp.KK4 kontotemp.KK5 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_STR kontotemp.KKBENAMNING 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_STR kontotemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_STR kontotemp
&Scoped-define QUERY-STRING-BRW_STR FOR EACH kontotemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_STR OPEN QUERY BRW_STR FOR EACH kontotemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_STR kontotemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_STR kontotemp


/* Definitions for DIALOG-BOX DIALOG-2                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-24 BRW_STR BRW_AONRKTO BRW_PLANKTO ~
BTN_over BTN_back BTN_KONTO BRW_K1 BRW_K2 BRW_K3 BRW_K4 BRW_K5 BTN_OK ~
BTN_AVB 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN_K1 FILL-IN_K2 FILL-IN_K3 ~
FILL-IN_K4 FILL-IN_K5 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB 
     LABEL "Avbryt" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_back 
     IMAGE-UP FILE "BILDER\prev-u":U
     LABEL "BTN 2":L 
     SIZE 4 BY 1.21 TOOLTIP "Skicka tillbaka markerade".

DEFINE BUTTON BTN_KONTO 
     IMAGE-UP FILE "BILDER\export-d":U
     LABEL "BTN 9":L 
     SIZE 4 BY 1.5.

DEFINE BUTTON BTN_OK AUTO-GO 
     LABEL "Ok":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_over 
     IMAGE-UP FILE "BILDER\next-u":U
     LABEL "BTN 1":L 
     SIZE 4 BY 1.21 TOOLTIP "Skicka över markerade".

DEFINE VARIABLE FILL-IN_K1 AS CHARACTER FORMAT "x(6)" 
      VIEW-AS TEXT 
     SIZE 11.5 BY 1.

DEFINE VARIABLE FILL-IN_K2 AS CHARACTER FORMAT "x(6)" 
      VIEW-AS TEXT 
     SIZE 11.5 BY 1.

DEFINE VARIABLE FILL-IN_K3 AS CHARACTER FORMAT "x(6)" 
      VIEW-AS TEXT 
     SIZE 11.5 BY 1.

DEFINE VARIABLE FILL-IN_K4 AS CHARACTER FORMAT "x(6)" 
      VIEW-AS TEXT 
     SIZE 11.5 BY 1.

DEFINE VARIABLE FILL-IN_K5 AS CHARACTER FORMAT "x(6)" 
      VIEW-AS TEXT 
     SIZE 11.5 BY 1.

DEFINE {&NEW} SHARED VARIABLE RAD_FAST AS LOGICAL 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Tillfälliga aonr", no,
"Fasta aonr", yes
     SIZE 37.5 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-24
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 63.25 BY 8
     BGCOLOR 8 .

DEFINE {&NEW} SHARED VARIABLE SEL_K1 AS CHARACTER INITIAL ? 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     LIST-ITEMS " Blank" 
     SIZE 12 BY 5.5 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE SEL_K2 AS CHARACTER INITIAL ? 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     LIST-ITEMS " Blank" 
     SIZE 12 BY 5.5 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE SEL_K3 AS CHARACTER INITIAL ? 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     LIST-ITEMS " Blank" 
     SIZE 12 BY 5.5 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE SEL_K4 AS CHARACTER INITIAL ? 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     LIST-ITEMS " Blank" 
     SIZE 12 BY 5.5 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE SEL_K5 AS CHARACTER INITIAL ? 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     LIST-ITEMS " Blank" 
     SIZE 12 BY 5.5 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_AONRKTO FOR 
      aokont SCROLLING.

DEFINE QUERY BRW_K1 FOR 
      ktotemp SCROLLING.

DEFINE QUERY BRW_K2 FOR 
      ktotemp SCROLLING.

DEFINE QUERY BRW_K3 FOR 
      ktotemp SCROLLING.

DEFINE QUERY BRW_K4 FOR 
      ktotemp SCROLLING.

DEFINE QUERY BRW_K5 FOR 
      ktotemp SCROLLING.

DEFINE QUERY BRW_PLANKTO FOR 
      plkont SCROLLING.

DEFINE QUERY BRW_STR FOR 
      kontotemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_AONRKTO
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_AONRKTO DIALOG-2 _STRUCTURED
  QUERY BRW_AONRKTO NO-LOCK DISPLAY
      aokont.K1 FORMAT "X(256)":U WIDTH 9
      aokont.K2 FORMAT "X(256)":U WIDTH 9
      aokont.K3 FORMAT "X(256)":U WIDTH 9
      aokont.K4 FORMAT "X(256)":U WIDTH 9
      aokont.K5 FORMAT "X(256)":U WIDTH 9
      aokont.SATS% COLUMN-LABEL "%" FORMAT ">99":U
  ENABLE
      aokont.SATS%
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH MULTIPLE SIZE 56 BY 11
         TITLE "".

DEFINE BROWSE BRW_K1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_K1 DIALOG-2 _STRUCTURED
  QUERY BRW_K1 NO-LOCK DISPLAY
      ktotemp.KONTONR FORMAT "x(256)":U WIDTH 9
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-LABELS NO-COLUMN-SCROLLING SIZE 11.75 BY 5.5 ROW-HEIGHT-CHARS .63.

DEFINE BROWSE BRW_K2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_K2 DIALOG-2 _STRUCTURED
  QUERY BRW_K2 NO-LOCK DISPLAY
      ktotemp.KONTONR FORMAT "x(256)":U WIDTH 9
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-LABELS NO-COLUMN-SCROLLING SIZE 11.75 BY 5.5 ROW-HEIGHT-CHARS .63.

DEFINE BROWSE BRW_K3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_K3 DIALOG-2 _STRUCTURED
  QUERY BRW_K3 NO-LOCK DISPLAY
      ktotemp.KONTONR FORMAT "x(256)":U WIDTH 9
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-LABELS NO-COLUMN-SCROLLING SIZE 11.75 BY 5.5 ROW-HEIGHT-CHARS .63.

DEFINE BROWSE BRW_K4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_K4 DIALOG-2 _STRUCTURED
  QUERY BRW_K4 NO-LOCK DISPLAY
      ktotemp.KONTONR FORMAT "x(256)":U WIDTH 9
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-LABELS NO-COLUMN-SCROLLING SIZE 11.75 BY 5.5 ROW-HEIGHT-CHARS .63.

DEFINE BROWSE BRW_K5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_K5 DIALOG-2 _STRUCTURED
  QUERY BRW_K5 NO-LOCK DISPLAY
      ktotemp.KONTONR FORMAT "x(256)":U WIDTH 9
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-LABELS NO-COLUMN-SCROLLING SIZE 11.75 BY 5.5 ROW-HEIGHT-CHARS .63.

DEFINE BROWSE BRW_PLANKTO
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_PLANKTO DIALOG-2 _STRUCTURED
  QUERY BRW_PLANKTO NO-LOCK DISPLAY
      plkont.K1 FORMAT "x(256)":U WIDTH 9
      plkont.K2 FORMAT "x(256)":U WIDTH 9
      plkont.K3 FORMAT "x(256)":U WIDTH 9
      plkont.K4 FORMAT "x(256)":U WIDTH 9
      plkont.K5 FORMAT "x(256)":U WIDTH 9
      plkont.SATS% COLUMN-LABEL "%" FORMAT "999":U
  ENABLE
      plkont.SATS%
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH MULTIPLE SIZE 56 BY 11
         TITLE "".

DEFINE BROWSE BRW_STR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_STR DIALOG-2 _STRUCTURED
  QUERY BRW_STR NO-LOCK DISPLAY
      kontotemp.KKBENAMNING COLUMN-LABEL "Benämning" FORMAT "X(256)":U
            WIDTH 42
      kontotemp.KK1 FORMAT "X(256)":U WIDTH 5
      kontotemp.KK2 FORMAT "X(256)":U WIDTH 5
      kontotemp.KK3 FORMAT "X(256)":U WIDTH 5
      kontotemp.KK4 FORMAT "X(256)":U WIDTH 5
      kontotemp.KK5 FORMAT "X(256)":U WIDTH 5
  ENABLE
      kontotemp.KKBENAMNING
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING MULTIPLE SIZE 57 BY 11
         TITLE "Färdiga kontosträngar".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-2
     BRW_STR AT ROW 1.5 COL 2
     BRW_AONRKTO AT ROW 1.5 COL 64.5
     BRW_PLANKTO AT ROW 1.5 COL 64.5
     BTN_over AT ROW 4.75 COL 59.63
     BTN_back AT ROW 8.46 COL 59.63
     BTN_KONTO AT ROW 14 COL 11.25
     BRW_K1 AT ROW 16 COL 18
     SEL_K1 AT ROW 16 COL 18 NO-LABEL
     SEL_K2 AT ROW 16 COL 30 NO-LABEL
     BRW_K2 AT ROW 16 COL 30.13
     SEL_K3 AT ROW 16 COL 42 NO-LABEL
     BRW_K3 AT ROW 16 COL 42.25
     SEL_K4 AT ROW 16 COL 54 NO-LABEL
     BRW_K4 AT ROW 16 COL 54.38
     SEL_K5 AT ROW 16 COL 66 NO-LABEL
     BRW_K5 AT ROW 16 COL 66.5
     RAD_FAST AT ROW 21.92 COL 1.88 NO-LABEL
     BTN_OK AT ROW 22.71 COL 91.38
     BTN_AVB AT ROW 22.71 COL 106.38
     FILL-IN_K1 AT ROW 15 COL 16 COLON-ALIGNED NO-LABEL
     FILL-IN_K2 AT ROW 15 COL 28 COLON-ALIGNED NO-LABEL
     FILL-IN_K3 AT ROW 15 COL 40 COLON-ALIGNED NO-LABEL
     FILL-IN_K4 AT ROW 15 COL 52 COLON-ALIGNED NO-LABEL
     FILL-IN_K5 AT ROW 15 COL 64 COLON-ALIGNED NO-LABEL
     RECT-24 AT ROW 14 COL 16.5
     SPACE(41.49) SKIP(1.78)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Kontosträng":L.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Temp-Tables and Buffers:
      TABLE: aokont T "?" NO-UNDO temp-db aokont
      TABLE: ktotemp T "?" NO-UNDO temp-db ktotemp
      TABLE: plankonttemp T "?" NO-UNDO temp-db plankonttemp
      TABLE: plkont T "?" NO-UNDO temp-db plkont
      TABLE: ? T "?" NO-UNDO temp-db kontotemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX DIALOG-2
   NOT-VISIBLE FRAME-NAME                                               */
/* BROWSE-TAB BRW_STR RECT-24 DIALOG-2 */
/* BROWSE-TAB BRW_AONRKTO BRW_STR DIALOG-2 */
/* BROWSE-TAB BRW_PLANKTO BRW_AONRKTO DIALOG-2 */
/* BROWSE-TAB BRW_K1 BTN_KONTO DIALOG-2 */
/* BROWSE-TAB BRW_K2 SEL_K2 DIALOG-2 */
/* BROWSE-TAB BRW_K3 SEL_K3 DIALOG-2 */
/* BROWSE-TAB BRW_K4 SEL_K4 DIALOG-2 */
/* BROWSE-TAB BRW_K5 SEL_K5 DIALOG-2 */
ASSIGN 
       FRAME DIALOG-2:SCROLLABLE       = FALSE
       FRAME DIALOG-2:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_K1 IN FRAME DIALOG-2
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN_K2 IN FRAME DIALOG-2
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN_K3 IN FRAME DIALOG-2
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN_K4 IN FRAME DIALOG-2
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN_K5 IN FRAME DIALOG-2
   NO-ENABLE                                                            */
/* SETTINGS FOR RADIO-SET RAD_FAST IN FRAME DIALOG-2
   NO-DISPLAY SHARED NO-ENABLE                                          */
ASSIGN 
       RAD_FAST:HIDDEN IN FRAME DIALOG-2           = TRUE.

/* SETTINGS FOR SELECTION-LIST SEL_K1 IN FRAME DIALOG-2
   NO-DISPLAY SHARED NO-ENABLE                                          */
ASSIGN 
       SEL_K1:HIDDEN IN FRAME DIALOG-2           = TRUE.

/* SETTINGS FOR SELECTION-LIST SEL_K2 IN FRAME DIALOG-2
   NO-DISPLAY SHARED NO-ENABLE                                          */
ASSIGN 
       SEL_K2:HIDDEN IN FRAME DIALOG-2           = TRUE.

/* SETTINGS FOR SELECTION-LIST SEL_K3 IN FRAME DIALOG-2
   NO-DISPLAY SHARED NO-ENABLE                                          */
ASSIGN 
       SEL_K3:HIDDEN IN FRAME DIALOG-2           = TRUE.

/* SETTINGS FOR SELECTION-LIST SEL_K4 IN FRAME DIALOG-2
   NO-DISPLAY SHARED NO-ENABLE                                          */
ASSIGN 
       SEL_K4:HIDDEN IN FRAME DIALOG-2           = TRUE.

/* SETTINGS FOR SELECTION-LIST SEL_K5 IN FRAME DIALOG-2
   NO-DISPLAY SHARED NO-ENABLE                                          */
ASSIGN 
       SEL_K5:HIDDEN IN FRAME DIALOG-2           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_AONRKTO
/* Query rebuild information for BROWSE BRW_AONRKTO
     _TblList          = "Temp-Tables.aokont"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.aokont.K1
"aokont.K1" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "9" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.aokont.K2
"aokont.K2" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "9" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.aokont.K3
"aokont.K3" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "9" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.aokont.K4
"aokont.K4" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "9" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.aokont.K5
"aokont.K5" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "9" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > Temp-Tables.aokont.SATS%
"aokont.SATS%" "%" ">99" "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_AONRKTO */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_K1
/* Query rebuild information for BROWSE BRW_K1
     _TblList          = "Temp-Tables.ktotemp"
     _Options          = "NO-LOCK "
     _Where[1]         = "Temp-Tables.ktotemp.KONTO = ""k1"""
     _FldNameList[1]   > Temp-Tables.ktotemp.KONTONR
"ktotemp.KONTONR" ? "x(256)" "character" ? ? ? ? ? ? no ? no no "9" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_K1 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_K2
/* Query rebuild information for BROWSE BRW_K2
     _TblList          = "Temp-Tables.ktotemp"
     _Options          = "NO-LOCK "
     _Where[1]         = "Temp-Tables.ktotemp.KONTO = ""k2"""
     _FldNameList[1]   > Temp-Tables.ktotemp.KONTONR
"ktotemp.KONTONR" ? "x(256)" "character" ? ? ? ? ? ? no ? no no "9" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_K2 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_K3
/* Query rebuild information for BROWSE BRW_K3
     _TblList          = "Temp-Tables.ktotemp"
     _Options          = "NO-LOCK "
     _Where[1]         = "Temp-Tables.ktotemp.KONTO = ""k3"""
     _FldNameList[1]   > Temp-Tables.ktotemp.KONTONR
"ktotemp.KONTONR" ? "x(256)" "character" ? ? ? ? ? ? no ? no no "9" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_K3 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_K4
/* Query rebuild information for BROWSE BRW_K4
     _TblList          = "Temp-Tables.ktotemp"
     _Options          = "NO-LOCK "
     _Where[1]         = "Temp-Tables.ktotemp.KONTO = ""k4"""
     _FldNameList[1]   > Temp-Tables.ktotemp.KONTONR
"ktotemp.KONTONR" ? "x(256)" "character" ? ? ? ? ? ? no ? no no "9" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_K4 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_K5
/* Query rebuild information for BROWSE BRW_K5
     _TblList          = "Temp-Tables.ktotemp"
     _Options          = "NO-LOCK "
     _Where[1]         = "Temp-Tables.ktotemp.KONTO = ""k5"""
     _FldNameList[1]   > Temp-Tables.ktotemp.KONTONR
"ktotemp.KONTONR" ? "x(256)" "character" ? ? ? ? ? ? no ? no no "9" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_K5 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_PLANKTO
/* Query rebuild information for BROWSE BRW_PLANKTO
     _TblList          = "Temp-Tables.plkont"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.plkont.K1
"plkont.K1" ? "x(256)" "character" ? ? ? ? ? ? no ? no no "9" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.plkont.K2
"plkont.K2" ? "x(256)" "character" ? ? ? ? ? ? no ? no no "9" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.plkont.K3
"plkont.K3" ? "x(256)" "character" ? ? ? ? ? ? no ? no no "9" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.plkont.K4
"plkont.K4" ? "x(256)" "character" ? ? ? ? ? ? no ? no no "9" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.plkont.K5
"plkont.K5" ? "x(256)" "character" ? ? ? ? ? ? no ? no no "9" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > Temp-Tables.plkont.SATS%
"plkont.SATS%" "%" ? "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_PLANKTO */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_STR
/* Query rebuild information for BROWSE BRW_STR
     _TblList          = "Temp-Tables.kontotemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.kontotemp.KKBENAMNING
"KKBENAMNING" "Benämning" "X(256)" "character" ? ? ? ? ? ? yes ? no no "42" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.kontotemp.KK1
"KK1" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "5" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.kontotemp.KK2
"KK2" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "5" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.kontotemp.KK3
"KK3" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "5" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.kontotemp.KK4
"KK4" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "5" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > Temp-Tables.kontotemp.KK5
"KK5" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "5" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_STR */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX DIALOG-2
/* Query rebuild information for DIALOG-BOX DIALOG-2
     _Options          = "NO-LOCK  KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX DIALOG-2 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME DIALOG-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-2 DIALOG-2
ON END-ERROR OF FRAME DIALOG-2 /* Kontosträng */
DO:
   IF RAD_FAST = FALSE THEN DO:
      IF vartvar = 1 THEN DO:
         FIND FIRST aokont WHERE aokont.AONR = aotemp.AONR AND
         aokont.DELNR = aotemp.DELNR  NO-LOCK NO-ERROR.
         IF NOT AVAILABLE aokont THEN DO:
            IF aotemp.OMRADE NE "" THEN DO:
               FIND FIRST omtemp NO-LOCK NO-ERROR.
               IF AVAILABLE omtemp AND omtemp.KONTOOB = TRUE THEN DO:
                  MESSAGE "Det måste finnas en kontosträng på varje " + LC(Guru.Konstanter:gaol) + "."
                  VIEW-AS ALERT-BOX.
                  RETURN NO-APPLY.
               END.
            END.
         END.
      END.
      IF vartvar = 2 THEN DO:
         FIND FIRST plkont WHERE plkont.PLANNR = plantemp.PLANNR AND
         plkont.ARTAL = plantemp.ARTAL  NO-LOCK NO-ERROR.
         IF NOT AVAILABLE plkont THEN DO:
            IF plantemp.OMRADE NE "" THEN DO:
               FIND FIRST omtemp NO-LOCK NO-ERROR.
               IF AVAILABLE omtemp AND omtemp.KONTOOB = TRUE THEN DO:
                  MESSAGE "Det måste finnas en kontosträng på varje " + LC(Guru.Konstanter:gpll) + "."
                  VIEW-AS ALERT-BOX.
                  RETURN NO-APPLY.
               END.
            END.
         END.
      END.
   END.
   ELSE DO:
      IF vartvar = 1 THEN DO:
         IF aotemp.OMRADE = "" THEN DO:
            RAD_FAST = RAD_FAST.
         END.
         ELSE DO:
            FIND FIRST aokont WHERE aokont.AONR = aotemp.AONR AND
            aokont.DELNR = aotemp.DELNR  NO-LOCK NO-ERROR.
            IF NOT AVAILABLE aokont THEN DO:
               FIND FIRST omtemp NO-LOCK NO-ERROR.
               IF AVAILABLE omtemp AND omtemp.KONTOOB = TRUE THEN DO:
                  MESSAGE "Det måste finnas en kontosträng på varje " + LC(Guru.Konstanter:gaol) + "."
                  VIEW-AS ALERT-BOX.
                  RETURN NO-APPLY.
               END.
            END.
         END.
      END.
      IF vartvar = 2 THEN DO:
         IF plantemp.OMRADE = "" THEN DO:
            RAD_FAST = RAD_FAST.
         END.
         ELSE DO:
            FIND FIRST plkont WHERE plkont.PLANNR = plantemp.PLANNR AND
            plkont.ARTAL = plantemp.ARTAL  NO-LOCK NO-ERROR.
            IF NOT AVAILABLE plkont THEN DO:
               FIND FIRST omtemp NO-LOCK NO-ERROR.
               IF AVAILABLE omtemp AND omtemp.KONTOOB = TRUE THEN DO:
                  MESSAGE "Det måste finnas en kontosträng på varje " + LC(Guru.Konstanter:gpll) + "."
                  VIEW-AS ALERT-BOX.
                  RETURN NO-APPLY.
               END.
            END.
         END.
      END.
      {BORTBRWPROC.I}
      RETURN.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-2 DIALOG-2
ON ENDKEY OF FRAME DIALOG-2 /* Kontosträng */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_AONRKTO
&Scoped-define SELF-NAME BRW_AONRKTO
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_AONRKTO DIALOG-2
ON MOUSE-SELECT-DBLCLICK OF BRW_AONRKTO IN FRAME DIALOG-2
DO:
   APPLY "CHOOSE" TO BTN_back.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME aokont.SATS%
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL aokont.SATS% BRW_AONRKTO _BROWSE-COLUMN DIALOG-2
ON LEAVE OF aokont.SATS% IN BROWSE BRW_AONRKTO /* % */
DO:
  IF AVAILABLE aokont THEN DO:
     aokont.SATS% = INPUT BROWSE BRW_AONRKTO aokont.SATS%.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_K1
&Scoped-define SELF-NAME BRW_K1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_K1 DIALOG-2
ON VALUE-CHANGED OF BRW_K1 IN FRAME DIALOG-2
DO:
   status-ok = BRW_K1:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
   APPLY "VALUE-CHANGED" TO BRW_K1.
   SEL_K1 = ktotemp.kontonr.
   BRW_K1:TOOLTIP = ktotemp.BENAMNING.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_K2
&Scoped-define SELF-NAME BRW_K2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_K2 DIALOG-2
ON VALUE-CHANGED OF BRW_K2 IN FRAME DIALOG-2
DO:
   status-ok = BRW_K2:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
   APPLY "VALUE-CHANGED" TO BRW_K2.
   SEL_K2 = ktotemp.kontonr.
   BRW_K2:TOOLTIP = ktotemp.BENAMNING.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_K3
&Scoped-define SELF-NAME BRW_K3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_K3 DIALOG-2
ON VALUE-CHANGED OF BRW_K3 IN FRAME DIALOG-2
DO:
  status-ok = BRW_K3:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
   APPLY "VALUE-CHANGED" TO BRW_K3.
   SEL_K3 = ktotemp.kontonr.
   BRW_K3:TOOLTIP = ktotemp.BENAMNING.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_K4
&Scoped-define SELF-NAME BRW_K4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_K4 DIALOG-2
ON VALUE-CHANGED OF BRW_K4 IN FRAME DIALOG-2
DO:
  status-ok = BRW_K4:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
   APPLY "VALUE-CHANGED" TO BRW_K4.
   SEL_K4 = ktotemp.kontonr.
   BRW_K4:TOOLTIP = ktotemp.BENAMNING.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_K5
&Scoped-define SELF-NAME BRW_K5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_K5 DIALOG-2
ON VALUE-CHANGED OF BRW_K5 IN FRAME DIALOG-2
DO:
  status-ok = BRW_K5:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
   APPLY "VALUE-CHANGED" TO BRW_K5.
   SEL_K5 = ktotemp.kontonr.
   BRW_K5:TOOLTIP = ktotemp.BENAMNING.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_PLANKTO
&Scoped-define SELF-NAME BRW_PLANKTO
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_PLANKTO DIALOG-2
ON MOUSE-SELECT-DBLCLICK OF BRW_PLANKTO IN FRAME DIALOG-2
DO:
   APPLY "CHOOSE" TO BTN_back.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_STR
&Scoped-define SELF-NAME BRW_STR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_STR DIALOG-2
ON MOUSE-SELECT-DBLCLICK OF BRW_STR IN FRAME DIALOG-2 /* Färdiga kontosträngar */
DO:
   APPLY "CHOOSE" TO BTN_OVER.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_STR DIALOG-2
ON VALUE-CHANGED OF BRW_STR IN FRAME DIALOG-2 /* Färdiga kontosträngar */
DO:
   IF AVAILABLE kontotemp THEN BRW_STR:TOOLTIP = kontotemp.KKBENAMNING.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB DIALOG-2
ON CHOOSE OF BTN_AVB IN FRAME DIALOG-2 /* Avbryt */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_back
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_back DIALOG-2
ON CHOOSE OF BTN_back IN FRAME DIALOG-2 /* BTN 2 */
DO:
   RUN fran_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_KONTO
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_KONTO DIALOG-2
ON CHOOSE OF BTN_KONTO IN FRAME DIALOG-2 /* BTN 9 */
DO:
   RUN konto_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK DIALOG-2
ON CHOOSE OF BTN_OK IN FRAME DIALOG-2 /* Ok */
DO:
   {muswait.i}
      
   RUN btnok_UI.
   {musarrow.i}
   IF musz = TRUE THEN DO:     
      musz = FALSE.
      RETURN NO-APPLY.
   END.
   ELSE DO:
      APPLY "GO" TO BTN_OK IN FRAME {&FRAME-NAME}.      
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK DIALOG-2
ON GO OF BTN_OK IN FRAME DIALOG-2 /* Ok */
DO:
   {BORTBRWPROC.I}
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_over
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_over DIALOG-2
ON CHOOSE OF BTN_over IN FRAME DIALOG-2 /* BTN 1 */
DO:
   IF vartvar = 1 THEN DO:
      FIND FIRST aotemp NO-LOCK NO-ERROR.
      FIND FIRST omtemp NO-LOCK NO-ERROR.
      antal_valda = BRW_STR:NUM-SELECTED-ROWS NO-ERROR.
      antal_raknare = 1.
      DO WHILE antal_raknare LE antal_valda TRANSACTION:
         status-ok = BRW_STR:FETCH-SELECTED-ROW(antal_raknare).
         CREATE aokont.
         ASSIGN
         aokont.AONR = aotemp.AONR
         aokont.DELNR = aotemp.DELNR
         aokont.K1 = kontotemp.KK1
         aokont.K2 = kontotemp.KK2
         aokont.K3 = kontotemp.KK3
         aokont.K4 = kontotemp.KK4
         aokont.K5 = kontotemp.KK5
         aokont.SATS% = 100.                
         CREATE exkontotemp.
         BUFFER-COPY kontotemp TO exkontotemp.
         DELETE kontotemp.
         RUN setlastrowid_UI IN brwproc[2] (INPUT ROWID(aokont)).
         RUN selnextprevrow_UI IN brwproc[1].
         antal_raknare = antal_raknare + 1.
      END.
      IF antal_valda > 0 THEN DO:
         RUN openbdyn_UI IN brwproc[1] (INPUT "").   
         RUN openbdyn_UI IN brwproc[2] (INPUT "").
         FIND FIRST aotemp NO-LOCK NO-ERROR.
         FIND FIRST omtemp NO-LOCK NO-ERROR.
         RUN lastselectdyn_UI IN brwproc[1].
         RUN lastselectdyn_UI IN brwproc[2].
      END.
   END.
   IF vartvar = 2 THEN DO:
      FIND FIRST plantemp NO-LOCK NO-ERROR.
      FIND FIRST omtemp NO-LOCK NO-ERROR.
      antal_valda = BRW_STR:NUM-SELECTED-ROWS NO-ERROR.
      antal_raknare = 1.
      DO WHILE antal_raknare LE antal_valda TRANSACTION:
         status-ok = BRW_STR:FETCH-SELECTED-ROW(antal_raknare).
         CREATE plkont.
         ASSIGN
         plkont.PLANNR = plantemp.PLANNR
         plkont.ARTAL = plantemp.ARTAL
         plkont.K1 = kontotemp.KK1
         plkont.K2 = kontotemp.KK2
         plkont.K3 = kontotemp.KK3
         plkont.K4 = kontotemp.KK4
         plkont.K5 = kontotemp.KK5
         plkont.SATS% = 100.
         IF Guru.Konstanter:globforetag = "CELPA" THEN DO:
            plkont.K4 = plantemp.PLANNR.
         END.
         CREATE exkontotemp.
         BUFFER-COPY kontotemp TO exkontotemp.
         DELETE kontotemp.
         RUN valrattright_UI IN brwproc[1] 
        (INPUT antal_raknare,INPUT TRUE,INPUT ROWID(kontotemp)).
         antal_raknare = antal_raknare + 1.
      END.
      IF antal_valda > 0 THEN DO:         
         RUN openbdyn_UI IN brwproc[1] (INPUT "").
         RUN openbdyn_UI IN brwproc[3] (INPUT "").
         FIND FIRST plantemp NO-LOCK NO-ERROR.
         FIND FIRST omtemp NO-LOCK NO-ERROR.
         RUN lastselectdyn_UI IN brwproc[1].                          
      END.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME SEL_K1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL SEL_K1 DIALOG-2
ON VALUE-CHANGED OF SEL_K1 IN FRAME DIALOG-2
DO:
   SEL_K1 = INPUT SEL_K1.
   FIND FIRST seltemp WHERE seltemp.KONTO = "K1" AND seltemp.KONTONR = SEL_K1 NO-ERROR.
   IF AVAILABLE seltemp THEN SEL_K1:TOOLTIP = seltemp.BENAMNING.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME SEL_K2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL SEL_K2 DIALOG-2
ON VALUE-CHANGED OF SEL_K2 IN FRAME DIALOG-2
DO:
   SEL_K2 = INPUT SEL_K2.
   FIND FIRST seltemp WHERE seltemp.KONTO = "K2" AND seltemp.KONTONR = SEL_K2 NO-ERROR.
   IF AVAILABLE seltemp THEN SEL_K2:TOOLTIP = seltemp.BENAMNING.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME SEL_K3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL SEL_K3 DIALOG-2
ON VALUE-CHANGED OF SEL_K3 IN FRAME DIALOG-2
DO:
   SEL_K3 = INPUT SEL_K3.
   FIND FIRST seltemp WHERE seltemp.KONTO = "K3" AND seltemp.KONTONR = SEL_K3 NO-ERROR.
   IF AVAILABLE seltemp THEN SEL_K3:TOOLTIP = seltemp.BENAMNING.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME SEL_K4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL SEL_K4 DIALOG-2
ON VALUE-CHANGED OF SEL_K4 IN FRAME DIALOG-2
DO:
   SEL_K4 = INPUT SEL_K4.
   FIND FIRST seltemp WHERE seltemp.KONTO = "K4" AND seltemp.KONTONR = SEL_K4 NO-ERROR.
   IF AVAILABLE seltemp THEN SEL_K4:TOOLTIP = seltemp.BENAMNING.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME SEL_K5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL SEL_K5 DIALOG-2
ON VALUE-CHANGED OF SEL_K5 IN FRAME DIALOG-2
DO:
   SEL_K5 = INPUT SEL_K5.
   FIND FIRST seltemp WHERE seltemp.KONTO = "K5" AND seltemp.KONTONR = SEL_K5 NO-ERROR.
   IF AVAILABLE seltemp THEN SEL_K5:TOOLTIP = seltemp.BENAMNING.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_AONRKTO
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK DIALOG-2 


/* ***************************  Main Block  *************************** */

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
   {DIA_M_START.I}
   {ALLSTARTDYN.I}
   /*Ladda områden*/
   {OMRHMT.I}
   EMPTY TEMP-TABLE plkont NO-ERROR. 
   EMPTY TEMP-TABLE kontotemp NO-ERROR. 
   EMPTY TEMP-TABLE aokont NO-ERROR. 
   EMPTY TEMP-TABLE kstrang NO-ERROR.
   EMPTY TEMP-TABLE ktotemp NO-ERROR. 
   EMPTY TEMP-TABLE kbtemp NO-ERROR. 
   EMPTY TEMP-TABLE jurtemp NO-ERROR. 
   EMPTY TEMP-TABLE aotemp NO-ERROR. 
   EMPTY TEMP-TABLE omtemp NO-ERROR. 
   vadgora = 1.   
   IF Guru.Konstanter:appcon THEN DO:
      RUN KBHMT.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
      (INPUT vadgora,INPUT Guru.Konstanter:globforetag,OUTPUT TABLE kbtemp,OUTPUT TABLE jurtemp).
   END.
   ELSE DO:
      RUN KBHMT.P
      (INPUT vadgora,INPUT Guru.Konstanter:globforetag,OUTPUT TABLE kbtemp,OUTPUT TABLE jurtemp).
   END.
   FIND FIRST kbtemp NO-ERROR.
   ASSIGN
   FILL-IN_K1 = kbtemp.K1
   FILL-IN_K2 = kbtemp.K2
   FILL-IN_K3 = kbtemp.K3
   FILL-IN_K4 = kbtemp.K4
   FILL-IN_K5 = kbtemp.K5.
   IF Guru.Konstanter:appcon THEN DO:
      RUN AOKONAPP.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
     (INPUT vartvar,INPUT aonrvar,INPUT delnrvar,INPUT plannrvar,INPUT artalvar,
      INPUT omrvar,INPUT RAD_FAST,INPUT Guru.Konstanter:globforetag,OUTPUT TABLE kontotemp,
      OUTPUT TABLE seltemp,OUTPUT TABLE aokont,OUTPUT TABLE aotemp,
      OUTPUT TABLE plkont,OUTPUT TABLE omtemp).
   END.
   ELSE DO:
      RUN AOKONAPP.P
      (INPUT vartvar,INPUT aonrvar,INPUT delnrvar,INPUT plannrvar,INPUT artalvar,
      INPUT omrvar,INPUT RAD_FAST,INPUT Guru.Konstanter:globforetag,OUTPUT TABLE kontotemp,
      OUTPUT TABLE seltemp,OUTPUT TABLE aokont,OUTPUT TABLE aotemp,
      OUTPUT TABLE plkont,OUTPUT TABLE omtemp).
   END.
   IF vartvar = 1 THEN DO:
      FOR EACH aokont:
         RUN extrakont_UI (INPUT TRUE).         
      END.
   END.
   IF vartvar = 2 THEN DO:
      FOR EACH plkont:
         RUN extrakont_UI (INPUT TRUE).         
      END.
   END.
   
   FIND FIRST omtemp NO-LOCK NO-ERROR.
   CREATE plantemp.
   ASSIGN
   plantemp.PLANNR = plannrvar
   plantemp.ARTAL = artalvar
   plantemp.OMRADE = omrvar.
   FIND FIRST aotemp NO-LOCK NO-ERROR.
   FIND FIRST plantemp NO-LOCK NO-ERROR.
   FIND FIRST omtemp NO-LOCK NO-ERROR.
   FIND FIRST kbtemp NO-LOCK NO-ERROR.
   ASSIGN
   aokont.K1:LABEL IN BROWSE BRW_AONRKTO = CAPS(SUBSTRING(kbtemp.K1,1,1)) + LC(SUBSTRING(kbtemp.K1,2))
   aokont.K2:LABEL IN BROWSE BRW_AONRKTO = CAPS(SUBSTRING(kbtemp.K2,1,1)) + LC(SUBSTRING(kbtemp.K2,2))
   aokont.K3:LABEL IN BROWSE BRW_AONRKTO = CAPS(SUBSTRING(kbtemp.K3,1,1)) + LC(SUBSTRING(kbtemp.K3,2))
   aokont.K4:LABEL IN BROWSE BRW_AONRKTO = CAPS(SUBSTRING(kbtemp.K4,1,1)) + LC(SUBSTRING(kbtemp.K4,2))
   aokont.K5:LABEL IN BROWSE BRW_AONRKTO = CAPS(SUBSTRING(kbtemp.K5,1,1)) + LC(SUBSTRING(kbtemp.K5,2))
   plkont.K1:LABEL IN BROWSE BRW_PLANKTO = CAPS(SUBSTRING(kbtemp.K1,1,1)) + LC(SUBSTRING(kbtemp.K1,2))  
   plkont.K2:LABEL IN BROWSE BRW_PLANKTO = CAPS(SUBSTRING(kbtemp.K2,1,1)) + LC(SUBSTRING(kbtemp.K2,2))
   plkont.K3:LABEL IN BROWSE BRW_PLANKTO = CAPS(SUBSTRING(kbtemp.K3,1,1)) + LC(SUBSTRING(kbtemp.K3,2))
   plkont.K4:LABEL IN BROWSE BRW_PLANKTO = CAPS(SUBSTRING(kbtemp.K4,1,1)) + LC(SUBSTRING(kbtemp.K4,2))
   plkont.K5:LABEL IN BROWSE BRW_PLANKTO = CAPS(SUBSTRING(kbtemp.K5,1,1)) + LC(SUBSTRING(kbtemp.K5,2))
   kontotemp.KK1:LABEL IN BROWSE BRW_STR = CAPS(SUBSTRING(kbtemp.K1,1,1)) + LC(SUBSTRING(kbtemp.K1,2))
   kontotemp.KK2:LABEL IN BROWSE BRW_STR = CAPS(SUBSTRING(kbtemp.K2,1,1)) + LC(SUBSTRING(kbtemp.K2,2))
   kontotemp.KK3:LABEL IN BROWSE BRW_STR = CAPS(SUBSTRING(kbtemp.K3,1,1)) + LC(SUBSTRING(kbtemp.K3,2))
   kontotemp.KK4:LABEL IN BROWSE BRW_STR = CAPS(SUBSTRING(kbtemp.K4,1,1)) + LC(SUBSTRING(kbtemp.K4,2))
   kontotemp.KK5:LABEL IN BROWSE BRW_STR = CAPS(SUBSTRING(kbtemp.K5,1,1)) + LC(SUBSTRING(kbtemp.K5,2)).
   vadgora = 1.
   IF Guru.Konstanter:appcon THEN DO:
      RUN KHMT.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
      (INPUT vadgora,INPUT Guru.Konstanter:globforetag,OUTPUT TABLE ktotemp).
   END.
   ELSE DO:
      RUN KHMT.P
      (INPUT vadgora,INPUT Guru.Konstanter:globforetag,OUTPUT TABLE ktotemp).
   END.
   IF vartvar = 1 THEN ASSIGN BRW_AONRKTO:TITLE IN FRAME {&FRAME-NAME} = "Kontosträngar lagrade på " + LC(Guru.Konstanter:gaok) + ":et".
   IF vartvar = 2 THEN ASSIGN BRW_PLANKTO:TITLE IN FRAME {&FRAME-NAME} = "Kontosträngar lagrade på " + LC(Guru.Konstanter:gplk) + " :et".
   RUN open_UI.
   ASSIGN SEL_K1:SCREEN-VALUE = " BLANK".
   ASSIGN SEL_K2:SCREEN-VALUE = " BLANK".
   ASSIGN SEL_K3:SCREEN-VALUE = " BLANK".
   ASSIGN SEL_K4:SCREEN-VALUE = " BLANK".
   ASSIGN SEL_K5:SCREEN-VALUE = " BLANK".
   {musarrow.i}
   RUN enable_UI.
   {FRMSIZED.I}
   RUN mainladda_UI.
   FOR EACH kontotemp:
      CREATE exkontotemp.
      BUFFER-COPY kontotemp TO exkontotemp.
   END.
   RUN openbdyn_UI IN brwproc[1] (INPUT "").   
   
   {DIA_M_SLUT.I}
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
   ASSIGN
   kontotemp.KKBENAMNING:READ-ONLY IN BROWSE BRW_STR = TRUE.    
   RUN DYNBRW.P PERSISTENT SET brwproc[1] 
      (INPUT BRW_STR:HANDLE IN FRAME {&FRAME-NAME}).
   RUN DYNBRW.P PERSISTENT SET brwproc[2] 
      (INPUT BRW_AONRKTO:HANDLE IN FRAME {&FRAME-NAME}).
   RUN DYNBRW.P PERSISTENT SET brwproc[3] 
      (INPUT BRW_PLANKTO:HANDLE IN FRAME {&FRAME-NAME}).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE appat_UI DIALOG-2 
PROCEDURE appat_UI :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
   IF vartvar = 1 THEN DO:
      ASSIGN omrvar = omtemp.OMRADE.
   END.
   IF Guru.Konstanter:appcon THEN DO:
      RUN AOKUPPD.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
      (INPUT vartvar,INPUT vadgora,INPUT ?,INPUT omrvar,OUTPUT benvar,
      INPUT-OUTPUT aonrvar,INPUT-OUTPUT delnrvar,INPUT-OUTPUT plannrvar,
      INPUT-OUTPUT artalvar,INPUT-OUTPUT TABLE plankonttemp,
      INPUT-OUTPUT TABLE aokont).
   END.
   ELSE DO:
      RUN AOKUPPD.P
      (INPUT vartvar,INPUT vadgora,INPUT ?,INPUT omrvar,OUTPUT benvar,
      INPUT-OUTPUT aonrvar,INPUT-OUTPUT delnrvar,INPUT-OUTPUT plannrvar,
      INPUT-OUTPUT artalvar,INPUT-OUTPUT TABLE plankonttemp,
      INPUT-OUTPUT TABLE aokont).
   END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnok_UI DIALOG-2 
PROCEDURE btnok_UI :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
   IF vartvar = 1 THEN DO:        
      FIND FIRST aotemp NO-LOCK NO-ERROR.
      FIND FIRST omtemp NO-LOCK NO-ERROR.
      IF RAD_FAST = FALSE THEN DO:
         FIND FIRST aokont WHERE aokont.AONR = aotemp.AONR AND
         aokont.DELNR = aotemp.DELNR  NO-LOCK NO-ERROR.
         IF NOT AVAILABLE aokont THEN DO:
            FIND FIRST omtemp NO-LOCK NO-ERROR.
            IF AVAILABLE omtemp THEN DO:
               IF AVAILABLE omtemp AND omtemp.KONTOOB = TRUE THEN DO:
                  MESSAGE "Det måste finnas en kontosträng på varje " + LC(Guru.Konstanter:gaol) + "."
                  VIEW-AS ALERT-BOX.
                  musz = TRUE.
                  RETURN NO-APPLY.
               END.
            END.            
         END.
         ELSE DO:
            IF Guru.Konstanter:globforetag = "SUND"  THEN DO:
               IF aokont.K5 = "" THEN DO:
                  MESSAGE "Det måste finnas NÄT i kontosträngen."
                  VIEW-AS ALERT-BOX.
                  musz = TRUE.
                  RETURN NO-APPLY.                  
               END.     
            END.
         END.   
      END.
      ELSE DO:
         IF aotemp.OMRADE = "" THEN DO:
            RAD_FAST = RAD_FAST.
         END.
         ELSE DO:
            FIND FIRST aokont WHERE aokont.AONR = aotemp.AONR AND
            aokont.DELNR = aotemp.DELNR  NO-LOCK NO-ERROR.
            IF NOT AVAILABLE aokont THEN DO:
               FIND FIRST omtemp NO-LOCK NO-ERROR.
               IF AVAILABLE omtemp AND omtemp.KONTOOB = TRUE THEN DO:
                  MESSAGE "Det måste finnas en kontosträng på varje " + LC(Guru.Konstanter:gaol) + "."
                  VIEW-AS ALERT-BOX.
                  musz = TRUE.
                  RETURN NO-APPLY.
               END.
            END.
         END.
      END.
      FIND FIRST aotemp WHERE aotemp.AONR = aonrvar AND 
      aotemp.DELNR = delnrvar NO-LOCK NO-ERROR.
      antal_raknare = 0.
      FIND FIRST omtemp NO-LOCK NO-ERROR.
      GET FIRST BRW_AONRKTO NO-LOCK.
      DO WHILE AVAILABLE(aokont):
         antal_raknare = antal_raknare + 1.
         GET NEXT BRW_AONRKTO NO-LOCK.
      END.
      IF antal_raknare > 0 THEN DO:
         GET FIRST BRW_AONRKTO NO-LOCK.
         DO WHILE AVAILABLE(aokont):
            IF aokont.SATS% = 100 THEN DO TRANSACTION:
               GET CURRENT BRW_AONRKTO EXCLUSIVE-LOCK.
               aokont.SATS% = aokont.SATS% / antal_raknare.
            END.
            GET NEXT BRW_AONRKTO NO-LOCK.
         END.
         RUN refreshbrw_UI IN brwproc[2].
         antal_raknare = 0.
         GET FIRST BRW_AONRKTO NO-LOCK.
         DO WHILE AVAILABLE(aokont):
            antal_raknare = antal_raknare + aokont.SATS%.
            GET NEXT BRW_AONRKTO NO-LOCK.
         END.
         IF AVAILABLE omtemp THEN DO:
            IF omtemp.KONTOOB = FALSE AND antal_raknare = 0  THEN musz = musz.
            ELSE IF antal_raknare ne 100 THEN DO:
               MESSAGE "Summan av fördelningarna måste alltid bli 100." VIEW-AS ALERT-BOX.
               musz = TRUE.
               RETURN NO-APPLY.
            END.
         END.
      END.
      musz = FALSE.
      OPEN QUERY BRW_AONRKTO FOR EACH aokont NO-LOCK.
      GET FIRST BRW_AONRKTO NO-LOCK.
      DO WHILE AVAILABLE(aokont):
         FIND FIRST aoktoinnan WHERE 
         aoktoinnan.K1 = aokont.K1 AND
         aoktoinnan.K2 = aokont.K2 AND 
         aoktoinnan.K3 = aokont.K3 AND 
         aoktoinnan.K4 = aokont.K4 AND 
         aoktoinnan.K5 = aokont.K5 AND 
         aoktoinnan.SATS% = aokont.SATS% NO-LOCK NO-ERROR.
         IF NOT AVAILABLE aoktoinnan THEN musz = TRUE.
         GET NEXT BRW_AONRKTO NO-LOCK.
      END.
      FOR EACH aoktoinnan:
         FIND FIRST aokont WHERE 
         aokont.K1 = aoktoinnan.K1 AND
         aokont.K2 = aoktoinnan.K2 AND 
         aokont.K3 = aoktoinnan.K3 AND 
         aokont.K4 = aoktoinnan.K4 AND 
         aokont.K5 = aoktoinnan.K5 AND 
         aokont.SATS% = aoktoinnan.SATS% NO-LOCK NO-ERROR.
         IF NOT AVAILABLE aokont THEN musz = TRUE.
      END.
      IF musz = TRUE THEN DO:
         musz = FALSE.
         vadgora = 5.
         RUN appat_UI.         
         IF Guru.Konstanter:appcon THEN DO:
            RUN TLAGAUTO.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
            (INPUT Guru.Konstanter:globanv,INPUT aonrrec,INPUT "AOKONTO").
         END.
         ELSE DO:
            RUN TLAGAUTO.P 
            (INPUT Guru.Konstanter:globanv,INPUT aonrrec,INPUT "AOKONTO").
         END.
      END.
   END.
   IF vartvar = 2 THEN DO:
      FIND FIRST plantemp NO-LOCK NO-ERROR.
      FIND FIRST omtemp NO-LOCK NO-ERROR.
      IF RAD_FAST = FALSE THEN DO:
         FIND FIRST plkont WHERE plkont.PLANNR = plantemp.PLANNR AND
         plkont.ARTAL = plantemp.ARTAL  NO-LOCK NO-ERROR.
         IF NOT AVAILABLE plkont THEN DO:
            FIND FIRST omtemp NO-LOCK NO-ERROR.
            IF AVAILABLE omtemp THEN DO:
               IF AVAILABLE omtemp AND omtemp.KONTOOB = TRUE THEN DO:
                  MESSAGE "Det måste finnas en kontosträng på varje " + LC(Guru.Konstanter:gpll) + "."
                  VIEW-AS ALERT-BOX.
                  musz = TRUE.
                  RETURN NO-APPLY.
               END.
            END.
         END.
      END.
      ELSE DO:
         IF plantemp.OMRADE = "" THEN DO:
            RAD_FAST = RAD_FAST.
         END.
         ELSE DO:
            FIND FIRST plkont WHERE plkont.PLANNR = plantemp.PLANNR AND
            plkont.ARTAL = plantemp.ARTAL  NO-LOCK NO-ERROR.
            IF NOT AVAILABLE plkont THEN DO:
               FIND FIRST omtemp NO-LOCK NO-ERROR.
               IF AVAILABLE omtemp AND omtemp.KONTOOB = TRUE THEN DO:
                  MESSAGE "Det måste finnas en kontosträng på varje " + LC(Guru.Konstanter:gpll) + "."
                  VIEW-AS ALERT-BOX.
                  musz = TRUE.
                  RETURN NO-APPLY.
               END.
            END.
         END.
      END.
      FIND plantemp WHERE plantemp.PLANNR = plannrvar AND
      plantemp.ARTAL = artalvar NO-LOCK NO-ERROR.
      antal_raknare = 0.
      FIND FIRST omtemp NO-LOCK NO-ERROR.
      GET FIRST BRW_PLANKTO NO-LOCK.
      DO WHILE AVAILABLE(plkont):
         antal_raknare = antal_raknare + 1.
         GET NEXT BRW_PLANKTO NO-LOCK.
      END.
      IF antal_raknare > 0 THEN DO:
         GET FIRST BRW_PLANKTO NO-LOCK.
         DO WHILE AVAILABLE(plkont):
            IF plkont.SATS% = 100 THEN DO TRANSACTION:
               GET CURRENT BRW_PLANKTO EXCLUSIVE-LOCK.
               plkont.SATS% = plkont.SATS% / antal_raknare.
            END.
            GET NEXT BRW_PLANKTO NO-LOCK.
         END.
         RUN refreshbrw_UI IN brwproc[3].
         antal_raknare = 0.
         GET FIRST BRW_PLANKTO NO-LOCK.
         DO WHILE AVAILABLE(plkont):
            antal_raknare = antal_raknare + plkont.SATS%.
            GET NEXT BRW_PLANKTO NO-LOCK.
         END.
         IF AVAILABLE omtemp THEN DO:
            IF omtemp.KONTOOB = FALSE AND antal_raknare = 0  THEN musz = musz.
            ELSE IF antal_raknare ne 100 THEN DO:
               MESSAGE "Summan av fördelningarna måste alltid bli 100." VIEW-AS ALERT-BOX.
               musz = TRUE.
               RETURN NO-APPLY.
            END.
         END.
      END.
      musz = FALSE.
      GET FIRST BRW_PLANKTO NO-LOCK.
      DO WHILE AVAILABLE(plkont):
         FIND FIRST planktoinnan WHERE 
         planktoinnan.K1 = plkont.K1 AND
         planktoinnan.K2 = plkont.K2 AND 
         planktoinnan.K3 = plkont.K3 AND 
         planktoinnan.K4 = plkont.K4 AND 
         planktoinnan.K5 = plkont.K5 AND 
         planktoinnan.SATS% = plkont.SATS% NO-LOCK NO-ERROR.
         IF NOT AVAILABLE planktoinnan THEN musz = TRUE.
         GET NEXT BRW_PLANKTO NO-LOCK.
      END.
      FOR EACH planktoinnan:
         FIND FIRST plkont WHERE 
         plkont.K1 = planktoinnan.K1 AND
         plkont.K2 = planktoinnan.K2 AND 
         plkont.K3 = planktoinnan.K3 AND 
         plkont.K4 = planktoinnan.K4 AND 
         plkont.K5 = planktoinnan.K5 AND 
         plkont.SATS% = planktoinnan.SATS% NO-LOCK NO-ERROR.
         IF NOT AVAILABLE plkont THEN musz = TRUE.
      END.
      IF musz = TRUE THEN DO:
         vadgora = 5.
         EMPTY TEMP-TABLE plankonttemp NO-ERROR. 
         FOR EACH plkont NO-LOCK:
            CREATE plankonttemp.
            BUFFER-COPY plkont TO plankonttemp.
         END.
         RUN appat_UI.
         musz = FALSE.
      END.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI DIALOG-2  _DEFAULT-ENABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY FILL-IN_K1 FILL-IN_K2 FILL-IN_K3 FILL-IN_K4 FILL-IN_K5 
      WITH FRAME DIALOG-2.
  ENABLE RECT-24 BRW_STR BRW_AONRKTO BRW_PLANKTO BTN_over BTN_back BTN_KONTO 
         BRW_K1 BRW_K2 BRW_K3 BRW_K4 BRW_K5 BTN_OK BTN_AVB 
      WITH FRAME DIALOG-2.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-2}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE emedd_UI DIALOG-2 
PROCEDURE emedd_UI :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
   
   DEFINE INPUT PARAMETER medpers AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER prmeddel AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER amnevar AS CHARACTER NO-UNDO.
   RUN EPOST.P (INPUT "", INPUT medpers, INPUT amnevar,
   INPUT prmeddel, INPUT "", INPUT "",INPUT Guru.Konstanter:globforetag, OUTPUT skick, OUTPUT efel).   
   IF efel NE "" THEN MESSAGE efel VIEW-AS ALERT-BOX.

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
  DISPLAY FILL-IN_K1 FILL-IN_K2 FILL-IN_K3 FILL-IN_K4 FILL-IN_K5 
      WITH FRAME DIALOG-2.
  ENABLE RECT-24 BRW_STR BRW_AONRKTO BRW_PLANKTO BTN_over BTN_back BTN_KONTO 
         BRW_K1 BRW_K2 BRW_K3 BRW_K4 BRW_K5 BTN_OK BTN_AVB 
      WITH FRAME DIALOG-2.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-2}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE extrakont_UI DIALOG-2 
PROCEDURE extrakont_UI :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER delvar AS LOGICAL NO-UNDO.
   IF vartvar = 1 THEN DO:
      IF Guru.Konstanter:globforetag = "CSUND"  THEN DO:
         FIND FIRST kontotemp WHERE 
         kontotemp.KK1 = aokont.K1 AND 
         kontotemp.KK2 = aokont.K2 AND
         kontotemp.KK3 = aokont.K3 AND 
         kontotemp.KK5 = aokont.K5 NO-ERROR.
      END.
      ELSE DO:
         FIND FIRST kontotemp WHERE 
         kontotemp.KK1 = aokont.K1 AND 
         kontotemp.KK2 = aokont.K2 AND
         kontotemp.KK3 = aokont.K3 AND 
         kontotemp.KK4 = aokont.K4 AND
         kontotemp.KK5 = aokont.K5 NO-ERROR.
      END.
      IF delvar = FALSE THEN DO: 
         IF AVAILABLE kontotemp THEN RETURN.
         ELSE DO:
            IF Guru.Konstanter:globforetag = "cSUND"  THEN DO:
               FIND FIRST exkontotemp WHERE 
               exkontotemp.KK1 = aokont.K1 AND 
               exkontotemp.KK2 = aokont.K2 AND
               exkontotemp.KK3 = aokont.K3 AND 
               exkontotemp.KK5 = aokont.K5
               NO-ERROR.
            END.
            ELSE DO:
               FIND FIRST exkontotemp WHERE 
               exkontotemp.KK1 = aokont.K1 AND 
               exkontotemp.KK2 = aokont.K2 AND
               exkontotemp.KK3 = aokont.K3 AND 
               exkontotemp.KK4 = aokont.K4 AND
               exkontotemp.KK5 = aokont.K5 NO-ERROR.
            END.
            IF AVAILABLE exkontotemp THEN DO:
               CREATE kontotemp.
               BUFFER-COPY exkontotemp TO kontotemp.
            END.
         END.
      END.      
   END.
   ELSE DO:
      IF Guru.Konstanter:globforetag = "cSUND"  THEN DO:
         FIND FIRST kontotemp WHERE 
         kontotemp.KK1 = plkont.K1 AND 
         kontotemp.KK2 = plkont.K2 AND
         kontotemp.KK3 = plkont.K3 AND 
         kontotemp.KK5 = plkont.K5 NO-ERROR.
      END.
      ELSE DO:
         FIND FIRST kontotemp WHERE 
         kontotemp.KK1 = plkont.K1 AND 
         kontotemp.KK2 = plkont.K2 AND
         kontotemp.KK3 = plkont.K3 AND 
         kontotemp.KK4 = plkont.K4 AND
         kontotemp.KK5 = plkont.K5 NO-ERROR.
      END.
      IF delvar = FALSE THEN DO: 
         IF AVAILABLE kontotemp THEN RETURN.
         ELSE DO:
            IF Guru.Konstanter:globforetag = "cSUND"  THEN DO:
               FIND FIRST exkontotemp WHERE 
               exkontotemp.KK1 = plkont.K1 AND 
               exkontotemp.KK2 = plkont.K2 AND
               exkontotemp.KK3 = plkont.K3 AND 
               exkontotemp.KK5 = plkont.K5 NO-ERROR.
            END.
            ELSE DO:
               FIND FIRST exkontotemp WHERE 
               exkontotemp.KK1 = plkont.K1 AND 
               exkontotemp.KK2 = plkont.K2 AND
               exkontotemp.KK3 = plkont.K3 AND 
               exkontotemp.KK4 = plkont.K4 AND
               exkontotemp.KK5 = plkont.K5 NO-ERROR.
            END.
            IF AVAILABLE exkontotemp THEN DO:
               CREATE kontotemp.
               BUFFER-COPY exkontotemp TO kontotemp.
            END.
         END.
      END.
   END.
   IF delvar = TRUE THEN DO:
      IF AVAILABLE kontotemp THEN DO:
         CREATE exkontotemp.
         BUFFER-COPY kontotemp TO exkontotemp.
         DELETE kontotemp.
      END.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fran_UI DIALOG-2 
PROCEDURE fran_UI :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
   IF vartvar = 1 THEN DO:
      antal_valda = BRW_AONRKTO:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME}.
      antal_raknare = 1.
      DO WHILE antal_raknare LE antal_valda:
         status-ok = BRW_AONRKTO:FETCH-SELECTED-ROW(antal_raknare).       
         RUN extrakont_UI (INPUT FALSE).
         IF Guru.Konstanter:globforetag = "cSUND" THEN DO:
            aokont.K4 = "".
         END.
         IF AVAILABLE aokont THEN DELETE aokont.
         RUN selnextprevrow_UI IN brwproc[2].
        
         antal_raknare = antal_raknare + 1.
      END.
      RUN openbdyn_UI IN brwproc[1] (INPUT "").
      RUN openbdyn_UI IN brwproc[2] (INPUT "").      
      RUN lastselectdyn_UI IN brwproc[2].
   END.
   IF vartvar = 2 THEN DO:
      antal_valda = BRW_PLANKTO:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME}.
      antal_raknare = 1.
      DO WHILE antal_raknare LE antal_valda:
         status-ok = BRW_PLANKTO:FETCH-SELECTED-ROW(antal_raknare).
         RUN extrakont_UI (INPUT FALSE).
         IF Guru.Konstanter:globforetag = "cSUND"  THEN DO:
            plkont.K4 = "".
         END.       
         IF AVAILABLE plkont THEN DELETE plkont.
         RUN valrattright_UI IN brwproc[3] 
         (INPUT antal_raknare,INPUT TRUE,INPUT ROWID(plkont)).

         antal_raknare = antal_raknare + 1.
      END.
      RUN openbdyn_UI IN brwproc[1] (INPUT "").
      RUN openbdyn_UI IN brwproc[3] (INPUT "").
      RUN lastselectdyn_UI IN brwproc[3].
      
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE konto_UI DIALOG-2 
PROCEDURE konto_UI :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
   IF kbtemp.K1 NE "" THEN DO:
      status-ok = BRW_K1:SELECT-FOCUSED-ROW()  IN FRAME {&FRAME-NAME} NO-ERROR.
      SEL_K1 = ktotemp.kontonr.
   END.
   IF kbtemp.K2 NE "" THEN DO:
      status-ok = BRW_K2:SELECT-FOCUSED-ROW() NO-ERROR.
      SEL_K2 = ktotemp.kontonr.
   END.
   ELSE SEL_K2 = "".
   IF kbtemp.K3 NE "" THEN DO:
      status-ok = BRW_K3:SELECT-FOCUSED-ROW() NO-ERROR.
      SEL_K3 = ktotemp.kontonr.
   END.
   ELSE SEL_K3 = "".
   IF kbtemp.K4 NE "" THEN DO:
      status-ok = BRW_K4:SELECT-FOCUSED-ROW() NO-ERROR.
      SEL_K4 = ktotemp.kontonr.
   END.
   ELSE SEL_K4 = "".
   IF kbtemp.K5 NE "" THEN DO:
      status-ok = BRW_K5:SELECT-FOCUSED-ROW() NO-ERROR.
      SEL_K5 = ktotemp.kontonr.
   END.
   ELSE SEL_K5 = "".
   IF SEL_K1 = " BLANK" THEN ASSIGN SEL_K1 = "" .
   ELSE IF SEL_K1 = ? THEN ASSIGN SEL_K1 = "" .
   IF SEL_K2 = " BLANK" THEN ASSIGN SEL_K2 = "" .
   ELSE IF SEL_K2 = ? THEN ASSIGN SEL_K2 = "" .
   IF SEL_K3 = " BLANK" THEN ASSIGN SEL_K3 = "" .
   ELSE IF SEL_K3 = ? THEN ASSIGN SEL_K3 = "" .
   IF SEL_K4 = " BLANK" THEN ASSIGN SEL_K4 = "" .
   ELSE IF SEL_K4 = ? THEN ASSIGN SEL_K4 = "" .
   IF SEL_K5 = " BLANK" THEN ASSIGN SEL_K5 = "" .
   ELSE IF SEL_K5 = ? THEN ASSIGN SEL_K5 = "" .
   
   /*AONR*/
   IF vartvar = 1 THEN DO:
      FIND FIRST aotemp NO-LOCK NO-ERROR.
      FIND FIRST omtemp NO-LOCK NO-ERROR.
      IF Guru.Konstanter:globforetag = "CELPA" THEN DO:
         SEL_K4 = aotemp.AONR.
      END.
      IF SEL_K1 + SEL_K2 + SEL_K3 + SEL_K4 + SEL_K5 = "" THEN DO:
         MESSAGE "Alla fält kan inte vara blanka!" VIEW-AS ALERT-BOX.
         RETURN.
      END.
      FIND FIRST aokont WHERE
      aokont.AONR = aotemp.AONR AND
      aokont.DELNR = aotemp.DELNR AND
      aokont.K1 = SEL_K1 AND
      aokont.K2 = SEL_K2 AND
      aokont.K3 = SEL_K3 AND
      aokont.K4 = SEL_K4 AND
      aokont.K5 = SEL_K5 NO-LOCK NO-ERROR.   
      IF AVAILABLE aokont THEN DO:
         MESSAGE Guru.Konstanter:gaol + ":et har redan denna kontosträng! " VIEW-AS ALERT-BOX.
         RETURN NO-APPLY.
      END.
      ELSE IF SEL_K1 = "" AND (Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" )  THEN DO:
         MESSAGE Guru.Konstanter:gaol + ":et kan inte ha blank i första kontofältet! " VIEW-AS ALERT-BOX.
         RETURN NO-APPLY.
      END.
      ELSE DO TRANSACTION:
         CREATE aokont.
         ASSIGN
         aokont.AONR = aotemp.AONR
         aokont.DELNR = aotemp.DELNR
         aokont.K1 = SEL_K1
         aokont.K2 = SEL_K2
         aokont.K3 = SEL_K3
         aokont.K4 = SEL_K4
         aokont.K5 = SEL_K5
         aokont.SATS% = 100.
         
         FIND FIRST kontotemp WHERE 
         kontotemp.KK1 = aokont.K1 AND
         kontotemp.KK2 = aokont.K2 AND
         kontotemp.KK3 = aokont.K3 AND
         kontotemp.KK4 = SEL_K4 AND
         kontotemp.KK5 = aokont.K5 USE-INDEX konttemp NO-LOCK NO-ERROR.
         IF NOT AVAILABLE kontotemp THEN DO :
            RUN KONTSPAR.W (INPUT aonrvar,INPUT delnrvar,INPUT plannrvar,INPUT artalvar).
            CREATE exkontotemp.
            ASSIGN                    
            exkontotemp.KK1 = aokont.K1
            exkontotemp.KK2 = aokont.K2
            exkontotemp.KK3 = aokont.K3
            exkontotemp.KK4 = aokont.K4
            exkontotemp.KK5 = aokont.K5               
            exkontotemp.KKBENAMNING = benvar.
         END.
         ELSE DO:
            DELETE kontotemp.
         END.

      END.
      RUN openbdyn_UI IN brwproc[1] (INPUT "").
      RUN openbdyn_UI IN brwproc[2] (INPUT "").
      FIND FIRST aotemp NO-LOCK NO-ERROR.
      FIND FIRST omtemp NO-LOCK NO-ERROR.
      APPLY "ENTRY" TO BRW_AONRKTO IN FRAME {&FRAME-NAME}.
   END.
   /*PLANNR*/
   IF vartvar = 2 THEN DO:
      FIND FIRST plantemp NO-LOCK NO-ERROR.
      FIND FIRST omtemp NO-LOCK NO-ERROR.
      IF Guru.Konstanter:globforetag = "CELPA" THEN DO:
         SEL_K4 = plantemp.PLANNR.
      END.
      IF SEL_K1 + SEL_K2 + SEL_K3 + SEL_K4 + SEL_K5 = "" THEN DO:
         MESSAGE "Alla fält kan inte vara blanka!" VIEW-AS ALERT-BOX.
         RETURN.
      END.
      FIND FIRST plkont WHERE
      plkont.PLANNR = plantemp.PLANNR  AND
      plkont.ARTAL = plantemp.ARTAL AND
      plkont.K1 = SEL_K1 AND
      plkont.K2 = SEL_K2 AND
      plkont.K3 = SEL_K3 AND
      plkont.K4 = SEL_K4 AND
      plkont.K5 = SEL_K5 NO-LOCK NO-ERROR.
      IF AVAILABLE plkont THEN DO:
         MESSAGE Guru.Konstanter:gpll + ":et har redan denna kontosträng! " VIEW-AS ALERT-BOX.
         RETURN NO-APPLY.
      END.
      ELSE IF SEL_K1 = "" AND ( Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV") THEN DO:
         MESSAGE Guru.Konstanter:gpll + ":et kan inte ha blank i första kontofältet! " VIEW-AS ALERT-BOX.
         RETURN NO-APPLY.
      END.
      ELSE DO:
         CREATE plkont.
         ASSIGN
         plkont.PLANNR = plantemp.PLANNR
         plkont.ARTAL = plantemp.ARTAL
         plkont.K1 = SEL_K1
         plkont.K2 = SEL_K2
         plkont.K3 = SEL_K3
         plkont.K4 = SEL_K4
         plkont.K5 = SEL_K5
         plkont.SATS% = 100.
         IF Guru.Konstanter:globforetag = "CELPA" THEN DO:
            plkont.K4 = plantemp.PLANNR.
         END.
         FIND FIRST kontotemp WHERE 
         kontotemp.KK1 = plkont.K1 AND
         kontotemp.KK2 = plkont.K2 AND
         kontotemp.KK3 = plkont.K3 AND
         kontotemp.KK4 = SEL_K4 AND
         kontotemp.KK5 = plkont.K5 USE-INDEX konttemp NO-LOCK NO-ERROR.
         IF NOT AVAILABLE kontotemp THEN DO :
            RUN KONTSPAR.W (INPUT aonrvar,INPUT delnrvar,INPUT plannrvar,INPUT artalvar).
            /*ggg*/
            CREATE exkontotemp.
            ASSIGN                    
            exkontotemp.KK1 = plkont.K1
            exkontotemp.KK2 = plkont.K2
            exkontotemp.KK3 = plkont.K3
            exkontotemp.KK4 = plkont.K4
            exkontotemp.KK5 = plkont.K5               
            exkontotemp.KKBENAMNING = benvar.
         END.
         ELSE DO:
            DELETE kontotemp. 
         END.
      END.
      RUN openbdyn_UI IN brwproc[1] (INPUT "").
      RUN openbdyn_UI IN brwproc[3] (INPUT "").
      FIND FIRST plantemp NO-LOCK NO-ERROR.
      FIND FIRST omtemp NO-LOCK NO-ERROR.
      APPLY "ENTRY" TO BRW_PLANKTO IN FRAME {&FRAME-NAME}.
   END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE mainladda_UI DIALOG-2 
PROCEDURE mainladda_UI :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
   IF vartvar = 1 THEN DO:
      ENABLE BRW_AONRKTO WITH FRAME {&FRAME-NAME}.
      BRW_PLANKTO:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
      OPEN QUERY BRW_AONRKTO FOR EACH aokont NO-LOCK.
      GET FIRST BRW_AONRKTO NO-LOCK.
      DO WHILE AVAILABLE (aokont):
         CREATE aoktoinnan.
         ASSIGN
         aoktoinnan.AONR = aokont.AONR
         aoktoinnan.DELNR = aokont.DELNR
         aoktoinnan.K1 = aokont.K1
         aoktoinnan.K2 = aokont.K2
         aoktoinnan.K3 = aokont.K3
         aoktoinnan.K4 = aokont.K4
         aoktoinnan.K5 = aokont.K5
         aoktoinnan.SATS% = aokont.SATS%.
         GET NEXT BRW_AONRKTO NO-LOCK.
      END.
      OPEN QUERY BRW_AONRKTO FOR EACH aokont NO-LOCK.
   END.
   IF vartvar = 2 THEN DO:
      ENABLE BRW_PLANKTO WITH FRAME {&FRAME-NAME}.
      BRW_AONRKTO:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
      OPEN QUERY BRW_PLANKTO FOR EACH plkont WHERE 
      plkont.PLANNR = plannrvar AND 
      plkont.ARTAL = artalvar NO-LOCK.
      GET FIRST BRW_PLANKTO NO-LOCK.
      DO WHILE AVAILABLE (plkont):
         CREATE planktoinnan.
         ASSIGN
         planktoinnan.PLANNR = plkont.PLANNR
         planktoinnan.ARTAL = plkont.ARTAL
         planktoinnan.K1 = plkont.K1
         planktoinnan.K2 = plkont.K2
         planktoinnan.K3 = plkont.K3
         planktoinnan.K4 = plkont.K4
         planktoinnan.K5 = plkont.K5
         planktoinnan.SATS% = plkont.SATS%.
         GET NEXT BRW_PLANKTO NO-LOCK.
      END.
      OPEN QUERY BRW_PLANKTO FOR EACH plkont NO-LOCK.      
   END.
   IF Guru.Konstanter:globforetag = "CELPA" THEN DO:
      ASSIGN
      FILL-IN_K4:HIDDEN = TRUE
      SEL_K4:HIDDEN = TRUE
      BRW_K4:HIDDEN = TRUE.
   END.
   IF kbtemp.K1 = "" THEN DO:
      ASSIGN
      FILL-IN_K1:HIDDEN = TRUE
      SEL_K1:HIDDEN = TRUE
      BRW_K1:HIDDEN = TRUE.
   END.
   IF kbtemp.K2 = "" THEN DO:
      ASSIGN
      FILL-IN_K2:HIDDEN = TRUE
      SEL_K2:HIDDEN = TRUE
      BRW_K2:HIDDEN = TRUE.
   END.
   IF kbtemp.K3 = "" THEN DO:
      ASSIGN
      FILL-IN_K3:HIDDEN = TRUE
      SEL_K3:HIDDEN = TRUE
      BRW_K3:HIDDEN = TRUE.
   END.
   IF kbtemp.K4 = "" THEN DO:
      ASSIGN
      FILL-IN_K4:HIDDEN = TRUE
      SEL_K4:HIDDEN = TRUE
      BRW_K4:HIDDEN = TRUE.
   END.
   IF kbtemp.K5 = "" THEN DO:
      ASSIGN
      FILL-IN_K4:HIDDEN = TRUE
      SEL_K5:HIDDEN = TRUE
      BRW_K5:HIDDEN = TRUE.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE open_UI DIALOG-2 
PROCEDURE open_UI :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
   vadgora = 2.
   IF Guru.Konstanter:appcon THEN DO:
      RUN KSTHMT.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
      (INPUT vadgora,INPUT Guru.Konstanter:globforetag,INPUT omtemp.AVDELNINGNR,INPUT-OUTPUT TABLE kstrang,OUTPUT jid).
   END.
   ELSE DO:
      RUN KSTHMT.P
      (INPUT vadgora,INPUT Guru.Konstanter:globforetag,INPUT omtemp.AVDELNINGNR, INPUT-OUTPUT TABLE kstrang,OUTPUT jid).
   END.
   CREATE ktotemp.
   ASSIGN
   ktotemp.KONTO = "K1"
   ktotemp.KONTONR = " Blank"
   ktotemp.JUDID = jid.
   CREATE ktotemp.
   ASSIGN
   ktotemp.KONTO = "K2"
   ktotemp.KONTONR = " Blank"
   ktotemp.JUDID = jid.
   CREATE ktotemp.
   ASSIGN
   ktotemp.KONTO = "K3"
   ktotemp.KONTONR = " Blank"
   ktotemp.JUDID = jid.
   CREATE ktotemp.
   ASSIGN
   ktotemp.KONTO = "K4"
   ktotemp.KONTONR = " Blank"
   ktotemp.JUDID = jid.
   CREATE ktotemp.
   ASSIGN
   ktotemp.KONTO = "K5"
   ktotemp.KONTONR = " Blank"
   ktotemp.JUDID = jid.
   OPEN QUERY BRW_K1 FOR EACH ktotemp WHERE ktotemp.JUDID = jid AND ktotemp.KONTO = "K1" NO-LOCK.
   OPEN QUERY BRW_K2 FOR EACH ktotemp WHERE ktotemp.JUDID = jid AND ktotemp.KONTO = "K2" NO-LOCK.
   OPEN QUERY BRW_K3 FOR EACH ktotemp WHERE ktotemp.JUDID = jid AND ktotemp.KONTO = "K3" NO-LOCK.
   OPEN QUERY BRW_K4 FOR EACH ktotemp WHERE ktotemp.JUDID = jid AND ktotemp.KONTO = "K4" NO-LOCK.
   OPEN QUERY BRW_K5 FOR EACH ktotemp WHERE ktotemp.JUDID = jid AND ktotemp.KONTO = "K5" NO-LOCK.   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

