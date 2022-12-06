&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME DIALOG-3


/* Temp-Table and Buffer definitions                                    */



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS DIALOG-3 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 95/05/04 -  1:19 pm

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEFINE INPUT PARAMETER infakplannr AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER faktypvar AS INTEGER NO-UNDO.
/* Local Variable Definitions ---                                       */
{ALLDEF.I}
&Scoped-define NEW 
{GLOBVAR2DEL1.I}
{ORGPRIS.I}
{EXTRATAB.I}  
{KUNDTYP.I}
&Scoped-define NEW 
&Scoped-define SHARED 

DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE fakthmth AS HANDLE NO-UNDO.
DEFINE SHARED VARIABLE fbestapph AS HANDLE NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE fragakoll  AS LOGICAL NO-UNDO.
DEFINE VARIABLE my1hand AS WIDGET-HANDL NO-UNDO.
DEFINE VARIABLE brwsortvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE defrow AS ROWID EXTENT 4 NO-UNDO.
DEFINE VARIABLE fbok AS LOGICAL NO-UNDO.
DEFINE TEMP-TABLE fbtemp NO-UNDO
   FIELD NAMN AS CHARACTER
   FIELD ANV AS CHARACTER
   INDEX ANV ANV
   INDEX NAMN NAMN.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DIALOG-3
&Scoped-define BROWSE-NAME BRW_BEF

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES kundbeftemp defpristemp kundovertemp

/* Definitions for BROWSE BRW_BEF                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_BEF kundbeftemp.NAMN kundbeftemp.PRISA ~
kundbeftemp.PRISRES 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_BEF kundbeftemp.NAMN 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_BEF kundbeftemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_BEF kundbeftemp
&Scoped-define QUERY-STRING-BRW_BEF FOR EACH kundbeftemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_BEF OPEN QUERY BRW_BEF FOR EACH kundbeftemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_BEF kundbeftemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_BEF kundbeftemp


/* Definitions for BROWSE BRW_DPRISBEF                                  */
&Scoped-define FIELDS-IN-QUERY-BRW_DPRISBEF defpristemp.PRISNUM ~
defpristemp.PRISTEXT 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_DPRISBEF defpristemp.PRISNUM 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_DPRISBEF defpristemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_DPRISBEF defpristemp
&Scoped-define QUERY-STRING-BRW_DPRISBEF FOR EACH defpristemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_DPRISBEF OPEN QUERY BRW_DPRISBEF FOR EACH defpristemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_DPRISBEF defpristemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_DPRISBEF defpristemp


/* Definitions for BROWSE BRW_DPRISOVER                                 */
&Scoped-define FIELDS-IN-QUERY-BRW_DPRISOVER defpristemp.PRISNUM ~
defpristemp.PRISTEXT 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_DPRISOVER defpristemp.PRISNUM 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_DPRISOVER defpristemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_DPRISOVER defpristemp
&Scoped-define QUERY-STRING-BRW_DPRISOVER FOR EACH defpristemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_DPRISOVER OPEN QUERY BRW_DPRISOVER FOR EACH defpristemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_DPRISOVER defpristemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_DPRISOVER defpristemp


/* Definitions for BROWSE BRW_KUNDO                                     */
&Scoped-define FIELDS-IN-QUERY-BRW_KUNDO kundovertemp.OTEXT ~
kundovertemp.DAGTYP kundovertemp.START kundovertemp.SLUT kundovertemp.PRISA 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_KUNDO kundovertemp.OTEXT 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_KUNDO kundovertemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_KUNDO kundovertemp
&Scoped-define QUERY-STRING-BRW_KUNDO FOR EACH kundovertemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_KUNDO OPEN QUERY BRW_KUNDO FOR EACH kundovertemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_KUNDO kundovertemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_KUNDO kundovertemp


/* Definitions for FRAME FRAME-OVER                                     */

/* Definitions for FRAME FRAME-PRISBEF                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RAD_VAL BTN_OK BTN_AVB 
&Scoped-Define DISPLAYED-OBJECTS RAD_VAL 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB 
     LABEL "Avbryt":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_OK 
     LABEL "Ok":L 
     SIZE 14 BY 1.

DEFINE VARIABLE RAD_VAL AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Grundregistreringar", 1
     SIZE 77 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_FB AS CHARACTER FORMAT "X(256)":U 
     LABEL "Faktura beställare" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Ingen" 
     DROP-DOWN-LIST
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_KOST AS CHARACTER FORMAT "X(14)":U 
     LABEL "Kostnadsregistrering" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Automatiska","Manuellt","Inga" 
     DROP-DOWN-LIST
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_KUND AS CHARACTER FORMAT "X(256)":U 
     LABEL "Kundtyp" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "CCC" 
     DROP-DOWN-LIST
     SIZE 17.5 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_LON AS CHARACTER FORMAT "X(256)":U 
     LABEL "Lönetillägg" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Endast utvalda löne.","Inga" 
     DROP-DOWN-LIST
     SIZE 23.5 BY 1 TOOLTIP "Endast utvalda lönetillägg" NO-UNDO.

DEFINE VARIABLE CMB_MTRL AS CHARACTER FORMAT "X(14)":U 
     LABEL "Materiel" 
     VIEW-AS COMBO-BOX INNER-LINES 1
     LIST-ITEMS "Inga" 
     DROP-DOWN-LIST
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_OVER AS CHARACTER FORMAT "X(14)":U 
     LABEL "Övertid" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Tidredovisning","Egna regler","Inga" 
     DROP-DOWN-LIST
     SIZE 23.5 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_OVR AS CHARACTER FORMAT "X(14)":U 
     LABEL "Övriga kostnader" 
     VIEW-AS COMBO-BOX INNER-LINES 1
     LIST-ITEMS "Inga" 
     DROP-DOWN-LIST
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_TIM AS CHARACTER FORMAT "X(14)":U 
     LABEL "Timpris" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Tidredovisning","Kundens egna","Inga" 
     DROP-DOWN-LIST
     SIZE 23.5 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_TRA AS CHARACTER FORMAT "X(14)":U 
     LABEL "Traktamente" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Tidredovisning","Eget pris","Inga" 
     DROP-DOWN-LIST
     SIZE 23.5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-MOTPART AS CHARACTER FORMAT "X(256)":U 
     LABEL "Motpart" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN_ENTRAK AS DECIMAL FORMAT "->>>>9.99" INITIAL 0 
     LABEL "Endagsförrättning i kr" 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1.

DEFINE VARIABLE FILL-IN_FAKINT AS INTEGER FORMAT ">>9" INITIAL 0 
     LABEL "Antal dagar mellan fakturering" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1.

DEFINE VARIABLE FILL-IN_FDAGAR AS INTEGER FORMAT ">>9" INITIAL 0 
     LABEL "Förfallodagar" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1.

DEFINE VARIABLE FILL-IN_FLERTRAK AS DECIMAL FORMAT "->>>>9.99" INITIAL 0 
     LABEL "Flerdygnsförrättning i kr" 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1.

DEFINE VARIABLE FILL-IN_FRTJPA AS DECIMAL FORMAT "->>9" INITIAL 0 
     LABEL "Fr. tjänst i %" 
     VIEW-AS FILL-IN 
     SIZE 6.75 BY 1.

DEFINE VARIABLE FILL-IN_MIL AS DECIMAL FORMAT "->>>>9.99" INITIAL 0 
     LABEL "Milersättning kr/mil" 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1.

DEFINE VARIABLE FILL-IN_MTRLPA AS DECIMAL FORMAT "->>9" INITIAL 0 
     LABEL "Materiel i %" 
     VIEW-AS FILL-IN 
     SIZE 6.75 BY 1.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_BEF FOR 
      kundbeftemp SCROLLING.

DEFINE QUERY BRW_DPRISBEF FOR 
      defpristemp SCROLLING.

DEFINE QUERY BRW_DPRISOVER FOR 
      defpristemp SCROLLING.

DEFINE QUERY BRW_KUNDO FOR 
      kundovertemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_BEF
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_BEF DIALOG-3 _STRUCTURED
  QUERY BRW_BEF NO-LOCK DISPLAY
      kundbeftemp.NAMN FORMAT "x(20)":U
      kundbeftemp.PRISA COLUMN-LABEL "Tim pris" FORMAT "->>>>9.99":U
      kundbeftemp.PRISRES COLUMN-LABEL "Res pris" FORMAT "->>>>9.99":U
  ENABLE
      kundbeftemp.NAMN
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 42.38 BY 15
         TITLE "Priser".

DEFINE BROWSE BRW_DPRISBEF
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_DPRISBEF DIALOG-3 _STRUCTURED
  QUERY BRW_DPRISBEF NO-LOCK DISPLAY
      defpristemp.PRISNUM COLUMN-LABEL "Prislista" FORMAT "->>>>>>9":U
      defpristemp.PRISTEXT COLUMN-LABEL "Text" FORMAT "X(25)":U
  ENABLE
      defpristemp.PRISNUM
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 37.75 BY 15
         TITLE "Valbara prislistor".

DEFINE BROWSE BRW_DPRISOVER
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_DPRISOVER DIALOG-3 _STRUCTURED
  QUERY BRW_DPRISOVER NO-LOCK DISPLAY
      defpristemp.PRISNUM COLUMN-LABEL "Pris!lista" FORMAT "->>>>>>9":U
      defpristemp.PRISTEXT COLUMN-LABEL "Benämning" FORMAT "X(25)":U
  ENABLE
      defpristemp.PRISNUM
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 37 BY 15
         TITLE "Övertidsprislistor".

DEFINE BROWSE BRW_KUNDO
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_KUNDO DIALOG-3 _STRUCTURED
  QUERY BRW_KUNDO NO-LOCK DISPLAY
      kundovertemp.OTEXT COLUMN-LABEL "Övertid" FORMAT "X(25)":U
      kundovertemp.DAGTYP COLUMN-LABEL "Dagtyp" FORMAT "X(8)":U
      kundovertemp.START COLUMN-LABEL "Start!tid" FORMAT "99.99":U
            COLUMN-FGCOLOR 1 LABEL-FGCOLOR 1
      kundovertemp.SLUT COLUMN-LABEL "Slut!tid" FORMAT "99.99":U
            COLUMN-FGCOLOR 3 LABEL-FGCOLOR 3
      kundovertemp.PRISA COLUMN-LABEL "Pris" FORMAT "->>>>9.99":U
  ENABLE
      kundovertemp.OTEXT
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 57.38 BY 15
         TITLE "Övertidsprislista".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-3
     RAD_VAL AT ROW 1.38 COL 1.5 NO-LABEL
     BTN_OK AT ROW 25.58 COL 72
     BTN_AVB AT ROW 25.58 COL 87
     SPACE(1.12) SKIP(0.24)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Faktura":L.

DEFINE FRAME FRAME-OVER
     BRW_DPRISOVER AT ROW 5 COL 1.5
     BRW_KUNDO AT ROW 5 COL 40.13
     "Då det normala övertidsavtalet ej ska gälla." VIEW-AS TEXT
          SIZE 51.75 BY 1.46 AT ROW 3.25 COL 1.5
          FONT 17
     "Uppläggning av övertidsregler för fakturering:" VIEW-AS TEXT
          SIZE 67.5 BY 2.04 AT ROW 1.5 COL 1.5
          FONT 17
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.5 ROW 2.75
         SIZE 98.5 BY 20.75.

DEFINE FRAME FRAME-PRISBEF
     BRW_DPRISBEF AT ROW 5 COL 1.5
     BRW_BEF AT ROW 5 COL 41.13
     "Välj prislista." VIEW-AS TEXT
          SIZE 41 BY 2 AT ROW 1.67 COL 1.5
          FONT 17
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.5 ROW 2.75
         SIZE 99.5 BY 22.5.

DEFINE FRAME FRAME-REGLER
     CMB_KUND AT ROW 2.92 COL 29.13 COLON-ALIGNED
     FILL-IN-MOTPART AT ROW 2.92 COL 78.38 COLON-ALIGNED
     FILL-IN_FDAGAR AT ROW 4.08 COL 29.13 COLON-ALIGNED
     FILL-IN_FAKINT AT ROW 4.08 COL 78.38 COLON-ALIGNED
     CMB_TIM AT ROW 6.67 COL 29.13 COLON-ALIGNED
     CMB_KOST AT ROW 6.67 COL 78.38 COLON-ALIGNED
     CMB_OVER AT ROW 7.83 COL 29.13 COLON-ALIGNED
     CMB_MTRL AT ROW 7.83 COL 78.38 COLON-ALIGNED
     CMB_LON AT ROW 9 COL 29.13 COLON-ALIGNED
     CMB_OVR AT ROW 9 COL 78.38 COLON-ALIGNED
     CMB_TRA AT ROW 10.17 COL 29.13 COLON-ALIGNED
     CMB_FB AT ROW 10.17 COL 78.38 COLON-ALIGNED
     FILL-IN_ENTRAK AT ROW 12.63 COL 29.13 COLON-ALIGNED
     FILL-IN_FLERTRAK AT ROW 13.83 COL 29.13 COLON-ALIGNED
     FILL-IN_MIL AT ROW 13.83 COL 78.38 COLON-ALIGNED
     FILL-IN_FRTJPA AT ROW 16.58 COL 29.13 COLON-ALIGNED
     FILL-IN_MTRLPA AT ROW 16.58 COL 78.38 COLON-ALIGNED
     "Procentpåslag för:" VIEW-AS TEXT
          SIZE 25.88 BY 1 AT ROW 15.5 COL 1.5
          FONT 17
     "Uppläggning av regler för fakturering:" VIEW-AS TEXT
          SIZE 43.63 BY 1.13 AT ROW 1.58 COL 1.5
          FONT 17
     "Ange varifrån de olika uppgifterna skall hämtas:" VIEW-AS TEXT
          SIZE 55.13 BY 1.17 AT ROW 5.29 COL 1.5
          FONT 17
     "Ersättning:" VIEW-AS TEXT
          SIZE 16.88 BY 1.08 AT ROW 11.33 COL 1.5
          FONT 17
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.5 ROW 2.75
         SIZE 99.55 BY 21.5.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Temp-Tables and Buffers:
      TABLE: defpristemp T "?" NO-UNDO temp-db defpristemp
      TABLE: kundbeftemp T "?" NO-UNDO temp-db kundbeftemp
      TABLE: kundovertemp T "?" NO-UNDO temp-db kundovertemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* REPARENT FRAME */
ASSIGN FRAME FRAME-OVER:FRAME = FRAME DIALOG-3:HANDLE
       FRAME FRAME-PRISBEF:FRAME = FRAME DIALOG-3:HANDLE
       FRAME FRAME-REGLER:FRAME = FRAME DIALOG-3:HANDLE.

/* SETTINGS FOR DIALOG-BOX DIALOG-3
   NOT-VISIBLE FRAME-NAME                                               */

DEFINE VARIABLE XXTABVALXX AS LOGICAL NO-UNDO.

ASSIGN XXTABVALXX = FRAME FRAME-REGLER:MOVE-AFTER-TAB-ITEM (RAD_VAL:HANDLE IN FRAME DIALOG-3)
       XXTABVALXX = FRAME FRAME-PRISBEF:MOVE-BEFORE-TAB-ITEM (BTN_OK:HANDLE IN FRAME DIALOG-3)
       XXTABVALXX = FRAME FRAME-OVER:MOVE-BEFORE-TAB-ITEM (FRAME FRAME-PRISBEF:HANDLE)
       XXTABVALXX = FRAME FRAME-REGLER:MOVE-BEFORE-TAB-ITEM (FRAME FRAME-OVER:HANDLE)
/* END-ASSIGN-TABS */.

ASSIGN 
       FRAME DIALOG-3:SCROLLABLE       = FALSE
       FRAME DIALOG-3:HIDDEN           = TRUE.

/* SETTINGS FOR FRAME FRAME-OVER
                                                                        */
/* BROWSE-TAB BRW_DPRISOVER TEXT-6 FRAME-OVER */
/* BROWSE-TAB BRW_KUNDO BRW_DPRISOVER FRAME-OVER */
/* SETTINGS FOR FRAME FRAME-PRISBEF
                                                                        */
/* BROWSE-TAB BRW_DPRISBEF TEXT-7 FRAME-PRISBEF */
/* BROWSE-TAB BRW_BEF BRW_DPRISBEF FRAME-PRISBEF */
/* SETTINGS FOR FRAME FRAME-REGLER
                                                                        */
/* SETTINGS FOR COMBO-BOX CMB_FB IN FRAME FRAME-REGLER
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       CMB_FB:HIDDEN IN FRAME FRAME-REGLER           = TRUE.

/* SETTINGS FOR COMBO-BOX CMB_MTRL IN FRAME FRAME-REGLER
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       CMB_MTRL:HIDDEN IN FRAME FRAME-REGLER           = TRUE.

/* SETTINGS FOR COMBO-BOX CMB_OVR IN FRAME FRAME-REGLER
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       CMB_OVR:HIDDEN IN FRAME FRAME-REGLER           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-MOTPART IN FRAME FRAME-REGLER
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN-MOTPART:HIDDEN IN FRAME FRAME-REGLER           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_BEF
/* Query rebuild information for BROWSE BRW_BEF
     _TblList          = "Temp-Tables.kundbeftemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.kundbeftemp.NAMN
"kundbeftemp.NAMN" ? ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.kundbeftemp.PRISA
"kundbeftemp.PRISA" "Tim pris" "->>>>9.99" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.kundbeftemp.PRISRES
"kundbeftemp.PRISRES" "Res pris" "->>>>9.99" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_BEF */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_DPRISBEF
/* Query rebuild information for BROWSE BRW_DPRISBEF
     _TblList          = "Temp-Tables.defpristemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.defpristemp.PRISNUM
"defpristemp.PRISNUM" "Prislista" ? "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.defpristemp.PRISTEXT
"defpristemp.PRISTEXT" "Text" "X(25)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_DPRISBEF */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_DPRISOVER
/* Query rebuild information for BROWSE BRW_DPRISOVER
     _TblList          = "Temp-Tables.defpristemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.defpristemp.PRISNUM
"defpristemp.PRISNUM" "Pris!lista" ? "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.defpristemp.PRISTEXT
"defpristemp.PRISTEXT" "Benämning" "X(25)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_DPRISOVER */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_KUNDO
/* Query rebuild information for BROWSE BRW_KUNDO
     _TblList          = "Temp-Tables.kundovertemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.kundovertemp.OTEXT
"kundovertemp.OTEXT" "Övertid" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.kundovertemp.DAGTYP
"kundovertemp.DAGTYP" "Dagtyp" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.kundovertemp.START
"kundovertemp.START" "Start!tid" ? "decimal" ? 1 ? ? 1 ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.kundovertemp.SLUT
"kundovertemp.SLUT" "Slut!tid" ? "decimal" ? 3 ? ? 3 ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.kundovertemp.PRISA
"kundovertemp.PRISA" "Pris" "->>>>9.99" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_KUNDO */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX DIALOG-3
/* Query rebuild information for DIALOG-BOX DIALOG-3
     _Options          = "NO-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX DIALOG-3 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-OVER
/* Query rebuild information for FRAME FRAME-OVER
     _Query            is NOT OPENED
*/  /* FRAME FRAME-OVER */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-PRISBEF
/* Query rebuild information for FRAME FRAME-PRISBEF
     _Query            is NOT OPENED
*/  /* FRAME FRAME-PRISBEF */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME DIALOG-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-3 DIALOG-3
ON END-ERROR OF FRAME DIALOG-3 /* Faktura */
DO:
   {muswait.i}
    {BORTBRWPROC.I}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-3 DIALOG-3
ON ENDKEY OF FRAME DIALOG-3 /* Faktura */
DO:
   APPLY "END-ERROR":U TO FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_BEF
&Scoped-define FRAME-NAME FRAME-PRISBEF
&Scoped-define SELF-NAME BRW_BEF
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_BEF DIALOG-3
ON ENTRY OF BRW_BEF IN FRAME FRAME-PRISBEF /* Priser */
DO:
   RUN tabstop_UI (INPUT TRUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_DPRISBEF
&Scoped-define SELF-NAME BRW_DPRISBEF
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_DPRISBEF DIALOG-3
ON VALUE-CHANGED OF BRW_DPRISBEF IN FRAME FRAME-PRISBEF /* Valbara prislistor */
DO:         
   IF fragakoll = FALSE THEN DO:
      IF AVAILABLE defpristemp THEN defrow[2] = ROWID(defpristemp).      
   END.
   status-ok = BRW_DPRISBEF:SELECT-FOCUSED-ROW() IN FRAME FRAME-PRISBEF NO-ERROR.
   IF NOT AVAILABLE defpristemp THEN RETURN NO-APPLY.  
   
   RUN nylista_UI (INPUT 1).      
   fragakoll = TRUE.
   IF musz = TRUE THEN DO: 
      musz = FALSE.      
   END.
   ELSE defrow[2] = ROWID(defpristemp).
   RUN setlastrowid_UI IN brwproc[2] (INPUT defrow[2]).              
   RUN lastselectdyn_UI IN brwproc[2].             
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_DPRISOVER
&Scoped-define FRAME-NAME FRAME-OVER
&Scoped-define SELF-NAME BRW_DPRISOVER
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_DPRISOVER DIALOG-3
ON VALUE-CHANGED OF BRW_DPRISOVER IN FRAME FRAME-OVER /* Övertidsprislistor */
DO:         
   IF fragakoll = FALSE THEN DO:
      IF AVAILABLE defpristemp THEN defrow[3] = ROWID(defpristemp).      
   END.
   status-ok = BRW_DPRISOVER:SELECT-FOCUSED-ROW() IN FRAME FRAME-OVER NO-ERROR.
   IF NOT AVAILABLE defpristemp THEN RETURN NO-APPLY.  
   
   RUN nylista_UI (INPUT 2).      
   fragakoll = TRUE.
   IF musz = TRUE THEN DO: 
      musz = FALSE.      
   END.
   ELSE defrow[3] = ROWID(defpristemp).
   RUN setlastrowid_UI IN brwproc[3] (INPUT defrow[3]).              
   RUN lastselectdyn_UI IN brwproc[3].            
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_KUNDO
&Scoped-define SELF-NAME BRW_KUNDO
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_KUNDO DIALOG-3
ON ENTRY OF BRW_KUNDO IN FRAME FRAME-OVER /* Övertidsprislista */
DO:
   RUN tabstop_UI (INPUT TRUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_KUNDO DIALOG-3
ON MOUSE-SELECT-DBLCLICK OF BRW_KUNDO IN FRAME FRAME-OVER /* Övertidsprislista */
DO:
   RUN andra_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DIALOG-3
&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB DIALOG-3
ON CHOOSE OF BTN_AVB IN FRAME DIALOG-3 /* Avbryt */
DO:
   APPLY "END-ERROR":U TO FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK DIALOG-3
ON CHOOSE OF BTN_OK IN FRAME DIALOG-3 /* Ok */
DO:
   musz = FALSE.
   
   ASSIGN   
   CMB_KOST = INPUT FRAME FRAME-REGLER CMB_KOST
   CMB_LON = INPUT CMB_LON 
   CMB_MTRL = INPUT CMB_MTRL
   CMB_OVER = INPUT CMB_OVER
   CMB_OVR = INPUT CMB_OVR
   CMB_TIM = INPUT CMB_TIM
   CMB_TRA = INPUT CMB_TRA
   CMB_KUND = INPUT CMB_KUND  
   CMB_FB = INPUT CMB_FB
   FILL-IN-MOTPART = INPUT FILL-IN-MOTPART
   FILL-IN_FRTJPA = INPUT FILL-IN_FRTJPA
   FILL-IN_MTRLPA = INPUT FILL-IN_MTRLPA
   FILL-IN_FAKINT = INPUT FILL-IN_FAKINT  
   
   
   FILL-IN_ENTRAK = INPUT FILL-IN_ENTRAK
   FILL-IN_FLERTRAK = INPUT FILL-IN_FLERTRAK 
   FILL-IN_MIL = INPUT FILL-IN_MIL
   FILL-IN_FDAGAR = INPUT FRAME FRAME-REGLER FILL-IN_FDAGAR.
   EMPTY TEMP-TABLE inextrakopptemp NO-ERROR.    
   CREATE inextrakopptemp.          
   ASSIGN
   inextrakopptemp.PROGRAM     = "FBFPLAN"           
   inextrakopptemp.KOPPLACHAR1 = ?                      
   inextrakopptemp.KOPPLAINT1  =  infakplannr
   inextrakopptemp.KOPPLACHAR2 = ?            
   inextrakopptemp.KOPPLAINT2 =  ?.   
   RUN exbort_UI IN fbestapph (INPUT TABLE inextrakopptemp).
   EMPTY TEMP-TABLE inextrakopptemp NO-ERROR. 
   IF CMB_FB = "" THEN.
   ELSE IF CMB_FB NE "Ingen" THEN DO:
      FIND FIRST fbtemp WHERE fbtemp.NAMN = CMB_FB NO-LOCK NO-ERROR.
      CREATE inextrakopptemp.          
      ASSIGN
      inextrakopptemp.PROGRAM     = "FBFPLAN"            
      inextrakopptemp.KOPPLACHAR1 = ?                         
      inextrakopptemp.KOPPLAINT1  =  infakplannr
      inextrakopptemp.KOPPLACHAR2 = fbtemp.ANV
      inextrakopptemp.KOPPLAINT2 =  ?.   
      inextrakopptemp.SOKCHAR[1] = fbtemp.ANV.
      RUN sparaextra_UI IN fbestapph (INPUT TABLE inextrakopptemp).
   END.
   
   FIND FIRST kundtyptemp WHERE kundtyptemp.KUNDTYP = CMB_KUND NO-LOCK NO-ERROR.   
   FIND FIRST kundregeltemp WHERE kundregeltemp.FAKTNR = infakplannr
   NO-ERROR.
   IF NOT AVAILABLE kundregeltemp THEN DO:
      FIND FIRST kundregeltemp NO-ERROR.
   END.
   IF FILL-IN_FAKINT = 0 THEN DO:
      MESSAGE "Faktureringsintervallet måste vara mer än 0 dagar." VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.      
   END.  
   IF CMB_LON = "Endast utvalda löne." THEN kundregeltemp.LONRGL = "ENDAST MILERSÄTTNING".
   ELSE kundregeltemp.LONRGL = CMB_LON.
   ASSIGN
   kundregeltemp.FAKTNR = infakplannr
   kundregeltemp.KOSTREGRGL = CMB_KOST      
   /*kundregeltemp.MTRLRGL = CMB_MTRL
   kundregeltemp.OVRKRGL = CMB_OVR*/   
   kundregeltemp.OVERTIDRGL  = CMB_OVER
   kundregeltemp.TIMRGL = CMB_TIM
   kundregeltemp.TRAKTRGL = CMB_TRA
   kundregeltemp.FAKINT = FILL-IN_FAKINT
   kundregeltemp.ENTRAK = FILL-IN_ENTRAK
   kundregeltemp.FLERTRAK = FILL-IN_FLERTRAK 
   kundregeltemp.MIL = FILL-IN_MIL
   kundregeltemp.FRTJPA = FILL-IN_FRTJPA
   kundregeltemp.MTRLPA = FILL-IN_MTRLPA
   kundregeltemp.FDAGAR = FILL-IN_FDAGAR
   kundregeltemp.KUNDID = kundtyptemp.KUNDID.                                      
   FOR EACH kundbeftemp:
      kundbeftemp.FAKTNR = infakplannr.
   END.
   FOR EACH kundovertemp:
      kundovertemp.FAKTNR = infakplannr.
   END.
   FOR EACH kundprislisttemp:
      kundprislisttemp.FAKTNR = infakplannr.
   END.

   RUN regprispara_UI IN fakthmth (INPUT infakplannr,INPUT TABLE kundregeltemp,INPUT TABLE kundbeftemp,
   INPUT TABLE kundovertemp,INPUT TABLE kundprislisttemp,OUTPUT TABLE felmeddtemp).                   
   FIND FIRST felmeddtemp NO-ERROR.
   IF AVAILABLE felmeddtemp THEN DO:
      MESSAGE felmeddtemp.FELMEDD VIEW-AS ALERT-BOX.
      DELETE felmeddtemp.
      RETURN NO-APPLY.
   END.  
   {BORTBRWPROC.I}
   APPLY "GO" TO FRAME {&FRAME-NAME}.         
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-REGLER
&Scoped-define SELF-NAME CMB_KUND
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_KUND DIALOG-3
ON ENTRY OF CMB_KUND IN FRAME FRAME-REGLER /* Kundtyp */
DO:
   RUN tabstop_UI (INPUT TRUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-MOTPART
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-MOTPART DIALOG-3
ON ENTRY OF FILL-IN-MOTPART IN FRAME FRAME-REGLER /* Motpart */
DO:
   RUN tabstop_UI (INPUT TRUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-MOTPART DIALOG-3
ON LEAVE OF FILL-IN-MOTPART IN FRAME FRAME-REGLER /* Motpart */
DO:
   
   FILL-IN-MOTPART = INPUT FILL-IN-MOTPART.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_MTRLPA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_MTRLPA DIALOG-3
ON ENTRY OF FILL-IN_MTRLPA IN FRAME FRAME-REGLER /* Materiel i % */
DO:
   RUN tabstop_UI (INPUT TRUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DIALOG-3
&Scoped-define SELF-NAME RAD_VAL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RAD_VAL DIALOG-3
ON VALUE-CHANGED OF RAD_VAL IN FRAME DIALOG-3
DO:
   RAD_VAL = INPUT RAD_VAL.
   /*RAD_VAL:ADD-LAST("Fakt.regler", 2) IN FRAME {&FRAME-NAME}.
   RAD_VAL:ADD-LAST("Kundpriser", 3).
   RAD_VAL:ADD-LAST("Övertidspriser", 4).
   */
   ASSIGN
   FRAME FRAME-REGLER:VISIBLE = FALSE
   FRAME FRAME-OVER:VISIBLE = FALSE
   FRAME FRAME-PRISBEF:VISIBLE = FALSE.
   IF RAD_VAL = 1 THEN DO:
 
   END.
   IF RAD_VAL = 2 THEN DO:
      ASSIGN
      FRAME FRAME-REGLER:VISIBLE = TRUE.      
      IF fbok = TRUE THEN DO:
         ENABLE CMB_FB WITH FRAME FRAME-REGLER.
         DISPLAY CMB_FB WITH FRAME FRAME-REGLER.
      END.
   END.
   IF RAD_VAL = 3 THEN DO:
      fragakoll = FALSE.
      ASSIGN
      FRAME FRAME-PRISBEF:VISIBLE = TRUE.
      brwsortvar = 'defpristemp.OVER = FALSE'.
      RUN setcolsortvar_UI IN brwproc[2] (INPUT brwsortvar).
      RUN openbdynspec_UI IN brwproc[2].
      FIND FIRST kundprislisttemp WHERE kundprislisttemp.FAKTNR = infakplannr AND 
      kundprislisttemp.OVER = FALSE
      NO-ERROR.
      IF AVAILABLE kundprislisttemp THEN DO:
         FIND FIRST defpristemp WHERE defpristemp.PRISID = kundprislisttemp.PRISID AND
         defpristemp.OVER = FALSE
         NO-LOCK NO-ERROR.
         IF AVAILABLE defpristemp THEN DO:
            RUN setlastrowid_UI IN brwproc[2] (INPUT ROWID(defpristemp)).              
            RUN lastselectdyn_UI IN brwproc[2].
            defrow[2] = ROWID(defpristemp).
            BRW_BEF:TITLE IN FRAME FRAME-PRISBEF = defpristemp.PRISTEXT + " startdatum " + STRING(kundprislisttemp.STARTDATUM).
         END.
      END.
      
   END.
   IF RAD_VAL = 4 THEN DO:
      fragakoll = FALSE.
      ASSIGN
      FRAME FRAME-OVER:VISIBLE = TRUE.
      brwsortvar = 'defpristemp.OVER = TRUE'.
      RUN setcolsortvar_UI IN brwproc[3] (INPUT brwsortvar).
      RUN openbdynspec_UI IN brwproc[3].         
      FIND FIRST kundprislisttemp WHERE kundprislisttemp.FAKTNR = infakplannr AND 
      kundprislisttemp.OVER = TRUE
      NO-LOCK NO-ERROR.
      IF AVAILABLE kundprislisttemp THEN DO:
         FIND FIRST defpristemp WHERE defpristemp.PRISID = kundprislisttemp.PRISID AND
         defpristemp.OVER = TRUE NO-LOCK NO-ERROR.
         IF AVAILABLE defpristemp THEN DO:
            RUN setlastrowid_UI IN brwproc[3] (INPUT ROWID(defpristemp)).              
            RUN lastselectdyn_UI IN brwproc[3].  
            defrow[3] = ROWID(defpristemp).
            BRW_KUNDO:TITLE  IN FRAME FRAME-OVER = defpristemp.PRISTEXT + " startdatum " + STRING(kundprislisttemp.STARTDATUM).
         END.
      END.
   END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_BEF
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK DIALOG-3 


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
   RUN faktupp_UI.
   FIND FIRST kundregeltemp WHERE kundregeltemp.FAKTNR = infakplannr NO-ERROR.
   RUN enable_UI.
   RAD_VAL = 2.
   APPLY "VALUE-CHANGED" TO RAD_VAL.
   DISPLAY RAD_VAL WITH FRAME {&FRAME-NAME}.     
   {FRMSIZED.I}
   RUN setapplyvc IN framesizeh (INPUT RAD_VAL:HANDLE).
   {musarrow.i}
   {DIA_M_SLUT.I}             
   WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE allstartbrw_UI DIALOG-3 
PROCEDURE allstartbrw_UI :
/* -----------------------------------------------------------
  Purpose: Changing screen-value for combo-box CMB_OMR     
  Parameters:  Input = Screen-value for CMB_FOR
  Notes:       
-------------------------------------------------------------*/    
   ASSIGN
   kundbeftemp.NAMN:READ-ONLY IN BROWSE BRW_BEF = TRUE    
   defpristemp.PRISNUM:READ-ONLY IN BROWSE BRW_DPRISBEF = TRUE
   defpristemp.PRISNUM:READ-ONLY IN BROWSE BRW_DPRISOVER = TRUE
   kundovertemp.OTEXT:READ-ONLY IN BROWSE BRW_KUNDO = TRUE.
   RUN DYNBRW.P PERSISTENT SET brwproc[1] 
      (INPUT BRW_BEF:HANDLE IN FRAME FRAME-PRISBEF).
   RUN DYNBRW.P PERSISTENT SET brwproc[2] 
      (INPUT BRW_DPRISBEF:HANDLE IN FRAME FRAME-PRISBEF).
   RUN DYNBRW.P PERSISTENT SET brwproc[3] 
      (INPUT BRW_DPRISOVER:HANDLE IN FRAME FRAME-OVER).
   RUN DYNBRW.P PERSISTENT SET brwproc[4] 
      (INPUT BRW_KUNDO:HANDLE IN FRAME FRAME-OVER).
   
   RUN mouseselclick_UI IN brwproc[2] (INPUT FALSE). /*Disable APPLY "VALUE-CHANGED" TO browser*/
   RUN mouseselclick_UI IN brwproc[3] (INPUT FALSE).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI DIALOG-3  _DEFAULT-DISABLE
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
  HIDE FRAME DIALOG-3.
  HIDE FRAME FRAME-OVER.
  HIDE FRAME FRAME-PRISBEF.
  HIDE FRAME FRAME-REGLER.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI DIALOG-3  _DEFAULT-ENABLE
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
  DISPLAY RAD_VAL 
      WITH FRAME DIALOG-3.
  ENABLE RAD_VAL BTN_OK BTN_AVB 
      WITH FRAME DIALOG-3.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-3}
  ENABLE BRW_DPRISOVER BRW_KUNDO 
      WITH FRAME FRAME-OVER.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-OVER}
  ENABLE BRW_DPRISBEF BRW_BEF 
      WITH FRAME FRAME-PRISBEF.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-PRISBEF}
  DISPLAY CMB_KUND FILL-IN_FDAGAR FILL-IN_FAKINT CMB_TIM CMB_KOST CMB_OVER 
          CMB_LON CMB_TRA FILL-IN_ENTRAK FILL-IN_FLERTRAK FILL-IN_MIL 
          FILL-IN_FRTJPA FILL-IN_MTRLPA 
      WITH FRAME FRAME-REGLER.
  ENABLE CMB_KUND FILL-IN_FDAGAR FILL-IN_FAKINT CMB_TIM CMB_KOST CMB_OVER 
         CMB_LON CMB_TRA FILL-IN_ENTRAK FILL-IN_FLERTRAK FILL-IN_MIL 
         FILL-IN_FRTJPA FILL-IN_MTRLPA 
      WITH FRAME FRAME-REGLER.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-REGLER}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE faktupp_UI DIALOG-3 
PROCEDURE faktupp_UI :
/* -----------------------------------------------------------
  Purpose: Changing screen-value for combo-box CMB_OMR     
  Parameters:  Input = Screen-value for CMB_FOR
  Notes:       
-------------------------------------------------------------*/    
   status-ok = CMB_KUND:DELETE("CCC") IN FRAME FRAME-REGLER NO-ERROR.
   RUN fakupphmt_UI IN fakthmth (INPUT infakplannr,OUTPUT TABLE kundtyptemp,
   OUTPUT TABLE kundregeltemp,OUTPUT TABLE kundbeftemp,OUTPUT TABLE defpristemp, 
   OUTPUT TABLE kundovertemp, OUTPUT TABLE kundprislisttemp,OUTPUT TABLE felmeddtemp).                   
   FIND FIRST felmeddtemp NO-ERROR.
   IF AVAILABLE felmeddtemp THEN DO:
      MESSAGE felmeddtemp.FELMEDD VIEW-AS ALERT-BOX.
      DELETE felmeddtemp.
      RETURN.
   END.       
   RAD_VAL:ADD-LAST("Fakt.regler", 2) IN FRAME {&FRAME-NAME}.
   IF faktypvar = 6 OR faktypvar = 7 THEN musz = musz.
   ELSE DO:
      RAD_VAL:ADD-LAST("Kundpriser", 3).
      RAD_VAL:ADD-LAST("Övertidspriser", 4).
   END.  
   RAD_VAL:DELETE("Grundregistreringar") IN FRAME {&FRAME-NAME}.
   RUN openbdyn_UI IN brwproc[1] (INPUT "").
   RUN setpdatacolsort_UI IN brwproc[4] (INPUT "OTEXT",INPUT " kundovertemp.BEFATTNING BY kundovertemp.EQDAG BY kundovertemp.START
   BY kundovertemp.SLUT").
   RUN setpdatacolsort_UI IN brwproc[4] (INPUT "DAGTYP",INPUT " kundovertemp.BEFATTNING BY kundovertemp.OTEXT BY kundovertemp.EQDAG BY kundovertemp.START
   BY kundovertemp.SLUT").
   RUN setpdatacolsort_UI IN brwproc[4] (INPUT "START",INPUT " kundovertemp.BEFATTNING BY kundovertemp.OTEXT BY kundovertemp.EQDAG 
   BY kundovertemp.SLUT").
   RUN setpdatacolsort_UI IN brwproc[4] (INPUT "SLUT",INPUT " kundovertemp.BEFATTNING BY kundovertemp.OTEXT BY kundovertemp.EQDAG BY kundovertemp.START").
   RUN setpdatacolsort_UI IN brwproc[4] (INPUT "PRISA",INPUT " kundovertemp.BEFATTNING BY kundovertemp.OTEXT BY kundovertemp.EQDAG BY kundovertemp.START
   BY kundovertemp.SLUT").
   RUN openbdynspec_UI IN brwproc[4].
   IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "cSNAT" THEN DO:
      OPEN QUERY ktq FOR EACH kundtyptemp WHERE kundtyptemp.KUNDTYP = "Intern" NO-LOCK.
   END.
   ELSE IF Guru.Konstanter:globforetag = "SNAT" THEN DO:
      OPEN QUERY ktq FOR EACH kundtyptemp WHERE kundtyptemp.KUNDTYP = "Intern" OR kundtyptemp.KUNDTYP = "Strökund" NO-LOCK.
   END.
   ELSE DO:
      OPEN QUERY ktq FOR EACH kundtyptemp NO-LOCK.
   END.
   GET FIRST ktq NO-LOCK.
   DO WHILE AVAILABLE(kundtyptemp):
      status-ok = CMB_KUND:ADD-LAST(kundtyptemp.KUNDTYP).
      CMB_KUND:SCREEN-VALUE = kundtyptemp.KUNDTYP.
      GET NEXT ktq NO-LOCK.
   END.   
   RUN laddkreg_UI.     
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE laddkreg_UI DIALOG-3 
PROCEDURE laddkreg_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   FIND FIRST kundregeltemp WHERE kundregeltemp.FAKTNR = infakplannr NO-ERROR.
   FIND FIRST kundtyptemp WHERE kundtyptemp.KUNDID = kundregeltemp.KUNDID NO-LOCK NO-ERROR.
   IF AVAILABLE kundtyptemp THEN CMB_KUND:SCREEN-VALUE IN FRAME FRAME-REGLER = kundtyptemp.KUNDTYP.           
   IF kundregeltemp.LONRGL = "ENDAST MILERSÄTTNING" THEN CMB_LON:SCREEN-VALUE = "Endast utvalda löne.". 
   ELSE DO:
      IF kundregeltemp.LONRGL = "" THEN kundregeltemp.LONRGL = "Inga".
      CMB_LON:SCREEN-VALUE = kundregeltemp.LONRGL. 
      CMB_LON = kundregeltemp.LONRGL. 
   END.
   ASSIGN     
   CMB_KOST:SCREEN-VALUE = kundregeltemp.KOSTREGRGL    
   CMB_MTRL:SCREEN-VALUE = kundregeltemp.MTRLRGL 
   CMB_OVER:SCREEN-VALUE = kundregeltemp.OVERTIDRGL 
   CMB_OVR:SCREEN-VALUE = kundregeltemp.OVRKRGL 
   CMB_TIM:SCREEN-VALUE = kundregeltemp.TIMRGL 
   CMB_TRA:SCREEN-VALUE = kundregeltemp.TRAKTRGL 
   CMB_KOST = kundregeltemp.KOSTREGRGL    
   CMB_MTRL = kundregeltemp.MTRLRGL 
   CMB_OVER = kundregeltemp.OVERTIDRGL 
   CMB_OVR  = kundregeltemp.OVRKRGL 
   CMB_TIM  = kundregeltemp.TIMRGL 
   CMB_TRA  = kundregeltemp.TRAKTRGL 
   FILL-IN_ENTRAK = kundregeltemp.ENTRAK
   FILL-IN_FLERTRAK = kundregeltemp.FLERTRAK 
   FILL-IN_MIL = kundregeltemp.MIL   
   FILL-IN_FAKINT =  kundregeltemp.FAKINT 
   FILL-IN_FRTJPA = kundregeltemp.FRTJPA
   FILL-IN_MTRLPA = kundregeltemp.MTRLPA
   FILL-IN_FDAGAR = kundregeltemp.FDAGAR.  
   RUN fakturabesthmtalla_UI IN fbestapph (OUTPUT TABLE extrakopptemp).
   FIND FIRST extrakopptemp NO-ERROR.
   IF AVAILABLE extrakopptemp THEN DO:
      ENABLE CMB_FB WITH FRAME FRAME-REGLER.
      DISPLAY CMB_FB WITH FRAME FRAME-REGLER.
      FOR EACH extrakopptemp BY extrakopptemp.SOKCHAR[1]:
         IF extrakopptemp.SOKCHAR[1] NE "" THEN DO:           
            CREATE fbtemp.
            ASSIGN 
            fbtemp.NAMN = extrakopptemp.SOKCHAR[2]
            fbtemp.ANV = extrakopptemp.KOPPLACHAR2.
            status-ok = CMB_FB:ADD-LAST(extrakopptemp.SOKCHAR[2]).
            /*
            status-ok = CMB_FB:ADD-LAST(SUBSTRING(STRING(extrakopptemp.SOKCHAR[2],"X(20)"),1,20) + extrakopptemp.KOPPLACHAR2).
            */
         END.
         
      END.                              
     
      CREATE inextrakopptemp.          
      ASSIGN
      inextrakopptemp.PROGRAM     = "FBFPLAN"                   
      inextrakopptemp.KOPPLACHAR1 = ?              
      inextrakopptemp.KOPPLAINT1  =  infakplannr      
      inextrakopptemp.KOPPLACHAR2 = ?            
      inextrakopptemp.KOPPLAINT2 =  ?.
      RUN etabhamt_UI IN fbestapph (INPUT TABLE inextrakopptemp,OUTPUT TABLE extrakopptemp).        
      FIND FIRST extrakopptemp NO-ERROR.
      IF AVAILABLE extrakopptemp THEN DO:         
         FIND FIRST fbtemp WHERE fbtemp.ANV = extrakopptemp.KOPPLACHAR2 NO-LOCK NO-ERROR.
         IF AVAILABLE fbtemp THEN DO:
            CMB_FB:SCREEN-VALUE = fbtemp.NAMN NO-ERROR.
         END.
         ELSE CMB_FB:SCREEN-VALUE = "Ingen".       
         /*
         CMB_FB:SCREEN-VALUE = SUBSTRING(STRING(extrakopptemp.SOKCHAR[2],"X(20)"),1,20) + extrakopptemp.KOPPLACHAR2 NO-ERROR. 
         */
      END.
      ELSE CMB_FB:SCREEN-VALUE = "Ingen".       
      fbok = TRUE.           
   END.
   ELSE fbok = FALSE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE nylista_UI DIALOG-3 
PROCEDURE nylista_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   {muswait.i}      
   DEFINE INPUT PARAMETER vad AS INTEGER NO-UNDO.
   IF fragakoll = TRUE THEN DO:
      MESSAGE "Är du säker på att du byta prislista. Allt gammalt kommer att tas bort"
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE defpristemp.PRISTEXT
      UPDATE answer AS LOGICAL.
      IF answer = FALSE THEN DO:
         musz = TRUE.
         RETURN NO-APPLY.
      END.
   END.
   IF vad = 1 THEN DO:
      FIND FIRST kundprislisttemp WHERE kundprislisttemp.FAKTNR = infakplannr AND 
      kundprislisttemp.OVER = FALSE
      NO-ERROR.
      IF NOT AVAILABLE kundprislisttemp THEN CREATE kundprislisttemp.
      ASSIGN
      kundprislisttemp.FAKTNR = infakplannr
      kundprislisttemp.PRISID = defpristemp.PRISID
      kundprislisttemp.OVER = FALSE
      kundprislisttemp.STARTDATUM = TODAY.      
      FIND FIRST kundregeltemp WHERE kundregeltemp.FAKTNR = infakplannr
      NO-ERROR.
      CMB_TIM = INPUT FRAME FRAME-REGLER CMB_TIM.
      ASSIGN      
      kundregeltemp.TIMRGL = CMB_TIM
      kundregeltemp.ENTRAK   =  FILL-IN_ENTRAK  
      kundregeltemp.FLERTRAK =   FILL-IN_FLERTRAK
      kundregeltemp.FRTJPA   =   FILL-IN_FRTJPA  
      kundregeltemp.MIL      =   FILL-IN_MIL     
      kundregeltemp.MTRLPA  =   FILL-IN_MTRLPA.  
      RUN nypris_UI IN fakthmth (INPUT infakplannr,INPUT defpristemp.PRISID,
                                 INPUT-OUTPUT TABLE kundregeltemp,
                                 OUTPUT TABLE kundbeftemp).       
      FIND FIRST kundregeltemp WHERE kundregeltemp.FAKTNR = infakplannr
      NO-ERROR.
                 
      IF AVAILABLE kundregeltemp THEN DO:
         ASSIGN
         FILL-IN_ENTRAK = kundregeltemp.ENTRAK
         FILL-IN_FLERTRAK = kundregeltemp.FLERTRAK 
         FILL-IN_FRTJPA = kundregeltemp.FRTJPA
         FILL-IN_MIL = kundregeltemp.MIL   
         FILL-IN_MTRLPA = kundregeltemp.MTRLPA.  
         DISPLAY FILL-IN_ENTRAK FILL-IN_FLERTRAK FILL-IN_FRTJPA FILL-IN_MIL FILL-IN_MTRLPA WITH FRAME FRAME-REGLER. 
      END.
      RUN openbdyn_UI IN brwproc[1] (INPUT "").
   END.
   IF vad = 2 THEN DO:
      FIND FIRST kundprislisttemp WHERE kundprislisttemp.FAKTNR = infakplannr AND 
      kundprislisttemp.OVER = TRUE
      NO-ERROR.
      IF NOT AVAILABLE kundprislisttemp THEN CREATE kundprislisttemp.
      ASSIGN
      kundprislisttemp.FAKTNR = infakplannr
      kundprislisttemp.PRISID = defpristemp.PRISID
      kundprislisttemp.OVER = TRUE
      kundprislisttemp.STARTDATUM = TODAY.      
      RUN nypriso_UI IN fakthmth (INPUT infakplannr,INPUT defpristemp.PRISID,OUTPUT TABLE kundovertemp).
      RUN openbdynspec_UI IN brwproc[4].
   END.
   /*
   APPLY "VALUE-CHANGED" TO RAD_VAL IN FRAME {&FRAME-NAME}.
   */
   {musarrow.i}
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tabstop_UI DIALOG-3 
PROCEDURE tabstop_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER tabstop AS LOGICAL NO-UNDO.
   ASSIGN
   BTN_OK:TAB-STOP IN FRAME {&FRAME-NAME} = tabstop
   BTN_AVB:TAB-STOP IN FRAME {&FRAME-NAME} = tabstop.    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

