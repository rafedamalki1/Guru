&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
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
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{FAKTPLANTEMP.I}
{FAKTTEMP.I}
/* Parameters Definitions ---                                           */
DEFINE INPUT PARAMETER infakplannr AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER fdelnrvar   AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER skarp AS LOGICAL NO-UNDO.
DEFINE INPUT PARAMETER TABLE FOR faktureradtemp.
DEFINE INPUT PARAMETER TABLE FOR faktaonrtemp.
/* Local Variable Definitions ---                                       */
{ALLDEF.I}
{GLOBVAR2DEL1.I}
&Scoped-define NEW                           

&Scoped-define SHARED SHARED 
{FAKKOTEMP.I}
DEFINE SHARED VARIABLE faktupphmth AS HANDLE NO-UNDO.
DEFINE SHARED VARIABLE skarpfaktok AS LOGICAL NO-UNDO.

DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE VARIABLE bytvar AS INTEGER  NO-UNDO.
DEFINE VARIABLE raknare AS INTEGER NO-UNDO.
DEFINE VARIABLE stat AS LOGICAL NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE fadatum AS DATE NO-UNDO.
DEFINE VARIABLE fodatum AS DATE NO-UNDO.
DEFINE VARIABLE kollsuma AS DECIMAL NO-UNDO.
DEFINE VARIABLE ejav AS LOGICAL NO-UNDO.
{VISKONTAB.I}



/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame
&Scoped-define BROWSE-NAME BRW_K1

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES faktkundtemp faktinkontotemp motparttemp ~
faktmomstemp viskonttemp

/* Definitions for BROWSE BRW_K1                                        */
&Scoped-define FIELDS-IN-QUERY-BRW_K1 faktkundtemp.KUNDKONTO ~
faktkundtemp.KUNDFODTEXT faktkundtemp.MOTPART faktkundtemp.MOTPARTTEXT ~
faktkundtemp.BELOPP faktkundtemp.FAKTDATUM faktkundtemp.FDATUM ~
faktkundtemp.AONR faktkundtemp.DELNR 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_K1 
&Scoped-define QUERY-STRING-BRW_K1 FOR EACH faktkundtemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_K1 OPEN QUERY BRW_K1 FOR EACH faktkundtemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_K1 faktkundtemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_K1 faktkundtemp


/* Definitions for BROWSE BRW_K2                                        */
&Scoped-define FIELDS-IN-QUERY-BRW_K2 faktinkontotemp.INTAKTKONTO ~
faktinkontotemp.INTAKTTEXT faktinkontotemp.MOTPART ~
faktinkontotemp.MOTPARTTEXT faktinkontotemp.BELOPP faktinkontotemp.AONR ~
faktinkontotemp.DELNR 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_K2 
&Scoped-define QUERY-STRING-BRW_K2 FOR EACH faktinkontotemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_K2 OPEN QUERY BRW_K2 FOR EACH faktinkontotemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_K2 faktinkontotemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_K2 faktinkontotemp


/* Definitions for BROWSE BRW_K3                                        */
&Scoped-define FIELDS-IN-QUERY-BRW_K3 motparttemp.MOTPART ~
motparttemp.MOTPARTTEXT 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_K3 
&Scoped-define QUERY-STRING-BRW_K3 FOR EACH motparttemp NO-LOCK ~
    BY motparttemp.MOTPART
&Scoped-define OPEN-QUERY-BRW_K3 OPEN QUERY BRW_K3 FOR EACH motparttemp NO-LOCK ~
    BY motparttemp.MOTPART.
&Scoped-define TABLES-IN-QUERY-BRW_K3 motparttemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_K3 motparttemp


/* Definitions for BROWSE BRW_K4                                        */
&Scoped-define FIELDS-IN-QUERY-BRW_K4 faktmomstemp.MOMSKOD ~
faktmomstemp.MOMSTEXT faktmomstemp.BELOPP faktmomstemp.MOMSEXTERNT ~
faktmomstemp.MOMSBELOPP faktmomstemp.FRITEXT faktmomstemp.AONR ~
faktmomstemp.DELNR 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_K4 
&Scoped-define QUERY-STRING-BRW_K4 FOR EACH faktmomstemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_K4 OPEN QUERY BRW_K4 FOR EACH faktmomstemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_K4 faktmomstemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_K4 faktmomstemp


/* Definitions for BROWSE BRW_VIS                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_VIS viskonttemp.KONTO ~
viskonttemp.MOTPART viskonttemp.OMRADE viskonttemp.AONR viskonttemp.VDELNR ~
viskonttemp.DEBET viskonttemp.KREDIT 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_VIS 
&Scoped-define QUERY-STRING-BRW_VIS FOR EACH viskonttemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_VIS OPEN QUERY BRW_VIS FOR EACH viskonttemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_VIS viskonttemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_VIS viskonttemp


/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define OPEN-BROWSERS-IN-QUERY-Dialog-Frame ~
    ~{&OPEN-QUERY-BRW_K3}~
    ~{&OPEN-QUERY-BRW_VIS}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RAD_KONTO BRW_K4 BRW_K3 BRW_VIS BRW_K2 ~
BRW_K1 BTN_ny BTN_DELA BTN_BORT BTN_OK 
&Scoped-Define DISPLAYED-OBJECTS RAD_KONTO 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_BORT 
     LABEL "Ta bort" 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_DELA 
     LABEL "Ändra" 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_ny 
     LABEL "Ny" 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_OK AUTO-GO 
     LABEL "Ok" 
     SIZE 14 BY 1
     BGCOLOR 8 .

DEFINE VARIABLE RAD_KONTO AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "", 1
     SIZE 85.63 BY 1.13 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_K1 FOR 
      faktkundtemp SCROLLING.

DEFINE QUERY BRW_K2 FOR 
      faktinkontotemp SCROLLING.

DEFINE QUERY BRW_K3 FOR 
      motparttemp SCROLLING.

DEFINE QUERY BRW_K4 FOR 
      faktmomstemp SCROLLING.

DEFINE QUERY BRW_VIS FOR 
      viskonttemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_K1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_K1 Dialog-Frame _STRUCTURED
  QUERY BRW_K1 NO-LOCK DISPLAY
      faktkundtemp.KUNDKONTO COLUMN-LABEL "Kundfodrans!konto" FORMAT "X(6)":U
      faktkundtemp.KUNDFODTEXT COLUMN-LABEL "Text" FORMAT "X(31)":U
      faktkundtemp.MOTPART COLUMN-LABEL "Mot!part" FORMAT "X(5)":U
      faktkundtemp.MOTPARTTEXT COLUMN-LABEL "Text" FORMAT "X(25)":U
      faktkundtemp.BELOPP COLUMN-LABEL "Belopp" FORMAT "->>>>>>>>>>9.99":U
      faktkundtemp.FAKTDATUM COLUMN-LABEL "Faktura !datum" FORMAT "99/99/99":U
      faktkundtemp.FDATUM COLUMN-LABEL "Förfallo!dag" FORMAT "99/99/99":U
            WIDTH 9
      faktkundtemp.AONR FORMAT "X(6)":U
      faktkundtemp.DELNR COLUMN-LABEL "Del!nr" FORMAT ">99":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SIZE 122.5 BY 18.

DEFINE BROWSE BRW_K2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_K2 Dialog-Frame _STRUCTURED
  QUERY BRW_K2 NO-LOCK DISPLAY
      faktinkontotemp.INTAKTKONTO COLUMN-LABEL "Inäkts!konto" FORMAT "X(6)":U
      faktinkontotemp.INTAKTTEXT COLUMN-LABEL "Text" FORMAT "X(35)":U
      faktinkontotemp.MOTPART COLUMN-LABEL "Motpart" FORMAT "X(6)":U
      faktinkontotemp.MOTPARTTEXT COLUMN-LABEL "Text" FORMAT "X(35)":U
      faktinkontotemp.BELOPP COLUMN-LABEL "Belopp" FORMAT "->>>>>>>>>>9.99":U
      faktinkontotemp.AONR FORMAT "X(6)":U
      faktinkontotemp.DELNR COLUMN-LABEL "Del!nr" FORMAT ">99":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SIZE 115.63 BY 18.

DEFINE BROWSE BRW_K3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_K3 Dialog-Frame _STRUCTURED
  QUERY BRW_K3 NO-LOCK DISPLAY
      motparttemp.MOTPART COLUMN-LABEL "Motpart" FORMAT "X(6)":U
      motparttemp.MOTPARTTEXT COLUMN-LABEL "Text" FORMAT "X(35)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SIZE 52.13 BY 18.

DEFINE BROWSE BRW_K4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_K4 Dialog-Frame _STRUCTURED
  QUERY BRW_K4 NO-LOCK DISPLAY
      faktmomstemp.MOMSKOD COLUMN-LABEL "Moms!konto" FORMAT "X(8)":U
      faktmomstemp.MOMSTEXT COLUMN-LABEL "Text" FORMAT "X(40)":U
      faktmomstemp.BELOPP COLUMN-LABEL "Belopp" FORMAT "->>>>>>>>>>9.99":U
      faktmomstemp.MOMSEXTERNT COLUMN-LABEL "Moms!sats" FORMAT ">>9.99":U
      faktmomstemp.MOMSBELOPP COLUMN-LABEL "Momsbelopp" FORMAT "->>>>>>>>>>9.99":U
      faktmomstemp.FRITEXT COLUMN-LABEL "Fritext" FORMAT "X(15)":U
      faktmomstemp.AONR FORMAT "X(6)":U
      faktmomstemp.DELNR COLUMN-LABEL "Del!nr" FORMAT ">99":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SIZE 116.5 BY 18.

DEFINE BROWSE BRW_VIS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_VIS Dialog-Frame _STRUCTURED
  QUERY BRW_VIS DISPLAY
      viskonttemp.KONTO COLUMN-LABEL "Konto" FORMAT "X(6)":U
      viskonttemp.MOTPART COLUMN-LABEL "Motpart" FORMAT "X(6)":U
      viskonttemp.OMRADE COLUMN-LABEL "Område" FORMAT "X(6)":U
      viskonttemp.AONR COLUMN-LABEL "Aonr" FORMAT "X(6)":U
      viskonttemp.VDELNR COLUMN-LABEL "Delnr" FORMAT "X(5)":U
      viskonttemp.DEBET COLUMN-LABEL "Debet" FORMAT "->>>>>>>9.99":U
            COLUMN-FGCOLOR 9 LABEL-FGCOLOR 9
      viskonttemp.KREDIT COLUMN-LABEL "Kredit" FORMAT "->>>>>>>9.99":U
            COLUMN-FGCOLOR 12 LABEL-FGCOLOR 12
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING MULTIPLE SIZE 66.38 BY 18
         TITLE "Summa".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     RAD_KONTO AT ROW 3 COL 1.5 NO-LABEL
     BRW_K4 AT ROW 4.33 COL 1.5
     BRW_K3 AT ROW 4.33 COL 1.5
     BRW_VIS AT ROW 4.33 COL 1.5
     BRW_K2 AT ROW 4.33 COL 1.5
     BRW_K1 AT ROW 4.33 COL 1.5
     BTN_ny AT ROW 22.67 COL 26.38
     BTN_DELA AT ROW 22.67 COL 44.38
     BTN_BORT AT ROW 22.67 COL 62.5
     BTN_OK AT ROW 22.67 COL 109.5
     "Välj konton för fakturering" VIEW-AS TEXT
          SIZE 52.63 BY 1.63 AT ROW 1.25 COL 1.5
          FONT 17
     SPACE(71.11) SKIP(21.07)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Fakturakonton".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Allow: Basic,Browse,DB-Fields,Query
   Temp-Tables and Buffers:
      TABLE: faktinkontotemp T "?" NO-UNDO temp-db faktinkontotemp
      TABLE: faktkundtemp T "?" NO-UNDO temp-db faktkundtemp
      TABLE: faktmomstemp T "?" NO-UNDO temp-db faktmomstemp
      TABLE: motparttemp T "?" NO-UNDO temp-db motparttemp
      TABLE: ? T "?" NO-UNDO temp-db viskonttemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   NOT-VISIBLE                                                          */
/* BROWSE-TAB BRW_K4 RAD_KONTO Dialog-Frame */
/* BROWSE-TAB BRW_K3 BRW_K4 Dialog-Frame */
/* BROWSE-TAB BRW_VIS BRW_K3 Dialog-Frame */
/* BROWSE-TAB BRW_K2 BRW_VIS Dialog-Frame */
/* BROWSE-TAB BRW_K1 BRW_K2 Dialog-Frame */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

ASSIGN 
       BRW_K4:COLUMN-RESIZABLE IN FRAME Dialog-Frame       = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_K1
/* Query rebuild information for BROWSE BRW_K1
     _TblList          = "Temp-Tables.faktkundtemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.faktkundtemp.KUNDKONTO
"faktkundtemp.KUNDKONTO" "Kundfodrans!konto" "X(6)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.faktkundtemp.KUNDFODTEXT
"faktkundtemp.KUNDFODTEXT" "Text" "X(31)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.faktkundtemp.MOTPART
"faktkundtemp.MOTPART" "Mot!part" "X(5)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[4]   > Temp-Tables.faktkundtemp.MOTPARTTEXT
"faktkundtemp.MOTPARTTEXT" "Text" "X(25)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[5]   > Temp-Tables.faktkundtemp.BELOPP
"faktkundtemp.BELOPP" "Belopp" "->>>>>>>>>>9.99" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[6]   > Temp-Tables.faktkundtemp.FAKTDATUM
"faktkundtemp.FAKTDATUM" "Faktura !datum" ? "date" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[7]   > Temp-Tables.faktkundtemp.FDATUM
"faktkundtemp.FDATUM" "Förfallo!dag" ? "date" ? ? ? ? ? ? no ? no no "9" yes no no "U" "" ""
     _FldNameList[8]   = Temp-Tables.faktkundtemp.AONR
     _FldNameList[9]   > Temp-Tables.faktkundtemp.DELNR
"faktkundtemp.DELNR" "Del!nr" ">99" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_K1 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_K2
/* Query rebuild information for BROWSE BRW_K2
     _TblList          = "Temp-Tables.faktinkontotemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.faktinkontotemp.INTAKTKONTO
"faktinkontotemp.INTAKTKONTO" "Inäkts!konto" "X(6)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.faktinkontotemp.INTAKTTEXT
"faktinkontotemp.INTAKTTEXT" "Text" "X(35)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.faktinkontotemp.MOTPART
"faktinkontotemp.MOTPART" "Motpart" "X(6)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[4]   > Temp-Tables.faktinkontotemp.MOTPARTTEXT
"faktinkontotemp.MOTPARTTEXT" "Text" "X(35)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[5]   > Temp-Tables.faktinkontotemp.BELOPP
"faktinkontotemp.BELOPP" "Belopp" "->>>>>>>>>>9.99" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[6]   = Temp-Tables.faktinkontotemp.AONR
     _FldNameList[7]   > Temp-Tables.faktinkontotemp.DELNR
"faktinkontotemp.DELNR" "Del!nr" ">99" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_K2 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_K3
/* Query rebuild information for BROWSE BRW_K3
     _TblList          = "Temp-Tables.motparttemp"
     _Options          = "NO-LOCK"
     _OrdList          = "Temp-Tables.motparttemp.MOTPART|yes"
     _FldNameList[1]   > Temp-Tables.motparttemp.MOTPART
"motparttemp.MOTPART" "Motpart" "X(6)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.motparttemp.MOTPARTTEXT
"motparttemp.MOTPARTTEXT" "Text" "X(35)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is OPENED
*/  /* BROWSE BRW_K3 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_K4
/* Query rebuild information for BROWSE BRW_K4
     _TblList          = "Temp-Tables.faktmomstemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.faktmomstemp.MOMSKOD
"faktmomstemp.MOMSKOD" "Moms!konto" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.faktmomstemp.MOMSTEXT
"faktmomstemp.MOMSTEXT" "Text" "X(40)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.faktmomstemp.BELOPP
"faktmomstemp.BELOPP" "Belopp" "->>>>>>>>>>9.99" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[4]   > Temp-Tables.faktmomstemp.MOMSEXTERNT
"faktmomstemp.MOMSEXTERNT" "Moms!sats" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[5]   > Temp-Tables.faktmomstemp.MOMSBELOPP
"faktmomstemp.MOMSBELOPP" "Momsbelopp" "->>>>>>>>>>9.99" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[6]   > Temp-Tables.faktmomstemp.FRITEXT
"faktmomstemp.FRITEXT" "Fritext" "X(15)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[7]   = Temp-Tables.faktmomstemp.AONR
     _FldNameList[8]   > Temp-Tables.faktmomstemp.DELNR
"faktmomstemp.DELNR" "Del!nr" ">99" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_K4 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_VIS
/* Query rebuild information for BROWSE BRW_VIS
     _TblList          = "Temp-Tables.viskonttemp"
     _FldNameList[1]   > Temp-Tables.viskonttemp.KONTO
"viskonttemp.KONTO" "Konto" "X(6)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.viskonttemp.MOTPART
"viskonttemp.MOTPART" "Motpart" "X(6)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.viskonttemp.OMRADE
"viskonttemp.OMRADE" "Område" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[4]   > Temp-Tables.viskonttemp.AONR
"viskonttemp.AONR" "Aonr" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[5]   > Temp-Tables.viskonttemp.VDELNR
"viskonttemp.VDELNR" "Delnr" "X(5)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[6]   > Temp-Tables.viskonttemp.DEBET
"viskonttemp.DEBET" "Debet" "->>>>>>>9.99" "decimal" ? 9 ? ? 9 ? no ? no no ? yes no no "U" "" ""
     _FldNameList[7]   > Temp-Tables.viskonttemp.KREDIT
"viskonttemp.KREDIT" "Kredit" "->>>>>>>9.99" "decimal" ? 12 ? ? 12 ? no ? no no ? yes no no "U" "" ""
     _Query            is OPENED
*/  /* BROWSE BRW_VIS */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX Dialog-Frame
/* Query rebuild information for DIALOG-BOX Dialog-Frame
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX Dialog-Frame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Fakturakonton */
DO:
   RUN avb_UI.
   IF musz = TRUE THEN DO:
      musz = FALSE.
      RETURN NO-APPLY.
   END.
   ELSE DO:
      {BORTBRWPROC.I}
      APPLY "END-ERROR":U TO SELF.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_K1
&Scoped-define SELF-NAME BRW_K1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_K1 Dialog-Frame
ON MOUSE-SELECT-DBLCLICK OF BRW_K1 IN FRAME Dialog-Frame
DO:
   /*RUN andra_UI.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_K1 Dialog-Frame
ON VALUE-CHANGED OF BRW_K1 IN FRAME Dialog-Frame
DO:
   status-ok = BRW_K1:SELECT-FOCUSED-ROW() NO-ERROR.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_K2
&Scoped-define SELF-NAME BRW_K2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_K2 Dialog-Frame
ON MOUSE-SELECT-DBLCLICK OF BRW_K2 IN FRAME Dialog-Frame
DO:
 /*  RUN andra_UI.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_K2 Dialog-Frame
ON VALUE-CHANGED OF BRW_K2 IN FRAME Dialog-Frame
DO:
   status-ok = BRW_K2:SELECT-FOCUSED-ROW()  NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_K3
&Scoped-define SELF-NAME BRW_K3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_K3 Dialog-Frame
ON MOUSE-SELECT-DBLCLICK OF BRW_K3 IN FRAME Dialog-Frame
DO:
   /*RUN andra_UI.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_K3 Dialog-Frame
ON VALUE-CHANGED OF BRW_K3 IN FRAME Dialog-Frame
DO:
   status-ok = BRW_K3:SELECT-FOCUSED-ROW()  NO-ERROR.   
   RUN bytmot_UI. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_K4
&Scoped-define SELF-NAME BRW_K4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_K4 Dialog-Frame
ON MOUSE-SELECT-DBLCLICK OF BRW_K4 IN FRAME Dialog-Frame
DO:
   /*RUN andra_UI.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_K4 Dialog-Frame
ON VALUE-CHANGED OF BRW_K4 IN FRAME Dialog-Frame
DO:
   status-ok = BRW_K4:SELECT-FOCUSED-ROW() NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_BORT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_BORT Dialog-Frame
ON CHOOSE OF BTN_BORT IN FRAME Dialog-Frame /* Ta bort */
DO:
   MESSAGE "Vill du verkligen ta bort denna post ?"   
   VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Bortag av kontering"
   UPDATE answer AS LOGICAL.
   IF answer THEN DO: 
      musz = musz.            
   END.
   ELSE RETURN NO-APPLY. 
   IF RAD_KONTO = 1 THEN DO:
      raknare = 0.
      FOR EACH faktkundtemp:
         raknare = raknare + 1.
      END.
      IF raknare = 1 THEN  DO:
         MESSAGE "Du kan inte ta bort sista posten" VIEW-AS ALERT-BOX.
         RETURN NO-APPLY. 
      END.
      RUN bort_UI (INPUT faktkundtemp.FROW).
      DELETE faktkundtemp.                                  
      RUN selnextprevrow_UI IN brwproc[1].
      RUN refreshbrw_UI IN brwproc[1].
      RUN lastselectdyn_UI IN brwproc[1].              
   END.
   ELSE IF RAD_KONTO = 2 THEN DO:
      raknare = 0.
      FOR EACH faktinkontotemp:
         raknare = raknare + 1.
      END.
      IF raknare = 1 THEN  DO:
         MESSAGE "Du kan inte ta bort sista posten" VIEW-AS ALERT-BOX.
         RETURN NO-APPLY. 
      END.
      
      RUN bort_UI (INPUT faktinkontotemp.FROW).
      DELETE faktinkontotemp.                                  
      RUN selnextprevrow_UI IN brwproc[2].
      RUN refreshbrw_UI IN brwproc[2].
      RUN lastselectdyn_UI IN brwproc[2].              
   END.
   IF RAD_KONTO = 4 THEN DO:
      raknare = 0.
      FOR EACH faktmomstemp:
         raknare = raknare + 1.
      END.
      IF raknare = 1 THEN  DO:
         MESSAGE "Du kan inte ta bort sista posten" VIEW-AS ALERT-BOX.
         RETURN NO-APPLY. 
      END.
      RUN bort_UI (INPUT faktmomstemp.FROW).
      DELETE faktmomstemp.                                  
      RUN selnextprevrow_UI IN brwproc[4].
      RUN refreshbrw_UI IN brwproc[4].
      RUN lastselectdyn_UI IN brwproc[4].                 
   END.   
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_DELA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_DELA Dialog-Frame
ON CHOOSE OF BTN_DELA IN FRAME Dialog-Frame /* Ändra */
DO:
   IF RAD_KONTO = 1 THEN DO:            
      status-ok = BRW_K1:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
   END.
   ELSE IF RAD_KONTO = 2 THEN DO:     
      status-ok = BRW_K2:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.               
   END.
   IF RAD_KONTO = 4 THEN DO:
      status-ok = BRW_K4:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.               
   END.
   RUN dela_UI.  
   bytvar = 0.
   RUN nykonton_UI IN faktupphmth (INPUT 2,INPUT infakplannr,INPUT fdelnrvar,INPUT bytvar,
   INPUT-OUTPUT TABLE kolltabeltemp,
   OUTPUT TABLE faktkundtemp,OUTPUT TABLE faktinkontotemp,OUTPUT TABLE faktmomstemp).
   RUN open_UI.
   
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_ny
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_ny Dialog-Frame
ON CHOOSE OF BTN_ny IN FRAME Dialog-Frame /* Ny */
DO:
   RUN ny_UI.   
   IF musz = TRUE THEN DO:
      musz = FALSE.
   END.
   ELSE DO:
      RUN nykonton_UI IN faktupphmth (INPUT 2,INPUT infakplannr,INPUT fdelnrvar,INPUT bytvar,
      INPUT-OUTPUT TABLE kolltabeltemp,
      OUTPUT TABLE faktkundtemp,OUTPUT TABLE faktinkontotemp,OUTPUT TABLE faktmomstemp).
      RUN open_UI.        
   END. 
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK Dialog-Frame
ON CHOOSE OF BTN_OK IN FRAME Dialog-Frame /* Ok */
DO:
   RUN avb_UI.
   IF musz = TRUE THEN DO:
      ejav = TRUE.
      musz = FALSE.
      RETURN NO-APPLY.
   END.
   {BORTBRWPROC.I}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RAD_KONTO
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RAD_KONTO Dialog-Frame
ON ENTRY OF RAD_KONTO IN FRAME Dialog-Frame
DO:
   /*ASSIGN RAD_KONTO = 1.  */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RAD_KONTO Dialog-Frame
ON VALUE-CHANGED OF RAD_KONTO IN FRAME Dialog-Frame
DO:  
   {muswait.i}
   ASSIGN RAD_KONTO = INPUT RAD_KONTO.   
   ASSIGN   
   BRW_K1:HIDDEN = TRUE         
   BRW_K2:HIDDEN = TRUE
   BRW_K3:HIDDEN = TRUE         
   BRW_K4:HIDDEN = TRUE
   BRW_VIS:HIDDEN = TRUE.   
   IF RAD_KONTO = 1 THEN DO:
      /*ENABLE BRW_K1 WITH FRAME {&FRAME-NAME}. */                 
      BRW_K1:HIDDEN = FALSE.    
   END. 
   ELSE IF RAD_KONTO = 2 THEN DO:
      BRW_K2:HIDDEN = FALSE.                   
   END.
   ELSE IF RAD_KONTO = 3 THEN DO:
      BRW_K3:HIDDEN = FALSE.             
      FIND FIRST faktkundtemp WHERE 
      faktkundtemp.FAKTNR = infakplannr AND 
      faktkundtemp.FDELNR = fdelnrvar NO-LOCK NO-ERROR.
      FIND FIRST motparttemp WHERE motparttemp.MOTPARTID = faktkundtemp.MOTPARTID NO-LOCK NO-ERROR.
      IF AVAILABLE motparttemp THEN DO:
         RUN setlastrowid_UI IN brwproc[3] (INPUT ROWID(motparttemp)).
         RUN lastselectdyn_UI IN brwproc[3].                 
      END.
   END. 
   ELSE IF RAD_KONTO = 4 THEN DO: 
      BRW_K4:HIDDEN = FALSE.            
   END.
   IF RAD_KONTO = 5 THEN DO:
      BRW_VIS:HIDDEN = FALSE.
      ASSIGN
      BTN_BORT:HIDDEN = TRUE
      BTN_DELA:HIDDEN = TRUE
      BTN_NY:HIDDEN = TRUE.
      RUN viskont_UI.
   END.
   ELSE IF RAD_KONTO = 3 THEN DO:
      ASSIGN
      BTN_BORT:HIDDEN = TRUE
      BTN_DELA:HIDDEN = TRUE
      BTN_NY:HIDDEN = TRUE.      
   END.
   ELSE DO:
      ASSIGN
      BTN_BORT:HIDDEN = FALSE
      BTN_DELA:HIDDEN = FALSE
      BTN_NY:HIDDEN = FALSE.
   END.
   
   DISPLAY RAD_KONTO WITH FRAME {&FRAME-NAME}.   
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_K1
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
   {DIA_M_START.I}
   {ALLSTARTDYN.I}
      
   stat = RAD_KONTO:DELETE("").
   stat = RAD_KONTO:ADD-LAST("Kundfodran",1).
   stat = RAD_KONTO:ADD-LAST("Intäktskonto",2).
   IF Guru.Konstanter:globforetag = "ELPA"  OR Guru.Konstanter:globforetag = "GRAN" OR Guru.Konstanter:globforetag = "GKAL" THEN DO:
      stat = RAD_KONTO:ADD-LAST("Byte motpart",3).
   END.  
   FIND FIRST faktureradtemp WHERE faktureradtemp.FAKTNR = infakplannr AND
   faktureradtemp.FDELNR = fdelnrvar NO-LOCK NO-ERROR.  
   FIND FIRST faktureringstyptemp WHERE 
   faktureringstyptemp.FAKTTYPID = faktureradtemp.FAKTTYPID NO-LOCK NO-ERROR.      
   IF AVAILABLE faktureringstyptemp THEN 
   BRW_VIS:TITLE = faktureringstyptemp.FAKTTYPTEXT.
   RUN enable_UI.       
   {FRMSIZED.I} 
   RUN nybelopp_UI.         
   RUN open_UI.
   GET FIRST BRW_K4 NO-LOCK.
   IF AVAILABLE faktmomstemp THEN stat = RAD_KONTO:ADD-LAST("Momskonto",4).
   stat = RAD_KONTO:ADD-LAST("Sammanställning",5).
   ASSIGN
   RAD_KONTO = 5.         
   DISPLAY RAD_KONTO WITH FRAME {&FRAME-NAME}.
   APPLY "VALUE-CHANGED" TO RAD_KONTO IN FRAME {&FRAME-NAME}.
   
   {DIA_M_SLUT.I}
   /*FAKTFOR*/
   IF Guru.Konstanter:globanv = CHR(69) + CHR(76) + CHR(80) + CHR(65) + CHR(79) THEN.
   ELSE IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" THEN DO: 
      APPLY "CHOOSE" TO BTN_OK.
      IF ejav = FALSE THEN LEAVE MAIN-BLOCK.
      ELSE ejav = FALSE.
   END.
   {musarrow.i}
   WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE allstartbrw_UI Dialog-Frame 
PROCEDURE allstartbrw_UI :
/* -----------------------------------------------------------
  Purpose: Changing screen-value for combo-box CMB_OMR     
  Parameters:  Input = Screen-value for CMB_FOR
  Notes:       
-------------------------------------------------------------*/    
   RUN DYNBRW.P PERSISTENT SET brwproc[1] 
      (INPUT BRW_K1:HANDLE IN FRAME {&FRAME-NAME}).
   RUN DYNBRW.P PERSISTENT SET brwproc[2] 
      (INPUT BRW_K2:HANDLE IN FRAME {&FRAME-NAME}).
   RUN DYNBRW.P PERSISTENT SET brwproc[3] 
      (INPUT BRW_K3:HANDLE IN FRAME {&FRAME-NAME}).
   RUN DYNBRW.P PERSISTENT SET brwproc[4] 
      (INPUT BRW_K4:HANDLE IN FRAME {&FRAME-NAME}).
   RUN DYNBRW.P PERSISTENT SET brwproc[5] 
      (INPUT BRW_VIS:HANDLE IN FRAME {&FRAME-NAME}).   
                                                                                            
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE avb_UI Dialog-Frame 
PROCEDURE avb_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/  
   {muswait.i}
   FIND FIRST kolltabeltemp NO-LOCK NO-ERROR.
   FOR EACH faktkundtemp WHERE 
   faktkundtemp.FAKTNR = infakplannr AND 
   faktkundtemp.FDELNR = fdelnrvar NO-LOCK BREAK BY faktkundtemp.VFAKTNR:
      ACCUMULATE 
      faktkundtemp.BELOPP (TOTAL BY faktkundtemp.VFAKTNR).
      IF LAST-OF(faktkundtemp.VFAKTNR) THEN DO:         
         IF kolltabeltemp.NYFAKT NE 
         (ACCUM TOTAL BY faktkundtemp.VFAKTNR faktkundtemp.BELOPP) THEN DO:
            kollsuma = kolltabeltemp.NYFAKT - 
            (ACCUM TOTAL BY faktkundtemp.VFAKTNR faktkundtemp.BELOPP).
            IF ABSOLUTE(kollsuma) >= 0.009 THEN DO:                 
               MESSAGE "Summan för kundreskontran stämmer ej. Den ska vara " 
               kolltabeltemp.NYFAKT
               ". Den är " ACCUM TOTAL BY faktkundtemp.VFAKTNR faktkundtemp.BELOPP "."
               VIEW-AS ALERT-BOX.
               musz = TRUE.
               RAD_KONTO = 1.
               DISPLAY RAD_KONTO WITH FRAME {&FRAME-NAME}.
               APPLY "VALUE-CHANGED" TO RAD_KONTO IN FRAME {&FRAME-NAME}.
               RETURN.
            END.
         END.
      END.
   END.
   FOR EACH faktinkontotemp WHERE 
   faktinkontotemp.FAKTNR = infakplannr AND 
   faktinkontotemp.FDELNR = fdelnrvar NO-LOCK BREAK BY faktinkontotemp.VFAKTNR:
      ACCUMULATE 
      faktinkontotemp.BELOPP (TOTAL BY faktinkontotemp.VFAKTNR).
      IF LAST-OF(faktinkontotemp.VFAKTNR) THEN DO:         
         IF kolltabeltemp.NYFAKT NE 
         (ACCUM TOTAL BY faktinkontotemp.VFAKTNR faktinkontotemp.BELOPP) THEN DO:
            kollsuma = kolltabeltemp.NYFAKT - 
            (ACCUM TOTAL BY faktinkontotemp.VFAKTNR faktinkontotemp.BELOPP).
            IF ABSOLUTE(kollsuma) >= 0.009 THEN DO: 
               MESSAGE "Summan för intäktskonteringen stämmer ej. Den ska vara " 
               kolltabeltemp.NYFAKT
               ". Den är " ACCUM TOTAL BY faktinkontotemp.VFAKTNR faktinkontotemp.BELOPP "."
               VIEW-AS ALERT-BOX.
               musz = TRUE.
               RAD_KONTO = 2.
               DISPLAY RAD_KONTO WITH FRAME {&FRAME-NAME}.
               APPLY "VALUE-CHANGED" TO RAD_KONTO IN FRAME {&FRAME-NAME}.
               RETURN.
            END.
         END.
      END. 
   END.
   IF faktmomstemp.MOMSEXTERNT = 0 THEN.
   ELSE DO: 
      IF kolltabeltemp.SLUTFAKT = TRUE AND kolltabeltemp.GMOMS = 0 THEN DO:
         IF kolltabeltemp.ACONTO = TRUE THEN DO:
            FOR EACH faktmomstemp WHERE faktmomstemp.FAKTNR = infakplannr AND 
            faktmomstemp.FDELNR = fdelnrvar NO-LOCK BREAK BY faktmomstemp.VFAKTNR: 
               ACCUMULATE 
               faktmomstemp.BELOPP (TOTAL BY faktmomstemp.VFAKTNR).
               IF LAST-OF(faktmomstemp.VFAKTNR) THEN DO:                        
                  IF kolltabeltemp.NYFAKT + kolltabeltemp.GFAKT NE 
                  (ACCUM TOTAL BY faktmomstemp.VFAKTNR faktmomstemp.BELOPP) 
                  THEN DO:            
                     kollsuma = (kolltabeltemp.NYFAKT + kolltabeltemp.GFAKT) -
                     (ACCUM TOTAL BY faktmomstemp.VFAKTNR faktmomstemp.BELOPP). 
                     IF ABSOLUTE(kollsuma) >= 0.009 THEN DO: 
                        MESSAGE "Summan för momsberäkningen stämmer ej. Den ska vara " 
                        kolltabeltemp.DENNAFAKT  
                        ". Den är " ACCUM TOTAL BY faktmomstemp.VFAKTNR faktmomstemp.BELOPP "."
                        VIEW-AS ALERT-BOX.
                        musz = TRUE.
                        RAD_KONTO = 4.
                        DISPLAY RAD_KONTO WITH FRAME {&FRAME-NAME}.
                        APPLY "VALUE-CHANGED" TO RAD_KONTO IN FRAME {&FRAME-NAME}.
                        RETURN.
                     END.
                  END.
               END.
            END.
         END.
         ELSE DO:
            FOR EACH faktmomstemp WHERE faktmomstemp.FAKTNR = infakplannr AND 
            faktmomstemp.FDELNR = fdelnrvar NO-LOCK BREAK BY faktmomstemp.VFAKTNR: 
               ACCUMULATE 
               faktmomstemp.BELOPP (TOTAL BY faktmomstemp.VFAKTNR).
               IF LAST-OF(faktmomstemp.VFAKTNR) THEN DO:                        
                  IF kolltabeltemp.NYFAKT + kolltabeltemp.GFAKT NE 
                  (ACCUM TOTAL BY faktmomstemp.VFAKTNR faktmomstemp.BELOPP) 
                  THEN DO:            
                     kollsuma = (kolltabeltemp.NYFAKT + kolltabeltemp.GFAKT) -
                     (ACCUM TOTAL BY faktmomstemp.VFAKTNR faktmomstemp.BELOPP). 
                     IF ABSOLUTE(kollsuma) >= 0.009 THEN DO:              
                        MESSAGE "Summan för momsberäkningen stämmer ej. Den ska vara " 
                        kolltabeltemp.NYFAKT + kolltabeltemp.GFAKT
                        ". Den är " ACCUM TOTAL BY faktmomstemp.VFAKTNR faktmomstemp.BELOPP "."
                        VIEW-AS ALERT-BOX.
                        musz = TRUE.
                        RAD_KONTO = 4.
                        DISPLAY RAD_KONTO WITH FRAME {&FRAME-NAME}.
                        APPLY "VALUE-CHANGED" TO RAD_KONTO IN FRAME {&FRAME-NAME}.
                        RETURN.
                     END.
                  END.
               END.
            END. 
         END.
      END.
      ELSE DO:
         FOR EACH faktmomstemp WHERE faktmomstemp.FAKTNR = infakplannr AND 
         faktmomstemp.FDELNR = fdelnrvar NO-LOCK BREAK BY faktmomstemp.VFAKTNR: 
            ACCUMULATE                    
            faktmomstemp.BELOPP (TOTAL BY faktmomstemp.VFAKTNR).
            IF LAST-OF(faktmomstemp.VFAKTNR) THEN DO:                     
               IF kolltabeltemp.NYFAKT NE (ACCUM TOTAL BY faktmomstemp.VFAKTNR faktmomstemp.BELOPP) THEN DO:            
                  kollsuma = kolltabeltemp.NYFAKT - (ACCUM TOTAL BY faktmomstemp.VFAKTNR faktmomstemp.BELOPP).
                  IF ABSOLUTE(kollsuma) >= 0.009 THEN DO:                   
                     MESSAGE "Summan för momsberäkningen stämmer ej. Den ska vara " 
                     kolltabeltemp.NYFAKT
                     ". Den är " ACCUM TOTAL BY faktmomstemp.VFAKTNR faktmomstemp.BELOPP "."
                     VIEW-AS ALERT-BOX.
                     musz = TRUE.
                     RAD_KONTO = 4.
                     DISPLAY RAD_KONTO WITH FRAME {&FRAME-NAME}.
                     APPLY "VALUE-CHANGED" TO RAD_KONTO IN FRAME {&FRAME-NAME}.
                     RETURN.
                  END.
               END.
            END. 
         END.
      END.               
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE bort_UI Dialog-Frame 
PROCEDURE bort_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER rowbort AS ROWID NO-UNDO.
   RUN kontonbort_UI IN faktupphmth (INPUT RAD_KONTO,INPUT rowbort).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE bytmot_UI Dialog-Frame 
PROCEDURE bytmot_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/     
   IF NOT AVAILABLE motparttemp THEN DO:
      MESSAGE "Du måste välja någon post."   
      VIEW-AS ALERT-BOX .
      RETURN.
   END.   
   MESSAGE "Vill du verkligen byta motpart på samtliga poster ?"   
   VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Bortag av kontering"
   UPDATE answer1 AS LOGICAL.
   IF answer1 = FALSE THEN DO: 
      RETURN.            
   END.
   bytvar = motparttemp.MOTPARTID.
   RUN nykonton_UI IN faktupphmth (INPUT 13,INPUT infakplannr,INPUT fdelnrvar,INPUT bytvar,
   INPUT-OUTPUT TABLE kolltabeltemp,
   OUTPUT TABLE faktkundtemp,OUTPUT TABLE faktinkontotemp,OUTPUT TABLE faktmomstemp).
   RUN open_UI.   
   {musarrow.i}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE dela_UI Dialog-Frame 
PROCEDURE dela_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/   
   EMPTY TEMP-TABLE efaktkundtemp NO-ERROR. 
   EMPTY TEMP-TABLE efaktinkontotemp NO-ERROR. 
   EMPTY TEMP-TABLE efaktmomstemp NO-ERROR. 
   IF RAD_KONTO = 1 THEN DO:
      CREATE efaktkundtemp.
      BUFFER-COPY faktkundtemp TO efaktkundtemp.
   END.
   ELSE IF RAD_KONTO = 2 THEN DO:
      CREATE efaktinkontotemp.
      BUFFER-COPY faktinkontotemp TO efaktinkontotemp.
   END.
   IF RAD_KONTO = 4 THEN DO:
      CREATE efaktmomstemp.
      BUFFER-COPY faktmomstemp TO efaktmomstemp.
   END.   
   RUN KONTDELAA.W (INPUT RAD_KONTO,
                    INPUT-OUTPUT TABLE efaktkundtemp,INPUT-OUTPUT TABLE efaktinkontotemp,
                    INPUT-OUTPUT TABLE efaktmomstemp,INPUT TABLE faktaonrtemp).      
   IF musz = TRUE THEN DO:
      musz = FALSE.
      RETURN.
   END.
   RUN kontondela_UI IN faktupphmth (INPUT infakplannr,INPUT fdelnrvar,INPUT RAD_KONTO,
   INPUT TABLE efaktkundtemp,INPUT TABLE efaktinkontotemp,INPUT TABLE efaktmomstemp).        
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dialog-Frame  _DEFAULT-DISABLE
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
  HIDE FRAME Dialog-Frame.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dialog-Frame  _DEFAULT-ENABLE
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
  DISPLAY RAD_KONTO 
      WITH FRAME Dialog-Frame.
  ENABLE RAD_KONTO BRW_K4 BRW_K3 BRW_VIS BRW_K2 BRW_K1 BTN_ny BTN_DELA BTN_BORT 
         BTN_OK 
      WITH FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE nybelopp_UI Dialog-Frame 
PROCEDURE nybelopp_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   /*TAKPRIS*/      
   {muswait.i}
   bytvar = 0.   
   RUN nykonton_UI IN faktupphmth (INPUT 1,INPUT infakplannr,INPUT fdelnrvar,INPUT bytvar,
   INPUT-OUTPUT TABLE kolltabeltemp,
   OUTPUT TABLE faktkundtemp,OUTPUT TABLE faktinkontotemp,OUTPUT TABLE faktmomstemp).

      
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ny_UI Dialog-Frame 
PROCEDURE ny_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   EMPTY TEMP-TABLE efaktkundtemp NO-ERROR. 
   EMPTY TEMP-TABLE efaktinkontotemp NO-ERROR. 
   EMPTY TEMP-TABLE efaktmomstemp NO-ERROR. 
   IF RAD_KONTO = 1 THEN DO:
      status-ok = BRW_K1:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
      CREATE efaktkundtemp.
      BUFFER-COPY faktkundtemp TO efaktkundtemp.
      efaktkundtemp.FROW = ?.      
   END.
   ELSE IF RAD_KONTO = 2 THEN DO:
      status-ok = BRW_K2:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
      CREATE efaktinkontotemp.
      BUFFER-COPY faktinkontotemp TO efaktinkontotemp.
      efaktinkontotemp.FROW = ?.      
   END.
   IF RAD_KONTO = 4 THEN DO:
      status-ok = BRW_K4:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
      CREATE efaktmomstemp.
      BUFFER-COPY faktmomstemp TO efaktmomstemp.
      efaktmomstemp.FROW = ?.      
   END.
   
   RUN KONTDELAA.W (INPUT RAD_KONTO,
                    INPUT-OUTPUT TABLE efaktkundtemp,
                    INPUT-OUTPUT TABLE efaktinkontotemp,
                    INPUT-OUTPUT TABLE efaktmomstemp,
                    INPUT TABLE faktaonrtemp).
   IF musz = FALSE THEN DO:
      RUN kontondela_UI IN faktupphmth (INPUT infakplannr,INPUT fdelnrvar,INPUT RAD_KONTO,
      INPUT TABLE efaktkundtemp,INPUT TABLE efaktinkontotemp,INPUT TABLE efaktmomstemp).        
   END.
   musz = FALSE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE open_UI Dialog-Frame 
PROCEDURE open_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/  
   RUN openbdyn_UI IN brwproc[1] (INPUT "").      
   RUN openbdyn_UI IN brwproc[2] (INPUT "").      
   RUN openbdyn_UI IN brwproc[4] (INPUT "").         
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE viskont_UI Dialog-Frame 
PROCEDURE viskont_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   RUN viskont_UI IN faktupphmth (INPUT infakplannr,INPUT fdelnrvar,
   OUTPUT TABLE viskonttemp).
   /*RUN openbdyn_UI IN brwproc[5] (INPUT "").*/
   OPEN QUERY BRW_VIS FOR EACH viskonttemp NO-LOCK.       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

