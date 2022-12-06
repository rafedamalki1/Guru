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

/* Parameters Definitions ---                                           */
{EXTRADATA.I}
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR inextradatatemp.
/* Local Variable Definitions ---                                       */
{ALLDEF.I}
{GLOBVAR2DEL1.I}
DEFINE SHARED VARIABLE fakkoproch AS HANDLE NO-UNDO. /* FAKKAPP.P */
DEFINE VARIABLE ktoproch AS HANDLE NO-UNDO. /* Procedure handle till KTOAPP.P */
DEFINE VARIABLE brwsortvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE momsnrvar AS LOGICAL NO-UNDO.
&SCOPED-DEFINE SHARED SHARED
{FAKKOTEMP.I}

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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame
&Scoped-define BROWSE-NAME BRW_K-1

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ktotemp kundfodrantemp intakttemp ~
motparttemp momstemp

/* Definitions for BROWSE BRW_K-1                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_K-1 ktotemp.KONTONR ktotemp.BENAMNING 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_K-1 
&Scoped-define QUERY-STRING-BRW_K-1 FOR EACH ktotemp ~
      WHERE ktotemp.KONTO = "k1" NO-LOCK
&Scoped-define OPEN-QUERY-BRW_K-1 OPEN QUERY BRW_K-1 FOR EACH ktotemp ~
      WHERE ktotemp.KONTO = "k1" NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_K-1 ktotemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_K-1 ktotemp


/* Definitions for BROWSE BRW_K-2                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_K-2 ktotemp.KONTONR ktotemp.BENAMNING 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_K-2 
&Scoped-define QUERY-STRING-BRW_K-2 FOR EACH ktotemp ~
      WHERE ktotemp.KONTO = "k2" NO-LOCK
&Scoped-define OPEN-QUERY-BRW_K-2 OPEN QUERY BRW_K-2 FOR EACH ktotemp ~
      WHERE ktotemp.KONTO = "k2" NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_K-2 ktotemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_K-2 ktotemp


/* Definitions for BROWSE BRW_K-3                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_K-3 ktotemp.KONTONR ktotemp.BENAMNING 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_K-3 
&Scoped-define QUERY-STRING-BRW_K-3 FOR EACH ktotemp ~
      WHERE ktotemp.KONTO = "k3" NO-LOCK
&Scoped-define OPEN-QUERY-BRW_K-3 OPEN QUERY BRW_K-3 FOR EACH ktotemp ~
      WHERE ktotemp.KONTO = "k3" NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_K-3 ktotemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_K-3 ktotemp


/* Definitions for BROWSE BRW_K-4                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_K-4 ktotemp.KONTONR ktotemp.BENAMNING 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_K-4 
&Scoped-define QUERY-STRING-BRW_K-4 FOR EACH ktotemp ~
      WHERE ktotemp.KONTO = "k4" NO-LOCK
&Scoped-define OPEN-QUERY-BRW_K-4 OPEN QUERY BRW_K-4 FOR EACH ktotemp ~
      WHERE ktotemp.KONTO = "k4" NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_K-4 ktotemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_K-4 ktotemp


/* Definitions for BROWSE BRW_K-5                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_K-5 ktotemp.KONTONR ktotemp.BENAMNING 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_K-5 
&Scoped-define QUERY-STRING-BRW_K-5 FOR EACH ktotemp ~
      WHERE ktotemp.KONTO = "k5" NO-LOCK
&Scoped-define OPEN-QUERY-BRW_K-5 OPEN QUERY BRW_K-5 FOR EACH ktotemp ~
      WHERE ktotemp.KONTO = "k5" NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_K-5 ktotemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_K-5 ktotemp


/* Definitions for BROWSE BRW_K1                                        */
&Scoped-define FIELDS-IN-QUERY-BRW_K1 kundfodrantemp.KUNDKONTO ~
kundfodrantemp.KUNDFODTEXT 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_K1 
&Scoped-define QUERY-STRING-BRW_K1 FOR EACH kundfodrantemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_K1 OPEN QUERY BRW_K1 FOR EACH kundfodrantemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_K1 kundfodrantemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_K1 kundfodrantemp


/* Definitions for BROWSE BRW_K2                                        */
&Scoped-define FIELDS-IN-QUERY-BRW_K2 intakttemp.INTAKTKONTO ~
intakttemp.INTAKTTEXT 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_K2 
&Scoped-define QUERY-STRING-BRW_K2 FOR EACH intakttemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_K2 OPEN QUERY BRW_K2 FOR EACH intakttemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_K2 intakttemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_K2 intakttemp


/* Definitions for BROWSE BRW_K3                                        */
&Scoped-define FIELDS-IN-QUERY-BRW_K3 motparttemp.MOTPART ~
motparttemp.MOTPARTTEXT 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_K3 
&Scoped-define QUERY-STRING-BRW_K3 FOR EACH motparttemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_K3 OPEN QUERY BRW_K3 FOR EACH motparttemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_K3 motparttemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_K3 motparttemp


/* Definitions for BROWSE BRW_K4                                        */
&Scoped-define FIELDS-IN-QUERY-BRW_K4 momstemp.MOMSKOD momstemp.MOMSTEXT ~
momstemp.MOMSNR 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_K4 
&Scoped-define QUERY-STRING-BRW_K4 FOR EACH momstemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_K4 OPEN QUERY BRW_K4 FOR EACH momstemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_K4 momstemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_K4 momstemp


/* Definitions for DIALOG-BOX Dialog-Frame                              */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BRW_K1 BRW_K2 BRW_K3 BRW_K4 BRW_K-1 BRW_K-2 ~
BRW_K-3 BRW_K-4 BRW_K-5 FILL-IN_KUNDKONTO FILL-IN_INTAKTKONTO ~
FILL-IN_MOTPART FILL-IN_MOMSKOD FILL-IN_K-1 FILL-IN_K-2 FILL-IN_K-3 ~
FILL-IN_K-4 FILL-IN_K-5 BTN_OK BTN_AVB 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-POST FILL-IN_KUNDKONTO ~
FILL-IN_INTAKTKONTO FILL-IN_MOTPART FILL-IN_MOMSKOD FILL-IN_K-1 FILL-IN_K-2 ~
FILL-IN_K-3 FILL-IN_K-4 FILL-IN_K-5 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB 
     LABEL "Avbryt" 
     SIZE 14 BY 1
     BGCOLOR 8 .

DEFINE BUTTON BTN_OK 
     LABEL "Ok" 
     SIZE 14 BY 1
     BGCOLOR 8 .

DEFINE VARIABLE FILL-IN-POST AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 52 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN_INTAKTKONTO AS CHARACTER FORMAT "X(8)" 
     LABEL "Inäktskonto" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1.

DEFINE VARIABLE FILL-IN_K-1 AS CHARACTER FORMAT "X(6)" 
     LABEL "k1" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1.

DEFINE VARIABLE FILL-IN_K-2 AS CHARACTER FORMAT "X(6)" 
     LABEL "k2" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1.

DEFINE VARIABLE FILL-IN_K-3 AS CHARACTER FORMAT "X(6)" 
     LABEL "k3" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1.

DEFINE VARIABLE FILL-IN_K-4 AS CHARACTER FORMAT "X(6)" 
     LABEL "k4" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1.

DEFINE VARIABLE FILL-IN_K-5 AS CHARACTER FORMAT "X(6)" 
     LABEL "k5" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1.

DEFINE VARIABLE FILL-IN_KUNDKONTO AS CHARACTER FORMAT "X(8)" 
     LABEL "Kundfodranskonto" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1.

DEFINE VARIABLE FILL-IN_MOMSKOD AS CHARACTER FORMAT "X(8)" 
     LABEL "Momskonto" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1.

DEFINE VARIABLE FILL-IN_MOTPART AS CHARACTER FORMAT "X(8)" 
     LABEL "Motpart" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_K-1 FOR 
      ktotemp SCROLLING.

DEFINE QUERY BRW_K-2 FOR 
      ktotemp SCROLLING.

DEFINE QUERY BRW_K-3 FOR 
      ktotemp SCROLLING.

DEFINE QUERY BRW_K-4 FOR 
      ktotemp SCROLLING.

DEFINE QUERY BRW_K-5 FOR 
      ktotemp SCROLLING.

DEFINE QUERY BRW_K1 FOR 
      kundfodrantemp SCROLLING.

DEFINE QUERY BRW_K2 FOR 
      intakttemp SCROLLING.

DEFINE QUERY BRW_K3 FOR 
      motparttemp SCROLLING.

DEFINE QUERY BRW_K4 FOR 
      momstemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_K-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_K-1 Dialog-Frame _STRUCTURED
  QUERY BRW_K-1 NO-LOCK DISPLAY
      ktotemp.KONTONR COLUMN-LABEL "Kod" FORMAT "x(6)":U
      ktotemp.BENAMNING COLUMN-LABEL "Benämning" FORMAT "X(256)":U
            WIDTH 15
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 24.5 BY 9.

DEFINE BROWSE BRW_K-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_K-2 Dialog-Frame _STRUCTURED
  QUERY BRW_K-2 NO-LOCK DISPLAY
      ktotemp.KONTONR COLUMN-LABEL "Kod" FORMAT "x(6)":U
      ktotemp.BENAMNING COLUMN-LABEL "Benämning" FORMAT "X(256)":U
            WIDTH 15
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 24.5 BY 9.

DEFINE BROWSE BRW_K-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_K-3 Dialog-Frame _STRUCTURED
  QUERY BRW_K-3 NO-LOCK DISPLAY
      ktotemp.KONTONR COLUMN-LABEL "Kod" FORMAT "x(6)":U
      ktotemp.BENAMNING COLUMN-LABEL "Benämning" FORMAT "X(256)":U
            WIDTH 15
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 24.5 BY 9.

DEFINE BROWSE BRW_K-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_K-4 Dialog-Frame _STRUCTURED
  QUERY BRW_K-4 NO-LOCK DISPLAY
      ktotemp.KONTONR COLUMN-LABEL "Kod" FORMAT "x(6)":U
      ktotemp.BENAMNING COLUMN-LABEL "Benämning" FORMAT "X(256)":U
            WIDTH 15
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 24.5 BY 9.

DEFINE BROWSE BRW_K-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_K-5 Dialog-Frame _STRUCTURED
  QUERY BRW_K-5 NO-LOCK DISPLAY
      ktotemp.KONTONR COLUMN-LABEL "Kod" FORMAT "x(6)":U
      ktotemp.BENAMNING COLUMN-LABEL "Benämning" FORMAT "X(256)":U
            WIDTH 15
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 24.5 BY 9.

DEFINE BROWSE BRW_K1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_K1 Dialog-Frame _STRUCTURED
  QUERY BRW_K1 NO-LOCK DISPLAY
      kundfodrantemp.KUNDKONTO COLUMN-LABEL "Konto" FORMAT "X(6)":U
      kundfodrantemp.KUNDFODTEXT COLUMN-LABEL "Kundfodranskonto" FORMAT "X(256)":U
            WIDTH 20
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 29.88 BY 10.21.

DEFINE BROWSE BRW_K2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_K2 Dialog-Frame _STRUCTURED
  QUERY BRW_K2 NO-LOCK DISPLAY
      intakttemp.INTAKTKONTO COLUMN-LABEL "Konto" FORMAT "X(6)":U
      intakttemp.INTAKTTEXT COLUMN-LABEL "Intäktskonto" FORMAT "X(256)":U
            WIDTH 20
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 29.88 BY 10.21.

DEFINE BROWSE BRW_K3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_K3 Dialog-Frame _STRUCTURED
  QUERY BRW_K3 NO-LOCK DISPLAY
      motparttemp.MOTPART COLUMN-LABEL "Konto" FORMAT "X(6)":U
      motparttemp.MOTPARTTEXT COLUMN-LABEL "Motpart" FORMAT "X(256)":U
            WIDTH 18
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 27.88 BY 10.21.

DEFINE BROWSE BRW_K4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_K4 Dialog-Frame _STRUCTURED
  QUERY BRW_K4 NO-LOCK DISPLAY
      momstemp.MOMSKOD COLUMN-LABEL "Konto" FORMAT "X(6)":U
      momstemp.MOMSTEXT COLUMN-LABEL "Momskonto" FORMAT "X(256)":U
            WIDTH 20
      momstemp.MOMSNR COLUMN-LABEL "Moms!kod" FORMAT "X(4)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 34.13 BY 10.21.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     BRW_K1 AT ROW 3 COL 1.5
     BRW_K2 AT ROW 3 COL 32.13
     BRW_K3 AT ROW 3 COL 62.75
     BRW_K4 AT ROW 3 COL 91.25
     BRW_K-1 AT ROW 13.46 COL 1.5
     BRW_K-2 AT ROW 13.46 COL 26.38
     BRW_K-3 AT ROW 13.46 COL 51.25
     BRW_K-4 AT ROW 13.46 COL 76.13
     BRW_K-5 AT ROW 13.46 COL 100.88
     FILL-IN-POST AT ROW 23.38 COL 40 COLON-ALIGNED NO-LABEL
     FILL-IN_KUNDKONTO AT ROW 24.67 COL 22 COLON-ALIGNED
     FILL-IN_INTAKTKONTO AT ROW 24.67 COL 33.75
     FILL-IN_MOTPART AT ROW 24.67 COL 60.5
     FILL-IN_MOMSKOD AT ROW 24.67 COL 81.25
     FILL-IN_K-1 AT ROW 26.08 COL 20 AUTO-RETURN 
     FILL-IN_K-2 AT ROW 26.08 COL 42.75 AUTO-RETURN 
     FILL-IN_K-3 AT ROW 26.08 COL 65.5 AUTO-RETURN 
     FILL-IN_K-4 AT ROW 26.08 COL 88.25 AUTO-RETURN 
     FILL-IN_K-5 AT ROW 26.08 COL 113 COLON-ALIGNED AUTO-RETURN 
     BTN_OK AT ROW 27.63 COL 96.63
     BTN_AVB AT ROW 27.63 COL 111.63
     "Välj konton genom att dubbel klicka" VIEW-AS TEXT
          SIZE 40.75 BY .96 AT ROW 1.33 COL 1.5
          FONT 17
     "Valda konton för:" VIEW-AS TEXT
          SIZE 20.63 BY 1.33 AT ROW 23.04 COL 1.5
          FONT 17
     SPACE(103.74) SKIP(4.62)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Styrning av konton".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Allow: Basic,Browse,DB-Fields,Query
   Temp-Tables and Buffers:
      TABLE: faktureringstyptemp T "?" NO-UNDO temp-db faktureringstyptemp
      TABLE: intakttemp T "?" NO-UNDO temp-db intakttemp
      TABLE: ktotemp T "?" NO-UNDO temp-db ktotemp
      TABLE: kundfodrantemp T "?" NO-UNDO temp-db kundfodrantemp
      TABLE: kundtyptemp T "?" NO-UNDO temp-db kundtyptemp
      TABLE: momstemp T "?" NO-UNDO temp-db momstemp
      TABLE: motparttemp T "?" NO-UNDO temp-db motparttemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   NOT-VISIBLE                                                          */
/* BROWSE-TAB BRW_K1 TEXT-1 Dialog-Frame */
/* BROWSE-TAB BRW_K2 BRW_K1 Dialog-Frame */
/* BROWSE-TAB BRW_K3 BRW_K2 Dialog-Frame */
/* BROWSE-TAB BRW_K4 BRW_K3 Dialog-Frame */
/* BROWSE-TAB BRW_K-1 BRW_K4 Dialog-Frame */
/* BROWSE-TAB BRW_K-2 BRW_K-1 Dialog-Frame */
/* BROWSE-TAB BRW_K-3 BRW_K-2 Dialog-Frame */
/* BROWSE-TAB BRW_K-4 BRW_K-3 Dialog-Frame */
/* BROWSE-TAB BRW_K-5 BRW_K-4 Dialog-Frame */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

ASSIGN 
       BRW_K-1:ALLOW-COLUMN-SEARCHING IN FRAME Dialog-Frame = TRUE
       BRW_K-1:COLUMN-RESIZABLE IN FRAME Dialog-Frame       = TRUE.

ASSIGN 
       BRW_K-2:ALLOW-COLUMN-SEARCHING IN FRAME Dialog-Frame = TRUE
       BRW_K-2:COLUMN-RESIZABLE IN FRAME Dialog-Frame       = TRUE.

ASSIGN 
       BRW_K-3:ALLOW-COLUMN-SEARCHING IN FRAME Dialog-Frame = TRUE
       BRW_K-3:COLUMN-RESIZABLE IN FRAME Dialog-Frame       = TRUE.

ASSIGN 
       BRW_K-4:ALLOW-COLUMN-SEARCHING IN FRAME Dialog-Frame = TRUE
       BRW_K-4:COLUMN-RESIZABLE IN FRAME Dialog-Frame       = TRUE.

ASSIGN 
       BRW_K-5:ALLOW-COLUMN-SEARCHING IN FRAME Dialog-Frame = TRUE
       BRW_K-5:COLUMN-RESIZABLE IN FRAME Dialog-Frame       = TRUE.

ASSIGN 
       BRW_K1:ALLOW-COLUMN-SEARCHING IN FRAME Dialog-Frame = TRUE
       BRW_K1:COLUMN-RESIZABLE IN FRAME Dialog-Frame       = TRUE.

ASSIGN 
       BRW_K2:ALLOW-COLUMN-SEARCHING IN FRAME Dialog-Frame = TRUE
       BRW_K2:COLUMN-RESIZABLE IN FRAME Dialog-Frame       = TRUE.

ASSIGN 
       BRW_K3:ALLOW-COLUMN-SEARCHING IN FRAME Dialog-Frame = TRUE
       BRW_K3:COLUMN-RESIZABLE IN FRAME Dialog-Frame       = TRUE.

ASSIGN 
       BRW_K4:ALLOW-COLUMN-SEARCHING IN FRAME Dialog-Frame = TRUE
       BRW_K4:COLUMN-RESIZABLE IN FRAME Dialog-Frame       = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-POST IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN_INTAKTKONTO IN FRAME Dialog-Frame
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FILL-IN_K-1 IN FRAME Dialog-Frame
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FILL-IN_K-2 IN FRAME Dialog-Frame
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FILL-IN_K-3 IN FRAME Dialog-Frame
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FILL-IN_K-4 IN FRAME Dialog-Frame
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FILL-IN_MOMSKOD IN FRAME Dialog-Frame
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FILL-IN_MOTPART IN FRAME Dialog-Frame
   ALIGN-L                                                              */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_K-1
/* Query rebuild information for BROWSE BRW_K-1
     _TblList          = "Temp-Tables.ktotemp"
     _Options          = "NO-LOCK"
     _Where[1]         = "Temp-Tables.ktotemp.KONTO = ""k1"""
     _FldNameList[1]   > Temp-Tables.ktotemp.KONTONR
"ktotemp.KONTONR" "Kod" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.ktotemp.BENAMNING
"ktotemp.BENAMNING" "Benämning" "X(256)" "character" ? ? ? ? ? ? no ? no no "15" yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_K-1 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_K-2
/* Query rebuild information for BROWSE BRW_K-2
     _TblList          = "Temp-Tables.ktotemp"
     _Options          = "NO-LOCK"
     _Where[1]         = "Temp-Tables.ktotemp.KONTO = ""k2"""
     _FldNameList[1]   > Temp-Tables.ktotemp.KONTONR
"ktotemp.KONTONR" "Kod" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.ktotemp.BENAMNING
"ktotemp.BENAMNING" "Benämning" "X(256)" "character" ? ? ? ? ? ? no ? no no "15" yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_K-2 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_K-3
/* Query rebuild information for BROWSE BRW_K-3
     _TblList          = "Temp-Tables.ktotemp"
     _Options          = "NO-LOCK"
     _Where[1]         = "Temp-Tables.ktotemp.KONTO = ""k3"""
     _FldNameList[1]   > Temp-Tables.ktotemp.KONTONR
"ktotemp.KONTONR" "Kod" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.ktotemp.BENAMNING
"ktotemp.BENAMNING" "Benämning" "X(256)" "character" ? ? ? ? ? ? no ? no no "15" yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_K-3 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_K-4
/* Query rebuild information for BROWSE BRW_K-4
     _TblList          = "Temp-Tables.ktotemp"
     _Options          = "NO-LOCK"
     _Where[1]         = "Temp-Tables.ktotemp.KONTO = ""k4"""
     _FldNameList[1]   > Temp-Tables.ktotemp.KONTONR
"ktotemp.KONTONR" "Kod" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.ktotemp.BENAMNING
"ktotemp.BENAMNING" "Benämning" "X(256)" "character" ? ? ? ? ? ? no ? no no "15" yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_K-4 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_K-5
/* Query rebuild information for BROWSE BRW_K-5
     _TblList          = "Temp-Tables.ktotemp"
     _Options          = "NO-LOCK"
     _Where[1]         = "Temp-Tables.ktotemp.KONTO = ""k5"""
     _FldNameList[1]   > Temp-Tables.ktotemp.KONTONR
"ktotemp.KONTONR" "Kod" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.ktotemp.BENAMNING
"ktotemp.BENAMNING" "Benämning" "X(256)" "character" ? ? ? ? ? ? no ? no no "15" yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_K-5 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_K1
/* Query rebuild information for BROWSE BRW_K1
     _TblList          = "Temp-Tables.kundfodrantemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.kundfodrantemp.KUNDKONTO
"kundfodrantemp.KUNDKONTO" "Konto" "X(6)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.kundfodrantemp.KUNDFODTEXT
"kundfodrantemp.KUNDFODTEXT" "Kundfodranskonto" "X(256)" "character" ? ? ? ? ? ? no ? no no "20" yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_K1 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_K2
/* Query rebuild information for BROWSE BRW_K2
     _TblList          = "Temp-Tables.intakttemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.intakttemp.INTAKTKONTO
"intakttemp.INTAKTKONTO" "Konto" "X(6)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.intakttemp.INTAKTTEXT
"intakttemp.INTAKTTEXT" "Intäktskonto" "X(256)" "character" ? ? ? ? ? ? no ? no no "20" yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_K2 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_K3
/* Query rebuild information for BROWSE BRW_K3
     _TblList          = "Temp-Tables.motparttemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.motparttemp.MOTPART
"motparttemp.MOTPART" "Konto" "X(6)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.motparttemp.MOTPARTTEXT
"motparttemp.MOTPARTTEXT" "Motpart" "X(256)" "character" ? ? ? ? ? ? no ? no no "18" yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_K3 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_K4
/* Query rebuild information for BROWSE BRW_K4
     _TblList          = "Temp-Tables.momstemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.momstemp.MOMSKOD
"momstemp.MOMSKOD" "Konto" "X(6)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.momstemp.MOMSTEXT
"momstemp.MOMSTEXT" "Momskonto" "X(256)" "character" ? ? ? ? ? ? no ? no no "20" yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.momstemp.MOMSNR
"momstemp.MOMSNR" "Moms!kod" "X(4)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_K4 */
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
ON END-ERROR OF FRAME Dialog-Frame /* Styrning av konton */
DO:
   /*IF VALID-HANDLE(fakkoproch) THEN DELETE PROCEDURE fakkoproch. */
   IF VALID-HANDLE(ktoproch) THEN DELETE PROCEDURE ktoproch.
   {BORTBRWPROC.I}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON ENDKEY OF FRAME Dialog-Frame /* Styrning av konton */
DO:
   APPLY "END-ERROR":U TO FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Styrning av konton */
DO:
   APPLY "END-ERROR":U TO FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_K-1
&Scoped-define SELF-NAME BRW_K-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_K-1 Dialog-Frame
ON MOUSE-SELECT-DBLCLICK OF BRW_K-1 IN FRAME Dialog-Frame
DO:
   RUN andra_UI (INPUT 11).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_K-2
&Scoped-define SELF-NAME BRW_K-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_K-2 Dialog-Frame
ON MOUSE-SELECT-DBLCLICK OF BRW_K-2 IN FRAME Dialog-Frame
DO:
   RUN andra_UI (INPUT 12).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_K-3
&Scoped-define SELF-NAME BRW_K-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_K-3 Dialog-Frame
ON MOUSE-SELECT-DBLCLICK OF BRW_K-3 IN FRAME Dialog-Frame
DO:
   RUN andra_UI (INPUT 13).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_K-4
&Scoped-define SELF-NAME BRW_K-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_K-4 Dialog-Frame
ON MOUSE-SELECT-DBLCLICK OF BRW_K-4 IN FRAME Dialog-Frame
DO:
   RUN andra_UI (INPUT 14).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_K-5
&Scoped-define SELF-NAME BRW_K-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_K-5 Dialog-Frame
ON MOUSE-SELECT-DBLCLICK OF BRW_K-5 IN FRAME Dialog-Frame
DO:
   RUN andra_UI (INPUT 15).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_K1
&Scoped-define SELF-NAME BRW_K1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_K1 Dialog-Frame
ON MOUSE-SELECT-DBLCLICK OF BRW_K1 IN FRAME Dialog-Frame
DO:
   RUN andra_UI (INPUT 1).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_K2
&Scoped-define SELF-NAME BRW_K2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_K2 Dialog-Frame
ON MOUSE-SELECT-DBLCLICK OF BRW_K2 IN FRAME Dialog-Frame
DO:
   RUN andra_UI (INPUT 2).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_K3
&Scoped-define SELF-NAME BRW_K3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_K3 Dialog-Frame
ON MOUSE-SELECT-DBLCLICK OF BRW_K3 IN FRAME Dialog-Frame
DO:
   RUN andra_UI (INPUT 3).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_K4
&Scoped-define SELF-NAME BRW_K4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_K4 Dialog-Frame
ON MOUSE-SELECT-DBLCLICK OF BRW_K4 IN FRAME Dialog-Frame
DO:
   RUN andra_UI (INPUT 4).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB Dialog-Frame
ON CHOOSE OF BTN_AVB IN FRAME Dialog-Frame /* Avbryt */
DO:
   APPLY "END-ERROR":U TO FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK Dialog-Frame
ON CHOOSE OF BTN_OK IN FRAME Dialog-Frame /* Ok */
DO:
   
   ASSIGN
   FILL-IN_K-1 = INPUT FILL-IN_K-1
   FILL-IN_K-2 = INPUT FILL-IN_K-2
   FILL-IN_K-3 = INPUT FILL-IN_K-3
   FILL-IN_K-4 = INPUT FILL-IN_K-4
   FILL-IN_K-5 = INPUT FILL-IN_K-5
   FILL-IN_KUNDKONTO   = INPUT FILL-IN_KUNDKONTO  
   FILL-IN_INTAKTKONTO = INPUT FILL-IN_INTAKTKONTO
   FILL-IN_MOTPART     = INPUT FILL-IN_MOTPART    
   FILL-IN_MOMSKOD     = INPUT FILL-IN_MOMSKOD.    
   
   FIND FIRST inextradatatemp NO-LOCK NO-ERROR.
   IF FILL-IN_KUNDKONTO NE "" THEN DO:
      FIND FIRST kundfodrantemp WHERE kundfodrantemp.KUNDKONTO  = FILL-IN_KUNDKONTO              NO-LOCK NO-ERROR.
      IF AVAILABLE kundfodrantemp THEN inextradatatemp.SOKINT[1] = kundfodrantemp.KUNDKONTOID.
      ELSE DO:
         MESSAGE "Fel på kundfodranskontot."
         VIEW-AS ALERT-BOX.
         RETURN NO-APPLY.
      END.
   END.
   ELSE inextradatatemp.SOKINT[1] = 0.
   IF FILL-IN_INTAKTKONTO NE "" THEN DO:
      FIND FIRST intakttemp WHERE intakttemp.INTAKTKONTO    = FILL-IN_INTAKTKONTO           NO-LOCK NO-ERROR.
      IF AVAILABLE intakttemp THEN inextradatatemp.SOKINT[2] = intakttemp.INTAKTID.         
      ELSE DO:
         MESSAGE "Fel på intäktskontot."
         VIEW-AS ALERT-BOX.
         RETURN NO-APPLY.
      END.
   END.
   ELSE inextradatatemp.SOKINT[2] = 0.
   IF FILL-IN_MOTPART NE "" THEN DO:
      FIND FIRST motparttemp WHERE motparttemp.MOTPART       = FILL-IN_MOTPART        NO-LOCK NO-ERROR.
      IF AVAILABLE motparttemp THEN inextradatatemp.SOKINT[3] = motparttemp.MOTPARTID.              
      ELSE DO:
         MESSAGE "Fel på motpart"
         VIEW-AS ALERT-BOX.
         RETURN NO-APPLY.
      END.
   END.
   ELSE inextradatatemp.SOKINT[3] = 0.
   IF FILL-IN_MOMSKOD NE "" THEN DO:
      IF momsnrvar = TRUE THEN DO:
         FIND FIRST momstemp WHERE momstemp.MOMSNR = FILL-IN_MOMSKOD NO-LOCK NO-ERROR.
      END.
      ELSE DO:
         FIND FIRST momstemp WHERE momstemp.MOMSKOD = FILL-IN_MOMSKOD NO-LOCK NO-ERROR.
      END.
      IF AVAILABLE momstemp THEN inextradatatemp.SOKINT[4] = momstemp.MOMSID .             
      ELSE DO:
         MESSAGE "Fel på momskontot."
         VIEW-AS ALERT-BOX.
         RETURN NO-APPLY.
      END.
   END.
   ELSE inextradatatemp.SOKINT[4] = 0.
   ASSIGN
   inextradatatemp.SOKCHAR[1] = FILL-IN_K-1 
   inextradatatemp.SOKCHAR[2] = FILL-IN_K-2 
   inextradatatemp.SOKCHAR[3] = FILL-IN_K-3 
   inextradatatemp.SOKCHAR[4] = FILL-IN_K-4 
   inextradatatemp.SOKCHAR[5] = FILL-IN_K-5 
   inextradatatemp.SOKCHAR[6] = FILL-IN-POST.
   /*IF VALID-HANDLE(fakkoproch) THEN DELETE PROCEDURE fakkoproch. */
   IF VALID-HANDLE(ktoproch) THEN DELETE PROCEDURE ktoproch.
   {BORTBRWPROC.I}
   APPLY "GO":U TO FRAME {&FRAME-NAME}.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_K-1
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
   RUN kbjurhamt_UI IN ktoproch (INPUT Guru.Konstanter:globforetag,OUTPUT TABLE kbtemp,OUTPUT TABLE jurtemp).  
   FIND FIRST kbtemp NO-ERROR.
   ASSIGN
   ktotemp.BENAMNING:LABEL IN BROWSE BRW_K-1 = CAPS(SUBSTRING(kbtemp.K1,1,1)) + LC(SUBSTRING(kbtemp.K1,2))
   ktotemp.BENAMNING:LABEL IN BROWSE BRW_K-2 = CAPS(SUBSTRING(kbtemp.K2,1,1)) + LC(SUBSTRING(kbtemp.K2,2))
   ktotemp.BENAMNING:LABEL IN BROWSE BRW_K-3 = CAPS(SUBSTRING(kbtemp.K3,1,1)) + LC(SUBSTRING(kbtemp.K3,2))
   ktotemp.BENAMNING:LABEL IN BROWSE BRW_K-4 = CAPS(SUBSTRING(kbtemp.K4,1,1)) + LC(SUBSTRING(kbtemp.K4,2))
   ktotemp.BENAMNING:LABEL IN BROWSE BRW_K-5 = CAPS(SUBSTRING(kbtemp.K5,1,1)) + LC(SUBSTRING(kbtemp.K5,2))
   FILL-IN_K-1:LABEL = CAPS(SUBSTRING(kbtemp.K1,1,1)) + LC(SUBSTRING(kbtemp.K1,2))
   FILL-IN_K-2:LABEL = CAPS(SUBSTRING(kbtemp.K2,1,1)) + LC(SUBSTRING(kbtemp.K2,2))
   FILL-IN_K-3:LABEL = CAPS(SUBSTRING(kbtemp.K3,1,1)) + LC(SUBSTRING(kbtemp.K3,2))
   FILL-IN_K-4:LABEL = CAPS(SUBSTRING(kbtemp.K4,1,1)) + LC(SUBSTRING(kbtemp.K4,2))
   FILL-IN_K-5:LABEL = CAPS(SUBSTRING(kbtemp.K5,1,1)) + LC(SUBSTRING(kbtemp.K5,2)).
   
   RUN ktohamt_UI IN ktoproch (INPUT Guru.Konstanter:globforetag,OUTPUT TABLE ktotemp).  
   /*
   RUN laddakontotemp_UI IN fakkoproch 
      (OUTPUT TABLE kundfodrantemp, 
       OUTPUT TABLE intakttemp, 
       OUTPUT TABLE motparttemp, 
       OUTPUT TABLE momstemp).
       */
   FIND FIRST inextradatatemp NO-LOCK NO-ERROR.
   FIND FIRST kundfodrantemp WHERE kundfodrantemp.KUNDKONTOID = inextradatatemp.SOKINT[1] NO-LOCK NO-ERROR.
   IF AVAILABLE kundfodrantemp THEN FILL-IN_KUNDKONTO = kundfodrantemp.KUNDKONTO.
   FIND FIRST intakttemp WHERE intakttemp.INTAKTID = inextradatatemp.SOKINT[2] NO-LOCK NO-ERROR.
   IF AVAILABLE intakttemp THEN FILL-IN_INTAKTKONTO = intakttemp.INTAKTKONTO.
   FIND FIRST motparttemp WHERE motparttemp.MOTPARTID = inextradatatemp.SOKINT[3] NO-LOCK NO-ERROR.
   IF AVAILABLE motparttemp THEN FILL-IN_MOTPART = motparttemp.MOTPART.
   FIND FIRST momstemp WHERE momstemp.MOMSID = inextradatatemp.SOKINT[4] NO-LOCK NO-ERROR.
   IF AVAILABLE momstemp THEN DO:
      IF momstemp.MOMSKOD = "" THEN DO:
         ASSIGN
         momsnrvar = TRUE 
         FILL-IN_MOMSKOD = momstemp.MOMSNR.
      END.
      ELSE DO:
         ASSIGN
         momsnrvar = FALSE
         FILL-IN_MOMSKOD = momstemp.MOMSKOD.
      END.      
   END.
   ASSIGN
   FILL-IN_K-1 = inextradatatemp.SOKCHAR[1]
   FILL-IN_K-2 = inextradatatemp.SOKCHAR[2]
   FILL-IN_K-3 = inextradatatemp.SOKCHAR[3]
   FILL-IN_K-4 = inextradatatemp.SOKCHAR[4]
   FILL-IN_K-5 = inextradatatemp.SOKCHAR[5]
   FILL-IN-POST = inextradatatemp.SOKCHAR[6].     
   RUN enable_UI.
   RUN openbdyn_UI IN brwproc[1] (INPUT "").      
   RUN openbdyn_UI IN brwproc[2] (INPUT "").      
   RUN openbdyn_UI IN brwproc[3] (INPUT "").      
   RUN openbdyn_UI IN brwproc[4] (INPUT "").      
   IF kbtemp.K1 = "" THEN DO:
      ASSIGN
      FILL-IN_K-1:HIDDEN = TRUE
      BRW_K-1:HIDDEN = TRUE.
   END.
   ELSE RUN openbdynspec_UI IN brwproc[5].
   IF kbtemp.K2 = "" THEN DO:
      ASSIGN
      FILL-IN_K-2:HIDDEN = TRUE
      BRW_K-2:HIDDEN = TRUE.
   END.
   ELSE RUN openbdynspec_UI IN brwproc[6].
   IF kbtemp.K3 = "" THEN DO:
      ASSIGN
      FILL-IN_K-3:HIDDEN = TRUE
      BRW_K-3:HIDDEN = TRUE.
   END.
   ELSE RUN openbdynspec_UI IN brwproc[7].
   IF kbtemp.K4 = "" THEN DO:
      ASSIGN
      FILL-IN_K-4:HIDDEN = TRUE
      BRW_K-4:HIDDEN = TRUE.
   END.
   ELSE RUN openbdynspec_UI IN brwproc[8].
   IF kbtemp.K5 = "" THEN DO:
      ASSIGN
      FILL-IN_K-5:HIDDEN = TRUE
      BRW_K-5:HIDDEN = TRUE.
   END.
   ELSE RUN openbdynspec_UI IN brwproc[9].
   
   {FRMSIZED.I} 
   {DIA_M_SLUT.I}
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
      (INPUT BRW_K-1:HANDLE IN FRAME {&FRAME-NAME}).
   RUN DYNBRW.P PERSISTENT SET brwproc[6] 
      (INPUT BRW_K-2:HANDLE IN FRAME {&FRAME-NAME}).
   RUN DYNBRW.P PERSISTENT SET brwproc[7] 
      (INPUT BRW_K-3:HANDLE IN FRAME {&FRAME-NAME}).
   RUN DYNBRW.P PERSISTENT SET brwproc[8] 
      (INPUT BRW_K-4:HANDLE IN FRAME {&FRAME-NAME}).
   RUN DYNBRW.P PERSISTENT SET brwproc[9] 
      (INPUT BRW_K-5:HANDLE IN FRAME {&FRAME-NAME}).
   IF Guru.Konstanter:appcon THEN DO:                           
      RUN KTOAPP.P PERSISTENT SET ktoproch ON Guru.Konstanter:apphand TRANSACTION DISTINCT (INPUT Guru.Konstanter:globanv).
   END.
   ELSE DO:
      RUN KTOAPP.P PERSISTENT SET ktoproch (INPUT Guru.Konstanter:globanv).   
   END. 
   /*
   IF Guru.Konstanter:appcon THEN DO:
      RUN FAKKAPP.P PERSISTENT SET fakkoproch ON Guru.Konstanter:apphand TRANSACTION DISTINCT.         
   END.
   ELSE DO:
      RUN FAKKAPP.P PERSISTENT SET fakkoproch.         
   END.
   */
   brwsortvar = 'ktotemp.KONTO = "K1"'.     
   RUN setcolsortvar_UI IN brwproc[5] (INPUT brwsortvar).
   brwsortvar = 'ktotemp.KONTO = "K2"'.      
   RUN setcolsortvar_UI IN brwproc[6] (INPUT brwsortvar).
   brwsortvar = 'ktotemp.KONTO = "K3"'.      
   RUN setcolsortvar_UI IN brwproc[7] (INPUT brwsortvar).
   brwsortvar = 'ktotemp.KONTO = "K4"'.      
   RUN setcolsortvar_UI IN brwproc[8] (INPUT brwsortvar).
   brwsortvar = 'ktotemp.KONTO = "K5"'.      
   RUN setcolsortvar_UI IN brwproc[9] (INPUT brwsortvar).
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE andra_UI Dialog-Frame 
PROCEDURE andra_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER vad AS INTEGER NO-UNDO.
   IF vad = 1 THEN DO:
      FILL-IN_KUNDKONTO = kundfodrantemp.KUNDKONTO.
      DISPLAY FILL-IN_KUNDKONTO WITH FRAME {&FRAME-NAME}.   
   END.
   
   IF vad = 2 THEN DO:
      FILL-IN_INTAKTKONTO = intakttemp.INTAKTKONTO.
      DISPLAY FILL-IN_INTAKTKONTO WITH FRAME {&FRAME-NAME}.
   END.
   IF vad = 3 THEN DO:
      FILL-IN_MOTPART = motparttemp.MOTPART.
      DISPLAY FILL-IN_MOTPART WITH FRAME {&FRAME-NAME}. 
   END.
   IF vad = 4 THEN DO:
      IF momstemp.MOMSKOD = "" THEN DO:
         ASSIGN
         momsnrvar = TRUE
         FILL-IN_MOMSKOD = momstemp.MOMSNR.
      END.
      ELSE DO:
         ASSIGN
         momsnrvar = FALSE
         FILL-IN_MOMSKOD = momstemp.MOMSKOD.
      END.
      DISPLAY FILL-IN_MOMSKOD WITH FRAME {&FRAME-NAME}. 
   END.
   IF vad = 11 THEN DO:
      FILL-IN_K-1 = ktotemp.KONTONR.
      DISPLAY FILL-IN_K-1 WITH FRAME {&FRAME-NAME}. 
   END.
   IF vad = 12 THEN DO:
      FILL-IN_K-2 = ktotemp.KONTONR.
      DISPLAY FILL-IN_K-2 WITH FRAME {&FRAME-NAME}. 
   END.
   IF vad = 13 THEN DO:
      FILL-IN_K-3 = ktotemp.KONTONR.
      DISPLAY FILL-IN_K-3 WITH FRAME {&FRAME-NAME}. 
   END.
   IF vad = 14 THEN DO:
      FILL-IN_K-4 = ktotemp.KONTONR.
      DISPLAY FILL-IN_K-4 WITH FRAME {&FRAME-NAME}. 
   END.
   IF vad = 15 THEN DO:
      FILL-IN_K-5 = ktotemp.KONTONR.
      DISPLAY FILL-IN_K-5 WITH FRAME {&FRAME-NAME}. 
   END.
   
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
  DISPLAY FILL-IN-POST FILL-IN_KUNDKONTO FILL-IN_INTAKTKONTO FILL-IN_MOTPART 
          FILL-IN_MOMSKOD FILL-IN_K-1 FILL-IN_K-2 FILL-IN_K-3 FILL-IN_K-4 
          FILL-IN_K-5 
      WITH FRAME Dialog-Frame.
  ENABLE BRW_K1 BRW_K2 BRW_K3 BRW_K4 BRW_K-1 BRW_K-2 BRW_K-3 BRW_K-4 BRW_K-5 
         FILL-IN_KUNDKONTO FILL-IN_INTAKTKONTO FILL-IN_MOTPART FILL-IN_MOMSKOD 
         FILL-IN_K-1 FILL-IN_K-2 FILL-IN_K-3 FILL-IN_K-4 FILL-IN_K-5 BTN_OK 
         BTN_AVB 
      WITH FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

