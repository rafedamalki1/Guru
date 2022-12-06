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
DEFINE INPUT PARAMETER valrad AS INTEGER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER valrec AS RECID NO-UNDO. 
/* Local Variable Definitions ---                                       */
{ALLDEF.I}

DEFINE SHARED VARIABLE fakkoproch AS HANDLE NO-UNDO. /* FAKKAPP.P */
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.

&SCOPED-DEFINE NEW
&SCOPED-DEFINE SHARED SHARED
{FAKKOTEMP.I}
DEFINE TEMP-TABLE extrastyrkfmtemp NO-UNDO LIKE styrkfmtemp.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame
&Scoped-define BROWSE-NAME BRW_FAKT

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES faktureringstyptemp kundfodrantemp ~
intakttemp motparttemp momstemp kundtyptemp

/* Definitions for BROWSE BRW_FAKT                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_FAKT faktureringstyptemp.FAKTTYPTEXT 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_FAKT 
&Scoped-define QUERY-STRING-BRW_FAKT FOR EACH faktureringstyptemp NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BRW_FAKT OPEN QUERY BRW_FAKT FOR EACH faktureringstyptemp NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BRW_FAKT faktureringstyptemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_FAKT faktureringstyptemp


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
momstemp.MOMSEXTERNT 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_K4 
&Scoped-define QUERY-STRING-BRW_K4 FOR EACH momstemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_K4 OPEN QUERY BRW_K4 FOR EACH momstemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_K4 momstemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_K4 momstemp


/* Definitions for BROWSE BRW_KTYP                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_KTYP kundtyptemp.KUNDTYP 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_KTYP 
&Scoped-define QUERY-STRING-BRW_KTYP FOR EACH kundtyptemp NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BRW_KTYP OPEN QUERY BRW_KTYP FOR EACH kundtyptemp NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BRW_KTYP kundtyptemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_KTYP kundtyptemp


/* Definitions for DIALOG-BOX Dialog-Frame                              */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BRW_KTYP BRW_K1 BRW_K3 BRW_FAKT BRW_K4 ~
BRW_K2 BTN_OK BTN_AVB 

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

DEFINE VARIABLE FILL-IN_FAKTTYP AS CHARACTER FORMAT "X(45)" 
     LABEL "Faktureringstyp" 
     VIEW-AS FILL-IN 
     SIZE 30.25 BY 1.

DEFINE VARIABLE FILL-IN_INTAKTKONTO AS CHARACTER FORMAT "X(8)" 
     LABEL "Inäktskonto" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1.

DEFINE VARIABLE FILL-IN_KUNDKONTO AS CHARACTER FORMAT "X(8)" 
     LABEL "Kundfodranskonto" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1.

DEFINE VARIABLE FILL-IN_KUNDTYP AS CHARACTER FORMAT "X(30)" 
     LABEL "Kundtyp" 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1.

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
DEFINE QUERY BRW_FAKT FOR 
      faktureringstyptemp SCROLLING.

DEFINE QUERY BRW_K1 FOR 
      kundfodrantemp SCROLLING.

DEFINE QUERY BRW_K2 FOR 
      intakttemp SCROLLING.

DEFINE QUERY BRW_K3 FOR 
      motparttemp SCROLLING.

DEFINE QUERY BRW_K4 FOR 
      momstemp SCROLLING.

DEFINE QUERY BRW_KTYP FOR 
      kundtyptemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_FAKT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_FAKT Dialog-Frame _STRUCTURED
  QUERY BRW_FAKT NO-LOCK DISPLAY
      faktureringstyptemp.FAKTTYPTEXT COLUMN-LABEL "Faktureringstyp" FORMAT "X(45)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 31 BY 10.

DEFINE BROWSE BRW_K1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_K1 Dialog-Frame _STRUCTURED
  QUERY BRW_K1 NO-LOCK DISPLAY
      kundfodrantemp.KUNDKONTO COLUMN-LABEL "Kundfodrans!konto" FORMAT "X(8)":U
      kundfodrantemp.KUNDFODTEXT COLUMN-LABEL "Text" FORMAT "X(20)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 35 BY 10.

DEFINE BROWSE BRW_K2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_K2 Dialog-Frame _STRUCTURED
  QUERY BRW_K2 NO-LOCK DISPLAY
      intakttemp.INTAKTKONTO COLUMN-LABEL "Intäkts!konto" FORMAT "X(6)":U
      intakttemp.INTAKTTEXT COLUMN-LABEL "Text" FORMAT "X(35)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 46 BY 10.

DEFINE BROWSE BRW_K3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_K3 Dialog-Frame _STRUCTURED
  QUERY BRW_K3 NO-LOCK DISPLAY
      motparttemp.MOTPART COLUMN-LABEL "Motpart" FORMAT "X(8)":U
      motparttemp.MOTPARTTEXT COLUMN-LABEL "Text" FORMAT "X(20)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 32 BY 10.

DEFINE BROWSE BRW_K4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_K4 Dialog-Frame _STRUCTURED
  QUERY BRW_K4 NO-LOCK DISPLAY
      momstemp.MOMSKOD COLUMN-LABEL "Moms!konto" FORMAT "X(8)":U
      momstemp.MOMSTEXT COLUMN-LABEL "Text" FORMAT "X(35)":U
      momstemp.MOMSEXTERNT COLUMN-LABEL "Moms!i procent" FORMAT ">>9.99":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 56.5 BY 10.

DEFINE BROWSE BRW_KTYP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_KTYP Dialog-Frame _STRUCTURED
  QUERY BRW_KTYP NO-LOCK DISPLAY
      kundtyptemp.KUNDTYP COLUMN-LABEL "Kundtyp" FORMAT "X(20)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 23 BY 10.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     BRW_KTYP AT ROW 2.92 COL 1.5
     BRW_K1 AT ROW 2.92 COL 25.88
     BRW_K3 AT ROW 2.92 COL 27.38
     BRW_FAKT AT ROW 2.92 COL 27.88
     BRW_K4 AT ROW 2.92 COL 62
     BRW_K2 AT ROW 13.33 COL 62
     FILL-IN_KUNDTYP AT ROW 15.63 COL 23.5 COLON-ALIGNED
     FILL-IN_KUNDKONTO AT ROW 16.83 COL 23.5 COLON-ALIGNED
     FILL-IN_MOTPART AT ROW 16.83 COL 23.63 COLON-ALIGNED
     FILL-IN_FAKTTYP AT ROW 16.83 COL 23.63 COLON-ALIGNED
     FILL-IN_MOMSKOD AT ROW 18.04 COL 23.5 COLON-ALIGNED
     FILL-IN_INTAKTKONTO AT ROW 19.25 COL 23.5 COLON-ALIGNED
     BTN_OK AT ROW 23.67 COL 89.5
     BTN_AVB AT ROW 23.67 COL 104.5
     "Välj poster som skall styra" VIEW-AS TEXT
          SIZE 40.75 BY .96 AT ROW 1.5 COL 1.5
          FONT 17
     "Valda styrparametrar:" VIEW-AS TEXT
          SIZE 30.13 BY 1.33 AT ROW 13.79 COL 12.88
          FONT 17
     SPACE(75.98) SKIP(9.91)
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
/* BROWSE-TAB BRW_KTYP TEXT-1 Dialog-Frame */
/* BROWSE-TAB BRW_K1 BRW_KTYP Dialog-Frame */
/* BROWSE-TAB BRW_K3 BRW_K1 Dialog-Frame */
/* BROWSE-TAB BRW_FAKT BRW_K3 Dialog-Frame */
/* BROWSE-TAB BRW_K4 BRW_FAKT Dialog-Frame */
/* BROWSE-TAB BRW_K2 BRW_K4 Dialog-Frame */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

ASSIGN 
       BRW_FAKT:ALLOW-COLUMN-SEARCHING IN FRAME Dialog-Frame = TRUE.

ASSIGN 
       BRW_K1:ALLOW-COLUMN-SEARCHING IN FRAME Dialog-Frame = TRUE.

ASSIGN 
       BRW_K2:ALLOW-COLUMN-SEARCHING IN FRAME Dialog-Frame = TRUE.

ASSIGN 
       BRW_K3:ALLOW-COLUMN-SEARCHING IN FRAME Dialog-Frame = TRUE.

ASSIGN 
       BRW_K4:ALLOW-COLUMN-SEARCHING IN FRAME Dialog-Frame = TRUE.

ASSIGN 
       BRW_KTYP:ALLOW-COLUMN-SEARCHING IN FRAME Dialog-Frame = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_FAKTTYP IN FRAME Dialog-Frame
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN_FAKTTYP:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_INTAKTKONTO IN FRAME Dialog-Frame
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN_INTAKTKONTO:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_KUNDKONTO IN FRAME Dialog-Frame
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN_KUNDKONTO:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_KUNDTYP IN FRAME Dialog-Frame
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN_KUNDTYP:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_MOMSKOD IN FRAME Dialog-Frame
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN_MOMSKOD:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_MOTPART IN FRAME Dialog-Frame
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN_MOTPART:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_FAKT
/* Query rebuild information for BROWSE BRW_FAKT
     _TblList          = "Temp-Tables.faktureringstyptemp"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > Temp-Tables.faktureringstyptemp.FAKTTYPTEXT
"faktureringstyptemp.FAKTTYPTEXT" "Faktureringstyp" "X(45)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_FAKT */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_K1
/* Query rebuild information for BROWSE BRW_K1
     _TblList          = "Temp-Tables.kundfodrantemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.kundfodrantemp.KUNDKONTO
"kundfodrantemp.KUNDKONTO" "Kundfodrans!konto" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.kundfodrantemp.KUNDFODTEXT
"kundfodrantemp.KUNDFODTEXT" "Text" "X(20)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_K1 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_K2
/* Query rebuild information for BROWSE BRW_K2
     _TblList          = "Temp-Tables.intakttemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.intakttemp.INTAKTKONTO
"intakttemp.INTAKTKONTO" "Intäkts!konto" "X(6)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.intakttemp.INTAKTTEXT
"intakttemp.INTAKTTEXT" "Text" "X(35)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_K2 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_K3
/* Query rebuild information for BROWSE BRW_K3
     _TblList          = "Temp-Tables.motparttemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.motparttemp.MOTPART
"motparttemp.MOTPART" "Motpart" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.motparttemp.MOTPARTTEXT
"motparttemp.MOTPARTTEXT" "Text" "X(20)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_K3 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_K4
/* Query rebuild information for BROWSE BRW_K4
     _TblList          = "Temp-Tables.momstemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.momstemp.MOMSKOD
"momstemp.MOMSKOD" "Moms!konto" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.momstemp.MOMSTEXT
"momstemp.MOMSTEXT" "Text" "X(35)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.momstemp.MOMSEXTERNT
"momstemp.MOMSEXTERNT" "Moms!i procent" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_K4 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_KTYP
/* Query rebuild information for BROWSE BRW_KTYP
     _TblList          = "Temp-Tables.kundtyptemp"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > Temp-Tables.kundtyptemp.KUNDTYP
"kundtyptemp.KUNDTYP" "Kundtyp" "X(20)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_KTYP */
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
   musz = TRUE.
   IF valrec NE ? THEN DO: 
      FIND FIRST extrastyrkfmtemp WHERE extrastyrkfmtemp.STYRREC = valrec NO-LOCK NO-ERROR.
      IF AVAILABLE extrastyrkfmtemp THEN DO:
         FIND FIRST styrkfmtemp WHERE styrkfmtemp.STYRREC = valrec NO-LOCK NO-ERROR.
         IF AVAILABLE styrkfmtemp THEN DO:
            BUFFER-COPY extrastyrkfmtemp TO styrkfmtemp.
         END.
      END.
   END.
   ELSE DO :
      FIND FIRST styrkfmtemp WHERE styrkfmtemp.STYRVAL < 5 NO-LOCK NO-ERROR.
      IF AVAILABLE styrkfmtemp THEN DO:
         DELETE styrkfmtemp.
      END.
   END.
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


&Scoped-define BROWSE-NAME BRW_FAKT
&Scoped-define SELF-NAME BRW_FAKT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_FAKT Dialog-Frame
ON VALUE-CHANGED OF BRW_FAKT IN FRAME Dialog-Frame
DO:
   IF AVAILABLE faktureringstyptemp THEN DO:
      FILL-IN_FAKTTYP = faktureringstyptemp.FAKTTYPTEXT.
      BUFFER-COPY faktureringstyptemp TO styrkfmtemp.                 
      DISPLAY FILL-IN_FAKTTYP WITH FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_K1
&Scoped-define SELF-NAME BRW_K1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_K1 Dialog-Frame
ON VALUE-CHANGED OF BRW_K1 IN FRAME Dialog-Frame
DO:
   IF AVAILABLE kundfodrantemp THEN DO:
      FILL-IN_KUNDKONTO = kundfodrantemp.KUNDKONTO.
      BUFFER-COPY kundfodrantemp TO styrkfmtemp.                
      DISPLAY FILL-IN_KUNDKONTO WITH FRAME {&FRAME-NAME}.   
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_K2
&Scoped-define SELF-NAME BRW_K2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_K2 Dialog-Frame
ON VALUE-CHANGED OF BRW_K2 IN FRAME Dialog-Frame
DO:
   IF AVAILABLE intakttemp THEN DO:
      FILL-IN_INTAKTKONTO = intakttemp.INTAKTKONTO.
      BUFFER-COPY intakttemp TO styrkfmtemp.                 
      DISPLAY FILL-IN_INTAKTKONTO WITH FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_K3
&Scoped-define SELF-NAME BRW_K3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_K3 Dialog-Frame
ON VALUE-CHANGED OF BRW_K3 IN FRAME Dialog-Frame
DO:
   IF AVAILABLE motparttemp THEN DO:
      FILL-IN_MOTPART = motparttemp.MOTPART.
      BUFFER-COPY motparttemp TO styrkfmtemp.            
      DISPLAY FILL-IN_MOTPART WITH FRAME {&FRAME-NAME}. 
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_K4
&Scoped-define SELF-NAME BRW_K4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_K4 Dialog-Frame
ON VALUE-CHANGED OF BRW_K4 IN FRAME Dialog-Frame
DO:
   IF AVAILABLE momstemp THEN DO:
      FILL-IN_MOMSKOD = momstemp.MOMSKOD.
      BUFFER-COPY momstemp TO styrkfmtemp.         
      DISPLAY FILL-IN_MOMSKOD WITH FRAME {&FRAME-NAME}. 
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_KTYP
&Scoped-define SELF-NAME BRW_KTYP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_KTYP Dialog-Frame
ON VALUE-CHANGED OF BRW_KTYP IN FRAME Dialog-Frame
DO:
   IF AVAILABLE kundtyptemp THEN DO:
      FILL-IN_KUNDTYP = kundtyptemp.KUNDTYP.
      BUFFER-COPY kundtyptemp TO styrkfmtemp.
      DISPLAY FILL-IN_KUNDTYP WITH FRAME {&FRAME-NAME}. 
   END.
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
   DEFINE VARIABLE tempstr AS CHARACTER NO-UNDO.
   /* Specialkontroll för att kolla om alla browsrar har en rad markerad. */
   tempstr = STRING(BRW_FAKT:NUM-SELECTED-ROWS) + STRING(BRW_K1:NUM-SELECTED-ROWS) + 
      STRING(BRW_K2:NUM-SELECTED-ROWS) + STRING(BRW_K3:NUM-SELECTED-ROWS) + STRING(BRW_K4:NUM-SELECTED-ROWS) + STRING(BRW_KTYP:NUM-SELECTED-ROWS).
   IF valrad = 5 AND tempstr NE "010001" OR valrad = 6 AND tempstr NE "101011" OR 
      valrad = 7 AND tempstr NE "000101" OR valrad = 8 AND tempstr NE "100011" THEN DO:
      
      MESSAGE "Du måste markera en rad i varje scrollista." VIEW-AS ALERT-BOX.
   END.
   ELSE DO:
      RUN sparastyr_UI IN fakkoproch (INPUT-OUTPUT valrec, INPUT-OUTPUT TABLE styrkfmtemp).
      {BORTBRWPROC.I}
      APPLY "GO":U TO FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_FAKT
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
      FRAME {&FRAME-NAME}:HIDDEN = FALSE. 
   {ALLSTARTDYN.I}
   RUN enable_UI.       
   {FRMSIZED.I} 

   RUN huvud_UI.
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
      (INPUT BRW_FAKT:HANDLE IN FRAME {&FRAME-NAME}).
   RUN DYNBRW.P PERSISTENT SET brwproc[6] 
      (INPUT BRW_KTYP:HANDLE IN FRAME {&FRAME-NAME}).
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
  ENABLE BRW_KTYP BRW_K1 BRW_K3 BRW_FAKT BRW_K4 BRW_K2 BTN_OK BTN_AVB 
      WITH FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE huvud_UI Dialog-Frame 
PROCEDURE huvud_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   ASSIGN
   BRW_FAKT:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
   BRW_K1:HIDDEN IN FRAME {&FRAME-NAME} = TRUE 
   BRW_K2:HIDDEN IN FRAME {&FRAME-NAME} = TRUE 
   BRW_K3:HIDDEN IN FRAME {&FRAME-NAME} = TRUE 
   BRW_K4:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
   RUN openbdynspec_UI IN brwproc[6].
   IF valrec NE ? THEN DO: 
      FIND FIRST styrkfmtemp WHERE styrkfmtemp.STYRREC = valrec NO-LOCK NO-ERROR.
      IF AVAILABLE styrkfmtemp THEN DO:
         CREATE extrastyrkfmtemp.
         BUFFER-COPY styrkfmtemp TO extrastyrkfmtemp.
      END.
   END.
   IF AVAILABLE styrkfmtemp THEN DO:
      FIND FIRST kundtyptemp WHERE kundtyptemp.KUNDID = styrkfmtemp.KUNDID NO-LOCK NO-ERROR.
      IF AVAILABLE kundtyptemp THEN DO: 
         FILL-IN_KUNDTYP = kundtyptemp.KUNDTYP.
         RUN selectbyrowid_UI IN brwproc[6] (INPUT ROWID(kundtyptemp)).            
      END.
   END.
   IF valrad = 5 THEN DO:
      BRW_K1:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
      DISPLAY FILL-IN_KUNDTYP FILL-IN_KUNDKONTO WITH FRAME {&FRAME-NAME}.
      RUN openbdynspec_UI IN brwproc[1].
      IF AVAILABLE styrkfmtemp THEN DO:
         FIND FIRST kundfodrantemp WHERE kundfodrantemp.KUNDKONTOID = styrkfmtemp.KUNDKONTOID NO-LOCK NO-ERROR.
         IF AVAILABLE kundfodrantemp THEN DO: 
            FILL-IN_KUNDKONTO = kundfodrantemp.KUNDKONTO.
            RUN selectbyrowid_UI IN brwproc[1] (INPUT ROWID(kundfodrantemp)).      
         END.        
      END.
      
   END.
   ELSE IF valrad = 6 THEN DO:
      ASSIGN
      BRW_FAKT:HIDDEN IN FRAME {&FRAME-NAME} = FALSE      
      BRW_K2:HIDDEN IN FRAME {&FRAME-NAME} = FALSE 
      BRW_K4:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
      RUN openbdynspec_UI IN brwproc[2].
      RUN openbdynspec_UI IN brwproc[4].
      RUN openbdynspec_UI IN brwproc[5].
      IF AVAILABLE styrkfmtemp THEN DO:
         FIND FIRST intakttemp WHERE intakttemp.INTAKTID = styrkfmtemp.INTAKTID NO-LOCK NO-ERROR.
         IF AVAILABLE intakttemp THEN DO: 
            FILL-IN_INTAKTKONTO = intakttemp.INTAKTKONTO.     
            RUN selectbyrowid_UI IN brwproc[2] (INPUT ROWID(intakttemp)).      
         END.
         FIND FIRST momstemp WHERE momstemp.MOMSID = styrkfmtemp.MOMSID NO-LOCK NO-ERROR.
         IF AVAILABLE momstemp THEN DO: 
            FILL-IN_MOMSKOD = momstemp.MOMSKOD.
            RUN selectbyrowid_UI IN brwproc[4] (INPUT ROWID(momstemp)).      
         END.
         FIND FIRST faktureringstyptemp WHERE faktureringstyptemp.FAKTTYPID = styrkfmtemp.FAKTTYPID NO-LOCK NO-ERROR.
         IF AVAILABLE faktureringstyptemp THEN DO: 
            FILL-IN_FAKTTYP = faktureringstyptemp.FAKTTYPTEXT. 
            RUN selectbyrowid_UI IN brwproc[5] (INPUT ROWID(faktureringstyptemp)).      
         END.                          
      END.
      DISPLAY FILL-IN_KUNDTYP FILL-IN_FAKTTYP FILL-IN_MOMSKOD FILL-IN_INTAKTKONTO WITH FRAME {&FRAME-NAME}.
   END.
   ELSE IF valrad = 7 THEN DO:
      BRW_K3:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.       
      DISPLAY FILL-IN_KUNDTYP FILL-IN_MOTPART WITH FRAME {&FRAME-NAME}.
      RUN openbdynspec_UI IN brwproc[3].
      IF AVAILABLE styrkfmtemp THEN DO:
         FIND FIRST motparttemp WHERE motparttemp.MOTPARTID = styrkfmtemp.MOTPARTID NO-LOCK NO-ERROR.
         IF AVAILABLE motparttemp THEN DO: 
            FILL-IN_MOTPART = motparttemp.MOTPART.
            RUN selectbyrowid_UI IN brwproc[3] (INPUT ROWID(motparttemp)).  
         END.
      END.
      
   END.
   ELSE IF valrad = 8 THEN DO:
      RUN openbdynspec_UI IN brwproc[4].
      RUN openbdynspec_UI IN brwproc[5].
      IF AVAILABLE styrkfmtemp THEN DO:
         FIND FIRST momstemp WHERE momstemp.MOMSID = styrkfmtemp.MOMSID NO-LOCK NO-ERROR.
         IF AVAILABLE momstemp THEN DO: 
            FILL-IN_MOMSKOD = momstemp.MOMSKOD.
            RUN selectbyrowid_UI IN brwproc[4] (INPUT ROWID(momstemp)).      
         END.
         FIND FIRST faktureringstyptemp WHERE faktureringstyptemp.FAKTTYPID = styrkfmtemp.FAKTTYPID NO-LOCK NO-ERROR.
         IF AVAILABLE faktureringstyptemp THEN DO: 
            FILL-IN_FAKTTYP = faktureringstyptemp.FAKTTYPTEXT. 
            RUN selectbyrowid_UI IN brwproc[5] (INPUT ROWID(faktureringstyptemp)). 
         END.
      END.
      ASSIGN
      BRW_FAKT:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      BRW_K4:HIDDEN IN FRAME {&FRAME-NAME} = FALSE. 
      DISPLAY FILL-IN_KUNDTYP FILL-IN_FAKTTYP FILL-IN_MOMSKOD WITH FRAME {&FRAME-NAME}.
   END.  
   IF valrec = ? THEN DO:
      CREATE styrkfmtemp.
      ASSIGN
      styrkfmtemp.STYRVAL = valrad - 4. /* STYRVAL kan få värdet 1-4 beroende på vilken RADIO-BUTTON som är satt */
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

