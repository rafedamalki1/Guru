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
&Scoped-define NEW 
{GLOBVAR2DEL1.I}
&Scoped-define SHARED SHARED 
{FAKKOTEMP.I}
{FAKTAONR.I}
DEFINE INPUT PARAMETER kontyp AS INTEGER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR efaktkundtemp.
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR efaktinkontotemp.
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR efaktmomstemp.
DEFINE INPUT PARAMETER TABLE FOR faktaonrtemp.


/* Local Variable Definitions ---                                       */
&Scoped-define SHARED
{ALLDEF.I}
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.  
DEFINE SHARED VARIABLE regdatum AS DATE NO-UNDO.
DEFINE VARIABLE stat AS LOGICAL NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.



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
&Scoped-define INTERNAL-TABLES kundfodrantemp intakttemp motparttemp ~
momstemp

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


/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define OPEN-BROWSERS-IN-QUERY-Dialog-Frame ~
    ~{&OPEN-QUERY-BRW_K1}~
    ~{&OPEN-QUERY-BRW_K2}~
    ~{&OPEN-QUERY-BRW_K3}~
    ~{&OPEN-QUERY-BRW_K4}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BRW_K1 BRW_K2 CMB_AONR BRW_K4 FILL-IN_BELOPP ~
FILL-IN_FRITEXT BRW_K3 BTN_OK BTN_AVS 
&Scoped-Define DISPLAYED-OBJECTS CMB_AONR FILL-IN_MOMSKOD FILL-IN_KUNDKONTO ~
FILL-IN_INTAKTKONTO FILL-IN_MOTPART FILL-IN_BELOPP FILL-IN_MOMSEXTERNT ~
FILL-IN_MOMSBELOPP FILL-IN_FAKTDATUM FILL-IN_FRITEXT FILL-IN_FDATUM 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVS 
     LABEL "Avbryt" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_FVEFA 
     LABEL "-" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_FVEFO 
     LABEL "-" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_NVEFA 
     LABEL "+" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_NVEFO 
     LABEL "+" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_OK AUTO-GO 
     LABEL "Ok" 
     SIZE 14 BY 1
     BGCOLOR 8 .

DEFINE VARIABLE CMB_AONR AS CHARACTER FORMAT "X(256)":U 
     LABEL "Aonr Delnr" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "xxx" 
     DROP-DOWN-LIST
     SIZE 15.5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN_BELOPP AS DECIMAL FORMAT "->>>>>>>>>9.99" INITIAL 0 
     LABEL "Belopp" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1.

DEFINE VARIABLE FILL-IN_FAKTDATUM AS DATE FORMAT "99/99/99" 
     LABEL "Faktura datum" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1.

DEFINE VARIABLE FILL-IN_FDATUM AS DATE FORMAT "99/99/99" 
     LABEL "Förfallodag" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1.

DEFINE VARIABLE FILL-IN_FRITEXT AS CHARACTER FORMAT "X(30)" 
     LABEL "Fritext" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1.

DEFINE VARIABLE FILL-IN_INTAKTKONTO AS CHARACTER FORMAT "X(5)" 
     LABEL "Inäktskonto" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1.

DEFINE VARIABLE FILL-IN_KUNDKONTO AS CHARACTER FORMAT "X(5)" 
     LABEL "Kundfodranskonto" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1.

DEFINE VARIABLE FILL-IN_MOMSBELOPP AS DECIMAL FORMAT "->>>>>>>>>9.99" INITIAL 0 
     LABEL "Moms" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1.

DEFINE VARIABLE FILL-IN_MOMSEXTERNT AS DECIMAL FORMAT ">>9.99" INITIAL 0 
     LABEL "Momsats" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1.

DEFINE VARIABLE FILL-IN_MOMSKOD AS CHARACTER FORMAT "X(5)" 
     LABEL "Momskonto" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1.

DEFINE VARIABLE FILL-IN_MOTPART AS CHARACTER FORMAT "X(5)" 
     LABEL "motparttemp" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
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
DEFINE BROWSE BRW_K1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_K1 Dialog-Frame _STRUCTURED
  QUERY BRW_K1 NO-LOCK DISPLAY
      kundfodrantemp.KUNDKONTO COLUMN-LABEL "Kundfodrans!konto" FORMAT "X(8)":U
      kundfodrantemp.KUNDFODTEXT COLUMN-LABEL "Text" FORMAT "X(34)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SIZE 49.63 BY 8.42.

DEFINE BROWSE BRW_K2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_K2 Dialog-Frame _STRUCTURED
  QUERY BRW_K2 NO-LOCK DISPLAY
      intakttemp.INTAKTKONTO COLUMN-LABEL "Intäkts!konto" FORMAT "X(8)":U
      intakttemp.INTAKTTEXT COLUMN-LABEL "Text" FORMAT "X(34)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SIZE 47.63 BY 9.

DEFINE BROWSE BRW_K3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_K3 Dialog-Frame _STRUCTURED
  QUERY BRW_K3 NO-LOCK DISPLAY
      motparttemp.MOTPART COLUMN-LABEL "Motpart" FORMAT "X(5)":U
      motparttemp.MOTPARTTEXT COLUMN-LABEL "Text" FORMAT "X(25)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SIZE 40 BY 9.

DEFINE BROWSE BRW_K4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_K4 Dialog-Frame _STRUCTURED
  QUERY BRW_K4 NO-LOCK DISPLAY
      momstemp.MOMSKOD COLUMN-LABEL "Moms!konto" FORMAT "X(8)":U
      momstemp.MOMSTEXT COLUMN-LABEL "Text" FORMAT "X(26)":U
      momstemp.MOMSEXTERNT COLUMN-LABEL "Moms!i procent" FORMAT ">>9.99":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SIZE 47.63 BY 9.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     BRW_K1 AT ROW 1.5 COL 42
     BRW_K2 AT ROW 1.58 COL 43.88
     CMB_AONR AT ROW 2.92 COL 17.5 COLON-ALIGNED
     BRW_K4 AT ROW 3 COL 42
     FILL-IN_MOMSKOD AT ROW 4.42 COL 17.5 COLON-ALIGNED
     FILL-IN_KUNDKONTO AT ROW 4.42 COL 17.5 COLON-ALIGNED
     FILL-IN_INTAKTKONTO AT ROW 4.42 COL 17.5 COLON-ALIGNED
     FILL-IN_MOTPART AT ROW 5.92 COL 17.5 COLON-ALIGNED
     FILL-IN_BELOPP AT ROW 7.42 COL 17.5 COLON-ALIGNED
     FILL-IN_MOMSEXTERNT AT ROW 8.92 COL 17.5 COLON-ALIGNED
     FILL-IN_MOMSBELOPP AT ROW 10.42 COL 17.5 COLON-ALIGNED
     BTN_NVEFA AT ROW 11.58 COL 29.75
     FILL-IN_FAKTDATUM AT ROW 11.92 COL 17.5 COLON-ALIGNED
     FILL-IN_FRITEXT AT ROW 11.92 COL 17.5 COLON-ALIGNED
     BRW_K3 AT ROW 12.38 COL 42
     BTN_FVEFA AT ROW 12.58 COL 29.75
     BTN_NVEFO AT ROW 13.58 COL 29.75
     FILL-IN_FDATUM AT ROW 14 COL 17.5 COLON-ALIGNED
     BTN_FVEFO AT ROW 14.58 COL 29.75
     BTN_OK AT ROW 23.42 COL 62.5
     BTN_AVS AT ROW 23.42 COL 77.5
     "Välj konton för fakturering" VIEW-AS TEXT
          SIZE 30.88 BY 1.42 AT ROW 1.33 COL 1.5
          FONT 17
     SPACE(59.61) SKIP(21.95)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Fakturakonton".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Allow: Basic,Browse,DB-Fields,Query
   Temp-Tables and Buffers:
      TABLE: intakttemp T "?" NO-UNDO temp-db intakttemp
      TABLE: kundfodrantemp T "?" NO-UNDO temp-db kundfodrantemp
      TABLE: momstemp T "?" NO-UNDO temp-db momstemp
      TABLE: motparttemp T "?" NO-UNDO temp-db motparttemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
                                                                        */
/* BROWSE-TAB BRW_K1 TEXT-1 Dialog-Frame */
/* BROWSE-TAB BRW_K2 BRW_K1 Dialog-Frame */
/* BROWSE-TAB BRW_K4 CMB_AONR Dialog-Frame */
/* BROWSE-TAB BRW_K3 FILL-IN_FRITEXT Dialog-Frame */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON BTN_FVEFA IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       BTN_FVEFA:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR BUTTON BTN_FVEFO IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       BTN_FVEFO:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR BUTTON BTN_NVEFA IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       BTN_NVEFA:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR BUTTON BTN_NVEFO IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       BTN_NVEFO:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_FAKTDATUM IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       FILL-IN_FAKTDATUM:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_FDATUM IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       FILL-IN_FDATUM:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_INTAKTKONTO IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       FILL-IN_INTAKTKONTO:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_KUNDKONTO IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       FILL-IN_KUNDKONTO:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_MOMSBELOPP IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       FILL-IN_MOMSBELOPP:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_MOMSEXTERNT IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       FILL-IN_MOMSEXTERNT:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_MOMSKOD IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       FILL-IN_MOMSKOD:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_MOTPART IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       FILL-IN_MOTPART:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_K1
/* Query rebuild information for BROWSE BRW_K1
     _TblList          = "temp-db.kundfodrantemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.kundfodrantemp.KUNDKONTO
"kundfodrantemp.KUNDKONTO" "Kundfodrans!konto" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.kundfodrantemp.KUNDFODTEXT
"kundfodrantemp.KUNDFODTEXT" "Text" "X(34)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is OPENED
*/  /* BROWSE BRW_K1 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_K2
/* Query rebuild information for BROWSE BRW_K2
     _TblList          = "temp-db.intakttemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.intakttemp.INTAKTKONTO
"intakttemp.INTAKTKONTO" "Intäkts!konto" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.intakttemp.INTAKTTEXT
"intakttemp.INTAKTTEXT" "Text" "X(34)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is OPENED
*/  /* BROWSE BRW_K2 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_K3
/* Query rebuild information for BROWSE BRW_K3
     _TblList          = "temp-db.motparttemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.motparttemp.MOTPART
"motparttemp.MOTPART" "Motpart" "X(5)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.motparttemp.MOTPARTTEXT
"motparttemp.MOTPARTTEXT" "Text" "X(25)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is OPENED
*/  /* BROWSE BRW_K3 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_K4
/* Query rebuild information for BROWSE BRW_K4
     _TblList          = "temp-db.momstemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.momstemp.MOMSKOD
"momstemp.MOMSKOD" "Moms!konto" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.momstemp.MOMSTEXT
"momstemp.MOMSTEXT" "Text" "X(26)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.momstemp.MOMSEXTERNT
"momstemp.MOMSEXTERNT" "Moms!i procent" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is OPENED
*/  /* BROWSE BRW_K4 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX Dialog-Frame
/* Query rebuild information for DIALOG-BOX Dialog-Frame
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX Dialog-Frame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON END-ERROR OF FRAME Dialog-Frame /* Fakturakonton */
DO:
  {BORTBRWPROC.I}
  musz = TRUE.
  APPLY "GO" TO FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON ENDKEY OF FRAME Dialog-Frame /* Fakturakonton */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Fakturakonton */
DO:
   APPLY "END-ERROR":U TO SELF.
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
   status-ok = BRW_K1:SELECT-FOCUSED-ROW().
   FILL-IN_KUNDKONTO = kundfodrantemp.KUNDKONTO.
   DISPLAY FILL-IN_KUNDKONTO WITH FRAME {&FRAME-NAME}.   
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
   status-ok = BRW_K2:SELECT-FOCUSED-ROW().
   FILL-IN_INTAKTKONTO = intakttemp.INTAKTKONTO.
   DISPLAY FILL-IN_INTAKTKONTO WITH FRAME {&FRAME-NAME}.
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
   status-ok = BRW_K3:SELECT-FOCUSED-ROW().
   FILL-IN_MOTPART = motparttemp.MOTPART.
   DISPLAY FILL-IN_MOTPART WITH FRAME {&FRAME-NAME}. 
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
   status-ok = BRW_K4:SELECT-FOCUSED-ROW().
   ASSIGN    
   FILL-IN_MOMSBELOPP = FILL-IN_BELOPP * momstemp.MOMSEXTERNT / 100
   FILL-IN_MOMSKOD = momstemp.MOMSKOD 
   FILL-IN_MOMSEXTERNT = momstemp.MOMSEXTERNT.
   DISPLAY FILL-IN_MOMSKOD FILL-IN_MOMSBELOPP FILL-IN_MOMSEXTERNT WITH FRAME {&FRAME-NAME}. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVS Dialog-Frame
ON CHOOSE OF BTN_AVS IN FRAME Dialog-Frame /* Avbryt */
DO:
   APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_FVEFA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_FVEFA Dialog-Frame
ON CHOOSE OF BTN_FVEFA IN FRAME Dialog-Frame /* - */
DO: 
   ASSIGN
   FILL-IN_FAKTDATUM = INPUT FILL-IN_FAKTDATUM.   
   FILL-IN_FAKTDATUM = FILL-IN_FAKTDATUM - 1.      
   DISPLAY FILL-IN_FAKTDATUM WITH FRAME {&FRAME-NAME}. 
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_FVEFO
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_FVEFO Dialog-Frame
ON CHOOSE OF BTN_FVEFO IN FRAME Dialog-Frame /* - */
DO: 
   ASSIGN
   FILL-IN_FDATUM = INPUT FILL-IN_FDATUM.   
   FILL-IN_FDATUM = FILL-IN_FDATUM - 1.      
   DISPLAY FILL-IN_FDATUM WITH FRAME {&FRAME-NAME}. 
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NVEFA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NVEFA Dialog-Frame
ON CHOOSE OF BTN_NVEFA IN FRAME Dialog-Frame /* + */
DO:   
   ASSIGN
   FILL-IN_FAKTDATUM = INPUT FILL-IN_FAKTDATUM.   
   FILL-IN_FAKTDATUM = FILL-IN_FAKTDATUM + 1.        
   DISPLAY FILL-IN_FAKTDATUM WITH FRAME {&FRAME-NAME}.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NVEFO
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NVEFO Dialog-Frame
ON CHOOSE OF BTN_NVEFO IN FRAME Dialog-Frame /* + */
DO:   
   ASSIGN
   FILL-IN_FDATUM = INPUT FILL-IN_FDATUM.   
   FILL-IN_FDATUM = FILL-IN_FDATUM + 1.        
   DISPLAY FILL-IN_FDATUM WITH FRAME {&FRAME-NAME}.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK Dialog-Frame
ON CHOOSE OF BTN_OK IN FRAME Dialog-Frame /* Ok */
DO:
   ASSIGN
   FILL-IN_FRITEXT = INPUT FILL-IN_FRITEXT
   FILL-IN_INTAKTKONTO = INPUT FILL-IN_INTAKTKONTO
   FILL-IN_KUNDKONTO = INPUT FILL-IN_KUNDKONTO 
   FILL-IN_MOMSKOD = INPUT FILL-IN_MOMSKOD
   FILL-IN_MOMSEXTERNT = INPUT FILL-IN_MOMSEXTERNT
   FILL-IN_MOTPART = INPUT FILL-IN_MOTPART
   FILL-IN_BELOPP = INPUT FILL-IN_BELOPP
   FILL-IN_FAKTDATUM = INPUT FILL-IN_FAKTDATUM
   FILL-IN_FDATUM = INPUT FILL-IN_FDATUM
   FILL-IN_MOMSBELOPP = INPUT FILL-IN_MOMSBELOPP
   CMB_AONR = INPUT CMB_AONR.
   IF kontyp = 1 THEN DO:
      ASSIGN
      efaktkundtemp.AONR = SUBSTRING(CMB_AONR,1,(INDEX(CMB_AONR," ") - 1))
      efaktkundtemp.DELNR = INTEGER(SUBSTRING(CMB_AONR,(INDEX(CMB_AONR," ") + 1)))  
      efaktkundtemp.BELOPP = FILL-IN_BELOPP 
      efaktkundtemp.FAKTDATUM = FILL-IN_FAKTDATUM   
      efaktkundtemp.FDATUM = FILL-IN_FDATUM 
      efaktkundtemp.KUNDKONTOID = kundfodrantemp.KUNDKONTOID 
      efaktkundtemp.MOTPARTID = motparttemp.MOTPARTID.
   END. 
   ELSE IF kontyp = 2 THEN DO:
      ASSIGN
      efaktinkontotemp.AONR = SUBSTRING(CMB_AONR,1,(INDEX(CMB_AONR," ") - 1))
      efaktinkontotemp.DELNR = INTEGER(SUBSTRING(CMB_AONR,(INDEX(CMB_AONR," ") + 1)))
      efaktinkontotemp.INTAKTID = intakttemp.INTAKTID 
      efaktinkontotemp.BELOPP = FILL-IN_BELOPP       
      efaktinkontotemp.MOTPARTID = motparttemp.MOTPARTID.          
   END.
   ELSE IF kontyp = 4 THEN DO:
      IF FILL-IN_MOMSBELOPP = 0 AND FILL-IN_MOMSEXTERNT > 0 THEN DO:
         MESSAGE "Du kan inte ha moms som är 0" VIEW-AS ALERT-BOX.
         RETURN NO-APPLY.
      END.
      ASSIGN
      efaktmomstemp.AONR = SUBSTRING(CMB_AONR,1,(INDEX(CMB_AONR," ") - 1))
      efaktmomstemp.DELNR = INTEGER(SUBSTRING(CMB_AONR,(INDEX(CMB_AONR," ") + 1)))
      efaktmomstemp.MOMSBELOPP = FILL-IN_MOMSBELOPP 
      efaktmomstemp.MOMSEXTERNT = FILL-IN_MOMSEXTERNT 
      efaktmomstemp.MOMSID = momstemp.MOMSID 
      efaktmomstemp.BELOPP = FILL-IN_BELOPP
      efaktmomstemp.FRITEXT = FILL-IN_FRITEXT.                     
   END.  
   {BORTBRWPROC.I}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_BELOPP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_BELOPP Dialog-Frame
ON LEAVE OF FILL-IN_BELOPP IN FRAME Dialog-Frame /* Belopp */
DO:
   FILL-IN_BELOPP = INPUT FILL-IN_BELOPP.
   IF kontyp = 4 THEN DO:
      FILL-IN_MOMSBELOPP = FILL-IN_BELOPP * FILL-IN_MOMSEXTERNT / 100.
      DISPLAY FILL-IN_MOMSKOD FILL-IN_MOMSBELOPP FILL-IN_MOMSEXTERNT WITH FRAME {&FRAME-NAME}.
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_FAKTDATUM
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_FAKTDATUM Dialog-Frame
ON LEAVE OF FILL-IN_FAKTDATUM IN FRAME Dialog-Frame /* Faktura datum */
DO:
   ASSIGN
   FILL-IN_FAKTDATUM = INPUT FILL-IN_FAKTDATUM.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_FAKTDATUM Dialog-Frame
ON MOUSE-MENU-CLICK OF FILL-IN_FAKTDATUM IN FRAME Dialog-Frame /* Faktura datum */
DO:
   ASSIGN
   FILL-IN_FAKTDATUM = INPUT FILL-IN_FAKTDATUM
   Guru.GlobalaVariabler:regdatum = INPUT FILL-IN_FAKTDATUM.
   RUN AlmanBtn.w.
   FILL-IN_FAKTDATUM = Guru.GlobalaVariabler:regdatum.
   DISPLAY FILL-IN_FAKTDATUM WITH FRAME {&FRAME-NAME}.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_FDATUM
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_FDATUM Dialog-Frame
ON LEAVE OF FILL-IN_FDATUM IN FRAME Dialog-Frame /* Förfallodag */
DO:
   ASSIGN
   FILL-IN_FDATUM = INPUT FILL-IN_FDATUM.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_FDATUM Dialog-Frame
ON MOUSE-MENU-CLICK OF FILL-IN_FDATUM IN FRAME Dialog-Frame /* Förfallodag */
DO:
   ASSIGN
   FILL-IN_FDATUM = INPUT FILL-IN_FDATUM
   Guru.GlobalaVariabler:regdatum = INPUT FILL-IN_FDATUM.
   RUN AlmanBtn.w.
   FILL-IN_FDATUM = Guru.GlobalaVariabler:regdatum.
   DISPLAY FILL-IN_FDATUM WITH FRAME {&FRAME-NAME}.

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
   IF kontyp = 1 THEN DO:
      FIND FIRST efaktkundtemp NO-ERROR.      
   END. 
   ELSE IF kontyp = 2 THEN DO:
      FIND FIRST efaktinkontotemp NO-ERROR.      
   END.
   ELSE IF kontyp = 4 THEN DO:
      FIND FIRST efaktmomstemp NO-ERROR.
   END.
   CMB_AONR:DELETE("XXX").
   RUN enable_UI.       
   {FRMSIZED.I}    
   OPEN QUERY faktaonrq FOR EACH faktaonrtemp NO-LOCK.   
   GET FIRST faktaonrq NO-LOCK.
   DO WHILE AVAILABLE(faktaonrtemp):
      status-ok = CMB_AONR:ADD-LAST(faktaonrtemp.AONR + " " + STRING(faktaonrtemp.DELNR,Guru.Konstanter:varforetypchar[1])).
      GET NEXT faktaonrq NO-LOCK.
   END.
   IF kontyp = 1 THEN DO:
      ASSIGN CMB_AONR:SCREEN-VALUE = efaktkundtemp.AONR + " " + STRING(efaktkundtemp.DELNR,Guru.Konstanter:varforetypchar[1]).
      ENABLE BTN_FVEFA BTN_FVEFO BTN_NVEFA BTN_NVEFO 
      FILL-IN_BELOPP FILL-IN_FAKTDATUM FILL-IN_FDATUM WITH FRAME {&FRAME-NAME}.
      FIND FIRST motparttemp WHERE motparttemp.MOTPARTID = efaktkundtemp.MOTPARTID NO-LOCK NO-ERROR.
      FIND FIRST kundfodrantemp WHERE kundfodrantemp.KUNDKONTOID = efaktkundtemp.KUNDKONTOID NO-LOCK NO-ERROR.
      ASSIGN      
      FILL-IN_BELOPP = efaktkundtemp.BELOPP
      FILL-IN_FAKTDATUM = efaktkundtemp.FAKTDATUM 
      FILL-IN_FDATUM = efaktkundtemp.FDATUM
      FILL-IN_KUNDKONTO = kundfodrantemp.KUNDKONTO
      FILL-IN_MOTPART = motparttemp.MOTPART      
      FILL-IN_INTAKTKONTO:HIDDEN = TRUE 
      FILL-IN_MOMSBELOPP:HIDDEN = TRUE 
      FILL-IN_MOMSEXTERNT:HIDDEN = TRUE 
      FILL-IN_MOMSKOD:HIDDEN = TRUE
      FILL-IN_FRITEXT:HIDDEN = TRUE
      BRW_K1:HIDDEN = FALSE
      BRW_K2:HIDDEN = TRUE
      BRW_K3:HIDDEN = FALSE
      BRW_K4:HIDDEN = TRUE.
      DISPLAY FILL-IN_KUNDKONTO FILL-IN_MOTPART FILL-IN_BELOPP FILL-IN_FAKTDATUM FILL-IN_FDATUM WITH FRAME {&FRAME-NAME}.
   END.
   ELSE IF kontyp = 2 THEN DO:
      ASSIGN CMB_AONR:SCREEN-VALUE = efaktinkontotemp.AONR + " " + STRING(efaktinkontotemp.DELNR,Guru.Konstanter:varforetypchar[1]).
      ENABLE 
      FILL-IN_BELOPP WITH FRAME {&FRAME-NAME}.
      FIND FIRST motparttemp WHERE motparttemp.MOTPARTID = efaktinkontotemp.MOTPARTID NO-LOCK NO-ERROR.
      FIND FIRST intakttemp WHERE intakttemp.INTAKTID = efaktinkontotemp.INTAKTID NO-LOCK NO-ERROR.
      ASSIGN      
      FILL-IN_INTAKTKONTO = intakttemp.INTAKTKONTO 
      FILL-IN_BELOPP = efaktinkontotemp.BELOPP      
      FILL-IN_MOTPART = motparttemp.MOTPART
      FILL-IN_FAKTDATUM:HIDDEN = TRUE 
      FILL-IN_FDATUM:HIDDEN = TRUE 
      FILL-IN_KUNDKONTO:HIDDEN = TRUE
      FILL-IN_MOMSBELOPP:HIDDEN = TRUE 
      FILL-IN_MOMSEXTERNT:HIDDEN = TRUE 
      FILL-IN_MOMSKOD:HIDDEN = TRUE
      FILL-IN_FRITEXT:HIDDEN = TRUE
      BRW_K1:HIDDEN = TRUE
      BRW_K2:HIDDEN = FALSE
      BRW_K3:HIDDEN = FALSE
      BRW_K4:HIDDEN = TRUE. 
      DISPLAY FILL-IN_INTAKTKONTO FILL-IN_MOTPART FILL-IN_BELOPP WITH FRAME {&FRAME-NAME}.
   END.
   ELSE IF kontyp = 4 THEN DO:
      ASSIGN CMB_AONR:SCREEN-VALUE = efaktmomstemp.AONR + " " + STRING(efaktmomstemp.DELNR,Guru.Konstanter:varforetypchar[1]).
      ENABLE FILL-IN_BELOPP WITH FRAME {&FRAME-NAME}.
      FIND FIRST momstemp WHERE momstemp.MOMSID = efaktmomstemp.MOMSID NO-LOCK NO-ERROR.
      ASSIGN     
      FILL-IN_FRITEXT = efaktmomstemp.FRITEXT
      FILL-IN_MOMSBELOPP = efaktmomstemp.MOMSBELOPP 
      FILL-IN_MOMSEXTERNT = efaktmomstemp.MOMSEXTERNT
      FILL-IN_MOMSKOD = momstemp.MOMSKOD      
      FILL-IN_BELOPP = efaktmomstemp.BELOPP
      FILL-IN_FAKTDATUM:HIDDEN = TRUE 
      FILL-IN_FDATUM:HIDDEN = TRUE 
      FILL-IN_KUNDKONTO:HIDDEN = TRUE
      FILL-IN_INTAKTKONTO:HIDDEN = TRUE
      FILL-IN_MOTPART:HIDDEN = TRUE
      BRW_K1:HIDDEN = TRUE
      BRW_K2:HIDDEN = TRUE
      BRW_K3:HIDDEN = TRUE
      BRW_K4:HIDDEN = FALSE.            
      DISPLAY FILL-IN_FRITEXT FILL-IN_BELOPP FILL-IN_MOMSBELOPP FILL-IN_MOMSEXTERNT FILL-IN_MOMSKOD WITH FRAME {&FRAME-NAME}.
   END.      
   IF kontyp = 1 THEN DO:
      RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(kundfodrantemp)).                  
      RUN lastselectdyn_UI IN brwproc[1].                     
   END.
   IF kontyp = 2 THEN DO:
      RUN setlastrowid_UI IN brwproc[2] (INPUT ROWID(intakttemp)).                  
      RUN lastselectdyn_UI IN brwproc[2].                           
   END.
   IF kontyp = 1 OR kontyp = 2 THEN DO:
      RUN setlastrowid_UI IN brwproc[3] (INPUT ROWID(motparttemp)).                  
      RUN lastselectdyn_UI IN brwproc[3].                     
   END.
   IF kontyp = 4 THEN DO:
      RUN setlastrowid_UI IN brwproc[4] (INPUT ROWID(momstemp)).                  
      RUN lastselectdyn_UI IN brwproc[4].                     
   END.
   CMB_AONR:LABEL = Guru.Konstanter:gaok + " delnr".
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
  DISPLAY CMB_AONR FILL-IN_MOMSKOD FILL-IN_KUNDKONTO FILL-IN_INTAKTKONTO 
          FILL-IN_MOTPART FILL-IN_BELOPP FILL-IN_MOMSEXTERNT FILL-IN_MOMSBELOPP 
          FILL-IN_FAKTDATUM FILL-IN_FRITEXT FILL-IN_FDATUM 
      WITH FRAME Dialog-Frame.
  ENABLE BRW_K1 BRW_K2 CMB_AONR BRW_K4 FILL-IN_BELOPP FILL-IN_FRITEXT BRW_K3 
         BTN_OK BTN_AVS 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

