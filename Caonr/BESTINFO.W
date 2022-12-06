&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
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
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEFINE INPUT PARAMETER vem AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER kontaktvar AS CHARACTER NO-UNDO.
kontaktvar = "INGEN".
/* Local Variable Definitions ---                                       */
{ALLDEF.I}
{EXTRADATA.I}
&Scoped-define NEW 
{GLOBVAR2DEL1.I}
&Scoped-define NEW 
&Scoped-define SHARED SHARED
{BESTKUNDALLT.I}

DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE VARIABLE bestapph AS HANDLE NO-UNDO.
DEFINE VARIABLE edataapph AS HANDLE NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE entryepost AS LOGICAL NO-UNDO.
{KONTAKTPERSTEMP.I}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame
&Scoped-define BROWSE-NAME BRW_KONTAKT

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES kontaktperstemp bestkundallt

/* Definitions for BROWSE BRW_KONTAKT                                   */
&Scoped-define FIELDS-IN-QUERY-BRW_KONTAKT kontaktperstemp.NAMN ~
kontaktperstemp.TELEFON kontaktperstemp.MOBIL kontaktperstemp.EPOST ~
kontaktperstemp.FAX 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_KONTAKT kontaktperstemp.NAMN ~
kontaktperstemp.TELEFON kontaktperstemp.MOBIL kontaktperstemp.EPOST ~
kontaktperstemp.FAX 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_KONTAKT kontaktperstemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_KONTAKT kontaktperstemp
&Scoped-define QUERY-STRING-BRW_KONTAKT FOR EACH kontaktperstemp WHERE TRUE /* Join to bestkundallt incomplete */ NO-LOCK
&Scoped-define OPEN-QUERY-BRW_KONTAKT OPEN QUERY BRW_KONTAKT FOR EACH kontaktperstemp WHERE TRUE /* Join to bestkundallt incomplete */ NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_KONTAKT kontaktperstemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_KONTAKT kontaktperstemp


/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define QUERY-STRING-Dialog-Frame FOR EACH bestkundallt NO-LOCK
&Scoped-define OPEN-QUERY-Dialog-Frame OPEN QUERY Dialog-Frame FOR EACH bestkundallt NO-LOCK.
&Scoped-define TABLES-IN-QUERY-Dialog-Frame bestkundallt
&Scoped-define FIRST-TABLE-IN-QUERY-Dialog-Frame bestkundallt


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS bestkundallt.BESTNAMN bestkundallt.KONTAKT ~
bestkundallt.TEL bestkundallt.FAXNR bestkundallt.ADRESS bestkundallt.PNR ~
bestkundallt.ORT bestkundallt.LEVADRESS bestkundallt.LEVPNR ~
bestkundallt.LEVORT bestkundallt.FAKADRESS bestkundallt.FAKPNR ~
bestkundallt.FAKORT bestkundallt.AOKUND 
&Scoped-define ENABLED-TABLES bestkundallt
&Scoped-define FIRST-ENABLED-TABLE bestkundallt
&Scoped-Define ENABLED-OBJECTS RECT-8 RECT-53 RECT-54 RECT-55 BRW_KONTAKT ~
FILL-IN-EPOST BTN_NY BTN_ANDRA BTN_BORT Btn_OK 
&Scoped-Define DISPLAYED-FIELDS bestkundallt.VIBESTID bestkundallt.BESTNAMN ~
bestkundallt.KONTAKT bestkundallt.TEL bestkundallt.FAXNR ~
bestkundallt.ADRESS bestkundallt.PNR bestkundallt.ORT ~
bestkundallt.LEVADRESS bestkundallt.LEVPNR bestkundallt.LEVORT ~
bestkundallt.FAKADRESS bestkundallt.FAKPNR bestkundallt.FAKORT ~
bestkundallt.AOKUND 
&Scoped-define DISPLAYED-TABLES bestkundallt
&Scoped-define FIRST-DISPLAYED-TABLE bestkundallt
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-EPOST 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD tabortblanka Dialog-Frame 
FUNCTION tabortblanka RETURNS CHARACTER
 ( INPUT namnblanka AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_ANDRA 
     LABEL "Ändra" 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_BORT 
     LABEL "Ta bort" 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_NY 
     LABEL "Ny" 
     SIZE 12 BY 1.

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 14 BY 1
     BGCOLOR 8 .

DEFINE VARIABLE FILL-IN-EPOST AS CHARACTER FORMAT "X(256)":U 
     LABEL "E-post" 
     VIEW-AS FILL-IN 
     SIZE 49.63 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-53
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 62 BY 3.25.

DEFINE RECTANGLE RECT-54
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 62 BY 3.

DEFINE RECTANGLE RECT-55
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 62 BY 2.25.

DEFINE RECTANGLE RECT-8
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 62 BY 7
     BGCOLOR 8 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_KONTAKT FOR 
      kontaktperstemp SCROLLING.

DEFINE QUERY Dialog-Frame FOR 
      bestkundallt SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_KONTAKT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_KONTAKT Dialog-Frame _STRUCTURED
  QUERY BRW_KONTAKT NO-LOCK DISPLAY
      kontaktperstemp.NAMN COLUMN-LABEL "Namn" FORMAT "X(256)":U
            WIDTH 17
      kontaktperstemp.TELEFON COLUMN-LABEL "Telefon" FORMAT "X(256)":U
            WIDTH 7.5
      kontaktperstemp.MOBIL COLUMN-LABEL "Mobil" FORMAT "X(256)":U
            WIDTH 7.5
      kontaktperstemp.EPOST COLUMN-LABEL "E-post" FORMAT "X(256)":U
            WIDTH 20.25
      kontaktperstemp.FAX COLUMN-LABEL "Fax" FORMAT "X(256)":U
            WIDTH 7.5
  ENABLE
      kontaktperstemp.NAMN
      kontaktperstemp.TELEFON
      kontaktperstemp.MOBIL
      kontaktperstemp.EPOST
      kontaktperstemp.FAX
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SIZE 61 BY 13.75
         FONT 4
         TITLE "Alla kontaktpersoner" FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     bestkundallt.VIBESTID AT ROW 2 COL 8.38 COLON-ALIGNED
          LABEL "Best-id" FORMAT "X(10)"
          VIEW-AS FILL-IN 
          SIZE 11 BY 1
     bestkundallt.BESTNAMN AT ROW 2 COL 37.63 COLON-ALIGNED
          LABEL "Namn" FORMAT "x(25)"
          VIEW-AS FILL-IN 
          SIZE 26 BY 1
     BRW_KONTAKT AT ROW 3.38 COL 64
     bestkundallt.KONTAKT AT ROW 3.71 COL 10.5 COLON-ALIGNED
          LABEL "Kontaktperson" FORMAT "x(256)"
          VIEW-AS FILL-IN 
          SIZE 49.63 BY 1
     bestkundallt.TEL AT ROW 5 COL 10.5 COLON-ALIGNED
          LABEL "Tel" FORMAT "X(256)"
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     bestkundallt.FAXNR AT ROW 5 COL 40.25 COLON-ALIGNED
          LABEL "Mobilnr" FORMAT "X(256)"
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     FILL-IN-EPOST AT ROW 6.29 COL 10.63 COLON-ALIGNED
     bestkundallt.ADRESS AT ROW 7.63 COL 10.63 COLON-ALIGNED
          LABEL "Adress" FORMAT "x(256)"
          VIEW-AS FILL-IN 
          SIZE 49.63 BY 1
     bestkundallt.PNR AT ROW 8.92 COL 10.63 COLON-ALIGNED
          LABEL "Postnr"
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
     bestkundallt.ORT AT ROW 8.92 COL 33.25 COLON-ALIGNED
          LABEL "Ort" FORMAT "x(256)"
          VIEW-AS FILL-IN 
          SIZE 27 BY 1
     bestkundallt.LEVADRESS AT ROW 10.79 COL 10.63 COLON-ALIGNED
          LABEL "Leveransadress" FORMAT "x(256)"
          VIEW-AS FILL-IN 
          SIZE 49.63 BY 1
     bestkundallt.LEVPNR AT ROW 12.08 COL 10.63 COLON-ALIGNED
          LABEL "Postnr"
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
     bestkundallt.LEVORT AT ROW 12.08 COL 33.25 COLON-ALIGNED
          LABEL "Ort" FORMAT "x(256)"
          VIEW-AS FILL-IN 
          SIZE 27 BY 1
     bestkundallt.FAKADRESS AT ROW 13.92 COL 10.63 COLON-ALIGNED
          LABEL "Fakturaadress" FORMAT "x(256)"
          VIEW-AS FILL-IN 
          SIZE 49.63 BY 1
     bestkundallt.FAKPNR AT ROW 15.21 COL 10.63 COLON-ALIGNED
          LABEL "Postnr"
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
     bestkundallt.FAKORT AT ROW 15.21 COL 33.25 COLON-ALIGNED
          LABEL "Ort" FORMAT "x(256)"
          VIEW-AS FILL-IN 
          SIZE 27 BY 1
     bestkundallt.AOKUND AT ROW 16.92 COL 10.5 COLON-ALIGNED
          LABEL "Koppling till aonr"
          VIEW-AS FILL-IN 
          SIZE 5 BY 1
     BTN_NY AT ROW 17.38 COL 75.13
     BTN_ANDRA AT ROW 17.38 COL 88.25
     BTN_BORT AT ROW 17.38 COL 101.38
     Btn_OK AT ROW 19.25 COL 59
     "Dubbel klicka för att välja kontakt person" VIEW-AS TEXT
          SIZE 46.5 BY 1.21 AT ROW 1.5 COL 73
          FONT 17
     RECT-8 AT ROW 3.25 COL 1.5
     RECT-53 AT ROW 10.25 COL 1.5
     RECT-54 AT ROW 13.5 COL 1.5
     RECT-55 AT ROW 16.5 COL 1.5
     SPACE(62.49) SKIP(1.70)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 4
         TITLE "Information"
         DEFAULT-BUTTON Btn_OK.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Dialog-Box
   Allow: Basic,Browse,DB-Fields,Query
   Temp-Tables and Buffers:
      TABLE: bestkundallt T "?" NO-UNDO temp-db bestkundallt
      TABLE: kontaktperstemp T "?" NO-UNDO temp-db kontaktperstemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   NOT-VISIBLE FRAME-NAME                                               */
/* BROWSE-TAB BRW_KONTAKT BESTNAMN Dialog-Frame */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN bestkundallt.ADRESS IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
ASSIGN 
       bestkundallt.ADRESS:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* SETTINGS FOR FILL-IN bestkundallt.AOKUND IN FRAME Dialog-Frame
   EXP-LABEL                                                            */
ASSIGN 
       bestkundallt.AOKUND:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* SETTINGS FOR FILL-IN bestkundallt.BESTNAMN IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
ASSIGN 
       bestkundallt.BESTNAMN:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* SETTINGS FOR FILL-IN bestkundallt.FAKADRESS IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
ASSIGN 
       bestkundallt.FAKADRESS:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* SETTINGS FOR FILL-IN bestkundallt.FAKORT IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
ASSIGN 
       bestkundallt.FAKORT:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* SETTINGS FOR FILL-IN bestkundallt.FAKPNR IN FRAME Dialog-Frame
   EXP-LABEL                                                            */
ASSIGN 
       bestkundallt.FAKPNR:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* SETTINGS FOR FILL-IN bestkundallt.FAXNR IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
ASSIGN 
       bestkundallt.FAXNR:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

ASSIGN 
       FILL-IN-EPOST:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* SETTINGS FOR FILL-IN bestkundallt.KONTAKT IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
ASSIGN 
       bestkundallt.KONTAKT:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* SETTINGS FOR FILL-IN bestkundallt.LEVADRESS IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
ASSIGN 
       bestkundallt.LEVADRESS:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* SETTINGS FOR FILL-IN bestkundallt.LEVORT IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
ASSIGN 
       bestkundallt.LEVORT:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* SETTINGS FOR FILL-IN bestkundallt.LEVPNR IN FRAME Dialog-Frame
   EXP-LABEL                                                            */
ASSIGN 
       bestkundallt.LEVPNR:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* SETTINGS FOR FILL-IN bestkundallt.ORT IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
ASSIGN 
       bestkundallt.ORT:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* SETTINGS FOR FILL-IN bestkundallt.PNR IN FRAME Dialog-Frame
   EXP-LABEL                                                            */
ASSIGN 
       bestkundallt.PNR:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* SETTINGS FOR FILL-IN bestkundallt.TEL IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
ASSIGN 
       bestkundallt.TEL:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* SETTINGS FOR FILL-IN bestkundallt.VIBESTID IN FRAME Dialog-Frame
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_KONTAKT
/* Query rebuild information for BROWSE BRW_KONTAKT
     _TblList          = "Temp-Tables.kontaktperstemp WHERE Temp-Tables.bestkundallt <external> ..."
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.kontaktperstemp.NAMN
"kontaktperstemp.NAMN" "Namn" "X(256)" "character" ? ? ? ? ? ? yes ? no no "17" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.kontaktperstemp.TELEFON
"kontaktperstemp.TELEFON" "Telefon" "X(256)" "character" ? ? ? ? ? ? yes ? no no "7.5" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.kontaktperstemp.MOBIL
"kontaktperstemp.MOBIL" "Mobil" "X(256)" "character" ? ? ? ? ? ? yes ? no no "7.5" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.kontaktperstemp.EPOST
"kontaktperstemp.EPOST" "E-post" "X(256)" "character" ? ? ? ? ? ? yes ? no no "20.25" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.kontaktperstemp.FAX
"kontaktperstemp.FAX" "Fax" "X(256)" "character" ? ? ? ? ? ? yes ? no no "7.5" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_KONTAKT */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX Dialog-Frame
/* Query rebuild information for DIALOG-BOX Dialog-Frame
     _TblList          = "Temp-Tables.bestkundallt"
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX Dialog-Frame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON END-ERROR OF FRAME Dialog-Frame /* Information */
DO:
   APPLY "CHOOSE" TO BTN_OK.
   {BORTBRWPROC.I}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON ENDKEY OF FRAME Dialog-Frame /* Information */
DO:
   APPLY "CHOOSE" TO BTN_OK.
   {BORTBRWPROC.I}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Information */
DO:
   APPLY "CHOOSE" TO BTN_OK.
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bestkundallt.AOKUND
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bestkundallt.AOKUND Dialog-Frame
ON MOUSE-SELECT-CLICK OF bestkundallt.AOKUND IN FRAME Dialog-Frame /* Koppling till aonr */
DO:                 
   IF INPUT bestkundallt.AOKUND = TRUE THEN bestkundallt.AOKUND = FALSE. 
   IF INPUT bestkundallt.AOKUND = FALSE THEN bestkundallt.AOKUND = TRUE. 
   DISPLAY bestkundallt.AOKUND WITH FRAME {&FRAME-NAME}.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_KONTAKT
&Scoped-define SELF-NAME BRW_KONTAKT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_KONTAKT Dialog-Frame
ON MOUSE-SELECT-DBLCLICK OF BRW_KONTAKT IN FRAME Dialog-Frame /* Alla kontaktpersoner */
DO:
   IF AVAILABLE kontaktperstemp THEN DO:
      kontaktvar = kontaktperstemp.NAMN + " " + kontaktperstemp.EPOST + " " + kontaktperstemp.TELEFON + " " + kontaktperstemp.MOBIL + " " + kontaktperstemp.FAX.       
      APPLY "CHOOSE" TO BTN_OK.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_KONTAKT Dialog-Frame
ON ROW-LEAVE OF BRW_KONTAKT IN FRAME Dialog-Frame /* Alla kontaktpersoner */
DO:
   status-ok = BRW_KONTAKT:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_KONTAKT Dialog-Frame
ON VALUE-CHANGED OF BRW_KONTAKT IN FRAME Dialog-Frame /* Alla kontaktpersoner */
DO:
  status-ok = BRW_KONTAKT:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME kontaktperstemp.NAMN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL kontaktperstemp.NAMN BRW_KONTAKT _BROWSE-COLUMN Dialog-Frame
ON ENTRY OF kontaktperstemp.NAMN IN BROWSE BRW_KONTAKT /* Namn */
DO:
   entryepost = TRUE.
   IF AVAILABLE kontaktperstemp THEN DISPLAY kontaktperstemp.NAMN kontaktperstemp.EPOST kontaktperstemp.TELEFON
   kontaktperstemp.MOBIL kontaktperstemp.FAX WITH BROWSE BRW_KONTAKT.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL kontaktperstemp.NAMN BRW_KONTAKT _BROWSE-COLUMN Dialog-Frame
ON LEAVE OF kontaktperstemp.NAMN IN BROWSE BRW_KONTAKT /* Namn */
DO:
   IF AVAILABLE kontaktperstemp THEN DO:
      IF kontaktperstemp.NAMN NE INPUT BROWSE BRW_KONTAKT kontaktperstemp.NAMN THEN DO:
         kontaktperstemp.NAMN = INPUT BROWSE BRW_KONTAKT kontaktperstemp.NAMN.   
         DISPLAY  kontaktperstemp.NAMN WITH BROWSE BRW_KONTAKT.         
      END.
   END.   
   entryepost = FALSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME kontaktperstemp.TELEFON
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL kontaktperstemp.TELEFON BRW_KONTAKT _BROWSE-COLUMN Dialog-Frame
ON ENTRY OF kontaktperstemp.TELEFON IN BROWSE BRW_KONTAKT /* Telefon */
DO:
   entryepost = TRUE.
   IF AVAILABLE kontaktperstemp THEN DISPLAY kontaktperstemp.NAMN kontaktperstemp.EPOST kontaktperstemp.TELEFON
   kontaktperstemp.MOBIL kontaktperstemp.FAX WITH BROWSE BRW_KONTAKT.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL kontaktperstemp.TELEFON BRW_KONTAKT _BROWSE-COLUMN Dialog-Frame
ON LEAVE OF kontaktperstemp.TELEFON IN BROWSE BRW_KONTAKT /* Telefon */
DO:
   IF AVAILABLE kontaktperstemp THEN DO:
      IF kontaktperstemp.TELEFON NE INPUT BROWSE BRW_KONTAKT kontaktperstemp.TELEFON THEN DO:
         kontaktperstemp.TELEFON = INPUT BROWSE BRW_KONTAKT kontaktperstemp.TELEFON.
         kontaktperstemp.TELEFON = tabortblanka(kontaktperstemp.TELEFON ).   
         DISPLAY  kontaktperstemp.TELEFON WITH BROWSE BRW_KONTAKT.         
      END.      
   END.   
   entryepost = FALSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME kontaktperstemp.MOBIL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL kontaktperstemp.MOBIL BRW_KONTAKT _BROWSE-COLUMN Dialog-Frame
ON ENTRY OF kontaktperstemp.MOBIL IN BROWSE BRW_KONTAKT /* Mobil */
DO:
   entryepost = TRUE.
   IF AVAILABLE kontaktperstemp THEN DISPLAY kontaktperstemp.NAMN kontaktperstemp.EPOST kontaktperstemp.TELEFON
   kontaktperstemp.MOBIL kontaktperstemp.FAX WITH BROWSE BRW_KONTAKT.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL kontaktperstemp.MOBIL BRW_KONTAKT _BROWSE-COLUMN Dialog-Frame
ON LEAVE OF kontaktperstemp.MOBIL IN BROWSE BRW_KONTAKT /* Mobil */
DO:
   IF AVAILABLE kontaktperstemp THEN DO:
      IF kontaktperstemp.MOBIL NE INPUT BROWSE BRW_KONTAKT kontaktperstemp.MOBIL THEN DO:
         kontaktperstemp.MOBIL = INPUT BROWSE BRW_KONTAKT kontaktperstemp.MOBIL.  
         kontaktperstemp.MOBIL = tabortblanka(kontaktperstemp.MOBIL ). 
         DISPLAY  kontaktperstemp.MOBIL WITH BROWSE BRW_KONTAKT.         
      END.      
   END.   
   entryepost = FALSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME kontaktperstemp.EPOST
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL kontaktperstemp.EPOST BRW_KONTAKT _BROWSE-COLUMN Dialog-Frame
ON ENTRY OF kontaktperstemp.EPOST IN BROWSE BRW_KONTAKT /* E-post */
DO:
   entryepost = TRUE.
   IF AVAILABLE kontaktperstemp THEN DISPLAY kontaktperstemp.NAMN kontaktperstemp.EPOST kontaktperstemp.TELEFON
   kontaktperstemp.MOBIL kontaktperstemp.FAX WITH BROWSE BRW_KONTAKT.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL kontaktperstemp.EPOST BRW_KONTAKT _BROWSE-COLUMN Dialog-Frame
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
   END.   
   entryepost = FALSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME kontaktperstemp.FAX
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL kontaktperstemp.FAX BRW_KONTAKT _BROWSE-COLUMN Dialog-Frame
ON ENTRY OF kontaktperstemp.FAX IN BROWSE BRW_KONTAKT /* Fax */
DO:
   entryepost = TRUE.
   IF AVAILABLE kontaktperstemp THEN DISPLAY kontaktperstemp.NAMN kontaktperstemp.EPOST kontaktperstemp.TELEFON
   kontaktperstemp.MOBIL kontaktperstemp.FAX WITH BROWSE BRW_KONTAKT.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL kontaktperstemp.FAX BRW_KONTAKT _BROWSE-COLUMN Dialog-Frame
ON LEAVE OF kontaktperstemp.FAX IN BROWSE BRW_KONTAKT /* Fax */
DO:
   IF AVAILABLE kontaktperstemp THEN DO:
      IF kontaktperstemp.FAX NE INPUT BROWSE BRW_KONTAKT kontaktperstemp.FAX THEN DO:
         kontaktperstemp.FAX = INPUT BROWSE BRW_KONTAKT kontaktperstemp.FAX.  
         kontaktperstemp.FAX = tabortblanka(kontaktperstemp.FAX ). 
         DISPLAY  kontaktperstemp.FAX WITH BROWSE BRW_KONTAKT.         
      END.      
   END.   
   entryepost = FALSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_ANDRA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_ANDRA Dialog-Frame
ON CHOOSE OF BTN_ANDRA IN FRAME Dialog-Frame /* Ändra */
DO:
   IF AVAILABLE kontaktperstemp THEN DO:
      ASSIGN
      kontaktperstemp.FAX:READ-ONLY IN BROWSE BRW_KONTAKT = FALSE
      kontaktperstemp.NAMN:READ-ONLY IN BROWSE BRW_KONTAKT = FALSE
      kontaktperstemp.EPOST:READ-ONLY IN BROWSE BRW_KONTAKT = FALSE
      kontaktperstemp.TELEFON:READ-ONLY IN BROWSE BRW_KONTAKT = FALSE
      kontaktperstemp.MOBIL:READ-ONLY IN BROWSE BRW_KONTAKT = FALSE.
      APPLY "ENTRY" TO kontaktperstemp.NAMN IN BROWSE BRW_KONTAKT. 
      DISPLAY BRW_KONTAKT WITH FRAME  {&FRAME-NAME}.  
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_BORT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_BORT Dialog-Frame
ON CHOOSE OF BTN_BORT IN FRAME Dialog-Frame /* Ta bort */
DO:
   status-ok = BRW_KONTAKT:SELECT-FOCUSED-ROW() IN FRAME  {&FRAME-NAME} NO-ERROR.
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
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NY Dialog-Frame
ON CHOOSE OF BTN_NY IN FRAME Dialog-Frame /* Ny */
DO:
   CREATE kontaktperstemp.   
   RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(kontaktperstemp)).            
   RUN openbdyn_UI IN brwproc[1] (INPUT "").
   RUN lastselectdyn_UI IN brwproc[1].
   ASSIGN
   kontaktperstemp.FAX:READ-ONLY IN BROWSE BRW_KONTAKT = FALSE
   kontaktperstemp.NAMN:READ-ONLY IN BROWSE BRW_KONTAKT = FALSE   
   kontaktperstemp.EPOST:READ-ONLY IN BROWSE BRW_KONTAKT = FALSE  
   kontaktperstemp.TELEFON:READ-ONLY IN BROWSE BRW_KONTAKT = FALSE
   kontaktperstemp.MOBIL:READ-ONLY IN BROWSE BRW_KONTAKT = FALSE. 
   APPLY "ENTRY" TO kontaktperstemp.NAMN IN BROWSE BRW_KONTAKT. 
   DISPLAY BRW_KONTAKT WITH FRAME  {&FRAME-NAME}.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:
   IF VALID-HANDLE(edataapph) THEN DO:
      EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
      EMPTY TEMP-TABLE extradatatemp NO-ERROR.
      CREATE inextradatatemp.          
      ASSIGN
      inextradatatemp.PROGRAM = "BESTEPOST2"                   
      inextradatatemp.HUVUDCH = bestkundallt.BESTID. 
      RUN exbort_UI IN edataapph (INPUT TABLE inextradatatemp).
      EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
      EMPTY TEMP-TABLE extradatatemp NO-ERROR.
      FOR EACH kontaktperstemp:
         CREATE inextradatatemp.          
         ASSIGN
         inextradatatemp.PROGRAM = "BESTEPOST2"                   
         inextradatatemp.HUVUDCH = bestkundallt.BESTID 
         inextradatatemp.SOKCHAR[1]  =  kontaktperstemp.NAMN   
         inextradatatemp.SOKCHAR[2]  =  kontaktperstemp.EPOST  
         inextradatatemp.SOKCHAR[3]  =  kontaktperstemp.TELEFON
         inextradatatemp.SOKCHAR[4]  =  kontaktperstemp.MOBIL
         inextradatatemp.SOKCHAR[5]  =  kontaktperstemp.FAX.        
      END.  
      RUN sparaextra_UI IN edataapph (INPUT TABLE inextradatatemp). 
      EMPTY TEMP-TABLE extradatatemp NO-ERROR. 
      EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
      IF VALID-HANDLE(bestapph) THEN DELETE PROCEDURE bestapph.      
      IF VALID-HANDLE(edataapph) THEN DELETE PROCEDURE edataapph.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bestkundallt.VIBESTID
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bestkundallt.VIBESTID Dialog-Frame
ON LEAVE OF bestkundallt.VIBESTID IN FRAME Dialog-Frame /* Best-id */
DO:
   IF INPUT bestkundallt.VIBESTID = "" THEN DO:
      MESSAGE Guru.Konstanter:gbestk "kan inte vara blank!" VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
   IF vem = ? THEN DO:
      IF bestkundallt.VIBESTID NE INPUT bestkundallt.VIBESTID THEN DO:
         bestkundallt.VIBESTID = INPUT bestkundallt.VIBESTID.
         RUN namnkoll_UI IN bestapph (INPUT vem,INPUT bestkundallt.VIBESTID,OUTPUT TABLE felmeddtemp).                   
         FIND FIRST felmeddtemp NO-ERROR.
         IF AVAILABLE felmeddtemp THEN DO:
            MESSAGE felmeddtemp.FELMEDD VIEW-AS ALERT-BOX.
            DELETE felmeddtemp.
            RETURN NO-APPLY.
         END.  
      END.
   END.
   bestkundallt.VIBESTID = INPUT bestkundallt.VIBESTID.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


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
   FIND FIRST bestkundallt WHERE bestkundallt.BESTID = vem NO-ERROR.
   ASSIGN
   bestkundallt.VIBESTID:LABEL = Guru.Konstanter:gbestk    
   bestkundallt.AOKUND:LABEL = "Koppling till " + LC(Guru.Konstanter:gaok).   
   EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
   EMPTY TEMP-TABLE extradatatemp NO-ERROR.       
   CREATE inextradatatemp.          
   ASSIGN
   inextradatatemp.PROGRAM = "BESTEPOST"                   
   inextradatatemp.HUVUDCH = bestkundallt.BESTID.                    
   RUN etabhamt_UI IN edataapph (INPUT TABLE inextradatatemp, OUTPUT TABLE extradatatemp). 
   FIND FIRST extradatatemp NO-LOCK NO-ERROR.
   IF AVAILABLE extradatatemp THEN DO:      
      FILL-IN-EPOST = extradatatemp.SOKCHAR[1].
   END.  
   
   EMPTY TEMP-TABLE kontaktperstemp NO-ERROR. 
   EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
   EMPTY TEMP-TABLE extradatatemp NO-ERROR.       
   CREATE inextradatatemp.          
   ASSIGN
   inextradatatemp.PROGRAM = "BESTEPOST2"                   
   inextradatatemp.HUVUDCH = bestkundallt.BESTID.                    
   RUN etabhamt_UI IN edataapph (INPUT TABLE inextradatatemp, OUTPUT TABLE extradatatemp). 
   FOR EACH extradatatemp:
      CREATE kontaktperstemp.
      ASSIGN
      kontaktperstemp.NAMN    = extradatatemp.SOKCHAR[1]
      kontaktperstemp.EPOST   = extradatatemp.SOKCHAR[2]
      kontaktperstemp.TELEFON = extradatatemp.SOKCHAR[3]
      kontaktperstemp.MOBIL   = extradatatemp.SOKCHAR[4]
      kontaktperstemp.FAX     = extradatatemp.SOKCHAR[5].
   END.
   EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
   EMPTY TEMP-TABLE extradatatemp NO-ERROR.       
   RUN enable_UI.   
   RUN openbdyn_UI IN brwproc[1] (INPUT "").
   {FRMSIZED.I}
   ASSIGN                                                     
    kontaktperstemp.NAMN:READ-ONLY IN BROWSE BRW_KONTAKT = TRUE    
    kontaktperstemp.EPOST:READ-ONLY IN BROWSE BRW_KONTAKT = TRUE   
    kontaktperstemp.TELEFON:READ-ONLY IN BROWSE BRW_KONTAKT = TRUE 
    kontaktperstemp.FAX:READ-ONLY IN BROWSE BRW_KONTAKT = TRUE
    kontaktperstemp.MOBIL:READ-ONLY IN BROWSE BRW_KONTAKT = TRUE.  
   {musarrow.i}
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
   RUN DYNBRW.P PERSISTENT SET brwproc[1] (INPUT BRW_KONTAKT:HANDLE IN FRAME {&FRAME-NAME}).       
   IF Guru.Konstanter:appcon THEN DO:
      RUN EXTRADATAHMT.P PERSISTENT SET edataapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT.                  
   END.
   ELSE DO:
      RUN EXTRADATAHMT.P PERSISTENT SET edataapph.      
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
  DISPLAY FILL-IN-EPOST 
      WITH FRAME Dialog-Frame.
  IF AVAILABLE bestkundallt THEN 
    DISPLAY bestkundallt.VIBESTID bestkundallt.BESTNAMN bestkundallt.KONTAKT 
          bestkundallt.TEL bestkundallt.FAXNR bestkundallt.ADRESS 
          bestkundallt.PNR bestkundallt.ORT bestkundallt.LEVADRESS 
          bestkundallt.LEVPNR bestkundallt.LEVORT bestkundallt.FAKADRESS 
          bestkundallt.FAKPNR bestkundallt.FAKORT bestkundallt.AOKUND 
      WITH FRAME Dialog-Frame.
  ENABLE RECT-8 RECT-53 RECT-54 RECT-55 bestkundallt.BESTNAMN BRW_KONTAKT 
         bestkundallt.KONTAKT bestkundallt.TEL bestkundallt.FAXNR FILL-IN-EPOST 
         bestkundallt.ADRESS bestkundallt.PNR bestkundallt.ORT 
         bestkundallt.LEVADRESS bestkundallt.LEVPNR bestkundallt.LEVORT 
         bestkundallt.FAKADRESS bestkundallt.FAKPNR bestkundallt.FAKORT 
         bestkundallt.AOKUND BTN_NY BTN_ANDRA BTN_BORT Btn_OK 
      WITH FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION tabortblanka Dialog-Frame 
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

