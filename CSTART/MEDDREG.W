&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
&Scoped-define WINDOW-NAME WINDOW-1



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS WINDOW-1 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 95/05/02 - 12:41 pm

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
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
{ALLDEF.I}
&Scoped-define NEW NEW
&Scoped-define SHARED SHARED
{TELENAMN.I}
DEFINE NEW SHARED VARIABLE tidtabrec AS RECID NO-UNDO.
DEFINE NEW SHARED VARIABLE knappval AS CHARACTER NO-UNDO.

DEFINE NEW SHARED VARIABLE infil AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE utfil AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE sparfil AS CHARACTER NO-UNDO. 
DEFINE NEW SHARED VARIABLE andra AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE andrec AS RECID NO-UNDO.
&Scoped-define NEW
{GLOBVAR2DEL1.I}


                               
DEFINE SHARED VARIABLE intele AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE vartpro AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE SHARED VARIABLE vart AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO. 
DEFINE VARIABLE ladda AS LOGICAL NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE antal_valda AS INTEGER NO-UNDO.
DEFINE VARIABLE antal_raknare AS INTEGER NO-UNDO.
DEFINE VARIABLE utvar AS LOGICAL NO-UNDO.
DEFINE TEMP-TABLE tidin
   FIELD TIN AS CHARACTER FORMAT "X(256)".

DEFINE NEW SHARED TEMP-TABLE meddpers    
   FIELD NAMN AS CHARACTER FORMAT "X(40)" 
   FIELD ANVANDARE AS CHARACTER  FORMAT "X(12)"
   INDEX ANV IS PRIMARY ANVANDARE ASCENDING.

   
DEFINE NEW SHARED TEMP-TABLE valpers      
   FIELD NAMN AS CHARACTER FORMAT "X(40)"      
   FIELD TELNR AS CHARACTER FORMAT "X(12)".
&Scoped-define NEW   
&Scoped-define SHARED
{ANVPERS.I}


DEFINE TEMP-TABLE valanvandartemp NO-UNDO LIKE anvandartemp.
DEFINE TEMP-TABLE valtele_namn NO-UNDO LIKE tele_namn.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE WINDOW
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-a
&Scoped-define BROWSE-NAME BRW_ANV

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES anvandartemp tele_namn valanvandartemp ~
valtele_namn

/* Definitions for BROWSE BRW_ANV                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_ANV anvandartemp.ANVANDARE ~
anvandartemp.PERSONALKOD anvandartemp.AV-NAMN 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_ANV anvandartemp.PERSONALKOD 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_ANV anvandartemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_ANV anvandartemp
&Scoped-define QUERY-STRING-BRW_ANV FOR EACH anvandartemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_ANV OPEN QUERY BRW_ANV FOR EACH anvandartemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_ANV anvandartemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_ANV anvandartemp


/* Definitions for BROWSE BRW_TEL                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_TEL tele_namn.OMRADE ~
tele_namn.PERSONALKOD tele_namn.NAMN tele_namn.TELNR 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_TEL tele_namn.NAMN 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_TEL tele_namn
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_TEL tele_namn
&Scoped-define QUERY-STRING-BRW_TEL FOR EACH tele_namn NO-LOCK
&Scoped-define OPEN-QUERY-BRW_TEL OPEN QUERY BRW_TEL FOR EACH tele_namn NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_TEL tele_namn
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_TEL tele_namn


/* Definitions for BROWSE BRW_VANV                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_VANV valanvandartemp.ANVANDARE ~
valanvandartemp.PERSONALKOD valanvandartemp.AV-NAMN 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_VANV valanvandartemp.PERSONALKOD 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_VANV valanvandartemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_VANV valanvandartemp
&Scoped-define QUERY-STRING-BRW_VANV FOR EACH valanvandartemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_VANV OPEN QUERY BRW_VANV FOR EACH valanvandartemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_VANV valanvandartemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_VANV valanvandartemp


/* Definitions for BROWSE BRW_VTEL                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_VTEL valtele_namn.OMRADE ~
valtele_namn.PERSONALKOD valtele_namn.NAMN valtele_namn.TELNR 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_VTEL valtele_namn.NAMN 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_VTEL valtele_namn
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_VTEL valtele_namn
&Scoped-define QUERY-STRING-BRW_VTEL FOR EACH valtele_namn NO-LOCK
&Scoped-define OPEN-QUERY-BRW_VTEL OPEN QUERY BRW_VTEL FOR EACH valtele_namn NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_VTEL valtele_namn
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_VTEL valtele_namn


/* Definitions for FRAME FRAME-a                                        */
&Scoped-define OPEN-BROWSERS-IN-QUERY-FRAME-a ~
    ~{&OPEN-QUERY-BRW_ANV}~
    ~{&OPEN-QUERY-BRW_VANV}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-21 RAD_VAL BRW_ANV BRW_VTEL BRW_VANV ~
BTN_ALLOVER FBTN_MARK FBTN_STA BTN_OVER FBTN_STA-2 BTN_BACK BTN_ALLBACK ~
FILL-IN_SPERSONALKOD FILL-IN_SFORNAMN BTN_AVB 
&Scoped-Define DISPLAYED-OBJECTS RAD_VAL FILL-IN_SPERSONALKOD ~
FILL-IN_SFORNAMN 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR WINDOW-1 AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_ALLBACK 
     IMAGE-UP FILE "BILDER\rewind-u":U
     LABEL "Alla personer i listan":L 
     SIZE 5.5 BY 1.75 TOOLTIP "Alla valda personer tas bort från vallistan"
     FONT 11.

DEFINE BUTTON BTN_ALLOVER 
     IMAGE-UP FILE "BILDER\forwrd-u":U
     LABEL "Alla personer i listan":L 
     SIZE 5.5 BY 1.75 TOOLTIP "Alla personer väljs"
     FONT 11.

DEFINE BUTTON BTN_AVB AUTO-END-KEY 
     LABEL "Avsluta":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_BACK 
     IMAGE-UP FILE "BILDER\prev-u":U
     LABEL "":L 
     SIZE 5.5 BY 1.75 TOOLTIP "Markerade tas bort från vallistan".

DEFINE BUTTON BTN_OVER 
     IMAGE-UP FILE "BILDER\next-u":U
     LABEL "":L 
     SIZE 5.5 BY 1.75 TOOLTIP "Markerade väljs".

DEFINE BUTTON FBTN_MARK 
     LABEL "Skriv meddelande":L 
     SIZE 14 BY 1.

DEFINE BUTTON FBTN_STA 
     LABEL "Meddelande status" 
     SIZE 14 BY 1.

DEFINE BUTTON FBTN_STA-2 
     LABEL "Sparade meddelande" 
     SIZE 14 BY 1.

DEFINE VARIABLE FILL-IN-TEL AS CHARACTER FORMAT "X(12)":U 
     LABEL "Telenr" 
     VIEW-AS FILL-IN 
     SIZE 18 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN_SFORNAMN AS CHARACTER FORMAT "x(40)" 
     LABEL "Namn" 
     VIEW-AS FILL-IN 
     SIZE 12.75 BY .83.

DEFINE VARIABLE FILL-IN_SPERSONALKOD AS CHARACTER FORMAT "x(12)" 
     LABEL "Användare" 
     VIEW-AS FILL-IN 
     SIZE 10 BY .83.

DEFINE VARIABLE RAD_VAL AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "GURU-meddelande", 1
     SIZE 48.75 BY .67 NO-UNDO.

DEFINE RECTANGLE RECT-21
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 47 BY 1.21
     BGCOLOR 8 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_ANV FOR 
      anvandartemp SCROLLING.

DEFINE QUERY BRW_TEL FOR 
      tele_namn SCROLLING.

DEFINE QUERY BRW_VANV FOR 
      valanvandartemp SCROLLING.

DEFINE QUERY BRW_VTEL FOR 
      valtele_namn SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_ANV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_ANV WINDOW-1 _STRUCTURED
  QUERY BRW_ANV NO-LOCK DISPLAY
      anvandartemp.ANVANDARE COLUMN-LABEL "Användare" FORMAT "x(12)":U
      anvandartemp.PERSONALKOD COLUMN-LABEL "Enhet/Sign" FORMAT "x(5)":U
      anvandartemp.AV-NAMN COLUMN-LABEL "Användarnamn" FORMAT "x(40)":U
  ENABLE
      anvandartemp.PERSONALKOD
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING MULTIPLE SIZE 47 BY 14
         TITLE "Guruanvändare".

DEFINE BROWSE BRW_TEL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_TEL WINDOW-1 _STRUCTURED
  QUERY BRW_TEL NO-LOCK DISPLAY
      tele_namn.OMRADE COLUMN-LABEL "Område" FORMAT "x(6)":U
      tele_namn.PERSONALKOD COLUMN-LABEL "Enhet/Sign" FORMAT "x(5)":U
      tele_namn.NAMN COLUMN-LABEL "Namn" FORMAT "X(40)":U
      tele_namn.TELNR COLUMN-LABEL "Telefonnummer" FORMAT "X(12)":U
  ENABLE
      tele_namn.NAMN
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING MULTIPLE SIZE 47 BY 14
         TITLE "SMS-användare".

DEFINE BROWSE BRW_VANV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_VANV WINDOW-1 _STRUCTURED
  QUERY BRW_VANV NO-LOCK DISPLAY
      valanvandartemp.ANVANDARE COLUMN-LABEL "Användare" FORMAT "x(12)":U
      valanvandartemp.PERSONALKOD COLUMN-LABEL "Enhet/Sign" FORMAT "x(5)":U
      valanvandartemp.AV-NAMN COLUMN-LABEL "Användarnamn" FORMAT "x(40)":U
  ENABLE
      valanvandartemp.PERSONALKOD
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING MULTIPLE SIZE 41 BY 14
         TITLE "Valda Guruanvändare".

DEFINE BROWSE BRW_VTEL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_VTEL WINDOW-1 _STRUCTURED
  QUERY BRW_VTEL NO-LOCK DISPLAY
      valtele_namn.OMRADE COLUMN-LABEL "Område" FORMAT "x(6)":U
      valtele_namn.PERSONALKOD COLUMN-LABEL "Enhet/Sign" FORMAT "x(5)":U
      valtele_namn.NAMN COLUMN-LABEL "Namn" FORMAT "X(40)":U WIDTH 35.25
      valtele_namn.TELNR COLUMN-LABEL "Telefonnummer" FORMAT "X(12)":U
  ENABLE
      valtele_namn.NAMN
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING MULTIPLE SIZE 55.75 BY 14
         TITLE "Valda SMS-användare".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-a
     RAD_VAL AT ROW 3.17 COL 1.5 NO-LABEL
     BRW_TEL AT ROW 4.42 COL 1.5
     BRW_ANV AT ROW 4.42 COL 1.75
     BRW_VTEL AT ROW 4.42 COL 54.75
     BRW_VANV AT ROW 4.42 COL 54.75
     BTN_ALLOVER AT ROW 8 COL 49
     FBTN_MARK AT ROW 8 COL 111.5
     FBTN_STA AT ROW 9.08 COL 111.5
     BTN_OVER AT ROW 10.17 COL 49
     FBTN_STA-2 AT ROW 10.2 COL 111.5
     BTN_BACK AT ROW 12.38 COL 49
     BTN_ALLBACK AT ROW 14.58 COL 49
     FILL-IN-TEL AT ROW 18.96 COL 15.63 COLON-ALIGNED
     FILL-IN_SPERSONALKOD AT ROW 18.96 COL 16.63 COLON-ALIGNED
     FILL-IN_SFORNAMN AT ROW 18.96 COL 32.75 COLON-ALIGNED
     BTN_AVB AT ROW 19 COL 111.5
     "Sök på:" VIEW-AS TEXT
          SIZE 8.25 BY .83 AT ROW 18.96 COL 2.25
     "Välj vilken eller vilka personer som skall få ett meddelande" VIEW-AS TEXT
          SIZE 70 BY 1.33 AT ROW 1.5 COL 1.5
          FONT 17
     RECT-21 AT ROW 18.79 COL 1.5
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 125 BY 19.38.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: WINDOW
   Temp-Tables and Buffers:
      TABLE: anvandartemp T "?" NO-UNDO temp-db anvandartemp
      TABLE: tele_namn T "?" NO-UNDO temp-db tele_namn
      TABLE: tele_temp T "?" NO-UNDO temp-db tele_temp
      TABLE: valanvandartemp T "?" NO-UNDO temp-db valanvandartemp
      TABLE: valtele_namn T "?" NO-UNDO temp-db valtele_namn
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW WINDOW-1 ASSIGN
         HIDDEN             = YES
         TITLE              = "Meddelande till Guruanvändare"
         HEIGHT             = 19.42
         WIDTH              = 125.13
         MAX-HEIGHT         = 27.25
         MAX-WIDTH          = 125.13
         VIRTUAL-HEIGHT     = 27.25
         VIRTUAL-WIDTH      = 125.13
         RESIZE             = yes
         SCROLL-BARS        = yes
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW WINDOW-1
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME FRAME-a
                                                                        */
/* BROWSE-TAB BRW_TEL RAD_VAL FRAME-a */
/* BROWSE-TAB BRW_ANV BRW_TEL FRAME-a */
/* BROWSE-TAB BRW_VTEL BRW_ANV FRAME-a */
/* BROWSE-TAB BRW_VANV BRW_VTEL FRAME-a */
ASSIGN 
       BRW_ANV:HIDDEN  IN FRAME FRAME-a                = TRUE.

/* SETTINGS FOR BROWSE BRW_TEL IN FRAME FRAME-a
   NO-ENABLE                                                            */
ASSIGN 
       BRW_TEL:HIDDEN  IN FRAME FRAME-a                = TRUE.

ASSIGN 
       BRW_VANV:HIDDEN  IN FRAME FRAME-a                = TRUE.

ASSIGN 
       BRW_VTEL:HIDDEN  IN FRAME FRAME-a                = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-TEL IN FRAME FRAME-a
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN-TEL:HIDDEN IN FRAME FRAME-a           = TRUE.

ASSIGN 
       RAD_VAL:HIDDEN IN FRAME FRAME-a           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-1)
THEN WINDOW-1:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_ANV
/* Query rebuild information for BROWSE BRW_ANV
     _TblList          = "Temp-Tables.anvandartemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.anvandartemp.ANVANDARE
"anvandartemp.ANVANDARE" "Användare" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.anvandartemp.PERSONALKOD
"anvandartemp.PERSONALKOD" "Enhet/Sign" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.anvandartemp.AV-NAMN
"anvandartemp.AV-NAMN" "Användarnamn" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is OPENED
*/  /* BROWSE BRW_ANV */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_TEL
/* Query rebuild information for BROWSE BRW_TEL
     _TblList          = "Temp-Tables.tele_namn"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.tele_namn.OMRADE
"tele_namn.OMRADE" "Område" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.tele_namn.PERSONALKOD
"tele_namn.PERSONALKOD" "Enhet/Sign" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.tele_namn.NAMN
"tele_namn.NAMN" "Namn" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[4]   > Temp-Tables.tele_namn.TELNR
"tele_namn.TELNR" "Telefonnummer" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_TEL */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_VANV
/* Query rebuild information for BROWSE BRW_VANV
     _TblList          = "Temp-Tables.valanvandartemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.valanvandartemp.ANVANDARE
"valanvandartemp.ANVANDARE" "Användare" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.valanvandartemp.PERSONALKOD
"valanvandartemp.PERSONALKOD" "Enhet/Sign" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.valanvandartemp.AV-NAMN
"valanvandartemp.AV-NAMN" "Användarnamn" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is OPENED
*/  /* BROWSE BRW_VANV */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_VTEL
/* Query rebuild information for BROWSE BRW_VTEL
     _TblList          = "Temp-Tables.valtele_namn"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.valtele_namn.OMRADE
"valtele_namn.OMRADE" "Område" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.valtele_namn.PERSONALKOD
"valtele_namn.PERSONALKOD" "Enhet/Sign" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.valtele_namn.NAMN
"valtele_namn.NAMN" "Namn" ? "character" ? ? ? ? ? ? yes ? no no "35.25" yes no no "U" "" ""
     _FldNameList[4]   > Temp-Tables.valtele_namn.TELNR
"valtele_namn.TELNR" "Telefonnummer" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_VTEL */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB WINDOW-1
ON CHOOSE OF BTN_AVB IN FRAME FRAME-a /* Avsluta */
DO:
   APPLY "CLOSE":U TO THIS-PROCEDURE.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_MARK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_MARK WINDOW-1
ON CHOOSE OF FBTN_MARK IN FRAME FRAME-a /* Skriv meddelande */
DO:     
   RUN btnmark_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_STA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_STA WINDOW-1
ON CHOOSE OF FBTN_STA IN FRAME FRAME-a /* Meddelande status */
DO:
   {muswait.i}  
   
   RUN MEDDSTA.W (INPUT 1).
   
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_STA-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_STA-2 WINDOW-1
ON CHOOSE OF FBTN_STA-2 IN FRAME FRAME-a /* Sparade meddelande */
DO:
   {muswait.i}  
   
   RUN MEDDSTA.W (INPUT 2).
   
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-TEL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-TEL WINDOW-1
ON ANY-KEY OF FILL-IN-TEL IN FRAME FRAME-a /* Telenr */
DO:
   {TRYCKS.I}
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN-TEL IN FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-TEL WINDOW-1
ON MOUSE-SELECT-DBLCLICK OF FILL-IN-TEL IN FRAME FRAME-a /* Telenr */
DO:
   FILL-IN-TEL = INPUT FILL-IN-TEL.
   IF FILL-IN-TEL = '' THEN DO:
      MESSAGE "Sökbegreppet kan inte vara blankt." VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN-TEL IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
   END.      
   RUN sokurvaldyn_UI IN brwproc[5] (INPUT "TELNR", INPUT FILL-IN-TEL). 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_SFORNAMN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_SFORNAMN WINDOW-1
ON ANY-KEY OF FILL-IN_SFORNAMN IN FRAME FRAME-a /* Namn */
DO:
   {TRYCKS.I}
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN_SFORNAMN IN FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_SFORNAMN WINDOW-1
ON MOUSE-SELECT-DBLCLICK OF FILL-IN_SFORNAMN IN FRAME FRAME-a /* Namn */
DO:
   FILL-IN_SFORNAMN = INPUT FILL-IN_SFORNAMN.
   IF FILL-IN_SFORNAMN = '' THEN DO:
      MESSAGE "Sökbegreppet kan inte vara blankt." VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN_SFORNAMN IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
   END.   
   IF RAD_VAL = 1 THEN DO:
      RUN sokurvaldyn_UI IN brwproc[5] (INPUT "AV-NAMN", INPUT FILL-IN_SFORNAMN).   
   END.
   ELSE DO:      
      RUN sokurvaldyn_UI IN brwproc[5] (INPUT "NAMN", INPUT FILL-IN_SFORNAMN). 
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_SPERSONALKOD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_SPERSONALKOD WINDOW-1
ON ANY-KEY OF FILL-IN_SPERSONALKOD IN FRAME FRAME-a /* Användare */
DO:
   {TRYCKS.I}
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN_SPERSONALKOD IN FRAME {&FRAME-NAME}.
   END. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_SPERSONALKOD WINDOW-1
ON MOUSE-SELECT-DBLCLICK OF FILL-IN_SPERSONALKOD IN FRAME FRAME-a /* Användare */
DO:
   FILL-IN_SPERSONALKOD = INPUT FILL-IN_SPERSONALKOD.
   IF FILL-IN_SPERSONALKOD = '' THEN DO:
      MESSAGE "Sökbegreppet kan inte vara blankt." VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN_SPERSONALKOD IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
   END.  
   RUN sokurvaldyn_UI IN brwproc[5] (INPUT "ANVANDARE", INPUT FILL-IN_SPERSONALKOD).   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RAD_VAL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RAD_VAL WINDOW-1
ON VALUE-CHANGED OF RAD_VAL IN FRAME FRAME-a
DO:
   {muswait.i}
   RAD_VAL = INPUT RAD_VAL.
   ASSIGN
   BRW_TEL:HIDDEN IN FRAME {&FRAME-NAME} = TRUE      
   BRW_ANV:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
   BRW_VTEL:HIDDEN IN FRAME {&FRAME-NAME} = TRUE      
   BRW_VANV:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
   FILL-IN-TEL:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
   FILL-IN_SPERSONALKOD:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
   FILL-IN_SFORNAMN:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
   FBTN_STA:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
/*    BTN_ALLA:HIDDEN IN FRAME {&FRAME-NAME} = TRUE */
   FBTN_MARK:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
   IF RAD_VAL = 1 THEN DO:
      RUN changebrwh_UI IN brwproc[5] (INPUT BRW_ANV:HANDLE, INPUT BRW_VANV:HANDLE).
      ASSIGN
      FBTN_STA:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      FBTN_MARK:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      BRW_ANV:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      BRW_VANV:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      FILL-IN_SPERSONALKOD:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      FILL-IN_SFORNAMN:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
/*       BTN_ALLA:HIDDEN IN FRAME {&FRAME-NAME} = FALSE. */
   END.
   ELSE IF RAD_VAL = 2 THEN DO:
      IF ladda = FALSE THEN DO:
         RUN ladda_UI.
         ladda = TRUE.
      END.
      RUN changebrwh_UI IN brwproc[5] (INPUT BRW_TEL:HANDLE, INPUT BRW_VTEL:HANDLE).
      ASSIGN
      FBTN_MARK:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      BRW_TEL:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      BRW_VTEL:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      FILL-IN-TEL:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      FILL-IN_SFORNAMN:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
      APPLY "HOME" TO BRW_TEL IN FRAME {&FRAME-NAME}.
   END.
   ELSE IF RAD_VAL = 3 THEN DO:
      
      RUN EMEDD.W (INPUT FALSE).
      
      RAD_VAL = 1.
      DISPLAY RAD_VAL WITH FRAME {&FRAME-NAME}.
      ASSIGN
      FBTN_STA:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      FBTN_MARK:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      BRW_ANV:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      BRW_VANV:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      FILL-IN_SPERSONALKOD:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      FILL-IN_SFORNAMN:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
/*       BTN_ALLA:HIDDEN IN FRAME {&FRAME-NAME} = FALSE. */
   END.
   
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_ANV
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK WINDOW-1 


/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE DO:
   {BORTBRWPROC.I}
   RUN disable_UI.
END.
   

/* These events will close the window and terminate the procedure.      */
/* (NOTE: this will override any user-defined triggers previously       */
/*  defined on the window.)                                             */
ON WINDOW-CLOSE OF {&WINDOW-NAME} DO:
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.
ON ENDKEY, END-ERROR OF {&WINDOW-NAME} ANYWHERE DO:
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
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
   ladda = FALSE.
   IF Guru.Konstanter:appcon THEN DO:                           
      RUN ANVSKAP.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT 4,INPUT "",INPUT-OUTPUT TABLE anvandartemp,INPUT-OUTPUT TABLE personaltemp).
   END.
   ELSE DO:
      RUN ANVSKAP.P 
      (INPUT 4,INPUT "",INPUT-OUTPUT TABLE anvandartemp,INPUT-OUTPUT TABLE personaltemp).
   END.
   FOR EACH tele_namn:
      DELETE tele_namn.
   END.
   FOR EACH valpers:
      DELETE valpers.
   END.   
   RUN enable_UI.
   {FRMSIZE.I}
   IF Guru.Konstanter:globforetag = "ELPA" OR Guru.Konstanter:globforetag = "GRAN" THEN DO:
      ENABLE BRW_TEL BRW_VTEL FILL-IN-TEL WITH FRAME {&FRAME-NAME}.
      ASSIGN
      BRW_TEL:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      BRW_VTEL:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      FILL-IN-TEL:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
      status-ok = RAD_VAL:ADD-LAST("SMS-meddelande", 2).
   END.   
   status-ok = RAD_VAL:ADD-LAST("E-post",3).
   RAD_VAL = 1.      
   APPLY "VALUE-CHANGED" TO RAD_VAL.
   RUN openbdyn_UI IN brwproc[1] (INPUT "").

   {musarrow.i}
   {WIN_M_SLUT.I}
   IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE allstartbrw_UI WINDOW-1 
PROCEDURE allstartbrw_UI :
/* -----------------------------------------------------------
  Purpose: Changing screen-value for combo-box CMB_OMR     
  Parameters:  Input = Screen-value for CMB_FOR
  Notes:       
-------------------------------------------------------------*/ 
   ASSIGN
   anvandartemp.PERSONALKOD:READ-ONLY IN BROWSE BRW_ANV = TRUE    
   tele_namn.NAMN:READ-ONLY IN BROWSE BRW_TEL = TRUE
   valanvandartemp.PERSONALKOD:READ-ONLY IN BROWSE BRW_VANV = TRUE    
   valtele_namn.NAMN:READ-ONLY IN BROWSE BRW_VTEL = TRUE.
   RUN DYNBRW.P PERSISTENT SET brwproc[1] 
      (INPUT BRW_ANV:HANDLE IN FRAME {&FRAME-NAME}).
   RUN DYNBRW.P PERSISTENT SET brwproc[2] 
      (INPUT BRW_TEL:HANDLE IN FRAME {&FRAME-NAME}).   
   RUN DYNBRW.P PERSISTENT SET brwproc[3] 
      (INPUT BRW_VANV:HANDLE IN FRAME {&FRAME-NAME}).
   RUN DYNBRW.P PERSISTENT SET brwproc[4] 
      (INPUT BRW_VTEL:HANDLE IN FRAME {&FRAME-NAME}).   
   RUN DYNARROW.P PERSISTENT SET brwproc[5]
      (INPUT BRW_ANV:HANDLE, INPUT BRW_VANV:HANDLE ,
       INPUT BTN_OVER:HANDLE, INPUT BTN_ALLOVER:HANDLE ,
       INPUT BTN_ALLBACK:HANDLE, INPUT BTN_BACK:HANDLE).    
   RUN setdefaultcol_UI IN brwproc[2] (INPUT 3).
   RUN colsortdynbrw_UI IN brwproc[2] (INPUT "").
   RUN setdefaultcol_UI IN brwproc[4] (INPUT 3).
   RUN colsortdynbrw_UI IN brwproc[4] (INPUT "").
   /*
   RUN deleteleftbrw_UI IN brwproc[5] (INPUT TRUE).
   */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnmark_UI WINDOW-1 
PROCEDURE btnmark_UI :
/* -----------------------------------------------------------
  Purpose: Changing screen-value for combo-box CMB_OMR     
  Parameters:  Input = Screen-value for CMB_FOR
  Notes:       
-------------------------------------------------------------*/ 
   {muswait.i}
   RUN urvalaktiv_UI IN brwproc[5] (OUTPUT utvar).
   IF utvar = TRUE THEN DO:
      IF RAD_VAL = 1 THEN DO:
         antal_valda = BRW_ANV:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME}.
         IF antal_valda = 0 THEN DO:
            MESSAGE "Du måste välja någon." VIEW-AS ALERT-BOX.
         END.
         ELSE DO:
            antal_raknare = 1.
            status-ok = BRW_ANV:FETCH-SELECTED-ROW(antal_raknare) IN FRAME {&FRAME-NAME}.
            IF antal_valda = 1 THEN knappval = anvandartemp.ANVANDARE.
            ELSE knappval = "MARKERADE".
            DO TRANSACTION:
               DO WHILE antal_raknare LE antal_valda :
                  status-ok = BRW_ANV:FETCH-SELECTED-ROW(antal_raknare) IN FRAME {&FRAME-NAME}.
                  CREATE meddpers.
                  ASSIGN
                  meddpers.ANVANDARE = anvandartemp.ANVANDARE
                  meddpers.NAMN = anvandartemp.AV-NAMN.
                  tidtabrec = RECID(anvandartemp).
                  antal_raknare = antal_raknare + 1.
               END.
            END.
            
            RUN ANDMEDD.W.
            
            FOR EACH meddpers:
               DELETE meddpers.
            END.
         END.
      END.
      ELSE DO:
         antal_valda = BRW_TEL:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME}.
         IF antal_valda = 0 THEN DO:
            MESSAGE "Du måste välja någon." VIEW-AS ALERT-BOX.
         END.
         ELSE DO:
            antal_raknare = 1.
            status-ok = BRW_TEL:FETCH-SELECTED-ROW(antal_raknare) IN FRAME {&FRAME-NAME}.
            IF antal_valda = 1 THEN knappval = tele_namn.NAMN.
            ELSE knappval = "MARKERADE".
            DO TRANSACTION:
               DO WHILE antal_raknare LE antal_valda :
                  status-ok = BRW_TEL:FETCH-SELECTED-ROW(antal_raknare) IN FRAME {&FRAME-NAME}.
                  CREATE valpers.
                  ASSIGN
                  valpers.TELNR = tele_namn.TELNR
                  valpers.NAMN = tele_namn.NAMN.
                  antal_raknare = antal_raknare + 1.
               END.
            END.
            
            RUN SMSMEDD.W.
            
            FOR EACH valpers:
               DELETE valpers.
            END.
         END.
      END.
   END.
   ELSE DO:
      IF RAD_VAL = 1 THEN DO:
         antal_valda = QUERY BRW_VANV:NUM-RESULTS IN FRAME {&FRAME-NAME}.
         IF antal_valda = 0 THEN DO:
            MESSAGE "Du måste välja någon." VIEW-AS ALERT-BOX.
         END.
         ELSE DO:
            FOR EACH valanvandartemp NO-LOCK:
               CREATE meddpers.
               ASSIGN
               knappval = valanvandartemp.ANVANDARE
               meddpers.ANVANDARE = valanvandartemp.ANVANDARE
               meddpers.NAMN = valanvandartemp.AV-NAMN.
               tidtabrec = RECID(valanvandartemp).
            END.
            IF antal_valda NE 1 THEN knappval = "MARKERADE".
            
            RUN ANDMEDD.W.
            
            FOR EACH meddpers:
               DELETE meddpers.
            END.
         END.
      END.
      ELSE DO:
         antal_valda = QUERY BRW_VTEL:NUM-RESULTS IN FRAME {&FRAME-NAME}.
         IF antal_valda = 0 THEN DO:
            MESSAGE "Du måste välja någon." VIEW-AS ALERT-BOX.
         END.
         ELSE DO:
            FOR EACH valtele_namn NO-LOCK:
               CREATE valpers.
               ASSIGN
               knappval = valtele_namn.NAMN
               valpers.TELNR = valtele_namn.TELNR
               valpers.NAMN = valtele_namn.NAMN.               
            END.
            IF antal_valda NE 1 THEN knappval = "MARKERADE".
            
            RUN SMSMEDD.W.
            
            FOR EACH valpers:
               DELETE valpers.
            END.
         END.
      END.
   END.
   {musarrow.i}   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI WINDOW-1  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-1)
  THEN DELETE WIDGET WINDOW-1.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI WINDOW-1  _DEFAULT-ENABLE
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
  DISPLAY RAD_VAL FILL-IN_SPERSONALKOD FILL-IN_SFORNAMN 
      WITH FRAME FRAME-a IN WINDOW WINDOW-1.
  ENABLE RECT-21 RAD_VAL BRW_ANV BRW_VTEL BRW_VANV BTN_ALLOVER FBTN_MARK 
         FBTN_STA BTN_OVER FBTN_STA-2 BTN_BACK BTN_ALLBACK FILL-IN_SPERSONALKOD 
         FILL-IN_SFORNAMN BTN_AVB 
      WITH FRAME FRAME-a IN WINDOW WINDOW-1.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-a}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ladda_UI WINDOW-1 
PROCEDURE ladda_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   FOR EACH tele_namn:
      DELETE tele_namn.
   END.   
   IF Guru.Konstanter:globforetag = "ELPA" THEN DO:
      ASSIGN 
      infil = {GRANSMS.I} + "settings\legalin.txt"
      utfil = {GRANSMS.I} + "RESULT\sms_send.txt"
      sparfil = {GRANSMS.I} + "RESULT\sms_spar2.txt".      
   END.  
   IF Guru.Konstanter:globforetag = "GRAN" THEN DO:
      ASSIGN 
      infil = {GRANSMS.I} + "settings\legalin.txt"
      utfil = {GRANSMS.I} + "RESULT\sms_send.txt"
      sparfil = {GRANSMS.I} + "RESULT\sms_spar2.txt".      
   END. 

   IF Guru.Konstanter:globforetag = "ELPA" THEN DO:
      RUN LEGALTXT.P
         (INPUT infil, INPUT Guru.Konstanter:dlcvar, INPUT Guru.Konstanter:wtidvar, OUTPUT TABLE tele_namn).
   END.
   ELSE DO:
      IF Guru.Konstanter:appcon THEN DO:                           
         RUN LEGALTXT.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
         (INPUT infil, INPUT Guru.Konstanter:dlcvar, INPUT Guru.Konstanter:wtidvar, OUTPUT TABLE tele_namn).
      END.
      ELSE DO:
         RUN LEGALTXT.P
         (INPUT infil, INPUT Guru.Konstanter:dlcvar, INPUT Guru.Konstanter:wtidvar, OUTPUT TABLE tele_namn).
      END.          
   END.
   FOR EACH tidin:
      DELETE tidin.
   END.   
   IF Guru.Konstanter:globforetag = "ELPA" THEN DO:
      ASSIGN 
      infil = {GRANSMS.I} + "settings\address.txt".
   END.
   IF Guru.Konstanter:globforetag = "GRAN" THEN DO:
      ASSIGN 
      infil = {GRANSMS.I} + "settings\address.txt".
   END. 
   {AMERICANEUROPEAN.I}
   IF Guru.Konstanter:appcon THEN DO:                           
      RUN ADRESTXT.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT infil, INPUT Guru.Konstanter:dlcvar, INPUT Guru.Konstanter:wtidvar, INPUT-OUTPUT TABLE tele_namn).
   END.
   ELSE DO:
      RUN ADRESTXT.P
      (INPUT infil, INPUT Guru.Konstanter:dlcvar, INPUT Guru.Konstanter:wtidvar, INPUT-OUTPUT TABLE tele_namn).
   END.  
   {EUROPEANAMERICAN.I}
   OPEN QUERY tq FOR EACH tele_namn,
   EACH anvandartemp WHERE anvandartemp.AV-NAMN = tele_namn.NAMN,
   EACH personaltemp WHERE personaltemp.PERSONALKOD = anvandartemp.PERSONALKOD.
   GET FIRST tq.
   DO WHILE AVAILABLE(tele_namn):
      ASSIGN 
      tele_namn.OMRADE = personaltemp.OMRADE 
      tele_namn.PERSONALKOD = anvandartemp.PERSONALKOD.                
      GET NEXT tq.   
   END.
   RUN openbdyn_UI IN brwproc[2] (INPUT "").
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

