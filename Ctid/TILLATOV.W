&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v7r11 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
&Scoped-define WINDOW-NAME WINDOW-2


/* Temp-Table and Buffer definitions                                    */




&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS WINDOW-2 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 95/09/15 -  2:57 pm

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
&Scoped-define NEW
{TIDPERS.I}
{GLOBVAR2DEL1.I}
{REGVAR.I}
{TILLATOV.I}
DEFINE NEW SHARED VARIABLE fnytid AS DECIMAL FORMAT "-99.99" NO-UNDO.
DEFINE SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE tidtabrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE persrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE persrec2 AS RECID NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.   
DEFINE VARIABLE periodtot AS DECIMAL FORMAT "99.99" NO-UNDO.
DEFINE VARIABLE pekodtot AS DECIMAL FORMAT "99.99" NO-UNDO.
DEFINE VARIABLE pertot AS INTEGER FORMAT "-9999999" NO-UNDO.
DEFINE VARIABLE pkodtot AS INTEGER FORMAT "-9999999" NO-UNDO.
DEFINE VARIABLE difftot AS DECIMAL FORMAT "-99.99" NO-UNDO.
DEFINE VARIABLE excellista AS INTEGER NO-UNDO.
DEFINE VARIABLE dispens AS LOGICAL NO-UNDO.
DEFINE VARIABLE dispensm AS LOGICAL NO-UNDO.
DEFINE VARIABLE dispens48 AS LOGICAL NO-UNDO.
DEFINE VARIABLE dmandatum AS DATE  NO-UNDO.
DEFINE VARIABLE dmangodk AS CHARACTER NO-UNDO.
DEFINE VARIABLE dardatum AS DATE  NO-UNDO.
DEFINE VARIABLE dargodk AS CHARACTER NO-UNDO.
DEFINE VARIABLE d48datum AS DATE  NO-UNDO.
DEFINE VARIABLE d48godk AS CHARACTER NO-UNDO.
DEFINE VARIABLE ater50 AS LOGICAL NO-UNDO.   
DEFINE VARIABLE a50datum AS DATE  NO-UNDO.
DEFINE VARIABLE a50godk AS CHARACTER NO-UNDO.   
DEFINE VARIABLE a50tim AS INTEGER NO-UNDO.




DEFINE VARIABLE asfaktapph AS HANDLE NO-UNDO.                     /**/

DEFINE TEMP-TABLE invartemp   
   FIELD GA AS CHARACTER
   FIELD GM AS LOGICAL 
   FIELD SK AS LOGICAL 
   FIELD TI AS RECID 
   FIELD PER AS RECID 
   FIELD PER2 AS RECID 
   FIELD MU AS LOGICAL    
   FIELD REGST AS DECIMAL
   FIELD REGSU AS DECIMAL 
   FIELD RV AS INTEGER FORMAT "999" 
   FIELD RDAG AS CHARACTER FORMAT "X(3)"         
   FIELD RD AS DATE 
   FIELD RM AS INTEGER FORMAT "99" 
   FIELD RMN AS CHARACTER  
   FIELD REGA AS INTEGER FORMAT "99" 
   FIELD RT AS DECIMAL       
   FIELD BD AS DATE 
   FIELD AD AS DATE 
   FIELD NY AS DECIMAL 
   FIELD SEK AS INTEGER FORMAT "-9999999" 
   FIELD RSEK AS INTEGER 
   FIELD REGS AS INTEGER 
   FIELD GL AS CHARACTER.
   
DEFINE VARIABLE str AS CHARACTER FORMAT "X(90)" NO-UNDO.
/*{TIDUTTTNEW.I}*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE WINDOW
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-TIDS
&Scoped-define BROWSE-NAME BRW_TILLATOV

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tillatov

/* Definitions for BROWSE BRW_TILLATOV                                  */
&Scoped-define FIELDS-IN-QUERY-BRW_TILLATOV tillatov.PERSONALKOD ~
tillatov.FORNAMN tillatov.EFTERNAMN tillatov.TOVAR tillatov.DARDATUM ~
tillatov.DARGODK tillatov.TOVMAN tillatov.DMANDATUM tillatov.DMANGODK ~
tillatov.TOV48 tillatov.D48DATUM tillatov.D48GODK tillatov.ATER50 ~
tillatov.ATERTIM tillatov.A50DATUM tillatov.A50GODK tillatov.OMRADE 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_TILLATOV tillatov.TOVAR ~
tillatov.TOVMAN tillatov.TOV48 tillatov.ATER50 tillatov.ATERTIM 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_TILLATOV tillatov
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_TILLATOV tillatov
&Scoped-define QUERY-STRING-BRW_TILLATOV FOR EACH tillatov NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BRW_TILLATOV OPEN QUERY BRW_TILLATOV FOR EACH tillatov NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BRW_TILLATOV tillatov
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_TILLATOV tillatov


/* Definitions for FRAME FRAME-TIDS                                     */
&Scoped-define OPEN-BROWSERS-IN-QUERY-FRAME-TIDS ~
    ~{&OPEN-QUERY-BRW_TILLATOV}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BRW_TILLATOV BTN_AVB 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR WINDOW-2 AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB AUTO-END-KEY 
     LABEL "Avsluta":L 
     SIZE 14 BY 1.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_TILLATOV FOR 
      tillatov SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_TILLATOV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_TILLATOV WINDOW-2 _STRUCTURED
  QUERY BRW_TILLATOV NO-LOCK DISPLAY
      tillatov.PERSONALKOD COLUMN-LABEL "Enhet!/Sign" FORMAT "x(6)":U
      tillatov.FORNAMN COLUMN-LABEL "Förnamn" FORMAT "x(256)":U
            WIDTH 10
      tillatov.EFTERNAMN COLUMN-LABEL "Efternamn" FORMAT "x(256)":U
            WIDTH 15
      tillatov.TOVAR COLUMN-LABEL "Tillåt övertid! mer än 200tim/år" FORMAT "Ja/Nej":U
            WIDTH 13
      tillatov.DARDATUM COLUMN-LABEL "Inlagt!datum" FORMAT "99/99/99":U
      tillatov.DARGODK COLUMN-LABEL "Inlagt!av" FORMAT "x(8)":U
      tillatov.TOVMAN COLUMN-LABEL "Tillåt övertid! mer än 50tim/månad" FORMAT "Ja/Nej":U
            WIDTH 13
      tillatov.DMANDATUM COLUMN-LABEL "Inlagt!datum" FORMAT "99/99/99":U
      tillatov.DMANGODK COLUMN-LABEL "Inlagt!av" FORMAT "x(8)":U
      tillatov.TOV48 COLUMN-LABEL "Tillåt övertid! mer än 48tim/4veckor" FORMAT "Ja/Nej":U
            WIDTH 13
      tillatov.D48DATUM COLUMN-LABEL "Inlagt!datum" FORMAT "99/99/99":U
      tillatov.D48GODK COLUMN-LABEL "Inlagt!av" FORMAT "x(8)":U
      tillatov.ATER50 COLUMN-LABEL "Återför!max 50 tim" FORMAT "Ja/Nej":U
            WIDTH 13
      tillatov.ATERTIM COLUMN-LABEL "Återför!timmar" FORMAT ">>9":U
      tillatov.A50DATUM COLUMN-LABEL "Inlagt!datum" FORMAT "99/99/99":U
      tillatov.A50GODK COLUMN-LABEL "Inlagt!av" FORMAT "x(8)":U
      tillatov.OMRADE FORMAT "x(6)":U WIDTH 1.63
  ENABLE
      tillatov.TOVAR
      tillatov.TOVMAN
      tillatov.TOV48
      tillatov.ATER50
      tillatov.ATERTIM
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 121.5 BY 22.25
         TITLE "Tillåt övertid mer än 200 tim/år - 50 tim/månad" FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-TIDS
     BRW_TILLATOV AT ROW 3.25 COL 2.5 WIDGET-ID 100
     BTN_AVB AT ROW 28 COL 110.5
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 123.63 BY 28.42.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: WINDOW
   Temp-Tables and Buffers:
      TABLE: tillatov T "?" NO-UNDO temp-db tillatov
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW WINDOW-2 ASSIGN
         HIDDEN             = YES
         TITLE              = "Tillåt övertid mer än 200 tim/år - 50 tim/månad"
         HEIGHT             = 28.42
         WIDTH              = 124.5
         MAX-HEIGHT         = 28.42
         MAX-WIDTH          = 124.5
         VIRTUAL-HEIGHT     = 28.42
         VIRTUAL-WIDTH      = 124.5
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
/* SETTINGS FOR WINDOW WINDOW-2
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME FRAME-TIDS
   FRAME-NAME                                                           */
/* BROWSE-TAB BRW_TILLATOV 1 FRAME-TIDS */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-2)
THEN WINDOW-2:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_TILLATOV
/* Query rebuild information for BROWSE BRW_TILLATOV
     _TblList          = "Temp-Tables.tillatov"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > Temp-Tables.tillatov.PERSONALKOD
"PERSONALKOD" "Enhet!/Sign" "x(6)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.tillatov.FORNAMN
"FORNAMN" "Förnamn" "x(256)" "character" ? ? ? ? ? ? no ? no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.tillatov.EFTERNAMN
"EFTERNAMN" "Efternamn" "x(256)" "character" ? ? ? ? ? ? no ? no no "15" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.tillatov.TOVAR
"TOVAR" "Tillåt övertid! mer än 200tim/år" ? "logical" ? ? ? ? ? ? yes ? no no "13" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.tillatov.DARDATUM
"DARDATUM" "Inlagt!datum" ? "date" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > Temp-Tables.tillatov.DARGODK
"DARGODK" "Inlagt!av" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > Temp-Tables.tillatov.TOVMAN
"TOVMAN" "Tillåt övertid! mer än 50tim/månad" ? "logical" ? ? ? ? ? ? yes ? no no "13" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > Temp-Tables.tillatov.DMANDATUM
"DMANDATUM" "Inlagt!datum" ? "date" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > Temp-Tables.tillatov.DMANGODK
"DMANGODK" "Inlagt!av" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > Temp-Tables.tillatov.TOV48
"TOV48" "Tillåt övertid! mer än 48tim/4veckor" ? "logical" ? ? ? ? ? ? yes ? no no "13" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > Temp-Tables.tillatov.D48DATUM
"D48DATUM" "Inlagt!datum" ? "date" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > Temp-Tables.tillatov.D48GODK
"D48GODK" "Inlagt!av" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > Temp-Tables.tillatov.ATER50
"ATER50" "Återför!max 50 tim" ? "logical" ? ? ? ? ? ? yes ? no no "13" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[14]   > Temp-Tables.tillatov.ATERTIM
"ATERTIM" "Återför timmar" ? "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[15]   > Temp-Tables.tillatov.A50DATUM
"A50DATUM" "Inlagt!datum" ? "date" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[16]   > Temp-Tables.tillatov.A50GODK
"A50GODK" "Inlagt!av" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[17]   > Temp-Tables.tillatov.OMRADE
"OMRADE" ? ? "character" ? ? ? ? ? ? no ? no no "1.63" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE BRW_TILLATOV */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-TIDS
/* Query rebuild information for FRAME FRAME-TIDS
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME FRAME-TIDS */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME BRW_TILLATOV
&Scoped-define SELF-NAME BRW_TILLATOV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_TILLATOV WINDOW-2
ON ROW-LEAVE OF BRW_TILLATOV IN FRAME FRAME-TIDS /* Tillåt övertid mer än 200 tim/år - 50 tim/månad */
DO:
   RUN rowleave_UI.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tillatov.TOVAR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tillatov.TOVAR BRW_TILLATOV _BROWSE-COLUMN WINDOW-2
ON ENTRY OF tillatov.TOVAR IN BROWSE BRW_TILLATOV /* Tillåt övertid! mer än 200tim/år */
DO:
  IF AVAILABLE tillatov THEN DO:
      DISPLAY tillatov.TOVAR WITH BROWSE BRW_TILLATOV.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tillatov.TOVAR BRW_TILLATOV _BROWSE-COLUMN WINDOW-2
ON LEAVE OF tillatov.TOVAR IN BROWSE BRW_TILLATOV /* Tillåt övertid! mer än 200tim/år */
DO:
  IF AVAILABLE tillatov THEN DO:
      tillatov.TOVAR = INPUT BROWSE BRW_TILLATOV tillatov.TOVAR.           
      DISPLAY tillatov.TOVAR WITH BROWSE BRW_TILLATOV .             
    END.        
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tillatov.TOVAR BRW_TILLATOV _BROWSE-COLUMN WINDOW-2
ON MOUSE-SELECT-CLICK OF tillatov.TOVAR IN BROWSE BRW_TILLATOV /* Tillåt övertid! mer än 200tim/år */
DO:
  IF AVAILABLE tillatov THEN DO:
      IF INPUT BROWSE BRW_TILLATOV tillatov.TOVAR = TRUE THEN tillatov.TOVAR = FALSE.
      IF INPUT BROWSE BRW_TILLATOV tillatov.TOVAR = FALSE THEN tillatov.TOVAR = TRUE.        
      DISPLAY tillatov.TOVAR WITH BROWSE BRW_TILLATOV.             
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tillatov.TOVMAN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tillatov.TOVMAN BRW_TILLATOV _BROWSE-COLUMN WINDOW-2
ON ENTRY OF tillatov.TOVMAN IN BROWSE BRW_TILLATOV /* Tillåt övertid! mer än 50tim/månad */
DO:
   IF AVAILABLE tillatov THEN DO:
      DISPLAY tillatov.TOVMAN WITH BROWSE BRW_TILLATOV.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tillatov.TOVMAN BRW_TILLATOV _BROWSE-COLUMN WINDOW-2
ON LEAVE OF tillatov.TOVMAN IN BROWSE BRW_TILLATOV /* Tillåt övertid! mer än 50tim/månad */
DO:
   IF AVAILABLE tillatov THEN DO:
      tillatov.TOVMAN = INPUT BROWSE BRW_TILLATOV tillatov.TOVMAN.           
      DISPLAY tillatov.TOVMAN WITH BROWSE BRW_TILLATOV .             
    END.    
     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tillatov.TOVMAN BRW_TILLATOV _BROWSE-COLUMN WINDOW-2
ON MOUSE-SELECT-CLICK OF tillatov.TOVMAN IN BROWSE BRW_TILLATOV /* Tillåt övertid! mer än 50tim/månad */
DO:
  IF AVAILABLE tillatov THEN DO:
      IF INPUT BROWSE BRW_TILLATOV tillatov.TOVMAN = TRUE THEN tillatov.TOVMAN = FALSE.
      IF INPUT BROWSE BRW_TILLATOV tillatov.TOVMAN = FALSE THEN tillatov.TOVMAN = TRUE.        
      DISPLAY tillatov.TOVMAN WITH BROWSE BRW_TILLATOV.             
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tillatov.TOV48
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tillatov.TOV48 BRW_TILLATOV _BROWSE-COLUMN WINDOW-2
ON ENTRY OF tillatov.TOV48 IN BROWSE BRW_TILLATOV /* Tillåt övertid! mer än 48tim/4veckor */
DO:
    IF AVAILABLE tillatov THEN DO:
      DISPLAY tillatov.TOV48 WITH BROWSE BRW_TILLATOV.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tillatov.TOV48 BRW_TILLATOV _BROWSE-COLUMN WINDOW-2
ON LEAVE OF tillatov.TOV48 IN BROWSE BRW_TILLATOV /* Tillåt övertid! mer än 48tim/4veckor */
DO:
  IF AVAILABLE tillatov THEN DO:
      tillatov.TOV48 = INPUT BROWSE BRW_TILLATOV tillatov.TOV48.           
      DISPLAY tillatov.TOV48 WITH BROWSE BRW_TILLATOV .             
    END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tillatov.TOV48 BRW_TILLATOV _BROWSE-COLUMN WINDOW-2
ON MOUSE-SELECT-CLICK OF tillatov.TOV48 IN BROWSE BRW_TILLATOV /* Tillåt övertid! mer än 48tim/4veckor */
DO:
    IF AVAILABLE tillatov THEN DO:
      IF INPUT BROWSE BRW_TILLATOV tillatov.TOV48 = TRUE THEN tillatov.TOV48 = FALSE.
      IF INPUT BROWSE BRW_TILLATOV tillatov.TOV48 = FALSE THEN tillatov.TOV48 = TRUE.        
      DISPLAY tillatov.TOV48 WITH BROWSE BRW_TILLATOV.             
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tillatov.ATER50
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tillatov.ATER50 BRW_TILLATOV _BROWSE-COLUMN WINDOW-2
ON ENTRY OF tillatov.ATER50 IN BROWSE BRW_TILLATOV /* Återför!max 50 tim */
DO:
  IF AVAILABLE tillatov THEN DO:
      DISPLAY tillatov.ATER50 WITH BROWSE BRW_TILLATOV.
   END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tillatov.ATER50 BRW_TILLATOV _BROWSE-COLUMN WINDOW-2
ON LEAVE OF tillatov.ATER50 IN BROWSE BRW_TILLATOV /* Återför!max 50 tim */
DO:
  IF AVAILABLE tillatov THEN DO:
      tillatov.ATER50 = INPUT BROWSE BRW_TILLATOV tillatov.ATER50.           
      DISPLAY tillatov.ATER50 WITH BROWSE BRW_TILLATOV .             
    END.  

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tillatov.ATER50 BRW_TILLATOV _BROWSE-COLUMN WINDOW-2
ON MOUSE-SELECT-CLICK OF tillatov.ATER50 IN BROWSE BRW_TILLATOV /* Återför!max 50 tim */
DO:
  IF AVAILABLE tillatov THEN DO:
      IF INPUT BROWSE BRW_TILLATOV tillatov.ATER50 = TRUE THEN tillatov.ATER50 = FALSE.
      IF INPUT BROWSE BRW_TILLATOV tillatov.ATER50= FALSE THEN tillatov.ATER50 = TRUE.              
      IF tillatov.ATER50 = TRUE THEN DO:          
         IF tillatov.ATERTIM > 50 THEN tillatov.ATERTIM = 50.
         IF tillatov.ATERTIM = 0 THEN tillatov.ATERTIM = 50.
         tillatov.A50DATUM = TODAY.
         tillatov.A50GODK = Guru.Konstanter:globanv.
      END.        
      ELSE DO:
         ASSIGN 
         tillatov.ATERTIM = 0
         tillatov.A50DATUM = ?
         tillatov.A50GODK = "".
      END.
      DISPLAY tillatov.ATER50 tillatov.A50DATUM tillatov.A50GODK tillatov.ATERTIM WITH BROWSE BRW_TILLATOV .
                                  
   END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tillatov.ATERTIM
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tillatov.ATERTIM BRW_TILLATOV _BROWSE-COLUMN WINDOW-2
ON LEAVE OF tillatov.ATERTIM IN BROWSE BRW_TILLATOV /* Återför timmar */
DO:
  IF AVAILABLE tillatov THEN DO:
     IF tillatov.ATER50 = TRUE THEN DO:
         tillatov.ATERTIM = INPUT BROWSE BRW_TILLATOV tillatov.ATERTIM.    
         IF tillatov.ATERTIM > 50 THEN tillatov.ATERTIM = 50.
         IF tillatov.ATERTIM = 0 THEN tillatov.ATERTIM = 50.
         tillatov.A50DATUM = TODAY.
         tillatov.A50GODK = Guru.Konstanter:globanv.
      END.        
      ELSE DO:
         ASSIGN 
         tillatov.ATERTIM = 0
         tillatov.A50DATUM = ?
         tillatov.A50GODK = "".
      END.
      DISPLAY tillatov.A50DATUM tillatov.A50GODK tillatov.ATERTIM WITH BROWSE BRW_TILLATOV .
                     
    END.  

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB WINDOW-2
ON CHOOSE OF BTN_AVB IN FRAME FRAME-TIDS /* Avsluta */
DO:
   RUN rowleave_UI.
   {BORTBRWPROC.I}
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK WINDOW-2 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

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
   {ALLSTARTDYN.I}  
   ASSIGN   str=                                                                              
"======.===============================.===================.".         
   {muswait.i} 
   CREATE invartemp.
   ASSIGN
   invartemp.GA = Guru.Konstanter:globanv
   invartemp.GM = gvisatidpermanad
   invartemp.SK = skrivut 
   invartemp.TI = tidtabrec 
   invartemp.PER = persrec 
   invartemp.PER2 = persrec2 
   invartemp.MU = musz    
   invartemp.REGST = regstart  
   invartemp.REGSU = regslut 
   invartemp.RV = regvnr 
   invartemp.RDAG = regdagnamn         
   invartemp.RD = regdatum 
   invartemp.RM = regmnr 
   invartemp.RMN = regmannamn 
   invartemp.REGA = regar 
   invartemp.RT = regtotalt       
   invartemp.BD = bdatum 
   invartemp.AD = avdatum 
   invartemp.NY = nytid 
   invartemp.SEK = sekunder 
   invartemp.RSEK = regstartsek 
   invartemp.REGS = regslutsek 
   invartemp.GL = Guru.Konstanter:globforetag.
   EMPTY TEMP-TABLE tillatov  NO-ERROR. 
   FOR EACH tidpers:
      CREATE tillatov.
      BUFFER-COPY tidpers TO tillatov.
   END.   
   FOR EACH tillatov:
      RUN hdispens IN asfaktapph (INPUT tillatov.PERSONALKOD, OUTPUT dispens, OUTPUT dardatum, OUTPUT dargodk).         
      RUN hdispensm IN asfaktapph (INPUT tillatov.PERSONALKOD, OUTPUT dispensm, OUTPUT dmandatum, OUTPUT dmangodk).
      RUN hdispens48 IN asfaktapph (INPUT tillatov.PERSONALKOD, OUTPUT dispens48, OUTPUT d48datum, OUTPUT d48godk).         
      RUN ater50_UI IN asfaktapph (INPUT tillatov.PERSONALKOD, OUTPUT ater50,OUTPUT a50tim, OUTPUT a50datum, OUTPUT a50godk).
      ASSIGN
      tillatov.TOVAR = dispens
      tillatov.TOVMA = dispensm
      tillatov.TOV48 = dispens48
      tillatov.ATER50 = ater50      
      tillatov.DARDATUM = dardatum
      tillatov.DARGODK = dargodk
      tillatov.DMANDATUM = dmandatum
      tillatov.DMANGODK = dmangodk
      tillatov.D48DATUM = d48datum
      tillatov.D48GODK = d48godk
      tillatov.A50DATUM = a50datum
      tillatov.A50GODK = a50godk
      tillatov.ATERTIM = a50tim.
   END.   
      
   RUN enable_UI.
   IF Guru.Konstanter:globforetag = "snat" OR Guru.Konstanter:globforetag = "sund" THEN DO:
      
      tillatov.TOV48:VISIBLE IN BROWSE BRW_TILLATOV = FALSE.
      tillatov.D48DATUM:VISIBLE IN BROWSE BRW_TILLATOV = FALSE.
      tillatov.D48GODK:VISIBLE IN BROWSE BRW_TILLATOV = FALSE.
   END.      
   {FRMSIZE.I}     
   {musarrow.i}
   {WIN_M_SLUT.I}
   IF NOT THIS-PROCEDURE:PERSISTENT THEN
   WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE allstartbrw_UI WINDOW-2 
PROCEDURE allstartbrw_UI :
/* -----------------------------------------------------------
  Purpose: Changing screen-value for combo-box CMB_OMR     
  Parameters:  Input = Screen-value for CMB_FOR
  Notes:       
-------------------------------------------------------------*/    
   RUN DYNBRW.P PERSISTENT SET brwproc[1]
      (INPUT BRW_TILLATOV:HANDLE IN FRAME {&FRAME-NAME}).
      IF Guru.Konstanter:appcon THEN DO:
      IF Guru.Konstanter:varforetypval[27] = 0 THEN RUN ASFAKTAPP.P PERSISTENT SET asfaktapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:      
      IF Guru.Konstanter:varforetypval[27] = 0 THEN RUN ASFAKTAPP.P PERSISTENT SET asfaktapph.
   END.
  
      
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI WINDOW-2  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-2)
  THEN DELETE WIDGET WINDOW-2.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI WINDOW-2  _DEFAULT-ENABLE
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
  ENABLE BRW_TILLATOV BTN_AVB 
      WITH FRAME FRAME-TIDS IN WINDOW WINDOW-2.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-TIDS}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE rowleave_UI WINDOW-2 
PROCEDURE rowleave_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF AVAILABLE tillatov THEN DO:     
      DISPLAY tillatov.TOVAR tillatov.TOVMAN WITH BROWSE BRW_TILLATOV.
      tillatov.TOVAR  = INPUT BROWSE BRW_TILLATOV tillatov.TOVAR .
      tillatov.TOVMAN  = INPUT BROWSE BRW_TILLATOV tillatov.TOVMAN .
      tillatov.TOV48  = INPUT BROWSE BRW_TILLATOV tillatov.TOV48 .
      tillatov.ATER50  = INPUT BROWSE BRW_TILLATOV tillatov.ATER50.
          
      IF tillatov.TOVAR = FALSE THEN RUN bortdispens IN asfaktapph (INPUT tillatov.PERSONALKOD).   
      ELSE DO:
          RUN spdispens IN asfaktapph (INPUT tillatov.PERSONALKOD, INPUT tillatov.TOVAR, INPUT TODAY, INPUT Guru.Konstanter:globanv).
          IF tillatov.DARGODK = "" THEN DO: 
             tillatov.DARDATUM = TODAY.
             tillatov.DARGODK = Guru.Konstanter:globanv.
          END.   
      END.    
      
      IF tillatov.TOVMAN = FALSE THEN RUN bortdispensm IN asfaktapph (INPUT tillatov.PERSONALKOD).   
      ELSE DO:
          RUN spdispensM IN asfaktapph (INPUT tillatov.PERSONALKOD, INPUT tillatov.TOVMAN, INPUT TODAY, INPUT Guru.Konstanter:globanv).
          IF tillatov.DMANGODK = "" THEN DO:
             tillatov.DMANDATUM = TODAY.
             tillatov.DMANGODK = Guru.Konstanter:globanv.
          END.   
      END. 
      IF tillatov.TOV48 = FALSE THEN RUN bortdispens48 IN asfaktapph (INPUT tillatov.PERSONALKOD).   
      ELSE DO:
          RUN spdispens48 IN asfaktapph (INPUT tillatov.PERSONALKOD, INPUT tillatov.TOV48, INPUT TODAY, INPUT Guru.Konstanter:globanv).
          IF tillatov.D48GODK = "" THEN DO:
             tillatov.D48DATUM = TODAY.
             tillatov.D48GODK = Guru.Konstanter:globanv.
          END.   
      END.     
      IF tillatov.ATER50 = FALSE THEN RUN bortater50 IN asfaktapph (INPUT tillatov.PERSONALKOD).   
      ELSE DO:
          RUN spater50 IN asfaktapph (INPUT tillatov.PERSONALKOD, INPUT tillatov.ATER50, INPUT tillatov.ATERTIM, INPUT TODAY, INPUT Guru.Konstanter:globanv).
          IF tillatov.A50GODK = "" THEN DO: 
             tillatov.A50DATUM = TODAY.
             tillatov.A50GODK = Guru.Konstanter:globanv.
          END.   
      END.
      DISPLAY tillatov.TOVAR tillatov.TOVMAN tillatov.TOV48 tillatov.ATER50 tillatov.DARDATUM tillatov.DARGODK tillatov.DMANDATUM tillatov.DMANGODK
      tillatov.D48DATUM tillatov.D48GODK tillatov.A50DATUM tillatov.A50GODK WITH BROWSE BRW_TILLATOV.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

